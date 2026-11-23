#Part A: benchmark across hospitals

# Question: which large hospitals have shorter (or longer) ED times for
# admitted patients than expected, given their ALC burden, frailty, size and
# teaching status?
#

library(dplyr)
library(tidyr)
library(readr)
library(sandwich)   # clustered standard errors
library(lmtest)     # coeftest() to print them
library(ggplot2)

dir.create("output", showWarnings = FALSE)

panel <- read_csv("data/clean/panel.csv", show_col_types = FALSE)
a <- panel |> filter(in_part_a)

cat("Part A sample:", n_distinct(a$key), "hospitals,", nrow(a), "hospital-years\n")


# Expected-time model 
# ED time (90th percentile, hours) on ALC %, frailty %, log acute beds and
# teaching status, plus a dummy for each year to remove province-wide swings.
# Beds are logged because going from 100 to 200 beds matters more than going from 1,000 to 1,100.

m <- lm(ed_hours_p90 ~ alc_pct + frail_pct + log_beds + teaching + factor(year),
        data = a)

# Each hospital appears 3 times, so its rows aren't independent. Clustered standard errors by hospital to account for it
vc <- vcovCL(m, cluster = ~ key)
cat("\n Model (clustered standard errors by hospital)\n")
print(coeftest(m, vcov. = vc))
cat("\n95% confidence intervals:\n")
print(round(coefci(m, vcov. = vc), 2))
cat("\nR-squared:", round(summary(m)$r.squared, 3),
    "| share of variation unexplained:", round(1 - summary(m)$r.squared, 3), "\n")


#  Model checks 

# VIF: how much each predictor overlaps with the others. Above ~5 is a worry.
predictors <- c("alc_pct", "frail_pct", "log_beds", "teaching")
vif <- sapply(predictors, function(v) {
  others <- setdiff(predictors, v)
  r2 <- summary(lm(reformulate(others, v), data = a))$r.squared
  1 / (1 - r2)
})
cat("\nVIF\n"); print(round(vif, 2))

# Cook's distance (influence) and leverage (unusual predictor values).
# Common cut-offs: Cook's D > 4/n, leverage > 2p/n.
n <- nrow(a); p <- length(coef(m))
a <- a |>
  mutate(cooks    = cooks.distance(m),
         leverage = hatvalues(m),
         influential = cooks > 4 / n | leverage > 2 * p / n)

cat("\nInfluential hospital-years (Cook's D >", round(4 / n, 3),
    "or leverage >", round(2 * p / n, 3), ")\n")
a |> filter(influential) |>
  select(hospital, fiscal_year, ed_hours_p90, alc_pct, acute_beds, cooks, leverage) |>
  mutate(across(c(cooks, leverage), \(x) round(x, 3))) |>
  print(n = Inf)

# Does dropping them change the ALC effect much? If not, no single hospital is bending the line.
m_drop <- update(m, data = filter(a, !influential))
cat("\nALC coefficient, all rows:", round(coef(m)["alc_pct"], 2),
    "| without influential rows:", round(coef(m_drop)["alc_pct"], 2), "\n")

# Is the ALC effect clearer without the influential rows? Use clustered errors again.
vc_drop <- vcovCL(m_drop, cluster = ~ key)
ci_drop <- coefci(m_drop, vcov. = vc_drop)["alc_pct", ]
p_drop  <- coeftest(m_drop, vcov. = vc_drop)["alc_pct", "Pr(>|t|)"]
cat("Without influential rows: 95% CI", round(ci_drop[1], 2), "to", round(ci_drop[2], 2),
    "| p =", round(p_drop, 3), "\n")

# Each hospital vs its prediction 
# Residual = actual - expected. Negative = shorter ED time than expected (good).
# Average over the 3 years, then standardize to a z-score for ease of comparison

a <- a |> mutate(expected = fitted(m), residual = resid(m))

hosp <- a |>
  group_by(hospital, key, region, teaching) |>
  summarise(acute_beds   = first(acute_beds),
            alc_pct      = mean(alc_pct),
            actual_hours = mean(ed_hours_p90),
            expected_hours = mean(expected),
            residual     = mean(residual),
            .groups = "drop") |>
  mutate(z = (residual - mean(residual)) / sd(residual),
         flag = case_when(z <= -2 ~ "Better than expected",
                          z >=  2 ~ "Worse than expected",
                          TRUE    ~ ""))


#  Raw vs adjusted ranking 
# Raw = rank by ED time alone (1 = shortest).
# Adjusted = rank by residual (1 = furthest below expected).

hosp <- hosp |>
  mutate(raw_rank      = rank(actual_hours, ties.method = "min"),
         adjusted_rank = rank(residual,     ties.method = "min"),
         rank_change   = raw_rank - adjusted_rank) |>   # positive = looks better once adjusted
  arrange(adjusted_rank)

cat("\nHospitals flagged (|z| > 2)\n")
hosp |> filter(flag != "") |>
  select(hospital, actual_hours, expected_hours, z, raw_rank, adjusted_rank) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |> print(n = Inf)

cat("\nTop 5 and bottom 5 by adjusted rank (shortlist for the follow-up)\n")
bind_rows(head(hosp, 5), tail(hosp, 5)) |>
  select(hospital, actual_hours, expected_hours, z, raw_rank, adjusted_rank) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |> print(n = Inf)

cat("\nBiggest movers between raw and adjusted ranking\n")
hosp |> arrange(desc(abs(rank_change))) |> head(8) |>
  select(hospital, acute_beds, alc_pct, raw_rank, adjusted_rank, rank_change) |>
  mutate(alc_pct = round(alc_pct, 1)) |> print(n = Inf)

cat("\nRank correlation, raw vs adjusted (Spearman):",
    round(cor(hosp$raw_rank, hosp$adjusted_rank, method = "spearman"), 2), "\n")

write_csv(hosp |> mutate(across(where(is.double), \(x) round(x, 2))),
          "output/part_a_hospitals.csv")


#  Persistence 
# Do hospitals that beat expectations one year also beat them the next?
# (a) correlation of each hospital's residual from one year to the next
# (b) Fisher's exact test on a 2x2 table: better than expected this year
#     (yes/no) vs better than expected next year (yes/no)

wide <- a |> select(key, year, residual) |>
  pivot_wider(names_from = year, values_from = residual)
yrs <- sort(unique(a$year))
cat("\n Persistence\n")
for (i in seq_along(yrs)[-1]) {
  r <- cor(wide[[as.character(yrs[i - 1])]], wide[[as.character(yrs[i])]],
           use = "complete.obs")
  cat("Residual correlation", yrs[i - 1], "->", yrs[i], ":", round(r, 2), "\n")
}

pairs <- a |> arrange(key, year) |> group_by(key) |>
  mutate(better_now = residual < 0, better_next = lead(residual) < 0) |>
  ungroup() |> filter(!is.na(better_next))
tab <- table(this_year = pairs$better_now, next_year = pairs$better_next)
cat("\nBetter than expected this year vs next year:\n"); print(tab)
ft <- fisher.test(tab)
cat("Fisher's exact test: odds ratio", round(ft$estimate, 1),
    "| p =", signif(ft$p.value, 2), "\n")


#  Robustness: add region 
# Region dummies partly capture regional long-term care and home care supply.
# Check whether the ALC effect and the shortlist hold up.

m_reg <- update(m, . ~ . + factor(region))
ci_reg <- coefci(m_reg, vcov. = vcovCL(m_reg, cluster = ~ key))["alc_pct", ]
cat("\n With region dummies\nALC coefficient:", round(coef(m_reg)["alc_pct"], 2),
    "(95% CI", round(ci_reg[1], 2), "to", round(ci_reg[2], 2), ")\n")

hosp_reg <- a |> mutate(residual_reg = resid(m_reg)) |>
  group_by(key) |> summarise(residual_reg = mean(residual_reg), .groups = "drop")
top_bottom <- function(d, col) {
  d <- arrange(d, .data[[col]])
  c(head(d$key, 5), tail(d$key, 5))
}
overlap <- length(intersect(top_bottom(hosp, "residual"),
                            top_bottom(hosp_reg, "residual_reg")))
cat("Shortlist overlap with the main model:", overlap, "of 10 hospitals\n")


# Charts

label_these <- hosp |> filter(adjusted_rank <= 5 | adjusted_rank > nrow(hosp) - 5)

p1 <- ggplot(hosp, aes(expected_hours, actual_hours)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(colour = z), size = 2.5) +
  geom_text(data = label_these, aes(label = hospital), size = 2.8,
            vjust = -0.9) +
  scale_colour_gradient2(low = "#2166ac", mid = "grey75", high = "#b2182b",
                         midpoint = 0, name = "z-score") +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  scale_y_continuous(expand = expansion(mult = 0.08)) +
  labs(title = "ED time for admitted patients: actual vs expected",
       subtitle = "Large Ontario hospitals, 2022-23 to 2024-25 average. Below the dashed line = shorter than expected",
       x = "Expected hours (90th percentile)", y = "Actual hours (90th percentile)") +
  theme_minimal(base_size = 11)
ggsave("output/part_a_predicted_vs_actual.png", p1, width = 10, height = 7, dpi = 200)

movers <- hosp |> arrange(desc(abs(rank_change))) |> head(12) |>
  mutate(hospital = reorder(hospital, rank_change))
p2 <- ggplot(movers) +
  geom_segment(aes(x = raw_rank, xend = adjusted_rank, y = hospital, yend = hospital),
               colour = "grey60") +
  geom_point(aes(x = raw_rank, y = hospital), colour = "grey40", size = 2.5) +
  geom_point(aes(x = adjusted_rank, y = hospital), colour = "#2166ac", size = 2.5) +
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, nrow(hosp))) +
  labs(title = "Biggest moves between raw and adjusted ranking",
       subtitle = "Grey = raw rank (ED time alone). Blue = rank after adjusting. 1 = best",
       x = "Rank", y = NULL) +
  theme_minimal(base_size = 11)
ggsave("output/part_a_rank_change.png", p2, width = 9, height = 6, dpi = 200)

cat("\nSaved: output/part_a_hospitals.csv and two charts in output/\n")
