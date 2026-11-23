# Part B: each hospital over time
# Questions:
#   1. When a hospital's own ALC share rises, does its own ED time rise?
#   2. Which hospitals improved (or got worse) more than their ALC change
#      would suggest?

library(dplyr)
library(tidyr)
library(readr)
library(sandwich)
library(lmtest)
library(ggplot2)
library(ggrepel)    # labels that don't overlap

dir.create("output", showWarnings = FALSE)

panel <- read_csv("data/clean/panel.csv", show_col_types = FALSE)
b <- panel |> filter(in_part_b)

cat("Part B sample:", n_distinct(b$key), "hospitals,", nrow(b),
    "hospital-years (", min(b$fiscal_year), "to", max(b$fiscal_year), ")\n")


# How much does ALC move within a hospital? 
# The over-time model can only learn from changes in ALC within each hospital.
# If ALC barely moves, there's little to learn from, so check this first.

within_alc <- b |>
  group_by(key) |>
  mutate(alc_dev = alc_pct - mean(alc_pct)) |>    # each year vs the hospital's own average
  ungroup()

cat("\n== How much ALC varies\n")
cat("Between hospitals (SD of hospital averages):",
    round(sd(tapply(b$alc_pct, b$key, mean)), 1), "points\n")
cat("Within hospitals (SD of yearly deviations from own average):",
    round(sd(within_alc$alc_dev), 1), "points\n")


# Over-time model (hospital and year fixed effects) 
# factor(key) gives every hospital its own intercept, so each hospital is only
# compared with itself. Anything stable about a hospital (size, region,
# building, catchment, how it records ALC) drops out.
# factor(year) removes province-wide swings that hit every hospital at once.
# What's left: does ED time go up in the years a hospital's ALC goes up?

fe <- lm(ed_hours_p90 ~ alc_pct + factor(key) + factor(year), data = b)
vc <- vcovCL(fe, cluster = ~ key)

alc_row <- function(model, vcov_matrix) {
  ct <- coeftest(model, vcov. = vcov_matrix)["alc_pct", ]
  ci <- coefci(model, vcov. = vcov_matrix)["alc_pct", ]
  sprintf("%+.2f hrs per ALC point (95%% CI %+.2f to %+.2f), p = %.3f",
          ct["Estimate"], ci[1], ci[2], ct["Pr(>|t|)"])
}

cat("\n== Over-time model:", n_distinct(b$key), "large hospitals,",
    min(b$fiscal_year), "to", max(b$fiscal_year), "\n")
cat("ALC:", alc_row(fe, vc), "\n")
cat("Year effects (vs", min(b$fiscal_year), "):\n")
print(round(coef(fe)[grep("factor\\(year\\)", names(coef(fe)))], 2))


# Robustness checks 

# (a) Add frailty. Frailty isn't published for 2025-26, so this drops a year.
b_fr <- b |> filter(!is.na(frail_pct))
fe_fr <- lm(ed_hours_p90 ~ alc_pct + frail_pct + factor(key) + factor(year), data = b_fr)
cat("\n== Robustness\n(a) With frailty,", min(b_fr$fiscal_year), "to",
    max(b_fr$fiscal_year), ":", alc_row(fe_fr, vcovCL(fe_fr, cluster = ~ key)), "\n")

# (b) Last year's ALC instead of this year's. ALC pressure might take time to
#     show up in the ED. lag() needs the rows in year order within each hospital.
#     Start from the full panel so 2022-23 can use 2021-22's ALC as its lag.
b_lag <- panel |>
  filter(large, !is.na(alc_pct)) |>
  arrange(key, year) |> group_by(key) |>
  mutate(alc_last_year = lag(alc_pct)) |> ungroup() |>
  filter(in_part_b, !is.na(alc_last_year))
fe_lag <- lm(ed_hours_p90 ~ alc_last_year + factor(key) + factor(year), data = b_lag)
ct_lag <- coeftest(fe_lag, vcov. = vcovCL(fe_lag, cluster = ~ key))["alc_last_year", ]
ci_lag <- coefci(fe_lag, vcov. = vcovCL(fe_lag, cluster = ~ key))["alc_last_year", ]
cat(sprintf("(b) Last year's ALC: %+.2f hrs per point (95%% CI %+.2f to %+.2f), p = %.3f\n",
            ct_lag["Estimate"], ci_lag[1], ci_lag[2], ct_lag["Pr(>|t|)"]))

# (c) All hospitals, not just large ones (footnote: small hospitals add
#     many more ALC changes to learn from).
b_all <- panel |> filter(year %in% unique(b$year), !is.na(alc_pct), !is.na(ed_hours_p90))
fe_all <- lm(ed_hours_p90 ~ alc_pct + factor(key) + factor(year), data = b_all)
cat("(c) All hospitals (", n_distinct(b_all$key), "):",
    alc_row(fe_all, vcovCL(fe_all, cluster = ~ key)), "\n")


# Improvers and decliners
# Compare each hospital's first and last year.
# Improver: ED time fell 5+ hours while ALC stayed flat or rose (>= -1 point).
#           It got faster without help from falling ALC.
# Decliner: ED time rose 5+ hours while ALC stayed flat or fell (<= +1 point).

first_yr <- min(b$year); last_yr <- max(b$year)

changes <- b |>
  filter(year %in% c(first_yr, last_yr)) |>
  select(hospital, key, region, year, ed_hours_p90, alc_pct) |>
  pivot_wider(names_from = year, values_from = c(ed_hours_p90, alc_pct)) |>
  rename(ed_first  = paste0("ed_hours_p90_", first_yr),
         ed_last   = paste0("ed_hours_p90_", last_yr),
         alc_first = paste0("alc_pct_", first_yr),
         alc_last  = paste0("alc_pct_", last_yr)) |>
  mutate(ed_change  = ed_last - ed_first,
         alc_change = alc_last - alc_first,
         status = case_when(
           ed_change <= -5 & alc_change >= -1 ~ "Improver",
           ed_change >=  5 & alc_change <=  1 ~ "Decliner",
           TRUE ~ ""))

cat("\n== Change from", first_yr, "to", last_yr, "\n")
cat("ALC change (points): median", round(median(changes$alc_change), 1),
    "| middle half", round(quantile(changes$alc_change, .25), 1), "to",
    round(quantile(changes$alc_change, .75), 1), "\n")
cat("ED change (hours):   median", round(median(changes$ed_change), 1),
    "| middle half", round(quantile(changes$ed_change, .25), 1), "to",
    round(quantile(changes$ed_change, .75), 1), "\n")
cat("Correlation of ALC change and ED change:",
    round(cor(changes$alc_change, changes$ed_change), 2), "\n")

# Link to Part A: where did these hospitals sit in the benchmark?
part_a <- read_csv("output/part_a_hospitals.csv", show_col_types = FALSE) |>
  select(key, part_a_z = z, part_a_rank = adjusted_rank)
changes <- changes |> left_join(part_a, by = "key")

cat("\n== Improvers and decliners\n")
changes |> filter(status != "") |>
  arrange(status, ed_change) |>
  select(status, hospital, ed_first, ed_last, ed_change, alc_first, alc_last,
         alc_change, part_a_rank) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |>
  print(n = Inf, width = Inf)

write_csv(changes |> mutate(across(where(is.double), \(x) round(x, 2))),
          "output/part_b_changes.csv")


# Charts 

flagged <- changes |> filter(status != "")
fy_label <- function(y) paste0(y, "-", substr(y + 1, 3, 4))
plot_data <- changes |> mutate(status = if_else(status == "", "Other", status))

p1 <- ggplot(plot_data, aes(alc_change, ed_change)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(aes(colour = status), size = 2.5) +
  geom_text_repel(data = flagged, aes(label = hospital, colour = status),
                  size = 2.9, box.padding = 0.6, min.segment.length = 0,
                  seed = 1, show.legend = FALSE) +
  scale_colour_manual(values = c("Improver" = "#2166ac", "Decliner" = "#b2182b",
                                 "Other" = "grey65"),
                      breaks = c("Improver", "Decliner"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title = "Change in ED time vs change in ALC, by hospital",
       subtitle = paste0("Large Ontario hospitals, ", fy_label(first_yr), " to ", fy_label(last_yr),
                         ". Improvers got 5+ hours faster with flat or rising ALC"),
       x = "Change in ALC share (percentage points)",
       y = "Change in ED time, 90th percentile (hours)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
ggsave("output/part_b_change_scatter.png", p1, width = 10, height = 7, dpi = 200)

traj <- b |>
  filter(key %in% flagged$key) |>
  left_join(select(flagged, key, status), by = "key") |>
  select(hospital, status, fiscal_year, `ED time (hours)` = ed_hours_p90,
         `ALC share (%)` = alc_pct) |>
  pivot_longer(c(`ED time (hours)`, `ALC share (%)`), names_to = "measure")

p2 <- ggplot(traj, aes(fiscal_year, value, group = measure, colour = measure)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ paste0(hospital, " (", status, ")"), ncol = 3) +
  scale_colour_manual(values = c("ED time (hours)" = "#1b1b1b", "ALC share (%)" = "#d6604d"),
                      name = NULL) +
  labs(title = "Improvers and decliners: ED time and ALC each year",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("output/part_b_trajectories.png", p2, width = 10, height = 6, dpi = 200)

cat("\nSaved: output/part_b_changes.csv and two charts in output/\n")
