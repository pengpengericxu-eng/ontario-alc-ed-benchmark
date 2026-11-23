# clean and merge the CIHI files into one panel
# Input  (data/raw/):   alc.xlsx, ed.xlsx, frailty.xlsx, beds_2024_25.xlsx
# Output (data/clean/): panel.csv = one row per hospital per fiscal year
#
# Unit of analysis: a hospital corporation, or a single-site hospital that
# isn't part of one. That's the level CIHI reports ALC at, so every other file
# is matched to the ALC file's list of hospitals.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Settings

raw_dir   <- "data/raw"
clean_dir <- "data/clean"
dir.create(clean_dir, showWarnings = FALSE, recursive = TRUE)

min_acute_beds <- 100        # "large hospital" cut-off
part_a_years   <- 2022:2024  # fiscal years 2022-23 to 2024-25 (year = start year)
part_b_years   <- 2022:2025  # fiscal years 2022-23 to 2025-26

# Dropped from every analysis
excluded <- c(
  "children's hospital of eastern ontario",  # pediatric
  "hospital for sick children",              # pediatric
  "shouldice hospital"                       # specialty surgery, no ED
)

# Peterborough and Timmins have suppressed ("x") bed counts in 2024-25.
# If you find their counts (e.g., in the 2023-24 beds table), enter them here.
# Left empty, they drop out of the analysis.
bed_overrides <- c(
  # "peterborough regional health centre corporation" = 000,
  # "timmins and district hospital corporation"        = 000
)


# Helpers

# Turn a hospital name into a matching key. The files spell names slightly
# differently: curly vs straight apostrophes, a "(Ont.)" suffix, stray tabs,
# [brackets] vs (parentheses), and dagger symbols in the beds table.
clean_key <- function(x) {
  x |>
    str_replace_all("\t", "") |>
    str_replace_all("[‘’]", "'") |>
    str_replace_all("[–—]", "-") |>    # en/em dashes -> plain hyphen
    str_replace_all("[†‡]", "") |>
    str_replace_all("\\[", "(") |>
    str_replace_all("\\]", ")") |>
    str_remove("\\s*\\(Ont\\.\\)\\s*$") |>
    str_squish() |>
    str_to_lower()
}

# Read one of the CIHI indicator files (ALC, ED time, frailty). They share a
# layout: one row per place per fiscal year. Keep Ontario hospitals only.
# `value_pattern` picks the value column by the start of its name.
read_cihi <- function(file, value_pattern, new_name) {
  d <- read_excel(file.path(raw_dir, file))
  value_col <- grep(value_pattern, names(d), value = TRUE)[1]
  if (is.na(value_col)) stop("No column matching '", value_pattern, "' in ", file)
  
  d |>
    filter(`Province/territory` == "Ontario",
           `Reporting level` %in% c("Corporation", "Facility")) |>
    transmute(
      key      = clean_key(`Place or organization`),
      hospital = `Place or organization` |>
        str_replace_all("\t", "") |>
        str_remove("\\s*\\(Ont\\.\\)\\s*$") |>
        str_squish(),
      region   = str_remove(Region, "\\s*\\(Ont\\.\\)\\s*$"),
      level    = `Reporting level`,
      year     = as.integer(str_sub(`Time frame`, 1, 4)),  # "2024–2025" -> 2024
      # "Suppressed" becomes NA
      !!new_name := suppressWarnings(as.numeric(.data[[value_col]]))
    )
}


# ALC, ED time and frailty 

alc       <- read_cihi("alc.xlsx",     "^Percent$",                        "alc_pct")
ed        <- read_cihi("ed.xlsx",      "^90th percentile$",                "ed_hours_p90")
frail_raw <- read_cihi("frailty.xlsx", "^Hospitalized seniors at risk",    "frail_pct")

# The ED file also lists individual sites of multi-site corporations
# (e.g., "Trillium Health Partners — Mississauga Hospital"). ALC is only
# reported for the whole corporation, so those site rows simply won't match
# and drop out in the join below.


#  Beds and teaching status (2024-25) 

beds_raw <- read_excel(file.path(raw_dir, "beds_2024_25.xlsx"),
                       sheet = "Table 1", skip = 1)
names(beds_raw) <- names(beds_raw) |> str_replace_all("\\s+", " ") |> str_trim()

# Beds rows are mostly named "Corporation — Site". The part before the dash
# is the corporation, which is the key. These rows need a manual fix
# (keys are lowercase, with plain hyphens and apostrophes):
beds_recode <- c(
  "southlake regional health centre"        = "southlake health corporation",
  # Merged into Waterloo Regional Health Network:
  "grand river hospital"                    = "waterloo regional health network",
  "st. mary's general hospital corporation" = "waterloo regional health network",
  # Merged into Rural Roads Health Services:
  "alexandra hospital"                      = "rural roads health services",
  "tillsonburg district memorial hospital"  = "rural roads health services"
)
# Note: "The Ottawa Hospital — University of Ottawa Heart Institute" is grouped
# under The Ottawa Hospital, following the beds table's own naming.

bed_sites <- beds_raw |>
  filter(`Province/territory` == "Ontario") |>
  mutate(
    site_key  = clean_key(str_split_i(`Hospital name`, " — ", 1)),
    key       = if_else(site_key %in% names(beds_recode),
                        unname(beds_recode[site_key]), site_key),
    acute_raw = str_trim(as.character(`Total Acute Beds`)),
    # "—" means none (0 beds); "x" means suppressed (unknown)
    acute     = case_when(
      acute_raw == "x"                    ~ NA_real_,
      str_detect(acute_raw, "^—")    ~ 0,
      TRUE                                ~ suppressWarnings(as.numeric(acute_raw))
    )
  )

beds <- bed_sites |>
  group_by(key) |>
  summarise(
    acute_beds = sum(acute),            # NA if any site is suppressed
    teaching   = as.integer(any(`Teaching status` == "Teaching")),
    n_sites    = n(),
    .groups = "drop"
  )

if (length(bed_overrides) > 0) {
  beds$acute_beds[match(names(bed_overrides), beds$key)] <- bed_overrides
}


#  Frailty for merged hospitals 

# The frailty file still reports the hospitals that merged as separate rows.
# Combine them into the new corporation, weighting each by its acute beds.
frailty_mergers <- tribble(
  ~old_key,                                                        ~new_key,                           ~site_key,
  "grand river hospital",                                          "waterloo regional health network", "grand river hospital",
  "st. mary's general hospital corporation",                       "waterloo regional health network", "st. mary's general hospital corporation",
  "rural roads health services - alexandra hospital",              "rural roads health services",      "alexandra hospital",
  "rural roads health services - tillsonburg district mem. hosp.", "rural roads health services",      "tillsonburg district memorial hospital"
)

site_weights <- bed_sites |>
  group_by(site_key) |>
  summarise(weight = sum(acute, na.rm = TRUE), .groups = "drop")

frail_merged <- frail_raw |>
  inner_join(frailty_mergers, by = c("key" = "old_key")) |>
  left_join(site_weights, by = "site_key") |>
  group_by(key = new_key, year) |>
  summarise(frail_pct = weighted.mean(frail_pct, w = weight, na.rm = TRUE),
            .groups = "drop") |>
  mutate(frail_pct = if_else(is.nan(frail_pct), NA_real_, frail_pct))

frail <- frail_raw |>
  filter(!key %in% frailty_mergers$old_key) |>
  select(key, year, frail_pct) |>
  bind_rows(frail_merged)


#  Build the panel 

panel <- alc |>
  select(key, hospital, region, level, year, alc_pct) |>
  left_join(select(ed, key, year, ed_hours_p90), by = c("key", "year")) |>
  left_join(frail, by = c("key", "year")) |>
  left_join(beds,  by = "key") |>
  filter(!key %in% excluded) |>
  mutate(
    fiscal_year = paste0(year, "-", str_sub(year + 1, 3, 4)),   # 2024 -> "2024-25"
    log_beds    = log(acute_beds),
    large       = !is.na(acute_beds) & acute_beds >= min_acute_beds,
    in_part_a   = large & year %in% part_a_years &
      !is.na(alc_pct) & !is.na(ed_hours_p90) & !is.na(frail_pct),
    in_part_b   = large & year %in% part_b_years &
      !is.na(alc_pct) & !is.na(ed_hours_p90)
  ) |>
  select(hospital, key, region, level, fiscal_year, year,
         ed_hours_p90, alc_pct, frail_pct, acute_beds, log_beds, teaching,
         n_sites, large, in_part_a, in_part_b) |>
  arrange(hospital, year)

write_csv(panel, file.path(clean_dir, "panel.csv"))


# Checks 
# Read these before moving on. Each should match the data check.

cat("\n== Hospitals in the ALC file (after exclusions):", n_distinct(panel$key), "\n")

no_match <- function(var) {
  panel |>
    group_by(hospital) |>
    filter(all(is.na(.data[[var]]))) |>
    distinct(hospital) |>
    pull(hospital)
}
cat("\nNo ED time in any year:\n");  print(no_match("ed_hours_p90"))
cat("\nNo frailty in any year:\n");  print(no_match("frail_pct"))
cat("\nNo bed count:\n");            print(no_match("acute_beds"))

cat("\n== Large hospitals (", min_acute_beds, "+ acute beds): ",
    n_distinct(panel$key[panel$large]), "\n", sep = "")

part_a <- filter(panel, in_part_a)
part_b <- filter(panel, in_part_b)
cat("\n== Part A:", n_distinct(part_a$key), "hospitals,", nrow(part_a),
    "hospital-years (expect about 46 and 138)\n")
cat("== Part B:", n_distinct(part_b$key), "hospitals,", nrow(part_b),
    "hospital-years (expect about 46 and 184)\n")

cat("\nLarge hospitals missing from Part A in some year, and why:\n")
panel |>
  filter(large, year %in% part_a_years, !in_part_a) |>
  select(hospital, fiscal_year, ed_hours_p90, alc_pct, frail_pct) |>
  print(n = Inf)

cat("\nPart A summary:\n")
part_a |>
  summarise(across(c(ed_hours_p90, alc_pct, frail_pct, acute_beds),
                   list(mean = mean, min = min, max = max))) |>
  pivot_longer(everything(),
               names_to = c("variable", "stat"), names_pattern = "(.*)_(mean|min|max)$") |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  print()

cat("\nTeaching hospitals in Part A:", n_distinct(part_a$key[part_a$teaching == 1]), "\n")
cat("\nSaved:", file.path(clean_dir, "panel.csv"), "\n")

