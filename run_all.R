# Runs the whole analysis in order. Open the project in RStudio (or set the
# working directory to this folder), then: source("run_all.R")

pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "readr",
          "sandwich", "lmtest", "ggplot2", "ggrepel")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)

source("R/01_clean_merge.R")  # data/raw/*.xlsx -> data/clean/panel.csv
source("R/02_part_a.R")       # benchmark across hospitals -> output/part_a_*
source("R/03_part_b.R")       # each hospital over time    -> output/part_b_*
