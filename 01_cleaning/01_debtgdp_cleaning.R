# 01_debtgdp_cleaning.R
#
# Script to clean the debtdgp(eurostat) dataset
#
# - Loads all data as lowercase
# - Converts all columns from the 8th onward to integer
# - Loads into debtgdp_raw dataframe
# Source definitions file
source("01_cleaning/definitions.R")

library(readr)
library(dplyr)
library(janitor)

# Define file path
debtgdp_file <- "data/raw/csv/debtdgp(eurostat).csv"


# Read CSV with no column names, all as character
debtgdp_raw <- read_csv(debtgdp_file, col_names = FALSE, col_types = cols(.default = "c"))

# Remove unwanted rows by number (adjust as needed)
# Example: remove first 9 rows, then rows 11, 13, 14 (after previous removal, these are 10, 12, 13), and rows 42 downward
rows_to_remove <- c(1:9, 11, 13, 14)
if (nrow(debtgdp_raw) >= 42) {
  rows_to_remove <- c(rows_to_remove, 42:nrow(debtgdp_raw))
}
debtgdp_raw <- debtgdp_raw[-rows_to_remove, ]

# Remove every second column starting with the 3rd
if (ncol(debtgdp_raw) > 2) {
  cols_to_remove <- seq(3, ncol(debtgdp_raw), by = 2)
  debtgdp_raw <- debtgdp_raw[, -cols_to_remove, drop = FALSE]
}

# Set first row as column names, then remove it from data
colnames(debtgdp_raw) <- as.character(debtgdp_raw[1, ])
debtgdp_raw <- debtgdp_raw[-1, ]

# Rename 'TIME' column to 'country' if it exists (case-insensitive)
time_col <- which(tolower(colnames(debtgdp_raw)) == 'time')
if (length(time_col) == 1) {
  colnames(debtgdp_raw)[time_col] <- 'country'
}

# Convert all columns except the first to integer
if (ncol(debtgdp_raw) > 1) {
  data_cols <- names(debtgdp_raw)[2:ncol(debtgdp_raw)]
  debtgdp_raw <- debtgdp_raw %>% mutate(across(all_of(data_cols), ~as.integer(.)))
}

# Convert all data to lowercase
# Only apply to character columns to avoid encoding issues
char_cols <- sapply(debtgdp_raw, is.character)
debtgdp_raw[char_cols] <- lapply(debtgdp_raw[char_cols], tolower)

# Rename 'time' column to 'country' if it exists
if ("time" %in% names(debtgdp_raw)) {
  debtgdp_raw <- debtgdp_raw %>% rename(country = time)
}

# Create a vector of all country names
debtgdp_countries <- debtgdp_raw$country

# Find EU countries missing from debtgdp_countries
missing_eu_countries <- setdiff(eu, debtgdp_countries)
cat("EU countries missing from debtgdp_countries:\n")
print(missing_eu_countries) # none missing

# Keep only rows where country is in the eu vector
debtgdp_raw <- debtgdp_raw %>%
  filter(country %in% eu)
  

debtgdpc <- unique(debtgdp_raw$country)

write_csv(debtgdp_raw, "data/clean/debtgdp_cleaned.csv")