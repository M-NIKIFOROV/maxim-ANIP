# 01_area_cleaning.R
#
# Script to clean the area(worldbank) dataset
#
# - Skips the first three metadata rows
# - Loads all data as lowercase
# - Loads all columns after the fourth as integers
# - Loads into area_raw dataframe
#
# Source definitions file (corrected path)
source("01_cleaning/definitions.R")

setwd("C:/Users/maxim/OneDrive/Documents/University - Year 3/ANIP/maxim-ANIP")

library(readr)
library(dplyr)
library(janitor)

# Define file path
area_file <- "data/raw/csv/area(worldbank).csv"


# Read CSV, skipping first 3 rows
area_raw <- read_csv(area_file, skip = 3, col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(everything(), ~tolower(as.character(.))))

# Rename the country name column to 'country' if present (case-insensitive)
name_col <- which(tolower(names(area_raw)) %in% c('country_name', 'country'))
if (length(name_col) == 1) {
  names(area_raw)[name_col] <- 'country'
}

# Convert columns after the fourth to integer
data_cols <- names(area_raw)[5:ncol(area_raw)]
area_raw <- area_raw %>%
  mutate(across(all_of(data_cols), ~as.integer(.)))

# Remove 'x' character from year column names (columns after the fourth)
names(area_raw)[5:ncol(area_raw)] <- gsub("x", "", names(area_raw)[5:ncol(area_raw)])

# Create a vector of all country names
area_countries <- area_raw$country

# Find EU countries missing from area_countries
missing_eu_countries <- setdiff(eu, area_countries)
cat("EU countries missing from area_countries:\n")
print(missing_eu_countries) # missing: slovakia

# Keep only rows where country_name is in the eu vector
area_raw <- area_raw %>%
  filter(country %in% eu)

# Remove year columns from 1960 to 1979 and 2024 to 2025
cols_to_remove <- as.character(c(1960:1979, 2024:2025))
area_raw <- area_raw %>% select(-all_of(cols_to_remove))

areac <- unique(area_raw$country)

write_csv(area_raw, "data/clean/area_cleaned.csv")

# has years: 1980-2023. Missing Slovakia