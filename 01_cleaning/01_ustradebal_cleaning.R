# 01_ustradebal_cleaning.R
#
# Script to clean the ustradebal(dataweb) dataset
#
# - Skips the first 2 metadata rows
# - Removes the 'Data Type' column
# - Removes the 'Total:' summary row
# - Lowercases all values
#
source("01_cleaning/definitions.R")

setwd("C:/Users/maxim/OneDrive/Documents/University - Year 3/ANIP/maxim-ANIP")

library(readr)
library(dplyr)
library(janitor)

ustradebal_file <- "data/raw/csv/ustradebal(dataweb).csv"

ustradebal_raw <- read_csv(ustradebal_file, skip = 2, col_types = cols(.default = "c")) %>%
  clean_names() %>%
  select(-data_type) %>%
  filter(tolower(trimws(country)) != "total:") %>%
  mutate(across(everything(), ~tolower(as.character(.))))

# Remove 'x' prefix from year column names
year_cols <- names(ustradebal_raw)[names(ustradebal_raw) != "country"]
names(ustradebal_raw)[names(ustradebal_raw) != "country"] <- gsub("^x", "", year_cols)

# Convert year columns to numeric
ustradebal_raw <- ustradebal_raw %>%
  mutate(across(-country, ~as.numeric(.)))

write_csv(ustradebal_raw, "data/clean/ustradebal_cleaned.csv")