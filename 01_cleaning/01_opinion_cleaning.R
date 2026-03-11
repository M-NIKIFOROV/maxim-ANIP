# 01_opinion_cleaning.R
# Cleans and loads opinion data, saves as RDS

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

source("01_cleaning/definitions.R")

# Load raw opinion data (update the path/filename as needed)
opinion_raw <- read_csv("data/raw/opinion_raw.csv", show_col_types = FALSE)

# Example cleaning steps (customize as needed)
opinion_clean <- opinion_raw %>%
  mutate(across(where(is.character), str_trim)) %>%
  rename_with(tolower) %>%
  # Add more cleaning steps as needed
  filter(!is.na(country))

# Save cleaned data as RDS
saveRDS(opinion_clean, file = "data/clean/opinion_cleaned.rds")

# Print a message
cat("Opinion data cleaned and saved to data/clean/opinion_cleaned.rds\n")
