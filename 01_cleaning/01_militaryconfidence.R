# 01_militaryconfidence_cleaning.R
#
# Cleans militaryconfidence(gallup).csv:
# - Loads with no column names
# - Removes first 7 rows (metadata) and first (empty) column
# - Promotes row 1 as column names (lowercase)
# - Filters to EU countries using eu vector from definitions.R
# - Keeps yes, no, dk/rf
# - Pivots wider: years as columns, countries as rows

library(readr)
library(dplyr)
library(tidyr)

source("01_cleaning/definitions.R")

# (a) Load with no column names
raw <- read_csv(
  "data/raw/csv/militaryconfidence(gallup).csv",
  col_names = FALSE,
  show_col_types = FALSE
)

# (b) Remove first 7 rows and first column
raw <- raw %>%
  slice(-(1:7)) %>%
  select(-1)

# Promote row 1 as column names (lowercase), then drop that row
colnames(raw) <- raw[1, ] %>% unlist() %>% tolower()
raw <- raw[-1, ]

# Drop trailing columns with blank or NA names
raw <- raw[, nchar(colnames(raw)) > 0 & !is.na(colnames(raw))]

# (c) Filter to EU countries
militaryconfidence_clean <- raw %>%
  mutate(geography = tolower(geography)) %>%
  filter(geography %in% eu) %>%
  # (d) Keep only geography, time, yes, no, and dk/rf
  select(geography, time, yes, no, "dk/rf") %>%
  rename(country = geography) %>%
  rename(dk_rf = "dk/rf") %>%
  # (e) Pivot wider: years as columns, countries as rows
  pivot_wider(names_from = time, values_from = c(yes, no, dk_rf))

militaryconfidencec <- unique(militaryconfidence_clean$country)

write_csv(militaryconfidence_clean, "data/clean/militaryconfidence_cleaned.csv", na = "")

# Has years 2006-2023. Missing Czechia and Netherlands.