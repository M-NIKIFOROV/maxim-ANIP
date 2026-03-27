# 01_EUapproval_cleaning.R
#
# Cleans EUapproval(gallup).csv:
# - Loads with no column names
# - Removes first 7 rows (metadata) and first (empty) column
# - Promotes row 1 as column names (lowercase)
# - Filters to EU countries using eu vector from definitions.R
# - Keeps geography, time, approve
# - Pivots wider: years as columns, countries as rows

library(readr)
library(dplyr)
library(tidyr)

source("01_cleaning/definitions.R")

# (a) Load with no column names
raw <- read_csv(
  "data/raw/csv/EUapproval(gallup).csv",
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
euapproval_clean <- raw %>%
  mutate(geography = tolower(geography)) %>%
  filter(geography %in% eu) %>%
  # (d) Keep only geography, time, approve, disapprove, and dk/rf
  select(geography, time, approve, disapprove, "dk/rf") %>%
  rename(country = geography) %>%
  rename(dk_rf = "dk/rf") %>%
  # (e) Pivot wider: years as columns, countries as rows
  pivot_wider(names_from = time, values_from = c(approve, disapprove, dk_rf))

euapprovalc <- unique(euapproval_clean$country)

write_csv(euapproval_clean, "data/clean/EUapproval_cleaned.csv", na = "")
