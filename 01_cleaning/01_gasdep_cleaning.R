# 01_gasdep_cleaning.R
#
# Cleans gasdep(IEA).csv:
# - Skips first (copyright) row; row 2 becomes header
# - Lowercases column names
# - Filters to EU countries using eu vector from definitions.R
# - Filters to Natural Gas / Imports (PJ) only
# - Removes '2024 provisional', nocountry, noproduct, noflow columns

library(readr)
library(dplyr)

source("01_cleaning/definitions.R")

# (a) Read without first row
gasdep_raw <- read_csv(
  "data/raw/csv/gasdep(IEA).csv",
  skip = 1,
  show_col_types = FALSE
)

# Lowercase column names
gasdep_raw <- rename_with(gasdep_raw, tolower)

# (b) Filter to EU countries
gasdep_raw <- gasdep_raw %>%
  mutate(country = tolower(country)) %>%
  filter(country %in% eu)

# (c) Filter by product and flow
gasdep_raw <- gasdep_raw %>%
  filter(tolower(product) == "natural gas",
         tolower(flow) == "imports (pj)")

# (d) Remove '2024 provisional' column
gasdep_raw <- gasdep_raw %>%
  select(-`2024 provisional`)

# (e) Remove nocountry, noproduct, noflow
gasdep_raw <- gasdep_raw %>%
  select(-nocountry, -noproduct, -noflow)

gasdepc <- unique(gasdep_raw$country)

write_csv(gasdep_raw, "data/clean/gasdep_cleaned.csv", na = "")

# Has years 1971-2023. Missing Croatia, Bulgaria, Cyprus, Czechia, Malta, Romania, Slovakia.