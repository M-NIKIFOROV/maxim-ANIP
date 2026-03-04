# 01_ideology_cleaned.R
#
# Converts election-level ideology data into annual government data:
# - Keep EU countries only
# - Treat each election as start of new government interval
# - Compute overlap days by calendar year
# - Apply 6-month (>=183 days) rule, else choose max overlap government
# - Output tidy panel: country, year, ideology, seats

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Source shared definitions
source("01_cleaning/definitions.R")

# Read and pre-clean
ideology_raw <- read_csv("data/raw/csv/ideology(parlgov).csv", show_col_types = FALSE)

ideology_raw <- ideology_raw %>%
  rename(country = country_name) %>%
  mutate(country = tolower(country)) %>%
  filter(country %in% eu, election_type == "parliament") %>%
  mutate(
    election_date = as.Date(election_date),
    left_right = as.numeric(left_right),
    seats = as.numeric(seats),
    vote_share = as.numeric(vote_share)
  )

# Collapse party rows to one row per election (largest-seat party as governing record)
gov_elections <- ideology_raw %>%
  filter(!is.na(election_date)) %>%
  group_by(country, election_id, election_date) %>%
  arrange(desc(seats), desc(vote_share), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(country, election_date, ideology = left_right, seats)


# Build government intervals by country
gov_intervals <- gov_elections %>%
  arrange(country, election_date) %>%
  group_by(country) %>%
  mutate(
    gov_start = election_date,
    gov_end = lead(election_date) - days(1),
    last_year = max(year(election_date), na.rm = TRUE),
    gov_end = if_else(is.na(gov_end), ymd(paste0(last_year, "-12-31")), gov_end)
  ) %>%
  ungroup() %>%
  select(country, gov_start, gov_end, ideology, seats) %>%
  filter(!is.na(gov_start) & !is.na(gov_end) & gov_end >= gov_start)

# Expand each government interval to overlapping years and compute overlap days
gov_year_overlaps <- gov_intervals %>%
  rowwise() %>%
  reframe(
    country = country,
    year = seq(year(gov_start), year(gov_end)),
    gov_start = gov_start,
    gov_end = gov_end,
    ideology = ideology,
    seats = seats
  ) %>%
  ungroup() %>%
  mutate(
    year_start = ymd(paste0(year, "-01-01")),
    year_end = ymd(paste0(year, "-12-31")),
    overlap_start = pmax(gov_start, year_start),
    overlap_end = pmin(gov_end, year_end),
    days_in_year = as.integer(overlap_end - overlap_start) + 1L
  ) %>%
  filter(days_in_year > 0)

# Apply 6-month rule; if none >=183 days, take largest overlap
ideology_annual <- gov_year_overlaps %>%
  group_by(country, year) %>%
  arrange(desc(days_in_year >= 183L), desc(days_in_year), gov_start, .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(country, year, ideology, seats)

ideology_annual <- pivot_wider(
    ideology_annual,
    id_cols = country,
    names_from = year,
    values_from = c(ideology, seats),
    names_sep = "_"
)

# Export
write_csv(ideology_annual, "data/clean/ideology_cleaned.csv", na = "")

