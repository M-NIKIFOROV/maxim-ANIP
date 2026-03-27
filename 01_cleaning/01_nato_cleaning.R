# 01_nato_cleaning.R
#
# Generates a panel of EU countries (1980-2025) with binary NATO membership variable and exports to data/clean/nato_cleaned.csv

library(readr)
library(dplyr)
library(tidyr)

# Source definitions
source("01_cleaning/definitions.R")

# NATO accession years for EU countries
nato_join_year <- c(
  austria = NA_integer_,
  belgium = 1949L,
  bulgaria = 2004L,
  croatia = 2009L,
  cyprus = NA_integer_,
  czechia = 1999L,
  denmark = 1949L,
  estonia = 2004L,
  finland = 2023L,
  france = 1949L,
  germany = 1955L,
  greece = 1952L,
  hungary = 1999L,
  ireland = NA_integer_,
  italy = 1949L,
  latvia = 2004L,
  lithuania = 2004L,
  luxembourg = 1949L,
  malta = NA_integer_,
  netherlands = 1949L,
  poland = 1999L,
  portugal = 1949L,
  romania = 2004L,
  slovakia = 2004L,
  slovenia = 2004L,
  spain = 1982L,
  sweden = 2024L
)

# Create panel
nato_panel <- expand.grid(
  country = eu,
  year = 1980:2025,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
nato_panel$join_year <- nato_join_year[nato_panel$country]
nato_panel$nato_member <- ifelse(!is.na(nato_panel$join_year) & nato_panel$year >= nato_panel$join_year, 1L, 0L)
nato_panel$join_year <- NULL


# Ensure countries never in NATO have all zeros (not NA)
never_nato <- is.na(nato_join_year[nato_panel$country])
nato_panel$nato_member[never_nato] <- 0L
nato_panel <- nato_panel[order(nato_panel$country, nato_panel$year), ]

# Pivot to wide format: one row per country, columns for each year

nato_wide <- pivot_wider(
  nato_panel,
  id_cols = country,
  names_from = year,
  values_from = nato_member
)

natoc <- unique(nato_wide$country)

# Export
write_csv(nato_wide, "data/clean/nato_cleaned.csv", na = "")
