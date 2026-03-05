# 01_gasdep_cleaning.R
#
# Cleans gasdep(eurostat).csv:
# - Splits first column by comma (same as energy cleaning)
# - Renames geo\TIME_PERIOD to country, lowercases values
# - Maps ISO codes to country names using eu/iso from definitions.R
# - Filters to EU countries only
# - Uses same column names for split as energy datasets

library(readr)
library(dplyr)
library(stringr)

source("01_cleaning/definitions.R")

split_first_column <- function(df) {
  split_names <- c("frequency", "balance", "type", "unit", "country")
  split_matrix <- do.call(rbind, strsplit(as.character(df[[1]]), ",", fixed = TRUE))
  split_df <- as.data.frame(split_matrix, stringsAsFactors = FALSE)
  names(split_df) <- split_names
  split_df[] <- lapply(split_df, trimws)
  split_df[] <- lapply(split_df, tolower)
  cbind(split_df, df[-1])
}

map_and_filter_countries <- function(df) {
  if (!"country" %in% names(df)) return(df)
  country_lower <- tolower(df$country)
  matched <- country_lower %in% names(iso)
  df$country[matched] <- iso[country_lower[matched]]
  df$country <- tolower(df$country)
  df[df$country %in% eu, ]
}

# Read data
gasdep_raw <- read_csv("data/raw/csv/gasdep(eurostat).csv", show_col_types = FALSE)

# Split first column
gasdep_raw <- split_first_column(gasdep_raw)

# geo\TIME_PERIOD is now handled in split_first_column as 'country', so just ensure lowercase
gasdep_raw$country <- tolower(gasdep_raw$country)



# Keep only rows where type is 'n900h' or 'total', and balance is 'imp'
gasdep_raw <- gasdep_raw %>% filter(type %in% c("n900h", "total"), balance == "imp")

# Map ISO to country names and filter to EU
gasdep_raw <- map_and_filter_countries(gasdep_raw)

# Write cleaned output
write_csv(gasdep_raw, "data/clean/gasdep_cleaned.csv", na = "")
