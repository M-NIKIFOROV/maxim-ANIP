# 01_debtgdp_cleaning.R
#
# Script to clean the debtgdp(imf) dataset
#
# a) Load IMF CSV
# b) Remove blank separator rows
# c) Rename first column to country
# d) Standardize country names to eu vector style
# e) Convert year columns to numeric ("no data" -> NA)
# f) Keep years 1980-2024
# g) Keep only EU countries

source("01_cleaning/definitions.R")

library(readr)
library(dplyr)

# Define file path
debtgdp_file <- "data/raw/csv/debtgdp(imf).csv"

# a) Load IMF CSV (first row already contains headers)
debtgdp_raw <- read_csv(
  debtgdp_file,
  col_types = cols(.default = "c"),
  show_col_types = FALSE
)

# b) Remove blank separator rows and lowercase character columns
char_cols <- sapply(debtgdp_raw, is.character)
debtgdp_raw[char_cols] <- lapply(debtgdp_raw[char_cols], function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  tolower(trimws(x))
})

debtgdp_raw <- debtgdp_raw %>%
  filter(.data[[names(debtgdp_raw)[1]]] != "")

# c) Rename first column to country
names(debtgdp_raw)[1] <- "country"

# d) Standardize country names to match eu vector
country_name_map <- c(
  "czech republic" = "czechia",
  "slovak republic" = "slovakia"
)

debtgdp_raw <- debtgdp_raw %>%
  mutate(country = recode(country, !!!country_name_map, .default = country))

# e) Convert year columns to numeric ("no data" and blank -> NA)
year_cols <- intersect(as.character(1950:2024), names(debtgdp_raw))

debtgdp_raw <- debtgdp_raw %>%
  mutate(across(all_of(year_cols), ~ na_if(., "no data"))) %>%
  mutate(across(all_of(year_cols), ~ na_if(., ""))) %>%
  mutate(across(all_of(year_cols), as.numeric))

# f) Keep years 1980-2024
keep_cols <- c("country", as.character(1980:2024))
debtgdp_raw <- debtgdp_raw %>% select(any_of(keep_cols))

# g) Keep only EU countries
debtgdp_raw <- debtgdp_raw %>%
  filter(country %in% eu)

# Optional check: missing EU countries
missing_eu_countries <- setdiff(eu, debtgdp_raw$country)
cat("EU countries missing from debtgdp(imf) data:\n")
print(missing_eu_countries)

debtgdpc <- unique(debtgdp_raw$country)

write_csv(debtgdp_raw, "data/clean/debtgdp_cleaned.csv")

# Has years 1980-2024. Missing none.