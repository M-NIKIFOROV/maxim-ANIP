# 01_gdppc_cleaning.R
#
# Script to clean the gdppc(worldbank) dataset
#
# a) Load in without colnames
# b) Delete rows 1:3
# d) Change all to lowercase
# e) Make the first row the colnames
# f) Make all cols starting from col 5 an integer
# g) Remove columns called 1960 to 1979, and 2025
# h) Check for missing countries (as in other scripts)
# i) Remove countries not needed

source("01_cleaning/definitions.R")

library(readr)
library(dplyr)

# Define file path
gdppc_file <- "data/raw/csv/gdppc(correct).csv"

# a) Load in without colnames
gdppc_raw <- read_csv(gdppc_file, col_names = FALSE, col_types = cols(.default = "c"))

# b) Delete rows 1:3
gdppc_raw <- gdppc_raw[-c(1:3), ]

# d) Change all character columns to lowercase (handle NA and encoding issues)
char_cols <- sapply(gdppc_raw, is.character)
gdppc_raw[char_cols] <- lapply(gdppc_raw[char_cols], function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "?")
  tolower(x)
})

# e) Make the first row the colnames
colnames(gdppc_raw) <- as.character(gdppc_raw[1, ])
gdppc_raw <- gdppc_raw[-1, ]

# f) Make all cols starting from col 5 an integer
if (ncol(gdppc_raw) >= 5) {
  data_cols <- names(gdppc_raw)[5:ncol(gdppc_raw)]
  gdppc_raw <- gdppc_raw %>% mutate(across(all_of(data_cols), ~as.numeric(.)))
}

# g) Remove columns called 1960 to 1979, and 2025
cols_to_remove <- c(as.character(1960:1979), "2025")
gdppc_raw <- gdppc_raw %>% select(-any_of(cols_to_remove))

# Rename 'country name' column to 'country' if present (case-insensitive)
name_col <- which(tolower(names(gdppc_raw)) == 'country name')
if (length(name_col) == 1) {
  names(gdppc_raw)[name_col] <- 'country'
}

# Recode country names to match eu vector
gdppc_name_map <- c(
  "czech republic" = "czechia",
  "slovak republic" = "slovakia"
)
gdppc_raw <- gdppc_raw %>%
  mutate(country = recode(country, !!!gdppc_name_map))

# h) Check for missing countries (as in other scripts)
country_col <- which(tolower(names(gdppc_raw)) %in% c('country', 'country_name', 'name'))
if (length(country_col) == 1) {
  countries <- gdppc_raw[[country_col]]
  missing_eu_countries <- setdiff(eu, countries)
  cat("EU countries missing from gdppc data:\n")
  print(missing_eu_countries)
} # missing one: slovakia

# i) Remove countries not needed (keep only those in eu)
if (length(country_col) == 1) {
  gdppc_raw <- gdppc_raw %>% filter(.data[[names(gdppc_raw)[country_col]]] %in% eu)
}

gdppcc <- unique(gdppc_raw$country)

write_csv(gdppc_raw, "data/clean/gdppc_cleaned.csv")

# Has years 1980-2024. Missing Slovakia.