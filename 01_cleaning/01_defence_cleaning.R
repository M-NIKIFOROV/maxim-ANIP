# 01_defence_cleaning.R
#
# Script to clean the defence(SIPRI) dataset
#
# a) Load in without colnames
# b) Delete rows 1:5, 7
# c) Remove cols 2:3
# d) Change all to lowercase
# e) Make the first row the colnames
# f) Make all cols starting with 2 an integer
# g) Remove columns called 1949 to 1979
# h) Check for missing countries (as in other scripts)
# i) Remove countries not needed

source("01_cleaning/definitions.R")

library(readr)
library(dplyr)

# Define file path
def_file <- "data/raw/csv/defence(SIPRI).csv"

# a) Load in without colnames
defence_raw <- read_csv(def_file, col_names = FALSE, col_types = cols(.default = "c"))

# b) Delete rows 1:5, 7 (after previous removal, row 7 is now row 6)
rows_to_remove <- c(1:5, 7)
defence_raw <- defence_raw[-rows_to_remove, ]

# c) Remove cols 2:3
defence_raw <- defence_raw[, -c(2,3)]

# e) Make the first row the colnames
colnames(defence_raw) <- as.character(defence_raw[1, ])
defence_raw <- defence_raw[-1, ]

# f) Make all cols starting with 2 an integer
if (ncol(defence_raw) > 1) {
  data_cols <- names(defence_raw)[2:ncol(defence_raw)]
  defence_raw <- defence_raw %>% mutate(across(all_of(data_cols), ~as.integer(.)))
}

# g) Remove columns called 1949 to 1979
cols_to_remove <- as.character(1949:1979)
defence_raw <- defence_raw %>% select(-any_of(cols_to_remove))

# d) Change all character columns to lowercase (handle NA and encoding issues)
char_cols <- sapply(defence_raw, is.character)
defence_raw[char_cols] <- lapply(defence_raw[char_cols], function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "?")
  tolower(x)
})

# h) Check for missing countries (as in other scripts)
country_col <- which(tolower(names(defence_raw)) %in% c('country', 'country_name', 'name'))
if (length(country_col) == 1) {
  countries <- defence_raw[[country_col]]
  missing_eu_countries <- setdiff(eu, countries)
  cat("EU countries missing from defence data:\n")
  print(missing_eu_countries)
} # none missing

# i) Remove countries not needed (keep only those in eu)
if (length(country_col) == 1) {
  defence_raw <- defence_raw %>% filter(.data[[names(defence_raw)[country_col]]] %in% eu)
}

write_csv(defence_raw, "data/clean/defence_cleaned.csv")
