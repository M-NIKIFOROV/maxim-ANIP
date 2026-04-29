# 01_eubrent_cleaning.R
#
# Cleans eubrent(FRED).csv:
# - observation_date -> year (integer)
# - DCOILBRENTEU -> price (numeric)

library(readr)
library(dplyr)

eubrent_raw <- read_csv(
  "data/raw/csv/eubrent(FRED).csv",
  col_types = cols(
    observation_date = col_character(),
    DCOILBRENTEU = col_character()
  ),
  show_col_types = FALSE
)

eubrent_clean <- eubrent_raw %>%
  transmute(
    year = as.integer(substr(trimws(observation_date), 1, 4)),
    price = readr::parse_number(DCOILBRENTEU, na = c("", "NA", "."))
  ) %>%
  mutate(price = round(price, 2)) %>%
  filter(!is.na(year), !is.na(price)) %>%
  arrange(year)

write_csv(eubrent_clean, "data/clean/eubrent_cleaned.csv", na = "")
