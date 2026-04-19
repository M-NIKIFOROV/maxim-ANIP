# 01_defencegdp_cleaning.R
#
# Cleans defencegdp(SIPRI).csv:
# - Removes Series Name, Series Code, Country Code
# - Renames Country Name -> country
# - Removes " [YR####]" from year column names
# - Lowercases country names
# - Keeps only EU countries

library(readr)
library(dplyr)

source("01_cleaning/definitions.R")

clean_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  tolower(trimws(x))
}

defencegdp_raw <- read_csv(
  "data/raw/csv/defencegdp(SIPRI).csv",
  show_col_types = FALSE,
  col_types = cols(.default = "c")
)

defencegdp_raw <- defencegdp_raw %>%
  rename_with(tolower) %>%
  select(-`series name`, -`series code`, -`country code`) %>%
  rename(country = `country name`)

names(defencegdp_raw) <- gsub("\\s*\\[yr[0-9]{4}\\]$", "", names(defencegdp_raw))

defencegdp_clean <- defencegdp_raw %>%
  mutate(country = clean_text(country)) %>%
  filter(country %in% eu) %>%
  mutate(across(matches("^[0-9]{4}$"), ~ suppressWarnings(as.numeric(.x))))

defencegdpc <- sort(unique(defencegdp_clean$country))
defencegdp_overlap_n <- length(defencegdpc)

write_csv(defencegdp_clean, "data/clean/defencegdp_cleaned.csv", na = "")

# EU overlap count is stored in defencegdp_overlap_n.
