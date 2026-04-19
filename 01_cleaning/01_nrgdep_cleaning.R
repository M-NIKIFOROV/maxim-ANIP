# 01_nrgdep_cleaning.R
#
# Cleans nrgdep(eurostat).csv:
# - Uses the TIME row as the header
# - Removes the first 11 rows except the TIME header row
# - Removes the last 4 footer rows
# - Lowercases country names
# - Keeps EU countries only
# - Parses year columns as numeric

library(readr)
library(dplyr)

source("01_cleaning/definitions.R")

clean_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  trimws(x)
}

clean_numeric <- function(x) {
  x <- clean_text(x)
  suppressWarnings(readr::parse_number(x, na = c("", ":", "na", "n/a", "null")))
}

nrgdep_raw <- read_csv(
  "data/raw/csv/nrgdep(eurostat).csv",
  col_names = FALSE,
  col_types = cols(.default = "c"),
  show_col_types = FALSE,
  name_repair = "minimal"
)

header_row <- unlist(nrgdep_raw[9, ], use.names = FALSE)
keep_cols <- !is.na(header_row) & trimws(header_row) != ""

nrgdep_clean <- nrgdep_raw %>%
  slice(12:(n() - 4)) %>%
  select(which(keep_cols))

names(nrgdep_clean) <- tolower(trimws(header_row[keep_cols]))
names(nrgdep_clean)[1] <- "country"

nrgdep_clean <- nrgdep_clean %>%
  mutate(across(where(is.character), clean_text)) %>%
  mutate(country = tolower(country)) %>%
  filter(!is.na(country), country != "") %>%
  filter(country %in% eu) %>%
  mutate(across(matches("^[0-9]{4}$"), ~ round(clean_numeric(.x), 3)))

nrgdepc <- sort(unique(nrgdep_clean$country))
nrgdep_overlap_n <- length(nrgdepc)

nrgdep_output <- nrgdep_clean %>%
  mutate(across(matches("^[0-9]{4}$"), ~ formatC(.x, format = "f", digits = 3)))

write_csv(nrgdep_output, "data/clean/nrgdep_cleaned.csv", na = "")

# EU overlap: 27 usable countries.
