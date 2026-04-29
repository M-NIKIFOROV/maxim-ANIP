# 01_healthspending_cleaning.R
#
# Cleans healthspending(eurostat).csv exported in Eurostat table format:
# - Finds TIME row and uses it as header
# - Removes metadata/header/footer blocks
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

health_raw <- read_csv(
  "data/raw/csv/healthspending(eurostat).csv",
  col_names = FALSE,
  col_types = cols(.default = "c"),
  show_col_types = FALSE,
  name_repair = "minimal"
)

first_col <- clean_text(health_raw[[1]])
time_row <- which(tolower(first_col) == "time")[1]
geo_row <- which(tolower(first_col) == "geo (labels)")[1]

if (is.na(time_row) || is.na(geo_row)) {
  stop("Could not find TIME/GEO rows in healthspending(eurostat).csv")
}

header_row <- unlist(health_raw[time_row, ], use.names = FALSE)
keep_cols <- !is.na(header_row) & clean_text(header_row) != ""

# Start at first row after 'GEO (Labels)' and end before footer sections.
data_start <- geo_row + 1
footer_idx <- which(tolower(first_col) %in% c("special value", "observation flags:"))
data_end <- if (length(footer_idx) > 0) min(footer_idx) - 1 else nrow(health_raw)

health_clean <- health_raw %>%
  slice(data_start:data_end) %>%
  select(which(keep_cols))

names(health_clean) <- tolower(clean_text(header_row[keep_cols]))
names(health_clean)[1] <- "country"

health_clean <- health_clean %>%
  mutate(across(where(is.character), clean_text)) %>%
  mutate(country = tolower(country)) %>%
  filter(!is.na(country), country != "") %>%
  filter(country %in% eu) %>%
  mutate(across(matches("^[0-9]{4}$"), ~ round(clean_numeric(.x), 3)))

healthspendingc <- sort(unique(health_clean$country))

write_csv(health_clean, "data/clean/healthspending_cleaned.csv", na = "")

# Expected years in this extract: 1998-2025.
