# 01_hicp_cleaning.R
#
# Cleans hcip(Eurostat).csv (HICP annual average index, 2015=100, all-items CP00):
# - Finds TIME row and uses it as header
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

hicp_raw <- read_csv(
  "data/raw/csv/hcip(Eurostat).csv",
  col_names = FALSE,
  col_types = cols(.default = "c"),
  show_col_types = FALSE,
  name_repair = "minimal"
)

first_col <- clean_text(hicp_raw[[1]])
time_row  <- which(tolower(first_col) == "time")[1]
geo_row   <- which(tolower(first_col) == "geo (labels)")[1]

if (is.na(time_row) || is.na(geo_row)) {
  stop("Could not find TIME/GEO rows in hcip(Eurostat).csv")
}

header_row <- unlist(hicp_raw[time_row, ], use.names = FALSE)
keep_cols  <- !is.na(header_row) & clean_text(header_row) != ""

data_start <- geo_row + 1
footer_idx <- which(tolower(first_col) %in% c("special value", "observation flags:"))
data_end   <- if (length(footer_idx) > 0) min(footer_idx) - 1 else nrow(hicp_raw)

hicp_clean <- hicp_raw %>%
  slice(data_start:data_end) %>%
  select(which(keep_cols))

names(hicp_clean) <- tolower(clean_text(header_row[keep_cols]))
names(hicp_clean)[1] <- "country"

hicp_name_map <- c(
  "czechia (czech republic)" = "czechia",
  "czech republic"           = "czechia",
  "slovak republic"          = "slovakia"
)

hicp_clean <- hicp_clean %>%
  mutate(across(where(is.character), clean_text)) %>%
  mutate(country = tolower(country)) %>%
  mutate(country = recode(country, !!!hicp_name_map)) %>%
  filter(!is.na(country), country != "") %>%
  filter(country %in% eu) %>%
  mutate(across(matches("^[0-9]{4}$"), ~ round(clean_numeric(.x), 4)))

hicpc <- sort(unique(hicp_clean$country))

write_csv(hicp_clean, "data/clean/hicp_cleaned.csv", na = "")
