suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr)
})
source("01_cleaning/definitions.R")

pivot_yv <- function(df, value_name) {
  df %>%
    pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = value_name) %>%
    mutate(year = as.integer(year))
}

energy_long <- read_csv("data/clean/energy_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_yv("energy_price") %>%
  mutate(energy_price = as.numeric(energy_price)) %>%
  filter(country %in% eu) %>%
  group_by(country, year) %>%
  summarise(energy_price = mean(energy_price, na.rm = TRUE), .groups = "drop") %>%
  mutate(energy_price = if_else(is.nan(energy_price), NA_real_, energy_price))

cat("=== Cross-country variation by year (2020+) ===\n")
energy_long %>%
  filter(year >= 2020) %>%
  group_by(year) %>%
  summarise(
    n   = sum(!is.na(energy_price)),
    mean = round(mean(energy_price, na.rm=TRUE), 4),
    sd   = round(sd(energy_price, na.rm=TRUE), 4),
    min  = round(min(energy_price, na.rm=TRUE), 4),
    max  = round(max(energy_price, na.rm=TRUE), 4),
    cv   = round(sd(energy_price,na.rm=TRUE)/mean(energy_price,na.rm=TRUE), 3),
    .groups="drop"
  ) %>%
  print(n=10)

for (yr in 2020:2023) {
  cat(sprintf("\n=== %d country values ===\n", yr))
  energy_long %>%
    filter(year == yr, !is.na(energy_price)) %>%
    arrange(desc(energy_price)) %>%
    select(country, energy_price) %>%
    print(n=30)
}
