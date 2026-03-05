# 02_pretest1.R
#
# Goal: estimate the effect of lagged energy prices on defence spending
# with country and year fixed effects.

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(fixest)

source("01_cleaning/definitions.R")

# Helper: pivot year columns like 1980, 1981, ... to long
pivot_year_values <- function(df, value_name) {
  df %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = value_name
    ) %>%
    mutate(year = as.integer(year))
}

# Defence spending
panel_defence <- read_csv("data/clean/defence_cleaned.csv", show_col_types = FALSE) %>%
  rename(country = Country) %>%
  mutate(country = tolower(country)) %>%
  pivot_year_values("def_spend") %>%
  mutate(def_spend = as.numeric(def_spend)) %>%
  select(country, year, def_spend)

# Energy prices (already partially long; pivot year columns to long)
panel_energy <- read_csv("data/clean/energy_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  filter(country %in% eu) %>%
  pivot_year_values("energy_price") %>%
  mutate(energy_price = as.numeric(energy_price)) %>%
  group_by(country, year) %>%
  summarise(energy_price = mean(energy_price, na.rm = TRUE), .groups = "drop") %>%
  mutate(energy_price = if_else(is.nan(energy_price), NA_real_, energy_price))

# GDP per capita
panel_gdp <- read_csv("data/clean/gdppc_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_year_values("gdp_pc") %>%
  mutate(gdp_pc = as.numeric(gdp_pc)) %>%
  select(country, year, gdp_pc)

# Area
panel_area <- read_csv("data/clean/area_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_year_values("area") %>%
  mutate(area = as.numeric(area)) %>%
  select(country, year, area)

# NATO membership
panel_nato <- read_csv("data/clean/nato_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_year_values("nato") %>%
  mutate(nato = as.numeric(nato)) %>%
  select(country, year, nato)

# Ideology (wide columns are ideology_YYYY and seats_YYYY)
panel_ideology <- read_csv("data/clean/ideology_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_longer(
    cols = starts_with("ideology_"),
    names_to = "year",
    values_to = "ideology",
    names_prefix = "ideology_"
  ) %>%
  mutate(year = as.integer(year), ideology = as.numeric(ideology)) %>%
  select(country, year, ideology)

# Seats (wide columns are seats_YYYY)
panel_seats <- read_csv("data/clean/ideology_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_longer(
    cols = starts_with("seats_"),
    names_to = "year",
    values_to = "seats",
    names_prefix = "seats_"
  ) %>%
  mutate(year = as.integer(year), seats = as.numeric(seats)) %>%
  select(country, year, seats)

# Threat (wide columns are dist_border_km_YYYY and dist_capital_km_YYYY)
panel_threat <- read_csv("data/clean/threat_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_longer(
    cols = starts_with("dist_border_km_"),
    names_to = "year",
    values_to = "threat",
    names_prefix = "dist_border_km_"
  ) %>%
  mutate(year = as.integer(year), threat = as.numeric(threat)) %>%
  select(country, year, threat)

# Build final panel and create lagged energy variable
panel <- panel_defence %>%
  left_join(panel_energy, by = c("country", "year")) %>%
  left_join(panel_gdp, by = c("country", "year")) %>%
  left_join(panel_threat, by = c("country", "year")) %>%
  left_join(panel_area, by = c("country", "year")) %>%
  left_join(panel_nato, by = c("country", "year")) %>%
  left_join(panel_ideology, by = c("country", "year")) %>%
  left_join(panel_seats, by = c("country", "year")) %>%
  filter(country %in% eu) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(energy_lag = dplyr::lag(energy_price, 1)) %>%
  mutate(ideol_seats = ideology * seats) %>%
  ungroup()

# Keep complete observations for model variables
model_data <- panel %>%
  filter(
    !is.na(def_spend),
    !is.na(energy_lag),
    !is.na(gdp_pc),
    !is.na(threat),
    !is.na(area),
    !is.na(nato),
    !is.na(ideology),
    !is.na(seats),
    !is.na(ideol_seats),
    def_spend > 0,
    energy_lag > 0,
    gdp_pc > 0,
    area > 0
  )

# FE model with clustered SE by country
pretest_model <- feols(
  log(def_spend) ~ log(energy_lag) + log(gdp_pc) + threat + log(area) + nato + ideology + seats + ideol_seats,
  data = model_data,
  fixef = c("country", "year"),
  cluster = "country"
)

# Output: model summary
print(summary(pretest_model))

# Output: coefficient table
coef_table <- as.data.frame(coeftable(pretest_model))
coef_table$term <- rownames(coef_table)
coef_table <- coef_table %>% select(term, everything())
print(coef_table)

# Output: significance of log(energy_lag)
energy_lag_row <- coef_table %>% filter(term == "log(energy_lag)")
print(energy_lag_row)

# Output: R-squared and number of observations
fit_stats <- tibble(
  n_obs = nobs(pretest_model),
  r2 = as.numeric(fitstat(pretest_model, "r2")),
  r2_within = as.numeric(fitstat(pretest_model, "wr2"))
)
print(fit_stats)
