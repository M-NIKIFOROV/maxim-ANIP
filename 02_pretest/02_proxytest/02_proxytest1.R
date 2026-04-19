# 02_proxytest1.R
#
# Auxiliary Model 2 (Political Stress) proxy test.
# Dependent variable is political stress proxy, estimated separately for:
# 1) EU approval disapprove share
# 2) Government approval no share
# 3) Military confidence no share

library(readr)
library(dplyr)
library(tidyr)
library(fixest)

source("01_cleaning/definitions.R")

pivot_year_values <- function(df, value_name) {
  df %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = value_name
    ) %>%
    mutate(year = as.integer(year))
}

build_core_panel <- function() {
  panel_energy <- read_csv("data/clean/energy_cleaned.csv", show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    filter(country %in% eu, currency == "eur", tax == "i_tax") %>%
    pivot_year_values("energy_price") %>%
    mutate(energy_price = as.numeric(energy_price)) %>%
    group_by(country, year) %>%
    summarise(energy_price = mean(energy_price, na.rm = TRUE), .groups = "drop") %>%
    mutate(energy_price = if_else(is.nan(energy_price), NA_real_, energy_price))

  panel_gasdep <- read_csv("data/clean/gasdep_cleaned.csv", show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    select(country, matches("^[0-9]{4}$")) %>%
    mutate(across(matches("^[0-9]{4}$"), as.character)) %>%
    pivot_year_values("gasdep") %>%
    mutate(
      gasdep = na_if(gasdep, ".."),
      gasdep = na_if(gasdep, ""),
      gasdep = as.numeric(gasdep)
    ) %>%
    select(country, year, gasdep)

  panel_gdp <- read_csv("data/clean/gdppc_cleaned.csv", show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    pivot_year_values("gdp_pc") %>%
    mutate(gdp_pc = as.numeric(gdp_pc)) %>%
    select(country, year, gdp_pc)

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

  panel_area <- read_csv("data/clean/area_cleaned.csv", show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    pivot_year_values("area") %>%
    mutate(area = as.numeric(area)) %>%
    select(country, year, area)

  panel_nato <- read_csv("data/clean/nato_cleaned.csv", show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    pivot_year_values("nato") %>%
    mutate(nato = as.numeric(nato)) %>%
    select(country, year, nato)

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

  panel_energy %>%
    left_join(panel_gasdep, by = c("country", "year")) %>%
    left_join(panel_gdp, by = c("country", "year")) %>%
    left_join(panel_threat, by = c("country", "year")) %>%
    left_join(panel_area, by = c("country", "year")) %>%
    left_join(panel_nato, by = c("country", "year")) %>%
    left_join(panel_ideology, by = c("country", "year")) %>%
    left_join(panel_seats, by = c("country", "year")) %>%
    filter(country %in% eu) %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      energy_lag = dplyr::lag(energy_price, 1),
      ideol_seats = ideology * seats
    ) %>%
    ungroup()
}

build_proxy_panel <- function(file_path, prefix) {
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    pivot_longer(
      cols = starts_with(prefix),
      names_to = "year",
      values_to = "political_stress",
      names_prefix = prefix
    ) %>%
    mutate(
      year = as.integer(year),
      political_stress = as.numeric(political_stress)
    ) %>%
    select(country, year, political_stress)
}

run_aux2_model <- function(core_panel, proxy_df, proxy_label) {
  model_data <- core_panel %>%
    left_join(proxy_df, by = c("country", "year")) %>%
    mutate(
      log_energy_lag = if_else(!is.na(energy_lag) & energy_lag > 0, log(energy_lag), NA_real_),
      log1p_gasdep = log1p(gasdep),
      energy_gasdep_int = log_energy_lag * log1p_gasdep,
      log_gdp_pc = if_else(!is.na(gdp_pc) & gdp_pc > 0, log(gdp_pc), NA_real_),
      log_area = if_else(!is.na(area) & area > 0, log(area), NA_real_)
    ) %>%
    filter(
      !is.na(political_stress),
      !is.na(log_energy_lag),
      !is.na(log1p_gasdep),
      !is.na(energy_gasdep_int),
      !is.na(log_gdp_pc),
      !is.na(threat),
      !is.na(log_area),
      !is.na(nato),
      !is.na(ideology),
      !is.na(seats),
      !is.na(ideol_seats)
    )

  model <- feols(
    political_stress ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats,
    data = model_data,
    fixef = c("country", "year"),
    cluster = "country"
  )

  cat("\n====================================================\n")
  cat("AUX MODEL 2 PROXY:", proxy_label, "\n")
  cat("====================================================\n")
  cat("Countries:", length(unique(model_data$country)), "\n")
  cat("Observations:", nrow(model_data), "\n\n")
  print(summary(model))

  coef_tbl <- as.data.frame(coeftable(model))
  coef_tbl$term <- rownames(coef_tbl)
  coef_tbl <- coef_tbl %>% select(term, everything())

  cat("\nKey terms (energy, gasdep, interaction):\n")
  print(coef_tbl %>% filter(term %in% c("log_energy_lag", "log1p_gasdep", "energy_gasdep_int")))

  invisible(list(model = model, data = model_data, coef_tbl = coef_tbl))
}

core_panel <- build_core_panel()

proxy_euapproval <- build_proxy_panel("data/clean/EUapproval_cleaned.csv", "disapprove_")
proxy_govt <- build_proxy_panel("data/clean/govtapproval_cleaned.csv", "no_")
proxy_military <- build_proxy_panel("data/clean/militaryconfidence_cleaned.csv", "no_")

results <- list(
  euapproval_disapprove = run_aux2_model(core_panel, proxy_euapproval, "EUAPPROVAL_DISAPPROVE"),
  govtapproval_no = run_aux2_model(core_panel, proxy_govt, "GOVTAPPROVAL_NO"),
  militaryconfidence_no = run_aux2_model(core_panel, proxy_military, "MILITARYCONFIDENCE_NO")
)

invisible(results)
