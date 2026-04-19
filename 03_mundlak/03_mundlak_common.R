# 03_mundlak_common.R
# Shared helpers for Mundlak Type 1 and Type 2 runs across all ANIP models.

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

build_panel <- function() {
  panel_defence <- read_csv("data/clean/defence_cleaned.csv", show_col_types = FALSE) %>%
    rename(country = Country) %>%
    mutate(country = tolower(country)) %>%
    pivot_year_values("def_spend") %>%
    mutate(def_spend = as.numeric(def_spend)) %>%
    select(country, year, def_spend)

  panel_energy <- read_csv("data/clean/energy_cleaned.csv", show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    filter(currency == "eur", tax == "i_tax") %>%
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

  panel_debt <- read_csv("data/clean/debtgdp_cleaned.csv", show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    pivot_year_values("debtgdp") %>%
    mutate(debtgdp = as.numeric(debtgdp)) %>%
    select(country, year, debtgdp)

  panel_political_stress <- read_csv("data/clean/govtapproval_cleaned.csv", show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    pivot_longer(
      cols = starts_with("no_"),
      names_to = "year",
      values_to = "political_stress",
      names_prefix = "no_"
    ) %>%
    mutate(
      year = as.integer(year),
      political_stress = as.numeric(political_stress)
    ) %>%
    select(country, year, political_stress)

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

  panel_defence %>%
    left_join(panel_energy, by = c("country", "year")) %>%
    left_join(panel_gasdep, by = c("country", "year")) %>%
    left_join(panel_gdp, by = c("country", "year")) %>%
    left_join(panel_debt, by = c("country", "year")) %>%
    left_join(panel_political_stress, by = c("country", "year")) %>%
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
      debtgdp_lag = dplyr::lag(debtgdp, 1),
      political_stress_lag = dplyr::lag(political_stress, 1),
      ideol_seats = ideology * seats
    ) %>%
    ungroup()
}

prepare_data <- function(panel, model_name, type_name) {
  data <- panel

  if (type_name == "type1") {
    countries_with_gasdep <- data %>%
      group_by(country) %>%
      summarise(has_gasdep = any(!is.na(gasdep)), .groups = "drop") %>%
      filter(has_gasdep) %>%
      pull(country)

    data <- data %>% filter(country %in% countries_with_gasdep)
  }

  if (type_name == "type2") {
    data <- data %>% mutate(gasdep = if_else(is.na(gasdep), 0, gasdep))
  }

  data <- data %>%
    mutate(
      log_energy_lag = if_else(!is.na(energy_lag) & energy_lag > 0, log(energy_lag), NA_real_),
      log_gdp_pc = if_else(!is.na(gdp_pc) & gdp_pc > 0, log(gdp_pc), NA_real_),
      log_area = if_else(!is.na(area) & area > 0, log(area), NA_real_),
      log_def_spend = if_else(!is.na(def_spend) & def_spend > 0, log(def_spend), NA_real_),
      log1p_gasdep = log1p(gasdep),
      energy_gasdep_int = log_energy_lag * log1p_gasdep
    )

  common_filter <- data %>%
    filter(
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

  if (model_name == "baseline") {
    model_data <- common_filter %>%
      filter(!is.na(log_def_spend), !is.na(debtgdp_lag), !is.na(political_stress_lag))
  } else if (model_name == "aux_fiscal") {
    model_data <- common_filter %>%
      filter(!is.na(debtgdp))
  } else if (model_name == "aux_political") {
    model_data <- common_filter %>%
      filter(!is.na(political_stress))
  } else if (model_name == "main") {
    model_data <- common_filter %>%
      filter(!is.na(log_def_spend), !is.na(debtgdp_lag), !is.na(political_stress_lag))
  } else {
    stop("Unknown model name")
  }

  model_data %>%
    group_by(country) %>%
    mutate(
      mean_log_energy_lag = mean(log_energy_lag, na.rm = TRUE),
      mean_log1p_gasdep = mean(log1p_gasdep, na.rm = TRUE),
      mean_energy_gasdep_int = mean(energy_gasdep_int, na.rm = TRUE),
      mean_log_gdp_pc = mean(log_gdp_pc, na.rm = TRUE),
      mean_threat = mean(threat, na.rm = TRUE),
      mean_log_area = mean(log_area, na.rm = TRUE),
      mean_nato = mean(nato, na.rm = TRUE),
      mean_ideology = mean(ideology, na.rm = TRUE),
      mean_seats = mean(seats, na.rm = TRUE),
      mean_ideol_seats = mean(ideol_seats, na.rm = TRUE),
      mean_debtgdp_lag = mean(debtgdp_lag, na.rm = TRUE),
      mean_political_stress_lag = mean(political_stress_lag, na.rm = TRUE)
    ) %>%
    ungroup()
}

run_one_model <- function(panel, type_name, model_name, header_text) {
  model_data <- prepare_data(panel, model_name, type_name)

  if (model_name == "baseline") {
    fml <- log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year
  } else if (model_name == "aux_fiscal") {
    fml <- debtgdp ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year
  } else if (model_name == "aux_political") {
    fml <- political_stress ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year
  } else if (model_name == "main") {
    fml <- log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      debtgdp_lag + political_stress_lag +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_debtgdp_lag + mean_political_stress_lag +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year
  } else {
    stop("Unknown model name")
  }

  model <- feols(fml, data = model_data, cluster = "country")
  mundlak_test <- wald(model, keep = "^mean_")

  cat("\n====================================================\n")
  cat(header_text, "\n")
  cat("====================================================\n")
  cat("Type:", type_name, "\n")
  cat("Countries:", length(unique(model_data$country)), "\n")
  cat("Observations:", nrow(model_data), "\n\n")
  print(summary(model))
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(mundlak_test)

  invisible(list(model = model, mundlak_test = mundlak_test, data = model_data))
}

run_all_mundlak_models <- function(type_name = c("type1", "type2")) {
  type_name <- match.arg(type_name)
  panel <- build_panel()

  results <- list(
    baseline = run_one_model(panel, type_name, "baseline", "MODEL 1: BASELINE"),
    aux_fiscal = run_one_model(panel, type_name, "aux_fiscal", "MODEL 2: AUXILIARY FISCAL STRESS"),
    aux_political = run_one_model(panel, type_name, "aux_political", "MODEL 3: AUXILIARY POLITICAL STRESS"),
    main = run_one_model(panel, type_name, "main", "MODEL 4: MAIN MODEL")
  )

  invisible(results)
}
