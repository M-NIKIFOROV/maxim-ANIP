# 05_models_common.R
# Final CRE (Mundlak) models for all four ANIP specifications, 2006-2023.
#
# Type 2 (all EU, missing gas dependency zero-imputed).
# "Same number of countries" is enforced by deriving a common country set from
# the most restrictive (main) model requirements, then applying it to all four.

library(readr)
library(dplyr)
library(tidyr)
library(fixest)

source("03_mundlak/03_mundlak_common.R")

# ---------------------------------------------------------------------------
# Build panel with year restriction and type-2 gas treatment
# ---------------------------------------------------------------------------

build_05_panel <- function() {
  build_panel() %>%
    filter(year >= 2006, year <= 2023) %>%
    mutate(gasdep = if_else(is.na(gasdep), 0, gasdep)) %>%
    mutate(
      log_energy_lag    = if_else(!is.na(energy_lag) & energy_lag > 0,
                                  log(energy_lag), NA_real_),
      log_gdp_pc        = if_else(!is.na(gdp_pc)    & gdp_pc     > 0,
                                  log(gdp_pc),     NA_real_),
      log_area          = if_else(!is.na(area)       & area       > 0,
                                  log(area),       NA_real_),
      log_def_spend     = if_else(!is.na(def_spend)  & def_spend  > 0,
                                  log(def_spend),  NA_real_),
      log1p_gasdep      = log1p(gasdep),
      energy_gasdep_int = log_energy_lag * log1p_gasdep
    )
}

# ---------------------------------------------------------------------------
# Derive the common country set from the main model's requirements.
# All four models will be restricted to exactly these countries.
# ---------------------------------------------------------------------------

get_common_countries <- function(panel) {
  panel %>%
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
      !is.na(ideol_seats),
      !is.na(log_def_spend),
      !is.na(debtgdp_lag),
      !is.na(political_stress_lag)
    ) %>%
    pull(country) %>%
    unique()
}

# ---------------------------------------------------------------------------
# Prepare model-specific data and add CRE (Mundlak) group means
# ---------------------------------------------------------------------------

prepare_05_data <- function(panel, model_name, common_countries) {
  base <- panel %>%
    filter(
      country %in% common_countries,
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

  data <- if (model_name == "baseline") {
    base %>% filter(!is.na(log_def_spend))
  } else if (model_name == "aux_fiscal") {
    base %>% filter(!is.na(debtgdp))
  } else if (model_name == "aux_political") {
    base %>% filter(!is.na(political_stress))
  } else if (model_name == "main") {
    base %>% filter(!is.na(log_def_spend), !is.na(debtgdp_lag),
                    !is.na(political_stress_lag))
  } else {
    stop("Unknown model name")
  }

  data %>%
    group_by(country) %>%
    mutate(
      mean_log_energy_lag       = mean(log_energy_lag,       na.rm = TRUE),
      mean_log1p_gasdep         = mean(log1p_gasdep,         na.rm = TRUE),
      mean_energy_gasdep_int    = mean(energy_gasdep_int,    na.rm = TRUE),
      mean_log_gdp_pc           = mean(log_gdp_pc,           na.rm = TRUE),
      mean_threat               = mean(threat,               na.rm = TRUE),
      mean_log_area             = mean(log_area,             na.rm = TRUE),
      mean_nato                 = mean(nato,                 na.rm = TRUE),
      mean_ideology             = mean(ideology,             na.rm = TRUE),
      mean_seats                = mean(seats,                na.rm = TRUE),
      mean_ideol_seats          = mean(ideol_seats,          na.rm = TRUE),
      mean_debtgdp_lag          = mean(debtgdp_lag,          na.rm = TRUE),
      mean_political_stress_lag = mean(political_stress_lag, na.rm = TRUE)
    ) %>%
    ungroup()
}

# ---------------------------------------------------------------------------
# Estimate CRE model and print results
# ---------------------------------------------------------------------------

run_05_model <- function(panel, model_name, common_countries) {
  model_data <- prepare_05_data(panel, model_name, common_countries)

  fml <- switch(model_name,
    baseline = log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,

    aux_fiscal = debtgdp ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,

    aux_political = political_stress ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,

    main = log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      debtgdp_lag + political_stress_lag +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_debtgdp_lag + mean_political_stress_lag +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,

    stop("Unknown model name")
  )

  model        <- feols(fml, data = model_data, cluster = "country")
  mundlak_test <- wald(model, keep = "^mean_")

  cat("\n====================================================\n")
  cat("MODEL:", toupper(model_name), "\n")
  cat("====================================================\n")
  cat("Countries   :", length(unique(model_data$country)), "\n")
  cat("Observations:", nrow(model_data), "\n")
  cat("Year range  :", min(model_data$year), "-", max(model_data$year), "\n\n")
  print_signif(model)
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(mundlak_test)

  invisible(list(model = model, mundlak_test = mundlak_test, data = model_data))
}
