# 04_hausman_common.R
# Hausman (FE vs RE) test for each ANIP model variant.
#
# Sources build_panel() and prepare_data() from 03_mundlak_common.R so that
# sample definitions are kept identical between the Mundlak and Hausman steps.
# Year effects are included via a year_factor variable to avoid interacting with
# the plm time index (using factor(year) directly on the index can cause issues).

library(plm)
source("03_mundlak/03_mundlak_common.R")

run_one_hausman <- function(panel, type_name, model_name, header_text) {
  model_data <- prepare_data(panel, model_name, type_name) %>%
    mutate(year_factor = factor(year))

  pdata <- pdata.frame(as.data.frame(model_data), index = c("country", "year"))

  if (model_name == "baseline") {
    fml <- log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + ideology + seats + ideol_seats +
      year_factor
  } else if (model_name == "aux_fiscal") {
    fml <- debtgdp ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + ideology + seats + ideol_seats +
      year_factor
  } else if (model_name == "aux_political") {
    fml <- political_stress ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + ideology + seats + ideol_seats +
      year_factor
  } else if (model_name == "main") {
    fml <- log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      debtgdp_lag + political_stress_lag +
      log_gdp_pc + threat + ideology + seats + ideol_seats +
      year_factor
  } else {
    stop("Unknown model name")
  }

  # effect = "individual": country FE/RE only; year handled via explicit year_factor
  # dummies so both models have the same time controls.
  # Amemiya variance estimator for RE: uses within-model residuals to estimate
  # variance components — does not involve the between estimator, so it is robust
  # when the number of regressors exceeds the number of groups (small-N problem
  # that breaks the default Swamy-Arora estimator).
  # log_area and nato are time-invariant: FE silently drops them; RE estimates
  # them; phtest compares only the jointly estimable (time-varying) coefficients.
  fe_mod <- plm(fml, data = pdata, effect = "individual", model = "within")
  re_mod <- plm(fml, data = pdata, effect = "individual", model = "random",
                random.method = "amemiya")

  haus_test <- phtest(fe_mod, re_mod)

  cat("\n====================================================\n")
  cat(header_text, "\n")
  cat("====================================================\n")
  cat("Type:", type_name, "\n")
  cat("Countries:", length(unique(model_data$country)), "\n")
  cat("Observations:", nrow(model_data), "\n\n")
  cat("Hausman Test (FE vs RE):\n")
  print(haus_test)

  invisible(list(fe = fe_mod, re = re_mod, hausman = haus_test, data = model_data))
}

run_all_hausman <- function(type_name = c("type1", "type2")) {
  type_name <- match.arg(type_name)
  panel <- build_panel()

  results <- list(
    baseline     = run_one_hausman(panel, type_name, "baseline",      "MODEL 1: BASELINE"),
    aux_fiscal   = run_one_hausman(panel, type_name, "aux_fiscal",    "MODEL 2: AUXILIARY FISCAL STRESS"),
    aux_political = run_one_hausman(panel, type_name, "aux_political", "MODEL 3: AUXILIARY POLITICAL STRESS"),
    main         = run_one_hausman(panel, type_name, "main",          "MODEL 4: MAIN MODEL")
  )

  invisible(results)
}
