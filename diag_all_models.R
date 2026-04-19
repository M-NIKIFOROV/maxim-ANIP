suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(fixest)
})
setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/ANIP/maxim-ANIP")
source("05_models/05_models_common.R")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

report_model <- function(model_name, label) {
  model_data <- prepare_05_data(panel, model_name, common_countries)

  fml <- switch(model_name,
    baseline = log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats | year,

    aux_fiscal = debtgdp ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats | year,

    aux_political = political_stress ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats | year,

    main = log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      debtgdp_lag + political_stress_lag +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_debtgdp_lag + mean_political_stress_lag +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats | year
  )

  m  <- feols(fml, data = model_data, cluster = "country")
  ct <- coeftable(m)
  wt <- wald(m, keep = "^mean_")

  cat("\n\n========================================\n")
  cat(label, "\n")
  cat("========================================\n")
  cat("N =", nobs(m), "| Within R2 =", round(fitstat(m,"wr2")[[1]], 4), "\n\n")

  sig <- ct[ct[,"Pr(>|t|)"] < 0.10, , drop=FALSE]
  if (nrow(sig) == 0) {
    cat("No terms significant at 10%.\n")
  } else {
    cat(sprintf("%-32s  %8s  %8s  %6s  %s\n","Term","Coef","SE","p",""))
    cat(strrep("-", 65), "\n")
    for (i in seq_len(nrow(sig))) {
      stars <- ifelse(sig[i,"Pr(>|t|)"] < 0.01, "***",
               ifelse(sig[i,"Pr(>|t|)"] < 0.05, "**", "*"))
      cat(sprintf("%-32s  %8.4f  %8.4f  %6.3f  %s\n",
          rownames(sig)[i], sig[i,"Estimate"], sig[i,"Std. Error"],
          sig[i,"Pr(>|t|)"], stars))
    }
  }
  cat("\nJoint Mundlak test: F =", round(wt$stat, 3),
      "| p =", round(wt$p, 4), "\n")
}

report_model("baseline",      "BASELINE  (DV: log defence spending)")
report_model("aux_fiscal",    "AUX 1     (DV: debt/GDP)")
report_model("aux_political", "AUX 2     (DV: political stress)")
report_model("main",          "MAIN      (DV: log defence spending + stress mediators)")
