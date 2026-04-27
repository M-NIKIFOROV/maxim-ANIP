# 05.5_fe_baseline.R
# FE counterpart to 05 baseline, using near-identical sample construction.

library(fixest)
library(dplyr)

source("05_models/05_models_common.R")

# Use the same panel builder and common-country logic as 05 baseline.
panel            <- build_05_panel()
common_countries <- get_common_countries(panel)
model_data       <- prepare_05_data(panel, "baseline", common_countries)

# FE version: same substantive regressors as CRE baseline, but with country + year FE
# and without Mundlak mean_* terms.
fe_baseline <- feols(
  log_def_spend ~
    log_energy_lag + log1p_gasdep + energy_gasdep_int +
    log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats |
    country + year,
  data = model_data,
  cluster = "country"
)

# Build plain-language spec notes for paper reproducibility.
spec_notes <- c(
  "Specification Notes:",
  "1) Sample construction is reused from 05 baseline (build_05_panel + get_common_countries + prepare_05_data('baseline')).",
  "2) Time window is 2006-2023, inherited from build_05_panel().",
  paste0("3) Countries are restricted to the shared 05 common-country set (N countries = ", length(unique(model_data$country)), ")."),
  paste0("4) Observations used in FE baseline = ", nrow(model_data), "."),
  "5) Energy data use EUR + i_tax filter (already applied in shared panel builder).",
  "6) Gas dependency uses Type-2 treatment (missing gasdep imputed to 0 via build_05_panel).",
  "7) Controls match 05 CRE baseline substantive RHS: log_gdp_pc, threat, log_area, nato, ideology, seats, ideol_seats.",
  "8) Added terms matching 05 baseline: log1p_gasdep and energy_gasdep_int.",
  "9) Estimator difference vs 05 baseline: this model uses country and year fixed effects (| country + year) instead of CRE Mundlak mean_* terms.",
  "10) SEs clustered by country (same as 05 baseline)."
)

# Console output
cat("\n====================================================\n")
cat("MODEL: 05.5 FE BASELINE (CRE-COMPATIBLE SAMPLE)\n")
cat("====================================================\n")
cat("Countries   :", length(unique(model_data$country)), "\n")
cat("Observations:", nrow(model_data), "\n")
cat("Year range  :", min(model_data$year), "-", max(model_data$year), "\n\n")
print(summary(fe_baseline))
cat("\n")
cat(paste(spec_notes, collapse = "\n"), "\n")

# Save regression summary + notes for paper drafting.
out_file <- "05.5_FE/output/fe_baseline_regression_and_notes.txt"
cat(
  capture.output({
    cat("MODEL: 05.5 FE BASELINE (CRE-COMPATIBLE SAMPLE)\n\n")
    print(summary(fe_baseline))
    cat("\n")
    cat(paste(spec_notes, collapse = "\n"), "\n")
  }),
  sep = "\n",
  file = out_file
)

message("Saved: ", out_file)
