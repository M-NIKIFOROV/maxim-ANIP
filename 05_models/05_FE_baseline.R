# 05_FE_baseline.R
# FE counterpart to 05 baseline, aligned to the 05 sample pipeline.

library(fixest)
library(dplyr)

source("05_models/05_models_common.R")

dir.create("05_models/05_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "05_models/05_output/05_FE_baseline_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 05_FE_baseline.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)
model_data       <- prepare_05_data(panel, "baseline", common_countries)

fe_baseline <- feols(
  log_def_spend ~
    log_energy_lag + log1p_gasdep + energy_gasdep_int +
    log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats |
    country + year,
  data = model_data,
  cluster = "country"
)

cat("====================================================\n")
cat("MODEL: 05 FE BASELINE\n")
cat("====================================================\n")
cat("Countries   :", length(unique(model_data$country)), "\n")
cat("Observations:", nrow(model_data), "\n")
cat("Year range  :", min(model_data$year), "-", max(model_data$year), "\n\n")
print_signif(fe_baseline)
