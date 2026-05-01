# 07_diagnostics.R
# Panel diagnostic tests for the ANIP main model (05 sample, 2006-2023).
#
# Tests:
#   1. Wooldridge / Breusch-Godfrey serial correlation  -- plm::pbgtest()
#   2. Breusch-Pagan heteroskedasticity                 -- lmtest::bptest()
#   3. Pesaran cross-sectional dependence               -- plm::pcdtest()
#   4. Variance Inflation Factors (multicollinearity)   -- car::vif()
#
# VIF note: car::vif() does not support plm objects directly.
# We run a pooled OLS on the within-demeaned regressors (country means
# subtracted), which isolates the within-country variation and correctly
# reflects the multicollinearity structure of the FE estimator.

library(plm)
library(lmtest)
library(car)
library(dplyr)

source("05_models/05_models_common.R")

dir.create("07_diagnostics/07_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "07_diagnostics/07_output/07_diagnostics_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 07_diagnostics.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

write_result_file_07 <- function(file_name, expr) {
  path <- file.path("07_diagnostics/07_output", file_name)
  sink(path, split = TRUE)
  on.exit(sink(), add = TRUE)

  cat("SCRIPT: 07_diagnostics.R\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Result file:", normalizePath(path, winslash = "/", mustWork = FALSE), "\n\n")

  eval.parent(substitute(expr))
}

# ---------------------------------------------------------------------------
# Build data (main model = most complete specification)
# ---------------------------------------------------------------------------

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)
model_data       <- prepare_05_data(panel, "main", common_countries)

cat("Countries   :", length(unique(model_data$country)), "\n")
cat("Observations:", nrow(model_data), "\n")
cat("Year range  :", min(model_data$year), "-", max(model_data$year), "\n\n")

# ---------------------------------------------------------------------------
# Build plm panel data object and within (FE) model for plm-based tests
# ---------------------------------------------------------------------------

pdata <- pdata.frame(model_data, index = c("country", "year"))

fe_formula <- log_def_spend ~
  log_energy_lag + log1p_gasdep + energy_gasdep_int +
  debtgdp_lag + political_stress_lag +
  log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats

fe_model <- plm(fe_formula, data = pdata, model = "within", effect = "twoways")

# ---------------------------------------------------------------------------
# 1. Wooldridge / Breusch-Godfrey serial correlation test
#    H0: no serial correlation in the idiosyncratic errors
# ---------------------------------------------------------------------------

cat("====================================================\n")
cat("TEST 1: WOOLDRIDGE SERIAL CORRELATION (pbgtest)\n")
cat("H0: No serial correlation in idiosyncratic errors\n")
cat("====================================================\n")
test_serial <- pbgtest(fe_model)
print(test_serial)
cat("\n")

# ---------------------------------------------------------------------------
# 2. Breusch-Pagan heteroskedasticity test
#    H0: homoskedastic errors
# ---------------------------------------------------------------------------

cat("====================================================\n")
cat("TEST 2: BREUSCH-PAGAN HETEROSKEDASTICITY (bptest)\n")
cat("H0: Homoskedastic errors\n")
cat("====================================================\n")
test_bp <- bptest(fe_model)
print(test_bp)
cat("\n")

# ---------------------------------------------------------------------------
# 3. Pesaran cross-sectional dependence test
#    H0: no cross-sectional dependence
# ---------------------------------------------------------------------------

cat("====================================================\n")
cat("TEST 3: PESARAN CD TEST (pcdtest)\n")
cat("H0: No cross-sectional dependence\n")
cat("====================================================\n")
test_cd <- pcdtest(fe_model)
print(test_cd)
cat("\n")

# ---------------------------------------------------------------------------
# 4. Variance Inflation Factors
#    Approach: demean all regressors by country mean (within transformation),
#    then run pooled OLS on demeaned data and compute VIF on that object.
#    This mirrors the variation used by the FE estimator.
# ---------------------------------------------------------------------------

regressors <- c(
  "log_energy_lag", "log1p_gasdep", "energy_gasdep_int",
  "debtgdp_lag", "political_stress_lag",
  "log_gdp_pc", "threat", "log_area", "nato", "ideology", "seats", "ideol_seats"
)

demeaned <- model_data %>%
  group_by(country) %>%
  mutate(across(all_of(c("log_def_spend", regressors)),
                ~ . - mean(., na.rm = TRUE),
                .names = "{.col}_dm")) %>%
  ungroup()

vif_formula <- as.formula(
  paste("log_def_spend_dm ~",
        paste(paste0(regressors, "_dm"), collapse = " + "))
)

vif_lm <- lm(vif_formula, data = demeaned)

cat("====================================================\n")
cat("TEST 4: VARIANCE INFLATION FACTORS (car::vif)\n")
cat("Applied to within-demeaned regressors (FE variation)\n")
cat("Rule of thumb: VIF > 10 indicates high multicollinearity\n")
cat("====================================================\n")
vif_vals <- vif(vif_lm)
names(vif_vals) <- sub("_dm$", "", names(vif_vals))
print(round(vif_vals, 3))
cat("\n")
cat("Max VIF  :", round(max(vif_vals), 3), "\n")
cat("Mean VIF :", round(mean(vif_vals), 3), "\n")

write_result_file_07("07_01_model_context.txt", {
  cat("MODEL CONTEXT: MAIN SPECIFICATION (05 SAMPLE)\n")
  cat("Countries   :", length(unique(model_data$country)), "\n")
  cat("Observations:", nrow(model_data), "\n")
  cat("Year range  :", min(model_data$year), "-", max(model_data$year), "\n\n")
  print(summary(fe_model))
})

write_result_file_07("07_02_serial_correlation_pbgtest.txt", {
  cat("TEST 1: WOOLDRIDGE SERIAL CORRELATION (pbgtest)\n")
  cat("H0: No serial correlation in idiosyncratic errors\n\n")
  print(test_serial)
})

write_result_file_07("07_03_heteroskedasticity_bptest.txt", {
  cat("TEST 2: BREUSCH-PAGAN HETEROSKEDASTICITY (bptest)\n")
  cat("H0: Homoskedastic errors\n\n")
  print(test_bp)
})

write_result_file_07("07_04_cross_section_dependence_pcdtest.txt", {
  cat("TEST 3: PESARAN CD TEST (pcdtest)\n")
  cat("H0: No cross-sectional dependence\n\n")
  print(test_cd)
})

write_result_file_07("07_05_vif_within_demeaned.txt", {
  cat("TEST 4: VARIANCE INFLATION FACTORS (car::vif)\n")
  cat("Applied to within-demeaned regressors (FE variation)\n")
  cat("Rule of thumb: VIF > 10 indicates high multicollinearity\n\n")
  print(round(vif_vals, 3))
  cat("\n")
  cat("Max VIF  :", round(max(vif_vals), 3), "\n")
  cat("Mean VIF :", round(mean(vif_vals), 3), "\n")
})

message("Saved: 07_diagnostics/07_output/07_01_model_context.txt")
message("Saved: 07_diagnostics/07_output/07_02_serial_correlation_pbgtest.txt")
message("Saved: 07_diagnostics/07_output/07_03_heteroskedasticity_bptest.txt")
message("Saved: 07_diagnostics/07_output/07_04_cross_section_dependence_pcdtest.txt")
message("Saved: 07_diagnostics/07_output/07_05_vif_within_demeaned.txt")
