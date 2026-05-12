# 05_summary_stats.R
# Compute summary statistics for the common-country sample and save to txt.

source("05_models/05_models_common.R")
library(dplyr)

out_dir <- "05_models/05_output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, "05_summary_stats.txt")

panel <- build_05_panel()
common_countries <- get_common_countries(panel)
base <- panel %>% filter(country %in% common_countries)

summarize_var <- function(x) {
  x <- as.numeric(x)
  x_non_na <- x[!is.na(x)]

  data.frame(
    N = length(x_non_na),
    Mean = mean(x_non_na),
    SD = sd(x_non_na),
    Min = min(x_non_na),
    Max = max(x_non_na)
  )
}

spec <- data.frame(
  label = c(
    "ln(Defence_i,t)",
    "Defence_i,t (million $)",
    "ln(Energy_i,t-1)",
    "Energy_i,t-1 (price index)",
    "GasDep_i,t (%)",
    "ln(1+GasDep_i,t)",
    "ln(Energy_i,t-1) x ln(1+GasDep_i,t)",
    "FiscStress_i,t-1 (Debt/GDP %)",
    "PolStress_i,t-1 (share)",
    "ln(GDPpc_i,t)",
    "Threat_i,t (km to Russia)",
    "ln(Area_i)",
    "NATO_i,t (binary)",
    "Ideology_i,t (left-right)",
    "Seats_i,t (parliamentary)",
    "Ideology_i,t x Seats_i,t"
  ),
  var = c(
    "log_def_spend",
    "def_spend",
    "log_energy_lag",
    "energy_lag",
    "gasdep",
    "log1p_gasdep",
    "energy_gasdep_int",
    "debtgdp_lag",
    "political_stress_lag",
    "log_gdp_pc",
    "threat",
    "log_area",
    "nato",
    "ideology",
    "seats",
    "ideol_seats"
  ),
  stringsAsFactors = FALSE
)

rows <- lapply(seq_len(nrow(spec)), function(i) {
  v <- spec$var[i]
  s <- summarize_var(base[[v]])
  data.frame(
    Variable = spec$label[i],
    Mean = s$Mean,
    SD = s$SD,
    Min = s$Min,
    Max = s$Max,
    N = s$N,
    stringsAsFactors = FALSE
  )
})

result <- bind_rows(rows)

sink(out_file)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 05_summary_stats.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Result file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")
cat("Sample definition: common-country sample from get_common_countries(build_05_panel())\n")
cat("Countries:", length(unique(base$country)), "\n")
cat("Observations in panel (country-year rows):", nrow(base), "\n\n")

print(result, row.names = FALSE, digits = 15)

message("Saved: ", out_file)
