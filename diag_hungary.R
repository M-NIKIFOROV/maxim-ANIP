suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(fixest)
})
setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/ANIP/maxim-ANIP")
source("05_models/05_models_common.R")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)
model_data       <- prepare_05_data(panel, "baseline", common_countries)

cat("Hungary in panel:", "hungary" %in% model_data$country, "\n")

cat("\nHungary energy_lag values after fix (should be ~0.1-0.5 EUR/kWh):\n")
model_data %>% filter(country == "hungary") %>%
  mutate(energy_raw = exp(log_energy_lag)) %>%
  select(year, energy_raw) %>% arrange(year) %>% print(n=20)

cat("\nTop 10 countries by mean energy price after fix:\n")
model_data %>%
  group_by(country) %>%
  summarise(mean_e = round(mean(exp(log_energy_lag), na.rm=TRUE), 3), .groups="drop") %>%
  arrange(desc(mean_e)) %>% print(n=10)

fml <- log_def_spend ~
  log_energy_lag + log1p_gasdep + energy_gasdep_int +
  log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
  mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
  mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
  mean_ideology + mean_seats + mean_ideol_seats |
  year

m_fixed <- feols(fml, data = model_data, cluster = "country")
ct <- coeftable(m_fixed)

vars <- c("log_energy_lag","mean_log_energy_lag","log_gdp_pc","mean_log_gdp_pc",
          "log1p_gasdep","mean_log1p_gasdep","energy_gasdep_int","mean_energy_gasdep_int",
          "threat","nato")

cat("\n=== Baseline after currency fix ===\n")
cat(sprintf("%-28s  %8s %6s\n","Variable","Coef","p"))
cat(strrep("-", 48), "\n")
for (v in vars) {
  cat(sprintf("%-28s  %8.4f %6.3f\n", v, ct[v,"Estimate"], ct[v,"Pr(>|t|)"]))
}
cat("\nN:", nobs(m_fixed), "| Within R2:", round(fitstat(m_fixed,"wr2")[[1]], 4), "\n")
