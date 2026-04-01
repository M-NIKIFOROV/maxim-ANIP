# 05_baseline.R
# CRE baseline model (defence spending ~ energy + controls), 2006-2023.

source("05_models/05_models_common.R")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

result_baseline  <- run_05_model(panel, "baseline", common_countries)
