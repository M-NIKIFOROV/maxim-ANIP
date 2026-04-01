# 05_main.R
# CRE main model (defence spending ~ energy + fiscal + political stress), 2006-2023.

source("05_models/05_models_common.R")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

result_main      <- run_05_model(panel, "main", common_countries)
