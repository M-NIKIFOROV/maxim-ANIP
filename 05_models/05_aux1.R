# 05_aux1.R
# CRE auxiliary model 1: fiscal stress (debt/GDP) as DV, 2006-2023.

source("05_models/05_models_common.R")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

result_aux1      <- run_05_model(panel, "aux_fiscal", common_countries)
