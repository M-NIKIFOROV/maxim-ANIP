# 05_aux2.R
# CRE auxiliary model 2: political stress (govt disapproval) as DV, 2006-2023.

source("05_models/05_models_common.R")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

result_aux2      <- run_05_model(panel, "aux_political", common_countries)
