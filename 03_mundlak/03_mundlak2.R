# 03_mundlak2.R
#
# Mundlak Type 2 (all countries, with missing gas dependency set to 0)
# for each model in Models_and_hypotheses___ANIP.pdf.

source("03_mundlak/03_mundlak_common.R")

results_type2 <- run_all_mundlak_models(type_name = "type2")
