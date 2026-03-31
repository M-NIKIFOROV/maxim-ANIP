# 03_mundlak1.R
#
# Mundlak Type 1 (countries with non-missing gas dependency)
# for each model in Models_and_hypotheses___ANIP.pdf.

source("03_mundlak/03_mundlak_common.R")

results_type1 <- run_all_mundlak_models(type_name = "type1")