# 04_hausman1.R
#
# Hausman (FE vs RE) test — Type 1: countries with non-missing gas dependency.

source("04_hausman/04_hausman_common.R")

results_type1 <- run_all_hausman(type_name = "type1")
