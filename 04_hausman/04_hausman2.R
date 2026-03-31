# 04_hausman2.R
#
# Hausman (FE vs RE) test — Type 2: all EU countries (gas dependency zero-imputed).
# Netherlands and Czechia are absent from govtapproval data, so Models 3 and 4
# will naturally have 2 fewer countries — this is handled by prepare_data().

source("04_hausman/04_hausman_common.R")

results_type2 <- run_all_hausman(type_name = "type2")
