# 04_hausman2.R
#
# Hausman (FE vs RE) test — Type 2: all EU countries (gas dependency zero-imputed).
# Netherlands and Czechia are absent from govtapproval data, so Models 3 and 4
# will naturally have 2 fewer countries — this is handled by prepare_data().

source("04_hausman/04_hausman_common.R")

dir.create("05_models/05_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "05_models/05_output/04_hausman2_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 04_hausman2.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

results_type2 <- run_all_hausman(type_name = "type2")
