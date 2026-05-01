# 04_hausman1.R
#
# Hausman (FE vs RE) test — Type 1: countries with non-missing gas dependency.

source("04_hausman/04_hausman_common.R")

dir.create("05_models/05_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "05_models/05_output/04_hausman1_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 04_hausman1.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

results_type1 <- run_all_hausman(type_name = "type1")
