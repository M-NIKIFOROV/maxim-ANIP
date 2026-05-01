# 03_mundlak2.R
#
# Mundlak Type 2 (all countries, with missing gas dependency set to 0)
# for each model in Models_and_hypotheses___ANIP.pdf.

source("03_mundlak/03_mundlak_common.R")

dir.create("05_models/05_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "05_models/05_output/03_mundlak2_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 03_mundlak2.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

results_type2 <- run_all_mundlak_models(type_name = "type2")
