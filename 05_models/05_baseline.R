# 05_baseline.R
# CRE baseline model (defence spending ~ energy + controls), 2006-2023.

source("05_models/05_models_common.R")

dir.create("05_models/05_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "05_models/05_output/05_baseline_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 05_baseline.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

result_baseline  <- run_05_model(panel, "baseline", common_countries)
