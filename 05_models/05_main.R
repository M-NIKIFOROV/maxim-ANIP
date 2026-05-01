# 05_main.R
# CRE main model (defence spending ~ energy + fiscal + political stress), 2006-2023.

source("05_models/05_models_common.R")

dir.create("05_models/05_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "05_models/05_output/05_main_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 05_main.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

result_main      <- run_05_model(panel, "main", common_countries)
