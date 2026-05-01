# 05_aux1.R
# CRE auxiliary model 1: fiscal stress (debt/GDP) as DV, 2006-2023.

source("05_models/05_models_common.R")

dir.create("05_models/05_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "05_models/05_output/05_aux1_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 05_aux1.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

result_aux1      <- run_05_model(panel, "aux_fiscal", common_countries)
