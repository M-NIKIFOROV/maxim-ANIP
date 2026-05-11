# 05_robustness.R
# Robustness checks aligned to 05 model setup, written to 05_output.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(fixest)
})

source("05_models/05_models_common.R")

dir.create("05_models/05_output", showWarnings = FALSE, recursive = TRUE)

write_block <- function(path, header, model, notes = NULL) {
  cat(
    capture.output({
      cat(header, "\n\n")
      print_signif(model)
      if (!is.null(notes)) {
        cat("\nNotes:\n")
        cat(paste0("- ", notes), sep = "\n")
      }
    }),
    sep = "\n",
    file = path
  )
}

write_lag_block <- function(path, model_name, lag_k, model_obj, p_original) {
  cat(
    capture.output({
      cat("SCRIPT: 05_robustness.R\n")
      cat("CHECK: Alternative lag as standalone result\n")
      cat("Model:", toupper(model_name), "\n")
      cat("Lag  :", lag_k, "\n")
      cat("Original p(log_energy_lag):", round(p_original, 4), "\n\n")
      print_signif(model_obj)
    }),
    sep = "\n",
    file = path
  )
}

get_formula_05 <- function(model_name) {
  switch(model_name,
    baseline = log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,

    aux_fiscal = debtgdp ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,

    aux_political = political_stress ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,

    main = log_def_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      debtgdp_lag + political_stress_lag +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_debtgdp_lag + mean_political_stress_lag +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,

    stop("Unknown model name")
  )
}

get_p_log_energy <- function(model) {
  ct <- coeftable(model)
  if (!"log_energy_lag" %in% rownames(ct)) return(NA_real_)
  as.numeric(ct["log_energy_lag", "Pr(>|t|)"])
}

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

base_for_dv <- prepare_05_data(panel, "baseline", common_countries)

defgdp_long <- read_csv("data/clean/defencegdp_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_year_values("defencegdp") %>%
  mutate(defencegdp = as.numeric(defencegdp)) %>%
  select(country, year, defencegdp)

alt_dv_data <- base_for_dv %>%
  left_join(defgdp_long, by = c("country", "year")) %>%
  mutate(log_defencegdp = if_else(!is.na(defencegdp) & defencegdp > 0, log(defencegdp), NA_real_)) %>%
  filter(!is.na(log_defencegdp))

alt_dv_model <- feols(
  log_defencegdp ~
    log_energy_lag + log1p_gasdep + energy_gasdep_int +
    log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
    mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
    mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
    mean_ideology + mean_seats + mean_ideol_seats |
    year,
  data = alt_dv_data,
  cluster = "country"
)

write_block(
  "05_models/05_output/05_robustness_01_alt_dv_full.txt",
  "SCRIPT: 05_robustness.R\nCHECK: Alternative DV (log(defencegdp)) with baseline specification",
  alt_dv_model,
  notes = c(
    paste0("Countries: ", length(unique(alt_dv_data$country))),
    paste0("Observations: ", nrow(alt_dv_data)),
    paste0("Year range in estimation sample: ", min(alt_dv_data$year), "-", max(alt_dv_data$year))
  )
)

base_for_energy <- prepare_05_data(panel, "baseline", common_countries)

brent <- read_csv("data/clean/eubrent_cleaned.csv", show_col_types = FALSE) %>%
  transmute(year = as.integer(year), brent_price = as.numeric(price)) %>%
  arrange(year) %>%
  mutate(brent_lag = lag(brent_price, 1))

alt_energy_data <- base_for_energy %>%
  left_join(brent %>% select(year, brent_lag), by = "year") %>%
  mutate(
    log_brent_lag = if_else(!is.na(brent_lag) & brent_lag > 0, log(brent_lag), NA_real_),
    brent_gasdep_int = log_brent_lag * log1p_gasdep
  ) %>%
  filter(!is.na(log_brent_lag), !is.na(brent_gasdep_int)) %>%
  group_by(country) %>%
  mutate(
    mean_log_brent_lag = mean(log_brent_lag, na.rm = TRUE),
    mean_brent_gasdep_int = mean(brent_gasdep_int, na.rm = TRUE)
  ) %>%
  ungroup()

alt_energy_model <- feols(
  log_def_spend ~
    log_brent_lag + log1p_gasdep + brent_gasdep_int +
    log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
    mean_log_brent_lag + mean_log1p_gasdep + mean_brent_gasdep_int +
    mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
    mean_ideology + mean_seats + mean_ideol_seats |
    year,
  data = alt_energy_data,
  cluster = "country"
)

write_block(
  "05_models/05_output/05_robustness_02_alt_energy_full.txt",
  "SCRIPT: 05_robustness.R\nCHECK: Alternative energy (Brent) in baseline specification",
  alt_energy_model,
  notes = c(
    paste0("Countries: ", length(unique(alt_energy_data$country))),
    paste0("Observations: ", nrow(alt_energy_data)),
    paste0("Year range in estimation sample: ", min(alt_energy_data$year), "-", max(alt_energy_data$year))
  )
)

models_05 <- c("baseline", "aux_fiscal", "aux_political", "main")

base_models <- lapply(models_05, function(mn) {
  d <- prepare_05_data(panel, mn, common_countries)
  m <- feols(get_formula_05(mn), data = d, cluster = "country")
  list(name = mn, data = d, model = m, p_main = get_p_log_energy(m))
})
names(base_models) <- models_05

selected <- vapply(base_models, function(x) {
  isTRUE(!is.na(x$p_main) && x$p_main < 0.10) || x$name == "main"
}, logical(1))
selected_models <- names(base_models)[selected]

panel_lags <- panel %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    energy_lag0 = energy_price,
    energy_lag2 = lag(energy_price, 2),
    energy_lag3 = lag(energy_price, 3)
  ) %>%
  ungroup()

lag_file <- "05_models/05_output/05_robustness_03_alt_lags_full.txt"
writeLines(c(
  "SCRIPT: 05_robustness.R",
  "CHECK: Alternative lags for selected models",
  paste0("Selected models: ", paste(selected_models, collapse = ", ")),
  ""
), con = lag_file)

for (mn in selected_models) {
  write(
    paste0("\n====================================================\nMODEL: ", toupper(mn),
           "\n====================================================\nOriginal p(log_energy_lag)=", round(base_models[[mn]]$p_main, 4), "\n"),
    file = lag_file,
    append = TRUE
  )

  for (k in c(0L, 2L, 3L)) {
    lag_col <- paste0("energy_lag", k)

    panel_k <- panel_lags %>%
      mutate(
        log_energy_lag = if_else(!is.na(.data[[lag_col]]) & .data[[lag_col]] > 0,
                                 log(.data[[lag_col]]), NA_real_),
        energy_gasdep_int = log_energy_lag * log1p_gasdep
      )

    data_k <- prepare_05_data(panel_k, mn, common_countries)
    model_k <- feols(get_formula_05(mn), data = data_k, cluster = "country")

    cat(
      capture.output({
        cat("\n-- Lag", k, "--\n\n")
        print_signif(model_k)
      }),
      sep = "\n",
      file = lag_file,
      append = TRUE
    )

    lag_file_single <- sprintf(
      "05_models/05_output/05_robustness_03_%s_lag%s_full.txt",
      mn,
      k
    )

    write_lag_block(
      path = lag_file_single,
      model_name = mn,
      lag_k = k,
      model_obj = model_k,
      p_original = base_models[[mn]]$p_main
    )
  }
}

message("Saved: 05_models/05_output/05_robustness_01_alt_dv_full.txt")
message("Saved: 05_models/05_output/05_robustness_02_alt_energy_full.txt")
message("Saved: 05_models/05_output/05_robustness_03_alt_lags_full.txt")
for (mn in selected_models) {
  for (k in c(0L, 2L, 3L)) {
    message(sprintf("Saved: 05_models/05_output/05_robustness_03_%s_lag%s_full.txt", mn, k))
  }
}

# ---------------------------------------------------------------------------
# Robustness 04: Real energy prices (HICP-deflated) vs nominal — baseline CRE
# ---------------------------------------------------------------------------

hicp_long <- read_csv("data/clean/hicp_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_year_values("hicp") %>%
  mutate(hicp = as.numeric(hicp)) %>%
  select(country, year, hicp)

base_for_real_energy <- prepare_05_data(panel, "baseline", common_countries)

energy_nominal_long <- read_csv("data/clean/energy_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  filter(currency == "eur", tax == "i_tax") %>%
  pivot_year_values("energy_price_raw") %>%
  mutate(energy_price_raw = as.numeric(energy_price_raw)) %>%
  group_by(country, year) %>%
  summarise(energy_price_raw = mean(energy_price_raw, na.rm = TRUE), .groups = "drop") %>%
  mutate(energy_price_raw = if_else(is.nan(energy_price_raw), NA_real_, energy_price_raw))

real_energy_data <- base_for_real_energy %>%
  left_join(hicp_long, by = c("country", "year")) %>%
  left_join(energy_nominal_long, by = c("country", "year")) %>%
  mutate(
    real_energy = if_else(!is.na(energy_price_raw) & !is.na(hicp) & hicp > 0,
                          energy_price_raw / (hicp / 100), NA_real_),
    real_energy_lag = lag(real_energy, 1),
    log_real_energy_lag = if_else(!is.na(real_energy_lag) & real_energy_lag > 0,
                                  log(real_energy_lag), NA_real_),
    real_energy_gasdep_int = log_real_energy_lag * log1p_gasdep
  ) %>%
  filter(!is.na(log_real_energy_lag), !is.na(real_energy_gasdep_int)) %>%
  group_by(country) %>%
  mutate(
    mean_log_real_energy_lag    = mean(log_real_energy_lag,    na.rm = TRUE),
    mean_real_energy_gasdep_int = mean(real_energy_gasdep_int, na.rm = TRUE)
  ) %>%
  ungroup()

real_energy_model <- feols(
  log_def_spend ~
    log_real_energy_lag + log1p_gasdep + real_energy_gasdep_int +
    log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
    mean_log_real_energy_lag + mean_log1p_gasdep + mean_real_energy_gasdep_int +
    mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
    mean_ideology + mean_seats + mean_ideol_seats |
    year,
  data = real_energy_data,
  cluster = "country"
)

out_real <- "05_models/05_output/05_robustness_04_real_energy_full.txt"
sink(out_real, split = TRUE)
cat("SCRIPT: 05_robustness.R\n")
cat("CHECK: Real energy prices (HICP-deflated, 2015=100) vs nominal — baseline CRE\n\n")
cat("Nominal baseline log_energy_lag coef :", round(coef(base_models$baseline$model)["log_energy_lag"], 5), "\n")
cat("Real    baseline log_real_energy_lag  :", round(coef(real_energy_model)["log_real_energy_lag"], 5), "\n\n")
cat("--- NOMINAL MODEL (baseline) ---\n\n")
print_signif(base_models$baseline$model)
cat("\n--- REAL ENERGY MODEL ---\n\n")
print_signif(real_energy_model)
sink()
message("Saved: 05_models/05_output/05_robustness_04_real_energy_full.txt")
