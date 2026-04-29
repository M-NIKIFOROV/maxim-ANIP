# 07_run_robustness.R
# Robustness checks requested for ANIP:
# 1) Alternative DV: defencegdp instead of defence spending (baseline spec)
# 2) Alternative energy: baseline spec with Brent (eubrent) replacing energy price
# 3) Alternative lags for models where the main explanatory variable is significant
#    (forcing inclusion of 05 main as requested)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(fixest)
})

source("05_models/05_models_common.R")

write_block <- function(path, header, model, notes = NULL) {
  cat(
    capture.output({
      cat(header, "\n\n")
      print(summary(model))
      if (!is.null(notes)) {
        cat("\nNotes:\n")
        cat(paste0("- ", notes), sep = "\n")
      }
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

# ---------------------------------------------------------------------------
# 1) Alternative DV robustness (baseline covariates, DV = defencegdp)
# ---------------------------------------------------------------------------

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
  "07_robustness/output/01_alt_dv_defencegdp.txt",
  "Robustness 1: Alternative DV (log(defencegdp)) with baseline specification",
  alt_dv_model,
  notes = c(
    paste0("Countries: ", length(unique(alt_dv_data$country))),
    paste0("Observations: ", nrow(alt_dv_data)),
    paste0("Year range in estimation sample: ", min(alt_dv_data$year), "-", max(alt_dv_data$year)),
    "Sample pipeline reused from 05 baseline, then DV replaced with defencegdp."
  )
)

# ---------------------------------------------------------------------------
# 2) Alternative energy robustness (baseline spec, energy replaced with eubrent)
# ---------------------------------------------------------------------------

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

ct_alt_energy <- coeftable(alt_energy_model)
brent_term_present <- "log_brent_lag" %in% rownames(ct_alt_energy)

write_block(
  "07_robustness/output/02_alt_energy_eubrent.txt",
  "Robustness 2: Alternative energy (Brent) in baseline specification",
  alt_energy_model,
  notes = c(
    paste0("Countries: ", length(unique(alt_energy_data$country))),
    paste0("Observations: ", nrow(alt_energy_data)),
    paste0("Year range in estimation sample: ", min(alt_energy_data$year), "-", max(alt_energy_data$year)),
    "Brent is common across countries by year, so year FE can absorb the main Brent term.",
    paste0("Main Brent term reported in coefficient table: ", brent_term_present)
  )
)

# ---------------------------------------------------------------------------
# 3) Alternative lag robustness for selected 05 models
#    Rule: include models with significant log_energy_lag at 10%, and force include 05 main.
# ---------------------------------------------------------------------------

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
    energy_lag2 = lag(energy_price, 2),
    energy_lag3 = lag(energy_price, 3)
  ) %>%
  ungroup()

lag_lines <- c(
  "Robustness 3: Alternative lags for selected models",
  "Selection rule: p(log_energy_lag)<0.10 in original 05 model, plus forced inclusion of main.",
  paste0("Selected models: ", paste(selected_models, collapse = ", ")),
  ""
)

for (mn in selected_models) {
  lag_lines <- c(lag_lines, paste0("Model: ", mn))
  lag_lines <- c(
    lag_lines,
    paste0("Original p(log_energy_lag) = ", round(base_models[[mn]]$p_main, 4))
  )

  for (k in c(2L, 3L)) {
    lag_col <- paste0("energy_lag", k)

    panel_k <- panel_lags %>%
      mutate(
        log_energy_lag = if_else(!is.na(.data[[lag_col]]) & .data[[lag_col]] > 0,
                                 log(.data[[lag_col]]), NA_real_),
        energy_gasdep_int = log_energy_lag * log1p_gasdep
      )

    data_k <- prepare_05_data(panel_k, mn, common_countries)
    model_k <- feols(get_formula_05(mn), data = data_k, cluster = "country")
    ct_k <- coeftable(model_k)

    if ("log_energy_lag" %in% rownames(ct_k)) {
      est <- ct_k["log_energy_lag", "Estimate"]
      se  <- ct_k["log_energy_lag", "Std. Error"]
      p   <- ct_k["log_energy_lag", "Pr(>|t|)"]
      lag_lines <- c(
        lag_lines,
        sprintf("  lag %d -> coef=%.4f, se=%.4f, p=%.4f, N=%d", k, est, se, p, nobs(model_k))
      )
    } else {
      lag_lines <- c(
        lag_lines,
        sprintf("  lag %d -> main term dropped/omitted, N=%d", k, nobs(model_k))
      )
    }
  }

  lag_lines <- c(lag_lines, "")
}

writeLines(lag_lines, con = "07_robustness/output/03_alt_lags_selected_models.txt")

message("Saved: 07_robustness/output/01_alt_dv_defencegdp.txt")
message("Saved: 07_robustness/output/02_alt_energy_eubrent.txt")
message("Saved: 07_robustness/output/03_alt_lags_selected_models.txt")
