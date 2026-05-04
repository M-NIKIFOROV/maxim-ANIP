# 08_robustness.R
# Split-sample robustness checks for the baseline CRE model.
#
# Splits:
#   1) Gas dependency split (median by country mean gasdep)
#   2) East vs West EU split (explicit country grouping)

source("05_models/05_models_common.R")
library(plm)
library(lmtest)

dir.create("08_robustness/08_output", showWarnings = FALSE, recursive = TRUE)
out_file <- "08_robustness/08_output/08_robustness_full.txt"

sink(out_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("SCRIPT: 08_robustness.R\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output file:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n\n")

write_result_file_08 <- function(file_name, expr) {
  path <- file.path("08_robustness/08_output", file_name)

  sink(path, split = TRUE)
  on.exit(sink(), add = TRUE)

  cat("SCRIPT: 08_robustness.R\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Result file:", normalizePath(path, winslash = "/", mustWork = FALSE), "\n\n")

  eval.parent(substitute(expr))
}

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

run_alt_model_families <- function(panel, common_countries) {
  model_data <- prepare_05_data(panel, "baseline", common_countries)
  pdata <- pdata.frame(model_data, index = c("country", "year"))

  base_formula <- log_def_spend ~
    log_energy_lag + log1p_gasdep + energy_gasdep_int +
    log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats

  cat("====================================================\n")
  cat("ALTERNATIVE MODEL FAMILIES (BASELINE SPEC)\n")
  cat("====================================================\n")
  cat("Countries   :", length(unique(model_data$country)), "\n")
  cat("Observations:", nrow(model_data), "\n")
  cat("Year range  :", min(model_data$year), "-", max(model_data$year), "\n\n")

  # 1) Random effects (GLS-like), estimated as two-way RE.
  re_model <- plm(
    base_formula,
    data = pdata,
    model = "random",
    effect = "twoways",
    random.method = "amemiya"
  )

  cat("------------------------------\n")
  cat("MODEL A: RANDOM EFFECTS (RE)\n")
  cat("------------------------------\n")
  print(summary(re_model))
  cat("\n")

  # 2) FE model with Driscoll-Kraay SE (two-way FE).
  fe_dk_model <- plm(
    base_formula,
    data = pdata,
    model = "within",
    effect = "twoways"
  )

  dk_vcov <- vcovSCC(fe_dk_model, type = "HC1", maxlag = 2)
  fe_dk_ct <- coeftest(fe_dk_model, vcov. = dk_vcov)

  cat("------------------------------\n")
  cat("MODEL B: FE WITH DRISCOLL-KRAAY SE\n")
  cat("------------------------------\n")
  print(summary(fe_dk_model))
  cat("\nDriscoll-Kraay robust coefficient table:\n")
  print(fe_dk_ct)
  cat("\n")

  # 3) Pooled model with Beck-Katz PCSE covariance.
  pcse_model <- plm(
    update(base_formula, . ~ . + factor(year)),
    data = pdata,
    model = "pooling"
  )

  pcse_vcov <- vcovBK(pcse_model, type = "HC1", cluster = "group")
  pcse_ct <- coeftest(pcse_model, vcov. = pcse_vcov)

  cat("------------------------------\n")
  cat("MODEL C: POOLED OLS WITH PCSE (BECK-KATZ VCOV)\n")
  cat("------------------------------\n")
  print(summary(pcse_model))
  cat("\nPCSE robust coefficient table:\n")
  print(pcse_ct)
  cat("\n")

  invisible(list(
    re_model = re_model,
    fe_dk_model = fe_dk_model,
    fe_dk_coeftest = fe_dk_ct,
    pcse_model = pcse_model,
    pcse_coeftest = pcse_ct
  ))
}

run_placebo_health_model <- function(panel, common_countries) {
  panel_health <- read_csv("data/clean/healthspending_cleaned.csv",
                           show_col_types = FALSE) %>%
    mutate(country = tolower(country)) %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = "health_spend"
    ) %>%
    mutate(
      year = as.integer(year),
      health_spend = as.numeric(health_spend)
    ) %>%
    select(country, year, health_spend)

  placebo_data <- panel %>%
    left_join(panel_health, by = c("country", "year")) %>%
    mutate(log_health_spend = if_else(!is.na(health_spend) & health_spend > 0,
                                      log(health_spend), NA_real_)) %>%
    filter(
      country %in% common_countries,
      !is.na(log_health_spend),
      !is.na(log_energy_lag),
      !is.na(log1p_gasdep),
      !is.na(energy_gasdep_int),
      !is.na(log_gdp_pc),
      !is.na(threat),
      !is.na(log_area),
      !is.na(nato),
      !is.na(ideology),
      !is.na(seats),
      !is.na(ideol_seats)
    ) %>%
    group_by(country) %>%
    mutate(
      mean_log_energy_lag = mean(log_energy_lag, na.rm = TRUE),
      mean_log1p_gasdep = mean(log1p_gasdep, na.rm = TRUE),
      mean_energy_gasdep_int = mean(energy_gasdep_int, na.rm = TRUE),
      mean_log_gdp_pc = mean(log_gdp_pc, na.rm = TRUE),
      mean_threat = mean(threat, na.rm = TRUE),
      mean_log_area = mean(log_area, na.rm = TRUE),
      mean_nato = mean(nato, na.rm = TRUE),
      mean_ideology = mean(ideology, na.rm = TRUE),
      mean_seats = mean(seats, na.rm = TRUE),
      mean_ideol_seats = mean(ideol_seats, na.rm = TRUE)
    ) %>%
    ungroup()

  placebo_model <- feols(
    log_health_spend ~
      log_energy_lag + log1p_gasdep + energy_gasdep_int +
      log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
      mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
      mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
      mean_ideology + mean_seats + mean_ideol_seats |
      year,
    data = placebo_data,
    cluster = "country"
  )

  placebo_mundlak <- wald(placebo_model, keep = "^mean_")

  cat("====================================================\n")
  cat("PLACEBO TEST: HEALTH SPENDING AS DEPENDENT VARIABLE\n")
  cat("Baseline CRE specification with defence spending replaced by health spending\n")
  cat("====================================================\n")
  cat("Countries   :", length(unique(placebo_data$country)), "\n")
  cat("Observations:", nrow(placebo_data), "\n")
  cat("Year range  :", min(placebo_data$year), "-", max(placebo_data$year), "\n\n")
  print(summary(placebo_model))
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(placebo_mundlak)
  cat("\n")

  invisible(list(
    model = placebo_model,
    mundlak_test = placebo_mundlak,
    data = placebo_data
  ))
}

extract_term_stats <- function(model, term) {
  ct <- summary(model)$coeftable
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)

  if (!(term %in% rownames(ct)) || length(p_col) == 0) {
    return(c(estimate = NA_real_, p_value = NA_real_))
  }

  c(
    estimate = unname(ct[term, "Estimate"]),
    p_value = unname(ct[term, p_col[1]])
  )
}

build_country_robust_table <- function(panel, countries, reference_model,
                                       split_name) {
  ref_energy <- extract_term_stats(reference_model, "log_energy_lag")
  ref_inter  <- extract_term_stats(reference_model, "energy_gasdep_int")

  rows <- lapply(sort(countries), function(country_drop) {
    keep_countries <- setdiff(countries, country_drop)

    model_loo <- NULL
    fit_error <- NA_character_

    tryCatch(
      {
        suppressWarnings(capture.output({
          model_loo <- run_05_model(panel, "baseline", keep_countries)
        }))
      },
      error = function(e) {
        fit_error <<- conditionMessage(e)
      }
    )

    if (is.null(model_loo)) {
      return(data.frame(
        split = split_name,
        omitted_country = country_drop,
        n_countries = length(keep_countries),
        n_obs = NA_integer_,
        coef_energy = NA_real_,
        p_energy = NA_real_,
        sign_same_energy = FALSE,
        sig10_energy = FALSE,
        coef_interaction = NA_real_,
        p_interaction = NA_real_,
        sign_same_interaction = FALSE,
        sig10_interaction = FALSE,
        robust_flag = FALSE,
        robust_flag_strict = FALSE,
        fit_error = fit_error,
        stringsAsFactors = FALSE
      ))
    }

    loo_energy <- extract_term_stats(model_loo$model, "log_energy_lag")
    loo_inter  <- extract_term_stats(model_loo$model, "energy_gasdep_int")

    sign_same_energy <- !is.na(loo_energy["estimate"]) &&
      sign(loo_energy["estimate"]) == sign(ref_energy["estimate"])
    sign_same_inter <- !is.na(loo_inter["estimate"]) &&
      sign(loo_inter["estimate"]) == sign(ref_inter["estimate"])

    sig10_energy <- !is.na(loo_energy["p_value"]) && loo_energy["p_value"] < 0.10
    sig10_inter <- !is.na(loo_inter["p_value"]) && loo_inter["p_value"] < 0.10

    data.frame(
      split = split_name,
      omitted_country = country_drop,
      n_countries = length(keep_countries),
      n_obs = nrow(model_loo$data),
      coef_energy = as.numeric(loo_energy["estimate"]),
      p_energy = as.numeric(loo_energy["p_value"]),
      sign_same_energy = sign_same_energy,
      sig10_energy = sig10_energy,
      coef_interaction = as.numeric(loo_inter["estimate"]),
      p_interaction = as.numeric(loo_inter["p_value"]),
      sign_same_interaction = sign_same_inter,
      sig10_interaction = sig10_inter,
      robust_flag = sign_same_energy && sig10_energy,
      robust_flag_strict = sign_same_energy && sig10_energy &&
        sign_same_inter && sig10_inter,
      fit_error = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  bind_rows(rows) %>% arrange(p_energy)
}

summarise_loo_significance <- function(tab, ref_model) {
  ref_energy <- extract_term_stats(ref_model, "log_energy_lag")

  valid <- tab[!is.na(tab$p_energy) & !is.na(tab$coef_energy), , drop = FALSE]
  n_total <- nrow(tab)
  n_valid <- nrow(valid)

  if (n_valid == 0) {
    return(list(
      n_total = n_total,
      n_valid = 0L,
      n_sig_10 = 0L,
      n_sig_05 = 0L,
      n_sig_01 = 0L,
      n_negative = 0L,
      n_same_sign = 0L,
      min_p = NA_real_,
      max_p = NA_real_,
      min_beta = NA_real_,
      max_beta = NA_real_,
      ref_beta = as.numeric(ref_energy["estimate"]),
      ref_p = as.numeric(ref_energy["p_value"])
    ))
  }

  list(
    n_total = n_total,
    n_valid = n_valid,
    n_sig_10 = sum(valid$p_energy < 0.10),
    n_sig_05 = sum(valid$p_energy < 0.05),
    n_sig_01 = sum(valid$p_energy < 0.01),
    n_negative = sum(valid$coef_energy < 0),
    n_same_sign = sum(sign(valid$coef_energy) == sign(ref_energy["estimate"])),
    min_p = min(valid$p_energy),
    max_p = max(valid$p_energy),
    min_beta = min(valid$coef_energy),
    max_beta = max(valid$coef_energy),
    ref_beta = as.numeric(ref_energy["estimate"]),
    ref_p = as.numeric(ref_energy["p_value"])
  )
}

cat("====================================================\n")
cat("BASE SAMPLE (05 PIPELINE)\n")
cat("====================================================\n")
cat("Countries:", length(common_countries), "\n")
cat("Country list:\n")
cat(paste(sort(common_countries), collapse = ", "), "\n\n")

cat("------------------------------\n")
cat("BASELINE MODEL: FULL SAMPLE\n")
cat("------------------------------\n")
res_full <- run_05_model(panel, "baseline", common_countries)

alt_models <- run_alt_model_families(panel, common_countries)
placebo_health <- run_placebo_health_model(panel, common_countries)

cat("\n====================================================\n")
cat("COUNTRY ROBUSTNESS TABLE: FULL SAMPLE (LEAVE-ONE-OUT)\n")
cat("Rule: robust_flag = same sign as full sample AND p<0.10 for log_energy_lag\n")
cat("Strict rule adds interaction sign and p<0.10 for energy_gasdep_int\n")
cat("====================================================\n")
tab_full <- build_country_robust_table(panel, common_countries,
                     res_full$model, "full")
print(tab_full, row.names = FALSE)
cat("\nRobust count (main rule):", sum(tab_full$robust_flag, na.rm = TRUE),
  "out of", nrow(tab_full), "\n")
cat("Robust count (strict rule):", sum(tab_full$robust_flag_strict, na.rm = TRUE),
  "out of", nrow(tab_full), "\n\n")

loo_full_sig <- summarise_loo_significance(tab_full, res_full$model)

cat("LOO SIGNIFICANCE SUMMARY (FULL SAMPLE, log_energy_lag)\n")
cat("Total exclusions:", loo_full_sig$n_total, "\n")
cat("Successful re-estimations:", loo_full_sig$n_valid, "\n")
cat("Significant at 10%:", loo_full_sig$n_sig_10, "\n")
cat("Significant at 5%:", loo_full_sig$n_sig_05, "\n")
cat("Significant at 1%:", loo_full_sig$n_sig_01, "\n")
cat("Negative coefficient count:", loo_full_sig$n_negative, "\n")
cat("Same sign as full-sample model:", loo_full_sig$n_same_sign, "\n")
cat("Coefficient range:", round(loo_full_sig$min_beta, 6), "to",
    round(loo_full_sig$max_beta, 6), "\n")
cat("p-value range:", signif(loo_full_sig$min_p, 4), "to",
    signif(loo_full_sig$max_p, 4), "\n\n")

# ---------------------------------------------------------------------------
# Split 1: Gas dependency median split (country-level mean gasdep)
# ---------------------------------------------------------------------------

gasdep_country <- panel %>%
  filter(country %in% common_countries) %>%
  group_by(country) %>%
  summarise(mean_gasdep = mean(gasdep, na.rm = TRUE), .groups = "drop")

median_gasdep <- median(gasdep_country$mean_gasdep, na.rm = TRUE)

low_gasdep_countries <- gasdep_country %>%
  filter(mean_gasdep <= median_gasdep) %>%
  pull(country)

high_gasdep_countries <- gasdep_country %>%
  filter(mean_gasdep > median_gasdep) %>%
  pull(country)

cat("====================================================\n")
cat("SPLIT 1: GASDEP MEDIAN SPLIT\n")
cat("====================================================\n")
cat("Median of country-level mean gasdep:", round(median_gasdep, 3), "\n")
cat("Low gasdep countries (<= median):", length(low_gasdep_countries), "\n")
cat("High gasdep countries (> median):", length(high_gasdep_countries), "\n\n")

cat("Low gasdep country list:\n")
cat(paste(sort(low_gasdep_countries), collapse = ", "), "\n\n")

cat("High gasdep country list:\n")
cat(paste(sort(high_gasdep_countries), collapse = ", "), "\n\n")

cat("------------------------------\n")
cat("BASELINE MODEL: LOW GASDEP\n")
cat("------------------------------\n")
res_low_gasdep <- run_05_model(panel, "baseline", low_gasdep_countries)

cat("\n------------------------------\n")
cat("BASELINE MODEL: HIGH GASDEP\n")
cat("------------------------------\n")
res_high_gasdep <- run_05_model(panel, "baseline", high_gasdep_countries)

cat("\n====================================================\n")
cat("COUNTRY ROBUSTNESS TABLE: LOW GASDEP (LEAVE-ONE-OUT)\n")
cat("====================================================\n")
tab_low <- build_country_robust_table(panel, low_gasdep_countries,
                    res_low_gasdep$model, "low_gasdep")
print(tab_low, row.names = FALSE)
cat("\nRobust count (main rule):", sum(tab_low$robust_flag, na.rm = TRUE),
  "out of", nrow(tab_low), "\n")
cat("Robust count (strict rule):", sum(tab_low$robust_flag_strict, na.rm = TRUE),
  "out of", nrow(tab_low), "\n\n")

cat("====================================================\n")
cat("COUNTRY ROBUSTNESS TABLE: HIGH GASDEP (LEAVE-ONE-OUT)\n")
cat("====================================================\n")
tab_high <- build_country_robust_table(panel, high_gasdep_countries,
                     res_high_gasdep$model, "high_gasdep")
print(tab_high, row.names = FALSE)
cat("\nRobust count (main rule):", sum(tab_high$robust_flag, na.rm = TRUE),
  "out of", nrow(tab_high), "\n")
cat("Robust count (strict rule):", sum(tab_high$robust_flag_strict, na.rm = TRUE),
  "out of", nrow(tab_high), "\n\n")

# ---------------------------------------------------------------------------
# Split 2: East vs West split
# ---------------------------------------------------------------------------

# East definition used here: Bulgaria, Croatia, Estonia, Hungary,
# Latvia, Lithuania, Poland, Romania, Slovenia.
# Remaining countries in the common 05 sample are treated as West.

east_countries <- c(
  "bulgaria", "croatia", "estonia", "hungary", "latvia",
  "lithuania", "poland", "romania", "slovenia"
)

east_countries <- sort(intersect(east_countries, common_countries))
west_countries <- sort(setdiff(common_countries, east_countries))

cat("\n====================================================\n")
cat("SPLIT 2: EAST VS WEST\n")
cat("====================================================\n")
cat("East countries:", length(east_countries), "\n")
cat("West countries:", length(west_countries), "\n\n")

cat("East country list:\n")
cat(paste(east_countries, collapse = ", "), "\n\n")

cat("West country list:\n")
cat(paste(west_countries, collapse = ", "), "\n\n")

cat("------------------------------\n")
cat("BASELINE MODEL: EAST\n")
cat("------------------------------\n")
res_east <- run_05_model(panel, "baseline", east_countries)

cat("\n------------------------------\n")
cat("BASELINE MODEL: WEST\n")
cat("------------------------------\n")
res_west <- run_05_model(panel, "baseline", west_countries)

cat("\n====================================================\n")
cat("COUNTRY ROBUSTNESS TABLE: EAST (LEAVE-ONE-OUT)\n")
cat("====================================================\n")
tab_east <- build_country_robust_table(panel, east_countries,
                                       res_east$model, "east")
print(tab_east, row.names = FALSE)
cat("\nRobust count (main rule):", sum(tab_east$robust_flag, na.rm = TRUE),
    "out of", nrow(tab_east), "\n")
cat("Robust count (strict rule):", sum(tab_east$robust_flag_strict, na.rm = TRUE),
    "out of", nrow(tab_east), "\n\n")

cat("====================================================\n")
cat("COUNTRY ROBUSTNESS TABLE: WEST (LEAVE-ONE-OUT)\n")
cat("====================================================\n")
tab_west <- build_country_robust_table(panel, west_countries,
                                       res_west$model, "west")
print(tab_west, row.names = FALSE)
cat("\nRobust count (main rule):", sum(tab_west$robust_flag, na.rm = TRUE),
    "out of", nrow(tab_west), "\n")
cat("Robust count (strict rule):", sum(tab_west$robust_flag_strict, na.rm = TRUE),
    "out of", nrow(tab_west), "\n\n")

write_result_file_08("08_01_baseline_full_model.txt", {
  cat("BASELINE MODEL: FULL SAMPLE\n")
  cat("Countries   :", length(unique(res_full$data$country)), "\n")
  cat("Observations:", nrow(res_full$data), "\n")
  cat("Year range  :", min(res_full$data$year), "-", max(res_full$data$year), "\n\n")
  print(summary(res_full$model))
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(res_full$mundlak_test)
})

write_result_file_08("08_02_model_re.txt", {
  cat("ALTERNATIVE MODEL FAMILY\n")
  cat("MODEL A: RANDOM EFFECTS (RE)\n\n")
  print(summary(alt_models$re_model))
})

write_result_file_08("08_03_model_fe_driscoll_kraay.txt", {
  cat("ALTERNATIVE MODEL FAMILY\n")
  cat("MODEL B: FE WITH DRISCOLL-KRAAY SE\n\n")
  print(summary(alt_models$fe_dk_model))
  cat("\nDriscoll-Kraay robust coefficient table:\n")
  print(alt_models$fe_dk_coeftest)
})

write_result_file_08("08_04_model_pcse.txt", {
  cat("ALTERNATIVE MODEL FAMILY\n")
  cat("MODEL C: POOLED OLS WITH PCSE (BECK-KATZ VCOV)\n\n")
  print(summary(alt_models$pcse_model))
  cat("\nPCSE robust coefficient table:\n")
  print(alt_models$pcse_coeftest)
})

write_result_file_08("08_05_placebo_health_model.txt", {
  cat("PLACEBO TEST: HEALTH SPENDING AS DEPENDENT VARIABLE\n")
  cat("Countries   :", length(unique(placebo_health$data$country)), "\n")
  cat("Observations:", nrow(placebo_health$data), "\n")
  cat("Year range  :", min(placebo_health$data$year), "-", max(placebo_health$data$year), "\n\n")
  print(summary(placebo_health$model))
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(placebo_health$mundlak_test)
})

write_result_file_08("08_06_country_robust_full.txt", {
  cat("COUNTRY ROBUSTNESS TABLE: FULL SAMPLE (LEAVE-ONE-OUT)\n")
  print(tab_full, row.names = FALSE)
  cat("\nRobust count (main rule):", sum(tab_full$robust_flag, na.rm = TRUE),
      "out of", nrow(tab_full), "\n")
  cat("Robust count (strict rule):", sum(tab_full$robust_flag_strict, na.rm = TRUE),
      "out of", nrow(tab_full), "\n")
})

write_result_file_08("08_07_baseline_low_gasdep_model.txt", {
  cat("BASELINE MODEL: LOW GASDEP\n")
  print(summary(res_low_gasdep$model))
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(res_low_gasdep$mundlak_test)
})

write_result_file_08("08_08_baseline_high_gasdep_model.txt", {
  cat("BASELINE MODEL: HIGH GASDEP\n")
  print(summary(res_high_gasdep$model))
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(res_high_gasdep$mundlak_test)
})

write_result_file_08("08_09_country_robust_low_gasdep.txt", {
  cat("COUNTRY ROBUSTNESS TABLE: LOW GASDEP (LEAVE-ONE-OUT)\n")
  print(tab_low, row.names = FALSE)
  cat("\nRobust count (main rule):", sum(tab_low$robust_flag, na.rm = TRUE),
      "out of", nrow(tab_low), "\n")
  cat("Robust count (strict rule):", sum(tab_low$robust_flag_strict, na.rm = TRUE),
      "out of", nrow(tab_low), "\n")
})

write_result_file_08("08_10_country_robust_high_gasdep.txt", {
  cat("COUNTRY ROBUSTNESS TABLE: HIGH GASDEP (LEAVE-ONE-OUT)\n")
  print(tab_high, row.names = FALSE)
  cat("\nRobust count (main rule):", sum(tab_high$robust_flag, na.rm = TRUE),
      "out of", nrow(tab_high), "\n")
  cat("Robust count (strict rule):", sum(tab_high$robust_flag_strict, na.rm = TRUE),
      "out of", nrow(tab_high), "\n")
})

write_result_file_08("08_11_baseline_east_model.txt", {
  cat("BASELINE MODEL: EAST\n")
  print(summary(res_east$model))
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(res_east$mundlak_test)
})

write_result_file_08("08_12_baseline_west_model.txt", {
  cat("BASELINE MODEL: WEST\n")
  print(summary(res_west$model))
  cat("\nJoint Mundlak test (all mean_* terms = 0):\n")
  print(res_west$mundlak_test)
})

write_result_file_08("08_13_country_robust_east.txt", {
  cat("COUNTRY ROBUSTNESS TABLE: EAST (LEAVE-ONE-OUT)\n")
  print(tab_east, row.names = FALSE)
  cat("\nRobust count (main rule):", sum(tab_east$robust_flag, na.rm = TRUE),
      "out of", nrow(tab_east), "\n")
  cat("Robust count (strict rule):", sum(tab_east$robust_flag_strict, na.rm = TRUE),
      "out of", nrow(tab_east), "\n")
})

write_result_file_08("08_14_country_robust_west.txt", {
  cat("COUNTRY ROBUSTNESS TABLE: WEST (LEAVE-ONE-OUT)\n")
  print(tab_west, row.names = FALSE)
  cat("\nRobust count (main rule):", sum(tab_west$robust_flag, na.rm = TRUE),
      "out of", nrow(tab_west), "\n")
  cat("Robust count (strict rule):", sum(tab_west$robust_flag_strict, na.rm = TRUE),
      "out of", nrow(tab_west), "\n")
})

write_result_file_08("08_15_loo_significance_summary.txt", {
  cat("LEAVE-ONE-OUT COUNTRY EXCLUSION TEST\n")
  cat("Target coefficient: log_energy_lag\n\n")

  cat("Full-sample benchmark (baseline CRE):\n")
  cat("  beta =", round(loo_full_sig$ref_beta, 6),
    " | p =", signif(loo_full_sig$ref_p, 4), "\n\n")

  cat("LOO summary over omitted-country re-estimations:\n")
  cat("  Total exclusions:", loo_full_sig$n_total, "\n")
  cat("  Successful fits:", loo_full_sig$n_valid, "\n")
  cat("  Significant at 10%:", loo_full_sig$n_sig_10, "\n")
  cat("  Significant at 5%:", loo_full_sig$n_sig_05, "\n")
  cat("  Significant at 1%:", loo_full_sig$n_sig_01, "\n")
  cat("  Negative coefficient count:", loo_full_sig$n_negative, "\n")
  cat("  Same sign as benchmark:", loo_full_sig$n_same_sign, "\n")
  cat("  Coefficient range:", round(loo_full_sig$min_beta, 6), "to",
    round(loo_full_sig$max_beta, 6), "\n")
  cat("  p-value range:", signif(loo_full_sig$min_p, 4), "to",
    signif(loo_full_sig$max_p, 4), "\n")
})

message("Saved: 08_robustness/08_output/08_01_baseline_full_model.txt")
message("Saved: 08_robustness/08_output/08_02_model_re.txt")
message("Saved: 08_robustness/08_output/08_03_model_fe_driscoll_kraay.txt")
message("Saved: 08_robustness/08_output/08_04_model_pcse.txt")
message("Saved: 08_robustness/08_output/08_05_placebo_health_model.txt")
message("Saved: 08_robustness/08_output/08_06_country_robust_full.txt")
message("Saved: 08_robustness/08_output/08_07_baseline_low_gasdep_model.txt")
message("Saved: 08_robustness/08_output/08_08_baseline_high_gasdep_model.txt")
message("Saved: 08_robustness/08_output/08_09_country_robust_low_gasdep.txt")
message("Saved: 08_robustness/08_output/08_10_country_robust_high_gasdep.txt")
message("Saved: 08_robustness/08_output/08_11_baseline_east_model.txt")
message("Saved: 08_robustness/08_output/08_12_baseline_west_model.txt")
message("Saved: 08_robustness/08_output/08_13_country_robust_east.txt")
message("Saved: 08_robustness/08_output/08_14_country_robust_west.txt")
message("Saved: 08_robustness/08_output/08_15_loo_significance_summary.txt")

invisible(list(
  full = list(result = res_full, table = tab_full),
  alternative_models = alt_models,
  placebo_health = placebo_health,
  gasdep_split = list(low = res_low_gasdep, high = res_high_gasdep),
  east_west_split = list(east = res_east, west = res_west),
  tables = list(
    low_gasdep = tab_low,
    high_gasdep = tab_high,
    east = tab_east,
    west = tab_west
  )
))
