# 09_images.R
# Generate appendix images: energy prices and defence spending time series
# by East/West and Low/High gas dependency splits.

source("05_models/05_models_common.R")

library(dplyr)
library(tidyr)
library(ggplot2)

out_dir <- "09_images"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

panel <- build_05_panel()
common_countries <- get_common_countries(panel)

# East definition follows 08_robustness.R
core_east <- c(
  "bulgaria", "croatia", "estonia", "hungary", "latvia",
  "lithuania", "poland", "romania", "slovenia"
)

east_countries <- sort(intersect(core_east, common_countries))
west_countries <- sort(setdiff(common_countries, east_countries))

# Gas dependency split follows 08_robustness.R (median of country means)
gasdep_country <- panel %>%
  filter(country %in% common_countries) %>%
  group_by(country) %>%
  summarise(mean_gasdep = mean(gasdep, na.rm = TRUE), .groups = "drop")

median_gasdep <- median(gasdep_country$mean_gasdep, na.rm = TRUE)

low_gasdep_countries <- gasdep_country %>%
  filter(mean_gasdep <= median_gasdep) %>%
  pull(country) %>%
  sort()

high_gasdep_countries <- gasdep_country %>%
  filter(mean_gasdep > median_gasdep) %>%
  pull(country) %>%
  sort()

# Four requested groups
groups <- list(
  west_low_gas = intersect(west_countries, low_gasdep_countries),
  east_low_gas = intersect(east_countries, low_gasdep_countries),
  west_high_gas = intersect(west_countries, high_gasdep_countries),
  east_high_gas = intersect(east_countries, high_gasdep_countries)
)

group_titles <- c(
  west_low_gas = "West + Low Gas Dependency",
  east_low_gas = "East + Low Gas Dependency",
  west_high_gas = "West + High Gas Dependency",
  east_high_gas = "East + High Gas Dependency"
)

first_non_na <- function(x) {
  idx <- match(TRUE, !is.na(x))
  if (is.na(idx)) {
    return(NA_real_)
  }
  as.numeric(x[idx])
}

build_plot_data <- function(countries) {
  raw <- panel %>%
    filter(country %in% countries) %>%
    select(country, year, energy_price, def_spend) %>%
    arrange(country, year)

  bases <- raw %>%
    group_by(country) %>%
    summarise(
      base_energy = first_non_na(energy_price),
      base_def = first_non_na(def_spend),
      .groups = "drop"
    )

  raw %>%
    left_join(bases, by = "country") %>%
    mutate(
      energy_index = ifelse(!is.na(base_energy) & base_energy > 0,
                            100 * energy_price / base_energy, NA_real_),
      def_index = ifelse(!is.na(base_def) & base_def > 0,
                         100 * def_spend / base_def, NA_real_)
    ) %>%
    select(country, year, energy_index, def_index) %>%
    pivot_longer(
      cols = c(energy_index, def_index),
      names_to = "series",
      values_to = "index_value"
    ) %>%
    mutate(
      series = recode(series,
                      energy_index = "Energy price index",
                      def_index = "Defence spending index")
    ) %>%
    filter(!is.na(index_value), is.finite(index_value))
}

plot_group <- function(group_key, countries) {
  if (length(countries) == 0) {
    message("Skipping ", group_key, " (no countries)")
    return(invisible(NULL))
  }

  pdata <- build_plot_data(countries)

  p <- ggplot(pdata, aes(x = year, y = index_value, color = series)) +
    geom_line(linewidth = 0.6, alpha = 0.9) +
    facet_wrap(~ country, ncol = 4) +
    scale_color_manual(values = c(
      "Energy price index" = "#C0392B",
      "Defence spending index" = "#1F4E79"
    )) +
    labs(
      title = paste0(group_titles[[group_key]], ": Energy and Defence Time Series"),
      subtitle = "Indexed to first available country-year = 100",
      x = "Year",
      y = "Index (base = 100)",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  out_path <- file.path(out_dir, paste0(group_key, "_energy_defence_timeseries.png"))
  ggsave(out_path, p, width = 14, height = 9, dpi = 300)
  message("Saved: ", out_path)
}

for (k in names(groups)) {
  plot_group(k, groups[[k]])
}

# First-difference diagnostic: within-country changes in logs.
# This better aligns with FE-style identifying variation than indexed levels.
plot_fd_diagnostic <- function(group_key, countries) {
  if (length(countries) == 0) {
    message("Skipping ", group_key, " FD plot (no countries)")
    return(invisible(NULL))
  }

  fd_data <- panel %>%
    filter(country %in% countries) %>%
    select(country, year, log_energy_lag, log_def_spend) %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      d_log_energy_lag = log_energy_lag - lag(log_energy_lag),
      d_log_def_spend = log_def_spend - lag(log_def_spend)
    ) %>%
    ungroup() %>%
    filter(
      !is.na(d_log_energy_lag),
      !is.na(d_log_def_spend),
      is.finite(d_log_energy_lag),
      is.finite(d_log_def_spend)
    )

  if (nrow(fd_data) == 0) {
    message("Skipping ", group_key, " FD plot (no complete first-difference rows)")
    return(invisible(NULL))
  }

  p_fd <- ggplot(fd_data, aes(x = d_log_energy_lag, y = d_log_def_spend, color = country)) +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "#6E6E6E") +
    geom_vline(xintercept = 0, linewidth = 0.3, color = "#6E6E6E") +
    geom_point(alpha = 0.85, size = 2.2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "#111111") +
    labs(
      title = paste0(group_titles[[group_key]], ": First-Difference Diagnostic"),
      subtitle = "Within-country annual changes in log energy (lagged) vs log defence spending",
      x = "Delta log(energy_lag)",
      y = "Delta log(defence spending)",
      color = "Country"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )

  out_path_fd <- file.path(out_dir, paste0(group_key, "_fd_scatter_energy_defence.png"))
  ggsave(out_path_fd, p_fd, width = 10, height = 7, dpi = 300)
  message("Saved: ", out_path_fd)
}

for (k in names(groups)) {
  plot_fd_diagnostic(k, groups[[k]])
}

cat("\nDone. Generated files in ", normalizePath(out_dir, winslash = "/"), "\n", sep = "")
