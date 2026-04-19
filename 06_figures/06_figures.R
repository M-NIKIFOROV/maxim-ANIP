# 06_figures.R
# Produces:
#   (b) Line chart: avg defence spending vs avg energy prices (full timeline)
#   (c) Line chart: avg defence spending vs avg gasdep – Eurostat (gas) and
#                   Eurostat (net energy dependency) shown as separate series
#   (d) Table: baseline CRE model – energy coef + country×energy interactions
#              for Ireland, Bulgaria, and Slovakia

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(fixest)
library(gt)
library(sf)
library(rnaturalearth)

source("01_cleaning/definitions.R")
source("05_models/05_models_common.R")

dir.create("06_figures/output", showWarnings = FALSE, recursive = TRUE)

# ANU-inspired high-contrast palette for white-background outputs
anu_black <- "#000000"
anu_gold  <- "#BE830E"
anu_blue  <- "#003B5C"
anu_teal  <- "#007A87"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

pivot_yv <- function(df, value_name) {
  df %>%
    pivot_longer(cols = matches("^[0-9]{4}$"),
                 names_to  = "year",
                 values_to = value_name) %>%
    mutate(year = as.integer(year))
}

sig_stars <- function(p) {
  case_when(
    p < 0.01  ~ "***",
    p < 0.05  ~ "**",
    p < 0.10  ~ "*",
    TRUE      ~ ""
  )
}

# ---------------------------------------------------------------------------
# Load series
# ---------------------------------------------------------------------------

# Defence spending
defence_long <- read_csv("data/clean/defence_cleaned.csv", show_col_types = FALSE) %>%
  rename(country = Country) %>%
  mutate(country = tolower(country)) %>%
  pivot_yv("def_spend") %>%
  mutate(def_spend = as.numeric(def_spend)) %>%
  filter(country %in% eu)

# Energy prices (semi-annual → annual mean)
energy_long <- read_csv("data/clean/energy_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  pivot_yv("energy_price") %>%
  mutate(energy_price = as.numeric(energy_price)) %>%
  filter(country %in% eu) %>%
  group_by(country, year) %>%
  summarise(energy_price = mean(energy_price, na.rm = TRUE), .groups = "drop") %>%
  mutate(energy_price = if_else(is.nan(energy_price), NA_real_, energy_price))

# Gas dependency – Eurostat (IEA cleaned = gasdep_cleaned, gas supply)
gasdep_long <- read_csv("data/clean/gasdep_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  select(country, matches("^[0-9]{4}$")) %>%
  mutate(across(matches("^[0-9]{4}$"), as.character)) %>%
  pivot_yv("gasdep") %>%
  mutate(gasdep = na_if(gasdep, ".."),
         gasdep = na_if(gasdep, ""),
         gasdep = as.numeric(gasdep)) %>%
  filter(country %in% eu)

# Net energy dependency – Eurostat (nrgdep_cleaned)
nrgdep_long <- read_csv("data/clean/nrgdep_cleaned.csv", show_col_types = FALSE) %>%
  mutate(country = tolower(country)) %>%
  select(country, matches("^[0-9]{4}$")) %>%
  mutate(across(matches("^[0-9]{4}$"), as.character)) %>%
  pivot_yv("nrgdep") %>%
  mutate(nrgdep = na_if(nrgdep, ".."),
         nrgdep = na_if(nrgdep, ""),
         nrgdep = as.numeric(nrgdep)) %>%
  filter(country %in% eu)

# ---------------------------------------------------------------------------
# Compute EU averages by year
# ---------------------------------------------------------------------------

avg_defence <- defence_long %>%
  group_by(year) %>%
  summarise(avg_def = mean(def_spend, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(avg_def))

avg_energy <- energy_long %>%
  group_by(year) %>%
  summarise(avg_energy = mean(energy_price, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(avg_energy))

avg_gasdep <- gasdep_long %>%
  group_by(year) %>%
  summarise(avg_gasdep = mean(gasdep, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(avg_gasdep))

avg_nrgdep <- nrgdep_long %>%
  group_by(year) %>%
  summarise(avg_nrgdep = mean(nrgdep, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(avg_nrgdep))

# ---------------------------------------------------------------------------
# (b) Line chart: avg defence spending vs avg energy prices
# ---------------------------------------------------------------------------

be_data <- full_join(avg_defence, avg_energy, by = "year") %>%
  filter(!is.na(avg_def) & !is.na(avg_energy))

# Scale energy to defence axis for dual-axis display
def_range  <- range(be_data$avg_def,    na.rm = TRUE)
eng_range  <- range(be_data$avg_energy, na.rm = TRUE)

scale_eng <- function(x) {
  (x - eng_range[1]) / diff(eng_range) * diff(def_range) + def_range[1]
}
inv_eng <- function(x) {
  (x - def_range[1]) / diff(def_range) * diff(eng_range) + eng_range[1]
}

fig_b <- full_join(avg_defence, avg_energy, by = "year") %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_def, colour = "Avg defence spending (USD mn)"),
            linewidth = 1.2, na.rm = TRUE) +
  geom_line(aes(y = scale_eng(avg_energy), colour = "Avg energy price (EUR/kWh)"),
            linewidth = 1.2, linetype = "dashed", na.rm = TRUE) +
  scale_y_continuous(
    name = "Avg defence spending (USD mn)",
    labels = comma,
    sec.axis = sec_axis(~ inv_eng(.),
                        name   = "Avg energy price (EUR/kWh)",
                        labels = number_format(accuracy = 0.01))
  ) +
  scale_colour_manual(
      values = c("Avg defence spending (USD mn)" = anu_blue,
         "Avg energy price (EUR/kWh)"    = anu_gold)
  ) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
  labs(title   = "Average EU Defence Spending and Energy Prices",
       x       = NULL,
       colour  = NULL) +
    theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "#D9D9D9", linewidth = 0.4),
        panel.grid.minor = element_blank())

ggsave("06_figures/output/fig_b_defence_energy.png",
       fig_b, width = 9, height = 5, dpi = 300)

message("Saved: fig_b_defence_energy.png")

# ---------------------------------------------------------------------------
# (c) Line chart: avg defence spending vs avg gasdep (two series)
# ---------------------------------------------------------------------------

# Combine both dependency series
dep_combined <- bind_rows(
  avg_gasdep  %>% rename(dep_value = avg_gasdep)  %>% mutate(source = "Gas dependency (Eurostat)"),
  avg_nrgdep  %>% rename(dep_value = avg_nrgdep)  %>% mutate(source = "Net energy dependency (Eurostat)")
)

all_dep_range <- range(dep_combined$dep_value, na.rm = TRUE)

scale_dep <- function(x) {
  def_data <- avg_defence %>% filter(!is.na(avg_def))
  dr <- range(def_data$avg_def, na.rm = TRUE)
  (x - all_dep_range[1]) / diff(all_dep_range) * diff(dr) + dr[1]
}
inv_dep <- function(x) {
  def_data <- avg_defence %>% filter(!is.na(avg_def))
  dr <- range(def_data$avg_def, na.rm = TRUE)
  (x - dr[1]) / diff(dr) * diff(all_dep_range) + all_dep_range[1]
}

def_range_c <- range(avg_defence$avg_def, na.rm = TRUE)

fig_c_data <- avg_defence %>%
  full_join(avg_gasdep  %>% rename(gasdep_val  = avg_gasdep),  by = "year") %>%
  full_join(avg_nrgdep  %>% rename(nrgdep_val  = avg_nrgdep),  by = "year")

scale_dep2 <- function(x) {
  (x - all_dep_range[1]) / diff(all_dep_range) * diff(def_range_c) + def_range_c[1]
}
inv_dep2 <- function(x) {
  (x - def_range_c[1]) / diff(def_range_c) * diff(all_dep_range) + all_dep_range[1]
}

fig_c <- fig_c_data %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_def,
                colour = "Avg defence spending (USD mn)"),
            linewidth = 1.2, na.rm = TRUE) +
  geom_line(aes(y = scale_dep2(gasdep_val),
                colour = "Gas dependency (Eurostat)"),
            linewidth = 1.2, linetype = "dashed", na.rm = TRUE) +
  geom_line(aes(y = scale_dep2(nrgdep_val),
                colour = "Net energy dependency (Eurostat)"),
            linewidth = 1.2, linetype = "dotted", na.rm = TRUE) +
  scale_y_continuous(
    name = "Avg defence spending (USD mn)",
    labels = comma,
    sec.axis = sec_axis(~ inv_dep2(.),
                        name = "Energy dependency (%)",
                        labels = number_format(accuracy = 0.1))
  ) +
  scale_colour_manual(
    values = c(
      "Avg defence spending (USD mn)"      = anu_blue,
      "Gas dependency (Eurostat)"          = anu_teal,
      "Net energy dependency (Eurostat)"   = anu_gold
    )
  ) +
  scale_x_continuous(breaks = seq(1970, 2025, by = 5)) +
  labs(title  = "Average EU Defence Spending and Energy Dependency",
       x      = NULL,
       colour = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "#D9D9D9", linewidth = 0.4),
        panel.grid.minor = element_blank())

ggsave("06_figures/output/fig_c_defence_gasdep.png",
       fig_c, width = 9, height = 5, dpi = 300)

message("Saved: fig_c_defence_gasdep.png")

# ---------------------------------------------------------------------------
# (e1) Map: Defence spending, Europe 2023
# (e2) Map: Energy prices, Europe 2023
# ---------------------------------------------------------------------------

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)

def_2023 <- defence_long %>%
  filter(year == 2023, country %in% common_countries, !is.na(def_spend))

energy_2023 <- energy_long %>%
  filter(year == 2023, country %in% common_countries, !is.na(energy_price))

iso_map <- c(iso, gr = "greece")

europe_base <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf") %>%
  mutate(
    iso2    = tolower(iso_a2),
    country = if_else(iso2 %in% names(iso_map), unname(iso_map[iso2]), tolower(admin)),
    country = recode(country, "czech republic" = "czechia", .default = country)
  )

map_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank(),
    legend.position  = "right",
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank()
  )

# --- (e1) Defence spending ---

def_map <- europe_base %>%
  left_join(def_2023, by = "country")

eu_def_map <- def_map %>% filter(country %in% def_2023$country)

fig_e1 <- ggplot() +
  geom_sf(data = europe_base, fill = "#F5F5F5", color = "#CCCCCC", linewidth = 0.25) +
  geom_sf(data = eu_def_map, aes(fill = def_spend), color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low  = "#FDF3DC",
    high = anu_gold,
    name = "Defence spending\n(USD mn, 2023)",
    labels = comma,
    na.value = "#F5F5F5"
  ) +
  labs(title = "Europe 2023: Defence Spending") +
  coord_sf(xlim = c(-12, 35), ylim = c(34, 72), expand = FALSE) +
  map_theme

ggsave("06_figures/output/fig_e1_defence_map_2023.png",
       fig_e1, width = 10, height = 7, dpi = 320, bg = "white")
message("Saved: fig_e1_defence_map_2023.png")

# --- (e2) Energy prices ---

energy_map <- europe_base %>%
  left_join(energy_2023, by = "country")

eu_energy_map <- energy_map %>% filter(country %in% energy_2023$country)

fig_e2 <- ggplot() +
  geom_sf(data = europe_base, fill = "#F5F5F5", color = "#CCCCCC", linewidth = 0.25) +
  geom_sf(data = eu_energy_map, aes(fill = energy_price), color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low  = "#FDF3DC",
    high = anu_gold,
    name = "Energy price\n(EUR/kWh, 2023)",
    labels = number_format(accuracy = 0.001),
    na.value = "#F5F5F5"
  ) +
  labs(title = "Europe 2023: Energy Prices") +
  coord_sf(xlim = c(-12, 35), ylim = c(34, 72), expand = FALSE) +
  map_theme

ggsave("06_figures/output/fig_e2_energy_map_2023.png",
       fig_e2, width = 10, height = 7, dpi = 320, bg = "white")
message("Saved: fig_e2_energy_map_2023.png")

# ---------------------------------------------------------------------------
# (d) Table: baseline CRE model selected coefficients
# ---------------------------------------------------------------------------

panel            <- build_05_panel()
common_countries <- get_common_countries(panel)
model_data       <- prepare_05_data(panel, "baseline", common_countries)

# Plain baseline model (for the single overall energy coefficient)
fml_baseline <- log_def_spend ~
  log_energy_lag + log1p_gasdep + energy_gasdep_int +
  log_gdp_pc + threat + log_area + nato + ideology + seats + ideol_seats +
  mean_log_energy_lag + mean_log1p_gasdep + mean_energy_gasdep_int +
  mean_log_gdp_pc + mean_threat + mean_log_area + mean_nato +
  mean_ideology + mean_seats + mean_ideol_seats |
  year

m_base <- feols(fml_baseline, data = model_data, cluster = "country")
ct_base <- coeftable(m_base)

n_obs  <- nobs(m_base)
r2_w   <- fitstat(m_base, "wr2")[[1]]

extract_row <- function(ct, varname, label) {
  if (!varname %in% rownames(ct)) {
    return(tibble(Term = label, Estimate = NA_real_, SE = NA_real_,
                  Stars = "", `Coef (SE)` = "—"))
  }
  est <- ct[varname, "Estimate"]
  se  <- ct[varname, "Std. Error"]
  pv  <- ct[varname, "Pr(>|t|)"]
  tibble(
    Term       = label,
    Estimate   = est,
    SE         = se,
    Stars      = sig_stars(pv),
    `Coef (SE)` = sprintf("%.4f (%.4f)%s", est, se, sig_stars(pv))
  )
}

rows <- bind_rows(
  extract_row(ct_base, "log_energy_lag",                 "Energy price (t−1)"),
  extract_row(ct_base, "mean_log_energy_lag",            "Mundlak mean energy price"),
  extract_row(ct_base, "log_gdp_pc",                     "GDP per capita (log)"),
  extract_row(ct_base, "mean_log_gdp_pc",                "Mundlak mean GDP per capita")
)

tbl_d <- rows %>%
  select(Term, `Coef (SE)`) %>%
  gt() %>%
  tab_header(
    title    = "CRE Baseline: Energy Price Effects",
    subtitle = "Selected significant coefficients from the baseline CRE model"
  ) %>%
  tab_source_note(
    source_note = md(
      glue::glue("N = {n_obs}; Within R² = {round(r2_w, 4)}.  ",
                 "SE clustered by country. * p<0.10  ** p<0.05  *** p<0.01")
    )
  ) %>%
  cols_label(Term = "Term", `Coef (SE)` = "Coef. (SE)") %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_options(table.width = pct(60))

gtsave(tbl_d, "06_figures/output/tbl_d_baseline_energy.html")
message("Saved: tbl_d_baseline_energy.html")
