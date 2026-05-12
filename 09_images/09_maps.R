# 09_maps.R
# Generate two maps of Europe showing:
# 1. East/West EU division with a legend key
# 2. High/Low gas dependency division with a median-threshold legend key

source("05_models/05_models_common.R")
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)

out_dir <- "09_images"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

panel <- build_05_panel()
common_countries <- get_common_countries(panel)

# =========================================================================
# Define country groupings
# =========================================================================

core_east <- c(
  "bulgaria", "croatia", "estonia", "hungary", "latvia",
  "lithuania", "poland", "romania", "slovenia"
)

east_countries <- sort(intersect(core_east, common_countries))
west_countries <- sort(setdiff(common_countries, east_countries))

# Gas dependency split
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

cat("East countries (n=", length(east_countries), "):\n", sep = "")
print(east_countries)
cat("\nWest countries (n=", length(west_countries), "):\n", sep = "")
print(west_countries)

cat("\nLow gas-dep countries (n=", length(low_gasdep_countries), "):\n", sep = "")
print(low_gasdep_countries)
cat("\nHigh gas-dep countries (n=", length(high_gasdep_countries), "):\n", sep = "")
print(high_gasdep_countries)

# =========================================================================
# Load country map data
# =========================================================================

world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter to Europe and standardize country names
europe <- world %>%
  filter(continent == "Europe") %>%
  mutate(country_lower = tolower(name))

europe <- europe %>%
  mutate(country_lower = case_when(
    country_lower == "czechia" ~ "czech republic",
    country_lower == "republic of serbia" ~ "serbia",
    TRUE ~ country_lower
  ))

# Add grouping columns
europe <- europe %>%
  mutate(
    east_west = case_when(
      country_lower %in% east_countries ~ "East",
      country_lower %in% west_countries ~ "West",
      TRUE ~ NA_character_
    ),
    gas_dep = case_when(
      country_lower %in% low_gasdep_countries ~ "Low",
      country_lower %in% high_gasdep_countries ~ "High",
      TRUE ~ NA_character_
    ),
    in_sample = !is.na(east_west) & !is.na(gas_dep)
  )

# =========================================================================
# Map 1: East/West with Chow test
# =========================================================================

map_east_west <- function() {
  # Filter to European region + buffer
  bbox <- c(xmin = -10, ymin = 35, xmax = 45, ymax = 70)
  europe_region <- st_crop(europe, bbox)

  europe_region <- europe_region %>%
    mutate(
      map_group = case_when(
        !in_sample ~ "Outside Sample",
        east_west == "East" ~ "East (coef = -0.283)",
        east_west == "West" ~ "West (coef = -0.115)",
        TRUE ~ "Outside Sample"
      ),
      map_group = factor(
        map_group,
        levels = c("West (coef = -0.115)", "East (coef = -0.283)", "Outside Sample")
      )
    )

  p <- ggplot(europe_region) +
    geom_sf(aes(fill = map_group), color = "#808080", linewidth = 0.25, alpha = 0.9) +
    scale_fill_manual(
      name = "Country Group",
      values = c(
        "West (coef = -0.115)" = "#4472C4",
        "East (coef = -0.283)" = "#ED7D31",
        "Outside Sample" = "#F5F5F5"
      )
    ) +
    labs(
      title = "East vs. West EU Division",
      subtitle = "Chow test: F(20,335) = 79.41, p < 0.001",
      caption = "Split definition: West = 15 countries, East = 9 countries"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    coord_sf(expand = FALSE)

  out_path <- file.path(out_dir, "map_east_west_chow.png")
  ggsave(out_path, p, width = 14, height = 10, dpi = 300)
  message("Saved: ", out_path)
}

# =========================================================================
# Map 2: High/Low Gas Dependency with Chow test
# =========================================================================

map_gas_dependency <- function() {
  bbox <- c(xmin = -10, ymin = 35, xmax = 45, ymax = 70)
  europe_region <- st_crop(europe, bbox)

  europe_region <- europe_region %>%
    mutate(
      map_group = case_when(
        !in_sample ~ "Outside Sample",
        gas_dep == "Low" ~ "Gas dep <= median (coef = -0.123)",
        gas_dep == "High" ~ "Gas dep > median (coef = -0.751)",
        TRUE ~ "Outside Sample"
      ),
      map_group = factor(
        map_group,
        levels = c(
          "Gas dep <= median (coef = -0.123)",
          "Gas dep > median (coef = -0.751)",
          "Outside Sample"
        )
      )
    )

  p <- ggplot(europe_region) +
    geom_sf(aes(fill = map_group), color = "#808080", linewidth = 0.25, alpha = 0.9) +
    scale_fill_manual(
      name = "Gas Dependency Split",
      values = c(
        "Gas dep <= median (coef = -0.123)" = "#70AD47",
        "Gas dep > median (coef = -0.751)" = "#C00000",
        "Outside Sample" = "#F5F5F5"
      )
    ) +
    labs(
      title = "High vs. Low Gas Dependency EU Division",
      subtitle = "Chow test: F(20,335) = 74.34, p < 0.001",
      caption = "Median split of country mean gas dependency: Low = 12 countries, High = 12 countries"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    coord_sf(expand = FALSE)

  out_path <- file.path(out_dir, "map_gas_dependency_chow.png")
  ggsave(out_path, p, width = 14, height = 10, dpi = 300)
  message("Saved: ", out_path)
}

# =========================================================================
# Generate both maps
# =========================================================================

map_east_west()
map_gas_dependency()

cat("\nDone. Generated maps in ", normalizePath(out_dir, winslash = "/"), "\n", sep = "")
