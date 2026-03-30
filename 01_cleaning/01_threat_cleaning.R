# 01_threat_cleaning.R
#
# Builds annual threat-distance panel for EU countries (1980-2024):
# - Minimum border distance to USSR/Russia (COW 365)
# - Capital distance to Moscow
# Output columns: country, year, dist_border_km, dist_capital_km

library(dplyr)
library(purrr)
library(sf)
library(readr)
library(stringr)
library(rnaturalearth)
library(cshapes)

# Source shared definitions (eu vector)
source("01_cleaning/definitions.R")

build_threat_panel <- function(years = 1980:2024, eu_countries = eu) {
  # Detect latest available cshapes date
  cshapes_meta <- cshapes::cshp() # loads all available
  all_dates <- as.Date(unique(cshapes_meta$end))
  all_dates <- all_dates[!is.na(all_dates)]
  max_date <- max(all_dates)
  max_year <- as.integer(format(max_date, "%Y"))
  # Restrict years to available range
  years <- years[years <= max_year]
  cshapes_name_map <- c(
    "czech republic" = "czechia",
    "slovak republic" = "slovakia"
  )

  ne_name_map <- c(
    "czech republic" = "czechia"
  )

  capitals_sf <- rnaturalearth::ne_download(
    scale = 10,
    type = "populated_places",
    category = "cultural",
    returnclass = "sf"
  ) %>%
    mutate(
      country = str_to_lower(ADM0NAME),
      country = recode(country, !!!ne_name_map, .default = country)
    )

  moscow_sf <- capitals_sf %>%
    filter(str_to_lower(NAME) %in% c("moscow", "moskva")) %>%
    slice_head(n = 1) %>%
    st_as_sf()

  if (nrow(moscow_sf) == 0) {
    moscow_sf <- st_as_sf(
      tibble(name = "moscow", lon = 37.6173, lat = 55.7558),
      coords = c("lon", "lat"),
      crs = 4326
    )
  }


  # Find the population column name (case-insensitive)
  pop_col <- intersect(tolower(names(capitals_sf)), c("pop_max", "pop_est"))
  pop_col <- if (length(pop_col) > 0) names(capitals_sf)[tolower(names(capitals_sf)) == pop_col[1]] else NA_character_

  # Use 'capital' column if present (capital == 1 for admin-0 capitals), else fallback to largest city or first row
  if ("capital" %in% names(capitals_sf)) {
    capitals_eu <- capitals_sf %>%
      filter(capital == 1) %>%
      filter(country %in% eu_countries) %>%
      group_by(country)
    if (!is.na(pop_col)) {
      capitals_eu <- capitals_eu %>% slice_max(order_by = .data[[pop_col]], n = 1, with_ties = FALSE)
    } else {
      capitals_eu <- capitals_eu %>% slice_head(n = 1)
    }
    capitals_eu <- capitals_eu %>% ungroup()
  } else {
    capitals_eu <- capitals_sf %>%
      filter(country %in% eu_countries) %>%
      group_by(country)
    if (!is.na(pop_col)) {
      capitals_eu <- capitals_eu %>% slice_max(order_by = .data[[pop_col]], n = 1, with_ties = FALSE)
    } else {
      capitals_eu <- capitals_eu %>% slice_head(n = 1)
    }
    capitals_eu <- capitals_eu %>% ungroup()
  }

  capital_dist <- tibble(
    country = capitals_eu$country,
    dist_capital_km = as.numeric(st_distance(st_geometry(capitals_eu), st_geometry(moscow_sf))[, 1]) / 1000
  )

  year_panel <- purrr::map_dfr(years, function(y) {
    borders_sf <- cshapes::cshp(date = as.Date(sprintf("%d-01-01", y)), useGW = TRUE) %>%
      st_as_sf()

    # Use 'country_name' and 'gwcode' for your cshapes version
    name_col <- "country_name"
    cow_col <- "gwcode"


    borders_sf <- borders_sf %>%
      mutate(
        country_raw = str_to_lower(.data[[name_col]]),
        country = recode(country_raw, !!!cshapes_name_map, .default = country_raw)
      )
    # Make all geometries valid
    borders_sf <- st_make_valid(borders_sf)

    reference_geom <- borders_sf %>%
      filter(.data[[cow_col]] == 365) %>%
      st_geometry() %>%
      st_union() %>%
      st_make_valid()

    eu_borders <- borders_sf %>%
      filter(country %in% eu_countries)
    eu_borders <- st_make_valid(eu_borders)

    border_dist <- tibble(
      country = eu_borders$country,
      dist_border_km = as.numeric(st_distance(st_geometry(eu_borders), reference_geom)[, 1]) / 1000
    ) %>%
      group_by(country) %>%
      summarise(dist_border_km = min(dist_border_km, na.rm = TRUE), .groups = "drop")

    border_dist %>%
      mutate(year = y) %>%
      left_join(capital_dist, by = "country") %>%
      select(country, year, dist_border_km, dist_capital_km)
  })

  year_panel %>%
    filter(country %in% eu_countries) %>%
    arrange(country, year)
}

threat_cleaned <- build_threat_panel(years = 1980:2024, eu_countries = eu)

# Forward-fill 2019 values to 2020-2023 (time-invariant variable)
threat_2019_fill <- threat_cleaned %>%
  filter(year == 2019) %>%
  crossing(year = 2020:2023) %>%
  select(country, year, dist_border_km, dist_capital_km)

threat_cleaned <- bind_rows(threat_cleaned, threat_2019_fill)

threat_cleaned <- pivot_wider(
  threat_cleaned,
  id_cols = country,
  names_from = year,
  values_from = c(dist_border_km, dist_capital_km),
  names_sep = "_"
)

threatc <- unique(threat_cleaned$country)

write_csv(threat_cleaned, "data/clean/threat_cleaned.csv", na = "")

# Has years 1980-2019 (but variable is time invariant). Copied values from the year 2019 to 2020-2023.
# Missing Germany, Italy, and Romania.