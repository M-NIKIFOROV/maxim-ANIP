
# 01_energy2007_cleaning.R
#
# Clean and harmonise:
# - data/raw/csv/energy(eurostat2007).csv
# - data/raw/csv/energy(eurostat).csv (drop columns starting with 2007 first)

source("01_cleaning/definitions.R")

library(readr)
library(dplyr)

clean_numeric <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("(?i)p$", "", x, perl = TRUE)
  suppressWarnings(readr::parse_number(x, na = c("", ":", "na", "n/a", "null")))
}

collapse_semester_columns <- function(df) {
  names(df) <- trimws(names(df))

  s1_cols <- grep("^[0-9]{4}-S1$", names(df), value = TRUE)
  s2_cols <- grep("^[0-9]{4}-S2$", names(df), value = TRUE)

  get_year <- function(col) sub("-S[12]$", "", col)
  s1_years <- sapply(s1_cols, get_year)
  s2_years <- sapply(s2_cols, get_year)
  all_years <- unique(c(s1_years, s2_years))

  for (yr in all_years) {
    s1_col <- s1_cols[which(s1_years == yr)]
    s2_col <- s2_cols[which(s2_years == yr)]

    if (length(s1_col) == 1 && length(s2_col) == 1) {
      s1_vals <- clean_numeric(df[[s1_col]])
      s2_vals <- clean_numeric(df[[s2_col]])
      df[[yr]] <- rowMeans(cbind(s1_vals, s2_vals), na.rm = TRUE)
    } else if (length(s1_col) == 1) {
      df[[yr]] <- clean_numeric(df[[s1_col]])
    }
  }

  df <- df %>% select(-any_of(c(s1_cols, s2_cols)))
  names(df) <- sub("-$", "", names(df))
  df
}

split_first_column <- function(df) {
  split_names <- c("frequency", "product", "final_consumption", "unit", "tax", "currency", "country")
  split_matrix <- do.call(rbind, strsplit(as.character(df[[1]]), ",", fixed = TRUE))
  split_df <- as.data.frame(split_matrix, stringsAsFactors = FALSE)
  names(split_df) <- split_names
  split_df[] <- lapply(split_df, trimws)
  split_df[] <- lapply(split_df, tolower)
  cbind(split_df, df[-1])
}

map_and_filter_countries <- function(df) {
  if (!"country" %in% names(df)) return(df)
  country_lower <- tolower(df$country)
  matched <- country_lower %in% names(iso)
  df$country[matched] <- iso[country_lower[matched]]
  df$country <- tolower(df$country)
  df[df$country %in% eu, ]
}

clean_energy_dataset <- function(path, remove_2007_columns = FALSE) {
  df <- read_csv(path, col_types = cols(.default = "c"))
  names(df) <- trimws(names(df))

  if (remove_2007_columns) {
    cols_2007 <- grep("^2007", names(df), value = TRUE)
    df <- df %>% select(-any_of(cols_2007))
  }

  df[[1]] <- tolower(as.character(df[[1]]))
  df <- collapse_semester_columns(df)
  df <- split_first_column(df)
  df <- map_and_filter_countries(df)
  df
}


# Clean both datasets
energy2007_raw <- clean_energy_dataset("data/raw/csv/energy(eurostat2007).csv", remove_2007_columns = FALSE)
energy_raw <- clean_energy_dataset("data/raw/csv/energy(eurostat).csv", remove_2007_columns = TRUE)

# Combine them
energy_combined <- bind_rows(energy2007_raw, energy_raw)

# Write combined output
write_csv(energy_combined, "data/clean/energy_cleaned.csv", na = "")

