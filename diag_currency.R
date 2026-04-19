suppressPackageStartupMessages(library(readr))

raw <- read_csv("data/raw/csv/energy(eurostat).csv", col_types=cols(.default="c"), n_max=5)
cat("Raw Eurostat file - first column (first 5 rows):\n")
print(raw[[1]])

e <- read_csv("data/clean/energy_cleaned.csv", show_col_types=FALSE)
cat("\nAll currency codes across all countries:\n")
print(table(e$currency))

cat("\nHungary: currency x tax breakdown (row counts):\n")
hu <- e[tolower(e$country)=="hungary",]
print(table(hu$currency, hu$tax))

cat("\nHungary: EUR vs NAC sample values for 2020-2023:\n")
library(dplyr); library(tidyr)
hu_tbl <- hu %>%
  select(currency, tax, `2020`, `2021`, `2022`, `2023`) %>%
  filter(tax == "i_tax")
print(as.data.frame(hu_tbl))

cat("\nEurozone country (Austria) - currency breakdown:\n")
at <- e[tolower(e$country)=="austria",]
print(table(at$currency, at$tax))
cat("\nAustria EUR vs NAC for 2020 (i_tax only):\n")
print(as.data.frame(at %>% select(currency, tax, `2020`) %>% filter(tax=="i_tax")))
