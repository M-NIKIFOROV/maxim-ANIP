# 02_pretest1_table_full.R
#
# Outputs a LaTeX regression table with n, estimate, std error, significance, R2 within, and notes

library(knitr)
library(dplyr)


# Source the main script to get model and fit_stats
source("02_pretest/02_pretest1.R")

tidy_coef1 <- coef_table %>%
  mutate(
    signif = case_when(
      `Pr(>|t|)` < 0.001 ~ '***',
      `Pr(>|t|)` < 0.01 ~ '**',
      `Pr(>|t|)` < 0.05 ~ '*',
      `Pr(>|t|)` < 0.1 ~ '.',
      TRUE ~ ''
    )
  ) %>%
  select(term, Estimate, `Std. Error`, signif)

n_obs1 <- fit_stats$n_obs[1]
r2_within1 <- fit_stats$r2_within[1]

latex_table1 <- kable(
  tidy_coef1,
  format = "latex",
  booktabs = TRUE,
  caption = paste0("Regression Results (t-1 lag, n = ", n_obs1, ", R^2_{within} = ", round(r2_within1, 3), ")")
)

tidy_coef2 <- coef_table2 %>%
  mutate(
    signif = case_when(
      `Pr(>|t|)` < 0.001 ~ '***',
      `Pr(>|t|)` < 0.01 ~ '**',
      `Pr(>|t|)` < 0.05 ~ '*',
      `Pr(>|t|)` < 0.1 ~ '.',
      TRUE ~ ''
    )
  ) %>%
  select(term, Estimate, `Std. Error`, signif)

n_obs2 <- fit_stats2$n_obs[1]
r2_within2 <- fit_stats2$r2_within[1]

latex_table2 <- kable(
  tidy_coef2,
  format = "latex",
  booktabs = TRUE,
  caption = paste0("Regression Results (t-2 lag, n = ", n_obs2, ", R^2_{within} = ", round(r2_within2, 3), ")")
)

notes <- "\\begin{tablenotes}[para,flushleft]
Significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1. \\ 
United Kingdom and Slovakia excluded from the analysis. R^2_{within} is the within-country R-squared. Standard errors are clustered by country.
\\end{tablenotes}"

cat(latex_table1, "\n\n", latex_table2, "\n\n", notes)
