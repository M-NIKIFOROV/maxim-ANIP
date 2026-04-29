# 01_executor.R
# Runs all cleaning scripts in sequence

setwd("C:/Users/maxim/OneDrive/Documents/University - Year 3/ANIP/maxim-ANIP")

source("01_cleaning/01_area_cleaning.R")
source("01_cleaning/01_debtgdp_cleaning.R")
source("01_cleaning/01_defence_cleaning.R")
source("01_cleaning/01_defencegdp_cleaning.R")
source("01_cleaning/01_eubrent_cleaning.R")
source("01_cleaning/01_energy2007_cleaning.R")
source("01_cleaning/01_EUapproval_cleaning.R")
source("01_cleaning/01_gasdep_cleaning.R")
source("01_cleaning/01_gdppc_cleaning.R")
source("01_cleaning/01_govtapproval.R")
source("01_cleaning/01_healthspending_cleaning.R")
source("01_cleaning/01_ideology_cleaned.R")
source("01_cleaning/01_militaryconfidence.R")
source("01_cleaning/01_nato_cleaning.R")
source("01_cleaning/01_nrgdep_cleaning.R")
source("01_cleaning/01_threat_cleaning.R")
