# run after the executor

countries <- list(
  area = areac,
  debtgdp = debtgdpc,
  defence = defencec,
  defencegdp = defencegdpc,
  energy = energyc,
  euapproval = euapprovalc,
  gasdep = gasdepc,
  gdppc = gdppcc,
  govtapproval = govtapprovalc,
  ideology = ideologyc,
  militaryconfidence = militaryconfidencec,
  nato = natoc,
  nrgdep = nrgdepc,
  threat = threatc
)

all_countries <- sort(unique(unlist(countries)))

country_table <- data.frame(
  country = all_countries,
  lapply(countries, function(x) as.integer(all_countries %in% x)),
  check.names = FALSE
)

dataset_cols <- names(countries)
country_table$in_all <- as.integer(rowSums(country_table[, dataset_cols]) == length(dataset_cols))

write.csv(country_table, "01_cleaning/countries_check.csv", row.names = FALSE)