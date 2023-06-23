library("data.table")

load("data-raw/BDDSegX.RData")

rename <- c(
  "Date" = "DATE",
  "S_A" = "SEG_A",
  "S_B" = "SEG_B",
  "S_C" = "SEG_C",
  "S_D" = "SEG_D",
  "S_E" = "SEG_E",
  "PIB_Courant_t"  = "GDP",
  "DC_Men_Courant" = "HOUSEHOLD_EXPENDITURE",
  "TTC_Gazole"     = "GAS_PRICE",
  "PAC"            = "SCRAPPING_SUBSIDY")


car_market <- BDDSegX
setnames(car_market,old = names(rename), new = rename)
car_market <- car_market[,rename]
usethis::use_data(car_market)
