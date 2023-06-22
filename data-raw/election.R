library("data.table")

load("data-raw/VOTE.rda")

rename <- c(
  "others"     = "extreme_right",
  "Age_65"     = "Age_65plus",
  "No_CAPBEP"  = "Educ_BeforeHighschool",
  "Bac"        = "Educ_Highschool",
  "Diplom_sup" = "Educ_Higher",
  "owner_rate" = "asset_owner_rate",
  "income_rate"= "income_taxpayer_rate",
  "foreign"    = "forgeigner_rate")


election <- VOTE
setnames(election,old = names(rename), new = rename)
usethis::use_data(election)
