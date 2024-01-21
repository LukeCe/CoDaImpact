mci_sim_params <- readRDS("data-raw/mci_sim_params.Rds")
toulouse_malls <- readRDS("data-raw/toulouse_malls.Rds")
toulouse_market <- readRDS("data-raw/toulouse_market.Rds")


attr(toulouse_market,which = "mall_locations")  <- toulouse_malls[,c("KEY_MALL", "LON_MALL", "LAT_MALL")]
attr(toulouse_market,which = "simulation_parameters") <- mci_sim_params$org_params
toulouse_retail <- toulouse_market
usethis::use_data(toulouse_retail, overwrite = TRUE)
