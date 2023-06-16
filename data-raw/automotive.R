library("data.table")

load("data-raw/BDDSegX.rda")

rename <- c(
  "S_A" = "Seg_A",
  "S_B" = "Seg_B",
  "S_C" = "Seg_C",
  "S_D" = "Seg_D",
  "S_E" = "Seg_E")


car_market <- BDDSegX
setnames(election,old = names(rename), new = rename)
usethis::use_data(election)



#' European car market data
#'
#' The data is simulated from a model that is based on actual market shares.
#' It is first used by Morais et. al (2018) who compare compositional and Dirichlet models for market shares.
#'
#' @details
#'   + `Seg_`: Corresponds to the shares of sales in each of the five market
#'     segments A,B,C,D and E. Where A are the smallest cars and E luxury.
#'   + `Brent`: Is the crude oil price.
#'   + `Petrol`: Corresponds to the price of petroleum (HT without tax TTC with)
#'   + `PAC`
#'
#'
#' @references
#'    Joanna Morais, Christine Thomas-Agnan & Michel Simioni (2018) Using compositional and Dirichlet models for market share regression, Journal of Applied Statistics, 45:9, 1670-1689, DOI: 10.1080/02664763.2017.1389864
"car_market"
