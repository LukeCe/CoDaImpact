
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CoDaImpact

<!-- badges: start -->
<!-- badges: end -->

**CoDaImpact** provides additional tools for the interpretation of CoDa
models. It is conceived as an extension of the
[**compositions**](http://www.stat.boogaart.de/compositions/) package.

## Installation

You can install the development version of CoDaImpact from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LukeCe/CoDaImpact")
```

## Example of a Y-compositional model

Below we estimate a CoDa model where the dependent variable is a
compositional and represents the market shares in different segments of
the car market. To illustrate the influence of the
`HOUSEHOLD_EXPENDITURE` on the market shares in each segment we use a
variation scenario for one observation.

``` r
library("CoDaImpact")
data("car_market")

model_car_segements <- lmCoDa(
  ilr(cbind(SEG_A, SEG_B, SEG_C, SEG_D, SEG_E)) ~
    GDP + HOUSEHOLD_EXPENDITURE + GAS_PRICE,
  data = car_market)

vs_exp2 <- VariationScenario(
  model_car_segements,
  Xvar = "HOUSEHOLD_EXPENDITURE",
  obs = 1,
  inc_size = 100, 
  n_steps = 150,
  add_opposite = TRUE)


plot(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,1],type = "l", col = "red",
     main = "Variation scenario of household expenditure for observation 1",
     xlab = "Household expenditure", ylab = "Market share by segment")
lines(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,2],type = "l", col = "blue" )
lines(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,3],type = "l", col = "green")
lines(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,4],type = "l", col = "orange")
lines(x = vs_exp2$HOUSEHOLD_EXPENDITURE, y = vs_exp2$Y[,5],type = "l", col = "black")
legend("topleft",
       legend = paste0("SEG_", LETTERS[1:5]),
       col = c("red", "blue", "green", "orange", "black"),
       lty = 1)
```

<img src="man/figures/README-example-1.png" width="100%" />
