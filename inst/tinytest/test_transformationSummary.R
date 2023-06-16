# ---- test name_invTrans -----------------------------------------------------
expect_equal(
  name_invTrans("ilr   (a)"),
  "ilrInv(ilr   (a))")

expect_equal(
  name_invTrans("alr(a)"),
  "alrInv(alr(a))")

expect_equal(
  name_invTrans("log(a)"),
  "log(a)")

# ---- test whichTrans --------------------------------------------------------
expect_equal({
  tt <- rice_yields[1:10,]
  tt$"ilr(TEMPERATURES)" <- ilr(tt$TEMPERATURES)
  whichTrans(tt["ilr(TEMPERATURES)"])[["name"]]
}, "ilr")

expect_equal({
  tt <- rice_yields[1:10,]
  tt$"ilr(TEMPERATURES)" <- ilr(tt$TEMPERATURES)
  whichTrans(tt["ilr(TEMPERATURES)"])[["base"]]
}, {
  V <- ilrBase(D = 3)
  rownames(V) <- colnames(tt$TEMPERATURES)
  colnames(V) <- paste0("ilr(TEMPERATURES)", 1:2)
  V
})

expect_error({
  whichTrans(tt$TEMPERATURES)
})

# ---- transformationSummary 3. (X compo) -------------------------------------
expect_equal({
  V <- ilrBase(D = 3)
  lr <- function(x) ilr(x, V)
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  # res["lr(TEMPERATURES)","COEF_COORD"][[1]] # differ
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
},
{
  V <- ilrBase(D = 3)[3:1,]
  lr <- function(x) ilr(x, V)
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  # res["lr(TEMPERATURES)","COEF_COORD"][[1]] # differ
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
},info = "Two diffrent ilr bases.")

expect_equal({
  lr <- function(x) alr(x, ivar = 1)
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr <- function(x) alr(x, ivar = 3)
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
},info = "Two diffrent alr bases.")


expect_equal({
  lr <- function(x) alr(x, ivar = 1)
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr <- function(x) alr(x, ivar = 3)
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
},info = "Two diffrent ilr and alr bases.")



expect_equal({
  lr <- compositions::ilr
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr <- compositions::alr
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  res["lr(TEMPERATURES)","COEF_SIMPLEX"][[1]]
})
compo
