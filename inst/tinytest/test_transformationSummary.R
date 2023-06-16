library("compositions")

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
  whichTrans(tt["ilr(TEMPERATURES)"])[["base_K"]]
}, {
  V <- ilrBase(D = 3)
  rownames(V) <- colnames(tt$TEMPERATURES)
  colnames(V) <- paste0("ilr(TEMPERATURES)", 1:2)
  V
})

expect_error({
  whichTrans(rice_yields[1:10,]$TEMPERATURES)
})

# ---- alr_K2F ----------------------------------------------------------------
expect_equal({
  K <- clrBase(3)[,-3]
  alr_K2F(K)
},
  cbind(diag(2),-1)
)

expect_equal({
  K <- clrBase(3)[,-2]
  alr_K2F(K)
},
cbind(c(1,0),-1,c(0,1))
)

# ---- transformationSummary 1. (YX compo) ------------------------------------
expect_equal({
  lr1 <- function(x) ilr(x, ilrBase(D = 3))
  lr2 <- function(x) ilr(x, ilrBase(D = 2))
  lr3 <- function(x) ilr(x, ilrBase(D = 3)[3:1,])
  tt <- election[1:20,]
  res <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~
      lr2(cbind(Age_1839, Age_4064)) +
      lr3(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate,
    data = tt))
  res[,c("COEF_SIMPLEX","COEF_CLR")]
},
{
  V <- ilrBase(D = 3)
  lr1 <- function(x) ilr(x, ilrBase(D = 3)[3:1,])
  lr2 <- function(x) ilr(x, ilrBase(D = 2)[2:1,])
  lr3 <- function(x) ilr(x, ilrBase(D = 3)[c(2,1,3),])
  tt <- election[1:20,]
  res <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~
      lr2(cbind(Age_1839, Age_4064)) +
      lr3(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = tt))
  res[,c("COEF_SIMPLEX","COEF_CLR")]
},info = "Three diffrent ilr bases in X and Y")

expect_equal({
  lr1 <- function(x) alr(x,ivar = 1)
  lr2 <- function(x) alr(x,ivar = 2)
  lr3 <- function(x) alr(x,ivar = 3)
  tt <- election[1:20,]
  res1 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~
      lr2(cbind(Age_1839, Age_4064)) +
      lr3(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)),
    data = tt))
  res1[,c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr1 <- function(x) alr(x,ivar = 3)
  lr2 <- function(x) alr(x,ivar = 1)
  lr3 <- function(x) alr(x,ivar = 2)
  tt <- election[1:20,]
  res2 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~
      lr2(cbind(Age_1839, Age_4064)) +
      lr3(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher))
    , data = tt))
  res2[,c("COEF_SIMPLEX","COEF_CLR")]
},info = "Three diffrent alr bases in X and Y.")

expect_equal({
  lr1 <- function(x) alr(x,ivar = 1)
  lr2 <- function(x) alr(x,ivar = 2)
  lr3 <- function(x) alr(x,ivar = 3)
  tt <- election[1:20,]
  res1 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~
      lr2(cbind(Age_1839, Age_4064, Age_65plus)) +
      lr3(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = tt))
  res1[,c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr1 <- function(x) alr(x,ivar = 3)
  lr2 <- function(x) alr(x,ivar = 1)
  lr3 <- function(x) alr(x,ivar = 2)
  tt <- election[1:20,]
  res2 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~
      lr2(cbind(Age_1839, Age_4064, Age_65plus)) +
      lr3(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = tt))
  res2[,c("COEF_SIMPLEX","COEF_CLR")]
},info = "Two diffrent alr bases in X and a diffrent ilr Y.")

expect_equal({
  lr1 <- function(x) alr(x,ivar = 1)
  lr2 <- function(x) alr(x,ivar = 2)
  lr3 <- function(x) alr(x,ivar = 3)
  tt <- election[1:20,]
  res1 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~
      lr2(cbind(Age_1839, Age_4064, Age_65plus)) +
      lr3(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = tt))
  res1[,c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr1 <- function(x) alr(x,ivar = 3)
  lr2 <- function(x) alr(x,ivar = 1)
  lr3 <- function(x) alr(x,ivar = 2)
  tt <- election[1:20,]
  res2 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~
      lr2(cbind(Age_1839, Age_4064, Age_65plus)) +
      lr3(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = tt))
  res2[,c("COEF_SIMPLEX","COEF_CLR")]
},info = "Two diffrent ilr bases in X and a diffrent alr Y.")


# ---- transformationSummary 2. (Y compo) -------------------------------------
expect_equal({
  lr1 <- function(x) ilr(x)
  tt <- election[1:20,]
  res1 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~ unemp_rate, data = tt))
  res1[,c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr1 <- function(x) alr(x,ivar = 3)
  tt <- election[1:20,]
  res2 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~ unemp_rate, data = tt))
  res2[,c("COEF_SIMPLEX","COEF_CLR")]
},info = "Diffrent ilr and alr bases in Y.")

expect_equal({
  lr1 <- function(x) alr(x,ivar = 1)
  tt <- election[1:20,]
  res1 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~ unemp_rate, data = tt))
  res1[,c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr1 <- function(x) alr(x,ivar = 3)
  tt <- election[1:20,]
  res2 <- transformationSummary(lm(
    lr1(cbind(left, right, extreme_right)) ~ unemp_rate, data = tt))
  res2[,c("COEF_SIMPLEX","COEF_CLR")]
},info = "Diffrent alr bases in Y.")

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
  lr <- compositions::ilr
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
},
{
  lr <- compositions::alr
  tt <- rice_yields[1:20,]
  res <- transformationSummary(lm(YIELD ~ PRECIPITATION + lr(TEMPERATURES), data = tt))
  res["lr(TEMPERATURES)",c("COEF_SIMPLEX","COEF_CLR")]
}, info = "Ilr and alr bases.")
