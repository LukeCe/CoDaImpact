library("compositions")

# ---- methods ----------------------------------------------------------------
ddd <- election[1:20,]
res <- lm(
  ilr(cbind(left, right, extreme_right)) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate, data = ddd)

expect_equal(
  ilrInv(fitted(res)),
  predict.lmSimplex(res))

expect_equal(
  ilrInv(resid(res)),
  residuals.lmSimplex(res))

expect_equal(
  ilrInv(resid(res)),
  acomp(ddd[,c("left", "right", "extreme_right")]) - predict.lmSimplex(res))
