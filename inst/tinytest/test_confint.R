library("compositions")

# ---- confint.clr (univariate) -----------------------------------------------
# Y-compositional
res <- lm(
  ilr(cbind(left, right, extreme_right)) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate,
  data = election[1:20,])
expect_true(is.data.frame(confint.clr(res, "unemp_rate")))
expect_true(is.data.frame(confint.clr(res, "ilr(cbind(Age_1839, Age_4064))")))
expect_true(is.data.frame(confint.clr(res, "ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher))")))

res2 <- confint.clr(res, "ilr(cbind(Age_1839, Age_4064))")
expect_true(all(res2$EST > res2$Q025) && all(res2$EST < res2$Q975))
expect_equal(
  res2[,c("Y","X")],
  expand.grid(Y = c("left", "right", "extreme_right"),
              X = c("Age_1839", "Age_4064"), stringsAsFactors = FALSE),
  check.attributes = FALSE)
rm(res, res2)

# Y-scalar
res <- lm(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = rice_yields[1:20,])
expect_true(is.null(confint.clr(res, "PRECIPITATION")))
expect_true(is.data.frame(confint.clr(res, "ilr(TEMPERATURES)")))
expect_equal(confint.clr(res, "ilr(TEMPERATURES)"),
             confint.clr(res, "ilr(TEMPERATURES)",y_ref = 3))
rm(res)

# ---- confint.clr (differences) ----------------------------------------------
expect_true({
  res <- confint.clr(lm(
    ilr(cbind(left, right, extreme_right)) ~
      ilr(cbind(Age_1839, Age_4064)) +
      ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = election[1:20,]),
    x_var_name = "ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher))",
    y_ref = 1)
  all(res[res$Y_ref == res$Y, c("DIFF", "SD", "Q025", "Q975")] == 0)
},info = "Y compo - X comp")


expect_true({
  res <- confint.clr(lm(
    ilr(cbind(left, right, extreme_right)) ~
      ilr(cbind(Age_1839, Age_4064)) +
      ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      unemp_rate, data = election[1:20,]),
    x_var_name = "unemp_rate",
    y_ref = 1)
  all(res[res$Y_ref == res$Y, c("DIFF", "SD", "Q025", "Q975")] == 0)
},info = "Y compo - X scalar")
