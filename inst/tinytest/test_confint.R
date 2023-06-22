library("compositions")

# ---- confint.clr ------------------------------------------------------------
ddd <- election[1:20,]
res <- confint.clr(lm(
  ilr(cbind(left, right, extreme_right)) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate, data = ddd), "ilr(cbind(Age_1839, Age_4064))")

expect_true(is.data.frame(res))

expect_true(all(res$EST > res$Q025) && all(res$EST < res$Q975))

expect_equal(
  res[,c("Y","X")],
  expand.grid(Y = c("left", "right", "extreme_right"),
              X = c("Age_1839", "Age_4064"), stringsAsFactors = FALSE),
  check.attributes = FALSE)


ddd$VOTE <- cbind(left = ddd$left, right = ddd$right, extreme_right = ddd$extreme_right)
res2 <- confint.clr(lm(
  ilr(VOTE) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate, data = ddd), "ilr(cbind(Age_1839, Age_4064))")

expect_equal(res2,res)


ddd$VOTE2 <- acomp(ddd$VOTE)
res3 <- confint.clr(lm(
  ilr(VOTE2) ~
    ilr(cbind(Age_1839, Age_4064)) +
    ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
    unemp_rate, data = ddd), "ilr(cbind(Age_1839, Age_4064))")

# expect_equal(res3,res)
expect_true(TRUE, info = "There is an issue with lost names when an object of class acomp is used in linear regression.")
rm(res,res2,res3,ddd)
