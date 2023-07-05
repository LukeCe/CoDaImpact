expect_equal({
  tt <- election[1:20,]
  res <- lm(
    ilr(cbind(left, right, extreme_right)) ~
      ilr(cbind(Age_1839, Age_4064)) +
      ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      log(unemp_rate),
    data = tt)

  list(
    VariationScenario(res, "cbind(Age_1839,Age_4064)",Xdir = "Age_1839", n_steps = 5, add_opposite = FALSE),
    VariationScenario(res, "log(unemp_rate)", n_steps = 5, add_opposite = FALSE))
},
{
  res <- lm(
    alr(cbind(left, right, extreme_right)) ~
      alr(cbind(Age_1839, Age_4064)) +
      alr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
      log(unemp_rate),
    data = tt)
  list(
    VariationScenario(res, "cbind(Age_1839,Age_4064)",Xdir = "Age_1839", add_opposite = FALSE, n_steps = 5),
    VariationScenario(res, "log(unemp_rate)",Xdir = "Age_1839",add_opposite = FALSE,n_steps = 5))
},info = "Y compo & (X compo + X scalar)")

expect_equal({
  tt <- rice_yields[1:20,]
  res <- lm(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = tt)
  list(
    VariationScenario(res, "TEMPERATURES", Xdir = "MIDDLE", n_steps = 5, add_opposite = FALSE),
    VariationScenario(res, "PRECIPITATION", n_steps = 5, add_opposite = FALSE))
},
{
  res <- lm(YIELD ~ PRECIPITATION + alr(TEMPERATURES), data = tt)
  list(
    VariationScenario(res, "TEMPERATURES", Xdir = "MIDDLE", n_steps = 5, add_opposite = FALSE),
    VariationScenario(res, "PRECIPITATION", n_steps = 5, add_opposite = FALSE))
},info = "Y scalar & (X compo + X scalar)")
