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


# ---- test CoDa_path --------------------------------------------------
expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_path(res,n_steps = 1)
  is.data.frame(res)
})

expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_path(res,n_steps = 1)
  all(colnames(res) == c("A","B","C"))
})

expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_path(res,n_steps = 1,add_opposite = TRUE)
  all(dim(res) == c(3,3))
})

expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_path(res,n_steps = 1,add_opposite = TRUE)
  all(res[2,] == 1/c(3,3))
})


expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_path(res,n_steps = 1,add_opposite = FALSE,step_size = 1)
  abs(1 - attr(res, "step_size")) < 1e12
})

