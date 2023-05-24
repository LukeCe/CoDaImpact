# ---- test CoDa_seq ------------------------------------------------------
expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_seq(res,n_steps = 2)
  is.data.frame(res)
})

expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_seq(res,n_steps = 2)
  all(colnames(res) == c("A","B","C"))
})

expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_seq(res,n_steps = 2,add_opposite = TRUE)
  all(dim(res) == c(5,3))
})


expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_seq(res,n_steps = 2)
  all(abs(res["0",] - t(c(A =.4,B = .3, C= .3))) < .Machine$double.eps)
})


expect_true({
  res <- c(A =.4,B = .3, C= .3)
  res <- CoDa_seq(res,n_steps = 1,add_opposite = TRUE)
  all(row.names(res) == c("-1","0","1"))
})

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
