#' Confidence intervals for clr coefficients in CoDa models
#'
#'
#' @return data.frame
#' @export
confint.clr <- function(res_lm, x_var_name, level = .95) {

  tranSumary <- transformationSummary(res_lm)
  x_vars <- rownames(tranSumary)[-1]

  stopifnot(level > 0.5 && level < 1)
  if (!x_var_name %in% x_vars)
    stop("x_var_name must be one of ", deparse(x_vars))

  vcov_y <- tranSumary[["VARCOV_CLR"]][[1]]
  vcov_x <- tranSumary[["VARCOV_CLR"]][[x_var_name]]

  sd_coef <- lapply(sqrt(diag(vcov_x)), "*", sqrt(diag(vcov_y)))
  sd_coef <- t(t(do.call("cbind", sd_coef)))

  df_eq <- nobs(res_lm) - ncol(t(coef(res_lm)))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  qlevel <- qt(a, df_eq)

  est_coef <- t(tranSumary[["COEF_CLR"]][[x_var_name]])

  result <- vector("list", length = ncol(est_coef))
  for (i in seq_along(result)) {
    result[[i]] <- data.frame(
      "Y" = rownames(vcov_y),
      "X" = colnames(est_coef)[i],
      "est" = est_coef[,i],
      "sd"  = sd_coef[,i],
      "qLo" = sd_coef[,i] * qlevel[1] + est_coef[,i],
      "qHi" = sd_coef[,i] * qlevel[2] + est_coef[,i],row.names = NULL)
  }
  result <- Reduce("rbind", result)
  colnames(result) <- c("Y","X","EST","SD", paste0("Q", substr(a,start = 3, 10)))
  return(result)
}
