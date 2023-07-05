#' @title Confidence Intervals for CoDa Model Parameters and differences
#'
#' @description
#' Dargel and Thomas-Agnan (2023) show to compute variances and confidence
#' intervals for parameters of CoDa models in log-ratio spaces.
#'
#' Of particular interest are the clr parameters since they can be directly
#' interpreted as differences from an average elasticity.
#'
#' Another option is interpret the difference in clr parameters as these
#' coincide with the difference in elasticities.
#'
#' Since CoDa models are often multivariate this function only allows to
#' specify one explanatory variable at a time.
#' The return value is also more complicated than in [confint.default()].
#'
#' @inheritParams predict.lmCoDa
#' @param parm a character, indicating the name of one explanatory variable
#' @param level a numeric, indicating the confidence level required
#' @param y_ref an optional argument that indicates the reference component of
#'   the response variable using its name or its position. \cr
#'   This argument is only used in the Y-compositional model.
#'   If it is supplied confidence intervals of difference are used instead of
#'   the direct intervals of the parameters.
#' @param ...
#' @return data.frame
#' @export
#' @author Lukas Dargel
# TODO refs
#' @references
#'
confint.lmCoDa <- function(object, parm, level = .95, y_ref = NULL, ...) {

  stopifnot(level > 0.5 & level < 1,
            is.character(parm) & length(parm) == 1,
            is.null(y_ref) | length(y_ref) == 1)

  tranSumary <- transformationSummary(object)
  x_vars <- rownames(tranSumary)[-1]
  if (!parm %in% x_vars) stop("parm must be one of ", deparse(x_vars))
  if (all(0 == c(tranSumary$D[[1]], tranSumary$D[[parm]]))) return(NULL)

  est_coef <- t(tranSumary[["COEF_CLR"]][[parm]])
  vcov_x <- tranSumary[["VARCOV_CLR"]][[parm]]
  vcov_y <- tranSumary[["VARCOV_CLR"]][[1]]
  y_vars <- colnames(vcov_y)
  if (ncol(vcov_y) < 2) y_ref <- NULL

  df_eq <- nobs(object) - ncol(t(coef(object)))
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  qlevel <- qt(a, df_eq)

  marginal_intervals <- is.null(y_ref)
  if (marginal_intervals) {
    sd_coef <- lapply(sqrt(diag(vcov_x)), "*", sqrt(diag(vcov_y)))
    sd_coef <- t(t(do.call("cbind", sd_coef)))

    result <- vector("list", length = ncol(est_coef))
    for (i in seq_along(result)) {
      result[[i]] <- data.frame(
        "Y"   = colnames(vcov_y),
        "X"   = colnames(est_coef)[i],
        "est" = est_coef[,i],
        "sd"  = sd_coef[,i],
        "qLo" = sd_coef[,i] * qlevel[1] + est_coef[,i],
        "qHi" = sd_coef[,i] * qlevel[2] + est_coef[,i],row.names = NULL)
    }
    result <- Reduce("rbind", result)
    colnames(result) <- c("Y","X","EST","SD", paste0("Q", substr(a,start = 3, 10)))
    return(result)
  }

  diff_intervals <- !is.null(y_ref)
  if (diff_intervals) {
    if (is.character(y_ref)) y_ref <- which(y_ref == y_vars)
    if (!any(y_ref %in% seq_len(ncol(vcov_y)))) stop("y_ref is not identifyable!")


    diff_coef <- est_coef - est_coef[rep(y_ref,length(y_vars)), ]
    sd_dy <- sqrt(diag(vcov_y) + vcov_y[y_ref,y_ref] - 2*vcov_y[y_ref,])

    result <- vector("list", length = ncol(est_coef))
    for (i in seq_along(result)) {
      sd_dcoef <- sqrt(vcov_x[i,i]) * sd_dy

      result[[i]] <- data.frame(
        "Y_ref" = colnames(vcov_y)[y_ref],
        "Y"   = colnames(vcov_y),
        "X"   = colnames(est_coef)[i],
        "dif" = diff_coef[,i],
        "sd"  = sd_dcoef,
        "qLo" = sd_dcoef * qlevel[1] + diff_coef[,i],
        "qHi" = sd_dcoef * qlevel[2] + diff_coef[,i],row.names = NULL)
    }
    result <- Reduce("rbind", result)
    colnames(result) <- c("Y_ref","Y","X","DIFF","SD", paste0("Q", substr(a,start = 3, 10)))
    return(result)
  }
}
