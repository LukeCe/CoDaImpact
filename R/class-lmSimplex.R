


# ---- methods ----------------------------------------------------------------
#' Predictions in CoDa models
#'
#' @param object of type `lmSimplex`
#' @param ... passed on to [`predict.lm()]
#' @export
predict.lmSimplex <- function(object, ...) {
  pred <- predict(object, ...)
  Ky <- transformationSummary(object)[["LR_BASE_K"]][[1]]
  if (length(Ky) > 0) pred <- clrInv(pred %*% t(Ky))
  return(pred)
}


#' Simplex valued predictions in CoDa models
#'
#' @param object of type `lmSimplex`
#' @param ... passed on to [`predict.lm()]
#'
#' @importFrom compositions clrInv
#' @export
predict.lmSimplex <- function(object, ...) {
  pred <- predict(object, ...)
  Ky <- transformationSummary(object)[["LR_BASE_K"]][[1]]
  if (length(Ky) > 0) pred <- clrInv(pred %*% t(Ky))
  return(pred)
}

#' Simplex valued residuals in CoDa models
#'
#' @param object of type `lmSimplex`
#' @importFrom compositions clrInv
#' @export
residuals.lmSimplex <- function(object) {
  resi <- residuals(object)
  Ky <- transformationSummary(object)[["LR_BASE_K"]][[1]]
  if (length(Ky) > 0) resi <- clrInv(resi %*% t(Ky))
  return(resi)
}


