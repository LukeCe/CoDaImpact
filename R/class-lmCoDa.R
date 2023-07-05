# ---- constructors -----------------------------------------------------------

#' Estimating CoDa regression models
#'
#' @description
#' CoDa regression models are models in which the response or at least
#' one explanatory variable of compositional nature.
#'
#' @details
#' This is only a thin wrapper around [lm()] followed by [ToSimplex()].
#' Most of the work is done by the [transformationSummary()] function, which
#' has its own documentation page, but should be reserved for internal use.
#'
#'
#' @param formula as in [lm()]
#' @param data as in [lm()]
#' @param ... arguments passed on to [lm()]
#'
#' @author Lukas Dargel
#' @seealso [lm()], [ToSimplex()]
#' @export
#' @examples
#'
#' aa
lmCoDa <- function(formula, data, ...) {
  res <- lm(formula, data, ...)
  res <- ToSimplex(res)
  return(res)
}


#' Converting Linear models to a CoDa models
#'
#' @description
#' The function converts the output of a [lm()] to the lmCoDa class, which
#' offers additional tools for the interpretation of a CoDa regression models.
#'
#' @inherit lmCoDa return examples
#' @seealso [lm()], [lmCoDa()]
#' @author
#'   - Lukas Dargel
#'   - Rodrigue Nasr
#' @export
ToSimplex <- function(object){

  trSry <- transformationSummary(object)
  if (all(trSry[,'LR_TRAN'] == ""))
    return(object)


  object$trSry <- trSry
  y_trans <- trSry[1,'LR_TRAN']
  if (y_trans != "") {
    invTran <- match.fun(paste0(y_trans,"Inv"))
    attributes(object$fitted.values)$orig <- invTran(object$fitted.values)
    attributes(object$residuals)$orig     <- invTran(object$residuals)

    meanImpacts <- t(Reduce(f = "rbind", trSry$COEF_CLR[-1]))
    meanImpacts <- as(attributes(object$fitted.values)$orig, "matrix") %*% meanImpacts
    object$meanImpacts <- meanImpacts
  }

  class(object) <- c(class(object),'lmCoDa')
  return(object)
}


# ---- methods ----------------------------------------------------------------
#' Predictions, fitted values, residuals, and coefficients in CoDa models
#'
#' These functions work as in the usual lm object.
#' They additionally offer the possibility use the `space` argument
#' which transforms them into directly into clr space or in the simplex.
#'
#' @param object of type `lmSimplex`
#' @param space a character indicating in which space the prediction should
#'   be returned. Supported are the options `c("clr", "simplex")`.
#' @param ... passed on to [`predict.lm()]
#' @export
predict.lmCoDa <- function(object, space = NULL, ...) {

  stopifnot(is.null(space) || space %in% c("clr", "simplex"))
  pred <- predict(object, ...)
  if (is.null(space))
    return(Pred)

  Ky <- object$trSry[["LR_BASE_K"]][[1]]
  if (length(Ky) == 0)
    stop("The space argument can only be used for Y compositional models!")

  if (space == "simplex")
    return(clrInv(pred %*% t(Ky)))

  if (space == "clr")
    return(pred %*% t(Ky))
}

#' @inherit predict.lmCoDa title description details params
#' @param ... not used
#' @return matrix or vector
#' @importFrom compositions clrInv
#' @export
residuals.lmCoDa <- function(object, space, ...) {

  stopifnot(is.null(space) || space %in% c("clr", "simplex"))
  resi <- resid(object)
  if (is.null(space))
    return(resi)

  Ky <- object$trSry[["LR_BASE_K"]][[1]]
  if (length(Ky) == 0)
    stop("The space argument can only be used for Y compositional models!")

  if (space == "simplex")
    return(attr(resi, "orig"))

  if (space == "clr")
    return(resi %*% t(Ky))
}


#' @inherit predict.lmCoDa title description details params
#' @param ... not used
#' @return matrix or vector
#' @importFrom compositions clrInv
#' @export
fitted.lmCoDa <- function(object, space, ...) {

  stopifnot(is.null(space) || space %in% c("clr", "simplex"))
  fity <- fitted(object)
  if (is.null(space))
    return(fity)

  Ky <- object$trSry[["LR_BASE_K"]][[1]]
  if (length(Ky) == 0)
    stop("The space argument can only be used for Y compositional models!")

  if (space == "simplex")
    return(attr(fity, "orig"))

  if (space == "clr")
    return(fity %*% t(Ky))
}



#' @inherit predict.lmCoDa title description details params
#' @param ... not used
#' @return a matrix
#' @importFrom compositions clrInv
#' @export
coef.lmCoDa <- function(object, space, ...) {

}
