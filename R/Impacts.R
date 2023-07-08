#' @title Computation of elasticities in CoDa regression models
#'
#' @description
#' This function computes elasticities and semi-elasticities for CoDa
#' regression model.
#' where we have to distinguish four cases:
#'  - Y and X are both compositional: this leads to an elasticity
#'  - Y is compositional and X is scalar: this leads to a semi-elasticity
#'  - Y is scalar and X is compositional: this leads to a semi-elasticity
#'  - Y and X are both scalar: this case is not implemented as it leads to constant marginal effects
#'
#' @details
#' The mathematical foundation for elasticity computations in CoDa model come
#' from Morais and Thomas-Agnan (2021).
#' Dargel and Thomas-Agnan (2021) present further results and illustrations.
#'
#' @param object an object type lmCoDa
#' @param Xvar a character indicating the name of one explnanotry variable
#' @param obs a numeric that refers to the indicator of one observation
#'
#' @return a matrix
#' @author
#'   - Lukas Dargel
#'   - Rodrigue Nasr
#' @exportS3Method
# TODO refs
#' @references "
#'   - Dargel & T-A (2023)
#'   - Morais & T-A (2021)
#'
#' @examples
#'
#' aa
Impacts.lmCoDa <- function(object, Xvar=NULL, obs=1){

  stopifnot(is.character(Xvar) || length(Xvar) == 1,  # IDEA allow multiple variable
            is.numeric(obs) && isTRUE(obs >= 1) && obs <= nobs(object))

  trSry <- transformationSummary(object)
  Anames <- unlist(trSry$NAME_SIMPLEX)
  Xnames <- setdiff(Anames[-1], "(Intercept)")
  if (!Xvar %in% Xnames) stop("Xvar must be one of the following:", list(Xnames))
  Xvar <- which(Xvar == Anames)
  Xcoef <- trSry$COEF_CLR[[Xvar]]

  YX_is_compo <- c("" != trSry$LR_TRAN[c(1, Xvar)], use.names = FALSE)
  if (identical(YX_is_compo, c(FALSE, FALSE)))
    stop("Impacts are only meaningful if X or Y are compositional!")

  if (identical(YX_is_compo, c(FALSE, TRUE)))
    return(Xcoef)

  # when Y is compositional
  Dy <- trSry$D[[1]]
  Wz <- diag(Dy) - as(fitted(object, space = "simplex")[rep(obs,Dy), ], "matrix")
  imp <- Xcoef %*% t(Wz)
  colnames(imp) <- colnames(Xcoef)
  attr(imp, "obs") <- obs
  return(imp)
}

#' @keywords internal
#' @noRd
Impacts <- function(x, ...) {
  UseMethod("Impacts")
}


