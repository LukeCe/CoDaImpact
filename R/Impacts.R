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
#' @export
# TODO refs
#' @references "
#'   - Dargel & T-A (2023)
#'   - Morais & T-A (2021)
#'
#' @examples
#'
#' aa
Impacts <- function(object, Xvar=NULL, obs=1){

  stopifnot(is.character(Xvar) && length(Xvar) == 1,
            is.numeric(obs) && length(obs) == 1)

  trSry <- transformationSummary(object)
  Anames <- unlist(trSry$NAME_SIMPLEX)
  Xnames <- setdiff(Anames[-1], "(Intercept)")
  if (!Xvar %in% Xnames) stop("Xvar must be one of the following:", list(Xnames))
  Xvar <- which(Xvar == Anames)
  Xcoef <- trSry$COEF_CLR[[Xvar]]

  YX_is_compo <- unlist("" != trSry$LR_TRAN[c(1, Xvar)], use.names = FALSE)
  if (identical(YX_is_compo, c(FALSE, FALSE)))
    stop("Impacts are not meaningful when Y and X are both not compositional!")

  if (identical(YX_is_compo, c(FALSE, TRUE)))
    return(Xcoef)

  if (identical(YX_is_compo, c(TRUE, FALSE)))
    return(Xcoef + object$meanImpact[obs, Xvar])

  if (identical(YX_is_compo, c(TRUE, TRUE)))
    return(Xcoef + t(object$meanImpact[rep(obs,ncol(Xcoef)), rownames(Xvar)]))
}
