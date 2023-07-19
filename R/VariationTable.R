#' @title Effects of infinitesimal changes in CoDa models
#'
#' @description
#' This function allows to evaluate how a change in an explanatory variables
#' impacts the response variable in a CoDa regression model.
#' The changes are calculated based from the approximate formal presented
#' in Dargel and Thomas-Agnan (2023).
#' Changes in the response variables are provided as data.frame and the
#' underlying changes in the explanatory variable are given as attributes.
#'
#' @details
#' Developed in Dargel and Thomas-Agnan (2023)
#'
#' @inheritParams VariationScenario
#' @param inc_rate a numeric that can be used as an parametrization of the step size
#' @param Ytotal a numeric indicating the total of Y
#' @return data.frame
#'
#' @author
#'   - Lukas Dargel
#'   - Rodrigue Nasr
#' @references
#' @export
#' @examples
#'
#' # XY-compositional model
#' res <- lmCoDa(
#'   ilr(cbind(left, right, extreme_right)) ~
#'   ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)),
#'   data =  head(election, 20))
#'
#' # Variation of age the education composition towards a summit ...
#' # (higher share of people with lower education)
#' VariationTable(res, Xvar = "cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)", Xdir = "Educ_BeforeHighschool")
#'
#' # The same changes using a compositional vector as direction
#' VariationTable(res, Xvar = "cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)", Xdir = c(.5,.25,.25))
#'
#' # Changes in a more general direction and for a diffrent observation
#' VariationTable(res, Xvar = "cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)", Xdir = c(.35,.45,.10), obs = 2)
VariationTable <- function(
    object,
    Xvar,
    Xdir,
    obs = 1,
    inc_size = .1,
    inc_rate = NULL,
    Ytotal = 1) {

  stopifnot(is(object, "lmCoDa"),
            is.character(Xvar) && length(Xvar) == 1,
            missing(Xdir) || is.character(Xdir) || is.numeric(Xdir),
            is.numeric(obs) && isTRUE(obs >= 1) && obs <= nobs(object),
            is.numeric(inc_size) && length(inc_size) == 1,
            is.null(inc_rate) || (0 < inc_rate && inc_rate < 1),
            is.numeric(Ytotal) && length(Ytotal) == 1)

  trSry <- object$trSry
  if (all(trSry$LR_TRAN[c(1, Xvar)] == "")) stop("Variation tables are only meaningful if X or Y are compositional!")

  # get X0
  Anames <- unlist(trSry$NAME_SIMPLEX)
  Xnames <- setdiff(Anames[-1], "(Intercept)")
  if (!Xvar %in% Xnames) stop("Xvar musst be one of ", list(Xnames), "!")
  Xvar <- unlist(trSry$NAME_COORD[Xvar == Anames])
  Dx <- trSry$D[[Xvar]]
  scalar_x <- Dx == 0
  X0 <- object$model[[Xvar]]
  X0 <- if (scalar_x) X0[obs] else attr(X0, "orig")[obs,]

  # get Y0
  scalar_y <- trSry$D[[1]] == 0
  Y0 <- if (scalar_y) structure(fitted(object)[obs], names = names(object$model)[1]) else as(fitted(object, space = "simplex")[obs,],"vector")

  # define elasticities
  elasti <- Impacts(object, trSry$NAME_SIMPLEX[[Xvar]], obs)

  if (!scalar_x) {
    # for compositional X we need to account for the in which X changes
    vertex_dir <- is.character(Xdir)
    if (vertex_dir) {
      Xvertex <- Xdir == names(X0)
      if (sum(Xvertex) != 1) stop("When charater; Xdir must be one of ", list(names(X0)), "!")
      Xdir <- exp(Xvertex)^sqrt(Dx/(Dx-1))
      Xdir <- Xdir/sum(Xdir)
      if (!is.null(inc_rate)) inc_size <- inc_rate * sqrt((Dx-1)/Dx) / (1 - X0[Xvertex])
      if (is.null(inc_rate))  inc_rate <- inc_size * sqrt(Dx/(Dx-1)) * (1 - X0[Xvertex])
    }
    if (!vertex_dir) {
      valid_dir <- length(Xdir) == length(X0) && all(Xdir > 0)
      if (!valid_dir) stop("When numeric; Xdir must be a positive vector of length ", length(X0), "!")

      Xdir <- ilr(Xdir)
      Xdir <- as(ilrInv(Xdir/sqrt(sum(Xdir^2))),"vector")
      inc_rate <- NULL
    }
    elasti   <- log(Xdir) %*% elasti
  }

  Ytotal <- if (scalar_y) 1 else Ytotal
  Y1  <- Y0 * (1 + elasti * inc_size)
  YD  <- Y1 - Y0
  explanation <- c(
    "Y0"        = if (scalar_y) "Initial value" else "Initial parts",
    "Y1"        = if (scalar_y) "New value" else "New parts",
    "elasti"    = if (scalar_y || scalar_x) "Semi elasticity" else "Elasticity",
    "YD/Y0*100" = "Variation in %",
    "YD*100"    = if (scalar_y) NULL else "Variation in % points",
    "YD*Ytotal" = if (scalar_y || Ytotal != 1) "Variation in units" else NULL)

  evalString <- function(s) eval(str2lang(s), environment())
  explanation <- explanation[!is.null(explanation)]
  result <- lapply(names(explanation), evalString)
  result <- data.frame(Reduce("rbind", result), row.names = explanation, check.names = FALSE)

  if (scalar_x) Xdir <- inc_rate <- NULL
  attr(result,'X(0)')     <- X0
  attr(result,'X(h)')     <- if (scalar_x) X0 + inc_size else X0 * (1 + inc_size * (log(Xdir) - sum(X0 * log(Xdir))))
  attr(result,'Xdir')     <- Xdir
  attr(result,'inc_size') <- inc_size
  attr(result,'inc_rate') <- inc_rate
  return(result)
}

