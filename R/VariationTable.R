#' @title Tabulate effects of infinitesimal changes in CoDa models
#'
#' @description
#' Compute variations
#'
#' @details
#' Developed in Dargel and Thomas-Agnan (2023)
#'
#' @inheritParams VariationScenario
#' @param Ytotal a numeric indicating the total of Y
#' @return data.frame
#' @export
#'
#' @examples
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
            length(obs) == 1,
            is.numeric(inc_size) && length(inc_size) == 1,
            is.null(inc_rate) || (0 < inc_rate && inc_rate < 1),
            is.numeric(Ytotal) && length(Ytotal) == 1)

  clo2 <- function(x) x/sum(x)
  trSry <- object$trSry
  if (all(trSry$LR_TRAN[c(1, Xvar)] == "")) stop("Variations are only meaningful if X or Y are compositional!")

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
  Y0 <- if (scalar_y) fitted(object)[obs] else fitted(object, space = "simplex")[obs,]
  Y0 <- as(Y0, "vector")


  # define elasticities
  elasti <- Impacts(object, trSry$NAME_SIMPLEX[[Xvar]], obs)
  if (!scalar_x) {
    # for compositional X we need to account for the in which X changes
    if (is.character(Xdir)) {
      Xvertex <- Xdir == names(X0)
      if (sum(Xvertex) != 1) stop("When charater; Xdir must be one of ", list(names(X0)), "!")

      Xdir <- clo2(exp(Xvertex)^sqrt(Dx/(Dx-1)))
      if (!is.null(inc_rate)) inc_size <- inc_rate * log(Xdir)[Xvertex]
    } else {
      valid_dir <- length(Xdir) == length(X0) && all(Xdir > 0)
      if (!valid_dir) stop("When numeric; Xdir must be a positive vector of length ", length(X0), "!")

      Xdir <- ilr(Xdir)
      Xdir <- as(ilrInv(Xdir/sqrt(sum(Xdir^2))),"vector")
    }
    inc_rate <- as(log(Xdir)*inc_size, "vector")
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
  result <- data.frame(Reduce("rbind", result), row.names = explanation)

  if (scalar_x) Xdir <- inc_rate <- NULL
  attr(result,'X(0)')     <- X0
  attr(result,'Xdir')     <- Xdir
  attr(result,'inc_size') <- inc_size
  attr(result,'inc_rate') <- inc_rate
  return(result)
}

