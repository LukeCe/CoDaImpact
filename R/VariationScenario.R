#' @title Scenarios for variation in CoDa regressions models
#'
#' @description
#' Scenarios of this type are illustrated in Dargel and Thomas-Agnan (2023).
#' They allow to evaluate how the response variable (Y) in a CoDa model would evolve under a hypothetical scenario for linear changes in one explanatory variable (X).
#' When the changing explanatory variable is compositional the term "linear" is understood with respect to the geometry of the simplex.
#'
#' @details
#' The linear scenario for X is computed with [seq()] in the scalar case and with [CoDa_seq()] in the compositional case.
#' The corresponding changes in Y are computed with the prediction formula, where we exploit the fact that only in one variable is changing.
#'
#'
#' @param object an object of class lmCoDa
# TODO use lmCoDa instead of lmSimplex
#' @param Xvar a character indicating the name of the explanatory variable that changes
#' @param Xdir either character or numeric, to indicate the direction in which Xvar should change
#'   - when character this should be one of the components of X, in which case the direction is the corresponding vertex of the simplex
#'   - when numeric this argument is coerced to a unit vector in the simplex
#'   - (when Xvar refers to a scalar variable this argument is ignored)
#' @param obs a numeric indicating the observation used for the scenario
#' @param inc_size a numeric indicating the distance between each point in the scenario of X
#' @param inc_rate a numeric that can be used as an parametrization of the step size
#' @param n_steps a numeric indicating the number of points in the scenario
#' @param add_opposite a logical, if `TRUE` the scenario also includes changes in the opposite direction
#'
#' @return a data.frame containing the scenario of X and the corresponding predicted values of Y
#' @author Lukas Dargel
# TODO update reference
#' @references "full citation of Dargel and Thomas-Agnan (2023)"
#' @export
#'
#' @examples
#'
#' # ---- model with scalar response ----
#' res <- lm(YIELD ~ PRECIPITATION + ilr(TEMPERATURES), data = head(rice_yields,20))
#' VariationScenario(res, Xvar = "TEMPERATURES", Xdir = "MIDDLE", n_steps = 5)
#' VariationScenario(res, Xvar = "PRECIPITATION", n_steps = 5))
#'
#'
#' # ---- model with compositional response ----
#' res <- lm(ilr(cbind(left, right, extreme_right)) ~
#'             ilr(cbind(Age_1839, Age_4064)) +
#'             ilr(cbind(Educ_BeforeHighschool, Educ_Highschool, Educ_Higher)) +
#'             log(unemp_rate),
#'           data = head(election))
#'
#' VariationScenario(res, "cbind(Age_1839,Age_4064)",Xdir = "Age_1839", n_steps = 5)
#' VariationScenario(res, "log(unemp_rate)", n_steps = 5)
VariationScenario <- function(
    object,
    Xvar,
    Xdir,
    obs = 1,
    inc_size = .1,
    inc_rate,
    n_steps = 100,
    add_opposite = TRUE) {

  stopifnot(is.character(Xvar) && length(Xvar) == 1,
            length(obs) == 1,
            is.numeric(inc_size) && length(inc_size) == 1,
            is.numeric(n_steps) && length(n_steps) == 1,
            isTRUE(add_opposite) || isFALSE(add_opposite),
            is.numeric(inc_size) && length(inc_size) == 1,
            missing(inc_rate) || (0 < inc_rate && inc_rate < 1))

  clo2 <- function(x) x/rowSums(x)
  traSry <- transformationSummary(object)

  # get X0
  Anames <- unlist(traSry$NAME_SIMPLEX)
  Xnames <- setdiff(Anames[-1], "(Intercept)")
  if (!Xvar %in% Xnames) stop("Xvar musst be one of ", list(Xnames), "!")
  Xvar <- unlist(traSry$NAME_COORD[Xvar == Anames])
  Dx <- traSry$D[[Xvar]]
  scalar_x <- Dx == 0
  Kx <- traSry$LR_BASE_K[[Xvar]]
  X0 <- object$model[[Xvar]]
  X0 <- if (scalar_x) X0[obs] else t(X0[obs,]) %*% t(Kx) # work with clr in compositional case

  # get Y0
  scalar_y <- traSry$D[[1]] == 0
  Ky <- traSry$LR_BASE_K[[1]]
  Y0 <- fitted(object)
  Y0 <- if (scalar_y) Y0[obs] else Y0[obs,] %*% t(Ky) # work with clr in compositional case

  inc_seq <- seq(from = -n_steps*add_opposite, to = n_steps)
  if (scalar_x) {
    # IDEA allow for "log-linear" sequences
    Xcoef <- traSry$COEF_CLR[[Xvar]]
    Xscenario <- inc_seq*inc_size
    Yscenario <- if (scalar_y) Y0 else Y0[rep(1,nrow(Xscenario)),,drop=FALSE]
    Yscenario <- Xscenario %*% Xcoef + Yscenario
    Xscenario <- X0 + Xscenario
  }

  if (!scalar_x) {
    if (is.character(Xdir)) {
      # convert preferential to general direction
      Xdir <- Xdir == colnames(X0)
      Xdir <- if (sum(Xdir) == 1) exp(Xdir) else stop("Xdir must be one of ", list(colnames(X0)), "!")
      if (!missing(inc_rate)) inc_size <- inc_rate * Xdir && stop("To be corrected")
    }
    valid_dir <- length(Xdir) == length(X0) && all(Xdir > 0)
    if (!valid_dir) stop("Xdir must be a positive vector of length ", length(X0), "!")

    Xcoef <- traSry$COEF_CLR[[Xvar]]
    Xscenario <- CoDa_path(rep(1/Dx, Dx), comp_direc = Xdir, step_size = inc_size, n_steps = n_steps, add_opposite = add_opposite)
    Xscenario <- log(as.matrix(Xscenario)) %*% clrBase(Dx)
    Yscenario <- if (scalar_y) Y0 else Y0[rep(1,nrow(Xscenario)),]
    Yscenario <- Xscenario %*% Xcoef + Yscenario
    Xscenario <- clo2(exp(X0[rep(1,nrow(Xscenario)),] + Xscenario))
  }


  if (!scalar_x) Xvar <- "X"
  if (!scalar_y) Yvar <- "Y" else Yvar <- rownames(traSry)[1]
  if (!scalar_y) Yscenario <- clo2(exp(Yscenario))
  result <- data.frame(row.names = as.character(inc_seq))
  result[[Yvar]] <- Yscenario
  result[[Xvar]] <- Xscenario
  attr(result, "inc_size") <- inc_size
  attr(result, "obs") <- obs
  return(result)
}

