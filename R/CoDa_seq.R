#' A sequence connecting two points in a simplex
#'
#' @details
#' The sequence is evenly spaced corresponds to a straight line in the simplex geometry.
#' It is derived in ilr-space using standard linear algebra techniques and then back transformed to the simplex.
#' If no end point is provided the line will connect the initial point with the first summit of the simplex.
#' Since exact zeros are not handled by the ilr they are replaced by a small constant.
#'
#' @param comp_from A numeric vector, representing the initial compositions
#' @param comp_to A numeric vector, representing the final compositions.
#' @param n_steps An integer, indicating the number of steps used to go from comp_from to comp_to
#' @param add_opposite A logical, if `TRUE` the path in the opposite direction is added
#'
#' @importFrom compositions ilr ilrInv
#' @return A data.frame frame where each point corresponds to one compositional vector
#' @seealso simplex_increment
#' @export
#'
#' @author Lukas Dargel
#' @references
#' @examples
#'
#' # path to the first summit of the simplex
#' start_comp <- c(A =.4,B = .35, C= .25)
#' compositions::plot.acomp(CoDa_seq(start_comp))
#' compositions::plot.acomp(CoDa_seq(start_comp, add_opposite = TRUE))
#'
#' # path to an edge of the simplex
#' end_comp <- c(0,.8,.2)
#' compositions::plot.acomp(CoDa_seq(start_comp, end_comp))
#' compositions::plot.acomp(CoDa_seq(start_comp, end_comp,add_opposite = TRUE))
CoDa_seq <- function(
  comp_from,
  comp_to,
  n_steps = 100,
  add_opposite = FALSE) {

  if (missing(comp_to)){
    comp_to <- comp_from * 0
    comp_to[1] <- 1
  }

  n_steps <- round(n_steps)
  stopifnot(
    length(comp_from) > 2,
    length(comp_from) == length(comp_to),
    is.null(names(comp_to)) || identical(names(comp_to), names(comp_from)),
    n_steps >= 1,
    is.logical(add_opposite),
    is.numeric(comp_from),
    is.numeric(comp_to))

  # save total and replace zeros
  Tstart <- sum(comp_from)
  zero_values_present <- any(comp_from == 0, comp_to == 0)
  if (zero_values_present) {
    epsilon <- 1e-10
    warn <- "Zero values in `comp_from` or `comp_to` are not permitted and were replaced by %s!'"
    warning(sprintf(warn, epsilon))
    comp_from[comp_from == 0] <- epsilon
    comp_from <- comp_from/sum(comp_from)
    comp_to[comp_to == 0] <- epsilon
    comp_to <- comp_to/sum(comp_to)
  }

  ilr_start <- ilr(comp_from)
  ilr_end <- ilr(comp_to)
  ilr_dir <- ilr_end - ilr_start

  steps <- c(seq(0,1,length.out = n_steps + 1))
  ilr_path <- lapply(steps, function(s) ilr_start + s * ilr_dir)
  ilr_path <- do.call("rbind", ilr_path)
  CoDa_seq <- rbind(ilrInv(ilr_path))
  if (add_opposite) {
    steps <- rev(steps[-1])
    ilr_path <- lapply(steps, function(s) ilr_start - s * ilr_dir)
    ilr_path <- do.call("rbind", ilr_path)
    CoDa_seq <- rbind(ilrInv(ilr_path), CoDa_seq)
  }
  row.names(CoDa_seq) <- seq(-n_steps *add_opposite, n_steps)


  if (!is.null(names(comp_from)))
    colnames(CoDa_seq) <- names(comp_from)
  return(as.data.frame(CoDa_seq * Tstart))
}




#' Create a linear path in the simplex by defining a direction and a step size
#'
#' @details
#' The function is very similar to [CoDa_seq()].
#' However, of drawing a line between a starting and end point it uses only a starting point and a direction.
#'
#' @param comp_direc A numeric vector, defining a direction in the simplex
#' @param comp_from A numeric vector, an initial point in the simplex - defaults to a balanced composition, which represents the origin in the simplex
#' @param step_size A numeric, indicting the step size
#' @param n_steps A numeric, indicating the number of steps to be taking from `comp_from`
#' @param add_opposite A logical, if `TRUE` steps in the opposite direction are also computed
#' @param dir_from_start A logical, if `TRUE` the direction is calculated from the difference between `comp_from` and `comp_direc`
#'
#' @importFrom compositions ilr
#' @return A data.frame frame where each point corresponds to one compositional vector
#' @seealso CoDa_seq
#' @export
#'
#' @author Lukas Dargel
#' @references
#' @examples
#'
#'
#' # three steps that go from the origin towards the defined direction
#' comp_direc <- c(A =.4,B = .35, C= .25)
#' CoDa_path(comp_direc, n_steps = 3)
#'
#'
#' # we can draw the path that is defined by this direction
#' comp_direc <- c(A =.4,B = .35, C= .25)
#' compositions::plot.acomp(CoDa_path(comp_direc,n_steps = 10))
#' compositions::plot.acomp(CoDa_path(comp_direc,n_steps = 100))
#' compositions::plot.acomp(CoDa_path(comp_direc,add_opposite = TRUE))
#'
#'
#' # using the same direction we can draw a new path that does not go through the origin
#' comp_direc <- c(A =.4,B = .35, C= .25)
#' comp_from <- c(.7,.2,.1)
#' compositions::plot.acomp(CoDa_path(comp_direc, comp_from,n_steps = 10))
#' compositions::plot.acomp(CoDa_path(comp_direc, comp_from,n_steps = 100))
#' compositions::plot.acomp(CoDa_path(comp_direc, comp_from,add_opposite = TRUE))
#'
#'
#' # the balanced composition does not define a direction by itself
#' comp_dir <- c(A = 1/3, B = 1/3, C= 1/3) # corresponds to a zero vector in real space
#' \dontrun{CoDa_path(comp_direc, comp_from,add_opposite = TRUE)}
#'
#' # with the dir_from_start option the direction is derived from the line connecting two compositions
#' compositions::plot.acomp(CoDa_path(comp_direc, comp_from,add_opposite = TRUE, dir_from_start = TRUE))
CoDa_path <- function(
  comp_direc,
  comp_from,
  step_size = 0.01,
  n_steps = 100,
  add_opposite = FALSE,
  dir_from_start = FALSE) {


  D <- length(comp_direc)
  if (missing(comp_from))
    comp_from <- comp_direc * 0 + 1/D

  n_steps <- round(n_steps)
  stopifnot(
    all(comp_direc > 0, comp_from > 0),
    length(comp_direc) > 2,
    length(comp_from) == length(comp_direc),
    is.numeric(step_size) & length(step_size) == 1,
    isTRUE(n_steps >= 1))

  if (is.null(names(comp_from)))
    names(comp_from) <- names(comp_direc)


  ilr_direc <- ilr(comp_direc)
  ilr_start <- ilr(comp_from)
  if (dir_from_start)
    ilr_direc <- ilr_start - ilr_direc
  if (all(ilr_direc == 0))
    stop("The provided information does not identify a direction in the simplex!")
  ilr_direc <- ilr_direc/sqrt(sum(ilr_direc^2))

  # calculate the end point and use CoDa_seq
  ilr_end <- ilr_start + ilr_direc * n_steps * step_size
  comp_to <- ilrInv(ilr_end)
  return(CoDa_seq(
    comp_from,
    comp_to,
    n_steps,
    add_opposite))
}

