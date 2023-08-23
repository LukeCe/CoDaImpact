#' Internal: check for valid computational direction arguments
#'
#' @param Xdir a character or numeric indicating the direction
#' @param Xopts a character indicating the names of the vertices
#' @param normalize a logical if true Xdir is normalized to have an Aitchison norm of 1
#' @return a numeric vector
#'
#' @author Lukas Dargel
#' @keywords internal
check_Xdir <- function(Xdir, Xopts, normalize = FALSE) {

  stopifnot(is.numeric(Xdir) || is.character(Xdir),
            is.character(Xopts),
            is.logical(normalize))
  Dx <- length(Xopts)

  if (is.numeric(Xdir)) {
    check <- "When Xdir is numeric it musst be a non-negative vector of dimension %s!"
    if (length(Xdir) != Dx || any(Xdir < 0)) stop(sprintf(check, Dx))
    if (normalize) {
      Xdir <- ilr(Xdir)
      Xdir <- as(ilrInv(Xdir/sqrt(sum(Xdir^2))),"vector")
    } else Xdir <- Xdir/sum(Xdir)
    return(Xdir)
  }

  if (is.character(Xdir)) {
    check <- "When Xdir is character it musst be one of %s!"
    Xbin <- Xopts %in% Xdir
    if (sum(Xbin) != 1) stop(sprintf(check, list(Xopts)))

    Xdir <- exp(Xbin)
    if (normalize) Xdir[Xbin] <- Xdir[Xbin]^sqrt(Dx/(Dx-1))
    return(Xdir/sum(Xdir))
  }
}
