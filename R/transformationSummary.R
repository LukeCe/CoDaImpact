#' Summarize the transformations in a CoDa model
#'
#' Extract from a CoDa model estimated by `lm()` all information related to the
#' log-ratio transformations of variables and parameters.
#'
#' The structure of the object resembles a data.frame, where rows correspond
#' to the variables and columns to diffrent information sets related to the
#' transformations invloved with each variable.
#'
#' @importFrom compositions clrInv
#' @keywords internal
transformationSummary <- function(lm_res) {

  coef_mat <- t(t(coef(lm_res)))
  mod <- lm_res[["model"]]
  if ("(Intercept)" %in% rownames(coef_mat))
    mod <- cbind(mod[1], "(Intercept)" = 1, mod[-1])

  mat0 <- list(matrix(0,0,0))
  info_list <- list(
    "NAME_COORD"   = list(NA_character_),
    "NAME_SIMPLEX" = list(NA_character_),
    "IS_RESPONSE"  = list(TRUE),
    "D"            = list(NA_integer_),
    "LR_TRAN"      = list(NA_character_),
    "LR_BASE"      = mat0,
    "COEF_COORD"   = mat0,
    "COEF_CLR"     = mat0,
    "COEF_SIMPLEX" = mat0)

  result <- data.frame()
  result[names(mod), names(info_list)] <- info_list
  for (i in seq_along(info_list)) names(result[[i]]) <- names(mod)

  y_is_compo <- inherits(fitted(lm_res),"rmult")
  if (y_is_compo) {
    Vy <- whichTrans(mod[1])[["base"]]
    coef_mat2 <- coef_mat %*% t(Vy)
  }

  for (i in seq_along(mod)) {

    i_var <- names(mod)[i]
    trans <- whichTrans(mod[i_var])
    result$"LR_TRAN"[[i]] <- trans[["name"]]
    result$"LR_BASE"[[i]] <- trans[["base"]]
    result$"D"[[i]]       <- max(dim(trans[["base"]]))

    result$"NAME_COORD"[[i]]   <- i_var
    result$"NAME_SIMPLEX"[[i]] <- name_invTrans(i_var)

    if (i > 1) {
      result$"IS_RESPONSE"[[i]]  <- FALSE

      Vx <- trans[["base"]]
      Dx <- result$"D"[[i]]
      x_is_compo <- Dx >= 1
      x_names <- paste0(i_var, if (Dx >= 3) seq(Dx-1) else "")

      # Four cases....
      # 1. (YX)
      if (y_is_compo & x_is_compo) {
        result$"COEF_COORD"[[i]]   <- coef_mat[x_names,,drop = FALSE]
        result$"COEF_CLR"[[i]]     <- Vx %*% result$"COEF_COORD"[[i]] %*% t(Vy)
        result$"COEF_SIMPLEX"[[i]] <- result$"COEF_CLR"[[i]]
      }

      # 2. (Y)
      if (y_is_compo & !x_is_compo) {
        result$"COEF_COORD"[[i]]   <- coef_mat[i_var,,drop = FALSE]
        result$"COEF_CLR"[[i]]     <- result$"COEF_COORD"[[i]] %*% t(Vy)
        result$"COEF_SIMPLEX"[[i]] <- clrInv(result$"COEF_CLR"[[i]])
      }

      # 3. (X)
      if (!y_is_compo & x_is_compo) {
        result$"COEF_COORD"[[i]]   <- coef_mat[x_names,,drop = FALSE]
        result$"COEF_CLR"[[i]]     <- Vx %*% result$"COEF_COORD"[[i]]
        result$"COEF_SIMPLEX"[[i]] <- t(clrInv(t(result$"COEF_CLR"[[i]])))
      }

      # 4. ( )
      if (!y_is_compo & !x_is_compo) {
        result$"COEF_COORD"[[i]]   <- coef_mat[i_var,,drop = FALSE]
        result$"COEF_CLR"[[i]]     <- coef_mat[i_var,,drop = FALSE]
        result$"COEF_SIMPLEX"[[i]] <- coef_mat[i_var,,drop = FALSE]
      }
    }
  }
  return(result)
}


#' @keywords internal
name_invTrans <- function(x) {
  xTrans <- switch (substr(gsub("\\s", "", x), 1,4),
                    "ilr(" = "ilrInv(%s)",
                    "alr(" = "alrInv(%s)",
                    "clr(" = "clrInv(%s)",
                    "%s")
  sprintf(xTrans, x)
}

#' @keywords internal
whichTrans <- function(x) {

  stopifnot(is.data.frame(x) & length(x) == 1)
  resTrans <- function(n = "",b = matrix(0,0,0)) list(name = n, base = b)
  if (!inherits(x[[1]], what = "rmult"))
    return(resTrans())

  V <- compositions:::gsi.getV(x[[1]])
  colnames(V) <- paste0(names(x), if (ncol(V) == 1) "" else seq_len(ncol(V)))
  rownames(V) <- colnames(attr(x[[1]],"orig"))

  VVi <- crossprod(V)
  VVo <- tcrossprod(V)
  D <- nrow(VVo)

  if (all(abs(VVi - diag(nrow(VVi))) < 1e-12))
    return(resTrans("ilr",V))

  if (all(abs(VVi - clrBase(D)[-1,-1, drop = FALSE]) < 1e-12))
    return(resTrans("alr",V))

  stop("Transformation not identifiable!")
}


#' @keywords internal
clrBase <- function(D) {
  diag(D) - 1/D
}
