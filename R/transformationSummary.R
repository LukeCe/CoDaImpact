#' Summarize the transformations in a CoDa model
#'
#' Extract from a CoDa model estimated by `lm()` all information related to the
#' log-ratio transformations of the variables and the parameters.
#'
#' The structure of the object resembles a data.frame, where rows correspond
#' to the variables and columns to different information sets related to the
#' transformations involved with each variable.
#'
#' @importFrom compositions clrInv
#' @keywords internal
transformationSummary <- function(lm_res) {

  coef_mat <- t(t(coef(lm_res)))
  mod <- lm_res[["model"]]
  if ("(Intercept)" %in% rownames(coef_mat))
    mod <- cbind(mod[1], "(Intercept)" = 1, mod[-1])

  info_list <- list(
    "NAME_COORD"   = list(NA_character_),
    "NAME_SIMPLEX" = list(NA_character_),
    "IS_RESPONSE"  = list(TRUE),
    "D"            = list(NA_integer_),
    "LR_TRAN"      = list(NA_character_),
    "LR_BASE_F"    = list(matrix(0,0,0)),
    "LR_BASE_K"    = list(matrix(0,0,0)),
    "COEF_COORD"   = list(matrix(0,0,0)),
    "COEF_CLR"     = list(matrix(0,0,0)),
    "COEF_SIMPLEX" = list(matrix(0,0,0)))

  result <- data.frame()
  result[names(mod), names(info_list)] <- info_list
  for (i in seq_along(info_list)) names(result[[i]]) <- names(mod)

  i_coef <- 0
  for (i in seq_along(mod)) {

    i_var <- names(mod)[i]
    trans <- whichTrans(mod[i_var])
    result$"LR_TRAN"[[i]]   <- trans[["name"]]
    result$"LR_BASE_F"[[i]] <- trans[["base_F"]]
    result$"LR_BASE_K"[[i]] <- trans[["base_K"]]
    result$"D"[[i]]         <- max(dim(trans[["base_F"]]))

    result$"NAME_COORD"[[i]]   <- i_var
    result$"NAME_SIMPLEX"[[i]] <- if (trans[["name"]] == "") i_var else sprintf("%sInv(%s)", trans[["name"]], i_var)

    if (i > 1) {
      result$"IS_RESPONSE"[[i]]  <- FALSE

      Ky <- result$"LR_BASE_K"[[1]] # K and F are both V in the case of ilr
      Fy <- result$"LR_BASE_F"[[1]]
      Kx <- result$"LR_BASE_K"[[i]]
      Fx <- result$"LR_BASE_F"[[i]]
      Dx <- result$"D"[[i]]
      i_coef <- max(i_coef) + seq_len(max(1,Dx - 1))

      # Four cases....
      y_is_compo <- inherits(fitted(lm_res),"rmult")
      x_is_compo <- Dx >= 1

      if (y_is_compo & x_is_compo) {
        result$"COEF_COORD"[[i]]   <- coef_mat[i_coef,,drop = FALSE]
        result$"COEF_CLR"[[i]]     <- t(Fx) %*% result$"COEF_COORD"[[i]] %*% t(Ky)
        result$"COEF_SIMPLEX"[[i]] <- result$"COEF_CLR"[[i]]
      } # 1. (YX)

      if (y_is_compo & !x_is_compo) {
        result$"COEF_COORD"[[i]]   <- coef_mat[i_var,,drop = FALSE]
        result$"COEF_CLR"[[i]]     <- result$"COEF_COORD"[[i]] %*% t(Ky)
        result$"COEF_SIMPLEX"[[i]] <- clrInv(result$"COEF_CLR"[[i]])
      } # 2. (Y)

      if (!y_is_compo & x_is_compo) {
        result$"COEF_COORD"[[i]]   <- coef_mat[i_coef,,drop = FALSE]
        result$"COEF_CLR"[[i]]     <- t(Fx) %*% result$"COEF_COORD"[[i]]
        result$"COEF_SIMPLEX"[[i]] <- t(clrInv(t(result$"COEF_CLR"[[i]])))
      } # 3. (X)

      if (!y_is_compo & !x_is_compo) {
        result$"COEF_COORD"[[i]]   <- coef_mat[i_var,,drop = FALSE]
        result$"COEF_CLR"[[i]]     <- coef_mat[i_var,,drop = FALSE]
        result$"COEF_SIMPLEX"[[i]] <- coef_mat[i_var,,drop = FALSE]
      } # 4. ( )
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
  resTrans <- function(n = "", bF = matrix(0,0,0), bK = matrix(0,0,0)) list(name = n, base_F = bF, base_K = bK)
  if (!inherits(x[[1]], what = "rmult"))
    return(resTrans())

  V <- compositions:::gsi.getV(x[[1]])
  colnames(V) <- paste0(names(x), if (ncol(V) == 1) "" else seq_len(ncol(V)))
  rownames(V) <- colnames(attr(x[[1]],"orig"))

  VVi <- crossprod(V)
  VVo <- tcrossprod(V)
  D <- nrow(VVo)

  r0 <- function(x) round(x, 12)
  if (all(r0(VVi) == diag(D-1)))
    return(resTrans("ilr",t(V), V))


  F_alr <- alr_K2F(V)
  K_alr <- V
  KF <- K_alr %*% F_alr
  FK <- F_alr %*% K_alr
  if (all(r0(FK - diag(D-1)) == 0) && all(r0(KF - clrBase(D)) == 0))

    return(resTrans("alr", F_alr, K_alr))

  stop("Transformation not identifiable!")
}


#' @keywords
alr_K2F <- function(K_alr) {
  i_ref <- which.min(rowSums(K_alr))
  F_alr <- 0*K_alr - 1
  F_alr[-i_ref,] <- diag(min(dim(K_alr)))
  return(t(F_alr))
}


#' @keywords internal
clrBase <- function(D) {
  diag(D) - 1/D
}
