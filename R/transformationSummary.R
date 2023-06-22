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
  if ("(weights)" %in% names(mod))
    mod[["(weights)"]] <- NULL

  info_list <- list(
    "NAME_COORD"   = list(NA_character_),
    "NAME_SIMPLEX" = list(NA_character_),
    "IS_RESPONSE"  = list(FALSE),
    "D"            = list(NA_integer_),
    "LR_TRAN"      = list(NA_character_),
    "LR_BASE_F"    = list(matrix(0,0,0)),
    "LR_BASE_K"    = list(matrix(0,0,0)),
    "COEF_COORD"   = list(matrix(0,0,0)),
    "COEF_CLR"     = list(matrix(0,0,0)),
    "COEF_SIMPLEX" = list(matrix(0,0,0)),
    "VARCOV_COORD" = list(matrix(0,0,0)),
    "VARCOV_CLR"   = list(matrix(0,0,0)))

  result <- data.frame()
  result[names(mod), names(info_list)] <- info_list
  for (i in seq_along(info_list)) names(result[[i]]) <- names(mod)


  # First treat the elements related to the response variable
  trans_Y <- whichTrans(mod[1])
  Ky <- trans_Y[["base_K"]] # K and F are both V in the case of ilr
  Fy <- trans_Y[["base_F"]]

  cov_Yclr <- cov_Y <- crossprod(resid(lm_res))/(nobs(lm_res) - nrow(coef_mat))
  if (length(Fy) > 0) cov_Yclr <- t(Fy) %*% cov_Y %*% t(Ky)

  result$"NAME_COORD"[[1]]   <- names(mod)[1]
  result$"NAME_SIMPLEX"[[1]] <- name_invTrans(names(mod)[1], trans_Y[["name"]])
  result$"IS_RESPONSE"[[1]]  <- TRUE
  result$"D"[[1]]            <- max(dim(Ky))
  result$"LR_TRAN"[[1]]      <- trans_Y[["name"]]
  result$"LR_BASE_K"[[1]]    <- Ky
  result$"LR_BASE_F"[[1]]    <- Fy
  result$"VARCOV_COORD"[[1]] <- cov_Y
  result$"VARCOV_CLR"[[1]]   <- cov_Yclr


  # prepare loop over variables
  cov_X <- qr.R(lm_res[["qr"]])
  cov_X <- chol2inv(cov_X)
  dimnames(cov_X) <- list(rownames(coef_mat))[c(1,1)]
  i_coef <- 0
  for (i in seq_along(mod)[-1]) {

    i_var <- names(mod)[i]
    trans_X <- whichTrans(mod[i_var])
    Fx <- trans_X[["base_F"]]
    Kx <- trans_X[["base_K"]]
    Dx <- max(dim(Kx))
    i_coef <- max(i_coef) + seq_len(max(1,Dx - 1))
    cov_Xi <- cov_X[i_coef, i_coef, drop = FALSE]

    result$"NAME_COORD"[[i]]   <- i_var
    result$"NAME_SIMPLEX"[[i]] <- name_invTrans(i_var, trans_X[["name"]])
    result$"LR_TRAN"[[i]]   <- trans_X[["name"]]
    result$"LR_BASE_F"[[i]] <- Fx
    result$"LR_BASE_K"[[i]] <- Kx
    result$"D"[[i]]         <- Dx

    # Four cases....
    y_is_compo <- inherits(fitted(lm_res),"rmult")
    x_is_compo <- Dx >= 1

    if (y_is_compo & x_is_compo) {
      result$"COEF_COORD"[[i]]   <- coef_mat[i_coef,,drop = FALSE]
      result$"COEF_CLR"[[i]]     <- t(Fx) %*% result$"COEF_COORD"[[i]] %*% t(Ky)
      result$"COEF_SIMPLEX"[[i]] <- result$"COEF_CLR"[[i]]
      result$"VARCOV_COORD"[[i]] <- cov_Xi
      result$"VARCOV_CLR"[[i]]   <- t(Fx) %*% cov_Xi %*% t(Kx)
    } # 1. (Y & X)

    if (y_is_compo & !x_is_compo) {
      result$"COEF_COORD"[[i]]   <- coef_mat[i_var,,drop = FALSE]
      result$"COEF_CLR"[[i]]     <- result$"COEF_COORD"[[i]] %*% t(Ky)
      result$"COEF_SIMPLEX"[[i]] <- clrInv(result$"COEF_CLR"[[i]])
      result$"VARCOV_COORD"[[i]] <- cov_Xi
      result$"VARCOV_CLR"[[i]]   <- cov_Xi
    } # 2. (Y only)

    if (!y_is_compo & x_is_compo) {
      result$"COEF_COORD"[[i]]   <- coef_mat[i_coef,,drop = FALSE]
      result$"COEF_CLR"[[i]]     <- t(Fx) %*% result$"COEF_COORD"[[i]]
      result$"COEF_SIMPLEX"[[i]] <- t(clrInv(t(result$"COEF_CLR"[[i]])))
      result$"VARCOV_COORD"[[i]] <- cov_Xi
      result$"VARCOV_CLR"[[i]]   <- t(Fx) %*% cov_Xi %*% t(Kx)
    } # 3. (X only)

    if (!y_is_compo & !x_is_compo) {
      result$"COEF_COORD"[[i]]   <- coef_mat[i_var,,drop = FALSE]
      result$"COEF_CLR"[[i]]     <- coef_mat[i_var,,drop = FALSE]
      result$"COEF_SIMPLEX"[[i]] <- coef_mat[i_var,,drop = FALSE]
      result$"VARCOV_COORD"[[i]] <- cov_Xi
      result$"VARCOV_CLR"[[i]]   <- cov_Xi
    } # 4. (no composition)
  }
  return(result)
}


#' @keywords internal
name_invTrans <- function(name, trans) {
  if (trans != "") name <- sprintf("%sInv(%s)",trans,name)
  return(name)
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

  K_alr <- V
  F_alr <- alr_K2F(K_alr)
  KF <- K_alr %*% F_alr
  FK <- F_alr %*% K_alr
  if (all(r0(FK - diag(D-1)) == 0) && all(r0(KF - clrBase(D)) == 0))
    return(resTrans("alr", F_alr, K_alr))

  stop("Transformation not identifiable!")
}


#' @keywords
alr_K2F <- function(K_alr) {
  i_ref <- which.min(rowSums(K_alr))
  F_alr <- (K_alr * 0) - 1
  F_alr[-i_ref,] <- diag(min(dim(K_alr)))
  return(t(F_alr))
}


#' @keywords internal
clrBase <- function(D) {
  diag(D) - 1/D
}
