#' @keywords internal
transformationSummary <- function(lm_res) {

  mod <- lm_res[["model"]]
  mat0 <- list(matrix(nrow = 0,ncol = 0))

  info_list <- list(
    "NAME_COORD" = NA_character_,
    "NAME_SIMPLEX" = NA_character_,
    "IS_RESPONSE" = TRUE,
    "D" = NA_integer_,
    "LR_BASE" = mat0,
    "COEF_COORD" = mat0,
    "COEF_SIMPLEX" = mat0,
    "COEF_CLR" = mat0)


  result <- data.frame()
  result[names(mod), names(info_list)] <- info_list
  names(result$LR_BASE)      <- names(mod)
  names(result$COEF_COORD)   <- names(mod)
  names(result$COEF_SIMPLEX) <- names(mod)
  names(result$COEF_CLR)     <- names(mod)

  y_is_compo <- inherits(fitted(lm_res),"rmult")
  coef_mat <- t(t(coef(lm_res)))
  for (i in seq_along(mod)) {

    i_var <- names(mod)[i]
    result[i, "NAME_COORD"]  <- i_var
    result[i, "NAME_SIMPLEX"] <- name_invTrans(i_var)

    trans <- whichTrans(mod[[i_var]])
    result[i, "TRANS"]   <- trans[["name"]]
    result$LR_BASE[[i]] <- trans[["base"]]
    result[i, "D"]       <- max(dim(trans[["base"]]))

    if (i > 1) {
      result[i, "IS_RESPONSE"]  <- FALSE
      Dx <- result[i_var, "D"]
      x_is_compo <- Dx >= 1
      x_names <- paste0(if (Dx >= 3) seq(Dx-1) else "", i_var)

      # Four cases....
      # 1. (YX)
      if (y_is_compo & x_is_compo) {
        result$COEF_COORD[[i]] <- coef_mat[x_names,,drop=FALSE]

        coef_clr <- result$LR_BASE[[i]] %*% result$COEF_COORD[[i]]
        coef_clr <- coef_clr %*% t(result$LR_BASE[[1]])
        rownames(coef_clr) <- colnames(attr(mod[[i_var]], "orig"))
        result$COEF_CLR[[i]]     <- coef_clr

        result$COEF_SIMPLEX[[i]] <- coef_clr
      }

      # 2. (Y)
      if (y_is_compo & !x_is_compo) {
        result[i_var, "COEF_COORD"]   <- coef_mat[i_var,,drop = FALSE]
        result[i_var, "COEF_CLR"]     <- result[i_var,"COEF_COORD"] %*% t(result[1, "LR_BASE"])
        result[i_var, "COEF_SIMPLEX"] <- clrInv(result[i_var, "COEF_CLR"])
      }

      # 3. (X)
      if (!y_is_compo & x_is_compo) {
        result[i_var, "COEF_COORD"] <- coef_mat[x_names,,drop=FALSE]
        result[i_var, "COEF_CLR"] <- result[i_var, "LR_BASE"] %*% result[,"COEF_COORD"]
        result[i_var, "COEF_SIMPLEX"] <- clrInv(result[i_var,"COEF_CLR"])
      }

      # 4. ( )
      if (!y_is_compo & !x_is_compo)
        result[i_var, "COEF_COORD"]   <- coef_mat[i_var,,drop = FALSE]
    }
  }
  return(result)
}


#' @keywords internal
name_invTrans <- function(x) {

  xTrans <- switch (substr(gsub("\\s", "", x),1,4),
                    "ilr(" = "ilrInv(%s)",
                    "alr(" = "alrInv(%s)",
                    "clr(" = "clrInc(%s)",
                    "%s")
  sprintf(xTrans, x)
}

#' @keywords
whichTrans <- function(x) {


  resTrans <- function(n = "",b = matrix(0,0,0)) list(name = n, base = b)
  if (!inherits(x, what = "rmult"))
    return(resTrans())

  V <- compositions:::gsi.getV(x)
  VVi <- crossprod(V)
  VVo <- tcrossprod(V)
  D <- nrow(VVo)

  if (all(abs(VVi - diag(nrow(VVi))) < 1e-12))
    return(resTrans("ilr",V))

  if (all(abs(VVi - clrBase(D)[-1,-1, drop = FALSE]) < 1e-12))
    return(resTrans("alr",V))

  stop("Transformation not identifiable!")
}


clrBase <- function(D) {
  diag(D) - 1/D
}

#' @keywords internal
"%||%" <- function(x, y) {
  if (length(x) == 0) y else x
}
