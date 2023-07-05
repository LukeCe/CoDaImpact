#' @title Tabulate effects of infinitesimal changes in CoDa models
#'
#' @description
#' Compute variations
#'
#' @details
#' Developped in Dargel and Thomas-Agnan (2023)
#'
#' @param object
#' @param Xvar
#' @param Xdir
#' @param inc_size
#' @param inc_rate
#' @param obs
#' @param Xtotal
#'
#' @return
#' @export
#'
#' @examples
VariationTable <- function(
    object,
    Xvar,
    Xdir,
    Ytotal = 1,
    obs = 1,
    inc_size = .1,
    inc_rate = NULL) {

  stopifnot(is(object, "lmCoDa"),
            is.character(Xvar) && length(Xvar) == 1,
            is.numeric(Ytotal) && length(Ytotal) == 1,
            length(obs) == 1,
            is.numeric(inc_size) && length(inc_size) == 1,
            is.null(inc_rate) || (0 < inc_rate && inc_rate < 1))

  trSry <- object$trSry
  XY_is_compo <- unlist("" != trSry$LR_TRAN[c(1, Xvar)], use.names = FALSE)
  if (all(!YX_is_compo)) stop("Variations are not meaningful when Y and X are both not compositional!")

  # get X0
  Anames <- unlist(traSry$NAME_SIMPLEX)
  Xnames <- setdiff(Anames[-1], "(Intercept)")
  if (!Xvar %in% Xnames) stop("Xvar musst be one of ", list(Xnames), "!")
  Xvar <- unlist(traSry$NAME_COORD[Xvar == Anames])
  Dx <- traSry$D[[Xvar]]
  scalar_x <- Dx == 0
  Kx <- traSry$LR_BASE_K[[Xvar]]
  X0 <- object$model[[Xvar]]
  X0 <- if (scalar_x) X0[obs] else clo2(exp(t(X0[obs,]) %*% t(Kx)))

  # get Y0
  scalar_y <- traSry$D[[1]] == 0
  Y0 <- if (scalar_y) fitted(object)[obs] else fitted(object, space = "simplex")[obs,]
  Y0 <- as(Y0, "vector")

  if (is.character(Xdir)) {
    # convert preferential to general direction
    if (!isTRUE(Xdir %in%  colnames(X0))) stop("Xdir must be one of ", list(colnames(X0)), "!")
    Xdir <- clo2(exp(colnames(X0) == Xdir)^sqrt(Dx/(Dx-1)))
    if (!is.null(inc_rate)) inc_size <- inc_rate * Xdir && stop("To be corrected")
  }


  elasti <- Impacts(object, Xvar, obs)
  elasti <- elasti %*% log(Xdir)
  Y1     <- Y0 * (1 + elasti * inc_size)
  diff_Y <- Y1 - Y0
  result <- data.frame(
    "Elasticity"            = elasti,
    "Initial parts"         = Y0,
    "New parts"             = Y1,
    "Variation in %"        = diff_Y/Y0 * 100,
    "Variation in % points" = diff_Y    * 100,
    "Variation in units"    = diff_Y*Ytotal)



  attr(result,'X(0)')        <- X0
  attr(result,'Xdirection')  <- Xdir
  attr(result,'inc_size')    <- inc_size
  attr(result,'inc_rate')    <- inc_rate


}

Variation_Table <- function(object, Var_Name, Xdirection, Inc_rate, Inc_size, Obs=1 , Total_obs=1){
  #Remark: add alpha's computation for both cases
  Var_Names_COORD <- names(which(object$ToSimplex$TSR$NAME_SIMPLEX==Var_Name))
  Dx <- object$ToSimplex$TSR$D[[Var_Names_COORD]]
  MS<- model.simplex(object)

  }else{
    if (is.character(Xdir)){
      n=which(rownames(elasti)==Xdir)
      Inc_size          <- Inc_rate/(1-model.simplex(object)[Obs,Xvar][n])
    }else{if (is.numeric(Xdir)){
      Inc_size          <- Inc_rate/(sqrt(Dx/(Dx-1))*(1-mean(model.simplex(object)[Obs,Xvar])))}
    }
  }
  }
  if ((Dx > 0) & is.character(Xdir)){
    if (Xdir %in% colnames(MS[[Xvar]])){
      k=which(colnames(MS[[Xvar]])==Xdir)
    }else{
      stop("As a character, Xdirection must be one of the following", list(colnames(MS[[Xvar]])))
    }
    if (k==1){Xdir <- c(exp(1),rep(1,Dx-1))
    }else{if (k==Dx){Xdir <- c(rep(1,Dx-1),exp(1))
    }else{Xdir <- c(rep(1,k-1),exp(1),rep(1,Dx-k))}
    }
  }
  if(is.numeric(Xdir)){
    Xdir <- ilrInv((ilr(Xdir))/(norm(ilr(Xdir))))
  }
  if (is.null(ncol(elasti)) | ncol(elasti)==1){
    Table <- c()
    Sum=0
    for (m in 1:length(Xdir)){
      Sum = Sum + (elasti[m])*log(Xdir[m])
    }
    Sum1 = Inc_size * Sum
    Table <- (fity[Obs])*(1 + Sum1)
    Table <- as.matrix(Table)
    Table <- rbind(Sum,
                   fity[Obs],
                   Table,
                   (Table-as.numeric(fity[Obs])))
    rownames(Table)           <- c('Elasticity','Initial value','New value','Variation in units')
    colnames(Table)           <- colnames(coefficients.lmSimplex(object)$Coefficients_SIMPLEX)
  }
  else{
    if (nrow(elasti)==1){
      Table <- c()
      for (j in colnames(coefficients.lmSimplex(object)$Coefficients_SIMPLEX)){
        Prod<-Inc_size*(elasti[,j])
        Table[j] <- (fity[Obs][j])*(1 + Prod)
      }
      Table <- t(as.matrix(Table))
      Table <- rbind(elasti,
                     fity[Obs],
                     Table,
                     (Table-as.numeric(fity[Obs]))/as.numeric(fity[Obs])*100,
                     (Table-as.numeric(fity[Obs]))*100,
                     (Table-as.numeric(fity[Obs]))*Total_obs)
      rownames(Table)           <- c('Elasticity','Initial parts','New parts','Variation in %','Variation in % points','Variation in units')
    }
    else{
      Table <- c()
      ELASTICITY  <- list()
      for (j in colnames(coefficients.lmSimplex(object)$Coefficients_SIMPLEX)){
        Sum=0
        for (m in 1:length(Xdir)){
          Sum = Sum + (elasti[m,j])*log(Xdir[m])
        }
        Sum1 = Inc_size * Sum
        Table[j] <- (fity[Obs,j])*(1 + Sum1)
        ELASTICITY[j]<-Sum
      }
      Table <- t(as.matrix(Table))
      Table <- rbind(ELASTICITY,
                     fity[Obs],
                     Table,
                     (Table-as.numeric(fity[Obs]))/as.numeric(fity[Obs])*100,
                     (Table-as.numeric(fity[Obs]))*100,
                     (Table-as.numeric(fity[Obs]))*Total_obs)
      rownames(Table)           <- c('Elasticity','Initial parts','New parts','Variation in %','Variation in % points','Variation in units')
    }
  }
  attr(Table,'X(0)')        <- MS[Obs,Xvar]
  attr(Table,'Xdirection')  <- ifelse(nrow(elasti)==1,rownames(elasti),Xdir)
  attr(Table,'Inc_size')    <- Inc_size
  attr(Table,'Inc_rate')    <- ifelse (missing(Inc_rate),NA,Inc_rate)
  return(Table)
}
