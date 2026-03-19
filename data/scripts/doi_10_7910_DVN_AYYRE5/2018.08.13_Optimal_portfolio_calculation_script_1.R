library("nloptr")
library("VineCopula")
library("QRM")
library("R.utils")

AL.CALC <- function(PAY, m, m_Y, AL){
  AL_L <- PAY[m] * m_Y * AL
  return(AL_L)
}

PAY.CALC <- function(PAY_START, INC_RATE, m){
  x1 <- matrix(0,ncol = 1,nrow = m)
  x1[1,] <- PAY_START * (1 + INC_RATE[1,])
  for (i in 2:m) {
    x1[i,] <- x1[i - 1,]*(1 + INC_RATE[i,])
  }
  return(x1)
}

PAY.CALC <- function(PAY_START, INC_RATE, m){
  x1 <- matrix(0,ncol = 1,nrow = m)
  x1[1,] <- PAY_START * (1 + INC_RATE[1,])
  for (i in 2:m) {
    x1[i,] <- x1[i - 1,]*(1 + INC_RATE[i,])
  }
  return(x1)
}


TOT.INC <- function(RET, RENT_RATE, PAY_START, PAY, m){
  x1 <- (RENT_RATE * PAY[1,]) + (RENT_RATE * PAY_START)*(1 + RET[1,])
  for (i in 2:m) {
    x1 <- (RENT_RATE * PAY[i,]) + x1 * (1 + RET[i,])
  }
  return(x1)		
}

INC.CALC <- function(WEIGHTS, BCSS, RENT_RATE, PAY_START, PAY, n, m, muY){
  x1 <- matrix(1,ncol = 1,nrow = n - 1)
  WEIGHTSraw <- matrix(WEIGHTS,ncol = n - 1,nrow = m,byrow = FALSE)
  mWEIGHTS <- matrix(WEIGHTS,ncol = n,nrow = m,byrow = FALSE)
  mWEIGHTS[,n] <- 1 - WEIGHTSraw %*% x1
  TOT_INC <- matrix(1, ncol = 1, nrow = m) 
  for (i in 1:m) {
    BCSS2 <- matrix(1,ncol = 2,nrow = m)
    for (j in 1:m) {
      if (j > i) {
        BCSS2[j,1] <- muY[1,1]
        BCSS2[j,2] <- muY[2,1]
      } else {
        BCSS2[j,1] <- BCSS[j,1]
        BCSS2[j,2] <- BCSS[j,2]
      }
    }
    RET <- matrix(rowSums(BCSS2*mWEIGHTS),ncol = 1,nrow = m)
    TOT_INC[i,1] <- TOT.INC(RET, RENT_RATE, PAY_START, PAY, m)
  }
  return(TOT_INC)
}

prob.not.al <- function(WEIGHTS, BCSS, RENT_RATE, PAY_START, PAY, AL, n, m, muY){
  inctab <- INC.CALC(WEIGHTS, BCSS, RENT_RATE, PAY_START, PAY, n, m, muY)
  sdinc <- sd(inctab)
  meaninc <- mean(inctab)
  result <- pnorm(AL, meaninc, sdinc)
  return(result)
}

wsum <- function(WEIGHTS, BCSS, RENT_RATE, PAY_START, PAY, AL, n, m, muY){
  mWEIGHTS <- matrix(WEIGHTS,ncol = n - 1,nrow = m,byrow = FALSE)
  x1 <- matrix(1,ncol = 1,nrow = n - 1)
  x2 <- matrix(1,ncol = 1,nrow = m)
  result <- mWEIGHTS %*% x1 - x2
  return(result)
}

VS.min.al <- function(BiCopStr_n, nu, muY, sigmaY, lp, RENT_RATE, PAY_START, PAY, AL, n, m) {
  result <- list()
  x0 <- matrix(0.5,ncol = n - 1, nrow = m)
  lb <- matrix(0,ncol = n - 1, nrow = m)
  ub <- matrix(1,ncol = n - 1, nrow = m)
  x1 <- matrix(1,ncol = 1,nrow = n - 1)
  for (i in (1:lp)) {
    BCS <- BiCopSim(m, BiCopStr_n)
    colnames(BCS) <- c("ZUS_POL", "ST_POL")
    BCSR <- data.frame(BCS)
    obj <- matrix(0,ncol = 1,nrow = m)
    for (j in 1:ncol(BCSR)) {
      BCSR[,j] <- qt(BCSR[,j], nu[j,1])
      BCSR[,j] <- (BCSR[,j]*sigmaY[j,1]) + muY[j,1]
    }
    BCSS <- cbind(BCSR["ZUS_POL"],BCSR["ST_POL"])
    res <- nloptr(x0 = x0, eval_f = prob.not.al, lb = lb, ub = ub, eval_g_ineq = wsum, 
                    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-8, "maxeval" = 1000),
                    BCSS = BCSS, RENT_RATE = RENT_RATE, PAY_START = PAY_START, PAY = PAY, AL = AL, n = n, m = m, muY = muY)
    WEIGHTS <- matrix(res$solution, ncol = n - 1, nrow = m, byrow = FALSE)
    mWEIGHTS <- matrix(WEIGHTS, ncol = n, nrow = m,byrow = FALSE)
    mWEIGHTS[,n] <- 1 - WEIGHTS %*% x1
    obj <- res$objective
    print(i)
    result[[i]] <- list(mWEIGHTS,BCSS,obj)
  }
  return(result)
}

VS.mean <- function(VSmin, lp, n, m) {
  mr <- matrix(0, nrow = m, ncol = n)
  for (nk in 1:n) {
    for (i in 1:m) {
      for (j in 1:lp) {
        mr[i, nk] <- mr[i, nk] + VSmin[[j]][[1]][i, nk]
      }
    }
  }
  mr <- mr/lp
  result <- data.frame(mr)
  return(result)
}


P.mean <- function(VSmin, lp) {
  mr <- 0
  for (j in 1:lp) {
    mr <- mr + VSmin[[j]][[3]]
  }
  mr <- mr/lp
  return(mr)
}

res3_20_1000_40 <- VS.min.al(BiCopStr_n, nu, muY1, sigmaY, lp, RENT_RATE, PAY_START_40, PAY_40, AL_20_20, n, m_40)
mres3_20_1000_40 <- VS.mean(res3_20_1000_40, lp, n, m_40)
pres3_20_1000_40 <- P.mean(res3_20_1000_40, lp)

res3_30_1000_40 <- VS.min.al(BiCopStr_n, nu, muY1, sigmaY, lp, RENT_RATE, PAY_START_40, PAY_40, AL_30_20, n, m_40)
mres3_30_1000_40 <- VS.mean(res3_30_1000_40, lp, n, m_40)
pres3_30_1000_40 <- P.mean(res3_30_1000_40, lp)

res3_40_1000_40 <- VS.min.al(BiCopStr_n, nu, muY1, sigmaY, lp, RENT_RATE, PAY_START_40, PAY_40, AL_40_20, n, m_40)
mres3_40_1000_40 <- VS.mean(res3_40_1000_40, lp, n, m_40)
pres3_40_1000_40 <- P.mean(res3_40_1000_40, lp)

res3_50_1000_40 <- VS.min.al(BiCopStr_n, nu, muY1, sigmaY, lp, RENT_RATE, PAY_START_40, PAY_40, AL_50_20, n, m_40)
mres3_50_1000_40 <- VS.mean(res3_50_1000_40, lp, n, m_40)
pres3_50_1000_40 <- P.mean(res3_50_1000_40, lp)

res3_60_1000_40 <- VS.min.al(BiCopStr_n, nu, muY1, sigmaY, lp, RENT_RATE, PAY_START_40, PAY_40, AL_60_20, n, m_40)
mres3_60_1000_40 <- VS.mean(res3_60_1000_40, lp, n, m_40)
pres3_60_1000_40 <- P.mean(res3_60_1000_40, lp)
