library("nloptr")
library("VineCopula")
library("QRM")
library("tictoc")
library("parallel")
library("R.utils")
library("readxl")
library("xlsx")
library("rJava")

A <- 35
RENT_RATE <- 0.1952
n <- 2
lp <- 10000

make_kapital_mtrx <- function(kapital) {
  kapital_matrix <- matrix(0, nrow = lp, ncol = 101)
  for (j in 1:101) {
    kapital_matrix[,j] <- kapital[[j]][[2]]
  }
  return(kapital_matrix) 
}

AL.CALC <- function(PAY, m, m_Y, AL){
  AL_L <- PAY[m] * m_Y * AL
  return(AL_L)
}

kapital_srednia_odch <- function(kapital){
  res <- list()
  resm <- matrix(0, nrow = 11, ncol = 101)
  ress <- matrix(0, nrow = 11, ncol = 101)
  for (j in 1:101) {
    for (i in 1:11) {
      D <- i + 19
      AL_100 <- AL.CALC(PAY_35, m = A, m_Y = D, 1)
      resm[i,j] <- mean(kapital[[j]][[2]]/AL_100)
      ress[i,j] <- sd(kapital[[j]][[2]]/AL_100)
    }
  }
  res[[1]] <- resm
  res[[2]] <- ress
  return(res) 
}

kapital_AL <- function(kapital) {
  res <- list()
  for (k in 1:11) {
    P_AL <- k*0.01 + 0.39
    respl <- matrix(0, nrow = 11, ncol = 101)
    for (j in 1:101) {
        for (i in 1:11) { 
          D <- i + 19
          AL <- AL.CALC(PAY_35, m = A, m_Y = D, P_AL)
          respl[i, j] <- pnorm(AL, mean(kapital[[j]][[2]]), sd(kapital[[j]][[2]]))
        }
    }
    res[[k]] <- respl
  }
  return(res) 
}

AL_levels <- function() {
  res <- matrix(0, nrow = 11, ncol = 12)
  for (i in 1:11){
    D <- i + 19
    for (j in 1:11) {
        P_AL <- j*0.01 + 0.39
        res[i, j] <- AL.CALC(PAY_35, m = A, m_Y = D, P_AL)
      }
    res[i,12] <- AL.CALC(PAY_35, m = A, m_Y = D, 1)
  }
  return(res) 
}

kapital_safety_level <- function(kapital, alpha){
  resm <- matrix(0, nrow = 11, ncol = 101)
  for (j in 1:101) {
    for (i in 1:11) {
      D <- i + 19
      AL_100 <- AL.CALC(PAY_35, m = A, m_Y = D, 1)
      tab <- kapital[[j]][[2]]/AL_100
      tab <- sort(tab)
      resm[i, j] <- tab[length(tab)*alpha]
    }
  }
  return(resm) 
}

TOT.INC <- function(RET, RENT_RATE, PAY_START, PAY, m){
  x1 <- (RENT_RATE * PAY[1,]) + (RENT_RATE * PAY_START)*(1 + RET[1,])
  for (i in 2:m) {
    x1 <- (RENT_RATE * PAY[i,]) + x1 * (1 + RET[i,])
  }
  return(x1)		
}

kapital_symulacja <- function(stopy, A, lp, n){
  res <- list()
  for (num in 1:101){
    resn <- list() 
    print(paste("num = ", 0.01*(num - 1)))
    WEIGHTS <- matrix(0.01*(num - 1), ncol = n - 1, nrow = A, byrow = FALSE)
    x1 <- matrix(1,ncol = 1,nrow = n - 1)
    WEIGHTSraw <- matrix(WEIGHTS, ncol = n - 1, nrow = A, byrow = FALSE)
    mWEIGHTS <- matrix(WEIGHTS, ncol = n, nrow = A, byrow = FALSE)
    mWEIGHTS[,n] <- 1 - WEIGHTSraw %*% x1
    inctab <- matrix(0, ncol = 1, nrow = lp)
    for (j in (1:lp)) {
      RET <- matrix(rowSums(stopy[[num]][[j]] * mWEIGHTS), ncol = 1, nrow = A)
      TOT_INC <- TOT.INC(RET, RENT_RATE, PAY_START_35, PAY_35, A)
      inctab[j] <- TOT_INC
    }
    resn[[1]] <- 0.01*num
    resn[[2]] <- inctab
    res[[num]] <- resn
  }
  return(res)
}

suwak_symulacja <- function(stopy, A, lp, n) {
  res <- list()
  for (num in 1:101) {
    resn <- list() 
    print(paste("num = ", 0.01*(num - 1)))
    WEIGHTS <- matrix(0.01*(num - 1), ncol = n - 1, nrow = A, byrow = FALSE)
    x1 <- matrix(1,ncol = 1,nrow = n - 1)
    WEIGHTSraw <- matrix(WEIGHTS, ncol = n - 1, nrow = A, byrow = FALSE)
    mWEIGHTS <- matrix(WEIGHTS, ncol = n, nrow = A, byrow = FALSE)
    mWEIGHTS[,n] <- 1 - WEIGHTSraw %*% x1
    for (i in 1:10) {
      num1 <- mWEIGHTS[A - 10 + (i-1),n]
      dzielnik <- 10 - i + 1
      num1 <- num1 - num1/dzielnik
      mWEIGHTS[A - 10 + i,n] <- num1
      mWEIGHTS[A - 10 + i,1] <- 1-num1
    }
    inctab <- matrix(0, ncol = 1, nrow = lp)
    for (j in (1:lp)) {
      RET <- matrix(rowSums(stopy[[num]][[j]] * mWEIGHTS), ncol = 1, nrow = A)
      TOT_INC <- TOT.INC(RET, RENT_RATE, PAY_START_35, PAY_35, A)
      inctab[j] <- TOT_INC
    }
    resn[[1]] <- 0.01*num
    resn[[2]] <- inctab
    res[[num]] <- resn
  }
  return(res)
}

suwak_0 <- suwak_0_0_25
suwak_0 <- append(suwak_0, suwak_0_26_50[27:51])
suwak_0 <- append(suwak_0, suwak_0_51_75[52:76])
suwak_0 <- append(suwak_0, suwak_0_76_100[77:101])

suwak_3 <- suwak_3_0_25
suwak_3 <- append(suwak_3, suwak_3_26_50[27:51])
suwak_3 <- append(suwak_3, suwak_3_51_75[52:76])
suwak_3 <- append(suwak_3, suwak_3_76_100[77:101])


stopy_0 <- stopy_0_0_25
stopy_0 <- append(stopy_0, stopa_0_26_50[27:51])
stopy_0 <- append(stopy_0, stopa_0_51_75[52:76])
stopy_0 <- append(stopy_0, stopa_0_76_100[77:101])

stopy_3 <- stopy_3_0_25
stopy_3 <- append(stopy_3, stopa_3_26_50[27:51])
stopy_3 <- append(stopy_3, stopa_3_51_75[52:76])
stopy_3 <- append(stopy_3, stopa_3_76_100[77:101])

stopat_0 <- stopat_0_0_25
stopat_0 <- append(stopat_0, stopat_0_26_50[27:51])
stopat_0 <- append(stopat_0, stopat_0_51_75[52:76])
stopat_0 <- append(stopat_0, stopat_0_76_100[77:101])

stopat_3 <- stopat_3_0_25
stopat_3 <- append(stopat_3, stopat_3_26_50[27:51])
stopat_3 <- append(stopat_3, stopat_3_51_75[52:76])
stopat_3 <- append(stopat_3, stopat_3_76_100[77:101])

suwak_3 <- suwak_symulacja(stopy_3, A, lp, n)
suwak_0 <- suwak_symulacja(stopy_0, A, lp, n)

bez_suwaka_3 <- kapital_symulacja(stopy_3, A, lp, n)
bez_suwaka_0 <- kapital_symulacja(stopy_0, A, lp, n)

suw_sr3 <- kapital_srednia_odch(suwak_3)
suw_sr0 <- kapital_srednia_odch(suwak_0)
suw_AL3 <- kapital_AL(suwak_3)
suw_AL0 <- kapital_AL(suwak_0)
suw_SL_3_01 <- kapital_safety_level(suwak_3, 0.01)
suw_SL_0_01 <- kapital_safety_level(suwak_0, 0.01)
suw_SL_3_05 <- kapital_safety_level(suwak_3, 0.05)
suw_SL_0_05 <- kapital_safety_level(suwak_0, 0.05)
suw_SL_3_10 <- kapital_safety_level(suwak_3, 0.1)
suw_SL_0_10 <- kapital_safety_level(suwak_0, 0.1)

bsuw_sr3 <- kapital_srednia_odch(bez_suwaka_3)
bsuw_sr0 <- kapital_srednia_odch(bez_suwaka_0)
bsuw_AL3 <- kapital_AL(bez_suwaka_3)
bsuw_AL0 <- kapital_AL(bez_suwaka_0)
bsuw_SL_3_01 <- kapital_safety_level(bez_suwaka_3, 0.01)
bsuw_SL_0_01 <- kapital_safety_level(bez_suwaka_0, 0.01)
bsuw_SL_3_05 <- kapital_safety_level(bez_suwaka_3, 0.05)
bsuw_SL_0_05 <- kapital_safety_level(bez_suwaka_0, 0.05)
bsuw_SL_3_10 <- kapital_safety_level(bez_suwaka_3, 0.1)
bsuw_SL_0_10 <- kapital_safety_level(bez_suwaka_0, 0.1)

write.xlsx(suw_SL_3_01, "suw.xls", sheetName = "suw_SL_3_01", append = FALSE)
write.xlsx(suw_SL_3_05, "suw.xls", sheetName = "suw_SL_3_05", append = TRUE)
write.xlsx(suw_SL_3_10, "suw.xls", sheetName = "suw_SL_3_10", append = TRUE)

write.xlsx(suw_SL_0_01, "suw.xls", sheetName = "suw_SL_0_01", append = TRUE)
write.xlsx(suw_SL_0_05, "suw.xls", sheetName = "suw_SL_0_05", append = TRUE)
write.xlsx(suw_SL_0_10, "suw.xls", sheetName = "suw_SL_0_10", append = TRUE)

write.xlsx(suw_sr3[[1]], "suw.xls", sheetName = "suwak_sr3_sr", append = TRUE)
write.xlsx(suw_sr3[[2]], "suw.xls", sheetName = "suwak_sr3_od", append = TRUE)
write.xlsx(suw_sr0[[1]], "suw.xls", sheetName = "suwak_sr0_sr", append = TRUE)
write.xlsx(suw_sr0[[2]], "suw.xls", sheetName = "suwak_sr0_od", append = TRUE)


write.xlsx(suw_AL3[[1]], "suw.xls", sheetName = "suw_AL3_040", append = TRUE)
write.xlsx(suw_AL3[[2]], "suw.xls", sheetName = "suw_AL3_041", append = TRUE)
write.xlsx(suw_AL3[[3]], "suw.xls", sheetName = "suw_AL3_042", append = TRUE)
write.xlsx(suw_AL3[[4]], "suw.xls", sheetName = "suw_AL3_043", append = TRUE)
write.xlsx(suw_AL3[[5]], "suw.xls", sheetName = "suw_AL3_044", append = TRUE)
write.xlsx(suw_AL3[[6]], "suw.xls", sheetName = "suw_AL3_045", append = TRUE)
write.xlsx(suw_AL3[[7]], "suw.xls", sheetName = "suw_AL3_046", append = TRUE)
write.xlsx(suw_AL3[[8]], "suw.xls", sheetName = "suw_AL3_047", append = TRUE)
write.xlsx(suw_AL3[[9]], "suw.xls", sheetName = "suw_AL3_048", append = TRUE)
write.xlsx(suw_AL3[[10]], "suw.xls", sheetName = "suw_AL3_049", append = TRUE)
write.xlsx(suw_AL3[[11]], "suw.xls", sheetName = "suw_AL3_050", append = TRUE)

write.xlsx(suw_AL0[[1]], "suw.xls", sheetName = "suw_AL0_040", append = TRUE)
write.xlsx(suw_AL0[[2]], "suw.xls", sheetName = "suw_AL0_041", append = TRUE)
write.xlsx(suw_AL0[[3]], "suw.xls", sheetName = "suw_AL0_042", append = TRUE)
write.xlsx(suw_AL0[[4]], "suw.xls", sheetName = "suw_AL0_043", append = TRUE)
write.xlsx(suw_AL0[[5]], "suw.xls", sheetName = "suw_AL0_044", append = TRUE)
write.xlsx(suw_AL0[[6]], "suw.xls", sheetName = "suw_AL0_045", append = TRUE)
write.xlsx(suw_AL0[[7]], "suw.xls", sheetName = "suw_AL0_046", append = TRUE)
write.xlsx(suw_AL0[[8]], "suw.xls", sheetName = "suw_AL0_047", append = TRUE)
write.xlsx(suw_AL0[[9]], "suw.xls", sheetName = "suw_AL0_048", append = TRUE)
write.xlsx(suw_AL0[[10]], "suw.xls", sheetName = "suw_AL0_049", append = TRUE)
write.xlsx(suw_AL0[[11]], "suw.xls", sheetName = "suw_AL0_050", append = TRUE)


write.xlsx(bsuw_SL_3_01, "bsuw.xls", sheetName = "bsuw_SL_3_01", append = FALSE)
write.xlsx(bsuw_SL_3_05, "bsuw.xls", sheetName = "bsuw_SL_3_05", append = TRUE)
write.xlsx(bsuw_SL_3_10, "bsuw.xls", sheetName = "bsuw_SL_3_10", append = TRUE)

write.xlsx(bsuw_SL_0_01, "bsuw.xls", sheetName = "bsuw_SL_0_01", append = TRUE)
write.xlsx(bsuw_SL_0_05, "bsuw.xls", sheetName = "bsuw_SL_0_05", append = TRUE)
write.xlsx(bsuw_SL_0_10, "bsuw.xls", sheetName = "bsuw_SL_0_10", append = TRUE)

write.xlsx(bsuw_sr3[[1]], "bsuw.xls", sheetName = "bsuw_sr3_sr", append = TRUE)
write.xlsx(bsuw_sr3[[2]], "bsuw.xls", sheetName = "bsuw_sr3_od", append = TRUE)
write.xlsx(bsuw_sr0[[1]], "bsuw.xls", sheetName = "bsuw_sr0_sr", append = TRUE)
write.xlsx(bsuw_sr0[[2]], "bsuw.xls", sheetName = "bsuw_sr0_od", append = TRUE)

write.xlsx(bsuw_AL3[[1]], "bsuw.xls", sheetName = "bsuw_AL3_040", append = TRUE)
write.xlsx(bsuw_AL3[[2]], "bsuw.xls", sheetName = "bsuw_AL3_041", append = TRUE)
write.xlsx(bsuw_AL3[[3]], "bsuw.xls", sheetName = "bsuw_AL3_042", append = TRUE)
write.xlsx(bsuw_AL3[[4]], "bsuw.xls", sheetName = "bsuw_AL3_043", append = TRUE)
write.xlsx(bsuw_AL3[[5]], "bsuw.xls", sheetName = "bsuw_AL3_044", append = TRUE)
write.xlsx(bsuw_AL3[[6]], "bsuw.xls", sheetName = "bsuw_AL3_045", append = TRUE)
write.xlsx(bsuw_AL3[[7]], "bsuw.xls", sheetName = "bsuw_AL3_046", append = TRUE)
write.xlsx(bsuw_AL3[[8]], "bsuw.xls", sheetName = "bsuw_AL3_047", append = TRUE)
write.xlsx(bsuw_AL3[[9]], "bsuw.xls", sheetName = "bsuw_AL3_048", append = TRUE)
write.xlsx(bsuw_AL3[[10]], "bsuw.xls", sheetName = "bsuw_AL3_049", append = TRUE)
write.xlsx(bsuw_AL3[[11]], "bsuw.xls", sheetName = "bsuw_AL3_050", append = TRUE)

write.xlsx(bsuw_AL0[[1]], "bsuw.xls", sheetName = "bsuw_AL0_040", append = TRUE)
write.xlsx(bsuw_AL0[[2]], "bsuw.xls", sheetName = "bsuw_AL0_041", append = TRUE)
write.xlsx(bsuw_AL0[[3]], "bsuw.xls", sheetName = "bsuw_AL0_042", append = TRUE)
write.xlsx(bsuw_AL0[[4]], "bsuw.xls", sheetName = "bsuw_AL0_043", append = TRUE)
write.xlsx(bsuw_AL0[[5]], "bsuw.xls", sheetName = "bsuw_AL0_044", append = TRUE)
write.xlsx(bsuw_AL0[[6]], "bsuw.xls", sheetName = "bsuw_AL0_045", append = TRUE)
write.xlsx(bsuw_AL0[[7]], "bsuw.xls", sheetName = "bsuw_AL0_046", append = TRUE)
write.xlsx(bsuw_AL0[[8]], "bsuw.xls", sheetName = "bsuw_AL0_047", append = TRUE)
write.xlsx(bsuw_AL0[[9]], "bsuw.xls", sheetName = "bsuw_AL0_048", append = TRUE)
write.xlsx(bsuw_AL0[[10]], "bsuw.xls", sheetName = "bsuw_AL0_049", append = TRUE)
write.xlsx(bsuw_AL0[[11]], "bsuw.xls", sheetName = "bsuw_AL0_050", append = TRUE)


suwakt_3 <- suwak_symulacja(stopat_3, A, lp, n)
suwakt_0 <- suwak_symulacja(stopat_0, A, lp, n)

bez_suwakat_3 <- kapital_symulacja(stopat_3, A, lp, n)
bez_suwakat_0 <- kapital_symulacja(stopat_0, A, lp, n)

suw_sr3t <- kapital_srednia_odch(suwakt_3)
suw_sr0t <- kapital_srednia_odch(suwakt_0)
suw_AL3t <- kapital_AL(suwakt_3)
suw_AL0t <- kapital_AL(suwakt_0)
suw_SL_3_01t <- kapital_safety_level(suwakt_3, 0.01)
suw_SL_0_01t <- kapital_safety_level(suwakt_0, 0.01)
suw_SL_3_05t <- kapital_safety_level(suwakt_3, 0.05)
suw_SL_0_05t <- kapital_safety_level(suwakt_0, 0.05)
suw_SL_3_10t <- kapital_safety_level(suwakt_3, 0.1)
suw_SL_0_10t <- kapital_safety_level(suwakt_0, 0.1)

bsuw_sr3t <- kapital_srednia_odch(bez_suwakat_3)
bsuw_sr0t <- kapital_srednia_odch(bez_suwakat_0)
bsuw_AL3t <- kapital_AL(bez_suwakat_3)
bsuw_AL0t <- kapital_AL(bez_suwakat_0)
bsuw_SL_3_01t <- kapital_safety_level(bez_suwakat_3, 0.01)
bsuw_SL_0_01t <- kapital_safety_level(bez_suwakat_0, 0.01)
bsuw_SL_3_05t <- kapital_safety_level(bez_suwakat_3, 0.05)
bsuw_SL_0_05t <- kapital_safety_level(bez_suwakat_0, 0.05)
bsuw_SL_3_10t <- kapital_safety_level(bez_suwakat_3, 0.1)
bsuw_SL_0_10t <- kapital_safety_level(bez_suwakat_0, 0.1)

write.xlsx(suw_SL_3_01t, "suwt.xls", sheetName = "suw_SL_3_01", append = FALSE)
write.xlsx(suw_SL_3_05t, "suwt.xls", sheetName = "suw_SL_3_05", append = TRUE)
write.xlsx(suw_SL_3_10t, "suwt.xls", sheetName = "suw_SL_3_10", append = TRUE)

write.xlsx(suw_SL_0_01t, "suwt.xls", sheetName = "suw_SL_0_01", append = TRUE)
write.xlsx(suw_SL_0_05t, "suwt.xls", sheetName = "suw_SL_0_05", append = TRUE)
write.xlsx(suw_SL_0_10t, "suwt.xls", sheetName = "suw_SL_0_10", append = TRUE)

write.xlsx(suw_sr3t[[1]], "suwt.xls", sheetName = "suwak_sr3_sr", append = TRUE)
write.xlsx(suw_sr3t[[2]], "suwt.xls", sheetName = "suwak_sr3_od", append = TRUE)
write.xlsx(suw_sr0t[[1]], "suwt.xls", sheetName = "suwak_sr0_sr", append = TRUE)
write.xlsx(suw_sr0t[[2]], "suwt.xls", sheetName = "suwak_sr0_od", append = TRUE)


write.xlsx(suw_AL3t[[1]], "suwt.xls", sheetName = "suw_AL3_040", append = TRUE)
write.xlsx(suw_AL3t[[2]], "suwt.xls", sheetName = "suw_AL3_041", append = TRUE)
write.xlsx(suw_AL3t[[3]], "suwt.xls", sheetName = "suw_AL3_042", append = TRUE)
write.xlsx(suw_AL3t[[4]], "suwt.xls", sheetName = "suw_AL3_043", append = TRUE)
write.xlsx(suw_AL3t[[5]], "suwt.xls", sheetName = "suw_AL3_044", append = TRUE)
write.xlsx(suw_AL3t[[6]], "suwt.xls", sheetName = "suw_AL3_045", append = TRUE)
write.xlsx(suw_AL3t[[7]], "suwt.xls", sheetName = "suw_AL3_046", append = TRUE)
write.xlsx(suw_AL3t[[8]], "suwt.xls", sheetName = "suw_AL3_047", append = TRUE)
write.xlsx(suw_AL3t[[9]], "suwt.xls", sheetName = "suw_AL3_048", append = TRUE)
write.xlsx(suw_AL3t[[10]], "suwt.xls", sheetName = "suw_AL3_049", append = TRUE)
write.xlsx(suw_AL3t[[11]], "suwt.xls", sheetName = "suw_AL3_050", append = TRUE)

write.xlsx(suw_AL0t[[1]], "suwt.xls", sheetName = "suw_AL0_040", append = TRUE)
write.xlsx(suw_AL0t[[2]], "suwt.xls", sheetName = "suw_AL0_041", append = TRUE)
write.xlsx(suw_AL0t[[3]], "suwt.xls", sheetName = "suw_AL0_042", append = TRUE)
write.xlsx(suw_AL0t[[4]], "suwt.xls", sheetName = "suw_AL0_043", append = TRUE)
write.xlsx(suw_AL0t[[5]], "suwt.xls", sheetName = "suw_AL0_044", append = TRUE)
write.xlsx(suw_AL0t[[6]], "suwt.xls", sheetName = "suw_AL0_045", append = TRUE)
write.xlsx(suw_AL0t[[7]], "suwt.xls", sheetName = "suw_AL0_046", append = TRUE)
write.xlsx(suw_AL0t[[8]], "suwt.xls", sheetName = "suw_AL0_047", append = TRUE)
write.xlsx(suw_AL0t[[9]], "suwt.xls", sheetName = "suw_AL0_048", append = TRUE)
write.xlsx(suw_AL0t[[10]], "suwt.xls", sheetName = "suw_AL0_049", append = TRUE)
write.xlsx(suw_AL0t[[11]], "suwt.xls", sheetName = "suw_AL0_050", append = TRUE)


write.xlsx(bsuw_SL_3_01t, "bsuwt.xls", sheetName = "bsuw_SL_3_01", append = FALSE)
write.xlsx(bsuw_SL_3_05t, "bsuwt.xls", sheetName = "bsuw_SL_3_05", append = TRUE)
write.xlsx(bsuw_SL_3_10t, "bsuwt.xls", sheetName = "bsuw_SL_3_10", append = TRUE)

write.xlsx(bsuw_SL_0_01t, "bsuwt.xls", sheetName = "bsuw_SL_0_01", append = TRUE)
write.xlsx(bsuw_SL_0_05t, "bsuwt.xls", sheetName = "bsuw_SL_0_05", append = TRUE)
write.xlsx(bsuw_SL_0_10t, "bsuwt.xls", sheetName = "bsuw_SL_0_10", append = TRUE)

write.xlsx(bsuw_sr3t[[1]], "bsuwt.xls", sheetName = "bsuw_sr3_sr", append = TRUE)
write.xlsx(bsuw_sr3t[[2]], "bsuwt.xls", sheetName = "bsuw_sr3_od", append = TRUE)
write.xlsx(bsuw_sr0t[[1]], "bsuwt.xls", sheetName = "bsuw_sr0_sr", append = TRUE)
write.xlsx(bsuw_sr0t[[2]], "bsuwt.xls", sheetName = "bsuw_sr0_od", append = TRUE)

write.xlsx(bsuw_AL3t[[1]], "bsuwt.xls", sheetName = "bsuw_AL3_040", append = TRUE)
write.xlsx(bsuw_AL3t[[2]], "bsuwt.xls", sheetName = "bsuw_AL3_041", append = TRUE)
write.xlsx(bsuw_AL3t[[3]], "bsuwt.xls", sheetName = "bsuw_AL3_042", append = TRUE)
write.xlsx(bsuw_AL3t[[4]], "bsuwt.xls", sheetName = "bsuw_AL3_043", append = TRUE)
write.xlsx(bsuw_AL3t[[5]], "bsuwt.xls", sheetName = "bsuw_AL3_044", append = TRUE)
write.xlsx(bsuw_AL3t[[6]], "bsuwt.xls", sheetName = "bsuw_AL3_045", append = TRUE)
write.xlsx(bsuw_AL3t[[7]], "bsuwt.xls", sheetName = "bsuw_AL3_046", append = TRUE)
write.xlsx(bsuw_AL3t[[8]], "bsuwt.xls", sheetName = "bsuw_AL3_047", append = TRUE)
write.xlsx(bsuw_AL3t[[9]], "bsuwt.xls", sheetName = "bsuw_AL3_048", append = TRUE)
write.xlsx(bsuw_AL3t[[10]], "bsuwt.xls", sheetName = "bsuw_AL3_049", append = TRUE)
write.xlsx(bsuw_AL3t[[11]], "bsuwt.xls", sheetName = "bsuw_AL3_050", append = TRUE)

write.xlsx(bsuw_AL0t[[1]], "bsuwt.xls", sheetName = "bsuw_AL0_040", append = TRUE)
write.xlsx(bsuw_AL0t[[2]], "bsuwt.xls", sheetName = "bsuw_AL0_041", append = TRUE)
write.xlsx(bsuw_AL0t[[3]], "bsuwt.xls", sheetName = "bsuw_AL0_042", append = TRUE)
write.xlsx(bsuw_AL0t[[4]], "bsuwt.xls", sheetName = "bsuw_AL0_043", append = TRUE)
write.xlsx(bsuw_AL0t[[5]], "bsuwt.xls", sheetName = "bsuw_AL0_044", append = TRUE)
write.xlsx(bsuw_AL0t[[6]], "bsuwt.xls", sheetName = "bsuw_AL0_045", append = TRUE)
write.xlsx(bsuw_AL0t[[7]], "bsuwt.xls", sheetName = "bsuw_AL0_046", append = TRUE)
write.xlsx(bsuw_AL0t[[8]], "bsuwt.xls", sheetName = "bsuw_AL0_047", append = TRUE)
write.xlsx(bsuw_AL0t[[9]], "bsuwt.xls", sheetName = "bsuw_AL0_048", append = TRUE)
write.xlsx(bsuw_AL0t[[10]], "bsuwt.xls", sheetName = "bsuw_AL0_049", append = TRUE)
write.xlsx(bsuw_AL0t[[11]], "bsuwt.xls", sheetName = "bsuw_AL0_050", append = TRUE)

bez_suwakat_0m <- make_kapital_mtrx(bez_suwakat_0)
bez_suwakat_3m <- make_kapital_mtrx(bez_suwakat_3)
suwakt_0m <- make_kapital_mtrx(suwakt_0)
suwakt_3m <- make_kapital_mtrx(suwakt_3)

write.xlsx(bez_suwakat_0m, "bez_suwakat_0m.xls", sheetName = "bez_suwakat_0m", append = FALSE)
write.xlsx(bez_suwakat_3m, "bez_suwakat_3m.xls", sheetName = "bez_suwakat_3m", append = FALSE)
write.xlsx(suwakt_0m, "suwakt_0m.xls", sheetName = "suwakt_0m", append = FALSE)
write.xlsx(suwakt_3m, "suwakt_3me.xls", sheetName = "suwakt_3m", append = FALSE)

AL_lev <- AL_levels()
write.xlsx(AL_lev, "AL_lev.xls", sheetName = "AL_lev", append = FALSE)

i <- 1

D <- i + 19
respl <- matrix(0, nrow = 11, ncol = 1)
for (j in 1:101) {
  for (k in 1:11) { 
    P_AL <- k*0.01 + 0.39
    AL <- AL.CALC(PAY_35, m = A, m_Y = D, P_AL)
    respl[k, j] <- pnorm(AL, mean(suwakt_0[[j]][[2]]), sd(suwakt_0[[j]][[2]]))
  }
}

write.xlsx(respl, "respl.xls", sheetName = "respl", append = FALSE)