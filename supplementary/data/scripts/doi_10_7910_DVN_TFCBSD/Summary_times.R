# Results Summary

rm(list=ls())
library(xtable)

# Set working directory
# setwd()

load("Time_and_fcre.RData")


# Large N, Small T
Ns <- c(50,100,150,200)
Ts <- c(5,10,20)

beta.time.long <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)



k<- 1
for (i in 1:length(Ns)) {
  for (j in 1:length(Ts)) {
    N <- Ns[i]
    T <- Ts[j]
    
    beta.time.long[k,1:5] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,6:10])
    beta.time.long[k,6] <- N
    beta.time.long[k,7] <- T
    
    k <- k+1
  }
}


# Small N, Large T
Ns <- c(200,500,1000,2000)
Ts <- c(2,3,5)

beta.time.short <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)



k<- 1
for (i in 1:length(Ns)) {
  for (j in 1:length(Ts)) {
    N <- Ns[i]
    T <- Ts[j]
    
    beta.time.short[k,1:5] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,6:10])
    beta.time.short[k,6] <- N
    beta.time.short[k,7] <- T
    
    k <- k+1
  }
}

times <- cbind(beta.time.long[,c(1,3,6,7)],beta.time.short[,c(1,3,6,7)])
colnames(times) <- rep(c("PF-CRE","CRE","N","T"),2)

addrow <- list()
addrow$pos <- list(-1)
addrow$command <- paste0('\\multicolumn{4}{|c|}{Large T} & \\multicolumn{4}{c|}{Small T} \\\\')
print(xtable(times, digits = c(1,1,1,0,0,1,1,0,0), align = "l|rrrr|rrrr|"), include.rownames = FALSE, add.to.row = addrow)
