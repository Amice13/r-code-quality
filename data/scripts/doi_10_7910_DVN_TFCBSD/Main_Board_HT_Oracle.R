rm(list=ls())

# Set working directory
# setwd()

source("sims_HT_Oracle_Only.R")
library(parallel)

#########################################################################################
N <- 50
T <- 5
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))



#########################################################################################
N <- 50
T <- 10
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))



#########################################################################################
N <- 50
T <- 20
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 100
T <- 5
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 100
T <- 10
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))



#########################################################################################
N <- 100
T <- 20
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))



#########################################################################################
N <- 150
T <- 5
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 150
T <- 10
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 150
T <- 20
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 200
T <- 5
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))



#########################################################################################
N <- 200
T <- 10
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 200
T <- 20
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))



#########################################################################################
N <- 200
T <- 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 200
T <- 3
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 500
T <- 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))

#########################################################################################
N <- 500
T <- 3
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))

#########################################################################################
N <- 500
T <- 5
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))

#########################################################################################
N <- 1000
T <- 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 1000
T <- 3
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))

#########################################################################################
N <- 1000
T <- 5
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))

#########################################################################################
N <- 2000
T <- 2
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))



#########################################################################################
N <- 2000
T <- 3
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


#########################################################################################
N <- 2000
T <- 5
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(survival),library(PFCRE)))
clusterExport(cl,c("simsHTOR","N","T"))
R=1000
simulations=parLapply(cl,1:R,function(i) tryCatch(simsHTOR(),error=function(e) NULL))
stopCluster(cl)

assign(paste("srHTOR","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=2,byrow=TRUE))


save.image("HTOR_Simulations.RData")
