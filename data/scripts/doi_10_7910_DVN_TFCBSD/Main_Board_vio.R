rm(list=ls())

# Set working directory
# setwd()

source("sims_function_vio.R")


#########################################################################################
N <- 50
T <- 5
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))


#########################################################################################
N <- 50
T <- 10
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))



#########################################################################################
N <- 50
T <- 20
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))


#########################################################################################
N <- 100
T <- 5
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

#########################################################################################
N <- 100
T <- 10
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

#########################################################################################
N <- 100
T <- 20
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))


#########################################################################################
N <- 150
T <- 5
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

#########################################################################################
N <- 150
T <- 10
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))


#########################################################################################
N <- 150
T <- 20
lambda.cand <-exp(seq(0,7,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))


#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

#########################################################################################
N <- 200
T <- 2
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")

#########################################################################################
N <- 200
T <- 3
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")


#########################################################################################
N <- 200
T <- 5
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")


#########################################################################################
N <- 200
T <- 10
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")

#########################################################################################
N <- 200
T <- 20
lambda.cand <-exp(seq(0,7,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")


#########################################################################################
N <- 500
T <- 2
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")



#########################################################################################
N <- 500
T <- 3
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")


#########################################################################################
N <- 500
T <- 5
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")




#########################################################################################
N <- 1000
T <- 2
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")


#########################################################################################
N <- 1000
T <- 3
lambda.cand <-exp(seq(0,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")



#########################################################################################
N <- 1000
T <- 5
lambda.cand <-exp(seq(0,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")



#########################################################################################
N <- 2000
T <- 2
lambda.cand <-exp(seq(-1,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")



#########################################################################################
N <- 2000
T <- 3
lambda.cand <-exp(seq(-0.5,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")


#########################################################################################
N <- 2000
T <- 5
lambda.cand <-exp(seq(0,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe),library(plyr)))
clusterExport(cl,c("sims_vio","N","T","lambda.cand"))
R=250
simulations=parLapply(cl,1:R,function(i) tryCatch(sims_vio(),error=function(e) NULL))
stopCluster(cl)

assign(paste("sr","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))
save.image("Simulations_Exchange_Vio.RData")
