rm(list=ls())

# Set working directory 
# setwd()

source("sims_function_coverage.R")

first_time <- Sys.time()
#########################################################################################
N <- 1000
T <- 2
lambda.cand <-exp(seq(-0.5,5.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores())
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


first_time <- Sys.time()
#########################################################################################
N <- 1000
T <- 5
lambda.cand <-exp(seq(-0.5,5.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores())
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


first_time <- Sys.time()
#########################################################################################
N <- 100
T <- 10
lambda.cand <-exp(seq(-0.5,5.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores())
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))

first_time <- Sys.time()
#########################################################################################
N <- 100
T <- 20
lambda.cand <-exp(seq(-0.5,5.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores())
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))



CI.PF.N1000T2 <- cbind(sim.resN1000T2[,1] - 1.96 * sim.resN1000T2[,6], sim.resN1000T2[,1] + 1.96 * sim.resN1000T2[,6]  )
CI.OR.N1000T2 <- cbind(sim.resN1000T2[,2] - 1.96 * sim.resN1000T2[,7], sim.resN1000T2[,2] + 1.96 * sim.resN1000T2[,7]  )
CI.CR.N1000T2 <- cbind(sim.resN1000T2[,3] - 1.96 * sim.resN1000T2[,8], sim.resN1000T2[,3] + 1.96 * sim.resN1000T2[,8]  )
CI.CL.N1000T2 <- cbind(sim.resN1000T2[,4] - 1.96 * sim.resN1000T2[,9], sim.resN1000T2[,4] + 1.96 * sim.resN1000T2[,9]  )
CI.FE.N1000T2 <- cbind(sim.resN1000T2[,5] - 1.96 * sim.resN1000T2[,10], sim.resN1000T2[,5] + 1.96 * sim.resN1000T2[,10]  )


Cov.N1000.T2 <- cbind(prop.table(table(ifelse(1.5 > CI.PF.N1000T2[,1] & 1.5 < CI.PF.N1000T2[,2],1,0 )))[2],
                      prop.table(table(ifelse(1.5 > CI.OR.N1000T2[,1] & 1.5 < CI.OR.N1000T2[,2],1,0 )))[2],
                      prop.table(table(ifelse(1.5 > CI.CR.N1000T2[,1] & 1.5 < CI.CR.N1000T2[,2],1,0 )))[2],
                      prop.table(table(ifelse(1.5 > CI.CL.N1000T2[,1] & 1.5 < CI.CL.N1000T2[,2],1,0 )))[2],
                      prop.table(table(ifelse(1.5 > CI.FE.N1000T2[,1] & 1.5 < CI.FE.N1000T2[,2],1,0 )))[2])


##
CI.PF.N1000T5 <- cbind(sim.resN1000T5[,1] - 1.96 * sim.resN1000T5[,6], sim.resN1000T5[,1] + 1.96 * sim.resN1000T5[,6]  )
CI.OR.N1000T5 <- cbind(sim.resN1000T5[,2] - 1.96 * sim.resN1000T5[,7], sim.resN1000T5[,2] + 1.96 * sim.resN1000T5[,7]  )
CI.CR.N1000T5 <- cbind(sim.resN1000T5[,3] - 1.96 * sim.resN1000T5[,8], sim.resN1000T5[,3] + 1.96 * sim.resN1000T5[,8]  )
CI.CL.N1000T5 <- cbind(sim.resN1000T5[,4] - 1.96 * sim.resN1000T5[,9], sim.resN1000T5[,4] + 1.96 * sim.resN1000T5[,9]  )
CI.FE.N1000T5 <- cbind(sim.resN1000T5[,5] - 1.96 * sim.resN1000T5[,10], sim.resN1000T5[,5] + 1.96 * sim.resN1000T5[,10]  )


Cov.N1000.T5 <- cbind(prop.table(table(ifelse(1.5 > CI.PF.N1000T5[,1] & 1.5 < CI.PF.N1000T5[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.OR.N1000T5[,1] & 1.5 < CI.OR.N1000T5[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.CR.N1000T5[,1] & 1.5 < CI.CR.N1000T5[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.CL.N1000T5[,1] & 1.5 < CI.CL.N1000T5[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.FE.N1000T5[,1] & 1.5 < CI.FE.N1000T5[,2],1,0 )))[2])

##
CI.PF.N100T10 <- cbind(sim.resN100T10[,1] - 1.96 * sim.resN100T10[,6], sim.resN100T10[,1] + 1.96 * sim.resN100T10[,6]  )
CI.OR.N100T10 <- cbind(sim.resN100T10[,2] - 1.96 * sim.resN100T10[,7], sim.resN100T10[,2] + 1.96 * sim.resN100T10[,7]  )
CI.CR.N100T10 <- cbind(sim.resN100T10[,3] - 1.96 * sim.resN100T10[,8], sim.resN100T10[,3] + 1.96 * sim.resN100T10[,8]  )
CI.CL.N100T10 <- cbind(sim.resN100T10[,4] - 1.96 * sim.resN100T10[,9], sim.resN100T10[,4] + 1.96 * sim.resN100T10[,9]  )
CI.FE.N100T10 <- cbind(sim.resN100T10[,5] - 1.96 * sim.resN100T10[,10], sim.resN100T10[,5] + 1.96 * sim.resN100T10[,10]  )


Cov.N100.T10 <- cbind(prop.table(table(ifelse(1.5 > CI.PF.N100T10[,1] & 1.5 < CI.PF.N100T10[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.OR.N100T10[,1] & 1.5 < CI.OR.N100T10[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.CR.N100T10[,1] & 1.5 < CI.CR.N100T10[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.CL.N100T10[,1] & 1.5 < CI.CL.N100T10[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.FE.N100T10[,1] & 1.5 < CI.FE.N100T10[,2],1,0 )))[2])


##
CI.PF.N100T20 <- cbind(sim.resN100T20[,1] - 1.96 * sim.resN100T20[,6], sim.resN100T20[,1] + 1.96 * sim.resN100T20[,6]  )
CI.OR.N100T20 <- cbind(sim.resN100T20[,2] - 1.96 * sim.resN100T20[,7], sim.resN100T20[,2] + 1.96 * sim.resN100T20[,7]  )
CI.CR.N100T20 <- cbind(sim.resN100T20[,3] - 1.96 * sim.resN100T20[,8], sim.resN100T20[,3] + 1.96 * sim.resN100T20[,8]  )
CI.CL.N100T20 <- cbind(sim.resN100T20[,4] - 1.96 * sim.resN100T20[,9], sim.resN100T20[,4] + 1.96 * sim.resN100T20[,9]  )
CI.FE.N100T20 <- cbind(sim.resN100T20[,5] - 1.96 * sim.resN100T20[,10], sim.resN100T20[,5] + 1.96 * sim.resN100T20[,10]  )


Cov.N100.T20 <- cbind(prop.table(table(ifelse(1.5 > CI.PF.N100T20[,1] & 1.5 < CI.PF.N100T20[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.OR.N100T20[,1] & 1.5 < CI.OR.N100T20[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.CR.N100T20[,1] & 1.5 < CI.CR.N100T20[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.CL.N100T20[,1] & 1.5 < CI.CL.N100T20[,2],1,0 )))[2],
prop.table(table(ifelse(1.5 > CI.FE.N100T20[,1] & 1.5 < CI.FE.N100T20[,2],1,0 )))[2])


Coverage <- rbind(Cov.N100.T10,Cov.N100.T20,Cov.N1000.T2,Cov.N1000.T5)
colnames(Coverage) <- c("PF-CRE","Oracle","CRE","CMLE","FE")
rownames(Coverage) <- c("N = 1000, T = 2", "N = 1000, T = 5", "N = 100, T = 10", "N = 100, T = 20")
Coverage <- round(Coverage*100, digits = 1)
Coverage

library(xtable)
xtable(Coverage)

save.image("Coverage.RData")
