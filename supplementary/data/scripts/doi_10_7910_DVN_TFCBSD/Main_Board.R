rm(list=ls())

# Set working directory
#setwd()

# Load the function for simulated data and estimation
source("sims_function.R")

first_time <- Sys.time()
#########################################################################################
N <- 50
T <- 5
lambda.cand <-exp(seq(-1,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 100
T <- 5
lambda.cand <-exp(seq(0,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 200
T <- 5
lambda.cand <-exp(seq(0,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################



#########################################################################################
N <- 50
T <- 10
lambda.cand <-exp(seq(0,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################


#########################################################################################
N <- 100
T <- 10
lambda.cand <-exp(seq(1,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################


#########################################################################################
N <- 200
T <- 10
lambda.cand <-exp(seq(1,4.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################


#########################################################################################
N <- 50
T <- 20
lambda.cand <-exp(seq(0,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################



#########################################################################################
N <- 100
T <- 20
lambda.cand <-exp(seq(1,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################


#########################################################################################
N <- 200
T <- 20
lambda.cand <-exp(seq(1,4.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################



#########################################################################################
N <- 150
T <- 5
lambda.cand <-exp(seq(0,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################


#########################################################################################
N <- 150
T <- 10
lambda.cand <-exp(seq(0,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################


#########################################################################################
N <- 150
T <- 20
lambda.cand <-exp(seq(1,4.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################


#last_time <- Sys.time()


save.image("TSCS_Simulations.RData")

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################

rm(list=ls())
# Small T Simulations
source("sims_function.R")


#########################################################################################
N <- 200
T <- 2
lambda.cand <-exp(seq(0,5.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 500
T <- 2
lambda.cand <-exp(seq(0,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 1000
T <- 2
lambda.cand <-exp(seq(0.5,4.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 2000
T <- 2
lambda.cand <-exp(seq(1,4.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 200
T <- 3
lambda.cand <-exp(seq(0,5.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 500
T <- 3
lambda.cand <-exp(seq(1,4.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 1000
T <- 3
lambda.cand <-exp(seq(1,4.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 2000
T <- 3
lambda.cand <-exp(seq(1.5,4.5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
#N <- 200
#T <- 5
#lambda.cand <-exp(seq(0,5,by=0.25))

#library(parallel)
#cl <- makeCluster(detectCores()-1)
#clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
#clusterExport(cl,c("sims","N","T","lambda.cand"))
#R=250
#start_time <- Sys.time()
#simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
##simulations=parLapply(cl,1:R,function(i) sims())
#Sys.time() - start_time
#stopCluster(cl)

#assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

#write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 500
T <- 5
lambda.cand <-exp(seq(0,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 1000
T <- 5
lambda.cand <-exp(seq(2,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################

#########################################################################################
N <- 2000
T <- 5
lambda.cand <-exp(seq(2,5,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=250
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=23,byrow=TRUE))

write.csv(get(paste("sim.res","N",N,"T",T,sep="")),paste("SimsN",N,"T",T,".csv"))
#########################################################################################


save.image("SmallT_Simulations.RData")



