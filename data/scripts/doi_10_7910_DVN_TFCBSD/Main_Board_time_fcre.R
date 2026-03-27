rm(list=ls())

# Set working directory
# setwd()

source("sims_function_time_fcre.R")

first_time <- Sys.time()
#########################################################################################
N <- 1000
T <- 2
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 1000
T <- 3
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 1000
T <- 5
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 500
T <- 2
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 500
T <- 3
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 500
T <- 5
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################



first_time <- Sys.time()
#########################################################################################
N <- 200
T <- 2
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 200
T <- 3
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 200
T <- 5
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 2000
T <- 2
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 2000
T <- 3
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 2000
T <- 5
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################



first_time <- Sys.time()
#########################################################################################
N <- 200
T <- 10
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 200
T <- 20
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 150
T <- 5
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################



first_time <- Sys.time()
#########################################################################################
N <- 150
T <- 10
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################



first_time <- Sys.time()
#########################################################################################
N <- 150
T <- 20
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 100
T <- 5
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################



first_time <- Sys.time()
#########################################################################################
N <- 100
T <- 10
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################



first_time <- Sys.time()
#########################################################################################
N <- 100
T <- 20
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################


first_time <- Sys.time()
#########################################################################################
N <- 50
T <- 5
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################



first_time <- Sys.time()
#########################################################################################
N <- 50
T <- 10
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################



first_time <- Sys.time()
#########################################################################################
N <- 50
T <- 20
lambda.cand <-exp(seq(-2,6,by=0.25))

library(parallel)
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(MASS),library(pglm),library(survival),library(PFCRE),library(alpaca),library(lme4),library(optimx),library(plm),library(margins),library(lfe)))
clusterExport(cl,c("sims","N","T","lambda.cand"))
R=100
start_time <- Sys.time()
simulations=parLapply(cl,1:R,function(i) tryCatch(sims(),error=function(e) NULL))
#simulations=parLapply(cl,1:R,function(i) sims())
Sys.time() - start_time
stopCluster(cl)

assign(paste("sim.res","N",N,"T",T,sep=""),matrix(unlist(simulations),ncol=10,byrow=TRUE))


#########################################################################################

save.image("Time_and_fcre.RData")