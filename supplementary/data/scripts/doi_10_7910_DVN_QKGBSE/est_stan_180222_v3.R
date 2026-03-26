setwd("/Statistics/dataset03/stan")
trials <- read.csv("trial2.csv", header=TRUE)

# Number of trials
Ntotal = length(trials$C)

# C: Confucianism; T: Taoism; B: Buddhism; P: Popular; S: Secular
C = trials[,"C"]
T = trials[,"T"]
B = trials[,"B"]
P = trials[,"P"]
S = trials[,"S"]

#V of C: Confucianism; T: Taoism; B: Buddhism; P: Popular; S: Secular
VC = trials[,"VC"]
VT = trials[,"VT"]
VB = trials[,"VB"]
VP = trials[,"VP"]
VS = trials[,"VS"]

#AV of C: Confucianism; T: Taoism; B: Buddhism; P: Popular; S: Secular
AVC = trials[,"AVC"]
AVT = trials[,"AVT"]
AVB = trials[,"AVB"]
AVP = trials[,"AVP"]
AVS = trials[,"AVS"]

library(rstan)

#Setup datalist
dataList = list(
		C = C,
		T = T,
		B = B,
		P = P,
		S = S,
		VC = VC,
		VT = VT,
		VB = VB,
		VP = VP,
		VS = VS,
		AVC = AVC,
		AVT = AVT,
		AVB = AVB,
		AVP = AVP,
		AVS = AVS,
    N = Ntotal
  )

warmup = 500
iter = 2000
chains = 4
cores = 1

#======= model mB1
modname <- "mB1.stan"
pars = c("a", "bVB", "bAVB")

# The Stan logistic model as a string.
model_string <- readLines(modname)

# Compiling and producing posterior samples from the model.
#mB1stan <- stan(model_code = model_string, data = dataList)
mB1stan <- stan(file = modname, data = dataList, model_name=modname , pars=pars ,
            warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
mB1stan.post = subset(as.data.frame(mB1stan),select=pars)

#======= model mB2
modname <- "mB2.stan"
pars = c("a", "bVB", "bVC", "bVT", "bAVB")

# Compiling and producing posterior samples from the model.
mB2stan <- stan(file = modname, data = dataList, model_name=modname , pars=pars ,
            warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
mB2stan.post = subset(as.data.frame(mB2stan),select=pars)

#======= model mB3
modname <- "mB2.stan"
pars = c("a", "bVB", "bVC", "bVT", "bAVB")

# Compiling and producing posterior samples from the model.
mB3stan <- stan(file = modname, data = dataList, model_name=modname , pars=pars ,
            warmup=warmup , iter = iter, chains = chains, cores = cores, refresh=-1)
mB3stan.post = subset(as.data.frame(mB3stan),select=pars)

#===========================
# Plotting and summarizing the posterior distribution
#mB1stan
#traceplot(mB1stan)
#plot(mB1stan)

# Export the samples to a data.frame for easier handling.
#posterior <- as.data.frame(mB1stan)

library(rethinking)

message("Result Phuong - Hoang's stan code...")
precis(mB1stan)
mcmcpairs(mB1stan.post)

plot(coeftab(mB1stan.post,mB2stan.post,mB3stan.post))
