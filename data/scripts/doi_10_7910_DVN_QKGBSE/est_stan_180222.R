setwd("/Statistics/dataset03/jags")
trials <- read.csv("trial1b.csv", header=T)

Ntotal = length(trials$Out)

#Nho giao
NG = trials[,"NG"]
PG = trials[,"PG"]

#Nho giao
V.NG = trials[,"V.NG"]
#Phat giao
V.PG = trials[,"V.PG"]
#Lao giao
V.LG = trials[,"V.LG"]
#Quyen luc
V.QL = trials[,"V.QL"]
#Dan gian
V.DG = trials[,"V.DG"]

#Nguoc Phat giao????
AV.PG = trials[,"AV.PG"]

library(rstan)

# The Stan model as a string.
model_string <- "
// Here we define the data we are going to pass into the model
data {
  int N; // Number of trials
  int V_NG[N];
  int V_PG[N];
  int V_LG[N];
  int V_QL[N];
  int V_DG[N];
  int AV_PG[N];
  int PG[N];
}

// Here we define what 'unknowns' aka parameters we have.
parameters {
  real a;
  real bvpg;
  real bvng;
  real bvlg;
  real bavpg;
  //real<lower=0, upper=1> p[N];
}

// The generative model
model {
  for (n in 1:N)
  {
		 PG[n] ~ bernoulli(inv_logit( a + (bvpg + bvng*V_NG[n] +bvlg*V_LG[n])*V_PG[n] + bavpg*AV_PG[n] ));
		 //logit(p[n]) <- a + (bvpg + bvng*V_NG[n] +bvlg*V_LG[n])*V_PG[n] + bavpg*AV_PG[n];
  }

 a ~ normal(0,10);
 bvpg ~ normal(0,10);
 bvng ~ normal(0,10);
 bvlg ~ normal(0,10);
 bavpg ~ normal(0,10);
}
"

#Setup datalist
dataList = list(
    NG = NG,
    PG = PG,
    V_NG = V.NG,
    V_PG = V.PG,
    V_LG = V.LG,
    V_QL = V.QL,
    V_DG = V.DG,
    AV_NG = AV.NG,
    AV_PG = AV.PG,
    N = Ntotal
  )

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = dataList)

# Plotting and summarizing the posterior distribution
#stan_samples
#traceplot(stan_samples)
plot(stan_samples)

# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)

message("Result Phuong - Hoang's stan code...")
precis(stan_samples)

####==== RETHINKING - map ====
mPG.1 <- map(
 alist(
 PG ~ dbinom( 1 , p ) ,
 logit(p) <- a + (bvpg + bvng*V.NG +bvlg*V.LG)*V.PG + bavpg*AV.PG,
 a ~ dnorm(0,10) ,
 bvpg ~ dnorm(0,10),
 bvng ~ dnorm(0,10),
 bvlg ~ dnorm(0,10),
 bavpg ~ dnorm(0,10)
 ) ,
 data=trials )
 
 message("Result rethinking - map...")
 precis(mPG.1)
 
####==== RETHINKING - map2stan ====
mPG.1 <- map2stan(
 alist(
 PG ~ dbinom( 1 , p ) ,
 logit(p) <- a + (bvpg + bvng*V.NG +bvlg*V.LG)*V.PG + bavpg*AV.PG,
 a ~ dnorm(0,10) ,
 bvpg ~ dnorm(0,10),
 bvng ~ dnorm(0,10),
 bvlg ~ dnorm(0,10),
 bavpg ~ dnorm(0,10)
 ) , debug=FALSE, 
 data=trials )

 message("Result rethinking - map2stan...")
 precis(mPG.1)
 