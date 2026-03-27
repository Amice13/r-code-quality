
rm(list = ls())


library(rjags)
library(coda)

### Data
# female = 1, male = 2;
# age categories: 1 = 15-24, 2 = 25-34, 3 = 35-44, 4 = 45-54, 5 = 55-64;
# regions: 1 = central, 2 = coast_mombasa, 3 = coast_other, 4 = eastern/north eastern, 
# 5 = nairobi, 6 = nyanza, 7 = rift valley, 8 = western

dat <- read.csv("stratum_level_data_prov7mom.csv")
nr <- 8
na <- 5
ns <- 2
y <- array(dat$spikepos, dim = c(na, nr, ns))
n <- array(dat$n, dim = c(na, nr, ns))
pw <- array(dat$pw, dim = c(na, nr, ns))

### Model 

model_string <- "model{
		     
# Likelihood 

    for(i in 1:na){
      for(j in 1:nr){
        for(k in 1:ns){
 		  	  y[i, j, k] ~ dbinom(se * p[i, j, k]  +
 		  	                      (1 - sp) * (1 - p[i, j, k]) ,
 		  	                      n[i, j, k])
 			
 		  	  logit(p[i, j, k]) <- (b0 + b1 * (k - 1) 
 		  	                          + u[i] - mean(u[]) + v[j] - mean(v[])) 
 		
        }
      }
    }

# Sensitivity and specificity    
   
   x ~ dbinom(se, 179)
   z ~ dbinom(sp, 910)
    
# Age effect
    
  tau_a <- 1/pow(sd_a, 2)  
  for(i in 1:na){
    u[i] ~ dnorm(mu_a, tau_a)
 	  }
   
# Region effect

 	tau_r <- 1/pow(sd_r, 2)
 	for(j in 1:nr){
 	  v[j] ~ dnorm(mu_r, tau_r)
 	  }
 
# Priors
  
  b0 ~ dnorm(0, 1e-04)
	b1 ~ dnorm(0, 1e-04)
	se ~ dunif(0,1)
	sp ~ dunif(0,1)

# Hyperpriors

  sd_a ~ dnorm(0, 4) T(0,)
  sd_r ~ dnorm(0, 4) T(0,)
  mu_a ~ dnorm(0, 1e-04)
  mu_r ~ dnorm(0, 1e-04)
  
# Predicted prevalence by age, region and sex

  for(i in 1:na){
    age[i] <- inprod(p[i,1:nr,1:ns], pw[i,1:nr,1:ns])/sum(pw[i,1:nr,1:ns])
  }

  for(j in 1:nr){
    region[j] <- inprod(p[1:na,j,1:ns], pw[1:na,j,1:ns])/sum(pw[1:na,j,1:ns])
  }

  for(k in 1:ns){
    sex[k] <- inprod(p[1:na,1:nr,k], pw[1:na,1:nr,k])/sum(pw[1:na,1:nr,k])
  }
  
  national <- inprod(p[1:na,1:nr,1:ns], pw[1:na,1:nr,1:ns])
  
 	}"

### Compile and update

model <- jags.model(textConnection(model_string), 
                    data = list(y = y, 
                                n = n, 
                                x = 166, 
                                z = 901, 
                                pw = pw,
                                na = na,
                                nr = nr,
                                ns = ns), 
                    n.chains = 1,
                    inits = list(.RNG.name = "base::Wichmann-Hill", 
                                 .RNG.seed = 999))
                    
update(model, 1000, progress.bar = "none") # Burn-in period = 1000 samples

samp <- coda.samples(model, 
                     variable.names = c("age", "region", "sex", "national",
                                        "se", "sp"), 
                     n.iter = 10000, 
                     progress.bar = "none")
summary(samp)

### Diagnostics

gelman.plot(samp)

#plot(samp, ask=T)

#autocorr.plot(samp, ask=T)

### Table 

est <- apply(data.frame(samp[[1]]), 2, quantile, probs = c(0.5, 0.025, 0.975))
est <- 100 * round(est, 4)
est_f <- cbind(1:(na + nr + ns + 3),
                paste0(est[1, ], " ", "(", est[2, ], "-", est[3, ], ")")
               )
rbind(est_f[1:na, ], 
      est_f[(na + nr + 3):(na + nr + 4), ],
      est_f[(na + 2):(na + nr + 1), ],
      est_f[(na + 1), ], 
      est_f[(na + nr + 2), ],
      est_f[(na + nr + ns + 3), ]
      )



