##Clean workspace
rm(list=ls())

##------------
## PACKAGES
##------------
library(coda)

##-------------
## IMAGES
##-------------
##Choose state to summarize (i.e., MT or NJ)
state <- "MT"
#state <- "NJ"

##Load the SAE model MCMC samples and data image
load(paste(state,"-chains.RData", sep = ''))
load("data.RData")

##-----------
## Data
##-----------
dat.sub <- dat[dat$STATE == state,]

Ytrue <- Ytrue[Ytrue$STATE == state,]

##------------
## CONSTANTS
##------------
##length of chain
G <- 3100
##burn-in
B <- 100
##number of counties
m <- nrow(dat.sub)

##post burn-in samples were not thinned
sub <- seq(B+1, G, by = 1)

##Median; 95% Credible Interval
quant <- function(x){quantile(x, prob=c(0.5, 0.025, 0.975))}

##--------------------
## FH model summary
##--------------------
##three chains of FH model
chain1 <- FH.chains[[1]]
chain2 <- FH.chains[[2]]
chain3 <- FH.chains[[3]]

##-------------------
## MCMC samples
##-------------------
##Chain 1
theta.mat <- chain1[[1]]
beta.mat <- chain1[[2]]
sigma.sq.v.vec <- chain1[[3]]
g.mat <- chain1[[4]]

##Chain 2
theta.mat2 <- chain2[[1]]
beta.mat2 <- chain2[[2]]
sigma.sq.v.vec2 <- chain2[[3]]
g.mat2 <- chain2[[4]]

##Chain 3
theta.mat3 <- chain3[[1]]
beta.mat3 <- chain3[[2]]
sigma.sq.v.vec3 <- chain3[[3]]
g.mat3 <- chain3[[4]]

##------------------------
##DIAGNOSTICS for 3 Chains
##------------------------
##mcmc objects
tsamps.list <- mcmc.list(mcmc(t(theta.mat[,sub])), mcmc(t(theta.mat2[,sub])),
                         mcmc(t(theta.mat3[,sub])))
bsamps.list <- mcmc.list(mcmc(t(beta.mat[,sub])), mcmc(t(beta.mat2[,sub])),
                         mcmc(t(beta.mat3[,sub])))
ssamps.list <- mcmc.list(mcmc(sigma.sq.v.vec[sub]), mcmc(sigma.sq.v.vec2[sub]),
                         mcmc(sigma.sq.v.vec3[sub]))
gsamps.list <- mcmc.list(mcmc(t(g.mat[,sub])), mcmc(t(g.mat2[,sub])),
                         mcmc(t(g.mat3[,sub])))

##Post burn-in samples
tsamps <- rbind(tsamps.list[[1]], tsamps.list[[2]], tsamps.list[[3]])
bsamps <- rbind(bsamps.list[[1]], bsamps.list[[2]], bsamps.list[[3]])
ssamps <- c(ssamps.list[[1]], ssamps.list[[2]], ssamps.list[[3]])
gsamps <- rbind(gsamps.list[[1]], gsamps.list[[2]], gsamps.list[[3]])

##Summarize posteriors
theta.mean <- apply(exp(tsamps), 2, mean)
theta.mean <- round(theta.mean, digits = 0)

theta.hat <- apply(exp(tsamps), 2, quant)

beta.mean <- apply(bsamps, 2, mean)
beta.hat <- apply(bsamps, 2, quant)

sigma.sq.v.mean <-  apply(as.matrix(ssamps), 2, mean)
sigma.sq.v.hat <- apply(as.matrix(ssamps), 2, quant)

gamma.hat <- apply(gsamps, 2, quant)


##bias
theta.bias <- round(theta.mean - Ytrue$Y.T, digits = 0)
y.bias <- round(dat.sub$Y.i - Ytrue$Y.T, digits = 0)

##Mean squared error (MSE) at state-level
theta.mse <- round(mean((theta.mean - Ytrue$Y.T)^2), digits = 0)
y.mse <- round(mean((dat.sub$Y.i - Ytrue$Y.T)^2), digits = 0)

##Root mean squared error (RMSE) at state-level
theta.rmse <- sqrt(theta.mse)
y.rmse <- sqrt(y.mse)

##coverage at state-level
state.cover <- sum(ifelse(Ytrue$Y.T >= theta.hat[2,] & Ytrue$Y.T <= theta.hat[3,], 1, 0)) / m
state.cover <- round(100 * state.cover, digits = 1)

##95% Credible interval widths for each county
theta.ciw <- round(theta.hat[3,] - theta.hat[2,], digits = 0)

##-------------------
## Plot
##-------------------
##FH model population estimates, direct estimates, and "truth"
plot(theta.mean ~ Ytrue$Y.T, col = "lightseagreen", pch = 19,
     xlim = c(0, round(max(theta.mean), digits = -3) + 1000),
     ylim = c(0, round(max(theta.mean), digits = -3) + 1000),
     xlab = "True Population", ylab = "Population Estimate")
abline(0,1)
points(dat.sub$Y.i ~ Ytrue$Y.T)
legend("bottomright",
  legend = c("FH", "Direct"),
  col = c("lightseagreen", "black"),
  lty = rep(NULL, 2),
  pch = c(19, 1),
  bty = "n")
