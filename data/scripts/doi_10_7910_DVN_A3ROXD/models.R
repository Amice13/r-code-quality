##Clean workspace
rm(list=ls())

##------------
## PACKAGES
##------------
library(MCMCpack)  ##function for inverse gamma distribution

##load the data from the R image
load("data.RData")

##source the two SAE models
source("FH.R")
source("FHCAR.R")

##-----------
## CONSTANTS
##-----------
##Choose the state to analyze (i.e., MT or NJ)
state <- "MT"
#state <- "NJ"

##Select the correct neighborhood matrix (i.e., R.mt or R.nj)
R <- R.mt
#R <- R.nj

##length of each chain
G <- 3100

##set the random seed for reproducibility
set.seed(19)

##----------------------------------
## Multivariate Normal Distribution
##----------------------------------
rmvn <- function(n, mu=0, V = matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}

##-----------
## Data
##-----------
dat.sub <- dat[dat$STATE == state,]

##number of counties
m <- nrow(dat.sub)

##Confirms the covariates are ordered sequentially by county number
X <- X[X$STATE == state, ]
X <- X[order(X$COUNTYNUMB, decreasing = FALSE),]
X <- as.matrix(X[,c("POPDEN2010", "Forest.ha")], nrow = m)
##Design Matrix
X <- cbind(1, X)

y <- as.matrix(dat.sub$Y.i, nrow = m)
logy <- log(y)
tilde.sigma.sq <- as.numeric(dat.sub$sigma.sq.i / y^2)

lm.beta <- lm(logy ~ -1 + X)
beta.start <- as.matrix(coef(lm.beta), nrow = 3)

##------------------------------------------------------
## Runs three chains for the Fay-Herriot (FH) SAE model
##------------------------------------------------------
chain1 <- FH.model(y = logy, X = X, m = m, sigma.sq = tilde.sigma.sq, G = G,
                    beta = beta.start, a0 = 2, b0 = mean(tilde.sigma.sq), sigma.sq.v = 1)

chain2 <- FH.model(y = logy, X = X, m = m, sigma.sq = tilde.sigma.sq, G = G,
                    beta = as.matrix(c(1, -1, 1), nrow = 3), a0 = 2, b0 = mean(tilde.sigma.sq),
                    sigma.sq.v = 0.1)

chain3 <- FH.model(y = logy, X = X, m = m, sigma.sq = tilde.sigma.sq, G = G,
                    beta = as.matrix(c(10, -0.1, 0.1), nrow = 3), a0 = 2, b0 = mean(tilde.sigma.sq),
                    sigma.sq.v = 0.5)

##a list combining the three chains
FH.chains <- list(chain1, chain2, chain3)

##--------------------------------------------
## Runs three chains for the FHCAR SAE model
##--------------------------------------------
chain1 <- FHCAR.model(y = logy, X = X, m = m, sigma.sq = tilde.sigma.sq, G = G, lambda = 0.5,
                      beta = beta.start, a0 = 2, b0 = mean(tilde.sigma.sq), sigma.sq.v = 1, R = R)

chain2 <- FHCAR.model(y = logy, X = X, m = m, sigma.sq = tilde.sigma.sq, G = G, lambda = 0.8,
                      beta = as.matrix(c(1,-1,1), nrow = 3), a0 = 2, b0 = mean(tilde.sigma.sq),
                      sigma.sq.v = 0.1, R = R)

chain3 <- FHCAR.model(y = logy, X = X, m = m, sigma.sq = tilde.sigma.sq, G = G, lambda = 0.1,
                      beta = as.matrix(c(10,-0.1,0.1), nrow = 3), a0 = 2, b0 = mean(tilde.sigma.sq),
                      sigma.sq.v = 0.5, R = R)

## a list combining the three chains
FHCAR.chains <- list(chain1, chain2, chain3)

##--------------------------------------
##save output for summarizing the models
##--------------------------------------
save(list = c("FH.chains","FHCAR.chains"), file = paste(state,"-chains.RData", sep = ''))
