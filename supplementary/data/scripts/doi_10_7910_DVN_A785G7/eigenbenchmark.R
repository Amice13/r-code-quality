# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

# all scripts in the replication materials assume bigKRLS 3.0.0 or higher
require(pacman)                   
p_load(bigKRLS, update = TRUE)

# this script illustrates the cumulative impact of different approaches to eigendecomposition, truncation
# including lambdasearch and variance-covariance calculations 
# (but not kernel calculation, derivative estimation, etc.)

######################
# fix path as needed #
######################
y <- as.matrix(read.csv("2016_election_application/y_gop_2016_delta.csv"))
X <- as.matrix(read.csv("2016_election_application/X_2016.csv"))
length(y) == nrow(X) # should be TRUE

bX <- as.big.matrix(X)

KRLS.K.time <- system.time(KRLS::gausskernel(X,ncol(X)))[3]
bKRLS.K.time <- system.time(bigKRLS:::bGaussKernel(bX))[3]

bKRLS.time <- system.time(bigKRLS(y,X, eigtrunc=0, derivative = FALSE, noisy=FALSE, instructions = FALSE))[3]
KRLS.time <- system.time(KRLS::krls(X,y, derivative=FALSE))[3]

bKRLS.time.trunc <- system.time(bigKRLS(y,X, derivative = FALSE, noisy=FALSE, instructions = FALSE))[3]
KRLS.time.trunc <- system.time(KRLS::krls(X,y, derivative=FALSE,eigtrunc=0.01))[3]

bKRLS.time.Neig <- system.time(bigKRLS(y,X, Neig=50,eigtrunc=0.01,  
                                       derivative = FALSE, noisy=FALSE, instructions = FALSE))[3]

remove(bX)

wallclock <- matrix(nrow=2, ncol=3)
rownames(wallclock) <- c("KRLS", "bigKRLS")
colnames(wallclock) <- c("Full Decomp", "Eigentruncation", "Estimating Fewer")

wallclock[1,1] <- KRLS.time - KRLS.K.time
wallclock[2,1] <- bKRLS.time - bKRLS.K.time

wallclock[1,2] <- KRLS.time.trunc - KRLS.K.time
wallclock[2,2] <- bKRLS.time.trunc - bKRLS.K.time

wallclock[2,3] <- bKRLS.time.Neig - bKRLS.K.time

wallclock


