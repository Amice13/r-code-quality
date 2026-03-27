############################## FULL DATA UPDATE ################################

### WARNING: The update run took 420.79 hours.

### The analysis was run on AWS using a c5.12xlarge instance with an optimized 
### configuration of BLAS/LAPACK. The instance houses 2nd generation Intel Xeon 
### Scalable Processors (Cascade Lake). Due to different precision of floating points,
### the exact same results may only be replicable using similar CPUs.

# FRONT MATTER
## Clean up
# rm(list=ls())

# PRELIMINARIES
## Load packages
library(krige)

## Load results from previous run
load("results/krige.fit1.Rdata")

# UPDATE RUN
fullData.fit2 <- update(fullData.fit1, n.iter=4500)

# NONCONVERGENCE DIAGNOSTICS
geweke(fullData.fit2)
heidel.welch(fullData.fit2)

# SAVE RESULTS
save(fullData.fit2, "results/krige.fit2.Rdata")