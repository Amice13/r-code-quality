############################## FULL DATA ESTIMATION ############################

### WARNING: The full data will take a long time to run 10000 iterations (943.58 hours)
### Please use the code commented out to sample a sub-data for verifying the code.

### The analysis was run on AWS using a c5.12xlarge instance with an optimized 
### configuration of BLAS/LAPACK. The instance houses 2nd generation Intel Xeon 
### Scalable Processors (Cascade Lake). Due to different precision of floating points,
### the exact same results may only be replicable using similar CPUs.

# FRONT MATTER
## Clean up
# rm(list=ls())

# PRELIMINARIES
## Load packages
library(foreign); library(krige)

## Load data 
combined <-read.dta("data/cces08reformattedFine48.dta")
#combined <-combined[sample(c(1:nrow(combined)), 500), ]
# This smaller sample can be used to verify the code

## Data cleaning and formatting variable
combined$cathOrth<-combined$catholic+combined$orthodox
combined$consRelig<-combined$mormon+combined$evangelical
combined$jewMus<-combined$jewish+combined$islam
combined$black <- as.numeric(combined$race==2)
combined$nonBlack <- as.numeric(combined$race==3)
combined$unemployed <- as.numeric(combined$empstat==2)
combined$notWorkforce <- as.numeric(combined$empstat==3)

fullData <- subset(combined, select = c(ideology, age, educ, black, nonBlack, 
                                        female, cathOrth, consRelig, jewMus, 
                                        mainline, rural, ownership, unemployed, 
                                        notWorkforce, inc14, eastings, northings))

dimnames(fullData)[[2]] <- c("Ideology", "Age", "Education", "African.American", 
                             "Nonwhite.nonblack", "Female", "Catholic.Orthodox", 
                             "Evang.Mormon", "Jewish.Muslim", "Mainline", 
                             "Ruralism", "Homeowner", "Unemployed", 
                             "Not.in.workforce","Income", "Eastings", "Northings")

# RUN SAMPLER
## Set parameter
### M <- 100 # For verifying the code
M <- 10000

## Set seed
set.seed(20200202)

## Run
###NYC PRIORS WERE: spatial.share=0.58, range.share=0.54 
###PRIORS BASED ON SPPQ: spatial.share=87/(87+611)=0.12, range.share=4861/4955=0.98.
###PARAMETERS BASED ON EARLY TUNING:range.tol=0.1, beta.var=1000, nugget.tune=1000
#begin.time <- Sys.time() 
fullData.fit <- metropolis.krige(formula = Ideology ~ Age + Education +
                                   I(Age*Education) + African.American +
                                   Nonwhite.nonblack + Female +
                                   I(African.American*Female) +
                                   I(Nonwhite.nonblack*Female) + Catholic.Orthodox +
                                   Evang.Mormon + Jewish.Muslim +
                                   Mainline + Ruralism + Homeowner + Unemployed +
                                   Not.in.workforce + Income + Eastings + Northings +
                                   I(Eastings*Northings) + I(Eastings^2) +
                                   I(Northings^2),
                                 coords = c("Eastings", "Northings"), data = fullData,
                                 n.iter = M, powered.exp=2, spatial.share=0.1,
                                 range.share=0.9, range.tol=0.1, beta.var=1000,
                                 b.tune=0.1, nugget.tune=1000, psill.tune=10)
#end.time <- Sys.time()
#end.time - begin.time

# SUMMARY
class(fullData.fit)
fullData.fit1 <- burnin(fullData.fit1)
summary(fullData.fit1)

## Convergence
geweke(fullData.fit1)
heidel.welch(fullData.fit1)

# SAVE THE RESULTS
save(fullData.fit, fullData.fit1, file="results/krige.fit1.RData")