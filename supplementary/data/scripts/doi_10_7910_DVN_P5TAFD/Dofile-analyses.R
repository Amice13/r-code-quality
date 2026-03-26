#############################################################################
### DO FILE FOR REPLICATION OF:                                           ###
### Winning Hearts and Minds in Civil Wars:                               ###
### Governance, Leadership Change, and Support for Violent Groups in Iraq ###
### (by Christoph Mikulaschek, Saurabh Pant, and Beza Tesfaye)            ###
############################################################################# 

# Note: This do file contains code for replication of all results displayed in the text or in tables in the main text or in the Appendix. Run the Dofile-dataset-construction-1.do in STATA and the Dofile-dataset-construction-2.R in R before replicating any analyses.
 
#################################
### ANALYSES IN THE MAIN TEXT ###
#################################

### Install relevant packages

install.packages('multiwayvcov')
install.packages('lmtest')
install.packages('parallel')
install.packages('MASS')

### Replicates for wild bootstrap

replicates <- 10000000

###########################################################################################################################################################################################
### MODELS IN TABLE 1: Effect of al-Maliki's resignation on attitudes vis-a-vis armed opposition and government's performance legitimacy: results from governorate fixed-effects OLS models

### Model 1
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE,]) # run model without clustered standard errors
summary(fit1) # display results of model without clustered standard errors to get coefficients and R-squared
require(parallel) 
cl <- makeCluster(4) # create 4 copies of R running in parallel
options(boot.ncpus = 4) # set the number of CPU cores you want the boot function to use to 4
ptm <- proc.time() # time the process
set.seed(20170530) # set the seed of R's random number generator to make the analysis reproducible
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl) # cluster by governorate with wild bootstrap
boot1 <- as.data.frame(bootwild) # create data frame with bootstrapped cluster-robust variance-covariance matrix
coeftest(fit1, bootwild) # get coefficients and standard errors obtained from wild bootstrap and clustered by governorate 
proc.time() - ptm # calculate running time
stopCluster(cl) # stop running 4 copies of R running in parallel
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
  sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1
# sig. effect for Kurds
kurd1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 1])) /
  sqrt(boot1[2, 2] + boot1[nrow(boot1) - 1, nrow(boot1) - 1] + (2 * boot1[2, nrow(boot1) - 1]))
kurd1

### Model 2 
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE,]) # run model without clustered standard errors
summary(fit2) # display results of model without clustered standard errors to get coefficients and R-squared
require(parallel)
cl <- makeCluster(4) # create 4 copies of R running in parallel
options(boot.ncpus = 4) # set the number of CPU cores you want the boot function to use to 4
ptm <- proc.time() # time the process
set.seed(20170530) # set the seed of R's random number generator to make the analysis reproducible
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl) # cluster by governorate with wild bootstrap
boot2 <- as.data.frame(bootwild2) # create data frame with bootstrapped cluster-robust variance-covariance matrix
coeftest(fit2, bootwild2) # get coefficients and standard errors obtained from wild bootstrap and clustered by governorate 
proc.time() - ptm # calculate running time
stopCluster(cl) # stop running 4 copies of R in parallel
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
  sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2
# sig. effect for Kurds
kurd2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 1])) /
  sqrt(boot2[2, 2] + boot2[nrow(boot2) - 1, nrow(boot2) - 1] + (2 * boot2[2, nrow(boot2) - 1]))
kurd2

### Model 3
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE,]) # run model without clustered standard errors
summary(fit3) # display results of model without clustered standard errors to get coefficients and R-squared
require(parallel)
cl <- makeCluster(4) # create 4 copies of R running in parallel
options(boot.ncpus = 4) # set the number of CPU cores you want the boot function to use to 4
ptm <- proc.time() # time the process
set.seed(20170530) # set the seed of R's random number generator to make the analysis reproducible
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl) # cluster by governorate with wild bootstrap
boot3 <- as.data.frame(bootwild3) # create data frame with bootstrapped cluster-robust variance-covariance matrix
coeftest(fit3, bootwild3) # get coefficients and standard errors obtained from wild bootstrap and clustered by governorate 
proc.time() - ptm # calculate running time
stopCluster(cl) # stop running 4 copies of R in parallel
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
  sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3
# sig. effect for Kurds
kurd3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 1])) /
  sqrt(boot3[2, 2] + boot3[nrow(boot3) - 1, nrow(boot3) - 1] + (2 * boot3[2, nrow(boot3) - 1]))
kurd3

### Model 4
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE,]) # run model without clustered standard errors
summary(fit4) # display results of model without clustered standard errors to get coefficients and R-squared
require(parallel)
cl <- makeCluster(4) # create 4 copies of R running in parallel
options(boot.ncpus = 4) # set the number of CPU cores you want the boot function to use to 4
ptm <- proc.time() # time the process
set.seed(20170530) # set the seed of R's random number generator to make the analysis reproducible
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl) # cluster by governorate with wild bootstrap
boot4 <- as.data.frame(bootwild4) # create data frame with bootstrapped cluster-robust variance-covariance matrix
coeftest(fit4, bootwild4) # get coefficients and standard errors obtained from wild bootstrap and clustered by governorate 
proc.time() - ptm # calculate running time
stopCluster(cl) # stop running 4 copies of R in parallel
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
  sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4
# sig. effect for Kurds
kurd4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 1])) /
  sqrt(boot4[2, 2] + boot4[nrow(boot4) - 1, nrow(boot4) - 1] + (2 * boot4[2, nrow(boot4) - 1]))
kurd4

### Model 5
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE,]) # run model without clustered standard errors
summary(fit5) # display results of model without clustered standard errors to get coefficients and R-squared
require(parallel)
cl <- makeCluster(4) # create 4 copies of R running in parallel
options(boot.ncpus = 4) # set the number of CPU cores you want the boot function to use to 4
ptm <- proc.time() # time the process
set.seed(20170530) # set the seed of R's random number generator to make the analysis reproducible
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl) # cluster by governorate with wild bootstrap
boot5 <- as.data.frame(bootwild5) # create data frame with bootstrapped cluster-robust variance-covariance matrix
coeftest(fit5, bootwild5) # get coefficients and standard errors obtained from wild bootstrap and clustered by governorate 
proc.time() - ptm # calculate running time
stopCluster(cl) # stop running 4 copies of R in parallel
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
  sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5
# sig. effect for Kurds
kurd5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 1])) /
  sqrt(boot5[2, 2] + boot5[nrow(boot5) - 1, nrow(boot5) - 1] + (2 * boot5[2, nrow(boot5) - 1]))
kurd5

###################################################################################################################################
### MODELS IN TABLE 2: Effect of al-Malikis resignation on democratic legitimacy: results from governorate fixed effects OLS models

### Model 6
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE,]) # run model without clustered standard errors
summary(fit6) # display results of model without clustered standard errors to get coefficients and R-squared
require(parallel)
cl <- makeCluster(4) # create 4 copies of R running in parallel
options(boot.ncpus = 4) # set the number of CPU cores you want the boot function to use to 4
ptm <- proc.time() # time the process
set.seed(20170530) # set the seed of R's random number generator to make the analysis reproducible
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl) # cluster by governorate with wild bootstrap
boot6 <- as.data.frame(bootwild6) # create data frame with bootstrapped cluster-robust variance-covariance matrix
coeftest(fit6, bootwild6) # get coefficients and standard errors obtained from wild bootstrap and clustered by governorate 
proc.time() - ptm # calculate running time
stopCluster(cl) # stop running 4 copies of R in parallel
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
  sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6
# sig. effect for Kurds
kurd6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 1])) /
  sqrt(boot6[2, 2] + boot6[nrow(boot6) - 1, nrow(boot6) - 1] + (2 * boot6[2, nrow(boot6) - 1]))
kurd6

### Model 7
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE,]) # run model without clustered standard errors
summary(fit7) # display results of model without clustered standard errors to get coefficients and R-squared
require(parallel)
cl <- makeCluster(4) # create 4 copies of R running in parallel
options(boot.ncpus = 4) # set the number of CPU cores you want the boot function to use to 4
ptm <- proc.time() # time the process
set.seed(20170530) # set the seed of R's random number generator to make the analysis reproducible
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl) # cluster by governorate with wild bootstrap
boot7 <- as.data.frame(bootwild7) # create data frame with bootstrapped cluster-robust variance-covariance matrix
coeftest(fit7, bootwild7) # get coefficients and standard errors obtained from wild bootstrap and clustered by governorate 
proc.time() - ptm # calculate running time
stopCluster(cl) # stop running 4 copies of R in parallel
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
  sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7
# sig. effect for Kurds
kurd7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 1])) /
  sqrt(boot7[2, 2] + boot7[nrow(boot7) - 1, nrow(boot7) - 1] + (2 * boot7[2, nrow(boot7) - 1]))
kurd7

#######################################
### ANALYSES IN THE ONLINE APPENDIX ###
#######################################

# Please refer to the commented out notes in the code for models 1-7 above for a description of each command used to run OLS models with clustered standard errors obtained from wild bootstrapping.

###########################################################################################
### MODELS IN APPENDIX TABLE 2: Bivariate OLS models of support for armed opposition groups

### MODEL 8
library(multiwayvcov) # full sample
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild)
proc.time() - ptm
stopCluster(cl)

### MODEL 9
library(multiwayvcov) # Sunnis
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[responses$sunni == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[responses$sunni == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild)
proc.time() - ptm
stopCluster(cl)

### MODEL 10
library(multiwayvcov) # Kurds
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[responses$kurd == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[responses$kurd == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild)
proc.time() - ptm
stopCluster(cl)

### MODEL 11
library(multiwayvcov) # Shiites 
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[responses$shia == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[responses$shia == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild)
proc.time() - ptm
stopCluster(cl)

### MODEL 12
library(multiwayvcov) # Other 
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[responses$other == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[responses$other == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild)
proc.time() - ptm
stopCluster(cl)

#######################################################################################################################################################################################
### MODEL IN APPENDIX TABLE 3: Effect of al-Malikis resignation: results from governorate fixed effects OLS model with three-point outcome variable measuring sympathy for armed groups  

### MODEL 13
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1

####################################################################################################################
### MODELS IN APPENDIX TABLE 4: Change in attitudes in eight subsets of Sunni respondents: estimates from OLS models

# Note: Table 4 only reports the coefficients for resign with standard errors. 

### Primary school and not seeking employment 

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Junior high school and not seeking employment

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Senior high school and not seeking employment

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# University/academy and not seeking employment

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Senior high school & working or unemployed

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Univ./ academy & working or unemployed

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Primary school & working or unemployed

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Junior high school & working or unemployed

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1),])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & (responses$work.1 == 1 | responses$work.2 == 1)], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

#####################################################################################################################
### MODELS IN APPENDIX TABLE 5: Change in attitudes in twelve subsets of Sunni respondents: estimates from OLS models

# Note: Table 5 only reports the coefficients for resign with standard errors. 

# Primary school and not seeking employment

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Junior high school and not seeking employment

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Senior high school and not seeking employment

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# University/academy and not seeking employment

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.3 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Primary school & unemployed

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Univ./ academy & unemployed

# See note in Appendix Table 5
library(multiwayvcov) 
library(lmtest)
fit1 <- lm(use.vio.just ~ resign, data = responses[is.na(responses$use.vio.just) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$use.vio.just) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Senior high school & working

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Univ./ academy & working

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.4 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Junior high school & unemployed

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Senior high school & unemployed

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.3 == 1 & responses$work.2 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Primary school & working

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.1 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

# Junior high school & working

library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign, data = responses[is.na(responses$gov.rating.good) == FALSE  & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign, data = responses[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign, data = responses[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign, data = responses[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$sunni == 1 & responses$educ.2 == 1 & responses$work.1 == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

##############################################################################################################################################################
### MODELS IN APPENDIX TABLE 6: Effect of al-Malikis resignation: results from governorate fixed effects OLS models with standard errors clustered by district

### Model 14
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses$district)
coeftest(fit1, fit1.vcovCL) # s.e. clustered by district

### Model 15
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit2)
library(lmtest)
library(multiwayvcov)
fit2.vcovCL <- cluster.vcov(fit2, responses$district)
coeftest(fit2, fit2.vcovCL) # s.e. clustered by district

### Model 16
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit3)
library(lmtest)
library(multiwayvcov)
fit3.vcovCL <- cluster.vcov(fit3, responses$district)
coeftest(fit3, fit3.vcovCL) # s.e. clustered by district

### Model 17
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit4)
library(lmtest)
library(multiwayvcov)
fit4.vcovCL <- cluster.vcov(fit4, responses$district)
coeftest(fit4, fit4.vcovCL) # s.e. clustered by district

### Model 18
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit5)
library(lmtest)
library(multiwayvcov)
fit5.vcovCL <- cluster.vcov(fit5, responses$district)
coeftest(fit5, fit5.vcovCL) # s.e. clustered by district

##############################################################################################################################################################
### MODELS IN APPENDIX TABLE 7: Effect of al-Malikis resignation: results from governorate fixed effects OLS models with standard errors clustered by district

### Model 19
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit6)
library(lmtest)
library(multiwayvcov)
fit6.vcovCL <- cluster.vcov(fit6, responses$district)
coeftest(fit6, fit6.vcovCL) # clustered by governorate

### Model 20
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit7)
library(lmtest)
library(multiwayvcov)
fit7.vcovCL <- cluster.vcov(fit7, responses$district)
coeftest(fit7, fit7.vcovCL) # clustered by governorate

###############################################################################################################################################################################
### MODEL IN APPENDIX TABLE 8: Effect of al-Malikis resignation on attitudes toward political violence against the government: results from governorate fixed effects OLS model

### Model 21
library(multiwayvcov)
library(lmtest)
fit1 <- lm(use.vio.just ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$use.vio.just) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$use.vio.just) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

##############################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 9: Effect of al-Malikis resignation on democratic legitimacy: results from governorate fixed effects OLS models with additional measures of democratic legitimacy

### Model 22
library(multiwayvcov)
library(lmtest)
fit1 <- lm(petition.nofear ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$petition.nofear) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$petition.nofear) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 23
library(multiwayvcov)
library(lmtest)
fit1 <- lm(official.nofear ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$official.nofear) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$official.nofear) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 24
library(multiwayvcov)
library(lmtest)
fit1 <- lm(run.nofear ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$run.nofear) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$run.nofear) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

###############################################################################################################################################
### MODELS IN APPENDIX TABLE 10: Effect of al-Malikis resignation: results from logit and ordinal logit models (latent variable representation)

### Model 25
library(multiwayvcov)
library(lmtest)
library(MASS)
fit1 <- glm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp) == FALSE,], family = "binomial")
summary(fit1)

### Model 26
library(multiwayvcov)
library(lmtest)
library(MASS)
fit1 <- polr(as.factor(gov.rating.good) ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE,], Hess = TRUE, method = "logistic")
summary(fit1)

### Model 27
library(multiwayvcov)
library(lmtest)
library(MASS)
fit1 <- polr(as.factor(sec.improve) ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE,], Hess = TRUE, method = "logistic")
summary(fit1)

### Model 28
library(multiwayvcov)
library(lmtest)
library(MASS)
fit1 <- polr(as.factor(jobs.improve) ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE,], Hess = TRUE, method = "logistic")
summary(fit1)

### Model 29
library(multiwayvcov)
library(lmtest)
library(MASS)
fit1 <- polr(as.factor(elec.improve) ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE,], Hess = TRUE, method = "logistic")
summary(fit1)

###############################################################################################################################################
### MODELS IN APPENDIX TABLE 11: Effect of al-Malikis resignation: results from logit and ordinal logit models (latent variable representation)

### Model 30
library(multiwayvcov)
library(lmtest)
library(MASS)
fit1 <- polr(as.factor(your.influence.gov) ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE,], Hess = TRUE, method = "logistic")
summary(fit1)

### Model 31
library(multiwayvcov)
library(lmtest)
library(MASS)
fit1 <- polr(as.factor(sit.dem.good) ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE,], Hess = TRUE, method = "logistic")
summary(fit1)

##############################################################################################################################################################
### MODEL IN APPENDIX TABLE 12: Effect of al-Malikis resignation on perception of recent security situation: results from governorate fixed effects OLS models

### Model 32
library(multiwayvcov)
library(lmtest)
fit1 <- lm(sec.past.better ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1

########################################################################################################################################################
### MODELS IN APPENDIX TABLE 13: Al-Malikis resignation and support for armed opposition groups: results from bivariate OLS models with +/-21 day window

### Model 33 # full sample
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses21) # full sample
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses21$gov.fe)
coeftest(fit1, fit1.vcovCL) # clustered by governorate

### Model 34 # Sunni
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses21[responses21$sunni == 1,]) # Sunnis
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses21$gov.fe[responses21$sunni == 1])
coeftest(fit1, fit1.vcovCL) # clustered by governorate

### Model 35 # Kurds
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses21[responses21$kurd == 1,]) # Kurds
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses21$gov.fe[responses21$kurd == 1])
coeftest(fit1, fit1.vcovCL) # clustered by governorate

### Model 36 # Shiite
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses21[responses21$shia == 1,]) # Shiites
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses21$gov.fe[responses21$shia == 1])
coeftest(fit1, fit1.vcovCL) # clustered by governorate

### Model 37 # Other
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses21[responses21$other == 1,]) # others
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses21$gov.fe[responses21$other == 1])
coeftest(fit1, fit1.vcovCL) # clustered by governorate

########################################################################################################################################################################
### MODELS IN APPENDIX TABLE 14: Al-Malikis resignation and support for armed opposition groups: results from governorate fixed effects OLS models with +/-21 day window

### Model 38
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses21[is.na(responses21$armedopp.symp.bin) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses21$gov.fe[is.na(responses21$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

### Model 39 
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses21[is.na(responses21$gov.rating.good) == FALSE,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses21$gov.fe[is.na(responses21$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

### Model 40
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses21[is.na(responses21$sec.improve) == FALSE,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses21$gov.fe[is.na(responses21$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

### Model 41
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses21[is.na(responses21$jobs.improve) == FALSE,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses21$gov.fe[is.na(responses21$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

### Model 42
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses21[is.na(responses21$elec.improve) == FALSE,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses21$gov.fe[is.na(responses21$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

########################################################################################################################################################################
### MODELS IN APPENDIX TABLE 15: Al-Malikis resignation and support for armed opposition groups: results from governorate fixed effects OLS models with +/-21 day window

### Model 43
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses21[is.na(responses21$your.influence.gov) == FALSE,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses21$gov.fe[is.na(responses21$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)

### Model 44
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses21[is.na(responses21$sit.dem.good) == FALSE,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses21$gov.fe[is.na(responses21$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)

########################################################################################################################################################
### MODELS IN APPENDIX TABLE 16: Al-Malikis resignation and support for armed opposition groups: results from bivariate OLS models with +/-14 day window

### Model 45
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses14) # full sample
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses14$gov.fe)
coeftest(fit1, fit1.vcovCL) # clustered by governorate

### Model 46
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses14[responses14$sunni == 1,]) # Sunnis
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses14$gov.fe[responses14$sunni == 1])
coeftest(fit1, fit1.vcovCL) # clustered by governorate

### Model 47
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses14[responses14$kurd == 1,]) # Kurds
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses14$gov.fe[responses14$kurd == 1])
coeftest(fit1, fit1.vcovCL) # clustered by governorate

### Model 48
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses14[responses14$shia == 1,]) # Shiites
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses14$gov.fe[responses14$shia == 1])
coeftest(fit1, fit1.vcovCL) # clustered by governorate

### Model 49
fit1 <- lm(armedopp.symp.bin ~ resign, data = responses14[responses14$other == 1,]) # other
summary(fit1)
library(lmtest)
library(multiwayvcov)
fit1.vcovCL <- cluster.vcov(fit1, responses14$gov.fe[responses14$other == 1])
coeftest(fit1, fit1.vcovCL) # clustered by governorate

########################################################################################################################################################################
### MODELS IN APPENDIX TABLE 17: Al-Malikis resignation and support for armed opposition groups: results from governorate fixed effects OLS models with +/-14 day window

### Model 50
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses14[is.na(responses14$armedopp.symp.bin) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses14$gov.fe[is.na(responses14$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

### Model 51 
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses14[is.na(responses14$gov.rating.good) == FALSE,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses14$gov.fe[is.na(responses14$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)

### Model 52
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses14[is.na(responses14$sec.improve) == FALSE,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses14$gov.fe[is.na(responses14$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

### Model 53
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses14[is.na(responses14$jobs.improve) == FALSE,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses14$gov.fe[is.na(responses14$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

### Model 54
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses14[is.na(responses14$elec.improve) == FALSE,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses14$gov.fe[is.na(responses14$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)

########################################################################################################################################################################
### MODELS IN APPENDIX TABLE 18: Al-Malikis resignation and support for armed opposition groups: results from governorate fixed effects OLS models with +/-14 day window

### Model 55
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses14[is.na(responses14$your.influence.gov) == FALSE,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses14$gov.fe[is.na(responses14$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)

### Model 56
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses14[is.na(responses14$sit.dem.good) == FALSE,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses14$gov.fe[is.na(responses14$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)

##################################################################################################################################################
### MODELS IN APPENDIX TABLE 19: Effect of al-Maliki's resignation with intersection of samples: results from governorate fixed effects OLS models

### Model 57
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
  sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1

### Model 58
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
  sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2

### Model 59
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
  sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3

### Model 60
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
  sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4

### Model 61
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & is.na(responses$gov.rating.good) == FALSE & is.na(responses$sec.improve) == FALSE & is.na(responses$jobs.improve) == FALSE & is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
  sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5

###############################################################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 20: Covariate balance: Effect of pre-treatment covariates on likelihood of inclusion in sample & on likelihood of assignment to treatment group: results from governorate fixed effects OLS models

### Model 62
library(multiwayvcov)
library(lmtest)
fit1 <- lm(aug ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.all$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

### Model 63
library(multiwayvcov)
library(lmtest)
fit1 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

### Model 64
library(multiwayvcov)
library(lmtest)
fit1 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses21)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses21$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

#############################################################################################################################################################
### MODELS IN APPENDIX TABLE 21: Effect of al-Malikis resignation on likelihood of non-response: Main model results from governorate fixed effects OLS models

### Model 65
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.noresp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 66
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good.noresp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 67
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve.noresp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 68
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve.noresp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 69
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve.noresp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

### Model 70
library(multiwayvcov)
library(lmtest)
fit1 <- lm(use.vio.just.noresp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

#############################################################################################################################################################
### MODELS IN APPENDIX TABLE 22: Effect of al-Malikis resignation on likelihood of non-response: Main model results from governorate fixed effects OLS models

### Model 71
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov.noresp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 72
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good.noresp ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

##########################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 23: Effect of pre-treatment covariates on likelihood of assignment to the treatment group: Results from governorate fixed effects OLS models of non-responses

### Model 73
library(multiwayvcov)
library(lmtest)
fit1 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$armedopp.symp.noresp == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[responses$armedopp.symp.noresp == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

### Model 74
library(multiwayvcov)
library(lmtest)
fit2 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$gov.rating.good.noresp == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[responses$gov.rating.good.noresp == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2) 
proc.time() - ptm
stopCluster(cl)

### Model 75
library(multiwayvcov)
library(lmtest)
fit3 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$sec.improve.noresp == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[responses$sec.improve.noresp == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)

### Model 76
library(multiwayvcov)
library(lmtest)
fit4 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$jobs.improve.noresp == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[responses$jobs.improve.noresp == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)

### Model 77
library(multiwayvcov)
library(lmtest)
fit1 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$use.vio.just.noresp == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[responses$use.vio.just.noresp == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

##########################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 24: Effect of pre-treatment covariates on likelihood of assignment to the treatment group: Results from governorate fixed effects OLS models of non-responses

### Model 78
library(multiwayvcov)
library(lmtest)
fit6 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$your.influence.gov.noresp == 1,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[responses$your.influence.gov.noresp == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)

### Model 79
library(multiwayvcov)
library(lmtest)
fit7 <- lm(resign ~ sunni + kurd + other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$sit.dem.good.noresp == 1,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[responses$sit.dem.good.noresp == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)

#################################################################################################################################################################
### MODELS IN APPENDIX TABLE 25: Effect of al-Malikis resignation on interview location and privacy: Main model results from governorate fixed effects OLS models

### Model 80
library(multiwayvcov)
library(lmtest)
fit1 <- lm(athome ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 81
library(multiwayvcov)
library(lmtest)
fit1 <- lm(resp.mostly.alone ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

#############################################################################################################################################################
### MODELS IN APPENDIX TABLE 26: Effect of al-Malikis resignation for those interviewed at home: Main model results from governorate fixed effects OLS models

### Model 82
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$athome == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[responses$athome == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 83
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$athome == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 84
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$athome == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 85
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$athome == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 86
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$athome == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

### Model 87
library(multiwayvcov)
library(lmtest)
fit1 <- lm(use.vio.just ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$athome == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[responses$athome == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

#############################################################################################################################################################
### MODELS IN APPENDIX TABLE 27: Effect of al-Malikis resignation for those interviewed at home: Main model results from governorate fixed effects OLS models

### Model 88
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$athome == 1,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 89
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[responses$athome == 1,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

######################################################################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 28: Effect of al-Malikis resignation on performance legitimacy for those interviewed at home and alone with the enumerators for almost the entire time: results from governorate fixed-effects OLS models

### Model 90
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 91
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 92
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 93
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 94
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

### Model 95
library(multiwayvcov)
library(lmtest)
fit1 <- lm(use.vio.just ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$use.vio.just) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$use.vio.just) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

######################################################################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 29: Effect of al-Malikis resignation on performance legitimacy for those interviewed at home and alone with the enumerators for almost the entire time: results from governorate fixed-effects OLS models

### Model 96
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 97
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE & responses$resp.mostly.alone == 1 & responses$athome == 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

##############################################################################################################################################################################
### MODELS IN APPENDIX TABLE 30: Effect of al-Malikis resignation: Analyses of persistence of the effects through September: results from governorate fixed effects OLS models

### Model 98
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + sep + sep*sunni + sep*kurd + sep*other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.sep)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.sep$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis: change in attitudes from early to late August
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 5])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 5, nrow(boot1) - 5] + (2 * boot1[2, nrow(boot1) - 5]))
sunni1 
# sig. effect for Sunnis: change in attitudes from late August to September
sunni1 <- (as.numeric(coef(fit1)[6]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[6, 6] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[6, nrow(boot1) - 2]))
sunni1 

### Model 99
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + sep + sep*sunni + sep*kurd + sep*other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.sep)
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses.sep$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis: change in attitudes from early to late August
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 5])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 5, nrow(boot2) - 5] + (2 * boot2[2, nrow(boot2) - 5]))
sunni2 
# sig. effect for Sunnis: change in attitudes from late August to September
sunni2 <- (as.numeric(coef(fit2)[6]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[6, 6] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[6, nrow(boot2) - 2]))
sunni2 

### Model 100
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + sep + sep*sunni + sep*kurd + sep*other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.sep)
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses.sep$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis: change in attitudes from early to late August
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 5])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 5, nrow(boot3) - 5] + (2 * boot3[2, nrow(boot3) - 5]))
sunni3 
# sig. effect for Sunnis: change in attitudes from late August to September
sunni3 <- (as.numeric(coef(fit3)[6]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[6, 6] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[6, nrow(boot3) - 2]))
sunni3 

### Model 101
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + sep + sep*sunni + sep*kurd + sep*other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.sep)
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses.sep$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis: change in attitudes from early to late August
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 5])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 5, nrow(boot4) - 5] + (2 * boot4[2, nrow(boot4) - 5]))
sunni4 
# sig. effect for Sunnis: change in attitudes from late August to September
sunni4 <- (as.numeric(coef(fit4)[6]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[6, 6] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[6, nrow(boot4) - 2]))
sunni4 

### Model 102
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + sep + sep*sunni + sep*kurd + sep*other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.sep)
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses.sep$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis: change in attitudes from early to late August
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 5])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 5, nrow(boot5) - 5] + (2 * boot5[2, nrow(boot5) - 5]))
sunni5 
# sig. effect for Sunnis: change in attitudes from late August to September
sunni5 <- (as.numeric(coef(fit5)[6]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[6, 6] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[6, nrow(boot5) - 2]))
sunni5 

##############################################################################################################################################################################
### MODELS IN APPENDIX TABLE 31: Effect of al-Malikis resignation: Analyses of persistence of the effects through September: results from governorate fixed effects OLS models

### Model 103
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + sep + sep*sunni + sep*kurd + sep*other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.sep[is.na(responses.sep$your.influence.gov) == FALSE,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses.sep$gov.fe[is.na(responses.sep$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis: change in attitudes from early to late August
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 5])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 5, nrow(boot6) - 5] + (2 * boot6[2, nrow(boot6) - 5]))
sunni6 
# sig. effect for Sunnis: change in attitudes from late August to September
sunni6 <- (as.numeric(coef(fit6)[6]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[6, 6] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[6, nrow(boot6) - 2]))
sunni6 

### Model 104
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + sep + sep*sunni + sep*kurd + sep*other + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.sep[is.na(responses.sep$sit.dem.good) == FALSE,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses.sep$gov.fe[is.na(responses.sep$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# sig. effect for Sunnis: change in attitudes from early to late August
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 5])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 5, nrow(boot7) - 5] + (2 * boot7[2, nrow(boot7) - 5]))
sunni7 
# sig. effect for Sunnis: change in attitudes from late August to September
sunni7 <- (as.numeric(coef(fit7)[6]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[6, 6] + boot7[nrow(boot7) - 2, nrow(boot7) - 2]  + (2 * boot7[6, nrow(boot7) - 2]))
sunni7 

######################################################################################################################################
### MODELS IN APPENDIX TABLE 32: Effect of al-Malikis resignation: results from district weighted governorate fixed effects OLS models

### Model 105
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE,], weights = responses$dist.weight[is.na(responses$armedopp.symp.bin) == FALSE])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 106
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE,], weights = responses$dist.weight[is.na(responses$gov.rating.good) == FALSE])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 107
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE,], weights = responses$dist.weight[is.na(responses$sec.improve) == FALSE])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 108
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE,], weights = responses$dist.weight[is.na(responses$jobs.improve) == FALSE])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 109
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE,], weights = responses$dist.weight[is.na(responses$elec.improve) == FALSE])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

######################################################################################################################################
### MODELS IN APPENDIX TABLE 33: Effect of al-Malikis resignation: results from district weighted governorate fixed effects OLS models

### Model 110
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE,], weights = responses$dist.weight[is.na(responses$your.influence.gov) == FALSE])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 111
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE,], weights = responses$dist.weight[is.na(responses$sit.dem.good) == FALSE])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

#########################################################################################################################################
### MODELS IN APPENDIX TABLE 34: Effect of al-Malikis resignation: results from governorate weighted governorate fixed effects OLS models

### Model 112
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE,], weights = gov.weight)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 113
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE,], weights = gov.weight)
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 114
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE,], weights = gov.weight)
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 115
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE,], weights = gov.weight)
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 116
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE,], weights = gov.weight)
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

#########################################################################################################################################
### MODELS IN APPENDIX TABLE 35: Effect of al-Malikis resignation: results from governorate weighted governorate fixed effects OLS models

### Model 117
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE,], weights = gov.weight)
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 118
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE,], weights = gov.weight)
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

##################################################################################################################################################################
### MODELS IN APPENDIX TABLE 36: Effect of al-Malikis resignation: Main model results from governorate fixed effects OLS models with fatalities measure from START

### Model 119
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + fatalities.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 120 
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + fatalities.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 121
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + fatalities.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 122
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + fatalities.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 123
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + fatalities.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

##################################################################################################################################################################
### MODELS IN APPENDIX TABLE 37: Effect of al-Malikis resignation: Main model results from governorate fixed effects OLS models with fatalities measure from START

### Model 124
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + fatalities.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 125
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + fatalities.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

#################################################################################################################################################################
### MODELS IN APPENDIX TABLE 38: Effect of al-Malikis resignation: Main model results from governorate fixed effects OLS models with fatalities measure from UCDP

### Model 126
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + best.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 127
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + best.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 128
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + best.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 129
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + best.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 130
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + best.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

#################################################################################################################################################################
### MODELS IN APPENDIX TABLE 39: Effect of al-Malikis resignation: Main model results from governorate fixed effects OLS models with fatalities measure from UCDP

### Model 131
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + best.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 132
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + best.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 40: Effect of al-Malikis resignation: Main model results from governorate fixed effects OLS models with disaggregated fatalities measures from UCDP

### Model 133
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + deaths.civilians.adj.lag1x + deaths.a.adj.lag1x + deaths.b.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 134
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + deaths.civilians.adj.lag1x + deaths.a.adj.lag1x + deaths.b.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 135
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + deaths.civilians.adj.lag1x + deaths.a.adj.lag1x + deaths.b.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 136
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + deaths.civilians.adj.lag1x + deaths.a.adj.lag1x + deaths.b.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 137
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + deaths.civilians.adj.lag1x + deaths.a.adj.lag1x + deaths.b.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 41: Effect of al-Malikis resignation: Main model results from governorate fixed effects OLS models with disaggregated fatalities measures from UCDP

### Model 138
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + deaths.civilians.adj.lag1x + deaths.a.adj.lag1x + deaths.b.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 139
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + deaths.civilians.adj.lag1x + deaths.a.adj.lag1x + deaths.b.adj.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

###########################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 42: Effect of al-Malikis resignation: Main model results from governorate fixed effects OLS models with measure of casualties over four-week period from START

### Model 140
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag28x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 141
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag28x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 142
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag28x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 143
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag28x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 144
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag28x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

###########################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 43: Effect of al-Malikis resignation: Main model results from governorate fixed effects OLS models with measure of casualties over four-week period from START

### Model 145
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag28x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 146
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag28x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

##########################################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 44: Effect of daily number of casualties in a governorate on daily number of survey responses gathered in that governorate: results from governorate fixed effects OLS models

### Model 147
library(multiwayvcov) 
library(lmtest)
fit1 <- lm(responses ~ casualties.lag1x + gov.fe2 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe15 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.perday[responses.perday$mo == 8,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.perday$gov.fe[responses.perday$mo == 8], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

### Model 148
library(multiwayvcov) 
library(lmtest)
fit1 <- lm(responses ~ casualties.lag1x + gov.fe2 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe15 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.perday[(responses.perday$mo == 7 & responses.perday$day > 23) | responses.perday$mo == 8 | (responses.perday$mo == 9 & responses.perday$day < 5),])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.perday$gov.fe[(responses.perday$mo == 7 & responses.perday$day > 23) | responses.perday$mo == 8 | (responses.perday$mo == 9 & responses.perday$day < 5)], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

### Model 149
library(multiwayvcov) 
library(lmtest)
fit1 <- lm(responses ~ casualties.lag1x + gov.fe2 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe15 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.perday[(responses.perday$mo == 7 & responses.perday$day == 31) | (responses.perday$mo == 8 & responses.perday$day < 29),])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.perday$gov.fe[(responses.perday$mo == 7 & responses.perday$day == 31) | (responses.perday$mo == 8 & responses.perday$day < 29)], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

### Model 150
library(multiwayvcov) 
library(lmtest)
fit1 <- lm(responses ~ casualties.lag1x + gov.fe2 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe15 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.perday)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.perday$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

##################################################################################################################
### MODELS IN APPENDIX TABLE 48: Effect of enumerator gender on likelihood of non-response: Main OLS model results

### Model 151
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.noresp ~ enumerator.gender + enumerator.gender*female + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. of effect of likely enumerator gender among female respondents
fem1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 3])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 3, nrow(boot1) - 3] + (2 * boot1[2, nrow(boot1) - 3]))
fem1

### Model 152
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good.noresp ~ enumerator.gender + enumerator.gender*female + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2) 
proc.time() - ptm
stopCluster(cl)
# sig. of effect of likely enumerator gender among female respondents
fem2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 3])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 3, nrow(boot2) - 3] + (2 * boot2[2, nrow(boot2) - 3]))
fem2

### Model 153
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve.noresp ~ enumerator.gender + enumerator.gender*female + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
# sig. of effect of likely enumerator gender among female respondents
fem3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 3])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 3, nrow(boot3) - 3] + (2 * boot3[2, nrow(boot3) - 3]))
fem3

### Model 154
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve.noresp ~ enumerator.gender + enumerator.gender*female + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
# sig. of effect of likely enumerator gender among female respondents
fem4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 3])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 3, nrow(boot4) - 3] + (2 * boot4[2, nrow(boot4) - 3]))
fem4

### Model 155
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve.noresp ~ enumerator.gender + enumerator.gender*female + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
# sig. of effect of likely enumerator gender among female respondents
fem5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 3])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 3, nrow(boot5) - 3] + (2 * boot5[2, nrow(boot5) - 3]))
fem5

### Model 156
library(multiwayvcov)
library(lmtest)
fit1 <- lm(use.vio.just.noresp ~ enumerator.gender + enumerator.gender*female + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. of effect of likely enumerator gender among female respondents
fem1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 3])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 3, nrow(boot1) - 3] + (2 * boot1[2, nrow(boot1) - 3]))
fem1

#######################################################################################################################################################
### MODELS IN APPENDIX TABLE 49: Effect of al-Maliki’s resignation on likelihood of non-response with enumerator gender effects: Main OLS model results

### Model 157
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.noresp ~ enumerator.male + enumerator.male*resign + enumerator.male*male + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + male + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. of change in effect of likely enumerator gender among female respondents
femchange1 <- (as.numeric(coef(fit1)[3]) + as.numeric(coef(fit1)[nrow(boot1) - 4])) /
   sqrt(boot1[3, 3] + boot1[nrow(boot1) - 4, nrow(boot1) - 4] + (2 * boot1[3, nrow(boot1) - 4]))
femchange1

### Model 158
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good.noresp ~ enumerator.male + enumerator.male*resign + enumerator.male*male + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + male + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2) 
proc.time() - ptm
stopCluster(cl)
# sig. of change in effect of likely enumerator gender among female respondents
femchange2 <- (as.numeric(coef(fit2)[3]) + as.numeric(coef(fit2)[nrow(boot2) - 4])) /
   sqrt(boot2[3, 3] + boot2[nrow(boot2) - 4, nrow(boot2) - 4] + (2 * boot2[3, nrow(boot2) - 4]))
femchange2

### Model 159
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve.noresp ~ enumerator.male + enumerator.male*resign + enumerator.male*male + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + male + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
# sig. of change in effect of likely enumerator gender among female respondents
femchange3 <- (as.numeric(coef(fit3)[3]) + as.numeric(coef(fit3)[nrow(boot3) - 4])) /
   sqrt(boot3[3, 3] + boot3[nrow(boot3) - 4, nrow(boot3) - 4] + (2 * boot3[3, nrow(boot3) - 4]))
femchange3

### Model 160
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve.noresp ~ enumerator.male + enumerator.male*resign + enumerator.male*male + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + male + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
# sig. of change in effect of likely enumerator gender among female respondents
femchange4 <- (as.numeric(coef(fit4)[3]) + as.numeric(coef(fit4)[nrow(boot4) - 4])) /
   sqrt(boot4[3, 3] + boot4[nrow(boot4) - 4, nrow(boot4) - 4] + (2 * boot4[3, nrow(boot4) - 4]))
femchange4

### Model 161
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve.noresp ~ enumerator.male + enumerator.male*resign + enumerator.male*male + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + male + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe, boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
# sig. of change in effect of likely enumerator gender among female respondents
femchange5 <- (as.numeric(coef(fit5)[3]) + as.numeric(coef(fit5)[nrow(boot5) - 4])) /
   sqrt(boot5[3, 3] + boot5[nrow(boot5) - 4, nrow(boot5) - 4] + (2 * boot5[3, nrow(boot5) - 4]))
femchange5

### Model 162
library(multiwayvcov)
library(lmtest)
fit1 <- lm(use.vio.just.noresp ~ enumerator.male + enumerator.male*resign + enumerator.male*male + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + male + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# sig. of change in effect of likely enumerator gender among female respondents
femchange1 <- (as.numeric(coef(fit1)[3]) + as.numeric(coef(fit1)[nrow(boot1) - 4])) /
   sqrt(boot1[3, 3] + boot1[nrow(boot1) - 4, nrow(boot1) - 4] + (2 * boot1[3, nrow(boot1) - 4]))
femchange1

########################################################################################################################################
### MODELS IN APPENDIX TABLE 50: Effect of enumerator gender effects on choice to conduct interview outside home: Main OLS model results

### Model 163
library(multiwayvcov)
library(lmtest)
fit1 <- lm(athome ~ enumerator.male + enumerator.male*male + resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + male + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5, data = responses)
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe, boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)

#########################################################################################################################################################################
### MODELS IN APPENDIX TABLE 51: Trends in attitudes vis-`a-vis armed opposition groups between May and 13 August 2014: results from governorate fixed-effects OLS models

### Model 164
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ late.interview.1 + sunni + kurd + other + sunni*late.interview.1 + kurd*late.interview.1 + other*late.interview.1 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & is.na(responses.all$late.interview.1) == FALSE,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & is.na(responses.all$late.interview.1) == FALSE], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# Difference between trends for Sunnis and for Kurds: Var(A-B)=Var(A)+Var(B)-2Cov(A,B)
diffsunnikurd <- (as.numeric(coef(fit1)[nrow(boot1) - 2]) - as.numeric(coef(fit1)[nrow(boot1) - 1])) /
   sqrt(boot1[nrow(boot1) - 2, nrow(boot1) - 2] + boot1[nrow(boot1) - 1, nrow(boot1) - 1] - (2 * boot1[nrow(boot1) - 2, nrow(boot1) - 1]))
diffsunnikurd

### Model 165
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ late.interview.2 + sunni + kurd + other + sunni*late.interview.2 + kurd*late.interview.2 + other*late.interview.2 + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses.all[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 69 & responses.all$day.nr <= 98,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses.all$gov.fe[is.na(responses.all$armedopp.symp.bin) == FALSE & responses.all$day.nr >= 69 & responses.all$day.nr <= 98], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
# Difference between trends for Sunnis and for Kurds: Var(A-B)=Var(A)+Var(B)-2Cov(A,B)
diffsunnikurd <- (as.numeric(coef(fit1)[nrow(boot1) - 2]) - as.numeric(coef(fit1)[nrow(boot1) - 1])) /
   sqrt(boot1[nrow(boot1) - 2, nrow(boot1) - 2] + boot1[nrow(boot1) - 1, nrow(boot1) - 1] - (2 * boot1[nrow(boot1) - 2, nrow(boot1) - 1]))
diffsunnikurd

##################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 52: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Babil: results from governorate fixed-effects OLS models

### Model 166
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 9,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 9], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 167 
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 9,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 9], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 168
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 9,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 9], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 169
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 9,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 9], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 170
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 9,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 9], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

##################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 53: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Babil: results from governorate fixed-effects OLS models

### Model 171
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 9,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 9], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 172
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 9,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 9], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

####################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 54: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Baghdad: results from governorate fixed-effects OLS models

### Model 173
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 1,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 1], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 174 
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 1,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 175
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 1,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 176
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 1,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 177
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 1,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

####################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 55: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Baghdad: results from governorate fixed-effects OLS models

### Model 178
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 1,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 179
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 1,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 1], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

##################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 56: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Dahuk: results from governorate fixed-effects OLS models

### Model 180
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 17,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 17], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 181
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 17,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 17], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 182
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 17,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 17], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 183
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 17,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 17], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 184
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 17,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 17], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

##################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 57: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Dahuk: results from governorate fixed-effects OLS models

### Model 185
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 17,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 17], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 186
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 17,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 17], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

##################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 58: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Erbil: results from governorate fixed-effects OLS models

### Model 187
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 5,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 5], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 188
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 5,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 5], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 189
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 5,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 5], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 190
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 5,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 5], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 191
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 5,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 5], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

##################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 59: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Erbil: results from governorate fixed-effects OLS models

### Model 192
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 5,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 5], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 193
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 5,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 5], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

####################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 60: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Karbala: results from governorate fixed-effects OLS models

### Model 194
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 18,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 18], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 195
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 18,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 18], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 196
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 18,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 18], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 197
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 18,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 18], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 198
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 18,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 18], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

####################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 61: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Karbala: results from governorate fixed-effects OLS models

### Model 199
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 18,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 18], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 200
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 18,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 18], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7

#######################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 62: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Salahhadin: results from governorate fixed-effects OLS models

### Model 201
library(multiwayvcov)
library(lmtest)
fit1 <- lm(armedopp.symp.bin ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 11,])
summary(fit1)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild <- cluster.boot(fit1, responses$gov.fe[is.na(responses$armedopp.symp.bin) == FALSE & responses$gov.fe != 11], boot_type = "wild",
                         wild_type = "rademacher", R = replicates, parallel = cl)
boot1 <- as.data.frame(bootwild) 
coeftest(fit1, bootwild) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni1 <- (as.numeric(coef(fit1)[2]) + as.numeric(coef(fit1)[nrow(boot1) - 2])) /
   sqrt(boot1[2, 2] + boot1[nrow(boot1) - 2, nrow(boot1) - 2] + (2 * boot1[2, nrow(boot1) - 2]))
sunni1
# sig. effect for Shiites
shia1 <- as.numeric(coef(fit1)[2]) / sqrt(boot1[2, 2])
shia1

### Model 202
library(multiwayvcov)
library(lmtest)
fit2 <- lm(gov.rating.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 11,])
summary(fit2)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild2 <- cluster.boot(fit2, responses$gov.fe[is.na(responses$gov.rating.good) == FALSE & responses$gov.fe != 11], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot2 <- as.data.frame(bootwild2) 
coeftest(fit2, bootwild2)
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni2 <- (as.numeric(coef(fit2)[2]) + as.numeric(coef(fit2)[nrow(boot2) - 2])) /
   sqrt(boot2[2, 2] + boot2[nrow(boot2) - 2, nrow(boot2) - 2] + (2 * boot2[2, nrow(boot2) - 2]))
sunni2
# sig. effect for Shiites
shia2 <- as.numeric(coef(fit2)[2]) / sqrt(boot2[2, 2])
shia2

### Model 203
library(multiwayvcov)
library(lmtest)
fit3 <- lm(sec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 11,])
summary(fit3)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild3 <- cluster.boot(fit3, responses$gov.fe[is.na(responses$sec.improve) == FALSE & responses$gov.fe != 11], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot3 <- as.data.frame(bootwild3) 
coeftest(fit3, bootwild3) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni3 <- (as.numeric(coef(fit3)[2]) + as.numeric(coef(fit3)[nrow(boot3) - 2])) /
   sqrt(boot3[2, 2] + boot3[nrow(boot3) - 2, nrow(boot3) - 2] + (2 * boot3[2, nrow(boot3) - 2]))
sunni3
# sig. effect for Shiites
shia3 <- as.numeric(coef(fit3)[2]) / sqrt(boot3[2, 2])
shia3

### Model 204
library(multiwayvcov)
library(lmtest)
fit4 <- lm(jobs.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 11,])
summary(fit4)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild4 <- cluster.boot(fit4, responses$gov.fe[is.na(responses$jobs.improve) == FALSE & responses$gov.fe != 11], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot4 <- as.data.frame(bootwild4) 
coeftest(fit4, bootwild4) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni4 <- (as.numeric(coef(fit4)[2]) + as.numeric(coef(fit4)[nrow(boot4) - 2])) /
   sqrt(boot4[2, 2] + boot4[nrow(boot4) - 2, nrow(boot4) - 2] + (2 * boot4[2, nrow(boot4) - 2]))
sunni4
# sig. effect for Shiites
shia4 <- as.numeric(coef(fit4)[2]) / sqrt(boot4[2, 2])
shia4

### Model 205
library(multiwayvcov)
library(lmtest)
fit5 <- lm(elec.improve ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 11,])
summary(fit5)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild5 <- cluster.boot(fit5, responses$gov.fe[is.na(responses$elec.improve) == FALSE & responses$gov.fe != 11], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot5 <- as.data.frame(bootwild5) 
coeftest(fit5, bootwild5) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni5 <- (as.numeric(coef(fit5)[2]) + as.numeric(coef(fit5)[nrow(boot5) - 2])) /
   sqrt(boot5[2, 2] + boot5[nrow(boot5) - 2, nrow(boot5) - 2] + (2 * boot5[2, nrow(boot5) - 2]))
sunni5
# sig. effect for Shiites
shia5 <- as.numeric(coef(fit5)[2]) / sqrt(boot5[2, 2])
shia5

#######################################################################################################################################################################################
### MODELS IN APPENDIX TABLE 63: Effect of al-Maliki’s resignation on performance legitimacy for those respondents not in Salahhadin: results from governorate fixed-effects OLS models

### Model 206
library(multiwayvcov)
library(lmtest)
fit6 <- lm(your.influence.gov ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 11,])
summary(fit6)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild6 <- cluster.boot(fit6, responses$gov.fe[is.na(responses$your.influence.gov) == FALSE & responses$gov.fe != 11], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot6 <- as.data.frame(bootwild6) 
coeftest(fit6, bootwild6) 
proc.time() - ptm
stopCluster(cl)
#sig. effect for Sunnis
sunni6 <- (as.numeric(coef(fit6)[2]) + as.numeric(coef(fit6)[nrow(boot6) - 2])) /
   sqrt(boot6[2, 2] + boot6[nrow(boot6) - 2, nrow(boot6) - 2] + (2 * boot6[2, nrow(boot6) - 2]))
sunni6
# sig. effect for Shiites
shia6 <- as.numeric(coef(fit6)[2]) / sqrt(boot6[2, 2])
shia6

### Model 207
library(multiwayvcov)
library(lmtest)
fit7 <- lm(sit.dem.good ~ resign + sunni + kurd + other + sunni*resign + kurd*resign + other*resign + casualties.lag1x + female + educ.2 + educ.3 + educ.4 + rural.2 + rural.3 + rural.4 + work.2 + work.3 + eco.sit.bin + age.2 + age.3 + age.4 + age.5 + gov.fe3 + gov.fe4 + gov.fe5 + gov.fe6 + gov.fe7 + gov.fe8 + gov.fe9 + gov.fe10 + gov.fe11 + gov.fe12 + gov.fe13 + gov.fe14 + gov.fe16 + gov.fe17 + gov.fe18, data = responses[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 11,])
summary(fit7)
require(parallel)
cl <- makeCluster(4)
options(boot.ncpus = 4)
ptm <- proc.time()
set.seed(20170530)
bootwild7 <- cluster.boot(fit7, responses$gov.fe[is.na(responses$sit.dem.good) == FALSE & responses$gov.fe != 11], boot_type = "wild",
                          wild_type = "rademacher", R = replicates, parallel = cl)
boot7 <- as.data.frame(bootwild7) 
coeftest(fit7, bootwild7) 
proc.time() - ptm
stopCluster(cl)
# effect for Sunnis
sunni7 <- (as.numeric(coef(fit7)[2]) + as.numeric(coef(fit7)[nrow(boot7) - 2])) /
   sqrt(boot7[2, 2] + boot7[nrow(boot7) - 2, nrow(boot7) - 2] + (2 * boot7[2, nrow(boot7) - 2]))
sunni7
# sig. effect for Shiites
shia7 <- as.numeric(coef(fit7)[2]) / sqrt(boot7[2, 2])
shia7
