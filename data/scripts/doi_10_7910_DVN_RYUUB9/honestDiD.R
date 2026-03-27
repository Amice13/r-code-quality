# Flexible Wages, Bargaining, and The Gender Gap				
# Barbara Biasi and Heather Sarsons									
# Rambachan and Roth (2020) Honest DID test							


library(haven) # install.packages("haven")
library(usethis) # install.packages("usethis")
library(devtools) # install.packages("devtools")
library(caTools) # install.packages("caTools")
library(HonestDiD) # devtools::install_github("asheshrambachan/HonestDiD", force = TRUE)
library(doParallel) # install.packages("doParallel")
library(Rglpk) # install.packages("https://cran.r-project.org/src/contrib/Archive/Rglpk/Rglpk_0.6-3.tar.gz")
library(fixest) # install.packages("fixest")
library(ggplot2)
​
setwd("~/Dropbox/Research/wisconsin_women/data")
# setwd("/mnt/data2/parsley_scratch/honestDiD")​
​
maindata <- read_dta("honestdid.dta")  #This data only uses a subset of stacked_experiments = amazon experiment
​
# Factorize fixed effects variables first
names <- c('district_code','postexp', 'totalexp','master', 'phd', 'high', 'mathematics', 'year', 'expire', 'extension')
maindata[,names] <- lapply(maindata[,names] , factor)
​
# run model
eventstudy = feols(logsalary ~ femx_5 + femx_6 + femx_7 + femx_8 + femx_9 +
                     femx_10 + femx_11 + femx_12 + femx_13 + femx_14 + femx_15 |
                     female +
                     master +  i(master,postexp) +
                     totalexp +  i(postexp,totalexp) +
                     phd + i(phd,postexp) + 
                     high + i(high,postexp) +
                     mathematics + i(mathematics,postexp) +
                     year + i(year,expire) + i(year,extension) +
                     district_code + i(postexp,district_code), 
                     data = maindata, nthreads = 12)


​
# extract objects for honest did functions
betahat = eventstudy$coeftable$Estimate
sigma = summary(eventstudy, cluster = "district_code")$cov.scaled
​stdErrors = summary(eventstudy, cluster = "district_code")$coeftable[,2]
​
# construct vector of event times and the scal reference period
timeVec = c(seq(from=-5, to = -1, by = 1), seq(from=1, to=5, by = 1))
referencePeriod = which(timeVec > 0)
prePeriodIndices = which(timeVec < 0)
postPeriodIndices = which(timeVec > 0)
​
#Create list containing objects produced by the event study
eventstudy_list = list(
  betahat = betahat,
  sigma = sigma,
  timeVec = timeVec,
  referencePeriod = referencePeriod,
  prePeriodIndices = prePeriodIndices,
  postPeriodIndices = postPeriodIndices,
  stdErrors = stdErrors
)
​
# create l_vec corresponding with 12 month of exposure
numPrePeriods = length(eventstudy_list$prePeriodIndices)
numPostPeriods = length(eventstudy_list$postPeriodIndices)
l_vec = basisVector(5, numPostPeriods)
​
### HONEST DID FUNCTIONS
​
# make pool for parallel implementation
registerDoParallel(cores=12)​
​
# Construct robust confidence intervals for Delta^{SD}(M) for 12 months of exposure
DeltaSD_RobustResults = createSensitivityResults(betahat = eventstudy_list$betahat,
                                                 sigma = eventstudy_list$sigma,
                                                 numPrePeriods= numPrePeriods,
                                                 numPostPeriods= numPostPeriods,
                                                 l_vec = l_vec,
                                                 Mvec = seq(from=0, to = 0.01, by=0.001),
                                                 parallel = TRUE)
​
OriginalResults = constructOriginalCS(betahat = eventstudy_list$betahat,
                                      sigma = eventstudy_list$sigma,
                                      numPrePeriods = numPrePeriods,
                                      numPostPeriods = numPostPeriods,
                                      l_vec = l_vec)
​
DeltaSD_SensitivityPlot = createSensitivityPlot(robustResults = DeltaSD_RobustResults,
                                                originalResults = OriginalResults)
# save it out
ggsave("DeltaSD_SensitivityPlot.pdf",plot = DeltaSD_SensitivityPlot + theme_bw())
​
​
# Construct robust confidence intervals for Delta^{SDD}(M)
DeltaSDD_RobustResults = createSensitivityResults(betahat = eventstudy_list$betahat,
                                                  sigma = eventstudy_list$sigma,
                                                  monotonicityDirection = "decreasing",
                                                  numPrePeriods = numPrePeriods,
                                                  numPostPeriods = numPostPeriods,
                                                  l_vec = l_vec,
                                                  Mvec = seq(from=0, to = 0.01, by=0.001),
                                                  parallel = TRUE)
​
DeltaSDD_SensitivityPlot = createSensitivityPlot(robustResults = DeltaSDD_RobustResults,
                                                 originalResults = OriginalResults)
# save it out
ggsave("DeltaSDD_SensitivityPlot.pdf",plot = DeltaSDD_SensitivityPlot + theme_bw())