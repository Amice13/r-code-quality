#	Several indivudals contributed to this code.
#
#	Jonathan Hanson throughout the project
#
#   Juraj Medzihorsky provded much of the base code
#   2017-06-03
#
#	Bennet Fauber provided valuable assistance for the code to
#	create a snow cluster via Rmpi on the University of Michgian's 
#	Great Lakes HPC cluster.
#
#	For replication: first run "make data for analysis.do" in Stata.
#	Place the resulting "Indicators.dta" in working directory.
#	The variables bureau_qual and law_order need to be obtained from
#	the PRS Group (https://www.prsgroup.com)
#
#   The problem:
#   A LV model
#   unit: country-year
#   multiple observed variables per country-year
#   different sampling distributions per observed variable
#       ... perhaps they can be grouped based on the sampling dist
#   each variable has an observed binary missingness companion
#   different countries by year
#       ... the Q is why are these missing, options:
#           1) the country did not exist
#           2) the country did exist, but the value is not available
#           option 2 is explicitely modeled, option 1 handled with exclusion

library(readstata13)
library(rjags); load.module("glm"); load.module("lecuyer")
rm(list=ls(all=TRUE)) 

#   for simplicity, this is just a single latent dimension
#   for simplicity, two groups of obs variables
#       one group has a normal sampling dist, the other poisson

#   indicator 
#       group1  ... j1 in 1:n_V1
#       group2  ... j2 in 1:n_V2

#   country   ... ci in 1:n_C
#   time      ... ti in 1:n_T  

#   data
#   country         n_C = 1e1
#   time            n_T = 54

# 	We make a datalist with the required elements: 
# 	N: total number of country-year rows
# 	n_V1: scalar number of indicators that are gaussian
# 	n_V2: scalar number of indicators that are poisson
# 	n_C: scalar number of countries in analysis 
# 	n_T: scalar number of years in analysis 
# 	country: length N vector of country ids present in the data sorted by year and country id within year.
# 	time: length N vector of years present in the data. Repeated values of each year such that there one for every country included in the analysis that year. Sorted by year.
# 	y1: matrix of gaussian indicators, has N rows and n_V1 columns
# 	y2: matrix of poisson indicator, has N rows and n_V2 columns

# Load in data
# Load Stata-formatted dataset

indicators <- read.dta13("Data/Indicators.dta")

#   how many country-years are in the data?
N <- nrow(indicators)

n_C <- max(indicators$cntrynum)
min_T <- min(indicators$year)
max_T <- max(indicators$year)
n_T <- max_T - min_T + 1

#   ys
#	specify numbers of gaussian and poisson indicators
n_V1 <- 16
n_V2 <- 5

#   y1 consists of the guassian indicators
#	y2 consists of the poisson indicators

y1 <- indicators[,3:(2+n_V1)]
y2 <- indicators[,(3+n_V1):(2+n_V1+n_V2)]

data_list <- list(N=N, n_V1=n_V1, n_V2=n_V2,
                  country=indicators$cntrynum, time=indicators$year,
                  y1=y1, y2=y2)

rm(list="indicators", "N", "max_T", "min_T", "n_C", "n_T", "n_V1", "n_V2", "y1", "y2")

# identify number of nodes
if ( ! Sys.getenv("SLURM_NPROCS") == "") {
    n.nodes <- as.numeric(Sys.getenv('SLURM_NPROCS')) - 1
} else {
    n.nodes <- 4
}

var_names <- c('x', 'lambda1', 'lambda2', 'c1', 'c2', 'sigma',
               'beta01', 'beta11', 'beta21', 'beta02', 'beta12', 'beta22')

# The cluster object should be created by the mpirun command. We need its name.
# obtain cluster size and name
cl <- makeCluster()

# export the data to the workers
clusterExport(cl, list("data_list", "var_names", "n.nodes"))

# load modules to workers
clusterEvalQ(cl, library(rjags))
clusterEvalQ(cl, load.module("glm"))
clusterEvalQ(cl, load.module("lecuyer"))
clusterEvalQ(cl, setwd(""))

# This is the first time that the dclone/parallel library is
# needed, so we load it here.

library(dclone)

list.factories(type="rng")
parallel.seeds("lecuyer::RngStream", n.nodes);
parallel.inits(inits = NULL, n.chains = n.nodes)


date()

system.time(parJagsModel(cl, name="oneDtest", "oneDmodel.bug", data=data_list, inits = NULL, n.chains = n.nodes, n.adapt=5000, quiet=FALSE)
)
date()
      
parUpdate(cl, "oneDtest", n.iter=10000)

date()

coda1<-parCodaSamples(cl, "oneDtest", var_names, n.iter=5000, thin = 5)

date()

sink("Estimates/Parameter summary priors.txt",split=TRUE)
summary(coda1)
sink()

library(mcmcplots)
png()
mcmcplot(coda1, dir="Plots", parms="lambda1", filename = "MCMCplots-lambda1", extension = "html")
mcmcplot(coda1, dir="Plots", parms="lambda2", filename = "MCMCplots-lambda2", extension = "html")
mcmcplot(coda1, dir="Plots", parms="c1", filename = "MCMCplots-c1", extension = "html")
mcmcplot(coda1, dir="Plots", parms="c2", filename = "MCMCplots-c2", extension = "html")
mcmcplot(coda1, dir="Plots", parms="sigma", filename = "MCMCplots-sigma", extension = "html")

# always stop the cluster when done
stopCluster(cl)

#mpi.quit()
#q(save='no')

Rmpi::mpi.quit()

