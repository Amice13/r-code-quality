##### Setup -----
## If you haven't installed the included "bapvar" package,
## you can do so via the following:
# devtools::install("bapvar")
## Load required packages
library(bapvar)   ## For BaP-VAR sampling and analysis functions
library(dplyr)    ## For data wrangling
library(parallel) ## For parallel computation when sampling
## Source auxiliary functions
source("code/00-util.R") ## The functions are documented (roxygen-style) there
## Ensure needed subdirectories exist
prepare_directories()


##### Data prep -----
## Read in replication data
dat <- read.csv("data/citation-data.csv")
## There are some cases we cannot analyze because one level of court has not
## cited it (the vast majority of un-analyzable cases) or there are so few
## citations by one level of court that there are numerical stability issues.
## So first we create a full list of all the precedents
all_cases <- unique(dat$cited_case)
## Then we create an empty logical vector of the same size
can_use   <- logical(length(all_cases))
## We'll loop over all the cases
for ( i in seq_along(all_cases) ) {
    ## Updating progress in the console as we go
    cat("\r", i, "/", length(all_cases))
    ## Create a temp dataset subset down to the observations for just this case
    case <- all_cases[i]
    tmp  <- dat %>% filter(cited_case == case)
    ## Determine if we can generate start values, since that function will
    ## throw an error if the aforementioned problems are present
    ## (we see if the case was ever overruled as gen_inits() needs this info)
    ovrl <- any(tmp$overruled)
    init <- try(bapvar:::gen_inits(tmp, p = 1, overrule = ovrl), silent = TRUE)
    ## Then we store FALSE if gen_inits() threw an error and TRUE otherwise
    can_use[i] <- !inherits(init, "try-error")
}
## As reported in the paper, we can analyze 4661 of the 5240 precedents
table(can_use)
# can_use
# FALSE  TRUE 
#   579  4661
## Now we get a vector with the cases we can analyze
cases <- all_cases[can_use]
## Subset the data down to observations just for those cases
dat   <- dat %>% filter(cited_case %in% cases)
## (and remove some objects created above we no longer need)
rm(list = c("all_cases", "can_use", "case", "i", "ovrl", "tmp", "init"))


##### Sampling -----
## This script is created with the assumption you will only want to replicate
## a (random) subset of the results, as the models take quite a while to run.
## If you'd like to replicate a specific subset, replace the lines below with
## an appropriate subset selection. If you have several weeks and want to
## replicate the full results, you can simply comment out the lines below.
set.seed(42) ## set seed for reproducibility
N <- 50      ## how many cases do you want to sample for?
cases_to_analyze <- sample(x = cases, size = N)
## You will surely want to do this in parallel;
## if you do not, trivial alteration to the code can avoid parallelization.
## So we setup a cluster (note that Windows users may need to alter this setup)
num_cores <- detectCores() - 2              ## How many cores should we use?
cl <- makeCluster(num_cores, type = "FORK") ## Initiate the cluster
## Now we can analyze each case
parSapplyLB(cl = cl, X = cases_to_analyze, FUN = function(case) {
    ## Subset the data to observations just for the instant case
    tmp <- dat %>% filter(cited_case == case)
    ## Change spaces to dashes and remove periods for filename convenience,
    base_name <- fix_citation(case)
    ## and prepend the subdirectory we want to save them in
    base_name <- paste0("output/", base_name)
    ## Sample for each lag length, saving the results to disk
    saveRDS(bapvar(tmp, lags = 1), file = paste0(base_name, "-lag1.rds"))
    saveRDS(bapvar(tmp, lags = 2), file = paste0(base_name, "-lag2.rds"))
    saveRDS(bapvar(tmp, lags = 3), file = paste0(base_name, "-lag3.rds"))
    ## And invisibly return NULL
    return(invisible(NULL))
})
## Close the cluster
stopCluster(cl)
