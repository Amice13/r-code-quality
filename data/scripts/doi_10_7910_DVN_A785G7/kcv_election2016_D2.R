# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

# all scripts in the replication materials assume bigKRLS 3.0.0 or higher
require(pacman)                   
p_load(bigKRLS, update = TRUE)

# source("https://raw.githubusercontent.com/rdrr1990/bigKRLS/master/examples/cv_election2016.R")
# cv() # with starting starting seed and desired number of reps
# saves two files with high level output
# overview*.csv and summaries*.RData 
# wherein the full file name reflects range of seeds (startseed, startseed + Nreps - 1)


y <- as.matrix(read.csv("2016_election_data/_gop_2016_delta.csv"))
X <- as.matrix(read.csv("2016_election_data/X_2016.csv"))
census <- read.csv("2016_election_data/datasets/US_census_bureau_regions_divisions.csv", stringsAsFactors = FALSE)

# convert state to brodaer census categories 
state <- colnames(X)[18:67][apply(X[,18:67], 1, function(x) which(x == 1))]
division <- census$Division[match(state, census$State.Code)]
r <- matrix(nrow=nrow(X), ncol=length(unique(census$Division)), 0)
r[cbind(1:nrow(r), match(division, unique(census$Division)))] <- 1
Xcensus <- cbind(X[,1:17], r)
colnames(Xcensus)[18:ncol(Xcensus)] <- unique(census$Division)
Xcensus <- Xcensus[,-2]

# kcv is a convenience function to perform many k folds CV iterations.
# each iteration splits data based on a seed which is inputted to bigKRLS.
# then stores the 'overview' (train vs. test statistics) in file named something like
# cv_seeds_1_20.csv
# the summaries (p-value, etc. of training model) are stored in a file named something like
# cv_seeds_summaries_1_to_20.csv
# other items, especially the big matrices, are deliberately not stored.

kcv <- function(startseed, Nreps, ...){
  
   summaries <- list()       # if we cared about p values for this excercise would uncomment lines 66, 67
  
  seeds <- startseed:(startseed + Nreps - 1)
  
  for(i in 1:Nreps){
    
    out <- crossvalidate.bigKRLS(seed = seeds[i], ...)
    
    summaries[[i]] <- summary(out)
    overview <- summaries[[i]][["overview"]]
    colnames(overview) <- paste0("Seed_", i, "-", colnames(overview))
    overview <- cbind(overview, seeds[i])

    if(i == 1){
      overviews <- overview[-5,]
      colnames(overviews)[ncol(overviews)] <- "seed"
    }else{
      overviews <- rbind(overviews, overview[-5,])
    }
    
    cat("\n\n\n\nfinished", i, "\n\n")
    
    write.csv(overviews, 
         file = paste0("kcv_seeds_", startseed, "_to_", startseed + Nreps - 1, ".csv")) 
    
    # deliberately overwrite results after each model is sucessfully estimated
    # in case server connection is lost, there's some rare singularity after 27 runs, etc.
    
    # derivative=FALSE so the summaries won't contain much info but could uncomment below too...
    #
    # save(summaries, 
    #    file = paste0("kcv_seeds_summaries_", startseed, "_to_", startseed + Nreps - 1, ".RData")) 
    #
    
    remove(out)
    
  }
  return(list(summaries = summaries, overviews = overviews))
}
##############################################################################################################
# to estimate all 100 as in Appendix D.2 (500 hour runtime)                                                  #
##############################################################################################################
# kcv_results <- kcv(1, 100, y = y, X = census, Neig=NULL, Kfolds=5, derivative=FALSE, instructions = FALSE) #
##############################################################################################################

overviews <- read.csv("kcv_seeds_1_to_100.csv")
colnames(overviews)[1] <- "statistic"

