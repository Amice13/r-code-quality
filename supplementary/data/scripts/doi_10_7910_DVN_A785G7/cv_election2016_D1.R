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


y <- as.matrix(read.csv("../2016_election_application/y_gop_2016_delta.csv"))

load("../Appendix_D/Xsmall.RData")
X <- as.matrix(replication_data)[, 3:ncol(replication_data)]

# cv is a convenience function to perform many CV
# each iteration splits data based on a seed which is inputted to bigKRLS.
# then stores the 'overview' (train vs. test statistics) in file named something like
# cv_seeds_1_20.csv
# the summaries (p-value, etc. of training model) are stored in a file named something like
# cv_seeds_summaries_1_to_20.csv
# other items, especially the big matrices, are deliberately not stored.

cv <- function(startseed, Nreps, ...){
  
  summaries <- list()

  seeds <- startseed:(startseed + Nreps - 1)
  
  for(i in 1:Nreps){
    
    out <- crossvalidate.bigKRLS(seed = seeds[i], ...)
    
    summaries[[i]] <- summary(out)
    overview <- summaries[[i]][["overview"]]
    
    overview <- cbind(overview, seeds[i])
    colnames(overview) <- paste0("Seed_", i, "-", colnames(overview))

    if(i == 1){
      overviews <- overview[-5,]
      colnames(overviews)[ncol(overviews)] <- "seed"
    }else{
      overviews <- rbind(overviews, overview[-5,])
    }
    
    cat("\n\nfinished", i, "\n\n")
    
    write.csv(overviews, 
         file = paste0("cv_seeds_", startseed, "_to_", startseed + Nreps - 1, ".csv")) 
    
    save(summaries, 
        file = paste0("cv_seeds_summaries_", startseed, "_to_", startseed + Nreps - 1, ".RData")) 
    
    # deliberately overwrite results after each model is sucessfully estimated
    # in case server connection is lost, there's some rare singularity after 27 runs, etc.
    
    remove(out)
    
  }
  return(list(summaries = summaries, overviews = overviews))
}

##############################################################################################
# to estimate all 100 as in Appendix D.1 (100 hour runtime...)                            ####
##############################################################################################
# cv_results <- cv(1, 100, y = y, X = Xsmall, Neig=50, ptesting=20, instructions = FALSE) ####
##############################################################################################

cv_results <- read.csv("Appendix_D.1/cv_results.csv")

# put back on original scale with ^.5
RMSEs <- cv_results$In.Sample[cv_results$stat == "Mean Squared Error (Full Model)"]^.5
RPMSEs <- cv_results$Out.of.Sample[cv_results$stat == "Mean Squared Error (Full Model)"]^.5
RMSE_AMEs <- cv_results$In.Sample[cv_results$stat == "Mean Squared Error (Average Marginal Effects Only)"]^.5
RPMSE_AMEs <- cv_results$Out.of.Sample[cv_results$stat == "Mean Squared Error (Average Marginal Effects Only)"]^.5

R2s <- cv_results$In.Sample[cv_results$stat == "Pseudo-R^2 (Full Model)"]
R2_Ps <- cv_results$Out.of.Sample[cv_results$stat == "Pseudo-R^2 (Full Model)"]
R2_AMEs <- cv_results$In.Sample[cv_results$stat == "Pseudo-R^2 (Average Marginal Effects Only)"]
R2_AME_Ps <- cv_results$Out.of.Sample[cv_results$stat == "Pseudo-R^2 (Average Marginal Effects Only)"]

ME <- function(In, Out, lab, z=1.96, digits=3){
  cat(lab, "&", round(mean(In), digits), "&", round(mean(Out),digits),
      "\\tabularnewline", "\n\t&",
      paste0("(", round(z*sd(In), digits), ") &"), 
      paste0("(", round(z*sd(Out), digits), ")"), 
      "\\tabularnewline \\tabularnewline \n\n")
}

ME(RMSEs, RPMSEs, "RMSE")
ME(RMSE_AMEs, RPMSE_AMEs, "RMSE$_{AMEs}$")
ME(R2s, R2_Ps, "Pseudo R$^2$")
ME(R2_AMEs, R2_AME_Ps, "Pseudo $R^2_{AME}$")





