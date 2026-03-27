#Code to model and impute correlations in bootstrapped samples

#Change the working directory as needed
setwd("/Users/djd78/Box/Belief Networks/Replication Materials/Results")

rm(list=ls())

library(lme4)
library(doBy)
library(parallel)
library(plyr)

load(file="yearlist.saved")
reps = 5000 #number of bootstrap replications
###############################################
#define function

predict_correlations = function(i) {
    print(i)
    
    preds = data.frame() #empty data frame to hold predicted correlations 
    bc = data.frame() #appended data frame containing bootstrapped correlations for all years for a given replication set 
    for(y in yearlist) {
        load(file = paste("bootstrapped_corrs_", y, ".saved", sep=""))
        b_corrs[[i]]$year = y
        bc = rbind(bc, b_corrs[[i]])
        }
  
    #statistical models
    m=lmer(abs_c~yearcen+(1+yearcen|j), data=bc, control = lmerControl(optimizer ="Nelder_Mead"))
    m2 = lmer(abs_cpart~yearcen+(1+yearcen|j), data=bc, control = lmerControl(optimizer ="Nelder_Mead"))
    
    #predict correlations based on models
    bc$count = 1
    master = summaryBy(count ~ j, data=bc, FUN="sum") # master set of unique item-pairs to use later
  
    for(y in yearlist) {
        sub = bc[bc$year==y, ] #subset of observed correlations for focal year
        e = master #set up for predictions
    
    if(!(empty(sub))) {
        e$year=y
        e$yearcen = sub$yearcen[1]
        e$c_est = predict(m, newdata=e)
        e$c_obs = sub$abs_c[match(e$j, sub$j)]
        e$cpart_est = predict(m2, newdata=e, allow.new.levels=T)
        e$cpart_EXISTS = as.character(e$j) %in% rownames(ranef(m2)$j)
        e$cpart_est[e$cpart_EXISTS==F]=NA
        e$cpart_obs = sub$abs_cpart[match(e$j, sub$j)]
        e = e[, c("j", "year", "c_est", "c_obs", "cpart_est", "cpart_obs")]
      
        preds = rbind(preds, e)
    }
  }
  return(preds)
}

boot_preds = lapply(1:reps, predict_correlations)
save(boot_preds, file="bootstrapped_pred_corrs.saved")
