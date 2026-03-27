#Code to create bootstrapped GSS samples and gather correlations from them 

#Change the working directory as needed
setwd("/Users/djd78/Box/Belief Networks/Replication Materials/Results")

rm(list=ls())

library(igraph)
library(foreign)
library(sjstats)
library(psych)
library(janitor)
library(parallel)

load(file="modelinput.saved")
load(file="yearlist.saved")
d=read.dta("GSS_Recoded.dta", convert.factors=FALSE)

#only keep GSS variables we need
load(file="full_network_2016.saved")
nodes = V(g)$name
d = d[, colnames(d)=="year"|colnames(d) %in% nodes]

#number of bootstrap replications
reps = 5000
###############################################

#define function
bootstrap_GSS_samples = function(y) {
    print(y)
    sub = subset(d, d$year==y) #reduce to data for focal year 
    sub = remove_empty(sub, which=c("cols"))
  
    b = bootstrap(sub, reps) #bootstrap GSS sample N times with replacement
    
    b_corrs = list() #list to store correlations from bootstrapped samples
    
    for(j in 1:reps) { #loop through bootstrapped samples 
        bs = as.data.frame(b$strap[[j]]) #make given sample into data frame 
        b_corrs[[j]] = r[r$year==y, c("x", "y", "year", "j", "year72", "year72_dec", "yearcen")] #correlations that need to be gathered (same as in overall sample)
        b_corrs[[j]]$abs_c = NA #placeholders to be filled in 
        b_corrs[[j]]$abs_cpart = NA
     
     for(c in 1:nrow(b_corrs[[j]])) { #now loop through correlation-pairs and gather them 
        var1 = as.character(b_corrs[[j]]$x[c])
        var2 = as.character(b_corrs[[j]]$y[c])
        b_corrs[[j]]$abs_c[c]=abs(cor.test(bs[,names(bs)==var1], bs[,names(bs)==var2],  use = "pairwise.complete.obs", method="pearson")$estimate)
        if(!(var1=="polviews"|var2=="polviews"|var1=="partyid"|var2=="partyid") & ("polviews" %in% colnames(bs)) & ("partyid" %in% colnames(bs)) ) {
            b_corrs[[j]]$abs_cpart[c] = abs(partial.r(bs, x=c(var1, var2), y=c("polviews", "partyid"), use="pairwise.complete.obs")[2])
        }
     }
    }
    save(b_corrs, file=paste("bootstrapped_corrs_", y, ".saved", sep=""))
    return(b_corrs)
}

#run function
all_bcorrs = lapply(yearlist, bootstrap_GSS_samples)
