#Generating measures of network time trends using bootstrapped data 

#Change the working directory as needed
setwd("/Users/djd78/Box/Belief Networks/Replication Materials/Results")

rm(list=ls())

library(igraph)
library(ineq)

load(file="bootstrapped_pred_corrs.saved")
load(file="modelinput.saved")
load(file="yearlist.saved")

reps = length(boot_preds) #number of bootstrap replications
#####################################################

for(condition in c("full", "partial", "intermed")) { #looping through three conditions: 1) baseline; 2) ideology and party ID removed from network; and 3) ideology and ID-adjusted partial correlations
  
  #Set up matrices to hold results
  Numcom = matrix(NA, nrow=length(yearlist), ncol = reps + 1) #number of modules/communities
  Large = matrix(NA, nrow=length(yearlist), ncol = reps + 1) #size of largest module
  Large2 = matrix(NA, nrow=length(yearlist), ncol = reps + 1) #size of largest two modules
  Rosen = matrix(NA, nrow=length(yearlist), ncol = reps + 1) #Rosenbluth concentration
  Mod = matrix(NA, nrow=length(yearlist), ncol = reps + 1) #Modularity 
  Dens = matrix(NA, nrow=length(yearlist), ncol = reps + 1) #Density 

  Numcom[,1]=yearlist
  Large[,1]=yearlist
  Large2[,1]=yearlist
  Rosen[,1]=yearlist
  Mod[,1]=yearlist
  Dens[,1]=yearlist

  for(i in 1:reps) {
    print(i)
    
    #Set up master edgelist across all years 
    e = boot_preds[[i]]
    e$x = r$x[match(e$j, r$j)]
    e$y = r$y[match(e$j, r$j)]
    
    if(condition == "full") {
      e$weight = ifelse(is.na(e$c_obs), e$c_est, e$c_obs) #baseline condition
    } else{
      if(condition == "partial") {
        e$weight = ifelse(is.na(e$cpart_obs), e$cpart_est, e$cpart_obs) #ideology-adjusted condition
        e = subset(e, !(is.na(e$weight)))
      } else {
        e = subset(e, !(e$x=="polviews") & !(e$y=="polviews") & !(e$x=="partyid") & !(e$y=="partyid"))
        e$weight = ifelse(is.na(e$c_obs), e$c_est, e$c_obs) #baseline condition, but with ideology and party ID excluded
      }
    }
    e$weight[e$weight<0]=0 #bound weights between 0 and 1
    e$weight[e$weight>1]=1
    
    #record network properties for each year
    j = 1 #need a counter to keep track of where to put results in matrices 
    for(y in yearlist) {
      sub = e[e$year==y,]
      g = graph_from_data_frame(sub[, c("x", "y", "weight")], directed=FALSE)
      c = cluster_walktrap(g, weights = E(g)$weight)
      V(g)$com = c$membership
      
      Numcom[j,i+1] = length(table(V(g)$com))
      Large[j,i+1]= (sort(table(V(g)$com), decreasing=T)[1])/length(V(g))
      Large2[j,i+1]= ((sort(table(V(g)$com), decreasing=T)[1])/length(V(g))) + ((sort(table(V(g)$com), decreasing=T)[2])/length(V(g)))
      Rosen[j,i+1] = conc(table(V(g)$com), type="Rosenbluth")
      Mod[j,i+1] = modularity(g, V(g)$com, weights = E(g)$weight)
      Dens[j,i+1] = sum(E(g)$weight) / ((vcount(g)*(vcount(g)-1)))
      
      j = j+1
    }
  }
  
  save(Large, file=paste("Large_", condition, ".saved", sep=""))
  save(Large2, file=paste("Large2_", condition, ".saved", sep=""))
  save(Numcom, file=paste("Numcom_", condition, ".saved", sep=""))
  save(Rosen, file=paste("Rosen_", condition, ".saved", sep=""))
  save(Mod, file=paste("Mod_", condition, ".saved", sep=""))
  save(Dens, file=paste("Dens_", condition, ".saved", sep=""))

}
