#Code to turn observed correlations into networks for each year

#Change the working directory as needed
setwd("/Users/djd78/Box/Belief Networks/Replication Materials/Results")

rm(list=ls())

require(igraph)

load(file="yearlist.saved")
load(file="modelinput.saved")
###############################################

for(y in yearlist) {
    print(y)
    filename = paste("corr_predictions_", y, ".saved", sep="")
    load(file=filename)
  
    #Prepare weighted edgelist
    e$x = r$x[match(e$j, r$j)]
    e$y = r$y[match(e$j, r$j)]
  
    #Using baseline network 
    e$weight = ifelse(is.na(e$c_obs), e$c_est, e$c_obs) #if observed correlation is available, use it as edge weight; otherwise, use the estimate 

    e$weight[e$weight<0]=0 #bound weights at 0 and 1 for consistency with observed correlations, which are bounded
    e$weight[e$weight>1]=1
    g = graph_from_data_frame(e[, c("x", "y", "weight")], directed=FALSE)
  
    c = cluster_walktrap(g, weights = E(g)$weight) #community detection 
    V(g)$com = c$membership #save module membership for future use 
  
    save(g, file=paste("full_network_", y, ".saved", sep=""))
  }
