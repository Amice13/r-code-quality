#####################################################################################################

### R code used to model spread of a hypothetical disease through dynmaic and static networks


#Authors: Andrea Springer, Peter M. Kappeler, Charles L. Nunn

#Reference: "Dynamic vs. static social networks in models of parasite transmission: Predicting Cryptosporidium spread in wild lemurs"
# Journal of Animal Ecology, 2016

###################################################################################################

### Simplistic Scenario of random networks

## 4 groups of 8 individuals

# 3 scenarios of intergroup connections

# randomly combine these scenarios over N two-weekly intervals

### vary tie strength

#################################################################################################

# clear workspace
rm(list = ls())

library(igraph)

# a LHS sample for beta, gamma, intergroup weight and N

#library(tgp)

#param_sample <- as.data.frame(lhs(n=1000,rect=rbind(c(3,15),c(0.05,0.5),c(0.001,0.05),c(0.01,0.2)), shape=c(1,1,1,1)))
#colnames(param_sample)<-c("N","intergroup_weight","beta","gamma")
#param_sample$N<-round(param_sample$N)
#str(param_sample)

#write lhs sample to file

#write.table(param_sample, file=file.path(getwd(),paste0("June24","_","param_sample",".txt")))

param_sample <- read.csv("H:/Modelling/Random_networks/June24_param_sample.txt", sep="")

#################################

#import the 3 networks (here, intergroup weight is still 0.1)

Scen_1 <- as.matrix(read.csv("H:/Modelling/Random_networks/Scen_1.csv", row.names=1, sep=";"))
Scen_2 <- as.matrix(read.csv("H:/Modelling/Random_networks/Scen_2.csv", row.names=1, sep=";"))
Scen_3 <- as.matrix(read.csv("H:/Modelling/Random_networks/Scen_3.csv", row.names=1, sep=";"))


#for each parameter combination, make a combination of length N of these 3 networks

Scenarios<-list()
Scenarios[[1]]<-Scen_1
Scenarios[[2]]<-Scen_2
Scenarios[[3]]<-Scen_3

Samples<-list()
Statics<-list()
IE_weight<-list()
Dynamics<-list()

for (i in 1:nrow(param_sample)){
  N = param_sample$N[i]
  Samples[[i]]<-sample(c(1,2,3), size=N, replace=T)
  IE_weight[[i]]<-sample(c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5), size=N,replace=T) # create a sample of length N of the intergroup weight
  
  Dynamics[[i]]<-list()
  #calculate a static network for each of these combinations 
  for (j in 1:N){
    Dynamics[[i]][[j]]<-Scenarios[[Samples[[i]][[j]]]] #create a list of the networks for each combination
    Dynamics[[i]][[j]]<- replace(Dynamics[[i]][[j]], Dynamics[[i]][[j]]==0.1,IE_weight[[i]][[j]]) #replace intergroup weights by the value in the Weights sample
  }
  Statics[[i]]<-Reduce('+',Dynamics[[i]])/N #calculate the static network
}

######

## calculate some network metrics on the static networks

Static_density<-list()
for ( i in 1:length(Statics)){
  Static_density[[i]]<-graph.density(graph.adjacency(Statics[[i]], mode="undirected", weighted=T))
}

Static_mod<-list()
for ( i in 1:length(Statics)){
  x<-graph.adjacency(Statics[[i]], mode="undirected", weighted=T)
  Static_mod[[i]]<-infomap.community(x,e.weights=E(x)$weight,
                                     nb.trials=500)$modularity
}

Static_mean_tie_strength<-list()
for ( i in 1:length(Statics)){
  m<-Statics[[i]]
  Tri<-m[lower.tri(m,diag=F)]
  Static_mean_tie_strength[[i]]<-mean(Tri)
}
  

##calculate how much rewiring there is in the dynamic networks

#absolute no. of rewiring events
rewiring_events<-list()

for (i in 1:length(Samples)){
  rewiring_events[[i]]<-length(rle(Samples[[i]])$values)-1
}

#rewiring events relative to sampling N
relative_rewiring<-list()
for (i in 1:length(Samples)){
  N=param_sample$N[i]
  relative_rewiring[[i]]<-(length(rle(Samples[[i]])$values)-1)/N
}

#calculate the range and SD of mean tie strength in the dynamic network sets
range_weights<-list()
sd_weights<-list()

for (i in 1:length(Dynamics)){
  means<-list()
  for (j in 1:length(Dynamics[[i]])){
    m<-Dynamics[[i]][[j]]
    Tri<-m[lower.tri(m,diag=F)]
  means[[j]]<-mean(Tri)}
  range_weights[[i]]<-max(unlist(means))-min(unlist(means))
  sd_weights[[i]]<-sd(unlist(means))
}

#######

# save the workspace

save.image()

######################################

#####the functions

#Inf_status = 1 --> S, 2 --> I, 3 --> R

#initialize
initialize = function(init){
  indexcase<-sample(1:nrow(init),1)
  init$Inf_status[indexcase]<-2
  return(init)
}

#infect
infect = function(init1,input,beta){    
  x <-which(init1$Inf_status>= 2)
  for (k in x)  {                                                #for all infected individuals k 
    y <-which(input[k,]> 0)
    for (o in y) {                                               #and all of their contacts o 
      if (runif(1)<= beta*input[k,o]){                           #if random number between 0 and 1 is greater or equal to beta * tie strength
        if (init1$Inf_status[o] ==1 ){                           # and if o is susceptible
          init1$Inf_status[o]<-2                                 #change o to infected
        }
      }
    }
  }
  return(init1)
}

#recovery
recover=function(init1,gamma){
  x <-which(init1$Inf_status> 2)
  for(k in x) {                                          #for all infected individuals after 1 day of infection
    if (runif(1)<= gamma) {       #probability of recovery
      init1$Inf_status[k]<- 0          #change to recovered
    }
  }
  return(init1)
}

#move through infection
move=function(init1){
  x<-which(init1$Inf_status>=2)                         #for all exposed
  for (k in x){
    init1$Inf_status[k]<-init1$Inf_status[k]+1          #at each time step, add 1 to infection status
  }
  return(init1)
}


###count the outcome

count = function(init1){
  run <- i
  step <-j
  S <-sum(length(which(init1$Inf_status==1)))  #No. of susceptible
  I <-sum(length(which(init1$Inf_status>=2)))  #No. of infected
  R <-sum(length(which(init1$Inf_status==0)))  #No. of recovered
  N <- S+I+R                                                       #Total population size
  prevalence <- I/N*100
  Infected_groups <- c(init1$group[which(!init1$Inf_status==1)])
  Infected_groups <- sum(length(unique(Infected_groups)))
  cum_outbreak_size <- sum(length(which(!init1$Inf_status==1)))/nrow(init1) #outbreak size as a fraction of the population
  return(list(c(run=run, step=step,S=S,I=I,R=R,N=N,prevalence=prevalence, Infected_groups=Infected_groups, cumulative_outbreak_size=cum_outbreak_size)))
}

################################################################

### 3 levels of beta and gamma to hold beta and gamma constant
#beta1<-0.001
#beta2<-0.025
#beta3<-0.05
#gamma1<-0.01
#gamma2<-0.1
#gamma3<-0.2

################################################################

# Simulations on the DYNAMIC networks

summary_dynamic1<-list()

ID <- rownames(Scen_1)
init<-as.data.frame(ID, col.names=c("ID","Inf_status"))
init$Inf_status <-1

run_out=list()
output=list()
mean_outbreak_size<-list()

for (i in 1:nrow(param_sample)){
  
  N=param_sample$N[i]
  
  times = 14*N  # 14 days for each network
  
  #beta <- beta3
  #gamma <-gamma3
 beta <- param_sample$beta[i]
 gamma <- param_sample$gamma[i]
  
  run_out[[i]]<-list()
  
  output[[i]]<-list()
  outbreak_size<-list()
  
  for (m in 1:30){               #30 simulations with each parameter combination
    
    out<-list()
    output[[i]][[m]]<-list()
    
    init1<- initialize(init)      #initialize before starting each simulation
    
    for (j in 1:times){
      
      if (j<=14){
        input<-Dynamics[[i]][[1]]} 
      if (j> 14 & j <= 28){
        input<-Dynamics[[i]][[2]]}
      if (j> 28 & j <= 42){
        input<-Dynamics[[i]][[3]]}
      if (j> 42 & j <= 56){
        input<-Dynamics[[i]][[4]]}
      if (j> 56 & j <= 70){
        input<-Dynamics[[i]][[5]]}
      if (j> 70 & j <= 84){
        input<-Dynamics[[i]][[6]]} 
      if (j> 84 & j <= 98){
        input<-Dynamics[[i]][[7]]} 
      if (j> 98 & j <= 112){
        input<-Dynamics[[i]][[8]]}
      if (j> 112 & j <= 126){
        input<-Dynamics[[i]][[9]]}
      if (j> 126 & j <= 140){
        input<-Dynamics[[i]][[10]]}
      if (j> 140 & j <= 154){
        input<-Dynamics[[i]][[11]]}
      if (j> 154 & j <= 168){
        input<-Dynamics[[i]][[12]]}
      if (j> 168 & j <= 182){
        input<-Dynamics[[i]][[13]]}
      if (j> 182 & j <= 196){
        input<-Dynamics[[i]][[14]]}
      if (j> 196 & j <= 210){
        input<-Dynamics[[i]][[15]]}
      
      init1 <- infect(init1,input, beta)
      
      init1 <- recover(init1,gamma)
      
      init1 <- move(init1)
      
      out[[j]] <- count(init1)
      
    }
    
    row.names<-list(1:times)
    col.names<-list(c("run","step","S","I","R","N","prevalence","Infected_Groups","Cum_outbreak_size_percent"))    #save what happened at each timestep in each run
    output[[i]][[m]]<-as.data.frame(matrix(unlist(out), nrow=times, ncol=9,byrow=TRUE,dimnames=c(row.names,col.names)),stringsAsFactors=FALSE)
    
    outbreak_size[[m]] <-output[[i]][[m]][times,"Cum_outbreak_size_percent"]
    
    #save a summary of the simulations for each parameterization
    
    run_out[[i]][[m]]<-list(c(i,beta, gamma, outbreak_size[[m]]))
  }    
  
  run_out[[i]] <- as.data.frame(matrix(unlist(run_out[[i]]),nrow=30,ncol=4,byrow=TRUE))
  col.names1 <- c("run", "beta","gamma", "outbreak_size_percent")
  colnames(run_out[[i]])<-col.names1
  
  
  mean_outbreak_size[[i]]<-mean(run_out[[i]]$outbreak_size_percent)
  
}

summary_dynamic1 <- param_sample
summary_dynamic1$rewiring_absolut <- unlist(rewiring_events)
summary_dynamic1$rewiring_rel <-unlist(relative_rewiring)
summary_dynamic1$mean_outbreak_size <- unlist(mean_outbreak_size)
summary_dynamic1$range_weights <- unlist(range_weights)

summary_dynamic1$beta<-beta3
summary_dynamic1$gamma <- gamma3

#save as text file
mytime <- format(Sys.time(), "%b_%d_%H_%M_%Y")
filename2 <- file.path(getwd(), paste0(mytime, "_","summary_dynamic",".txt"))
write.table(summary_dynamic1,file=filename2)

setwd("I:/Arbeit/DPZ/Modelling/Random_networks/with_strength_var")
for (i in 1:nrow(param_sample)){
  for (m in 1:30){
    filename2 <- file.path(getwd(), paste0(mytime, "_","stepwise_results_dynamic","_",i,"_",m,".txt"))
    write.table(output[[i]][[m]],file=filename2)
  }
}
setwd("H:/Modelling/Random_networks")

#####################################################################


# simulations on their STATIC approximations

summary_static1<-list()

run_out=list()
output=list()

mean_outbreak_size<-list() 

ID <- rownames(Scen_1)   
init<-as.data.frame(ID, col.names=c("ID","Inf_status"))
init$Inf_status <-1


for (i in 1:nrow(param_sample)){
  
  run_out[[i]]<-list()
  
  output[[i]]<-list()
  outbreak_size<-list()
  
  #beta<-beta3
  #gamma<-gamma3
  beta <- param_sample$beta[i]
  gamma <- param_sample$gamma[i]
  
  N=param_sample$N[i]
  
  times=14*N
  
  for (m in 1:30){               #30 simulations with each parameter combination
    
    out<-list()
    output[[i]][[m]]<-list()
    
    input<-Statics[[i]] 
    init1<- initialize(init)      #initialize before starting each simulation
    
    for (j in 1:times){
      
      init1 <- infect(init1,input, beta)
      
      init1 <- recover(init1,gamma)
      
      init1 <- move(init1)
      
      out[[j]] <- count(init1)
      
    }
    
    row.names<-list(1:times)
    col.names<-list(c("run","step","S","I","R","N","prevalence","Infected_Groups","Cum_outbreak_size_percent"))    #save what happened at each timestep in each run
    output[[i]][[m]]<-as.data.frame(matrix(unlist(out), nrow=times, ncol=9,byrow=TRUE,dimnames=c(row.names,col.names)),stringsAsFactors=FALSE)
    
    outbreak_size[[m]] <-output[[i]][[m]][times,"Cum_outbreak_size_percent"]
    
    #save a summary of the simulations for each parameterization
    
    run_out[[i]][[m]]<-list(c(i,beta, gamma, outbreak_size[[m]]))
  }    
  
  run_out[[i]] <- as.data.frame(matrix(unlist(run_out[[i]]),nrow=30,ncol=4,byrow=TRUE))
  col.names1 <- c("run", "beta","gamma", "outbreak_size_percent")
  colnames(run_out[[i]])<-col.names1
  
  mean_outbreak_size[[i]]<-mean(run_out[[i]]$outbreak_size_percent)
  
}

summary_static1 <- param_sample
summary_static1$density<-unlist(Static_density)
summary_static1$modularity<-unlist(Static_mod)
summary_static1$mean_outbreak_size <- unlist(mean_outbreak_size)
summary_static1$mean_tie_strength <- unlist(Static_mean_tie_strength)

summary_static1$beta<-beta3
summary_static1$gamma<-gamma3

#save as text file
mytime <- format(Sys.time(), "%b_%d_%H_%M_%Y")
filename2 <- file.path(getwd(), paste0(mytime, "_","summary_static",".txt"))
write.table(summary_static1,file=filename2)

setwd("I:/Arbeit/DPZ/Modelling/Random_networks/with_strength_var")

for (i in 1:nrow(param_sample)){
  for (m in 1:30){
    filename2 <- file.path(getwd(), paste0(mytime, "_","stepwise_results_static","_",i,"_",m,".txt"))
    write.table(output[[i]][[m]],file=filename2)
  }
}

setwd("H:/Modelling/Random_networks")
###############################################################


