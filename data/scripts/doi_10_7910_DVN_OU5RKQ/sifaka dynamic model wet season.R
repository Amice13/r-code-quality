#####################################################################################################

### R code used to model Cryptospordidium spread through a sifaka population

### WET SEASON  / DYNAMIC NETWORKS 

###

#Authors: Andrea Springer, Peter M. Kappeler, Charles L. Nunn

#Reference: "Dynamic vs. static social networks in models of parasite transmission: Predicting Cryptosporidium spread in wild lemurs"
# Journal of Animal Ecology, 2016

###############################################################################################################

#sampling: latin hypercube for the parameter space

library(tgp)

#sample_wet<-lhs(n=100,rect=rbind(c(2,7),c(0.01,0.1),c(0.001,0.1),
#                                 c(0.01,0.1),c(0.02,0.15),c(0.07,0.2),c(0.1,0.5),c(0.05,0.25),c(0.01,0.1), c(10,20)),
#                shape=c(4,1,1,4,4,4,1,1,1,1),
#                mode = c(4.5,0,0,0.055, 0.085, 0.135, 0,0,0,0))


#colnames(sample_wet)<-c("inc_period","beta","beta_env","gamma_x","gamma_y","gamma_z", "alpha_x","alpha_y","alpha_z","half_life")
#save the sample

#mytime <- format(Sys.time(), "%b_%d_%H_%M_%Y")
#filename_sample <- file.path(getwd(), paste0(mytime, "_","lhs", "_","sample ",".txt"))
#write.table(sample_wet,file=filename_sample,col.names=NA,row.names=TRUE)


sample_wet<- read.csv("P:/Daten Andrea/Modelling/Feb_25_15_57_2016_lhs_sample new wet.txt", sep="")


#########################################################################

#The data frame of the individuals

individuals_wet <- read.csv("P:/Daten Andrea/Modelling/individuals_wet_wo_unm.csv", sep=",")

init <- individuals_wet # the initial data frame 

#The interaction matrices 2-weekly

#social interaction
bc_matrix_wet_1 <- read.table("P:/Daten Andrea/Modelling/interval1_wet.csv", sep=",", quote="\"")
input_social_1 = as.matrix(bc_matrix_wet_1)

bc_matrix_wet_2 <- read.table("P:/Daten Andrea/Modelling/interval2_wet.csv", sep=",", quote="\"")
input_social_2 = as.matrix(bc_matrix_wet_2)

bc_matrix_wet_3 <- read.table("P:/Daten Andrea/Modelling/interval3_wet.csv", sep=",", quote="\"")
input_social_3 = as.matrix(bc_matrix_wet_3)

bc_matrix_wet_4 <- read.table("P:/Daten Andrea/Modelling/interval4_wet.csv", sep=",", quote="\"")
input_social_4 = as.matrix(bc_matrix_wet_4)

bc_matrix_wet_5 <- read.table("P:/Daten Andrea/Modelling/interval5_wet.csv", sep=",", quote="\"")
input_social_5 = as.matrix(bc_matrix_wet_5)



#environmental: 8*8 matrix of overlap between the groups' home ranges 2 weekly
Overlap_wet_1 <- read.table("P:/Daten Andrea/Modelling/Overlap_wet_1.csv", sep=",", quote="\"")
input_environ_1 = as.matrix(Overlap_wet_1)

Overlap_wet_2 <- read.table("P:/Daten Andrea/Modelling/Overlap_wet_2.csv", sep=",", quote="\"")
input_environ_2 = as.matrix(Overlap_wet_2)

Overlap_wet_3 <- read.table("P:/Daten Andrea/Modelling/Overlap_wet_3.csv", sep=",", quote="\"")
input_environ_3 = as.matrix(Overlap_wet_3)

Overlap_wet_4 <- read.table("P:/Daten Andrea/Modelling/Overlap_wet_4.csv", sep=",", quote="\"")
input_environ_4 = as.matrix(Overlap_wet_4)

Overlap_wet_5 <- read.table("P:/Daten Andrea/Modelling/Overlap_wet_5.csv", sep=",", quote="\"")
input_environ_5 = as.matrix(Overlap_wet_5)


###HOME RANGE SIZE ALSO NEEDS TO BE VARIED:

HR_size1<-read.csv("P:/Daten Andrea/Modelling/HR_size1_wet.csv", sep=",")
HR_size2<-read.csv("P:/Daten Andrea/Modelling/HR_size2_wet.csv", sep=",")
HR_size3<-read.csv("P:/Daten Andrea/Modelling/HR_size3_wet.csv", sep=",")
HR_size4<-read.csv("P:/Daten Andrea/Modelling/HR_size4_wet.csv", sep=",")
HR_size5<-read.csv("P:/Daten Andrea/Modelling/HR_size5_wet.csv", sep=",")

HR_contam <- read.csv("P:/Daten Andrea/Modelling/HR_contam_wet.csv", sep=",")


#The number of simulations for each parameter combination
simulation_runs = 1

#The time frame (days)
times = 70  #2.5 months, 5 x 14 days



#The individual processes

#Inf_status: 1 = susceptible, 2 - inc_period = exposed, incubation_period +2 = infectious, inc_period + 3 = recovered, 
#0.5 = returned to susceptible after recovery, 0 = dead 

#initialize
initialize = function(init){
  indexcase<-sample(1:nrow(init),1)
  init$Inf_status[indexcase]<-2
  return(init)
}

#contaminate environment
contaminate.dyn = function (init1,HR_contam, HR_size,incubation_period){
  x <-which(init1$Inf_status == ceiling(incubation_period) + 2)                             #the infectious ones
  for (k in x)  {                                                                           #for all infectious individuals k
    HR_contam$contam_core_area[init1$group[k]] <- HR_contam$contam_core_area[init1$group[k]] + 1/HR_size$core_area[init1$group[k]]   #add 1/corea area size to the contam_level of it's core area
    HR_contam$contam_non_core[init1$group[k]]  <- HR_contam$contam_non_core[init1$group[k]]+ 1/HR_size$non_core[init1$group[k]]      #same for non-core area
  }
  return(HR_contam)
}

##infect

#by social contact
infect_social.dyn = function(init1,input_social,beta_social,incubation_period){    
  x <-which(init1$Inf_status== ceiling(incubation_period) + 2)
  for (k in x)  {                                                     #for all infected individuals k 
    y <-which(input_social[k,]> 0)
    for (m in y) {                                                    #and all of their contacts m 
      if (runif(1)<=                                                  #if random number between 0 and 1 is greater or equal to the following value:
          1-(1-beta_social[init1$ageclass[m]])^input_social[k,m]){  #the probability of infection dependent on the minutes of body contact per day
        
        if (init1$Inf_status[m] <=1 & init1$Inf_status[m] >=0.5 ){    # and if m is susceptible
          
          init1$Inf_status[m]<-2                                      #change m to infected
        }
      }
    }
  }
  return(init1)
}


# infect from the build-up in the environment
infect_environ.dyn = function(init1,input_environ,beta_environ){
  x <-which(init1$Inf_status ==1)
  for (k in x)   {                                  # for every susceptible individual k
    for (l in seq(ncol(input_environ))) {           # and every group's home range l
      if(input_environ[init1$group[k],l]< 1){                        # between groups
        if (runif(1)<=                              # if random number between 0 and 1 is greater or equal to the following value:
            input_environ[init1$group[k],l]         # the overlap between k's home range and every other home range l
            * HR_contam$contam_non_core[l]          # * the contamination level in non_core_area of HR l
            * beta_environ[init1$ageclass[k]])      # * the beta specific for k's age-class
        {init1$Inf_status[k]<-2 }                   #change k to infected
      }       
      
      if (input_environ[init1$group[k],l]==1)  {    # same for within groups: they can get infected from both core and non-core area
        if (runif(1)<=                                
            input_environ[init1$group[k],l]        
            * (HR_contam$contam_non_core[l] + HR_contam$contam_core_area[l])   
            * beta_environ[init1$ageclass[k]]){       
          init1$Inf_status[k]<-2  }
      }
    }
  }
  return(init1)
}

## decay of infectiousness in the environment
decay.dyn = function(HR_list){
  x <- which(HR_contam$contam_core_area > 0)                                           # for all contaminated home ranges
  for (i in x){
    HR_contam$contam_core_area[i]<-HR_contam$contam_core_area[i]*(0.5^(1/half_life))
    HR_contam$contam_non_core[i]<-HR_contam$contam_non_core[i]*(0.5^(1/half_life))   # reduce contamination exponentially
  }
  return(HR_list)
}


##die
die = function(init1,alpha,incubation_period){
  x <- which(init1$Inf_status==ceiling(incubation_period) +2)    #if mortality only occurs at specific stages of infection, can change those numbers
  for (k in x){                                          #for all infected
    if (runif(1)<=alpha[init1$ageclass[k]]){
      init1$Inf_status[k]<-0                             #change to dead
    }
  }
  return(init1)
}

##move through infection
move=function(init1,incubation_period){
  x<-which(init1$Inf_status>=2 & init1$Inf_status<ceiling(incubation_period) + 2)   #for all exposed
  for (k in x){
    init1$Inf_status[k]<-init1$Inf_status[k]+1          #at each time step, move through the "exposed" compartment
  }
  return(init1)
}

##recover
recover=function(init1,gamma,incubation_period){
  x <-which(init1$Inf_status==ceiling(incubation_period) + 2)
  for(k in x) {                                          #for all infected individuals after day 4 of infection
    if (runif(1)<= gamma[init1$ageclass[k]]) {       #probability of recovery
      init1$Inf_status[k]<-ceiling(incubation_period) + 3          #change to recovered
      
    }
  }
  return(init1)
}



###count the outcome

count = function(init1){
  S <-sum(length(which(init1$Inf_status<=1 & init1$Inf_status>=0.5)))                      #No. of susceptible
  I <-sum(length(which(init1$Inf_status>1 & init1$Inf_status <= ceiling(incubation_period) + 2)))  #No. of infected
  R <-sum(length(which(init1$Inf_status==ceiling(incubation_period) + 3)))  #No. of recovered
  N <- S+I+R                                                       #Total population size
  prevalence <- I/N*100
  Infected_groups <- c(init1$group[which(!init1$Inf_status==1)])
  Infected_groups <- sum(length(unique(Infected_groups)))
  cum_outbreak_size <- sum(length(which(!init1$Inf_status==1)))
  return(list(c(S=S,I=I,R=R,N=N,prevalence=prevalence, Infected_groups=Infected_groups, cumulative_outbreak_size=cum_outbreak_size)))
}


#############################################################################

#an empty list for the summary output of each run

output<-list()

#an empty list to save the status of the population of each time step of each run

pop_list=list()     

#an empty list for the overall model output
summary = list()   

####################################################################################

#Do the iterations

# h stands for the parameter-combi

# i for the simulation

# j for each time step 
#--> this is how e.g. the pop_lists are stored: 
#[[1]][[1]][[1]] denotes init1 for the 1st param combi in the 1st simulation at the 1st timestep

parameterizations <- nrow(sample_wet)

for (h in 1:parameterizations){                            #for all parameterizations
  
  incubation_period = sample_wet[h,"inc_period"]            #define the parameters
  beta_social = c(sample_wet[h,"beta"]*5,sample_wet[h,"beta"]*3,sample_wet[h,"beta"])
  beta_environ = c(sample_wet[h,"beta_env"]*5,sample_wet[h,"beta_env"]*3,sample_wet[h,"beta_env"])
  gamma = c(sample_wet[h,"gamma_x"],sample_wet[h,"gamma_y"],sample_wet[h,"gamma_z"])
  alpha = c(sample_wet[h,"alpha_x"],sample_wet[h,"alpha_y"],sample_wet[h,"alpha_z"])
  half_life = sample_wet[h,"half_life"]
  
  output[[h]] = list()
  
  pop_list[[h]] = list()
  
  run_out = list()              #an empty list for the output of each parameterization
  
  for (i in 1:simulation_runs){
    
    init1= initialize(init)       #initialize before starting each simulation
    indexcase <- as.character(init1[which(init1$Inf_status==2),"ID"]) #save the ID of the indexcase
    
    HR_contam$contam_core_area <- 0
    HR_contam$contam_non_core  <- 0  #set the level of contamination in each HR to 0
    
    out=list()                   #an empty list for the summary output of each time step            
    
    pop_list[[h]][[i]] = list()
    
    for (j in 1:times){
      
      if (j <= 14){ 
        input_social <- input_social_1
        input_environ <- input_environ_1
        HR_size <- HR_size1}
      
      if (j > 14 & j <= 28){ 
        input_social <- input_social_2
        input_environ <- input_environ_2
        HR_size <- HR_size2}
      
      if (j > 28 & j <= 42){ 
        input_social <- input_social_3
        input_environ <- input_environ_3
        HR_size <- HR_size3 }
      
      if (j > 42 & j <= 56){ 
        input_social <- input_social_4
        input_environ <- input_environ_4
        HR_size <- HR_size4}
      
      if (j > 56 & j <= 70){ 
        input_social <- input_social_5
        input_environ <- input_environ_5
        HR_size <- HR_size5}
      
      HR_contam = contaminate.dyn(init1,HR_contam, HR_size,incubation_period)
      
      init1 = infect_social.dyn(init1,input_social,beta_social,incubation_period)
      
      init1 = infect_environ.dyn(init1,input_environ,beta_environ)
      
      init1 = die(init1,alpha,incubation_period)
      
      init1 = recover(init1,gamma,incubation_period)
      
      init1 = move(init1,incubation_period)
      
      HR_contam = decay.dyn(HR_contam)
      
      pop_list[[h]][[i]][[j]]=init1
      
      out[[j]] = count(init1)
      
    }
    
    row.names<-list(1:times)
    col.names<-list(c("S","I","R","N","prevalence","Infected_Groups","Cum_outbreak_size"))                    #save what happened at each timestep in each run
    output[[h]][[i]]<-as.data.frame(matrix(unlist(out), nrow=times, ncol=7,byrow=TRUE,dimnames=c(row.names,col.names)),stringsAsFactors=FALSE)
    
    outbreak_size <-output[[h]][[i]][times,"Cum_outbreak_size"]
    groups_affected<-max(output[[h]][[i]][,"Infected_Groups"])
    deaths<-nrow(init) - output[[h]][[i]][times,"N"]
    
    #save a summary of the simulations for each parameterization
    
    run_out[[i]]<-list(c(h,i,incubation_period,beta_social,beta_environ,gamma,alpha,half_life, outbreak_size,groups_affected,deaths,indexcase))
    
    }
  
  row.names1<-list(c(1:simulation_runs))
  col.names1<-list(c("parameterization","simulation_no","Inc_period", "beta_x","beta_y","beta_z","beta_env_x","beta_env_y","beta_env_z","gamma_x","gamma_y",
                     "gamma_z", "alpha_x","alpha_y","alpha_z","half_life","outbreak_size","groups_affected","deaths","indexcase"))
  
  summary[[h]] = as.data.frame(matrix(unlist(run_out),nrow=simulation_runs,ncol=20,byrow=TRUE,
                                      dimnames=c(row.names1,col.names1)),stringsAsFactors=FALSE)
  
  cols.num <- c("parameterization","simulation_no","Inc_period", "beta_x","beta_y","beta_z","beta_env_x","beta_env_y","beta_env_z","gamma_x","gamma_y",
                "gamma_z", "alpha_x","alpha_y","alpha_z","half_life","outbreak_size","groups_affected","deaths")
  summary[[h]][cols.num] <- sapply(summary[[h]][cols.num],as.numeric)
  
}

model_output_wet<-do.call(rbind,summary)


##### SAVE ALL THE RESULTS OF THE SIMULATION into text files####################################

setwd("P:/Daten Andrea/Modelling/Results/NEW_WET_DYN_ENV")

for (h in 1:parameterizations){
  
  for (i in 1:simulation_runs){
    mytime <- format(Sys.time(), "%b_%d_%H_%M_%Y")
    filename1 <- file.path(getwd(), paste0(mytime, "_","output","_", "sim", h, "_","run", i, ".txt"))
    write.table(output[[h]][[i]],file=filename1,col.names=NA,row.names=TRUE)
  }
}


mytime <- format(Sys.time(), "%b_%d_%H_%M_%Y")
filename3<-file.path(getwd(), paste0(mytime, "_","output_wet",".txt"))

write.table(model_output_wet, file=filename3,row.names=FALSE)


##################################################################################################

