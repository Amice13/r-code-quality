#####################################################################################################

### R code used to model Cryptospordidium spread through a sifaka population

### WET SEASON  / STATIC VERSION

###

#Authors: Andrea Springer, Peter M. Kappeler, Charles L. Nunn

#Reference: "Dynamic vs. static social networks in models of parasite transmission: Predicting Cryptosporidium spread in wild lemurs"
# Journal of Animal Ecology, 2016

###############################################################################################################


#sampling: latin hypercube for the parameter space

#same sample used as in the dynamic simulation

sample_wet<- read.csv("P:/Daten Andrea/Modelling/Feb_25_15_57_2016_lhs_sample new wet.txt", sep="")


#########################################################################

#The data frame of the individuals

individuals_wet <- read.csv("P:/Daten Andrea/Modelling/individuals_wet_wo_unm.csv", sep=",")

init <- individuals_wet # the initial data frame 

#The interaction matrices

#social interaction
bc_matrix_wet <- read.table("P:/Daten Andrea/Modelling/BC_matrix_wet_with_IE.csv", sep=",", quote="\"")
input_social = as.matrix(bc_matrix_wet)

#environmental: 8*8 matrix of overlap between the groups' home ranges
Overlap_wet <- read.table("P:/Daten Andrea/Modelling/Overlap_wet.csv", sep=",", quote="\"")

input_environ = as.matrix(Overlap_wet)

#the list of the home ranges
HR_list <- read.csv("P:/Daten Andrea/Modelling/HR_list_wet.csv", sep=",")

#The number of simulations for each parameter combination
simulation_runs = 1

#The time frame (days)
times = 70



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
contaminate = function (init1,HR_list,incubation_period){
  x <-which(init1$Inf_status == ceiling(incubation_period) + 2) #the infectious ones
  for (k in x)  {                                                                     #for all infectious individuals k
    HR_list$contam_core_area[init1$group[k]] <- HR_list$contam_core_area[init1$group[k]]+ 1/HR_list$core_area_mean[init1$group[k]]   #add 1/corea area size to the contam_level of it's core area
    HR_list$contam_non_core[init1$group[k]] <- HR_list$contam_non_core[init1$group[k]]+ 1/HR_list$non_core[init1$group[k]] #same for non-core area
  }
  return(HR_list)
}

##infect

#by social contact
infect_social = function(init1,input_social,beta_social,incubation_period){    
  x <-which(init1$Inf_status== ceiling(incubation_period) + 2)
  for (k in x)  {                                                       #for all infected individuals k 
    y <-which(input_social[k,]> 0)
    for (m in y) {                                                      #and all of their contacts m 
      if (runif(1)<=                                                   #if random number between 0 and 1 is greater or equal to the following value:
          1-(1-beta_social[init1$ageclass[m]])^input_social[k,m]){    #the probability of infection dependent on the minutes of body contact per day
        
        if (init1$Inf_status[m] <=1 & init1$Inf_status[m] >=0.5 ){     # and if m is susceptible
          
          init1$Inf_status[m]<-2                                      #change m to infected
        }
      }
    }
  }
  return(init1)
}


# infect from the build-up in the environment
infect_environ = function(init1,input_environ,beta_environ){
  x <-which(init1$Inf_status ==1)
  for (k in x)   {                                    # for every susceptible individual k
    for (l in seq(ncol(input_environ))) {               # and every group's home range l
      if(input_environ[init1$group[k],l]< 1){                        # between groups
        if (runif(1)<=                                # if random number between 0 and 1 is greater or equal to the following value:
            input_environ[init1$group[k],l]         # the overlap between k's home range and every other home range l
            * HR_list$contam_non_core[l]            # * the contamination level in non_core_area of HR l
            * beta_environ[init1$ageclass[k]])      # * the beta specific for k's age-class
        {init1$Inf_status[k]<-2 }            #change k to infected
      }       
      if (input_environ[init1$group[k],l]==1)  {                      # same for within groups
        if (runif(1)<=                                
            input_environ[init1$group[k],l]        
            * (HR_list$contam_non_core[l] + HR_list$contam_core_area[l])   
            * beta_environ[init1$ageclass[k]]){       
          init1$Inf_status[k]<-2  }
      }
    }
  }
  return(init1)
}

##decay of infectiousness in the environment
decay = function(HR_list){
  x <- which(HR_list$contam_core_area > 0)                                           # for all contaminated home ranges
  for (i in x){
    HR_list$contam_core_area[i]<-HR_list$contam_core_area[i]*(0.5^(1/half_life))
    HR_list$contam_non_core[i]<-HR_list$contam_non_core[i]*(0.5^(1/half_life))       # reduce contamination exponentially
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
  for(k in x) {                                          #for all infected individuals 
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
  beta_social = c(sample_wet[h,"beta"]*5,sample_wet[h,"beta"]*3,sample_wet[h,"beta"])                   #for the three age classes, youngest age class has highest value
  beta_environ = c(sample_wet[h,"beta_env"]*5,sample_wet[h,"beta_env"]*3,sample_wet[h,"beta_env"])
  gamma = c(sample_wet[h,"gamma_x"],sample_wet[h,"gamma_y"],sample_wet[h,"gamma_z"])
  alpha = c(sample_wet[h,"alpha_x"],sample_wet[h,"alpha_y"],sample_wet[h,"alpha_z"])
  half_life = sample_wet[h,"half_life"]
  
  output[[h]] = list()
  
  pop_list[[h]] = list()
  
  run_out = list()              #an empty list for the output of each parameterization
  
  for (i in 1:simulation_runs){
    
    init1= initialize(init)      #initialize before starting each simulation
    indexcase <- as.character(init1[which(init1$Inf_status==2),"ID"]) # save the ID of the indexcase
    
    HR_list$contam_core_area <- 0
    HR_list$contam_non_core  <- 0 #set the level of contamination in each HR to 0
    
    out=list()                   #an empty list for the summary output of each time step            
    
    pop_list[[h]][[i]] = list()
    
    for (j in 1:times){
      
      HR_list = contaminate(init1,HR_list,incubation_period)
      
      init1 = infect_social(init1,input_social,beta_social,incubation_period)
      
      init1 = infect_environ(init1,input_environ,beta_environ)
      
      init1 = die(init1,alpha,incubation_period)
      
      init1 = recover(init1,gamma,incubation_period)
      
      init1 = move(init1,incubation_period)
      
      HR_list = decay(HR_list)
      
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

setwd("P:/Daten Andrea/Modelling/Results/NEW_WET_STAT_ENV")

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

