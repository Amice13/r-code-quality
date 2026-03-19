####################
### Packages
####################

library(coda)
library(R2WinBUGS)
library(R2jags)
library(jagsUI)

####################
### Data 
####################


data<- read.csv("Vespertilio_murinus_food_sexmaturation_data.csv", sep=";")


data <-data[!is.na(data$testes_class),]
data <-data[!is.na(data$BAT_ID),]
data$day <- data$day +1
data$BM <- as.numeric(as.character(data$BM))
data$FA <- as.numeric(as.character(data$FA))


####################
### create data 
####################
data <- data[!is.na(data$HI5),]

#Nb of individuals
NID <- length(levels(as.factor(data$BAT_ID)))


#Nb of data
N <- nrow(data)
#variable of bat identity
id <- as.numeric(as.factor(data$BAT_ID))


data$testes_class <- data$testes_class+1

tauJ <- (15)/365
################################        
####Model
################################
sink("Testes.jags")
cat("
model {

for(i in 1:N){  
		## loop over observations
	    ## form the linear predictor
        
		mu[i] <-x[i]*beta+eps[id[i]]
	
	      ## cumulative logistic probabilities
	      logit(Q[i,1]) <- tau[1]-mu[i]
	      p[i,1] <- Q[i,1]
	      
	      for(j in 2:4){
                logit(Q[i,j]) <- tau[j]+effhi[j-1]*HI5[i]-mu[i]
                p[i,j] <- Q[i,j] - Q[i,j-1]   
	      }
	      p[i,5] <- 1 - Q[i,4]
	      y[i] ~ dcat(p[i,1:5])  
	      ## p[i,] sums to 1 for each i
	}
	## priors over lin parameter

	beta~ dnorm(0,0.001)


    ## hierarchical model over BAT_ID 
	for(k in 1: NID){
	      eps[k] ~ dnorm(0,eta)
  }  
	eta <- 1/pow(sd,2)	
	sd ~ dunif(0,5)

	
	## priors over thresholds constraint to be ordered
		for(i in 1:3){
	tau0[i]~dunif(tau[1],100)}
	
	tau[2:4] <- sort(tau0) 
	tau[1] <- tauJ*beta
	
	
	
	for(i in 1:3){
		effhi[i] ~dnorm(0,0.001)}
}
     ",fill = TRUE)
sink()
# Bundle data
jags.data <- list(NID=NID,x=data$day/365,y= data$testes_class,id=id,N=N,tauJ= tauJ,HI5=data$HI5)
# Initial values 
inits <- function(){list(eps=runif(NID),tau0 = runif(3,tauJ,1),beta=runif(1,0,0.1),effhi=runif(3,0,0.001),sd=runif(1)) }  


# Parameters monitored
parameters <- c("beta","tau","effhi","alpha")

# MCMC settings
ni <- 50000
nt <- 3
nb <- 30000
nc <- 3

# Call JAGS from R 
mtestes<- jagsUI::jags(jags.data, inits, parameters, "Testes.jags", n.chains = nc, n.thin = nt, n.iter = ni,  n.burnin = nb,parallel=T)




