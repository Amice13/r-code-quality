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

data <- read.csv2("Vespertilio_murinus_food_sexmaturation_data.csv",sep=";",header=T)



data <-data[!is.na(data$testes_class),]
data <-data[!is.na(data$BAT_ID),]
data$day <- data$day+1
####################
### create data 
####################
#Nb of individuals
NID <- length(levels(as.factor(data$BAT_ID)))


#Nb of data
N <- nrow(data)
#variable of bat identity
id <- as.numeric(as.factor(data$BAT_ID))


data$REGIME <- as.numeric(as.factor(data$REGIME))-1
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
        
		mu[i] <-x[i]*(beta+eps2[id[i]])+eps[id[i]]
	
	      ## cumulative logistic probabilities
	      logit(Q[i,1]) <- tau[1]-mu[i]
	      p[i,1] <- Q[i,1]
	      
	      for(j in 2:4){
                logit(Q[i,j]) <- (tau[j]+(eff[j-1]*treat[i]))-mu[i]
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
	      eps2[k] ~ dnorm(0,eta2)  }  
	eta <- 1/pow(sd,2)	
	sd ~ dunif(0,5)
  	eta2 <- 1/pow(sd2,2)	
	sd2 ~ dunif(0,5)
	
	## priors over thresholds constraint to be ordered
		for(i in 1:3){
	tau0[i]~dunif(tau[1],100)}
	
	tau[2:4] <- sort(tau0) 
	tau[1] <- tauJ*beta
	
	
	
	for(i in 1:3){
		eff[i] ~dnorm(0,0.001)}
}
     ",fill = TRUE)
sink()
# Bundle data
jags.data <- list(NID=NID,x=data$day/365,y= data$testes_class,id=id,N=N,tauJ= tauJ,treat=data$REGIME)
# Initial values 
inits <- function(){list(eps=runif(NID),eps2=runif(NID),tau0 = runif(3,tauJ,1),beta=runif(1,0,0.1),eff=runif(3,0,0.1),sd=runif(1),sd2=runif(1)) }  


# Parameters monitored
parameters <- c("beta","tau",  "eps","eps2","eff")

# MCMC settings
ni <- 5000
nt <- 3
nb <- 2000
nc <- 3

# Call JAGS from R 
mtestes<- jagsUI::jags(jags.data, inits, parameters, "Testes.jags", n.chains = nc, n.thin = nt, n.iter = ni,  n.burnin = nb,parallel=T)

mtestesres <- mtestes$summary
#slopes and threshold
beta <- mtestesres[rownames(mtestesres)=="beta",1]
tau <- c(mtestesres["tau[1]",1], mtestesres["tau[2]",1], mtestesres["tau[3]",1], mtestesres["tau[4]",1])

tauq <- mtestes$sims.list$tau
betaq <- mtestes$sims.list$beta
tauJq <- tauq*365/betaq
effq <- mtestes$sims.list$eff
effJq <- effq*365/betaq


taul1 <- c(quantile(tauJq[,1],0.025),
quantile(tauJq[,2]+ effJq[,1],0.025),
quantile(tauJq[,3]+ effJq[,2],0.025),
quantile(tauJq[,4]+ effJq[,3],0.025))

taul0 <- c(quantile(tauJq[,1],0.025),
quantile(tauJq[,2],0.025),
quantile(tauJq[,3],0.025),
quantile(tauJq[,4],0.025))

tauh1 <- c(quantile(tauJq[,1],0.975),
quantile(tauJq[,2]+effJq[,1],0.975),
quantile(tauJq[,3]+effJq[,2],0.975),
quantile(tauJq[,4]+effJq[,3],0.975))

tauh0 <- c(quantile(tauJq[,1],0.975),
quantile(tauJq[,2],0.975),
quantile(tauJq[,3],0.975),
quantile(tauJq[,4],0.975))

effh <- c(quantile(effJq[,1],0.975),
quantile(effJq[,2],0.975),
quantile(effJq[,3],0.975))

effl <- c(quantile(effJq[,1], 0.025),
quantile(effJq[,2], 0.025),
quantile(effJq[,3], 0.025))

eff		 <- colMeans(effJq)

####Graph

plot(1,ylim=c(0,120),type="n",ylab="Days", xlab="Days difference",xlim=c(-5,100),frame=F,main="T10-T27")

abline(v=0,col="dimgrey")
segments(
c(0,effl),
c(colMeans(tauJq)),
c(0,effh),
c(colMeans(tauJq)),lwd=3)
points(c(0,eff),c(colMeans(tauJq)))

eps <- mtestesres[6:37,1]
eps2 <- mtestesres[38:69,1]
predicts <- array(NA, dim=c(32,100))
for(i in 1:100){
predicts[,i] <- i/365*(beta+eps2)+eps}

predicts0 <- predicts[tapply(data$REGIME,id,mean)==0,]
predicts1 <- predicts[tapply(data$REGIME,id,mean)==1,]


predicts0 <- 
ifelse(predicts0 <tau[1],1,
ifelse(predicts0 <tau[2],2,
ifelse(predicts0 <tau[3],3,
ifelse(predicts0 <tau[4],4,5))))
predicts1 <- 
ifelse(predicts1 <tau[1],1,
ifelse(predicts1 <tau[2]+colMeans(effq)[1],2,
ifelse(predicts1 <tau[3]+colMeans(effq)[2],3,
ifelse(predicts1 <tau[4]+colMeans(effq)[3],4,5))))

predicts[tapply(data$REGIME,id,mean)==1,] <- predicts1
predicts[tapply(data$REGIME,id,mean)==0,] <- predicts0


realdata <- array(NA, dim=c(32,100))


for(i in 1:length(id)){
realdata[id[i],data$day[i]] <- 
data$testes_class[i]}

(table(realdata, predicts)[2,2]+
table(realdata, predicts)[3,3]+
table(realdata, predicts)[4,4]+
table(realdata, predicts)[5,5])/sum(table(realdata, predicts)[,-1])


sexmat <- function(x,beta) beta*x

eps <- mtestesres[6:37,1]
predicts <- array(rep(sexmat((1:100)/365,beta),each=32),dim=c(32,100))+eps

predicts0 <- predicts[tapply(data$REGIME,id,mean)==0,]
predicts1 <- predicts[tapply(data$REGIME,id,mean)==1,]

predicts0 <- 
ifelse(predicts0 <tau[1],1,
ifelse(predicts0 <tau[2],2,
ifelse(predicts0 <tau[3],3,
ifelse(predicts0 <tau[4],4,5))))
predicts1 <- 
ifelse(predicts1 <tau[1],1,
ifelse(predicts1 <tau[2]+colMeans(effq)[1],2,
ifelse(predicts1 <tau[3]+colMeans(effq)[2],3,
ifelse(predicts1 <tau[4]+colMeans(effq)[3],4,5))))

predicts[tapply(data$REGIME,id,mean)==1,] <- predicts1
predicts[tapply(data$REGIME,id,mean)==0,] <- predicts0

realdata <- array(NA, dim=c(32,100))

for(i in 1:length(id)){
realdata[id[i],data$day[i]] <- 
data$testes_class[i]}

(table(realdata, predicts)[1,1]+
table(realdata, predicts)[2,2]+
table(realdata, predicts)[3,3]+
table(realdata, predicts)[4,4]+
table(realdata, predicts)[5,5])/sum(table(realdata, predicts))


