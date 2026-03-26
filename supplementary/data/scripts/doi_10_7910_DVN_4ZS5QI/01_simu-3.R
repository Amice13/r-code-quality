
# This code simulates the model in two cases:
# 1. Baseline model
# 2. Extension with inflow and outflow modeled separately


### PARAMETERS

#rho in the contract return policy
rho <- gamma*(1/3)*0.4 # see appendix A8

#Contract return policy 
y0 <- (1-phi)*r
a1 <- (1-phi)*(alpha+rho)/(alpha+(1+r)/r*rho)
temp <- (alpha+beta*theta)/(1+r-theta) * (2*alpha*(1+r)+beta*theta*r) - alpha*(alpha+beta*theta) - beta^2*theta*(1-theta) + (alpha+beta*theta)*rho
a2 <- (temp*(1-theta)*(1-phi)*r) / (temp*(1-theta)*r+alpha*rho)


### SIMULATE MODEL

# Set seed for reproducibility
set.seed(42)


### Asset returns
x <- matrix(NA, Nsimu, T1)
#Returns are only defined for t>=1, that is, starting in the second column
#Simulate contract returns
x[,2:(T+1)] <- matrix(r+exp(-0.5*sigx^2+sigx*rnorm(Nsimu*T,0,1))-1, Nsimu, T)


### Baseline model

#Contract returns
y1 <- matrix(NA, Nsimu, T1)
#Account value
V1 <- matrix(NA, Nsimu, T1)
#Beginning-of-period reserves
R1 <- matrix(NA, Nsimu, T1)


### Extension: model with inflow and outflow modeled separately

#Contract returns
y2 <- matrix(NA, Nsimu, T1)
#Flow and account value
Inflow2 <- matrix(NA, Nsimu, T1)
Outflow2 <- matrix(NA, Nsimu, T1)
V2 <- matrix(NA, Nsimu, T1)
#Beginning-of-period reserves
R2 <- matrix(NA, Nsimu, T1)


### Set initial values

#First column corresponds to t=0 in the model

#Set initial account value
V1[,1] <- sj
V2[,1] <- sj/(1-theta)

#Set initial reserves to zero
R1[,1] <- 0
R2[,1] <- 0



### Iterate over time periods

for (t in 2:(T1+1)) {
  print(paste("Model simulation step ",t-1,"/",T1,sep=""))
  if (t>2) {
    V1[,t-1] <- (sj + alpha*sj*(1-phi)*r*R1[,t-1]/V1[,t-2])
    Inflow2[,t-1] <- sj + alpha*sj*(1-phi)*r*R2[,t-1]/V2[,t-2]
    Outflow2[,t-1] <- 1-theta - beta*theta*(1-theta)*(1-phi)*r*R2[,t-1]/V2[,t-2]
    V2[,t-1] <- Inflow2[,t-1] + (1-Outflow2[,t-1])*V2[,t-2]
  }
  if (t<T1+1) {
    y1[,t] <- y0 + (1-phi)*r*R1[,t-1]/V1[,t-1] + a1*(x[,t]-r)
    y2[,t] <- y0 + (1-phi)*r*R2[,t-1]/V2[,t-1] + a2*(x[,t]-r)
    R1[,t] <- (1+x[,t])*R1[,t-1] + (x[,t]-y1[,t]/(1-phi))*V1[,t-1]
    R2[,t] <- (1+x[,t])*R2[,t-1] + (x[,t]-y2[,t]/(1-phi))*V2[,t-1]
  }
}
