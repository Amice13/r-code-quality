######## Producing Raw network and timeseries data for plots summary plots

############## Contagion only data #####################
rm(list=ls())
set.seed(43210)

Xval = replicate(50, list(runif(200)))
rawdat = list()  
rawnet = list()
finalYseries = list()

for(a in 1:50){
  x = Xval[[a]]
admat <- matrix(0, nrow=200, ncol=200)
## Probability of tie between i and j
for(i in 1:200){
  for(j in 1:200){
    prob = 1/(1 + exp(3*abs(x[i]-x[j])))
    if(runif(1)<prob) admat[i,j] = 1
    else admat[i,j] = 0
  }
}
diag(admat) <- 0      ## makes the diagonal zero

rawnet[[a]] = admat

### Making the time series variables
### Data Generation model
Y0=c()
Y0 = 0.25*x + rnorm(200, mean=0,sd=0.06)
Y1 = c()
Yiseries = NULL
raw = data.frame(NULL)

time = 50
for(t in 1:time){
  for(i in 1:200){
    ##### Changing t-1 coeficient 
    if(rowSums(admat)[i] != 0) 
    {Y1[i] = 0.25*x[i] + 0.3*Y0[i] + 0.7*mean(Y0[admat[i,]==1]) + rnorm(1, mean=0,sd=1)
      raw = rbind(raw, c(t, Y1[i],x[i],Y0[i],mean(Y0[admat[i,]==1])))}
    else {Y1[i] = 0.25*x[i] + 0.3*Y0[i] + rnorm(1, mean=0,sd=1) ## change coefficient here too
    raw = rbind(raw, c(t, Y1[i],x[i],Y0[i],0))}
  }
  Yiseries = rbind(Yiseries,Y1)
  Y0 = Y1
}
finalYseries[[a]] = Yiseries
rawdat[[a]] = raw
print(a)
}

## Rawdat: Each list element is a unique network's data and each element has names time, Yt, x, Yt-1 
## rawnet: Each element is a unique network 
saveRDS(rawdat, file="../Data/contagion_rawdat.rds")
saveRDS(rawnet, file="../Data/contagion_rawnet.rds")
saveRDS(finalYseries, file="../Data/contagion_yseries.rds")

############# Constant Network Density Homophily only data #################
rm(list=ls())
set.seed(10101)

Xval = replicate(50, list(runif(200)))
rawdat = list()  
rawnet = list()
finalYseries = list()

for(a in 1:50){
  x = Xval[[a]]
  ## Initializaing Latent variable X and time series variable Yi(t=0)
  # Data Generation model producing homophily only data
  
  admat <- matrix(0, nrow=200, ncol=200)
  Y0=c()
  Y0 = 0.25*x + rnorm(200, mean=0,sd=0.06)  ###
  Y1 = c()
  Yiseries = NULL
  raw = data.frame(NULL)
  adjmat.list = list()
  
  for(i in 1:200){
      for(j in 1:200){
        prob = 1/(1 + exp(3*abs(Y0[i]-Y0[j])))
        if(runif(1)<prob) admat[i,j] = 1
        else admat[i,j] = 0
      }
    }
    diag(admat) <- 0      ## makes the diagonal zero
    
    for(t in 1:50){     ## 50 timesteps of data
      admat = network(admat, directed=TRUE)
      set.vertex.attribute(admat,'y',Y0)
      admat <- simulate(admat~edges+absdiff('y'),coef=c(-.5,-.1),constraints =~ edges)
      admat <- as.matrix(admat)
      adjmat.list[[t]] = admat
      Y1 = 0.25*x + 0.3*Y0 + rnorm(200, mean=0,sd=1)
      C = (admat %*% t(t(Y0))) / rowSums(admat)
      raw0 = cbind(rep(t,200),Y1, x, Y0, C)
      raw = rbind(raw, raw0)
      Yiseries = rbind(Yiseries,Y1)
      Y0 = Y1
    }
  finalYseries[[a]] = Yiseries
  rawdat[[a]] = raw
  rawnet[[a]] = adjmat.list
  print(a)
}

saveRDS(rawdat, file="../Data/homophily_rawdat.rds")
saveRDS(rawnet, file="../Data/homophily_rawnet.rds")
saveRDS(finalYseries, file="../Data/homophily_yseries.rds")

############### Constant Network Density Contagion + Homophily data ################
rm(list=ls())
set.seed(13579)

Xval = replicate(50, list(runif(200)))
rawdat = list()  
rawnet = list()
finalYseries = list()

for(a in 1:50){
  x = Xval[[a]]
admat <- matrix(0, nrow=200, ncol=200)
Y0=c()
Y0 = 0.25*x + rnorm(200, mean=0,sd=0.06)
Y1 = c()
Yiseries = NULL
adjmat.list = list()
raw = data.frame(NULL)


  for(i in 1:200){
    for(j in 1:200){
      prob = 1/(1 + exp(3*abs(Y0[i]-Y0[j])))
      if(runif(1)<prob) admat[i,j] = 1
      else admat[i,j] = 0
    }
  }
  diag(admat) <- 0      ## makes the diagonal zero
  
  for(t in 1:50){     ## 50 timesteps of data
    admat = network(admat, directed=TRUE)
    set.vertex.attribute(admat,'y',Y0)
    admat <- simulate(admat~edges+absdiff('y'),coef=c(-.5,-.1),constraints =~ edges)
    admat <- as.matrix(admat)
    adjmat.list[[t]] = admat
  for(i in 1:200){
    if(rowSums(admat)[i] != 0) 
    {Y1[i] = 0.25*x[i] + 0.3*Y0[i] + 0.7*mean(Y0[admat[i,]==1]) + rnorm(1, mean=0,sd=1)
    raw = rbind(raw, c(t, Y1[i],x[i],Y0[i],mean(Y0[admat[i,]==1])))}
    else {Y1[i] = 0.25*x[i] + 0.3*Y0[i] + rnorm(1, mean=0,sd=1) ## change coefficient here too
    raw = rbind(raw, c(t, Y1[i],x[i],Y0[i],0))}
  }
  Yiseries = rbind(Yiseries,Y1)
  Y0 = Y1
}
finalYseries[[a]] = Yiseries
rawdat[[a]] = raw
rawnet[[a]] = adjmat.list
print(a)
}

saveRDS(rawdat, file="../Data/c+h_rawdat.rds")
saveRDS(rawnet, file="../Data/c+h_rawnet.rds")
saveRDS(finalYseries, file="../Data/c+h_yseries.rds")


