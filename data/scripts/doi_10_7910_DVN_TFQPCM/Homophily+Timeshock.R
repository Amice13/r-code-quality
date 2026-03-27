### Shalizi Thomas test on contagion data that also incorporates "contemporaneous individual causation" via a common temporal error at each time step
### We do not use ERGM to maintain the network density over time in this case. 

rm(list=ls())
set.seed(23456)

timeshock <- function(x){

## Network matrix
admat <- matrix(0, nrow=200, ncol=200)

# Data Generation model producing homophily only data
Y0=c()
Y0 = 0.25*x + rnorm(200, mean=0,sd=0.06)
Y1 = c()
Yiseries = NULL
adjmat.list = list()

for(t in 1:50){     ## 50 timesteps of data
  for(i in 1:200){
    for(j in 1:200){
      prob = 1/(1 + exp(3*abs(Y0[i]-Y0[j])))
      if(runif(1)<prob) admat[i,j] = 1
      else admat[i,j] = 0
    }
  }
   diag(admat) <- 0      ## makes the diagonal zero
  adjmat.list[[t]] = admat
  Y1 = 0.25*x + 0.3*Y0 + rnorm(200, mean=0,sd=1)
  Y1 = Y1 + rnorm(1, mean=0, sd=1) ## Temporal shock each year  
  Yiseries = rbind(Yiseries,Y1)
  Y0 = Y1
}

############## 
### Below is running the shalizi thomas over multiple time series length. 
timeseries = list()
for(l in 1:24){
  simmodels = NULL
  for(i in 1:10000){
    j = sample(c(1,2), size=200,replace=TRUE) ## produces unequal bins
    Yj1 = Yiseries[,j==1]
    Yj2 = Yiseries[,j==2]
    
    j1mean = apply(Yj1, 1, mean)
    j2mean = apply(Yj2, 1, mean)
    j1mean.t = j1mean[2:(length(j1mean))]   # y2 to y50
    j2mean.t = j2mean[2:(length(j2mean))]   # y2 to y50
    
    j1mean.tm1 = j1mean[1:(length(j1mean)-1)]  ## y1 to y49
    j2mean.tm1 = j2mean[1:(length(j2mean)-1)]
    
    regmod <- lm(c(j1mean.t,j2mean.t) ~ c(j1mean.tm1,j2mean.tm1) + 
                   c(j2mean.tm1, j1mean.tm1))
    simmodels <- rbind(simmodels, coefficients(regmod))
  }
  timeseries[[l]] = as.data.frame(simmodels)
  Yiseries = Yiseries[-c(1:2),]
  #d = dim(Yiseries)
  #print(d)
}

timeseries.full = do.call("rbind", timeseries)
timeseries.length =  rep(seq(50, 2, length.out=25)[-25],each=10000)
timeseries.full = cbind(timeseries.full,as.character(timeseries.length))
colnames(timeseries.full) <- c("Intercept", "SameBin", "CounterpartBin", "Timeseries.Length")
timeseries.full$Timeseries.Length = factor(timeseries.full$Timeseries.Length, levels=c("50","48","46","44","42","40","38","36","34","32","30","28","26","24","22","20","18","16","14","12","10","8","6","4"))

return(timeseries.full) 
}


Xval = replicate(50, list(runif(200)))
timeshockMC = list()
for(a in 1:50){
  timeshockMC[[a]] = timeshock(Xval[[a]])
  print(a)
}


dat.full <- do.call("rbind", timeshockMC)
bootstrap.run = rep(seq(1,50),each=240000)
dat.full = cbind(dat.full, bootstrap.run)


timeshockMCsum = data.frame()
tl = unique(dat.full$Timeseries.Length)
## Below loop summarizes the tests for each unique network.
for(i in 1:50){
  for(t in 1:24){ 
    runmean = mean(dat.full$CounterpartBin[dat.full$bootstrap.run==i & dat.full$Timeseries.Length==tl[t]])
    runsd = sd(dat.full$CounterpartBin[dat.full$bootstrap.run==i & dat.full$Timeseries.Length==tl[t]])
    timeshockMCsum = rbind(timeshockMCsum, c(runmean,runsd,i,tl[t]))
  }
  print(i)
}

colnames(timeshockMCsum) = c("mean","sd","MCrun",
                             "timeseries.length")
timeshockMCsum$timeseries.length = tl[timeshockMCsum$timeseries.length]


### P-value at every test 
pval = c()
tl = unique(dat.full$Timeseries.Length)

for(i in 1:50){
  for(t in 1:24){
    pv = sum(dat.full$CounterpartBin[dat.full$bootstrap.run==i & dat.full$Timeseries.Length==tl[t]] <= 0)/10000
    pval = c(pval, pv)
  }
  print(i)
}

pval = 1 - pval
timeshockMCsum = cbind(timeshockMCsum, pval)

saveRDS(timeshockMCsum, file="../Data/H+TSummary.rds")
