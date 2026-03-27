## Monte Carlo Simulation of the Shalizi & Thomas test on contagion data, over different timeseries lengths. 


rm(list=ls())
set.seed(43210)


shalizithomas.mc <- function(x){
  
  ## Network matrix
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
  
  ### Making the time series variables
  ### Data Generation model
  Y0=c()
  Y0 = 0.25*x + rnorm(200, mean=0,sd=0.06)
  Y1 = c()
  Yiseries = NULL
  
  time = 50
  for(t in 1:time){
    for(i in 1:200){
      ##### Changing t-1 coeficient 
      if(rowSums(admat)[i] != 0) {Y1[i] = 0.25*x[i] + 0.3*Y0[i] + 0.7*mean(Y0[admat[i,]==1]) + rnorm(1, mean=0,sd=1)}
      else Y1[i] = 0.25*x[i] + 0.3*Y0[i] + rnorm(1, mean=0,sd=1)  ## change coefficient here too
    }
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
bootstrap.tsl = list()
for(i in 1:50){
  bootstrap.tsl[[i]] = shalizithomas.mc(Xval[[i]])
  print(i)
}

##############################################################
### Summarizing the test data over each network and timeseries length
bootstrap.tsl.full = do.call("rbind",bootstrap.tsl)
bootstrap.run = rep(seq(1,50),each=240000)
bootstrap.tsl.full = cbind(bootstrap.tsl.full, bootstrap.run)
bootstrap.tsl.full$bootstrap.run = factor(bootstrap.tsl.full$bootstrap.run)
  
ts.mcsim.summary = data.frame(NULL)
tl = unique(bootstrap.tsl.full$Timeseries.Length)
for(i in 1:50){
  for(r in 1:24){
    mydat = subset(bootstrap.tsl.full, (Timeseries.Length == tl[r] &
                                          bootstrap.run == i))
    ts.mcsim.summary = rbind(ts.mcsim.summary,
                             c(i, tl[r],
                               mean(mydat$CounterpartBin),
                               sd(mydat$CounterpartBin), 
                               quantile(bootstrap.tsl.full$CounterpartBin[bootstrap.tsl.full$Timeseries.Length == tl[r]],probs=c(0.025)), 
                               quantile(bootstrap.tsl.full$CounterpartBin[bootstrap.tsl.full$Timeseries.Length == tl[r]], probs=c(0.975))))
   }
   print(i)
 }
 
### Estimating p-value for each test
 pval = c()
 tl = unique(bootstrap.tsl.full$Timeseries.Length)
 for(i in 1:50){
   for(t in 1:24){
     pv = sum(bootstrap.tsl.full$CounterpartBin[bootstrap.tsl.full$bootstrap.run==i & bootstrap.tsl.full$Timeseries.Length==tl[t]] <= 0)/10000
     pval = c(pval, pv)
   }
   
 }
 
colnames(ts.mcsim.summary) <- c("bootstrap.run","timeseries.length","mean","sd")
 
ts.mcsim.summary$timeseries.length = tl[ts.mcsim.summary$timeseries.length]
ts.mcsim.summary$mean = as.numeric(ts.mcsim.summary$mean)
ts.mcsim.summary$sd = as.numeric(ts.mcsim.summary$sd)
ts.mcsim.summary = cbind(ts.mcsim.summary, pval)
 
saveRDS(ts.mcsim.summary, file="../Data/ContagionSummary.rds")
