##### Using ERGM::simulate we are holding the network density constant for the 
##### contagion + homophily data generating process


rm(list=ls())
set.seed(13579)

combinedergm <- function(x){
  
  admat <- matrix(0, nrow=200, ncol=200)
  Y0=c()
  Y0 = 0.25*x + rnorm(200, mean=0,sd=0.06)
  Y1 = c()
  Yiseries = NULL
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
    for(i in 1:200){
      ##### Changing t-1 coeficient 
      if(rowSums(admat)[i] != 0) {Y1[i] = 0.25*x[i] + 0.3*Y0[i] + 0.7*mean(Y0[admat[i,]==1]) + rnorm(1, mean=0,sd=1)}
      else Y1[i] = 0.25*x[i] + 0.3*Y0[i] + rnorm(1, mean=0,sd=1)  ## change coefficient here too
    }
    adjmat.list[[t]] = admat
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
  }
  
  timeseries.full = do.call("rbind", timeseries)
  timeseries.length =  rep(seq(50, 2, length.out=25)[-25],each=10000)
  timeseries.full = cbind(timeseries.full,as.character(timeseries.length))
  colnames(timeseries.full) <- c("Intercept", "SameBin", "CounterpartBin", "Timeseries.Length")
  timeseries.full$Timeseries.Length = factor(timeseries.full$Timeseries.Length, levels=c("50","48","46","44","42","40","38","36","34","32","30","28","26","24","22","20","18","16","14","12","10","8","6","4"))
  
  return(timeseries.full) 
}

Xval = replicate(50, list(runif(200)))
combineddat = list()
for(a in 1:50){
  combineddat[[a]] = combinedergm(Xval[[a]])
  print(a)
}


##############################################################
### Summarizing the test data over each network and timeseries length
combineddat.full = do.call("rbind",combineddat)
bootstrap.run = rep(seq(1,50),each=240000)
combineddat.full = cbind(combineddat.full, bootstrap.run)
combineddat.full$bootstrap.run = factor(combineddat.full$bootstrap.run)

ch.mcsim.summary = data.frame(NULL)
tl = unique(combineddat.full$Timeseries.Length)
for(i in 1:50){
  for(r in 1:24){
    mydat = subset(combineddat.full, (Timeseries.Length == tl[r] &
                                        bootstrap.run == i))
    ch.mcsim.summary = rbind(ch.mcsim.summary,
                             c(i, tl[r],
                               mean(mydat$CounterpartBin),
                               sd(mydat$CounterpartBin)))
	}
  print(i)
}
 
### Estimating p-value for each test
pval = c()
tl = unique(combineddat.full$Timeseries.Length)
for(i in 1:50){
  for(t in 1:24){
    pv = sum(combineddat.full$CounterpartBin[combineddat.full$bootstrap.run==i & combineddat.full$Timeseries.Length==tl[t]] <= 0)/10000
    pval = c(pval, pv)
  }
  print(i)
}

colnames(ch.mcsim.summary) <- c("bootstrap.run","timeseries.length","mean","sd")

ch.mcsim.summary$timeseries.length = tl[ch.mcsim.summary$timeseries.length]
ch.mcsim.summary$mean = as.numeric(ch.mcsim.summary$mean)
ch.mcsim.summary$sd = as.numeric(ch.mcsim.summary$sd)

ch.mcsim.summary = cbind(ch.mcsim.summary, pval)

saveRDS(ch.mcsim.summary, file="../Data/C+HSummary.rds")

