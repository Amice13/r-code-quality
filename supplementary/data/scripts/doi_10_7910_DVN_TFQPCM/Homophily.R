### Monte Carlo Simulation of the Shalizi & Thomas test on Homophily only data, over multiple timeseries lengths
### Use ergm's simulate function to maintain network density over time. 

rm(list=ls())
set.seed(10101)

homophilyonly.mc <- function(x){
  admat <- matrix(0, nrow=200, ncol=200)
  
  ## Initializaing Latent variable X and time series variable Yi(t=0)
  # Data Generation model producing homophily only data
  Y0=c()
  Y0 = 0.25*x + rnorm(200, mean=0,sd=0.06)  ###
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
 
   ## The below time series is producing a Y series that is dependent on the previous time step and also producing a different adjacency matrix for each timestep and hence perpetuating homophily and the network density is kept constant by using ERGM's simulate
  
  for(t in 1:50){     ## 50 timesteps of data
    admat = network(admat, directed=TRUE)
    set.vertex.attribute(admat,'y',Y0)
    admat <- simulate(admat~edges+absdiff('y'),coef=c(-.5,-.1),constraints =~ edges)
    admat <- as.matrix(admat)
    adjmat.list[[t]] = admat
    Y1 = 0.25*x + 0.3*Y0 + rnorm(200, mean=0,sd=1)
    Yiseries = rbind(Yiseries,Y1)
    Y0 = Y1
  }
  
  ### Below is running the shalizi thomas test over multiple time series length.
  timeseries = list()
  for(l in 1:24){
    simmodels = NULL
    for(i in 1:10000){
      j = sample(c(1,2), size=200,replace=TRUE) ## produces unequal bins
      Yj1 = Yiseries[,j==1]
      Yj2 = Yiseries[,j==2]
      
      j1mean = apply(Yj1, 1, mean)
      j2mean = apply(Yj2, 1, mean)
      j1mean.t = j1mean[2:(length(j1mean))]   # y2 to 50
      j2mean.t = j2mean[2:(length(j2mean))]   # y2 to 50
      
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
homophilyMC = list()
for(a in 1:50){
  homophilyMC[[a]] = homophilyonly.mc(Xval[[a]])
  print(a)
}


dat.full <- do.call("rbind", homophilyMC)
bootstrap.run = rep(seq(1,50),each=240000)
dat.full = cbind(dat.full, bootstrap.run)


homophilyMCsum = data.frame()
tl = unique(dat.full$Timeseries.Length)
## Below loop summarizes the tests for each unique network.
for(i in 1:50){
  for(t in 1:24){
    runmean = mean(dat.full$CounterpartBin[dat.full$bootstrap.run==i & dat.full$Timeseries.Length==tl[t]])
    runsd = sd(dat.full$CounterpartBin[dat.full$bootstrap.run==i & dat.full$Timeseries.Length==tl[t]])
    # quant2.5 = quantile(dat.full$CounterpartBin[dat.full$bootstrap.run==i & dat.full$Timeseries.Length==tl[t]], prob=0.025)
    # quant97.5 = quantile(dat.full$CounterpartBin[dat.full$bootstrap.run==i & dat.full$Timeseries.Length==tl[t]], prob=0.975)
    homophilyMCsum = rbind(homophilyMCsum, c(runmean,runsd,i,tl[t]))
  }
}

colnames(homophilyMCsum) = c("mean","sd","MCrun",
                             "timeseries.length")
homophilyMCsum$timeseries.length = tl[homophilyMCsum$timeseries.length]


### P-value at every test
pval = c()
tl = unique(dat.full$Timeseries.Length)

for(i in 1:50){
  for(t in 1:24){
    pv = sum(dat.full$CounterpartBin[dat.full$bootstrap.run==i & dat.full$Timeseries.Length==tl[t]] <= 0)/10000
    pval = c(pval, pv)
  }
}

pval = 1 - pval
homophilyMCsum = cbind(homophilyMCsum, pval)

saveRDS(homophilyMCsum, file="../Data/homophilySummary.rds")




