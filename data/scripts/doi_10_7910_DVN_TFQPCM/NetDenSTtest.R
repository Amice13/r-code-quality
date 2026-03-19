#### Testing Effect of network density on contagion signal detection

rm(list=ls())
set.seed(24680)
library("sna")

netdenSTsna <- function(tp,x){
  
  admat = rgraph(n=200, tprob=tp, diag=TRUE, mode="graph")
  country <- network(admat, directed = FALSE)
  net.den <- network.density(country)
  
  ### Making the time series variables
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
    simmodels = NULL
    for(i in 1:10000){
      j = sample(c(1,2), size=200,replace=TRUE) ## produces unequal bins
      Yj1 = Yiseries[,j==1]
      Yj2 = Yiseries[,j==2]
      
      j1mean = apply(Yj1, 1, mean)
      j2mean = apply(Yj2, 1, mean)
      j1mean.t = j1mean[2:(length(j1mean))]   # y49 to y1
      j2mean.t = j2mean[2:(length(j2mean))]   # y49 to y1
      
      j1mean.tm1 = j1mean[1:(length(j1mean)-1)]  ## y50 to y2
      j2mean.tm1 = j2mean[1:(length(j2mean)-1)]
      dat = cbind(c(j1mean.t,j2mean.t),c(j1mean.tm1,j2mean.tm1),c(j2mean.tm1, j1mean.tm1))
      
      regmod <- lm(c(j1mean.t,j2mean.t) ~ c(j1mean.tm1,j2mean.tm1) + 
                     c(j2mean.tm1, j1mean.tm1))
      simmodels <- rbind(simmodels, coefficients(regmod))
    }

  colnames(simmodels) <- c("Intercept", "SameBin", "CounterpartBin")
  return(simmodels)
}

netdensity = seq(0.2,0.8,length.out = 7)
Xval = replicate(50, list(runif(200)))
netden005 = list()
for(a in 1:50){
  netden005[[a]] = netdenSTsna(0.05, Xval[[a]])
  print(a)
}
netden2 = list()
for(a in 1:50){
  netden2[[a]] = netdenSTsna(netdensity[1],Xval[[a]]) 
  print(a)
}
netden3 = list()
for(a in 1:50){
  netden3[[a]] = netdenSTsna(netdensity[2],Xval[[a]])
  print(a)
}
netden4 = list()
for(a in 1:50){
  netden4[[a]] = netdenSTsna(netdensity[3],Xval[[a]])
  print(a)
}
netden5 = list()
for(a in 1:50){
  netden5[[a]] = netdenSTsna(netdensity[4],Xval[[a]])
  print(a)
}
netden6 = list()
for(a in 1:50){
  netden6[[a]] = netdenSTsna(netdensity[5],Xval[[a]])
  print(a)
}
netden7 = list()
for(a in 1:50){
  netden7[[a]] = netdenSTsna(netdensity[6],Xval[[a]])
  print(a)
}
netden8 = list()
for(a in 1:50){
  netden8[[a]] = netdenSTsna(netdensity[7],Xval[[a]])
  print(a)
}

netden005 = do.call("rbind",netden005)
net.den = rep(0.05,500000)
net.no = rep(seq(1:50),each=10000)
netden005 = cbind(netden005, net.no, net.den)

netden2 = do.call("rbind",netden2)
net.den = rep(0.2,500000)
netden2 = cbind(netden2, net.no,net.den)

netden3 = do.call("rbind", netden3)
net.den = rep(0.3, 500000)
netden3 = cbind(netden3, net.no, net.den)

netden4 = do.call("rbind", netden4)
net.den = rep(0.4,500000)
netden4 = cbind(netden4, net.no,net.den)

netden5 = do.call("rbind", netden5)
net.den = rep(0.5, 500000)
netden5 = cbind(netden5, net.no, net.den)

netden6 = do.call("rbind", netden6)
net.den = rep(0.6, 500000)
netden6 = cbind(netden6, net.no, net.den)

netden7 = do.call("rbind", netden7)
net.den = rep(0.7, 500000)
netden7 = cbind(netden7, net.no, net.den)

netden8 = do.call("rbind", netden8)
net.den = rep(0.8, 500000)
netden8 = cbind(netden8, net.no, net.den)

allnets = rbind(netden2,netden3,netden4,netden5,netden6,netden7,netden8)
allnets = as.data.frame(allnets)
colnames(allnets) = c("Intercept", "SameBin", "CounterpartBin", 
                      "net.no", "net.den")

# saveRDS(allnets, file="../Data/allnets.rds")  ## all tests are timeseries length of 50

# allnets = readRDS("../Data/allnets.rds")

allnet.summary = data.frame(NULL)
nd = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
for(d in 1:8){
  for(n in 1:50){
    netdat = subset(allnets, (net.den==nd[d]&net.no == n))
    allnet.summary = rbind(allnet.summary,
                           c(mean(netdat$CounterpartBin),
                             sd(netdat$CounterpartBin),
                             n, d))
  }
  print(d)
}
colnames(allnet.summary) = c("mean", "sd", "net.no", "net.den")
allnet.summary$net.den = nd[allnet.summary$net.den]
allnet.summary$net.den = as.factor(allnet.summary$net.den)

saveRDS(allnet.summary, file="../Data/allnetSummary.rds")


