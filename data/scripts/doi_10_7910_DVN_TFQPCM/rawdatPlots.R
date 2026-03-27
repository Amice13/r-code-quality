########## Plots from Raw data, Raw networks and RAw Yiseries
rm(list=ls())

######################################################
####### Plotting NETWORK DENSITY comparison plot  ###################################################### 
cnet = readRDS(file="../Data/contagion_rawnet.rds")
hnet = readRDS(file="../Data/homophily_rawnet.rds")
chnet = readRDS(file="../Data/c+h_rawnet.rds")

cnetden = c()
for(a in 1:50){
  netden = network.density(network(cnet[[a]],directed=T))
  cnetden = c(cnetden, netden)
  print(a)
}

hnetden = c()
for(a in 1:50){
  for(b in 1:50){
    netden = network.density(network(hnet[[a]][[b]],directed = T))
    hnetden = c(hnetden, netden)
  }
  print(a)
}

chnetden = c()
for(a in 1:50){
  for(b in 1:50){
    netden = network.density(network(chnet[[a]][[b]],directed = T))
    chnetden = c(chnetden, netden)
  }
  print(a)
}

cnetden = c(cnetden,rep(NA, 2450))
netden = c(cnetden, hnetden, chnetden)
dgp = rep(c("Contagion", "Homophily", "Contagion+Homophily"), each=2500)
fullnetden = data.frame(cbind(netden, dgp))
fullnetden$dgp = as.factor(fullnetden$dgp)
fullnetden$netden = as.numeric(fullnetden$netden)

ggplot(fullnetden, aes(dgp,netden)) +
  geom_boxplot(size=0.6, outlier.size=0.4) + 
  theme_light() +
  ylab("Network Density") + 
  xlab("Data Generating Process") + 
  ggtitle("Network Density Summary")

ggsave("../Plots/Appendix-Fig 1a.png")

###############################################
########### Plotting Network Tie Reciprocity 
###############################################
cnet = readRDS(file="../Data/contagion_rawnet.rds")
hnet = readRDS(file="../Data/homophily_rawnet.rds")
chnet = readRDS(file="../Data/c+h_rawnet.rds")

cnetreci = c()
for(a in 1:50){
  netreci = grecip(network(cnet[[a]],directed=T))
  cnetreci = c(cnetreci, netreci)
  print(a)
}

hnetreci = c()
for(a in 1:50){
  for(b in 1:50){
    netreci = grecip(network(hnet[[a]][[b]],directed = T))
    hnetreci = c(hnetreci, netreci)
  }
  print(a)
}

chnetreci = c()
for(a in 1:50){
  for(b in 1:50){
    netreci = grecip(network(chnet[[a]][[b]],directed = T))
    chnetreci = c(chnetreci, netreci)
  }
  print(a)
}

cnetreci = c(cnetreci,rep(NA, 2450))
netreci = c(cnetreci, hnetreci, chnetreci)
dgp = rep(c("Contagion", "Homophily", "Contagion+Homophily"), each=2500)
fullnetreci = data.frame(cbind(netreci, dgp))
fullnetreci$dgp = as.factor(fullnetreci$dgp)
fullnetreci$netreci = as.numeric(fullnetreci$netreci)

ggplot(fullnetreci, aes(dgp,netreci)) +
  geom_boxplot(size=0.6, outlier.size=0.4) + 
  theme_light() +
  ylab("Tie Reciprocity") + 
  xlab("Data Generating Process") 

ggsave("../Plots/Appendix-Fig 1b.png")

############################################################
### Running ACF plot for different data generating processes ###########################################################
cy = readRDS(file="../Data/contagion_yseries.rds")
hy = readRDS(file="../Data/homophily_yseries.rds") 
chy = readRDS(file="../Data/c+h_yseries.rds")

############## Running ACF plot for a random unique network data out of the 50

png(file="../Plots/Appendix-Fig 1c.png", width=480, height=610)

par(mfrow=c(3,1))

cdat = cy[[10]]
Contagion = c(cdat)
country = rep(c(1:200),each=50)
year = rep(c(1:50),200)
cdat = data.frame(cbind(Contagion, country, year))
psacf(cdat, Contagion ~ country, ~year, lag.max = 50)

hdat = hy[[14]]
Homophily = c(hdat)
country = rep(c(1:200),each=50)
year = rep(c(1:50),200)
hdat = data.frame(cbind(Homophily, country, year))
psacf(hdat, Homophily ~ country, ~year, lag.max = 50)

chdat = chy[[33]]
CplusH = c(chdat)
country = rep(c(1:200),each=50)
year = rep(c(1:50),200)
chdat = data.frame(cbind(CplusH, country, year))
psacf(chdat, CplusH ~ country, ~year, lag.max = 50)

dev.off()

######################################################
##### Running OLS on rawdat of different data generating processes
######################################################
c.rawdat = readRDS(file="../Data/contagion_rawdat.rds")
h.rawdat = readRDS(file="../Data/homophily_rawdat.rds")
ch.rawdat = readRDS(file="../Data/c+h_rawdat.rds")

### Running ordinary OLS on contagion rawdat at different timelengths
timelength = seq(from=4,  to=50, by=2)
c.olsdata = data.frame(NULL)
for(l in 1:50){
  workdat = c.rawdat[[l]]
  colnames(workdat) = c("tl", "y1", "x", "y0", "contagion")
  for(i in 1:24){
    workdat.sub = subset(workdat, tl <= timelength[i])
    olsmod = lm(workdat.sub$y1 ~ 
                  workdat.sub$x + workdat.sub$y0 + workdat.sub$contagion)
    c.olsdata = rbind(c.olsdata, c(i,l,coefficients(olsmod),summary(olsmod)$coefficients[16]))
  }
  print(l)
}

colnames(c.olsdata) = c("tl","network","Intercept", "x", "Ytm1", "C","pval")
c.olsdata$tl = timelength[c.olsdata$tl]

c.pvals005 = c()
c.pvals01 = c()
for(i in 1:24){
  tl.data = subset(c.olsdata, tl == timelength[i])
  pval005 = (sum(tl.data$C > 0 & tl.data$pval < 0.1))/50 
  pval01 = (sum(tl.data$C > 0 & tl.data$pval < 0.2))/50 
  c.pvals005 = c(c.pvals005,pval005)
  c.pvals01 = c(c.pvals01, pval01)
  print(i)
}

################ OLS on  homophily data ################
timelength = seq(from=4,  to=50, by=2)
h.olsdata = data.frame(NULL)
for(l in 1:50){
  workdat = h.rawdat[[l]]
  colnames(workdat) = c("tl", "y1", "x", "y0", "contagion")
  workdat = as.data.frame(workdat)
  for(i in 1:24){
    workdat.sub = subset(workdat, tl <= timelength[i])
    olsmod = lm(workdat.sub$y1 ~ 
                  workdat.sub$x + workdat.sub$y0 + workdat.sub$contagion)
    h.olsdata = rbind(h.olsdata, c(i,l,coefficients(olsmod),summary(olsmod)$coefficients[16]))
  }
  print(l)
}

colnames(h.olsdata) = c("tl","network","Intercept", "x", "Ytm1", "C","pval")
h.olsdata$tl = timelength[h.olsdata$tl]

h.pvals005 = c()
h.pvals01 = c()
for(i in 1:24){
  tl.data = subset(h.olsdata, tl == timelength[i])
  pval005 = (sum(tl.data$C < 0 & tl.data$pval < 0.1))/50 
  pval01 = (sum(tl.data$C < 0 & tl.data$pval < 0.2))/50 
  h.pvals005 = c(h.pvals005,pval005)
  h.pvals01 = c(h.pvals01, pval01)
  print(i)
}

################ OLS on  contagion + homophily data 
timelength = seq(from=4,  to=50, by=2)
ch.olsdata = data.frame(NULL)
for(l in 1:50){
  workdat = ch.rawdat[[l]]
  colnames(workdat) = c("tl", "y1", "x", "y0", "contagion")
  for(i in 1:24){
    workdat.sub = subset(workdat, tl <= timelength[i])
    olsmod = lm(workdat.sub$y1 ~ 
                  workdat.sub$x + workdat.sub$y0 + workdat.sub$contagion)
    ch.olsdata = rbind(ch.olsdata, c(i,l,coefficients(olsmod),summary(olsmod)$coefficients[16]))
  }
  print(l)
}

colnames(ch.olsdata) = c("tl","network","Intercept", "x", "Ytm1", "C","pval")
ch.olsdata$tl = timelength[ch.olsdata$tl]

ch.pvals005 = c()
ch.pvals01 = c()
for(i in 1:24){
  tl.data = subset(ch.olsdata, tl == timelength[i])
  pval005 = (sum(tl.data$C > 0 & tl.data$pval < 0.1))/50 
  pval01 = (sum(tl.data$C > 0 & tl.data$pval < 0.2))/50 
  ch.pvals005 = c(ch.pvals005,pval005)
  ch.pvals01 = c(ch.pvals01, pval01)
  print(i)
}

fullpvals.ols = cbind(timelength, c.pvals005, c.pvals01, h.pvals005, h.pvals01,
                      ch.pvals005, ch.pvals01)
fullpvals.ols = as.data.frame(fullpvals.ols)
names(fullpvals.ols) = c("timeseries.length", "con05", "con01",
                         "hom05", "hom01","ch05", "ch01")

ggplot(fullpvals.ols) +
  geom_path(aes(timeseries.length, hom05,linetype="0.05")) +
  geom_point(aes(timeseries.length, hom05,shape="Homophily"),size=2) +
  geom_path(aes(timeseries.length, hom01,linetype="0.1")) +
  geom_point(aes(timeseries.length, hom01,shape="Homophily"),size=2) +
  geom_path(aes(timeseries.length, con05,linetype="0.05")) +
  geom_point(aes(timeseries.length, con05,shape="Contagion"),size=2) +
  geom_path(aes(timeseries.length, con01,linetype="0.1")) +
  geom_point(aes(timeseries.length, con01,shape="Contagion"),size=2) +
  geom_path(aes(timeseries.length, ch05,linetype="0.05")) +
  geom_point(aes(timeseries.length, ch05,shape="Contagion + Homophily"),size=2) +
  geom_path(aes(timeseries.length, ch01,linetype="0.1")) +
  geom_point(aes(timeseries.length, ch01,shape="Contagion + Homophily"),size=2) +
  scale_linetype_manual("Type 1 error",values=c("0.1"=2,"0.05"=1)) +
  # scale_x_continuous(breaks = seq(50,4,by=-2)) +
  scale_x_reverse() +
  scale_shape_manual(name="Data type", values=c("Contagion"=16,"Homophily"=4,"Contagion + Homophily"=17)) +
  ggtitle("Proportion of OLS models with Type 1 error of 0.1 and 0.05")+
  xlab("Timeseries length") +
  ylab("Proportion of tests") +
  theme_light() 

ggsave("../Plots/Appendix-Fig 2a.png")
