#Prediction Plots Code for Conflict Environment Scores
#Replication of Figure 5 & 6 for "Conflict Environments and Civil War" (Reid et al. 2020, JOGSS)
#Code adapted from Ward, Greenhill, & Bakke's (2010, JPR) on prediction & civil conflict

# Prep the Data -----------------------------------------------------------

#setwd("")

library(readstata13)
data = read.dta13("RMKC Rep Data.dta")

#Base Model

data.subset = subset(data, select=c(acdonset, ccode, cces_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))
data.subset = na.omit(data.subset)

basemodel = glm(as.factor(acdonset) ~ 
                     lnrgdpchl + lnpopl + ethnichet + polityl + politysql + t + t2 + t3, 
                 na.action = na.exclude, family = binomial(link = probit),
                 data = data.subset)
summary(basemodel)

#CE Model
cemodel = glm(as.factor(acdonset) ~ 
                   cces_acd + lnrgdpchl + lnpopl + ethnichet + polityl + politysql + t + t2 + t3, 
               na.action = na.exclude, family = binomial(link = probit),
               data = data.subset)
summary(cemodel)

# Prediction Plots for Base Model ----------------------------------------------------

library(Hmisc)

#Calculate the AUC for the CE model
ce.AUC.main=somers2(plogis(predict(cemodel)),cemodel$y)[[1]]

# Calculate AUC while deleting each variable:
ce.IVs=subset(data, select=c(cces_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))
ce.AUCs=rep(NA,9)
names(ce.AUCs)=names(ce.IVs)
for (i in 1:9){
    IVs.current=ce.IVs[,-i]
    IVs.current=as.matrix(IVs.current)
    model.current=glm(as.factor(data$acdonset)~IVs.current, family = binomial(link = probit))
    ce.AUCs[i]=somers2(plogis(predict(model.current)),model.current$y)[[1]]
}

# Create Stata clustered standard errors for consistency with tables
library(clubSandwich)
c = coef_test(cemodel, vcov = "CR1S", cluster = data.subset$ccode)[2:10, ]

# Calculate statistical significance
ce.zs= abs(c$beta)/c$SE
ce.names=c("CE Score","GDP","Population","Ethnic Het","Polity", "Polity^2","T","T^2","T^3")

ce.ppower=ce.AUC.main-ce.AUCs
plot(ce.zs,ce.ppower,type="n",bty="n",xlim=c(0,6),ylim=c(-0.02,0.04),ylab="Change in Predictive Power",xlab="Statistical Significance", main="Predictive vs. Statistical Significance")
points(ce.zs,ce.ppower,pch=16)

#Add 0 line
#segments(x0=0,x1=6,y0=0,y1=0,col="gray", lwd=2, lty = 2)

# Add Text
h.adj=c(rep(0.003, 4), -0.35, rep(0.003, 4))
v.adj=c(rep(0.002, 4), -0.002, 0.002, -0.002, -0.002, -0.002)
text(ce.zs+h.adj,ce.ppower+v.adj,ce.names,adj=0)

# Prediction Plots for Other Neighborhood Measures ------------------------

data = read.dta13("RMKC Rep Data.dta")

#Just Spatial Lag
data.subset = subset(data, select=c(ccode, acdonset, slc_acd, lnrgdpchl, lnpopl, ethnichet, polityl, politysql, t, t2, t3))
data.subset = na.omit(data.subset)

slcmodel = glm(as.factor(acdonset) ~ 
                    slc_acd + lnrgdpchl + lnpopl + ethnichet + polityl + politysql + t + t2 + t3, 
                family = binomial(link = probit),
                data = data.subset)

slc.AUC.main=somers2(plogis(predict(slcmodel)),slcmodel$y)[1]
slc.IVs=subset(data, select=c(slc_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))
slc.AUCs=rep(NA,9)
names(slc.AUCs)=names(slc.IVs)

for (i in 1:9){
    IVs.current=slc.IVs[,-i]
    IVs.current=as.matrix(IVs.current)
    model.current=glm(as.factor(data$acdonset)~IVs.current, family = binomial(link = probit))
    slc.AUCs[i]=somers2(plogis(predict(model.current)),model.current$y)[1]
}

slc = coef_test(slcmodel, vcov = "CR1S", cluster = data.subset$ccode)[2:10, ]
slc.z= abs(slc$beta)/slc$SE
slc.name=c("SLC")
slc.ppower=slc.AUC.main-slc.AUCs

# Spatial Lag of Conflict Onset
data.subset = subset(data, select=c(ccode, acdonset, slco_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))
data.subset = na.omit(data.subset)

slcomodel = glm(as.factor(acdonset) ~ 
                     slco_acd + lnrgdpchl + lnpopl + ethnichet + polityl + politysql + t + t2 + t3, 
                 family = binomial(link = probit),
                 data = data.subset)

slco.AUC.main=somers2(plogis(predict(slcomodel)),slcomodel$y)[1]

slco.IVs=subset(data, select=c(slco_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))

slco.AUCs=rep(NA,9)
names(slco.AUCs)=names(slco.IVs)

for (i in 1:9){
    IVs.current=slco.IVs[,-i]
    IVs.current=as.matrix(IVs.current)
    model.current=glm(as.factor(data$acdonset)~IVs.current, family = binomial(link = probit))
    slco.AUCs[i]=somers2(plogis(predict(model.current)),model.current$y)[1]
}

slco = coef_test(slcomodel, vcov = "CR1S", cluster = data.subset$ccode)[2:10, ]
slco.z= abs(slco$beta)/slco$SE
slco.name=c("slco")
slco.ppower=slco.AUC.main-slco.AUCs

#Neighboring Civil War (NCIVWAR)
data.subset = subset(data, select=c(ccode, acdonset, ncivwar_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))
data.subset = na.omit(data.subset)

ncivmodel = glm(as.factor(acdonset) ~ 
                     ncivwar_acd + lnrgdpchl + lnpopl + ethnichet + politysql + t + t2 + t3, 
                 family = binomial(link = probit),
                 data = data.subset)

nciv.AUC.main=somers2(plogis(predict(ncivmodel)),ncivmodel$y)[1]

nciv.IVs=subset(data, select=c(ncivwar_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))

nciv.AUCs=rep(NA,9)
names(nciv.AUCs)=names(nciv.IVs)

for (i in 1:9){
    IVs.current=nciv.IVs[,-i]
    IVs.current=as.matrix(IVs.current)
    model.current=glm(as.factor(data$acdonset)~IVs.current, family = binomial(link = probit))
    nciv.AUCs[i]=somers2(plogis(predict(model.current)),model.current$y)[1]
}


nciv = coef_test(ncivmodel, vcov = "CR1S", cluster = data.subset$ccode)[2:10, ]
nciv.z= abs(nciv$beta)/nciv$SE
nciv.name=c("nciv")
nciv.ppower=nciv.AUC.main-nciv.AUCs

# Regional Civil War 
data.subset = subset(data, select=c(ccode, acdonset, regwar_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))
data.subset = na.omit(data.subset)

regmodel = glm(as.factor(acdonset) ~ 
                    regwar_acd + lnrgdpchl + lnpopl + ethnichet + polityl + politysql + t + t2 + t3, 
                family = binomial(link = probit),
                data = data.subset)

reg.AUC.main=somers2(plogis(predict(regmodel)),regmodel$y)[1]
reg.IVs=subset(data, select=c(regwar_acd, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))

reg.AUCs=rep(NA,9)
names(reg.AUCs)=names(reg.IVs)

for (i in 1:9){
    IVs.current=reg.IVs[,-i]
    IVs.current=as.matrix(IVs.current)
    model.current=glm(as.factor(data$acdonset)~IVs.current, family = binomial(link = probit))
    reg.AUCs[i]=somers2(plogis(predict(model.current)),model.current$y)[1]
}

reg = coef_test(regmodel, vcov = "CR1S", cluster = data.subset$ccode)[2:10, ]
reg.z= abs(reg$beta)/reg$SE
reg.name=c("regwar")
reg.ppower=reg.AUC.main-reg.AUCs

# Neighborhood GDP
data.subset = subset(data, select=c(ccode, acdonset, neighlgdp, lnrgdpchl, lnpopl, ethnichet, polityl, polityl, politysql, t, t2, t3))
data.subset = na.omit(data.subset)

ngdpmodel = glm(as.factor(acdonset) ~ 
                     neighlgdp + lnrgdpchl + lnpopl + ethnichet + polityl + politysql + t + t2 + t3, 
                 family = binomial(link = probit),
                 data = data.subset)

ngdp.AUC.main=somers2(plogis(predict(ngdpmodel)),ngdpmodel$y)[1]
ngdp.IVs=subset(data, select=c(neighlgdp, lnrgdpchl,lnpopl, ethnichet, polityl, politysql, t, t2, t3))

ngdp.AUCs=rep(NA,9)
names(ngdp.AUCs)=names(ngdp.IVs)

for (i in 1:9){
    IVs.current=ngdp.IVs[,-i]
    IVs.current=as.matrix(IVs.current)
    model.current=glm(as.factor(data$acdonset)~IVs.current, family = binomial(link = probit))
    ngdp.AUCs[i]=somers2(plogis(predict(model.current)),model.current$y)[1]
}

ngdp = coef_test(ngdpmodel, vcov = "CR1S", cluster = data.subset$ccode)[2:10, ]
ngdp.z= abs(ngdp$beta)/ngdp$SE
ngdp.name=c("ngdpwar")
ngdp.ppower=ngdp.AUC.main-ngdp.AUCs


#Plots
all.names = c("CE", "SLC", "SLCO", "NCIVWAR", "REGWAR", "NGDP")
all.zs = c(ce.zs[1], slc.z[1], slco.z[1], nciv.z[1], reg.z[1], ngdp.z[1])
all.ppower = c(ce.ppower[1], slc.ppower[1], slco.ppower[1], nciv.ppower[1], reg.ppower[1], ngdp.ppower[1])

plot(ce.zs,as.vector(ce.ppower),type="n",bty="n",xlim=c(0,6),ylim=c(-0.02,0.04),ylab="Change in Predictive Power",
     xlab="Statistical Significance", main="Predictive vs. Statistical Significance: Neighborhood Measures")
points(all.zs,all.ppower,pch=16)

#Add 0 line
#segments(x0=0,x1=6,y0=0,y1=0,col="black", lwd=2, lty = 2)

# Add Text
text(all.zs, all.ppower+0.0025, all.names,adj=0)


