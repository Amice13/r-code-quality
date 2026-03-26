### Replication for Trussler
##"The Effects of High Information Environments on Legislative Behavior in the US House of Representatives" 
## Prepared November 2020
rm(list=ls())
#Required packages
library(sandwich)
library(foreign)
library(stargazer)
library(plm)
library(lfe)

#Working directory with files
setwd("C:/Google Drive/Information and Accountability/Legislator Response Paper/Replication Material")


##### Figure 1: Broadband Levels Over Time#######
rm(list=ls())
load(file="All Years Legislator Response Analysis Data.Rdata")
congress <- seq(107,111,1)
title <- c("107th Congress (2001-2003)",
           "108th Congress (2003-2005)",
           "109th Congress (2005-2007)",
           "110th Congress (2007-2009)",
           "111th Congress (2009-2011)")

tiff(file="./Figures/Figure1.tiff", height=6, width=8, units = "in", res=600)
par(mfrow=c(2,3))
for(i in 1:5){
plot(density(cd$providers[cd$cong==congress[i]],na.rm=T),
     ylim=c(0,.40), xlab="Providers", main=title[i])
  abline(v=median(cd$providers[cd$cong==congress[i]], na.rm=T), lty=2, col="gray80")
  legend("topright", c(paste("Median = ", print(round(median(cd$providers[cd$cong==congress[i]], na.rm=T),1)))))
}
dev.off()

##### Figure 3: Levels and Changes in Broadband due to 2002 Redistricting#######

rm(list=ls())
load(file="Redistrict Legislator Response Data.Rdata")



districts7 <- unique(cdr$icpsr.id[!is.na(cdr$providers.redist) & cdr$cong==107])
districts8 <- unique(cdr$icpsr.id[!is.na(cdr$providers.redist) & cdr$cong==108])
districts <- districts8[districts8 %in% districts7]
districts <- districts[!is.na(districts)]
providers.107 <- rep(NA, length(districts))
providers.108 <- rep(NA, length(districts))
change <- cbind.data.frame(districts,providers.107, providers.108)

for(i in 1:nrow(change)){
  change$providers.107[i] <- cdr$providers.redist[cdr$icpsr.id==change$districts[i] & cdr$cong==107]
  change$providers.108[i] <- cdr$providers.redist[cdr$icpsr.id==change$districts[i] & cdr$cong==108]
  
}

tiff(file="./Figures/Figure3a.tiff", height=6, width= 6, unit = "in", res=600)
plot(change$providers.107,change$providers.108, xlab="Providers - Old Boundaries", ylab="Providers - New Boundaries",
     axes=F)
axis(side=1, at=seq(2,10,2))
axis(side=2, at=seq(2,10,2), las=2)
abline(0,1)
dev.off()

change$change <- change$providers.108 - change$providers.107
mean(change$change)
sd(change$change)

tiff(file="./Figures/Figure3b.tiff", height=6, width= 6, unit = "in", res=600)
plot(density(change$change,na.rm=T), xlab="Change in Providers due to Redistricting", main="", axes=F)
axis(side=1, seq(-2,4,2))
axis(side=2, seq(0,2,.5), las=2)
dev.off()

##### Table 1 Main Analyses#######
rm(list=ls())
load(file="Redistrict Legislator Response Data.Rdata")
names(cdr)



m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income)  + log(pop) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                  + log(med.income)  + log(pop) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income) + log(pop) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))

save(m.pu.leg,m.pres.leg,m.ig.leg, m.pu.leg.se,m.pres.leg.se,m.ig.leg.se, file="Main Estimates.Rdata")
coefs <- c(coef(m.pu.leg)["log(providers.redist)"], coef(m.pres.leg)["log(providers.redist)"],
           coef(m.ig.leg)["log(providers.redist)"])
save(coefs, file="Redistricting Coefficients.Rdata")


m.full.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                  perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.full.pu.leg.vcov <- vcovHC(m.full.pu.leg, method="white1")
m.full.pu.leg.se <- sqrt(diag(m.full.pu.leg.vcov))

m.full.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                  + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                    perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.full.pres.leg.vcov <- vcovHC(m.full.pres.leg, method="white1")
m.full.pres.leg.se <- sqrt(diag(m.full.pres.leg.vcov))

m.full.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                  perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.full.ig.leg.vcov <- vcovHC(m.full.ig.leg, method="white1")
m.full.ig.leg.se <- sqrt(diag(m.full.ig.leg.vcov))

stargazer(m.pu.leg,m.full.pu.leg,m.pres.leg,m.full.pres.leg,
          m.ig.leg,m.full.ig.leg,
          omit=c("congress","party", "med.income",
                 "perc.poverty","pop","perc.white","perc.black",
                 "perc.bachelor","med.age"), type="text")


stargazer(m.pu.leg,m.full.pu.leg,m.pres.leg,m.full.pres.leg,
          m.ig.leg,m.full.ig.leg,
          omit=c("congress","party", "med.income",
                 "perc.poverty","pop","perc.white","perc.black",
                 "perc.bachelor","med.age"), 
          se=list( m.pu.leg.se, m.full.pu.leg.se, m.pres.leg.se, m.full.pres.leg.se,
                   m.ig.leg.se ,m.full.ig.leg.se),
          star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$",
                               "$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Demographics", "Simple","Full","Simple","Full","Simple","Full")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="ResultsI",
          type="html",
          out="./Figures/Table1.htm")


##### Figure 4: Effects Moderated by District Partisanship
rm(list=ls())
load(file="Redistrict Legislator Response Data.Rdata")

m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                + log(med.income)  + log(pop) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, cluster="group", method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                  + log(med.income)  + log(pop) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, cluster="group", method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                + log(med.income) + log(pop) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, cluster="group", method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))





#Marginal effect of broadband by district partisanship
Rmargins <- function(model,xname,xzname,vcov.matrix,levels, ci=.95, latex=F){
  # Generate Marginal Effects
  m_effect <- coef(model)[xname]+coef(model)[xzname]*levels
  #Variances and Covariances
  v_x <- vcov.matrix[xname,xname]
  v_xz <- vcov.matrix[xzname,xzname]
  cov <- vcov.matrix[xname,xzname]
  #Standard Errors
  se <- sqrt(v_x + (levels^2)*v_xz+2*levels*cov)
  #T value for 95%CI
  if (ci==.95){
    t <- qt(0.025,model$df)
  }
  #T value for 90%CI
  if (ci==.9){
    t <- qt(0.05,model$df)
  }
  #Confidence Bounds
  m_upper <- m_effect + se*t
  m_lower <- m_effect - se*t
  # Remove Flotsom and Jetson
  #Printing Table
  table <- cbind(levels,m_effect,se,m_upper,m_lower)
  if (latex==T){
    library(xtable)
    print(xtable(table),include.rownames=FALSE)
  }
  if (latex==F){
    return(table)
  }
}


district.partisanship<- seq(25,100,.01)
example.marginality <- c(50,75,90)


margins <- Rmargins(m.pu.leg, "log(providers.redist)","log(providers.redist):district.partisanship", m.pu.leg.vcov, district.partisanship)

tiff(file="./Figures/Figure4a.tiff", height=6, width= 6, unit = "in", res=600)
plot(margins[,1], margins[,2], type="l", ylim=c(-2,10), xlim=c(20,100),
     ylab="Effect of 100% Change in Broadband on Party Unity",
     xlab="District Partisanship", axes=F)
points(margins[,1], margins[,4], type="l", lty=2)
points(margins[,1], margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$district.partisanship[cdr$cong==107])
axis(side=1, at=seq(20,100,20))
axis(side=2, at=seq(-2,10,2))
dev.off()


margins <- Rmargins(m.pres.leg, "log(providers.redist)","log(providers.redist):district.partisanship", m.pres.leg.vcov, district.partisanship)

tiff(file="./Figures/Figure4b.tiff", height=6, width= 6, unit = "in", res=600)
plot(margins[,1], margins[,2], type="l", ylim=c(-10,20), xlim=c(20,100),
     ylab="Effect of 100% Change in Broadband on Presidential Support",
     xlab="District Partisanship", axes=F)
points(margins[,1], margins[,4], type="l", lty=2)
points(margins[,1], margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$district.partisanship[cdr$cong==107])
axis(side=1, at=seq(20,100,20))
axis(side=2, at=seq(-10,20,5))
dev.off()



margins <- Rmargins(m.ig.leg, "log(providers.redist)","log(providers.redist):district.partisanship", m.ig.leg.vcov, district.partisanship)

tiff(file="./Figures/Figure4c.tiff", height=6, width= 6, unit = "in", res=600)
plot(margins[,1], margins[,2], type="l", ylim=c(-15,20), xlim=c(20,100),
     ylab="Effect of 100% Change in Broadband on Interest Group Scores",
     xlab="District Partisanship", axes=F)
points(margins[,1], margins[,4], type="l", lty=2)
points(margins[,1], margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$district.partisanship[cdr$cong==107])
axis(side=1, at=seq(20,100,20))
axis(side=2, at=seq(-15,20,5))
dev.off()


Rmargins(m.pu.leg, "log(providers.redist)","log(providers.redist):district.partisanship", m.pu.leg.vcov, example.marginality)
Rmargins(m.pres.leg, "log(providers.redist)","log(providers.redist):district.partisanship", m.pres.leg.vcov, example.marginality)
Rmargins(m.ig.leg, "log(providers.redist)","log(providers.redist):district.partisanship", m.ig.leg.vcov, example.marginality)






##### Figure 5: Predicted Effect of Broadband: 107th to 108th Congresses #######
rm(list=ls())
load(file="All Years Legislator Response Analysis Data.Rdata")

members <- unique(cd$icpsr.id)

members107 <-  unique(cd$icpsr.id[cd$congress==107])
members108 <-  unique(cd$icpsr.id[cd$congress==108])

members <- members[(members %in% members107) & (members %in% members108)]

#Replace 107th Congress numbers with ``reconstructed'' 108th Congress districts
#That is, use 2000 broadband to construct what the broadband level would have been in post-redistricting boundaries

cd <- cd[cd$congress==108,]
cd7 <- cd

cd7$congress <- "107"
cd7$providers <- NA
cd7$year <- "2000"
#cd7$cdfip <- cd7$cdfip.x
cd7$unique <- paste(cd7$year, cd7$cdfip, sep="")


#Load Broadband Data, by zip code, 2000
bbz <- read.csv("FCCDataJune2000.csv")
bbz <- bbz[,-1]
names(bbz) <- c("zip","providers")
bbz$providers <- as.character(bbz$providers)
#Delte rows with no Zip codes
bbz <- bbz[!is.na(bbz$zip),]
#Replace *[1-3 providers] with 2
bbz$providers[bbz$providers=="*"] <- 2

#Make zip codes in both datasets have 5 characters
bbz$zip[nchar(bbz$zip)==4] <- paste("0",bbz$zip[nchar(bbz$zip)==4],sep="")
bbz$zip[nchar(bbz$zip)==3] <- paste("0","0",bbz$zip[nchar(bbz$zip)==3],sep="")


#use 108 crossover data
zct <- read.csv("108ZCTACD.csv")
zct <- zct[-1,]
#Create cds FIPS
zct$cdfip <- paste(zct$state,zct$cd108, sep="")

#Merge the two datasets, using crossover data as "master"
bbzct<- merge(zct,bbz,by.x="zcta5",by.y="zip",all.x=T)

#For any zips with no data from FCC,d  assign a 0 for providers
bbzct$providers[is.na(bbzct$providers)] <- 0
bbzct$providers <- as.numeric(bbzct$providers)
table(bbzct$providers)
# Now for all cdss take weighted average of providers
cds <- unique(bbzct$cdfip)
providers<- rep(NA,length(cds))

#Make weighting variable numeric
bbzct$afact <- as.numeric(as.character(bbzct$afact))
for (i in 1:length(cds)){
  providers[i] <- weighted.mean(bbzct$providers[bbzct$cdfip==cds[i]],bbzct$afact[bbzct$cdfip==cds[i]])
}
june00 <- cbind.data.frame(cds,providers)
june00$year <- 2000

june00$unique <- paste(june00$year, june00$cds, sep="")
june00$cds <- NULL
june00$year <- NULL

#Merge in 2000 broadband data
cd7$providers <- NULL
cd7 <- merge(cd7, june00, by="unique", all.x=T)

cd <- rbind.data.frame(cd, cd7)

#Calculate levels and changes for these districts, by member
bb.levels <- NA
bb.change <- NA

for(i in 1:length(members)){
  bb.levels[i]<- cd$providers[cd$icpsr.id==members[i] & cd$congress==107] 
  bb.change[i] <- cd$providers[cd$icpsr.id==members[i] & cd$congress==108] - cd$providers[cd$icpsr.id==members[i] & cd$congress==107] 
}

load(file="Redistricting Coefficients.Rdata")

cor(bb.change,bb.levels, use="pairwise.complete")

effect.pu <- bb.change *coefs[1]*(1/bb.levels)
effect.pres <- bb.change *coefs[2]*(1/bb.levels)
effect.ig <- bb.change *coefs[3]*(1/bb.levels)

tiff(file="./Figures/Figure5a.tiff", height=6, width= 6, unit = "in", res=600)
plot(density(effect.pu, na.rm=T), xlim=c(0,11),
     xlab="107th- 108th Congress Predicted Impact", main="", axes=F)
abline(v=median(effect.pu, na.rm=T), lty=2)
abline(v=quantile(effect.pu,0.025, na.rm=T), lty=4)
abline(v=quantile(effect.pu,0.975, na.rm=T), lty=4)
legend("topright", c(paste("Median = ", print(round(median(effect.pu, na.rm=T),2))),
                     paste("95% of Cases = [", print(round(quantile(effect.pu, 0.025, na.rm=T),2)),"-",
                                              print(round(quantile(effect.pu, 0.975, na.rm=T),2)),"]",sep="")), lty=c(2,4),
                                              bg="white")
axis(side=1, seq(0,10,2))
axis(side=2, seq(0,0.35, 0.05), las=2)
dev.off()

tiff(file="./Figures/Figure5b.tiff", height=6, width= 6, unit = "in", res=600)
plot(density(effect.pres, na.rm=T), xlim=c(0,11),
     xlab="107th- 108th Congress Predicted Impact", main="", axes=F)
abline(v=median(effect.pres, na.rm=T), lty=2)
abline(v=quantile(effect.pres,0.025, na.rm=T), lty=4)
abline(v=quantile(effect.pres,0.975, na.rm=T), lty=4)
legend("topright", c(paste("Median = ", print(round(median(effect.pres, na.rm=T),2))),
                     paste("95% of Cases = [", print(round(quantile(effect.pres, 0.025, na.rm=T),2)),"-",
                           print(round(quantile(effect.pres, 0.975, na.rm=T),2)),"]",sep="")), lty=c(2,4), bg="white")
axis(side=1, seq(0,10,2))
axis(side=2, seq(0,0.15, 0.05), las=2)
dev.off()

tiff(file="./Figures/Figure5c.tiff", height=6, width= 6, unit = "in", res=600)
plot(density(effect.ig, na.rm=T), xlim=c(0,11),
     xlab="107th- 108th Congress Predicted Impact", main="", axes=F)
abline(v=median(effect.ig, na.rm=T), lty=2)
abline(v=quantile(effect.ig,0.025, na.rm=T), lty=4)
abline(v=quantile(effect.ig,0.975, na.rm=T), lty=4)
legend("topright", c(paste("Median = ", print(round(median(effect.ig, na.rm=T),2))),
                     paste("95% of Cases = [", print(round(quantile(effect.ig, 0.025, na.rm=T),2)),"-",
                           print(round(quantile(effect.ig, 0.975, na.rm=T),2)),"]",sep="")), lty=c(2,4))
axis(side=1, seq(0,10,2))
axis(side=2, seq(0,0.6, 0.1), las=2)
dev.off()

median(bb.levels,na.rm=T)
median(bb.change, na.rm=T)

#108-109

load(file="Panel Legislator Response Analysis Data.Rdata")

members <- unique(cd$icpsr.id)

members107 <-  unique(cd$icpsr.id[cd$congress==108])
members108 <-  unique(cd$icpsr.id[cd$congress==109])

members <- members[(members %in% members107) & (members %in% members108)]

bb.levels <- NA
bb.change <- NA

for(i in 1:length(members)){
  bb.levels[i]<- cd$providers[cd$icpsr.id==members[i] & cd$congress==108] 
  bb.change[i] <- cd$providers[cd$icpsr.id==members[i] & cd$congress==109] - cd$providers[cd$icpsr.id==members[i] & cd$congress==108] 
}

cor(bb.change,bb.levels, use="pairwise.complete")
summary(lm(bb.change ~ bb.levels))

load(file="Redistricting Coefficients.Rdata")


effect.pu <- bb.change *coefs[1]*(1/bb.levels)
effect.pres <- bb.change *coefs[2]*(1/bb.levels)
effect.ig <- bb.change *coefs[3]*(1/bb.levels)


median(bb.change)
median(bb.levels)

median(effect.pu)
median(effect.pres)
median(effect.ig)




##### Figure 6: Placebo Analysis
rm(list=ls())
load(file="Redistrict Legislator Response Data PLACEBO.Rdata")


p.m.mpu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                   + log(pop) + log(med.income) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
p.m.mpu.leg.vcov <- vcovHC(p.m.mpu.leg, method="white1")
p.m.mpu.leg.se <- sqrt(diag(p.m.mpu.leg.vcov))

p.m.mpres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                     + log(pop) + log(med.income) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
p.m.mpres.leg.vcov <- vcovHC(p.m.mpres.leg, method="white1")
p.m.mpres.leg.se <- sqrt(diag(p.m.mpres.leg.vcov))

p.m.mig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                   + log(pop) + log(med.income) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
p.m.mig.leg.vcov <- vcovHC(p.m.mig.leg, method="white1")
p.m.mig.leg.se <- sqrt(diag(p.m.mig.leg.vcov))



#Load main results (generated in code above)
load("C:/Google Drive/Information and Accountability/Legislator Response Paper/Main Estimates.Rdata")

#Plot for paper


estimates.main <- c(m.pu.leg$coefficients["log(providers.redist)"],
                    m.pres.leg$coefficients["log(providers.redist)"])
up.main <- estimates.main + 1.96*c(m.pu.leg.se["log(providers.redist)"],
                                   m.pres.leg.se["log(providers.redist)"])
down.main <- estimates.main - 1.96*c(m.pu.leg.se["log(providers.redist)"],
                                     m.pres.leg.se["log(providers.redist)"])



estimates.placebo <- c(p.m.mpu.leg$coefficients["log(providers.redist)"],
                       p.m.mpres.leg$coefficients["log(providers.redist)"])
up.placebo <- estimates.placebo + 1.96*c(p.m.mpu.leg.se["log(providers.redist)"],
                                         p.m.mpres.leg.se["log(providers.redist)"])
down.placebo <- estimates.placebo - 1.96*c(p.m.mpu.leg.se["log(providers.redist)"],
                                           p.m.mpres.leg.se["log(providers.redist)"])



ypos1 <- -1*c(1,5)
ypos2 <- -1*c(2,6)

tiff(file="./Figures/PlaceboResults.tiff", height=6, width= 6, unit = "in", res=600)
par(mar = c(5.1, 6.5, 4.1, 2.1))

plot(estimates.main, ypos1, ylim=c(-7,0), xlim=c(-10,10), pch=16, axes=F, ylab="",
     xlab="Coefficient on ln(Providers)")
segments(estimates.main, ypos1, up.main, ypos1, lwd=2)
segments(estimates.main, ypos1, down.main, ypos1, lwd=2)

points(estimates.placebo, ypos2, pch=17, col="gray60")
segments(estimates.placebo, ypos2, up.placebo, ypos2, lwd=2, col="gray60")
segments(estimates.placebo, ypos2, down.placebo, ypos2, lwd=2, col="gray60")

abline(h=-4, lty=2)
abline(v=0,lty=2, lwd=2)
axis(side=2, at=c(-1.25,-1.75,-5.25,-5.75), labels=c("Party", "Unity", "Presidential", "Voting"), las=2,
     col = NA)
axis(side=1, at=seq(-10,10,5))

legend("topleft", c("Main Estimates", "Placebo Estimates"), pch=c(16,17), col=c("black", "gray60"))
dev.off()


##### Within Legislator Variance for Footnote 16 ######
rm(list=ls())
load(file="Panel Legislator Response Analysis Data.Rdata")


mems <- unique(cd$icpsr.id)
var.pu <- NA
var.pres <- NA
var.ig <- NA

for(i in 1:length(mems)){
  var.pu[i]<- sd(cd$party.unity[cd$icpsr.id==mems[i]],na.rm=T)
  var.pres[i]<- sd(cd$pres.voting[cd$icpsr.id==mems[i]],na.rm=T)
  var.ig[i]<- sd(cd$ig[cd$icpsr.id==mems[i]],na.rm=T)
  
}

summary(var.pu)
summary(var.pres)
summary(var.ig)



