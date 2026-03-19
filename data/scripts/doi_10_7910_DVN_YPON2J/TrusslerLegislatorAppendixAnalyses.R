### Replication for Appendix of Trussler
#"The Effects of High Information Environments on Legislative Behavior in the US House of Representatives" 
## PRepared November 2020
rm(list=ls())
library(sandwich)
library(foreign)
library(stargazer)
library(plm)
library(lfe)
library(gtools)
library(xtable)

setwd("C:/Google Drive/Information and Accountability/Legislator Response Paper/Replication Material")



##### Section 1: Full Model Results for Interactive Hypothesis#######
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




stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$","$ln(Med.Income_{ic})$",
                               "$ln(Population_{ic})$",
                               "$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="Appendix III")

stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se), report =('vc*p'))

##### Section 2: Analysis of committee choice#######
rm(list=ls())
load(file="Redistrict Legislator Response Data.Rdata")


m.comm.leg <- plm(constituency ~ factor(congress)*factor(party)  + log(providers.redist)
                  + log(med.income) + log(pop) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.comm.leg.vcov <- vcovHC(m.comm.leg, method="white1")
m.comm.leg.se <- sqrt(diag(m.comm.leg.vcov))

stargazer(m.comm.leg, omit=c("congress","party"),
          se=list(m.comm.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$",
                               "$Perc.Poverty_{ic}$","$ln(Population_{ic}$",
                               "$Partisan.Composition_{ic}$"),
          dep.var.labels   = "$Perc.Constituency.Committees_{ic}$",
          add.lines = list(c("Legislator F.E.", "Yes"),
                           c("Congress-Party F.E.","Yes")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Committee Selection",
          label="ResultsIII")

##### Section 3: Replication with full panel#######
rm(list=ls())
load(file="Panel Legislator Response Analysis Data.Rdata")

m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers) 
                + log(med.income)  + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, method="arellano", cluster="group")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)   + log(providers) 
                  + log(med.income)  + log(pop)   , data=cd, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, method="arellano", cluster="group")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers) 
                + log(med.income)  + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, method="arellano", cluster="group")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))



stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$",
                               "$ln(Population_{ic})$",
                               "$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="AppendixI")


m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers) 
                + log(med.income)  + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, method="arellano", cluster="group")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)   + log(providers) 
                  + log(med.income)  + log(pop)   , data=cd, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, method="arellano", cluster="group")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers) 
                + log(med.income)  + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, method="arellano", cluster="group")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))



stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="AppendixIa")

##Interactions and marginal effects.
m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers)*district.partisanship
                + log(med.income)  + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, cluster="group", method="arellano")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.comm.leg <- plm(constituency ~ factor(congress)*factor(party)  + log(providers)*district.partisanship
                  + log(med.income)  + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.comm.leg.vcov <- vcovHC(m.comm.leg, cluster="group", method="arellano")
m.comm.leg.se <- sqrt(diag(m.comm.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers)*district.partisanship
                  + log(med.income)  + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, cluster="group", method="arellano")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers)*district.partisanship
                + log(med.income) + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, cluster="group", method="arellano")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))


stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se), report="vc*p")

stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$","$ln(Med.Income_{ic})$",
                               "$ln(Population_{ic})$",
                               "$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="Appendix III")



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


margins <- Rmargins(m.pu.leg, "log(providers)","log(providers):district.partisanship", m.pu.leg.vcov, district.partisanship)

Rmargins(m.pu.leg, "log(providers)","log(providers):district.partisanship", m.pu.leg.vcov, example.marginality)

pdf(file="./Appendix/MarginsPUappend.pdf", height=6, width = 6)
plot(margins[,1], margins[,2], type="l", ylim=c(-4,10), xlim=c(20,100),
     ylab="Effect of 100% Change in Broadband",
     xlab="District Partisanship", axes=F)
points(margins[,1], margins[,4], type="l", lty=2)
points(margins[,1], margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cd$district.partisanship[cd$cong==109])
axis(side=1, at=seq(20,100,20))
axis(side=2, at=seq(-4,10,2))
dev.off()


margins <- Rmargins(m.pres.leg, "log(providers)","log(providers):district.partisanship", m.pres.leg.vcov, district.partisanship)

pdf(file="./Appendix/MarginsPRappend.pdf", height=6, width = 6)
plot(margins[,1], margins[,2], type="l", ylim=c(-10,20), xlim=c(20,100),
     ylab="Effect of 100% Change in Broadband",
     xlab="District Partisanship", axes=F)
points(margins[,1], margins[,4], type="l", lty=2)
points(margins[,1], margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cd$district.partisanship[cd$cong==109])
axis(side=1, at=seq(20,100,20))
axis(side=2, at=seq(-10,20,5))
dev.off()



margins <- Rmargins(m.ig.leg, "log(providers)","log(providers):district.partisanship", m.ig.leg.vcov, district.partisanship)

pdf(file="./Appendix/MarginsIGappend.pdf", height=6, width = 6)
plot(margins[,1], margins[,2], type="l", ylim=c(-4,8), xlim=c(20,100),
     ylab="Effect of 100% Change in Broadband",
     xlab="District Partisanship", axes=F)
points(margins[,1], margins[,4], type="l", lty=2)
points(margins[,1], margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cd$district.partisanship[cd$cong==109])
axis(side=1, at=seq(20,100,20))
axis(side=2, at=seq(-4,8,2))
dev.off()


Rmargins(m.pu.leg, "log(providers)","log(providers):district.partisanship", m.pu.leg.vcov, example.marginality)
Rmargins(m.pres.leg, "log(providers)","log(providers):district.partisanship", m.pu.leg.vcov, example.marginality)
Rmargins(m.ig.leg, "log(providers)","log(providers):district.partisanship", m.pu.leg.vcov, example.marginality)

## Committee Selection

m.comm.leg <- plm(constituency ~ factor(congress)*factor(party)   + log(providers)
                  + log(med.income) + log(pop)  , data=cd, model="within", index=c("icpsr.id","congress"))
m.comm.leg.vcov <- vcovHC(m.comm.leg, method="arellano", cluster="group")
m.comm.leg.se <- sqrt(diag(m.comm.leg.vcov))

stargazer(m.comm.leg, omit=c("congress","party"),
          se=list(m.comm.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$",
                               "$Perc.Poverty_{ic}$","$ln(Population_{ic}$",
                               "$Partisan.Composition_{ic}$"),
          dep.var.labels   = "$Perc.Constituency.Committees_{ic}$",
          add.lines = list(c("Legislator F.E.", "Yes"),
                           c("Congress-Party F.E.","Yes")),
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Committee Selection",
          label="AppendixIII")




##### Section 4: CPS Internet Validation#######

rm(list = ls())
load(file="CPS Internet Merged.Rdata")

cps.internet$providers_nonzero <- cps.internet$providers
cps.internet$providers_nonzero[cps.internet$providers_nonzero==0] <- 0.1
cps.internet$lnproviders <- log(cps.internet$providers_nonzero)

m.bb <- felm(broadband ~  factor(year)  +log(providers) +  lnpop+
               log(med_income)| county | 0 | county,
             data=cps.internet ,
             weights=cps.internet$wtsupp)

stargazer(m.bb, type="text")

#Creation of Table 7
stargazer(m.bb, omit=c("year"),
          star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",omit.stat = c("adj.rsq", "f"), digits=2,
          covariate.labels = c("$ln(Providers_{ict})$", "$ln(Population_{ict}$",
                               "$ln(Med.Income_{ict})$"),
          dep.var.labels   = "P(Home Broadband)",
          model.numbers = F,
          add.lines = list(c("County F.E.", "Yes"),
                           c("Year F.E.","Yes")),
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Broadband on Home Broadband Subscription",
          label="cps.broadband")


##### Section 5: Results with various demographic covariates#######
rm(list=ls())

##Redistricting

#Simple Demographics
load(file="Redistrict Legislator Response Data.Rdata")

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

stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$"
                               ,"$ln(Population_{ic})$",
                               "$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="ResultsI")

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




stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$","$ln(Med.Income_{ic})$",
                               "$ln(Population_{ic})$",
                               "$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="Appendix III")


#Full Demographics

m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                  perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.comm.leg <- plm(constituency ~ factor(congress)*factor(party)  + log(providers.redist)
                  + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                    perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.comm.leg.vcov <- vcovHC(m.comm.leg, method="white1")
m.comm.leg.se <- sqrt(diag(m.comm.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                  + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                    perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                  perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))


stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$",
                               "$Perc.Poverty_{ic}$","$ln(Population_{ic})$",
                               "$Perc.White_{ic}$","$Perc.Black_{ic}$","$Perc.Bachelor_{ic}$","$Med.Age_{ic}$","$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="ResultsIcov")

m.pu.leg <- plm(party.unity ~ factor(congress)  + log(providers.redist)*district.partisanship
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black + perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, cluster="group", method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))



m.pres.leg <- plm(pres.voting ~  factor(congress)  + log(providers.redist)*district.partisanship
                  + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black + perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, cluster="group", method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)  + log(providers.redist)*district.partisanship
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black + perc.bachelor + med.age + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, cluster="group", method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))



stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$","$ln(Med.Income_{ic})$",
                               "$Perc.Poverty_{ic}$","$ln(Population_{ic})$",
                               "$Perc.White_{ic}$","$Perc.Black_{ic}$","$Perc.Bachelor_{ic}$","$Med.Age_{ic}$","$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="ResultsIVcov")



#No demographics
m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)+
                  district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))


m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                  +district.partisanship,
                  data=cdr, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                + district.partisanship,
                data=cdr, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))



stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="ResultsInocov")


m.pu.leg <- plm(party.unity ~ factor(congress)  + log(providers.redist)*district.partisanship
                , data=cdr, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, cluster="group", method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))



m.pres.leg <- plm(pres.voting ~  factor(congress)  + log(providers.redist)*district.partisanship
                  , data=cdr, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, cluster="group", method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)  + log(providers.redist)*district.partisanship
                , data=cdr, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, cluster="group", method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))



stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$",
                               "$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="ResultsIVnocov")



##Panel
rm(list=ls())
load(file="Panel Legislator Response Analysis Data.Rdata")

#Full Demographics

m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers)
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                  perc.bachelor + med.age , data=cd, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.comm.leg <- plm(constituency ~ factor(congress)*factor(party)  + log(providers)
                  + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                    perc.bachelor + med.age , data=cd, model="within", index=c("icpsr.id","congress"))
m.comm.leg.vcov <- vcovHC(m.comm.leg, method="white1")
m.comm.leg.se <- sqrt(diag(m.comm.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers)
                  + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                    perc.bachelor + med.age , data=cd, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers)
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black +
                  perc.bachelor + med.age , data=cd, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))


stargazer(m.comm.leg, omit=c("congress","party"),
          se=list(m.comm.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$",
                               "$Perc.Poverty_{ic}$","$ln(Population_{ic}$",
                               "$Perc.White_{ic}$","$Perc.Black_{ic}$","$Perc.Bachelor_{ic}$","$Med.Age_{ic}$","$Partisan.Composition_{ic}$"),
          dep.var.labels   = "$Perc.Constituency.Committees_{ic}$",
          add.lines = list(c("Legislator F.E.", "Yes"),
                           c("Congress-Party F.E.","Yes")),
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Committee Selection",
          label="ResultsIIIcov")


stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$",
                               "$Perc.Poverty_{ic}$","$ln(Population_{ic})$",
                               "$Perc.White_{ic}$","$Perc.Black_{ic}$","$Perc.Bachelor_{ic}$","$Med.Age_{ic}$","$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="ResultsIcov")



m.pu.leg <- plm(party.unity ~ factor(congress)  + log(providers)*district.partisanship
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black + perc.bachelor + med.age , data=cd, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, cluster="group", method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))


m.pres.leg <- plm(pres.voting ~  factor(congress)  + log(providers)*district.partisanship
                  + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black + perc.bachelor + med.age , data=cd, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, cluster="group", method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)  + log(providers)*district.partisanship
                + log(med.income) + perc.poverty + log(pop ) + perc.white + perc.black + perc.bachelor + med.age , data=cd, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, cluster="group", method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))



stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$","$ln(Med.Income_{ic})$",
                               "$Perc.Poverty_{ic}$","$ln(Population_{ic})$",
                               "$Perc.White_{ic}$","$Perc.Black_{ic}$","$Perc.Bachelor_{ic}$","$Med.Age_{ic}$","$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="ResultsIVcov")


#No Demographics

m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers)
                , data=cd, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers)
                  , data=cd, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers)
                , data=cd, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))

stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="ResultsIcov")



m.pu.leg <- plm(party.unity ~ factor(congress)  + log(providers)*district.partisanship
                , data=cd, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, cluster="group", method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))


m.pres.leg <- plm(pres.voting ~  factor(congress)  + log(providers)*district.partisanship
                  , data=cd, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, cluster="group", method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)  + log(providers)*district.partisanship
                , data=cd, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, cluster="group", method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))



stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$",
                               "$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="ResultsIVcov")




##### Section 6: Testing Sensitivity to Partisan Control of Redistricting#######
rm(list=ls())
load(file="Redistrict Legislator Response Data.Rdata")

pc.2000 <- read.csv("C:/Google Drive/Information and Accountability/Legislator Response Paper/2000StatehouseControl.csv")

pc.2000$state.fips <- as.character(pc.2000$state.fips)
pc.2000$state.fips[nchar(pc.2000$state.fips)==1] <- paste("0",pc.2000$state.fips[nchar(pc.2000$state.fips)==1], sep="")


cdr <- merge(cdr, pc.2000, by="state.fips", all.x=T)

#Create a variable that is 1 if legislator's party controlled redistrcting
#0 if other party controls or if legislature is split

cdr$party.control.redist <- NA
cdr$party.control.redist[(cdr$party==100 & cdr$r.state.control.2000==-1 | 
                           cdr$party==200 & cdr$r.state.control.2000==1) ]<- 1
cdr$party.control.redist[(cdr$party==100 & cdr$r.state.control.2000!=-1 | 
                           cdr$party==200 & cdr$r.state.control.2000!=1) ]<- 0


m1.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                 + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$party.control.redist==0,], model="within", index=c("icpsr.id","congress"))
m1.pu.leg.vcov <- vcovHC(m1.pu.leg, method="white1")
m1.pu.leg.se <- sqrt(diag(m1.pu.leg.vcov))

m1.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                   + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$party.control.redist==0,], model="within", index=c("icpsr.id","congress"))
m1.pres.leg.vcov <- vcovHC(m1.pres.leg, method="white1")
m1.pres.leg.se <- sqrt(diag(m1.pres.leg.vcov))

m1.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                 + log(med.income) + log(pop) + district.partisanship, data=cdr[cdr$party.control.redist==0,], model="within", index=c("icpsr.id","congress"))
m1.ig.leg.vcov <- vcovHC(m1.ig.leg, method="white1")
m1.ig.leg.se <- sqrt(diag(m1.ig.leg.vcov))


m2.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                 + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$party.control.redist==1,], model="within", index=c("icpsr.id","congress"))
m2.pu.leg.vcov <- vcovHC(m2.pu.leg, method="white1")
m2.pu.leg.se <- sqrt(diag(m2.pu.leg.vcov))

m2.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                   + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$party.control.redist==1,], model="within", index=c("icpsr.id","congress"))
m2.pres.leg.vcov <- vcovHC(m2.pres.leg, method="white1")
m2.pres.leg.se <- sqrt(diag(m2.pres.leg.vcov))

m2.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                 + log(med.income) + log(pop) + district.partisanship, data=cdr[cdr$party.control.redist==1,], model="within", index=c("icpsr.id","congress"))
m2.ig.leg.vcov <- vcovHC(m2.ig.leg, method="white1")
m2.ig.leg.se <- sqrt(diag(m2.ig.leg.vcov))



stargazer(m1.pu.leg, m2.pu.leg, m1.pres.leg, m2.pres.leg,m1.ig.leg, m2.ig.leg, omit=c("congress","party"),
          se=list( m1.pu.leg.se, m2.pu.leg.se, m1.pres.leg.se, m2.pres.leg.se,m1.ig.leg.se, m2.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$"
                               ,"$ln(Population_{ic})$",
                               "$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator's Party Controls Redistricting", "No","Yes","No", "Yes","No","Yes"),
                           c("Legislator F.E.", "Yes","Yes","Yes", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes", "Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics, by Party Control of Redistricting",
          label="PartisanControl")


#Bootstrap differences - Note that this is very computationally intensize

library(tcltk)
total <- 1000
## create progress bar
#pb <- tkProgressBar(title = "progress bar", min = 0,
#                    max = total, width = 300)
#
#set.seed(2105)
#pu.diff <- NA
#pres.diff <- NA
#ig.diff <- NA
#
#cdr.pc0 <- cdr[cdr$party.control.redist==0,]
#cdr.pc0 <- cdr.pc0[!is.na(cdr.pc0$icpsr.id),]
#cdr.pc0$row <- seq(1,nrow(cdr.pc0), 1)
#
#cdr.pc1 <- cdr[cdr$party.control.redist==1,]
#cdr.pc1 <- cdr.pc1[!is.na(cdr.pc1$icpsr.id),]
#
#
#cdr0.legs <- unique(cdr.pc0$icpsr.id)
#cdr1.legs <- unique(cdr.pc1$icpsr.id)
#
#
#
#for(i in 1:1000){
#
#  #Create bootstrap dataset for control group, sampling legislators, rebuilding data with those legislators, relabelling duplicate icpsr.id
#  #to be unique legislators
#boot.cdr0.legs <- sample(cdr0.legs, length(cdr0.legs), replace=T)  
#
#
#boot.cdr.pc0 <- cdr.pc0[cdr.pc0$icpsr.id==boot.cdr0.legs[1],]
#
#  for(j in 2:length(boot.cdr0.legs)){
#    boot.cdr.pc0 <- rbind(boot.cdr.pc0, cdr.pc0[cdr.pc0$icpsr.id==boot.cdr0.legs[j],])
#  }
#boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".2", sep="")
#boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".2", sep="")
#boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".3", sep="")
#boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".3", sep="")
#boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".4", sep="")
#boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".4", sep="")
#boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".5", sep="")
#boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".5", sep="")
#boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".6", sep="")
#boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".6", sep="")
#boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".7", sep="")
#boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".7", sep="")
#boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".8", sep="")
#boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id.unique[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".8", sep="")
#boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)] <- paste(boot.cdr.pc0$icpsr.id[duplicated(boot.cdr.pc0$icpsr.id.unique)], ".9", sep="")
#
#boot.cdr.pc0$members.unique <- paste(boot.cdr.pc0$icpsr.id, boot.cdr.pc0$congress, sep="")
#boot.cdr.pc0$duplicated <- duplicated(boot.cdr.pc0$members.unique)
#
#
##Do same for treatment group
#boot.cdr1.legs <- sample(cdr1.legs, length(cdr1.legs), replace=T)  
#
#
#boot.cdr.pc1 <- cdr.pc1[cdr.pc1$icpsr.id==boot.cdr1.legs[1],]
#
#for(j in 2:length(boot.cdr1.legs)){
#  boot.cdr.pc1 <- rbind(boot.cdr.pc1, cdr.pc1[cdr.pc1$icpsr.id==boot.cdr1.legs[j],])
#}
#boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".2", sep="")
#boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".2", sep="")
#boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".3", sep="")
#boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".3", sep="")
#boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".4", sep="")
#boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".4", sep="")
#boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".5", sep="")
#boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".5", sep="")
#boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".6", sep="")
#boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".6", sep="")
#boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".7", sep="")
#boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".7", sep="")
#boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".8", sep="")
#boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id.unique[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".8", sep="")
#boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)] <- paste(boot.cdr.pc1$icpsr.id[duplicated(boot.cdr.pc1$icpsr.id.unique)], ".9", sep="")
#  #Run models
#m1.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
#                 + log(med.income)  + log(pop) + district.partisanship, data=boot.cdr.pc0, model="within", index=c("icpsr.id","congress"))
#
#
#m1.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
#                   + log(med.income)  + log(pop) + district.partisanship, data=boot.cdr.pc0, model="within", index=c("icpsr.id","congress"))
#
#m1.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
#                 + log(med.income) + log(pop) + district.partisanship, data=boot.cdr.pc0, model="within", index=c("icpsr.id","congress"))
#
#
#m2.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
#                 + log(med.income)  + log(pop) + district.partisanship, data=boot.cdr.pc1, model="within", index=c("icpsr.id","congress"))
#
#
#m2.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
#                   + log(med.income)  + log(pop) + district.partisanship, data=boot.cdr.pc1, model="within", index=c("icpsr.id","congress"))
#
#
#m2.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
#                 + log(med.income) + log(pop) + district.partisanship, data=boot.cdr.pc1, model="within", index=c("icpsr.id","congress"))
#
#
#
#pu.diff[i] <- coef(m2.pu.leg)["log(providers.redist)"]  - coef(m1.pu.leg)["log(providers.redist)"]
#pres.diff[i] <- coef(m2.pres.leg)["log(providers.redist)"]  - coef(m1.pres.leg)["log(providers.redist)"]
#ig.diff[i] <- coef(m2.ig.leg)["log(providers.redist)"]  - coef(m1.ig.leg)["log(providers.redist)"]
#
#setTkProgressBar(pb, i, label=paste( round(i/total*100, 0),
#                                     "% done"))
#
#}

#save(pu.diff, pres.diff, ig.diff, file="Partisan Control Bootstrap.Rdata")

load(file="Partisan Control Bootstrap.Rdata")

pdf(file="./Appendix/BootstrapPU.pdf")
plot(density(pu.diff), main="Difference in Effect of Providers on Party Unity Voting", 
     xlab = "Party Control=1 - Party Control=0", xlim=c(-20,20))
abline(v=median(pu.diff), lwd=2)
abline(v=quantile(pu.diff, .025), lty=2, lwd=2)
abline(v=quantile(pu.diff, .975), lty=2, lwd=2)
legend("topright", c("Median Estimate", "95% of Estimates"), lwd=c(2,2), lty=c(1,1))
dev.off()

pdf(file="./Appendix/BootstrapPres.pdf")
plot(density(pres.diff), main="Difference in Effect of Providers on Presidential Voting", 
     xlab = "Party Control=1 - Party Control=0", xlim=c(-20,20))
abline(v=median(pres.diff), lwd=2)
abline(v=quantile(pres.diff, .025), lty=2, lwd=2)
abline(v=quantile(pres.diff, .975), lty=2, lwd=2)
legend("topright", c("Median Estimate", "95% of Estimates"), lwd=c(2,2), lty=c(1,1))
dev.off()

pdf(file="./Appendix/BootstrapIG.pdf")
plot(density(ig.diff), main="Difference in Effect of Providers on Interest Group Scores", 
     xlab = "Party Control=1 - Party Control=0", xlim=c(-20,20))
abline(v=median(ig.diff), lwd=2)
abline(v=quantile(ig.diff, .025), lty=2, lwd=2)
abline(v=quantile(ig.diff, .975), lty=2, lwd=2)
legend("topright", c("Median Estimate", "95% of Estimates"), lwd=c(2,2), lty=c(1,1))
dev.off()







##### Section 7: Balance Tests on Sample Inclusion#######
rm(list=ls())
library(xtable)
load(file="Redistrict Legislator Response Data.Rdata")

two.obs <- names(table(cdr$icpsr.id)[(table(cdr$icpsr.id)==2)])


cdr$two.obs[cdr$icpsr.id %in% two.obs]<- 1
cdr$two.obs[!(cdr$icpsr.id %in% two.obs)]<- 0

table(cdr$two.obs)


cdr.107 <- cdr[cdr$congress==107,]
cdr.108 <- cdr[cdr$congress==108,]

covariates <- c("Party Unity", "Presidential Voting", "Interest Group Scores", "District Partisanship")

###107
column.labels <- c("Leavers", "Remainers", "P Value")

pu <- t.test(cdr.107$party.unity[cdr.107$two.obs==0], cdr.107$party.unity[cdr.107$two.obs==1])
pres<- t.test(cdr.107$pres.voting[cdr.107$two.obs==0], cdr.107$pres.voting[cdr.107$two.obs==1])
ig<- t.test(cdr.107$ig[cdr.107$two.obs==0], cdr.107$ig[cdr.107$two.obs==1])
prov<- t.test(cdr.107$district.partisanship[cdr.107$two.obs==0], cdr.107$district.partisanship[cdr.107$two.obs==1])


balance <- matrix(NA, nrow=5, ncol=4)


balance[2,2:3] <- round(pu$estimate,2)
balance[3,2:3] <- round(pres$estimate,2)
balance[4,2:3] <- round(ig$estimate,2)
balance[5,2:3] <- round(prov$estimate,2)

balance[2,4] <- round(pu$p.value,2)
balance[3,4] <- round(pres$p.value,2)
balance[4,4] <- round(ig$p.value,2)
balance[5,4] <- round(prov$p.value,2)


balance[2:5,1] <- covariates

balance[1,2:4] <- column.labels

print(xtable(balance), include.rownames=F, include.colnames=F)


###108
column.labels <- c("Joiners", "Remainers", "P Value")

pu <- t.test(cdr.108$party.unity[cdr.108$two.obs==0], cdr.108$party.unity[cdr.108$two.obs==1])
pres<- t.test(cdr.108$pres.voting[cdr.108$two.obs==0], cdr.108$pres.voting[cdr.108$two.obs==1])
ig<- t.test(cdr.108$ig[cdr.108$two.obs==0], cdr.108$ig[cdr.108$two.obs==1])
prov<- t.test(cdr.108$district.partisanship[cdr.108$two.obs==0], cdr.108$district.partisanship[cdr.108$two.obs==1])


balance <- matrix(NA, nrow=5, ncol=4)


balance[2,2:3] <- round(pu$estimate,2)
balance[3,2:3] <- round(pres$estimate,2)
balance[4,2:3] <- round(ig$estimate,2)
balance[5,2:3] <- round(prov$estimate,2)

balance[2,4] <- round(pu$p.value,2)
balance[3,4] <- round(pres$p.value,2)
balance[4,4] <- round(ig$p.value,2)
balance[5,4] <- round(prov$p.value,2)


balance[2:5,1] <- covariates

balance[1,2:4] <- column.labels

print(xtable(balance), include.rownames=F, include.colnames=F)



#Test differences in at-large members


cdr$at.large[cdr$cdfip %in% c("0201","1001", "3001","3801","4601", "5001", "5601")]<- 1
cdr$at.large[!(cdr$cdfip %in% c("0201","1001", "3001","3801","4601", "5001", "5601"))]<- 0

table(cdr$at.large)


cdr.107 <- cdr[cdr$congress==107,]
cdr.108 <- cdr[cdr$congress==108,]

###107
column.labels <- c("Redistricted", "At-Large", "P Value")
covariates <- c("Party Unity", "Presidential Voting", "Interest Group Scores", "District Partisanship")

pu <- t.test(cdr.107$party.unity[cdr.107$at.large==0], cdr.107$party.unity[cdr.107$at.large==1])
pres<- t.test(cdr.107$pres.voting[cdr.107$at.large==0], cdr.107$pres.voting[cdr.107$at.large==1])
ig<- t.test(cdr.107$ig[cdr.107$at.large==0], cdr.107$ig[cdr.107$at.large==1])
prov<- t.test(cdr.107$district.partisanship[cdr.107$at.large==0], cdr.107$district.partisanship[cdr.107$at.large==1])


balance <- matrix(NA, nrow=5, ncol=4)


balance[2,2:3] <- round(pu$estimate,2)
balance[3,2:3] <- round(pres$estimate,2)
balance[4,2:3] <- round(ig$estimate,2)
balance[5,2:3] <- round(prov$estimate,2)

balance[2,4] <- round(pu$p.value,2)
balance[3,4] <- round(pres$p.value,2)
balance[4,4] <- round(ig$p.value,2)
balance[5,4] <- round(prov$p.value,2)


balance[2:5,1] <- covariates

balance[1,2:4] <- column.labels

print(xtable(balance), include.rownames=F, include.colnames=F)


###108
column.labels <- c("Redistricted", "At-Large", "P Value")

pu <- t.test(cdr.108$party.unity[cdr.108$at.large==0], cdr.108$party.unity[cdr.108$at.large==1])
pres<- t.test(cdr.108$pres.voting[cdr.108$at.large==0], cdr.108$pres.voting[cdr.108$at.large==1])
ig<- t.test(cdr.108$ig[cdr.108$at.large==0], cdr.108$ig[cdr.108$at.large==1])
prov<- t.test(cdr.108$district.partisanship[cdr.108$at.large==0], cdr.108$district.partisanship[cdr.108$at.large==1])


balance <- matrix(NA, nrow=5, ncol=4)


balance[2,2:3] <- round(pu$estimate,2)
balance[3,2:3] <- round(pres$estimate,2)
balance[4,2:3] <- round(ig$estimate,2)
balance[5,2:3] <- round(prov$estimate,2)

balance[2,4] <- round(pu$p.value,2)
balance[3,4] <- round(pres$p.value,2)
balance[4,4] <- round(ig$p.value,2)
balance[5,4] <- round(prov$p.value,2)


balance[2:5,1] <- covariates

balance[1,2:4] <- column.labels

print(xtable(balance), include.rownames=F, include.colnames=F)




##### Section 8: Testing Non-Linearity in District Partisanship#######
rm(list=ls())
load(file="Redistrict Legislator Response Data.Rdata")


cdr$marginal<- NA
cdr$marginal[cdr$district.partisanship<55]<- 1
cdr$marginal[cdr$district.partisanship>=55] <- 0
boxplot(cdr$district.partisanship ~ cdr$marginal)
table(cdr$marginal)


m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==1,], model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, cluster="group", method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))


m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                  + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==1,], model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, cluster="group", method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income)  + log(pop) + district.partisanship , data=cdr[cdr$marginal==1,], model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, cluster="group", method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))

stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"), type="text")


m.pu.leg2 <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                 + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==0,], model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov2 <- vcovHC(m.pu.leg2, cluster="group", method="white1")
m.pu.leg.se2 <- sqrt(diag(m.pu.leg.vcov2))


m.pres.leg2 <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                   + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==0,], model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov2 <- vcovHC(m.pres.leg2, cluster="group", method="white1")
m.pres.leg.se2 <- sqrt(diag(m.pres.leg.vcov2))

m.ig.leg2 <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                 + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==0,], model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov2 <- vcovHC(m.ig.leg2, cluster="group", method="white1")
m.ig.leg.se2 <- sqrt(diag(m.ig.leg.vcov2))

stargazer(m.pu.leg2,m.pres.leg2,m.ig.leg2, omit=c("congress","party"), type="text")


stargazer(m.pu.leg,m.pu.leg2, m.pres.leg,m.pres.leg2, m.ig.leg,m.ig.leg2, omit=c("congress","party"),
          se=list(m.pu.leg.se,m.pu.leg.se2, m.pres.leg.se,m.pres.leg.se2, m.ig.leg.se,m.ig.leg.se2),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$","$ln(Med.Income_{ic})$",
                               "$ln(Population_{ic})$",
                               "$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list( c("Marginal District", "Yes","No","Yes", "No","Yes","No"),
                            c("Legislator F.E.", "Yes","Yes","Yes", "Yes","Yes","Yes"),
                            c("Congress-Party F.E.","Yes","Yes","Yes","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="MarginalI")

#Interaction

cor(cbind(cdr$district.partisanship, cdr$party.unity, cdr$pres.voting, cdr$ig), use="pairwise.complete")

m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==1,], model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, cluster="group", method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))


m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                  + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==1,], model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, cluster="group", method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                + log(med.income)  + log(pop) + district.partisanship , data=cdr[cdr$marginal==1,], model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, cluster="group", method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))

stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"), type="text")


m.pu.leg2 <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                 + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==0,], model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov2 <- vcovHC(m.pu.leg2, cluster="group", method="white1")
m.pu.leg.se2 <- sqrt(diag(m.pu.leg.vcov2))


m.pres.leg2 <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                   + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==0,], model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov2 <- vcovHC(m.pres.leg2, cluster="group", method="white1")
m.pres.leg.se2 <- sqrt(diag(m.pres.leg.vcov2))

m.ig.leg2 <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)*district.partisanship
                 + log(med.income)  + log(pop) + district.partisanship, data=cdr[cdr$marginal==0,], model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov2 <- vcovHC(m.ig.leg2, cluster="group", method="white1")
m.ig.leg.se2 <- sqrt(diag(m.ig.leg.vcov2))

stargazer(m.pu.leg,m.pu.leg2, m.pres.leg,m.pres.leg2, m.ig.leg,m.ig.leg2, omit=c("congress","party"),
          se=list(m.pu.leg.se,m.pu.leg.se2, m.pres.leg.se,m.pres.leg.se2, m.ig.leg.se,m.ig.leg.se2),type="text")

stargazer(m.pu.leg,m.pu.leg2, m.pres.leg,m.pres.leg2, m.ig.leg,m.ig.leg2, omit=c("congress","party"),
          se=list(m.pu.leg.se,m.pu.leg.se2, m.pres.leg.se,m.pres.leg.se2, m.ig.leg.se,m.ig.leg.se2),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$Partisan.Composition_{ic}$","$ln(Med.Income_{ic})$",
                               "$ln(Population_{ic})$",
                               "$ln(Providers_{ic})*Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          add.lines = list( c("Marginal District", "Yes","No","Yes", "No","Yes","No"),
                            c("Legislator F.E.", "Yes","Yes","Yes", "Yes","Yes","Yes"),
                            c("Congress-Party F.E.","Yes","Yes","Yes","Yes","Yes","Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Dyadic Representation",
          label="MarginalII")



#Marginal Effects Plots



providers.redist <- log(seq(1,10,.1))

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



margins <- Rmargins(m.pu.leg, "district.partisanship","log(providers.redist):district.partisanship", m.pu.leg.vcov, providers.redist)

pdf(file="./Appendix/MarginalPU1.pdf", height=6, width = 6)
plot(exp(margins[,1]), 10*margins[,2], type="l", ylim=c(-15,15),
     ylab="Marginal Effect",
     xlab="Broadband Providers")
points(exp(margins[,1]), 10*margins[,4], type="l", lty=2)
points(exp(margins[,1]), 10*margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$providers[cdr$cong==107])
dev.off()


margins <- Rmargins(m.pres.leg, "district.partisanship","log(providers.redist):district.partisanship", m.pres.leg.vcov, providers.redist)

pdf(file="./Appendix/MarginalPR1.pdf", height=6, width = 6)
plot(exp(margins[,1]), 10*margins[,2], type="l", ylim=c(-15,15),
     ylab="Marginal Effect",
     xlab="Broadband Providers")
points(exp(margins[,1]), 10*margins[,4], type="l", lty=2)
points(exp(margins[,1]), 10*margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$providers[cdr$cong==107])
dev.off()



margins <- Rmargins(m.ig.leg, "district.partisanship","log(providers.redist):district.partisanship", m.ig.leg.vcov, providers.redist)

pdf(file="./Appendix/MarginalIG1.pdf", height=6, width = 6)
plot(exp(margins[,1]), 10*margins[,2], type="l", ylim=c(-15,15),
     ylab="Marginal Effect",
     xlab="Broadband Providers")
points(exp(margins[,1]), 10*margins[,4], type="l", lty=2)
points(exp(margins[,1]), 10*margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$providers[cdr$cong==107])
dev.off()

#Non Marginal


margins <- Rmargins(m.pu.leg2, "district.partisanship","log(providers.redist):district.partisanship", m.pu.leg.vcov2, providers.redist)

pdf(file="./Appendix/MarginalPU2.pdf", height=6, width = 6)
plot(exp(margins[,1]), 10*margins[,2], type="l", ylim=c(-10,10),
     ylab="Marginal Effect",
     xlab="Broadband Providers")
points(exp(margins[,1]), 10*margins[,4], type="l", lty=2)
points(exp(margins[,1]), 10*margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$providers[cdr$cong==107])
dev.off()


margins <- Rmargins(m.pres.leg2, "district.partisanship","log(providers.redist):district.partisanship", m.pres.leg.vcov2, providers.redist)

pdf(file="./Appendix/MarginalPR2.pdf", height=6, width = 6)
plot(exp(margins[,1]), 10*margins[,2], type="l", ylim=c(-10,10),
     ylab="Marginal Effect",
     xlab="Broadband Providers")
points(exp(margins[,1]), 10*margins[,4], type="l", lty=2)
points(exp(margins[,1]), 10*margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$providers[cdr$cong==107])
dev.off()



margins <- Rmargins(m.ig.leg2, "district.partisanship","log(providers.redist):district.partisanship", m.ig.leg.vcov2, providers.redist)

pdf(file="./Appendix/MarginalIG2.pdf", height=6, width = 6)
plot(exp(margins[,1]), 10*margins[,2], type="l", ylim=c(-10,10),
     ylab="Marginal Effect",
     xlab="Broadband Providers")
points(exp(margins[,1]), 10*margins[,4], type="l", lty=2)
points(exp(margins[,1]), 10*margins[,5], type="l", lty=2)
abline(h=0, lty=2, col="gray70")
rug(cdr$providers[cdr$cong==107])
dev.off()

##### Section 9: Testing the Elctoral Connection#######
rm(list=ls())
load(file="PartyUnitySeries.Rdata")

pu <- pu[!is.na(pu$icpsr.id),]
pu <- pu[!is.na(pu$congress),]
pu <- pu[!is.na(pu$dpres),]
pu <- pu[!is.na(pu$dv),]


sessions <- unique(pu$congress)

pu$resid.nat <- NA
coefs <- rep(NA, length(sessions))
se <- rep(NA, length(sessions))

pu.r <- pu[pu$congress==sessions[1],]

m.nat <- lm(dv ~ dpres , data=pu.r)
coefs[1] <- coef(m.nat)["dpres"]
se[1] <- sqrt(vcov(m.nat)["dpres","dpres"])
pu.r$resid.nat <- abs(residuals(m.nat))

for(i in 2: length(sessions)){
  pu.r.loop <- pu[pu$congress==sessions[i],]
  m.nat <- lm(dv ~ dpres , data=pu.r.loop)
  
  coefs[i] <- coef(m.nat)["dpres"]
  se[i] <- sqrt(vcov(m.nat)["dpres","dpres"]) 
  pu.r.loop$resid.nat <- abs(residuals(m.nat))
  
  pu.r <- rbind.data.frame(pu.r, pu.r.loop)
}


head(pu.r)
pu <- pu.r
summary(pu$resid.nat)

library(lfe)

#Models
m.pu1 <- felm(party.unity ~ factor(congress) + resid.nat | icpsr.id | 0 | icpsr.id, data=pu)
m.pu2 <- felm(party.unity ~ factor(congress)*factor(party) + resid.nat | icpsr.id | 0 | icpsr.id, data=pu)


stargazer(m.pu1, m.pu2, omit=c("congress", "party"),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$Local.Residual_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$" ),
          add.lines = list( c("Legislator F.E.", "Yes","Yes"),
                            c("Congress F.E.","Yes","No"),
                            c("Congress-Party F.E", "No", "Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Local Residual on Party Unity Voting",
          label="LocalResidual")



pdf(file="./Figures/LocalResiduals.pdf",
    width=10, height=7)
boxplot(pu$resid.nat ~ pu$congress, outline=F, axes=F, xlab="Congress", ylab="Local Residual")
axis(side=2, at=seq(0,35,5), las=2)
axis(side=1, at=seq(1,31,1),label=seq(83,113,1),las=2)
dev.off()



pu.p <- pu[pu$congress==99,]

pdf(file="./Appendix/99Dyadic.pdf")
plot(pu.p$dpres[pu.p$party==100], pu.p$dv[pu.p$party==100], xlim=c(0,100), ylim=c(0,100), col="darkblue", pch=16,
     xlab="Democratic Vote For President", ylab="Democratic Vote for House", axes=F)
points(pu.p$dpres[pu.p$party==200], pu.p$dv[pu.p$party==200], xlim=c(0,100), ylim=c(0,100), col="firebrick", pch=16)
abline(lm(pu.p$dv~pu.p$dpres))
axis(side=1, at=seq(0,100,20))
axis(side=2, at=seq(0,100,20), las=2)
dev.off()




pu.p <- pu[pu$congress==113,]

pdf(file="./Appendix/113Dyadic.pdf")
plot(pu.p$dpres[pu.p$party==100], pu.p$dv[pu.p$party==100], xlim=c(0,100), ylim=c(0,100), col="darkblue", pch=16,
     xlab="Democratic Vote For President", ylab="Democratic Vote for House", axes=F)
points(pu.p$dpres[pu.p$party==200], pu.p$dv[pu.p$party==200], xlim=c(0,100), ylim=c(0,100), col="firebrick", pch=16)
abline(lm(pu.p$dv~pu.p$dpres))
axis(side=1, at=seq(0,100,20))
axis(side=2, at=seq(0,100,20), las=2)
dev.off()



##### Section 10: Placebo Test#######
rm(list=ls())
load(file="Redistrict Legislator Response Data PLACEBO.Rdata")
names(cdr)

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




stargazer(p.m.mpu.leg,p.m.mpres.leg,p.m.mig.leg, omit=c("congress","party"), se=list( p.m.mpu.leg.se,p.m.mpres.leg.se,p.m.mig.leg.se),
          type="text", report = "vc*p")
stargazer(p.m.mpu.leg,p.m.mpres.leg,p.m.mig.leg, omit=c("congress","party"),
          se=list( p.m.mpu.leg.se,p.m.mpres.leg.se,p.m.mig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$Future.ln(Providers_{ic})$","$ln(Med.Income_{ic})$"
                               ,"$ln(Population_{ic})$",
                               "$Partisan.Composition_{ic}$"),
          dep.m.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.m.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="PlaceboResults")

#Bootstrapping Difference between placebo and main
#load(file="Redistrict Legislator Response Data PLACEBO.Rdata")
#cdr$district.partisanship <- cdr$district.partisanship
#cdr.p <- cdr
#load(file="Redistrict Legislator Response Data.Rdata")
#
#
#pu.boot <- rep(NA, 1000)
#pres.boot <- rep(NA, 1000)
#ig.boot <- rep(NA, 1000)
#
#for(i in 1:1000){
#pairs.m <- sample(unique(cdr$icpsr.id),487,replace=T)
#pairs.p <- sample(unique(cdr.p$icpsr.id),487,replace=T)
#
#b.cdr <- cdr[cdr$icpsr.id==pairs.m[1],]
#b.cdr$boot.id <- NA
#b.cdr$boot.id <- 1
#
#for(j in 2:length(pairs.m)){
#  new.add <- cdr[cdr$icpsr.id==pairs.m[j],]
#  new.add$boot.id <- j
#  b.cdr <- rbind.data.frame(b.cdr,new.add)
#  }
#
#b.cdr.p <- cdr.p[cdr.p$icpsr.id==pairs.p[1],]
#b.cdr.p$boot.id <- NA
#b.cdr.p$boot.id <- 1
#
#for(j in 2:length(pairs.m)){
#  new.add <- cdr.p[cdr.p$icpsr.id==pairs.p[j],]
#  new.add$boot.id <- j
#  b.cdr.p <- rbind.data.frame(b.cdr.p,new.add)
#}
#
#
#    
#pu.p <- coef(plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
#                   + log(pop) + log(med.income) + district.partisanship,
#                   data=b.cdr.p, model="within", index=c("boot.id","congress")))["log(providers.redist)"]
#
#pres.p <- coef(plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
#                     + log(pop) + log(med.income) + district.partisanship,
#                     data=b.cdr.p, model="within", index=c("boot.id","congress")))["log(providers.redist)"]
#
#ig.p <- coef(plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
#                   + log(pop) + log(med.income) + district.partisanship,
#                   data=b.cdr.p, model="within", index=c("boot.id","congress")))["log(providers.redist)"]
#
#
#pu.m<- coef(plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
#                + log(med.income)  + log(pop) + district.partisanship,
#                data=b.cdr, model="within", index=c("boot.id","congress")))["log(providers.redist)"]
#
#
#pres.m <- coef(plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
#                  + log(med.income)  + log(pop) + district.partisanship,
#                  data=b.cdr, model="within", index=c("boot.id","congress")))["log(providers.redist)"]
#
#
#ig.m<- coef(plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
#                + log(med.income) + log(pop) + district.partisanship, data=b.cdr,
#                model="within", index=c("boot.id","congress")))["log(providers.redist)"]
#
#pu.boot[i] <- pu.m - pu.p 
#pres.boot[i] <- pres.m - pres.p 
#ig.boot[i] <- ig.m - ig.p
#}

#save(pu.boot, pres.boot, ig.boot, file="PlaceboBootstrap.Rdata")
load(file="PlaceboBootstrap.Rdata")






quantile(pu.boot, .975,na.rm = T)
quantile(pu.boot, .025,na.rm=T)

quantile(pres.boot, .975)
quantile(pres.boot, .025)

length(pu.boot[pu.boot>0])/length(pu.boot)
length(pres.boot[pres.boot>0])/length(pres.boot)

pdf(file="./Appendix/PlaceboBootstrap.pdf", width=8, height=12)
par(mfrow=c(2,1))
plot(density(pu.boot), xlab="Difference in Coefficients on Main and Placebo Estimates",
     main="Party Unity")
abline(v=quantile(pu.boot, .975), lty=2)
abline(v=quantile(pu.boot, .025), lty=2)

plot(density(pres.boot), xlab="Difference in Coefficients on Main and Placebo Estimates",
     main="Presidential Support")
abline(v=quantile(pres.boot, .975), lty=2)
abline(v=quantile(pres.boot, .025), lty=2)
dev.off()

##### Section 11: District correlates of Broadband Rollout#######
rm(list=ls())
load(file="Panel Legislator Response Analysis Data.Rdata")

library(lfe)



m.partisan <- felm(providers ~ factor(congress) + district.partisanship| cdfip | 0 | cdfip, data=cd)
summary(m.partisan)

m.income <- felm(providers ~ factor(congress) + log(med.income)| cdfip | 0 | cdfip, data=cd)

m.pop <- felm(providers ~ factor(congress) +log(pop)|cdfip | 0 | cdfip, data=cd)

m.pov<- felm(providers ~ factor(congress) + perc.poverty| cdfip | 0 | cdfip, data=cd)

m.white<- felm(providers ~ factor(congress) + perc.white | cdfip | 0 | cdfip, data=cd)

m.black<- felm(providers ~ factor(congress) + perc.black| cdfip | 0 | cdfip, data=cd)

m.bachelor<- felm(providers ~ factor(congress) + perc.bachelor| cdfip | 0 | cdfip, data=cd)

m.age<- felm(providers ~ factor(congress) + med.age| cdfip | 0 | cdfip, data=cd)


stargazer( m.income, m.pop, m.pov, m.white, m.black, m.bachelor, m.age,m.partisan, type="text", report="vc*p")

stargazer(m.income, m.pop, m.pov, m.white, m.black, m.bachelor, m.age,m.partisan, 
          star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("109th Congress","110th Congress","111th Congress", "$ln(Med.Income_{jc})$","$ln(Population_{jc})$",
                               "$Perc.Poverty_{jc}$",
                               "$Perc.White_{jc}$","$Perc.Black_{jc}$","$Perc.Bachelor_{jc}$",
                               "$Med.Age_{jc}$","$Partisan.Composition_{jc}$"),
          dep.var.labels   = "Providers_{jc}",
          add.lines = list(c("District F.E.", "Yes","Yes","Yes", "Yes", "Yes","Yes","Yes", "Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Distrjct Correlates of Broadband Change",
          label="BBChangeCor")


load(file="Redistrict Legislator Response Data.Rdata")
table(cdr$congress)
cdr$perc.bachelor <- cdr$perc.bachelor*100
cdr$perc.poverty <- cdr$perc.poverty*100
cdr$perc.white<- cdr$perc.white*100
cdr$perc.black<- cdr$perc.black*100
cdr$providers.redist

m<- felm(providers.redist ~ factor(congress)| icpsr.id | 0 | icpsr.id, data=cdr)
summary(m)

m.partisan <- felm(providers.redist ~ factor(congress) + district.partisanship| icpsr.id | 0 | icpsr.id, data=cdr)
summary(m.partisan)

m.income <- felm(providers.redist ~ factor(congress) + log(med.income)| icpsr.id | 0 | icpsr.id, data=cdr)

m.pop <- felm(providers.redist ~ factor(congress) +log(pop)|icpsr.id | 0 | icpsr.id, data=cdr)

m.pov<- felm(providers.redist ~ factor(congress) + perc.poverty| icpsr.id | 0 | icpsr.id, data=cdr)

m.white<- felm(providers.redist ~ factor(congress) + perc.white | icpsr.id | 0 | icpsr.id, data=cdr)

m.black<- felm(providers.redist ~ factor(congress) + perc.black| icpsr.id | 0 | icpsr.id, data=cdr)

m.bachelor<- felm(providers.redist ~ factor(congress) + perc.bachelor| icpsr.id | 0 | icpsr.id, data=cdr)

m.age<- felm(providers.redist ~ factor(congress) + med.age| icpsr.id | 0 | icpsr.id, data=cdr)



stargazer( m.income, m.pop, m.pov, m.white, m.black, m.bachelor, m.age,m.partisan, type="text", report="vc*p")

stargazer(m.income, m.pop, m.pov, m.white, m.black, m.bachelor, m.age,m.partisan, 
          star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("108th Congress", "$ln(Med.Income_{jc})$","$ln(Population_{jc})$",
                               "$Perc.Poverty_{jc}$",
                               "$Perc.White_{jc}$","$Perc.Black_{jc}$","$Perc.Bachelor_{jc}$",
                               "$Med.Age_{jc}$","$Partisan.Composition_{jc}$"),
          dep.var.labels   = "$Providers_{jc}$",
          add.lines = list(c("Member F.E.", "Yes","Yes","Yes", "Yes", "Yes","Yes","Yes", "Yes")),
          model.numbers= FALSE,
          notes        = "$^{*}$p $<$ .05; Cluster-Robust Standard Errors", 
          notes.append = FALSE,
          title= "District Correlates of Broadband Post Redistricting",
          label="BBChangeCor2")

##### Section 11: Sensitivity to Population Density#######
rm(list=ls())

##Redistricting
load(file="Redistrict Legislator Response Data.Rdata")

cdr$pop.density <- cdr$pop.density/100

m.pu.leg <- plm(party.unity ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income)  + pop.density + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pu.leg.vcov <- vcovHC(m.pu.leg, method="white1")
m.pu.leg.se <- sqrt(diag(m.pu.leg.vcov))

m.pres.leg <- plm(pres.voting ~  factor(congress)*factor(party)  + log(providers.redist)
                  + log(med.income)  + pop.density + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.pres.leg.vcov <- vcovHC(m.pres.leg, method="white1")
m.pres.leg.se <- sqrt(diag(m.pres.leg.vcov))

m.ig.leg <- plm(ig ~ factor(congress)*factor(party)  + log(providers.redist)
                + log(med.income) + pop.density + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.ig.leg.vcov <- vcovHC(m.ig.leg, method="white1")
m.ig.leg.se <- sqrt(diag(m.ig.leg.vcov))



stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"), type="text",
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se))
stargazer(m.pu.leg,m.pres.leg,m.ig.leg, omit=c("congress","party"),
          se=list( m.pu.leg.se,m.pres.leg.se,m.ig.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$"
                               ,"Population Density (per 100 sq. mile)$_{ic}$",
                               "$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$Party.Unity_{ic}$","$Presidential.Voting_{ic}$", "$Interest.Group.Score_{ic}$" ),
          model.numbers= FALSE,
          add.lines = list(c("Legislator F.E.", "Yes","Yes","Yes"),
                           c("Congress-Party F.E.","Yes","Yes","Yes")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on National Voting Metrics",
          label="PopulationDensity")




##### Section 12: DWNOM Ideology as DV#######
rm(list=ls())
load(file="Redistrict Legislator Response Data.Rdata")

m.ideo1.leg <- plm(ext.ideo1 ~ factor(congress)*factor(party)  + log(providers.redist)
                  + log(med.income) + log(pop) + district.partisanship , data=cdr, model="within", index=c("icpsr.id","congress"))
m.ideo1.leg.vcov <- vcovHC(m.ideo1.leg, method="white1")
m.ideo1.leg.se <- sqrt(diag(m.ideo1.leg.vcov))

m.ideo2.leg <- plm(ext.ideo2 ~ factor(congress)*factor(party)  + log(providers.redist)
                   + log(med.income) + log(pop) + district.partisanship, data=cdr, model="within", index=c("icpsr.id","congress"))
m.ideo2.leg.vcov <- vcovHC(m.ideo2.leg, method="white1")
m.ideo2.leg.se <- sqrt(diag(m.ideo2.leg.vcov))

stargazer(m.ideo1.leg,m.ideo2.leg, omit=c("congress","party"),
          se=list(m.ideo1.leg.se,m.ideo2.leg.se),star.char = c("*"),
          star.cutoffs = c(0.05), style="ajps",
          omit.stat = c("f","adj.rsq"),digits=2,
          covariate.labels = c("$ln(Providers_{ic})$","$ln(Med.Income_{ic})$",
                               "$Perc.Poverty_{ic}$","$ln(Population_{ic})$",
                               "$Partisan.Composition_{ic}$"),
          dep.var.labels   = c("$|DWNOM|$","$|DWNOM - Median(DWNOM)|$" ),
          add.lines = list(c("Legislator F.E.", "Yes", "Yes"),
                           c("Congress-Party F.E.","Yes", "Yes")),
          notes        = "$^{*}$p $<$ .05; Heteroskedastic-Robust Standard Errors", 
          notes.append = FALSE,
          title= "Effect of Communication Environment on Committee Selection",
          label="DWNOM")

#Make correlation table
cor(cdr$ext.ideo1[cdr$cong==108], cdr$party.unity[cdr$cong==108], use="pairwise.complete")

cdr.108 <- cdr[cdr$cong==108,]

keep <- c("ext.ideo1", "party.unity", "pres.voting", "ig","icpsr.id")
cdr.108 <- cdr.108[keep]

cdr.107 <- cdr[cdr$cong==107,]
cdr.107 <- cdr.107[keep]


legs <- unique(cdr$icpsr.id[cdr$icpsr.id %in% cdr.108$icpsr.id & cdr$icpsr.id %in% cdr.107$icpsr.id])

cdr.108 <- cdr.108[cdr.108$icpsr.id %in% legs,]
cdr.107 <- cdr.107[cdr.107$icpsr.id %in% legs,]


cdr.c <- merge(cdr.107, cdr.108, all.x=T, by="icpsr.id")

cdr.c$diff <- cdr.c$ext.ideo1.x -cdr.c$ext.ideo1.y


xtable(cor(cdr.108, use="pairwise.complete"))

