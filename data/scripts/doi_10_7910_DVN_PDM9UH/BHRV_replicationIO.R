# -----------------------------------------------------------------------
# This file tests the relationship between the Millennium Declaration 
# and (1) primary enrollment rates, (2) secondary enrollment rates, and 
# (3) the substitution coefficient from a cointegration. It further 
# characterizes the heterogeneous nature of this relationship over 
# different values of transparency and democratic accountability. 
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# Loading the functions
# -----------------------------------------------------------------------
rm(list=ls())
# Set working directory for where bhrvDAT.RData is saved. setwd(paste(substr(getwd(),1,regexpr("Dropbox",getwd())[1]-1),"Dropbox/coauthors/3_MDGS/mdgpaper/analysis/",sep=""))
getwd()
require(lme4)
require(foreign)
require(dplyr)
require(stargazer)
source("./R-functions.R")
set.seed(1234)


# -----------------------------------------------------------------------
# Loading the data
# -----------------------------------------------------------------------
load("./bhrvDAT.RData")

# -----------------------------------------------------------------
# Summary Statistics - Table 2
#------------------------------------------------------------------
full.sum <- dat.am$imputations$imp1 %>% group_by(mdg) %>% summarise(prim.mean = mean(primraw),prim.sd = sd(primraw),
                                                                    sec.mean = mean(secraw),sec.sd = sd(secraw),
                                                                    gdp.mean = mean(gdppc,na.rm=T), gdp.sd = sd(gdppc,na.rm=T),
                                                                    trans.mean = mean(transparencyindex),trans.sd = sd(transparencyindex),
                                                                    dem.mean = mean(democracy),dem.sd = sd(democracy),
                                                                    aid.mean = mean(AID_TOTAL/pop),aid.sd = sd(AID_TOTAL/pop),
                                                                    aid.prim = mean(AID_Primary_education/pop),aid.prim.sd = sd(AID_Primary_education/pop),
                                                                    aid.sec = mean(AID_Secondary_education/pop),aid.sec.sd = sd(AID_Secondary_education/pop),
                                                                    N = sum(!is.na(wdicode)))
sum.1000 <- dat.am$imputations$imp1 %>% group_by(mdg) %>% 
  filter(gdppc.2000 <= 1000) %>%
  summarise(prim.mean = mean(primraw),prim.sd = sd(primraw),
            sec.mean = mean(secraw),sec.sd = sd(secraw),
            gdp.mean = mean(gdppc,na.rm=T), gdp.sd = sd(gdppc,na.rm=T),
            trans.mean = mean(transparencyindex),trans.sd = sd(transparencyindex),
            dem.mean = mean(democracy),dem.sd = sd(democracy),
            aid.mean = mean(AID_TOTAL/pop),aid.sd = sd(AID_TOTAL/pop),
            aid.prim = mean(AID_Primary_education/pop),aid.prim.sd = sd(AID_Primary_education/pop),
            aid.sec = mean(AID_Secondary_education/pop),aid.sec.sd = sd(AID_Secondary_education/pop),
            N = sum(!is.na(wdicode)))
sum.13000 <- dat.am$imputations$imp1 %>% group_by(mdg) %>% 
  filter(gdppc.2000 <= 13000) %>%
  summarise(prim.mean = mean(primraw),prim.sd = sd(primraw),
            sec.mean = mean(secraw),sec.sd = sd(secraw),
            gdp.mean = mean(gdppc,na.rm=T), gdp.sd = sd(gdppc,na.rm=T),
            trans.mean = mean(transparencyindex),trans.sd = sd(transparencyindex),
            dem.mean = mean(democracy),dem.sd = sd(democracy),
            aid.mean = mean(AID_TOTAL/pop),aid.sd = sd(AID_TOTAL/pop),
            aid.prim = mean(AID_Primary_education/pop),aid.prim.sd = sd(AID_Primary_education/pop),
            aid.sec = mean(AID_Secondary_education/pop),aid.sec.sd = sd(AID_Secondary_education/pop),
            N = sum(!is.na(wdicode)))
stargazer(cbind(t(round(full.sum,2)),t(round(sum.1000,2)),t(round(sum.13000,2))),digits = 2,out = "./Tables/summary-stats.tex")

# -----------------------------------------------------------------------
# Table 3: Non-interacted and interacted results for primary enrollment
# -----------------------------------------------------------------------
(primres11 <- mi.extract(paste("primadjdiff ~ mdg + rgdpch + 
                               primadjlag + (1|year) + (mdg|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(primres12 <- mi.extract(paste("primadjdiff ~ mdg + scale(transparencyindex) + democracy + 
                               scale(log((AID_Total_lag/pop)+1)) + 
                               scale(log((AID_Primary_education_lag/pop)+1)) + 
                               scale(log((AID_Secondary_education_lag/pop)+1)) + 
                               rgdpch + primadjlag + (1|year) + (mdg|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(primres13 <- mi.extract(paste("primadjdiff ~ mdg*democracy + scale(transparencyindex) + 
                               scale(log((AID_Total_lag/pop)+1)) + 
                               scale(log((AID_Primary_education_lag/pop)+1)) + 
                               scale(log((AID_Secondary_education_lag/pop)+1)) + 
                               rgdpch + primadjlag + (1|year) + (mdg|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(primres14 <- mi.extract(paste("primadjdiff ~ mdg*scale(transparencyindex) + democracy + 
                               scale(log((AID_Total_lag/pop)+1)) + 
                               scale(log((AID_Primary_education_lag/pop)+1)) + 
                               scale(log((AID_Secondary_education_lag/pop)+1)) + 
                               rgdpch + primadjlag + (1|year) + (1|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(primres15 <- mi.extract(paste("primadjdiff ~ mdg*(scale(transparencyindex) + democracy) + 
                               scale(log((AID_Total_lag/pop)+1)) + 
                               scale(log((AID_Primary_education_lag/pop)+1)) + 
                               scale(log((AID_Secondary_education_lag/pop)+1)) + 
                               rgdpch + primadjlag + (1|year) + (1|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
primres <- list(primres11,primres12,primres13,primres14,primres15)
p1 <- p2 <- p3 <- p4 <- p5 <- lmer(paste("primadjdiff ~ mdg*(scale(transparencyindex) + democracy) + scale(log((AID_Total_lag/pop)+1)) + scale(log((AID_Primary_education_lag/pop)+1)) + scale(log((AID_Secondary_education_lag/pop)+1)) + rgdpch + trend + primlag + (1|year) + (mdg|wdicode)",sep=""),dat.am$imputations$imp1)

stargazer(p1,p2,p3,p4,p5,
          coef = sapply(1:5,function(x) primres[[x]][,1]),
          se = sapply(1:5,function(x) primres[[x]][,2]),
          t = sapply(1:5,function(x) primres[[x]][,3]),
          keep = c("mdg","transparencyindex",
                   "democracy","AID_Total_lag","AID_Primary_education_lag",
                   "AID_Secondary_education_lag","mdg:scale(transparencyindex)",
                   "mdg:democracy","Constant"),
          covariate.labels = c("MDG","Trans","Dem","Total Aid",
                               "Aid Primary","Aid Secondary",
                               "MDG*Trans","MDG*Dem","Constant"),
          out = "./Tables/resprimary.tex",keep.stat = c("N"),digits = 3)

# -----------------------------------------------------------------------
# Figure 2: Marginal Effects for Democracy and Transparency, primary enrollment
# -----------------------------------------------------------------------
primres15mfxtrans <- mi.extract(paste("primadjdiff ~ mdg*(scale(transparencyindex) + democracy) + 
                                      scale(log((AID_Total_lag/pop)+1)) + 
                                      scale(log((AID_Primary_education_lag/pop)+1)) + 
                                      scale(log((AID_Secondary_education_lag/pop)+1)) + 
                                      rgdpch + primadjlag + (1|year) + (1|wdicode)",sep=""),dat.am$imputations,mfx = T,effect = "mdg",moderator = "scale(transparencyindex)")[[2]]

pdf('./Figures/mfx-primtrans.pdf',width=5,height=7)
plot(primres15mfxtrans[,2],primres15mfxtrans[,1],type='n',main="MDG on Primary over Transparency",
     xlab="Transparency", ylab="Marginal effect of MDG on Primary Enrollment",ylim=c(min(primres15mfxtrans[,3]),max(primres15mfxtrans[,4])))
lines(primres15mfxtrans[,2],primres15mfxtrans[,3],lty=2)
lines(primres15mfxtrans[,2],primres15mfxtrans[,4],lty=2)
lines(primres15mfxtrans[,2],primres15mfxtrans[,1])
abline(h = 0,lty=2)
stars <- ifelse(abs(primres15["mdg:scale(transparencyindex)",4]) < .01,"***",
                ifelse(abs(primres15["mdg:scale(transparencyindex)",4]) < 0.05,"**",
                       ifelse(abs(primres15["mdg:scale(transparencyindex)",4]) < .1,"*","")))
text(par('usr')[ 2 ], par('usr')[ 4 ],adj=c(1.05,1.2),
     labels = paste("Interaction: ",round(primres15["mdg:scale(transparencyindex)",1],3),stars," (",
                    round(primres15["mdg:scale(transparencyindex)",2],3),")",sep=""))
dev.off()

primres15mfxdem <- mi.extract(paste("primadjdiff ~ mdg*(scale(transparencyindex) + democracy) + 
                                    scale(log((AID_Total_lag/pop)+1)) + 
                                    scale(log((AID_Primary_education_lag/pop)+1)) + 
                                    scale(log((AID_Secondary_education_lag/pop)+1)) + 
                                    rgdpch + primadjlag + (1|year) + (1|wdicode)",sep=""),dat.am$imputations,mfx = T,effect = "mdg",moderator = "democracy",num_points = 2)[[2]]

pdf('./Figures/mfx-primdem.pdf',width=5,height=7)
plot(c(1,2),primres15mfxdem[,1],main="MDG on Primary over Democracy",
     xlab="Democracy", ylab="Marginal effect of MDG on Primary Enrollment",ylim=c(min(primres15mfxdem[,3]),max(primres15mfxdem[,4])),xlim=c(.5,2.5),xaxt='n')
axis(1,at = c(1,2),labels = c("Non-Democracy","Democracy"))
segments(c(1,2),primres15mfxdem[,3],c(1,2),primres15mfxdem[,4])
points(c(1,2),primres15mfxdem[,1],pch=21,bg = "white")
abline(h = 0,lty=2)
stars <- ifelse(abs(primres15["mdg:democracy",4]) < .01,"***",
                ifelse(abs(primres15["mdg:democracy",4]) < 0.05,"**",
                       ifelse(abs(primres15["mdg:democracy",4]) < .1,"*","")))
text(par('usr')[ 2 ], par('usr')[ 4 ],adj=c(1.05,1.2),
     labels = paste("Interaction: ",round(primres15["mdg:democracy",1],3),stars," (",
                    round(primres15["mdg:democracy",2],3),")",sep=""))
dev.off()



# -----------------------------------------------------------------------
# Table 4: Non-interacted and interacted results for secondary enrollment
# -----------------------------------------------------------------------
(secres11 <- mi.extract(paste("secdiff ~ mdg + rgdpch + seclag + 
                              (1|year) + (1|wdicode)",sep=""),
                        dat.am$imputations)[[1]])
(secres12 <- mi.extract(paste("secdiff ~ mdg + scale(transparencyindex) + democracy + 
                              scale(log((AID_Total_lag/pop)+1)) + 
                              scale(log((AID_Primary_education_lag/pop)+1)) + 
                              scale(log((AID_Secondary_education_lag/pop)+1)) + 
                              rgdpch + seclag + (1|year) + (1|wdicode)",sep=""),
                        dat.am$imputations)[[1]])
(secres13 <- mi.extract(paste("secdiff ~ mdg*democracy + scale(transparencyindex) + 
                              scale(log((AID_Total_lag/pop)+1)) + 
                              scale(log((AID_Primary_education_lag/pop)+1)) + 
                              scale(log((AID_Secondary_education_lag/pop)+1)) + 
                              rgdpch + seclag + (1|year) + (1|wdicode)",sep=""),
                        dat.am$imputations)[[1]])
(secres14 <- mi.extract(paste("secdiff ~ mdg*scale(transparencyindex) + democracy + 
                              scale(log((AID_Total_lag/pop)+1)) + 
                              scale(log((AID_Primary_education_lag/pop)+1)) + 
                              scale(log((AID_Secondary_education_lag/pop)+1)) + 
                              rgdpch + seclag + (1|year) + (1|wdicode)",sep=""),
                        dat.am$imputations)[[1]])
(secres15 <- mi.extract(paste("secdiff ~ mdg*(scale(transparencyindex) + democracy) + 
                              scale(log((AID_Total_lag/pop)+1)) + 
                              scale(log((AID_Primary_education_lag/pop)+1)) + 
                              scale(log((AID_Secondary_education_lag/pop)+1)) + 
                              rgdpch + seclag + (1|year) + (1|wdicode)",sep=""),
                        dat.am$imputations)[[1]])
secres <- list(secres11,secres12,secres13,secres14,secres15)
p1 <- p2 <- p3 <- p4 <- p5 <- lmer(paste("secadjdiff ~ mdg*(scale(transparencyindex) + democracy) + scale(log((AID_Total_lag/pop)+1)) + scale(log((AID_Primary_education_lag/pop)+1)) + scale(log((AID_Secondary_education_lag/pop)+1)) + rgdpch + trend + secadjlag + (1|year) + (1|wdicode)",sep=""),dat.am$imputations$imp1)

stargazer(p1,p2,p3,p4,p5,
          coef = sapply(1:5,function(x) secres[[x]][,1]),
          se = sapply(1:5,function(x) secres[[x]][,2]),
          t = sapply(1:5,function(x) secres[[x]][,3]),
          keep = c("mdg","transparencyindex",
                   "democracy","AID_Total_lag/pop","AID_Primary_education_lag/pop",
                   "AID_Secondary_education_lag/pop","mdg:scale(transparencyindex)",
                   "mdg:democracy","Constant"),
          covariate.labels = c("MDG","Trans","Dem","Total Aid",
                               "Aid Primary","Aid Secondary",
                               "MDG*Trans","MDG*Dem","Constant"),
          out = "./Tables/ressecter.tex",keep.stat = c("N"),digits = 3)

# -----------------------------------------------------------------------
# Table 5: Distance measure
# -----------------------------------------------------------------------
(distres11 <- mi.extract(paste("dist.raw ~ mdg + rgdpch + trend + (1|year) + (1|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(distres12 <- mi.extract(paste("dist.raw ~ mdg + rgdpch + trend + scale(transparencyindex) + 
                               democracy + (1|year) + (1|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(distres13 <- mi.extract(paste("dist.raw ~ mdg + rgdpch + trend + scale(transparencyindex) + 
                               democracy + scale(log((AID_Total_lag/pop)+1)) + 
                               scale(log((AID_Primary_education_lag/pop)+1)) + 
                               scale(log((AID_Secondary_education_lag/pop)+1)) + 
                               (1|year) + (1|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(distres14 <- mi.extract(paste("dist.raw ~ mdg*scale(transparencyindex) + democracy + rgdpch + trend +
                               scale(log((AID_Total_lag/pop)+1)) + 
                               scale(log((AID_Primary_education_lag/pop)+1)) + 
                               scale(log((AID_Secondary_education_lag/pop)+1)) + 
                               (1|year) + (1|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(distres15 <- mi.extract(paste("dist.raw ~ mdg*democracy + scale(transparencyindex) + rgdpch + trend +
                               scale(log((AID_Total_lag/pop)+1)) + 
                               scale(log((AID_Primary_education_lag/pop)+1)) + 
                               scale(log((AID_Secondary_education_lag/pop)+1)) + 
                               (1|year) + (1|wdicode)",sep=""),
                         dat.am$imputations)[[1]])
(distres16 <- mi.extract(paste("dist.raw ~ mdg*(scale(transparencyindex) + democracy) + 
                               rgdpch + trend + scale(log((AID_Total_lag/pop)+1)) + 
                               scale(log((AID_Primary_education_lag/pop)+1)) + 
                               scale(log((AID_Secondary_education_lag/pop)+1)) + 
                               (1|year) + (1|wdicode)",sep=""),dat.am$imputations)[[1]])
distres <- list(distres11,distres12,distres13,distres14,distres15,distres16)
p1 <- p2 <- p3 <- p4 <- p5 <- p6 <- lmer(paste("dist.raw ~ mdg*(scale(transparencyindex) + democracy) + scale(log((AID_Total_lag/pop)+1)) + scale(log((AID_Primary_education_lag/pop)+1)) + scale(log((AID_Secondary_education_lag/pop)+1)) + rgdpch + trend + (1|year) + (1|wdicode)",sep=""),dat.am$imputations$imp1)

stargazer(p1,p2,p3,p4,p5,p6,
          coef = sapply(1:5,function(x) distres[[x]][,1]),
          se = sapply(1:5,function(x) distres[[x]][,2]),
          t = sapply(1:5,function(x) distres[[x]][,3]),
          keep = c("mdg","transparencyindex",
                   "democracy","AID_Total_lag/pop","AID_Primary_education_lag/pop",
                   "AID_Secondary_education_lag/pop","mdg:scale(transparencyindex)",
                   "mdg:democracy","Constant"),
          covariate.labels = c("MDG","Trans","Dem","Total Aid",
                               "Aid Primary","Aid Secondary",
                               "MDG*Trans","MDG*Dem","Constant"),
          out = "./Tables/resdist.tex",keep.stat = c("N"),digits = 3)


# -----------------------------------------------------------------------
# Table 6: linear models and fixed effects
# -----------------------------------------------------------------------
# Estimating the relationship wrt delta beta
(beta.d1 <- mi.extract(paste("beta ~ mdg + log.gdppc.2000 + scale(log((AID_Total_mean)+1)) + 
                              scale(log((AID_Primary_education_mean)+1)) + 
                             scale(log((AID_Secondary_education_mean)+1)) + (1|wdicode)",sep=""),ECM.am)[[1]])
(beta.d2 <- mi.extract(paste("beta ~ mdg*dem + log.gdppc.2000 + scale(log((AID_Total_mean)+1)) + 
                             scale(log((AID_Primary_education_mean)+1)) + 
                             scale(log((AID_Secondary_education_mean)+1)) + (1|wdicode)",sep=""),ECM.am)[[1]])
(beta.d3 <- mi.extract(paste("beta ~ mdg*trans + log.gdppc.2000 + scale(log((AID_Total_mean)+1)) + 
                             scale(log((AID_Primary_education_mean)+1)) + 
                             scale(log((AID_Secondary_education_mean)+1)) + (1|wdicode)",sep=""),ECM.am)[[1]])
(beta.d4 <- mi.extract(paste("beta ~ mdg*(trans + dem) + log.gdppc.2000 + 
                             scale(log((AID_Total_mean)+1)) + 
                             scale(log((AID_Primary_education_mean)+1)) + 
                             scale(log((AID_Secondary_education_mean)+1)) + (1|wdicode)",sep=""),ECM.am)[[1]])

betares <- list(beta.d1,beta.d2,beta.d3,beta.d4)
p1 <- p2 <- p3 <- p4 <- lmer(paste("beta ~ mdg*(trans + dem) + log.gdppc.2000 + scale(log((AID_Total_mean)+1)) + scale(log((AID_Primary_education_mean)+1)) + scale(log((AID_Secondary_education_mean)+1)) + (1|wdicode)",sep=""),ECM.am[[1]])

stargazer(p1,p2,p3,p4,
          coef = sapply(1:4,function(x) betares[[x]][,1]),
          se = sapply(1:4,function(x) betares[[x]][,2]),
          t = sapply(1:4,function(x) betares[[x]][,3]),
          keep = c("mdg","trans","dem","AID_Total_mean",
                   "AID_Primary_education_mean","AID_Secondary_education_mean",
                   "mdg:trans",
                   "mdg:dem","Constant"),
          covariate.labels = c("MDG","Trans","Dem","Total Aid",
                               "Aid Primary","Aid Secondary",
                               "MDG*Trans","MDG*Dem","Constant"),
          out = "./Tables/resECM.tex",keep.stat = c("N"),digits = 3)

# -----------------------------------------------------------------------
# Figure 3: Mediation effect of foreign aid
# -----------------------------------------------------------------------
# Aid as an outcome
(aidres01 <- mi.extract(model = paste("scale(AID_totdiff_pc) ~ mdg*primenroll.2000 + 
                                      rgdpch + log(gdppc.2000) + 
                                      scale(AID_Total_pc_lag) + (1|year) + (1|wdicode)",sep=""),
                        im.dat = dat.am$imputations)[[1]])

(aidres11 <- mi.extract(paste("scale(AID_educ_totaldiff_pc) ~ mdg*primenroll.2000 + 
                              rgdpch + log(gdppc.2000) + 
                              scale(AID_educ_total_pc_lag) + (1|year) + (1|wdicode)",sep=""),
                        dat.am$imputations)[[1]])

(aidres21 <- mi.extract(paste("scale(AID_primdiff_pc) ~ mdg*primenroll.2000 + 
                              rgdpch + log(gdppc.2000) + 
                              scale(AID_Primary_education_pc_lag) + (1|year) + (1|wdicode)",sep=""),dat.am$imputations)[[1]])

(aidres31 <- mi.extract(paste("scale(AID_secdiff_pc) ~ mdg*primenroll.2000 + 
                              rgdpch + log(gdppc.2000) + 
                              scale(AID_Secondary_education_pc_lag) + (1|year) + (1|wdicode)",sep=""),dat.am$imputations)[[1]])

(aidres41 <- mi.extract(paste("scale(AID_highdiff_pc) ~ mdg*primenroll.2000 + 
                              rgdpch + log(gdppc.2000) + 
                              scale(AID_Higher_education_pc_lag) + (1|year) + (1|wdicode)",sep=""),dat.am$imputations)[[1]])


pct25 <- mean(unlist(lapply(dat.am$imputations,function(d) quantile(d$primenroll.2000,na.rm=T,p = .25))))
substs <- lapply(1:length(dat.am$imputations), function(x) which(dat.am$imputations[[x]]$primenroll.2000 <= quantile(dat.am$imputations[[x]]$primenroll.2000,na.rm=T,p = .25)))
res.dat <- list()
for(i in 1:length(dat.am$imputations)) {
  res.dat[[i]] <- dat.am$imputations[[i]][substs[[i]],]
}

aidres22 <- mi.extract(paste("scale(AID_primdiff_pc) ~ mdg*primenroll.2000 + 
                             rgdpch + log(gdppc.2000) + 
                             scale(AID_Primary_education_pc_lag) + (1|year) + (1|wdicode)",sep=""),
                       res.dat)


pdf("./Figures/aid-mdgregs.pdf",width = 6,height = 6)
par(mar=c(5,9,3,1))
plot(1:5,rep(0,5),type='n',xlab="MDG Coefficient",ylab = "",yaxt='n',main = "Aid Measures Regressed on MDG Adoption",
     ylim=c(0.5,5.5),xlim=c(-.55,.95))
abline(v = 0,lty=2)
axis(2,at = 1:5,labels = c("Total Aid","Total Educ. Aid","Primary Educ.","Secondary Educ.","Higher Educ."),las = 1)
segments(aidres01["mdg",1] - 2*aidres01["mdg",2],1,aidres01["mdg",1] + 2*aidres01["mdg",2])
segments(aidres01["mdg",1] - 1.65*aidres01["mdg",2],1,aidres01["mdg",1] + 1.65*aidres01["mdg",2],lwd=2)
points(aidres01["mdg",1],1,pch=21,bg = "white")
segments(aidres11["mdg",1] - 2*aidres11["mdg",2],2,aidres11["mdg",1] + 2*aidres11["mdg",2])
segments(aidres11["mdg",1] - 1.65*aidres11["mdg",2],2,aidres11["mdg",1] + 1.65*aidres11["mdg",2],lwd=2)
points(aidres11["mdg",1],2,pch=21,bg = "white")
segments(aidres21["mdg",1] - 2*aidres21["mdg",2],3,aidres21["mdg",1] + 2*aidres21["mdg",2])
segments(aidres21["mdg",1] - 1.65*aidres21["mdg",2],3,aidres21["mdg",1] + 1.65*aidres21["mdg",2],lwd=2)
points(aidres21["mdg",1],3,pch=21,bg = "white")
segments(aidres22[[1]]["mdg",1] - 2*aidres22[[1]]["mdg",2],2.85,
         aidres22[[1]]["mdg",1] + 2*aidres22[[1]]["mdg",2],col = "grey50")
segments(aidres22[[1]]["mdg",1] - 1.65*aidres22[[1]]["mdg",2],2.85,
         aidres22[[1]]["mdg",1] + 1.65*aidres22[[1]]["mdg",2],lwd=2,col = "grey50")
points(aidres22[[1]]["mdg",1],2.85,pch=21,bg = "grey50",col = "white")
segments(aidres31["mdg",1] - 2*aidres31["mdg",2],4,aidres31["mdg",1] + 2*aidres31["mdg",2])
segments(aidres31["mdg",1] - 1.65*aidres31["mdg",2],4,aidres31["mdg",1] + 1.65*aidres31["mdg",2],lwd=2)
points(aidres31["mdg",1],4,pch=21,bg = "white")
segments(aidres41["mdg",1] - 2*aidres41["mdg",2],5,aidres41["mdg",1] + 2*aidres41["mdg",2])
segments(aidres41["mdg",1] - 1.65*aidres41["mdg",2],5,aidres41["mdg",1] + 1.65*aidres41["mdg",2],lwd=2)
points(aidres41["mdg",1],5,pch=21,bg = "white")
legend("bottomright",legend = c("Full Data","25th %ile"),pch = 21,
       lwd = 1,col = c("black","grey50"),pt.bg = c("white","grey50"),
       cex = .8)
dev.off()



pvals <- NA
j <- 1
delt <- list(NA,NA)
subst = substs
for(a in c("ALL","rest")) { # Using the subst object created above for the lowest performers in 2000
  for(i in 1:200) {
    if(a != "ALL") {
      boot.dat <- res.dat
    } else {
      boot.dat <- dat.am$imputations
    }
    (reg.1 <- mi.extract(model = paste("scale(AID_primdiff_pc) ~ mdg*primenroll.2000 + 
                                       scale(transparencyindex) + democracy + rgdpch + 
                                       scale(AID_Primary_education_pc_lag) + (1|year) + (1|wdicode)",
                                       sep=""),im.dat = boot.dat,boot = T))
    bet <- reg.1[[1]]['mdg',1]
    (reg.2 <- mi.extract(paste("scale(primadjdiff) ~ mdg + scale(AID_primdiff_pc) + 
                               scale(transparencyindex) + democracy + rgdpch + 
                               primadjlag + (1|year) + (1|wdicode)",
                               sep=""),boot.dat,boot = T))
    gam <- reg.2[[1]][paste("scale(AID_primdiff_pc)",sep=""),1]
    delt[[j]][i] <- bet*gam
    cat(paste(i,"\n",sep=""))
  }
  pvals[j] <- round(min(length(which(delt[[j]] > 0))/length(delt[[j]]),1-length(which(delt[[j]] > 0))/length(delt[[j]])),2)
  j <- j+1
}


pdf("./Figures/aid-medprim.pdf",height = 6,width = 6)
plot(density(delt[[1]],adjust = 3),xlim = c(-.025,.025),
     main = "Mediation Analysis: Aid and Primary Enrollment",
     xlab = "AME: 200 Bootstrapped Samples",
     ylab = "Density",lwd = 2)
par(new = T)
plot(density(delt[[2]],adjust = 3),xlim = c(-.025,.025),
     main = "",xlab = "",ylab = "",xaxt='n',yaxt='n',bty = 'n',lty = 2)
abline(v = 0,lty=1)
legend("topright",
       legend=paste(c("Full Data","25th %ile"),": ",pvals,sep=""),
       lty=1:3,lwd = c(2,1,1),cex=.8)
abline(v = 0)
dev.off()
