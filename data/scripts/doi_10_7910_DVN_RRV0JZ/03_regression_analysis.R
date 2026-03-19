####
#Author: M.R. Kenwick
#Date: Dec 28, 2020
#Purpose: Conducts regression analysis and related plots.
####
#Install packages, as necessary
#install.packages('haven')
#install.packages('dplyr')
#install.packages('prais')
#install.packages('ggplot2')

rm(list=ls())
library(haven)
library(dplyr)
library(prais)
library(ggplot2)

#Set working directory to top level of replication folder
setwd()
##Residualization (Table B1, Figure B1)
#Load regression data and latent estiamtes
data<-read.csv('regression/regression_data.csv')
data<-arrange(data,state1,state2,year,borderid)
load("model/bo_static_output.RData")
#Summary indicators from latent data
data$theta_up<-data$theta + (1.96*data$theta_sd)
data$theta_lw<-data$theta - (1.96*data$theta_sd)
data<-cbind(as.data.frame(data),t(output$theta[1:500,]))

#Residualization
#Note: R^2 statistics reported in fn. 17 of the main text
#Directed border crossing year model for residualization (Table B1, Model 1)
fit1<-lm(theta~elevation+lngdp1,data=data,na.action = na.exclude)
summary(fit1)
round(fit1$coefficients,3)
residuals_c<-residuals(fit1)
data$resid<-residuals(fit1)

#Directed Dyad Year model for residualization (Table B1, Model 2)
data_collapsed<- as.data.frame(data %>%
                                 dplyr::select(ddyad,year,xi,elevation,lngdp1) %>%
                                 group_by(ddyad,year) %>%
                                 mutate(elevation = mean(elevation,na.rm=T))  %>%
                                 mutate(lngdp1 = mean(lngdp1,na.rm=T))  %>%
                                 mutate(xi = mean(xi)))  %>%
                                 unique()
fit2<-lm(xi~elevation+lngdp1,data=data_collapsed,na.action = na.exclude)
summary(fit2)
round(fit2$coefficients,3)
residuals<-residuals(fit2)

#Figure B1
#Note: correlations also reported in fn. 17 of the main text. 
pdf("plots/FigB_1.pdf",height=8,width=16.6)
par(mar=c(4.5,4.5,4.5,4.5),mfrow=c(1,2))
plot(NULL,
     ylim = c(-3,3), 
     xlim = c(-3,3), 
     axes = F, xlab = NA, ylab = NA)
grid()
reg<-lm(na.omit(residuals_c)~data$theta[!is.na(residuals_c)])
abline(reg, col="#d73027",lwd=1.5)
points(data$theta[!is.na(residuals_c)],na.omit(residuals_c),
       bg=alpha("#74add1",.3),col=alpha("#313695",.3), cex=1, pch=21,lwd=1.5)
axis(side=2, las=1, cex.axis=1.25)
axis(side=1, las=1,cex.axis=1.25)
mtext("Border Orientation at the Crossing Level", side = 1,line=2.5, cex=1) 
mtext("Border Orientation Residualized on \nGDP and Elevation", side = 2, line=2.25,cex=1) 
box()
text(-3, 2.75,  
     bquote(rho==.(round(cor(
       data$theta[!is.na(residuals_c)],na.omit(residuals_c),
       method="spearman",
       use="pairwise.complete.obs"),3)
     ))
     ,cex=1.5,col="#d73027",pos=4)
plot(NULL,
     ylim = c(-2,2), 
     xlim = c(-2,2), 
     axes = F, xlab = NA, ylab = NA)
grid()
reg<-lm(na.omit(residuals)~data_collapsed$xi[!is.na(residuals)])
abline(reg, col="#d73027",lwd=1.5)
points(data_collapsed$xi[!is.na(residuals)],na.omit(residuals),
       bg=alpha("#74add1",.3),col=alpha("#313695",.3), cex=1, pch=21,lwd=1.5)
axis(side=2, las=1, cex.axis=1.25)
axis(side=1, las=1,cex.axis=1.25)
mtext("Border Orientation at the Directed Dyad Level", side = 1,line=2.5, cex=1) 
mtext("Border Orientation Residualized on \nGDP and Elevation", side = 2, line=2.25,cex=1) 
box()
text(-2, 1.75,  
     bquote(rho==.(round(cor(
       data_collapsed$xi[!is.na(residuals)],na.omit(residuals),
       method="spearman",
       use="pairwise.complete.obs"),3)
     ))
     ,cex=1.5,col="#d73027",pos=4)
dev.off()


#Table 2: Correlates of Border Orientation
rm(list=ls())
data_c<-read.csv('regression/regression_data.csv')
#Aggregate to directed dyad years
data<- data_c %>% 
  group_by(ddyad,year) %>% 
  summarize(xi = mean(xi,na.rm=T),
            xi_neighbor_lag = mean(xi_neighbor_lag,na.rm=T),
            xi_lag = mean(xi_lag,na.rm=T),
            elevation = mean(elevation,na.rm=T),
            pop = mean(pop,na.rm=T),
            lngdp1  = mean(lngdp1,na.rm=T),
            lngdp_diff = mean(lngdp_diff,na.rm=T),
            cultural_homog = mean(cultural_homog,na.rm=T),
            relig_1same_2diff  = mean(relig_1same_2diff,na.rm=T),
            relig_1diff_2diff  = mean(relig_1diff_2diff,na.rm=T),
            relig_1diff_2same = mean(relig_1diff_2same,na.rm=T),
            polity2_1  = mean(polity2_1,na.rm=T),
            mid_count  = mean(mid_count,na.rm=T),
            civ2  = mean(civ2,na.rm=T),
            teriss = mean(teriss,na.rm=T),
            foreign_born_pct = mean(foreign_born_pct,na.rm=T),
            chng_fborn_pct  = mean(chng_fborn_pct,na.rm=T),
            elevation  = mean(elevation,na.rm=T),
            schengen1  = mean(schengen1,na.rm=T),
            kofgidf_5yr_chng  = mean(KOFGIdf_5yr_chng,na.rm=T),
            gtd_count_10yr_ma  = mean(gtd_count_10yr_ma,na.rm=T)
  )
#fn. 25 (proportion of countries with different primary but same secondary religion)
table(data$relig_1diff_2same)
nrow(subset(data,data$relig_1diff_2same==1))/nrow(data[!is.na(data$relig_1diff_2same),])


#Model Specification Variables
vars1<-c('xi','cultural_homog','foreign_born_pct','chng_fborn_pct',
         'relig_1same_2diff','relig_1diff_2diff',
         'lngdp1','elevation','polity2_1','schengen1','xi_neighbor_lag','ddyad','year')
vars2<-c('xi','cultural_homog','foreign_born_pct','chng_fborn_pct',
         'relig_1same_2diff','relig_1diff_2diff',
         'lngdp1','elevation','polity2_1','schengen1','xi_neighbor_lag','ddyad','year',
         'kofgidf_5yr_chng','lngdp_diff')
vars3<-c('xi','cultural_homog','foreign_born_pct','chng_fborn_pct',
         'relig_1same_2diff','relig_1diff_2diff',
         'lngdp1','elevation','polity2_1','schengen1','xi_neighbor_lag','ddyad','year',
         'kofgidf_5yr_chng','lngdp_diff','mid_count','civ2','gtd_count_10yr_ma')
vars4<-c('xi','cultural_homog','foreign_born_pct','chng_fborn_pct',
         'relig_1same_2diff','relig_1diff_2diff',
         'lngdp1','elevation','polity2_1','schengen1','xi_neighbor_lag','ddyad','year',
         'kofgidf_5yr_chng','lngdp_diff','mid_count','civ2','gtd_count_10yr_ma','teriss')
vars5<-c('xi','cultural_homog','foreign_born_pct','chng_fborn_pct',
         'relig_1same_2diff','relig_1diff_2diff',
         'lngdp1','elevation','polity2_1','schengen1','xi_neighbor_lag','ddyad','year',
         'kofgidf_5yr_chng','lngdp_diff','mid_count','civ2','gtd_count_10yr_ma','pop')
data.m1<-na.omit(data[,vars1])
data.m2<-na.omit(data[,vars2])
data.m3<-na.omit(data[,vars3])
data.m4<-na.omit(data[,vars4])
data.m5<-na.omit(data[,vars5])
f1<- xi~cultural_homog+foreign_born_pct+chng_fborn_pct+
  relig_1same_2diff+relig_1diff_2diff+
  lngdp1+elevation+polity2_1+schengen1+xi_neighbor_lag
f2<- xi~cultural_homog+foreign_born_pct+chng_fborn_pct+
  relig_1same_2diff+relig_1diff_2diff+
  lngdp1+elevation+polity2_1+schengen1+xi_neighbor_lag+
  kofgidf_5yr_chng+lngdp_diff
f3<- xi~cultural_homog+foreign_born_pct+chng_fborn_pct+
  relig_1same_2diff+relig_1diff_2diff+
  lngdp1+elevation+polity2_1+schengen1+xi_neighbor_lag+
  kofgidf_5yr_chng+lngdp_diff+mid_count+civ2+gtd_count_10yr_ma
f4<- xi~cultural_homog+foreign_born_pct+chng_fborn_pct+
  relig_1same_2diff+relig_1diff_2diff+
  lngdp1+elevation+polity2_1+schengen1+xi_neighbor_lag+
  kofgidf_5yr_chng+lngdp_diff+mid_count+civ2+gtd_count_10yr_ma+teriss
f5<- xi~cultural_homog+foreign_born_pct+chng_fborn_pct+
  relig_1same_2diff+relig_1diff_2diff+
  lngdp1+elevation+polity2_1+schengen1+xi_neighbor_lag+
  kofgidf_5yr_chng+lngdp_diff+mid_count+civ2+gtd_count_10yr_ma+pop


####Table 2: Primary Regression Analysis
pw1<-prais_winsten(f1,data=data.m1,index=c('ddyad','year'),twostep=T)
pw2<-prais_winsten(f2,data=data.m2,index=c('ddyad','year'),twostep=T)
pw3<-prais_winsten(f3,data=data.m3,index=c('ddyad','year'),twostep=T)
pw4<-prais_winsten(f4,data=data.m4,index=c('ddyad','year'),twostep=T)
pw5<-prais_winsten(f5,data=data.m5,index=c('ddyad','year'),twostep=T)
summary(pw1)
summary(pw2)
summary(pw3)
summary(pw4)
summary(pw5)


####Table C1: Standard models, no TS adjustments
m1<-lm(f1,data=data.m1)
m2<-lm(f2,data=data.m2)
m3<-lm(f3,data=data.m3)
m4<-lm(f4,data=data.m4)
m5<-lm(f5,data=data.m5)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

####Table C2: Crossing-level regressions
fit1<-lm(theta~cultural_homog+foreign_born_pct+chng_fborn_pct+relig_1same_2diff+
     relig_1diff_2diff+lngdp1++polity2_1+elevation+schengen1+theta_neighbor_lag, data=data_c)
fit2<-lm(theta~cultural_homog+foreign_born_pct+chng_fborn_pct+relig_1same_2diff+
     relig_1diff_2diff+KOFGIdf_5yr_chng+lngdp_diff+lngdp1++polity2_1+elevation+
     schengen1+theta_neighbor_lag, data=data_c)
fit3<-lm(theta~cultural_homog+foreign_born_pct+chng_fborn_pct+relig_1same_2diff+
     relig_1diff_2diff+KOFGIdf_5yr_chng+lngdp_diff+mid_count+civ2+gtd_count_10yr_ma+
     lngdp1++polity2_1+elevation+schengen1+theta_neighbor_lag, data=data_c)
fit4<-lm(theta~cultural_homog+foreign_born_pct+chng_fborn_pct+relig_1same_2diff+
     relig_1diff_2diff+KOFGIdf_5yr_chng+lngdp_diff+mid_count+civ2+gtd_count_10yr_ma+
     teriss+lngdp1++polity2_1+elevation+schengen1+theta_neighbor_lag, data=data_c)
fit5<-lm(theta~cultural_homog+foreign_born_pct+chng_fborn_pct+relig_1same_2diff+relig_1diff_2diff+
           KOFGIdf_5yr_chng+lngdp_diff+mid_count+civ2+gtd_count_10yr_ma+lngdp1++polity2_1+
     elevation+schengen1+theta_neighbor_lag+pop, data=data_c)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)


#Fig C1: Cross Sectional Regressions 1
#Cross-sectional regressions
data.m1.cs<-subset(data.m1,year==2017)
data.m2.cs<-subset(data.m2,year==2017)
data.m3.cs<-subset(data.m3,year==2017)
data.m4.cs<-subset(data.m4,year==2002)
data.m5.cs<-subset(data.m5,year==2017)
m1cs<-glm(f1,data=data.m1.cs)
m2cs<-glm(f2,data=data.m2.cs)
m3cs<-glm(f3,data=data.m3.cs)
m4cs<-glm(f4,data=data.m4.cs)
m5cs<-glm(f5,data=data.m5.cs)

data.m1.cs<-subset(data.m1,year==2017)
m1cs<-glm(f1,data=data.m1.cs)
out <- summary(m1cs)
out$coefficients[ , 2]
NAMES<-c('Intercept','Cultural Homogeneity','Percent Foreign Born Population','Change in Foreign Born Percent',
         'Same Primary Religion, Differnet Secondary','Different Primary and Secondary Religion',
         'Logged GDP Per Capita','Elevation','Polity','Schengen','Neighbor Border Orientation')

pdf("plots/FigC_1.pdf",height=8,width=7)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(6,2,2,2),
  mfrow= c(1,1)
)
plot(NULL,
     xlim = c(-.5,.5),
     ylim = c(.5,length(m1cs$coefficients)-1.5),
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 2:length(m1cs$coefficients)){
  lines(c(m1cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m1cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(11-i,11-i),col="#a50f15", lwd=1.5)
  points(m1cs$coefficients[i],length(m1cs$coefficients)-i,bg=c("#fee5d9"), col="#a50f15",  cex=.65, pch=21 )
  text(m1cs$coefficients[i],length(m1cs$coefficients)-i,NAMES[i],pos=3, cex=.75 )
}

data.m1.cs<-subset(data.m1,year==2015)
m1cs<-glm(f1,data=data.m1.cs)
out <- summary(m1cs)
out$coefficients[ , 2]
for (i in 2:length(m1cs$coefficients)){
  lines(c(m1cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m1cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(11-i-.1,11-i-.1),col="#de2d26", lwd=1.5)
  points(m1cs$coefficients[i],length(m1cs$coefficients)-i-.1,bg=c("#fee5d9"), col="#de2d26",  cex=.65, pch=21 )
}

data.m1.cs<-subset(data.m1,year==2010)
m1cs<-glm(f1,data=data.m1.cs)
out <- summary(m1cs)
out$coefficients[ , 2]
for (i in 2:length(m1cs$coefficients)){
  lines(c(m1cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m1cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(11-i-.2,11-i-.2),col="#fb6a4a", lwd=1.5)
  points(m1cs$coefficients[i],length(m1cs$coefficients)-i-.2,bg=c("#fee5d9"), col="#fb6a4a",  cex=.65, pch=21 )
}

data.m1.cs<-subset(data.m1,year==2005)
m1cs<-glm(f1,data=data.m1.cs)
out <- summary(m1cs)
out$coefficients[ , 2]
for (i in 2:length(m1cs$coefficients)){
  lines(c(m1cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m1cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(11-i-.3,11-i-.3),col="#fc9272", lwd=1.5)
  points(m1cs$coefficients[i],length(m1cs$coefficients)-i-.3,bg=c("#fee5d9"), col="#fc9272",  cex=.65, pch=21 )
}

data.m1.cs<-subset(data.m1,year==2000)
m1cs<-glm(f1,data=data.m1.cs)
out <- summary(m1cs)
out$coefficients[ , 2]
for (i in 2:length(m1cs$coefficients)){
  lines(c(m1cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m1cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(11-i-.4,11-i-.4),col="#fcbba1", lwd=1.5)
  points(m1cs$coefficients[i],length(m1cs$coefficients)-i-.4,bg=c("#fee5d9"), col="#fcbba1",  cex=.65, pch=21 )
}
#
axis(1,cex.axis=.8)
mtext(side = 1, "Coefficient Value", line =2.5,cex=1) 
text(.4,9,"2017",col="#a50f15",pos=2)
text(.4,8.8,"2015",col="#de2d26",pos=2)
text(.4,8.6,"2010",col="#fb6a4a",pos=2)
text(.4,8.4,"2005",col="#fc9272",pos=2)
text(.4,8.2,"2000",col="#fcbba1",pos=2)
dev.off()


#Fig C2: Cross Sectional Regressions 2
f3<- xi~cultural_homog+foreign_born_pct+chng_fborn_pct+
  relig_1same_2diff+relig_1diff_2diff+
  kofgidf_5yr_chng+lngdp_diff+mid_count+civ2+gtd_count_10yr_ma+
  lngdp1+polity2_1+elevation+schengen1+xi_neighbor_lag
data.m3.cs<-subset(data.m3,year==2017)
m3cs<-glm(f3,data=data.m3.cs)
out <- summary(m3cs)
out$coefficients[ , 2]
NAMES<-c('Intercept','Cultural Homogeneity','Percent Foreign Born Population','Change in Foreign Born Percent',
         'Same Primary Religion, Differnet Secondary','Different Primary and Secondary Religion',
         'Five-year Change in Glogalization','Difference in ln(GDPpc) with Neighbor',
         'Count of MIDs with Neighbor, 1980-1999','Count of Civil Conflicts with Neighbor, 1980-1999',
         'Logged Number of Terrorist Events (10yr avg)',
         'Logged GDP Per Capita','Polity','Elevation','Schengen','Neighbor Border Orientation')

pdf("plots/figC_2.pdf",height=10,width=7)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(6,2,2,2),
  mfrow= c(1,1)
)
plot(NULL,
     xlim = c(-.4,.4),
     ylim = c(1,length(m3cs$coefficients)-1.5),
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 2:length(m3cs$coefficients)){
  lines(c(m3cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m3cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(16-i,16-i),col="#a50f15", lwd=1.5)
  points(m3cs$coefficients[i],length(m3cs$coefficients)-i,bg=c("#fee5d9"), col="#a50f15",  cex=.65, pch=21 )
  text(m3cs$coefficients[i],length(m3cs$coefficients)-i,NAMES[i],pos=3, cex=.75 )
}

data.m3.cs<-subset(data.m3,year==2015)
m3cs<-glm(f3,data=data.m3.cs)
out <- summary(m3cs)
out$coefficients[ , 2]
for (i in 2:length(m3cs$coefficients)){
  lines(c(m3cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m3cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(16-i-.1,16-i-.1),col="#de2d26", lwd=1.5)
  points(m3cs$coefficients[i],length(m3cs$coefficients)-i-.1,bg=c("#fee5d9"), col="#de2d26",  cex=.65, pch=21 )
}

data.m3.cs<-subset(data.m3,year==2010)
m3cs<-glm(f3,data=data.m3.cs)
out <- summary(m3cs)
out$coefficients[ , 2]
for (i in 2:length(m3cs$coefficients)){
  lines(c(m3cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m3cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(16-i-.2,16-i-.2),col="#fb6a4a", lwd=1.5)
  points(m3cs$coefficients[i],length(m3cs$coefficients)-i-.2,bg=c("#fee5d9"), col="#fb6a4a",  cex=.65, pch=21 )
}

data.m3.cs<-subset(data.m3,year==2005)
m3cs<-glm(f3,data=data.m3.cs)
out <- summary(m3cs)
out$coefficients[ , 2]
for (i in 2:length(m3cs$coefficients)){
  lines(c(m3cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m3cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(16-i-.3,16-i-.3),col="#fc9272", lwd=1.5)
  points(m3cs$coefficients[i],length(m3cs$coefficients)-i-.3,bg=c("#fee5d9"), col="#fc9272",  cex=.65, pch=21 )
}

data.m3.cs<-subset(data.m3,year==2001)
m3cs<-glm(f3,data=data.m3.cs)
out <- summary(m3cs)
out$coefficients[ , 2]
for (i in 2:length(m3cs$coefficients)){
  lines(c(m3cs$coefficients[i]-(1.96*out$coefficients[i, 2]),m3cs$coefficients[i]+(1.96*out$coefficients[i, 2])),c(16-i-.4,16-i-.4),col="#fcbba1", lwd=1.5)
  points(m3cs$coefficients[i],length(m3cs$coefficients)-i-.4,bg=c("#fee5d9"), col="#fcbba1",  cex=.65, pch=21 )
}
#
axis(1,cex.axis=.8)
mtext(side = 1, "Coefficient Value", line =2.5,cex=1) 
text(.4,9+5,"2017",col="#a50f15",pos=2)
text(.4,8.8+5,"2015",col="#de2d26",pos=2)
text(.4,8.6+5,"2010",col="#fb6a4a",pos=2)
text(.4,8.4+5,"2005",col="#fc9272",pos=2)
text(.4,8.2+5,"2001",col="#fcbba1",pos=2)
dev.off()

