#Replication Script for "Pandemic Response as Border Politics"
#Authors: M.Kenwick and B.Simmons
rm(list=ls())
library(lubridate)
library(countrycode)
library(tidyverse)
library(MASS)
library(WDI)

#Set working directory
setwd('')

#Figure 2, Descriptive plot of border build-up over time
pdf("fig2.pdf",width=10,height=7.5)
annual<-read.csv('border_orientation_annual_means.csv')
par(mar=c(4,4,3,3), mfrow=c(1,1))
plot(NULL,# create empty plot
     xlim = c(2000,2018), 
     ylim = c(-.15,.1), 
     axes = F, xlab = NA, ylab = NA) 
grid()
lines(annual$year,annual$bo_mean,  col="steelblue", cex=1, pch=21,lwd=4 )
arrows( 2019.1,.09,2019.1,.11, xpd = TRUE, lwd=1.15, length=.1)
mtext("Stonger Commitment\nto Filtering",line=0.95,side=4,at=.05,col=,cex=1)
arrows(2019.1,-.14,2019.1,-.16, xpd = TRUE, lwd=1.15, length=.1)
mtext("Weaker Commitment\nto Filtering",line=0.95,side=4,at=-.1,col=,cex=1)
mtext("Year",side=1,line=2.25)
mtext("Border Orientation",side=2,line=2.5)
axis(1,at=seq(2000,2018,2))
axis(2)
box()
dev.off()

#Figure 3: Public Opinion
ipsos<-read.csv('ipsos.csv')
colnames(ipsos)[13]<-" "
pdf(file="fig3.pdf",height=11,width=8)
par(mfrow=c(1,1),mar=c(5,5,3,3))
barplot(as.matrix(ipsos),horiz=T,las=2,ylab="",xlab="Percent",col=c("steelblue4","steelblue2"),xlim=c(0,80),
        main="Statement: We should close the borders of my country and \nnot allow anyone in or out until the virus is proven to be contained")
text(50,15,"Somewhat Agree",col="steelblue1")
text(15,15,"Strongly Agree",col="steelblue4")
dev.off()


#Load Oxford Data and add country codes
stringen<-read.csv('OxCGRT_latest.csv')
stringen$ccode<-countrycode(stringen$CountryName,origin='country.name',destination='cown')
stringen$ccode[stringen$CountryName=="Serbia"]<-345

#Load and merge 2018 BO data
bo<-read.csv('border_orientation_2018_country_means_controls.csv')
stringen<-merge(stringen,bo,by.x='ccode',by.y='state1',all.x=T,all.y=F)

#Create date indicators
stringen$date<-ymd(stringen$Date)
stringen$duration<-stringen$date - ymd('20200101')
stringen$year<-year(stringen$date)
stringen$month<-month(stringen$date)
stringen$day<-day(stringen$date)

#Calculate Internal Stringency (start by re-calculating their overall index to ensure pairity)
#Transform flag indicators to be used in index calculations
stringen$C1_Flag[stringen$C1_School.closing==0]<-1
stringen$C2_Flag[stringen$C2_Workplace.closing==0]<-1
stringen$C3_Flag[stringen$C3_Cancel.public.events==0]<-1
stringen$C4_Flag[stringen$C4_Restrictions.on.gatherings==0]<-1
stringen$C5_Flag[stringen$C5_Close.public.transport==0]<-1
stringen$C6_Flag[stringen$C6_Stay.at.home.requirements==0]<-1
stringen$C7_Flag[stringen$C7_Restrictions.on.internal.movement==0]<-1
stringen$H1_Flag[stringen$H1_Public.information.campaigns==0]<-1
#Reproduce stringency index to check whether identical
stringen$index_v2<-( 100*(stringen$C1_School.closing - .5*(1 - stringen$C1_Flag))/3 +
                       100*(stringen$C2_Workplace.closing - .5*(1 - stringen$C2_Flag))/3 +
                       100*(stringen$C3_Cancel.public.events - .5*(1 - stringen$C3_Flag))/2 +
                       100*(stringen$C4_Restrictions.on.gatherings - .5*(1 - stringen$C4_Flag))/4 +
                       100*(stringen$C5_Close.public.transport - .5*(1 - stringen$C5_Flag))/2 +
                       100*(stringen$C6_Stay.at.home.requirements - .5*(1 - stringen$C6_Flag))/3 +
                       100*(stringen$C7_Restrictions.on.internal.movement - .5*(1 - stringen$C7_Flag))/2 +
                       100*(stringen$C8_International.travel.controls)/4 +
                       100*(stringen$H1_Public.information.campaigns - .5*(1 - stringen$H1_Flag))/2 )/9
#Reproduction appears identical
plot(stringen$index_v2,stringen$StringencyIndex)

#Note that the reproduced index as a bit more missingness since 
#the original index kept observations where one of the indicators is missing
table(is.na(stringen$StringencyIndex))
table(is.na(stringen$index_v2))

#Stringency Index without international travel control
stringen$index_only_internal<-(100*(stringen$C1_School.closing - .5*(1 - stringen$C1_Flag))/3 +
                                 100*(stringen$C2_Workplace.closing - .5*(1 - stringen$C2_Flag))/3 +
                                 100*(stringen$C3_Cancel.public.events - .5*(1 - stringen$C3_Flag))/2 +
                                 100*(stringen$C4_Restrictions.on.gatherings - .5*(1 - stringen$C4_Flag))/4 +
                                 100*(stringen$C5_Close.public.transport - .5*(1 - stringen$C5_Flag))/2 +
                                 100*(stringen$C6_Stay.at.home.requirements - .5*(1 - stringen$C6_Flag))/3 +
                                 100*(stringen$C7_Restrictions.on.internal.movement - .5*(1 - stringen$C7_Flag))/2 +
                                 100*(stringen$H1_Public.information.campaigns - .5*(1 - stringen$H1_Flag))/2 )/8
#Comparing internal only with the overall index. 
plot(stringen$StringencyIndex,stringen$index_only_internal)


#Figures 4-6
#Divide into Border Orientation Groups
stringen$bo_group<-0
stringen$bo_group[stringen$bo<= -0.651235]<- -1
stringen$bo_group[stringen$bo>= 0.655787]<- 1

stringen$color<-"gray80"
stringen$color[stringen$bo<= -0.651235]<-"#006837"
stringen$color[stringen$bo>= 0.655787]<- "#a50026"
#Calculate indicator for days before and since first reported death
first_death<- stringen %>% 
  group_by(CountryName) %>% 
  filter(ConfirmedDeaths > 0) %>%
  arrange(date) %>% dplyr::summarise(min_date=head(date,1))
stringen<-merge(stringen,first_death,by='CountryName',all.x=T,all.y=F)
stringen$first_death_duration<-stringen$date - stringen$min_date
stringen$first_death_duration<-as.numeric(stringen$first_death_duration)

#Figure 4: External vs. Internal Control Scales
mean_comp <- stringen %>% 
  group_by(date) %>% 
  dplyr::summarise(internal_mean = mean(index_only_internal,na.rm=T),
                   external_mean = mean(C8_International.travel.controls,na.rm=T))
#Data gets very noisy/incomplete after June 15, so keeping the window prior to that date
mean_comp<-mean_comp[mean_comp$date < ymd('2020-06-14'),]

pdf('fig4.pdf',width=8,height=7)
par(mfrow=c(1,1),mar=c(4,4,4,4))
plot(NULL,
     xlim = c(min(mean_comp$date),max(mean_comp$date)), 
     ylim = c(0,100), 
     axes = F, xlab = NA, ylab = NA) 
stringen<-arrange(stringen,CountryName,date)
countries<-unique(stringen$CountryName)
for(c in countries){
  test<-subset(stringen, CountryName==c)
  lines(as.numeric(test$date[!is.na(test$C8_International.travel.controls)]),
        100*test$C8_International.travel.controls[!is.na(test$C8_International.travel.controls)]/4,  col=alpha("orangered",.02), cex=1, pch=21,lwd=1.5 )
}
for(c in countries){
  test<-subset(stringen, CountryName==c)
  lines(as.numeric(test$date[!is.na(test$index_only_internal)]),
        test$index_only_internal[!is.na(test$index_only_internal)],  col=alpha("steelblue",.04), cex=1, pch=21,lwd=1.5 )
}

lines(mean_comp$date,100*mean_comp$external_mean/4,col="orangered1",lwd=4)
lines(mean_comp$date, mean_comp$internal_mean,col="steelblue",lwd=4)
axis(1,at=c(18262,18293,18322,18353,18383,18414),lab=c("Jan","Feb","Mar","Apr","May","June"))
axis(4,col="steelblue",col.ticks="steelblue",col.axis="steelblue")
axis(2,col="orangered",col.ticks="orangered",col.axis="orangered")
mtext("External Stringency",side=2,col="orangered",line=2)
mtext("Internal Stringency",side=4,col="steelblue",line=1.8)
dev.off()


#Figure 5: Scatterplots
f5<- stringen %>%
  group_by(CountryName) %>%
  dplyr::summarize(index_only_internal=mean(index_only_internal,na.rm=T),
                   bo_mean = mean(bo,na.rm=T),
                   external_mean = mean(C8_International.travel.controls,na.rm=T)) %>%
  unique()
names<-as.character(f5$CountryName)
names[ names!="China" &  
         names!="Kazakhstan" &  
         names!="Somalia" &  
         names!="Turkey" &  
         names!="Iran" &  
         names!="United States" &  
         names!="Ireland" &  
         names!="Sweden" &  
         names!="Latvia" &  
         names!="Germany" &  
         names!="Brazil" &  
         names!="Italy" ] <- ""

pdf('fig5.pdf',width=14,height=7)
par(mar=c(4,4,2,2),mfrow=c(1,2))
plot(NULL,# create empty plot
     xlim = c(-1.4,1.9), 
     ylim = c(0,100), 
     axes = F, xlab = NA, ylab = NA) 
grid()
abline(v=mean(f5$bo_mean[!is.na(f5$bo_mean)]), col="gray50",lty=2, lwd=1.5)
abline(h=mean(f5$index_only_internal[!is.na(f5$index_only_internal)]), col="gray50",lty=2, lwd=1.5)
reg<-lm(index_only_internal~bo_mean, data=f5)
abline(reg, col="gray50",lwd=1.85 )
points(f5$bo_mean,f5$index_only_internal,col=alpha("steelblue4",.8),bg=alpha("steelblue1",.5),lwd=1,pch=21,cex=1)
text(f5$bo_mean,
     f5$index_only_internal,
     labels=names,pos=3,col=alpha("black",1),cex=1)
axis(1)
axis(2)
mtext(side=2,"Internal Stringency",line=2,cex=1.25)
mtext(side=1,"Border Orientation",line=2.5,cex=1.25)
title("",col.main="white",cex.main=1.5)
box()
text(1, 95,  
     bquote(rho==.(round(cor(
       f5$index_only_internal,
       f5$bo_mean,method="spearman",
       use="pairwise.complete.obs"),3)
     ))
     ,cex=1.5,col="steelblue4",pos=4)

plot(NULL,# create empty plot
     xlim = c(-1.4,1.9), 
     ylim = c(0,4), 
     axes = F, xlab = NA, ylab = NA) 
grid()
abline(v=mean(f5$bo_mean[!is.na(f5$bo_mean)]), col="gray50",lty=2, lwd=1.5)
abline(h=mean(f5$external_mean[!is.na(f5$external_mean)]), col="gray50",lty=2, lwd=1.5)
reg<-lm(external_mean~bo_mean, data=f5)
abline(reg, col="gray50",lwd=1.85 )
points(f5$bo_mean,f5$external_mean,col=alpha("orangered4",.8),bg=alpha("orangered1",.5),lwd=1,pch=21,cex=1)
text(f5$bo_mean,
     f5$external_mean,
     labels=names,pos=3,col=alpha("black",1),cex=1)
axis(1)
axis(2,at=c(0,1,2,3,4),labels=c('0','25','50','70','100'))
mtext(side=2,"External Stringency",line=2,cex=1.25)
mtext(side=1,"Border Orientation",line=2.5,cex=1.25)
title("",col.main="white",cex.main=1.5)
box()
text(1, 3.8,  
     bquote(rho==.(round(cor(
       f5$external_mean,
       f5$bo_mean,method="spearman",
       use="pairwise.complete.obs"),3)
     ))
     ,cex=1.5,col="orangered4",pos=4)
dev.off()


#Figure 6: Border Orientation vs. Control Time Series
pdf('fig6.pdf',width=14,height=7)
par(mfrow=c(1,2))
plot(NULL,# create empty plot
     xlim = c(-50,90), 
     ylim = c(0,100), 
     axes = F, xlab = NA, ylab = NA) 
stringen<-arrange(stringen,CountryName,date)
countries<-unique(stringen$CountryName)
for(c in countries){
  test<-subset(stringen, CountryName==c & first_death_duration<90)
  lines(as.numeric(test$first_death_duration[!is.na(test$index_only_internal)]),
        test$index_only_internal[!is.na(test$index_only_internal)],  col=alpha(test$col[1],.1), cex=1, pch=21,lwd=1.5 )
}
axis(1)
axis(2)
test<- stringen %>%
  group_by(first_death_duration, bo_group) %>%
  dplyr::summarize(mean_stringen = mean(index_only_internal,na.rm=T)) %>%
  filter(first_death_duration<90) %>% 
  unique()
lines(test$first_death_duration[test$bo_group==-1],test$mean_stringen[test$bo_group==-1],col="#1a9850", cex=1, pch=21,lwd=4 )
lines(test$first_death_duration[test$bo_group==0],test$mean_stringen[test$bo_group==0],col="black", cex=1, pch=21,lwd=4 )
lines(test$first_death_duration[test$bo_group==1],test$mean_stringen[test$bo_group==1],col="#d73027", cex=1, pch=21,lwd=4 )
mtext("Days since First Confirmed Death",side=1,line=3)
mtext("Internal Stringency",side=2,line=3)
text(-50,100,"High Border Orientation Score",pos=4,col="#d73027",cex=1)
text(-50,95,"Intermediate Border Orientation Score",pos=4,col="black",cex=1)
text(-50,90,"Low Border Orientation Score",pos=4,col="#1a9850",cex=1)

plot(NULL,# create empty plot
     xlim = c(min(stringen$date),max(stringen$date)), 
     ylim = c(0,4), 
     axes = F, xlab = NA, ylab = NA) 
stringen<-arrange(stringen,CountryName,date)
countries<-unique(stringen$CountryName)
for(c in countries){
  test<-subset(stringen, CountryName==c & date< ymd('2020-06-14'))
  lines(as.numeric(test$date[!is.na(test$C8_International.travel.controls)]),
        test$C8_International.travel.controls[!is.na(test$C8_International.travel.controls)],  col=alpha(test$col[1],.1), cex=1, pch=21,lwd=1.5 )
}
axis(2,at=c(0,1,2,3,4),lab=c('0','25','50','75','100'))
test<- stringen %>%
  group_by(date, bo_group) %>%
  dplyr::summarize(mean_external = mean(C8_International.travel.controls,na.rm=T)) %>%
  filter(date<18414) %>% 
  unique()
lines(test$date[test$bo_group==-1],test$mean_external[test$bo_group==-1],col="#1a9850", cex=1, pch=21,lwd=4 )
lines(test$date[test$bo_group==0],test$mean_external[test$bo_group==0],col="black", cex=1, pch=21,lwd=4 )
lines(test$date[test$bo_group==1],test$mean_external[test$bo_group==1],col="#d73027", cex=1, pch=21,lwd=4 )
axis(1,at=c(18262,18293,18322,18353,18383,18414),lab=c("Jan","Feb","Mar","Apr","May","June"))
mtext("External Stringency",side=2,line=3)
mtext("Date",side=1,line=3)
dev.off()


#####################
#Cross-Sectional Regression Analysis
#condense data to cross-sectional observations
cs<-stringen %>% 
  group_by(CountryName,ccode) %>%
  summarize(bo = mean(bo,na.omit=T),
            v2x_libdem = mean(v2x_libdem,na.omit=T),
            lnpop = mean(lnpop,na.rm=T),
            lngdp1 = mean(lngdp1,na.rm=T),
            mean_external = mean(C8_International.travel.controls, na.rm=T),
            mean_index_only_internal = mean(index_only_internal,na.rm=T)) 
cs$lngdp1_sq<-cs$lngdp1^2

#Models
m1<-lm(mean_index_only_internal~bo,data=cs)
summary(m1)
nobs(m1)
round(m1$coefficients,3)
round(coef(summary(m1))[, 2],3)

m2<-lm(mean_index_only_internal~bo+lngdp1+lngdp1_sq,data=cs)
summary(m2)
m2$coefficients
round(m2$coefficients,3)
round(coef(summary(m2))[, 2],3)
nobs(m2)

m3<-lm(mean_index_only_internal~bo+lngdp1+lngdp1_sq+v2x_libdem,data=cs)
summary(m3)
nobs(m3)
round(m3$coefficients,3)
round(coef(summary(m3))[, 2],3)

m4<-lm(mean_index_only_internal~bo+lngdp1+lngdp1_sq+v2x_libdem+lnpop,data=cs)
summary(m4)
nobs(m4)
round(m4$coefficients,3)
round(coef(summary(m4))[, 2],3)

m5<-lm(mean_external~bo,data=cs)
summary(m5)
nobs(m5)
round(m5$coefficients,3)
round(coef(summary(m5))[, 2],3)


m6<-lm(mean_external~bo+lngdp1+lngdp1_sq,data=cs)
summary(m6)
nobs(m6)
round(m6$coefficients,3)
round(coef(summary(m6))[, 2],3)

m7<-lm(mean_external~bo+lngdp1+lngdp1_sq+v2x_libdem,data=cs)
summary(m7)
nobs(m7)
round(m7$coefficients,3)
round(coef(summary(m7))[, 2],3)

m8<-lm(mean_external~bo+lngdp1+lngdp1_sq+lnpop+v2x_libdem+lnpop,data=cs)
summary(m8)
nobs(m8)
round(m8$coefficients,3)
round(coef(summary(m8))[, 2],3)

