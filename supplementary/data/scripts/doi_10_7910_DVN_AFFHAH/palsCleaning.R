##front matter##
rm(list=ls())
library(foreign)
library(lattice)
library(utils)
library(Hmisc)
library(xtable)
library(car)
library(psych)
library(polycor)

##set the following to YOUR working directory##
setwd('/Volumes/MONOGAN/psARE/')

#The following line loads data from http://www.thearda.com/pals/. The file name may have changed.
#To replicate the data-cleaning process, you must ask the ARDA for the original data.
data<-read.spss('public v5.sav', use.value.labels=FALSE,to.data.frame=TRUE)

##Measure Religious Strictness Exogenously with Iannaconne Data.##
data$ri_3<-recode(data$ri_3,"NA=1000")
data$ri_4<-recode(data$ri_4,"NA=1000")

data$oneDenom <- data$ri_3
data$oneDenom[data$oneDenom==2 | data$oneDenom>=7] <- data$ri_4[data$oneDenom==2 | data$oneDenom>=7]
data$oneDenom[data$ci_1==4]<-11
data$oneDenom[data$ci_1==5]<-12

data$strictness[data$oneDenom==1]<-4.8305 #pentecostal
data$strictness[data$oneDenom==3]<-3.6459 #fundamentalist
data$strictness[data$oneDenom==4]<-4.8036 #evangelical
data$strictness[data$oneDenom==5]<-2.5802 #mainline
data$strictness[data$oneDenom==6]<-1.54558 #liberal
data$strictness[data$oneDenom==11]<-3.0577 #catholic
data$strictness[data$oneDenom==12]<-5.4153 #mormon

##Indicator for Evangelical##
data$evangelical<-as.numeric(data$oneDenom==4)

##Factor for protestants##
data$protTrad<-data$oneDenom
data$protTrad[data$protTrad>6]<-NA

##civic participation##
civic.data<-as.data.frame(cbind(data$sa_meeting,data$sa_smallgroup,data$sa_volunteer))
civic.cor<-polychoric(civic.data)
civic.fa<-fa(r=civic.cor$rho,nfactors=1,n.obs=2610,rotate='varimax')
civic.fa; civic.fa$R2
data$meeting<-scale(data$sa_meeting)
data$smallgroup<-scale(data$sa_smallgroup)
data$volunteer<-scale(data$sa_volunteer)
data$civic<-.600*data$meeting+.586*data$smallgroup+.801*data$volunteer

##church participation##
attendance.data<-as.data.frame(cbind(data$art_3,data$art_4,data$art_7))
attendance.cor<-polychoric(attendance.data)
attendance.fa<-fa(r=attendance.cor$rho,nfactors=1,n.obs=2610,rotate='varimax')
attendance.fa; attendance.fa$R2
data$a3<-scale(data$art_3)
data$a4<-scale(data$art_4)
data$a7<-scale(data$art_7)
data$attendance<-.638*data$a3+.338*data$a4+.814*data$a7

##Recode education from 12 points to 6 ordinal points##
data$education<-data$dm_2
data$education[data$education>3 & data$education<7]<-4
data$education[data$education==7]<-5
data$education[data$education>7 & data$education<12]<-6
data$education[data$education==12]<-NA

##Create race variables##
data$black<-as.numeric(data$re_race==2)
data$hispanic<-as.numeric(data$re_race==3)
data$other<-as.numeric(data$re_race>3)

##partisan strength##
data$strength<-abs(data$po_party-4)

##recruitment##
data$ne_26_1<-recode(data$ne_26_1,"NA=0")
data$ne_26_2<-recode(data$ne_26_2,"NA=0")
data$ne_26_3<-recode(data$ne_26_3,"NA=0")
data$ne_26_4<-recode(data$ne_26_4,"NA=0")
data$ne_26_5<-recode(data$ne_26_5,"NA=0")
data$ne_26_6<-recode(data$ne_26_6,"NA=0")
data$ne_26_7<-recode(data$ne_26_7,"NA=0")
data$ne_26_8<-recode(data$ne_26_8,"NA=0")
data$recruitment<-data$ne_26_1+data$ne_26_2+data$ne_26_3+data$ne_26_4+data$ne_26_5+data$ne_26_6+data$ne_26_7+data$ne_26_8

##south dummy##
data$south<-as.numeric(data$resp_region==3)

##homeowner dummy##
data$homeowner<-as.numeric(data$hc_5==2)

##define political participation variable##
data$vote<-as.numeric(data$po_4==1)
data$participation<-data$po_number+as.numeric(data$po_4==1)

##Measure strictness with individual-level cost variables##
#Total time doing volunteer work, in hours
data$no.work<-as.numeric(data$ca_28==2 & data$ca_29==2 & data$ca_30==2)
data$ca_32[data$no.work==1]<-0
data$no.vol<-as.numeric(data$ca_32a==2)
data$ca_32b[data$no.vol==1]<-0
data$time<-data$ca_32a+data$ca_32b
#Make the money measure continuous
data$money<-recode(data$art_8a,"1=24;2=100;3=300;4=1000;5=5000;6=10000;7=15000")
#Recode "not applicable" to "missing" for influence over career, marriage, where to live, and children.
data$rm_4[data$rm_4==6]<-NA
data$rm_5[data$rm_5==6]<-NA
data$rm_6[data$rm_6==6]<-NA
data$rm_7[data$rm_7==6]<-NA

#Factor analyze time and money, along with influence over career, marriage, where to live, and children.
cost.data<-as.data.frame(cbind(data$time,data$money,data$rm_4,data$rm_5,data$rm_6,data$rm_7))
cost.cor<-cor(cost.data, use="complete.obs")
cost.fa<-fa(r=cost.cor,nfactors=1,n.obs=2610,rotate='varimax')
cost.fa; cost.fa$R2
data$c1<-scale(data$time)
data$c2<-scale(data$money)
data$c3<-scale(data$rm_4)
data$c4<-scale(data$rm_5)
data$c5<-scale(data$rm_6)
data$c6<-scale(data$rm_7)
data$cost<-.152*data$c1+.229*data$c2+.602*data$c3+.693*data$c4+.793*data$c5+.759*data$c6

#Political Variable
data$bush<-recode(data$po_5,'1=1;2=0;3=NA')

##Parse-down data##
data.1<-subset(data, strictness!='NA')#This leaves N=1196 Christians
data.2<-subset(data.1, select=c(resp_zri, vote, po_sign, po_work, po_contact, po_persuade, po_attend, po_gave, po_workrel, po_demonstrate, participation, strictness, hr_age, education, dm_income, hr_gender, black, hispanic, other, strength, recruitment, south, attendance, homeowner, wj_unemployed, civic, ri_conslib, cost, evangelical, protTrad, bush))

##Rescale variables to make convergence easier##
data.2$strictness<-scale(data.2$strictness)
data.2$hr_age<-scale(data.2$hr_age)
data.2$education<-scale(data.2$education)
data.2$dm_income<-scale(data.2$dm_income)
data.2$strength<-scale(data.2$strength)
data.2$recruitment<-scale(data.2$recruitment)
data.2$attendance<-scale(data.2$attendance)
data.2$civic<-scale(data.2$civic)
data.2$ri_conslib<-scale(data.2$ri_conslib)
data.2$cost<-scale(data.2$cost)

##percentage to be imputed##
table(is.na(data.2))/sum(table(is.na(data.2)))

##output the file in CSV and Stata format##
write.csv(data.2, 'palsStricPartic.csv',row.names=F)
write.dta(data.2, 'palsStrictPartic.dta')

##Create Figure of Causal Model##
#pdf("causal.pdf",family="Times")
plot(x=c(0,1),y=c(0,1),type='n',axes=F,xlab="",ylab="")
polygon(x=c(0,0,.2,.2),y=c(.4,.6,.6,.4))
text(x=.1,y=.5,"Strictness")
polygon(x=c(1,1,.8,.8),y=c(.4,.6,.6,.4))
text(x=.9,y=.5,"Participation")
polygon(x=c(.35,.35,.65,.65),y=c(.8,1,1,.8))
text(x=.5,y=.9,"Civic engagement")
polygon(x=c(.4,.4,.6,.6),y=c(0,.2,.2,0))
text(x=.5,y=.1,"Income")
polygon(x=c(.35,.35,.65,.65),y=c(.3,.5,.5,.3))
text(x=.5,y=.4,"Religious attendance")
arrows(.2,.55,.8,.55,length=.1)
arrows(.2,.55,.35,.9,length=.1)
arrows(.2,.55,.35,.4,length=.1)
arrows(.2,.55,.4,.1,length=.1)
arrows(.65,.9,.8,.6,length=.1)
arrows(.65,.4,.8,.5,length=.1)
arrows(.6,.1,.8,.4,length=.1)
#dev.off()
