rm(list=ls())

##copy paste from previous script some are useless here
library(xlsx)
library(car)
library(psych)
library(gmodels)
library(aod)
library(stringi)
library(ggplot2)
library(plyr)
library(scales)
library(Hmisc)
library(gdata)
library(lattice)
library(gmodels)
library(car)
library(foreign)
library(memisc)
library(stargazer)
library(igraph)
library(plotrix)
library(grid)
library(gridExtra)
library(arm)
library(effsize)

#ANES TS 2012 recode
##loading in ANES TS 2012 data set
mydata <- read.dta("C:/Users/Ryan/Downloads/anes_timeseries_2012_stata12.dta")

##party id
table(mydata$pid_x)

table(mydata$ftf_oversample)
table(mydata$mode)
##Ideology
table(mydata$libcpre_self)
table(mydata$libcpre_choose)

I##Survey mode
table(mydata$mode)
table(mydata$ftf_oversample)
mydata$mode[mydata$ftf_oversample=="1. Hispanic oversample"]<-NA
mydata$mode[mydata$ftf_oversample=="2. AA oversample"]<-NA
mydata$mode[mydata$ftf_oversample=="3. Both Hispanic and AA oversample"]<-NA
mydata$mode


#Removing over samples 
mydata$mode[mydata$mode=="1. FTF (face-to-face) mode"]

##recoding mode to be usefull
mydata$mode
mydata$ftf<-NA
mydata$ftf[mydata$mode=="1. FTF (face-to-face) mode"]<-1
table(mydata$ftf)

mydata$int<-NA
mydata$int[mydata$mode=="2. Internet mode"]<-1
mydata$int

ftf=mydata$ftf ==1
int=mydata$int ==1

table(mydata[ftf,]$pid_x)
table(mydata[int,]$pid_x)


##Recoding big 5 to be "positive" and averaging questions
##extraversion
mydata$tipi_extra
mydata$extra<-NA
mydata$extra[mydata$tipi_extra=="1. Extremely poorly"]<-1
mydata$extra[mydata$tipi_extra=="2. Somewhat poorly"]<-2
mydata$extra[mydata$tipi_extra=="3. A little poorly"]<-3
mydata$extra[mydata$tipi_extra=="4. Neither poorly nor well"]<-4
mydata$extra[mydata$tipi_extra=="5. A little well"]<-5
mydata$extra[mydata$tipi_extra=="6. Somewhat well"]<-6
mydata$extra[mydata$tipi_extra=="7. Extremely well"]<-7
mydata$extra

mydata$tipi_resv
mydata$resv<-NA
mydata$resv[mydata$tipi_resv=="1. Extremely poorly"]<-7
mydata$resv[mydata$tipi_resv=="2. Somewhat poorly"]<-6
mydata$resv[mydata$tipi_resv=="3. A little poorly"]<-5
mydata$resv[mydata$tipi_resv=="4. Neither poorly nor well"]<-4
mydata$resv[mydata$tipi_resv=="5. A little well"]<-3
mydata$resv[mydata$tipi_resv=="6. Somewhat well"]<-2
mydata$resv[mydata$tipi_resv=="7. Extremely well"]<-1
mydata$resv

mydata$extraversion<- (mydata$extra+mydata$resv)/2
mydata$extraversion

##Agreeableness
mydata$tipi_crit
mydata$crit<-NA
mydata$crit[mydata$tipi_crit=="1. Extremely poorly"]<-7
mydata$crit[mydata$tipi_crit=="2. Somewhat poorly"]<-6
mydata$crit[mydata$tipi_crit=="3. A little poorly"]<-5
mydata$crit[mydata$tipi_crit=="4. Neither poorly nor well"]<-4
mydata$crit[mydata$tipi_crit=="5. A little well"]<-3
mydata$crit[mydata$tipi_crit=="6. Somewhat well"]<-2
mydata$crit[mydata$tipi_crit=="7. Extremely well"]<-1
mydata$crit

mydata$tipi_warm
mydata$warm<-NA
mydata$warm[mydata$tipi_warm=="1. Extremely poorly"]<-1
mydata$warm[mydata$tipi_warm=="2. Somewhat poorly"]<-2
mydata$warm[mydata$tipi_warm=="3. A little poorly"]<-3
mydata$warm[mydata$tipi_warm=="4. Neither poorly nor well"]<-4
mydata$warm[mydata$tipi_warm=="5. A little well"]<-5
mydata$warm[mydata$tipi_warm=="6. Somewhat well"]<-6
mydata$warm[mydata$tipi_warm=="7. Extremely well"]<-7
mydata$warm

mydata$agreeableness<- (mydata$crit+mydata$warm)/2
mydata$agreeableness

##conscientiousness
mydata$tipi_dep
mydata$dep<-NA
mydata$dep[mydata$tipi_dep=="1. Extremely poorly"]<-1
mydata$dep[mydata$tipi_dep=="2. Somewhat poorly"]<-2
mydata$dep[mydata$tipi_dep=="3. A little poorly"]<-3
mydata$dep[mydata$tipi_dep=="4. Neither poorly nor well"]<-4
mydata$dep[mydata$tipi_dep=="5. A little well"]<-5
mydata$dep[mydata$tipi_dep=="6. Somewhat well"]<-6
mydata$dep[mydata$tipi_dep=="7. Extremely well"]<-7
mydata$dep

table(mydata$tipi_disorg)
mydata$disorg<-NA
mydata$disorg[mydata$tipi_disorg=="1. Extremely poorly"]<-7
mydata$disorg[mydata$tipi_disorg=="2. Somewhat poorly"]<-6
mydata$disorg[mydata$tipi_disorg=="3. A little poorly"]<-5
mydata$disorg[mydata$tipi_disorg=="4. Neither poorly nor well"]<-4
mydata$disorg[mydata$tipi_disorg=="5. A little well"]<-3
mydata$disorg[mydata$tipi_disorg=="6. Somewhat well"]<-2
mydata$disorg[mydata$tipi_disorg=="7. Extremely well"]<-1
mydata$disorg

mydata$conscientiousness<- (mydata$dep+mydata$disorg)/2
mydata$conscientiousness


##emotional stability
mydata$tipi_anx
mydata$anx<-NA
mydata$anx[mydata$tipi_anx=="1. Extremely poorly"]<-7
mydata$anx[mydata$tipi_anx=="2. Somewhat poorly"]<-6
mydata$anx[mydata$tipi_anx=="3. A little poorly"]<-5
mydata$anx[mydata$tipi_anx=="4. Neither poorly nor well"]<-4
mydata$anx[mydata$tipi_anx=="5. A little well"]<-3
mydata$anx[mydata$tipi_anx=="6. Somewhat well"]<-2
mydata$anx[mydata$tipi_anx=="7. Extremely well"]<-1
mydata$anx

mydata$tipi_calm
mydata$calm<-NA
mydata$calm[mydata$tipi_calm=="1. Extremely poorly"]<-1
mydata$calm[mydata$tipi_calm=="2. Somewhat poorly"]<-2
mydata$calm[mydata$tipi_calm=="3. A little poorly"]<-3
mydata$calm[mydata$tipi_calm=="4. Neither poorly nor well"]<-4
mydata$calm[mydata$tipi_calm=="5. A little well"]<-5
mydata$calm[mydata$tipi_calm=="6. Somewhat well"]<-6
mydata$calm[mydata$tipi_calm=="7. Extremely well"]<-7
mydata$calm

mydata$emostab<- (mydata$anx+mydata$calm)/2
mydata$emostab

##openness
mydata$tipi_open
mydata$open<-NA
mydata$open[mydata$tipi_open=="1. Extremely poorly"]<-1
mydata$open[mydata$tipi_open=="2. Somewhat poorly"]<-2
mydata$open[mydata$tipi_open=="3. A little poorly"]<-3
mydata$open[mydata$tipi_open=="4. Neither poorly nor well"]<-4
mydata$open[mydata$tipi_open=="5. A little well"]<-5
mydata$open[mydata$tipi_open=="6. Somewhat well"]<-6
mydata$open[mydata$tipi_open=="7. Extremely well"]<-7
mydata$open

mydata$tipi_conv
mydata$conv<-NA
mydata$conv[mydata$tipi_conv=="1. Extremely poorly"]<-7
mydata$conv[mydata$tipi_conv=="2. Somewhat poorly"]<-6
mydata$conv[mydata$tipi_conv=="3. A little poorly"]<-5
mydata$conv[mydata$tipi_conv=="4. Neither poorly nor well"]<-4
mydata$conv[mydata$tipi_conv=="5. A little well"]<-3
mydata$conv[mydata$tipi_conv=="6. Somewhat well"]<-2
mydata$conv[mydata$tipi_conv=="7. Extremely well"]<-1
mydata$conv

mydata$openness<- (mydata$open+mydata$conv)/2
mydata$openness

##dropping missing from PID and making categroical variable
mydata$PID<-NA
mydata$PID[mydata$pid_x=="1. Strong Democrat"]<-1
mydata$PID[mydata$pid_x=="2. Not very strong Democract"]<-2
mydata$PID[mydata$pid_x=="3. Independent-Democrat"]<-3
mydata$PID[mydata$pid_x=="4. Independent"]<-4
mydata$PID[mydata$pid_x=="7. Strong Republican"]<-7
mydata$PID[mydata$pid_x=="6. Not very strong Republican"]<-6
mydata$PID[mydata$pid_x=="5. Independent-Republican"]<-5
table(mydata$PID)
table(mydata$pid_x)

#for FTF
table(mydata[ftf,]$PID)
R3=mydata[ftf,]$PID ==7
R2=mydata[ftf,]$PID ==6
R1=mydata[ftf,]$PID ==5
I1=mydata[ftf,]$PID ==4
D1=mydata[ftf,]$PID ==3
D2=mydata[ftf,]$PID ==2
D3=mydata[ftf,]$PID ==1

R33=mydata[int,]$PID ==7
R22=mydata[int,]$PID ==6
R11=mydata[int,]$PID ==5
I11=mydata[int,]$PID ==4
D11=mydata[int,]$PID ==3
D22=mydata[int,]$PID ==2
D33=mydata[int,]$PID ==1


##Same thing for ideology 
#We only ask on ideo quesion in our survey, so we don't care about the follow up question that's trying to put moderates somewhere.
mydata$ideology<-NA
mydata$ideology[mydata$libcpre_self=="1. Extremely liberal"]<-1
mydata$ideology[mydata$libcpre_self=="2. Liberal"]<-2
mydata$ideology[mydata$libcpre_self=="3. Slightly liberal"]<-3
mydata$ideology[mydata$libcpre_self=="4. Moderate; middle of the road"]<-4
mydata$ideology[mydata$libcpre_self=="5. Slightly conservative"]<-5
mydata$ideology[mydata$libcpre_self=="6. Conservative"]<-6
mydata$ideology[mydata$libcpre_self=="7. Extremely conservative"]<-7
table(mydata$ideology)
table(mydata$libcpre_self)

C3=mydata[ftf,]$ideology ==7
C2=mydata[ftf,]$ideology ==6
C1=mydata[ftf,]$ideology ==5
M1=mydata[ftf,]$ideology ==4
L1=mydata[ftf,]$ideology ==3
L2=mydata[ftf,]$ideology ==2
L3=mydata[ftf,]$ideology ==1
table(mydata[ftf,]$ideology)

C33=mydata[int,]$ideology ==7
C22=mydata[int,]$ideology ==6
C11=mydata[int,]$ideology ==5
M11=mydata[int,]$ideology ==4
L11=mydata[int,]$ideology ==3
L22=mydata[int,]$ideology ==2
L33=mydata[int,]$ideology ==1

##
##creating dummies for PID
table(mydata$pid_x)
mydata$pid_x
#dummy for democrats
mydata$dem<-0
mydata$dem[mydata$pid_x=="1. Strong Democrat"]<-1
mydata$dem[mydata$pid_x=="2. Not very strong Democract"]<-1
mydata$dem[mydata$pid_x=="3. Independent-Democrat"]<-1
mydata$dem[mydata$pid_x=="-2. Missing"]<-NA
mydata$dem

#dummy for true independents
mydata$ind<-0
mydata$ind[mydata$pid_x=="4. Independent"]<-1
mydata$ind[mydata$pid_x=="-2. Missing"]<-NA
mydata$ind

#dummy for republicans
mydata$rep<-0
mydata$rep[mydata$pid_x=="7. Strong Republican"]<-1
mydata$rep[mydata$pid_x=="6. Not very strong Republican"]<-1
mydata$rep[mydata$pid_x=="5. Independent-Republican"]<-1
mydata$rep[mydata$pid_x=="-2. Missing"]<-NA
table(mydata$rep)

##Recoding authoritarianism scale to be more "positive" as levels of author increase
table(mydata$auth_ind)
mydata$respect1<-NA
mydata$respect1[mydata$auth_ind=="2. Respect for elders"]<-1
mydata$respect1[mydata$auth_ind=="3. Both {VOL}"]<-.5
mydata$respect1[mydata$auth_ind=="1. Independence"]<-0
mydata$respect1

table(mydata$auth_obed)
mydata$obedience1<-NA
mydata$obedience1[mydata$auth_obed=="1. Obedience"]<-1
mydata$obedience1[mydata$auth_obed=="3. Both {VOL}"]<-.5
mydata$obedience1[mydata$auth_obed=="2. Self-reliance"]<-0
mydata$obedience1

mydata$auth_cur
mydata$manners1<-NA
mydata$manners1[mydata$auth_cur=="2. Good manners"]<-1
mydata$manners1[mydata$auth_cur=="3. Both {VOL}"]<-.5
mydata$manners1[mydata$auth_cur=="1. Curiosity"]<-0
mydata$manners1

mydata$auth_consid
mydata$behave1<-NA
mydata$behave1[mydata$auth_consid=="2. Well behaved"]<-1
mydata$behave1[mydata$auth_consid=="3. Both {VOL}"]<-.5
mydata$behave1[mydata$auth_consid=="1. Being considerate"]<-0
mydata$behave1

##making authoritarianism scale
mydata$ascale<-(mydata$respect1+mydata$obedience1+mydata$manners1+mydata$behave1)/4
mydata$ascale

##Recoding Racial resentment scale to be more "positive" as levels of resentment increase
table(mydata$resent_workway)
mydata$overcome<-NA
mydata$overcome[mydata$resent_workway=="1. Agree strongly"]<-1
mydata$overcome[mydata$resent_workway=="2. Agree somewhat"]<-.75
mydata$overcome[mydata$resent_workway=="3. Neither agree nor disagree"]<-.5
mydata$overcome[mydata$resent_workway=="4. Disagree somewhat"]<-.25
mydata$overcome[mydata$resent_workway=="5. Disagree strongly"]<-0
table(mydata$overcome)

table(mydata$resent_slavery)
mydata$slavery<-NA
mydata$slavery[mydata$resent_slavery=="1. Agree strongly"]<-0
mydata$slavery[mydata$resent_slavery=="2. Agree somewhat"]<-.25
mydata$slavery[mydata$resent_slavery=="3. Neither agree nor disagree"]<-.5
mydata$slavery[mydata$resent_slavery=="4. Disagree somewhat"]<-.75
mydata$slavery[mydata$resent_slavery=="5. Disagree strongly"]<-1
table(mydata$slavery)

table(mydata$resent_deserve)
mydata$deserve<-NA
mydata$deserve[mydata$resent_deserve=="1. Agree strongly"]<-0
mydata$deserve[mydata$resent_deserve=="2. Agree somewhat"]<-.25
mydata$deserve[mydata$resent_deserve=="3. Neither agree nor disagree"]<-.5
mydata$deserve[mydata$resent_deserve=="4. Disagree somewhat"]<-.75
mydata$deserve[mydata$resent_deserve=="5. Disagree strongly"]<-1
table(mydata$deserve)

table(mydata$resent_try)
mydata$harder<-NA
mydata$harder[mydata$resent_try=="1. Agree strongly"]<-1
mydata$harder[mydata$resent_try=="2. Agree somewhat"]<-.75
mydata$harder[mydata$resent_try=="3. Neither agree nor disagree"]<-.5
mydata$harder[mydata$resent_try=="4. Disagree somewhat"]<-.25
mydata$harder[mydata$resent_try=="5. Disagree strongly"]<-0
table(mydata$harder)

##making Racial Resentment scale
mydata$rrscale<-(mydata$overcome+mydata$slavery+mydata$deserve+mydata$harder)/4
mydata$rrscale


##Recoding Moral traditionalism scale to be more "positive" as levels of traditionalism increase
table(mydata$trad_adjust)
mydata$adjust<-NA
mydata$adjust[mydata$trad_adjust=="1. Agree strongly"]<-0
mydata$adjust[mydata$trad_adjust=="2. Agree somewhat"]<-.25
mydata$adjust[mydata$trad_adjust=="3. Neither agree nor disagree"]<-.5
mydata$adjust[mydata$trad_adjust=="4. Disagree somewhat"]<-.75
mydata$adjust[mydata$trad_adjust=="5. Disagree strongly"]<-1
table(mydata$adjust)

table(mydata$trad_lifestyle)
mydata$lifestyle<-NA
mydata$lifestyle[mydata$trad_lifestyle=="1. Agree strongly"]<-1
mydata$lifestyle[mydata$trad_lifestyle=="2. Agree somewhat"]<-.75
mydata$lifestyle[mydata$trad_lifestyle=="3. Neither agree nor disagree"]<-.5
mydata$lifestyle[mydata$trad_lifestyle=="4. Disagree somewhat"]<-.25
mydata$lifestyle[mydata$trad_lifestyle=="5. Disagree strongly"]<-0
table(mydata$lifestyle)

table(mydata$trad_tolerant)
mydata$tolerant<-NA
mydata$tolerant[mydata$trad_tolerant=="1. Agree strongly"]<-0
mydata$tolerant[mydata$trad_tolerant=="2. Agree somewhat"]<-.25
mydata$tolerant[mydata$trad_tolerant=="3. Neither agree nor disagree"]<-.5
mydata$tolerant[mydata$trad_tolerant=="4. Disagree somewhat"]<-.75
mydata$tolerant[mydata$trad_tolerant=="5. Disagree strongly"]<-1
table(mydata$tolerant)

table(mydata$trad_famval)
mydata$famval<-NA
mydata$famval[mydata$trad_famval=="1. Agree strongly"]<-1
mydata$famval[mydata$trad_famval=="2. Agree somewhat"]<-.75
mydata$famval[mydata$trad_famval=="3. Neither agree nor disagree"]<-.5
mydata$famval[mydata$trad_famval=="4. Disagree somewhat"]<-.25
mydata$famval[mydata$trad_famval=="5. Disagree strongly"]<-0
table(mydata$famval)

##making moral traditionalism scale
mydata$mtscale<-(mydata$adjust+mydata$lifestyle+mydata$tolerant+mydata$famval)/4
mydata$mtscale

##Recoding Egalitarianism scale to be more "positive" as levels of Egalitarianism increase
table(mydata$egal_equal)
mydata$equal<-NA
mydata$equal[mydata$egal_equal=="1. Agree strongly"]<-1
mydata$equal[mydata$egal_equal=="2. Agree somewhat"]<-.75
mydata$equal[mydata$egal_equal=="3. Neither agree nor disagree"]<-.5
mydata$equal[mydata$egal_equal=="4. Disagree somewhat"]<-.25
mydata$equal[mydata$egal_equal=="5. Disagree strongly"]<-0
table(mydata$equal)

table(mydata$egal_toofar)
mydata$toofar<-NA
mydata$toofar[mydata$egal_toofar=="1. Agree strongly"]<-0
mydata$toofar[mydata$egal_toofar=="2. Agree somewhat"]<-.25
mydata$toofar[mydata$egal_toofar=="3. Neither agree nor disagree"]<-.5
mydata$toofar[mydata$egal_toofar=="4. Disagree somewhat"]<-.75
mydata$toofar[mydata$egal_toofar=="5. Disagree strongly"]<-1
table(mydata$toofar)

table(mydata$egal_bigprob)
mydata$bigprob<-NA
mydata$bigprob[mydata$egal_bigprob=="1. Agree strongly"]<-1
mydata$bigprob[mydata$egal_bigprob=="2. Agree somewhat"]<-.75
mydata$bigprob[mydata$egal_bigprob=="3. Neither agree nor disagree"]<-.5
mydata$bigprob[mydata$egal_bigprob=="4. Disagree somewhat"]<-.25
mydata$bigprob[mydata$egal_bigprob=="5. Disagree strongly"]<-0
table(mydata$bigprob)

table(mydata$egal_worryless)
mydata$worryless<-NA
mydata$worryless[mydata$egal_worryless=="1. Agree strongly"]<-0
mydata$worryless[mydata$egal_worryless=="2. Agree somewhat"]<-.25
mydata$worryless[mydata$egal_worryless=="3. Neither agree nor disagree"]<-.5
mydata$worryless[mydata$egal_worryless=="4. Disagree somewhat"]<-.75
mydata$worryless[mydata$egal_worryless=="5. Disagree strongly"]<-1
table(mydata$worryless)

table(mydata$egal_notbigprob)
mydata$notbigprob<-NA
mydata$notbigprob[mydata$egal_notbigprob=="1. Agree strongly"]<-0
mydata$notbigprob[mydata$egal_notbigprob=="2. Agree somewhat"]<-.25
mydata$notbigprob[mydata$egal_notbigprob=="3. Neither agree nor disagree"]<-.5
mydata$notbigprob[mydata$egal_notbigprob=="4. Disagree somewhat"]<-.75
mydata$notbigprob[mydata$egal_notbigprob=="5. Disagree strongly"]<-1
table(mydata$notbigprob)

table(mydata$egal_fewerprobs)
mydata$fewerprobs<-NA
mydata$fewerprobs[mydata$egal_fewerprobs=="1. Agree strongly"]<-1
mydata$fewerprobs[mydata$egal_fewerprobs=="2. Agree somewhat"]<-.75
mydata$fewerprobs[mydata$egal_fewerprobs=="3. Neither agree nor disagree"]<-.5
mydata$fewerprobs[mydata$egal_fewerprobs=="4. Disagree somewhat"]<-.25
mydata$fewerprobs[mydata$egal_fewerprobs=="5. Disagree strongly"]<-0
table(mydata$fewerprobs)

##making  Egalitarianism scale
mydata$escale<-(mydata$equal+mydata$toofar+mydata$bigprob+mydata$worryless+mydata$notbigprob+mydata$fewerprobs)/6
mydata$escale

#we need age, education, sex, income, and religiosity
#age
table(mydata$dem_age_r_x)##this looks fine but lets get rid of -2's and 17 year olds
mydata$dem_age_r_x[mydata$dem_age_r_x=="-2"]<-NA
mydata$age<-(mydata$dem_age_r_x-17)/73
table(mydata$age)

##education
table(mydata$dem_edugroup_x)##lets get rid of refused, don't know, and missing
mydata$edu<-NA
mydata$edu[mydata$dem_edugroup_x=="1. Less than high school credential"]<-1
mydata$edu[mydata$dem_edugroup_x=="2. High school credential"]<-2
mydata$edu[mydata$dem_edugroup_x=="3. Some post-high-school, no bachelor's degree"]<-3
mydata$edu[mydata$dem_edugroup_x=="4. Bachelor's degree"]<-4
mydata$edu[mydata$dem_edugroup_x=="5. Graduate degree"]<-5
table(mydata$edu)

##sex
table(mydata$gender_respondent_x)##looks perfect but recode as dummy
mydata$male<-NA
mydata$male[mydata$gender_respondent_x=="1. Male"]<-1
mydata$male[mydata$gender_respondent_x=="2. Female"]<-0
table(mydata$male)

##income; this is family income
table(mydata$inc_incgroup_pre)
mydata$inc<-NA
mydata$inc[mydata$inc_incgroup_pre=="01. Under $5,000"]<-1
mydata$inc[mydata$inc_incgroup_pre=="02. $5,000-$9,999"]<-2
mydata$inc[mydata$inc_incgroup_pre=="03. $10,000-$12,499"]<-3
mydata$inc[mydata$inc_incgroup_pre=="04. $12,500-$14,999"]<-4
mydata$inc[mydata$inc_incgroup_pre=="05. $15,000-$17,499"]<-5
mydata$inc[mydata$inc_incgroup_pre=="06. $17,500-$19,999"]<-6
mydata$inc[mydata$inc_incgroup_pre=="07. $20,000-$22,499"]<-7
mydata$inc[mydata$inc_incgroup_pre=="08. $22,500-$24,999"]<-7
mydata$inc[mydata$inc_incgroup_pre=="09. $25,000-$27,499"]<-8
mydata$inc[mydata$inc_incgroup_pre=="10. $27,500-$29,999"]<-9
mydata$inc[mydata$inc_incgroup_pre=="11. $30,000-$34,999"]<-10
mydata$inc[mydata$inc_incgroup_pre=="12. $35,000-$39,999"]<-11
mydata$inc[mydata$inc_incgroup_pre=="13. $40,000-$44,999"]<-12
mydata$inc[mydata$inc_incgroup_pre=="14. $45,000-$49,999"]<-13
mydata$inc[mydata$inc_incgroup_pre=="15. $50,000-$54,999"]<-14
mydata$inc[mydata$inc_incgroup_pre=="16. $55,000-$59,999"]<-15
mydata$inc[mydata$inc_incgroup_pre=="17. $60,000-$64,999"]<-16
mydata$inc[mydata$inc_incgroup_pre=="18. $65,000-$69,999"]<-17
mydata$inc[mydata$inc_incgroup_pre=="19. $70,000-$74,999"]<-18
mydata$inc[mydata$inc_incgroup_pre=="20. $75,000-$79,999"]<-19
mydata$inc[mydata$inc_incgroup_pre=="21. $80,000-$89,999"]<-20
mydata$inc[mydata$inc_incgroup_pre=="22. $90,000-$99,999"]<-21
mydata$inc[mydata$inc_incgroup_pre=="23. $100,000-$109,999"]<-22
mydata$inc[mydata$inc_incgroup_pre=="24. $110,000-$124,999"]<-23
mydata$inc[mydata$inc_incgroup_pre=="25. $125,000-$149,999"]<-24
mydata$inc[mydata$inc_incgroup_pre=="26. $150,000-$174,999"]<-25
mydata$inc[mydata$inc_incgroup_pre=="27. $175,000-$249,999"]<-26
mydata$inc[mydata$inc_incgroup_pre=="28. $250,000 or more"]<-27
table(mydata$inc)

#religiosity
table(mydata$relig_churchoft)
table(mydata$relig_churchwk)
mydata$rel<-NA
mydata$rel[mydata$relig_churchoft=="-1. Inapplicable"]<-0 ##2,368 people here that don't attend church so they should be with nevers
mydata$rel[mydata$relig_churchoft=="5. Never"]<-0
mydata$rel[mydata$relig_churchoft=="4. A few times a year"]<-.2
mydata$rel[mydata$relig_churchoft=="3. Once or twice a month"]<-.4
mydata$rel[mydata$relig_churchoft=="2. Almost every week"]<-.6
mydata$rel[mydata$relig_churchwk=="1. Once a week"]<-.8
mydata$rel[mydata$relig_churchwk=="2. More often than once a week"]<-1
table(mydata$rel)

table(mydata$relig_guide)
mydata$relguide<-NA
mydata$relguide[mydata$relig_guide=="-1. Inapplicable"]<-0 
mydata$relguide[mydata$relig_guide=="1. Some"]<-.333333
mydata$relguide[mydata$relig_guide=="2. Quite a bit"]<-.6666666
mydata$relguide[mydata$relig_guide=="3. A great deal"]<-1
table(mydata$relguide)

table(mydata$relig_pray)
mydata$pray<-NA
mydata$pray[mydata$relig_pray=="05. Never"]<-0
mydata$pray[mydata$relig_pray=="04. Once a week or less"]<-.25
mydata$pray[mydata$relig_pray=="03. A few times a week"]<-.5
mydata$pray[mydata$relig_pray=="02. Once a day"]<-.75
mydata$pray[mydata$relig_pray=="01. Several times a day"]<-1
table(mydata$pray)

mydata$religiosity<-(mydata$rel+mydata$relguide+mydata$pray)/3

#other variables Feldman and jonston looked at
#High values mean more consevative answers
#women in workplace or home
#old var name: women_works_x
table(mydata$women_works)
mydata$womanhome<-NA
mydata$womanhome[mydata$women_works=="1. Better"]<-1
mydata$womanhome[mydata$women_works=="2. Worse"]<-0
mydata$womanhome[mydata$women_works=="3. Makes no difference"]<-0
table(mydata$womanhome)

#government services-spending
table(mydata$spsrvpr_ssself)
mydata$govspend<-NA
mydata$govspend[mydata$spsrvpr_ssself=="1"]<-1
mydata$govspend[mydata$spsrvpr_ssself=="2"]<-0.8333333
mydata$govspend[mydata$spsrvpr_ssself=="3"]<-0.6666667
mydata$govspend[mydata$spsrvpr_ssself=="4"]<-0.5
mydata$govspend[mydata$spsrvpr_ssself=="5"]<-0.3333333
mydata$govspend[mydata$spsrvpr_ssself=="6"]<-0.1666667
mydata$govspend[mydata$spsrvpr_ssself=="7"]<-0
table(mydata$govspend)

#medical insurance
table(mydata$inspre_self) 
mydata$med<-NA
mydata$med[mydata$inspre_self=="1"]<-0
mydata$med[mydata$inspre_self=="2"]<-0.1666667
mydata$med[mydata$inspre_self=="3"]<-0.3333333
mydata$med[mydata$inspre_self=="4"]<-0.5
mydata$med[mydata$inspre_self=="5"]<-0.6666667
mydata$med[mydata$inspre_self=="6"]<-0.8333333
mydata$med[mydata$inspre_self=="7"]<-1
table(mydata$med)

#guaranteed jobs
table(mydata$guarpr_self)
mydata$job<-NA
mydata$job[mydata$guarpr_self=="1"]<-0
mydata$job[mydata$guarpr_self=="2"]<-0.1666667
mydata$job[mydata$guarpr_self=="3"]<-0.3333333
mydata$job[mydata$guarpr_self=="4"]<-0.5
mydata$job[mydata$guarpr_self=="5"]<-0.6666667
mydata$job[mydata$guarpr_self=="6"]<-0.8333333
mydata$job[mydata$guarpr_self=="7"]<-1
table(mydata$job)

#assistance to the poor
table(mydata$fedspend_poor)
mydata$poor<-NA
mydata$poor[mydata$fedspend_poor=="1. Increased"]<-0
mydata$poor[mydata$fedspend_poor=="3. [kept about the same/ kept the same]"]<-.5
mydata$poor[mydata$fedspend_poor=="2. Decreased"]<-1
table(mydata$poor)

#abortion
table(mydata$abortpre_4point)
mydata$abort<-NA
mydata$abort[mydata$abortpre_4point=="1. By law, abortion should never be permitted."]<-1
mydata$abort[mydata$abortpre_4point=="2. The law should permit abortion only in case of rape, incest, or when the woman's life is in danger"]<-0.6666667
mydata$abort[mydata$abortpre_4point=="3. The law should permit abortion for reasons other than rape, incest, or danger to the woman"]<-0.3333333
mydata$abort[mydata$abortpre_4point=="4. By law, a woman should always be able to obtain an abortion as a matter of personal choice."]<-0
table(mydata$abort)

#gay adoption
table(mydata$gayrt_adopt)
mydata$gayad<-NA
mydata$gayad[mydata$gayrt_adopt=="1. Yes"]<-0
mydata$gayad[mydata$gayrt_adopt=="2. No"]<-1
table(mydata$gayad)

#Economic index
#government spending+medical insurance+guaranteed jobs + assistance to poor
mydata$econ<-NA
mydata$econ<-(mydata$govspend+mydata$med+mydata$job+mydata$poor)/4
table(mydata$econ)
#social index
#abortion+gay adoption+women's role
mydata$social<-NA
mydata$social<-(mydata$abort+mydata$gayad+mydata$womanhome)/3
table(mydata$social)


#MTurk Recoding

mydata2 <- read.xlsx("C:/Users/Ryan/Downloads/MTurk7.xlsx", 1)

##looking at pid from qualtrics data
table(mydata2$pid) #1 democrat, 2 republican, or 3 independent
table(mydata2$pidd) #1 strong democrat, 2 not strong democrat
table(mydata2$pidr) #1 strong republican, 2 not strong republican
table(mydata2$pidi) # 1 closer to D, 2 closer to R, 3 neiher

##Recoding Partisanship
mydata2$PID<-NA
mydata2$PID[mydata2$pidd=='1'] <-1
mydata2$PID[mydata2$pidd=='2'] <-2
mydata2$PID[mydata2$pidi=='1']<-3
mydata2$PID[mydata2$pidi=='3']<-4
mydata2$PID[mydata2$pidi=='2']<-5
mydata2$PID[mydata2$pidr=='2']<-6
mydata2$PID[mydata2$pidr=='1']<-7
table(mydata2$PID)
table(mydata2$pid)
##Looking at ideol from qualtrics data
table(mydata2$ideo) #1-7 extreme lib -> extrem con

##looking at and recoding demographics
#age
table(mydata2$born)
mydata2$age<-(mydata2$born+3-17)/58
table(mydata2$age)
summary(mydata2$age)
#sex
table(mydata2$Q21) #1 male, 2 female
mydata2$male<-NA
mydata2$male[mydata2$Q21=='1'] <-1
mydata2$male[mydata2$Q21=='2'] <-0
table(mydata2$male)

#education
table(mydata2$educ_TEXT)
table(mydata2$educ)
mydata2$edu<-NA
mydata2$edu[mydata2$educ=="21"]<-NA #other
mydata2$edu[mydata2$educ=="6"]<-1
mydata2$edu[mydata2$educ=="7"]<-1
mydata2$edu[mydata2$educ=="8"]<-1
mydata2$edu[mydata2$educ=="9"]<-1
mydata2$edu[mydata2$educ=="10"]<-1
mydata2$edu[mydata2$educ=="11"]<-1
mydata2$edu[mydata2$educ=="12"]<-1
mydata2$edu[mydata2$educ=="13"]<-1#less than HS diploma
mydata2$edu[mydata2$educ=="14"]<-2#HS diploma
mydata2$edu[mydata2$educ=="15"]<-3#Some college
mydata2$edu[mydata2$educ=="16"]<-3
mydata2$edu[mydata2$educ=="17"]<-3
mydata2$edu[mydata2$educ=="18"]<-4#BA
mydata2$edu[mydata2$educ=="19"]<-5#grad degree
mydata2$edu[mydata2$educ=="20"]<-5
mydata2$edu[mydata2$educ_TEXT=="A JD is a Doctorate - Juris Doctor, not a Master's Degree"]<-5 #this guy...
mydata2$edu[mydata2$educ_TEXT=="Finished from a technical school"]<-3
mydata2$edu[mydata2$educ_TEXT=="MD NOTE: this is NOT a masters level.  Please correct."]<-5
mydata2$edu[mydata2$educ_TEXT=="Vocational School"]<-3
mydata2$edu[mydata2$educ_TEXT=="JD"]<-5
mydata2$edu[mydata2$educ_TEXT=="Technical School Graduate after high school"]<-3#grad degree


table(mydata2$edu)

#income
table(mydata2$income)
mydata2$inc<- mydata2$income -3
table(mydata2$inc)

#religiosity
table(mydata2$attenda)
table(mydata2$attendb)#reverse order church attendance
mydata2$rel<-NA
mydata2$rel[mydata2$attenda=="8"]<-0
mydata2$rel[mydata2$attenda=="7"]<-.25
mydata2$rel[mydata2$attenda=="6"]<-.5
mydata2$rel[mydata2$attenda=="5"]<-.75
mydata2$rel[mydata2$attenda=="4"]<-1
mydata2$rel[mydata2$attendb=="4"]<-0
mydata2$rel[mydata2$attendb=="5"]<-.25
mydata2$rel[mydata2$attendb=="6"]<-.5
mydata2$rel[mydata2$attendb=="7"]<-.75
mydata2$rel[mydata2$attendb=="8"]<-1
table(mydata2$rel)

#religion provides guidance?
table(mydata2$religguide)
mydata2$relguide<-NA
mydata2$relguide[mydata2$religguide=="4"]<-0 #No
mydata2$relguide[mydata2$religguide=="5"]<-.333333
mydata2$relguide[mydata2$religguide=="6"]<-.6666666
mydata2$relguide[mydata2$religguide=="7"]<-1 #strong yes
table(mydata2$relguide)

table(mydata2$pray)
mydata2$PRAY<-NA
mydata2$PRAY[mydata2$pray=="8"]<-0 #never pray
mydata2$PRAY[mydata2$pray=="7"]<-.25
mydata2$PRAY[mydata2$pray=="6"]<-.5
mydata2$PRAY[mydata2$pray=="5"]<-.75
mydata2$PRAY[mydata2$pray=="4"]<-1 #pray the most
table(mydata2$PRAY)

mydata2$religiosity<-(mydata2$rel+mydata2$relguide+mydata2$PRAY)/3
mydata2$religiosity

#recoding TIPI from MTurk data
#Recoding big 5 to be "positive" and averaging questions
##extraversion
table(mydata2$tipi_extra)
mydata2$extra<-NA
mydata2$extra[mydata2$tipi_extra=="4"]<-1
mydata2$extra[mydata2$tipi_extra=="5"]<-2
mydata2$extra[mydata2$tipi_extra=="6"]<-3
mydata2$extra[mydata2$tipi_extra=="7"]<-4
mydata2$extra[mydata2$tipi_extra=="8"]<-5
mydata2$extra[mydata2$tipi_extra=="9"]<-6
mydata2$extra[mydata2$tipi_extra=="10"]<-7
table(mydata2$extra)

table(mydata2$tipi_reser)
mydata2$resv<-NA
mydata2$resv[mydata2$tipi_reser=="4"]<-7
mydata2$resv[mydata2$tipi_reser=="5"]<-6
mydata2$resv[mydata2$tipi_reser=="6"]<-5
mydata2$resv[mydata2$tipi_reser=="7"]<-4
mydata2$resv[mydata2$tipi_reser=="8"]<-3
mydata2$resv[mydata2$tipi_reser=="9"]<-2
mydata2$resv[mydata2$tipi_reser=="10"]<-1
table(mydata2$resv)

mydata2$extraversion<- (mydata2$extra+mydata2$resv)/2
mydata2$extraversion

##Agreeableness
table(mydata2$tipi_criti)
mydata2$crit<-NA
mydata2$crit[mydata2$tipi_criti=="4"]<-7
mydata2$crit[mydata2$tipi_criti=="5"]<-6
mydata2$crit[mydata2$tipi_criti=="6"]<-5
mydata2$crit[mydata2$tipi_criti=="7"]<-4
mydata2$crit[mydata2$tipi_criti=="8"]<-3
mydata2$crit[mydata2$tipi_criti=="9"]<-2
mydata2$crit[mydata2$tipi_criti=="10"]<-1
mydata2$crit

mydata2$tipi_sympa
mydata2$warm<-NA
mydata2$warm[mydata2$tipi_sympa=="4"]<-1
mydata2$warm[mydata2$tipi_sympa=="5"]<-2
mydata2$warm[mydata2$tipi_sympa=="6"]<-3
mydata2$warm[mydata2$tipi_sympa=="7"]<-4
mydata2$warm[mydata2$tipi_sympa=="8"]<-5
mydata2$warm[mydata2$tipi_sympa=="9"]<-6
mydata2$warm[mydata2$tipi_sympa=="10"]<-7
mydata2$warm

mydata2$agreeableness<- (mydata2$crit+mydata2$warm)/2
mydata2$agreeableness

mean(mydata$agreeableness, na.rm = TRUE)
##conscientiousness
mydata2$tipi_depen
mydata2$dep<-NA
mydata2$dep[mydata2$tipi_depen=="4"]<-1
mydata2$dep[mydata2$tipi_depen=="5"]<-2
mydata2$dep[mydata2$tipi_depen=="6"]<-3
mydata2$dep[mydata2$tipi_depen=="7"]<-4
mydata2$dep[mydata2$tipi_depen=="8"]<-5
mydata2$dep[mydata2$tipi_depen=="9"]<-6
mydata2$dep[mydata2$tipi_depen=="10"]<-7
mydata2$dep

mydata2$tipi_disor
mydata2$disorg<-NA
mydata2$disorg[mydata2$tipi_disor=="4"]<-7
mydata2$disorg[mydata2$tipi_disor=="5"]<-6
mydata2$disorg[mydata2$tipi_disor=="6"]<-5
mydata2$disorg[mydata2$tipi_disor=="7"]<-4
mydata2$disorg[mydata2$tipi_disor=="8"]<-3
mydata2$disorg[mydata2$tipi_disor=="9"]<-2
mydata2$disorg[mydata2$tipi_disor=="10"]<-1
mydata2$disorg

mydata2$conscientiousness<- (mydata2$dep+mydata2$disorg)/2
mydata2$conscientiousness


##emotional stability
mydata2$tipi_anxio
mydata2$anx<-NA
mydata2$anx[mydata2$tipi_anxio=="4"]<-7
mydata2$anx[mydata2$tipi_anxio=="5"]<-6
mydata2$anx[mydata2$tipi_anxio=="6"]<-5
mydata2$anx[mydata2$tipi_anxio=="7"]<-4
mydata2$anx[mydata2$tipi_anxio=="8"]<-3
mydata2$anx[mydata2$tipi_anxio=="9"]<-2
mydata2$anx[mydata2$tipi_anxio=="10"]<-1
mydata2$anx

mydata2$tipi_calm
mydata2$calm<-NA
mydata2$calm[mydata2$tipi_calm=="4"]<-1
mydata2$calm[mydata2$tipi_calm=="5"]<-2
mydata2$calm[mydata2$tipi_calm=="6"]<-3
mydata2$calm[mydata2$tipi_calm=="7"]<-4
mydata2$calm[mydata2$tipi_calm=="8"]<-5
mydata2$calm[mydata2$tipi_calm=="9"]<-6
mydata2$calm[mydata2$tipi_calm=="10"]<-7
mydata2$calm

mydata2$emostab<- (mydata2$anx+mydata2$calm)/2
mydata2$emostab

##openness
mydata2$tipi_open
mydata2$open<-NA
mydata2$open[mydata2$tipi_open=="4"]<-1
mydata2$open[mydata2$tipi_open=="5"]<-2
mydata2$open[mydata2$tipi_open=="6"]<-3
mydata2$open[mydata2$tipi_open=="7"]<-4
mydata2$open[mydata2$tipi_open=="8"]<-5
mydata2$open[mydata2$tipi_open=="9"]<-6
mydata2$open[mydata2$tipi_open=="10"]<-7
mydata2$open

mydata2$tipi_conve
mydata2$conv<-NA
mydata2$conv[mydata2$tipi_conve=="4"]<-7
mydata2$conv[mydata2$tipi_conve=="5"]<-6
mydata2$conv[mydata2$tipi_conve=="6"]<-5
mydata2$conv[mydata2$tipi_conve=="7"]<-4
mydata2$conv[mydata2$tipi_conve=="8"]<-3
mydata2$conv[mydata2$tipi_conve=="9"]<-2
mydata2$conv[mydata2$tipi_conve=="10"]<-1
mydata2$conv

mydata2$openness<- (mydata2$open+mydata2$conv)/2
mydata2$openness
mean(mydata$openness, na.rm = TRUE)
#Recoding scales
##Recoding authoritarianism scale to be more "positive" as levels of author increase
table(mydata2$auth1)
mydata2$respect1<-NA
mydata2$respect1[mydata2$auth1=="2"]<-1
mydata2$respect1[mydata2$auth1=="1"]<-0
mydata2$respect1

table(mydata2$auth3)
mydata2$obedience1<-NA
mydata2$obedience1[mydata2$auth3=="1"]<-1
mydata2$obedience1[mydata2$auth3=="2"]<-0
mydata2$obedience1

mydata2$auth2
mydata2$manners1<-NA
mydata2$manners1[mydata2$auth2=="2"]<-1
mydata2$manners1[mydata2$auth2=="1"]<-0
mydata2$manners1

mydata2$auth4
mydata2$behave1<-NA
mydata2$behave1[mydata2$auth4=="2"]<-1
mydata2$behave1[mydata2$auth4=="1"]<-0
mydata2$behave1



##making authoritarianism scale
mydata2$ascale<-(mydata2$respect1+mydata2$obedience1+mydata2$manners1+mydata2$behave1)/4
mydata2$ascale

##Recoding Racial resentment scale to be more "positive" as levels of resentment increase
table(mydata2$rr1)
mydata2$overcome<-NA
mydata2$overcome[mydata2$rr1=="4"]<-1
mydata2$overcome[mydata2$rr1=="5"]<-.75
mydata2$overcome[mydata2$rr1=="6"]<-.5
mydata2$overcome[mydata2$rr1=="7"]<-.25
mydata2$overcome[mydata2$rr1=="8"]<-0
table(mydata2$overcome)

table(mydata2$rr2)
mydata2$slavery<-NA
mydata2$slavery[mydata2$rr2=="4"]<-0
mydata2$slavery[mydata2$rr2=="5"]<-.25
mydata2$slavery[mydata2$rr2=="6"]<-.5
mydata2$slavery[mydata2$rr2=="7"]<-.75
mydata2$slavery[mydata2$rr2=="8"]<-1
table(mydata2$slavery)

table(mydata2$rr3)
mydata2$deserve<-NA
mydata2$deserve[mydata2$rr3=="4"]<-0
mydata2$deserve[mydata2$rr3=="5"]<-.25
mydata2$deserve[mydata2$rr3=="6"]<-.5
mydata2$deserve[mydata2$rr3=="7"]<-.75
mydata2$deserve[mydata2$rr3=="8"]<-1
table(mydata2$deserve)

table(mydata2$rr4)
mydata2$harder<-NA
mydata2$harder[mydata2$rr4=="4"]<-1
mydata2$harder[mydata2$rr4=="5"]<-.75
mydata2$harder[mydata2$rr4=="6"]<-.5
mydata2$harder[mydata2$rr4=="7"]<-.25
mydata2$harder[mydata2$rr4=="8"]<-0
table(mydata2$harder)

##making Racial Resentment scale
mydata2$rrscale<-(mydata2$overcome+mydata2$slavery+mydata2$deserve+mydata2$harder)/4
mydata2$rrscale

##Recoding Moral traditionalism scale to be more "positive" as levels of traditionalism increase
table(mydata2$tradchange)
mydata2$adjust<-NA
mydata2$adjust[mydata2$tradchange=="4"]<-0
mydata2$adjust[mydata2$tradchange=="5"]<-.25
mydata2$adjust[mydata2$tradchange=="6"]<-.5
mydata2$adjust[mydata2$tradchange=="7"]<-.75
mydata2$adjust[mydata2$tradchange=="8"]<-1
table(mydata2$adjust)

table(mydata2$tradnewer)
mydata2$lifestyle<-NA
mydata2$lifestyle[mydata2$tradnewer=="4"]<-1
mydata2$lifestyle[mydata2$tradnewer=="5"]<-.75
mydata2$lifestyle[mydata2$tradnewer=="6"]<-.5
mydata2$lifestyle[mydata2$tradnewer=="7"]<-.25
mydata2$lifestyle[mydata2$tradnewer=="8"]<-0
table(mydata2$lifestyle)

table(mydata2$tradtolera)
mydata2$tolerant<-NA
mydata2$tolerant[mydata2$tradtolera=="4"]<-0
mydata2$tolerant[mydata2$tradtolera=="5"]<-.25
mydata2$tolerant[mydata2$tradtolera=="6"]<-.5
mydata2$tolerant[mydata2$tradtolera=="7"]<-.75
mydata2$tolerant[mydata2$tradtolera=="8"]<-1
table(mydata2$tolerant)

table(mydata2$tradfewer)
mydata2$famval<-NA
mydata2$famval[mydata2$tradfewer=="4"]<-1
mydata2$famval[mydata2$tradfewer=="5"]<-.75
mydata2$famval[mydata2$tradfewer=="6"]<-.5
mydata2$famval[mydata2$tradfewer=="7"]<-.25
mydata2$famval[mydata2$tradfewer=="8"]<-0
table(mydata2$famval)

##making moral traditionalism scale
mydata2$mtscale<-(mydata2$adjust+mydata2$lifestyle+mydata2$tolerant+mydata2$famval)/4
mydata2$mtscale

##Recoding Egalitarianism scale to be more "positive" as levels of Egalitarianism increase
table(mydata2$egal1)
mydata2$equal<-NA
mydata2$equal[mydata2$egal1=="4"]<-1
mydata2$equal[mydata2$egal1=="5"]<-.75
mydata2$equal[mydata2$egal1=="6"]<-.5
mydata2$equal[mydata2$egal1=="7"]<-.25
mydata2$equal[mydata2$egal1=="8"]<-0
table(mydata2$equal)

table(mydata2$egal2)
mydata2$toofar<-NA
mydata2$toofar[mydata2$egal2=="4"]<-0
mydata2$toofar[mydata2$egal2=="5"]<-.25
mydata2$toofar[mydata2$egal2=="6"]<-.5
mydata2$toofar[mydata2$egal2=="7"]<-.75
mydata2$toofar[mydata2$egal2=="8"]<-1
table(mydata2$toofar)

table(mydata2$egal3)
mydata2$bigprob<-NA
mydata2$bigprob[mydata2$egal3=="4"]<-1
mydata2$bigprob[mydata2$egal3=="5"]<-.75
mydata2$bigprob[mydata2$egal3=="6"]<-.5
mydata2$bigprob[mydata2$egal3=="7"]<-.25
mydata2$bigprob[mydata2$egal3=="8"]<-0
table(mydata2$bigprob)

table(mydata2$egal4)
mydata2$worryless<-NA
mydata2$worryless[mydata2$egal4=="4"]<-0
mydata2$worryless[mydata2$egal4=="5"]<-.25
mydata2$worryless[mydata2$egal4=="6"]<-.5
mydata2$worryless[mydata2$egal4=="7"]<-.75
mydata2$worryless[mydata2$egal4=="8"]<-1
table(mydata2$worryless)

table(mydata2$egal5)
mydata2$notbigprob<-NA
mydata2$notbigprob[mydata2$egal5=="4"]<-0
mydata2$notbigprob[mydata2$egal5=="5"]<-.25
mydata2$notbigprob[mydata2$egal5=="6"]<-.5
mydata2$notbigprob[mydata2$egal5=="7"]<-.75
mydata2$notbigprob[mydata2$egal5=="8"]<-1
table(mydata2$notbigprob)

table(mydata2$egal6)
mydata2$fewerprobs<-NA
mydata2$fewerprobs[mydata2$egal6=="4"]<-1
mydata2$fewerprobs[mydata2$egal6=="5"]<-.75
mydata2$fewerprobs[mydata2$egal6=="6"]<-.5
mydata2$fewerprobs[mydata2$egal6=="7"]<-.25
mydata2$fewerprobs[mydata2$egal6=="8"]<-0
table(mydata2$fewerprobs)

##making Egalitarianism scale
mydata2$escale<-(mydata2$equal+mydata2$toofar+mydata2$bigprob+mydata2$worryless+mydata2$notbigprob+mydata2$fewerprobs)/6
mydata2$escale

##Looking at issue stances and recoding
#High values mean more consevative answers
#women in workplace or home
table(mydata2$iss_woman)
mydata2$womanhome<-NA
mydata2$womanhome[mydata2$iss_woman=="4"]<-1
mydata2$womanhome[mydata2$iss_woman=="5"]<-0
mydata2$womanhome[mydata2$iss_woman=="6"]<-0
table(mydata2$womanhome)

#government services-spending
table(mydata2$iss_serv)
mydata2$govspend<-NA
mydata2$govspend[mydata2$iss_serv=="4"]<-1
mydata2$govspend[mydata2$iss_serv=="5"]<-0.8333333
mydata2$govspend[mydata2$iss_serv=="6"]<-0.6666667
mydata2$govspend[mydata2$iss_serv=="7"]<-0.5
mydata2$govspend[mydata2$iss_serv=="8"]<-0.3333333
mydata2$govspend[mydata2$iss_serv=="9"]<-0.1666667
mydata2$govspend[mydata2$iss_serv=="10"]<-0
table(mydata2$govspend)

#medical insurance
table(mydata2$iss_med)
mydata2$med<-NA
mydata2$med[mydata2$iss_med=="20"]<-0
mydata2$med[mydata2$iss_med=="21"]<-0.1666667
mydata2$med[mydata2$iss_med=="22"]<-0.3333333
mydata2$med[mydata2$iss_med=="23"]<-0.5
mydata2$med[mydata2$iss_med=="24"]<-0.6666667
mydata2$med[mydata2$iss_med=="25"]<-0.8333333
mydata2$med[mydata2$iss_med=="26"]<-1
table(mydata2$med)

#guaranteed jobs
table(mydata2$iss_jobs)
mydata2$job<-NA
mydata2$job[mydata2$iss_jobs=="4"]<-0
mydata2$job[mydata2$iss_jobs=="5"]<-0.1666667
mydata2$job[mydata2$iss_jobs=="6"]<-0.3333333
mydata2$job[mydata2$iss_jobs=="7"]<-0.5
mydata2$job[mydata2$iss_jobs=="8"]<-0.6666667
mydata2$job[mydata2$iss_jobs=="9"]<-0.8333333
mydata2$job[mydata2$iss_jobs=="10"]<-1
table(mydata2$job)

#assistance to the poor
table(mydata2$iss_poor)
mydata2$poor<-NA
mydata2$poor[mydata2$iss_poor=="4"]<-0
mydata2$poor[mydata2$iss_poor=="5"]<-.5
mydata2$poor[mydata2$iss_poor=="6"]<-1
table(mydata2$poor)

#abortion
table(mydata2$iss_abort)
mydata2$abort<-NA
mydata2$abort[mydata2$iss_abort=="4"]<-1
mydata2$abort[mydata2$iss_abort=="5"]<-0.6666667
mydata2$abort[mydata2$iss_abort=="6"]<-0.3333333
mydata2$abort[mydata2$iss_abort=="7"]<-0
table(mydata2$abort)

#gay adoption
table(mydata2$iss_gayad)
mydata2$gayad<-NA
mydata2$gayad[mydata2$iss_gayad=="1"]<-0
mydata2$gayad[mydata2$iss_gayad=="2"]<-1
table(mydata2$gayad)

#Economic index
#government spending+medical insurance+guaranteed jobs + assistance to poor
mydata2$econ<-NA
mydata2$econ<-(mydata2$govspend+mydata2$med+mydata2$job+mydata2$poor)/4
table(mydata2$econ)
#social index
#abortion+gay adoption+women's role
mydata2$social<-NA
mydata2$social<-(mydata2$abort+mydata2$gayad+mydata2$womanhome)/3
table(mydata2$social)


#first recode PID and Ideo to be 7 point scales
##Recoding Partisanship
mydata2$PID3<-NA
mydata2$PID3[mydata2$pidd=='1'] <-1
mydata2$PID3[mydata2$pidd=='2'] <-2
mydata2$PID3[mydata2$pidi=='1']<-3
mydata2$PID3[mydata2$pidi=='3']<-4
mydata2$PID3[mydata2$pidi=='2']<-5
mydata2$PID3[mydata2$pidr=='2']<-6
mydata2$PID3[mydata2$pidr=='1']<-7
table(mydata2$PID3)
table(mydata2$pidi)
##recoing three point IDEO measure
table(mydata2$ideo) #1-7 extreme lib -> extrem con
mydata2$ideo3<-NA
mydata2$ideo3[mydata2$ideo=='1'] <-1
mydata2$ideo3[mydata2$ideo=='2'] <-2
mydata2$ideo3[mydata2$ideo=='3']<-3
mydata2$ideo3[mydata2$ideo=='4']<-4
mydata2$ideo3[mydata2$ideo=='5']<-5
mydata2$ideo3[mydata2$ideo=='6']<-6
mydata2$ideo3[mydata2$ideo=='7']<-7
table(mydata2$ideo3)

##assinging measures of ideo and partisanship to letters
table(mydata$PID3)
R333=mydata2$PID3 ==7
R222=mydata2$PID3 ==6
R111=mydata2$PID3 ==5
I111=mydata2$PID3 ==4
D111=mydata2$PID3 ==3
D222=mydata2$PID3 ==2
D333=mydata2$PID3 ==1


C333=mydata2$ideo3 ==7
C222=mydata2$ideo3 ==6
C111=mydata2$ideo3 ==5
M111=mydata2$ideo3 ==4
L111=mydata2$ideo3 ==3
L222=mydata2$ideo3 ==2
L333=mydata2$ideo3 ==1

##making graph for extraversion
##plotting critical values and se for PID
##for FTF
##and for internet

df1 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$extraversion)*1.96,std.error(mydata[R2,]$extraversion)*1.96,std.error(mydata[R1,]$extraversion)*1.96,
               std.error(mydata[I1,]$extraversion)*1.96,std.error(mydata[D1,]$extraversion)*1.96,std.error(mydata[D2,]$extraversion)*1.96,
               std.error(mydata[D3,]$extraversion)*1.96,std.error(mydata[R33,]$extraversion)*1.96,std.error(mydata[R22,]$extraversion)*1.96,
               std.error(mydata[R11,]$extraversion)*1.96,std.error(mydata[I11,]$extraversion)*1.96,std.error(mydata[D11,]$extraversion)*1.96,
               std.error(mydata[D22,]$extraversion)*1.96,std.error(mydata[D33,]$extraversion)*1.96,std.error(mydata2[R333,]$extraversion)*1.96,std.error(mydata2[R222,]$extraversion)*1.96,
               std.error(mydata2[R111,]$extraversion)*1.96,std.error(mydata2[I111,]$extraversion)*1.96,std.error(mydata2[D111,]$extraversion)*1.96,
               std.error(mydata2[D222,]$extraversion)*1.96,std.error(mydata2[D333,]$extraversion)*1.96)),
  Means =c(mean(mydata[R3,]$extraversion, na.rm=TRUE),mean(mydata[R2,]$extraversion, na.rm=TRUE),mean(mydata[R1,]$extraversion, na.rm=TRUE),
           mean(mydata[I1,]$extraversion, na.rm=TRUE),mean(mydata[D1,]$extraversion, na.rm=TRUE),mean(mydata[D2,]$extraversion, na.rm=TRUE),
           mean(mydata[D3,]$extraversion, na.rm=TRUE),mean(mydata[R33,]$extraversion, na.rm=TRUE),mean(mydata[R22,]$extraversion, na.rm=TRUE),
           mean(mydata[R11,]$extraversion, na.rm=TRUE),mean(mydata[I11,]$extraversion, na.rm=TRUE),mean(mydata[D11,]$extraversion, na.rm=TRUE),
           mean(mydata[D22,]$extraversion, na.rm=TRUE),mean(mydata[D33,]$extraversion, na.rm=TRUE),mean(mydata2[R333,]$extraversion, na.rm=TRUE),
           mean(mydata2[R222,]$extraversion, na.rm=TRUE),mean(mydata2[R111,]$extraversion, na.rm=TRUE),mean(mydata2[I111,]$extraversion, na.rm=TRUE),
           mean(mydata2[D111,]$extraversion, na.rm=TRUE),mean(mydata2[D222,]$extraversion, na.rm=TRUE),mean(mydata2[D333,]$extraversion, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df1

limits <- aes(ymax = Means + se, ymin=Means - se)

##+ theme(legend.position="none") save for later

a <- ggplot(df1, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset)) + theme(legend.position="none") + geom_pointrange(limits)+ggtitle("Extraversion")+
  ylim(1,7)  +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Partisanship")+ylab("Level of Extraversion (1 = Low - 7 = High)")+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .04   Web: r = .00   MTurk: r = .02", x = 4, y = 1.5, size = 4, colour = "black")
a 

##making graph for agreeableness mydata$agreeableness
##plotting critical values and se for PID
##for FTF and internet



df2 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$agreeableness)*1.96,std.error(mydata[R2,]$agreeableness)*1.96,std.error(mydata[R1,]$agreeableness)*1.96,
               std.error(mydata[I1,]$agreeableness)*1.96,std.error(mydata[D1,]$agreeableness)*1.96,std.error(mydata[D2,]$agreeableness)*1.96,
               std.error(mydata[D3,]$agreeableness)*1.96,std.error(mydata[R33,]$agreeableness)*1.96,std.error(mydata[R22,]$agreeableness)*1.96,
               std.error(mydata[R11,]$agreeableness)*1.96,std.error(mydata[I11,]$agreeableness)*1.96,std.error(mydata[D11,]$agreeableness)*1.96,
               std.error(mydata[D22,]$agreeableness)*1.96,std.error(mydata[D33,]$agreeableness)*1.96,std.error(mydata2[R333,]$agreeableness)*1.96,std.error(mydata2[R222,]$agreeableness)*1.96,
               std.error(mydata2[R111,]$agreeableness)*1.96,std.error(mydata2[I111,]$agreeableness)*1.96,std.error(mydata2[D111,]$agreeableness)*1.96,
               std.error(mydata2[D222,]$agreeableness)*1.96,std.error(mydata2[D333,]$agreeableness)*1.96)),
  Means =c(mean(mydata[R3,]$agreeableness, na.rm=TRUE),mean(mydata[R2,]$agreeableness, na.rm=TRUE),mean(mydata[R1,]$agreeableness, na.rm=TRUE),
           mean(mydata[I1,]$agreeableness, na.rm=TRUE),mean(mydata[D1,]$agreeableness, na.rm=TRUE),mean(mydata[D2,]$agreeableness, na.rm=TRUE),
           mean(mydata[D3,]$agreeableness, na.rm=TRUE),mean(mydata[R33,]$agreeableness, na.rm=TRUE),mean(mydata[R22,]$agreeableness, na.rm=TRUE),
           mean(mydata[R11,]$agreeableness, na.rm=TRUE),mean(mydata[I11,]$agreeableness, na.rm=TRUE),mean(mydata[D11,]$agreeableness, na.rm=TRUE),
           mean(mydata[D22,]$agreeableness, na.rm=TRUE),mean(mydata[D33,]$agreeableness, na.rm=TRUE),mean(mydata2[R333,]$agreeableness, na.rm=TRUE),
           mean(mydata2[R222,]$agreeableness, na.rm=TRUE),mean(mydata2[R111,]$agreeableness, na.rm=TRUE),mean(mydata2[I111,]$agreeableness, na.rm=TRUE),
           mean(mydata2[D111,]$agreeableness, na.rm=TRUE),mean(mydata2[D222,]$agreeableness, na.rm=TRUE),mean(mydata2[D333,]$agreeableness, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df2


limits2 <- aes(ymax = Means + se, ymin=Means - se)


b <- ggplot(df2, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits2)+ggtitle("Agreeableness")+
  ylim(1,7)  + xlab("Partisanship")+ylab("Level of Agreeableness (1 = Low - 7 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .00   Web: r = -.02   MTurk: r = -.06*", x = 4, y = 1.5, size = 4, colour = "black")

b 

##making graph for conscientiousness mydata$conscientiousness
##plotting critical values and se for PID
##for FTF
##for internet

df3 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$conscientiousness)*1.96,std.error(mydata[R2,]$conscientiousness)*1.96,std.error(mydata[R1,]$conscientiousness)*1.96,
               std.error(mydata[I1,]$conscientiousness)*1.96,std.error(mydata[D1,]$conscientiousness)*1.96,std.error(mydata[D2,]$conscientiousness)*1.96,
               std.error(mydata[D3,]$conscientiousness)*1.96,std.error(mydata[R33,]$conscientiousness)*1.96,std.error(mydata[R22,]$conscientiousness)*1.96,
               std.error(mydata[R11,]$conscientiousness)*1.96,std.error(mydata[I11,]$conscientiousness)*1.96,std.error(mydata[D11,]$conscientiousness)*1.96,
               std.error(mydata[D22,]$conscientiousness)*1.96,std.error(mydata[D33,]$conscientiousness)*1.96,std.error(mydata2[R333,]$conscientiousness)*1.96,std.error(mydata2[R222,]$conscientiousness)*1.96,
               std.error(mydata2[R111,]$conscientiousness)*1.96,std.error(mydata2[I111,]$conscientiousness)*1.96,std.error(mydata2[D111,]$conscientiousness)*1.96,
               std.error(mydata2[D222,]$conscientiousness)*1.96,std.error(mydata2[D333,]$conscientiousness)*1.96)),
  Means =c(mean(mydata[R3,]$conscientiousness, na.rm=TRUE),mean(mydata[R2,]$conscientiousness, na.rm=TRUE),mean(mydata[R1,]$conscientiousness, na.rm=TRUE),
           mean(mydata[I1,]$conscientiousness, na.rm=TRUE),mean(mydata[D1,]$conscientiousness, na.rm=TRUE),mean(mydata[D2,]$conscientiousness, na.rm=TRUE),
           mean(mydata[D3,]$conscientiousness, na.rm=TRUE),mean(mydata[R33,]$conscientiousness, na.rm=TRUE),mean(mydata[R22,]$conscientiousness, na.rm=TRUE),
           mean(mydata[R11,]$conscientiousness, na.rm=TRUE),mean(mydata[I11,]$conscientiousness, na.rm=TRUE),mean(mydata[D11,]$conscientiousness, na.rm=TRUE),
           mean(mydata[D22,]$conscientiousness, na.rm=TRUE),mean(mydata[D33,]$conscientiousness, na.rm=TRUE),mean(mydata2[R333,]$conscientiousness, na.rm=TRUE),
           mean(mydata2[R222,]$conscientiousness, na.rm=TRUE),mean(mydata2[R111,]$conscientiousness, na.rm=TRUE),mean(mydata2[I111,]$conscientiousness, na.rm=TRUE),
           mean(mydata2[D111,]$conscientiousness, na.rm=TRUE),mean(mydata2[D222,]$conscientiousness, na.rm=TRUE),mean(mydata2[D333,]$conscientiousness, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df3


limits3 <- aes(ymax = Means + se, ymin=Means - se)


c <- ggplot(df3, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits3)+ggtitle("Conscientiousness")+
  ylim(1,7)  +xlab("Partisanship")+ylab("Level of Conscientiousness (1 = Low - 7 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .05   Web: r = .07***   MTurk: r = .11***", x = 4, y = 1.5, size = 4, colour = "black")

c 

##making graph for ##emotional stability mydata$emostab
##plotting critical values and se for PID
##for FTF
##for internet

df4 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$emostab)*1.96,std.error(mydata[R2,]$emostab)*1.96,std.error(mydata[R1,]$emostab)*1.96,
               std.error(mydata[I1,]$emostab)*1.96,std.error(mydata[D1,]$emostab)*1.96,std.error(mydata[D2,]$emostab)*1.96,
               std.error(mydata[D3,]$emostab)*1.96,std.error(mydata[R33,]$emostab)*1.96,std.error(mydata[R22,]$emostab)*1.96,
               std.error(mydata[R11,]$emostab)*1.96,std.error(mydata[I11,]$emostab)*1.96,std.error(mydata[D11,]$emostab)*1.96,
               std.error(mydata[D22,]$emostab)*1.96,std.error(mydata[D33,]$emostab)*1.96,std.error(mydata2[R333,]$emostab)*1.96,std.error(mydata2[R222,]$emostab)*1.96,
               std.error(mydata2[R111,]$emostab)*1.96,std.error(mydata2[I111,]$emostab)*1.96,std.error(mydata2[D111,]$emostab)*1.96,
               std.error(mydata2[D222,]$emostab)*1.96,std.error(mydata2[D333,]$emostab)*1.96)),
  Means =c(mean(mydata[R3,]$emostab, na.rm=TRUE),mean(mydata[R2,]$emostab, na.rm=TRUE),mean(mydata[R1,]$emostab, na.rm=TRUE),
           mean(mydata[I1,]$emostab, na.rm=TRUE),mean(mydata[D1,]$emostab, na.rm=TRUE),mean(mydata[D2,]$emostab, na.rm=TRUE),
           mean(mydata[D3,]$emostab, na.rm=TRUE),mean(mydata[R33,]$emostab, na.rm=TRUE),mean(mydata[R22,]$emostab, na.rm=TRUE),
           mean(mydata[R11,]$emostab, na.rm=TRUE),mean(mydata[I11,]$emostab, na.rm=TRUE),mean(mydata[D11,]$emostab, na.rm=TRUE),
           mean(mydata[D22,]$emostab, na.rm=TRUE),mean(mydata[D33,]$emostab, na.rm=TRUE),mean(mydata2[R333,]$emostab, na.rm=TRUE),
           mean(mydata2[R222,]$emostab, na.rm=TRUE),mean(mydata2[R111,]$emostab, na.rm=TRUE),mean(mydata2[I111,]$emostab, na.rm=TRUE),
           mean(mydata2[D111,]$emostab, na.rm=TRUE),mean(mydata2[D222,]$emostab, na.rm=TRUE),mean(mydata2[D333,]$emostab, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df4


limits4 <- aes(ymax = Means + se, ymin=Means - se)


d <- ggplot(df4, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits4)+ggtitle("Emotional Stability")+
  ylim(1,7)  +
  xlab("Partisanship")+ylab("Level of Emotional Stability (1 = Low - 7 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .04   Web: r = .03   MTurk: r = .07***", x = 4, y = 1.5, size = 4, colour = "black")

d 


##making graph for openness mydata$openness
##plotting critical values and se for PID
##for FTF
##for internet


df5 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$openness)*1.96,std.error(mydata[R2,]$openness)*1.96,std.error(mydata[R1,]$openness)*1.96,
               std.error(mydata[I1,]$openness)*1.96,std.error(mydata[D1,]$openness)*1.96,std.error(mydata[D2,]$openness)*1.96,
               std.error(mydata[D3,]$openness)*1.96,std.error(mydata[R33,]$openness)*1.96,std.error(mydata[R22,]$openness)*1.96,
               std.error(mydata[R11,]$openness)*1.96,std.error(mydata[I11,]$openness)*1.96,std.error(mydata[D11,]$openness)*1.96,
               std.error(mydata[D22,]$openness)*1.96,std.error(mydata[D33,]$openness)*1.96,std.error(mydata2[R333,]$openness)*1.96,std.error(mydata2[R222,]$openness)*1.96,
               std.error(mydata2[R111,]$openness)*1.96,std.error(mydata2[I111,]$openness)*1.96,std.error(mydata2[D111,]$openness)*1.96,
               std.error(mydata2[D222,]$openness)*1.96,std.error(mydata2[D333,]$openness)*1.96)),
  Means =c(mean(mydata[R3,]$openness, na.rm=TRUE),mean(mydata[R2,]$openness, na.rm=TRUE),mean(mydata[R1,]$openness, na.rm=TRUE),
           mean(mydata[I1,]$openness, na.rm=TRUE),mean(mydata[D1,]$openness, na.rm=TRUE),mean(mydata[D2,]$openness, na.rm=TRUE),
           mean(mydata[D3,]$openness, na.rm=TRUE),mean(mydata[R33,]$openness, na.rm=TRUE),mean(mydata[R22,]$openness, na.rm=TRUE),
           mean(mydata[R11,]$openness, na.rm=TRUE),mean(mydata[I11,]$openness, na.rm=TRUE),mean(mydata[D11,]$openness, na.rm=TRUE),
           mean(mydata[D22,]$openness, na.rm=TRUE),mean(mydata[D33,]$openness, na.rm=TRUE),mean(mydata2[R333,]$openness, na.rm=TRUE),
           mean(mydata2[R222,]$openness, na.rm=TRUE),mean(mydata2[R111,]$openness, na.rm=TRUE),mean(mydata2[I111,]$openness, na.rm=TRUE),
           mean(mydata2[D111,]$openness, na.rm=TRUE),mean(mydata2[D222,]$openness, na.rm=TRUE),mean(mydata2[D333,]$openness, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df5


limits5 <- aes(ymax = Means + se, ymin=Means - se)


e <- ggplot(df5, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits5)+ggtitle("Openness")+
  ylim(1,7)  +
  xlab("Partisanship")+ylab("Level of Openness (1 = Low - 7 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = -.08***   Web: r = -.12***   MTurk: r = -.14***", x = 4, y = 1.5, size = 4, colour = "black")

e 


grid_arrange_shared_legend <- function(a,b,c,d, e) {
  plots <- list(a,b,c,d, e)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  main = "TIPI"
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

#grid.arrange(a,b,c,d, e, ncol = 2, main = "TIPI")
setwd("C:/Users/Ryan/Documents")

pdf(file="PIDbig5.pdf",height=25, width=20)
grid_arrange_shared_legend(a,b,c,d, e)
dev.off()

##making graph for extraversion
##plotting critical values and se for ideology
##for FTF
##and for internet
table(mydata$ideology)

df1 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$extraversion)*1.96,std.error(mydata[C2,]$extraversion)*1.96,std.error(mydata[C1,]$extraversion)*1.96,
               std.error(mydata[M1,]$extraversion)*1.96,std.error(mydata[L1,]$extraversion)*1.96,std.error(mydata[L2,]$extraversion)*1.96,
               std.error(mydata[L3,]$extraversion)*1.96,std.error(mydata[C33,]$extraversion)*1.96,std.error(mydata[C22,]$extraversion)*1.96,
               std.error(mydata[C11,]$extraversion)*1.96,std.error(mydata[M11,]$extraversion)*1.96,std.error(mydata[L11,]$extraversion)*1.96,
               std.error(mydata[L22,]$extraversion)*1.96,std.error(mydata[L33,]$extraversion)*1.96,std.error(mydata2[C333,]$extraversion)*1.96,std.error(mydata2[C222,]$extraversion)*1.96,
               std.error(mydata2[C111,]$extraversion)*1.96,std.error(mydata2[M111,]$extraversion)*1.96,std.error(mydata2[L111,]$extraversion)*1.96,
               std.error(mydata2[L222,]$extraversion)*1.96,std.error(mydata2[L333,]$extraversion)*1.96)),
  Means =c(mean(mydata[C3,]$extraversion, na.rm=TRUE),mean(mydata[C2,]$extraversion, na.rm=TRUE),mean(mydata[C1,]$extraversion, na.rm=TRUE),
           mean(mydata[M1,]$extraversion, na.rm=TRUE),mean(mydata[L1,]$extraversion, na.rm=TRUE),mean(mydata[L2,]$extraversion, na.rm=TRUE),
           mean(mydata[L3,]$extraversion, na.rm=TRUE),mean(mydata[C33,]$extraversion, na.rm=TRUE),mean(mydata[C22,]$extraversion, na.rm=TRUE),
           mean(mydata[C11,]$extraversion, na.rm=TRUE),mean(mydata[M11,]$extraversion, na.rm=TRUE),mean(mydata[L11,]$extraversion, na.rm=TRUE),
           mean(mydata[L22,]$extraversion, na.rm=TRUE),mean(mydata[L33,]$extraversion, na.rm=TRUE),mean(mydata2[C333,]$extraversion, na.rm=TRUE),
           mean(mydata2[C222,]$extraversion, na.rm=TRUE),mean(mydata2[C111,]$extraversion, na.rm=TRUE),mean(mydata2[M111,]$extraversion, na.rm=TRUE),
           mean(mydata2[L111,]$extraversion, na.rm=TRUE),mean(mydata2[L222,]$extraversion, na.rm=TRUE),mean(mydata2[L333,]$extraversion, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df1

limits <- aes(ymax = Means + se, ymin=Means - se)

##+ theme(legend.position="none") save for later

a <- ggplot(df1, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset)) + theme(legend.position="none") + geom_pointrange(limits)+ggtitle("Extraversion")+
  ylim(1,7)  +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Ideology")+ylab("Level of Extraversion (1 = Low - 7 = High)")+
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = -.00   Web: r = -.02   MTurk: r = .02", x = 4, y = 1.5, size = 4, colour = "black")
a 


##making graph for agreeableness mydata$agreeableness
##plotting critical values and se for ideology
##for FTF and internet



df2 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$agreeableness)*1.96,std.error(mydata[C2,]$agreeableness)*1.96,std.error(mydata[C1,]$agreeableness)*1.96,
               std.error(mydata[M1,]$agreeableness)*1.96,std.error(mydata[L1,]$agreeableness)*1.96,std.error(mydata[L2,]$agreeableness)*1.96,
               std.error(mydata[L3,]$agreeableness)*1.96,std.error(mydata[C33,]$agreeableness)*1.96,std.error(mydata[C22,]$agreeableness)*1.96,
               std.error(mydata[C11,]$agreeableness)*1.96,std.error(mydata[M11,]$agreeableness)*1.96,std.error(mydata[L11,]$agreeableness)*1.96,
               std.error(mydata[L22,]$agreeableness)*1.96,std.error(mydata[L33,]$agreeableness)*1.96,std.error(mydata2[C333,]$agreeableness)*1.96,std.error(mydata2[C222,]$agreeableness)*1.96,
               std.error(mydata2[C111,]$agreeableness)*1.96,std.error(mydata2[M111,]$agreeableness)*1.96,std.error(mydata2[L111,]$agreeableness)*1.96,
               std.error(mydata2[L222,]$agreeableness)*1.96,std.error(mydata2[L333,]$agreeableness)*1.96)),
  Means =c(mean(mydata[C3,]$agreeableness, na.rm=TRUE),mean(mydata[C2,]$agreeableness, na.rm=TRUE),mean(mydata[C1,]$agreeableness, na.rm=TRUE),
           mean(mydata[M1,]$agreeableness, na.rm=TRUE),mean(mydata[L1,]$agreeableness, na.rm=TRUE),mean(mydata[L2,]$agreeableness, na.rm=TRUE),
           mean(mydata[L3,]$agreeableness, na.rm=TRUE),mean(mydata[C33,]$agreeableness, na.rm=TRUE),mean(mydata[C22,]$agreeableness, na.rm=TRUE),
           mean(mydata[C11,]$agreeableness, na.rm=TRUE),mean(mydata[M11,]$agreeableness, na.rm=TRUE),mean(mydata[L11,]$agreeableness, na.rm=TRUE),
           mean(mydata[L22,]$agreeableness, na.rm=TRUE),mean(mydata[L33,]$agreeableness, na.rm=TRUE),mean(mydata2[C333,]$agreeableness, na.rm=TRUE),
           mean(mydata2[C222,]$agreeableness, na.rm=TRUE),mean(mydata2[C111,]$agreeableness, na.rm=TRUE),mean(mydata2[M111,]$agreeableness, na.rm=TRUE),
           mean(mydata2[L111,]$agreeableness, na.rm=TRUE),mean(mydata2[L222,]$agreeableness, na.rm=TRUE),mean(mydata2[L333,]$agreeableness, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df2


limits2 <- aes(ymax = Means + se, ymin=Means - se)

##+ theme(legend.position="none") save for later

b <- ggplot(df2, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits2)+ggtitle("Agreeableness")+
  ylim(1,7)  + xlab("Ideology")+ylab("Level of Agreeableness (1 = Low - 7 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = -.02   Web: r = -.02   MTurk: r = -.03", x = 4, y = 1.5, size = 4, colour = "black")
b 


##making graph for conscientiousness mydata$conscientiousness
##plotting critical values and se for ideology
##for FTF
##for internet

df3 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$conscientiousness)*1.96,std.error(mydata[C2,]$conscientiousness)*1.96,std.error(mydata[C1,]$conscientiousness)*1.96,
               std.error(mydata[M1,]$conscientiousness)*1.96,std.error(mydata[L1,]$conscientiousness)*1.96,std.error(mydata[L2,]$conscientiousness)*1.96,
               std.error(mydata[L3,]$conscientiousness)*1.96,std.error(mydata[C33,]$conscientiousness)*1.96,std.error(mydata[C22,]$conscientiousness)*1.96,
               std.error(mydata[C11,]$conscientiousness)*1.96,std.error(mydata[M11,]$conscientiousness)*1.96,std.error(mydata[L11,]$conscientiousness)*1.96,
               std.error(mydata[L22,]$conscientiousness)*1.96,std.error(mydata[L33,]$conscientiousness)*1.96,std.error(mydata2[C333,]$conscientiousness)*1.96,std.error(mydata2[C222,]$conscientiousness)*1.96,
               std.error(mydata2[C111,]$conscientiousness)*1.96,std.error(mydata2[M111,]$conscientiousness)*1.96,std.error(mydata2[L111,]$conscientiousness)*1.96,
               std.error(mydata2[L222,]$conscientiousness)*1.96,std.error(mydata2[L333,]$conscientiousness)*1.96)),
  Means =c(mean(mydata[C3,]$conscientiousness, na.rm=TRUE),mean(mydata[C2,]$conscientiousness, na.rm=TRUE),mean(mydata[C1,]$conscientiousness, na.rm=TRUE),
           mean(mydata[M1,]$conscientiousness, na.rm=TRUE),mean(mydata[L1,]$conscientiousness, na.rm=TRUE),mean(mydata[L2,]$conscientiousness, na.rm=TRUE),
           mean(mydata[L3,]$conscientiousness, na.rm=TRUE),mean(mydata[C33,]$conscientiousness, na.rm=TRUE),mean(mydata[C22,]$conscientiousness, na.rm=TRUE),
           mean(mydata[C11,]$conscientiousness, na.rm=TRUE),mean(mydata[M11,]$conscientiousness, na.rm=TRUE),mean(mydata[L11,]$conscientiousness, na.rm=TRUE),
           mean(mydata[L22,]$conscientiousness, na.rm=TRUE),mean(mydata[L33,]$conscientiousness, na.rm=TRUE),mean(mydata2[C333,]$conscientiousness, na.rm=TRUE),
           mean(mydata2[C222,]$conscientiousness, na.rm=TRUE),mean(mydata2[C111,]$conscientiousness, na.rm=TRUE),mean(mydata2[M111,]$conscientiousness, na.rm=TRUE),
           mean(mydata2[L111,]$conscientiousness, na.rm=TRUE),mean(mydata2[L222,]$conscientiousness, na.rm=TRUE),mean(mydata2[L333,]$conscientiousness, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df3


limits3 <- aes(ymax = Means + se, ymin=Means - se)


c <- ggplot(df3, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits3)+ggtitle("Conscientiousness")+
  ylim(1,7)  +xlab("Ideology")+ylab("Level of Conscientiousness (1 = Low - 7 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = .11***   Web: r = .08***   MTurk: r = .17***", x = 4, y = 1.5, size = 4, colour = "black")
c 


##making graph for ##emotional stability mydata$emostab
##plotting critical values and se for ideology
##for FTF
##for internet

df4 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$emostab)*1.96,std.error(mydata[C2,]$emostab)*1.96,std.error(mydata[C1,]$emostab)*1.96,
               std.error(mydata[M1,]$emostab)*1.96,std.error(mydata[L1,]$emostab)*1.96,std.error(mydata[L2,]$emostab)*1.96,
               std.error(mydata[L3,]$emostab)*1.96,std.error(mydata[C33,]$emostab)*1.96,std.error(mydata[C22,]$emostab)*1.96,
               std.error(mydata[C11,]$emostab)*1.96,std.error(mydata[M11,]$emostab)*1.96,std.error(mydata[L11,]$emostab)*1.96,
               std.error(mydata[L22,]$emostab)*1.96,std.error(mydata[L33,]$emostab)*1.96,std.error(mydata2[C333,]$emostab)*1.96,std.error(mydata2[C222,]$emostab)*1.96,
               std.error(mydata2[C111,]$emostab)*1.96,std.error(mydata2[M111,]$emostab)*1.96,std.error(mydata2[L111,]$emostab)*1.96,
               std.error(mydata2[L222,]$emostab)*1.96,std.error(mydata2[L333,]$emostab)*1.96)),
  Means =c(mean(mydata[C3,]$emostab, na.rm=TRUE),mean(mydata[C2,]$emostab, na.rm=TRUE),mean(mydata[C1,]$emostab, na.rm=TRUE),
           mean(mydata[M1,]$emostab, na.rm=TRUE),mean(mydata[L1,]$emostab, na.rm=TRUE),mean(mydata[L2,]$emostab, na.rm=TRUE),
           mean(mydata[L3,]$emostab, na.rm=TRUE),mean(mydata[C33,]$emostab, na.rm=TRUE),mean(mydata[C22,]$emostab, na.rm=TRUE),
           mean(mydata[C11,]$emostab, na.rm=TRUE),mean(mydata[M11,]$emostab, na.rm=TRUE),mean(mydata[L11,]$emostab, na.rm=TRUE),
           mean(mydata[L22,]$emostab, na.rm=TRUE),mean(mydata[L33,]$emostab, na.rm=TRUE),mean(mydata2[C333,]$emostab, na.rm=TRUE),
           mean(mydata2[C222,]$emostab, na.rm=TRUE),mean(mydata2[C111,]$emostab, na.rm=TRUE),mean(mydata2[M111,]$emostab, na.rm=TRUE),
           mean(mydata2[L111,]$emostab, na.rm=TRUE),mean(mydata2[L222,]$emostab, na.rm=TRUE),mean(mydata2[L333,]$emostab, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df4


limits4 <- aes(ymax = Means + se, ymin=Means - se)


d <- ggplot(df4, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits4)+ggtitle("Emotional Stability")+
  ylim(1,7)  +
  xlab("Ideology")+ylab("Level of Emotional Stability (1 = Low - 7 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = .05   Web: r = .03   MTurk: r = .12***", x = 4, y = 1.5, size = 4, colour = "black")

d 



##making graph for openness mydata$openness
##plotting critical values and se for ideology
##for FTF
##for internet


df5 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$openness)*1.96,std.error(mydata[C2,]$openness)*1.96,std.error(mydata[C1,]$openness)*1.96,
               std.error(mydata[M1,]$openness)*1.96,std.error(mydata[L1,]$openness)*1.96,std.error(mydata[L2,]$openness)*1.96,
               std.error(mydata[L3,]$openness)*1.96,std.error(mydata[C33,]$openness)*1.96,std.error(mydata[C22,]$openness)*1.96,
               std.error(mydata[C11,]$openness)*1.96,std.error(mydata[M11,]$openness)*1.96,std.error(mydata[L11,]$openness)*1.96,
               std.error(mydata[L22,]$openness)*1.96,std.error(mydata[L33,]$openness)*1.96,std.error(mydata2[C333,]$openness)*1.96,std.error(mydata2[C222,]$openness)*1.96,
               std.error(mydata2[C111,]$openness)*1.96,std.error(mydata2[M111,]$openness)*1.96,std.error(mydata2[L111,]$openness)*1.96,
               std.error(mydata2[L222,]$openness)*1.96,std.error(mydata2[L333,]$openness)*1.96)),
  Means =c(mean(mydata[C3,]$openness, na.rm=TRUE),mean(mydata[C2,]$openness, na.rm=TRUE),mean(mydata[C1,]$openness, na.rm=TRUE),
           mean(mydata[M1,]$openness, na.rm=TRUE),mean(mydata[L1,]$openness, na.rm=TRUE),mean(mydata[L2,]$openness, na.rm=TRUE),
           mean(mydata[L3,]$openness, na.rm=TRUE),mean(mydata[C33,]$openness, na.rm=TRUE),mean(mydata[C22,]$openness, na.rm=TRUE),
           mean(mydata[C11,]$openness, na.rm=TRUE),mean(mydata[M11,]$openness, na.rm=TRUE),mean(mydata[L11,]$openness, na.rm=TRUE),
           mean(mydata[L22,]$openness, na.rm=TRUE),mean(mydata[L33,]$openness, na.rm=TRUE),mean(mydata2[C333,]$openness, na.rm=TRUE),
           mean(mydata2[C222,]$openness, na.rm=TRUE),mean(mydata2[C111,]$openness, na.rm=TRUE),mean(mydata2[M111,]$openness, na.rm=TRUE),
           mean(mydata2[L111,]$openness, na.rm=TRUE),mean(mydata2[L222,]$openness, na.rm=TRUE),mean(mydata2[L333,]$openness, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df5


limits5 <- aes(ymax = Means + se, ymin=Means - se)


e <- ggplot(df5, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits5)+ggtitle("Openness")+
  ylim(1,7)  +
  xlab("Ideology")+ylab("Level of Openness (1 = Low - 7 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = -.12***   Web: r = -.16***   MTurk: r = -.19***", x = 4, y = 1.5, size = 4, colour = "black")

e 



grid_arrange_shared_legend <- function(a,b,c,d, e) {
  plots <- list(a,b,c,d, e)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  main = "TIPI"
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend, 
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

#grid.arrange(a,b,c,d, e, ncol = 2, main = "TIPI")
pdf(file="figure1.pdf",height=25, width=20)
grid_arrange_shared_legend(a,b,c,d, e)
dev.off()


## authoritarianism scale
##plotting critical values and se for PID


df6 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$ascale)*1.96,std.error(mydata[R2,]$ascale)*1.96,std.error(mydata[R1,]$ascale)*1.96,
               std.error(mydata[I1,]$ascale)*1.96,std.error(mydata[D1,]$ascale)*1.96,std.error(mydata[D2,]$ascale)*1.96,
               std.error(mydata[D3,]$ascale)*1.96,std.error(mydata[R33,]$ascale)*1.96,std.error(mydata[R22,]$ascale)*1.96,
               std.error(mydata[R11,]$ascale)*1.96,std.error(mydata[I11,]$ascale)*1.96,std.error(mydata[D11,]$ascale)*1.96,
               std.error(mydata[D22,]$ascale)*1.96,std.error(mydata[D33,]$ascale)*1.96,std.error(mydata2[R333,]$ascale)*1.96,std.error(mydata2[R222,]$ascale)*1.96,
               std.error(mydata2[R111,]$ascale)*1.96,std.error(mydata2[I111,]$ascale)*1.96,std.error(mydata2[D111,]$ascale)*1.96,
               std.error(mydata2[D222,]$ascale)*1.96,std.error(mydata2[D333,]$ascale)*1.96)),
  Means =c(mean(mydata[R3,]$ascale, na.rm=TRUE),mean(mydata[R2,]$ascale, na.rm=TRUE),mean(mydata[R1,]$ascale, na.rm=TRUE),
           mean(mydata[I1,]$ascale, na.rm=TRUE),mean(mydata[D1,]$ascale, na.rm=TRUE),mean(mydata[D2,]$ascale, na.rm=TRUE),
           mean(mydata[D3,]$ascale, na.rm=TRUE),mean(mydata[R33,]$ascale, na.rm=TRUE),mean(mydata[R22,]$ascale, na.rm=TRUE),
           mean(mydata[R11,]$ascale, na.rm=TRUE),mean(mydata[I11,]$ascale, na.rm=TRUE),mean(mydata[D11,]$ascale, na.rm=TRUE),
           mean(mydata[D22,]$ascale, na.rm=TRUE),mean(mydata[D33,]$ascale, na.rm=TRUE),mean(mydata2[R333,]$ascale, na.rm=TRUE),
           mean(mydata2[R222,]$ascale, na.rm=TRUE),mean(mydata2[R111,]$ascale, na.rm=TRUE),mean(mydata2[I111,]$ascale, na.rm=TRUE),
           mean(mydata2[D111,]$ascale, na.rm=TRUE),mean(mydata2[D222,]$ascale, na.rm=TRUE),mean(mydata2[D333,]$ascale, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df6


limits6 <- aes(ymax = Means + se, ymin=Means - se)


h <- ggplot(df6, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset)) + geom_pointrange(limits6)+ggtitle("Authoritarianism Scale")+
  ylim(0,1) +
  xlab("Partisanship")+ylab("Level of Authoritarianism (0 = Low - 1 = High)")+
  theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .03   Web: r = .09***   MTurk: r = .24***", x = 4, y = 0, size = 4, colour = "black")

h 

## authoritarianism scale
##plotting critical values and se for Ideo
df7 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$ascale)*1.96,std.error(mydata[C2,]$ascale)*1.96,std.error(mydata[C1,]$ascale)*1.96,
               std.error(mydata[M1,]$ascale)*1.96,std.error(mydata[L1,]$ascale)*1.96,std.error(mydata[L2,]$ascale)*1.96,
               std.error(mydata[L3,]$ascale)*1.96,std.error(mydata[C33,]$ascale)*1.96,std.error(mydata[C22,]$ascale)*1.96,
               std.error(mydata[C11,]$ascale)*1.96,std.error(mydata[M11,]$ascale)*1.96,std.error(mydata[L11,]$ascale)*1.96,
               std.error(mydata[L22,]$ascale)*1.96,std.error(mydata[L33,]$ascale)*1.96,std.error(mydata2[C333,]$ascale)*1.96,std.error(mydata2[C222,]$ascale)*1.96,
               std.error(mydata2[C111,]$ascale)*1.96,std.error(mydata2[M111,]$ascale)*1.96,std.error(mydata2[L111,]$ascale)*1.96,
               std.error(mydata2[L222,]$ascale)*1.96,std.error(mydata2[L333,]$ascale)*1.96)),
  Means =c(mean(mydata[C3,]$ascale, na.rm=TRUE),mean(mydata[C2,]$ascale, na.rm=TRUE),mean(mydata[C1,]$ascale, na.rm=TRUE),
           mean(mydata[M1,]$ascale, na.rm=TRUE),mean(mydata[L1,]$ascale, na.rm=TRUE),mean(mydata[L2,]$ascale, na.rm=TRUE),
           mean(mydata[L3,]$ascale, na.rm=TRUE),mean(mydata[C33,]$ascale, na.rm=TRUE),mean(mydata[C22,]$ascale, na.rm=TRUE),
           mean(mydata[C11,]$ascale, na.rm=TRUE),mean(mydata[M11,]$ascale, na.rm=TRUE),mean(mydata[L11,]$ascale, na.rm=TRUE),
           mean(mydata[L22,]$ascale, na.rm=TRUE),mean(mydata[L33,]$ascale, na.rm=TRUE),mean(mydata2[C333,]$ascale, na.rm=TRUE),
           mean(mydata2[C222,]$ascale, na.rm=TRUE),mean(mydata2[C111,]$ascale, na.rm=TRUE),mean(mydata2[M111,]$ascale, na.rm=TRUE),
           mean(mydata2[L111,]$ascale, na.rm=TRUE),mean(mydata2[L222,]$ascale, na.rm=TRUE),mean(mydata2[L333,]$ascale, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df7


limits7 <- aes(ymax = Means + se, ymin=Means - se)


i <- ggplot(df7, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset)) + geom_pointrange(limits7)+ggtitle("Authoritarianism Scale")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Ideology")+ylab("Level of Authoritarianism (0 = Low - 1 = High)")+ 
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = .18***   Web: r = .23***   MTurk: r = .36***", x = 4, y = 0, size = 4, colour = "black")

i 

##Now lets look at Racial Resentment
##plotting critical values and se for PID


df8 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$rrscale)*1.96,std.error(mydata[R2,]$rrscale)*1.96,std.error(mydata[R1,]$rrscale)*1.96,
               std.error(mydata[I1,]$rrscale)*1.96,std.error(mydata[D1,]$rrscale)*1.96,std.error(mydata[D2,]$rrscale)*1.96,
               std.error(mydata[D3,]$rrscale)*1.96,std.error(mydata[R33,]$rrscale)*1.96,std.error(mydata[R22,]$rrscale)*1.96,
               std.error(mydata[R11,]$rrscale)*1.96,std.error(mydata[I11,]$rrscale)*1.96,std.error(mydata[D11,]$rrscale)*1.96,
               std.error(mydata[D22,]$rrscale)*1.96,std.error(mydata[D33,]$rrscale)*1.96,std.error(mydata2[R333,]$rrscale)*1.96,std.error(mydata2[R222,]$rrscale)*1.96,
               std.error(mydata2[R111,]$rrscale)*1.96,std.error(mydata2[I111,]$rrscale)*1.96,std.error(mydata2[D111,]$rrscale)*1.96,
               std.error(mydata2[D222,]$rrscale)*1.96,std.error(mydata2[D333,]$rrscale)*1.96)),
  Means =c(mean(mydata[R3,]$rrscale, na.rm=TRUE),mean(mydata[R2,]$rrscale, na.rm=TRUE),mean(mydata[R1,]$rrscale, na.rm=TRUE),
           mean(mydata[I1,]$rrscale, na.rm=TRUE),mean(mydata[D1,]$rrscale, na.rm=TRUE),mean(mydata[D2,]$rrscale, na.rm=TRUE),
           mean(mydata[D3,]$rrscale, na.rm=TRUE),mean(mydata[R33,]$rrscale, na.rm=TRUE),mean(mydata[R22,]$rrscale, na.rm=TRUE),
           mean(mydata[R11,]$rrscale, na.rm=TRUE),mean(mydata[I11,]$rrscale, na.rm=TRUE),mean(mydata[D11,]$rrscale, na.rm=TRUE),
           mean(mydata[D22,]$rrscale, na.rm=TRUE),mean(mydata[D33,]$rrscale, na.rm=TRUE),mean(mydata2[R333,]$rrscale, na.rm=TRUE),
           mean(mydata2[R222,]$rrscale, na.rm=TRUE),mean(mydata2[R111,]$rrscale, na.rm=TRUE),mean(mydata2[I111,]$rrscale, na.rm=TRUE),
           mean(mydata2[D111,]$rrscale, na.rm=TRUE),mean(mydata2[D222,]$rrscale, na.rm=TRUE),mean(mydata2[D333,]$rrscale, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df8

limits8 <- aes(ymax = Means + se, ymin=Means - se)

j <- ggplot(df8, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits8) +ggtitle("Racial Resentment Scale")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Partisanship")+ylab("Level of Racial Resentment (0 = Low - 1 = High)")+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .35***   Web: r = .47***   MTurk: r = .48***", x = 4, y = 0, size = 4, colour = "black")

j    

#ido
df9 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$rrscale)*1.96,std.error(mydata[C2,]$rrscale)*1.96,std.error(mydata[C1,]$rrscale)*1.96,
               std.error(mydata[M1,]$rrscale)*1.96,std.error(mydata[L1,]$rrscale)*1.96,std.error(mydata[L2,]$rrscale)*1.96,
               std.error(mydata[L3,]$rrscale)*1.96,std.error(mydata[C33,]$rrscale)*1.96,std.error(mydata[C22,]$rrscale)*1.96,
               std.error(mydata[C11,]$rrscale)*1.96,std.error(mydata[M11,]$rrscale)*1.96,std.error(mydata[L11,]$rrscale)*1.96,
               std.error(mydata[L22,]$rrscale)*1.96,std.error(mydata[L33,]$rrscale)*1.96,std.error(mydata2[C333,]$rrscale)*1.96,std.error(mydata2[C222,]$rrscale)*1.96,
               std.error(mydata2[C111,]$rrscale)*1.96,std.error(mydata2[M111,]$rrscale)*1.96,std.error(mydata2[L111,]$rrscale)*1.96,
               std.error(mydata2[L222,]$rrscale)*1.96,std.error(mydata2[L333,]$rrscale)*1.96)),
  Means =c(mean(mydata[C3,]$rrscale, na.rm=TRUE),mean(mydata[C2,]$rrscale, na.rm=TRUE),mean(mydata[C1,]$rrscale, na.rm=TRUE),
           mean(mydata[M1,]$rrscale, na.rm=TRUE),mean(mydata[L1,]$rrscale, na.rm=TRUE),mean(mydata[L2,]$rrscale, na.rm=TRUE),
           mean(mydata[L3,]$rrscale, na.rm=TRUE),mean(mydata[C33,]$rrscale, na.rm=TRUE),mean(mydata[C22,]$rrscale, na.rm=TRUE),
           mean(mydata[C11,]$rrscale, na.rm=TRUE),mean(mydata[M11,]$rrscale, na.rm=TRUE),mean(mydata[L11,]$rrscale, na.rm=TRUE),
           mean(mydata[L22,]$rrscale, na.rm=TRUE),mean(mydata[L33,]$rrscale, na.rm=TRUE),mean(mydata2[C333,]$rrscale, na.rm=TRUE),
           mean(mydata2[C222,]$rrscale, na.rm=TRUE),mean(mydata2[C111,]$rrscale, na.rm=TRUE),mean(mydata2[M111,]$rrscale, na.rm=TRUE),
           mean(mydata2[L111,]$rrscale, na.rm=TRUE),mean(mydata2[L222,]$rrscale, na.rm=TRUE),mean(mydata2[L333,]$rrscale, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df9



limits9 <- aes(ymax = Means + se, ymin=Means - se)

k <- ggplot(df9, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits9) +ggtitle("Racial Resentment Scale")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Ideology")+ylab("Level of Racial Resentment (0 = Low - 1 = High)")+
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = .34***   Web: r = .44***   MTurk: r = .57***", x = 4, y = 0, size = 4, colour = "black")

k    

#Now to look at Moral Traditionalism :D Isn't this fun!
##plotting critical values and se for PID

df10 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$mtscale)*1.96,std.error(mydata[R2,]$mtscale)*1.96,std.error(mydata[R1,]$mtscale)*1.96,
               std.error(mydata[I1,]$mtscale)*1.96,std.error(mydata[D1,]$mtscale)*1.96,std.error(mydata[D2,]$mtscale)*1.96,
               std.error(mydata[D3,]$mtscale)*1.96,std.error(mydata[R33,]$mtscale)*1.96,std.error(mydata[R22,]$mtscale)*1.96,
               std.error(mydata[R11,]$mtscale)*1.96,std.error(mydata[I11,]$mtscale)*1.96,std.error(mydata[D11,]$mtscale)*1.96,
               std.error(mydata[D22,]$mtscale)*1.96,std.error(mydata[D33,]$mtscale)*1.96,std.error(mydata2[R333,]$mtscale)*1.96,std.error(mydata2[R222,]$mtscale)*1.96,
               std.error(mydata2[R111,]$mtscale)*1.96,std.error(mydata2[I111,]$mtscale)*1.96,std.error(mydata2[D111,]$mtscale)*1.96,
               std.error(mydata2[D222,]$mtscale)*1.96,std.error(mydata2[D333,]$mtscale)*1.96)),
  Means =c(mean(mydata[R3,]$mtscale, na.rm=TRUE),mean(mydata[R2,]$mtscale, na.rm=TRUE),mean(mydata[R1,]$mtscale, na.rm=TRUE),
           mean(mydata[I1,]$mtscale, na.rm=TRUE),mean(mydata[D1,]$mtscale, na.rm=TRUE),mean(mydata[D2,]$mtscale, na.rm=TRUE),
           mean(mydata[D3,]$mtscale, na.rm=TRUE),mean(mydata[R33,]$mtscale, na.rm=TRUE),mean(mydata[R22,]$mtscale, na.rm=TRUE),
           mean(mydata[R11,]$mtscale, na.rm=TRUE),mean(mydata[I11,]$mtscale, na.rm=TRUE),mean(mydata[D11,]$mtscale, na.rm=TRUE),
           mean(mydata[D22,]$mtscale, na.rm=TRUE),mean(mydata[D33,]$mtscale, na.rm=TRUE),mean(mydata2[R333,]$mtscale, na.rm=TRUE),
           mean(mydata2[R222,]$mtscale, na.rm=TRUE),mean(mydata2[R111,]$mtscale, na.rm=TRUE),mean(mydata2[I111,]$mtscale, na.rm=TRUE),
           mean(mydata2[D111,]$mtscale, na.rm=TRUE),mean(mydata2[D222,]$mtscale, na.rm=TRUE),mean(mydata2[D333,]$mtscale, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df10

limits10 <- aes(ymax = Means + se, ymin=Means - se)

l <- ggplot(df10, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits10) +ggtitle("Moral Traditionalism Scale")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Partisanship")+ylab("Level of Moral Traditionalism (0 = Low - 1 = High)")+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .36***   Web: r = .46***   MTurk: r = .51***", x = 4, y = 0, size = 4, colour = "black")
l    

##plotting critical values and se for Ideology

df11 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$mtscale)*1.96,std.error(mydata[C2,]$mtscale)*1.96,std.error(mydata[C1,]$mtscale)*1.96,
               std.error(mydata[M1,]$mtscale)*1.96,std.error(mydata[L1,]$mtscale)*1.96,std.error(mydata[L2,]$mtscale)*1.96,
               std.error(mydata[L3,]$mtscale)*1.96,std.error(mydata[C33,]$mtscale)*1.96,std.error(mydata[C22,]$mtscale)*1.96,
               std.error(mydata[C11,]$mtscale)*1.96,std.error(mydata[M11,]$mtscale)*1.96,std.error(mydata[L11,]$mtscale)*1.96,
               std.error(mydata[L22,]$mtscale)*1.96,std.error(mydata[L33,]$mtscale)*1.96,std.error(mydata2[C333,]$mtscale)*1.96,std.error(mydata2[C222,]$mtscale)*1.96,
               std.error(mydata2[C111,]$mtscale)*1.96,std.error(mydata2[M111,]$mtscale)*1.96,std.error(mydata2[L111,]$mtscale)*1.96,
               std.error(mydata2[L222,]$mtscale)*1.96,std.error(mydata2[L333,]$mtscale)*1.96)),
  Means =c(mean(mydata[C3,]$mtscale, na.rm=TRUE),mean(mydata[C2,]$mtscale, na.rm=TRUE),mean(mydata[C1,]$mtscale, na.rm=TRUE),
           mean(mydata[M1,]$mtscale, na.rm=TRUE),mean(mydata[L1,]$mtscale, na.rm=TRUE),mean(mydata[L2,]$mtscale, na.rm=TRUE),
           mean(mydata[L3,]$mtscale, na.rm=TRUE),mean(mydata[C33,]$mtscale, na.rm=TRUE),mean(mydata[C22,]$mtscale, na.rm=TRUE),
           mean(mydata[C11,]$mtscale, na.rm=TRUE),mean(mydata[M11,]$mtscale, na.rm=TRUE),mean(mydata[L11,]$mtscale, na.rm=TRUE),
           mean(mydata[L22,]$mtscale, na.rm=TRUE),mean(mydata[L33,]$mtscale, na.rm=TRUE),mean(mydata2[C333,]$mtscale, na.rm=TRUE),
           mean(mydata2[C222,]$mtscale, na.rm=TRUE),mean(mydata2[C111,]$mtscale, na.rm=TRUE),mean(mydata2[M111,]$mtscale, na.rm=TRUE),
           mean(mydata2[L111,]$mtscale, na.rm=TRUE),mean(mydata2[L222,]$mtscale, na.rm=TRUE),mean(mydata2[L333,]$mtscale, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df11

limits11 <- aes(ymax = Means + se, ymin=Means - se)

m <- ggplot(df11, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits11) +ggtitle("Moral Traditionalism Scale")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Ideology")+ylab("Level of Moral Traditionalism (0 = Low - 1 = High)")+
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = .46***   Web: r = .58***   MTurk: r = .62***", x = 4, y = 0, size = 4, colour = "black")
m    

##Now for Egalitarianism
##plotting critical values and se for PID
#all

df12 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$escale)*1.96,std.error(mydata[R2,]$escale)*1.96,std.error(mydata[R1,]$escale)*1.96,
               std.error(mydata[I1,]$escale)*1.96,std.error(mydata[D1,]$escale)*1.96,std.error(mydata[D2,]$escale)*1.96,
               std.error(mydata[D3,]$escale)*1.96,std.error(mydata[R33,]$escale)*1.96,std.error(mydata[R22,]$escale)*1.96,
               std.error(mydata[R11,]$escale)*1.96,std.error(mydata[I11,]$escale)*1.96,std.error(mydata[D11,]$escale)*1.96,
               std.error(mydata[D22,]$escale)*1.96,std.error(mydata[D33,]$escale)*1.96,std.error(mydata2[R333,]$escale)*1.96,std.error(mydata2[R222,]$escale)*1.96,
               std.error(mydata2[R111,]$escale)*1.96,std.error(mydata2[I111,]$escale)*1.96,std.error(mydata2[D111,]$escale)*1.96,
               std.error(mydata2[D222,]$escale)*1.96,std.error(mydata2[D333,]$escale)*1.96)),
  Means =c(mean(mydata[R3,]$escale, na.rm=TRUE),mean(mydata[R2,]$escale, na.rm=TRUE),mean(mydata[R1,]$escale, na.rm=TRUE),
           mean(mydata[I1,]$escale, na.rm=TRUE),mean(mydata[D1,]$escale, na.rm=TRUE),mean(mydata[D2,]$escale, na.rm=TRUE),
           mean(mydata[D3,]$escale, na.rm=TRUE),mean(mydata[R33,]$escale, na.rm=TRUE),mean(mydata[R22,]$escale, na.rm=TRUE),
           mean(mydata[R11,]$escale, na.rm=TRUE),mean(mydata[I11,]$escale, na.rm=TRUE),mean(mydata[D11,]$escale, na.rm=TRUE),
           mean(mydata[D22,]$escale, na.rm=TRUE),mean(mydata[D33,]$escale, na.rm=TRUE),mean(mydata2[R333,]$escale, na.rm=TRUE),
           mean(mydata2[R222,]$escale, na.rm=TRUE),mean(mydata2[R111,]$escale, na.rm=TRUE),mean(mydata2[I111,]$escale, na.rm=TRUE),
           mean(mydata2[D111,]$escale, na.rm=TRUE),mean(mydata2[D222,]$escale, na.rm=TRUE),mean(mydata2[D333,]$escale, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df12

limits12 <- aes(ymax = Means + se, ymin=Means - se)

n <- ggplot(df12, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits12) +ggtitle("Egalitarianism Scale")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Partisanship")+ylab("Level of Egalitarianism (0 = Low - 1 = High)")+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = -.42***   Web: r = -.52***   MTurk: r = -.57***", x = 4, y = 0, size = 4, colour = "black")

n    


##plotting critical values and se for ideo
df13 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$escale)*1.96,std.error(mydata[C2,]$escale)*1.96,std.error(mydata[C1,]$escale)*1.96,
               std.error(mydata[M1,]$escale)*1.96,std.error(mydata[L1,]$escale)*1.96,std.error(mydata[L2,]$escale)*1.96,
               std.error(mydata[L3,]$escale)*1.96,std.error(mydata[C33,]$escale)*1.96,std.error(mydata[C22,]$escale)*1.96,
               std.error(mydata[C11,]$escale)*1.96,std.error(mydata[M11,]$escale)*1.96,std.error(mydata[L11,]$escale)*1.96,
               std.error(mydata[L22,]$escale)*1.96,std.error(mydata[L33,]$escale)*1.96,std.error(mydata2[C333,]$escale)*1.96,std.error(mydata2[C222,]$escale)*1.96,
               std.error(mydata2[C111,]$escale)*1.96,std.error(mydata2[M111,]$escale)*1.96,std.error(mydata2[L111,]$escale)*1.96,
               std.error(mydata2[L222,]$escale)*1.96,std.error(mydata2[L333,]$escale)*1.96)),
  Means =c(mean(mydata[C3,]$escale, na.rm=TRUE),mean(mydata[C2,]$escale, na.rm=TRUE),mean(mydata[C1,]$escale, na.rm=TRUE),
           mean(mydata[M1,]$escale, na.rm=TRUE),mean(mydata[L1,]$escale, na.rm=TRUE),mean(mydata[L2,]$escale, na.rm=TRUE),
           mean(mydata[L3,]$escale, na.rm=TRUE),mean(mydata[C33,]$escale, na.rm=TRUE),mean(mydata[C22,]$escale, na.rm=TRUE),
           mean(mydata[C11,]$escale, na.rm=TRUE),mean(mydata[M11,]$escale, na.rm=TRUE),mean(mydata[L11,]$escale, na.rm=TRUE),
           mean(mydata[L22,]$escale, na.rm=TRUE),mean(mydata[L33,]$escale, na.rm=TRUE),mean(mydata2[C333,]$escale, na.rm=TRUE),
           mean(mydata2[C222,]$escale, na.rm=TRUE),mean(mydata2[C111,]$escale, na.rm=TRUE),mean(mydata2[M111,]$escale, na.rm=TRUE),
           mean(mydata2[L111,]$escale, na.rm=TRUE),mean(mydata2[L222,]$escale, na.rm=TRUE),mean(mydata2[L333,]$escale, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df13

limits13 <- aes(ymax = Means + se, ymin=Means - se)

o <- ggplot(df13, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits13) +ggtitle("Egalitarianism Scale")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Ideology")+ylab("Level of Moral Egalitarianism (0 = Low - 1 = High)")+ 
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = -.40***   Web: r = -.50***   MTurk: r = -.62***", x = 4, y = 0, size = 4, colour = "black")
o    

#Plot table for PID

grid_arrange_shared_legend <- function(h,j,l,n) {
  plots <- list(h,j,l,n)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  main = "TIPI"
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend(h,j,l,n)
pdf(file="PIDvalues.pdf",height=15, width=20)
grid_arrange_shared_legend(h,j,l,n)
dev.off()

#Plot table for Idoe

grid_arrange_shared_legend <- function(i,k,m,o) {
  plots <- list(i,k,m,o)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  main = "TIPI"
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend(i,k,m,o)
pdf(file="figure2.pdf",height=12.5, width=20)
grid_arrange_shared_legend(i,k,m,o)
dev.off()

##Ploting Econ by PID for mturk and ANES data
##plotting critical values and se for PID

df15 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$econ)*1.96,std.error(mydata[R2,]$econ)*1.96,std.error(mydata[R1,]$econ)*1.96,
               std.error(mydata[I1,]$econ)*1.96,std.error(mydata[D1,]$econ)*1.96,std.error(mydata[D2,]$econ)*1.96,
               std.error(mydata[D3,]$econ)*1.96,std.error(mydata[R33,]$econ)*1.96,std.error(mydata[R22,]$econ)*1.96,
               std.error(mydata[R11,]$econ)*1.96,std.error(mydata[I11,]$econ)*1.96,std.error(mydata[D11,]$econ)*1.96,
               std.error(mydata[D22,]$econ)*1.96,std.error(mydata[D33,]$econ)*1.96,std.error(mydata2[R333,]$econ)*1.96,std.error(mydata2[R222,]$econ)*1.96,
               std.error(mydata2[R111,]$econ)*1.96,std.error(mydata2[I111,]$econ)*1.96,std.error(mydata2[D111,]$econ)*1.96,
               std.error(mydata2[D222,]$econ)*1.96,std.error(mydata2[D333,]$econ)*1.96)),
  Means =c(mean(mydata[R3,]$econ, na.rm=TRUE),mean(mydata[R2,]$econ, na.rm=TRUE),mean(mydata[R1,]$econ, na.rm=TRUE),
           mean(mydata[I1,]$econ, na.rm=TRUE),mean(mydata[D1,]$econ, na.rm=TRUE),mean(mydata[D2,]$econ, na.rm=TRUE),
           mean(mydata[D3,]$econ, na.rm=TRUE),mean(mydata[R33,]$econ, na.rm=TRUE),mean(mydata[R22,]$econ, na.rm=TRUE),
           mean(mydata[R11,]$econ, na.rm=TRUE),mean(mydata[I11,]$econ, na.rm=TRUE),mean(mydata[D11,]$econ, na.rm=TRUE),
           mean(mydata[D22,]$econ, na.rm=TRUE),mean(mydata[D33,]$econ, na.rm=TRUE),mean(mydata2[R333,]$econ, na.rm=TRUE),
           mean(mydata2[R222,]$econ, na.rm=TRUE),mean(mydata2[R111,]$econ, na.rm=TRUE),mean(mydata2[I111,]$econ, na.rm=TRUE),
           mean(mydata2[D111,]$econ, na.rm=TRUE),mean(mydata2[D222,]$econ, na.rm=TRUE),mean(mydata2[D333,]$econ, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df15

limits15 <- aes(ymax = Means + se, ymin=Means - se)


p <- ggplot(df15, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits15) +ggtitle("Economic Issue Preferences")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Partisanship")+ylab("Indexed Economic Issue Preferences (0 = Liberal - 1 = Conservative)")+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .60***   Web: r = .63***   MTurk: r = .59***", x = 4, y = 0, size = 4, colour = "black")

p 

##Ploting Econ by ideo for ANES and mturk data
##plotting critical values and se for ideo

##assigning  deology by mode to letters


df16 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$econ)*1.96,std.error(mydata[C2,]$econ)*1.96,std.error(mydata[C1,]$econ)*1.96,
               std.error(mydata[M1,]$econ)*1.96,std.error(mydata[L1,]$econ)*1.96,std.error(mydata[L2,]$econ)*1.96,
               std.error(mydata[L3,]$econ)*1.96,std.error(mydata[C33,]$econ)*1.96,std.error(mydata[C22,]$econ)*1.96,
               std.error(mydata[C11,]$econ)*1.96,std.error(mydata[M11,]$econ)*1.96,std.error(mydata[L11,]$econ)*1.96,
               std.error(mydata[L22,]$econ)*1.96,std.error(mydata[L33,]$econ)*1.96,std.error(mydata2[R333,]$econ)*1.96,std.error(mydata2[R222,]$econ)*1.96,
               std.error(mydata2[R111,]$econ)*1.96,std.error(mydata2[I111,]$econ)*1.96,std.error(mydata2[D111,]$econ)*1.96,
               std.error(mydata2[D222,]$econ)*1.96,std.error(mydata2[D333,]$econ)*1.96)),
  Means =c(mean(mydata[C3,]$econ, na.rm=TRUE),mean(mydata[C2,]$econ, na.rm=TRUE),mean(mydata[C1,]$econ, na.rm=TRUE),
           mean(mydata[M1,]$econ, na.rm=TRUE),mean(mydata[L1,]$econ, na.rm=TRUE),mean(mydata[L2,]$econ, na.rm=TRUE),
           mean(mydata[L3,]$econ, na.rm=TRUE),mean(mydata[C33,]$econ, na.rm=TRUE),mean(mydata[C22,]$econ, na.rm=TRUE),
           mean(mydata[C11,]$econ, na.rm=TRUE),mean(mydata[M11,]$econ, na.rm=TRUE),mean(mydata[L11,]$econ, na.rm=TRUE),
           mean(mydata[L22,]$econ, na.rm=TRUE),mean(mydata[L33,]$econ, na.rm=TRUE),mean(mydata2[C333,]$econ, na.rm=TRUE),
           mean(mydata2[C222,]$econ, na.rm=TRUE),mean(mydata2[C111,]$econ, na.rm=TRUE),mean(mydata2[M111,]$econ, na.rm=TRUE),
           mean(mydata2[L111,]$econ, na.rm=TRUE),mean(mydata2[L222,]$econ, na.rm=TRUE),mean(mydata2[L333,]$econ, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df16

limits16 <- aes(ymax = Means + se, ymin=Means - se)


q <- ggplot(df16, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits16) +ggtitle("Economic Issue Preferences")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Ideology")+ylab("Indexed Economic Issue Preferences (0 = Liberal - 1 = Conservative)")+ 
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = .57***   Web: r = .59***   MTurk: r = .65***", x = 4, y = 0, size = 4, colour = "black")

q 

##Ploting social by PID for ANES data
##plotting critical values and se for PID

df17 <- data.frame(
  se =matrix(c(std.error(mydata[R3,]$social)*1.96,std.error(mydata[R2,]$social)*1.96,std.error(mydata[R1,]$social)*1.96,
               std.error(mydata[I1,]$social)*1.96,std.error(mydata[D1,]$social)*1.96,std.error(mydata[D2,]$social)*1.96,
               std.error(mydata[D3,]$social)*1.96,std.error(mydata[R33,]$social)*1.96,std.error(mydata[R22,]$social)*1.96,
               std.error(mydata[R11,]$social)*1.96,std.error(mydata[I11,]$social)*1.96,std.error(mydata[D11,]$social)*1.96,
               std.error(mydata[D22,]$social)*1.96,std.error(mydata[D33,]$social)*1.96,std.error(mydata2[R333,]$social)*1.96,std.error(mydata2[R222,]$social)*1.96,
               std.error(mydata2[R111,]$social)*1.96,std.error(mydata2[I111,]$social)*1.96,std.error(mydata2[D111,]$social)*1.96,
               std.error(mydata2[D222,]$social)*1.96,std.error(mydata2[D333,]$social)*1.96)),
  Means =c(mean(mydata[R3,]$social, na.rm=TRUE),mean(mydata[R2,]$social, na.rm=TRUE),mean(mydata[R1,]$social, na.rm=TRUE),
           mean(mydata[I1,]$social, na.rm=TRUE),mean(mydata[D1,]$social, na.rm=TRUE),mean(mydata[D2,]$social, na.rm=TRUE),
           mean(mydata[D3,]$social, na.rm=TRUE),mean(mydata[R33,]$social, na.rm=TRUE),mean(mydata[R22,]$social, na.rm=TRUE),
           mean(mydata[R11,]$social, na.rm=TRUE),mean(mydata[I11,]$social, na.rm=TRUE),mean(mydata[D11,]$social, na.rm=TRUE),
           mean(mydata[D22,]$social, na.rm=TRUE),mean(mydata[D33,]$social, na.rm=TRUE),mean(mydata2[R333,]$social, na.rm=TRUE),
           mean(mydata2[R222,]$social, na.rm=TRUE),mean(mydata2[R111,]$social, na.rm=TRUE),mean(mydata2[I111,]$social, na.rm=TRUE),
           mean(mydata2[D111,]$social, na.rm=TRUE),mean(mydata2[D222,]$social, na.rm=TRUE),mean(mydata2[D333,]$social, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Strong Republican", "Republican", "Not Strong Republican", "Independents", "Not Strong Democrat", "Democrat", "Strong Democrat"))
  
)
df17

limits17 <- aes(ymax = Means + se, ymin=Means - se)


r <- ggplot(df17, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits17) +ggtitle("Social Issue Preferences")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Partisanship")+ylab("Indexed Social Issue Preferences (0 = Liberal - 1 = Conservative)")+
  scale_x_discrete(limits=c("Strong Democrat","Democrat","Not Strong Democrat", "Independents","Not Strong Republican", "Republican", "Strong Republican"))+
  annotate("text", label = "FTF: r = .29***   Web: r = .34***   MTurk: r = .44***", x = 4, y = 0, size = 4, colour = "black")

r 

##Ploting social by ideo for ANES data
##plotting critical values and se for ideo

##assigning  deology by mode to letters

df18 <- data.frame(
  se =matrix(c(std.error(mydata[C3,]$social)*1.96,std.error(mydata[C2,]$social)*1.96,std.error(mydata[C1,]$social)*1.96,
               std.error(mydata[M1,]$social)*1.96,std.error(mydata[L1,]$social)*1.96,std.error(mydata[L2,]$social)*1.96,
               std.error(mydata[L3,]$social)*1.96,std.error(mydata[C33,]$social)*1.96,std.error(mydata[C22,]$social)*1.96,
               std.error(mydata[C11,]$social)*1.96,std.error(mydata[M11,]$social)*1.96,std.error(mydata[L11,]$social)*1.96,
               std.error(mydata[L22,]$social)*1.96,std.error(mydata[L33,]$social)*1.96,std.error(mydata2[R333,]$social)*1.96,std.error(mydata2[R222,]$social)*1.96,
               std.error(mydata2[R111,]$social)*1.96,std.error(mydata2[I111,]$social)*1.96,std.error(mydata2[D111,]$social)*1.96,
               std.error(mydata2[D222,]$social)*1.96,std.error(mydata2[D333,]$social)*1.96)),
  Means =c(mean(mydata[C3,]$social, na.rm=TRUE),mean(mydata[C2,]$social, na.rm=TRUE),mean(mydata[C1,]$social, na.rm=TRUE),
           mean(mydata[M1,]$social, na.rm=TRUE),mean(mydata[L1,]$social, na.rm=TRUE),mean(mydata[L2,]$social, na.rm=TRUE),
           mean(mydata[L3,]$social, na.rm=TRUE),mean(mydata[C33,]$social, na.rm=TRUE),mean(mydata[C22,]$social, na.rm=TRUE),
           mean(mydata[C11,]$social, na.rm=TRUE),mean(mydata[M11,]$social, na.rm=TRUE),mean(mydata[L11,]$social, na.rm=TRUE),
           mean(mydata[L22,]$social, na.rm=TRUE),mean(mydata[L33,]$social, na.rm=TRUE),mean(mydata2[C333,]$social, na.rm=TRUE),
           mean(mydata2[C222,]$social, na.rm=TRUE),mean(mydata2[C111,]$social, na.rm=TRUE),mean(mydata2[M111,]$social, na.rm=TRUE),
           mean(mydata2[L111,]$social, na.rm=TRUE),mean(mydata2[L222,]$social, na.rm=TRUE),mean(mydata2[L333,]$social, na.rm=TRUE)), 
  Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                     "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                     "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
  PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
  
)
df18

limits18 <- aes(ymax = Means + se, ymin=Means - se)


s <- ggplot(df18, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ geom_pointrange(limits18) +ggtitle("Social Issue Preferences")+
  ylim(0,1) +theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  xlab("Ideology")+ylab("Indexed Social Issue Preferences (0 = Liberal - 1 = Conservative)")+
  scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))+
  annotate("text", label = "FTF: r = .37***   Web: r = .46***   MTurk: r = .53***", x = 4, y = 0, size = 4, colour = "black")
  s 

  #Plot table for PID
  
  grid_arrange_shared_legend <- function(p,r) {
    plots <- list(p,r)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    main = "TIPI"
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  }
  
  grid_arrange_shared_legend(p,r)
  pdf(file="PIDsocecon.pdf",height=20, width=15)
  grid_arrange_shared_legend(p,r)
  dev.off()
  
  #Plot table for Idoe
  
 
   grid_arrange_shared_legend <- function(q,s) {
    plots <- list(q,s)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    main = "TIPI"
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  }
  
  grid_arrange_shared_legend(q,s)
  pdf(file="figure3.pdf",height=20, width=15)
  grid_arrange_shared_legend(q,s)
  dev.off()  
  
#regressions
  #regression time
  #TIPI
  #PID(7-point scale) = TIPI + demographics 
  #MTurk
  reg1<-lm(mydata2$PID~ mydata2$extraversion +mydata2$agreeableness+mydata2$conscientiousness+mydata2$emostab+
             mydata2$openness+mydata2$age+mydata2$edu+mydata2$male+mydata2$inc+mydata2$religiosity)
  
  #Ideo(7-point sclae) = TIPI + demographics
  #MTurk
  reg3<-lm(mydata2$ideo~ mydata2$extraversion +mydata2$agreeableness+mydata2$conscientiousness+mydata2$emostab+
             mydata2$openness+mydata2$age+mydata2$edu+mydata2$male+mydata2$inc+mydata2$religiosity)
  
  
  stargazer(reg1, type="text", title="Regression Results", align=TRUE)
  stargazer(reg3, type="text", title="Regression Results", align=TRUE)
  stargazer(reg1, reg3, type="text", title="TIPI", align=TRUE)
  
  #Values
  #PID(7-point scale)=values + demographics
  #MTurk
  reg5<-lm(mydata2$PID~ mydata2$escale +mydata2$mtscale+mydata2$rrscale+mydata2$ascale+
             mydata2$age+mydata2$edu+mydata2$male+mydata2$inc+mydata2$religiosity)
  
  #Ideo(7-point sclae) = values + demographics
  #MTurk
  reg7<-lm(mydata2$ideo~ mydata2$escale +mydata2$mtscale+mydata2$rrscale+mydata2$ascale+
             mydata2$age+mydata2$edu+mydata2$male+mydata2$inc+mydata2$religiosity)
  summary(reg7)
  stargazer(reg5, type="text", title="Regression Results", align=TRUE)
  stargazer(reg51, reg61, reg7, type="text", title="Regression Results", align=TRUE)
  stargazer(reg5,reg7, type="text", title="Scales", align=TRUE)
 
  #regression time for economic and social indices 
  #TIPI
  #Econ index = TIPI + demographics 
  #MTurk
  reg10<-lm(mydata2$econ~ mydata2$extraversion +mydata2$agreeableness+mydata2$conscientiousness+mydata2$emostab+
              mydata2$openness+mydata2$age+mydata2$edu+mydata2$male+mydata2$inc+mydata2$religiosity)
  
  #social index = TIPI + demographics
  #MTurk
  reg1111<-lm(mydata2$social~ mydata2$extraversion +mydata2$agreeableness+mydata2$conscientiousness+mydata2$emostab+
              mydata2$openness+mydata2$age+mydata2$edu+mydata2$male+mydata2$inc+mydata2$religiosity)
  
  
  
  stargazer(reg10, reg11, type="text", title="TIPI", align=TRUE)
  
  
  #Values
  #Econ index=values + demographics
  #MTurk
  reg12<-lm(mydata2$econ~ mydata2$escale +mydata2$mtscale+mydata2$rrscale+mydata2$ascale+
              mydata2$age+mydata2$edu+mydata2$male+mydata2$inc+mydata2$religiosity)
  
  #social index = values + demographics
  #MTurk
  reg13<-lm(mydata2$social~ mydata2$escale +mydata2$mtscale+mydata2$rrscale+mydata2$ascale+
              mydata2$age+mydata2$edu+mydata2$male+mydata2$inc+mydata2$religiosity)
  
  
  stargazer(reg12,reg13, type="text", title="Scales", align=TRUE)
  
     
  #Regression Models for ANES TS 2012
  #regression time
  #TIPI
  #PID(7-point scale) = TIPI + demographics 
  #FTF
  reg11<-lm(mydata$PID~ mydata[ftf,]$extraversion +mydata[ftf,]$agreeableness+mydata[ftf,]$conscientiousness+mydata[ftf,]$emostab+
             mydata[ftf,]$openness+mydata[ftf,]$age+mydata[ftf,]$edu+mydata[ftf,]$male+mydata[ftf,]$inc+mydata[ftf,]$religiosity)
  
  #Internet
  reg21<-lm(mydata$PID~ mydata[int,]$extraversion +mydata[int,]$agreeableness+mydata[int,]$conscientiousness+mydata[int,]$emostab+
             mydata[int,]$openness+mydata[int,]$age+mydata[int,]$edu+mydata[int,]$male+mydata[int,]$inc+mydata[int,]$religiosity)
  
  #Ideo(7-point sclae) = TIPI + demographics
  #FTF
  reg31<-lm(mydata$ideology~ mydata[ftf,]$extraversion +mydata[ftf,]$agreeableness+mydata[ftf,]$conscientiousness+mydata[ftf,]$emostab+
             mydata[ftf,]$openness+mydata[ftf,]$age+mydata[ftf,]$edu+mydata[ftf,]$male+mydata[ftf,]$inc+mydata[ftf,]$religiosity)
  
  #Internet
  reg4<-lm(mydata$ideology~ mydata[int,]$extraversion +mydata[int,]$agreeableness+mydata[int,]$conscientiousness+mydata[int,]$emostab+
             mydata[int,]$openness+mydata[int,]$age+mydata[int,]$edu+mydata[int,]$male+mydata[int,]$inc+mydata[int,]$religiosity)
  
  stargazer(reg1, type="text", title="Regression Results", align=TRUE) #FTF TIPI PID
  stargazer(reg31, type="text", title="Regression Results", align=TRUE) #FTF TIPI IDOE
  stargazer(reg2, type="text", title="Regression Results", align=TRUE) #INT TIPI PID
  stargazer(reg4, type="text", title="Regression Results", align=TRUE) #INT TIPI IDOE
  #Values
  #PID(7-point scale)=values + demographics
  #FTF
  reg51<-lm(mydata$PID~ mydata[ftf,]$escale +mydata[ftf,]$mtscale+mydata[ftf,]$rrscale+mydata[ftf,]$ascale+
             mydata[ftf,]$age+mydata[ftf,]$edu+mydata[ftf,]$male+mydata[ftf,]$inc+mydata[ftf,]$religiosity)
  
  #Internet
  reg61<-lm(mydata$PID~ mydata[int,]$escale +mydata[int,]$mtscale+mydata[int,]$rrscale+mydata[int,]$ascale+
             mydata[int,]$age+mydata[int,]$edu+mydata[int,]$male+mydata[int,]$inc+mydata[int,]$religiosity)
  
  #Ideo(7-point sclae) = values + demographics
  #FTF
  reg71<-lm(mydata$ideology~ mydata[ftf,]$escale +mydata[ftf,]$mtscale+mydata[ftf,]$rrscale+mydata[ftf,]$ascale+
             mydata[ftf,]$age+mydata[ftf,]$edu+mydata[ftf,]$male+mydata[ftf,]$inc+mydata[ftf,]$religiosity)
  
  #Internet
  reg81<-lm(mydata$ideology~ mydata[int,]$escale +mydata[int,]$mtscale+mydata[int,]$rrscale+mydata[int,]$ascale+
             mydata[int,]$age+mydata[int,]$edu+mydata[int,]$male+mydata[int,]$inc+mydata[int,]$religiosity)
  
  stargazer(reg5, type="text", title="Regression Results", align=TRUE) #Values FTF PID
  stargazer(reg7, type="text", title="Regression Results", align=TRUE) #Values FTF IDEO
  stargazer(reg6, type="text", title="Regression Results", align=TRUE) #Values INT PID
  stargazer(reg8, type="text", title="Regression Results", align=TRUE) #Values INT IDeo
  
  #regression time for economic and social indices 
  #TIPI
  #Econ index = TIPI + demographics 
  #ANES TS 2012 FTF
  reg101<-lm(mydata$econ~ mydata[ftf,]$extraversion +mydata[ftf,]$agreeableness+mydata[ftf,]$conscientiousness+mydata[ftf,]$emostab+
              mydata[ftf,]$openness+mydata[ftf,]$age+mydata[ftf,]$edu+mydata[ftf,]$male+mydata[ftf,]$inc+mydata[ftf,]$religiosity)
  
  #social index = TIPI + demographics
  #ANES TS 2012 FTF
  reg111<-lm(mydata$social~ mydata[ftf,]$extraversion +mydata[ftf,]$agreeableness+mydata[ftf,]$conscientiousness+mydata[ftf,]$emostab+
              mydata[ftf,]$openness+mydata[ftf,]$age+mydata[ftf,]$edu+mydata[ftf,]$male+mydata[ftf,]$inc+mydata[ftf,]$religiosity)
  
  #Econ index = TIPI + demographics 
  #ANES TS 2012 INT
  reg121<-lm(mydata$econ~ mydata[int,]$extraversion +mydata[int,]$agreeableness+mydata[int,]$conscientiousness+mydata[int,]$emostab+
              mydata[int,]$openness+mydata[int,]$age+mydata[int,]$edu+mydata[int,]$male+mydata[int,]$inc+mydata[int,]$religiosity)
  
  #social index = TIPI + demographics
  #ANES TS 2012 INT
  reg131<-lm(mydata$social~ mydata[int,]$extraversion +mydata[int,]$agreeableness+mydata[int,]$conscientiousness+mydata[int,]$emostab+
              mydata[int,]$openness+mydata[int,]$age+mydata[int,]$edu+mydata[int,]$male+mydata[int,]$inc+mydata[int,]$religiosity)
  
  
  
  stargazer(reg10, reg11, type="text", title="TIPI", align=TRUE) #Reg10=Econ ANES FTF TIPI #reg11 Social ANES FTF TIPI
  stargazer(reg12, reg13, type="text", title="TIPI", align=TRUE) #Reg10=Econ ANES INT TIPI #reg11 Social ANES INT TIPI
  
  
  #Values
  #Econ index=values + demographics
  #MTurk FTF
  reg141<-lm(mydata$econ~ mydata[ftf,]$escale +mydata[ftf,]$mtscale+mydata[ftf,]$rrscale+mydata[ftf,]$ascale+
              mydata[ftf,]$age+mydata[ftf,]$edu+mydata[ftf,]$male+mydata[ftf,]$inc+mydata[ftf,]$religiosity)
  
  #social index = values + demographics
  #MTurk FTF
  reg151<-lm(mydata$social~ mydata[ftf,]$escale +mydata[ftf,]$mtscale+mydata[ftf,]$rrscale+mydata[ftf,]$ascale+
              mydata[ftf,]$age+mydata[ftf,]$edu+mydata[ftf,]$male+mydata[ftf,]$inc+mydata[ftf,]$religiosity)
  
  #Values
  #Econ index=values + demographics
  #MTurk INT
  reg161<-lm(mydata$econ~ mydata[int,]$escale +mydata[int,]$mtscale+mydata[int,]$rrscale+mydata[int,]$ascale+
              mydata[int,]$age+mydata[int,]$edu+mydata[int,]$male+mydata[int,]$inc+mydata[int,]$religiosity)
  
  #social index = values + demographics
  #MTurk INT
  reg171<-lm(mydata$social~ mydata[int,]$escale +mydata[int,]$mtscale+mydata[int,]$rrscale+mydata[int,]$ascale+
              mydata[int,]$age+mydata[int,]$edu+mydata[int,]$male+mydata[int,]$inc+mydata[int,]$religiosity)
  
  
  
  stargazer(reg14, reg15, type="text", title="TIPI", align=TRUE) #Reg10=Econ ANES FTF VALUES #reg11 Social ANES FTF VALUES
  stargazer(reg16, reg17, type="text", title="TIPI", align=TRUE) #Reg10=Econ ANES INT VALUES #reg11 Social ANES INT VALUES

  ##Creating coefficent plots
  ##MTurk ideo TIPI

  pdf(file="figure4.pdf",height=20, width=15)
 
  par(mfrow=c(3,2))
  var.names1 <- c("Extraversion", "Agreeableness",
                  "Conscientiousness", "Emotional Stability",
                  "Openness")
  
  # set the graphical parameters
###FIgure 4
  par(
    family = "serif",  # I don't plot in anything but serif
    oma = c(0,0,0,0),  # Since it is a single plot, I set the outer margins to zero.
    mar = c(2.5,7.5,4,2)   # Inner margins are set through a little trial and error.
  )
#Ideology regressed by TIPI  
  plot(NULL,                              # create empty plot
       xlim = c(-.35, .35),                      # set xlim by guessing
       ylim = c(.7, length(var.names1) + .3), # set ylim by the number of variables
       axes = F, xlab = NA, ylab = NA)       # turn off axes and labels
  
  #add ANES FTF data
 est <- coef(reg31)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg31)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd=1.5)         # add the 95% confidence intervals
    text(-.375, i, var.names1[i], xpd = T, cex = .9, pos = 2)                      # add the variable names
  }
  
  #add ANES Web data
  est <- coef(reg4)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg4)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.15, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.15, i-.15), lty=2, lwd=1.5)         # add the 95% confidence intervals
                       # add the variable names
  }
  
  #add MTurk data
  est <- coef(reg3)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg3)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.3, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.3, i-.3), lty=3, lwd=1.5)         # add the 95% confidence intervals
                     # add the variable names
  }
  
  # add axes and labels
  axis(side = 1)                                                                                          # add bottom axis
  abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
  #mtext(side = 1, "OLS Coefficient (positive values indicate greater conservatism)", line = 3)                                              # label bottom axis
  mtext(side = 3, "Self-Reported Political Ideology and The Big Five", line = 1)   # add title
  box()                                                                                                   # add lines around the plot
  

  labels<-c("ANES FTF","ANES WEB", "MTurk")
  legend("topright", title="Datasets", labels, lwd=1.5, lty=c(1,2,3),bty = "n")
  

###FIgure 5
  #Ideology regressed by Values  
  var.names1 <- c("Egalitarianism ", "Moral Traditionalism",
                  "Racial Resentment",
                  "Authoritarianism")
  plot(NULL,                              # create empty plot
       xlim = c(-3, 3),                      # set xlim by guessing
       ylim = c(.7, length(var.names1) + .3), # set ylim by the number of variables
       axes = F, xlab = NA, ylab = NA)       # turn off axes and labels
  
  #add ANES FTF data
  est <- coef(reg71)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg71)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i))         # add the 95% confidence intervals
    text(-3.25, i, var.names1[i], xpd = T, cex = .9, pos = 2)                      # add the variable names
  }
  
  #add ANES Web data
  est <- coef(reg81)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg81)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.15, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.15, i-.15), lty=2, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  #add MTurk data
  est <- coef(reg7)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg7)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.3, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.3, i-.3), lty=3, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  # add axes and labels
  axis(side = 1)                                                                                          # add bottom axis
  abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
  #mtext(side = 1, "OLS Coefficient (positive values indicate greater conservatism)", line = 3)                                              # label bottom axis
  mtext(side = 3, "Self-Reported Political Ideology and Values", line = 1)   # add title
  box()                                                                                                   # add lines around the plot
  
  
  labels<-c("ANES FTF","ANES WEB", "MTurk")
  legend("topright", title="Datasets", labels, lwd=1.5, lty=c(1,2,3),bty = "n")

##Figure 6  
  #Economic preferences regressed by TIPI  
  
  var.names1 <- c("Extraversion", "Agreeableness",
                  "Conscientiousness", "Emotional Stability",
                  "Openness")
  plot(NULL,                              # create empty plot
       xlim = c(-.05, .05),                      # set xlim by guessing
       ylim = c(.7, length(var.names1) + .3), # set ylim by the number of variables
       axes = F, xlab = NA, ylab = NA)       # turn off axes and labels
  
  
  #add ANES FTF data
  est <- coef(reg101)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg101)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd=1.5)         # add the 95% confidence intervals
    text(-.055, i, var.names1[i], xpd = T, cex = .9, pos = 2)                      # add the variable names
  }
  
  #add ANES Web data
  est <- coef(reg121)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg121)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.15, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.15, i-.15), lty=2, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  #add MTurk data
  est <- coef(reg10)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg10)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.3, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.3, i-.3), lty=3, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  # add axes and labels
  axis(side = 1)                                                                                          # add bottom axis
  abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
  #mtext(side = 1, "OLS Coefficient (positive values indicate greater conservatism)", line = 3)                                              # label bottom axis
  mtext(side = 3, "Economic Ideology and The Big Five", line = 1)   # add title
  box()                                                                                                   # add lines around the plot
  
  
  labels<-c("ANES FTF","ANES WEB", "MTurk")
  legend("topright", title="Datasets", labels, lwd=1.5, lty=c(1,2,3),bty = "n")
  
  #Economic preferences regressed by Values  table 7
  
  var.names1 <- c("Egalitarianism ", "Moral Traditionalism",
                  "Racial Resentment",
                  "Authoritarianism")
  plot(NULL,                              # create empty plot
       xlim = c(-.6, .6),                      # set xlim by guessing
       ylim = c(.7, length(var.names1) + .3), # set ylim by the number of variables
       axes = F, xlab = NA, ylab = NA)       # turn off axes and labels
  
  
  #add ANES FTF data
  est <- coef(reg141)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg141)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd=1.5)         # add the 95% confidence intervals
    text(-.65, i, var.names1[i], xpd = T, cex = .9, pos = 2)                      # add the variable names
  }
  
  #add ANES Web data
  est <- coef(reg161)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg161)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.15, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.15, i-.15), lty=2, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  #add MTurk data
  est <- coef(reg12)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg12)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.3, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.3, i-.3), lty=3, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  #plotting significant differences 
  #points(.29, 2-.15, pch = "*", cex = 1)
  #points(.32, 2-.15, pch = "*", cex = 1)
  #points(.35, 2-.15, pch = "*", cex = 1)
  
  # add axes and labels
  axis(side = 1)                                                                                          # add bottom axis
  abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
  #mtext(side = 1, "OLS Coefficient (positive values indicate greater conservatism)", line = 3)                                              # label bottom axis
  mtext(side = 3, "Economic Ideology and Values", line = 1)   # add title
  box()                                                                                                   # add lines around the plot
  
  
  labels<-c("ANES FTF","ANES WEB", "MTurk")
  legend("topright", title="Datasets", labels, lwd=1.5, lty=c(1,2,3),bty = "n")
  
  #social preferences regressed by TIPI  Figure 8
  
  var.names1 <- c("Extraversion", "Agreeableness",
                  "Conscientiousness", "Emotional Stability",
                  "Openness")
  plot(NULL,                              # create empty plot
       xlim = c(-.07, .07),                      # set xlim by guessing
       ylim = c(.7, length(var.names1) + .3), # set ylim by the number of variables
       axes = F, xlab = NA, ylab = NA)       # turn off axes and labels
  
  
  #add ANES FTF data
  est <- coef(reg111)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg111)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd=1.5)         # add the 95% confidence intervals
    text(-.075, i, var.names1[i], xpd = T, cex = .9, pos = 2)                      # add the variable names
  }
  
  #add ANES Web data
  est <- coef(reg131)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg131)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.15, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.15, i-.15), lty=2, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  #add MTurk data
  est <- coef(reg1111)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg1111)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.3, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.3, i-.3), lty=3, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  # add axes and labels
  axis(side = 1)                                                                                          # add bottom axis
  abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
  mtext(side = 1, "OLS Coefficient (positive values indicate greater conservatism)", line = 3)                                              # label bottom axis
  mtext(side = 3, "Social Ideology and The Big Five", line = 1)   # add title
  box()                                                                                                   # add lines around the plot
  
  
  labels<-c("ANES FTF","ANES WEB", "MTurk")
  legend("topright", title="Datasets", labels, lwd=1.5, lty=c(1,2,3),bty = "n")
  
  #Social preferences regressed by Values  Figure 9
  
  var.names1 <- c("Egalitarianism ", "Moral Traditionalism",
                  "Racial Resentment",
                  "Authoritarianism")
  plot(NULL,                              # create empty plot
       xlim = c(-.6, .6),                      # set xlim by guessing
       ylim = c(.7, length(var.names1) + .3), # set ylim by the number of variables
       axes = F, xlab = NA, ylab = NA)       # turn off axes and labels
  
  
  #add ANES FTF data
  est <- coef(reg151)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg151)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd=1.5)         # add the 95% confidence intervals
    text(-.65, i, var.names1[i], xpd = T, cex = .9, pos = 2)                      # add the variable names
  }
  
  #add ANES Web data
  est <- coef(reg171)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg171)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.15, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.15, i-.15), lty=2, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  #add MTurk data
  est <- coef(reg13)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg13)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.3, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.3, i-.3), lty=3, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
#plotting significant differences 
  #points(.20, 4, pch = "*", cex = 1)
  #points(.23, 4, pch = "*", cex = 1)
  #points(.26, 4, pch = "*", cex = 1)
  
  #points(.13, 4-.15, pch = "*", cex = 1)
  #points(.16, 4-.15, pch = "*", cex = 1)
  
  
  # add axes and labels
  axis(side = 1)                                                                                          # add bottom axis
  abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
  mtext(side = 1, "OLS Coefficient (positive values indicate greater conservatism)", line = 3)                                              # label bottom axis
  mtext(side = 3, "Social Ideology and Values", line = 1)   # add title
  box()                                                                                                   # add lines around the plot
  
  
  labels<-c("ANES FTF","ANES WEB", "MTurk")
  legend("topright", title="Datasets", labels, lwd=1.5, lty=c(1,2,3),bty = "n")
  dev.off()
  
  
  pdf(file="pidcoefs.pdf",height=8, width=8)
  #summary(reg71)
  #attach(mtcars)
  par(mfrow=c(2,1)) 
  var.names1 <- c("Extraversion", "Agreeableness",
                  "Conscientiousness", "Emotional Stability",
                  "Openness")
  par(
    family = "serif",  # I don't plot in anything but serif
    oma = c(0,0,0,0),  # Since it is a single plot, I set the outer margins to zero.
    mar = c(5,7.5,3,2)   # Inner margins are set through a little trial and error.
  )
  ###figure 4a
  #PID regressed by TIPI  
  plot(NULL,                              # create empty plot
       xlim = c(-.4, .4),                      # set xlim by guessing
       ylim = c(.7, length(var.names1) + .3), # set ylim by the number of variables
       axes = F, xlab = NA, ylab = NA)       # turn off axes and labels
  
  #add ANES FTF data
  est <- coef(reg11)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg11)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd=1.5)         # add the 95% confidence intervals
    text(-.43, i, var.names1[i], xpd = T, cex = .9, pos = 2)                      # add the variable names
  }
  
  #add ANES Web data
  est <- coef(reg21)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg21)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.15, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.15, i-.15), lty=2, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  #add MTurk data
  est <- coef(reg1)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg1)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.3, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.3, i-.3), lty=3, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  # add axes and labels
  axis(side = 1)                                                                                          # add bottom axis
  abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
 # mtext(side = 1, "OLS Coefficient (positive values indicate greater conservatism)", line = 3)                                              # label bottom axis
  mtext(side = 3, "Self-Reported Partisanship and the Big Five", line = 1)   # add title
  box()                                                                                                   # add lines around the plot
  
  
  labels<-c("ANES FTF","ANES WEB", "MTurk")
  legend("topright", title="Datasets", labels, lwd=1.5, lty=c(1,2,3),bty = "n")
 
  ###Figure 5A 
  #PID regressed by Values  
  var.names1 <- c("Egalitarianism", "Moral Traditionalism",
                  "Racial Resentment",
                  "Authoritarianism")
  plot(NULL,                              # create empty plot
       xlim = c(-3, 3),                      # set xlim by guessing
       ylim = c(.7, length(var.names1) + .3), # set ylim by the number of variables
       axes = F, xlab = NA, ylab = NA)       # turn off axes and labels
  
  #add ANES FTF data
  est <- coef(reg51)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg51)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd=1.5)         # add the 95% confidence intervals
    text(-3.25, i, var.names1[i], xpd = T, cex = .9, pos = 2)                      # add the variable names
  }
  
  #add ANES Web data
  est <- coef(reg61)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg61)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.15, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.15, i-.15), lty=2, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  #add MTurk data
  est <- coef(reg5)[-1]                                                    # conveniently store the estimates (minus the constant)
  se <- sqrt(diag(vcov(reg5)))[-1]                                         # conveniently store the std. errors (minus the constant)
  for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
    points(est[i], i-.3, pch = 19, cex = .8)                               # add the points to the plot
    lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i-.3, i-.3), lty=3, lwd=1.5)         # add the 95% confidence intervals
    # add the variable names
  }
  
  # add axes and labels
  axis(side = 1)                                                                                          # add bottom axis
  abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
  mtext(side = 1, "OLS Coefficient (positive values indicate greater conservatism)", line = 3)                                              # label bottom axis
  mtext(side = 3, "Self-Reported Partisanship and Values", line = 1)   # add title
  box()                                                                                                   # add lines around the plot
  
  
  labels<-c("ANES FTF","ANES WEB", "MTurk")
  legend("topright", title="Datasets", labels, lwd=1.5, lty=c(1,2,3),bty = "n")
  dev.off()
  
   
#looking at feeling therms
  table(mydata$ft_rep)
  mydata$ft_rep[mydata$ft_rep=="-9"]<-NA
  mydata$ft_rep[mydata$ft_rep=="-8"]<-NA
  mydata$ft_rep[mydata$ft_rep=="-2"]<-NA
  
  table(mydata$ft_dem)
  mydata$ft_dem[mydata$ft_dem=="-9"]<-NA
  mydata$ft_dem[mydata$ft_dem=="-8"]<-NA
  mydata$ft_dem[mydata$ft_dem=="-2"]<-NA
  
  table(mydata$ftgr_cons)
  mydata$ftgr_cons[mydata$ftgr_cons=="-9"]<-NA
  mydata$ftgr_cons[mydata$ftgr_cons=="-8"]<-NA
  mydata$ftgr_cons[mydata$ftgr_cons=="-2"]<-NA
  mydata$ftgr_cons[mydata$ftgr_cons=="-7"]<-NA
  mydata$ftgr_cons[mydata$ftgr_cons=="-6"]<-NA
  
  table(mydata$ftgr_liberals)
  mydata$ftgr_liberals[mydata$ftgr_liberals=="-9"]<-NA
  mydata$ftgr_liberals[mydata$ftgr_liberals=="-8"]<-NA
  mydata$ftgr_liberals[mydata$ftgr_liberals=="-2"]<-NA
  mydata$ftgr_liberals[mydata$ftgr_liberals=="-7"]<-NA
  mydata$ftgr_liberals[mydata$ftgr_liberals=="-6"]<-NA
  
  table(mydata2$therm_1)#libs
  table(mydata2$therm_2)#cons
  table(mydata2$therm_3)#dem party
  table(mydata2$therm_4)#rep party

  #indexing therms
  mydata2$repcontherm<-(mydata2$therm_2+mydata2$therm_4)/2
  table(mydata2$repcontherm)
  
  mydata$repcontherm<-(mydata$ft_rep+mydata$ftgr_cons)/2
  table(mydata2$repcontherm)
  
  mydata2$demlibtherm<-(mydata2$therm_1+mydata2$therm_3)/2
  table(mydata2$demlibtherm)
  
  mydata$demlibtherm<-(mydata$ft_dem+mydata$ftgr_liberals)/2
  table(mydata$demlibtherm)
  
  
  df200 <- data.frame(
    se =matrix(c(std.error(mydata[C3,]$ft_rep)*1.96,std.error(mydata[C2,]$ft_rep)*1.96,std.error(mydata[C1,]$ft_rep)*1.96,
                 std.error(mydata[M1,]$ft_rep)*1.96,std.error(mydata[L1,]$ft_rep)*1.96,std.error(mydata[L2,]$ft_rep)*1.96,
                 std.error(mydata[L3,]$ft_rep)*1.96,std.error(mydata[C33,]$ft_rep)*1.96,std.error(mydata[C22,]$ft_rep)*1.96,
                 std.error(mydata[C11,]$ft_rep)*1.96,std.error(mydata[M11,]$ft_rep)*1.96,std.error(mydata[L11,]$ft_rep)*1.96,
                 std.error(mydata[L22,]$ft_rep)*1.96,std.error(mydata[L33,]$ft_rep)*1.96,std.error(mydata2[C333,]$therm_4)*1.96,std.error(mydata2[C222,]$therm_4)*1.96,
                 std.error(mydata2[C111,]$therm_4)*1.96,std.error(mydata2[M111,]$therm_4)*1.96,std.error(mydata2[L111,]$therm_4)*1.96,
                 std.error(mydata2[L222,]$therm_4)*1.96,std.error(mydata2[L333,]$therm_4)*1.96)),
    Means =c(mean(mydata[C3,]$ft_rep, na.rm=TRUE),mean(mydata[C2,]$ft_rep, na.rm=TRUE),mean(mydata[C1,]$ft_rep, na.rm=TRUE),
             mean(mydata[M1,]$ft_rep, na.rm=TRUE),mean(mydata[L1,]$ft_rep, na.rm=TRUE),mean(mydata[L2,]$ft_rep, na.rm=TRUE),
             mean(mydata[L3,]$ft_rep, na.rm=TRUE),mean(mydata[C33,]$ft_rep, na.rm=TRUE),mean(mydata[C22,]$ft_rep, na.rm=TRUE),
             mean(mydata[C11,]$ft_rep, na.rm=TRUE),mean(mydata[M11,]$ft_rep, na.rm=TRUE),mean(mydata[L11,]$ft_rep, na.rm=TRUE),
             mean(mydata[L22,]$ft_rep, na.rm=TRUE),mean(mydata[L33,]$ft_rep, na.rm=TRUE),mean(mydata2[C333,]$therm_4, na.rm=TRUE),
             mean(mydata2[C222,]$therm_4, na.rm=TRUE),mean(mydata2[C111,]$therm_4, na.rm=TRUE),mean(mydata2[M111,]$therm_4, na.rm=TRUE),
             mean(mydata2[L111,]$therm_4, na.rm=TRUE),mean(mydata2[L222,]$therm_4, na.rm=TRUE),mean(mydata2[L333,]$therm_4, na.rm=TRUE)), 
    Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                       "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                       "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
    PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
    
  )
  df200
  
  
  limits200 <- aes(ymax = Means + se, ymin=Means - se)
  
  
  t <- ggplot(df200, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits200)+ggtitle("Republican Therm")+
    ylim(0,100)  + xlab("Ideology")+ylab("Level of Republican Therm (0 = Low - 7 = High)")+
    theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))
  t 
  
  df201 <- data.frame(
    se =matrix(c(std.error(mydata[C3,]$ft_dem)*1.96,std.error(mydata[C2,]$ft_dem)*1.96,std.error(mydata[C1,]$ft_dem)*1.96,
                 std.error(mydata[M1,]$ft_dem)*1.96,std.error(mydata[L1,]$ft_dem)*1.96,std.error(mydata[L2,]$ft_dem)*1.96,
                 std.error(mydata[L3,]$ft_dem)*1.96,std.error(mydata[C33,]$ft_dem)*1.96,std.error(mydata[C22,]$ft_dem)*1.96,
                 std.error(mydata[C11,]$ft_dem)*1.96,std.error(mydata[M11,]$ft_dem)*1.96,std.error(mydata[L11,]$ft_dem)*1.96,
                 std.error(mydata[L22,]$ft_dem)*1.96,std.error(mydata[L33,]$ft_dem)*1.96,std.error(mydata2[C333,]$therm_3)*1.96,std.error(mydata2[C222,]$therm_3)*1.96,
                 std.error(mydata2[C111,]$therm_3)*1.96,std.error(mydata2[M111,]$therm_3)*1.96,std.error(mydata2[L111,]$therm_3)*1.96,
                 std.error(mydata2[L222,]$therm_3)*1.96,std.error(mydata2[L333,]$therm_3)*1.96)),
    Means =c(mean(mydata[C3,]$ft_dem, na.rm=TRUE),mean(mydata[C2,]$ft_dem, na.rm=TRUE),mean(mydata[C1,]$ft_dem, na.rm=TRUE),
             mean(mydata[M1,]$ft_dem, na.rm=TRUE),mean(mydata[L1,]$ft_dem, na.rm=TRUE),mean(mydata[L2,]$ft_dem, na.rm=TRUE),
             mean(mydata[L3,]$ft_dem, na.rm=TRUE),mean(mydata[C33,]$ft_dem, na.rm=TRUE),mean(mydata[C22,]$ft_dem, na.rm=TRUE),
             mean(mydata[C11,]$ft_dem, na.rm=TRUE),mean(mydata[M11,]$ft_dem, na.rm=TRUE),mean(mydata[L11,]$ft_dem, na.rm=TRUE),
             mean(mydata[L22,]$ft_dem, na.rm=TRUE),mean(mydata[L33,]$ft_dem, na.rm=TRUE),mean(mydata2[C333,]$therm_3, na.rm=TRUE),
             mean(mydata2[C222,]$therm_3, na.rm=TRUE),mean(mydata2[C111,]$therm_3, na.rm=TRUE),mean(mydata2[M111,]$therm_3, na.rm=TRUE),
             mean(mydata2[L111,]$therm_3, na.rm=TRUE),mean(mydata2[L222,]$therm_3, na.rm=TRUE),mean(mydata2[L333,]$therm_3, na.rm=TRUE)), 
    Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                       "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                       "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
    PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
    
  )
  df201
  
  
  limits201 <- aes(ymax = Means + se, ymin=Means - se)
  
  
  u <- ggplot(df201, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits201)+ggtitle("Democrat Therm")+
    ylim(0,100)  + xlab("Ideology")+ylab("Level of Democrat Therm (0 = Low - 7 = High)")+
    theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))
  u 
  
  df202 <- data.frame(
    se =matrix(c(std.error(mydata[C3,]$ftgr_cons)*1.96,std.error(mydata[C2,]$ftgr_cons)*1.96,std.error(mydata[C1,]$ftgr_cons)*1.96,
                 std.error(mydata[M1,]$ftgr_cons)*1.96,std.error(mydata[L1,]$ftgr_cons)*1.96,std.error(mydata[L2,]$ftgr_cons)*1.96,
                 std.error(mydata[L3,]$ftgr_cons)*1.96,std.error(mydata[C33,]$ftgr_cons)*1.96,std.error(mydata[C22,]$ftgr_cons)*1.96,
                 std.error(mydata[C11,]$ftgr_cons)*1.96,std.error(mydata[M11,]$ftgr_cons)*1.96,std.error(mydata[L11,]$ftgr_cons)*1.96,
                 std.error(mydata[L22,]$ftgr_cons)*1.96,std.error(mydata[L33,]$ftgr_cons)*1.96,std.error(mydata2[C333,]$therm_2)*1.96,std.error(mydata2[C222,]$therm_2)*1.96,
                 std.error(mydata2[C111,]$therm_2)*1.96,std.error(mydata2[M111,]$therm_2)*1.96,std.error(mydata2[L111,]$therm_2)*1.96,
                 std.error(mydata2[L222,]$therm_2)*1.96,std.error(mydata2[L333,]$therm_2)*1.96)),
    Means =c(mean(mydata[C3,]$ftgr_cons, na.rm=TRUE),mean(mydata[C2,]$ftgr_cons, na.rm=TRUE),mean(mydata[C1,]$ftgr_cons, na.rm=TRUE),
             mean(mydata[M1,]$ftgr_cons, na.rm=TRUE),mean(mydata[L1,]$ftgr_cons, na.rm=TRUE),mean(mydata[L2,]$ftgr_cons, na.rm=TRUE),
             mean(mydata[L3,]$ftgr_cons, na.rm=TRUE),mean(mydata[C33,]$ftgr_cons, na.rm=TRUE),mean(mydata[C22,]$ftgr_cons, na.rm=TRUE),
             mean(mydata[C11,]$ftgr_cons, na.rm=TRUE),mean(mydata[M11,]$ftgr_cons, na.rm=TRUE),mean(mydata[L11,]$ftgr_cons, na.rm=TRUE),
             mean(mydata[L22,]$ftgr_cons, na.rm=TRUE),mean(mydata[L33,]$ftgr_cons, na.rm=TRUE),mean(mydata2[C333,]$therm_2, na.rm=TRUE),
             mean(mydata2[C222,]$therm_2, na.rm=TRUE),mean(mydata2[C111,]$therm_2, na.rm=TRUE),mean(mydata2[M111,]$therm_2, na.rm=TRUE),
             mean(mydata2[L111,]$therm_2, na.rm=TRUE),mean(mydata2[L222,]$therm_2, na.rm=TRUE),mean(mydata2[L333,]$therm_2, na.rm=TRUE)), 
    Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                       "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                       "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
    PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
    
  )
  df202
  
  
  limits202 <- aes(ymax = Means + se, ymin=Means - se)
  
  
  v <- ggplot(df202, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits202)+ggtitle("Conservative Therm")+
    ylim(0,100)  + xlab("Ideology")+ylab("Level of Conservative Therm (0 = Low - 7 = High)")+
    theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))
  v 
  
  df203 <- data.frame(
    se =matrix(c(std.error(mydata[C3,]$ftgr_liberals)*1.96,std.error(mydata[C2,]$ftgr_liberals)*1.96,std.error(mydata[C1,]$ftgr_liberals)*1.96,
                 std.error(mydata[M1,]$ftgr_liberals)*1.96,std.error(mydata[L1,]$ftgr_liberals)*1.96,std.error(mydata[L2,]$ftgr_liberals)*1.96,
                 std.error(mydata[L3,]$ftgr_liberals)*1.96,std.error(mydata[C33,]$ftgr_liberals)*1.96,std.error(mydata[C22,]$ftgr_liberals)*1.96,
                 std.error(mydata[C11,]$ftgr_liberals)*1.96,std.error(mydata[M11,]$ftgr_liberals)*1.96,std.error(mydata[L11,]$ftgr_liberals)*1.96,
                 std.error(mydata[L22,]$ftgr_liberals)*1.96,std.error(mydata[L33,]$ftgr_liberals)*1.96,std.error(mydata2[C333,]$therm_1)*1.96,std.error(mydata2[C222,]$therm_1)*1.96,
                 std.error(mydata2[C111,]$therm_1)*1.96,std.error(mydata2[M111,]$therm_1)*1.96,std.error(mydata2[L111,]$therm_1)*1.96,
                 std.error(mydata2[L222,]$therm_1)*1.96,std.error(mydata2[L333,]$therm_1)*1.96)),
    Means =c(mean(mydata[C3,]$ftgr_liberals, na.rm=TRUE),mean(mydata[C2,]$ftgr_liberals, na.rm=TRUE),mean(mydata[C1,]$ftgr_liberals, na.rm=TRUE),
             mean(mydata[M1,]$ftgr_liberals, na.rm=TRUE),mean(mydata[L1,]$ftgr_liberals, na.rm=TRUE),mean(mydata[L2,]$ftgr_liberals, na.rm=TRUE),
             mean(mydata[L3,]$ftgr_liberals, na.rm=TRUE),mean(mydata[C33,]$ftgr_liberals, na.rm=TRUE),mean(mydata[C22,]$ftgr_liberals, na.rm=TRUE),
             mean(mydata[C11,]$ftgr_liberals, na.rm=TRUE),mean(mydata[M11,]$ftgr_liberals, na.rm=TRUE),mean(mydata[L11,]$ftgr_liberals, na.rm=TRUE),
             mean(mydata[L22,]$ftgr_liberals, na.rm=TRUE),mean(mydata[L33,]$ftgr_liberals, na.rm=TRUE),mean(mydata2[C333,]$therm_1, na.rm=TRUE),
             mean(mydata2[C222,]$therm_1, na.rm=TRUE),mean(mydata2[C111,]$therm_1, na.rm=TRUE),mean(mydata2[M111,]$therm_1, na.rm=TRUE),
             mean(mydata2[L111,]$therm_1, na.rm=TRUE),mean(mydata2[L222,]$therm_1, na.rm=TRUE),mean(mydata2[L333,]$therm_1, na.rm=TRUE)), 
    Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                       "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                       "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
    PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
    
  )
  df203
  
  
  limits203 <- aes(ymax = Means + se, ymin=Means - se)
  
  
  w <- ggplot(df203, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits203)+ggtitle("Liberal Therm")+
    ylim(0,100)  + xlab("Ideology")+ylab("Level of Liberal Therm (0 = Low - 7 = High)")+
    theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))
  w 
  
  df204 <- data.frame(
    se =matrix(c(std.error(mydata[C3,]$repcontherm)*1.96,std.error(mydata[C2,]$repcontherm)*1.96,std.error(mydata[C1,]$repcontherm)*1.96,
                 std.error(mydata[M1,]$repcontherm)*1.96,std.error(mydata[L1,]$repcontherm)*1.96,std.error(mydata[L2,]$repcontherm)*1.96,
                 std.error(mydata[L3,]$repcontherm)*1.96,std.error(mydata[C33,]$repcontherm)*1.96,std.error(mydata[C22,]$repcontherm)*1.96,
                 std.error(mydata[C11,]$repcontherm)*1.96,std.error(mydata[M11,]$repcontherm)*1.96,std.error(mydata[L11,]$repcontherm)*1.96,
                 std.error(mydata[L22,]$repcontherm)*1.96,std.error(mydata[L33,]$repcontherm)*1.96,std.error(mydata2[C333,]$repcontherm)*1.96,std.error(mydata2[C222,]$repcontherm)*1.96,
                 std.error(mydata2[C111,]$repcontherm)*1.96,std.error(mydata2[M111,]$repcontherm)*1.96,std.error(mydata2[L111,]$repcontherm)*1.96,
                 std.error(mydata2[L222,]$repcontherm)*1.96,std.error(mydata2[L333,]$repcontherm)*1.96)),
    Means =c(mean(mydata[C3,]$repcontherm, na.rm=TRUE),mean(mydata[C2,]$repcontherm, na.rm=TRUE),mean(mydata[C1,]$repcontherm, na.rm=TRUE),
             mean(mydata[M1,]$repcontherm, na.rm=TRUE),mean(mydata[L1,]$repcontherm, na.rm=TRUE),mean(mydata[L2,]$repcontherm, na.rm=TRUE),
             mean(mydata[L3,]$repcontherm, na.rm=TRUE),mean(mydata[C33,]$repcontherm, na.rm=TRUE),mean(mydata[C22,]$repcontherm, na.rm=TRUE),
             mean(mydata[C11,]$repcontherm, na.rm=TRUE),mean(mydata[M11,]$repcontherm, na.rm=TRUE),mean(mydata[L11,]$repcontherm, na.rm=TRUE),
             mean(mydata[L22,]$repcontherm, na.rm=TRUE),mean(mydata[L33,]$repcontherm, na.rm=TRUE),mean(mydata2[C333,]$repcontherm, na.rm=TRUE),
             mean(mydata2[C222,]$repcontherm, na.rm=TRUE),mean(mydata2[C111,]$repcontherm, na.rm=TRUE),mean(mydata2[M111,]$repcontherm, na.rm=TRUE),
             mean(mydata2[L111,]$repcontherm, na.rm=TRUE),mean(mydata2[L222,]$repcontherm, na.rm=TRUE),mean(mydata2[L333,]$repcontherm, na.rm=TRUE)), 
    Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                       "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                       "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
    PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
    
  )
  df204
  
  
  limits204 <- aes(ymax = Means + se, ymin=Means - se)
  
  
  x <- ggplot(df204, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits204)+ggtitle("index con and rep Therm")+
    ylim(0,100)  + xlab("Ideology")+ylab("Level of index con and rep Therm (0 = Low - 7 = High)")+
    theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))
  x 
  
  df205 <- data.frame(
    se =matrix(c(std.error(mydata[C3,]$demlibtherm)*1.96,std.error(mydata[C2,]$demlibtherm)*1.96,std.error(mydata[C1,]$demlibtherm)*1.96,
                 std.error(mydata[M1,]$demlibtherm)*1.96,std.error(mydata[L1,]$demlibtherm)*1.96,std.error(mydata[L2,]$demlibtherm)*1.96,
                 std.error(mydata[L3,]$demlibtherm)*1.96,std.error(mydata[C33,]$demlibtherm)*1.96,std.error(mydata[C22,]$demlibtherm)*1.96,
                 std.error(mydata[C11,]$demlibtherm)*1.96,std.error(mydata[M11,]$demlibtherm)*1.96,std.error(mydata[L11,]$demlibtherm)*1.96,
                 std.error(mydata[L22,]$demlibtherm)*1.96,std.error(mydata[L33,]$demlibtherm)*1.96,std.error(mydata2[C333,]$demlibtherm)*1.96,std.error(mydata2[C222,]$demlibtherm)*1.96,
                 std.error(mydata2[C111,]$demlibtherm)*1.96,std.error(mydata2[M111,]$demlibtherm)*1.96,std.error(mydata2[L111,]$demlibtherm)*1.96,
                 std.error(mydata2[L222,]$demlibtherm)*1.96,std.error(mydata2[L333,]$demlibtherm)*1.96)),
    Means =c(mean(mydata[C3,]$demlibtherm, na.rm=TRUE),mean(mydata[C2,]$demlibtherm, na.rm=TRUE),mean(mydata[C1,]$demlibtherm, na.rm=TRUE),
             mean(mydata[M1,]$demlibtherm, na.rm=TRUE),mean(mydata[L1,]$demlibtherm, na.rm=TRUE),mean(mydata[L2,]$demlibtherm, na.rm=TRUE),
             mean(mydata[L3,]$demlibtherm, na.rm=TRUE),mean(mydata[C33,]$demlibtherm, na.rm=TRUE),mean(mydata[C22,]$demlibtherm, na.rm=TRUE),
             mean(mydata[C11,]$demlibtherm, na.rm=TRUE),mean(mydata[M11,]$demlibtherm, na.rm=TRUE),mean(mydata[L11,]$demlibtherm, na.rm=TRUE),
             mean(mydata[L22,]$demlibtherm, na.rm=TRUE),mean(mydata[L33,]$demlibtherm, na.rm=TRUE),mean(mydata2[C333,]$demlibtherm, na.rm=TRUE),
             mean(mydata2[C222,]$demlibtherm, na.rm=TRUE),mean(mydata2[C111,]$demlibtherm, na.rm=TRUE),mean(mydata2[M111,]$demlibtherm, na.rm=TRUE),
             mean(mydata2[L111,]$demlibtherm, na.rm=TRUE),mean(mydata2[L222,]$demlibtherm, na.rm=TRUE),mean(mydata2[L333,]$demlibtherm, na.rm=TRUE)), 
    Dataset = factor(c("FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012", "FTF ANES 2012",
                       "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", "Web ANES 2012", 
                       "Web ANES 2012", "Web ANES 2012","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk","MTurk")),
    PID = factor(c("Extremely conservative", "Conservative", "Slightly conservative", "Moderate", "Slightly liberal", "Liberal", "Extremely liberal"))
    
  )
  df205
  
  
  limits205 <- aes(ymax = Means + se, ymin=Means - se)
  
  
  y <- ggplot(df205, aes(linetype=Dataset, y=Means, x=PID))+ geom_line(aes(group=Dataset))+ theme(legend.position="none")  + geom_pointrange(limits205)+ggtitle("index lib and dem Therm")+
    ylim(0,100)  + xlab("Ideology")+ylab("Level of index lib and dem Therm (0 = Low - 7 = High)")+
    theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    scale_x_discrete(limits=c("Extremely liberal","Liberal","Slightly liberal", "Moderate","Slightly conservative", "Conservative", "Extremely conservative"))
  y 
  
