#######################################
# table 1: Willingness of vaccination #
#######################################





###-----load package
library(tidyverse)
library(openxlsx)
library(psych)
library(xtable)
##########Yahoo###########

###-------load data

cd<-read.csv("covid19_normal.csv")
attach(cd)



###------setting variables: Yahoo


#age
cd$age<-as.numeric(cd$Q2.2)
cd$age[cd$age==444]<-NA
cd$age[cd$age==19750618]<-NA
cd$age[cd$age<=19]<-NA
cd$age[cd$age>80]<-NA

cd$age[cd$age>=20 & cd$age<30]<-"20-29"
cd$age[cd$age>=30 & cd$age<40]<-"30-39"
cd$age[cd$age>=40 & cd$age<50]<-"40-49"
cd$age[cd$age>=50 & cd$age<60]<-"50-59"
cd$age[cd$age>=60 & cd$age<70]<-"60-69"
cd$age[cd$age>=70 & cd$age<=80]<-"70-79"


#income
cd$income<-as.numeric(cd$Q4.1_1)


#gender
cd$genderd<-as.numeric(cd$Q2.1)
cd$genderd[cd$gender==2]<-0
cd$genderd[cd$gender==3]<-NA
cd$genderd[cd$gender==4]<-NA

#education
cd$educationd<-cd$Q2.4
cd$educationd[cd$Q2.4==1]<-0
cd$educationd[cd$Q2.4==2]<-0
cd$educationd[cd$Q2.4==3]<-0
cd$educationd[cd$Q2.4==4]<-1
cd$educationd[cd$Q2.4==5]<-1
cd$educationd[cd$Q2.4==6]<-NA
cd$educationd[cd$Q2.4==7]<-NA



###-----simple descriptive analysis------###

#willingness of vaccination
Q21.1[Q21.1==4]<-NA
Q21.1[Q21.1==5]<-NA
cd$y<-Q21.1

#arrange table
ycspr0<-c("vaccination","wait and see","not vaccination")
ycspr<-round(prop.table(table(Q21.1)),3)
tb_ycs1<-rbind(ycspr0,ycspr)


###-------descriptive statistics by group-----###

#by age
ycspr_age<-round(prop.table(table(Q21.1,cd$age)),3)


#by gender
ycspr_gender<-round(prop.table(table(Q21.1,cd$genderd)),3)

#by education
ycspr_education<-round(prop.table(table(Q21.1,cd$educationd)),3)

#by gender and education
ycspr_cross<-round(prop.table(table(Q21.1,cd$educationd,cd$genderd)),3)



#########Lucid############


###-------load data

cdl<-read.csv("covid19_normal_lucid.csv")
attach(cdl)

###------setting variables: lucid


#agel
cdl$agel<-as.numeric(cdl$Q2.2)
cdl$agel[cdl$agel==3690306]<-NA
cdl$agel[cdl$agel==5770818]<-NA
cdl$agel[cdl$agel==97]<-NA
cdl$agel[cdl$agel==100]<-NA
cdl$agel[cdl$agel==2]<-NA
cdl$agel[cdl$agel==3]<-NA
cdl$agel[cdl$agel==5]<-NA
cdl$agel[cdl$agel==11]<-NA
cdl$agel[cdl$agel==12]<-NA
cdl$agel[cdl$agel==13]<-NA
cdl$agel[cdl$agel==14]<-NA
cdl$agel[cdl$agel==15]<-NA
cdl$agel[cdl$agel==16]<-NA
cdl$agel[cdl$agel==17]<-NA
cdl$agel[cdl$agel<=19]<-NA
cdl$agel[cdl$agel>80]<-NA
cdl$agel[cdl$agel>=20 & cdl$agel<30]<-"20-29"
cdl$agel[cdl$agel>=30 & cdl$agel<40]<-"30-39"
cdl$agel[cdl$agel>=40 & cdl$agel<50]<-"40-49"
cdl$agel[cdl$agel>=50 & cdl$agel<60]<-"50-59"
cdl$agel[cdl$agel>=60 & cdl$agel<70]<-"60-69"
cdl$agel[cdl$agel>=70 & cdl$agel<=80]<-"70-79"


#income
cdl$income<-as.numeric(cdl$Q4.1_1)


#genderl
cdl$genderdl<-as.numeric(cdl$Q2.1)
cdl$genderdl[cdl$genderdl==2]<-0
cdl$genderdl[cdl$genderdl==3]<-NA
cdl$genderdl[cdl$genderdl==4]<-NA

#educationl
cdl$educationld<-cdl$Q2.4
cdl$educationld[cdl$Q2.4==1]<-0
cdl$educationld[cdl$Q2.4==2]<-0
cdl$educationld[cdl$Q2.4==3]<-0
cdl$educationld[cdl$Q2.4==4]<-1
cdl$educationld[cdl$Q2.4==5]<-1
cdl$educationld[cdl$Q2.4==6]<-NA
cdl$educationld[cdl$Q2.4==7]<-NA





###-----simple descriptive analysis------###

#willingness of vaccination
cdl$Q12.1[cdl$Q12.1==4]<-NA
cdl$Q12.1[cdl$Q12.1==5]<-NA
l<-cdl$Q12.1

#arrange table
lucpr0<-c("vaccination","wait and see","not vaccination")
lucpr<-round(prop.table(table(cdl$Q12.1)),3)

#output to latex

tb_des_1<- rbind(tb_ycs1,lucpr)
xtable(tb_des_1)

#Test of Equal or Given Proportions 
prop.test(c(1170,425), c(2802,934), correct=F)
prop.test(c(1259,394), c(2802,934), correct=F)
prop.test(c(373,115), c(2802,934), correct=F)


###-------descriptive statistics by group-----###

#by agel
ycspr_agel<-round(prop.table(table(cdl$Q12.1,cdl$agel)),3)


#by gender
ycspr_gender<-round(prop.table(table(cdl$Q12.1,cdl$genderdl)),3)

#by education
ycspr_education<-round(prop.table(table(cdl$Q12.1,cdl$educationld)),3)

#by gender and education
ycspr_cross<-round(prop.table(table(cdl$Q12.1,cdl$educationld,cdl$genderdl)),3)


