#Replication files for "Expressive Power of Anti-Violence Legislation: Changes in Social Norms on Violence Against Women in 
#Mexico" by Mala Htun and Francesca R. Jensenius (World Politics 2022)
##Code to compare ENDIREH 2003, 2006, 2011, 2016 over time

#The data we draw on are created using the codes Code_ENDIREH2003_setupDta.R, Code_ENDIREH2006_setupDta.R, Code_ENDIREH2011_setupDta.R, Code_ENDIREH2016_setupDta.R

rm(list = ls())

library(foreign)
library(lme4)
library(Rmisc)
library(memisc)
library(grDevices)

setwd("") #Set to where you have saved the data folders

#################################################################################################
#Patterns in 2003 data
#################################################################################################

dta2003<-read.csv("ENDIREH2003_ours.csv")
dta2003$NIV_small<-factor(dta2003$NIV_small, levels=unique(dta2003$NIV_small)[c(5, 1, 2, 3, 4, 7)]) #Reorder education levels
table(dta2003$NIV_small)

#Create generation variable
dta2003$yearofbirth<-2003-dta2003$EDAD

#Explore main outcome variables

#Domestic abuse in past year, all comparable types of violence (VIII. RELACIÓN ACTUAL)
names(dta2003)
inx<-c(91,  95,  99, 103, 107, 111, 115, 119, 124, 125, 126,  21, 53, 41, 45, 29, 37, 57, 61, 25, 17, 49, 33, 86, 78, 74, 70, 66)  #Begins with EEMJP1, every fourth, and the last three are EEXSE1, EOBSE2, EFUSE3, these are all the questions in the categories 5 (maltrato físico) ó 7 (maltrato sexual). Then add in other common ones followng the order in the 2016 survey.

table(dta2003[,inx][1])
names(dta2003)[inx]

dta2003$domViol <-rowSums(dta2003[,inx]==1, na.rm=T) ##Include only affirmative answer
#dta2003$domViol<-rowSums(dta2003[,inx]==1 | dta2003[,inx]==3 | dta2003[,inx]==9, na.rm=T) ##Includes no answer in violence as well

dta2003$domViol_dummy <-ifelse(dta2003$domViol>0, 1, 0)

table(dta2003$domViol_dummy)
prop.table(table(dta2003$domViol_dummy))

#physical domestic abuse in past year (VIII. RELACIÓN ACTUAL)
names(dta2003)
inx<-c(91,  95,  99, 103, 107, 111, 115, 119, 124, 125, 126) #Begins with EEMJP1, every fourth, and the last three are EEXSE1, EOBSE2, EFUSE3, these are all the questions in the categories 5 (maltrato físico) ó 7 (maltrato sexual).

table(dta2003[,inx][1])

dta2003$domViol_phys<-rowSums(dta2003[,inx]==1, na.rm=T) ##Here we only include affirmative answer, NA is included in the denominator
#dta2003$domViol_phys<-rowSums(dta2003[,inx]==1 | dta2003[,inx]==3 | dta2003[,inx]==9, na.rm=T) ##Try to include no answer in violence as well

dta2003$domViol_phys_dummy <-ifelse(dta2003$domViol_phys>0, 1, 0)

table(dta2003$domViol_phys_dummy)
prop.table(table(dta2003$domViol_phys_dummy))

#Reported of incidence (the survey covers physical abuse only)
inx<-grep("ENCONF", names(dta2003))

dta2003$domViol_react<-rowSums(dta2003[,inx]==1 | dta2003[,inx]==2 | dta2003[,inx]==3, na.rm=T)
dta2003$domViol_react <-ifelse(dta2003$domViol_phys_dummy==0, NA, dta2003$domViol_react) #Only look at the women who experienced abuse in previous year
dta2003$domViol_react_dummy<-ifelse(dta2003$domViol_react!=0, 1, dta2003$domViol_react)

table(dta2003$domViol_react_dummy)
prop.table(table(dta2003$domViol_react_dummy))

#Reported of incidence in 2003
table(dta2003$ULA.U.00D1.O)

dta2003$ULA.U.00D1.O <-ifelse(is.na(dta2003$ULA.U.00D1.O), 9999, dta2003$ULA.U.00D1.O)

dta2003$domViol_react_dummy_lastyear<-ifelse(dta2003$ULA.U.00D1.O==2003, 1, 0)
dta2003$domViol_react_dummy_lastyear<-ifelse(is.na(dta2003$domViol_react_dummy), NA, dta2003$domViol_react_dummy_lastyear)

table(dta2003$domViol_react_dummy_lastyear)
prop.table(table(dta2003$domViol_react_dummy_lastyear))

##Talk to family/friends
inx<-grep("PLATIC", names(dta2003))
dta2003$familiares<- ifelse(is.na(dta2003$PLATIC1) & rowSums(dta2003[,inx]>0, na.rm=T)>0, 0, dta2003$PLATIC1)
dta2003$familiares<- ifelse(dta2003$domViol_phys_dummy==0, NA, dta2003$familiares)
prop.table(table(dta2003$domViol_phys_dummy, dta2003$familiares))

dta2003$amigos<- ifelse(is.na(dta2003$PLATIC2) & rowSums(dta2003[,inx]>0, na.rm=T)>0, 0, dta2003$PLATIC2)
dta2003$amigos <- ifelse(dta2003$domViol_phys_dummy==0, NA, dta2003$amigos)
dta2003$amigos<-ifelse(dta2003$amigos==2, 1, dta2003$amigos)
prop.table(table(dta2003$domViol_phys_dummy, dta2003$amigos))

dta2003$nadie<- ifelse(is.na(dta2003$PLATIC7) & rowSums(dta2003[,inx]>0, na.rm=T)>0, 0, dta2003$PLATIC7)
dta2003$nadie <- ifelse(dta2003$domViol_phys_dummy==0, NA, dta2003$nadie)
dta2003$nadie <-ifelse(dta2003$nadie ==7, 1, dta2003$nadie)
prop.table(table(dta2003$domViol_phys_dummy, dta2003$nadie))

#Summary statistics matrix (2003 part)
mymatrix<-matrix(nrow=7, ncol=4)
colnames(mymatrix)<-c("2003 women", "2006 women", "2011 women", "2016 women")
row.names(mymatrix)<-c("Education level (1-6)", "Age", "Working (%)", "Indigenous (%)", "Non-married (%)", "Urban (%)", "Number of women")
mymatrix[1, 1]<-mean(as.numeric(dta2003$NIV_small), na.rm=T)
mymatrix[2, 1]<-mean(dta2003$EDAD,na.rm=T)
mymatrix[3, 1]<-100*(mean(dta2003$worked,na.rm=T))
mymatrix[4, 1]<-100*(mean(dta2003$indigena,na.rm=T))
mymatrix[5, 1]<-100*(mean(dta2003$CivStatus=="Unida",na.rm=T))
mymatrix[7, 1]<-dim(dta2003)[1]


##Gender roles
#Wife should obey husband
table(dta2003$OBEDECER)
prop.table(table(dta2003$OBEDECER))
dta2003$obey<-ifelse(dta2003$OBEDECER==1, 1, 0)
prop.table(table(dta2003$obey)) #41% say yes, 15%depends

#Husband has right to hit her
table(dta2003$NOCUMPLE)
prop.table(table(dta2003$NOCUMPLE))
dta2003$right_to_hit<-ifelse(dta2003$NOCUMPLE ==1, 1, 0)
prop.table(table(dta2003$right_to_hit))  #90% say no

#Man is in charge of gastos
names(dta2003)
prop.table(table(dta2003$RESPGAST))
dta2003$gastos<-ifelse(dta2003$RESPGAST==1, 1, 0)
prop.table(table(dta2003$gastos))  #68% say yes


#################################################################################################
#Patterns of VAW and reporting in 2006 data
#################################################################################################

dta2006<-read.csv("ENDIREH2006_ours.csv")
dta2006$NIV_small<-factor(dta2006$NIV_small, levels=unique(dta2006$NIV_small)[c(5, 1, 2, 3, 4, 6)]) #Reorder education variable
table(dta2006$NIV_small)

#Create generation variable
dta2006$yearofbirth<-2006-dta2006$EDAD
table(dta2006$yearofbirth)

#Explore outcome variables

#Experiences with domestic abuse during past year
allviol<-grep("P7_4_", names(dta2006))

inx<-allviol[c(1:15, 17:25, 27, 28:30)] #all comparable types of abuse
length(inx)

table(dta2006[,inx][5])
dta2006$domViol<-rowSums(dta2006[,inx]==1 | dta2006[,inx]==2 | dta2006[,inx]==8 | dta2006[,inx]==9, na.rm=T) ##Include no answer
dta2006$domViol <-rowSums(dta2006[,inx]==1 | dta2006[,inx]==2, na.rm=T) ##Include only affirmative answer
dta2006$domViol_dummy <-ifelse(dta2006$domViol>0, 1, 0)
table(dta2006$domViol_dummy)
prop.table(table(dta2006$domViol_dummy))

inx<-allviol[c(1:8, 28:30)] #Only physical domestic abuse

table(dta2006[,inx][5])
#dta2006$domViol_phys<-rowSums(dta2006[,inx]==1 | dta2006[,inx]==2 | dta2006[,inx]==8 | dta2006[,inx]==9, na.rm=T) ##Include no answer
dta2006$domViol_phys<-rowSums(dta2006[,inx]==1 | dta2006[,inx]==2, na.rm=T) ##Include only affirmative answer
dta2006$domViol_phys_dummy <-ifelse(dta2006$domViol_phys>0, 1, 0)
table(dta2006$domViol_phys_dummy)
prop.table(table(dta2006$domViol_phys_dummy))

#reported physical domestic violation  
inx<-grep("P7_7_", names(dta2006))

table(dta2006[,inx[1]])
table(dta2006[,inx[2]])
table(dta2006[,inx[3]])
table(dta2006[,inx[4]])

dta2006$domViol_react<-rowSums(dta2006[,inx]==1 | dta2006[,inx]==2 | dta2006[,inx]==3, na.rm=T)
dta2006$domViol_react<-ifelse(rowSums(dta2006[,inx]==8 | dta2006[,inx]==9, na.rm=T), NA, dta2006$domViol_react)
dta2006$domViol_react<-ifelse(dta2006$domViol_phys_dummy==0, NA, dta2006$domViol_react) #Exclude those who had not experienced domestic abuse in previous year
dta2006$domViol_react_dummy<-ifelse(dta2006$domViol_react!=0, 1, dta2006$domViol_react)

table(dta2006$domViol_react_dummy, dta2006$domViol_phys_dummy)
prop.table(table(dta2006$domViol_react_dummy))

#reported physical domestic violation in 2006
table(dta2006$P7_8_2) #Year

dta2006$P7_8_2<-ifelse(is.na(dta2006$P7_8_2), 9998, dta2006$P7_8_2)

dta2006$domViol_react_dummy_lastyear<-ifelse(dta2006$P7_8_2==2006, 1, 0)
dta2006$domViol_react_dummy_lastyear<-ifelse(is.na(dta2006$domViol_react_dummy), NA, dta2006$domViol_react_dummy_lastyear)

table(dta2006$domViol_react_dummy_lastyear)
prop.table(table(dta2006$domViol_react_dummy_lastyear))

##Tell family/friends
table(dta2006$P7_16_1)

inx<-grep("P7_16", names(dta2006))
dta2006$familiares<- ifelse(is.na(dta2006$P7_16_1) & rowSums(dta2006[,inx]>0, na.rm=T)>0, 0, dta2006$P7_16_1)
dta2006$familiares<-ifelse(dta2006$familiares==9, 0, dta2006$familiares)
dta2006$familiares<- ifelse(dta2006$domViol_phys_dummy==0, NA, dta2006$familiares)
prop.table(table(dta2006$domViol_phys_dummy, dta2006$familiares))

table(dta2006$P7_16_2)
dta2006$amigos<- ifelse(is.na(dta2006$P7_16_2) & rowSums(dta2006[,inx]>0, na.rm=T)>0, 0, dta2006$P7_16_2)
dta2006$amigos <- ifelse(dta2006$domViol_phys_dummy==0, NA, dta2006$amigos)
dta2006$amigos<-ifelse(dta2006$amigos==2, 1, dta2006$amigos)
prop.table(table(dta2006$domViol_phys_dummy, dta2006$amigos))

table(dta2006$P7_16_8)
dta2006$nadie<- ifelse(is.na(dta2006$P7_16_8) & rowSums(dta2006[,inx]>0, na.rm=T)>0, 0, dta2006$P7_16_8)
dta2006$nadie <- ifelse(dta2006$domViol_phys_dummy==0, NA, dta2006$nadie)
dta2006$nadie <-ifelse(dta2006$nadie ==8, 1, dta2006$nadie)
prop.table(table(dta2006$domViol_phys_dummy, dta2006$nadie))

#Summary statistics matrix (2006 part)
mymatrix[1, 2]<-mean(as.numeric(dta2006$NIV_small), na.rm=T)
mymatrix[2, 2]<-mean(dta2006$EDAD,na.rm=T)
mymatrix[3, 2]<-100*(mean(dta2006$worked,na.rm=T))
mymatrix[4, 2]<-100*(mean(dta2006$indigena,na.rm=T))
mymatrix[5, 2]<-100*(mean(dta2006$CivStatus=="Unida",na.rm=T))
mymatrix[6, 2]<-100*(mean(dta2006$DOM=="U",na.rm=T))
mymatrix[7, 2]<-dim(dta2006)[1]

#Why not complain
inx<-grep("P7_12_", names(dta2006))

por_miedo<-rowSums(dta2006[,inx]==1, na.rm=T)
por_emanazas<-rowSums(dta2006[,inx]==2, na.rm=T)
por_hijos<-rowSums(dta2006[,inx]==3, na.rm=T)
por_familia<-rowSums(dta2006[,inx]==4, na.rm=T)
por_verguenza<-rowSums(dta2006[,inx]==5, na.rm=T)
por_familia2<-rowSums(dta2006[,inx]==6, na.rm=T)
nosabia<-rowSums(dta2006[,inx]==7, na.rm=T)
noimporta<-rowSums(dta2006[,inx]==8, na.rm=T)
por_derecho<-rowSums(dta2006[,inx]==9, na.rm=T)
noconfio<-rowSums(dta2006[,inx]==10, na.rm=T)
nocambia<-rowSums(dta2006[,inx]==11, na.rm=T)
otro<-rowSums(dta2006[,inx]==12, na.rm=T)

dta2006$noimporta<-ifelse(dta2006$domViol_phys_dummy==0, NA, noimporta)
prop.table(table(dta2006$noimporta))

#Out of violated women
table(por_miedo, dta2006$domViol_react_dummy_lastyear)
table(noimporta, dta2006$domViol_react_dummy_lastyear)

razones06<-cbind(table(dta2006$domViol_react_dummy_lastyear), 
table(por_miedo[!is.na(dta2006$domViol_react_dummy_lastyear)]), 
#table(por_emanazas[!is.na(dta2006$domViol_react_dummy)]),
table(por_hijos[!is.na(dta2006$domViol_react_dummy_lastyear)]),
#table(por_familia[!is.na(dta2006$domViol_react_dummy)]),
table(por_verguenza[!is.na(dta2006$domViol_react_dummy_lastyear)]),
table(por_familia2[!is.na(dta2006$domViol_react_dummy_lastyear)]),
#table(nosabia[!is.na(dta2006$domViol_react_dummy_lastyear)]),
table(noimporta[!is.na(dta2006$domViol_react_dummy_lastyear)]),
#table(por_derecho[!is.na(dta2006$domViol_react_dummy)]),
table(noconfio[!is.na(dta2006$domViol_react_dummy_lastyear)]),
table(nocambia[!is.na(dta2006$domViol_react_dummy_lastyear)]),
table(otro[!is.na(dta2006$domViol_react_dummy_lastyear)]))

prop.table(razones06,2)[2,]

#Gender roles
#Wife should obey husband
table(dta2006$P11_1_1)
dta2006$obey<-ifelse(dta2006$P11_1_1==1, 1, 0)
prop.table(table(dta2006$obey)) #35% say yes

#Husband has right to hit her
table(dta2006$P11_1_6)
dta2006$right_to_hit<-ifelse(dta2006$P11_1_6 ==1, 1, 0)
prop.table(table(dta2006$right_to_hit))  #96% say no

#Man should be in charge of gastos
table(dta2006$P11_1_3)
dta2006$gastos<-ifelse(dta2006$P11_1_3==1, 1, 0)
prop.table(table(dta2006$gastos)) #66% say no

#################################################################################################
###Setting up 2011 data
#################################################################################################

dta2011<-read.csv("ENDIREH2011_ours.csv")
dta2011$NIV_small<-factor(dta2011$NIV_small, levels=unique(dta2011$NIV_small)[c(5, 2, 4, 1, 3, 6)])
table(dta2011$NIV_small)

#Create generation variable
dta2011$yearofbirth<-2011-dta2011$EDAD
table(dta2011$yearofbirt)

#Explore outcome variables

#Experiences with domestic abuse during past year

allviol<-grep("AP6_3_", names(dta2011))

inx<-allviol[c(1:7, 9:17, 19, 20:30)] 
length(inx)

dta2011$domViol<-rowSums(dta2011[,inx]==1 | dta2011[,inx]==2 | dta2011[,inx]==3 | dta2011[,inx]==9, na.rm=T) ##Include no answer
dta2011$domViol <-rowSums(dta2011[,inx]==1 | dta2011[,inx]==2 | dta2011[,inx]==3, na.rm=T) ##Include only affirmative answer
dta2011$domViol_dummy <-ifelse(dta2011$domViol>0, 1, 0)

table(dta2011$domViol_dummy)
prop.table(table(dta2011$domViol_dummy))


inx<-allviol[c(20:30)] #Only physical abuse

table(dta2011[,inx][1])
#dta2011$domViol_phys<-rowSums(dta2011[,inx]==1 | dta2011[,inx]==2 | dta2011[,inx]==3 | dta2011[,inx]==9, na.rm=T) ##Include no answer
dta2011$domViol_phys<-rowSums(dta2011[,inx]==1 | dta2011[,inx]==2 | dta2011[,inx]==3, na.rm=T) ##Include only affirmative answer
dta2011$domViol_phys_dummy <-ifelse(dta2011$domViol_phys>0, 1, 0)

table(dta2011$domViol_phys_dummy)
prop.table(table(dta2011$domViol_phys_dummy))

#reported physical domestic violation 
inx<-grep("AP6_5_", names(dta2011))

table(dta2011[,inx[1]])
table(dta2011[,inx[2]])
table(dta2011[,inx[3]])
table(dta2011[,inx[4]])
table(dta2011[,inx[5]])
table(dta2011[,inx[6]])

dta2011$domViol_react<-rowSums(dta2011[,inx]==1, na.rm=T)
dta2011$domViol_react<-ifelse(dta2011$domViol_phys_dummy==0, NA, dta2011$domViol_react)
dta2011$domViol_react_dummy<-ifelse(dta2011$domViol_react==0, 0, 1)
table(dta2011$domViol_react_dummy, dta2011$domViol_phys_dummy)

table(dta2011$domViol_react_dummy)
prop.table(table(dta2011$domViol_react_dummy))

#reported physical domestic violation in 2011
inx<-grep("AP6_7", names(dta2011))

table(dta2011[,inx[2]])
table(dta2011[,inx[4]])
table(dta2011[,inx[6]])
table(dta2011[,inx[8]])
table(dta2011[,inx[10]])
table(dta2011[,inx[12]])

dta2011$domViol_react_dummy_lastyear<-rowSums(dta2011[,inx]=="2011", na.rm=T)
dta2011$domViol_react_dummy_lastyear<-ifelse(dta2011$domViol_react_dummy_lastyear>0, 1, 0)
dta2011$domViol_react_dummy_lastyear <-ifelse(is.na(dta2011$domViol_react_dummy), NA, dta2011$domViol_react_dummy_lastyear)

table(dta2011$domViol_react_dummy_lastyear)
prop.table(table(dta2011$domViol_react_dummy_lastyear))

##Talk to family/friends
table(dta2011$AP6_15_1, dta2011$domViol_phys_dummy)

inx<-grep("AP6_15", names(dta2011))
dta2011$familiares<- ifelse(is.na(dta2011$AP6_15_1) & rowSums(dta2011[,inx]>0, na.rm=T)>0, 0, dta2011$AP6_15_1)
dta2011$familiares<- ifelse(dta2011$domViol_phys_dummy==0, NA, dta2011$familiares)
dta2011$familiares<- ifelse(dta2011$familiares==9, 0, dta2011$familiares)
prop.table(table(dta2011$domViol_phys_dummy, dta2011$familiares))

dta2011$amigos<- ifelse(is.na(dta2011$AP6_15_2) & rowSums(dta2011[,inx]>0, na.rm=T)>0, 0, dta2011$AP6_15_2)
dta2011$amigos <- ifelse(dta2011$domViol_phys_dummy==0, NA, dta2011$amigos)
dta2011$amigos<-ifelse(dta2011$amigos==2, 1, dta2011$amigos)
prop.table(table(dta2011$domViol_phys_dummy, dta2011$amigos))

table(dta2011$AP6_15_7)
dta2011$nadie<- ifelse(is.na(dta2011$AP6_15_7) & rowSums(dta2011[,inx]>0, na.rm=T)>0, 0, dta2011$AP6_15_7)
dta2011$nadie <- ifelse(dta2011$domViol_phys_dummy==0, NA, dta2011$nadie)
dta2011$nadie <-ifelse(dta2011$nadie ==7, 1, dta2011$nadie)
prop.table(table(dta2011$domViol_phys_dummy, dta2011$nadie))

#Summary statistics matrix (2011 part)
mymatrix[1, 3]<-mean(as.numeric(dta2011$NIV_small), na.rm=T)
mymatrix[2, 3]<-mean(dta2011$EDAD,na.rm=T)
mymatrix[3, 3]<-100*(mean(dta2011$worked,na.rm=T))
mymatrix[4, 3]<-100*(mean(dta2011$indigena,na.rm=T))
mymatrix[5, 3]<-100*(mean(dta2011$CivStatus=="Unida",na.rm=T))
mymatrix[6, 3]<-100*(mean(dta2011$DOMINIO=="U",na.rm=T))
mymatrix[7, 3]<-dim(dta2011)[1]

#Why not complain
inx<-grep("AP6_11_", names(dta2011))

por_miedo<-rowSums(dta2011[,inx]==1, na.rm=T)
por_emanazas<-rowSums(dta2011[,inx]==2, na.rm=T)
por_hijos<-rowSums(dta2011[,inx]==3, na.rm=T)
por_familia<-rowSums(dta2011[,inx]==4, na.rm=T)
por_cambio<-rowSums(dta2011[,inx]==5, na.rm=T)
por_verguenza<-rowSums(dta2011[,inx]==6, na.rm=T)
por_familia2<-rowSums(dta2011[,inx]==7, na.rm=T)
nosabia<-rowSums(dta2011[,inx]==8, na.rm=T)
noimporta<-rowSums(dta2011[,inx]==9, na.rm=T)
por_derecho<-rowSums(dta2011[,inx]==10, na.rm=T)
domViol_noconfio<-rowSums(dta2011[,inx]==11, na.rm=T)
domViol_otro<-rowSums(dta2011[,inx]==13, na.rm=T)

dta2011$noimporta<-ifelse(dta2011$domViol_phys_dummy==0, NA, noimporta)
prop.table(table(dta2011$noimporta))

##Percentages out of those violated
table(dta2011$domViol_phys_dummy, dta2011$domViol_react_dummy_lastyear)

razones11<-cbind(table(dta2011$domViol_react_dummy_lastyear), 
table(por_miedo[dta2011$domViol_phys_dummy ==1]), 
#table(por_emanazas[dta2011$domViol_phys_dummy ==1]), 
table(por_hijos[dta2011$domViol_phys_dummy ==1]),
#table(por_familia[dta2011$domViol_phys_dummy ==1]),
table(por_verguenza[dta2011$domViol_phys_dummy ==1]),
table(por_familia2[dta2011$domViol_phys_dummy ==1]),
#table(nosabia[dta2011$domViol_phys_dummy ==1]),
table(noimporta[dta2011$domViol_phys_dummy ==1]),
#table(por_derecho[dta2011$domViol_phys_dummy ==1]),
table(domViol_noconfio[dta2011$domViol_phys_dummy ==1]),
table(por_cambio[dta2011$domViol_phys_dummy ==1]),
table(domViol_otro[dta2011$domViol_phys_dummy ==1]))

prop.table(razones11,2)[2,]

##Gender roles 
#Wife should obey husband
table(dta2011$AP10_1_1)
dta2011$obey<-ifelse(dta2011$AP10_1_1==1,1,0)
prop.table(table(dta2011$obey)) #23% say yes

#¿Es obligación de la mujer tener relaciones sexuales con su esposo o pareja?
table(dta2011$AP10_1_5)
dta2011$sex<-ifelse(dta2011$AP10_1_5 ==1,1,0)
prop.table(table(dta2011$sex)) #81% say no

#Husband has right to hit her
table(dta2011$AP10_1_7)
dta2011$right_to_hit <-ifelse(dta2011$AP10_1_7 ==1,1,0)
prop.table(table(dta2011$right_to_hit))  #98% say no

#Man should be in charge of the gastos
table(dta2011$AP10_1_3)
dta2011$gastos<-ifelse(dta2011$AP10_1_3 ==1,1,0)
prop.table(table(dta2011$gastos)) #66% say yes

#Hitting is private matter
table(dta2011$AP10_1_10)
dta2011$private <-ifelse(dta2011$AP10_1_10 ==1,1,0)
prop.table(table(dta2011$private))  #27% say yes

#10_3_1 Conoce la ley de 2007?
table(dta2011$AP10_3_2)
dta2011$ley_viol <-ifelse(dta2011$AP10_3_2 ==1,1,0)
table(dta2011$ley_viol)
prop.table(table(dta2011$ley_viol))  #84% say yes

tapply(dta2011$ley_viol, dta2011$estado, summary)

##All our main outcomes by whether or not you know the law
tapply(dta2011$domViol_dummy,  dta2011$ley_viol, mean) 
tapply(dta2011$domViol_phys_dummy,  dta2011$ley_viol, mean) 
tapply(dta2011$domViol_react_dummy_lastyear,  dta2011$ley_viol, mean, na.rm=T) 
tapply(dta2011$noimporta,  dta2011$ley_viol, mean, na.rm=T) 
tapply(dta2011$obey,  dta2011$ley_viol, mean, na.rm=T) 
tapply(dta2011$sex,  dta2011$ley_viol, mean, na.rm=T) 
tapply(dta2011$right_to_hit,  dta2011$ley_viol, mean, na.rm=T) 
tapply(dta2011$gastos,  dta2011$ley_viol, mean, na.rm=T) 
tapply(dta2011$private,  dta2011$ley_viol, mean, na.rm=T) 

tapply(dta2011$ley_viol, dta2011$estado, summary)
tapply(dta2011$ley_viol, dta2011$age, summary)
tapply(dta2011$ley_viol, dta2011$NIV_small, summary)
tapply(dta2011$ley_viol, dta2011$worked, summary)
tapply(dta2011$ley_viol, dta2011$DOMINIO, summary)
tapply(dta2011$ley_viol, dta2011$CivStatus, summary)
tapply(dta2011$ley_viol, dta2011$indigena, summary)

##TABLE B.2
mlm1<-lmer(domViol_dummy ~   ley_viol + as.numeric(NIV_small)+worked + CivStatus + EDAD + indigena + DOMINIO+ (1| estado), data= dta2011) 
mlm2<-lmer(domViol_react_dummy_lastyear ~  ley_viol+  as.numeric(NIV_small)+worked + CivStatus + EDAD + indigena + DOMINIO + (1| estado), data= dta2011) 
mlm3<-lmer(right_to_hit ~ ley_viol+  as.numeric(NIV_small)+worked + CivStatus + EDAD + indigena + DOMINIO + (1| estado), data=dta2011)
mlm4<-lmer(obey ~ ley_viol+  as.numeric(NIV_small)+worked + CivStatus + EDAD + indigena + DOMINIO + (1| estado), data= dta2011) 

summary(mlm1)
summary(mlm2)
summary(mlm3)
summary(mlm4)

table_ley <- mtable(mlm1, mlm2, mlm3, mlm4, summary.stats=T,digits=3)
cat(toLatex(table_ley), file = "table_b2.txt")

#################################################################################################
###Setting up 2016 data
#################################################################################################

dta2016<-read.csv("ENDIREH2016_ours.csv")
dta2016$NIV_small<-factor(dta2016$NIV_small, levels=unique(dta2016$NIV_small)[c(5, 3, 4, 1, 2, 6)])
table(dta2016$NIV_small)

#Create generation variable
dta2016$yearofbirth<-2016-dta2016$EDAD
table(dta2016$yearofbirth)

#Explore main outcome
#Experiences with physical domestic abuse during past year
allviol <-grep("P13_3_", names(dta2016))

inx<-allviol[c(1, 3:9, 10:15, 18:21, 23:27, 30, 33:36)] #all comparable forms of violence
length(inx)

table(dta2016[,inx][1])
#dta2016$domViol <-rowSums(dta2016[,inx]==1 | dta2016[,inx]==2 | dta2016[,inx]==3 | dta2016[,inx]==9, na.rm=T) ##Include no answer
dta2016$domViol <-rowSums(dta2016[,inx]==1 | dta2016[,inx]==2 | dta2016[,inx]==3, na.rm=T) ##Include only affirmative answer
dta2016$domViol_dummy <-ifelse(dta2016$domViol>0, 1, 0)

table(dta2016$domViol_dummy)
prop.table(table(dta2016$domViol_dummy))

inx<-allviol[c(1, 3:9, 25:27)] #Only physical abuse

table(dta2016[,inx][1])
dta2016$domViol_phys<-rowSums(dta2016[,inx]==1 | dta2016[,inx]==2 | dta2016[,inx]==3, na.rm=T) ##Include only affirmative answer
dta2016$domViol_phys_dummy <-ifelse(dta2016$domViol_phys>0, 1, 0)

table(dta2016$domViol_phys_dummy)
prop.table(table(dta2016$domViol_phys_dummy))

table(dta2016$domViol_phys_dummy)

#knowing where to report
table(dta2016$P13_6)
prop.table(table(dta2016$P13_6))
table(dta2016$P13_6,  dta2016$domViol_phys_dummy)
prop.table(table(dta2016$P13_6,  dta2016$domViol_phys_dummy), 2) #35% of the violated knew where to report

#reported physical domestic violation 
inx<-grep("P13_8", names(dta2016))

dta2016$domViol_react<-rowSums(dta2016[,inx]==1, na.rm=T)
dta2016$domViol_react<-ifelse(dta2016$domViol_phys_dummy==0 | dta2016$P13_7_1==9, NA, dta2016$domViol_react) 
dta2016$domViol_react_dummy<-ifelse(dta2016$domViol_react==0, 0, 1)
table(dta2016$domViol_react_dummy, dta2016$domViol_phys_dummy)

table(dta2016$domViol_react_dummy)
prop.table(table(dta2016$domViol_react_dummy))

#reported physical domestic violation 2015-16 
inx<-grep("P13_10", names(dta2016))
table(dta2016[,inx[1]])
dta2016$domViol_react_dummy_lastyear <-rowSums(dta2016[,inx]==6, na.rm=T)
dta2016$domViol_react_dummy_lastyear <-ifelse(dta2016$domViol_phys_dummy ==0, NA, dta2016$domViol_react_dummy_lastyear) 
dta2016$domViol_react_dummy_lastyear <-ifelse(dta2016$domViol_react_dummy_lastyear!=0, 1, dta2016$domViol_react_dummy_lastyear)

table(dta2016$domViol_react_dummy_lastyear)
prop.table(table(dta2016$domViol_react_dummy_lastyear))

#Usted notifico o presento una queja
inx<-grep("P13_14", names(dta2016))
dta2016$domViol_complain_lastyear<-rowSums(dta2016[,inx]==6, na.rm=T)
dta2016$domViol_complain_lastyear <-ifelse(dta2016$domViol_phys_dummy==0 | dta2016$P13_7_2==9, NA, dta2016$domViol_complain_lastyear)
dta2016$domViol_complain_lastyear_dummy<-ifelse(dta2016$domViol_complain_lastyear!=0, 1, dta2016$domViol_complain_lastyear)

table(dta2016$domViol_complain_lastyear_dummy)
prop.table(table(dta2016$domViol_complain_lastyear_dummy))

##Adding the two types of reporting
dta2016$anyreporting_lastyear<-ifelse(dta2016$domViol_react_dummy_lastyear ==1 | dta2016$domViol_complain_lastyear_dummy ==1, 1, 0)
table(dta2016$anyreporting_lastyear, dta2016$domViol_phys_dummy)
prop.table(table(dta2016$anyreporting_lastyear))

##Talk to family/friends
table(dta2016$P13_33AB_1, dta2016$domViol_phys_dummy)

dta2016$familiares<- ifelse(dta2016$P13_33AB_1==9, 0, dta2016$P13_33AB_1)
dta2016$familiares<- ifelse(dta2016$domViol_phys_dummy==0, NA, dta2016$familiares)
table(dta2016$domViol_phys_dummy, dta2016$familiares)
prop.table(table(dta2016$domViol_phys_dummy, dta2016$familiares))

table(dta2016$P13_33AB_2)
dta2016$amigos<- ifelse(dta2016$P13_33AB_2==9, 0, dta2016$P13_33AB_2)
dta2016$amigos<- ifelse(dta2016$domViol_phys_dummy==0, NA, dta2016$amigos)
table(dta2016$domViol_phys_dummy, dta2016$amigos)
prop.table(table(dta2016$domViol_phys_dummy, dta2016$amigos))

table(dta2016$P13_33AB_7)
dta2016$nadie<- ifelse(dta2016$P13_33AB_7==9, 0, dta2016$P13_33AB_7)
dta2016$nadie<- ifelse(dta2016$domViol_phys_dummy==0, NA, dta2016$nadie)
table(dta2016$domViol_phys_dummy, dta2016$nadie)
prop.table(table(dta2016$domViol_phys_dummy, dta2016$nadie))

###Summary statistics matrix:
mymatrix[1, 4]<-mean(as.numeric(dta2016$NIV_small), na.rm=T)
mymatrix[2, 4]<-mean(dta2016$EDAD,na.rm=T)
mymatrix[3, 4]<-100*mean(dta2016$worked,na.rm=T)
mymatrix[4, 4]<-100*mean(dta2016$indigena,na.rm=T)
mymatrix[5, 4]<-100*mean(dta2016$CivStatus=="Unida",na.rm=T)
mymatrix[6, 4]<-100*mean(dta2016$DOM=="U",na.rm=T)
mymatrix[7, 4]<-dim(dta2016)[1]

library(xtable)
cat(print(xtable(mymatrix, dig=1)), file="table_a1.txt")

##########################################################
##SHOW PATTERNS OVER TIME
##########################################################

##Plot domestic abuse in previous year 2003-16
myfigure<-matrix(ncol=3, nrow=4)

myfigure[1,]<-100*CI(dta2003$domViol_dummy)
myfigure[2,]<-100*CI(dta2006$domViol_dummy)
myfigure[3,]<-100*CI(dta2011$domViol_dummy)
myfigure[4,]<-100*CI(dta2016$domViol_dummy)

colnames(myfigure)<-names(CI(dta2003$domViol_dummy))
row.names(myfigure)<-c("2003", "2006", "2011", "2016")

pdf(file="Figure_1.pdf", width=5, height=3.5, pointsize=10)
par(mar=c(4, 4, 2, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 50), names.arg=row.names(myfigure), las=1, ylab="Percentage", xlab="Year of survey")
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

tiff(file="Figure_1.tiff", width=5, height=3.5, units= "in", res=300, pointsize=10)
par(mar=c(4, 4, 2, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 50), names.arg=row.names(myfigure), las=1, ylab="Percentage", xlab="Year of survey")
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

##Change in abuse 2003-16, by education level
myplot16<-100*unlist(tapply(dta2016$domViol_dummy, dta2016$NIV_small, CI))[seq(2, 18, 3)]
myplot03<-100*unlist(tapply(dta2003$domViol_dummy, dta2003$NIV_small, CI))[seq(2, 18, 3)]

CI_low16<-100*unlist(tapply(dta2016$domViol_dummy, dta2016$NIV_small, CI))[seq(3, 18, 3)]
CI_low03<-100*unlist(tapply(dta2003$domViol_dummy, dta2003$NIV_small, CI))[seq(3, 18, 3)]

CI_high16<-100*unlist(tapply(dta2016$domViol_dummy, dta2016$NIV_small, CI))[seq(1, 18, 3)]
CI_high03<-100*unlist(tapply(dta2003$domViol_dummy, dta2003$NIV_small, CI))[seq(1, 18, 3)]

pdf("Figure_B1.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot16), beside=T, xlab="Education Level", ylab="Percentage", col=c("gray90", "gray70"),
space=rep(c(0.4, 0.1), 6), las=1, ylim=c(0, 60), names.arg=levels(dta2016$NIV_small))
legend("topleft", c("2003 survey", "2016 survey"), fill=c("gray90", "gray70"))
text(myplot[1,], y=0, paste(round(myplot03,1), "%", sep=""), pos=3, cex=0.9)
text(myplot[2,], y=0, paste(round(myplot16,1), "%", sep=""), pos=3, cex=0.9)
arrows(x0=myplot[c(1,3,5,7,9, 11)], x1=myplot[c(1,3,5,7,9, 11)], y0= CI_low03, y1= CI_high03, lwd = 1.5, angle = 90, code = 3, length = 0.05)
arrows(x0=myplot[c(2,4,6,8,10, 12)], x1=myplot[c(2,4,6,8,10, 12)], y0= CI_low16, y1= CI_high16, lwd = 1.5, angle = 90, code = 3, length = 0.05)
dev.off()


##Change in abuse 2003-16, by generation
dta2003$generation<-cut(as.numeric(dta2003$yearofbirth), c(1908, 1960, 1970, 1980, 2005))
table(dta2003$generation)
dta2006$generation<-cut(as.numeric(dta2006$yearofbirth), c(1908, 1960, 1970, 1980, 2005))
table(dta2006$generation)
dta2011$generation<-cut(as.numeric(dta2011$yearofbirth), c(1908, 1960, 1970, 1980, 2005))
table(dta2011$generation)
dta2016$generation<-cut(as.numeric(dta2016$yearofbirth), c(1908, 1960, 1970, 1980, 2005))
table(dta2016$generation)

myplot03<-100*unlist(tapply(dta2003$domViol_dummy, dta2003$generation, mean))
myplot06<-100*unlist(tapply(dta2006$domViol_dummy, dta2006$generation, mean))
myplot11<-100*unlist(tapply(dta2011$domViol_dummy, dta2011$generation, mean))
myplot16<-100*unlist(tapply(dta2016$domViol_dummy, dta2016$generation, mean))

pdf("Figure_B8.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot06, myplot11, myplot16), beside=T, xlab="Year of birth", ylab="Percentage experiencing abuse", col=c("gray40", "gray60", "gray80", "gray95"),
 las=1, ylim=c(0, 60), names.arg=c("<1960", "(1960-70]", "(1970-80]", ">1980"))
legend("topleft", c("2003 survey", "2006 survey", "2011 survey", "2016 survey"), fill=c("gray40", "gray60", "gray80", "gray95"))
text(myplot[1,], y=0, paste(round(myplot03), "%", sep=""), pos=3, cex=0.8)
text(myplot[2,], y=0, paste(round(myplot06), "%", sep=""), pos=3, cex=0.8)
text(myplot[3,], y=0, paste(round(myplot11), "%", sep=""), pos=3, cex=0.8)
text(myplot[4,], y=0, paste(round(myplot16), "%", sep=""), pos=3, cex=0.8)
dev.off()

##Table B.1
dta2011$DOM<-dta2011$DOMINIO
dta2016$estado<-dta2016$CVE_ENT

mlm1<-lmer(domViol_dummy ~   as.numeric(NIV_small)+worked + CivStatus + EDAD + indigena + (1| estado), data=dta2003) 
mlm2<-lmer(domViol_dummy ~   as.numeric(NIV_small)+worked + CivStatus + EDAD + indigena + DOM + (1| estado), data=dta2006) 
mlm3<-lmer(domViol_dummy ~   as.numeric(NIV_small)+worked + CivStatus + EDAD + indigena + DOM + (1| estado), data=dta2011)
mlm4<-lmer(domViol_dummy ~   as.numeric(NIV_small)+worked + CivStatus + EDAD + indigena + DOM + (1| estado), data=dta2016)

summary(mlm1)
summary(mlm2)
summary(mlm3)
summary(mlm4)

table_all <- mtable(mlm1, mlm2, mlm3, mlm4, summary.stats=T,digits=3)
cat(toLatex(table_all), file="table_b1.txt")

##Attitudes
myvalues <-matrix(ncol=3, nrow=4)

myvalues[1,]<-100*CI(dta2011$private)
myvalues[2,]<-100*CI(dta2011$sex)
myvalues[3,]<-100*CI(dta2011$obey)
myvalues[4,]<-100*CI(dta2011$right_to_hit)

pdf("Figure_3.pdf", width=4.5, height=3, pointsize=10)
par(mar=c(4, 12, .5, .5))
myplot<-barplot(myvalues[,2], horiz=T, xlim=c(0,35), las=1,
names.arg=c("Violence is a family matter\n and should stay in the family",  "A woman is obliged to have\n sex with her partner", "A wife should obey \nher partner","A man has the right\n to hit his wife"), xlab="Percentage agreeing")
text(y= myplot, x=myvalues[,2], paste(round(myvalues[,2],1),"%"), pos=4)
arrows(y0=myplot, y1=myplot, x0= myvalues[,1], x1= myvalues[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
dev.off()

tiff(file="Figure_3.tiff", width=4.5, height=3, units= "in", res=300, pointsize=10)
par(mar=c(4, 12, .5, .5))
myplot<-barplot(myvalues[,2], horiz=T, xlim=c(0,35), las=1,
names.arg=c("Violence is a family matter\n and should stay in the family",  "A woman is obliged to have\n sex with her partner", "A wife should obey \nher partner","A man has the right\n to hit his wife"), xlab="Percentage agreeing")
text(y= myplot, x=myvalues[,2], paste(round(myvalues[,2],1),"%"), pos=4)
arrows(y0=myplot, y1=myplot, x0= myvalues[,1], x1= myvalues[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
dev.off()


##Attitudes over time
myfig<-matrix(ncol=3, nrow=3)

myfig[1,]<-100*CI(dta2003$right_to_hit[complete.cases(dta2003$right_to_hit)])
myfig[2,]<-100*CI(dta2006$right_to_hit[complete.cases(dta2006$right_to_hit)])
myfig[3,]<-100*CI(dta2011$right_to_hit[complete.cases(dta2011$right_to_hit)])

myfigure<-matrix(ncol=3, nrow=3)

myfigure[1,]<-100*CI(dta2003$obey[complete.cases(dta2003$obey)])
myfigure[2,]<-100*CI(dta2006$obey[complete.cases(dta2006$obey)])
myfigure[3,]<-100*CI(dta2011$obey[complete.cases(dta2011$obey)])

colnames(myfigure)<-names(CI(dta2003$domViol_react_dummy_lastyear))
row.names(myfigure)<-c("2003", "2006", "2011")

pdf(file="Figure_4.pdf", width=6, height=3.5, pointsize=10)
par(mar=c(4, 4, 2, 0.5), mfrow=c(1,2))
myplot<- barplot(myfig[,2], ylim=c(0, 15), names.arg=row.names(myfigure), las=1, ylab="Percentage agreeing", xlab="Year of survey", main="A man has the right to hit his wife", cex.main=1)
arrows(x0=myplot, x1=myplot, y0= myfig[,1], y1= myfig[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfig[,2], 1), "%", sep=""), pos=3)

myplot<- barplot(myfigure[,2], ylim=c(0, 70), names.arg=row.names(myfigure), las=1, ylab="Percentage agreeing", xlab="Year of survey", main="A wife should obey her partner", cex.main=1)
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

tiff(file="Figure_4.tiff", width=6, height=3.5, units= "in", res=300, pointsize=10)
par(mar=c(4, 4, 2, 0.5), mfrow=c(1,2))
myplot<- barplot(myfig[,2], ylim=c(0, 15), names.arg=row.names(myfigure), las=1, ylab="Percentage agreeing", xlab="Year of survey", main="A man has the right to hit his wife", cex.main=1)
arrows(x0=myplot, x1=myplot, y0= myfig[,1], y1= myfig[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfig[,2], 1), "%", sep=""), pos=3)

myplot<- barplot(myfigure[,2], ylim=c(0, 70), names.arg=row.names(myfigure), las=1, ylab="Percentage agreeing", xlab="Year of survey", main="A wife should obey her partner", cex.main=1)
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

##Over time by age and generation -- right to hit

myplot03<-100*unlist(tapply(dta2003$right_to_hit, dta2003$age, mean, na.rm=T))
myplot06<-100*unlist(tapply(dta2006$right_to_hit, dta2006$age, mean, na.rm=T))
myplot11<-100*unlist(tapply(dta2011$right_to_hit, dta2011$age, mean, na.rm=T))

pdf("Figure_B4.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot06, myplot11), beside=T, xlab="Age group", ylab="Percentage agreeing", col=c("gray50", "gray75", "gray95"),
 las=1, ylim=c(0, 20), names.arg=levels(dta2011$age))
legend("topleft", c("2003 survey", "2006 survey", "2011 survey"), fill=c("gray50", "gray75", "gray95"))
text(myplot[1,], y=0, paste(round(myplot03), "%", sep=""), pos=3, cex=0.8)
text(myplot[2,], y=0, paste(round(myplot06), "%", sep=""), pos=3, cex=0.8)
text(myplot[3,], y=0, paste(round(myplot11), "%", sep=""), pos=3, cex=0.8)
dev.off()

myplot03<-100*unlist(tapply(dta2003$right_to_hit, dta2003$generation, mean, na.rm=T))
myplot06<-100*unlist(tapply(dta2006$right_to_hit, dta2006$generation, mean, na.rm=T))
myplot11<-100*unlist(tapply(dta2011$right_to_hit, dta2011$generation, mean, na.rm=T))

pdf("Figure_B6.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot06, myplot11), beside=T, xlab="Year of birth", ylab="Percentage agreeing", col=c("gray50", "gray75", "gray95"),
 las=1, ylim=c(0, 20), names.arg=c("<1960", "(1960-70]", "(1970-80]", ">1980"))
legend("topright", c("2003 survey", "2006 survey", "2011 survey"), fill=c("gray50", "gray75", "gray95"))
text(myplot[1,], y=0, paste(round(myplot03), "%", sep=""), pos=3, cex=0.8)
text(myplot[2,], y=0, paste(round(myplot06), "%", sep=""), pos=3, cex=0.8)
text(myplot[3,], y=0, paste(round(myplot11), "%", sep=""), pos=3, cex=0.8)
dev.off()


##Over time by age and generation -- obey
myplot03<-100*unlist(tapply(dta2003$obey, dta2003$age, mean, na.rm=T))
myplot06<-100*unlist(tapply(dta2006$obey, dta2006$age, mean, na.rm=T))
myplot11<-100*unlist(tapply(dta2011$obey, dta2011$age, mean, na.rm=T))

pdf("Figure_B5.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot06, myplot11), beside=T, xlab="Age group", ylab="Percentage agreeing", col=c("gray50", "gray75", "gray95"),
 las=1, ylim=c(0, 80), names.arg=levels(dta2011$age))
legend("topleft", c("2003 survey", "2006 survey", "2011 survey"), fill=c("gray50", "gray75", "gray95"))
text(myplot[1,], y=0, paste(round(myplot03), "%", sep=""), pos=3, cex=0.8)
text(myplot[2,], y=0, paste(round(myplot06), "%", sep=""), pos=3, cex=0.8)
text(myplot[3,], y=0, paste(round(myplot11), "%", sep=""), pos=3, cex=0.8)
dev.off()


myplot03<-100*unlist(tapply(dta2003$obey, dta2003$generation, mean, na.rm=T))
myplot06<-100*unlist(tapply(dta2006$obey, dta2006$generation, mean, na.rm=T))
myplot11<-100*unlist(tapply(dta2011$obey, dta2011$generation, mean, na.rm=T))

pdf("Figure_B7.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot06, myplot11), beside=T, xlab="Year of birth", ylab="Percentage agreeing", col=c("gray50", "gray75", "gray95"),
 las=1, ylim=c(0, 80), names.arg=c("<1960", "(1960-70]", "(1970-80]", ">1980"))
legend("topright", c("2003 survey", "2006 survey", "2011 survey"), fill=c("gray50", "gray75", "gray95"))
text(myplot[1,], y=0, paste(round(myplot03), "%", sep=""), pos=3, cex=0.8)
text(myplot[2,], y=0, paste(round(myplot06), "%", sep=""), pos=3, cex=0.8)
text(myplot[3,], y=0, paste(round(myplot11), "%", sep=""), pos=3, cex=0.8)
dev.off()


##Attitudes over time by education level
myplot03<-100*unlist(tapply(dta2003$right_to_hit, dta2003$NIV_small, mean, na.rm=T))
myplot06<-100*unlist(tapply(dta2006$right_to_hit, dta2006$NIV_small, mean, na.rm=T))
myplot11<-100*unlist(tapply(dta2011$right_to_hit, dta2011$NIV_small, mean, na.rm=T))

pdf("Figure_B10.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot06, myplot11), beside=T, xlab="Education level", ylab="Percentage agreeing", col=c("gray50", "gray75", "gray95"),
 las=1, ylim=c(0, 35), names.arg=levels(dta2011$NIV_small))
legend("topright", c("2003 survey", "2006 survey", "2011 survey"), fill=c("gray50", "gray75", "gray95"))
text(myplot[1,], y= myplot03, paste(round(myplot03), "%", sep=""), pos=3, cex=0.8)
text(myplot[2,], y= myplot06, paste(round(myplot06), "%", sep=""), pos=3, cex=0.8)
text(myplot[3,], y= myplot11, paste(round(myplot11), "%", sep=""), pos=3, cex=0.8)
dev.off()

##By education level
myplot03<-100*unlist(tapply(dta2003$obey, dta2003$NIV_small, mean, na.rm=T))
myplot06<-100*unlist(tapply(dta2006$obey, dta2006$NIV_small, mean, na.rm=T))
myplot11<-100*unlist(tapply(dta2011$obey, dta2011$NIV_small, mean, na.rm=T))

pdf("Figure_B11.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot06, myplot11), beside=T, xlab="Education level", ylab="Percentage agreeing", col=c("gray50", "gray75", "gray95"),
 las=1, ylim=c(0, 100), names.arg=levels(dta2011$NIV_small))
legend("topright", c("2003 survey", "2006 survey", "2011 survey"), fill=c("gray50", "gray75", "gray95"))
text(myplot[1,], y= myplot03, paste(round(myplot03), "%", sep=""), pos=3, cex=0.8)
text(myplot[2,], y= myplot06, paste(round(myplot06), "%", sep=""), pos=3, cex=0.8)
text(myplot[3,], y= myplot11, paste(round(myplot11), "%", sep=""), pos=3, cex=0.8)
dev.off()

##Look at reporting over time

myfigure<-matrix(ncol=3, nrow=3)

myfigure[1,]<-100*CI(dta2003$domViol_react_dummy_lastyear[complete.cases(dta2003$domViol_react_dummy_lastyear)])
myfigure[2,]<-100*CI(dta2006$domViol_react_dummy_lastyear[complete.cases(dta2006$domViol_react_dummy_lastyear)])
myfigure[3,]<-100*CI(dta2011$domViol_react_dummy_lastyear[complete.cases(dta2011$domViol_react_dummy_lastyear)])

colnames(myfigure)<-names(CI(dta2003$domViol_react_dummy_lastyear))
row.names(myfigure)<-c("2003", "2006", "2011")

pdf(file="Figure_5.pdf", width=4, height=3, pointsize=10)
par(mar=c(4, 4, 1, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 15), names.arg=row.names(myfigure), las=1, ylab="Percentage", xlab="Year of survey")
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

tiff(file="Figure_5.tiff", width=4, height=3, units= "in", res=300, pointsize=10)
par(mar=c(4, 4, 1, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 15), names.arg=row.names(myfigure), las=1, ylab="Percentage", xlab="Year of survey")
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

##By birth cohort
myplot03<-100*unlist(tapply(dta2003$domViol_react_dummy_lastyear, dta2003$generation, mean, na.rm=T))
myplot06<-100*unlist(tapply(dta2006$domViol_react_dummy_lastyear, dta2006$generation, mean, na.rm=T))
myplot11<-100*unlist(tapply(dta2011$domViol_react_dummy_lastyear, dta2011$generation, mean, na.rm=T))

pdf("Figure_B9.pdf", width=9, height=4.5)
par(mar=c(4,4,1, 0.5))
myplot<-barplot(rbind(myplot03, myplot06, myplot11), beside=T, xlab="Year of birth", ylab="Percentage reporting", col=c("gray40", "gray60", "gray80"),
 las=1, ylim=c(0, 15), names.arg=c("<1960", "(1960-70]", "(1970-80]", ">1980"))
legend("topleft", c("2003 survey", "2006 survey", "2011 survey"), fill=c("gray40", "gray60", "gray80"))
text(myplot[1,], y=0, paste(round(myplot03), "%", sep=""), pos=3, cex=0.8)
text(myplot[2,], y=0, paste(round(myplot06), "%", sep=""), pos=3, cex=0.8)
text(myplot[3,], y=0, paste(round(myplot11), "%", sep=""), pos=3, cex=0.8)
dev.off()

###reasons for not reporting

pdf("Figure_7.pdf", width=6, height=4, pointsize=10)
par(mar=c(6.2,4,0.5,0.5))
myplot<-barplot(100*rbind(prop.table(razones06,2)[2,c(2:9)],prop.table(razones11,2)[2,c(2:9)]), beside=T, las=2, ylim=c(0, 35),ylab="Percentage", names.arg=c("Fear", "For children", "Shame", "Keeping\nit quiet", "Not\nimportant", "Don't trust\n authorities", "Said he\nwould change", "Other"), main="", col=c("gray50", "gray80"))
legend("topright", c("2006 survey", "2011 survey"), fill=c("gray50", "gray80"))
text(x=0, y=32, "Sums to more than 100 since\nwomen could give multiple reasons", pos=4)
dev.off()

tiff(file="Figure_7.tiff", width=6, height=4, units= "in", res=300, pointsize=10)
par(mar=c(6.2,4,0.5,0.5))
myplot<-barplot(100*rbind(prop.table(razones06,2)[2,c(2:9)],prop.table(razones11,2)[2,c(2:9)]), beside=T, las=2, ylim=c(0, 35),ylab="Percentage", names.arg=c("Fear", "For children", "Shame", "Keeping\nit quiet", "Not\nimportant", "Don't trust\n authorities", "Said he\nwould change", "Other"), main="", col=c("gray50", "gray80"))
legend("topright", c("2006 survey", "2011 survey"), fill=c("gray50", "gray80"))
text(x=0, y=32, "Sums to more than 100 since\nwomen could give multiple reasons", pos=4)
dev.off()


##Look at telling familiar over time

myfigure<-matrix(ncol=3, nrow=4)

myfigure[1,]<-100*CI(dta2003$familiares[complete.cases(dta2003$familiares)])
myfigure[2,]<-100*CI(dta2006$familiares[complete.cases(dta2006$familiares)])
myfigure[3,]<-100*CI(dta2011$familiares[complete.cases(dta2011$familiares)])
myfigure[4,]<-100*CI(dta2016$familiares[complete.cases(dta2016$familiares)])

colnames(myfigure)<-names(CI(dta2003$familiares))
row.names(myfigure)<-c("2003", "2006", "2011", "2016")

pdf(file="Figure_6a.pdf", width=3, height=3.5, pointsize=10)
par(mar=c(4, 4, 2, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 50), names.arg=row.names(myfigure), las=1, ylab="Percentage", xlab="Year of survey", main="Talk to relatives about abuse", cex.main=1)
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

tiff(file="Figure_6a.tiff", width=3, height=3.5, units= "in", res=300, pointsize=10)
par(mar=c(4, 4, 2, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 50), names.arg=row.names(myfigure), las=1, ylab="Percentage", xlab="Year of survey", main="Talk to relatives about abuse", cex.main=1)
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()


##Look at telling amigos over time

myfigure<-matrix(ncol=3, nrow=4)

myfigure[1,]<-100*CI(dta2003$amigos[complete.cases(dta2003$amigos)])
myfigure[2,]<-100*CI(dta2006$amigos[complete.cases(dta2006$amigos)])
myfigure[3,]<-100*CI(dta2011$amigos[complete.cases(dta2011$amigos)])
myfigure[4,]<-100*CI(dta2016$amigos[complete.cases(dta2016$amigos)])

colnames(myfigure)<-names(CI(dta2003$amigos))
row.names(myfigure)<-c("2003", "2006", "2011", "2016")

pdf(file="Figure_6b.pdf", width=3, height=3.5, pointsize=10)
par(mar=c(4, 4, 2, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 20), names.arg=row.names(myfigure), las=1, ylab="Percentage", xlab="Year of survey", main="Talk to friends about abuse", cex.main=1)
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

tiff(file="Figure_6b.tiff", width=3, height=3.5, units= "in", res=300, pointsize=10)
par(mar=c(4, 4, 2, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 20), names.arg=row.names(myfigure), las=1, ylab="Percentage", xlab="Year of survey", main="Talk to friends about abuse", cex.main=1)
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

###Gastos
myfigure<-matrix(ncol=3, nrow=3)

myfigure[1,]<-100*CI(dta2003$gastos[complete.cases(dta2003$gastos)])
myfigure[2,]<-100*CI(dta2006$gastos[complete.cases(dta2006$gastos)])
myfigure[3,]<-100*CI(dta2011$gastos[complete.cases(dta2011$gastos)])

colnames(myfigure)<-names(CI(dta2003$domViol_react_dummy_lastyear))
row.names(myfigure)<-c("2003", "2006", "2011")

pdf(file="Figure_8.pdf", width=4, height=3, pointsize=10)
par(mar=c(4, 4, 2, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 100), names.arg=row.names(myfigure), las=1, ylab="Percentage agreeing", xlab="Year of survey", main="A man should be in charge of all the costs in the family", cex.main=0.9)
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()

tiff(file="Figure_8.tiff", width=4, height=3, units= "in", res=300, pointsize=10)
par(mar=c(4, 4, 1, 0.5))
myplot<- barplot(myfigure[,2], ylim=c(0, 100), names.arg=row.names(myfigure), las=1, ylab="Percentage agreeing", xlab="Year of survey", main="A man should be in charge of all the costs in the family", cex.main=0.9)
arrows(x0=myplot, x1=myplot, y0= myfigure[,1], y1= myfigure[,3], lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=myplot, y=0, paste(round(myfigure[,2], 1), "%", sep=""), pos=3)
dev.off()



##############################################################
#Show patterns by state characteristics
##############################################################

##Collapse key variables for each year to the state level
names(dta2003)
state2003<-aggregate(dta2003[,c(5, 219, 221:dim(dta2003)[2])], by=list(State= dta2003$estado), FUN="mean", na.rm=T)

names(dta2006)
state2006<-aggregate(dta2006[,c(235, 238, 240:dim(dta2006)[2])], by=list(State= dta2006$estado), FUN="mean", na.rm=T)

names(dta2011)
state2011<-aggregate(dta2011[,c(393, 396, 398:dim(dta2011)[2])], by=list(State= dta2011$estado), FUN="mean", na.rm=T)

names(dta2016)
state2016<-aggregate(dta2016[,c(417, 420, 423:dim(dta2016)[2])], by=list(State= dta2016$CVE_ENT), FUN="mean", na.rm=T)

#Put all together into one dataset
names(state2003)<-paste(names(state2003), "_03", sep="")
names(state2006)<-paste(names(state2006), "_06", sep="")
names(state2011)<-paste(names(state2011), "_11", sep="")
names(state2016)<-paste(names(state2016), "_16", sep="")

state_dta<-cbind(state2003, state2006, state2011, state2016)


#Create variables
state_dta$change_viol03_06<-100*(state_dta$domViol_dummy_06-state_dta$domViol_dummy_03)
state_dta$change_viol06_11<-100*(state_dta$domViol_dummy_11-state_dta$domViol_dummy_06)
state_dta$change_viol11_16<-100*(state_dta$domViol_dummy_16-state_dta$domViol_dummy_11)

state_dta$change_report06_11<-100*(state_dta$domViol_react_dummy_lastyear_11-state_dta$domViol_react_dummy_lastyear_06)

state_dta$change_familiares11_16<-100*(state_dta$familiares_16-state_dta$familiares_11)

state_dta$change_obey03_06<-100*(state_dta$obey_06-state_dta$obey_03)
state_dta$change_obey06_11<-100*(state_dta$obey_11-state_dta$obey_06)

state_dta$change_hit03_06<-100*(state_dta$right_to_hit_06-state_dta$right_to_hit_03)
state_dta$change_hit06_11<-100*(state_dta$right_to_hit_11-state_dta$right_to_hit_06)

dev<-read.csv("Dev_data.csv")
names(dev)

murders<-read.csv("Data_murders_municipal.csv")
murders<-aggregate(murders[,c(7:32)], by=list(State=murders$State.Code), FUN="sum", na.rm=T)

dev<-merge(dev, murders, by.x="State_no", by.y="State")
dev$State_short<-as.character(dev$State_short)
dev$State_short[18]<-"NA"

dev$murder_rate00<-1000*(dev$X2000/dev$pop2000)
dev$murder_rate05<-1000*(dev$X2005/dev$pop2005)
dev$murder_rate10<-1000*(dev$X2010/dev$pop2010)
dev$murder_rate15<-1000*(dev$X2015/dev$pop2015)
dev$murder_change00_05<-dev$murder_rate05-dev$murder_rate00
dev$murder_change05_10<-dev$murder_rate10-dev$murder_rate05
dev$murder_change10_15<-dev$murder_rate15-dev$murder_rate10

dev$GDPChange_05_10<-dev$GDP2010-dev$GDP2005
dev$GDPChange_10_15<-dev$GDP2015-dev$GDP2010

state_dta<-merge(state_dta, dev, by.x="State_03", by.y="State_no")


#Change in GDP and outcome variables
pdf(file="Figure_B2.pdf", width=6, height=7)
par(mfrow=c(3,2), mar=c(4,4,0.5, 0.5))
plot(state_dta$change_viol06_11 ~ state_dta$GDPChange_05_10, 
ylab="Change in VAW 2006-11", xlab="Change in state GDP 2005-10",
col="transparent", las=1)
text(state_dta$change_viol06_11 ~ state_dta$GDPChange_05_10, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_viol06_11 ~ state_dta$GDPChange_05_10), col="gray30", lty=2)
legend("bottomleft", lty=c(2), col=c("gray30"), c("Trend line" ))

plot(state_dta$change_report06_11 ~ state_dta$GDPChange_05_10, 
ylab="Change in reporting 2006-11", xlab="Change in state GDP 2005-10",
col="transparent", las=1)
text(state_dta$change_report06_11 ~ state_dta$GDPChange_05_10, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_report06_11 ~ state_dta$GDPChange_05_10), col="gray30", lty=2)

plot(state_dta$change_viol11_16 ~ state_dta$GDPChange_10_15, 
ylab="Change in VAW 2011-16", xlab="Change in state GDP 2010-15",
col="transparent", las=1)
text(state_dta$change_viol11_16 ~ state_dta$GDPChange_10_15, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_viol11_16 ~ state_dta$GDPChange_10_15), col="gray30", lty=2)

plot(state_dta$change_familiares11_16 ~ state_dta$GDPChange_10_15, 
ylab="Change in telling family 2011-16", xlab="Change in state GDP 2010-15",
col="transparent", las=1)
text(state_dta$change_familiares11_16 ~ state_dta$GDPChange_10_15, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_familiares11_16 ~ state_dta$GDPChange_10_15), col="gray30", lty=2)

plot(state_dta$change_obey06_11 ~ state_dta$GDPChange_05_10, 
ylab="Should obey, 2006-11", xlab="Change in state GDP 2005-10",
col="transparent", las=1)
text(state_dta$change_obey06_11 ~ state_dta$GDPChange_05_10, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_obey06_11 ~ state_dta$GDPChange_05_10), col="gray30", lty=2)

plot(state_dta$change_hit06_11 ~ state_dta$GDPChange_05_10, 
ylab="Right to hit, 2006-11", xlab="Change in state GDP 2005-10",
col="transparent", las=1)
text(state_dta$change_hit06_11 ~ state_dta$GDPChange_05_10, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_hit06_11 ~ state_dta$GDPChange_05_10), col="gray30", lty=2)
dev.off()


##Look at chance in homocides and VAW

pdf(file="Figure_B3.pdf", width=8, height=3)
par(mfrow=c(1, 3), mar=c(4,4,0.5, 0.5))
plot(state_dta$change_viol03_06 ~ state_dta$murder_change00_05, 
ylab="Change in VAW 2003-06", xlab="Change in murder rate per 1000, 2000-05",
col="transparent", las=1)
text(state_dta$change_viol03_06 ~ state_dta$murder_change00_05, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_viol03_06 ~ state_dta$murder_change00_05), col="gray30", lty=2)

plot(state_dta$change_viol06_11 ~ state_dta$murder_change05_10, 
ylab="Change in VAW 2006-11", xlab="Change in murder rate per 1000, 2005-10",
col="transparent", las=1)
text(state_dta$change_viol06_11 ~ state_dta$murder_change05_10, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_viol06_11 ~ state_dta$murder_change05_10), col="gray30", lty=2)

plot(state_dta$change_viol11_16 ~ state_dta$murder_change10_15, 
ylab="Change in VAW 2011-16", xlab="Change in murder rate per 1000, 2010-15",
col="transparent", las=1)
text(state_dta$change_viol11_16 ~ state_dta$murder_change10_15, labels= state_dta$State_short, cex= 0.8, col="gray30", pos=1)
abline(lm(state_dta$change_viol11_16 ~ state_dta$murder_change10_15), col="gray30", lty=2)
dev.off()


########################################################################################################################
################# Maps 2006 and 2011 #################################################################################

library(maps)
library(rgdal)
library(PBSmapping)
library(maptools)
library(RColorBrewer) 
library(Hmisc)
library(rgeos)

#Step 1 load maps downloaded from https://www.arcgis.com/home/item.html?id=ac9041c51b5c49c683fbfec61dc03ba8
States<-readOGR("~/Dropbox (UiO)/Shared/Mala_Francesca/Mexico/World_Politics/Final_submission/Replication_materials/Map_files/mexstates.shp")

#States<-readOGR("mexstates.shp")
NationalBoundary<-gUnaryUnion(States)

#Step 2 Merge with survey data 
Statesorig <- as(States, "data.frame")
Statesorig$SORT_ID<-1:length(Statesorig$OBJECTID)

col2003<-100*prop.table(table(dta2003$estado, dta2003$domViol_dummy), 1)
col2003<-data.frame(col2003[,2])
names(col2003)<-"viol2003"

col2006<-100*prop.table(table(dta2006$estado, dta2006$domViol_dummy), 1)
col2006 <-data.frame(col2006[,2])
names(col2006)[1]<-"viol2006"

col2011<-100*prop.table(table(dta2011$estado, dta2011$domViol_dummy), 1)
col2011<-data.frame(col2011[,2])
names(col2011)<-"viol2011"

col2016<-100*prop.table(table(dta2016$CVE_ENT, dta2016$domViol_dummy), 1)
col2016 <-data.frame(col2016[,2])
names(col2016)<-"viol2016"

mytable<-cbind(col2003, col2006, col2011, col2016)
mytable$change<-mytable$viol2011-mytable$viol2006

mytable$State_no<-row.names(mytable)

for (i in 1:32){
mytable$State_name[i]<-as.character(dta2016$NOM_ENT[dta2016$CVE_ENT==i][1])
}

mytable[order(mytable$viol2003),]
mytable$State_name[order(mytable$viol2003)][28:32]

mytable[order(mytable$viol2006),]
mytable$State_name[order(mytable$viol2006)][28:32]

mytable[order(mytable$change),]
mytable$State_name[order(mytable$viol2006)][28:32]

cor(mytable$viol2003, mytable$viol2006)
cor(mytable$viol2006, mytable$viol2011)
cor(mytable$viol2011, mytable$viol2016)

Statesnew<-merge(Statesorig, mytable, by.x=c("STATE_CODE"), by.y=c("State_no"), all.x=TRUE, all.y=FALSE)

##Remember to sort the data again after the merge!
Statesnew<-Statesnew[order(Statesnew$SORT_ID),]

Statesnew$viol2003_cut<-cut(Statesnew$viol2003, breaks= c(0,25, 30, 35, 40, 45, 50, 60))
levels(Statesnew$viol2003_cut)<-c("<25", "25-30", "30-35", "35-40","40-45", "45-50", ">50")

Statesnew$viol2006_cut<-cut(Statesnew$viol2006, breaks= c(0,25, 30, 35, 40, 45, 50, 60))
levels(Statesnew$viol2006_cut)<-c("<25", "25-30", "30-35", "35-40","40-45", "45-50", ">50")

Statesnew$viol2011_cut<-cut(Statesnew$viol2011, breaks= c(0,25, 30, 35, 40, 45, 50, 60))
levels(Statesnew$viol2011_cut)<-c("<25", "25-30", "30-35", "35-40","40-45", "45-50", ">50")

Statesnew$viol2016_cut<-cut(Statesnew$viol2016, breaks= c(0,25, 30, 35, 40, 45, 50, 60))
levels(Statesnew$viol2016_cut)<-c("<25", "25-30", "30-35", "35-40","40-45", "45-50", ">50")

#Overwrite the data part in the orginal dataset
States@data<-Statesnew

#Look at the color possibilities with display.brewer.all()

pdf(file="Figure_2a.pdf")
addon<-list("sp.polygons", NationalBoundary, col="black", first=FALSE, lwd=1.2)
spplot(States, zcol="viol2003_cut", col.regions=brewer.pal(7, "YlOrRd"), lwd=.3,
main="2003 survey", 
sp.layout=addon,
	par.settings=list(
    axis.line=list(col=NA),
  	fontsize=list(text=15)))
dev.off()

pdf(file="Figure_2b.pdf")
addon<-list("sp.polygons", NationalBoundary, col="black", first=FALSE, lwd=1.2)
spplot(States, zcol="viol2016_cut", col.regions=brewer.pal(7, "YlOrRd"), lwd=.3,
main="2016 survey", 
sp.layout=addon,
	par.settings=list(
    axis.line=list(col=NA),
  	fontsize=list(text=15)))
dev.off()

tiff(file="Figure_2a.tiff", width=7, height=7, units= "in", res=300, pointsize=10)
addon<-list("sp.polygons", NationalBoundary, col="black", first=FALSE, lwd=1.2)
spplot(States, zcol="viol2003_cut", col.regions=gray.colors(7, start = 0.9, end = 0.25), lwd=.3,
main="2003 survey", 
sp.layout=addon,
	par.settings=list(
    axis.line=list(col=NA),
  	fontsize=list(text=15)))
dev.off()

tiff(file="Figure_2b.tiff", width=7, height=7, units= "in", res=300, pointsize=10)
addon<-list("sp.polygons", NationalBoundary, col="black", first=FALSE, lwd=1.2)
spplot(States, zcol="viol2016_cut", col.regions=gray.colors(7, start = 0.9, end = 0.25), lwd=.3,
main="2016 survey", 
sp.layout=addon,
	par.settings=list(
    axis.line=list(col=NA),
  	fontsize=list(text=15)))
dev.off()


