#Replication files for "Expressive Power of Anti-Violence Legislation: Changes in Social Norms on Violence Against Women in 
#Mexico" by Mala Htun and Francesca R. Jensenius (World Politics 2022)

##Code to merge data files and pick out variables from the ENDIREH 2006 data for our analysis


rm(list = ls())

library(foreign)
library(readxl)


#All files used here were downloaded from http://en.www.inegi.org.mx/programas/endireh/2006/ and saved in a folder called Endireh2006
setwd("") #Enter the path to where the folder Endireh2006 is located

files<-list.files(path="ENDIREH2006/base_datos_endireh06", pattern=".XLSX", recursive=TRUE,full.names=TRUE)

dem<-read_excel(files[1])
viv<-read_excel(files[9])

#Create estado variable
viv$estado<-gsub("([0-9]{2}).*", replacement="\\1", viv$N_CON) 
table(viv$estado, viv$DOM)

#Now merge them together, keeping just the variables we need
names(viv)
names(dem)
dta<-merge(viv[,c(1,2,31)], dem, by=c("N_CON", "V_SEL")) 
dim(dta)

#Then merge in the data for the rest of the survey (unidas)
unid1<-read_excel(files[2])
unid2<-read_excel(files[3])
unid3<-read_excel(files[4])

#Merge together
dtaUnid<-merge(unid1[,c(1:5, #control and working or not
24:64, #discrimination outside of household
65:71)], #Indigenous language
unid2[,c(1:123, 127:133, 142:153, 182: 190)], by=c("N_CON", "V_SEL", "N_REN")) 

dtaUnid<-merge(dtaUnid,
unid3[,c(1:3, 45:60)], by=c("N_CON", "V_SEL", "N_REN"))  #Roles masculinos y femininos

#Civil status
table(dtaUnid$P4_1)
dtaUnid$CivStatus<-ifelse(dtaUnid$P4_1==1, "Unida", "Casada")

table(dtaUnid$P4_2)
dtaUnid$CivStatus<-ifelse(dtaUnid$P4_2==1, as.character(dtaUnid$CivStatus), NA)

table(dtaUnid$CivStatus)

#Subset to women currently live with their partner
dtaUnid<-dtaUnid[complete.cases(dtaUnid$CivStatus),]

#Merge with Vivienda info
names(dta)
names(dtaUnid)
dta<-merge(dta, dtaUnid, by.x=c("N_CON", "V_SEL", "N_REN"), by.y=c("N_CON", "V_SEL", "N_REN"), all.x=F, all.y=T)  
dim(dta)

names(dta)

for (i in c(6, 23:dim(dta)[2])){
	if (names(dta)[i]!="CivStatus"){
	dta[,i]<-as.numeric(as.character(dta[,i]))
}}


#################################################################################################
###Creating key variables
#################################################################################################

#Worked in previous week 
table(dta$P2_6)
dta$worked<-ifelse(dta$P2_6==1, 1, 0)

#dta$age
table(dta$EDAD)
dta$EDAD<-ifelse(dta$EDAD==998, NA, dta$EDAD) #continous measure
dta$age<-cut(as.numeric(dta$EDAD), c(15, 25, 35, 50, 110)) #Measure used for figures

# Education
table(dta$NIV)
dta$NIV<-ifelse(dta$NIV==99, NA, dta$NIV)
dta$NIV<-as.numeric(as.character(dta$NIV))

dta$NIV_small<-ifelse(dta$NIV==0 | dta$NIV==1, "No school", "Graduate\nstudies")
dta$NIV_small<-ifelse(dta$NIV==2, "Primary\nschool", dta$NIV_small)
dta$NIV_small<-ifelse(dta$NIV==4 | dta$NIV==3, "Middle School", dta$NIV_small) 
dta$NIV_small<-ifelse(dta$NIV==6 | dta$NIV==5 | dta$NIV==7 , "High school", dta$NIV_small)
dta$NIV_small<-ifelse(dta$NIV==8, "Undergraduate\nstudies", dta$NIV_small)
dta$NIV_small<-as.factor(dta$NIV_small)
dta$NIV_small<-factor(dta$NIV_small, levels(dta$NIV_small)[c(4, 5, 3, 2, 6, 1)])
table(dta$NIV_small)

#Indigenous, woman speaks indigenous language
dta$indigena<-ifelse(dta$P4_6==1, 1, 0)


write.csv(dta, "ENDIREH2006_ours.csv", row.names=F)