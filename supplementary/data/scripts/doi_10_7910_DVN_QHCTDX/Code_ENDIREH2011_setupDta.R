#Replication files for "Expressive Power of Anti-Violence Legislation: Changes in Social Norms on Violence Against Women in 
#Mexico" by Mala Htun and Francesca R. Jensenius (World Politics 2022)

##Code to merge data files and pick out variables from the ENDIREH 2011 data for our analysis

rm(list = ls())

library(foreign)
library(readxl)

#All files used here were downloaded from http://en.www.inegi.org.mx/programas/endireh/2011/ and saved in a folder called Endireh2011
setwd("") #Enter the path to where the folder Endireh2011 is located


files<-list.files(path="ENDIREH2011/base_datos_endireh11", pattern=".xlsx", recursive=TRUE,full.names=TRUE)

hogar<-read_excel(files[4])
dem<-read_excel(files[5])
viv<-read_excel(files[11])

#Create estado variable
viv$estado<-gsub("([0-9]{2}).*", replacement="\\1", viv$CONTROL) 
table(viv$estado, viv$DOMINIO)

#Now merge them together, keeping just the variables we need
names(viv)
names(hogar)
dta<-merge(hogar[,c(1:4)], viv[,c(1,2, 24, 27)], by=c("CONTROL", "VIV_SEL"))
dim(dta)

names(dta)
names(dem)
dta<-merge(dta, dem[,c(1:14)], by=c("CONTROL", "VIV_SEL", "HOGAR"))  
dim(dta)

dta$CivStatus<-ifelse(dta$AC11==1, "Unida", NA)
dta$CivStatus<-ifelse(dta$AC11==5, "Casada", as.character(dta$CivStatus))
table(dta$CivStatus)

#Then merge in the data for the rest of the survey (unidas)
unid1<-read_excel(files[8])
unid2<-read_excel(files[9])
unid3<-read_excel(files[10])

#Merge together
dtaUnid<-merge(unid1[,c(1:7, 31:206)], unid2[,c(1:4, 58:225)], by=c("CONTROL", "VIV_SEL","HOGAR", "R_SEL_M"))  

dtaUnid<-merge(dtaUnid , unid3[,c(1:12, 98:115)], by=c("CONTROL", "VIV_SEL","HOGAR", "R_SEL_M"))  

#Merge with Vivienda info
names(dta)
names(dtaUnid)

dta<-merge(dta, dtaUnid, by.x=c("CONTROL", "VIV_SEL", "HOGAR", "N_REN"), by.y=c("CONTROL", "VIV_SEL", "HOGAR", "R_SEL_M"))  
dim(dta)

for (i in 20:dim(dta)[2]){
	dta[,i]<-as.numeric(as.character(dta[,i]))
}

###CREATING KEY VARIABLES

#worked last week
table(dta$AC7) #The ones who had worked in last year
dta$worked<-ifelse(dta$AC7 ==1, 1, 0)
dta$worked<-ifelse(is.na(dta$worked), 0, dta$worked)
table(dta$worked)

#Age
table(dta$EDAD)
dta$EDAD<-ifelse(dta$EDAD==98, NA, as.numeric(as.character(dta$EDAD)))
dta$age<-cut(as.numeric(dta$EDAD), c(15, 25, 35, 50, 100))

# Education
dta$NIV<-ifelse(dta$NIV==99, NA, dta$NIV)
dta$NIV<-as.numeric(as.character(dta$NIV))

dta$NIV_small<-ifelse(dta$NIV==0 | dta$NIV==1, "No school", "Graduate\nstudies")
dta$NIV_small<-ifelse(dta$NIV==2, "Primary\nschool", dta$NIV_small)
dta$NIV_small<-ifelse(dta$NIV==5 | dta$NIV==4 | dta$NIV==3, "Middle School", dta$NIV_small)
dta$NIV_small<-ifelse(dta$NIV==6 | dta$NIV==7, "High school", dta$NIV_small)
dta$NIV_small<-ifelse(dta$NIV==8, "Undergraduate\nstudies", dta$NIV_small)
dta$NIV_small<-as.factor(dta$NIV_small)
dta$NIV_small<-factor(dta$NIV_small, levels(dta$NIV_small)[c(4, 5, 3, 2, 6, 1)])
table(dta$NIV_small)

#Indigenous, woman speaks indigenous language
dta$indigena<-ifelse(dta$AP1_2==1 , 1, 0)
table(dta$indigena)

write.csv(dta, "ENDRH2011_ours.csv", row.names=F)