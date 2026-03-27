
#Replication files for "Expressive Power of Anti-Violence Legislation: Changes in Social Norms on Violence Against Women in 
#Mexico" by Mala Htun and Francesca R. Jensenius (World Politics 2022)

##Code to merge data files and pick out variables from the ENDIREH2003 data for our analysis

rm(list = ls())

library(foreign)
library(readxl)


#All files used here were downloaded from https://www.inegi.org.mx/programas/endireh/2003/#Microdatos and saved in a folder called Endireh2003
setwd("") #Enter the path to where the folder Endireh2003 is located

files<-list.files(path="Endireh2003", pattern=".xls", recursive=TRUE,full.names=TRUE) #This created a list of the path to all the data files in the folder

dem<-read_excel(files[9])

viv<-read_excel(files[8])a

#Create estado variable
viv$estado<-gsub("([0-9]{2}).*", replacement="\\1", viv$LLAVE) 

#Now merge them together, keeping just the variables we need
names(viv)
names(dem)
dta<-merge(viv[,c(2,27)], dem, by=c("LLAVE")) 
dim(dta)

#Reduce the dta only to the women selected for interviews (Sólo para mujeres de15años y más con pareja residente en la vivienda ))
dta<-dta[dta$ELEGIB==1 & !is.na(dta$ELEGIB==1) & (dta$ESTCON==1 | dta$ESTCON==5), ]
dta<-dta[complete.cases(dta$LLAVE),]
length(unique(dta$LLAVE))

#Create variables wanted from dta

#Civil statusl
dta$CivStatus<-ifelse(dta$ESTCON==5, "Casada", "Unida")

#Create varable for worked in the previous week
dta$worked<-ifelse(dta$CONDACT==1, 1, 0)

#Age
dta$EDAD<-ifelse(dta$EDAD==99, NA, as.numeric(dta$EDAD)) #Continuous measure
dta$age<-cut(as.numeric(dta$EDAD), c(15, 25, 35, 50, 100)) #Measure used for figures

# Education
dta$NIV<-ifelse(dta$ESCOLNIV ==99, NA, as.numeric(dta$ESCOLNIV))

dta$NIV_small<-ifelse(dta$NIV ==1 | dta$NIV ==2, "No school", "Graduate\nstudies")
dta$NIV_small <-ifelse(dta$NIV ==3, "Primary\nschool", dta$NIV_small)
dta$NIV_small <-ifelse(dta$NIV ==5 | dta$NIV ==4, "Middle School", dta$NIV_small)
dta$NIV_small <-ifelse(dta$NIV ==6 | dta$NIV ==7 | dta$NIV ==8, "High school", dta$NIV_small)
dta$NIV_small <-ifelse(dta$NIV ==9, "Undergraduate\nstudies", dta$NIV_small)
dta$NIV_small <-ifelse(dta$NIV ==12, NA, dta$NIV_small)
dta$NIV_small <-as.factor(dta$NIV_small)
dta$NIV_small <-factor(dta$NIV_small, levels(dta$NIV_small)[c(4, 5, 3, 2, 6, 1)])

dta$NIV_smaller<-ifelse(dta$NIV %in% c(1,2,3) , "Up to Primary school", "More than primary school")

table(dta$NIV_smaller)
prop.table(table(dta$NIV_smaller))

names(dta)
dta<-dta[,c(1, 2, 9, 24:29)]

#Then merge in the data for the rest of the survey

#Women
w1<-read_excel(files[10])
w2<-read_excel(files[11])
w3<-read_excel(files[12])

length(unique(w1$LLAVE))
length(unique(w2$LLAVE))
length(unique(w3$LLAVE))

#Merge together

w_dta<-merge(w1[,c(1:6, 45, 46)], w2, by="LLAVE") ##(Picking out violence variables [,c(1, 76:191)])
w_dta2<-merge(w_dta, w3[,c(2, 142:150)], by="LLAVE") ##Picking up gender roles (from OBEDECER to NOCUMPLE)

#Merge with Vivienda info
dta<-merge(dta, w_dta2, by.x=c("LLAVE"), by.y=c("LLAVE"), all.x=F, all.y=F)  
dim(dta)

#Indigenous, woman speaks indigenous language
dta$indigena<-ifelse(dta$HABLIM ==1, 1, 0)

write.csv(dta, "ENDIREH2003_ours.csv", row.names=F)


