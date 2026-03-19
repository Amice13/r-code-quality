#Replication files for "Expressive Power of Anti-Violence Legislation: Changes in Social Norms on Violence Against Women in 
#Mexico" by Mala Htun and Francesca R. Jensenius (World Politics 2022)

##Code to merge data files and pick out variables from the ENDIREH 2016 data for our analysis

rm(list = ls())

library(foreign)
library(readxl)

#All files used here were downloaded from http://en.www.inegi.org.mx/programas/endireh/2016/ and saved in a folder called Endireh2016
setwd("") #Enter the path to where the folder Endireh2016 is located

files<-list.files(path="ENDIREH2016/base_datos_endireh2016_dbf", pattern=".dbf", recursive=TRUE,full.names=TRUE)

##In this survey all women are included in all the files (except for viv and dem, which cover all households and all people in those households, respectively. In this survey they selected only one woman from each household)

dem<-read.dbf(files[25])
Sec3<-read.dbf(files[2])
Sec4<-read.dbf(files[3])
Sec12<-read.dbf(files[15]) #Vida en pareja
Sec13<-read.dbf(files[16]) 
Sec13_2<-read.dbf(files[17])
Sec15<-read.dbf(files[20])

names(dem)
dem <-dem[,c(1, 2, 3, 6:9, 16, 17, 20:30, 38)]
names(Sec3)
Sec3<-Sec3[,c(2, 14, 15)]
names(Sec4)
Sec4<-Sec4[,c(2, 15, 17, 29, 30, 62:75)]
names(Sec12)
Sec12<-Sec12[,c(2, 30, 34)]
names(Sec13)
Sec13 <-Sec13[,c(2, 16:198)]
names(Sec13_2)
Sec13_2 <-Sec13_2[,c(2, 16:195)]
names(Sec15)
Sec15 <-Sec15[,c(2, 16:24)]

dta<-merge(dem, Sec3, by="ID_MUJ")
dta_0<-merge(dta, Sec4, by="ID_MUJ")
dta_1<-merge(dta_0, Sec12, by="ID_MUJ")
dta_2<-merge(dta_1, Sec13, by="ID_MUJ")
dta_3<-merge(dta_2, Sec13_2, by="ID_MUJ")
dta<-merge(dta_3, Sec15, by="ID_MUJ")

#########Create variables
names(dta)

#Civil status, only include casada/unida who currently live together in the dataset
table(dta$P3_1)

dta$CivStatus<-ifelse(dta$P3_1==1, "Unida", NA)
dta$CivStatus<-ifelse(dta$P3_1==5, "Casada", as.character(dta$CivStatus))

table(dta$P3_2, dta$P3_1)
dta$CivStatus<-ifelse(dta$P3_2 ==1, as.character(dta$CivStatus), NA)

dta<-dta[complete.cases(dta$CivStatus),]
dim(dta)

#work
table(dta$P2_13) #Trabajo la semana pasada?
dta$worked<-ifelse(dta$P2_13 ==1, 1, 0)
dta$worked <-ifelse(is.na(dta$worked), 0, dta$worked)
table(dta$worked)

#Age
table(dta$EDAD)
dta$EDAD<-ifelse(dta$EDAD==98, NA, as.numeric(as.character(dta$EDAD)))
dta$EDAD<-ifelse(dta$EDAD==99, NA, as.numeric(as.character(dta$EDAD)))
dta$age<-cut(as.numeric(dta$EDAD), c(15, 25, 35, 50, 100))

# Education
table(dta$NIV)
dta$NIV<-ifelse(dta$NIV==99, NA, as.numeric(as.character(dta$NIV)))

dta$NIV_small<-ifelse(dta$NIV==0 | dta$NIV==1, "No school", "Graduate\nstudies")
dta$NIV_small<-ifelse(dta$NIV==2 | dta$NIV==5, "Primary\nschool", dta$NIV_small)
dta$NIV_small<-ifelse(dta$NIV==3 | dta$NIV==6 | dta$NIV==8, "Middle School", dta$NIV_small)
dta$NIV_small<-ifelse(dta$NIV==4 | dta$NIV==7, "High school", dta$NIV_small)
dta$NIV_small<-ifelse(dta$NIV==9 | dta$NIV==10, "Undergraduate\nstudies", dta$NIV_small)
dta$NIV_small<-as.factor(dta$NIV_small)
dta$NIV_small<-factor(dta$NIV_small, levels(dta$NIV_small)[c(4, 5, 3, 2, 6, 1)])
table(dta$NIV_small)

#Indigenous langudta$age, woman
table(dta$P2_11, dta$P2_10)
dta$indigena <-ifelse(dta$P2_11 ==1, 1, 0)

##Rural/ Urban
dta$DOM<-ifelse(dta$DOMINIO=="R", "R", "U")

names(dta)
write.csv(dta, "ENDRH2016_ours.csv", row.names=F)