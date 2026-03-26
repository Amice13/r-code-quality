# F75 raw metabolomic data preprocessing
# Author: Bijun Wen


library(dplyr)
library(tidyverse)
options(scipen = 9, digits = 3)

#Load raw data
dat<-read.csv("F75_Admission_Metabolomics.csv",check.names=F)

str(dat)

#data structure
#235 obs. 208 variables
#col 1 Subject ID, p180 LOD per plate, p180 QCs per plate, p180 pool samples per plate, TMIC pool samples
#col 2 p180 Plate ID
#col 3-208 Metabolites measured by two platforms -- p180: col2-190; TMIC: col191-208

###############################################################################################################################
# Assign class to metabolite names
###############################################################################################################################
names(dat)
names(dat)[3:23]<-paste0("AA_",names(dat[3:23]))          # AA = Amino acids (p=21); plateform: p180
names(dat)[24:44]<-paste0("BA_",names(dat[24:44]))        # BA = Biogenic amines (p=21); plateform: p180
names(dat)[45:84]<-paste0("AC_",names(dat[45:84]))        # AC = Acylcarnitines (p=40); plateform: p180
names(dat)[85:174]<-paste0("GP_",names(dat[85:174]))      # GP = Glycerophospholipids (p=90); plateform: p180
names(dat)[175:189]<-paste0("SM_",names(dat[175:189]))    # SM = Sphingomyelins (p=15); plateform: p180
names(dat)[190]<-paste0("MS_",names(dat[190]))            # MS = Monosaccharides (p=1); plateform: p180
names(dat)[191:208]<-paste0("OA_",names(dat[191:208]))    # OA = Organic acids (p=18); plateform: TMIC

###############################################################################################################################
# Identify and flag p180 measurements below plate-specific LOD (limit of detection)
###############################################################################################################################
## Measurements below LOD for TMIC-PRIME had already been flagged as "< LOD" in the raw datafile by the TMIC.
## For p180, LOD values are provided in raw datafile for each plate. 
## There are total 5 p180 plates and LOD is specific to each plate (ie.p180_LOD_1,p180_LOD_2,p180_LOD_3,p180_LOD_4,p180_LOD_5)

#Flag p180 measurements below plate-specific LOD as 99999

p180.inx<-grep( "AA_|BA_|AC_|GP_|SM_|MS_", names(dat) ) #p180 columns
dat2<-dat

for (i in 1:5) {
  for (j in p180.inx){
    dat2[j]<-ifelse(dat2$p180_plate==i & dat2[,j]<(dat2[dat2$subjid == paste0("p180_LOD_",i),j]),99999,dat2[,j])
  }
}

#extract row index for subject samples, since we only want to flag subject sample measurements, not the quality control measurements
subj.inx<-grep( "ID_|HC_", dat$subjid ) 
dat2<-dat2[subj.inx,]
#merge original quality control data back
dat2<-rbind(dat2,dat[-subj.inx,])

#Replace 99999 by a string "< LOD"

dat3<-dat2

for (i in p180.inx){
  dat3[i]<-ifelse(dat3[,i]==99999, "< LOD", as.character(dat3[,i]))
}


############### Summary of values below LOD ######################### 
n.p180.lod=length(which(dat2==99999)) #8425 values below LOD in p180
n.tmic.lod=length(which(dat2=="< LOD")) #26 values below LOD in TMIC
n.p180.lod+n.tmic.lod==length(which(dat3=="< LOD")) #Checked, total 8451 below LOD 


###############################################################################################################################
# write.csv(dat3,"F75_Admission_Metabolomics.csv",row.names=F)  # output master metabolomic dataset
###############################################################################################################################

