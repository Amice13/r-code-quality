###################################################################/
# "When the sum is more than its parts"                           #
# by: A. Wuttke, C.H. Schimpf, H. Schoen                          #
# Date: September 4, 2018                                         #
# R-Version: 3.4.0                                                #
# Interface: R-Studio                                             #
# CONVERTING Silva et al Dataset to STATA12                       #
# Original dataset was downloaded from:                           #
# http://populism.byu.edu/Pages/Appendix (September 6, 2018)      #
##################################################################/


#/////////////////////////////#
#### Step 1: load packages ####
#/////////////////////////////#

# The following loop installs packages if they have not
# been installed yet and will unpack them:

packages <- c("readstata13", "lavaan", "foreign")
for (p in packages) {
  if (p %in% installed.packages()[,1]) require(p, character.only=T)
  else {
    install.packages(p)
    library(p, character.only=T)
  }
}

#/////////////////////////////////#
#### Step 2: Working Directory ####
#/////////////////////////////////#

setwd("D:/Dropbox (Privat)/populism_concept and measurement/Data/Silva")

#/////////////////////////////#
#### Step 3: load data set ####
#/////////////////////////////#

data<-read.csv('data.merged_mar17.csv',header=F)
varnames<-unlist(strsplit(readLines('varnames_mar17.txt'), '\t'))
names(data)<-varnames
data[data==-999]<-NA
data<-subset(data, country != 10)
data$simple8.r<-8-data$simple8
data$rwpop8.r<-8-data$rwpop8
data$manich13.r<-8-data$manich13

#/////////////////////////////#
#### Step 4: Save as STATA ####
#/////////////////////////////#

write.dta(data, "D:/Dropbox (Privat)/populism_concept and measurement/Data/Silva/data.merged_mar17.dta") 