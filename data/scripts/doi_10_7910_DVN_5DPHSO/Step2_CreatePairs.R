#Code to save a listing of all correlations that need to be collected from the GSS

#Change the working directory as needed
setwd("/Users/djd78/Box/Belief Networks/Replication Materials/Results")

rm(list=ls())

require(plyr)
require(psych)

#Read in Stata file
d=read.dta("GSS_Recoded.dta", convert.factors=FALSE)

###############################################
#Looping through and gathering correlations

#parameters and inputs
years = unique(d$year) #vector of GSS years 
thresh = 100 #minimum number of respondents for a correlation to be gathered 

#create empty vector of correlation-pairs 
pairs = vector()
for(ye in years) {
  print(ye)
  d1=subset(d, year==ye)   #reduce to focal data for each year 
  d1 = d1[, colSums(is.na(d1))<nrow(d1) & !(colnames(d1)=="year")] #reduce to available variables in a given year 
  if(!(empty(d1))) {
    for(y in 1:ncol(d1)) {
      for(x in 1:ncol(d1)) {
        if(y>x) {
          n = pairwiseCount(d1[,y], d1[,x])
          if(n>thresh) {
            var1 = colnames(d1)[x]
            var2 = colnames(d1)[y]
	          pairs = append(pairs, paste(var1, var2, ye, n, sep=","))
          }
        }
      }
    }
  }
}
save(pairs, file="all_pairs.saved")
