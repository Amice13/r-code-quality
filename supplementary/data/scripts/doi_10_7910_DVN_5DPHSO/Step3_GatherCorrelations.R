#Code to gather correlations from the GSS

#Change the working directory as needed
setwd("/Users/djd78/Box/Belief Networks/Replication Materials/Results")

rm(list=ls())

require(foreign)
require(psych)
require(parallel)
require(plyr)
require(janitor)
require(stringr)

#Read in Stata file
d=read.dta("GSS_Recoded.dta", convert.factors=FALSE)

#Load in pairs
load(file="all_pairs.saved")

###############################################

#define function
compute_correlations = function(i) {
  var1 = str_split(pairs[i], ",")[[1]][1] #find variables and year for correlation
  var2 = str_split(pairs[i], ",")[[1]][2]
  ye = str_split(pairs[i], ",")[[1]][3]
  d1=subset(d, year==ye)   #reduce to focal data for given year 
  d1 = remove_empty(d1, which=c("cols"))
  
  #get zero-order pearson correlation
  c=cor.test(d1[,names(d1)==var1], d1[,names(d1)==var2], use = "pairwise.complete.obs", method="pearson")$estimate
  
  #get adjusted partial correlation (unless polviews or partyid are one of the two variables in the correlation pair, or one of these two variables is not available to adjust)
  if(!(var1=="polviews"|var2=="polviews"|var1=="partyid"|var2=="partyid") & ("polviews" %in% colnames(d1)) & ("partyid" %in% colnames(d1)) ) {
    cpart = partial.r(d1, x=c(var1, var2), y=c("polviews", "partyid"), use="pairwise.complete.obs")[2]
  }
  else {
    cpart = NA
  }
  corr = paste(var1, var2, ye, c, cpart, sep=",")
  return(corr)
}
  
#for speed, I recommend running function in parallel; however, this can be changed to "lapply" if you do not have access to parallel computing
n.core=detectCores()
results = mclapply(1:length(pairs), compute_correlations, mc.cores = n.core)

#Collect correlations in one data frame
x = vector()
y = vector()
year = vector()
c = vector()
cpart = vector()

for(j in 1:length(results)) {
  r = str_split(results[[j]], pattern=",")[[1]]
  x = append(x, r[1])
  y = append(y, r[2])
  year = append(year, r[3])
  c = append(c, r[4])
  cpart = append(cpart, r[5])
  }

x = as.character(x)
y = as.character(y)
year = as.numeric(year)
c = as.numeric(c)
cpart = as.numeric(cpart)
r = data.frame(x, y, year, c, cpart)

r$j = paste(r$y, "_", r$x, sep="") #grouping variable for model
r$abs_c=abs(r$c) #absolute correlations
r$abs_cpart=abs(r$cpart)
r$year72=r$year-1972 #year variable starting at 1972

save(r, file="all_correlations.saved")
