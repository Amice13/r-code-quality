# Library
install.packages(c("plyr"))
library(plyr)

# Read in the mpd data
mpd <- read.csv("MPDataset_MPDS2018b.csv",header=T)

# Subset the data to countries in our observational data
mpd_sub <- subset(mpd, countryname=="Czech Republic"|countryname=="Denmark"|countryname=="Germany"|countryname=="Hungary"|countryname=="Netherlands"|countryname=="Poland"|countryname=="Portugal"|countryname=="Spain"|countryname=="Sweden"|countryname=="United Kingdom")

# Create a binary variable for left and right using party family information
mpd_sub$left <- as.numeric(as.character(mapvalues(mpd_sub$parfam, c(10,20,30,40,50,60,70,80,90,95,98,999), c(1,1,1,NA,0,0,0,0,NA,NA,NA,NA))))

# Subset the data to 70s
mpd_sub_70 <- subset(mpd_sub, date>=197000&date<198000)

# Subset the data to 80s
mpd_sub_80 <- subset(mpd_sub, date>=198000&date<199000)

# Subset the data to 2010s
mpd_sub_10 <- subset(mpd_sub, date>=201000&date<202000)

# Lefist parties' change in vote shares over time in the 70s
mpd_sub_70_l <- subset(mpd_sub_70,left==1)

# Rightist parties' change in vote shares over time in the 70s
mpd_sub_70_r <- subset(mpd_sub_70,left==0)

# Lefist parties' change in vote shares over time in the 80s
mpd_sub_80_l <- subset(mpd_sub_80,left==1)

# Rightist parties' change in vote shares over time in the 80s
mpd_sub_80_r <- subset(mpd_sub_80,left==0)

# Lefist parties' change in vote shares over time in the 2010s
mpd_sub_10_l <- subset(mpd_sub_10,left==1)

# Rightist parties' change in vote shares over time in the 2010s
mpd_sub_10_r <- subset(mpd_sub_10,left==0)

##########################
## Table OA7.1: Differences between left and right in a given time period
##########################

# Create a vector of changes in vote share in the 70s for leftist parties
mpd_sub_70_l_list <- split(mpd_sub_70_l,mpd_sub_70_l$party)
changes_70_l <- unlist(lapply(mpd_sub_70_l_list,function(x){
  diff(x$pervote)
}))
# Create a vector of changes in vote share in the 70s for rightist parties
mpd_sub_70_r_list <- split(mpd_sub_70_r,mpd_sub_70_r$party)
changes_70_r <- unlist(lapply(mpd_sub_70_r_list,function(x){
  diff(x$pervote)
}))
# T-test of difference
t.test(changes_70_l,changes_70_r)

# Create a vector of changes in vote share in the 80s for leftist parties
mpd_sub_80_l_list <- split(mpd_sub_80_l,mpd_sub_80_l$party)
changes_80_l <- unlist(lapply(mpd_sub_80_l_list,function(x){
  diff(x$pervote)
}))
# Create a vector of changes in vote share in the 80s for rightist parties
mpd_sub_80_r_list <- split(mpd_sub_80_r,mpd_sub_80_r$party)
changes_80_r <- unlist(lapply(mpd_sub_80_r_list,function(x){
  diff(x$pervote)
}))
# T-test of difference
t.test(changes_80_l,changes_80_r)

# Create a vector of changes in vote share in the 2010s for leftist parties
mpd_sub_10_l_list <- split(mpd_sub_10_l,mpd_sub_10_l$party)
changes_10_l <- unlist(lapply(mpd_sub_10_l_list,function(x){
  diff(x$pervote)
}))
# Create a vector of changes in vote share in the 2010s for rightist parties
mpd_sub_10_r_list <- split(mpd_sub_10_r,mpd_sub_10_r$party)
changes_10_r <- unlist(lapply(mpd_sub_10_r_list,function(x){
  diff(x$pervote)
}))
# T-test of difference
t.test(changes_10_l,changes_10_r)

##########################
## Table OA7.2: Differences for left and right between time periods
##########################

# Comparison of vote share changes for leftist parties in 70s and 2010s
t.test(changes_70_l,changes_10_l) 

# Comparison of vote share changes for leftist parties in 80s and 2010s
t.test(changes_80_l,changes_10_l) 

# Comparison of vote share changes for rightist parties in 70s and 2010s
t.test(changes_70_r,changes_10_r) 

# Comparison of vote share changes for rightist parties in 80s and 2010s
t.test(changes_80_r,changes_10_r)

