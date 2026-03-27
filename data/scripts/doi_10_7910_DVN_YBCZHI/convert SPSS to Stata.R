# Data file available at: https://www.unl.edu/polphyslab/data 

# Conversion code adapted from Mike Gruszczynski's work at http://mikegruz.tumblr.com/post/704966440/convert-spss-to-stata-without-stat-transfer
# Note: the original link is dead

# Set working directory
setwd("")
# Load foreign library
library(foreign)
# Read in SPSS data
data <- read.spss("Uminn_twinsvy_jan2010 final_labeled.sav", to.data.frame=TRUE)
# Write Stata .dta file
write.dta(data, file="US_raw.dta")
