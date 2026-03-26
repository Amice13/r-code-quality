# set working directory
# wd <- ""
setwd(wd)

# open the cross sectional database
library(haven)
d <- read_dta("Data/venality_cs.dta")

# make table 4
t = table(d$widespread,d$protestant)
colnames(t) = c("Catholic","Protestant")
rownames(t) = c("Limited or no venality","Widespread venality")
t
