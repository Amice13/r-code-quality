# set working directory
# wd <- ""
setwd(wd)

# open the cross sectional database
library(haven)
d <- read_dta("Data/simple_cs.dta")

# make Table 1

t1 = table(d$protestant,d$bureaucracy_dummy)
colnames(t1) <- c("Patrimonial","Bureaucratic")
rownames(t1) <- c("Catholic","Protestant")

t1
