# set working directory
# wd <- ""
setwd(wd)

# open the cross sectional database
library(haven)
d <- read_dta("Data/simple_cs.dta")

# make Table 2
library(xtable)
t = cbind.data.frame(d$country_name,
                     d$protestant,
                     d$v2strenadm,
                     d$v2stcritrecadm,
                     d$v3struinvadm,
                     d$v2clrspct)

colnames(t) = c("Country",
                "Protestant",
                "Salaried",
                "Meritocratic",
                "Autonomy",
                "Impartial")

t = t[order(-t$Protestant),]
xtable(t)
