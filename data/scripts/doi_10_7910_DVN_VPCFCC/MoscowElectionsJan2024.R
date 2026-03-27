#install.packages("estimatr")	
#install.packages("ri2")
#install.packages("randomizr")
#install.packages("dplyr")

library(foreign)
library(estimatr)
library(randomizr)
library(ri2)
library(dplyr)

## put your way to file here.
mydata <- read.dta("D:/data/MoscowMayorElections/RAW_DATA/data_moscow_elections_Jan2024.dta")

#res_cl <- lm_robust(percent_sobyanin~newspaper + percent_putin + percent_turnout2012 + distmetro, data = subset(mydata, (col1==1) & (incl==1)), clusters = metrolinepairs)
#res_cl <- lm_robust(percent_sobyanin~newspaper + percent_putin + percent_turnout2012 + distmetro, data = subset(mydata, (col1==1)), clusters = id_line	)
#res_cl <- lm_robust(percent_sobyanin~newspaper, data = subset(mydata, (col1==1)), clusters = id_line	)
#summary(res_cl) 


#to run script for different column one should substitute (col1==1) with (col2==1) or (col3==1) or (col4==1)

dat <- mydata %>% filter((col1==1)) %>% select (percent_sobyanin,
                                                 newspaper, 
                                                 percent_putin,
                                                 percent_turnout2012,
                                                 distmetro,
                                                 metrolinepairs)

dat <- na.omit(dat)
count(dat)

declaration <- declare_ra(clusters=as.vector(dat$metrolinepairs), N = 8, m=2)



set.seed(1)

ri2_out <- conduct_ri(
  formula=percent_sobyanin~newspaper+percent_putin+percent_turnout2012+distmetro,
  declaration = declaration,
  assignment = "newspaper", 
  sharp_hypothesis = 0,
  data = dat
)
plot (ri2_out)
summary(ri2_out)

 





