rm(list=ls())
library(survey)
var.names <- read.csv("ihis_00028_names.csv",header=TRUE)
nhis10 <- read.fwf("ihis_00028.dat",widths=var.names$Len)
names(nhis10) <- var.names$Variable

#cessation tools included
#CSQPATCHYR is Used nicotine patch last quitting attempt in year 
#CSQGUMYR is Used nicotine gum last quitting attempt in year  
#CSQCHANTXYR is Used Chantix last quitting attempt in year
#CSQMEDSYR is Used medication last quitting attempt in year (a prescription pill, such as Zyban, Buproprion, or Wellbutrin while trying to quit smoking during the past 12 months)
#CSQOTRMEDSYR is Used other nicotine products last quitting attempt in year (nicotine-containing nasal spray, inhaler, lozenge, or tablet when she/he tried to quit smoking in the past 12 months)

#cessation tools not included
#CSQCOUNSELYR is 1-on-1 counseling 
#CSQPROGRAMYR is Used clinic/program last quitting attempt in year 
#CSQTELINEYR is Used telephone help/quit line last quitting attempt in year 

nhis10$cessation.tool <- NA
nhis10$cessation.tool[nhis10$CSQCHANTXYR==1 & nhis10$CSQGUMYR==1 & nhis10$CSQMEDSYR==1 & nhis10$CSQOTRMEDSYR==1 & nhis10$CSQPATCHYR==1] <- 0
nhis10$cessation.tool[nhis10$CSQCHANTXYR==2 | nhis10$CSQGUMYR==2 | nhis10$CSQMEDSYR==2 | nhis10$CSQOTRMEDSYR==2 | nhis10$CSQPATCHYR==2] <- 1
nhis10$age.cat <- 5*floor(nhis10$AGE/5)
nhis10$age.cat2 <- NA
nhis10$age.cat2[nhis10$age.cat %in% c(25,30)] <- "25-34"
nhis10$age.cat2[nhis10$age.cat %in% seq(35,45,5)] <- "35-49"
nhis10$age.cat2[nhis10$age.cat %in% seq(50,60,5)] <- "50-64"
nhis10$age.cat2[nhis10$age.cat %in% c(65)] <- "65-69"
nhis10.sample <- svydesign(ids=~1, weights=~SAMPWEIGHT, data=nhis10)
tmp <- svyby(~cessation.tool, ~age.cat2, subset(nhis10.sample, CSQTRYYR==2), svyciprop, na.rm=TRUE)
past.year.cessation.tool.age.pt.est.se <- rbind(tmp[1,],tmp[1,],tmp[2,],tmp[2,],tmp[2,],tmp[3,],tmp[3,],tmp[3,],tmp[4,])
past.year.cessation.tool.age.pt.est.se$age.cat2 <- seq(25,65,5)
names(past.year.cessation.tool.age.pt.est.se) <- c("age","cessation.tool","se.as.numeric(cessation.tool)")
rownames(past.year.cessation.tool.age.pt.est.se) <- seq(25,65,5)
save(past.year.cessation.tool.age.pt.est.se,file="data/past.year.cessation.tool.pt.est.se.Rdata")
