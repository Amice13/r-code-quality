## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
# regressions on the cluster
rm(list = ls())
options(stringsAsFactors = FALSE)
seed_to_use <- 216
set.seed(seed_to_use)

# install.packages(pkgs = c("data.table", "stargazer"), repos = "https://cloud.r-project.org/")
### Libraries
library(ggplot2)
library(foreign)
library(data.table)
library(stargazer)
library(lfe)
library(xtable)
library(tidyverse)
case_when <- dplyr::case_when

### load data
load("~/bkd_cd.rdata")

bkn_cd$eselon_label <- with(bkn_cd, ifelse(eselon_lab %in% c(10, 11, 12), "Echelon 1",
                                     ifelse(eselon_lab %in% c(21, 22), "Echelon 2",
                                            ifelse(eselon_lab %in% c(31, 32), "Echelon 3",
                                                   ifelse(eselon_lab %in% c(41, 42), "Echelon 4","Non-Echelon")))))



# create female dummy
bkn_cd$female <- ifelse(bkn_cd$gender=="F",1,0)

# create minority dummies
bkn_cd$muslim <- ifelse(bkn_cd$religion=="ISLAM",1,0)
bkn_cd$catholic <- ifelse(bkn_cd$religion=="KATHOLIK",1,0)
bkn_cd$protestant <- ifelse(bkn_cd$religion=="KRISTEN",1,0)
bkn_cd$hindu <- ifelse(bkn_cd$religion=="HINDU",1,0)
bkn_cd$buddhist <- ifelse(bkn_cd$religion=="BUDHA",1,0)
bkn_cd$confucian <- ifelse(bkn_cd$religion=="KONGHUCU",1,0)
bkn_cd$other <- ifelse(bkn_cd$religion=="LAINNYA",1,0)

# echelon by gender and religion
employees <- bkn_cd %>%
  group_by(eselon_label) %>%
  summarize(female_prop = mean(female,na.rm=T),
            muslim = mean(muslim,na.rm=T),
            catholic = mean(catholic,na.rm=T),
            protestant = mean(protestant,na.rm=T),
            hindu = mean(hindu,na.rm=T),
            buddhist = mean(buddhist,na.rm=T),
            confucian = mean(confucian,na.rm=T),
            other = mean(other,na.rm=T))

# make table
colnames(employees) <- c("Echelon", "% Female","% Muslim", "% Catholic", "% Protestant", "% Hindu","% Buddhist","% Confucian","% Other")
d <- rbind(employees, data.table("General Population",  t(c(0.497, 0.87, 0.03, 0.07, 0.017, 0.007, 0.005, 0.001))), use.names=FALSE)
print(xtable(d, digits = 3), include.rownames = FALSE , file = "~/paper_table_1_descriptive.tex")

# NOTE THAT WE REPLACED VERY SMALL PERCENTAGES WITH ">0" IN THE MANUSCRIPT
# TO DISTINGUISH FROM TRUE ZEROES AFTER ROUNDING 


