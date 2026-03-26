## swing.R
## Mark C. Wilson <markwilson@umass.edu>
##
## Purpose: Empirical part of Grofman-Wilson paper
##
##

#####################################################
##### Load libraries and helper functions

library(tidyverse)
library(haven)
library(extrafont)
library(fontcm)

#font_install('fontcm') # Needed to produce final graphs with CM fonts to match LaTeX
#loadfonts()

setwd("~/Dropbox/Mark_research/collective_decisions/electoral-engineering/GrWi2016")
source('swing.functions.R')

#####################################################
##### Output directory for figures

if (!dir.exists('figures')){
  dir.create('figures')
} else {
  print("figures/ already exists!")
}

#####################################################
### Data Wrangling
###
### 
sl6816.orig.dta <- read.table('data/102slersuoacontest20181024.csv', header=TRUE,sep="\t")

print(as_tibble(sl6816.orig.dta))

sl6816.dta <- sl6816.orig.dta %>% 
  rename(
    stabb = sab
  ) %>% 
  group_by(stabb, year, sen) %>% 
  filter(all(dtype == 1) & all(etype == "g")) %>% 
  mutate(
    unc = uncont,
    unc.lag = uncontlag,
    v1  = dper/100,
    v1.orig = v1,
    v1 = if_else(unc == TRUE, if_else(dwin == 1, 0.75, 0.25), v1),
    v0  = dperlag/100,
    v0.orig = v0,
    v0 = if_else(unc.lag == TRUE, if_else(dwin == 1, 0.75, 0.25), v0),
    votes = dvote + rvote,
    wd = v1 - dwin / 2,
    decade = cut(
      regime,
      breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2021),
      labels = c('1960s', '1970s', '1980s', '1990s', '2000s', '2010s')
    )
  ) %>% 
  filter(
    dontuse == 0  &
      bigthird == 0 &
      !is.na(v1)
  ) %>%
  mutate(
    nbar = mean(votes),
    td      = votes - nbar) %>% 
  select(stabb, year, sen,  dname, dno, geopost, redist,
         dontuse, dontuselag,
         v1,v1.orig, v0, v0.orig, dvote, rvote, votes, unc.lag, unc, inc, 
         dwin, wd, nbar, td, decade, dtype)

#print(sl6816.dta)


sl6816.unc2.dta <- sl6816.orig.dta %>% 
  rename(
    stabb = sab
  ) %>% 
  group_by(stabb, year, sen) %>% 
  filter(all(dtype == 1) & all(etype == "g")) %>% 
  mutate(
    unc = uncont,
    unc.lag = uncontlag,
    v1  = dper/100,
    v1.orig = v1,
#    v1 = if_else(unc == TRUE, if_else(dwin == 1, 0.75, 0.25), v1),
    v0  = dperlag/100,
    v0.orig = v0,
 #   v0 = if_else(unc.lag == TRUE, if_else(dwin == 1, 0.75, 0.25), v0),
    votes = dvote + rvote,
    wd = v1 - dwin / 2,
    decade = cut(
      regime,
      breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2021),
      labels = c('1960s', '1970s', '1980s', '1990s', '2000s', '2010s')
    )
  ) %>% 
  filter(
    dontuse == 0  &
      bigthird == 0 &
      !is.na(v1)
  ) %>%
  mutate(
    nbar = mean(votes),
    td      = votes - nbar) %>% 
  select(stabb, year, sen,  dname, dno, geopost, redist,
         dontuse, dontuselag,
         v1,v1.orig, v0, v0.orig, dvote, rvote, votes, unc.lag, unc, inc, 
         dwin, wd, nbar, td, decade, dtype)


## Create a lag safe version for prop analysis. Given inconsistent naming convention for districts plus redistricting,
## we can not safely calculate the lag vote share.

sl6816.lag.safe.dta <- sl6816.dta %>%
  group_by(stabb, year, sen) %>% 
  filter(sum(redist) == 0 & !is.na(v0)) 

sl6816.unc2.lag.safe.dta <- sl6816.unc2.dta %>%
  group_by(stabb, year, sen) %>% 
  filter(sum(redist) == 0 & !is.na(v0)) 

#####################################################
### data sets
###


KKR20.dta <- sl6816.lag.safe.dta %>%
  group_by(stabb, year, sen) %>%
  # Drop state elections with too many (greater than a half) uncontested districts
  filter(mean(unc) <= 0.50) %>%
  filter(n() >= 20)  

GW20.dta <-  sl6816.lag.safe.dta %>%
  group_by(stabb, year, sen) 
  
GW20.unc2.dta <-  sl6816.unc2.lag.safe.dta %>%
  group_by(stabb, year, sen) 
  
GW20.cont.dta <-  GW20.dta %>%
  # Drop all uncontested elections everywhere
  filter(unc == 0 & unc.lag == 0) 

#GW20.far.dta <-  GW20.dta %>%
#  filter(v0>0.7 || v0<0.3) 

GW20.close.dta <-  GW20.dta %>%
  filter(v0>0.475 & v0<0.525) 



# GW20 results

stra <- c("ups","prop","pw")
dtaa <- c("",".unc2",".cont")
out <- tibble(matrix(ncol = 8, nrow = 0))
out2 <- tibble(matrix(ncol = 8, nrow = 0))
out3 <- tibble(matrix(ncol = 8, nrow = 0))
out4 <- tibble(matrix(ncol = 8, nrow = 0))
out5 <- tibble(matrix(ncol = 8, nrow = 0))
out6 <- tibble(matrix(ncol = 8, nrow = 0))
#colnames(out) <- c("model","dataset","numrow","winner","sign","bounds","meansquare","rho")

for(dta in dtaa) {
  for (str in stra) {
    df = paste("GW20",dta,".dta", sep="")
    v = tibble(str, df)
    df = eval(as.name(df))
    res <- compute_results(df,str)
    w <- cbind(v, mean(res$error4),mean(res$error3),1-mean(res$error5),mean(res$error^2), cor(res$v1, res$v1h, method = "pearson"), nrow(res))
    out <-rbind(out, w)
    
    df6 = paste("GW20.close",dta,".dta", sep="")
    v6 = tibble(str, df6)
    df6 = eval(as.name(df6))
    res6 <- compute_results(df6,str)
    w <- cbind(v6, mean(res6$error4),mean(res6$error3),1-mean(res6$error5),mean(res6$error^2), cor(res6$v1, res6$v1h, method = "pearson"), nrow(res6))
    out6 <-rbind(out6, w)
    
    
   # print("lower quartile abs swing")
    res2 <- res[res$D <= quantile(res$D, 0.25 , na.rm=TRUE ),]
    res2 <- na.omit(res2)
    w <- cbind(v, mean(res2$error4),mean(res2$error3),1-mean(res2$error5),mean(res2$error^2), cor(res2$v1, res2$v1h, method = "pearson"), nrow(res2))
    out2 <-rbind(out2, w)
    # print("upper quartile abs swing")
    res3 <- res[res$D > quantile(res$D, 0.75 , na.rm=TRUE),]
    res3 <- na.omit(res3)
    w <- cbind(v, mean(res3$error4),mean(res3$error3),1-mean(res3$error5),mean(res3$error^2), cor(res3$v1, res3$v1h, method = "pearson"),nrow(res3))
    out3 <-rbind(out3, w)
    # print("lower quartile abs district swing variance")
    res4 <- res[res$dd <= quantile(res$dd, 0.25, na.rm=TRUE),]
    res4 <- na.omit(res4)
    w <- cbind(v, mean(res4$error4),mean(res4$error3),1-mean(res4$error5),mean(res4$error^2), cor(res4$v1, res4$v1h, method = "pearson"), nrow(res4))
    out4 <-rbind(out4, w)
   # print("upper quartile abs district swing variance")
    res5 <- res[res$dd > quantile(res$dd, 0.75, na.rm=TRUE ),]
    res5 <- na.omit(res5)
    w <- cbind(v, mean(res5$error4),mean(res5$error3),1-mean(res5$error5),mean(res5$error^2), cor(res5$v1, res5$v1h, method = "pearson"),nrow(res5))
    out5 <-rbind(out5, w)
  }
}
colnames(out) <- c("model","dataset","winner","sign","bounds","meansquare","rho", "numrow")
colnames(out2) <- c("model","dataset","winner","sign","bounds","meansquare","rho", "numrow")
colnames(out3) <- c("model","dataset","winner","sign","bounds","meansquare","rho", "numrow")
colnames(out4) <- c("model","dataset","winner","sign","bounds","meansquare","rho", "numrow")
colnames(out5) <- c("model","dataset","winner","sign","bounds","meansquare","rho", "numrow")
colnames(out6) <- c("model","dataset","winner","sign","bounds","meansquare","rho", "numrow")


print(out)
print(out2)
print(out3)
print(out4)
print(out5)

print(out6)

#ggplot(res, aes(res$winner, res$sign, colour = res$model)) + geom_point()
   

#####################################################
### System Info
### For replication

sessionInfo()


