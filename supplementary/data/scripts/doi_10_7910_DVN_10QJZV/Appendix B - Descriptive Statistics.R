###########################
## FORMATION DESCRIPTIVE STATISTICS
## Version: 1 September 2020
##########################
rm(list=ls(all=TRUE))
options(warn=-1)
require(texreg)
require(lmtest)
require(multiwayvcov)
require(DataCombine)

#Data Appendix
df = read.csv("blattmanshocksreplication2.csv")
#Country-Level Data

##
library(dplyr)

df2 = df %>% group_by(ccode, year) %>% mutate(min = ifelse(anynew==1, min(year), 2012))
df3 = df2 %>% group_by(ccode) %>% mutate(min2 = min(min))

df3$onset_form = ifelse(df3$year == df3$min2, 1, NA)
df3$onset_form[df3$year < df3$min2] = 0
df3$noonset_form = ifelse(df3$year == df3$min2, df3$nonew, NA)
df3$noonset_form[df3$year < df3$min2] = 0

df2 = df %>% group_by(ccode, year) %>% mutate(min = ifelse(newsep==1, min(year), 2012))
dfsep = df2 %>% group_by(ccode) %>% mutate(min2 = min(min))

dfsep$onset_sepform = ifelse(dfsep$year == dfsep$min2, 1, NA)
dfsep$onset_sepform[dfsep$year < dfsep$min2] = 0


df2 = df %>% group_by(ccode, year) %>% mutate(min = ifelse(newcenter==1, min(year), 2012))
dfcenter = df2 %>% group_by(ccode) %>% mutate(min2 = min(min))

dfcenter$onset_centerform = ifelse(dfcenter$year == dfcenter$min2, 1, NA)
dfcenter$onset_centerform[dfcenter$year < dfcenter$min2] = 0

formincid_df = with(df, data.frame(anynew, nonew))
formonset_df = with(df3, data.frame(onset_form, noonset_form))

formincid_df = with(df, data.frame(anynew, nonew))
formonset_df = with(df3, data.frame(onset_form, noonset_form))

outcomedf = with(df, data.frame(anynew[is.na(df$pshock_npi_p)==FALSE], nonew[is.na(df$pshock_npi_p)==FALSE]))
onsetdf = with(df3, data.frame(onset_form[is.na(df3$pshock_npi_p)==FALSE], noonset_form[is.na(df3$pshock_npi_p)==FALSE]))

preag = with(df, data.frame(l1noactform, I(l1noactform>0)))

## PRICE SHOCK VARIABLE DISTRIBUTION
pshock_df = with(df, data.frame(pshock_npi_p, pshock_npi_p1, pshock_npi_p2))
require(stargazer)
stargazer(pshock_df, median=T)



## FORMATION OUTCOME VARIABLE DISTRIBUTION
require(stargazer)
formdf = rbind(formincid_df, formonset_df)
stargazer(formincid_df, formonset_df, median=T)

## CIVIL WAR OUTCOME VARIABLE DISTRIBUTION
require(stargazer)
war_df = with(df, data.frame(onset_any_prio, any_prio, onset_war_prio_bb, war_prio_bb))
stargazer(formincid_df, formonset_df, war_df, median=T)

## PRE-EXISTING AG VARIABLE DISTRIBUTION
stargazer(preag, median=T)


#####
# Add histogram of number of militant campaigns
hist(log(df$l1noactform+1), xlim=c(0, 5))
hist(df$l1noactform, xlim=c(0, 60))
hist(df$l1noactform, xlim=c(0, 10), breaks=69, main="Number of Pre-Existing Militant Campaigns")
table(I(df$l1noactform>0))

#####

#GROUP-LEVEL STATISTICS

## 
df = read.csv("/users/irismalone/Dropbox/Dissertation/Data/TORG/January/agdgroupfullv2.csv")
list_of_ccode = c(2, 210, 211, 212, 220, 225, 232, 255, 260, 265, 325, 350)

df = subset(df, !(df$ccode %in% list_of_ccode))
df$ccode[df$ccode==678] = 679


df$cown = df$ccode
dfname = with(codelist, data.frame(cown, country.name.en, un.name.en, un.region.name))
#dfname$ccode[dfname$cown==678] = 679

#fix yemen

df = merge(df, dfname,id="cown", all=T)
df$noact[is.na(df$noact)==TRUE] = 0
df$nosep[is.na(df$nosep)==TRUE] = 0
df$nocenter[is.na(df$nocenter)==TRUE ] = 0
df$nocenterinsurg[is.na(df$nocenterinsurg)==TRUE] = 0
df$nosepinsurg[is.na(df$nosepinsurg)==TRUE] = 0

dim(df)
df = subset(df, df$polopp==1)
df$year1 = ifelse(is.na(df$yrform)==TRUE, df$yrfirstattack, df$yrform)
df = subset(df, df$year1 >= 1970)
length(unique(df$torgid[df$polopp==1]))
length(unique(df$torgid[df$polopp==1 & df$civwar==1]))
df$civwar1000 = ifelse(df$civwar == 1 & df$cumint == 2, 1, 0)
require(ggplot2)
df = with(df, data.frame(ccode, torgid, year1, civwar, civwar1000, aim2, americas, ssa, mena, asia, europe))
length(unique(df$torgid))
df$region[df$europe==1]="Europe"
df$region[df$mena==1]="Middle East/North Africa"
df$region[df$ssa==1]="Sub-Saharan Africa"
df$region[df$asia==1]="Asia"
df$region[df$americas == 1 ] = "Americas"

table(df$region) #number of AG per region
table(length(unique(df$ccode[df$americas==1])))
table(length(unique(df$ccode[df$asia==1])))
table(length(unique(df$ccode[df$mena==1])))
table(length(unique(df$ccode[df$ssa==1])))
table(length(unique(df$ccode[df$europe==1])))

#Base Table
dfnoact  = df %>% group_by(region) %>%
  summarise(ccode = n_distinct(ccode),
            ag = n_distinct(torgid),
            cc = n_distinct(torgid[civwar==1]),
            cw = n_distinct(torgid[civwar1000==1]))

xtable(dfnoact)

table(df3$region, df3$noact)
table(df3$region, df3$noonset)
table(df3$region, df3$noonset1000)


require(stargazer)
stargazer(df)

dfblattman = read.csv("/Users/irismalone/Dropbox/Dissertation/Data/BlattmanShocks/CommodityCountryYearv2.csv")
dfblattman = subset(dfblattman, dfblattman$year > 1969 & is.na(dfblattman$pshock_npi)==FALSE)
dfblattmanccode  = dfblattman %>% distinct(ccode)
dfblattmanccode$pshockcode = 1
df3 = merge(df, dfblattmanccode, by=c("ccode"), all=T)
df3 = subset(df3, df3$year1 < 2008 & df3$year1 >= 1970)
df3 = with(df3, data.frame(ccode, torgid, year1, civwar, civwar1000, aim2, region, americas, ssa, mena, asia, europe))
length(unique(df3$ccode))
length(unique(df3$torgid)) 
length(unique(df3$torgid[df3$civwar==1]))

require(stargazer)
stargazer(df3)

table(df3$region) #number of AG per region
#number of countries/per region
summary(df3$region)

table(length(unique(df3$ccode[df3$americas==1])))
table(length(unique(df3$ccode[df3$asia==1])))
table(length(unique(df3$ccode[df3$mena==1])))
table(length(unique(df3$ccode[df3$ssa==1])))
table(length(unique(df3$ccode[df3$europe==1])))

#GROUP DESCRIPTIVE STATISTICS
require(ggplot2)
suppressMessages(require(maps))
suppressWarnings(suppressMessages(require(ggmap)))
suppressMessages(require(mapproj))
suppressMessages(require(countrycode))
suppressWarnings(suppressMessages(require(dplyr)))
world = map_data("world")
df = read.csv("/Users/irismalone/Dropbox/Dissertation/Data/TORG/January/fullsamplebycountryv2.csv")

list_of_ccode = c(2, 210, 211, 212, 220, 225, 232, 255, 260, 265, 325, 350)

df = subset(df, !(df$ccode %in% list_of_ccode))
df$ccode[df$ccode==678] = 679


df$cown = df$ccode
dfname = with(codelist, data.frame(cown, country.name.en, un.name.en, un.region.name))
#dfname$ccode[dfname$cown==678] = 679

#fix yemen

df3 = merge(df, dfname,id="cown", all=T)
df3$noact[is.na(df3$noact)==TRUE] = 0
df3$nosep[is.na(df3$nosep)==TRUE] = 0
df3$nocenter[is.na(df3$nocenter)==TRUE ] = 0
df3$nocenterinsurg[is.na(df3$nocenterinsurg)==TRUE] = 0
df3$nosepinsurg[is.na(df3$nosepinsurg)==TRUE] = 0

dim(df3)
#df3 = subset(df3, df3$europe==0)
dim(df3)
df3$propcivwar = ifelse(df3$noact == 0, 0, df3$noonset/(df3$noact))
df3$propcentercivwar = ifelse(df3$nocenter == 0, 0, df3$nocenterinsurg/(df3$nocenter))
df3$propsepcivwar = ifelse(df3$nosep == 0, 0, df3$nosepinsurg/(df3$nosep))

df3$un.region.name[df3$mena==1] = "Middle East/North Africa"
dfblattman = read.csv("/Users/irismalone/Dropbox/Dissertation/Data/BlattmanShocks/CommodityCountryYearv2.csv")
dfblattman = subset(dfblattman, dfblattman$year > 1969 & is.na(dfblattman$pshock_npi)==FALSE)
length(unique(dfblattman$ccode))
dfblattman$cown = dfblattman$ccode
dfblattmanccode  = dfblattman %>% distinct(cown)
#fix yemen

dfblattmanccode$pshockcode = 1
dim(dfblattmanccode)

df3 = merge(df3, dfblattmanccode, by=c("cown"), all.y=T)

tapply(df3$ccode, df3$un.region.name, length) #total number of countries in data
tapply(df3$ccode[df3$pshockcode==1], df3$un.region.name[df3$pshockcode==1], length) #total number of countries in data

d =aggregate(noact ~ un.region.name, df3, FUN = summary)
xtable(do.call(data.frame, c(d, check.names = FALSE)))


d =aggregate(noonset ~ un.region.name, df3, FUN = summary)
xtable(do.call(data.frame, c(d, check.names = FALSE)))

d =aggregate(nocenter ~ un.region.name, df3, FUN = summary)
xtable(do.call(data.frame, c(d, check.names = FALSE)))

d =aggregate(nosep ~ un.region.name, df3, FUN = summary)
xtable(do.call(data.frame, c(d, check.names = FALSE)))

d =aggregate(propcivwar ~ un.region.name, df3, FUN = mean)
xtable(do.call(data.frame, c(d, check.names = FALSE)))

d =aggregate(propsepcivwar ~ un.region.name, df3, FUN = mean)
xtable(do.call(data.frame, c(d, check.names = FALSE)))

d =aggregate(propcentercivwar ~ un.region.name, df3, FUN = mean)
xtable(do.call(data.frame, c(d, check.names = FALSE)))

