
# THIS SCRIPT OUTPUTS FIGURES S11 AND S13, WHICH PLOTS THE DISTRIBUTION OF 
# WITHIN HEAD OF THE HOUSEHOLD OCCUPATION %DEMOCRAT IN 1940 FOR THE LINKED SAMPLES (FIGURE S11)
# AND THE COMPARISON OF THE MAIN EFFECTS AND THE EFFECTS CONTROLLING FOR WITHIN HEAD OF HOUSEHOLD OCCUPATION % DEMOCRAT (FIGURE S13)

rm(list=ls())

## LOAD PACKAGES
require(sandwich)
require(Hmisc)
require(data.table)
require(lmtest)
require(lfe)
require(gtools)
require(tidyverse)
require(stargazer)
require(reshape)


# LOOP THROUGH 2005/2009 sample, and 2017 sample

## LOAD DATA


###################################
#### 2005/2009 SAMPLE ANALYSIS ####
###################################
load("02-data/befm-main-analysis.Rdata")
df = as.data.table(df)
df = df[(df$state.vfile == 'CA05' & age <=35) |(df$state.vfile == 'NC09' & age <=31) ,]




## bring in occupation data and census data

party.by.occ = fread('02-data/party_by_occupation.csv')
party.by.occ[, occ.dem.group:=cut(pct.dem.40,breaks = quantile(pct.dem.40), include.lowest=T)]

party.by.occ = party.by.occ%>%
  pivot_longer(occ1950_1:occ1950_13)%>%
  filter(!duplicated(value))%>%
  as.data.table


df = merge(df,party.by.occ[,c('value', 'pct.dem.40')], by.x = 'occ1950',by.y='value', all.x=T)

##

##############################
####### MAKE VARIABLES #######
####### FOR ANALYSIS   #######
##############################

df$white = ifelse(df$race == 100, 1, 0) # WHITE INDICATOR

#### FIXED EFFECT VARIABLES

# county
df$county_state = paste0(df$statefip, "_", df$county)  


# enumdist
df$cnty_enumdist = paste0(df$county_state, "_", df$supdist,"_",df$enumdist)
df$cnty_enumdist = ifelse(grepl('NA', df$cnty_enumdist), NA, df$cnty_enumdist)
# 10 page
df$page10 = round(df$pageno, digits = -1)
df$reel_page10 = paste0(df$reel, "_", df$page10)

# 5 page
mround <- function(x,base){ 
  base*round(x/base) }
df$page5 = mround(df$pageno, 5)
df$reel_page5 = paste0(df$reel, "_", df$page5)




# HOMEOWNERSHIP
df$homeowner = ifelse(df$ownershp==10,1,ifelse(df$ownershp==20,0,NA))

### INDICATOR FOR NEIGHBOR WITH HIGH SCHOOL DEGREE

df$hs_dist1 = ifelse((df$hs_opposite1_dist_1 == 1 & df$hs == 0) | (df$hs_opposite1_dist_1 == 0 & df$hs == 1),1,0)
df$hs_dist2 = ifelse((df$hs_opposite1_dist_2 == 1 & df$hs == 0) | (df$hs_opposite1_dist_2 == 0 & df$hs == 1),1,0)
df$hs_dist3 = ifelse((df$hs_opposite1_dist_3 == 1 & df$hs == 0) | (df$hs_opposite1_dist_3 == 0 & df$hs == 1),1,0)
df$hs_dist4 = ifelse((df$hs_opposite1_dist_4 == 1 & df$hs == 0) | (df$hs_opposite1_dist_4 == 0 & df$hs == 1),1,0)
df$hs_dist5 = ifelse((df$hs_opposite1_dist_5 == 1 & df$hs == 0) | (df$hs_opposite1_dist_5 == 0 & df$hs == 1),1,0)
df$hs_dist6 = ifelse((df$hs_opposite1_dist_6 == 1 & df$hs == 0) | (df$hs_opposite1_dist_6 == 0 & df$hs == 1),1,0)
df$hs_dist7 = ifelse((df$hs_opposite1_dist_7 == 1 & df$hs == 0) | (df$hs_opposite1_dist_7 == 0 & df$hs == 1),1,0)
df$hs_dist8 = ifelse((df$hs_opposite1_dist_8 == 1 & df$hs == 0) | (df$hs_opposite1_dist_8 == 0 & df$hs == 1),1,0)
df$hs_dist9 = ifelse((df$hs_opposite1_dist_9 == 1 & df$hs == 0) | (df$hs_opposite1_dist_9 == 0 & df$hs == 1),1,0)
df$hs_dist10 = ifelse((df$hs_opposite1_dist_10 == 1 & df$hs == 0) | (df$hs_opposite1_dist_10 == 0 & df$hs == 1),1,0)


df$employed = ifelse(df$empstat %in% 10:15,1,ifelse(df$empstat%in%20:29,0,NA))

df$incnonwg = ifelse(df$incnonwg==9,NA,df$incnonwg)

plot_data_holder = df[white==1,c('pct.dem.40')]
plot_data_holder[,Sample := '2005/2009 Sample']


#####################################
#### SET COVARIATES FOR FORMULAS ####
#####################################

covars = c( 
  "age", "hs","incwage", "mover","hoh_age", "homeowner",
  "famsize", 'employed',  'hrswork1', 'wkswork1',
  paste0("opposite1_dist_", 1:10), paste0("hs_dist", 1:10), 'pct.dem.40')

# for models with no covariates
nocovars = paste0("opposite1_dist_", 1:10)

#########################
#### ESTIMATE MODELS ####
#########################


## NO FE
fml_dem = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|0|0|county_state",sep=""))
mod_dem1 = felm(formula=fml_dem, data = df[df$white==1,] )
sum_dem1 = summary(mod_dem1, robust =T)



## STATE FE
fml_dem_st = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|statefip|0|county_state",sep=""))
mod_dem_st = felm(formula=fml_dem_st, data = df[df$white==1,] )
sum_dem_st=summary(mod_dem_st, robust =T)

## COUNTY FE
fml_dem_c = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|county_state|0|county_state",sep=""))
mod_dem_c = felm(formula=fml_dem_c, data = df[df$white==1,] )
sum_dem_c=summary(mod_dem_c, robust =T)


## ENUMDIST FE
fml_dem_ed = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|cnty_enumdist|0|county_state",sep=""))
mod_dem_ed= felm(formula=fml_dem_ed, data = df[df$white==1,] )
sum_dem_ed=summary(mod_dem_ed, robust =T)


## REEL FE
fml_dem_r = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel|0|county_state",sep=""))
mod_dem_r = felm(formula=fml_dem_r, data = df[df$white==1,] )
sum_dem_r=summary(mod_dem_r, robust =T)





## REEL PAGE 5 FE
fml_dem_rp5 = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel_page5|0|county_state",sep=""))
mod_dem_rp5= felm(formula=fml_dem_rp5, data = df[df$white==1,] )
sum_dem_rp5=summary(mod_dem_rp5, robust =T)


## REEL PAGE 10 FE
fml_dem_rp10 = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel_page10|0|county_state",sep=""))
mod_dem_rp10 = felm(formula=fml_dem_rp10, data = df[df$white==1,] )
sum_dem_rp10 = summary(mod_dem_rp10, robust =T)



################
#### FIGURE ####  
################

## MAKE PLOT DATA
plot_data = as.data.frame(matrix(c(sum_dem1$coefficients['opposite1_dist_1',c(1,2,4)], "controls",  "none"), ncol =5, nrow=1), stringsAsFactors = F)
colnames(plot_data) = c("coef", "se", "p-value", "covars", 
                        "fe")

plot_data = rbind(plot_data, c(sum_dem_st$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "state"))
plot_data = rbind(plot_data, c(sum_dem_c$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "county"))
plot_data = rbind(plot_data, c(sum_dem_r$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "reel"))
plot_data = rbind(plot_data, c(sum_dem_rp5$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "page-5"))
plot_data = rbind(plot_data, c(sum_dem_rp10$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "page-10"))
plot_data = rbind(plot_data, c(sum_dem_ed$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "enumdist"))


plot_data$coef=as.numeric(plot_data$coef)
plot_data$se=as.numeric(plot_data$se)
plot_data$`p-value`=as.numeric(plot_data$`p-value`)
#plot_data$covars = factor(plot_data$covars, levels = c("no controls", "controls"))
plot_data$fe=factor(plot_data$fe, levels = c("none", "state", "county", "enumdist", "reel","page-10",
                                             "page-5"))


plot_data_0509 = plot_data


rm(list=ls()[!grepl('plot_data', ls())])
gc()


##############################
#### 2017 SAMPLE ANALYSIS ####
##############################

load("02-data/seg_analysis_2017.Rdata")
df=as.data.table(df)



## bring in occupation data and census data

party.by.occ = fread('02-data/party_by_occupation.csv')
party.by.occ[, occ.dem.group:=cut(pct.dem.40,breaks = quantile(pct.dem.40), include.lowest=T)]

party.by.occ = party.by.occ%>%
  pivot_longer(occ1950_1:occ1950_13)%>%
  filter(!duplicated(value))%>%
  as.data.table


df = merge(df,party.by.occ[,c('value', 'pct.dem.40')], by.x = 'occ1950',by.y='value', all.x=T)

##



df$white = ifelse(df$race == 100, 1, 0) # WHITE INDICATOR

#### FIXED EFFECT VARIABLES

# county
df$county_state = paste0(df$statefip, "_", df$county)  

# enumdist
df$cnty_enumdist = paste0(df$county_state, "_", df$supdist,"_",df$enumdist)
df$cnty_enumdist = ifelse(grepl('NA', df$cnty_enumdist), NA, df$cnty_enumdist)

# 10 page
df$page10 = round(df$pageno, digits = -1)
df$reel_page10 = paste0(df$reel, "_", df$page10)

# 5 page
mround <- function(x,base){ 
  base*round(x/base) }
df$page5 = mround(df$pageno, 5)
df$reel_page5 = paste0(df$reel, "_", df$page5)

# HOMEOWNERSHIP
df$homeowner = ifelse(df$ownershp==10,1,ifelse(df$ownershp==20,0,NA))


### INDICATOR FOR NEIGHBOR WITH HIGH SCHOOL DEGREE

df$hs_dist1 = ifelse((df$hs_opposite1_dist_1 == 1 & df$hs == 0) | (df$hs_opposite1_dist_1 == 0 & df$hs == 1),1,0)
df$hs_dist2 = ifelse((df$hs_opposite1_dist_2 == 1 & df$hs == 0) | (df$hs_opposite1_dist_2 == 0 & df$hs == 1),1,0)
df$hs_dist3 = ifelse((df$hs_opposite1_dist_3 == 1 & df$hs == 0) | (df$hs_opposite1_dist_3 == 0 & df$hs == 1),1,0)
df$hs_dist4 = ifelse((df$hs_opposite1_dist_4 == 1 & df$hs == 0) | (df$hs_opposite1_dist_4 == 0 & df$hs == 1),1,0)
df$hs_dist5 = ifelse((df$hs_opposite1_dist_5 == 1 & df$hs == 0) | (df$hs_opposite1_dist_5 == 0 & df$hs == 1),1,0)
df$hs_dist6 = ifelse((df$hs_opposite1_dist_6 == 1 & df$hs == 0) | (df$hs_opposite1_dist_6 == 0 & df$hs == 1),1,0)
df$hs_dist7 = ifelse((df$hs_opposite1_dist_7 == 1 & df$hs == 0) | (df$hs_opposite1_dist_7 == 0 & df$hs == 1),1,0)
df$hs_dist8 = ifelse((df$hs_opposite1_dist_8 == 1 & df$hs == 0) | (df$hs_opposite1_dist_8 == 0 & df$hs == 1),1,0)
df$hs_dist9 = ifelse((df$hs_opposite1_dist_9 == 1 & df$hs == 0) | (df$hs_opposite1_dist_9 == 0 & df$hs == 1),1,0)
df$hs_dist10 = ifelse((df$hs_opposite1_dist_10 == 1 & df$hs == 0) | (df$hs_opposite1_dist_10 == 0 & df$hs == 1),1,0)



df$employed = ifelse(df$empstat %in% 10:15,1,ifelse(df$empstat%in%20:29,0,NA))

df$incnonwg = ifelse(df$incnonwg==9,NA,df$incnonwg)


plot_data_holder = rbind(plot_data_holder,df[white==1,c('pct.dem.40')],fill=T)
plot_data_holder[is.na(Sample),Sample := '2017 Sample']


#####################################
#### SET COVARIATES FOR FORMULAS ####
#####################################

# for models with covariates
covars = c( 
  "age", "hs","incwage", "mover","hoh_age", "homeowner",
  "famsize", 'employed', 'hrswork1', 'wkswork1',
  paste0("opposite1_dist_", 1:10), paste0("hs_dist", 1:10), 'pct.dem.40')


#########################
#### ESTIMATE MODELS ####
#########################


## NO FE
fml_dem = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|0|0|county_state",sep=""))
mod_dem1 = felm(formula=fml_dem, data = df[df$white==1,] )
sum_dem1 = summary(mod_dem1, robust =T)



## STATE FE
fml_dem_st = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|statefip|0|county_state",sep=""))
mod_dem_st = felm(formula=fml_dem_st, data = df[df$white==1,] )
sum_dem_st=summary(mod_dem_st, robust =T)


## COUNTY FE
fml_dem_c = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|county_state|0|county_state",sep=""))
mod_dem_c = felm(formula=fml_dem_c, data = df[df$white==1,] )
sum_dem_c=summary(mod_dem_c, robust =T)


## ENUMDIST FE
fml_dem_ed = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|cnty_enumdist|0|county_state",sep=""))
mod_dem_ed= felm(formula=fml_dem_ed, data = df[df$white==1,] )
sum_dem_ed=summary(mod_dem_ed, robust =T)



## REEL FE
fml_dem_r = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel|0|county_state",sep=""))
mod_dem_r = felm(formula=fml_dem_r, data = df[df$white==1,] )
sum_dem_r=summary(mod_dem_r, robust =T)




## REEL PAGE 5 FE
fml_dem_rp5 = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel_page5|0|county_state",sep=""))
mod_dem_rp5= felm(formula=fml_dem_rp5, data = df[df$white==1,] )
sum_dem_rp5=summary(mod_dem_rp5, robust =T)


## REEL PAGE 10 FE
fml_dem_rp10 = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel_page10|0|county_state",sep=""))
mod_dem_rp10 = felm(formula=fml_dem_rp10, data = df[df$white==1,] )
sum_dem_rp10 = summary(mod_dem_rp10, robust =T)




################
#### FIGURE ####  
################

## MAKE PLOT DATA
plot_data = as.data.frame(matrix(c(sum_dem1$coefficients['opposite1_dist_1',c(1,2,4)], "controls",  "none"), ncol =5, nrow=1), stringsAsFactors = F)
colnames(plot_data) = c("coef", "se", "p-value", "covars", 
                        "fe")

plot_data = rbind(plot_data, c(sum_dem_st$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "state"))
plot_data = rbind(plot_data, c(sum_dem_c$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "county"))
plot_data = rbind(plot_data, c(sum_dem_r$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "reel"))
plot_data = rbind(plot_data, c(sum_dem_rp5$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "page-5"))
plot_data = rbind(plot_data, c(sum_dem_rp10$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "page-10"))
plot_data = rbind(plot_data, c(sum_dem_ed$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "enumdist"))

plot_data$coef=as.numeric(plot_data$coef)
plot_data$se=as.numeric(plot_data$se)
plot_data$`p-value`=as.numeric(plot_data$`p-value`)
# plot_data$covars = factor(plot_data$covars, levels = c("no controls", "controls"))
plot_data$fe=factor(plot_data$fe, levels = c("none", "state", "county", "enumdist", "reel","page-10",
                                             "page-5"))



plot_data_17 = plot_data




rm(list=ls()[!grepl('plot_data', ls())])
gc()



############################################
#### CREATE PLOT OF BOTH SAMPLE EFFECTS ####
############################################

plot_data1 = plot_data_0509 %>%
  as_tibble %>%
  mutate(year = '2005/2009 Sample') %>%
  bind_rows(plot_data_17 %>%
              as_tibble %>%
              mutate(year = '2017 Sample'))%>%
  mutate(fe = capitalize(as.character(fe)),
         fe = ifelse(fe=='Enumdist','District',fe),
         fe = factor(fe, levels = c('None', 'State', 'County', 'Reel', 'District', 'Page-10', 'Page-5', 'Pair', 'Page')),
         Control = 'Partisan Occupation Control')



##### RE-CALCULATE MAIN EFFECTS FOR COMPARISON
###################################
#### 2005/2009 SAMPLE ANALYSIS ####
###################################
load("02-data/befm-main-analysis.Rdata")
df = as.data.table(df)
df = df[(df$state.vfile == 'CA05' & age <=35) |(df$state.vfile == 'NC09' & age <=31) ,]





##

##############################
####### MAKE VARIABLES #######
####### FOR ANALYSIS   #######
##############################

df$white = ifelse(df$race == 100, 1, 0) # WHITE INDICATOR

#### FIXED EFFECT VARIABLES

# county
df$county_state = paste0(df$statefip, "_", df$county)  

# enumdist
df$cnty_enumdist = paste0(df$county_state, "_", df$supdist,"_",df$enumdist)
df$cnty_enumdist = ifelse(grepl('NA', df$cnty_enumdist), NA, df$cnty_enumdist)
# 10 page
df$page10 = round(df$pageno, digits = -1)
df$reel_page10 = paste0(df$reel, "_", df$page10)

# 5 page
mround <- function(x,base){ 
  base*round(x/base) }
df$page5 = mround(df$pageno, 5)
df$reel_page5 = paste0(df$reel, "_", df$page5)


# HOMEOWNERSHIP
df$homeowner = ifelse(df$ownershp==10,1,ifelse(df$ownershp==20,0,NA))

### INDICATOR FOR NEIGHBOR WITH HIGH SCHOOL DEGREE

df$hs_dist1 = ifelse((df$hs_opposite1_dist_1 == 1 & df$hs == 0) | (df$hs_opposite1_dist_1 == 0 & df$hs == 1),1,0)
df$hs_dist2 = ifelse((df$hs_opposite1_dist_2 == 1 & df$hs == 0) | (df$hs_opposite1_dist_2 == 0 & df$hs == 1),1,0)
df$hs_dist3 = ifelse((df$hs_opposite1_dist_3 == 1 & df$hs == 0) | (df$hs_opposite1_dist_3 == 0 & df$hs == 1),1,0)
df$hs_dist4 = ifelse((df$hs_opposite1_dist_4 == 1 & df$hs == 0) | (df$hs_opposite1_dist_4 == 0 & df$hs == 1),1,0)
df$hs_dist5 = ifelse((df$hs_opposite1_dist_5 == 1 & df$hs == 0) | (df$hs_opposite1_dist_5 == 0 & df$hs == 1),1,0)
df$hs_dist6 = ifelse((df$hs_opposite1_dist_6 == 1 & df$hs == 0) | (df$hs_opposite1_dist_6 == 0 & df$hs == 1),1,0)
df$hs_dist7 = ifelse((df$hs_opposite1_dist_7 == 1 & df$hs == 0) | (df$hs_opposite1_dist_7 == 0 & df$hs == 1),1,0)
df$hs_dist8 = ifelse((df$hs_opposite1_dist_8 == 1 & df$hs == 0) | (df$hs_opposite1_dist_8 == 0 & df$hs == 1),1,0)
df$hs_dist9 = ifelse((df$hs_opposite1_dist_9 == 1 & df$hs == 0) | (df$hs_opposite1_dist_9 == 0 & df$hs == 1),1,0)
df$hs_dist10 = ifelse((df$hs_opposite1_dist_10 == 1 & df$hs == 0) | (df$hs_opposite1_dist_10 == 0 & df$hs == 1),1,0)


df$employed = ifelse(df$empstat %in% 10:15,1,ifelse(df$empstat%in%20:29,0,NA))

df$incnonwg = ifelse(df$incnonwg==9,NA,df$incnonwg)


#####################################
#### SET COVARIATES FOR FORMULAS ####
#####################################

covars = c( 
  "age", "hs","incwage", "mover","hoh_age", "homeowner",
  "famsize", 'employed', 'hrswork1', 'wkswork1',
  paste0("opposite1_dist_", 1:10), paste0("hs_dist", 1:10))

# for models with no covariates
nocovars = paste0("opposite1_dist_", 1:10)

#########################
#### ESTIMATE MODELS ####
#########################


## NO FE
fml_dem = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|0|0|county_state",sep=""))
mod_dem1 = felm(formula=fml_dem, data = df[df$white==1,] )
sum_dem1 = summary(mod_dem1, robust =T)



## STATE FE
fml_dem_st = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|statefip|0|county_state",sep=""))
mod_dem_st = felm(formula=fml_dem_st, data = df[df$white==1,] )
sum_dem_st=summary(mod_dem_st, robust =T)

## COUNTY FE
fml_dem_c = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|county_state|0|county_state",sep=""))
mod_dem_c = felm(formula=fml_dem_c, data = df[df$white==1,] )
sum_dem_c=summary(mod_dem_c, robust =T)


## ENUMDIST FE
fml_dem_ed = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|cnty_enumdist|0|county_state",sep=""))
mod_dem_ed= felm(formula=fml_dem_ed, data = df[df$white==1,] )
sum_dem_ed=summary(mod_dem_ed, robust =T)


## REEL FE
fml_dem_r = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel|0|county_state",sep=""))
mod_dem_r = felm(formula=fml_dem_r, data = df[df$white==1,] )
sum_dem_r=summary(mod_dem_r, robust =T)





## REEL PAGE 5 FE
fml_dem_rp5 = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel_page5|0|county_state",sep=""))
mod_dem_rp5= felm(formula=fml_dem_rp5, data = df[df$white==1,] )
sum_dem_rp5=summary(mod_dem_rp5, robust =T)


## REEL PAGE 10 FE
fml_dem_rp10 = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel_page10|0|county_state",sep=""))
mod_dem_rp10 = felm(formula=fml_dem_rp10, data = df[df$white==1,] )
sum_dem_rp10 = summary(mod_dem_rp10, robust =T)



################
#### FIGURE ####  
################

## MAKE PLOT DATA
plot_data = as.data.frame(matrix(c(sum_dem1$coefficients['opposite1_dist_1',c(1,2,4)], "controls",  "none"), ncol =5, nrow=1), stringsAsFactors = F)
colnames(plot_data) = c("coef", "se", "p-value", "covars", 
                        "fe")

plot_data = rbind(plot_data, c(sum_dem_st$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "state"))
plot_data = rbind(plot_data, c(sum_dem_c$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "county"))
plot_data = rbind(plot_data, c(sum_dem_r$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "reel"))
plot_data = rbind(plot_data, c(sum_dem_rp5$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "page-5"))
plot_data = rbind(plot_data, c(sum_dem_rp10$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "page-10"))
plot_data = rbind(plot_data, c(sum_dem_ed$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "enumdist"))


plot_data$coef=as.numeric(plot_data$coef)
plot_data$se=as.numeric(plot_data$se)
plot_data$`p-value`=as.numeric(plot_data$`p-value`)
#plot_data$covars = factor(plot_data$covars, levels = c("no controls", "controls"))
plot_data$fe=factor(plot_data$fe, levels = c("none", "state", "county", "enumdist", "reel","page-10",
                                             "page-5"))


plot_data_0509 = plot_data


rm(list=ls()[!grepl('plot_data', ls())])
gc()


##############################
#### 2017 SAMPLE ANALYSIS ####
##############################

load("02-data/seg_analysis_2017.Rdata")
df=as.data.table(df)





df$white = ifelse(df$race == 100, 1, 0) # WHITE INDICATOR

#### FIXED EFFECT VARIABLES

# county
df$county_state = paste0(df$statefip, "_", df$county)  

# enumdist
df$cnty_enumdist = paste0(df$county_state, "_", df$supdist,"_",df$enumdist)
df$cnty_enumdist = ifelse(grepl('NA', df$cnty_enumdist), NA, df$cnty_enumdist)

# 10 page
df$page10 = round(df$pageno, digits = -1)
df$reel_page10 = paste0(df$reel, "_", df$page10)

# 5 page
mround <- function(x,base){ 
  base*round(x/base) }
df$page5 = mround(df$pageno, 5)
df$reel_page5 = paste0(df$reel, "_", df$page5)

# HOMEOWNERSHIP
df$homeowner = ifelse(df$ownershp==10,1,ifelse(df$ownershp==20,0,NA))


### INDICATOR FOR NEIGHBOR WITH HIGH SCHOOL DEGREE


df$hs_dist1 = ifelse((df$hs_opposite1_dist_1 == 1 & df$hs == 0) | (df$hs_opposite1_dist_1 == 0 & df$hs == 1),1,0)
df$hs_dist2 = ifelse((df$hs_opposite1_dist_2 == 1 & df$hs == 0) | (df$hs_opposite1_dist_2 == 0 & df$hs == 1),1,0)
df$hs_dist3 = ifelse((df$hs_opposite1_dist_3 == 1 & df$hs == 0) | (df$hs_opposite1_dist_3 == 0 & df$hs == 1),1,0)
df$hs_dist4 = ifelse((df$hs_opposite1_dist_4 == 1 & df$hs == 0) | (df$hs_opposite1_dist_4 == 0 & df$hs == 1),1,0)
df$hs_dist5 = ifelse((df$hs_opposite1_dist_5 == 1 & df$hs == 0) | (df$hs_opposite1_dist_5 == 0 & df$hs == 1),1,0)
df$hs_dist6 = ifelse((df$hs_opposite1_dist_6 == 1 & df$hs == 0) | (df$hs_opposite1_dist_6 == 0 & df$hs == 1),1,0)
df$hs_dist7 = ifelse((df$hs_opposite1_dist_7 == 1 & df$hs == 0) | (df$hs_opposite1_dist_7 == 0 & df$hs == 1),1,0)
df$hs_dist8 = ifelse((df$hs_opposite1_dist_8 == 1 & df$hs == 0) | (df$hs_opposite1_dist_8 == 0 & df$hs == 1),1,0)
df$hs_dist9 = ifelse((df$hs_opposite1_dist_9 == 1 & df$hs == 0) | (df$hs_opposite1_dist_9 == 0 & df$hs == 1),1,0)
df$hs_dist10 = ifelse((df$hs_opposite1_dist_10 == 1 & df$hs == 0) | (df$hs_opposite1_dist_10 == 0 & df$hs == 1),1,0)



df$employed = ifelse(df$empstat %in% 10:15,1,ifelse(df$empstat%in%20:29,0,NA))

df$incnonwg = ifelse(df$incnonwg==9,NA,df$incnonwg)
#####################################
#### SET COVARIATES FOR FORMULAS ####
#####################################

# for models with covariates
covars = c( 
  "age", "hs","incwage", "mover","hoh_age", "homeowner",
  "famsize", 'employed',  'hrswork1', 'wkswork1',
  paste0("opposite1_dist_", 1:10), paste0("hs_dist", 1:10))


#########################
#### ESTIMATE MODELS ####
#########################


## NO FE
fml_dem = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|0|0|county_state",sep=""))
mod_dem1 = felm(formula=fml_dem, data = df[df$white==1,] )
sum_dem1 = summary(mod_dem1, robust =T)



## STATE FE
fml_dem_st = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|statefip|0|county_state",sep=""))
mod_dem_st = felm(formula=fml_dem_st, data = df[df$white==1,] )
sum_dem_st=summary(mod_dem_st, robust =T)


## COUNTY FE
fml_dem_c = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|county_state|0|county_state",sep=""))
mod_dem_c = felm(formula=fml_dem_c, data = df[df$white==1,] )
sum_dem_c=summary(mod_dem_c, robust =T)


## ENUMDIST FE
fml_dem_ed = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|cnty_enumdist|0|county_state",sep=""))
mod_dem_ed= felm(formula=fml_dem_ed, data = df[df$white==1,] )
sum_dem_ed=summary(mod_dem_ed, robust =T)



## REEL FE
fml_dem_r = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel|0|county_state",sep=""))
mod_dem_r = felm(formula=fml_dem_r, data = df[df$white==1,] )
sum_dem_r=summary(mod_dem_r, robust =T)




## REEL PAGE 5 FE
fml_dem_rp5 = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel_page5|0|county_state",sep=""))
mod_dem_rp5= felm(formula=fml_dem_rp5, data = df[df$white==1,] )
sum_dem_rp5=summary(mod_dem_rp5, robust =T)


## REEL PAGE 10 FE
fml_dem_rp10 = as.formula(paste(paste("democrat ~ ", paste(covars, collapse= "+")),"|reel_page10|0|county_state",sep=""))
mod_dem_rp10 = felm(formula=fml_dem_rp10, data = df[df$white==1,] )
sum_dem_rp10 = summary(mod_dem_rp10, robust =T)




################
#### FIGURE ####  
################

## MAKE PLOT DATA
plot_data = as.data.frame(matrix(c(sum_dem1$coefficients['opposite1_dist_1',c(1,2,4)], "controls",  "none"), ncol =5, nrow=1), stringsAsFactors = F)
colnames(plot_data) = c("coef", "se", "p-value", "covars", 
                        "fe")

plot_data = rbind(plot_data, c(sum_dem_st$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "state"))
plot_data = rbind(plot_data, c(sum_dem_c$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "county"))
plot_data = rbind(plot_data, c(sum_dem_r$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "reel"))
plot_data = rbind(plot_data, c(sum_dem_rp5$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "page-5"))
plot_data = rbind(plot_data, c(sum_dem_rp10$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "page-10"))
plot_data = rbind(plot_data, c(sum_dem_ed$coefficients['opposite1_dist_1', c(1,2,4)], "controls",  "enumdist"))

plot_data$coef=as.numeric(plot_data$coef)
plot_data$se=as.numeric(plot_data$se)
plot_data$`p-value`=as.numeric(plot_data$`p-value`)
# plot_data$covars = factor(plot_data$covars, levels = c("no controls", "controls"))
plot_data$fe=factor(plot_data$fe, levels = c("none", "state", "county", "enumdist", "reel","page-10",
                                             "page-5"))



plot_data_17 = plot_data




rm(list=ls()[!grepl('plot_data', ls())])
gc()



############################################
#### CREATE PLOT OF BOTH SAMPLE EFFECTS ####
############################################

plot_data2 = plot_data_0509 %>%
  as_tibble %>%
  mutate(year = '2005/2009 Sample') %>%
  bind_rows(plot_data_17 %>%
              as_tibble %>%
              mutate(year = '2017 Sample'))%>%
  mutate(fe = capitalize(as.character(fe)),
         fe = ifelse(fe=='Enumdist','District',fe),
         fe = factor(fe, levels = c('None', 'State', 'County', 'Reel', 'District', 'Page-10', 'Page-5', 'Pair', 'Page')),
         Control = 'Main Specification')

plot_data = plot_data1 %>% bind_rows(plot_data2)

#####
## LOAD AESTHETIC
source("01-code/r_utils.R")
require(scales)
cols=c('#66CCFF',  '#6699FF', '#3366CC', '#3366FF',
       '#0033CC', '#0000CC', '#000099', '#000066', '#000033')
## MAKE PLOT
p=ggplot(data = plot_data %>% filter(covars=='controls' & fe %in% c('None', 'State', 'County', 'Reel', 'District', 'Page-10', 'Page-5')), aes(x = fe, y = coef,shape= Control, color = as.numeric(fe),fill = as.numeric(fe))) +
  scale_shape_manual(name = '', values = c(15, 16)) +
  xlab('')+
  ylab('Black Next Door Neighbor Coefficient') + geom_hline(yintercept = 0, alpha = .5, linetype = "dashed")+
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se), width=0, size = 1.5, 
                position=position_dodge(.5), alpha=.75) + 
  geom_point(size = 4, position=position_dodge(.5), fill = "white")  +# scale_y_continuous(limits=c(-0.01,0.3),oob=squish)+
  guides(alpha=NULL, fill=NULL)+
  theme_shom()+
  theme(text = element_text(size=18), legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color='none', fill = 'none')+
  facet_wrap(year~.,nrow = 2, strip.position = 'top', scales = 'free_y')+
  NULL

## SAVE PLOT
ggsave("03-output/01-plots/FigS13.jpeg", plot = p, width = 9, height = 5, units = 'in', dpi=600)


## FIGURE S11 HISTOGRAM
source("01-code/r_utils.R")
require(scales)
p = ggplot(plot_data_holder, aes(x=pct.dem.40))+
  geom_histogram(aes(y=..density..), binwidth=.1)+
  theme_shom()+
  xlab('Proportion Democrat in Occupation')+
  ylab('Density')+
  scale_y_continuous(labels=comma)+
  facet_grid(cols = vars(Sample))



ggsave("03-output/01-plots/FigS11.jpeg", plot = p, width = 9, height = 5, units = 'in', dpi=600)


