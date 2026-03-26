### THIS SCRIPT OUTPUTS THE SENSITIVITY ANALYSIS FIGURE IN FIGURE S9

rm(list=ls())

## LOAD PACKAGES
library(sandwich)
library(Hmisc)
library(data.table)
library(lmtest)
library(lfe)
library(gtools)
library(tidyverse)
library(stargazer)
library(reshape)
library(causalsens)
library(sensemakr)



## LOAD DATA


###################################
#### 2005/2009 SAMPLE ANALYSIS ####
###################################
load("02-data/befm-main-analysis.Rdata")
df = as.data.table(df)
df = df[(df$state.vfile == 'CA05' & age <=35) |(df$state.vfile == 'NC09' & age <=31),]
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

fml_dem_nocovar = as.formula(paste(paste("democrat ~ ", paste(nocovars, collapse= "+")),"|0|0|county_state",sep=""))
mod_dem1_nocovar = felm(formula=fml_dem_nocovar, data = df[df$white==1,] )
sum_dem1_nocovar = summary(mod_dem1_nocovar, robust =T)


## MAKE MODELS FOR SENSITIVITY FUNCTION
outcome_model <- lm(democrat ~ age + hs + incwage + mover + hoh_age + homeowner + 
                      famsize + employed +  hrswork1 + wkswork1 + opposite1_dist_1 + 
                      opposite1_dist_2 + opposite1_dist_3 + opposite1_dist_4 + 
                      opposite1_dist_5 + opposite1_dist_6 + opposite1_dist_7 + 
                      opposite1_dist_8 + opposite1_dist_9 + opposite1_dist_10 + 
                      hs_dist1 + hs_dist2 + hs_dist3 + hs_dist4 + hs_dist5 + hs_dist6 + 
                      hs_dist7 + hs_dist8 + hs_dist9 + hs_dist10, data = df[df$white==1,])

treatment_model <- glm(opposite1_dist_1 ~ age + hs + incwage + mover + hoh_age + homeowner + 
                      famsize + employed +  hrswork1 + wkswork1 + 
                      opposite1_dist_2 + opposite1_dist_3 + opposite1_dist_4 + 
                      opposite1_dist_5 + opposite1_dist_6 + opposite1_dist_7 + 
                      opposite1_dist_8 + opposite1_dist_9 + opposite1_dist_10 + 
                      hs_dist1 + hs_dist2 + hs_dist3 + hs_dist4 + hs_dist5 + hs_dist6 + 
                      hs_dist7 + hs_dist8 + hs_dist9 + hs_dist10, data = df[df$white==1,],
                      family = binomial())


#### Cinelli and Hazlett (2019) 

sens_ch <- sensemakr(model = outcome_model,
                     treatment = "opposite1_dist_1",
                     benchmark_covariates = c("incwage"),
                     kd = 40,
                     ky = 40,
                     q = 1,
                     alpha = 0.05,
                     reduce = TRUE)


jpeg(filename = "03-output/01-plots/FigS9.jpeg", res = 600, height=6, width = 6, units = 'in')
plot(sens_ch)
dev.off()
