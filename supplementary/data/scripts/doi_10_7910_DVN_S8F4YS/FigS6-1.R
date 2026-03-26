rm(list=ls())
gc()

## THIS SCRIPT OUTPUTS FIGURE 6, THE COVARIATE BALANCE PLOT ACROSS WHITE SAMPLES WITH AND WITHOUT BLACK NEXT DOOR NEIGHBOR

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
library(BBmisc)



# LOOP THROUGH 2005/2009 sample, and 2017 sample

## LOAD DATA


###################################
#### 2005/2009 SAMPLE ANALYSIS ####
###################################
load("02-data/befm-main-analysis.Rdata")
df = as.data.table(df)


### SUBSET TO ANALYSIS SAMPLE
df = df[(df$state.vfile == 'CA05' & age <=35) |(df$state.vfile == 'NC09' & age <=31),]
df = df[race==100]

##############################
####### MAKE VARIABLES #######
####### FOR ANALYSIS   #######
##############################

df$homeowner = ifelse(df$ownershp==10,1,ifelse(df$ownershp==20,0,NA))

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


data = df[,sample:='0509']

load("02-data/seg_analysis_2017.Rdata")
df = as.data.table(df)

## SUBSET TO ANALYSIS SAMPLE
df = df[race==100]

##############################
####### MAKE VARIABLES #######
####### FOR ANALYSIS   #######
##############################

df$homeowner = ifelse(df$ownershp==10,1,ifelse(df$ownershp==20,0,NA))

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



# balance

balance.vars = c('hoh_age', "wkswork1",  "hrswork1",  
                 "labforce", "employed", "incwage", "age", "married", "famsize",
                 "hs")


## NORMALIZE VARIABLES
data$hoh_age = normalize(data$hoh_age)

data$wkswork1 = normalize(data$wkswork1)

data$hrswork1 = normalize(data$hrswork1)

data$labforce = normalize(ifelse(data$labforce==0, NA, ifelse(data$labforce==2,1,0)))

data$employed = normalize(ifelse(data$empstat %in% 10:15,1,ifelse(data$empstat%in%20:29,0,NA)))

data$incnonwg = normalize(ifelse(data$incnonwg==9,NA,data$incnonwg))

data$married = normalize(ifelse(data$marst%in%1:2,1,0))
data$incwage = normalize(data$incwage)

data$hs = normalize(data$hs)

data$famsize = normalize(data$famsize)

data$age = normalize(data$age)

data$homeowner = normalize(data$homeowner)

data$mover = normalize(data$homeowner)

df$hoh_age = normalize(df$hoh_age)

df$wkswork1 = normalize(df$wkswork1)

df$hrswork1 = normalize(df$hrswork1)

df$labforce = normalize(ifelse(df$labforce==0, NA, ifelse(df$labforce==2,1,0)))

df$employed = normalize(ifelse(df$empstat %in% 10:15,1,ifelse(df$empstat%in%20:29,0,NA)))

df$incnonwg = normalize(ifelse(df$incnonwg==9,NA,df$incnonwg))

df$incwage = normalize(df$incwage)

df$hs = normalize(df$hs)

df$famsize = normalize(df$famsize)

df$age = normalize(df$age)

df$homeowner = normalize(df$homeowner)

df$mover = normalize(df$homeowner)

gc()


require(lfe)
balance.vars = c('hoh_age', "wkswork1",  "hrswork1",  
                 "labforce", "employed", "incwage", "age", "famsize",'homeowner','mover',
                 "hs", 'incnonwg')


l0509 = rbindlist(lapply(balance.vars, FUN = function(x){
  m = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|0|0|county_state")), data = data))$coefficients['opposite1_dist_1',1:2]
  m_statefip = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|statefip|0|county_state")), data = data))$coefficients['opposite1_dist_1',1:2]
  m_county_state = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|county_state|0|county_state")), data = data))$coefficients['opposite1_dist_1',1:2]
  m_reel = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|reel|0|county_state")), data = data))$coefficients['opposite1_dist_1',1:2]
  m_cnty_enumdist = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|cnty_enumdist|0|county_state")), data = data))$coefficients['opposite1_dist_1',1:2]
  m_reel_page10 = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|reel_page10|0|county_state")), data = data))$coefficients['opposite1_dist_1',1:2]
  m_reel_page5 = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|reel_page5|0|county_state")), data = data))$coefficients['opposite1_dist_1',1:2]
 
  
  
  
  output = as.data.table(rbind(m,m_statefip,m_county_state,m_reel,m_cnty_enumdist, m_reel_page10, m_reel_page5))[,fe:=c('None','State', 'County', 'Reel', 'District', 'Page-10', 'Page-5')][,variable:=x]
  return(output)
  
  
}))


l17 = rbindlist(lapply(balance.vars, FUN = function(x){
  m = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|0|0|county_state")), data = df))$coefficients['opposite1_dist_1',1:2]
  m_statefip = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|statefip|0|county_state")), data = df))$coefficients['opposite1_dist_1',1:2]
  m_county_state = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|county_state|0|county_state")), data = df))$coefficients['opposite1_dist_1',1:2]
  m_reel = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|reel|0|county_state")), data = df))$coefficients['opposite1_dist_1',1:2]
  m_cnty_enumdist = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|cnty_enumdist|0|county_state")), data = df))$coefficients['opposite1_dist_1',1:2]
  m_reel_page10 = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|reel_page10|0|county_state")), data = df))$coefficients['opposite1_dist_1',1:2]
  m_reel_page5 = summary(felm(formula=as.formula(paste(x,"~opposite1_dist_1|reel_page5|0|county_state")), data = df))$coefficients['opposite1_dist_1',1:2]

  
  
  output = as.data.table(rbind(m,m_statefip,m_county_state,m_reel,m_cnty_enumdist, m_reel_page10, m_reel_page5))[,fe:=c('None','State', 'County', 'Reel', 'District', 'Page-10', 'Page-5')][,variable:=x]
  return(output)
  
  
}))

l = rbind(l0509[,sample:='2005/2009'],l17[,sample:='2017'])
names(l)=c('coef', 'se', 'fe', 'variable', 'sample')

# LOAD PLOT PACKAGES
require(tidyverse)
source("01-code/r_utils.R")
require(scales)

p = l %>%
  mutate(fe=factor(fe, levels = c('None','State', 'County', 'Reel', 'District', 'Page-10', 'Page-5')))%>%
  filter(fe %in% c('None','State', 'County', 'Reel', 'District', 'Page-10', 'Page-5'))%>%
  ggplot(aes(x = fe, y = coef, color = as.numeric(fe),fill = as.numeric(fe)))+
  ylab('Covariate Coefficient') + geom_hline(yintercept = 0, alpha = .5, linetype = "dashed")+
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se), width=0, size = 1.5, 
                position=position_dodge(.5), alpha=.75) + 
  geom_point(size = 2, position=position_dodge(.5), fill = "white")  +
  theme_shom()+
  xlab('')+
  theme(text = element_text(size=8), legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color='none', fill = 'none', shape='none')+
  facet_grid(cols = vars(variable), rows = vars(sample))





## SAVE PLOT
ggsave(filename = "03-output/01-plots/FigS6.jpeg", plot = p, width = 9, height = 5, units = 'in', dpi=600)

