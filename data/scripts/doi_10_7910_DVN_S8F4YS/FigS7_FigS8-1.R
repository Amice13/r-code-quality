rm(list=ls())

### This figure outputs Figures S7 and S8, which plot the power analysis. 
### The power simulations take ~12 hours to run in their entirety.

library(data.table)
library(dplyr)
library(tidyr)
library(gtools)
library(pbapply)
rm(list=ls())
gc()

set.seed(02138)

load("02-data/seg_analysis_2017.Rdata")

df=as.data.table(df)[race==100 & !is.na(opposite1_dist_1)& !is.na(democrat) ]


df$homeowner = ifelse(df$ownershp==10,1,ifelse(df$ownershp==20,0,NA))

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

####



#### FIXED EFFECT VARIABLES

# county
df$county_state = paste0(df$statefip, "_", df$county)  

# reel_page
df$reel_page = paste0(df$reel, "_", df$pageno)    

# 2 page
df$page2 = ifelse(odd(df$pageno), df$pageno+1, df$pageno)      
df$reel_page2 = paste0(df$reel, "_", df$page2)                

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
df$statefip=as.character(df$statefip)
df$reel = as.character(df$reel)


p_treat = mean(df$opposite1_dist_1)
y_mean = mean(df$democrat)


df = df %>%
  select(statefip, county_state, cnty_enumdist, reel, reel_page10, reel_page5, reel_page2, reel_page,
         age,hs,incwage,mover,hoh_age,homeowner,democrat,famsize,employed,hrswork1,wkswork1,
         opposite1_dist_1,opposite1_dist_2,opposite1_dist_3,opposite1_dist_4,opposite1_dist_5,
         opposite1_dist_6, opposite1_dist_7, opposite1_dist_8, opposite1_dist_9, opposite1_dist_10,
         hs_dist1, hs_dist2, hs_dist3, hs_dist4, hs_dist5, hs_dist6, hs_dist7, hs_dist8, hs_dist9, hs_dist10)%>%
  drop_na %>%
  select(statefip, county_state, cnty_enumdist, reel, reel_page10, reel_page5, reel_page2, reel_page)


gc()

power_sim = function(iter=100, 
                     data=df, 
                     effect.size=0.05, 
                     y.mean = .5,
                     prob.treat = 0.05, 
                     base = "none", 
                     levels = c('statefip','county_state', 'cnty_enumdist', 'reel', 'reel_page10', 'reel_page5', 'reel_page2', 'reel_page')){
require(pbapply)
l = rbindlist(pblapply(1:iter,  FUN = function(x){
  
  if(base == 'none'){
 dt = data
    ### draw y's and z's
    dt[,z := rbinom(.N, size = 1, prob.treat)]
    dt[,p_y:=ifelse(z==1, y.mean+effect.size, y.mean)]
    dt[,y := rbinom(.N, size = 1, p_y)]
  } else {
  ### sample vector of p parameters for ys and zs
  p_z = runif(length(unique(data[[base]])), 0, prob.treat*2)
  p_y = runif(length(unique(data[[base]])), effect.size/2, max = y_mean*2-effect.size)
  
  ### merge to analysis dataframe
  m=data.table(mg = unique(data[[base]]),p_z, p_y)
  dt = merge(data,m,all.x=T,by.x=base, by.y='mg')
  
  ### draw y's and z's
  dt[,z := rbinom(.N, size = 1, p_z)]
  dt[,p_y:=ifelse(z==1, p_y+effect.size/2, p_y-effect.size/2)]
  dt[,y := rbinom(.N, size = 1, p_y)]
}
  require(lfe)

  out = rbindlist(lapply(levels, FUN = function(b){
    as.data.table(summary(felm(formula=as.formula(paste0("y ~ z|", b, "|0|county_state")), data = dt ),robust=T)$coefficients)[,level:=b]
    
  }))[,base:=base][,iter:=x]
  
  return(out)
  
}))
return(l)
}



effect.sizes = c(.03,.02,.01,.04,.05,.06,.07,.08,.09,.10)
library(pbapply)
lt = lapply(effect.sizes, FUN =function(es){
	out = power_sim( y.mean=y_mean, effect.size = es)[,effect.size:=es]
return(out)
	
})

l = rbindlist(lt)

pwr17 = l[,list(power = mean(`Pr(>|t|)`<0.05)),by = c('base', 'level', 'effect.size')][,sample:='2017']
gc()
   


##########################################################################

rm(list=ls()[!ls()%in%c('power_sim','pwr17')])
gc()

load('02-data/befm-main-analysis.Rdata')
df=as.data.table(df)[race==100 & !is.na(opposite1_dist_1)& !is.na(democrat) ]
df = df[(df$state.vfile == 'CA05' & age <=35) |(df$state.vfile == 'NC09' & age <=31),]


df$homeowner = ifelse(df$ownershp==10,1,ifelse(df$ownershp==20,0,NA))

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

####
df = df %>%
  drop_na


#### FIXED EFFECT VARIABLES

# county
df$county_state = paste0(df$statefip, "_", df$county)  

# reel_page
df$reel_page = paste0(df$reel, "_", df$pageno)    

# 2 page
df$page2 = ifelse(odd(df$pageno), df$pageno+1, df$pageno)      
df$reel_page2 = paste0(df$reel, "_", df$page2)                

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
df$statefip=as.character(df$statefip)
df$reel = as.character(df$reel)


p_treat = mean(df$opposite1_dist_1)
y_mean = mean(df$democrat)


df = df %>%
  select(statefip, county_state, cnty_enumdist, reel, reel_page10, reel_page5, reel_page2, reel_page,
         age,hs,incwage,mover,hoh_age,homeowner,democrat,famsize,employed,hrswork1,wkswork1,
         opposite1_dist_1,opposite1_dist_2,opposite1_dist_3,opposite1_dist_4,opposite1_dist_5,
         opposite1_dist_6, opposite1_dist_7, opposite1_dist_8, opposite1_dist_9, opposite1_dist_10,
         hs_dist1, hs_dist2, hs_dist3, hs_dist4, hs_dist5, hs_dist6, hs_dist7, hs_dist8, hs_dist9, hs_dist10)%>%
  drop_na %>%
  select(statefip, county_state, cnty_enumdist, reel, reel_page10, reel_page5, reel_page2, reel_page)


gc()


effect.sizes = c(.03,.02,.01,.04,.05,.06,.07,.08,.09,.10)
library(pbapply)
lt = lapply(effect.sizes, FUN =function(es){
	out = power_sim( y.mean=y_mean, effect.size = es)[,effect.size:=es]
return(out)
	
})

l = rbindlist(lt)

pwr0509 = l[,list(power = mean(`Pr(>|t|)`<0.05)),by = c('base', 'level', 'effect.size')][,sample:='2005/2009']
gc()

pwr=rbind(pwr0509,pwr17)


pwr[, base:= recode(base, `statefip`='State', `county_state`='County', `cnty_enumdist`='District', `reel` = 'Reel',
                    `reel_page10`='Page-10', `reel_page5`='Page-5', `reel_page2`='Page-2', `reel_page`='Page')]
pwr[, base:= factor(base, levels = c('State', 'County', 'Reel', 'District', 'Page-10', 'Page-5', 'Page-2', 'Page'))]
pwr[, level:= recode(level, `statefip`='State', `county_state`='County', `cnty_enumdist`='District', `reel` = 'Reel',
                    `reel_page10`='Page-10', `reel_page5`='Page-5', `reel_page2`='Page-2', `reel_page`='Page')]
pwr[, level:= factor(level, levels = c('State', 'County', 'Reel', 'District', 'Page-10', 'Page-5', 'Page-2', 'Page'))]

## LOAD AESTHETIC FUNCTIONS
source("01-code/r_utils.R")
require(scales)
cols=c('#66CCFF',  '#6699FF', '#3366CC', '#3366FF',
       '#0033CC', '#0000CC', '#000099', '#000066', '#000033')

p2017 = ggplot(data = pwr[sample=='2017'], aes (x = effect.size, y = power))+
  geom_point(shape = 21, size = 3)+
  geom_line()+
  geom_hline(yintercept = .8, linetype = 'dotted')+
  scale_x_continuous(limits=c(0.01,.1), breaks=seq(0.01,.1, by = .01))+
  theme_shom()+
  ylab('Power')+
  xlab('Effect Size')+
  facet_wrap(level ~ ., nrow = 4)

ggsave("03-output/01-plots/FigS8.jpeg",  plot = p2017, width = 9, height = 5, units = 'in', dpi=600)

p0509 = ggplot(data = pwr[sample=='2005/2009'], aes (x = effect.size, y = power))+
  geom_point(shape = 21, size = 3)+
  geom_line()+
  geom_hline(yintercept = .8, linetype = 'dotted')+
  scale_x_continuous(limits=c(0.01,.1), breaks=seq(0.01,.1, by = .01))+
  theme_shom()+
  ylab('Power')+
  xlab('Effect Size')+
  facet_wrap(level ~ ., nrow = 4)


ggsave("03-output/01-plots/FigS7.jpeg",  plot = p0509, width = 9, height = 5, units = 'in', dpi=600)


