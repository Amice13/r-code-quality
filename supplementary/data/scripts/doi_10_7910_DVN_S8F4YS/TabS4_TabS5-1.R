
rm(list=ls())
gc()
require(sandwich)
require(lmtest)
require(lfe)
require(gtools)
require(tidyverse)
require(haven)
require(data.table)

load("02-data/befm-main-analysis.Rdata")
df = as.data.table(df)
df = df[(df$state.vfile == 'CA05' & age <=35) |(df$state.vfile == 'NC09' & age <=31)]
load("02-data/context-vars.RData")


colnames(county_cens_1940) = c('fips', 'pct_black_county')
colnames(enumdist_cens_1940) = c('enumdist_id', 'pct_black_enumdist')


df[,countymatch:=1000*statefip + county/10]
df[,edmatch:=paste(statefip,county,supdist,enumdist,sep = "-")]

df = merge(df, county_cens_1940, by.x = 'countymatch', by.y = 'fips', all.x=T)
df = merge(df, enumdist_cens_1940, by.x = 'edmatch', by.y = 'enumdist_id', all.x=T)

df$white = ifelse(df$race == 100, 1, 0) # WHITE INDICATOR

df$county_state = paste0(df$statefip, "_", df$county)  
df$cnty_enumdist = paste0(df$county_state, "_", df$supdist,"_",df$enumdist)
df$cnty_enumdist = ifelse(grepl('NA', df$cnty_enumdist), NA, df$cnty_enumdist)

df$pct_black_enumdist = df$pct_black_enumdist*100
df$pct_black_county = df$pct_black_county*100


### COUNTY LEVEL VARS

cty_vars  = read_dta("02-data/haines-census.dta")
cty_vars = cty_vars %>%
  filter(level==1) %>%
  select(fips, mfgavear, totpop, urb940)

colnames(cty_vars) = paste0('cty_',colnames(cty_vars))

df = df %>% 
  as_tibble()%>%
  left_join(cty_vars, by = c('countymatch'='cty_fips'))

df$cty_mfgavear_pct= df$cty_mfgavear/df$cty_totpop*100
df$cty_pcturb40 = df$cty_urb940/df$cty_totpop*100


ed_count = read_csv('02-data/enumdist-counts.csv') %>%
  mutate(pct_urban_ed = tot.urban/n*100,
         pct_manuf_ed = manuf/n*100)

df = df %>% 
  left_join(ed_count, by = 'edmatch')
### MODELS
fml_dem_county = as.formula(paste(paste("democrat ~ ", "pct_black_county + cty_pcturb40 + cty_mfgavear_pct","|statefip|0|county_state",sep="")))
mod_dem_county = felm(formula=fml_dem_county, data = df[df$white==1,] )
sum_dem_county=summary(mod_dem_county, robust =T)



fml_dem_ed = as.formula(paste(paste("democrat ~ ", "pct_black_enumdist + pct_urban_ed + pct_manuf_ed","|county_state|0|cnty_enumdist",sep="")))
mod_dem_ed = felm(formula=fml_dem_ed, data = df[df$white==1 ,] )
sum_dem_ed=summary(mod_dem_ed, robust =T)




fml_dem_county_neighbors = as.formula(paste(paste("democrat ~ ", paste(paste0('opposite1_dist_', 1:10), collapse= '+'),"+ pct_black_county + cty_pcturb40 + cty_mfgavear_pct","|statefip|0|county_state",sep="")))
mod_dem_county_neighbors = felm(formula=fml_dem_county_neighbors, data = df[df$white==1 ,] )
sum_dem_county_neighbors=summary(mod_dem_county_neighbors, robust =T)



fml_dem_ed_neighbors = as.formula(paste(paste("democrat ~ ", paste(paste0('opposite1_dist_', 1:10), collapse= '+'),"+ pct_black_enumdist + pct_urban_ed + pct_manuf_ed","|county_state|0|cnty_enumdist",sep="")))
mod_dem_ed_neighbors = felm(formula=fml_dem_ed_neighbors, data = df[df$white==1,] )
sum_dem_ed_neighbors=summary(mod_dem_ed_neighbors, robust =T)

require(stargazer)
writeLines(capture.output(stargazer(
 mod_dem_county, mod_dem_county_neighbors,
 mod_dem_ed, mod_dem_ed_neighbors,
  style='apsr', covariate.labels =c(paste0("Black neighbor (k=", 1:10,")"), 'County % Black', 'County % Urban', 'County % Manufacturing','Enumdist % Black', 'Enumdist % Urban', 'Enumdist % Manufacturing'),
  add.lines = "FE")), "03-output/02-tables/TabS4.tex")

rm(list=ls())
gc()
#############################################
# 2017

load("02-data/seg_analysis_2017.Rdata")
df = as.data.table(df)
load("02-data/context-vars.RData")


colnames(county_cens_1940) = c('fips', 'pct_black_county')
colnames(enumdist_cens_1940) = c('enumdist_id', 'pct_black_enumdist')


df[,countymatch:=1000*statefip + county/10]
df[,edmatch:=paste(statefip,county,supdist,enumdist,sep = "-")]

df = merge(df, county_cens_1940, by.x = 'countymatch', by.y = 'fips', all.x=T)
df = merge(df, enumdist_cens_1940, by.x = 'edmatch', by.y = 'enumdist_id', all.x=T)

df$black = ifelse(df$race == 200, 1, 0) # BLACK INDICATOR
df$white = ifelse(df$race == 100, 1, 0) # WHITE INDICATOR

df$county_state = paste0(df$statefip, "_", df$county)  
df$cnty_enumdist = paste0(df$county_state, "_", df$supdist,"_",df$enumdist)
df$cnty_enumdist = ifelse(grepl('NA', df$cnty_enumdist), NA, df$cnty_enumdist)

df$pct_black_enumdist = df$pct_black_enumdist*100
df$pct_black_county = df$pct_black_county*100



### COUNTY LEVEL VARS

cty_vars  = read_dta("02-data/haines-census.dta")
cty_vars = cty_vars %>%
  filter(level==1) %>%
  select(fips, mfgavear, totpop, urb940)

colnames(cty_vars) = paste0('cty_',colnames(cty_vars))

df = df %>% 
  as_tibble()%>%
  left_join(cty_vars, by = c('countymatch'='cty_fips'))

df$cty_mfgavear_pct= df$cty_mfgavear/df$cty_totpop*100
df$cty_pcturb40 = df$cty_urb940/df$cty_totpop*100


ed_count = read_csv('02-data/enumdist-counts.csv') %>%
  mutate(pct_urban_ed = tot.urban/n*100,
         pct_manuf_ed = manuf/n*100)

df = df %>% 
  left_join(ed_count, by = 'edmatch')
### MODELS
fml_dem_county = as.formula(paste(paste("democrat ~ ", "pct_black_county + cty_pcturb40 + cty_mfgavear_pct","|statefip|0|county_state",sep="")))
mod_dem_county = felm(formula=fml_dem_county, data = df[df$white==1,] )
sum_dem_county=summary(mod_dem_county, robust =T)



fml_dem_ed = as.formula(paste(paste("democrat ~ ", "pct_black_enumdist + pct_urban_ed + pct_manuf_ed","|county_state|0|cnty_enumdist",sep="")))
mod_dem_ed = felm(formula=fml_dem_ed, data = df[df$white==1 ,] )
sum_dem_ed=summary(mod_dem_ed, robust =T)




fml_dem_county_neighbors = as.formula(paste(paste("democrat ~ ", paste(paste0('opposite1_dist_', 1:10), collapse= '+'),"+ pct_black_county + cty_pcturb40 + cty_mfgavear_pct","|statefip|0|county_state",sep="")))
mod_dem_county_neighbors = felm(formula=fml_dem_county_neighbors, data = df[df$white==1 ,] )
sum_dem_county_neighbors=summary(mod_dem_county_neighbors, robust =T)



fml_dem_ed_neighbors = as.formula(paste(paste("democrat ~ ", paste(paste0('opposite1_dist_', 1:10), collapse= '+'),"+ pct_black_enumdist + pct_urban_ed + pct_manuf_ed","|county_state|0|cnty_enumdist",sep="")))
mod_dem_ed_neighbors = felm(formula=fml_dem_ed_neighbors, data = df[df$white==1,] )
sum_dem_ed_neighbors=summary(mod_dem_ed_neighbors, robust =T)

require(stargazer)
writeLines(capture.output(stargazer(
  mod_dem_county, mod_dem_county_neighbors,
  mod_dem_ed, mod_dem_ed_neighbors,
  style='apsr', covariate.labels =c(paste0("Black neighbor (k=", 1:10,")"), 'County % Black', 'County % Urban', 'County % Manufacturing','Enumdist % Black', 'Enumdist % Urban', 'Enumdist % Manufacturing'),
  add.lines = "FE")), "03-output/02-tables/TabS5.tex")

