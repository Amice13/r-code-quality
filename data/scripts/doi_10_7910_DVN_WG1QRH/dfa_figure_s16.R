#### purpose: producing figure s16 #### 

#### installing key packages #### 

list.of.packages = 
  c('readstata13', 'haven', 'tidyverse', 'dplyr', 'estimatr', 'texreg',
    'gridExtra', 'ggthemes', 'wCorr', 'questionr', 'xtable', 'sf', 
    'TAM', 'purrr', 'kable', 'kableExtra', 'wCorr', 'psych',
    'psychTools')
new.packages =  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#### libraries #### 

suppressPackageStartupMessages(
  
  {
    
    library(readstata13)    
    library(haven)
    library(tidyverse)
    library(dplyr)
    library(estimatr)
    library(texreg)
    library(gridExtra)
    library(ggthemes)
    library(wCorr)
    library(questionr)
    library(xtable)
    library(sf)
    library(TAM)
    library(purrr)
    library(kable)
    library(kableExtra)
    library(wCorr)
    library(psych)
    library(psychTools)
    library(knitr)
    
  }
  
)

#### cmps '16 --- loading datasets #### 

load(file = "cmps_lat.RData")
load(file = "cmps_wht.RData")
load(file = "cmps_blk.RData")

#### cmps '20 --- loading datasets #### 

load(file = "cmps20w_clean.RData")
load(file = "cmps20b_clean.RData")
load(file = "cmps20l_clean.RData")
# mean(cmps20w$blm_ft)
# mean(cmps20b$blm_ft)
# mean(cmps20l$blm_ft)

# quick fix for missingness 

cmps20l$cath = ifelse(is.na(cmps20l$cath), 0, cmps20l$cath)

# another quick fix on dtp

cmps20l$dtps2 = (cmps20l$dtp + cmps20l$dtp2 + cmps20l$dtp3) / 3
cmps20w$dtps2 = (cmps20w$dtp + cmps20w$dtp2 + cmps20w$dtp3) / 3
cmps20b$dtps2 = (cmps20b$dtp + cmps20b$dtp2 + cmps20b$dtp3) / 3

#### more fixes #### 


# 2016 fixes --- recode so that outcomes = 1) oppose BLM and 2) belief BLM ineffective

cmps16$supp_blm  = 1 - cmps16$supp_blm 
cmps16$blm_eff = 1 - cmps16$blm_eff 

cmps16b$supp_blm  = 1 - cmps16b$supp_blm 
cmps16b$blm_eff = 1 - cmps16b$blm_eff

cmps16w$supp_blm  = 1 - cmps16w$supp_blm 

# 2020 fixes --- recode so 1 = anti-black 

# cmps20l$blm_supports = 1 - cmps20l$blm_supports
# cmps20l$blm_eff = 1 - cmps20l$blm_eff
# cmps20l$white_hood_rank_diff = cmps20l$white_hood_rank - cmps20l$black_hood_rank
# cmps20l$dtp = 1 - cmps20l$dtp
# cmps20l$dtp3 = 1 - cmps20l$dtp3
# cmps20l$dtps = (cmps20l$dtp + cmps20l$dtp3) / 2
# cmps20l$polint = abs(cmps20l$Q29 - 4)
# cmps20l$lmhhi = log(cmps20l$mhhi + 1)
# cmps20l$lmhhi_cty = log(cmps20l$mhhi_cty + 1)
# cmps20l$dep_rate = (cmps20l$total / ((cmps20l$pfb_cty / 100) * exp(cmps20l$ltpop_cty))) * 1000
cmps16$threat = cmps16$worry
cmps16$ltpop = log(cmps16$tpop + 1)
cmps16$lmhhi = log(cmps16$mhhi + 1)

cmps20l$threat2 = cmps20l$threat2 / max(cmps20l$threat2, na.rm = TRUE)
cmps20l$threat = cmps20l$threat2

cmps16$pnc = cmps16$pnc / max(cmps16$pnc, na.rm = TRUE)
cmps16$pfb = cmps16$pfb / max(cmps16$pfb, na.rm = TRUE)
cmps20l$pnc = cmps20l$pnc / max(cmps20l$pnc, na.rm = TRUE)
cmps20l$pfb = cmps20l$pfb / max(cmps20l$pfb, na.rm = TRUE)

#### validating protest measure (reproducing figure s16) #### 

# loading in acled blm data 

objprot = 
  read_csv("objective_protest/2020-01-01-2022-03-01-United_States.csv")
objprot = objprot %>% 
  mutate(blm = ifelse(grepl(x = tolower(assoc_actor_1), "blm"), 1, 0))
objprot = objprot %>% filter(blm == 1)
objprot$date = as.Date(objprot$event_date, format = "%d %B %Y")
objprot = objprot %>% filter(date <= as.Date("2021-04-02"))

# loading in county data 

cty_shp = 
  read_sf("census_data/county/COUNTY_2014_US_SL050_2020-02-24_12-50-56-120/COUNTY_2014_US_SL050_Coast_Clipped.shp")

cty_shp = cty_shp %>% dplyr::select(GEOID, geometry)

# merging in with acled data

objprot = objprot %>% st_as_sf(coords = c('longitude', 'latitude'))
st_crs(objprot) = st_crs(cty_shp$geometry)
sf_use_s2(FALSE)
objprot = st_intersection(objprot, cty_shp)

objprot = objprot %>% 
  mutate(count = 1) %>% 
  group_by(GEOID) %>% 
  summarize(count = sum(count, na.rm = TRUE))

# merging in with survey data

objprot = objprot %>% 
  rename(fips = GEOID) %>% 
  mutate(geometry = NULL)


cmps20l = merge(cmps20l, objprot, by = "fips", all.x = TRUE)
cmps20l$count = ifelse(is.na(cmps20l$count), 0, cmps20l$count)

# plot 
cmps20l$protest_rate = (cmps20l$count / cmps20l$tpop_cty) * 1000
cmps20l$lprotest_rate = log(cmps20l$protest_rate + 1)
cmps20l$protest_rate = 
  cmps20l$protest_rate / max(cmps20l$protest_rate, na.rm = TRUE)
cmps20l$lprotest_rate = 
  cmps20l$lprotest_rate / max(cmps20l$lprotest_rate, na.rm = TRUE)

validate_protest1 = 
  lm_robust(I(1 - blm_protest) ~ protest_rate,
            data = cmps20l,
            weight = weight, subset = black_lat == 0,
            cluster = fips)
validate_protest2 = 
  lm_robust(I(1 - blm_protest) ~ lprotest_rate,
            data = cmps20l,
            weight = weight, subset = black_lat == 0,
            cluster = fips)


predout1 = predict(object = validate_protest1,
                   newdata = data.frame(protest_rate = seq(from = 0, to = 1, by = .1)),
                   se.fit = TRUE)
predout2 = predict(object = validate_protest2,
                   newdata = data.frame(lprotest_rate = seq(from = 0, to = 1, by = .1)),
                   se.fit = TRUE)

dfpred1 = data.frame(
  est = predout1$fit,
  se = predout1$se.fit,
  protest_rate = seq(from = 0, to = 1, by = .1)
)

dfpred2 = data.frame(
  est = predout2$fit,
  se = predout2$se.fit,
  lprotest_rate = seq(from = 0, to = 1, by = .1)
)

# plot

valplot1 = dfpred1 %>% 
  ggplot() + 
  geom_point(aes(x = protest_rate,  y = est)) + 
  geom_errorbar(aes(x = protest_rate,
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0,
                size = .2) +
  geom_errorbar(aes(x = protest_rate,
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se),
                width = 0,
                size = .4) +
  labs(x = "BLM Protest Rate",
       y = "Predicted Value\n(BLM Protest Participation)",
       title = "A. Protest Rate") + 
  theme_tufte()

valplot2 = dfpred2 %>% 
  ggplot() + 
  geom_point(aes(x = lprotest_rate,  y = est)) + 
  geom_errorbar(aes(x = lprotest_rate,
                    ymin = est - 1.96 * se,
                    ymax = est + 1.96 * se),
                width = 0,
                size = .2) +
  geom_errorbar(aes(x = lprotest_rate,
                    ymin = est - 1.645 * se,
                    ymax = est + 1.645 * se),
                width = 0,
                size = .4) +
  labs(x = "Log(BLM Protest Rate + 1)",
       y = "Predicted Value\n(BLM Protest Participation)",
       title = "B. Log(Protest Rate + 1)") + 
  theme_tufte()

valplot_out = arrangeGrob(valplot1, valplot2, ncol = 2)

ggsave(plot = valplot_out, width = 8, height = 2.5, filename = "blmvalprot.png")
