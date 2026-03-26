setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(sf)
library(readstata13)
library(tidycensus)

## First, read in the precinct-level data
precs =  read.csv("../output/allstates_unusualness.csv")

precs %>% group_by(state) %>% summarize(firms = sum(firms),
                                        donors = sum(donors200k))
sum(precs$firms) # 3283
sum(precs$donors200k) # 5171 


## Next, read in the firm- and donor-level data
load("../data_clean/all_donors.RData")
donors1 = sf::st_as_sf(x = donor, coords = c("lon", "lat"), crs="NAD83")

load("../data_clean/all_firms.RData")
firms1 = sf::st_as_sf(x = firms1, coords = c("lon", "lat"), crs="NAD83")


setwd("../data_raw/shapefiles")
sf::sf_use_s2(FALSE)


firmlevel_onestate = function(shp, stateabb){
  state_intersect = st_intersection(shp, firms1 %>% filter(state == stateabb))
  matching_ids = as.numeric(str_extract(rownames(state_intersect), "[:digit:]*"))
  state_firms = state_intersect %>% mutate(NAME = shp$NAME[matching_ids]) %>%
    select(NAME, obj, state, type)
  return(state_firms)
}

donorlevel_onestate = function(shp, stateabb){
  state_intersect = st_intersection(shp, donors1 %>% filter(state == stateabb))
  matching_ids = as.numeric(str_extract(rownames(state_intersect), "[:digit:]*"))
  state_donors = state_intersect %>% mutate(NAME = shp$NAME[matching_ids]) %>%
    select(NAME, obj, state, type, ideo, dollars)
  return(state_donors)
}



## 1. Let's start with AZ
az <- sf::st_read(dsn = "az_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = CODE)

az_firms = firmlevel_onestate(az, "AZ")
az_donors = donorlevel_onestate(az, "AZ")

## 2. Georgia
ga <- sf::st_read(dsn = "GA_precincts16.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>% 
  mutate(NAME = ID,
         COUNTY = CTYNAME,
         CD = CD,
         PRS16D = PRES16D,
         PRS16R = PRES16R,
         VOT16D = (as.numeric(PRS16D) + as.numeric(SEN16D))/2,
         VOT16R = (as.numeric(PRS16R) + as.numeric(SEN16R))/2,
         minpop = TOTPOP - NH_WHITE)

ga_firms = firmlevel_onestate(ga, "GA")
ga_donors = donorlevel_onestate(ga, "GA")


## 3. IA
ia <- sf::st_read(dsn = "IA_counties.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = as.character(GEOID10),
         COUNTY = COUNTYFP10,
         CD = CD,
         VOTE12D = (as.numeric(PRES16D ) + as.numeric(PRES12D ) + as.numeric(PRES08D))/3,
         VOTE12R = (as.numeric(PRES16R ) + as.numeric(PRES12R ) + as.numeric(PRES08R ))/3)


ia_firms = firmlevel_onestate(ia, "IA")
ia_donors = donorlevel_onestate(ia, "IA")



## 4. MA
ma <- sf::st_read(dsn = "ma_precincts12_16.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(VOT12D = (as.numeric(PRES12D) + as.numeric(SEN12D) + as.numeric(SEN13D)+ as.numeric(SEN14D) + as.numeric(PRES16D) )/5,
         VOT12R = (as.numeric(PRES12R) + as.numeric(SEN12R) + as.numeric(SEN13R)+ as.numeric(SEN14R) + as.numeric(PRES16R))/5,
  )


ma_firms = firmlevel_onestate(ma, "MA")
ma_donors = donorlevel_onestate(ma, "MA")


## 5. MD

md <- sf::st_read(dsn = "md_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(
    VOT12D = (as.numeric(PRES12D) +as.numeric(SEN12D) +as.numeric(USH12D) +as.numeric(GOV14D))/4,
    VOT12R = (as.numeric(PRES12R) +as.numeric(SEN12R) +as.numeric(USH12R) +as.numeric(GOV14R))/4)

md_firms = firmlevel_onestate(md, "MD")
md_donors = donorlevel_onestate(md, "MD")



## 6. MN

mn <- sf::st_read(dsn = "mn_precincts12.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = VTD,
         COUNTY = COUNTYNAME ,
         CD = CONGDIST,
         VOT12D = (as.numeric(PRES12D) +as.numeric(SEN12D ) +as.numeric(USH12D ) +as.numeric(SSEN12D))/4,
         VOT12R = (as.numeric(PRES12R) +as.numeric(SEN12R) +as.numeric(USH12R) +as.numeric(SSEN12R))/4,
  )

mn_firms = firmlevel_onestate(mn, "MN")
mn_donors = donorlevel_onestate(mn, "MN")



## 7. NC

nc <- sf::st_read(dsn = "NC_VTD.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = VTD_Key,
         COUNTY = County,
         CD = oldplan,
         CD16 = newplan,
         
         EL12G_VT_D = (as.numeric(EL10G_USS_) +as.numeric(EL12G_GV_D ) +as.numeric(EL12G_PR_D ) +as.numeric(EL14G_US_1))/4,
         EL12G_VT_R = (as.numeric(EL10G_US_1) +as.numeric(EL12G_GV_R) +as.numeric(EL12G_PR_R) +as.numeric(EL14G_USS_))/4,
         EL16G_VT_D = (as.numeric(EL16G_PR_D) +as.numeric(EL16G_US_1 ) +as.numeric(EL16G_GV_D ) +as.numeric(EL14G_US_1))/4,
         EL16G_VT_R = (as.numeric(EL16G_PR_R) +as.numeric(EL16G_USS_) +as.numeric(EL16G_GV_R) +as.numeric(EL14G_USS_))/4
  )


nc_firms = firmlevel_onestate(nc, "NC")
nc_donors = donorlevel_onestate(nc, "NC")



## 8. NM

nm <- sf::st_read(dsn = "new_mexico_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(CD = CDDIST,
         COUNTY = County,
         NAME = NAME10,
         VOT18D = (as.numeric(AG18D ) +as.numeric(SEN18D  ) +as.numeric(PRES16D  ) +as.numeric(SOS16D ))/4,
         VOT18R = (as.numeric(AG18R ) +as.numeric(SEN18R ) +as.numeric(PRES16R ) +as.numeric(SOS16R))/4,
  )

nm_firms = firmlevel_onestate(nm, "NM")
nm_donors = donorlevel_onestate(nm, "NM")



## 9. OH

oh <- sf::st_read(dsn = "oh_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = PRECODE,
         COUNTY = CNTYFIPS,
         PRS16R = PRES16R,
         PRS16D = PRES16D,
         VOT12D = (as.numeric(PRS16D) +as.numeric(SEN16D  ) +as.numeric(USH16D  ) +as.numeric(SSEN16D))/4,
         VOT12R = (as.numeric(PRS16R) +as.numeric(SEN16R ) +as.numeric(USH16R ) +as.numeric(SSEN16R))/4,
  )

oh_firms = firmlevel_onestate(oh, "OH")
oh_donors = donorlevel_onestate(oh, "OH")



## 10. OR
or <- sf::st_read(dsn = "OR_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(NAME = Precinct,
         COUNTY = County,
         CD = CD,
         VOT12D = (as.numeric(SOS16D) + as.numeric(AG16D) + as.numeric(GOV16D)+ as.numeric(USH16D) + as.numeric(PRES16D) )/5,
         VOT12R = (as.numeric(SOS16R) + as.numeric(AG16R) + as.numeric(GOV16R)+ as.numeric(USH16R) + as.numeric(PRES16R))/5)

or_firms = firmlevel_onestate(or, "OR")
or_donors = donorlevel_onestate(or, "OR")



## 11. PA

pa <- sf::st_read(dsn = "PA.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(PRS12D = PRES12D, PRS12R = PRES12R) %>%
  mutate(NAME = NAME10,
         COUNTY = as.factor(COUNTYFP10),
         CD = REMEDIAL,
         PRS12D = PRES12D,
         PRS12R = PRES12R,
         USS16D = T16SEND,
         USS16R = T16SENR,
         PRS16D = T16PRESD,
         PRS16R = T16PRESR,
         
         VOT12D = (as.numeric(PRES12D) +as.numeric(USS16D ) +as.numeric(PRS16D ) +as.numeric(USS12D))/4,
         VOT12R = (as.numeric(PRES12R) +as.numeric(USS16R) +as.numeric(PRS16R) +as.numeric(USS12R))/4,
  )


pa_firms = firmlevel_onestate(pa, "PA")
pa_donors = donorlevel_onestate(pa, "PA")



## 12. TX

tx <- sf::st_read(dsn = "tx_vtds.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(VOT12D = (as.numeric(PRES12D) + as.numeric(SEN12D) + as.numeric(SEN14D) + as.numeric(GOV14D) )/4,
         VOT12R = (as.numeric(PRES12R) + as.numeric(SEN12R) + as.numeric(SEN14R) + as.numeric(GOV14R))/4,
         minpop = TOTPOP - WHITE,
         NAME = CNTYVTD)

tx_firms = firmlevel_onestate(tx, "TX")
tx_donors = donorlevel_onestate(tx, "TX")



## 13. VA
va <- sf::st_read(dsn = "VA_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = loc_prec,
         COUNTY = locality,
         CD = CD_12,
         G16DVOT = (as.numeric(G16DHOR) +as.numeric(G16DPRS) +as.numeric(G17DHOD) +as.numeric(G18DHOR))/4,
         G16RVOT = (as.numeric(G16RHOR) +as.numeric(G16RPRS) +as.numeric(G17RHOD) +as.numeric(G18RHOR))/4)

va_firms = firmlevel_onestate(va, "VA")
va_donors = donorlevel_onestate(va, "VA")



## 14. WI
wi <- sf::st_read(dsn = "WI_ltsb_corrected_final.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(VOTDEM12 = (as.numeric(USHDEM12) + as.numeric(USHDEM16) + as.numeric(PREDEM12) + as.numeric(PREDEM16) )/4,
         VOTREP12 = (as.numeric(USHREP12) + as.numeric(USHREP16) + as.numeric(PREREP12) + as.numeric(PREREP16) )/4,
         minpop = PERSONS - WHITE,NAME = GEOID10,
         COUNTY = CNTY_NAME,
         CD = CON)

wi_firms = firmlevel_onestate(wi, "WI")
wi_donors = donorlevel_onestate(wi, "WI")



#### Wrap it all up
out_firms = rbind(az_firms, ga_firms, ia_firms, ma_firms,
            md_firms, mn_firms, nc_firms, nm_firms,
            oh_firms, or_firms, pa_firms, tx_firms,
            va_firms, wi_firms)  %>% 
  data.table::data.table() %>%  
  select(-geometry)


save(out_firms, file = "../../data_clean/firmlevel_clean.RData")



#### Wrap it all up
out_donors = rbind(az_donors, ga_donors, ia_donors, ma_donors,
            md_donors, mn_donors, nc_donors, nm_donors,
            oh_donors, or_donors, pa_donors, tx_donors,
            va_donors, wi_donors)  %>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2))) %>%
  data.table::data.table() %>%
  select(-geometry)


save(out_donors, file = "../../data_clean/donorlevel_clean.RData")
write.csv(out_donors,  "../../data_clean/donorlevel_clean.csv")


### Merge it all together
#gdata::keep(out, precs, sure=T)
#firmlevel = out %>% left_join(precs, by="NAME") %>% select(-X)

#write.csv(firmlevel, file="../../output/firmlevel_clean.csv")