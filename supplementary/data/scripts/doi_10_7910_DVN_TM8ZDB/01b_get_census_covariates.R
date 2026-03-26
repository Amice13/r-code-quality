setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../data_raw/shapefiles")
library(tidyverse)
library(sf)
library(readstata13)
library(tidycensus)

sf::sf_use_s2(FALSE)

## Read in ZIP5 shapefile
zips <- sf::st_read(dsn = "zip_2012/tl_2012_us_zcta510.shp", quiet = TRUE) %>%
  sf::st_transform(crs = sf::st_crs('NAD83'))

## Collect ZIP5 income
#census_api_key("b6f6e661ba23a769620fbbc6f94227bfca2ac038", install=TRUE)
medinc <- get_acs(geography = "zcta",
                  variables = c(totpop = "B00001_001",
                                medincome = "B19013_001",
                                college_denom = "B15003_001",
                                college = "B15003_022",
                                unemploy = "B23025_005",
                                laborforce = "B23025_002",
                                homeowner_denom = "B25003_001",
                                homeowner = "B25003_002",
                                inc2 = "B19001_002",
                                inc3 = "B19001_003",
                                inc4 = "B19001_004",
                                inc5 = "B19001_005",
                                inc6 = "B19001_006",
                                inc7 = "B19001_007",
                                inc8 = "B19001_008",
                                inc9 = "B19001_009",
                                inc10 = "B19001_010",
                                inc11 = "B19001_011",
                                inc12 = "B19001_012",
                                inc13 = "B19001_013",
                                inc14 = "B19001_014",
                                inc15 = "B19001_015",
                                inc16 = "B19001_016",
                                inc17 = "B19001_017"), #https://api.census.gov/data/2012/acs/acs5/variables.html
                  year = 2012,
                  keep_geo_vars = TRUE) %>% 
  mutate(NAME = gsub("ZCTA5 ", "", NAME)) %>%
  select(-GEOID)

medinc2 = medinc %>%
  select(-moe) %>%
  pivot_wider(id_cols = "NAME",
              names_from = "variable",
              values_from = "estimate") %>%
  mutate(unemp = unemploy/laborforce,
         homeowner = homeowner/homeowner_denom,
         college = college/college_denom) %>%
  rowwise() %>%
  mutate(income_gini = DescTools::Gini(c(inc2,inc3,inc4,inc5,inc6,inc7,inc8,inc9,inc10,
                                         inc11,inc12,inc13,inc14,inc15,inc16,inc17)),
         topincome = inc17) %>%
  select(NAME, medincome, topincome, college,unemp,homeowner, income_gini)

## Read ZIP5 Urban/Rural
urban <- readxl::read_excel("zip_2012/RUCA2010zipcode.xlsx", sheet = "Data") %>%
  select(NAME = ZIP_CODE, urban = RUCA1)

covars = merge(medinc2, urban)
rm(medinc)
rm(medinc2)
rm(urban)

## Merge the ZIP-level data together
zips = merge(zips, covars, by.x = "ZCTA5CE10", by.y = "NAME", all.x = TRUE) %>%
  select(zip = ZCTA5CE10, medincome, topincome, college, unemp, homeowner, income_gini, urban, geometry)

### I am going to have to loop through each of the states
## Reading their SHPs in, merging with a ZIP-level data set
## Pulling out subsets of precinct name + covars
## Then merge back in with the original DTA file

## 1. Let's start with AZ
az <- sf::st_read(dsn = "az_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = CODE)

az_intersect = st_intersection(az, zips)
az_intersect2 = az_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             
            topincome = sum(weight * topincome, na.rm=T),
            topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry) %>%
  mutate(state = "AZ")


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

ga_intersect = st_intersection(ga, zips)
ga_intersect2 = ga_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "GA")


## 3. IA
ia <- sf::st_read(dsn = "../data_raw/shapefiles/IA_counties.shp", # shapefile name from MGGG --
                           quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = as.character(GEOID10),
         COUNTY = COUNTYFP10,
         CD = CD,
         VOTE12D = (as.numeric(PRES16D ) + as.numeric(PRES12D ) + as.numeric(PRES08D))/3,
         VOTE12R = (as.numeric(PRES16R ) + as.numeric(PRES12R ) + as.numeric(PRES08R ))/3,
         minpop = TOTPOP - NH_WHITE,
         white = NH_WHITE,
         black = NH_BLACK,
         hispanic = HISP,
         pop = TOTPOP)

ia_intersect = st_intersection(ia, zips)
ia_intersect2 = ia_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "IA")


## 4. MA
ma <- sf::st_read(dsn = "ma_precincts12_16.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(VOT12D = (as.numeric(PRES12D) + as.numeric(SEN12D) + as.numeric(SEN13D)+ as.numeric(SEN14D) + as.numeric(PRES16D) )/5,
         VOT12R = (as.numeric(PRES12R) + as.numeric(SEN12R) + as.numeric(SEN13R)+ as.numeric(SEN14R) + as.numeric(PRES16R))/5,
  )


ma_intersect = st_intersection(ma, zips)
ma_intersect2 = ma_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "MA")


## 5. MD

md <- sf::st_read(dsn = "md_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(
    VOT12D = (as.numeric(PRES12D) +as.numeric(SEN12D) +as.numeric(USH12D) +as.numeric(GOV14D))/4,
    VOT12R = (as.numeric(PRES12R) +as.numeric(SEN12R) +as.numeric(USH12R) +as.numeric(GOV14R))/4)

md_intersect = st_intersection(md, zips)
md_intersect2 = md_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "MD")


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

mn_intersect = st_intersection(mn, zips)
mn_intersect2 = mn_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "MN")


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


nc_intersect = st_intersection(nc, zips)
nc_intersect2 = nc_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "NC")


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

nm_intersect = st_intersection(nm, zips)
nm_intersect2 = nm_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "NM")


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

oh_intersect = st_intersection(oh, zips)
oh_intersect2 = oh_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "OH")


## 10. OR
or <- sf::st_read(dsn = "OR_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(NAME = Precinct,
         COUNTY = County,
         CD = CD,
         VOT12D = (as.numeric(SOS16D) + as.numeric(AG16D) + as.numeric(GOV16D)+ as.numeric(USH16D) + as.numeric(PRES16D) )/5,
         VOT12R = (as.numeric(SOS16R) + as.numeric(AG16R) + as.numeric(GOV16R)+ as.numeric(USH16R) + as.numeric(PRES16R))/5)

or_intersect = st_intersection(or, zips)
or_intersect2 = or_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "OR")


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


pa_intersect = st_intersection(pa, zips)
pa_intersect2 = pa_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "PA")


## 12. TX

tx <- sf::st_read(dsn = "tx_vtds.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(VOT12D = (as.numeric(PRES12D) + as.numeric(SEN12D) + as.numeric(SEN14D) + as.numeric(GOV14D) )/4,
         VOT12R = (as.numeric(PRES12R) + as.numeric(SEN12R) + as.numeric(SEN14R) + as.numeric(GOV14R))/4,
         minpop = TOTPOP - WHITE,
         NAME = CNTYVTD)

tx_intersect = st_intersection(tx, zips)
tx_intersect2 = tx_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "TX")


## 13. VA
va <- sf::st_read(dsn = "VA_precincts.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = loc_prec,
         COUNTY = locality,
         CD = CD_12,
         G16DVOT = (as.numeric(G16DHOR) +as.numeric(G16DPRS) +as.numeric(G17DHOD) +as.numeric(G18DHOR))/4,
         G16RVOT = (as.numeric(G16RHOR) +as.numeric(G16RPRS) +as.numeric(G17RHOD) +as.numeric(G18RHOR))/4)

va_intersect = st_intersection(va, zips)
va_intersect2 = va_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "VA")


## 14. WI
wi <- sf::st_read(dsn = "WI_ltsb_corrected_final.shp", # shapefile name from MGGG --
                  quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(VOTDEM12 = (as.numeric(USHDEM12) + as.numeric(USHDEM16) + as.numeric(PREDEM12) + as.numeric(PREDEM16) )/4,
         VOTREP12 = (as.numeric(USHREP12) + as.numeric(USHREP16) + as.numeric(PREREP12) + as.numeric(PREREP16) )/4,
         minpop = PERSONS - WHITE,NAME = GEOID10,
         COUNTY = CNTY_NAME,
         CD = CON)

wi_intersect = st_intersection(wi, zips)
wi_intersect2 = wi_intersect %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  group_by(NAME) %>%
  mutate(weight = area / sum(area)) %>% 
  summarize(medincome = sum(weight * medincome, na.rm=T),             topincome = sum(weight * topincome, na.rm=T),
            urban = sum(weight * urban, na.rm=T),
            college = sum(weight * college, na.rm=T),
            unemp = sum(weight * unemp, na.rm=T),
            homeowner = sum(weight * homeowner, na.rm=T),
            income_gini = sum(weight * income_gini, na.rm=T)) %>%
  select(-geometry)%>%
  mutate(state = "WI")


#### Wrap it all up
out = rbind(az_intersect2, ga_intersect2, ia_intersect2, ma_intersect2,
            md_intersect2, mn_intersect2, nc_intersect2, nm_intersect2,
            oh_intersect2, or_intersect2, pa_intersect2, tx_intersect2,
            va_intersect2, wi_intersect2)  %>% 
  data.table::data.table() %>%  
  select(-geometry)


save(out, file = "../../data_clean/allprecincts_census_covars.RData")
