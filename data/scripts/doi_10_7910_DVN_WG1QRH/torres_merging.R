library(tidycensus)
library(dplyr)
library(haven)
library(readr)

fips_zip_x <- read_csv("fips_zip_x.csv")

CMPS2016 <- read_dta("CMPS2016.dta")
View(CMPS2016)

census_api_key("995f6f076df2f2f1fc81a82e05e9828a7d79c11a", install=TRUE)

variables <- load_variables(2015, "acs5", cache = TRUE)


total_pop<- get_acs(geography = "zcta",
                    year = 2015,
                    variables = c(totalpop = "B05002_001"))
total_pop = total_pop %>% 
  transmute(geoid = GEOID,
         zcta5 = NAME,
         total_pop = estimate)

est_noncit<- get_acs(geography = "zcta",
                    year = 2015,
                    variables = c(est_noncit = "B05002_021"))
est_noncit = est_noncit %>% 
  transmute(geoid = GEOID,
            zcta5 = NAME,
            est_notcit = estimate)

est_latino<- get_acs(geography = "zcta",
                     year = 2015,
                     variables = c(est_latino = "B03003_003"))
est_latino = est_latino %>% 
  transmute(geoid = GEOID,
            zcta5 = NAME,
            est_latino = estimate)

est_fb<- get_acs(geography = "zcta",
                 year = 2015,
                 variables = c(est_fb = "B05002_013"))
est_fb = est_fb %>% 
  transmute(geoid = GEOID,
            zcta5 = NAME,
            est_fb = estimate)

est_latino_df <- data.frame(est_latino)
est_fb_df <- data.frame(est_fb)
est_noncit_df <- data.frame(est_noncit)
total_pop_df <- data.frame(total_pop)
merge_latino_fb <- merge(est_latino_df,est_fb_df, by.y=c("geoid","zcta5"))
merge_latinofb_noncit <- merge(merge_latino_fb,est_noncit_df, by=c("geoid","zcta5"))
acs2015 <- merge(merge_latinofb_noncit,total_pop_df, by=c("geoid","zcta5"))

acs2015 = acs2015 %>% 
  mutate(zcta = geoid,
        fullzcta5 = zcta5,
        est_latino = est_latino,
        est_fb = est_fb,
        total_pop = total_pop)

fips_zip_x <- fips_zip_x  %>% 
  transmute(zcta = ZCTA5,
            GEOID = GEOID,
            state = STATE,
            county = COUNTY)

controls <- merge(fips_zip_x,acs2015, by=c("zcta"))

acs2015_zip <- merge(acs2015,fips_zip_x, by=c("geoid","zcta5"))

local_enforcement<- read_csv("~/Dropbox/fear_deport/context/2018 Local Assistance Map Data New Colors.xlsx-filtered (2).csv")
