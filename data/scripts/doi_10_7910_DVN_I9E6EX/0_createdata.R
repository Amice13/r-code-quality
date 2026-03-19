##############################
#
# Replication file for creating the data frame for:
#
# Consolidating Progress:
# The Selection of Female Ministers in Autocracies and Democracies
#
# For publication in the the American Political Science Review
#
# Jacob Nyrup, Hikaru Yamagishi, & Stuart Bramwell
# 
##################

###################
## Load packages ##
###################

Sys.setlocale(category = "LC_ALL", locale = "")

pacman::p_load(here,tidyverse,openxlsx,stringr,countrycode,compiler,haven,Rmisc,vdemdata,WDI,readxlsx)

### Create functions 

## Decay function

CustomCumsum<-function(x,alpha){
  out<-x[1]
  for(i in 2:length(x))
    out[i] <- out[i-1]*alpha+x[i]
  out
} # Decay function

##################
# OPEC countries #
##################

opec <- c("SAU","ANG","DZA","GNQ","GAB","IRN","IRQ","KWT","LBY","NGA",
          "COG","ARE","VEN","ECU","IDN","QAT","RUS","AZE","BHR",
          "BRN","KAZ","MYS","MEX","OMN","SSD","SDN","PHL") 

###################
#### Load data ####
###################

####
## WhoGov (Nyrup and Bramwell, 2020) ---
####

## The WhoGov data can be downloaded from https://politicscentre.nuffield.ox.ac.uk/whogov-dataset/

# Individual-level data

df <- read.xlsx("../1_data/Original datasets/1_WhoGov_within_V2.0.xlsx")

# Create index

df <- df %>% 
             mutate(prestige_numeric = case_when(
                                       classification == "Prime Minister" ~ 3,
                                       classification == "Deputy Prime Minister" ~ 3,
                                       classification == "Minister (Full Rank)" & m_finance == 1 ~ 3,
                                       classification == "Minister (Full Rank)" & m_defense == 1 ~ 3,
                                       classification == "Minister (Full Rank)" & m_foreignaffairs == 1 ~ 3,
                                       classification == "Minister (Full Rank)" & portfolio_1 == "Government, Interior & Home Affairs" ~ 3,
                                       classification == "Minister (Full Rank)" & prestige_1 == "High" ~ 2,
                                       classification == "Minister (Full Rank)" & prestige_1 == "Medium" ~ 2,
                                       classification == "Minister (Full Rank)" & prestige_1 == "Low" ~ 1,
                                       classification == "Minister (Full Rank)" & portfolio_1 == "Other" ~ 1
             ),
             prestige_numeric = dplyr::case_when(country_isocode %in% c("IRN","AFG","MRT","PAK") & 
                                        (portfolio_1 == "Religion" | portfolio_2 == "Religion" | portfolio_3 == "Religion" | portfolio_4 == "Religion") ~ 3,
                                         country_isocode %in% opec & (portfolio_1 == "Natural Resources" | portfolio_2 == "Natural Resources" | portfolio_3 == "Natural Resources" | portfolio_4 == "Natural Resources") ~ 3,
                                         TRUE ~ prestige_numeric)) %>%
             dplyr::mutate(importance_numeric =
             case_when(leader == 1 ~ 5, # Tier 1
                     classification %in% c("President","Prime Minister","Chief of State") ~ 3, # Tier 2
                     classification %in% c("Vice President","Deputy Chief of State","Deputy Prime Minister") ~ 3, # Tier 3.1
                     prestige_numeric == 3 ~ 3, # Tier 3.2
                     classification %in% c("Attorney General, Chief Justice or Legal Official","Governor (Military)","Member, Royal Family","Member, Ruling Group") ~ 2, # Tier 4.1
                     prestige_numeric == 2 ~ 2, # Tier 4.2,
                     classification %in% c("Director of Government Agency","Government Spokesperson","Governor (Region)","Governor (Regional)") ~ 1, # Tier 5.1
                     prestige_numeric == 1 ~ 1, # Tier 5.2
                     classification %in% c("Minister (Junior)","Advisor","Ambassador to the United States","Chief of Staff","Deputy Director of Government Agency",
                                           "Governor (Central Bank)","Representative to the United Nations") ~ 0.5,
                     classification %in% c("Governor (General)") ~ 0
           ))

# Keep unique rows

df <- df %>% arrange(year,-leader,-core,-prestige_numeric) %>%
             distinct(year,name,country_isocode,.keep_all=TRUE) %>% filter(!is.na(name))

# Create cross-sectional data

## Create country-year variables

df_cross <- df %>% filter(gender != "Unknown" & classification %in% c("Minister (Full Rank)","Deputy Prime Minister","Prime Minister") & leader==0) %>% # Only full ranking ministers and no leaders
  group_by(country_isocode,year,gender) %>% 
  dplyr::summarize(representation = sum(prestige_numeric,na.rm=TRUE), number = n()) %>%
  pivot_wider(names_from = gender, values_from = c(representation,number)) %>%
  mutate(representation_Female = replace_na(representation_Female,0),
         representation_Male = replace_na(representation_Male,0),
         number_Female = replace_na(number_Female,0),
         number_Male = replace_na(number_Male,0),
         sum_points = representation_Female + representation_Male,
         importance_weight = representation_Female/sum_points, # Share of the total "points" to women
         share_female = number_Female/(number_Female+number_Male)) %>% # Share women
         select(country_isocode,year,share_female,importance_weight)

df_cross_prestige <- df %>% filter(gender != "Unknown" & classification %in% c("Minister (Full Rank)","Deputy Prime Minister","Prime Minister") & leader==0) %>% 
  group_by(country_isocode,year,gender,prestige_numeric) %>%
          dplyr::summarize(number = n()) %>%
  pivot_wider(names_from = c(gender,prestige_numeric), values_from = c(number)) %>%
  mutate_at(vars(Female_2:Female_NA),replace_na,0) %>% 
  dplyr::mutate(share_female_high = Female_3/(Male_3+Female_3),
         share_female_medium = Female_2/(Male_2+Female_2),
         share_female_low = Female_1/(Male_1+Female_1)) %>%
  select(-share_female_medium,-share_female_low)

df_cross_prestige_merge <- df_cross_prestige %>% select(country_isocode,year,share_female_high)

# Female leader

df_femaleleader <- df %>% filter(leader == 1) %>% distinct(year, name, country_isocode, .keep_all = TRUE) %>% mutate(female_leader = if_else(gender=="Female",1,0)) %>%
  select(year,country_isocode,female_leader)

# Cross-sectional data

df_panel <- read.xlsx("../1_data/Original datasets/2_WhoGov_crosssectional_V2.0.xlsx") %>% 
                     select(year,country_isocode,country_name) %>% mutate(year = as.integer(year))

#####
### Democracy data ---
#####

### V-Dem data ---

# The V-Dem project can be found at: https://v-dem.net/data/the-v-dem-dataset/.

# We downloaded the data using the vdemdata-package
# df_vdem <-  vdemdata::vdem 

df_vdem <- read.xlsx("../1_data/Original datasets/3_vdem_v12.xlsx")

df_parlvdem <- df_vdem %>% filter(year > 1899) %>% 
                           dplyr::select(country_name,country_id,year,v2lgfemleg,poly=v2x_polyarchy,e_pelifeex,
                                                          v2xcl_rol,v2xcl_prpty,v2x_rule,v2x_jucon,v2xlg_legcon,v2x_corr,v2clstown,
                                                          v2xcs_ccsi,v2xps_party,v2x_gender,v2x_freexp_altinf,v2x_frassoc_thick,v2x_suffr,v2xel_frefair,
                                                          v2x_elecoff) %>% 
  mutate(country_isocode = countrycode(country_id,"vdem","iso3c"), # 43 Kosovo, 209 Palestine/British Mandate, 137 Palestine/West Bank, 139 Somaliland, 236 Zanzibar not in WhoGOv
         country_isocode = case_when(country_name=="Malta" ~ "MLT",
                                     country_name=="German Democratic Republic" ~ "DDR",
                                     country_name=="Republic of Vietnam" ~ "RVN",
                                     country_name=="South Yemen" ~ "YPR",
                                     TRUE ~ country_isocode),
         poly = replace_na(poly,0)) %>%
  group_by(country_isocode) %>%
  dplyr::mutate(
         stock_v2x_freexp_altinf = CustomCumsum(v2x_freexp_altinf,0.95),
         stock_v2x_frassoc_thick = CustomCumsum(v2x_frassoc_thick,0.95),
         stock_v2x_suffr = CustomCumsum(v2x_suffr,0.95),
         stock_v2xel_frefair = CustomCumsum(v2xel_frefair,0.95),
         stock_v2x_elecoff = CustomCumsum(v2x_elecoff,0.95)
         ) %>% ungroup() %>%
  dplyr::mutate(stock_v2x_freexp_altinf = stock_v2x_freexp_altinf/max(stock_v2x_freexp_altinf,na.rm=TRUE),
                stock_v2x_frassoc_thick = stock_v2x_frassoc_thick/max(stock_v2x_frassoc_thick,na.rm=TRUE),
                stock_v2x_suffr = stock_v2x_suffr/max(stock_v2x_suffr,na.rm=TRUE),
                stock_v2xel_frefair = stock_v2xel_frefair/max(stock_v2xel_frefair,na.rm=TRUE),
                stock_v2x_elecoff = stock_v2x_elecoff/max(stock_v2x_elecoff,na.rm=TRUE)) %>%
  select(-country_name,-country_id)

for (i in 0:99){
  df_parlvdem <- df_parlvdem %>% group_by(country_isocode) %>% mutate(democracy_stock_poly = CustomCumsum(poly,i/100)) %>% ungroup() %>%
                         dplyr::mutate(democracy_stock_poly = democracy_stock_poly/max(democracy_stock_poly,na.rm=TRUE))
 ncol <- ncol(df_parlvdem)
 colnames(df_parlvdem)[ncol] <- c(paste("democracy_stock_poly",i,sep = "_"))
}

### BMR data ---

## Can be downloaded from https://sites.google.com/site/mkmtwo/data
# We use a slightly augmented data, where we account for whether the transition happened before or after July. This dataset has been uploaded to Harvard Dataverse.

df_boix_full <- read.xlsx("../1_data/Original datasets/4_bmr_V4_edited.xlsx") %>% 
                filter(year > 1899) %>% mutate(country_isocode = countrycode(ccode,"cown","iso3c"),
                                               country_isocode = case_when(
                                               country == "GERMANY WEST" ~ "DEU",
                                               country == "GERMANY EAST" ~ "DDR",
                                               country == "CZECHOSLOVAKIA" ~ "CZE",
                                               country == "MONTENEGRO" ~ "MNE",
                                               country == "KOSOVO" ~ "XXK",
                                               country == "SERBIA" ~ "SRB",
                                               country == "USSR" ~ "SUN",
                                               country == "VIETNAM" ~ "VNM",
                                               country == "VIETNAM NORTH" ~ "DVN",
                                               country == "VIETNAM SOUTH" ~ "RVN",
                                               country == "YEMEN NORTH" ~ "YEM",
                                               country == "YEMEN SOUTH" ~ "YPR",
                                               country == "YUGOSLAVIA" ~ "YUG",
                                               country == "YUGOSLAVIA FED. REP." ~ "YUG",
                                               country == "ETHIOPIA" ~ "ETH",
                                               country == "PAKISTAN  (INCL. BANGLAD.)" ~ "PAK",
                                               country == "SUDAN NORTH" ~ "SDN",
                                               country == "GERMANY" & year == 1990 ~ "GFR", # To avoid doublettes for Germany in 1990
                                               TRUE ~ country_isocode)) %>% select(year,country_isocode,democracy,bmr_new,bmr_changed) %>% 
                mutate(bmr_changed = if_else(bmr_changed=="changed",1,0)) %>%
                group_by(country_isocode) %>% dplyr::mutate(democracy_stock_bmr99 = CustomCumsum(democracy,0.99),
                                               democracy_stock_bmr95 = CustomCumsum(democracy,0.95),
                                               democracy_stock_bmr90 = CustomCumsum(democracy,0.90)) %>% 
                ungroup() %>% dplyr::mutate(democracy_stock_bmr99 = democracy_stock_bmr99/max(democracy_stock_bmr99,na.rm=TRUE),
                                               democracy_stock_bmr95 = democracy_stock_bmr95/max(democracy_stock_bmr95,na.rm=TRUE),
                                               democracy_stock_bmr90 = democracy_stock_bmr90/max(democracy_stock_bmr90,na.rm=TRUE)) %>% dplyr::rename(democracy_bmr = democracy)

### DD data ---

# Can be downloaded from: http://www.christianbjoernskov.com/bjoernskovrodedata/

df_dd <- read.xlsx("../1_data/Original datasets/5_bjornskovrode_V4.2.xlsx") %>% 
         select(year, country_isocode=country.isocode,democracy_dd=Democracy,electoral) %>%
         group_by(country_isocode) %>% dplyr::mutate(democracy_stock_dd99 = CustomCumsum(democracy_dd,0.99),
                                              democracy_stock_dd95 = CustomCumsum(democracy_dd,0.95),
                                              democracy_stock_dd90 = CustomCumsum(democracy_dd,0.90)) %>% 
         ungroup() %>% dplyr::mutate(democracy_stock_dd99 = democracy_stock_dd99/max(democracy_stock_dd99,na.rm=TRUE),
                                     democracy_stock_dd95 = democracy_stock_dd95/max(democracy_stock_dd95,na.rm=TRUE),
                                     democracy_stock_dd90 = democracy_stock_dd90/max(democracy_stock_dd90,na.rm=TRUE)) %>%
         relocate(1:3,5:7,4)

### Polity data ---

# Can be downloaded from: https://www.systemicpeace.org/inscrdata.html

polityiv <- read_rds("../1_data/Original datasets/6_polityiv.rds") %>% filter(year > 1950) %>% # NB. Starts in 1951 due to issues with coding during WW2.
  dplyr::mutate(country_isocode = countrycode(polityIV_country,"country.name","iso3c"),
         country_isocode = case_when(polityIV_country == "Germany East" ~ "DDR",
                                     polityIV_country == "Czechoslovakia" ~ "CZE",
                                     polityIV_country == "Yugoslavia" ~ "YUG",
                                     polityIV_country == "Yemen North" ~ "YEM",
                                     polityIV_country == "Yemen South" ~ "YPR",
                                     polityIV_country == "Vietnam South" ~ "RVN",
                                     TRUE ~ country_isocode),
         polity2 = polity2+10) %>%
  distinct(country_isocode,year,.keep_all =TRUE) %>%
  select(year,country_isocode,polity2) %>% group_by(country_isocode) %>% 
  fill(polity2,.direction="downup") %>%
  dplyr::mutate(polity2_stock99 = CustomCumsum(polity2,0.99),
                polity2_stock95 = CustomCumsum(polity2,0.95),
                polity2_stock90 = CustomCumsum(polity2,0.90)) %>% 
  ungroup() %>% dplyr::mutate(polity2_stock99 = polity2_stock99/max(polity2_stock99,na.rm=TRUE),
                              polity2_stock95 = polity2_stock95/max(polity2_stock95,na.rm=TRUE),
                              polity2_stock90 = polity2_stock90/max(polity2_stock90,na.rm=TRUE),
                              polity2 = polity2/20)

#####
## Add background variables ---
#####

### Magaloni data ---

# Can be downloaded from: https://cddrl.fsi.stanford.edu/research/autocracies_of_the_world_dataset & https://xmarquez.github.io/democracyData/reference/magaloni.html

df_magaloni <- read.xlsx("../1_data/Original datasets/7_autocraciesoftheworld.xlsx")

df_continent <- data.frame(table(df_magaloni$country,df_magaloni$un_continent)) %>% filter(Freq > 0) %>%
  mutate(country_isocode = countrycode(Var1,"country.name","iso3c")) %>%
  dplyr::rename(continent = Var2) %>%
  select(country_isocode,continent) %>% distinct(country_isocode,continent) # Need the last countries + USSR, VTN, DEU

df_continent <- df_panel %>% distinct(country_isocode,.keep_all = TRUE) %>% select(2,3) %>% left_join(.,df_continent, by=c("country_isocode")) %>% 
  dplyr::mutate(continent = case_when(
    country_isocode == "BRN" ~ "Asia",
    country_isocode == "DDR" ~ "Europe",
    country_isocode == "GRD" ~ "Americas",
    country_isocode == "ISL" ~ "Europe",
    country_isocode == "MDV" ~ "Asia",
    country_isocode == "MLT" ~ "Europe",
    country_isocode == "DVN" ~ "Asia",
    country_isocode == "STP" ~ "Africa",
    country_isocode == "RVN" ~ "Asia",
    country_isocode == "YPR" ~ "Asia",
    country_isocode == "SUN" ~ "Europe",
    country_isocode == "YUG" ~ "Europe",
    TRUE ~ as.character(continent))) %>% 
  dplyr::select(-country_name)

### PWT ---

# Can be downloaded from: https://www.rug.nl/ggdc/productivity/pwt/?lang=en

df_pwt <- read.xlsx("../1_data/Original datasets/8_pwt_v10.0.xlsx") %>% 
                     mutate(pop_pwt_ln = log(pop*1000000),
                            gdp_cap_pwt = rgdpna/pop,
                            gdp_cap_pwt_ln = log(gdp_cap_pwt)) %>%
                     group_by(countrycode,year) %>%
                     mutate(growth_pwt = ifelse(year-dplyr::lag(year) == 1,
                                                ((rgdpna - (dplyr::lag(rgdpna, 1))) /  (dplyr::lag(rgdpna, 1)))*100,NA)) %>%
                     select(year,country_isocode = countrycode,pop_pwt_ln,gdp_cap_pwt_ln,growth_pwt)

### QoG ---

# Can be downloaded from: https://www.gu.se/en/quality-government/qog-data/data-downloads/basic-dataset

df_qog_base <- read.xlsx("../1_data/Original datasets/9_qog_std_ts_jan22.xlsx")

df_qog <- df_qog_base %>% select(year,cname,country_isocode=ccodealp,ciri_wopol,ciri_wecon,wdi_popurb) %>% 
                          filter(cname != "Cyprus (-1974)" & cname != "France (-1962)" & (cname != "Germany" | year > 1990) & (cname != "Germany, West" | year < 1991) &
                                (cname != "Ethiopia (1993-)" | year > 1992) & (cname != "Ethiopia (-1992)" | year < 1993) & cname != "Malaysia (-1965)" & cname != "Pakistan (-1970)" &
                                (cname != "Sudan (2012-)" | year > 2011) & (cname != "Sudan (-2011)" | year < 2012) & cname != "Vietnam, North" &
                                (cname != "Yemen" | year > 1990) & (cname != "Yemen, North" | year < 1991)) %>% select(-cname) %>%
                          mutate_at(vars(matches("ciri")),~ replace(.x, .x<0, NA))

### QoG ---

# Can be downloaded from: https://www.gu.se/en/quality-government/qog-data/data-downloads/basic-dataset

df_qog_base_cross <- read.xlsx("../1_data/Original datasets/10_qog_std_cs_jan22.xlsx")

df_qoq_cross <- df_qog_base_cross %>% select(country_isocode=ccodealp,sai_statehiste0,al_ethnic2000,lp_lat_abst,lp_muslim80,lp_protmg80,wdi_area)

### World Bank ---

# Can be found at https://databank.worldbank.org/source/world-development-indicators

# We created the data using: 
#df_wb <- WDI(country ="all",
#             indicator = c(wb_oilrev = "NY.GDP.PETR.RT.ZS",
#                           wb_infantmortality = "SP.DYN.IMRT.IN",
#                           wb_primaryschoolenrolment = "SE.PRM.ENRR"),
#             start = 1960,
#             end = 2022,
#             extra = TRUE
#)

df_wb <- read.xlsx("../1_data/Original datasets/11_wb.xlsx")

df_wb <- df_wb %>% group_by(iso3c) %>% 
  arrange(year) %>%
  select(year,country_isocode = iso3c,wb_oilrev,wb_infantmortality,wb_primaryschoolenrolment)

###
## Merge datasets ---
###

df_merged <- left_join(df_panel,df_cross,by=c("year","country_isocode")) %>% 
            left_join(.,df_cross_prestige_merge,by=c("year","country_isocode")) %>%
            left_join(.,df_boix_full,by=c("country_isocode","year")) %>%
            left_join(.,df_dd,by=c("country_isocode","year")) %>%
            left_join(.,df_parlvdem,by=c("country_isocode","year")) %>%
            left_join(.,polityiv,by=c("country_isocode","year")) %>%
            left_join(.,df_pwt,by=c("country_isocode","year")) %>%  
            left_join(.,df_qog,by=c("country_isocode","year")) %>%
            left_join(.,df_femaleleader,by=c("country_isocode","year")) %>%
            left_join(.,df_wb,by=c("year","country_isocode")) %>%
            left_join(.,df_continent,by=c("country_isocode")) %>% 
            left_join(.,df_qoq_cross,by=c("country_isocode"))

###
# Add lags ---
###

df_merged <- df_merged %>% mutate(share_female = share_female*100,
                                  importance_weight = importance_weight*100,
                                  share_female_high = share_female_high*100) %>%
                           filter(year > 1965 & year < 2022)

df_merged <- df_merged %>% arrange(country_isocode,year) %>% group_by(country_isocode) %>%
                                dplyr::mutate_at(vars(share_female:ncol(df_merged)), .funs = list(lag = ~lag(., 1))) %>%
                                rename_at(vars( contains( "_lag") ), funs( paste("lag", gsub("_lag", "", .), sep = "_")))

####
## Print datasets ---
####

write.csv(df_merged,"../1_data/df_consolidatingprogress_V1.csv")
