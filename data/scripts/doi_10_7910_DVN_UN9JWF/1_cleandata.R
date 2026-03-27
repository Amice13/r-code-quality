rm(list=ls())
# Function to load packages
loadPkg=function(toLoad){
  for(lib in toLoad){
    if(! lib %in% installed.packages()[,1])
    {install.packages(lib, repos='http://cran.rstudio.com/')}
    suppressMessages( library(lib, character.only=TRUE))}}

# Load libraries
packs=c("cshapes","readstata13", "DataCombine", "countrycode","arm", "rgeos", "raster", "tidyr", "pwt9",
        "texreg", "reshape2","ggplot2", 'foreign', 'car', 'lme4', 'dplyr', "gridExtra","readr","readr",
        "pscl", "stargazer","stringi","multiwayvcov", "lmtest","scales","stringi","haven","readxl",
        "coefplot", "ggthemes", "sf", "tidyverse","dplyr","extrafont","showtext","ggmap")
loadPkg(packs)

#show chinese characters
showtext_auto(enable = TRUE)

#################################################################
#globalterrorismdb_0617dist <- read_excel("GTD_raw/GTD_0617dist/globalterrorismdb_0617dist.xlsx")
#save(globalterrorismdb_0617dist, file = "GTD_raw/GTD_0617dist/globalterrorismdb_0617dist.RData")
#################################################################

# load GTD data for 1970 - 2016 
load("Raw_data/GTD_0617dist/globalterrorismdb_0617dist.RData")
gtd <- globalterrorismdb_0617dist
rm(globalterrorismdb_0617dist)

gtdtotal <- gtd %>% 
        dplyr::mutate(ccode = countrycode(country_txt, "country.name", "cown")) %>% 
        filter(!is.na(ccode)) %>% 
        filter(!is.na(latitude)) %>% 
        filter(!is.na(longitude))


########## overlay terrorist attacks onto PRIO-GRID
##get all the grid for the globe: 58838400
priogrid_cell <- read_sf("Raw_data/priogrid_cellshp/priogrid_cell.shp") %>% 
  tbl_df() %>%
  st_sf()
## convert to SpatialPolygonsDataFrame for overlay
grid <- as_Spatial(priogrid_cell)

#convert it to SpatialPointDataFrame (xcoord = longitude; ycoord = latitude)
coordinates(gtdtotal) = ~longitude + latitude #~x+y
#make sure they have the same CRS 
proj4string(gtdtotal) <- proj4string(grid)
#overlay points to polygons and return index of polygons
##### This will take a long time to run

gtd_grid <- over(gtdtotal, grid)
# merge with original 
gtd_grid <- bind_cols(gtdtotal@data,gtd_grid)
save(gtd_grid, file = "./Clean_data/gtd_grid.RData")



##### merge with other data 

# load PrioGRID yearly data
load("Clean_data/priogrid_yearly9214.RData")
load("Clean_data/priogrid_yearly7091.RData")
load("Clean_data/prio_static.RData")
# GTD incident level
load("./Clean_data/gtd_grid.RData")
#combine 1970 to 2014
priogrid <- bind_rows(priogrid_yearly7091, priogrid_yearly9214)
# merge with prio-static
priogrid <- left_join(priogrid, prio_static, by = c("gid" = "id"))
# Grid-year level for GTD
grid_gtd <- gtd_grid %>% 
             group_by(gid, iyear) %>% 
             summarise(nterr = n())

##recode terrorism severity: target and attack

gtd_grid <- gtd_grid %>% 
          dplyr::mutate(target_severity1 = ifelse(targtype1 ==6 | targtype1 ==19 |
                                                 targtype1 ==16 | targtype1 ==11 |
                                                 targtype1 ==9 | targtype1 ==21 |
                                                 targtype1 ==13 |targtype1 ==20,
                                                 1, 0)) %>% 
          dplyr::mutate(target_severity2 = ifelse(targtype1 ==2 | targtype1 ==3 |
                                           targtype1 ==4 | targtype1 ==7 |
                                           targtype1 ==22, 1, 0)) %>% 
          dplyr::mutate(target_severity3 = ifelse(targtype1 ==1 |targtype1 ==5 |
                                            targtype1 ==8 | targtype1 ==10 |
                                            targtype1 ==12 |targtype1 ==15 |
                                            targtype1 ==14 | 
                                            targtype1 ==17| targtype1 ==18,
                                            1, 0))
##attack severity
gtd_grid <- gtd_grid %>% 
  dplyr::mutate(attack_severity1 = ifelse(attacktype1==7 | attacktype1==8 |
                                            attacktype1==9,1, 0)) %>% 
  dplyr::mutate(attack_severity2 = ifelse(attacktype1==6 |attacktype1==5 |
                                            attacktype1==4, 1, 0)) %>% 
  dplyr::mutate(attack_severity3 = ifelse(attacktype1==3 |attacktype1==2 |
                                            attacktype1==1,
                                          1, 0))
target_severity1 <- gtd_grid %>% 
          group_by(gid, iyear) %>% 
          summarise(target_severity1 = sum(target_severity1))
target_severity2 <- gtd_grid %>% 
  group_by(gid, iyear) %>% 
  summarise(target_severity2 = sum(target_severity2))
target_severity3 <- gtd_grid %>% 
  group_by(gid, iyear) %>% 
  summarise(target_severity3 = sum(target_severity3))

target_severity <- left_join(target_severity1, target_severity2, by = c("gid", "iyear"))
target_severity <- left_join(target_severity, target_severity3, by = c("gid", "iyear"))

######
attack_severity1 <- gtd_grid %>% 
  group_by(gid, iyear) %>% 
  summarise(attack_severity1 = sum(attack_severity1))

attack_severity2 <- gtd_grid %>% 
  group_by(gid, iyear) %>% 
  summarise(attack_severity2 = sum(attack_severity2))

attack_severity3 <- gtd_grid %>% 
  group_by(gid, iyear) %>% 
  summarise(attack_severity3 = sum(attack_severity3))

attack_severity <- left_join(attack_severity1, attack_severity2, by = c("gid", "iyear"))
attack_severity <- left_join(attack_severity, attack_severity3, by = c("gid", "iyear"))

severity <- left_join(target_severity, attack_severity,by = c("gid", "iyear"))
save(severity, file = "./Clean_data/severity.RData")
##merge with number
gtd_grid_all <- left_join(grid_gtd, severity, by = c("gid", "iyear"))
save(gtd_grid_all, file = "./Clean_data/gtd_grid_all.RData")


load("./Clean_data/gtd_grid_all.RData")

## merge with GTD data
priogrid <- left_join(priogrid, gtd_grid_all, by = c("gid" = "gid", "year" = "iyear"))
#recode nterr: NA = 0(no terr attack)
priogrid <- priogrid %>%
            mutate_at(.vars = vars(nterr:attack_severity3), funs(ifelse(is.na(.), 0, .)))


# merge with country level
##########WDI data
library(WDI)
##### SP.POP.TOTL = population; 
#NY.GDP.PCAP.KD.ZG = GDP per capita growth (annual %)
#NY.GDP.PCAP.KD = GDP per capita (constant 2010 US$)
#Military expenditure (% of central government expenditure) = MS.MIL.XPND.ZS
#Literacy rate, youth male (% of males ages 15-24) = SE.ADT.1524.LT.MA.ZS
wdi <- WDI(country = "all", indicator = c("SP.POP.TOTL", 
                                          "NY.GDP.PCAP.KD.ZG",
                                          "NY.GDP.PCAP.KD", 
                                          "MS.MIL.XPND.ZS",
                                          "SE.ADT.1524.LT.MA.ZS"),     
           start = 1970, end = 2014, extra = FALSE, cache = NULL)
head(wdi)  
library(countrycode)
wdi$scode <-  countrycode(wdi$iso2c,"iso2c","iso3c")    
wdi$ccode <-  countrycode(wdi$iso2c,"iso2c","cown")    

###rename variable names 
wdi <- wdi %>% 
        filter(!is.na(ccode)) %>% 
        mutate(lntpop = log(SP.POP.TOTL),
               gdppcrate = NY.GDP.PCAP.KD.ZG,
               lngdppc = log(NY.GDP.PCAP.KD),
               milexpr = MS.MIL.XPND.ZS,
               literacy = SE.ADT.1524.LT.MA.ZS) %>%
        select(year, ccode, lntpop, gdppcrate, lngdppc, milexpr, literacy) %>% 
        mutate_at(vars(ccode), as.numeric)
save(wdi, file = "./Clean_data/wdi.RData")




#merge with polity, cinc, gdppc, pop, 
load("./Clean_data/contiguousinterwar.RData")
load("./Clean_data/contiguousintrawar.RData")
load("./Clean_data/polity.RData")
load("./Clean_data/archigos_irregular.RData")
load("./Clean_data/wdi.RData")
load("./Clean_data/CINC.RData")
#234213
paperdata <- left_join(priogrid, contiguousinterwar, by = c("gwno" = "COWcode",
                                                             "year"))
paperdata <- left_join(paperdata, contiguousintrawar, by = c("gwno" = "COWcode",
                                                             "year"))
polity <- polity %>%
          select(ccode, year, polity2)
paperdata <- left_join(paperdata, polity, by = c("gwno" = "ccode", "year"))
archigos_irregular <- archigos_irregular %>%
                      select(ccode, year, irregular_dummy)
paperdata <- left_join(paperdata, archigos_irregular, by = c("gwno" = "ccode",
                                                            "year"))

paperdata <- left_join(paperdata, wdi, by = c("gwno" = "ccode",
                                              "year"))
names(CINC) <- paste(names(CINC), "cinc", sep = "_")

paperdata <- left_join(paperdata, CINC, by = c("gwno" = "ccode_cinc",
                                              "year" = "year_cinc"))

####download subnational-level map 
download.file(paste0("http://www.naturalearthdata.com/http//",
                     "www.naturalearthdata.com/download/10m/cultural/",
                     "ne_10m_admin_1_states_provinces.zip"), 
              f <- tempfile())
unzip(f, exdir=tempdir())
library(rgdal)
countries <- readOGR(tempdir(), 'ne_10m_admin_1_states_provinces')
save(countries, file = "./Clean_data/countries.shapefile.RData")


load("./Clean_data/countries.shapefile.RData")
###extrac coordinates info

location <- data.frame(lon = paperdata$xcoord,#longitude
                      lan = paperdata$ycoord) #latitude

coordinates(location) <- ~lon+lan
proj4string(location) <- proj4string(countries)
##overlay
province <- cbind.data.frame(location, country=over(location,countries)$name)
summary(province)
province <- province %>% 
          dplyr::rename(subnational = country)

###merge with original data
paperdata <- cbind.data.frame(paperdata, province)

load("./Clean_data/ucdp_intra.RData")

#merge with civil war data
paperdata <- left_join(paperdata, ucdp_intra, by = c("year", "gwno"="ccode"))

save(paperdata, file = "./Clean_data/paperdata.RData")


paperdata92 <- paperdata %>% 
              filter(year > 1990 & year < 2015)
save(paperdata92, file = "./Clean_data/paperdata92.RData")

gtd_grid92 <- gtd_grid %>% 
              filter(iyear > 1990 & iyear < 2015)

save(gtd_grid92, file = "./Clean_data/gtd_grid92.RData")

### create spatial lags
#show chinese characters
load("./Clean_data/paperdata.RData")
load("./Clean_data/paperdata_baysimput.RData") #for 2011, 2012, 2013, gid= 10469
load("./Clean_data/smat_sparse.RData")
load("./Clean_data/gtd_grid92.RData")
load("./Clean_data/gtd_grid.RData")
load("./Clean_data/AfricanCountryShapefile.RData")


#get unique grid for africa
grid <- unique(paperdata_baysimput$gid)
year <- 1970: 2016
#build tscc data: 10674*47 = 501678
africagrid_year <- expand.grid(gid = grid, year = year)
#subset paper data to keep african countries only
africagrid_year <- left_join(africagrid_year, paperdata, by = c("gid","year")) 

## recode terrorist attacks variables
#recode the number of terrorism: NA = 0
africagrid_year <- africagrid_year %>% 
            mutate(terr_dum = ifelse(nterr > 0, 1, 0)) %>% 
            dplyr::group_by(gid) %>% 
            dplyr::mutate(terr_dum_t1 = lag(terr_dum, 1),
                          terr_dum_t2 = lag(terr_dum, 2),
                          terr_dum_t3 = lag(terr_dum, 3),
                          terr_dum_t4 = lag(terr_dum, 4),
                          nterr_t1 = lag(nterr, 1),
                          nterr_t2 = lag(nterr, 2),
                          nterr_t3 = lag(nterr, 3),
                          nterr_t4 = lag(nterr, 4)) 
# for target severity
africagrid_year <- africagrid_year %>% 
  mutate(target_severity1_dum = ifelse(target_severity1 > 0, 1, 0)) %>% 
  dplyr::group_by(gid) %>% 
  dplyr::mutate(target_severity1_dum_t1 = lag(target_severity1_dum, 1),
                target_severity1_dum_t2 = lag(target_severity1_dum, 2),
                target_severity1_dum_t3 = lag(target_severity1_dum, 3),
                target_severity1_dum_t4 = lag(target_severity1_dum, 4),
                target_severity1_t1 = lag(target_severity1, 1),
                target_severity1_t2 = lag(target_severity1, 2),
                target_severity1_t3 = lag(target_severity1, 3),
                target_severity1_t4 = lag(target_severity1, 4))

africagrid_year <- africagrid_year %>% 
  mutate(target_severity2_dum = ifelse(target_severity2 > 0, 1, 0)) %>% 
  dplyr::group_by(gid) %>% 
  dplyr::mutate(target_severity2_dum_t1 = lag(target_severity2_dum, 1),
                target_severity2_dum_t2 = lag(target_severity2_dum, 2),
                target_severity2_dum_t3 = lag(target_severity2_dum, 3),
                target_severity2_dum_t4 = lag(target_severity2_dum, 4),
                target_severity2_t1 = lag(target_severity2, 1),
                target_severity2_t2 = lag(target_severity2, 2),
                target_severity2_t3 = lag(target_severity2, 3),
                target_severity2_t4 = lag(target_severity2, 4))

africagrid_year <- africagrid_year %>% 
  mutate(target_severity3_dum = ifelse(target_severity3 > 0, 1, 0)) %>% 
  dplyr::group_by(gid) %>% 
  dplyr::mutate(target_severity3_dum_t1 = lag(target_severity3_dum, 1),
                target_severity3_dum_t2 = lag(target_severity3_dum, 2),
                target_severity3_dum_t3 = lag(target_severity3_dum, 3),
                target_severity3_dum_t4 = lag(target_severity3_dum, 4),
                target_severity3_t1 = lag(target_severity3, 1),
                target_severity3_t2 = lag(target_severity3, 2),
                target_severity3_t3 = lag(target_severity3, 3),
                target_severity3_t4 = lag(target_severity3, 4))
## attack
africagrid_year <- africagrid_year %>% 
  mutate(attack_severity1_dum = ifelse(attack_severity1 > 0, 1, 0)) %>% 
  dplyr::group_by(gid) %>% 
  dplyr::mutate(attack_severity1_dum_t1 = lag(attack_severity1_dum, 1),
                attack_severity1_dum_t2 = lag(attack_severity1_dum, 2),
                attack_severity1_dum_t3 = lag(attack_severity1_dum, 3),
                attack_severity1_dum_t4 = lag(attack_severity1_dum, 4),
                attack_severity1_t1 = lag(attack_severity1, 1),
                attack_severity1_t2 = lag(attack_severity1, 2),
                attack_severity1_t3 = lag(attack_severity1, 3),
                attack_severity1_t4 = lag(attack_severity1, 4))

africagrid_year <- africagrid_year %>% 
  mutate(attack_severity2_dum = ifelse(attack_severity2 > 0, 1, 0)) %>% 
  dplyr::group_by(gid) %>% 
  dplyr::mutate(attack_severity2_dum_t1 = lag(attack_severity2_dum, 1),
                attack_severity2_dum_t2 = lag(attack_severity2_dum, 2),
                attack_severity2_dum_t3 = lag(attack_severity2_dum, 3),
                attack_severity2_dum_t4 = lag(attack_severity2_dum, 4),
                attack_severity2_t1 = lag(attack_severity2, 1),
                attack_severity2_t2 = lag(attack_severity2, 2),
                attack_severity2_t3 = lag(attack_severity2, 3),
                attack_severity2_t4 = lag(attack_severity2, 4))

africagrid_year <- africagrid_year %>% 
  mutate(attack_severity3_dum = ifelse(attack_severity3 > 0, 1, 0)) %>% 
  dplyr::group_by(gid) %>% 
  dplyr::mutate(attack_severity3_dum_t1 = lag(attack_severity3_dum, 1),
                attack_severity3_dum_t2 = lag(attack_severity3_dum, 2),
                attack_severity3_dum_t3 = lag(attack_severity3_dum, 3),
                attack_severity3_dum_t4 = lag(attack_severity3_dum, 4),
                attack_severity3_t1 = lag(attack_severity3, 1),
                attack_severity3_t2 = lag(attack_severity3, 2),
                attack_severity3_t3 = lag(attack_severity3, 3),
                attack_severity3_t4 = lag(attack_severity3, 4))

##create spatial lag
library(streg)
africagrid_year <- as.data.frame(africagrid_year)
africagrid_year <- streg.sort(africagrid_year, "year", "gid")
#spatial lag
africagrid_year$nterr_s0 <- streg.slag(africagrid_year, "year", "nterr", smat_sparse)
africagrid_year$nterr_s1 <- streg.slag(africagrid_year, "year", "nterr_t1", smat_sparse)
africagrid_year$nterr_s2 <- streg.slag(africagrid_year, "year", "nterr_t2", smat_sparse)
africagrid_year$nterr_s3 <- streg.slag(africagrid_year, "year", "nterr_t3", smat_sparse)
africagrid_year$nterr_s4 <- streg.slag(africagrid_year, "year", "nterr_t4", smat_sparse)

africagrid_year$terr_dum_s0 <- streg.slag(africagrid_year, "year", "terr_dum", smat_sparse)
africagrid_year$terr_dum_s1 <- streg.slag(africagrid_year, "year", "terr_dum_t1", smat_sparse)
africagrid_year$terr_dum_s2 <- streg.slag(africagrid_year, "year", "terr_dum_t2", smat_sparse)
africagrid_year$terr_dum_s3 <- streg.slag(africagrid_year, "year", "terr_dum_t3", smat_sparse)
africagrid_year$terr_dum_s4 <- streg.slag(africagrid_year, "year", "terr_dum_t4", smat_sparse)

africagrid_year$target_severity1_dum_s0 <- streg.slag(africagrid_year, "year", "target_severity1_dum", smat_sparse)
africagrid_year$target_severity1_dum_s1 <- streg.slag(africagrid_year, "year", "target_severity1_dum_t1", smat_sparse)
africagrid_year$target_severity1_dum_s2 <- streg.slag(africagrid_year, "year", "target_severity1_dum_t2", smat_sparse)
africagrid_year$target_severity1_dum_s3 <- streg.slag(africagrid_year, "year", "target_severity1_dum_t3", smat_sparse)
africagrid_year$target_severity1_dum_s4 <- streg.slag(africagrid_year, "year", "target_severity1_dum_t4", smat_sparse)

africagrid_year$target_severity2_dum_s0 <- streg.slag(africagrid_year, "year", "target_severity2_dum", smat_sparse)
africagrid_year$target_severity2_dum_s1 <- streg.slag(africagrid_year, "year", "target_severity2_dum_t1", smat_sparse)
africagrid_year$target_severity2_dum_s2 <- streg.slag(africagrid_year, "year", "target_severity2_dum_t2", smat_sparse)
africagrid_year$target_severity2_dum_s3 <- streg.slag(africagrid_year, "year", "target_severity2_dum_t3", smat_sparse)
africagrid_year$target_severity2_dum_s4 <- streg.slag(africagrid_year, "year", "target_severity2_dum_t4", smat_sparse)

africagrid_year$target_severity3_dum_s0 <- streg.slag(africagrid_year, "year", "target_severity3_dum", smat_sparse)
africagrid_year$target_severity3_dum_s1 <- streg.slag(africagrid_year, "year", "target_severity3_dum_t1", smat_sparse)
africagrid_year$target_severity3_dum_s2 <- streg.slag(africagrid_year, "year", "target_severity3_dum_t2", smat_sparse)
africagrid_year$target_severity3_dum_s3 <- streg.slag(africagrid_year, "year", "target_severity3_dum_t3", smat_sparse)
africagrid_year$target_severity3_dum_s4 <- streg.slag(africagrid_year, "year", "target_severity3_dum_t4", smat_sparse)

africagrid_year$attack_severity1_s0 <- streg.slag(africagrid_year, "year", "attack_severity1", smat_sparse)
africagrid_year$attack_severity1_s1 <- streg.slag(africagrid_year, "year", "attack_severity1_t1", smat_sparse)
africagrid_year$attack_severity1_s2 <- streg.slag(africagrid_year, "year", "attack_severity1_t2", smat_sparse)
africagrid_year$attack_severity1_s3 <- streg.slag(africagrid_year, "year", "attack_severity1_t3", smat_sparse)
africagrid_year$attack_severity1_s4 <- streg.slag(africagrid_year, "year", "attack_severity1_t4", smat_sparse)

africagrid_year$attack_severity2_s0 <- streg.slag(africagrid_year, "year", "attack_severity2", smat_sparse)
africagrid_year$attack_severity2_s1 <- streg.slag(africagrid_year, "year", "attack_severity2_t1", smat_sparse)
africagrid_year$attack_severity2_s2 <- streg.slag(africagrid_year, "year", "attack_severity2_t2", smat_sparse)
africagrid_year$attack_severity2_s3 <- streg.slag(africagrid_year, "year", "attack_severity2_t3", smat_sparse)
africagrid_year$attack_severity2_s4 <- streg.slag(africagrid_year, "year", "attack_severity2_t4", smat_sparse)

africagrid_year$attack_severity3_s0 <- streg.slag(africagrid_year, "year", "attack_severity3", smat_sparse)
africagrid_year$attack_severity3_s1 <- streg.slag(africagrid_year, "year", "attack_severity3_t1", smat_sparse)
africagrid_year$attack_severity3_s2 <- streg.slag(africagrid_year, "year", "attack_severity3_t2", smat_sparse)
africagrid_year$attack_severity3_s3 <- streg.slag(africagrid_year, "year", "attack_severity3_t3", smat_sparse)
africagrid_year$attack_severity3_s4 <- streg.slag(africagrid_year, "year", "attack_severity3_t4", smat_sparse)
##

####### merge with the imputed data: need to remove these variables from africagrid_year
# then joined by paperdata_baysimput
common_name <- intersect(names(paperdata_baysimput), names(africagrid_year))
names(paperdata_baysimput)
common_name <- common_name[-1:-2] #remove gid and year 
data <- africagrid_year[, -which(colnames(africagrid_year) %in% common_name)]
##merge with paperdata_baysimput
data <- left_join(paperdata_baysimput, data, by = c("gid", "year"))
save(data, file = "./Clean_data/data.RData")
#### create split-poulation data

library(spduration)
#need to use data.frame class instead of tbl class.
load("./Clean_data/data.RData")
data_full <- as.data.frame(data)
data_full <- add_duration(data_full, "terr_dum", unitID = "gid", tID ="year", freq = "year", ongoing = FALSE)
save(data_full, file = "./Clean_data/data_full.RData")
#make sure the terro in the test do not influence the risk coding in the training
###create training data 1994-2011; test data: 2012-2013
data_train <- data[data$year<2012,]
data_train <- as.data.frame(data_train)
data_train<- add_duration(data_train, "terr_dum", unitID = "gid", tID ="year", freq = "year", ongoing = FALSE)
save(data_train, file = "./Clean_data/data_train.RData")
## test data
data_test <- data_full[data_full$year>=2012,]
save(data_test, file = "./Clean_data/data_test.RData")
