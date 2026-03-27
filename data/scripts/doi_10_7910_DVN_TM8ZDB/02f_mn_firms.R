### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)


## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/mn_precincts12.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = VTD,
         COUNTY = COUNTYNAME ,
         CD = CONGDIST,
         VOT12D = (as.numeric(PRES12D) +as.numeric(SEN12D ) +as.numeric(USH12D ) +as.numeric(SSEN12D))/4,
         VOT12R = (as.numeric(PRES12R) +as.numeric(SEN12R) +as.numeric(USH12R) +as.numeric(SSEN12R))/4,
         minpop = TOTPOP - NH_WHITE,
         white = NH_WHITE,
         black = NH_BLACK,
         hispanic = HISP,
         pop = TOTPOP
  )

#### WEIRD STEP: also get the vetoed maps

mn_vetoed = sf::read_sf("../data_raw/shapefiles/C1101-0.shp")
mn_vetoed = sf::st_transform(mn_vetoed, crs = sf::st_crs(nm_sf))
precs_in_vetoed = sp::over(as(nm_sf, "Spatial"), as(mn_vetoed, "Spatial"))
nm_sf$CD_vetoed = precs_in_vetoed$DISTRICT
rm(mn_vetoed)
rm(precs_in_vetoed)



## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry)

geos <- c('NAME', 'PCTCODE', 'COUNTYNAME', 'CONGDIST')
short <- c('USH12', 'SEN12', 'PRES12', 'VOT12')
election <- c('2012 House',
              '2012 Senate',
              '2012 President',
              '2012 Avg Vote')


esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = stringr::str_remove(variable, ".*12"),
         election = stringr::str_extract(variable, '[:alpha:]{2,4}[:digit:]{2}')) %>%
  select(-variable) %>% filter(party %in% c("R", "D"))


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'MN') %>%
  mutate(district = as.numeric(CD)) %>%
  select(NAME, partition, district)

partition2011v <- nm_df %>%
  mutate(partition = 'MN_vetoed') %>%
  mutate(district = as.numeric(CD_vetoed)) %>%
  select(NAME, partition, district)

## District-level data
nm12 = nm_df %>%
  select(CD, NAME, VOT12D, VOT12R) %>%
  pivot_longer(cols = c(VOT12D, VOT12R)) %>%
  mutate(party = substr(name, 6,6),
         district=as.numeric(CD),
         partition = "MN")

nm11v = nm_df %>%
  select(CD_vetoed, NAME, VOT12D, VOT12R) %>%
  pivot_longer(cols = c(VOT12D, VOT12R)) %>%
  mutate(party = substr(name, 6,6),
         district=as.numeric(CD_vetoed),
         partition = "MN_vetoed")


## Building an ensemble
adj  <- redist::redist_map(nm_sf, pop_tol = 0.01, total_pop = "TOTPOP", existing_plan = "CD")


## Parameters of relevance:
## (1) popvec: a vector specifying precinct-level voting age population counts;
## (2) ndists: the number of legislative boundaries to create;
## (3) nsim: the number of partitions to create;
## (4) popcons: the weight of population constraint; and
## (5) compactness: the weight of the compactness constraint.

#remotes::install_github("kosukeimai/redist",dependencies=TRUE)


set.seed(9999)
smch <- redist::redist_smc(map = adj,
                           nsims = 50000,
                           compactness = 1,
                           adapt_k_thresh = 0.999,
                           counties = as.numeric(as.factor(nm_df$COUNTY)),
                           constraints = list(
                             vra = list(
                               strength = 5,
                               min_pop = nm_df$H_BLACK + nm_df$NH_BLACK
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))

save(smch, file="../data_clean/sims/mn_sims.RData")
load("../data_clean/sims/mn_sims.Rdata")

ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:50001)) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = district) %>%
  bind_rows(partition2012, partition2011v) ###



#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "MN"
partit = "MN_vetoed"
partit2 = "MN"
elec = "USH12"
statename = "Minnesota"

get_ds(stateabb = stateabb, elec = elec)

table(ds$election)
head(ds$partition)


#### Get the marginal and conditional histograms ####

## Marginal firms
p3 = get_hist(partit, elec, ds, var = "firms", xaxs_label = "Dem firms",
              partit2 = partit2)
p3

## Conditional firms
p3_cond = get_hist(partit, elec, ds, var = "firms", xaxs_label = "Dem firms", conditional = TRUE)
p3_cond = get_hist(partit2, elec, ds, var = "firms", xaxs_label = "Dem firms", conditional = TRUE)

## Marginal rich voters
p3_rich = get_hist(partit, elec, ds, var = "topincome", xaxs_label = "Dem $200k+ Residents",
                   partit2 = partit2)
p3_rich

## Conditional rich voters
p3_rich_cond = get_hist(partit, elec, ds, var = "topincome", xaxs_label = "Dem $200k+ Residents", conditional = TRUE)
p3_rich_cond = get_hist(partit2, elec, ds, var = "topincome", xaxs_label = "Dem $200k+ Residents", conditional = TRUE)

## Marginal donors
p3_donors = get_hist(partit, elec, ds, var = "donors200k", xaxs_label = "Dem Big Donors",
                     partit2 = partit2)
p3_donors

## Conditional donors
p3_donors_cond = get_hist(partit, elec, ds, var = "donors200k",
                          xaxs_label = "Dem Big Donors", conditional = TRUE)
p3_donors_cond = get_hist(partit2, elec, ds, var = "donors200k",
                          xaxs_label = "Dem Big Donors", conditional = TRUE)


#### geom_bin2d plots ####

p5 = get_bin2d(partit, elec, var = "firms", xaxs_label = "Dem Firms",
               ds, statename, partit2 = partit2)
p5


p5_donors = get_bin2d(partit, elec, var = "donors200k", xaxs_label = "Dem Donors",
                      ds, statename, partit2 = partit2)
p5_donors



#### pvalues ####
results = get_pvals(elec, partit, firms3, ds, "MN_vetoed", save = TRUE)
results2 = get_pvals(elec, partit2, firms3, ds, stateabb, save = TRUE)


#### Unusualness ####
unusualness = get_unusualness(elec, partit, stateabb, save = TRUE)
