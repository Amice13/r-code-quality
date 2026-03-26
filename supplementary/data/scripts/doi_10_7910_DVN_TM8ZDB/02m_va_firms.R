### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)

sf::sf_use_s2(FALSE)

## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/VA_precincts.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = loc_prec,
         COUNTY = locality,
         CD = CD_12,
         G16DVOT = (as.numeric(G16DHOR) +as.numeric(G16DPRS) +as.numeric(G17DHOD) +as.numeric(G18DHOR))/4,
         G16RVOT = (as.numeric(G16RHOR) +as.numeric(G16RPRS) +as.numeric(G17RHOD) +as.numeric(G18RHOR))/4,
         minpop = TOTPOP - NH_WHITE,
         white = NH_WHITE,
         black = NH_BLACK,
         hispanic = HISP,
         pop = TOTPOP)

## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry)

geos <- c('NAME', 'precinct', 'COUNTY', 'CD')
short <- c('G16.HOR', 'G16.PRS', 'G17.GOV', 'G16.VOT')
election <- c('2016 House',
              '2016 President',
              '2017 Governor',
              '2016 Avh')


esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = substr(variable, 4,4),
         election1 = substr(variable, 1,3),
         election2 = substr(variable,5,7),
         election = paste0(election1, election2)) %>%
  select(-variable) %>% filter(party %in% c("D", "R")) %>%
  mutate(value = as.numeric(value))


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'VA12') %>%
  mutate(district = as.numeric(CD_12)) %>%
  select(NAME, partition, district)


partition2016 <- nm_df %>%
  mutate(partition = 'VA16') %>%
  mutate(district = as.numeric(CD_16)) %>%
  select(NAME, partition, district)


## District-level data
nm12 = nm_df %>%
  select(CD, NAME, G16DVOT, G16RVOT) %>%
  pivot_longer(cols = c(G16DVOT, G16RVOT)) %>%
  mutate(party = substr(name, 4,4),
         district=CD,
         partition = "VA12")


nm16 = nm_df %>%
  select(CD, NAME, G16DVOT, G16RVOT) %>%
  pivot_longer(cols = c(G16DVOT, G16RVOT)) %>%
  mutate(party = substr(name, 4,4),
         district=CD,
         partition = "VA16")


## Building an ensemble
adj = redist::redist_map(nm_sf, total_pop = "TOTPOP",existing_plan = "CD")




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
                               strength = 50,
                               min_pop = nm_df$TOTPOP - nm_df$NH_WHITE
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))

save(smch, file="../data_clean/sims/va_sims.RData")
load("../data_clean/sims/va_sims.RData")

ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:50001)) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = district) %>%
  bind_rows(partition2012, partition2016) ###



#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "VA"
partit = "VA12"
partit2 = "VA16"
elec = "G16VOT"
statename = "Virginia"

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
results = get_pvals(elec, partit, firms3, ds, partit, save = TRUE)
results2 = get_pvals(elec, partit2, firms3, ds, partit2, save = TRUE)


#### Unusualness ####
unusualness = get_unusualness(elec, partit, stateabb, save = TRUE)
