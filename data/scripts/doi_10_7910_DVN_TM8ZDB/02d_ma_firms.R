### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)

## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/ma_precincts12_16.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(VOT12D = (as.numeric(PRES12D) + as.numeric(SEN12D) + as.numeric(SEN13D)+ as.numeric(SEN14D) + as.numeric(PRES16D) )/5,
         VOT12R = (as.numeric(PRES12R) + as.numeric(SEN12R) + as.numeric(SEN13R)+ as.numeric(SEN14R) + as.numeric(PRES16R))/5,
         minpop = TOTPOP - NH_WHITE,
         white = NH_WHITE,
         black = NH_BLACK,
         hispanic = HISP,
         pop = TOTPOP)

## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry) 

geos <- c('NAME', 'PRECINCT','TOWN', 'CD')
short <- c('SEN12', 'PRES12', 'SEN13', 'VOT12')
election <- c('2018 Senate',
              '2018 Governor',
              '2014 Governor',
              '2012 Avg Vote')

esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = gsub('^.*[0-9]', '', variable),
         election = gsub('.$', '', variable),
         NAME = NAME) %>%
  select(-variable) %>% filter(party %in% c("R", "D")) %>%
  filter(election == 'VOT12')


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'MA2012') %>%
  mutate(district =as.numeric(CD)) %>%
  select(NAME, partition, district)

nm16 = nm_df %>%
  select(CD, NAME, VOT12R, VOT12D) %>%
  pivot_longer(cols = c(VOT12R, VOT12D)) %>%
  mutate(party = gsub("VOT12", "", name),
         district=CD,
         partition = "MA2012")


## Build adj matrix
sf::sf_use_s2(FALSE)
adj <- redist::redist.adjacency(nm_sf)

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)

comps <- geomander::suggest_component_connection(shp = nm_sf, adjacency = adj,
                                                 group = rep(1, nrow(nm_sf)))
adj <- adj %>% geomander::add_edge(v1 = comps$x, v2 = comps$y)

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)

adj = redist::redist_map(nm_sf, adj = adj, total_pop = "TOTPOP",existing_plan = "CD")


## Parameters of relevance:
## (1) popvec: a vector specifying precinct-level voting age population counts;
## (2) ndists: the number of legislative boundaries to create;
## (3) nsim: the number of partitions to create;
## (4) popcons: the weight of population constraint; and
## (5) compactness: the weight of the compactness constraint.

set.seed(9999)
smch <- redist::redist_smc(map = adj,
                           nsims = 50000,
                           compactness = 1,
                           adapt_k_thresh = 0.999,
                           counties = as.numeric(as.factor(nm_df$TOWN)),
                           constraints = list(
                             vra = list(
                               strength = 5,
                               min_pop = nm_df$TOTPOP - nm_df$NH_WHITE
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))

save(smch, file="../data_clean/sims/ma_sims.RData")
load("../data_clean/sims/ma_sims.RData")


ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:50001)) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = district) %>%
  bind_rows(partition2012) %>%
  mutate(district = as.numeric(district))

### NOTE HERE: I may need different bin2d functions here

#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "MA"
elec = "VOT12"
statename = "Massachusetts"
partit = "MA2012"

get_ds(stateabb = stateabb, elec = elec)

table(ds$election)
head(ds$partition)


#### Get the marginal and conditional histograms ####

## Marginal firms
p3 = get_hist(partit, elec, ds, var = "firms", xaxs_label = "Dem firms")
p3

## Conditional firms
p3_cond = get_hist(partit, elec, ds, var = "firms", xaxs_label = "Dem firms", conditional = TRUE)
p3_cond

## Marginal rich voters
p3_rich = get_hist(partit, elec, ds, var = "topincome", xaxs_label = "Dem $200k+ Residents")
p3_rich

## Conditional rich voters
p3_rich_cond = get_hist(partit, elec, ds, var = "topincome", xaxs_label = "Dem $200k+ Residents", conditional = TRUE)
p3_rich_cond

## Marginal donors
p3_donors = get_hist(partit, elec, ds, var = "donors200k", xaxs_label = "Dem Big Donors")
p3_donors

## Conditional donors
p3_donors_cond = get_hist(partit, elec, ds, var = "donors200k",
                          xaxs_label = "Dem Big Donors", conditional = TRUE)
p3_donors_cond


#### geom_bin2d plots ####

p5 = get_bin2d(partit, elec, var = "firms", xaxs_label = "Dem Firms",
               ds, statename, partit2 = NULL)
p5


p5_donors = get_bin2d(partit, elec, var = "donors200k", xaxs_label = "Dem Donors",
                      ds, statename, partit2 = NULL)
p5_donors


#### pvalues ####
results = get_pvals(elec, partit, firms3, ds, stateabb, save = TRUE)

#### Unusualness ####
unusualness = get_unusualness(elec, partit, stateabb, save = TRUE)
