### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)


## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/md_precincts.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(
    VOT12D = (as.numeric(PRES12D) +as.numeric(SEN12D) +as.numeric(USH12D) +as.numeric(GOV14D))/4,
    VOT12R = (as.numeric(PRES12R) +as.numeric(SEN12R) +as.numeric(USH12R) +as.numeric(GOV14R))/4,
    minpop = TOTPOP - NH_WHITE,
    white = NH_WHITE,
    black = NH_BLACK,
    hispanic = HISP,
    pop = TOTPOP)


## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry) 

geos <- c('NAME', 'VTD', 'COUNTY', 'CD')
short <- c('PRES12', 'USH12', 'USH14', 'VOT12')
election <- c('2012 Senate',
              '2012 Congress',
              '2014 Congress',
              '2012 Vote Avg')

esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = gsub('^.*[0-9]', '', variable),
         election = gsub('.$', '', variable)) %>%
  select(-variable)


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'MD2012') %>%
  mutate(district = as.character(as.numeric(CD))) %>%
  select(NAME, partition, district)

## District-level data
nm16 = nm_df %>%
  select(CD, VTD,VOT12D,VOT12R) %>%
  pivot_longer(cols = c(VOT12D,VOT12R)) %>%
  mutate(party = gsub(".*1.", "", name),
         district=CD,
         partition = "MD2012")



## cleaning up the geography
adj  <- redist::redist.adjacency(nm_sf)
nbrs <- geomander::suggest_neighbors(shp = nm_sf, adjacency = adj, neighbors=8)
adj <- adj %>% geomander::add_edge(v1 = nbrs$x, v2 = nbrs$y)

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)

comps <- geomander::suggest_component_connection(shp = nm_sf, adjacency = adj,
                                      group = rep(1, nrow(nm_sf)))
adj <- adj %>% geomander::add_edge(v1 = comps$x, v2 = comps$y)

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
                           counties = as.numeric(as.factor(nm_df$COUNTY)),
                           constraints = list(
                             vra = list(
                               strength = 5,
                               min_pop = nm_df$TOTPOP - nm_df$NH_WHITE
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))

save(smch, file="../data_clean/sims/md_sims.RData")
load("../data_clean/sims/md_sims.RData")

ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:50001)) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = as.character(district)) %>%
  bind_rows(partition2012) ###



#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "MD"
partit = "MD2012"
elec = "VOT12"
statename = "Maryland"

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
