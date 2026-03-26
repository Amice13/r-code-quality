### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)
#source("./scripts/adj.R")


## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/az_precincts.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = CODE,
         white = NH_WHITE,
         black = NH_BLACK,
         hispanic = HISP,
         pop = TOTPOP)

## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry) %>%
  mutate(VOT18D = (as.numeric(USH18D) +as.numeric(GOV18D ) +as.numeric(SOS18D ) +as.numeric(SSEN18D))/4,
         VOT18R = (as.numeric(USH18R) +as.numeric(GOV18R) +as.numeric(SOS18R) +as.numeric(SSEN18R))/4,
  )

geos <- c('NAME', 'PCTNAME', 'COUNTY', 'CD')
short <- c('USH18', 'GOV18', 'SOS18', 'VOT18')
election <- c('2018 House',
              '2018 Governor',
              '2018 Sec of State',
              '2018 Avg Vote')


esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = substr(variable, 6,6),
         election = substr(variable, 1,5)) %>%
  select(-variable) %>% filter(party %in% c("R", "D"))


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'AZ') %>%
  mutate(district = as.numeric(CD)) %>%
  select(NAME, partition, district)

## District-level data
nm16 = nm_df %>%
  select(CD, NAME, VOT18D, VOT18R) %>%
  pivot_longer(cols = c(VOT18D, VOT18R)) %>%
  mutate(party = substr(name, 6,6),
         district=CD,
         partition = "AZ")



## Building an ensemble
sf::sf_use_s2(FALSE)

adj  <- redist.adjacency.fix(nm_sf)

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)


adj  <- redist::redist_map(nm_sf, adj=adj, pop_tol = 0.01, total_pop = "TOTPOP", existing_plan = "CD")



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
                               min_pop = nm_df$TOTPOP - nm_df$NH_WHITE
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))

save(smch, file="../data_clean/sims/az_sims.RData")
load("../data_clean/sims/az_sims.RData")

ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:(50001))) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = district) %>%
  bind_rows(partition2012) ###



#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "AZ"
elec = "VOT18"
statename = "Arizona"
partit = "AZ"

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
p3_rich_cond = get_hist(partit, elec, ds, var = "topincome",
                        xaxs_label = "Dem $200k+ Residents", conditional = TRUE)
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
results

#### Unusualness ####
unusualness = get_unusualness(elec, partit, stateabb, save = TRUE)
