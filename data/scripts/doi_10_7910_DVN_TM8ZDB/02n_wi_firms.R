### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/WI_ltsb_corrected_final.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(VOTDEM12 = (as.numeric(USHDEM12) + as.numeric(USHDEM16) + as.numeric(PREDEM12) + as.numeric(PREDEM16) )/4,
         VOTREP12 = (as.numeric(USHREP12) + as.numeric(USHREP16) + as.numeric(PREREP12) + as.numeric(PREREP16) )/4,
         minpop = PERSONS - WHITE,NAME = GEOID10,
         COUNTY = CNTY_NAME,
         CD = CON,
         white = WHITE,
         black = BLACK,
         hispanic = HISPANIC,
         pop = PERSONS)

## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry)

geos <- c('NAME', 'GEOID10', 'COUNTY', 'CD')
short <- c('USH.*12', 'WSS.*12', 'WSA.*12', 'VOT.*12')
election <- c('2012 House',
              '2012 State Senate',
              '2012 State Assembly',
              '2012 Senate')


esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = substr(variable, 4,6),
         election1 = substr(variable, 1,3),
         election2 = substr(variable,7,8),
         election = paste0(election1, election2)) %>%
  select(-variable) %>% filter(party %in% c("REP", "DEM"))
returns$party[returns$party=="DEM"] = "D"
returns$party[returns$party=="REP"] = "R"


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'WI') %>%
  mutate(district = as.numeric(CD)) %>%
  select(NAME, partition, district)

## District-level data
nm16 = nm_df %>%
  select(CD, NAME, VOTDEM12, VOTREP12) %>%
  pivot_longer(cols = c(VOTDEM12, VOTREP12)) %>%
  mutate(party = substr(name, 4,6),
         district=CD,
         partition = "WI")



## Building an ensemble
sf::sf_use_s2(FALSE)

adj  <- redist.adjacency.fix(nm_sf)

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)

nm_sf %>% mutate(rown = row_number()) %>%
  ggplot() +
  geom_sf(aes(fill = as.character(cont$component))) +
  labs(fill = 'comp') +
  geom_sf_text(aes(label = rown),cex=5)
ggsave("tmp.pdf", scale=20, limitsize=F)

adj <- adj %>% geomander::add_edge(v1  = 1396, v2 = 1353)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 185)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 186)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 163)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 162)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 73)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 158)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 159)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 160)
adj <- adj %>% geomander::add_edge(v1  = 64, v2 = 161)

adj <- adj %>% geomander::add_edge(v1  = 1388, v2 = 1393)
adj <- adj %>% geomander::add_edge(v1  = 1370, v2 = 1393)

adj <- adj %>% geomander::add_edge(v1  = 1387, v2 = 1370)
adj <- adj %>% geomander::add_edge(v1  = 1372, v2 = 1376)
adj <- adj %>% geomander::add_edge(v1  = 1372, v2 = 1379)

adj <- adj %>% geomander::add_edge(v1  = 1356, v2 = 1360)
adj <- adj %>% geomander::add_edge(v1  = 1356, v2 = 1365)
adj <- adj %>% geomander::add_edge(v1  = 1356, v2 = 1379)
adj <- adj %>% geomander::add_edge(v1  = 1356, v2 = 1379)

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)

adj = redist::redist_map(nm_sf, pop_tol = 0.01, adj = adj, total_pop = "PERSONS",existing_plan = "CD")



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
                               min_pop = nm_df$minpop
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))


save(smch, file="../data_clean/sims/wi_sims.RData")
load(file = "../data_clean/sims/wi_sims.RData")

ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:50001)) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = district) %>%
  bind_rows(partition2012) ###
rm(smch)



#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "WI"
partit = "WI"
elec = "VOT12"
statename = "Wisconsin"

get_ds(stateabb = stateabb, elec = elec)

table(ds$election)
head(ds$partition)
tail(ds$partition)

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
