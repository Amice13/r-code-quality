### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)


## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/oh_precincts.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = PRECODE,
         COUNTY = CNTYFIPS,
         PRS16R = PRES16R,
         PRS16D = PRES16D,
         VOT12D = (as.numeric(PRS16D) +as.numeric(SEN16D  ) +as.numeric(USH16D  ) +as.numeric(SSEN16D))/4,
         VOT12R = (as.numeric(PRS16R) +as.numeric(SEN16R ) +as.numeric(USH16R ) +as.numeric(SSEN16R))/4,
         minpop = TOTPOP - NH_WHITE,
         white = NH_WHITE,
         black = NH_BLACK,
         hispanic = HISP,
         pop = TOTPOP
  )

## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry)

geos <- c('NAME', 'PRECINCT', 'COUNTY', 'CD')
short <- c('PRS16', 'SEN16', 'USH16', 'VOT12')
election <- c('2018 House',
              '2018 Governor',
              '2018 Sec of State',
              '2012 Avg Vote')


esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = substr(variable, 6,6),
         election = substr(variable, 1,5)) %>%
  select(-variable) %>% filter(party %in% c("R", "D"))


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'OH') %>%
  mutate(district = as.numeric(CD)) %>%
  select(NAME, partition, district)

## District-level data
nm16 = nm_df %>%
  select(CD, NAME, VOT12D, VOT12R) %>%
  pivot_longer(cols = c(VOT12D, VOT12R)) %>%
  mutate(party = substr(name, 6,6),
         district=CD,
         partition = "OH")


redist.adjacency.fix <- function(shp){
  adj_spdep <- spdep::poly2nb(shp, queen = FALSE, snap = .0001)
  adj_spdep <- lapply(adj_spdep, function(x){x-1L})
  adj_spdep <- lapply(adj_spdep, function(x){if(all(x == -1L)){integer(0)} else {x} })
  return(adj_spdep)
}
## Building an ensemble
adj  <-redist.adjacency.fix(nm_sf)

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)

nm_sf %>% mutate(rown = row_number()) %>%
  ggplot() +
  geom_sf(aes(fill = as.character(cont$component))) +
  labs(fill = 'comp') +
  geom_sf_text(aes(label = rown),cex=5)
ggsave("tmp.pdf", scale=20, limitsize=F)

adj <- adj %>% geomander::add_edge(v1 = rep(4370, 9),
                                   v2 = c(2222,4379,4378,4372,4377,
                                          4376,4380,2228,2238))

adj <- adj %>% geomander::add_edge(v1 = c(2430, 2430,2430,6585,6585),
                                   v2 = c(6570,6577,6585,6582,6583))

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)

adj = redist::redist_map(nm_sf, adj = adj, total_pop = "TOTPOP",existing_plan = "CD")


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

save(smch, file="../data_clean/sims/oh_sims.RData")
load("../data_clean/sims/oh_sims.Rdata")

ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:50001)) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = district) %>%
  bind_rows(partition2012) ###




#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = partit = "OH"
elec = "VOT12"
statename = "Ohio"

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
