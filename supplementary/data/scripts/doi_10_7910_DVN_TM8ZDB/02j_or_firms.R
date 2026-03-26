### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)


## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/OR_precincts.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(NAME = Precinct,
         COUNTY = County,
         CD = CD,
         VOT12D = (as.numeric(SOS16D) + as.numeric(AG16D) + as.numeric(GOV16D)+ as.numeric(USH16D) + as.numeric(PRES16D) )/5,
         VOT12R = (as.numeric(SOS16R) + as.numeric(AG16R) + as.numeric(GOV16R)+ as.numeric(USH16R) + as.numeric(PRES16R))/5,
         minpop = TOTPOP - NH_WHITE,
         white = NH_WHITE,
         black = NH_BLACK,
         hispanic = HISP,
         pop = TOTPOP)

## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry)

geos <- c('NAME', 'Pct_Name', 'COUNTY', 'CD')
short <- c('SOS16', 'SEN16', 'USH16', 'VOT12')
election <- c('2012 President',
              '2016 President',
              '2008 President',
              '2012 Avg Vote')


esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = substr(variable, 6,6),
         election = substr(variable, 1,5)) %>%
  select(-variable) %>% filter(party %in% c("R", "D"))


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'OR') %>%
  mutate(district = as.numeric(CD)) %>%
  select(NAME, partition, district)

## District-level data
nm16 = nm_df %>%
  select(CD, NAME, VOT12D, VOT12R) %>%
  pivot_longer(cols = c(VOT12D, VOT12R)) %>%
  mutate(party = substr(name, 6,6),
         district=CD,
         partition = "OR")




redist.adjacency.fix <- function(shp){
  adj_spdep <- spdep::poly2nb(shp, queen = FALSE, snap = .0001)
  adj_spdep <- lapply(adj_spdep, function(x){x-1L})
  adj_spdep <- lapply(adj_spdep, function(x){if(all(x == -1L)){integer(0)} else {x} })
  return(adj_spdep)
}
## Building an ensemble
adj  <-redist::redist.adjacency(nm_sf)

cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
max(cont$component)

adj <- adj %>% geomander::add_edge(v1 = rep(1270, 2),
                                   v2 = c(1283,1279))

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

save(smch, file="../data_clean/sims/or_sims.RData")
load("../data_clean/sims/or_sims.RData")

nsims=50000
ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:(nsims+1))) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = district) %>%
  bind_rows(partition2012) ###


#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = partit = "OR"
elec = "VOT12"
statename = "Oregon"

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

  
#### Extra stuff
#load("./state_sims/or.RData")
max(ds$seats)
max(ds$firms)
min(ds$firms)
min(ds$firms[ds$seats==4])
max(ds$firms[ds$seats==4])

mean(ds$seats >= 4)
mean(ds$firms >= ds$firms[ds$partition=="OR"])

sum(ds$seats==4)

mean(ds$firms[ds$seats==4] >= 44)
sum(ds$firms[ds$seats==4]==44)
sum(ds$firms[ds$seats==4]<44)
sum(ds$firms[ds$seats==4]==45)

partit="OR"

p6 = ds %>% filter(seats==4) %>%
  ggplot(aes(x=firms)) +
  geom_bar(fill = 'steelblue2') +
  theme_bw() + theme(panel.border = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.text.y = element_blank()) +
  xlab('Oregon: Firms in Democratic districts') + ylab('') +
  geom_vline(xintercept = ds$firms[ds$partition==partit], lty='dashed') +
  scale_x_continuous(breaks= 43:45,labels = c(43,44,45))
p6
ggsave("../output/plots/or_extra.pdf")

