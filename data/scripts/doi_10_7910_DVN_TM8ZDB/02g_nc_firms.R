### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)


## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/NC_VTD.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83')) %>%
  mutate(NAME = VTD_Key,
         COUNTY = County,
         CD = oldplan-293,
         CD16 = newplan+1,
         
         EL12G_VT_D = (as.numeric(EL10G_USS_) +as.numeric(EL12G_GV_D ) +as.numeric(EL12G_PR_D ) +as.numeric(EL14G_US_1))/4,
         EL12G_VT_R = (as.numeric(EL10G_US_1) +as.numeric(EL12G_GV_R) +as.numeric(EL12G_PR_R) +as.numeric(EL14G_USS_))/4,
         EL16G_VT_D = (as.numeric(EL16G_PR_D) +as.numeric(EL16G_US_1 ) +as.numeric(EL16G_GV_D ) +as.numeric(EL14G_US_1))/4,
         EL16G_VT_R = (as.numeric(EL16G_PR_R) +as.numeric(EL16G_USS_) +as.numeric(EL16G_GV_R) +as.numeric(EL14G_USS_))/4,
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

geos <- c('NAME', 'VTD', 'COUNTY', 'CD')
short <- c('EL12G_VT', 'EL12G_PR', 'EL16G_GV', 'EL16G_PR')
election <- c('2012 Avg Vote',
              '2012 President',
              '2016 Governor',
              '2016 President')


esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = substr(variable, 10,11),
         election = substr(variable, 3,8)) %>%
  select(-variable) %>% filter(party %in% c("R", "D"))


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'NC12') %>%
  mutate(district = as.character(as.numeric(CD))) %>%
  select(NAME, partition, district)

partition2016 <- nm_df %>%
  mutate(partition = 'NC16') %>%
  mutate(district = as.character(as.numeric(CD16))) %>%
  select(NAME, partition, district)


## District-level data
nm12 = nm_df %>%
  select(CD, VTD, EL12G_VT_D, EL12G_VT_R) %>%
  pivot_longer(cols = c(EL12G_VT_D, EL12G_VT_R)) %>%
  mutate(party = gsub("G12", "", name),
         district=CD,
         partition = "NC12")


nm16 = nm_df %>%
  select(CD, VTD, EL16G_VT_D, EL16G_VT_R) %>%
  pivot_longer(cols = c(EL16G_VT_D, EL16G_VT_R)) %>%
  mutate(party = gsub("G12", "", name),
         district=CD,
         partition = "NC16")



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
                               strength = 50,
                               min_pop = nm_df$TOTPOP - nm_df$NH_WHITE
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))


save(smch, file="../data_clean/sims/nc_sims.RData")
load("../data_clean/sims/nc_sims.RData")

ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:50001)) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = as.character(district)) %>%
  bind_rows(partition2012, partition2016)


#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "NC"
partit = "NC12"
partit2 = "NC16"
elec = "12G_VT"
statename = "North Carolina"

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
results = get_pvals(elec, partit, firms3, ds, "NC12", save = TRUE)
results2 = get_pvals(elec, partit2, firms3, ds, "NC16", save = TRUE)


#### Unusualness ####
unusualness = get_unusualness(elec, partit, stateabb, save = TRUE)
