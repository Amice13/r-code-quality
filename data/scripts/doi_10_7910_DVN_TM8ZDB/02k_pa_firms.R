### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)



## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/PA.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(PRS12D = PRES12D, PRS12R = PRES12R) %>%
  mutate(NAME = NAME10,
         COUNTY = as.factor(COUNTYFP10),
         CD = REMEDIAL,
         PRS12D = PRES12D,
         PRS12R = PRES12R,
         USS16D = T16SEND,
         USS16R = T16SENR,
         PRS16D = T16PRESD,
         PRS16R = T16PRESR,
         VOT12D = (as.numeric(PRES12D) +as.numeric(USS16D ) +as.numeric(PRS16D ) +as.numeric(USS12D))/4,
         VOT12R = (as.numeric(PRES12R) +as.numeric(USS16R) +as.numeric(PRS16R) +as.numeric(USS12R))/4,
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

geos <- c('NAME', 'GEOID10', 'CD_2011', 'REMEDIAL')
short <- c('PRS12', 'USS12', 'USS16', 'VOT12')
election <- c('2012 President',
              '2012  Senate',
              '2016 Senate',
              '2016 Avg Vote')


esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = substr(variable, 6,6),
         election = substr(variable, 1,5)) %>%
  select(-variable) %>% filter(party %in% c("R", "D"))


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'PA18') %>%
  mutate(district = as.numeric(CD)) %>%
  select(NAME, partition, district)

partition2018 <- nm_df %>%
  mutate(partition = 'PA12') %>%
  mutate(district = as.numeric(CD_2011)) %>%
  select(NAME, partition, district)


## District-level data
nm18 = nm_df %>%
  select(CD, NAME, VOT12D, VOT12R) %>%
  pivot_longer(cols = c(VOT12D, VOT12R)) %>%
  mutate(party = substr(name, 6,6),
         district=CD,
         partition = "PA18")

nm16 = nm_df %>%
  select(CD_2011, NAME, VOT12D, VOT12R) %>%
  pivot_longer(cols = c(VOT12D, VOT12R)) %>%
  mutate(party = substr(name, 8,8),
         district=CD_2011,
         partition = "PA12")

## Building an ensemble
adj  <- redist::redist_map(nm_sf, total_pop = "TOTPOP", existing_plan = "CD")

#cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(nm_sf)))
#max(cont$component)


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
                               strength = 0,
                               min_pop = nm_df$TOTPOP - nm_df$NH_WHITE
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))

save(smch, file="../data_clean/sims/pa_sims.RData")
load("../data_clean/sims/pa_sims.RData")

nsims=50000
ensembles <- redist::get_plans_matrix(smch) %>% 
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:(nsims+1))) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = district) %>%
  bind_rows(partition2012, partition2018) ###



#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "PA"
partit = "PA12"
partit2 = "PA18"
elec = "VOT12"
statename = "Pennsylvania"

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



### Extra stuff
#load("./state_sims/pa.RData")

max(ds$firms)
min(ds$firms)

seats = ds$seats[ds$partition==partit]
seats2 = ds$seats[ds$partition==partit2]

firms = ds$firms[ds$partition==partit]
firms2 = ds$firms[ds$partition==partit2]

max(ds$firms[ds$seats==seats])
min(ds$firms[ds$seats==seats])

max(ds$firms[ds$seats==seats2])
min(ds$firms[ds$seats==seats2])

sum(ds$seats==4)/nrow(ds)
sum(ds$seats==4)

mean(ds$firms <= 114)
sum(ds$firms <= 114)

sum(ds$firms < 114)

table(ds$firms[ds$seats==4])

ds %>% filter(seats==ds$seats[ds$partition==partit],
              !partition %in% c("PA12", "PA18")) %>%
  ggplot(aes(x=firms)) +
  geom_bar(fill = 'steelblue2') +
  theme_bw() + theme(panel.border = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.text.y = element_blank()) +
  xlab('Pennsylvania: Firms in Democratic districts') + ylab('') +
  geom_vline(xintercept = ds$firms[ds$partition==partit], lty='dashed')
ggsave("../output/plots/pa_extra.pdf")

