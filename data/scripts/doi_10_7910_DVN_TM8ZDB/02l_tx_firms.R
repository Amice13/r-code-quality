### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)

## Load the shapefiles
nm_sf <- sf::st_read(dsn = "../data_raw/shapefiles/tx_vtds.shp", # shapefile name from MGGG --
                     quiet = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs('NAD83'))%>%
  mutate(NAME =CNTYVTD,
         VOT12D = (as.numeric(PRES12D) + as.numeric(SEN12D) + as.numeric(SEN14D) + as.numeric(GOV14D) )/4,
         VOT12R = (as.numeric(PRES12R) + as.numeric(SEN12R) + as.numeric(SEN14R) + as.numeric(GOV14R))/4,
         CD = USCD,
         minpop = TOTPOP - WHITE,
         white = WHITE,
         black = BLACK,
         hispanic = HISPANIC,
         pop = TOTPOP)

## as simple df for viewing -- 
nm_df <- nm_sf %>%
  data.table::data.table() %>%  
  select(-geometry) 

geos <- c('NAME', 'VTD', 'COUNTY', 'USCD')
short <- c('VOT12', 'SEN12', 'SEN14', 'GOV14')
election <- c('2012 Avg Vote',
              '2012 Senator',
              '2014 Senator',
              '2014 Governor')

esum = data.frame(geos = geos, short = short, election = election)

ecs <- grep(paste0(esum$short, collapse = '|'), colnames(nm_df), value = T)
returns <- data.table::melt(nm_df, geos, ecs) %>%
  mutate(party = gsub('^.*[0-9]', '', variable),
         election = gsub('.$', '', variable)) %>%
  select(-variable)


## Which precincts are in which districts?
partition2012 <- nm_df %>%
  mutate(partition = 'TX2012') %>%
  mutate(district = as.character(as.numeric(CD))) %>%
  select(CNTYVTD, partition, district)


## Building an ensemble

adj  <- redist::redist_map(nm_sf, total_pop = "TOTPOP", existing_plan = "CD",
                           pop_tol=0.01)


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
                           pop_temper = 0.01,
                           counties = as.numeric(as.factor(nm_df$COUNTY)),
                           constraints = list(
                             vra = list(
                               strength = 50,
                               min_pop = nm_df$TOTPOP - nm_df$WHITE
                             ),
                             multisplits = list(
                               strength = 0
                             )
                           ))

save(smch, file="../data_clean/sims/tx_sims.RData")
load("../data_clean/sims/tx_sims.RData")

nsims=50000
ensembles <- redist::get_plans_matrix(smch) %>%
  data.table::data.table() %>%
  mutate(NAME = nm_df$NAME) %>%
  data.table::melt(., 'NAME', c(1:(nsims+1))) %>%
  rename(partition = variable,
         district = value) %>%
  mutate(partition = as.character(partition),
         district = as.character(district)) %>%
  bind_rows(partition2012) %>% 
  select(-CNTYVTD)###
rm(smch)



#### Get precinct-level data set of firms etc plus covariates ####
source("00_utils.R")
stateabb = "TX"
partit = "USCD"
elec = "VOT12"
statename = "Texas"

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


### Extra stuff
ds$firms[ds$partition==partit]
mean(ds$seats==11)

mean(ds$firms <= 215)
mean(ds$firms[ds$seats==11] <= 215)

min(ds$seats)
max(ds$seats)
max(ds$firms)
min(ds$firms)

max(ds$firms[ds$seats==11])
min(ds$firms[ds$seats==11])

median(ds$firms)
mean(ds$firms)

p6 = ds %>% filter(seats==11) %>%
  ggplot(aes(x=firms)) +
  geom_bar(fill = 'steelblue2') +
  theme_bw() + theme(panel.border = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.text.y = element_blank()) +
  xlab('Texas: Firms in Democratic districts') + ylab('') +
  geom_vline(xintercept = 215, lty='dashed') +
  scale_x_continuous(breaks = scales::pretty_breaks())
p6
ggsave('../output/plots/tx_extra.pdf')



### Which firms are in strange districts?

load("./state_sims/tx.RData")
sf::sf_use_s2(FALSE)
## I think I need the x2 and ensembles objects

ens1 <- x2 %>%
  filter(election == "VOT12") %>%
  mutate(district = as.character(district)) %>%
  group_by(partition, district) %>%
  mutate(margin = max(value)/(max(value)+min(value))) %>%
  filter(party == party[which.max(value)])%>% 
  select(partition, district, party) %>%
  right_join(ensembles)

ens2012 <- ens1 %>% filter(partition == "TX2012")

ens2 <- ens1 %>%
  group_by(CNTYVTD) %>%
  summarize(prop.d = mean(party == "D")) %>%
  left_join(ens2012)

## Merge the "unusualness" outcome with precinct data
ens2b <- ens2 %>%
  left_join(nm_df, by="CNTYVTD")
write.csv(ens2b, file="./which/tx_unusualness.csv")

load("firms/tx_firms.RData")
firms1 = sf::st_as_sf(x = firms1, coords = c("lon", "lat"), crs="NAD83")


ens3 <- ens2 %>% left_join(firms3) %>%
  filter((prop.d > 0.95 & party == "R") | (prop.d < 0.05 & party == "D")) %>%
  filter(firms >= 1)

firms2 = sp::over(as(firms1, "Spatial"), as(nm_sf, "Spatial"))
firms4 = cbind(firms2$CNTYVTD, firms1[,c("obj", "type")])

dat2 = firms4
write.csv(firms4, file="./which/tx_firms.csv")


dat2 = dat2 %>% select(-geometry) %>%
  group_by(firms2.CNTYVTD, type) %>%
  summarize(count = n()) %>%
  pivot_wider(id_cols = "firms2.CNTYVTD", names_from = "type", values_from = "count",
              values_fill = 0)

dat3 = ens2b %>% left_join(dat2, by = c("CNTYVTD" = "firms2.CNTYVTD")) %>%
  replace_na(list(firm=0, walmart=0,hospital=0,airport=0,pacfirm=0))


write.csv(dat3, file="./which/tx_unusualness2.csv")




ens4 = merge(firms4, ens3, by.x="firms2.CNTYVTD", by.y="CNTYVTD")
ens4$label = toupper(letters[-26])

ens5 = ens2012 %>% select(-CNTYVTD)
ens5 = ens5[!duplicated(ens5),]

## To plot
test = nm_sf %>%
  mutate(district =  as.character(as.numeric(USCD))) %>%
  group_by(district) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  left_join(ens5, by="district")

lc <- tmaptools::geocode_OSM (q = 'Austin, Texas', as.sf = T)
lc$bbox <- sf::st_set_crs(lc$bbox, sf::st_crs(test))
cropped <- sf::st_crop(test, lc$bbox)
ens4_cropped <- sf::st_crop(ens4, lc$bbox)
  
cropped %>%
  ggplot(aes(geometry = geometry)) + 
  geom_sf(aes(fill = party),
          color = 'white',
          alpha = .65,
          lwd = .3) +
  scale_fill_manual(values = c('blue', 'red')) +
  geom_sf_text(aes(label = district),
               color = 'black',
               size = 2.5, 
               check_overlap = TRUE) +
  theme_minimal() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = 'none',
        panel.grid = element_blank()) +
  xlab('') + ylab('') +
  geom_sf_text(data = ens4_cropped, aes(geometry = geometry, label=label))
