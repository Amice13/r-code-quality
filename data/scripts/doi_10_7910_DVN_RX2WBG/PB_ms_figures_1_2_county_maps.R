# The Following R Code replicates the following items in the manuscript titled: "Nail in the Coffin or Lifeline? Evaluating the Electoral Impact of COVID-19 on President Trump in the 2020 Election"". The items are the following:

# 1) Figure 1: Change in County-Level Support for President Trump during the 2020 Election
# 2) Figure 2: COVID-19 Severity Across U.S. Counties

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Library #####################
library(tidyverse)
library(urbnmapr)
library(ggthemes)

######################## Upload Data ##################

#Set Local Working Directory
setwd("/PB Data Replication Files")

#Upload Data
load(file = "county_level_data.rda"); Pres_Master.1 <- Pres_Master.Pure

######################### County Map Geographic Data ##################

counties_sf <- get_urbn_map("counties", sf = TRUE)

######################### Examine Data ##################
head(Pres_Master.1)
head(counties_sf)

colnames(Pres_Master.1)
colnames(counties_sf)

####################### Checks ###################
#Make Sure State And County FIPS Are the Same
table(nchar(counties_sf$county_fips))

names(counties_sf)[names(counties_sf) == 'county_fips'] <- 'state_county_fips'

####################### Data Management ##############
#*********************** Create Dependent Variable ********************
Pres_Master.1$per_gop_2016 <- (Pres_Master.1$votes_gop_2016/(Pres_Master.1$votes_gop_2016+Pres_Master.1$votes_dem_2016)) * 100
Pres_Master.1$per_dem_2016 <- (Pres_Master.1$votes_dem_2016/(Pres_Master.1$votes_gop_2016+Pres_Master.1$votes_dem_2016)) * 100

Pres_Master.1$per_gop <- (Pres_Master.1$votes_gop/(Pres_Master.1$votes_gop+Pres_Master.1$votes_dem)) * 100
Pres_Master.1$per_dem <- (Pres_Master.1$votes_dem/(Pres_Master.1$votes_gop+Pres_Master.1$votes_dem)) * 100

Pres_Master.1$per_dem_2016 <- (Pres_Master.1$per_dem_2016/(Pres_Master.1$per_dem_2016+Pres_Master.1$per_gop_2016))*100
Pres_Master.1$per_gop_2016 <- (Pres_Master.1$per_gop_2016/(Pres_Master.1$per_dem_2016+Pres_Master.1$per_gop_2016))*100

Pres_Master.1$gop_dem_margin_2020 <- Pres_Master.1$per_gop - Pres_Master.1$per_dem
Pres_Master.1$gop_dem_margin_2016 <- Pres_Master.1$per_gop_2016 - Pres_Master.1$per_dem_2016

Pres_Master.1$gop_vote_change_2020_lagged <- Pres_Master.1$gop_dem_margin_2020-Pres_Master.1$gop_dem_margin_2016 

#Verify
cor(Pres_Master.1$gop_vote_change_2020_lagged, Pres_Master.1$relative_gop_margin_change_two_party, use = "complete")

subset(Pres_Master.1, state_abb=="MO" & county_name == "Jackson County")$relative_gop_margin_change_two_party
subset(Pres_Master.1, state_abb=="MO" & county_name == "Jackson County")$gop_vote_change_2020_lagged

##################### Merge Data with Map #######################

Pres_Master.2 <- left_join(counties_sf,
                           Pres_Master.1,
                           by = c("state_county_fips"))

apply(Pres_Master.2, 2, function(x) sum(is.na(x)))
##################### Maps ##################

#**************** List of Maps ****************
# 1. gop_vote_change_2020_lagged
# 2. total_covid_cases_100K
# 3. total_covid_deaths_100K
# 4. mean_covid_cases_1w_100K
# 5. mean_covid_deaths_1w_100K

#**************** gop_vote_change_2020_lagged ********************
P1 <- ggplot(subset(Pres_Master.2, state_abbv != "AK")) +
  geom_sf(mapping = aes(fill = gop_vote_change_2020_lagged),
          color = "lightgrey", size = 0.05) +
  coord_sf(datum = NA) +
  theme(panel.background = element_blank() ) +
  scale_fill_gradient2("2016-2020 Relative Partisan Swing",
                       low = "blue",
                       mid = "white",
                       high = "red", 
                       na.value = "grey86",
                       midpoint = 0) +
  # theme_map() +
  #labs(title = "Relative Partisan Swing in Republican Presidential Vote Share", subtitle = "2016 and 2020 Election") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position="bottom"); P1


ggsave(P1, 
       file = "fig_1_presidential_swing.png",
       width=7, height=6,  dpi = 300)

#**************** total_covid_cases_100K ********************
P2 <- ggplot(subset(Pres_Master.2, state_abbv != "AK")) +
  geom_sf(mapping = aes(fill = total_covid_cases_100K/100),
          color = "lightgrey", size = 0.05) +
  coord_sf(datum = NA) +
  theme(panel.background = element_blank()) +
  scale_fill_gradient2("Total Cases Per 100K (in Hundreds)",
                       low = "white",
                       high = "red",
                       na.value = "grey86") +
  # theme_map() +
  #labs(title = "Total Number of COVID-19 Cases Per 100K", subtitle = "As of November 2, 2020") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position="bottom"); P2


ggsave(P2, 
       file = "fig2c_total_cases.png",
       width=7, height=6,  dpi = 300)


#**************** total_covid_deaths_100K ********************
P3 <- ggplot(subset(Pres_Master.2, state_abbv != "AK")) +
  geom_sf(mapping = aes(fill = total_covid_deaths_100K),
          color = "lightgrey", size = 0.05) +
  coord_sf(datum = NA) +
  theme(panel.background = element_blank()) +
  scale_fill_gradient2("Total Deaths Per 100K",
                       low = "white",
                       high = "red",
                       na.value = "grey86") +
  #  theme_map() +
  #labs(title = "Total Number of COVID-19 Deaths Per 100K", subtitle = "As of November 2, 2020") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position="bottom"); P3


ggsave(P3, 
       file = "fig2a_total_deaths.png",
       width=7, height=6,  dpi = 300)


#**************** mean_covid_cases_1w_100K ********************

P4 <- ggplot(subset(Pres_Master.2, state_abbv != "AK")) +
  geom_sf(mapping = aes(fill = mean_covid_cases_1w_100K / 100),
          color = "lightgrey", size = 0.05) +
  coord_sf(datum = NA) +
  theme(panel.background = element_blank()) +
  scale_fill_gradient2("Average Daily Change in Cases Per 100K (in Hundreds)",
                       low = "white",
                       high = "red",
                       na.value = "grey86") +
  # theme_map() +
  #labs(title = "Average Daily Change in COVID-19 Cases Per 100K People", subtitle = "One Week Before November 3, 2020") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position="bottom"); P4

ggsave(P4, 
       file = "fig2b_trend_cases.png",
       width=7, height=6,  dpi = 300)


#**************** mean_covid_deaths_1w_100K ********************

P5 <- ggplot(subset(Pres_Master.2, state_abbv != "AK")) +
  geom_sf(mapping = aes(fill = mean_covid_deaths_1w_100K),
          color = "lightgrey", size = 0.05) +
  coord_sf(datum = NA) +
  theme(panel.background = element_blank()) +
  scale_fill_gradient2("Average Daily Change in Deaths Per 100K",
                       low = "white",
                       high = "red",
                       na.value = "grey86") +
  # theme_map() +
  #labs(title = "Average Daily Change in COVID-19 Deaths Per 100K People", subtitle = "One Week Before November 3, 2020") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position="bottom");P5

ggsave(P5, 
       file = "fig2d_trend_deaths.png",
       width=7, height=6,  dpi = 300)

