# Are cities delivering on climate mitigation? Evidence from 1,000+ European cities
# Angel Hsu, Jonas Tan, Yi Ming Ng, Wayne Toh, Regina Vanda, and Nihit Goyal
# Script for Paper Figures
# February 2020

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(scales)
library(ggpubr)
library(reshape2)
library(tidyverse)
library(UESIplots)
library(extrafont)

# load fonts
devtools::install_github("datadrivenyale/UESIplots")
loadfonts()


# read in data file
x <- read.csv("EUCOM_FinalDataset_forplots_public.csv", stringsAsFactors = F)
stm <- read.csv("stm_prevalences.csv", stringsAsFactors = F) # stm outputs
coefs <- read.csv("Model_coefficients.csv", stringsAsFactors = F)

# Explanation of variables
# name - city name
# iso - 3-letter UN ISO code for city's country
# country - country of city
# lat - latitude of city
# lng - longitude of city
# gdp - city gross domestic product in constant PPP with base year 2010 per capita
# gdp_national - country gross domestic product in constant PPP with base year 2010 per capita
# percent_reduction - emissions reduction target 
# target_year - year targeted for emissions reduction
# baseline_year - baseline emissions year
# baseline_epc - baseline emissions per capita
# inventory_epc - inventory emissions per capita
# PRT_Per_Year -  pro-rated emissions target per year
# peta - pro-rated emissions target achievement 
# topic_1..6 - topic prevalence 
# most_prevalent_topic - most prevalent topic 
# on_track - pro-rated emissions target achievement of either >= 0 (on track); <0 (not on track)
# population_density_ln natural log of population density
# population_ln natural log of population
# time_since_baseline - years from baseline to inventory year
# country_emissions_trend - national emissions reduction trend
# emissions_trend - emission reduction trend of city
# percapita_emissions_trend - per capita emission reductions trend
# annual_percapita_emissions_trend - annualized emissions trend per capita

# ambition - whether the emission reduction is >21
# baseline_after_2004 - whether the city has a baseline after year 2004
# target_after_2020 - whether the city 

# Figure 1
# basemap
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
world_map <- map_data("world")
base_world <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                               colour="light gray", fill="light gray")+
  theme_bw()

# reorder the factors for pro-rated emissions achievement target
x$peta_tiers <- factor(x$peta_tiers, c("<0", "0-1", ">1"))

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_point(data = x, aes(x = lng, y = lat, fill = as.factor(peta_tiers)), shape=21, color="gray", alpha = .7, size=2, na.rm=T) +
  scale_fill_manual(name = expression(paste("Pro-rated emissions achievement, ", rho)), values=c("#E63946","#F1FAEE", "#457B9D"), labels=c("Increased Emissions", "Insufficient Progress", "On track"), na.translate=FALSE) +
  coord_cartesian(xlim = c(-9,45), ylim = c(32,70)) + 
  xlab("")+
  ylab("")+
  theme_UESI()

# Figure 2
ontrack <- x[,grep("\\bpopulation\\b|\\bpop_density\\b|percent_reduction|\\bgdp\\b|annual_percapita_emissions_trend|country_emissions_trend|inventory_epc|no_climate|baseline_epc", colnames(x))]  %>%
  group_by(x$on_track)

names(ontrack)[grep("on_track", colnames(ontrack))] <- "ontrack"
ontrack.m <- melt(ontrack, id="ontrack")

ontrack_means <- ontrack %>%
  group_by(ontrack) %>%
  summarise_all(funs(mean), na.rm = TRUE) %>%
  gather(key=variable, value=value, -ontrack)

var_names <- c(
  `annual_percapita_emissions_trend` = "Annualized pc emissions trend",
  `baseline_epc` = "Baseline emissions per capita",
  `country_emissions_trend` = "National emissions trend",
  `gdp` = "GDP per capita in thousands",
  `inventory_epc` = "Inventory emissions per capita",
  `no_climate_policies` = "No. of national climate policies",
  `percent_reduction` = "Emissions reduction target",
  `pop_density` = "Population density",
  `population` = "Population"
)

ggplot(ontrack.m, aes(x=value, group=ontrack, fill=factor(ontrack)))+
  geom_histogram(position="identity", alpha = 0.3) + 
  scale_fill_manual(name="", values=c("#E63946","#457B9D"), labels=c("Not on track (n=423)", "On track (n=643)"))+
  facet_wrap(~variable, scales="free", labeller = as_labeller(var_names))+
  geom_vline(data = ontrack_means, aes(xintercept = value, colour = factor(ontrack), group=factor(ontrack)), show.legend = FALSE) +
  scale_colour_manual(name="", values=c("#E63946","#457B9D")) +
  xlab("")+
  ylab("Number of cities")+
  theme_UESI()+
  theme(strip.text = element_text(size=12))

## legends for the means/sds of on-track/off-track groups added in illustrator


# Figure 3
label.df <- data.frame(vars = c("Ambition", "Topic 3:energy efficiency", "Baseline emissions per capita", "Nat'l emissions trend"),
                       Estimate = c(-0.535, 2.23, 0.393, 0.487))

ggplot(coefs, aes(vars, Estimate)) + 
  geom_hline(yintercept=0, lty=1, lwd=0.5, colour="grey50") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se, colour=vars), 
                lwd=0.5, width=0) +
  geom_point(size=2, aes(colour=vars)) +
  scale_color_manual(name="", values=c("#E63946","#6BB3DD","#CBF7FF", "#517C96", "#457B9D", "#798897","#E63946","#6BB3DD","#CBF7FF", "#517C96", "#67001F","#D6604D","#FDDBC7", "#D1E5F0", "#4393C3"))+
  geom_text(data = label.df, label = c("*","***","***", "*"), vjust=-0.5, hjust=-0.25)+
  coord_flip() +
  guides(colour=FALSE) +
  ylab("Estimate with standard error") +
  xlab("")+
  theme_UESI()


# Figure 4

prev <- ggplot(stm, aes(x=Prevalence, y=reorder(Topic.number, Prevalence))) +
  geom_point()+
  geom_segment(aes(xend=0,yend=reorder(Topic.number, Prevalence)))+
  geom_text(aes(label=paste("Topic", paste(Topic.number, ":", sep=""),keywords)),hjust=-.02, vjust=0.2, size=3, family="Myriad Pro Light")+
  ylab("")+
  xlab("Expected topic proportions")+
  xlim(c(0,0.6))+
  theme_UESI()+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = "none")

x$most_prevalent_topic <- factor(x$most_prevalent_topic, c("1", "2", "3", "4", "5", "6"))

topic_map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_point(data = x, aes(x = lng, y = lat, fill = as.factor(most_prevalent_topic)), shape=21, color="gray", size=2, na.rm=T) +
  scale_fill_manual(name = "Most Prevalent \nTopic", values=c("#67001F","#D6604D","#FDDBC7", "#D1E5F0", "#4393C3", "#FFFFFF"), 
                    labels=c("1: residential buildings + planning", "2: cross-sectoral integration", "3: energy efficiency", "4: mobility + public transport", "5: buildings + lighting",
                             "6: municipal administration"), na.translate=FALSE) +
  coord_cartesian(xlim = c(-9,45), ylim = c(32,70)) + 
  xlab("")+
  ylab("")+
  theme_UESI() +
  theme(legend.position="bottom", legend.spacing.x = unit(0.1, 'cm'))

ggarrange(prev, topic_map, ncol = 1, nrow=2, heights=c(1,2))
