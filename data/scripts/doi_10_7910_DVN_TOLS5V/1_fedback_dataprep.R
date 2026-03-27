# --------------------------------------------------------------------- # 
# Federalism and Democratic Backsliding from a Comparative Perspective 
# Kaufman, Kelemen, Kolcak 
# Preparation of the Main Dataset
# --------------------------------------------------------------------- # 

rm(list = ls())

## set working directory, e.g. 
#setwd("~/Dropbox/fedbacksliding replication")

## Packages
library(dplyr)
library(ggplot2)
library(gghighlight)
library(gridExtra)
library(stringr)
library(reshape2)
library(ggthemes)
library(pander)
library(cowplot)

## Load the data
vdem12 <- read.csv("vdem-v12.csv")

#############################################
## Create a Dataset for the Analysis Sample 
#############################################
# 1. Identifying Democracies 
# 2. Measurement of Backsliding 

## subset to the time period 1974 - 2021 
vdem12_dat <- vdem12[vdem12$year >= 1974,]

## check the range 
range(vdem12_dat$year)

## Select necessary variables
vdem12_dat <- vdem12_dat %>% dplyr::select(country_name, country_id, COWcode, 
                                    year, v2x_libdem, v2x_libdem_sd, v2x_libdem_codehigh,v2x_libdem_codelow,
                                    v2x_polyarchy, v2x_polyarchy_sd, v2x_polyarchy_codehigh,
                                    v2x_polyarchy_codelow,
                                    v2x_regime,
                                    v2xel_locelec, v2xel_regelec, 
                                    v2elreggov, v2elregnam, v2elsrgel,
                                    v2ellocgov, v2ellocnam, v2ellocpwr,
                                    v2elffelr, v2elffelrbin, v2elsnlsff, 
                                    v2pssunpar, v2cacamps,
                                    v2cacamps)

## Create a new variable for federal systems 
# Based on Forum of Federations plus 3 more countries 
# https://forumfed.org/federal-countries/
# Forum of Federations list of federal countries: 
# Argentina, Australia, Austria, Belgium, Bosnia and Herzegovina, 
# Brazil, Canada, Comoros, Cyprus, Ethiopia, Germany, India, Malaysia, Mexico, 
# Micronesia, Nepal, Nigeria, Pakistan, Russia, Saint Kitts and Nevis, South Africa, 
# Spain, Switzerland, United Arab Emirates, and United States of America (N = 25). 
# We exclude Micronesia and Saint Kitts and Nevis because they are not included among 
# the countries in the V-Dem data set. 
# We include Iraq, Sudan, and Venezuela (N = 26). 

vdem12_dat$fedvsuni <- ifelse(vdem12_dat$country_name %in% c('Argentina', 'Australia', 'Austria', 'Belgium',
                                                             'Bosnia and Herzegovina','Brazil', 'Canada',
                                                             'Comoros', 'Cyprus','Ethiopia','Germany','India', 
                                                             'Iraq','Malaysia', 'Mexico', 'Nepal', 
                                                             'Nigeria', 'Pakistan', 'Russia',
                                                             'South Africa', 'Spain', 'Sudan', 
                                                             'Switzerland', 'United Arab Emirates',
                                                             'United States of America', 
                                                             'Venezuela'),'federal', 'unitary')

## Create a new variable for polyarchy threshold 
vdem12_dat$poly0.5 <- ifelse(vdem12_dat$v2x_polyarchy >= 0.5, 1, 0)

## Create a new variable for liberal democracy score (ld)
vdem12_dat$ld <- vdem12_dat$v2x_libdem

## Create a new variable for polyarchy (ed)
vdem12_dat$ed <- vdem12_dat$v2x_polyarchy

## Compute max. liberal democracy score (max_ld)
for(i.country in unique(vdem12_dat$country_name)) {
  vdem12_dat$max_ld[vdem12_dat$country_name == i.country] <-
    max(na.omit(vdem12_dat$ld[vdem12_dat$country_name == i.country]))
}

## Compute C.I.s for max ld upper bound (max_ld_ub) and lower bound (max_ld_lb)
## max ld ub
for(i.country in unique(vdem12_dat$country_name)) {
  vdem12_dat$max_ld_ub[vdem12_dat$country_name == i.country] <-
    vdem12_dat$v2x_libdem_codehigh[vdem12_dat$country_name ==
                                     i.country][which.max(vdem12_dat$v2x_libdem[vdem12_dat$country_name
                                                                                == i.country])]
}
## max ld lb
for(i.country in unique(vdem12_dat$country_name)) {
  vdem12_dat$max_ld_lb[vdem12_dat$country_name == i.country] <-
    vdem12_dat$v2x_libdem_codelow[vdem12_dat$country_name ==
                                    i.country][which.max(vdem12_dat$v2x_libdem
                                                         [vdem12_dat$country_name == i.country])]
}

## Compute max. electoral democracy score (max_ed)
for(i.country in unique(vdem12_dat$country_name)) {
  vdem12_dat$max_ed[vdem12_dat$country_name == i.country] <-
    max(na.omit(vdem12_dat$ed[vdem12_dat$country_name == i.country]))
}

## Compute C.I.s for for max_ed upper bound (max_ed_ub) and lower bound (max_ed_lb)
for(i.country in unique(vdem12_dat$country_name)) {
  vdem12_dat$max_ed_ub[vdem12_dat$country_name == i.country] <-
    vdem12_dat$v2x_polyarchy_codehigh[vdem12_dat$country_name == i.country][which.max(vdem12_dat$v2x_polyarchy[vdem12_dat$country_name
                                                                                                               == i.country])]
}

for(i.country in unique(vdem12_dat$country_name)) {
  vdem12_dat$max_ed_lb[vdem12_dat$country_name == i.country] <-
    vdem12_dat$v2x_polyarchy_codelow[vdem12_dat$country_name == i.country][which.max(vdem12_dat$v2x_polyarchy[vdem12_dat$country_name
                                                                                                              == i.country])]
}

## count ed_sig for consecutive years 
# eight or more consecutive years with a score of at least 0.5 on the V-Dem Electoral Democracy (ED) Index. 
counter.df <- vdem12_dat %>%
  group_by(country_name) %>%
  mutate(
    sequence = data.table::rleid(poly0.5 == 1),
  ) %>%
  filter(
    poly0.5 == 1
  ) %>%
  group_by(
    country_name, sequence
  ) %>%
  summarise(
    length = n()
  ) %>%
  summarise(
    max = max(length)
  ) %>% filter(max >=8)

# ** This is universe of democracies, N = 105 ** 

## create a vector for democracies and use it to create the ed_sig variable 
dem_vec <- counter.df$country_name

## compute ed_sig 
vdem12_dat$ed_sig <- ifelse(vdem12_dat$country_name %in% dem_vec, 1,0)

## subset to Universe of Democracies (N = 105)
vdem12_dat_dems <- vdem12_dat[vdem12_dat$ed_sig == 1, ]

## create a variable for peak score 
vdem12_dat_dems <- vdem12_dat_dems %>%
  group_by(country_name) %>%
  mutate(year_of_peak_score = year[which.max(ld)]) %>%
  ungroup()

## compute ld_sig
vdem12_dat_dems$ld_sig <- ifelse(vdem12_dat_dems$v2x_libdem_codehigh <=
                                   vdem12_dat_dems$max_ld_lb, 1, 0)

## Among the backsliders eliminate the ones that shows significance due to advancement prior to the peak score 
# exclude the countries from "exclude list": these are countries that achieve their peak score
# after the backsliding episode, we don't want to eliminate those. 
`%!in%` = Negate(`%in%`)
exclude_list <- c("Dominican Republic", "Ecuador", "Lesotho", "Malawi", "Romania",
                  "Burkina Faso", "Moldova", "Nepal", "Niger",  "Senegal",
                   "Sierra Leone", "Solomon Islands", "South Korea","Vanuatu")

vdem12_dat_dems <- vdem12_dat_dems %>%
  group_by(country_name) %>% 
  mutate(ld_sig2 = ifelse(ld_sig == 1 & year < year_of_peak_score & country_name %!in% exclude_list, 0, ld_sig)) %>% 
  ungroup()

vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Dominican Republic" & vdem12_dat_dems$year < 2001] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Ecuador" & vdem12_dat_dems$year < 1980] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Lesotho" & vdem12_dat_dems$year < 2004] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Malawi" & vdem12_dat_dems$year < 1998] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Romania" & vdem12_dat_dems$year < 2014] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Burkina Faso" & vdem12_dat_dems$year < 2013] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Moldova" & vdem12_dat_dems$year < 2014] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Nepal" & vdem12_dat_dems$year < 2009] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Niger" & vdem12_dat_dems$year < 2002] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Senegal" & vdem12_dat_dems$year < 1994] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Sierra Leone" & vdem12_dat_dems$year < 2012] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Solomon Islands" & vdem12_dat_dems$year < 2002] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "South Korea" & vdem12_dat_dems$year < 1998] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Vanuatu" & vdem12_dat_dems$year < 1984] <- 0
vdem12_dat_dems$ld_sig2[vdem12_dat_dems$country_name == "Fiji" & vdem12_dat_dems$year < 2004] <- 0

#######################################
## Visualize Initial List of Backsliders 
# Appendix 7 
#######################################

backsliders1 <- vdem12_dat_dems %>% filter(ld_sig2 == 1)
backsliders1_unique <- unique(backsliders1$country_name)

plotsbacksliders1 <- vdem12_dat_dems[vdem12_dat_dems$country_name %in% backsliders1_unique,]
plotsbacksliders1 <- plotsbacksliders1[order(plotsbacksliders1$country_name), ]

plots <- list()

for (i in unique(plotsbacksliders1$country_name)) {
  assign(paste0("dat"), plotsbacksliders1[plotsbacksliders1$country_name == i, ])
  
  assign(paste0("Highlight"), dat[dat$ld_sig2 >= 1, ] )
  
  if(nrow(Highlight) > 0){
    plots[[i]] <- ggplot(dat, aes(x = year , y = v2x_libdem)) +
      geom_point(aes(x = year , y = v2x_libdem),size = 0.2) +
      geom_point(data=Highlight , aes(x = year,y = v2x_libdem), colour = 'red', size=0.2) +
      geom_errorbar(aes(ymax = v2x_libdem_codehigh, ymin = v2x_libdem_codelow, width = 0.3)) +
      geom_errorbar(data=Highlight, aes(ymax = v2x_libdem_codehigh, ymin = v2x_libdem_codelow,
                                        colour = 'red', width = 0.3)) +
      scale_y_continuous(limits = c(0.00,1.00), breaks = seq (0.00, 1, by = 0.25)) +
      scale_x_continuous(breaks = seq(1974, 2021, by = 5)) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      ggtitle(i) +   theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold")) +
      labs(y = "V-Dem LD Score", x = "Year") +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=9))
    
  }
  else{
    plots[[i]] <- ggplot(dat, aes(x = year , y = v2x_libdem)) +
      geom_point(aes(x = year , y = v2x_libdem),size = 0.2) +
      geom_errorbar(aes(ymax = v2x_libdem_codehigh, ymin = v2x_libdem_codelow, width = 0.3)) +
      scale_y_continuous(limits = c(0.00,1.00), breaks = seq (0.00, 1, by = 0.25)) +
      scale_x_continuous(breaks = seq(1974, 2019, by = 5)) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      ggtitle(i) +   theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold")) +
      labs(y = "V-Dem LD Score", x = "year") +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=9))
  }
}

export <- marrangeGrob(plots, nrow = 3, ncol = 3,
                       top = quote(paste("Initial List of Backsliders \n Movements of Liberal Democracy Index (LDI), Democracies (N=52)")) ,
                       bottom = quote(paste(npages)))


#ggsave("backsliders_initial.pdf", export, width=11, height=8.5)

# ** Initial List of Backsliders before triangulation, N = 52 **

## Create a new variable for the final list of backsliders after triangulation 
# Note. See details of triangulation in Appendix Tables A3 & A4 
vdem12_dat_dems$backslider <- ifelse(vdem12_dat_dems$country_name %in% 
                                    c('Benin', 'Bolivia', 'Brazil', 
                                      'Croatia', 'Dominican Republic', 'Ecuador', 
                                      'El Salvador', 'Ghana', 'Greece',
                                      'Hungary', 'India', 'Mali', 
                                      'Nicaragua',  'Philippines', 'Poland',
                                      'Serbia', 'Slovenia', 'Turkey', 
                                      'United States of America', 'Venezuela', 'Zambia'),'1', '0')
  
  
## ** Final list of backsliders after triangulation, N = 21 **

#################################
## Save the analysis sample 
#################################

#write.csv(vdem12_dat_dems, 'dems_grand.csv')

