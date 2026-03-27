###### Graphing country trends in SWD and trust in parliament together, from Bayesian estimates

# libraries
library(arm)
library(dplyr)
library(tidyr)
library(loo)
library(ggplot2)
library(bayesplot)
library(cmdstanr)
library(rio)
library(tidyverse)

# combine estimates data
parl.reg.est = read.csv("parl_reg_est.csv")
gov.reg.est = read.csv("gov_reg_est.csv")
polpar.reg.est = read.csv("polpar_reg_est.csv")
civil.reg.est = read.csv("civil_reg_est.csv")
leg.reg.est = read.csv("leg_reg_est.csv")
police.reg.est = read.csv("police_reg_est.csv")

parl.reg.est$type <- "Parliament"
gov.reg.est$type <- "Government"
polpar.reg.est$type <- "Political parties"
civil.reg.est$type <- "Civil service"
leg.reg.est$type <- "Legal system"
police.reg.est$type <- "Police"

parl.reg.est$est <- parl.reg.est$parl
parl.reg.est$est_l95 <- parl.reg.est$parl_l95
parl.reg.est$est_u95 <- parl.reg.est$parl_u95

gov.reg.est$est <- gov.reg.est$gov
gov.reg.est$est_l95 <- gov.reg.est$gov_l95
gov.reg.est$est_u95 <- gov.reg.est$gov_u95

polpar.reg.est$est <- polpar.reg.est$polpar
polpar.reg.est$est_l95 <- polpar.reg.est$polpar_l95
polpar.reg.est$est_u95 <- polpar.reg.est$polpar_u95

civil.reg.est$est <- civil.reg.est$civil
civil.reg.est$est_l95 <- civil.reg.est$civil_l95
civil.reg.est$est_u95 <- civil.reg.est$civil_u95

leg.reg.est$est <- leg.reg.est$leg
leg.reg.est$est_l95 <- leg.reg.est$leg_l95
leg.reg.est$est_u95 <- leg.reg.est$leg_u95

police.reg.est$est <- police.reg.est$police
police.reg.est$est_l95 <- police.reg.est$police_l95
police.reg.est$est_u95 <- police.reg.est$police_u95

parl.reg.est2 = subset(parl.reg.est, select = c("Region", "Year", "type", "est", "est_l95", "est_u95"))
gov.reg.est2 = subset(gov.reg.est, select = c("Region", "Year", "type", "est", "est_l95", "est_u95"))
polpar.reg.est2 = subset(polpar.reg.est, select = c("Region", "Year", "type", "est", "est_l95", "est_u95"))
civil.reg.est2 = subset(civil.reg.est, select = c("Region", "Year", "type", "est", "est_l95", "est_u95"))
leg.reg.est2 = subset(leg.reg.est, select = c("Region", "Year", "type", "est", "est_l95", "est_u95"))
police.reg.est2 = subset(police.reg.est, select = c("Region", "Year", "type", "est", "est_l95", "est_u95"))

allpol.reg.est <- rbind(parl.reg.est2, gov.reg.est2, polpar.reg.est2)
allord.reg.est <- rbind(civil.reg.est2, leg.reg.est2, police.reg.est2)

allpol.reg.est$type <- factor(allpol.reg.est$type,levels = 
                                 c("Parliament", "Government", "Political parties"))

allord.reg.est$type <- factor(allord.reg.est$type,levels = 
                                c("Civil service", "Legal system", "Police"))

allpol.reg.est$Region[allpol.reg.est$Region=="W-Europe & N-America"] <- "Western Europe & North America"
allpol.reg.est$Region[allpol.reg.est$Region=="E-Europe & C-Asia"] <- "Eastern Europe & Central Asia"
allpol.reg.est$Region[allpol.reg.est$Region=="L-America & Caribbean"] <- "Latin America & the Caribbean"
allpol.reg.est$Region[allpol.reg.est$Region=="M-East & N-Africa"] <- "Middle East & North Africa"
allpol.reg.est$Region[allpol.reg.est$Region=="Asia & Pacific"] <- "Asia & the Pacific"

allord.reg.est$Region[allord.reg.est$Region=="W-Europe & N-America"] <- "Western Europe & North America"
allord.reg.est$Region[allord.reg.est$Region=="E-Europe & C-Asia"] <- "Eastern Europe & Central Asia"
allord.reg.est$Region[allord.reg.est$Region=="L-America & Caribbean"] <- "Latin America & the Caribbean"
allord.reg.est$Region[allord.reg.est$Region=="M-East & N-Africa"] <- "Middle East & North Africa"
allord.reg.est$Region[allord.reg.est$Region=="Asia & Pacific"] <- "Asia & the Pacific"
                      
allpol.reg.est$Region_f <- factor(allpol.reg.est$Region,levels = 
                                     c("Western Europe & North America", "Eastern Europe & Central Asia",
                                       "Latin America & the Caribbean",
                                       "Middle East & North Africa", "Sub-Saharan Africa", "Asia & the Pacific"))

allord.reg.est$Region_f <- factor(allord.reg.est$Region,levels = 
                                    c("Western Europe & North America", "Eastern Europe & Central Asia",
                                      "Latin America & the Caribbean",
                                      "Middle East & North Africa", "Sub-Saharan Africa", "Asia & the Pacific"))

# plot trends
col_parl = rgb(213, 94, 0, 255, maxColorValue = 255)
col_parl_ci = rgb(213, 94, 0, 100, maxColorValue = 255)
col_gov = rgb(220, 135, 0, 255, maxColorValue = 255)
col_gov_ci = rgb(220, 135, 0, 100, maxColorValue = 255)
col_polpar = rgb(230, 159, 0, 255, maxColorValue = 255)
col_polpar_ci = rgb(230, 159, 0, 100, maxColorValue = 255)

col_civil = rgb(0, 158, 115, 255, maxColorValue = 255)
col_civil_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_leg = rgb(10, 128, 158, 255, maxColorValue = 255)
col_leg_ci = rgb(10, 128, 158, 100, maxColorValue = 255)
col_police = rgb(19,93,216, 255, maxColorValue = 255)
col_police_ci = rgb(19,93,216, 100, maxColorValue = 255)

decadelabels <- c("'80", "'90", "'00", "'10", "'20")
decadebreaks <- c(1980, 1990, 2000, 2010, 2020)

reg_plot_pol <- allpol.reg.est %>%
  ggplot(aes(x = Year, y = est, color = type, linetype = type), size=1.4) + # this and next 2 are the bones
  geom_line() +
  scale_color_manual(values = c(col_parl, col_gov, col_polpar),
                     labels = c("Parliament", "Government", "Political parties")) +
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = est_l95, ymax = est_u95), alpha = 0.1, linetype = "blank", size = 0.2) +  
  facet_wrap(~Region_f) + # this makes a separate plot for each country
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8.5)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE))
reg_plot_pol
ggsave("reg_comb_pol.png", reg_plot_pol, dpi = 600)

reg_plot_ord <- allord.reg.est %>%
  ggplot(aes(x = Year, y = est, color = type, linetype = type), size=1.4) + # this and next 2 are the bones
  geom_line() +
  scale_color_manual(values = c(col_civil, col_leg, col_police),
                     labels = c("Civil service", "Legal system", "Police")) +
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = est_l95, ymax = est_u95), alpha = 0.1, linetype = "blank", size = 0.2) +  
  facet_wrap(~Region_f) + # this makes a separate plot for each country
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8.5)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE))
reg_plot_ord
ggsave("reg_comb_ord.png", reg_plot_ord, dpi = 600)

#########