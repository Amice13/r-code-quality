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
trustparl.est = read.csv("parl_mood2.csv")
trustleg.est = read.csv("leg_mood2.csv")
trustpolice.est = read.csv("police_mood2.csv")

trustparl.est$est <- trustparl.est$parl
trustparl.est$est_l95 <- trustparl.est$parl_l95
trustparl.est$est_u95 <- trustparl.est$parl_u95

trustleg.est$est <- trustleg.est$leg
trustleg.est$est_l95 <- trustleg.est$leg_l95
trustleg.est$est_u95 <- trustleg.est$leg_u95

trustpolice.est$est <- trustpolice.est$police
trustpolice.est$est_l95 <- trustpolice.est$police_l95
trustpolice.est$est_u95 <- trustpolice.est$police_u95

trustparl.est$type <- "Parliament"
trustleg.est$type <- "Legal system"
trustpolice.est$type <- "Police"

trustparl.est2 = subset(trustparl.est, select = c("Country", "regpol6", "Year", "type", "est", "est_l95", "est_u95") )
trustleg.est2 = subset(trustleg.est, select = c("Country", "regpol6", "Year", "type", "est", "est_l95", "est_u95") )
trustpolice.est2 = subset(trustpolice.est, select = c("Country", "regpol6", "Year", "type", "est", "est_l95", "est_u95") )

parl.leg.police <- rbind(trustparl.est2, trustleg.est2, trustpolice.est2)

parl.leg.police$type <- factor(parl.leg.police$type,levels = 
                                 c("Parliament", "Legal system", "Police"))
                               
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

cnts = levels(parl.leg.police$Country)
n.cnts = length(cnts)

decadelabels <- c("'70", "'80", "'90", "'00", "'10", "'20")
decadebreaks <- c(1970, 1980, 1990, 2000, 2010, 2020)

parl.leg.police$Country <- as.character(parl.leg.police$Country)
parl.leg.police$Country[parl.leg.police$Country=="Dominican Republic"] <- "Dominican Rep."
parl.leg.police$Country[parl.leg.police$Country=="Trinidad and Tobago"] <- "Trinidad & Tobago"
parl.leg.police$Country[parl.leg.police$Country=="Sao Tome and Principe"] <- "Sao Tome & P."
parl.leg.police$Country[parl.leg.police$Country=="Bosnia and Herzegovina"] <- "Bosnia & Herz."
parl.leg.police$Country[parl.leg.police$Country=="North Macedonia"] <- "N. Macedonia"

wena_plot_main <- parl.leg.police %>% filter(regpol6 == "W-Europe & N-America") %>% # the filter() filters the region
  ggplot(aes(x = Year, y = est, color = type, linetype = type)) + # this and next 2 are the bones
  geom_line() +
  scale_color_manual(values = c(col_parl, col_leg, col_police),
                     labels = c("Parliament", "Legal System", "Police")) +
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = est_l95, ymax = est_u95), alpha = 0.1, linetype = "blank", size = 0.2) +  
  facet_wrap(~Country) + # this makes a separate plot for each country
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 7)) +
  guides(linetype = "none") +
  theme(legend.title = element_blank()) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE))
wena_plot_main
ggsave("main_wena.png", wena_plot_main, dpi = 600)

eeca_plot_main <- parl.leg.police %>% filter(regpol6 == "E-Europe & C-Asia") %>% # the filter() filters the region
  ggplot(aes(x = Year, y = est, color = type, linetype = type)) + # this and next 2 are the bones
  geom_line() +
  scale_color_manual(values = c(col_parl, col_leg, col_police),
                     labels = c("Parliament", "Legal System", "Police")) +
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = est_l95, ymax = est_u95), alpha = 0.1, linetype = "blank", size = 0.2) +  
  facet_wrap(~Country) + # this makes a separate plot for each country
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 7)) +
  guides(linetype = "none") +
  theme(legend.title = element_blank()) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE))
eeca_plot_main
ggsave("main_eeca.png", eeca_plot_main, dpi = 600)

lac_plot_main <- parl.leg.police %>% filter(regpol6 == "L-America & Caribbean") %>% # the filter() filters the region
  ggplot(aes(x = Year, y = est, color = type, linetype = type)) + # this and next 2 are the bones
  geom_line() +
  scale_color_manual(values = c(col_parl, col_leg, col_police),
                     labels = c("Parliament", "Legal System", "Police")) +
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = est_l95, ymax = est_u95), alpha = 0.1, linetype = "blank", size = 0.2) +  
  facet_wrap(~Country) + # this makes a separate plot for each country
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 7)) +
  guides(linetype = "none") +
  theme(legend.title = element_blank()) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE))
lac_plot_main
ggsave("main_lac.png", lac_plot_main, dpi = 600)

mena_plot_main <- parl.leg.police %>% filter(regpol6 == "M-East & N-Africa") %>% # the filter() filters the region
  ggplot(aes(x = Year, y = est, color = type, linetype = type)) + # this and next 2 are the bones
  geom_line() +
  scale_color_manual(values = c(col_parl, col_leg, col_police),
                     labels = c("Parliament", "Legal System", "Police")) +
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = est_l95, ymax = est_u95), alpha = 0.1, linetype = "blank", size = 0.2) +  
  facet_wrap(~Country) + # this makes a separate plot for each country
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 7)) +
  guides(linetype = "none") +
  theme(legend.title = element_blank()) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE))
mena_plot_main
ggsave("main_mena.png", mena_plot_main, dpi = 600)

ssa_plot_main <- parl.leg.police %>% filter(regpol6 == "Sub-Saharan Africa") %>% # the filter() filters the region
  ggplot(aes(x = Year, y = est, color = type, linetype = type)) + # this and next 2 are the bones
  geom_line() +
  scale_color_manual(values = c(col_parl, col_leg, col_police),
                     labels = c("Parliament", "Legal System", "Police")) +
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = est_l95, ymax = est_u95), alpha = 0.1, linetype = "blank", size = 0.2) +  
  facet_wrap(~Country) + # this makes a separate plot for each country
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 7)) +
  guides(linetype = "none") +
  theme(legend.title = element_blank()) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE))
ssa_plot_main
ggsave("main_ssa.png", ssa_plot_main, dpi = 600)

aspac_plot_main <- parl.leg.police %>% filter(regpol6 == "Asia & Pacific") %>% # the filter() filters the region
  ggplot(aes(x = Year, y = est, color = type, linetype = type)) + # this and next 2 are the bones
  geom_line() +
  scale_color_manual(values = c(col_parl, col_leg, col_police),
                     labels = c("Parliament", "Legal System", "Police")) +
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = est_l95, ymax = est_u95), alpha = 0.1, linetype = "blank", size = 0.2) +  
  facet_wrap(~Country) + # this makes a separate plot for each country
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 7)) +
  guides(linetype = "none") +
  theme(legend.title = element_blank()) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42")),
                              byrow = TRUE))
aspac_plot_main
ggsave("main_aspac.png", aspac_plot_main, dpi = 600)

#########

