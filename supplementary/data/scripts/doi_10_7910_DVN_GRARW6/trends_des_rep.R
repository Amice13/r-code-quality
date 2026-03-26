###### Descriptive trends for trust in six institutions across countries, 1958-2019

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

# Read trust data
trust.des = read_dta("Trust_trends_rep.dta")

## Plot trends
decadebreaks <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
decadelabels <- c("'60", "'70", "'80", "'90", "'00", "'10", "'20")
trustbreaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
trustlabels <- c("0%", "20%", "40%", "60%", "80%", "100%")

trust.des$Country <- as.character(trust.des$country)
trust.des$Country[trust.des$Country=="Dominican Republic"] <- "Dominican Rep."
trust.des$Country[trust.des$Country=="Trinidad and Tobago"] <- "Trinidad & Tobago"
trust.des$Country[trust.des$Country=="Sao Tome and Principe"] <- "Sao Tome & P."
trust.des$Country[trust.des$Country=="Bosnia and Herzegovina"] <- "Bosnia & Herz."
trust.des$Country[trust.des$Country=="North Macedonia"] <- "N. Macedonia"

trust.des$study_broad[trust.des$study_broad=="Other"] <- "Other Int."

trust.des$study_broad_f <- factor(trust.des$study_broad,levels = 
                                    c("Barometers", "WVS/EVS", "ESS", "CSES/NES",
                                      "ISSP", "AmericasB", "Other Int.", "Other National"))
# Parliament
desplot_parl_wena <- trust.des %>% filter(regpol6 == 1, !is.na(trust_parl), trust_parl_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_parl, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_wena
ggsave("des_parl_wena.png", desplot_parl_wena)

desplot_parl_eeca <- trust.des %>% filter(regpol6 == 2, !is.na(trust_parl), trust_parl_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_parl, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_eeca
ggsave("des_parl_eeca.png", desplot_parl_eeca)

desplot_parl_lac <- trust.des %>% filter(regpol6 == 3, !is.na(trust_parl), trust_parl_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_parl, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_lac
ggsave("des_parl_lac.png", desplot_parl_lac)

desplot_parl_mena <- trust.des %>% filter(regpol6 == 4, !is.na(trust_parl), trust_parl_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_parl, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_mena
ggsave("des_parl_mena.png", desplot_parl_mena)

desplot_parl_ssa <- trust.des %>% filter(regpol6 == 5, !is.na(trust_parl), trust_parl_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_parl, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_ssa
ggsave("des_parl_ssa.png", desplot_parl_ssa)

desplot_parl_aspac <- trust.des %>% filter(regpol6 == 6, !is.na(trust_parl), trust_parl_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_parl, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_aspac
ggsave("des_parl_aspac.png", desplot_parl_aspac)

# Government
desplot_gov_wena <- trust.des %>% filter(regpol6 == 1, !is.na(trust_gov), trust_gov_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_gov, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_gov_wena
ggsave("des_gov_wena.png", desplot_gov_wena)

desplot_gov_eeca <- trust.des %>% filter(regpol6 == 2, !is.na(trust_gov), trust_gov_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_gov, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_gov_eeca
ggsave("des_gov_eeca.png", desplot_gov_eeca)

desplot_gov_lac <- trust.des %>% filter(regpol6 == 3, !is.na(trust_gov), trust_gov_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_gov, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_gov_lac
ggsave("des_gov_lac.png", desplot_gov_lac)

desplot_gov_mena <- trust.des %>% filter(regpol6 == 4, !is.na(trust_gov), trust_gov_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_gov, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_gov_mena
ggsave("des_gov_mena.png", desplot_gov_mena)

desplot_gov_ssa <- trust.des %>% filter(regpol6 == 5, !is.na(trust_gov), trust_gov_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_gov, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_gov_ssa
ggsave("des_gov_ssa.png", desplot_gov_ssa)

desplot_gov_aspac <- trust.des %>% filter(regpol6 == 6, !is.na(trust_gov), trust_gov_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_gov, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_gov_aspac
ggsave("des_gov_aspac.png", desplot_gov_aspac)

# Political parties
desplot_polpar_wena <- trust.des %>% filter(regpol6 == 1, !is.na(trust_polpar), trust_polpar_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_polpar, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_polpar_wena
ggsave("des_polpar_wena.png", desplot_polpar_wena)

desplot_polpar_eeca <- trust.des %>% filter(regpol6 == 2, !is.na(trust_polpar), trust_polpar_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_polpar, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_polpar_eeca
ggsave("des_polpar_eeca.png", desplot_polpar_eeca)

desplot_polpar_lac <- trust.des %>% filter(regpol6 == 3, !is.na(trust_polpar), trust_polpar_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_polpar, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_polpar_lac
ggsave("des_polpar_lac.png", desplot_polpar_lac)

desplot_polpar_mena <- trust.des %>% filter(regpol6 == 4, !is.na(trust_polpar), trust_polpar_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_polpar, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_polpar_mena
ggsave("des_polpar_mena.png", desplot_polpar_mena)

desplot_polpar_ssa <- trust.des %>% filter(regpol6 == 5, !is.na(trust_polpar), trust_polpar_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_polpar, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_polpar_ssa
ggsave("des_polpar_ssa.png", desplot_polpar_ssa)

desplot_polpar_aspac <- trust.des %>% filter(regpol6 == 6, !is.na(trust_polpar), trust_polpar_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_polpar, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_polpar_aspac
ggsave("des_polpar_aspac.png", desplot_polpar_aspac)

# Civil service
desplot_civil_wena <- trust.des %>% filter(regpol6 == 1, !is.na(trust_civil), trust_civil_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_civil, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_civil_wena
ggsave("des_civil_wena.png", desplot_civil_wena)

desplot_civil_eeca <- trust.des %>% filter(regpol6 == 2, !is.na(trust_civil), trust_civil_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_civil, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_civil_eeca
ggsave("des_civil_eeca.png", desplot_civil_eeca)

desplot_civil_lac <- trust.des %>% filter(regpol6 == 3, !is.na(trust_civil), trust_civil_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_civil, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_civil_lac
ggsave("des_civil_lac.png", desplot_civil_lac)

desplot_civil_mena <- trust.des %>% filter(regpol6 == 4, !is.na(trust_civil), trust_civil_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_civil, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_civil_mena
ggsave("des_civil_mena.png", desplot_civil_mena)

desplot_civil_ssa <- trust.des %>% filter(regpol6 == 5, !is.na(trust_civil), trust_civil_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_civil, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_civil_ssa
ggsave("des_civil_ssa.png", desplot_civil_ssa)

desplot_civil_aspac <- trust.des %>% filter(regpol6 == 6, !is.na(trust_civil), trust_civil_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_civil, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_civil_aspac
ggsave("des_civil_aspac.png", desplot_civil_aspac)

# Legal system
desplot_leg_wena <- trust.des %>% filter(regpol6 == 1, !is.na(trust_leg), trust_leg_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_leg, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_leg_wena
ggsave("des_leg_wena.png", desplot_leg_wena)

desplot_leg_eeca <- trust.des %>% filter(regpol6 == 2, !is.na(trust_leg), trust_leg_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_leg, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_leg_eeca
ggsave("des_leg_eeca.png", desplot_leg_eeca)

desplot_leg_lac <- trust.des %>% filter(regpol6 == 3, !is.na(trust_leg), trust_leg_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_leg, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_leg_lac
ggsave("des_leg_lac.png", desplot_leg_lac)

desplot_leg_mena <- trust.des %>% filter(regpol6 == 4, !is.na(trust_leg), trust_leg_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_leg, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_leg_mena
ggsave("des_leg_mena.png", desplot_leg_mena)

desplot_leg_ssa <- trust.des %>% filter(regpol6 == 5, !is.na(trust_leg), trust_leg_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_leg, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_leg_ssa
ggsave("des_leg_ssa.png", desplot_leg_ssa)

desplot_leg_aspac <- trust.des %>% filter(regpol6 == 6, !is.na(trust_leg), trust_leg_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_leg, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_leg_aspac
ggsave("des_leg_aspac.png", desplot_leg_aspac)

# Police
desplot_police_wena <- trust.des %>% filter(regpol6 == 1, !is.na(trust_police), trust_police_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_police, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_police_wena
ggsave("des_police_wena.png", desplot_police_wena)

desplot_police_eeca <- trust.des %>% filter(regpol6 == 2, !is.na(trust_police), trust_police_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_police, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_police_eeca
ggsave("des_police_eeca.png", desplot_police_eeca)

desplot_police_lac <- trust.des %>% filter(regpol6 == 3, !is.na(trust_police), trust_police_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_police, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_police_lac
ggsave("des_police_lac.png", desplot_police_lac)

desplot_police_mena <- trust.des %>% filter(regpol6 == 4, !is.na(trust_police), trust_police_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_police, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_police_mena
ggsave("des_police_mena.png", desplot_police_mena)

desplot_police_ssa <- trust.des %>% filter(regpol6 == 5, !is.na(trust_police), trust_police_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_police, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_police_ssa
ggsave("des_police_ssa.png", desplot_police_ssa)

desplot_police_aspac <- trust.des %>% filter(regpol6 == 6, !is.na(trust_police), trust_police_count_study>1) %>% #
  ggplot(aes(x = year, y = trust_police, color = study_broad_f, linetype = study_broad_f)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = trustbreaks,
                     labels = trustlabels) +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_police_aspac
ggsave("des_police_aspac.png", desplot_police_aspac)

## Compare dichotomized variables with full scales from the ESS, ISSP and AmericasBarometer
trust.des.scales <- trust.des %>% filter(study == "ESS" | study == "ISSP" | study == "LAPOP")
trust.des.scales$scale <- 1
trust.des.scales$trust_parl <- trust.des.scales$trust_parl_scale

trust.des.scales2 <- trust.des %>% filter(study == "ESS" | study == "ISSP" | study == "LAPOP")
trust.des.scales2$scale <- 0

trust.des.scales_comb <- rbind(trust.des.scales, trust.des.scales2)
trust.des.scales_comb$scale <- factor(trust.des.scales_comb$scale,
                                      labels = c("Dichotomized", "Full scale"))

desplot_parl_wena_scales_ESS <- trust.des.scales_comb %>%
  filter(regpol6 == 1, !is.na(trust_parl), trust_parl_count_study>1, study == "ESS") %>% #
  ggplot(aes(x = year, y = trust_parl, color = scale, linetype = scale)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_wena_scales_ESS
ggsave("des_parl_wena_scales_ESS.png", desplot_parl_wena_scales_ESS)

desplot_parl_wena_scales_ISSP <- trust.des.scales_comb %>%
  filter(regpol6 == 1, !is.na(trust_parl), trust_parl_count_study>1, study == "ISSP") %>% #
  ggplot(aes(x = year, y = trust_parl, color = scale, linetype = scale)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_wena_scales_ISSP
ggsave("des_parl_wena_scales_ISSP.png", desplot_parl_wena_scales_ISSP)

desplot_parl_lac_scales_LAPOP <- trust.des.scales_comb %>%
  filter(regpol6 == 3, !is.na(trust_parl), trust_parl_count_study>1, study == "LAPOP") %>% #
  ggplot(aes(x = year, y = trust_parl, color = scale, linetype = scale)) +
  geom_line(size=0.5) +
  theme_minimal() +
  facet_wrap(~Country) +
  scale_colour_viridis_d(end=0.9) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7)) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  labs(title = "", y ="", color = "", linetype ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(byrow = TRUE))
desplot_parl_lac_scales_LAPOP
ggsave("des_parl_lac_scales_LAPOP.png", desplot_parl_lac_scales_LAPOP)

############