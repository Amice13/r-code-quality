
##### 0 ToC V-Party Validation ----

# Figures and Regression Analysis for Validation Paper

# 1 Load main data
# 2 Figures for main text
# 3 Regression analysis





##### 1 Prerequisites ----

    # clean env
rm(list = ls())

    # load packages
library(tidyverse)
library(fuzzyjoin)
library(grid)
library(gridExtra)
library(ggridges)
library(ggpubr)
library(lme4)
library(dotwhisker)
library(broom.mixed)
library(ggeffects)
library(export)

    # load v-party
load("./data/vparty_for_partyorga.Rdata")





##### 2 Figures ----



## Figure 1 Global trends of party-organizational features excluding autocracies ----


scat1 <-
    df %>%
    filter(v2x_regime > 0) %>% 
    ggplot(aes(year, orgext)) + 
    coord_cartesian(ylim = c(-2, 4)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_point(aes(colour = factor(v2x_regime_lab)), alpha=0.05) + 
    geom_smooth(aes(colour = factor(v2x_regime_lab)), size=2) + 
    scale_y_continuous(limits = c(-200, 200), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme(legend.position = "bottom") + theme_minimal() +
    labs(x = "", y = "Organizational extensiveness", colour = "Regimes of the World (RoW)") +
    scale_color_brewer(palette="Set1")

scat2 <-
    df %>%
    filter(v2x_regime > 0) %>% 
    ggplot(aes(year, powercon)) + 
    coord_cartesian(ylim = c(-2, 4)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_point(aes(colour = factor(v2x_regime_lab)), alpha=0.05) + 
    geom_smooth(aes(colour = factor(v2x_regime_lab)), size=2) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme(legend.position = "none") + theme_minimal() +
    labs(x = "", y = "Intra-party power concentration") +
    scale_color_brewer(palette="Set1")

scat3 <-
    df %>%
    filter(v2x_regime > 0) %>% 
    ggplot(aes(year, cohesion)) + 
    coord_cartesian(ylim = c(-2, 4)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_point(aes(colour = factor(v2x_regime_lab)), alpha=0.05) + 
    geom_smooth(aes(colour = factor(v2x_regime_lab)), size=2) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme(legend.position = "none") + theme_minimal() +
    labs(x = "", y = "Elite cohesion") +
    scale_color_brewer(palette="Set1")

    # Write outfile for main text
grid.newpage()
combo_out <- ggarrange(scat1, scat2, scat3, ncol = 3, nrow = 1, common.legend = TRUE, legend="bottom", align = "h")
#graph2svg(x = combo_out, file = "./analyze_partyorga/fig1_global_trends.svg", width = 8, height = 6) # for journals
graph2png(x = combo_out, file = "./analyze_partyorga/fig1_global_trends.png", width = 8, height = 5) # for working paper



## Figure 2 Regional variation and trends ----


    ###### Region 1 ----

geo1_dist1 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 1) %>% 
    ggplot(aes(x = orgext, y = 0)) +
    geom_density_ridges(fill="#377EB8", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#377EB8") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))

geo1_dist2 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 1) %>% 
    ggplot(aes(x = powercon, y = 0)) +
    geom_density_ridges(fill="#4DAF4A", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#4DAF4A") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

geo1_dist3 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 1) %>% 
    ggplot(aes(x = cohesion, y = 0)) +
    geom_density_ridges(fill="#E41A1C", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#E41A1C") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +   
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

    # Combine all three density plots
grid.newpage()
combo_geo1_dist <- ggarrange(geo1_dist1, geo1_dist2, geo1_dist3, ncol = 3, nrow = 1, align = "h") 

    # Plot trends over time => exemption: include closed autocracies pre-1990 to show the notable break
geo1_line <- 
    df %>% 
    select(year, orgext, powercon, cohesion, v2x_regime, e_regionpol_6C) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-2, 6)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_smooth(data = df %>% filter(e_regionpol_6C == 1), aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", fill = "#808080", alpha = 0.1, linetype ="dotted") + # incl. closed autocracies
    geom_smooth(data = df %>% filter(e_regionpol_6C == 1), aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", fill = "#808080", alpha = 0.1, linetype ="dotted") + # incl. closed autocracies
    geom_smooth(data = df %>% filter(e_regionpol_6C == 1), aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", fill = "#808080", alpha = 0.1, linetype ="dotted") + # incl. closed autocracies
    geom_smooth(data = df %>% filter(e_regionpol_6C == 1 & v2x_regime >= 1), aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", fill = "#377EB8") +
    geom_smooth(data = df %>% filter(e_regionpol_6C == 1 & v2x_regime >= 1), aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", fill = "#4DAF4A") +
    geom_smooth(data = df %>% filter(e_regionpol_6C == 1 & v2x_regime >= 1), aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", fill = "#E41A1C") +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme_minimal() + theme(axis.title = element_blank())

    # Combine density with line plot
grid.newpage()
combo_geo1 <-
    ggarrange(combo_geo1_dist, geo1_line, ncol = 2, nrow = 1, align = "v") %>% 
    annotate_figure(left = text_grob("Eastern Europe \n and Central Asia", face = "bold", size = 10, rot = 90))
#print(combo_geo1)


    ###### Region 2 ----

geo2_dist1 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 2) %>% 
    ggplot(aes(x = orgext, y = 0)) +
    geom_density_ridges(fill="#377EB8", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#377EB8") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))

geo2_dist2 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 2) %>% 
    ggplot(aes(x = powercon, y = 0)) +
    geom_density_ridges(fill="#4DAF4A", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#4DAF4A") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

geo2_dist3 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 2) %>% 
    ggplot(aes(x = cohesion, y = 0)) +
    geom_density_ridges(fill="#E41A1C", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#E41A1C") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +   
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

    # Combine all three density plots
grid.newpage()
combo_geo2_dist <- ggarrange(geo2_dist1, geo2_dist2, geo2_dist3, ncol = 3, nrow = 1, align = "h")

    # Plot trends over time
geo2_line <- 
    df %>% 
    filter(v2x_regime > 0 & e_regionpol_6C == 2) %>%
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-2, 2)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_smooth(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", fill = "#377EB8") +
    geom_smooth(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", fill = "#4DAF4A") +
    geom_smooth(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", fill = "#E41A1C") +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme_minimal() + theme(axis.title = element_blank())

    # Combine density with line plot
grid.newpage()
combo_geo2 <-
    ggarrange(combo_geo2_dist, geo2_line, ncol = 2, nrow = 1, align = "v") %>% 
    annotate_figure(left = text_grob("Latin America \n and the Caribbean", face = "bold", size = 10, rot = 90))
#print(combo_geo2)


    ###### Region 3 ----

geo3_dist1 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 3) %>% 
    ggplot(aes(x = orgext, y = 0)) +
    geom_density_ridges(fill="#377EB8", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#377EB8") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))

geo3_dist2 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 3) %>% 
    ggplot(aes(x = powercon, y = 0)) +
    geom_density_ridges(fill="#4DAF4A", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#4DAF4A") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

geo3_dist3 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 3) %>% 
    ggplot(aes(x = cohesion, y = 0)) +
    geom_density_ridges(fill="#E41A1C", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#E41A1C") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +   
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

    # Combine all three density plots
grid.newpage()
combo_geo3_dist <- ggarrange(geo3_dist1, geo3_dist2, geo3_dist3, ncol = 3, nrow = 1, align = "h") 

    # Plot trends over time
geo3_line <- 
    df %>% 
    filter(v2x_regime > 0 & e_regionpol_6C == 3) %>%
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-4, 2)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_smooth(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", fill = "#377EB8") +
    geom_smooth(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", fill = "#4DAF4A") +
    geom_smooth(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", fill = "#E41A1C") +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme_minimal() + theme(axis.title = element_blank())

    # Combine density with line plot
grid.newpage()
combo_geo3 <- 
    ggarrange(combo_geo3_dist, geo3_line, ncol = 2, nrow = 1, align = "v") %>% 
    annotate_figure(left = text_grob("The Middle East \n and Northern Africa", face = "bold", size = 10, rot = 90))
#print(combo_geo3)


###### Region 4 ----

geo4_dist1 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 4) %>% 
    ggplot(aes(x = orgext, y = 0)) +
    geom_density_ridges(fill="#377EB8", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#377EB8") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))

geo4_dist2 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 4) %>% 
    ggplot(aes(x = powercon, y = 0)) +
    geom_density_ridges(fill="#4DAF4A", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#4DAF4A") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

geo4_dist3 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 4) %>% 
    ggplot(aes(x = cohesion, y = 0)) +
    geom_density_ridges(fill="#E41A1C", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#E41A1C") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +   
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

    # Combine all three density plots
grid.newpage()
combo_geo4_dist <- ggarrange(geo4_dist1, geo4_dist2, geo4_dist3, ncol = 3, nrow = 1, align = "h") 

    # Plot trends over time
geo4_line <- 
    df %>% 
    filter(v2x_regime > 0 & e_regionpol_6C == 4) %>%
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-2, 2)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_smooth(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", fill = "#377EB8") +
    geom_smooth(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", fill = "#4DAF4A") +
    geom_smooth(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", fill = "#E41A1C") +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous("Year", limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme_minimal() + theme(axis.title = element_blank())

    # Combine density with line plot
grid.newpage()
combo_geo4 <- 
    ggarrange(combo_geo4_dist, geo4_line, ncol = 2, nrow = 1, align = "v") %>% 
    annotate_figure(left = text_grob("Sub-Saharan Africa \n ", face = "bold", size = 10, rot = 90))
#print(combo_geo4)  


    ###### Region 5 ----

geo5_dist1 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 5) %>% 
    ggplot(aes(x = orgext, y = 0)) +
    geom_density_ridges(fill="#377EB8", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#377EB8") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))

geo5_dist2 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 5) %>% 
    ggplot(aes(x = powercon, y = 0)) +
    geom_density_ridges(fill="#4DAF4A", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#4DAF4A") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

geo5_dist3 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 5) %>% 
    ggplot(aes(x = cohesion, y = 0)) +
    geom_density_ridges(fill="#E41A1C", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#E41A1C") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +   
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

    # Combine all three density plots
grid.newpage()
combo_geo5_dist <- ggarrange(geo5_dist1, geo5_dist2, geo5_dist3, ncol = 3, nrow = 1, align = "h")

    # Plot trends over time
geo5_line <- 
    df %>% 
    filter(v2x_regime > 0 & e_regionpol_6C == 5) %>%
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-2, 2)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_smooth(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", fill = "#377EB8") +
    geom_smooth(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", fill = "#4DAF4A") +
    geom_smooth(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", fill = "#E41A1C") +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous("Year", limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme_minimal() + theme(axis.title = element_blank())

    # Combine density with line plot
grid.newpage()
combo_geo5 <-
    ggarrange(combo_geo5_dist, geo5_line, ncol = 2, nrow = 1, align = "v") %>% 
    annotate_figure(left = text_grob("Western Europe \n and North America", face = "bold", size = 10, rot = 90))
#print(combo_geo5)


    ###### Region 6 ----

geo6_dist1 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 6) %>% 
    ggplot(aes(x = orgext, y = 0)) +
    geom_density_ridges(fill="#377EB8", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#377EB8") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1))

geo6_dist2 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 6) %>% 
    ggplot(aes(x = powercon, y = 0)) +
    geom_density_ridges(fill="#4DAF4A", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#4DAF4A") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

geo6_dist3 <- 
    df %>%
    filter(v2x_regime > 0 & e_regionpol_6C == 6) %>% 
    ggplot(aes(x = cohesion, y = 0)) +
    geom_density_ridges(fill="#E41A1C", alpha=0.5, quantile_lines = TRUE, quantiles = 4) + 
    geom_rug(sides = "b", color = "#E41A1C") +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +   
    scale_x_continuous(limits = c(-6, 6), breaks = waiver()) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL, labels = NULL)

    # Combine all three density plots
grid.newpage()
combo_geo6_dist <- ggarrange(geo6_dist1, geo6_dist2, geo6_dist3, ncol = 3, nrow = 1, align = "h")

    # Plot trends over time
geo6_line <- 
    df %>% 
    filter(v2x_regime > 0 & e_regionpol_6C == 6) %>%
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-2, 2)) + # to force smooth to include all obs the y-scale needs to be set this way
    geom_smooth(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", fill = "#377EB8") +
    geom_smooth(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", fill = "#4DAF4A") +
    geom_smooth(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", fill = "#E41A1C") +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) + # arbitrarily set to 100 to avoid removal of obs outside of y-axis range
    scale_x_continuous("Year", limits = c(1970, 2020), breaks = waiver()) + 
    geom_hline(yintercept = 0, linetype="dashed") +
    theme_minimal() + theme(axis.title = element_blank())
print(geo6_line)

    # Combine density with line plot
grid.newpage()
combo_geo6 <-
    ggarrange(combo_geo6_dist, geo6_line, ncol = 2, nrow = 1, align = "v")  %>% 
    annotate_figure(left = text_grob("Asia and Pacific \n ", face = "bold", size = 10, rot = 90))
#print(combo_geo6)


# Write outfiles for main text: regional variation and development
grid.newpage()
combo_out <- 
    ggarrange(combo_geo1, combo_geo2, combo_geo3, combo_geo4, combo_geo5, combo_geo6, ncol = 1, nrow = 6, align = "h") %>% 
    annotate_figure(bottom = text_grob("\u25a0 Organizational extensiveness",    face = "bold", size = 10, color = "#377EB8", hjust = 0, x = 0.3)) %>% 
    annotate_figure(bottom = text_grob("\u25a0 Intra-party power concentration", face = "bold", size = 10, color = "#4DAF4A", hjust = 0, x = 0.3)) %>% 
    annotate_figure(bottom = text_grob("\u25a0 Elite cohesion",                  face = "bold", size = 10, color = "#E41A1C", hjust = 0, x = 0.3))
#print(combo_out)

#graph2svg(x = combo_out, file = "./analyze_partyorga/fig2_regional_var_trends.svg", width = 8, height = 10) # for journals
graph2png(x = combo_out, file = "./analyze_partyorga/fig2_regional_var_trends.png", width = 8, height = 10) # for working paper



## Figure 3 Selected parties ----

     
    ###### Germany ----
line1 <- 
    df %>% 
    filter(v2paid == "1816") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Germany: B90/Grue")

    ###### Hungary ----
line2 <- 
    df %>%
    filter(v2paid == "1691") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Hungary: Fidesz")

    ###### Turkey ----
line3 <- 
    df %>%
    filter(v2paid == "306") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Turkey: AKP")

    ###### Brazil ----
line4 <- 
    df %>%
    filter(v2paid == "356") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Brazil: PT")

    ###### Mexico ----
line5 <- 
    df %>%
    filter(v2paid == "1474") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Mexico: PRI")

    ###### Japan ----
line6 <- 
    df %>%
    filter(v2paid == "1746") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Japan: LDP")

    # Write outfile for main text
grid.newpage()
#combo_out <- ggarrange(line1, line2, line3, line4, line5, line6, ncol = 3, nrow = 2, common.legend = TRUE, legend="bottom")
grid.newpage()
combo_out <- 
    ggarrange(line1, line2, line3, line4, line5, line6, ncol = 3, nrow = 2, align = "h") %>% 
    annotate_figure(bottom = text_grob("\u25a0 Organizational extensiveness",    face = "bold", size = 10, color = "#377EB8", hjust = 0, x = 0.3)) %>% 
    annotate_figure(bottom = text_grob("\u25a0 Intra-party power concentration", face = "bold", size = 10, color = "#4DAF4A", hjust = 0, x = 0.3)) %>% 
    annotate_figure(bottom = text_grob("\u25a0 Elite cohesion",                  face = "bold", size = 10, color = "#E41A1C", hjust = 0, x = 0.3))

#graph2svg(x = combo_out, file = "./analyze_partyorga/fig3_selected_parties.svg", width = 8, height = 8) # for journals
graph2png(x = combo_out, file = "./analyze_partyorga/fig3_selected_parties.png", width = 8, height = 8)



## Figure 4 Criterion Validity ----


    ###### Rohrschneider and Whitefield 2012 ----
match1 <- crossv_rw12 %>% select(v2paid, year, q22) 
match2 <- df %>% select(v2x_regime, v2paid, year, v2pasoctie, orgext)
overlap <- match1 %>% fuzzy_left_join(match2, by = c("v2paid" = "v2paid", "year" = "year"), match_fun = list(`==`, `>=`)) %>% filter(year.x - 5 <= year.y) %>% filter(v2x_regime > 0)

tmp_n <- overlap %>% select(v2paid.x, q22, v2pasoctie, orgext) %>% filter(complete.cases(.)) %>% nrow() # no of obs
tmp_N <- overlap %>% select(v2paid.x, q22, v2pasoctie, orgext) %>% filter(complete.cases(.)) %>% distinct(v2paid.x) %>% nrow() # no of obs
annotation <- paste0("n = ", tmp_n, ", N = ", tmp_N)

scat1 <- 
    overlap %>%
    ggplot(aes(q22, v2pasoctie)) + 
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Affiliation with interest groups (q22)", y = "Affiliate organizations (v2pasoctie)")

scat2 <-
    overlap %>%
    ggplot(aes(q22, orgext)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Affiliation with interest groups (q22)", y = "Organizational extensiveness")


    ###### Kitschelt (2013) ----
match1 <- crossv_dalp %>% select(v2paid, year, a1)
match2 <- df %>% select(v2x_regime, v2paid, year, v2palocoff, orgext)
overlap <- match1 %>% fuzzy_left_join(match2, by = c("v2paid" = "v2paid", "year" = "year"), match_fun = list(`==`, `>=`)) %>% filter(year.x - 5 <= year.y) %>% filter(v2x_regime > 0)

tmp_n <- overlap %>% select(v2paid.x, a1, v2palocoff, orgext) %>% filter(complete.cases(.)) %>% nrow() # no of obs
tmp_N <- overlap %>% select(v2paid.x, a1, v2palocoff, orgext) %>% filter(complete.cases(.)) %>% distinct(v2paid.x) %>% nrow() # no of obs
annotation <- paste0("n = ", tmp_n, ", N = ", tmp_N)

scat3 <- 
    overlap %>%
    ggplot(aes(a1, v2palocoff)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Maintain offices at the local level (a1)", y = "Local party office (v2palocoff)")

scat4 <-
    overlap %>% 
    ggplot(aes(a1, orgext)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Maintain offices at the local level (a1)", y = "Organizational extensiveness")


    ###### Schumacher and Giger (2017) ----
match1 <- crossv_gs15 %>% select(v2paid, year, bopla)
match2 <- df %>% select(v2x_regime, v2paid, year, v2paindrev, powercon)
overlap <- match1 %>% fuzzy_left_join(match2, by = c("v2paid" = "v2paid", "year" = "year"), match_fun = list(`==`, `>=`)) %>% filter(year.x - 5 <= year.y) %>% filter(v2x_regime > 0)

tmp_n <- overlap %>% select(v2paid.x, bopla, v2paindrev, powercon) %>% filter(complete.cases(.)) %>% nrow() # no of obs
tmp_N <- overlap %>% select(v2paid.x, bopla, v2paindrev, powercon) %>% filter(complete.cases(.)) %>% distinct(v2paid.x) %>% nrow() # no of obs
annotation <- paste0("n = ", tmp_n, ", N = ", tmp_N)

scat5 <- 
    overlap %>%
    ggplot(aes(bopla, v2paindrev)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    scale_y_continuous(limits = c(-3, 5), breaks = waiver()) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Leadership domination (bopla)", y = "Personalization of party (v2paindrev)")

scat6 <-
    overlap %>% 
    ggplot(aes(bopla, powercon)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    scale_y_continuous(limits = c(-3, 5), breaks = waiver()) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Leadership domination (bopla)", y = "Intra-party power concentration")


    ###### Janda (1980) ----
match1 <- crossv_ja80 %>% select(v2paid, year, selparliacan)
match2 <- df %>% select(v2x_regime, v2paid, year, v2panom, powercon)
overlap <- match1 %>% fuzzy_left_join(match2, by = c("v2paid" = "v2paid", "year" = "year"), match_fun = list(`==`, `>=`)) %>% filter(year.x - 5 <= year.y) %>% filter(v2x_regime > 0)

tmp_n <- overlap %>% select(v2paid.x, selparliacan, v2panom, powercon) %>% filter(complete.cases(.)) %>% nrow() # no of obs
tmp_N <- overlap %>% select(v2paid.x, selparliacan, v2panom, powercon) %>% filter(complete.cases(.)) %>% distinct(v2paid.x) %>% nrow() # no of obs
annotation <- paste0("n = ", tmp_n, ", N = ", tmp_N)

scat7 <- 
    overlap %>%
    ggplot(aes(selparliacan, v2panom)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "spearman", p.accuracy = 0.001, r.accuracy = 0.001, cor.coef.name = "rho", label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Selecting parliamentary candidates (selparliacan)", y = "Candidate nomination (v2panom)")

scat8 <-
    overlap %>% 
    ggplot(aes(selparliacan, powercon)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "spearman", p.accuracy = 0.001, r.accuracy = 0.001, cor.coef.name = "rho", label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Selecting parliamentary candidates (selparliacan)", y = "Intra-party power concentration")


    # Write outfile for main text
combo1 <- ggarrange(scat1, scat2, ncol = 2) %>% annotate_figure(bottom = text_grob("Rohrschneider and Whitefield (2012) \n", face = "bold", size = 10))
combo2 <- ggarrange(scat3, scat4, ncol = 2) %>% annotate_figure(bottom = text_grob("Kitschelt (2013) \n", face = "bold", size = 10))
combo3 <- ggarrange(scat5, scat6, ncol = 2) %>% annotate_figure(bottom = text_grob("Schumacher and Giger (2017) \n", face = "bold", size = 10))
combo4 <- ggarrange(scat7, scat8, ncol = 2) %>% annotate_figure(bottom = text_grob("Janda (1980) \n", face = "bold", size = 10))

grid.newpage()
combo_out <- ggarrange(combo1, combo2, combo3, combo4, ncol = 1)
#graph2svg(x = combo_out, file = "./analyze_partyorga/fig4_cross_validations.svg", width = 8, height = 10.5) # for journals
graph2png(x = combo_out, file = "./analyze_partyorga/fig4_cross_validations.png", width = 8, height = 10.5) # for working paper





##### 3 Regression Analysis ----


    # define common sample as some obs do not have complete data e.g. due to lags
df_analysis <- 
    df %>%
    filter(v2x_regime > 0) %>% 
    select(death1, orgext, powercon, v2padisa, statefund, logconsecel, v2x_polyarchy, parl, proportional, mixed, logcounter1, country_id, v2paid) %>%
    filter(complete.cases(.))

    # base model
m1 <- glmer(death1 ~ orgext + powercon + v2padisa + logcounter1 + (1|country_id) + (1|v2paid),
            family = binomial(link = "logit"), control = glmerControl(optimizer = "Nelder_Mead"), nAGQ = 0, data = df_analysis)
summary(m1)

    # model including controls
m2 <- glmer(death1 ~ orgext + powercon + v2padisa + statefund + logconsecel + v2x_polyarchy + parl + proportional + mixed + logcounter1 + (1|country_id) + (1|v2paid),
            family = binomial(link = "logit"), control = glmerControl(optimizer = "Nelder_Mead"), nAGQ = 0, data = df_analysis)
summary(m2)

    # prepare output
m1tidy <- tidy(m1) %>% mutate(model = "Model 1")
m2tidy <- tidy(m2) %>% mutate(model = "Model 2")

models_out <- bind_rows(m1tidy, m2tidy) %>% filter(!term %in% c("(Intercept)", "sd__(Intercept)"))


    # coeff plot
coeffplot <- 
    dwplot(models_out, dodge_size = .4) %>% 
    relabel_predictors(
        c(orgext = "Organizational extensiveness", powercon = "Intra-party power concentration", v2padisa = "Elite cohesion",
          statefund = "State funding", seniorgov = "Executive party", juniorgov = "Junior partner", logconsecel = "ln(Consecutive elections #)", v2x_polyarchy = "Polyarchy", 
          parl = "Parliamentary system", proportional = "Proportional electoral system", mixed = "Mixed electoral system",  logcounter1 = "ln(Duration)")) + 
    theme_bw() + scale_color_brewer(palette="Set1") +
    xlab("") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2, size = 0.4) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(), legend.text = element_text(size = 10),
          axis.text = element_text(size = 10), legend.spacing = unit(0, "cm"), 
          legend.spacing.x = unit(0, "cm"), legend.box.spacing = unit(-0.5, "cm"))

#graph2svg(x = coeffplot, file = "./analyze_partyorga/fig5_coeffplot.svg", width = 8, height = 6) # for journals
graph2png(x = coeffplot, file = "./analyze_partyorga/fig5_coeffplot.png", width = 8, height = 6) # for working paper


    # predicted probabilities 
m1pred <- ggpredict(m2, terms = c("orgext[all]"))
plot1 <- plot(m1pred, ci.style = "ribbon", line.size = 0.8) + geom_rug(sides = "b", color = "black") + ggtitle("") + ylab("Pr(Party death)") + xlab("Organizational extensiveness") + ylim(0,1)

m2pred <- ggpredict(m2, terms = c("powercon[all]"))
plot2 <- plot(m2pred, ci.style = "ribbon", line.size = 0.8) + geom_rug(sides = "b", color = "black") + ggtitle("") + ylab("Pr(Party death)") + xlab("Intra-party power concentration") + ylim(0,0.8)

m2pred1 <- ggpredict(m2, terms = c("v2padisa[all]"))
plot3 <- plot(m2pred1, ci.style = "ribbon", line.size = 0.8) + geom_rug(sides = "b", color = "black") + ggtitle("") + ylab("Pr(Party death)") + xlab("Elite cohesion") + ylim(0,0.8)

grid.newpage()
combo_out <- ggarrange(plot1, plot2, plot3, nrow = 1)
#graph2svg(x = combo_out, file = "./analyze_partyorga/fig6_predicted_prob.svg", width = 8, height = 4) # for journals
graph2png(x = combo_out, file = "./analyze_partyorga/fig6_predicted_prob.png", width = 8, height = 4) # for working paper

