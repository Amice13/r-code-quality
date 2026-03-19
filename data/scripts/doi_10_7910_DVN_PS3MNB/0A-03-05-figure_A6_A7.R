#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting
##########################################################################################
# Description
##########################################################################################
# Figure  A6 and A7 (responsiveness with parties split)
##########################################################################################
# Contents
##########################################################################################
# 1) Dependencies
# 2) Data Import
# 3) Data Processing Figure A6 and A7
# 4) Responsiveness Plot Figure A6 and A7
##########################################################################################
# 1) Dependencies
##########################################################################################
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(showtext)
library(ggforce)
##########################################################################################
# 2) Data Import
##########################################################################################
rm(list = ls())
# - set dir
args = commandArgs()

scriptName = args[substr(args,1,7) == '--file=']

if (length(scriptName) == 0) {
  scriptName <- rstudioapi::getSourceEditorContext()$path
} else {
  scriptName <- substr(scriptName, 8, nchar(scriptName))
}

pathName = substr(
  scriptName, 
  1, 
  nchar(scriptName) - nchar(strsplit(scriptName, '.*[/|\\]')[[1]][2])
)

setwd(pathName)
parent_path <- getwd()

# - load fonts used in plots
font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")

# - define a global seed (used in all scripts)
set.seed(2019)

# - load irf data 
final_input_b <- readRDS("../var/onetime-structural-shock-irfs-results_all_big.RDS")
# - topic names for model selection
mod_name_nice <- c("Environment", "Gender", "Immigration", "Europe", "All Four")

# - load ddl theme
suppressMessages(suppressWarnings(source('../ggplot_theme_ddl.R')))
##########################################################################################
# 3) Data Processing Figure A6 and A7
##########################################################################################
#-----------------------------------------------------------------------------------------
# - get data for responsiveness plot of all topics togehter
all_opics_all <- final_input_b[[5]]
all_opics_all$topic <- mod_name_nice[5]
all_opics_all <- all_opics_all %>% dplyr::filter(day %in% c(1,3,5,7,9))
all_opics_all <- all_opics_all %>% dplyr::filter(out %in% c("Media_SMD", "Candidate_FDP_TW", "Candidate_GLP_TW", "Candidate_Grüne_TW",
                                                            "Candidate_SP_TW", "Candidate_SVP_TW", "Party_FDP_TW", "Party_GLP_TW",
                                                            "Party_Grüne_TW", "Party_SP_TW", "Party_SVP_TW")) %>% 
  dplyr::filter(cov %in% c("Media_SMD", "Candidate_FDP_TW", "Candidate_GLP_TW", "Candidate_Grüne_TW",
                           "Candidate_SP_TW", "Candidate_SVP_TW", "Party_FDP_TW", "Party_GLP_TW",
                           "Party_Grüne_TW", "Party_SP_TW", "Party_SVP_TW"))

# - relabel the outcomes so they fit/look better in the plot
all_opics_all$out <- recode(all_opics_all$out,
                            `Media_SMD` = "Newspapers",
                            `Candidate_FDP_TW` = "Tweets by FDP Politicians",
                            `Candidate_GLP_TW` = "Tweets by GLP Politicians",
                            `Candidate_Grüne_TW` = "Tweets by Green Politicians",
                            `Candidate_SP_TW` = "Tweets by SP Politicians",
                            `Candidate_SVP_TW` = "Tweets by SVP Politicians",
                            `Party_FDP_TW` = "Twets by FDP",
                            `Party_GLP_TW` = "Twets by GLP",
                            `Party_Grüne_TW` = "Twets by Green Party",
                            `Party_SP_TW` = "Twets by SP",
                            `Party_SVP_TW` = "Twets by SVP")

# - relabel the covariates so they fit/look better in the plot
all_opics_all$cov <- recode(all_opics_all$cov,
                            `Media_SMD` = "Newspapers",
                            `Candidate_FDP_TW` = "Tweets by FDP Politicians",
                            `Candidate_GLP_TW` = "Tweets by GLP Politicians",
                            `Candidate_Grüne_TW` = "Tweets by Green Politicians",
                            `Candidate_SP_TW` = "Tweets by SP Politicians",
                            `Candidate_SVP_TW` = "Tweets by SVP Politicians",
                            `Party_FDP_TW` = "Twets by FDP",
                            `Party_GLP_TW` = "Twets by GLP",
                            `Party_Grüne_TW` = "Twets by Green Party",
                            `Party_SP_TW` = "Twets by SP",
                            `Party_SVP_TW` = "Twets by SVP")


#-----------------------------------------------------------------------------------------
# - combine data from all four topics models
for(n in 1:(length(final_input_b)-1)){
  tmp <- final_input_b[[n]]
  tmp$topic <- mod_name_nice[n]
  
  if(n == 1){
    all_topics <- tmp
  } else if(n == 4){
    all_topics <- rbind(all_topics, tmp)
    all_topics <- all_topics %>% dplyr::filter(day %in% c(1,3,5,7,9))
    all_topics <- all_topics %>% dplyr::filter(out %in% c("Media_SMD", "Candidate_FDP_TW", "Candidate_GLP_TW","Candidate_Grüne_TW",
                                                          "Candidate_SP_TW","Candidate_SVP_TW","Party_FDP_TW","Party_GLP_TW",
                                                          "Party_Grüne_TW","Party_SP_TW","Party_SVP_TW")) %>% 
      dplyr::filter(cov %in% c("Media_SMD", "Candidate_FDP_TW", "Candidate_GLP_TW","Candidate_Grüne_TW",
                               "Candidate_SP_TW","Candidate_SVP_TW","Party_FDP_TW","Party_GLP_TW",
                               "Party_Grüne_TW","Party_SP_TW","Party_SVP_TW"))
    # - relabel the outcomes so they fit/look better in the plot
    all_topics$out <- recode(all_topics$out,
                             `Media_SMD` = "Newspapers",
                             `Candidate_FDP_TW` = "Tweets by FDP Politicians",
                             `Candidate_GLP_TW` = "Tweets by GLP Politicians",
                             `Candidate_Grüne_TW` = "Tweets by Green Politicians",
                             `Candidate_SP_TW` = "Tweets by SP Politicians",
                             `Candidate_SVP_TW` = "Tweets by SVP Politicians",
                             `Party_FDP_TW` = "Tweets by FDP",
                             `Party_GLP_TW` = "Tweets by GLP",
                             `Party_Grüne_TW` = "Tweets by Green Party",
                             `Party_SP_TW` = "Tweets by SP",
                             `Party_SVP_TW` = "Tweets by SVP")
    
    # - relabel the covariates so they fit/look better in the plot
    all_topics$cov <- recode(all_topics$cov,
                             `Media_SMD` = "Newspapers",
                             `Candidate_FDP_TW` = "Tweets by FDP Politicians",
                             `Candidate_GLP_TW` = "Tweets by GLP Politicians",
                             `Candidate_Grüne_TW` = "Tweets by Green Politicians",
                             `Candidate_SP_TW` = "Tweets by SP Politicians",
                             `Candidate_SVP_TW` = "Tweets by SVP Politicians",
                             `Party_FDP_TW` = "Tweets by FDP",
                             `Party_GLP_TW` = "Tweets by GLP",
                             `Party_Grüne_TW` = "Tweets by Green Party",
                             `Party_SP_TW` = "Tweets by SP",
                             `Party_SVP_TW` = "Tweets by SVP")
    
  } else {
    all_topics <- rbind(all_topics, tmp)
    rm(tmp)
  }
}

# - rename values for nicer plot of responsiveness plot per topic
plot_resp_ii <- all_topics %>%
  filter(day == 7 & data_type == "Effect of a one time 10 percentage point attention increase at day 0") %>%
  mutate(direction = paste0(cov, " -> ", out)) %>%
  mutate(TOPIC = str_to_upper(topic)) %>%
  mutate(topic = fct_relevel(topic, "Environment", "Gender", "Europe", "Immigration")) %>%
  mutate(topic_rev = fct_relevel(topic, "Immigration", "Europe", "Gender", "Environment")) %>% 
  filter(out != "Newspapers") %>% 
  filter(cov != "Newspapers")


plot_resp_ii$cov <- as.factor(plot_resp_ii$cov)
levels(plot_resp_ii$cov)

plot_resp_ii$cov <- factor(plot_resp_ii$cov, level = c("Tweets by FDP","Tweets by GLP","Tweets by Green Party","Tweets by SP", "Tweets by SVP",
                                                       "Tweets by FDP Politicians", "Tweets by GLP Politicians", "Tweets by Green Politicians", 
                                                       "Tweets by SP Politicians", "Tweets by SVP Politicians"))
##########################################################################################
# 4) Responsiveness Plot Figure A6 and A7
##########################################################################################
# - make facet names
facet_names <- c(`Tweets by FDP Politicians` = "Effect of FDP Politicians on:",
                 `Tweets by GLP Politicians` = "Effect of GLP Politicians on:",
                 `Tweets by Green Politicians` = "Effect of Green Politicians on:",
                 `Tweets by SP Politicians` = "Effect of SP Politicians on:",
                 `Tweets by SVP Politicians` = "Effect of SVP Politicians on:",
                 `Tweets by FDP` = "Effect of the FDP on:",
                 `Tweets by GLP` = "Effect of the GLP on:",
                 `Tweets by Green Party` = "Effect of the Green Party on:",
                 `Tweets by SP` = "Effect of the SP on:",
                 `Tweets by SVP` = "Effect of the SVP on:")

# - make axis names
plot_resp_ii$out <- recode(plot_resp_ii$out,
                           `Tweets by FDP` =            "FDP              ",
                           `Tweets by FDP Politicians` = "FDP Politicians  ",
                           `Tweets by GLP` =            "GLP              ",
                           `Tweets by GLP Politicians` = "GLP Politicians  ",
                           `Tweets by Green Party` =    "Green Party      ",
                           `Tweets by Green Politicians`="Green Politicians",
                           `Tweets by SP` =             "SP               ",
                           `Tweets by SP Politicians` =  "SP Politicians   ",
                           `Tweets by SVP` =            "SVP              ",
                           `Tweets by SVP Politicians` = "SVP Politicians  ")

# - order axis names
level_order <- factor(plot_resp_ii$out, level = c("FDP              ",
                                                  "FDP Politicians  ",
                                                  "GLP              ",
                                                  "GLP Politicians  ",
                                                  "Green Party      ",
                                                  "Green Politicians",
                                                  "SP               ",
                                                  "SP Politicians   ",
                                                  "SVP              ",
                                                  "SVP Politicians  "))



for(pn in 1:2){
  if(pn <= 2){
    nn <- 5
  } else {
    nn <- 1
  }
  ggplot(data = plot_resp_ii) +
    aes(x = level_order, y = pe, ymin = lwr, ymax = upr, group = topic_rev, color = topic, shape = topic) +
    geom_linerange(size = 1.8, position = position_dodge(width = 0.55), alpha = 0.4) +  
    geom_point(position = position_dodge(width = 0.55), size = 1.8) +
    geom_hline(aes(yintercept = 0), color = "black", linetype = 2, alpha = 0.75) +
    coord_flip() +
    labs (subtitle = "",
          title = "",
          y = "Percentage points", x = "") +
    scale_x_discrete() +
    scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3,4)) +
    scale_color_manual(labels = c("Environment","Gender","Europe","Immigration"), 
                       values = c("#009E73","#DD2461","#0072B2","#999999")) +
    scale_fill_manual(labels = c("Environment","Gender","Europe","Immigration"), 
                      values = c("#009E73","#DD2461","#0072B2","#999999")) +
    facet_wrap_paginate(~ cov,  ncol = 1, nrow = nn, scales = "free_y", 
                        labeller = as_labeller(facet_names), page = pn) + 
    ddl_theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
    theme(legend.position = "bottom", legend.title = element_blank(), 
          strip.background = element_blank(), strip.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.0, size = 16, color = "black"),  
          axis.text.y = element_text(hjust=0, size = 16, color = "black"),
          axis.ticks.y = element_blank(),
          strip.text.x = element_text(size = 16, color = "black"),
          axis.title = element_text(size = 16, color = "black"),
          legend.text = element_text(size = 16, color = "black"),
          plot.title = element_blank(),
          plot.margin = unit(c(.5,.5,.5,.5), "cm"),
          legend.key.size = unit(1.5,"line"),
          legend.key = element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          panel.spacing.y = unit(2, "lines"),
          panel.spacing.x = unit(.8, "lines")) +
    guides(color = guide_legend(override.aes = list(size=2)))
  
  if(pn <= 3){
    ggsave(paste0("../images/figure_A",pn+5,".pdf"), width = 10, height = 15, device = cairo_pdf)
  } else {
    ggsave(paste0("../images/figure_A",pn+5,".pdf"), width = 10, height = 8, device = cairo_pdf)
  }
  
}
##########################################################################################