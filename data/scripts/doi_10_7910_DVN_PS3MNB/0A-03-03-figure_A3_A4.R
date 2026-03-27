#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting
##########################################################################################
# Description
##########################################################################################
# Figure A3 and A4
##########################################################################################
# Contents
##########################################################################################
# 1) Dependencies
# 2) Data Import
# 3) Data Processing Figure A3 and A4
# 4) Responsiveness Plot Figure A3 and A4
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
final_input_s <- readRDS("../var/onetime-structural-shock-irfs-results_all_small.RDS")
# - topic names for model selection
mod_name_nice <- c("Environment", "Gender", "Immigration", "Europe", "All Four")

# - load ddl theme
suppressMessages(suppressWarnings(source('../ggplot_theme_ddl.R')))
##########################################################################################
# 3) Data Processing Figure A3 & A4
##########################################################################################
#-----------------------------------------------------------------------------------------
# - get data for responsivness plot of all topics togehter
all_opics_all <- final_input_s[[5]]
all_opics_all$topic <- mod_name_nice[5]
all_opics_all <- all_opics_all %>% dplyr::filter(day %in% c(1,3,5,7,9))


# - relabel the outcomes so they fit/look better in the plot
all_opics_all$out <- recode(all_opics_all$out,
                            `Media_SMD` = "Newspaper Articles",
                            `Politician_TW` = "Tweets by Politicians",
                            `Party_TW` = "Tweets by Parties",
                            `Gov_TW` = "Tweets by Newspapers",
                            `Media_TW` = "Tweets by Media",
                            `Org_PR` = "Press Releases by Organizations",
                            `Org_TW` = "Tweets by Organizations",
                            `Party_PR` = "Press Releases by Parties",
                            `Gov_PR` = "Press Releases by Government")

# - relabel the covariates so they fit/look better in the plot
all_opics_all$cov <- recode(all_opics_all$cov,
                            `Media_SMD` = "Newspaper Articles",
                            `Politician_TW` = "Tweets by Politicians",
                            `Party_TW` = "Tweets by Parties",
                            `Gov_TW` = "Tweets by Government",
                            `Media_TW` = "Tweets by Newspapers",
                            `Org_PR` = "Press Releases by Organizations",
                            `Org_TW` = "Tweets by Organizations",
                            `Party_PR` = "Press Releases by Parties",
                            `Gov_PR` = "Press Releases by Government")


#-----------------------------------------------------------------------------------------
# - combine data from all four topics models
for(n in 1:(length(final_input_s)-1)){
  tmp <- final_input_s[[n]]
  tmp$topic <- mod_name_nice[n]
  
  if(n == 1){
    all_topics <- tmp
  } else if(n == 4){
    all_topics <- rbind(all_topics, tmp)
    all_topics <- all_topics %>% dplyr::filter(day %in% c(1,3,5,7,9))
    # - relabel the outcomes so they fit/look better in the plot
    all_topics$out <- recode(all_topics$out,
                             `Media_SMD` = "Newspaper Articles",
                             `Politician_TW` = "Tweets by Politicians",
                             `Party_TW` = "Tweets by Parties",
                             `Gov_TW` = "Tweets by Government",
                             `Media_TW` = "Tweets by Newspapers",
                             `Org_PR` = "Press Releases by Organizations",
                             `Org_TW` = "Tweets by Organizations",
                             `Party_PR` = "Press Releases by Parties",
                             `Gov_PR` = "Press Releases by Government")
    
    # - relabel the covariates so they fit/look better in the plot
    all_topics$cov <- recode(all_topics$cov,
                             `Media_SMD` = "Newspaper Articles",
                             `Politician_TW` = "Tweets by Politicians",
                             `Party_TW` = "Tweets by Parties",
                             `Gov_TW` = "Tweets by Government",
                             `Media_TW` = "Tweets by Newspapers",
                             `Org_PR` = "Press Releases by Organizations",
                             `Org_TW` = "Tweets by Organizations",
                             `Party_PR` = "Press Releases by Parties",
                             `Gov_PR` = "Press Releases by Government")
    
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
  mutate(topic_rev = fct_relevel(topic, "Immigration", "Europe", "Gender", "Environment"))
##########################################################################################
# 4) Responsiveness Plot Figure A3 and A4
##########################################################################################
# - mkae facet names for titles
facet_names <- c(`Tweets by Parties` = "Effect of Tweets by Parties on:",
                 `Tweets by Politicians` = "Effect of Tweets by Politicians on:",
                 `Tweets by Newspapers` = "Effect of Tweets by Newspaper on:",
                 `Tweets by Organizations` = "Effect of Tweets by Organizations on:",
                 `Tweets by Government` = "Effect of Tweets by the Government on:",
                 `Newspaper Articles` = "Effect of Newspaper Articles on:",
                 `Press Releases by Parties` = "Effect of Press Releases by Parties on:",
                 `Press Releases by Government` = "Effect of Press Releases by the Government on:",
                 `Press Releases by Organizations` = "Effect of Press Releases by Organizations on:")

max_width <- max(nchar(as.character(plot_resp_ii$out)))
plot_resp_ii$out <- sprintf(paste0("%-",max_width, "s"), plot_resp_ii$out)

for(pn in 1:2){
  if(pn <= 1){
    nn <- 5
  } else {
    nn <- 4
  }
  
  ggplot(data = plot_resp_ii) +
    aes(x = out, y = pe, ymin = lwr, ymax = upr, group = topic_rev, color = topic, shape = topic) +
    geom_linerange(size = 2.5, position = position_dodge(width = 0.55), alpha = 0.4) +  
    geom_point(position = position_dodge(width = 0.55), size = 2.5) +
    geom_hline(aes(yintercept = 0), color = "black", linetype = 2, alpha = 0.75) +
    coord_flip() +
    labs (subtitle = "",
          title = "",
          y = "Percentage points", x = "") +
    facet_wrap_paginate(~ cov,  ncol = 1, nrow = nn, scales = "free_y", 
                        labeller = as_labeller(facet_names), page = pn) + 
    scale_y_continuous(breaks = c(-2,-1,0,1,2,3,4)) +
    scale_color_manual(labels = c("Environment","Gender","Europe","Immigration"), 
                       values = c("#009E73","#DD2461","#0072B2","#999999")) +
    scale_fill_manual(labels = c("Environment","Gender","Europe","Immigration"), 
                      values = c("#009E73","#DD2461","#0072B2","#999999")) +
    ddl_theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
    theme(legend.position = "bottom", legend.title = element_blank(), 
          strip.background = element_blank(), strip.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.0, size = 16, color = "black"),  
          axis.text.y = element_text(hjust=0, size = 16, color = "black"),
          axis.ticks.y = element_blank(),
          axis.title = element_text(size = 16, color = "black"),
          strip.text.x = element_text(size = 16, color = "black"),
          legend.text = element_text(size = 16, color = "black"),
          plot.title = element_blank(),
          plot.margin = unit(c(.5,.5,.5,.5), "cm"),
          legend.key.size = unit(1.5,"line"),
          legend.key = element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          panel.spacing.y = unit(2, "lines"),
          panel.spacing.x = unit(1, "lines")) +
    guides(color = guide_legend(override.aes = list(size=2)))
  
  if(pn <= 2){
    ggsave(paste0("../images/figure_A",pn+2,".pdf"), width = 10, height = 15, device = cairo_pdf)
  } else {
    ggsave(paste0("../images/figure_A",pn+2,".pdf"), width = 10, height = 12, device = cairo_pdf)

  }
  
}
##########################################################################################