#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting
##########################################################################################
# Description
##########################################################################################
# Resposivness Figure A2 (no press releases)
##########################################################################################
# Contents
##########################################################################################
# 1) Dependencies
# 2) Data Import
# 3) Data Processing small model
# 4) Responsiveness Plot small model
# 5) Data Processing big model
# 6) Responsiveness Plot big model
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
# 3) Data Processing Figure A2
##########################################################################################
#-----------------------------------------------------------------------------------------
# - get data for responsiveness plot of all topics together
all_opics_all <- final_input_s[[5]]
all_opics_all$topic <- mod_name_nice[5]
all_opics_all <- all_opics_all %>% dplyr::filter(day %in% c(1,3,5,7,9))
all_opics_all <- all_opics_all %>% dplyr::filter(out %in% c("Media_SMD", "Politician_TW", "Party_TW")) %>% 
                                   dplyr::filter(cov %in% c("Media_SMD", "Politician_TW", "Party_TW"))

# - relabel the outcomes so they fit/look better in the plot
all_opics_all$out <- recode(all_opics_all$out,
                            `Media_SMD` = "Newspaper Articles",
                            `Politician_TW` = "Tweets by Politicians",
                            `Party_TW` = "Tweets by Parties")

# - relabel the covariates so they fit/look better in the plot
all_opics_all$cov <- recode(all_opics_all$cov,
                            `Media_SMD` = "Newspaper Articles",
                            `Politician_TW` = "Tweets by Politicians",
                            `Party_TW` = "Tweets by Parties")

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
    all_topics <- all_topics %>% dplyr::filter(out %in% c("Media_SMD", "Politician_TW", "Party_TW")) %>% 
                                 dplyr::filter(cov %in% c("Media_SMD", "Politician_TW", "Party_TW"))
    # - relabel the outcomes so they fit/look better in the plot
    all_topics$out <- recode(all_topics$out,
                             `Media_SMD` = "Newspaper Articles",
                             `Politician_TW` = "Tweets by Politicians",
                             `Party_TW` = "Tweets by Parties")
    
    # - relabel the covariates so they fit/look better in the plot
    all_topics$cov <- recode(all_topics$cov,
                             `Media_SMD` = "Newspaper Articles",
                             `Politician_TW` = "Tweets by Politicians",
                             `Party_TW` = "Tweets by Parties")
    
  } else {
    all_topics <- rbind(all_topics, tmp)
    rm(tmp)
  }
}

# - rename values for nicer plot of responsiveness plot per topic
plot_resp_ii <- all_topics %>%
  filter(day == 7 & data_type == "Effect of a one time 10 percentage point attention increase at day 0") %>%
  mutate(cov2 = case_when(cov == "Newspaper Articles" ~ "Newspapers", cov == "Tweets by Parties" ~ "Parties", 
                          cov == "Tweets by Politicians" ~ "Politicians")) %>%
  mutate(out2 = case_when(out == "Newspaper Articles" ~ "Newspapers", out == "Tweets by Parties" ~ "Parties", 
                          out == "Tweets by Politicians" ~ "Politicians")) %>%
  mutate(direction = paste0(cov2, " -> ", out2)) %>%
  mutate(TOPIC = str_to_upper(topic)) %>%
  mutate(topic = fct_relevel(topic, "Environment", "Gender", "Europe", "Immigration")) %>%
  mutate(topic_rev = fct_relevel(topic, "Immigration", "Europe", "Gender", "Environment"))
##########################################################################################
# 4) Responsiveness Figure A2
##########################################################################################
# - make facet names 
facet_names <- c(`Tweets by Parties` = "Effect of Tweets by Parties on:",
                 `Tweets by Politicians` = "Effect of Tweets by Politicians on:",
                 `Newspaper Articles` = "Effect of Newspaper Articles on:")

max_width <- max(nchar(as.character(plot_resp_ii$out)))
plot_resp_ii$out <- sprintf(paste0("%-",max_width, "s"), plot_resp_ii$out)

ggplot(data = plot_resp_ii) +
  aes(x = out, y = pe, ymin = lwr, ymax = upr, group = topic_rev, color = topic, shape = topic) +
  geom_linerange(size = 2.5, position = position_dodge(width = 0.55), alpha = 0.4) +  
  geom_point(position = position_dodge(width = 0.55), size = 2.5) +
  geom_hline(aes(yintercept = 0), color = "black", linetype = 2, alpha = 0.75) +
  coord_flip() +
  labs (subtitle = "",
        title = "",
        y = "Percentage points", x = "") +
  facet_wrap(~ cov,  ncol = 1, nrow = 3, scales = "free_y", 
             labeller = as_labeller(facet_names)) + 
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

ggsave("../images/figure_A2.pdf", width = 10, height = 8, device = cairo_pdf)
##########################################################################################

