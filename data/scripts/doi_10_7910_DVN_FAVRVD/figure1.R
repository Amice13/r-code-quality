############ Setup ##########
# This analysis was conducted using R Studio Version 1.1.419 for Mac and R version 3.5.2 for Mac,
# as well as R version 4.2.0 for Windows
remove(list=ls())
set.seed(8675309)

# remember to set working directory to source file location

pacman::p_load(foreign, ggplot2, plm, reshape2, countrycode, sandwich, lmtest, MASS, haven,
               rworldmap, RColorBrewer, states, mice, VIM, stargazer, margins, clusterSEs, lme4, optimx,
               coefplot, lattice, survey, dplyr, wordcloud, sampleSelection, quanteda,
               Matrix, ldatuning, topicmodels, readtext, stm, lda, bursts, tidytext, 
               tm, readstata13, estimatr, grid, gridExtra, data.table, readxl, 
               quanteda, fixest, xtable, ggpubr, xtable, PanelMatch) # load relevant packages

## Load Pubs ##
bur <- read_xls("bureaucracies_WoS_RC.xls") # load Web of Science data drawn from query 
# pulling publications mentioning both “bureaucrat” or “bureaucracy” and “international” 
# or “foreign” in the American Journal of Political Science, American Political Science 
# Review, International Organization, Journal of Politics, International Security, International 
# Studies Quarterly, Review of International Organizations, and World Politics; query made in Feb 2025

bur$count <- 1 # create count measure
bur_agg <- aggregate(bur$count, list(bur$`Publication Year`, bur$Level), FUN = sum) # sum by year and level
colnames(bur_agg) <- c("Year", "Level", "Count") # change col names
bur_agg$Level <- as.factor(bur_agg$Level) # set as factor for plotting
ggplot(bur_agg, aes(y = Count, x = Year, fill = Level)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12)) # side by side plot

ggplot(bur_agg, aes(y = Count, x = Year, fill = Level)) +
  scale_fill_grey() +
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12, 14)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12)) # stacked plot,  reported in Figure 1
  