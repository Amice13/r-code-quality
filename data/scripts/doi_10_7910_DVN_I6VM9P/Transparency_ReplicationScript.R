##################################################################
##################################################################
###                 Replication Script for                     ###
###       Transparency Matters: The Positive Effect of         ###
### Politicians' Side Income Disclosure on Voters' Perceptions ###
##################################################################
##################################################################

# Authors: Oliver Huwyler, Stefanie Bailer, Nathalie Giger


# Change the language and date formatting to English if it is not already
Sys.setenv(LANG = "EN")
Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
Sys.getlocale(category = "LC_ALL")


# Set working directory
# setwd("...")



############
# Packages #
############

# Installing
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
if("openxlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("openxlsx")}
if("lme4" %in% rownames(installed.packages()) == FALSE) {install.packages("lme4")}
if("mgsub" %in% rownames(installed.packages()) == FALSE) {install.packages("mgsub")}
if("ggeffects" %in% rownames(installed.packages()) == FALSE) {install.packages("ggeffects")}
if("stargazer" %in% rownames(installed.packages()) == FALSE) {install.packages("stargazer")}
if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}
if("grid" %in% rownames(installed.packages()) == FALSE) {install.packages("grid")}
if("ggpubr" %in% rownames(installed.packages()) == FALSE) {install.packages("ggpubr")}
if("vtable" %in% rownames(installed.packages()) == FALSE) {install.packages("vtable")}
if('dotwhisker' %in% rownames(installed.packages()) == FALSE) {install.packages('dotwhisker')}
if('nnet' %in% rownames(installed.packages()) == FALSE) {install.packages('nnet')}
if('cowplot' %in% rownames(installed.packages()) == FALSE) {install.packages('cowplot')}



# Loading
library(stringr)
library(dplyr)
library(sqldf)
library(ggplot2)
library(gtools)
library(openxlsx)
library(lme4)
library(mgsub)
library(ggeffects)
library(stargazer)
library(gridExtra)
library(grid)
library(ggpubr)
library(vtable)
library(dotwhisker)
library(nnet)
library(cowplot)



#################
### Load Data ###
#################

# Load data
SURVEYSAVE <- readRDS(file = "Transparency_Survey.rds")

# Do not include "prefer no to say" cases for gender
SURVEY <- SURVEYSAVE[which(SURVEYSAVE$gender != "prefer not to say"),]
SURVEY$gender <- droplevels(SURVEY$gender)


#####################
#####################
### Main Analyses ###
#####################
#####################



######################
### Tables A6 / A7 ### 
######################


### Table A6

# 1) Model with categories of income transparency:
# Trust:
lmmodt1 <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
              data=SURVEY)
summary(lmmodt1)

# Vote intention:
lmmodv1 <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
              data=SURVEY)
summary(lmmodv1)


# Output the two models
stargazer(lmmodt1,lmmodv1, title="The effect of (non-)transparency on citizens' perceptions",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
          )


### Table A7

# The same models without respondent characteristics:
# 1) Model with categories of income transparency:
# Trust:
lmmodt1alt <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                country,
              data=SURVEY)
summary(lmmodt1alt)

# Vote intention:
lmmodv1alt <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                country,
              data=SURVEY)
summary(lmmodv1alt)


# Output the two models
stargazer(lmmodt1alt,lmmodv1alt, title="The effect of (non-)transparency on citizens' perceptions",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Constant"),
          notes.align = "l" # left alignment
)




################
### Figure 3 ###
################


# 1) Obtain the predicted values for trustworthiness
lmmodt1predict <- ggpredict(lmmodt1, terms = c("transp_vignette_income"))

lmmodt1predict$x = recode(lmmodt1predict$x,
                          "IT"="Non-transparent",
                          "150"="150% of parliamentary salary",
                          "20"="20% of parliamentary salary",
                          "PB"="Pro bono")

lmmodt1predict$x <- factor(lmmodt1predict$x, rev(lmmodt1predict$x))

lmmodt1predict$group <- as.character(lmmodt1predict$group)
lmmodt1predict[which(lmmodt1predict$x == "Non-transparent"), "group"] <- "0"
lmmodt1predict$group <- factor(lmmodt1predict$group)  # convert back to factor if needed
lmmodt1predict$group <- factor(lmmodt1predict$group, levels = c(0, 1), labels = c("Non-disclosure", "Disclosure"))

centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



# 2) Plot the values for trustworthiness
figure3transptrust <- ggplot() +
  geom_bar(data = lmmodt1predict, 
           aes(x = x, y = predicted, fill = group, colour = group), 
           stat = "identity", position = "dodge", linewidth = 0.4) +
  geom_errorbar(data = lmmodt1predict, 
                aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low), 
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("Non-disclosure" = "grey40", "Disclosure" = "white")) +
  scale_colour_manual(values = c("Non-disclosure" = "grey40", "Disclosure" = "black")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid', colour = "grey"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0.1,0.5,0,0.1), "cm")) +
  scale_y_continuous(name = "Trustworthiness", position = "left", 
                     limits = c(0, 6), breaks = seq(0, 5.5, by = 1), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(rev(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))) +
  coord_flip()

figure3transptrust




# 3) Obtain the predicted values for vote intention
lmmodv1predict <- ggpredict(lmmodv1, terms = c("transp_vignette_income"))

lmmodv1predict$x = recode(lmmodv1predict$x,
                          "IT"="Non-transparent",
                          "150"="150% of parliamentary salary",
                          "20"="20% of parliamentary salary",
                          "PB"="Pro bono")

lmmodv1predict$x <- factor(lmmodv1predict$x, rev(lmmodv1predict$x))

lmmodv1predict$group <- as.character(lmmodv1predict$group)
lmmodv1predict[which(lmmodv1predict$x == "Non-transparent"), "group"] <- "0"
lmmodv1predict$group <- factor(lmmodv1predict$group)  # convert back to factor if needed
lmmodv1predict$group <- factor(lmmodv1predict$group, levels = c(0, 1), labels = c("Non-disclosure", "Disclosure"))


centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



# 4) Plot the values for voteworthiness
figure3transpvote <- ggplot() +
  geom_bar(data = lmmodv1predict, 
           aes(x = x, y = predicted, fill = group, colour = group), 
           stat = "identity", position = "dodge", linewidth = 0.4) +
  geom_errorbar(data = lmmodv1predict, 
                aes(x = x, y = predicted, ymax = conf.high, ymin = conf.low), 
                position = position_dodge(width = 0.9), width = 0.25) +
  scale_fill_manual(values = c("Non-disclosure" = "grey40", "Disclosure" = "white")) +
  scale_colour_manual(values = c("Non-disclosure" = "grey40", "Disclosure" = "black")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid', colour = "grey"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0.1,0.5,0,0.1), "cm")) +
  scale_y_continuous(name = "Electability", position = "left", 
                     limits = c(0, 6), breaks = seq(0, 5.5, by = 1), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(rev(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))) +
  coord_flip()

figure3transpvote


# 5) Combined figure output

# a) Extract the legend from one of the figures (assuming it's identical in both)
legend <- ggpubr::get_legend(figure3transptrust + theme(legend.position = "bottom"))

# b) Define a no-margin theme to make alignment cleaner
no_margin_theme <- theme(
  legend.position = "none",
  plot.margin = margin(0, 5, 0, 0)  # top, right, bottom, left
)

# c) Apply no-margin theme to both plots
figure3transptrust_noleg <- figure3transptrust + no_margin_theme
figure3transpvote_noleg <- figure3transpvote + no_margin_theme

# d) Combine the two plots side by side (adjust ncol if you want them stacked)
plot_combined <- plot_grid(
  figure3transptrust_noleg,
  figure3transpvote_noleg,
  ncol = 2,
  align = "hv",  # Align both horizontally and vertically
  axis = "tblr"  # Align axes on all sides
)

# e) Add the shared legend below
figure3 <- plot_grid(
  plot_combined,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.15)  # 90% plot, 15% legend
)

figure3



completefilename <- paste(getwd(),"/Figure3_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 7, width = 20, units = "cm",res = 1200)
figure3
dev.off()

rm(figure3transptrust, figure3transpvote, figure3transptrust_noleg, figure3transpvote_noleg, legend, plot_combined, no_margin_theme)





###########################
### Table A8 / Figure 4 ###
###########################


### Table A8

# 1a) Models with dichotomous variable for transparency status
# Trust:
lmmodt3a <- lm(transp_trust ~ transp_vignette_transparent*transp_vignette_orgtype*transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY[which(SURVEY$transp_vignette_income != "PB" & SURVEY$transp_vignette_income != "20"),])
summary(lmmodt3a)

# Vote intention:
lmmodv3a <- lm(transp_vote ~ transp_vignette_transparent*transp_vignette_orgtype*transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY[which(SURVEY$transp_vignette_income != "PB" & SURVEY$transp_vignette_income != "20"),])
summary(lmmodv3a)



# Output the two models
stargazer(lmmodt3a,lmmodv3a, title="Citizens' perceptions under non-transparency",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c(
            "MP: transparent\\textsuperscript{1}",
            "MP: work for company\\textsuperscript{2}",
            "MP: 5 board seats\\textsuperscript{3}",
            "MP: female",
            "Citizen: right-leaning",
            "Citizen: income",
            "Citizen: education level",
            "Citizen: female",
            "Citizen \\& MP: same gender",
            "Citizen: age",
            "MP: transparent\\textsuperscript{1}$\\\\times$work for company\\textsuperscript{2}",
            "MP: transparent\\textsuperscript{1}$\\\\times$5 board seats\\textsuperscript{3}",
            "MP: work for company\\textsuperscript{2}$\\\\times$5 board seats\\textsuperscript{3}",
            "MP: transparent\\textsuperscript{1}$\\\\times$work for company\\textsuperscript{2}$\\\\times$5 board seats\\textsuperscript{3}",
            "Constant"),
          notes.align = "l" # left alignment
)


### Figure 4

# 2) Obtain the predicted values
# a) for trustworthiness
lmmodt3apredict <- ggpredict(lmmodt3a, terms = c("transp_vignette_orgtype", "transp_vignette_seats", "transp_vignette_transparent"))

lmmodt3apredict$x = recode(lmmodt3apredict$x,
                           "C"="Company",
                           "P"="Public interest group")


lmmodt3apredict$group = recode(lmmodt3apredict$group,
                               "1"="1 board seat",
                               "5"="5 board seats")

lmmodt3apredict$facet = recode(lmmodt3apredict$facet,
                               "IT"="Non-transparent",
                               "T"="Transparent")


lmmodt3apredict$dv <- "Trustworthiness"


# b) for electability
lmmodv3apredict <- ggpredict(lmmodv3a, terms = c("transp_vignette_orgtype", "transp_vignette_seats", "transp_vignette_transparent"))

lmmodv3apredict$x = recode(lmmodv3apredict$x,
                           "C"="Company",
                           "P"="Public interest group")


lmmodv3apredict$group = recode(lmmodv3apredict$group,
                               "1"="1 board seat",
                               "5"="5 board seats")

lmmodv3apredict$facet = recode(lmmodv3apredict$facet,
                               "IT"="Non-transparent",
                               "T"="Transparent")


lmmodv3apredict$dv <- "Electability"


# 3) Combine the output
lmmod2predict <- smartbind(as.data.frame(lmmodt3apredict), 
                           as.data.frame(lmmodv3apredict))



centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



# 4) Plot Figure 2
# Top left (trustworthiness by org_type when non-transparent)
figure4_topleft <- ggplot() +
  geom_bar(data=lmmod2predict[which(lmmod2predict$dv == "Trustworthiness" & lmmod2predict$facet == "Non-transparent" & grepl("Company|Public", lmmod2predict$x)),],
           aes(x=x, y=predicted ,fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmod2predict[which(lmmod2predict$dv == "Trustworthiness" & lmmod2predict$facet == "Non-transparent" & grepl("Company|Public", lmmod2predict$x)),],
                aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  ggtitle(label = "Non-transparent") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0.0,0.5,0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = NULL,position = "right", 
                     limits = c(0, 6), breaks = seq(0, 5.5, by = 1), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(rev(c("Company","Public interest group")))) +
  scale_fill_manual(values = c("1 board seat" = "#7570b3", "5 board seats" = "#d95f02")) +
  coord_flip()

figure4_topleft


# Bottom left (Trustworthiness by org_type when non-transparent)
figure4_bottomleft <- ggplot() +
  geom_bar(data=lmmod2predict[which(lmmod2predict$dv == "Trustworthiness" & lmmod2predict$facet == "Transparent" & grepl("Company|Public", lmmod2predict$x)),],
           aes(x=x, y=predicted ,fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmod2predict[which(lmmod2predict$dv == "Trustworthiness" & lmmod2predict$facet == "Transparent" & grepl("Company|Public", lmmod2predict$x)),],
                aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  ggtitle(label = "Transparent (150% side income)") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="none", legend.direction="horizontal",
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0.0,0.5,0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Trustworthiness",position = "left", 
                     limits = c(0, 6), breaks = seq(0, 5.5, by = 1), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(rev(c("Company","Public interest group")))) +
  scale_fill_manual(values = c("1 board seat" = "#7570b3", "5 board seats" = "#d95f02")) +
  coord_flip()

figure4_bottomleft


# Top right (Electability by seat number when non-transparent)
figure4_topright <- ggplot() +
  geom_bar(data=lmmod2predict[which(lmmod2predict$dv == "Electability" & lmmod2predict$facet == "Non-transparent" & grepl("Company|Public", lmmod2predict$x)),],
           aes(x=x, y=predicted ,fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmod2predict[which(lmmod2predict$dv == "Electability" & lmmod2predict$facet == "Non-transparent" & grepl("Company|Public", lmmod2predict$x)),],
                aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  ggtitle(label = "") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="none", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0.0,0.5,0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = NULL,position = "right", 
                     limits = c(0, 6), breaks = seq(0, 5.5, by = 1), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(values = c("1 board seat" = "#7570b3", "5 board seats" = "#d95f02")) +
  coord_flip()

figure4_topright


# Bottom right (Electability by org_type when transparent)
figure4_bottomright <- ggplot() +
  geom_bar(data=lmmod2predict[which(lmmod2predict$dv == "Electability" & lmmod2predict$facet == "Transparent" & grepl("Company|Public", lmmod2predict$x)),],
           aes(x=x, y=predicted ,fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmod2predict[which(lmmod2predict$dv == "Electability" & lmmod2predict$facet == "Transparent" & grepl("Company|Public", lmmod2predict$x)),],
                aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  ggtitle(label = "") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="none", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0.0,0.5,0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Electability",position = "left", 
                     limits = c(0, 6), breaks = seq(0, 5.5, by = 1), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels = NULL) +
  scale_fill_manual(values = c("1 board seat" = "#7570b3", "5 board seats" = "#d95f02")) +
  coord_flip()

figure4_bottomright





# 5) Combined figure output
# Use one of your plots to extract the legend (assuming the legend is identical)
legend <- ggpubr::get_legend(figure4_topleft)

# Define a shared no-margin theme
no_margin_theme <- theme(
  legend.position = "none",
  plot.margin = margin(0, 5, 0, 0)  # top, right, bottom, left (in pts)
)


# Apply the theme to all plots
figure4_topleft <- figure4_topleft + no_margin_theme
figure4_topright <- figure4_topright + no_margin_theme
figure4_bottomleft <- figure4_bottomleft + no_margin_theme
figure4_bottomright <- figure4_bottomright + no_margin_theme


# Combine plots in 2x2 grid and align
plot_matrix <- plot_grid(
  figure4_topleft + theme(legend.position = "none"),
  figure4_topright + theme(legend.position = "none"),
  figure4_bottomleft + theme(legend.position = "none"),
  figure4_bottomright + theme(legend.position = "none"),
  ncol = 2,
  align = "hv",      # Align both horizontally and vertically
  axis = "tblr"      # Align axes on all sides (top, bottom, left, right)
)

# Combine the plot matrix with the shared legend
figure4 <- plot_grid(
  plot_matrix,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.1)  # Adjust to control relative space
)


figure4

# Export figure
completefilename <- paste(getwd(),"/Figure4_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 14, width = 20, units = "cm",res = 1200)
figure4
dev.off()


rm(figure4_topleft, figure4_topright, figure4_bottomleft, figure4_bottomright, legend, plot_matrix, no_margin_theme)





#########################################
### Figure 5 / Tables A9, A10 and A11 ###
#########################################

### Table A9 / top row of Figure 5

# 1) Model with interaction effects of citizens' income and transparency:
# Trust:
lmmodt4a <- lm(transp_trust ~ transp_vignette_income*income2  + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender+
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodt4a)


# Obtain the predicted values
lmmodt4apredict <- ggpredict(lmmodt4a, terms = c("income2", "transp_vignette_income"))

lmmodt4apredict$group = recode(lmmodt4apredict$group,
                               "IT"="Non-transparent",
                               "150"="150% of parliamentary salary",
                               "20"="20% of parliamentary salary",
                               "PB"="Pro bono")


figure5incometrust <- ggplot(lmmodt4apredict, 
                             aes(x = x, 
                                 y = predicted,
                                 group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(2.5, 7.5), breaks=seq(3,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' income level",position = "bottom", 
                     limits = c(1, 10), breaks=seq(1,10,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0))  +
  scale_color_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77"))


figure5incometrust




# Vote intention:
lmmodv4a <- lm(transp_vote ~ transp_vignette_income*income2  + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodv4a)


# Obtain the predicted values
lmmodv4apredict <- ggpredict(lmmodv4a, terms = c("income2", "transp_vignette_income"))

lmmodv4apredict$group = recode(lmmodv4apredict$group,
                               "IT"="Non-transparent",
                               "150"="150% of parliamentary salary",
                               "20"="20% of parliamentary salary",
                               "PB"="Pro bono")



figure5incomevote <- ggplot(lmmodv4apredict, 
                            aes(x = x, 
                                y = predicted,
                                group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.3,0.1,0.3), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(2.5, 7.5), breaks=seq(3,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' income level",position = "bottom", 
                     limits = c(1, 10), breaks=seq(1,10,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0))  +
  scale_color_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77"))


figure5incomevote



# Combined figure output
ggarrange(figure5incometrust, figure5incomevote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


# Underlying models for the appendix
stargazer(lmmodt4a,lmmodv4a, title="The effect of (non-)transparency by citizens' income",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-45pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$citizen: income",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$citizen: income",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$citizen: income",
                               "Constant"),
          notes.align = "l" # left alignment
)





### Table A10 / center row of Figure 5


# 1) Model with interaction effects of citizens' income and transparency:
# Trust:
lmmodt4b <- lm(transp_trust ~ transp_vignette_income*isced11  + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender+
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodt4b)


# Obtain the predicted values
lmmodt4bpredict <- ggpredict(lmmodt4b, terms = c("isced11", "transp_vignette_income"))

lmmodt4bpredict$group = recode(lmmodt4bpredict$group,
                               "IT"="Non-transparent",
                               "150"="150% of parliamentary salary",
                               "20"="20% of parliamentary salary",
                               "PB"="Pro bono")


figure5edutrust <- ggplot(lmmodt4bpredict, 
                          aes(x = x, 
                              y = predicted,
                              group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(2.5, 7.5), breaks=seq(3,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' education level",position = "bottom", 
                     limits = c(0, 8), breaks=seq(0,8,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0))  +
  scale_color_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77"))

figure5edutrust




# Vote intention:
lmmodv4b <- lm(transp_vote ~ transp_vignette_income*isced11  + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodv4b)


# Obtain the predicted values
lmmodv4bpredict <- ggpredict(lmmodv4b, terms = c("isced11", "transp_vignette_income"))

lmmodv4bpredict$group = recode(lmmodv4bpredict$group,
                               "IT"="Non-transparent",
                               "150"="150% of parliamentary salary",
                               "20"="20% of parliamentary salary",
                               "PB"="Pro bono")


# Obtain the predicted values
figure5eduvote <- ggplot(lmmodv4bpredict, 
                         aes(x = x, 
                             y = predicted,
                             group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.3,0.1,0.3), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(2.5, 7.5), breaks=seq(3,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' education level",position = "bottom", 
                     limits = c(0, 8), breaks=seq(0,8,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0))  +
  scale_color_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77"))


figure5eduvote



# Underlying models for the appendix
stargazer(lmmodt4b,lmmodv4b, title="The effect of (non-)transparency by citizens' education level",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-45pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$citizen: education level",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$citizen: education level",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$citizen: education level",
                               "Constant"),
          notes.align = "l" # left alignment
)



### Table A11 / bottom row of Figure 5


# 1) Model with interaction effects of citizens' income and transparency:
# Trust:
lmmodt4c <- lm(transp_trust ~ transp_vignette_income*left_right_1  + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender+
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodt4c)


# Obtain the predicted values
lmmodt4cpredict <- ggpredict(lmmodt4c, terms = c("left_right_1[0:10]", "transp_vignette_income"))

lmmodt4cpredict$group = recode(lmmodt4cpredict$group,
                               "IT"="Non-transparent",
                               "150"="150% of parliamentary salary",
                               "20"="20% of parliamentary salary",
                               "PB"="Pro bono")


figure5rightleantrust <- ggplot(lmmodt4cpredict, 
                                aes(x = x, 
                                    y = predicted,
                                    group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(2, 7.5), breaks=seq(2,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' right-leaningness",position = "bottom", 
                     limits = c(0, 10), breaks=seq(0,10,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0))  +
  scale_color_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77"))


figure5rightleantrust




# Vote intention:
lmmodv4c <- lm(transp_vote ~ transp_vignette_income*left_right_1  + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodv4c)


# Obtain the predicted values
lmmodv4cpredict <- ggpredict(lmmodv4c, terms = c("left_right_1[0:10]", "transp_vignette_income"))

lmmodv4cpredict$group = recode(lmmodv4cpredict$group,
                               "IT"="Non-transparent",
                               "150"="150% of parliamentary salary",
                               "20"="20% of parliamentary salary",
                               "PB"="Pro bono")


# Obtain the predicted values
figure5rightleanvote <- ggplot(lmmodv4cpredict, 
                               aes(x = x, 
                                   y = predicted,
                                   group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.3,0.1,0.3), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(2, 7.5), breaks=seq(2,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' right-leaningness",position = "bottom", 
                     limits = c(0, 10), breaks=seq(0,10,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0))  +
  scale_color_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#e7298a", "#d95f02", "#7570b3", "#1b9e77"))


figure5rightleanvote



# Combined figure output
ggarrange(figure5rightleantrust, figure5rightleanvote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


# Underlying models for the appendix
stargazer(lmmodt4c,lmmodv4c, title="The effect of (non-)transparency by Citizens' right-leaningness",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-55pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$citizen: right-leaning",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$citizen: right-leaning",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$citizen: right-leaning",
                               "Constant"),
          notes.align = "l" # left alignment
)






completefilename <- paste(getwd(),"/Figure5_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 30, width = 25, units = "cm",res = 1200)
ggarrange(figure5incometrust, figure5incomevote,
          figure5edutrust, figure5eduvote,
          figure5rightleantrust, figure5rightleanvote,
          ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
dev.off() 






##############################
##############################
### Supplementary Analyses ###
##############################
##############################

################
### Table A3 ###
################

# Descriptive statistis

st(SURVEY, 
   vars = c("transp_trust","transp_vote","transp_vignette_income","transp_vignette_orgtype","transp_vignette_seats","transp_vignette_gender","left_right_1","income2","isced11","gender","mp_respondent_gender","age","country"),
   labels = c("Trustworthiness","Electability", "MP: income transparency", "MP: type of side job(s)", "MP: number of side jobs", "MP: gender", "Citizen: right-leaning", "Citizen: income level", "Citizen: education level", "Citizen: gender", "Citizen & MP: gender homophily", "Age", "Country"),
   summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
   summ.names = c('N','Mean','Std. Dev.','Min','Max'),
   out = "latex")



##################
### Figure A15 ###
##################


# Bar charts of trustworthiness and vote intention per vignette

# 1) Work with a subset of the data
VIGNSCRS <- subset(SURVEY, select = c("transp_trust", "transp_vote", "transp_vignette"))

# 2) Recode transp_vignette to no longer include the gender differences
VIGNSCRS$transp_vignette <- gsub("^(F_|M_)", "", VIGNSCRS$transp_vignette)

# 3) Make sure the variables are numeric
VIGNSCRS$transp_trust <- as.numeric(VIGNSCRS$transp_trust)
VIGNSCRS$transp_vote <- as.numeric(VIGNSCRS$transp_vote)

# 4) For transp_trust and transp_vote: obtain mean and ci
alpha <- 0.05

VIGNSCRS %>% 
  group_by(transp_vignette) %>% 
  summarize(mean = mean(transp_trust, na.rm = T),
            lower = mean(transp_trust, na.rm = T) - qt(1- alpha/2, (n() - 1))*sd(transp_trust, na.rm = T)/sqrt(n()),
            upper = mean(transp_trust, na.rm = T) + qt(1- alpha/2, (n() - 1))*sd(transp_trust, na.rm = T)/sqrt(n())) -> VIGNTRUST


VIGNSCRS %>% 
  group_by(transp_vignette) %>% 
  summarize(mean = mean(transp_vote, na.rm = T),
            lower = mean(transp_vote, na.rm = T) - qt(1- alpha/2, (n() - 1))*sd(transp_vote, na.rm = T)/sqrt(n()),
            upper = mean(transp_vote, na.rm = T) + qt(1- alpha/2, (n() - 1))*sd(transp_vote, na.rm = T)/sqrt(n())) -> VIGNVOTE


# 5) Create grouping variable
VIGNTRUST$transp_vignette_income <- str_extract(VIGNTRUST$transp_vignette, "(IT|PB|20|150)")
VIGNVOTE$transp_vignette_income <- str_extract(VIGNVOTE$transp_vignette, "(IT|PB|20|150)")

VIGNTRUST$transp_vignette_income = recode(VIGNTRUST$transp_vignette_income,
                                          "IT"="Non-transparent",
                                          "150"="150% of parliamentary salary",
                                          "20"="20% of parliamentary salary",
                                          "PB"="Pro bono")

VIGNVOTE$transp_vignette_income = recode(VIGNVOTE$transp_vignette_income,
                                         "IT"="Non-transparent",
                                         "150"="150% of parliamentary salary",
                                         "20"="20% of parliamentary salary",
                                         "PB"="Pro bono")



# 6) Plotting

trustobserved <- ggplot(VIGNTRUST, aes(x=reorder(transp_vignette, desc(mean)), y=mean, fill=transp_vignette_income)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(x=reorder(transp_vignette, desc(mean)), y=mean, ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        #panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0,0), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Trustworthiness", 
                     limits = c(0, 6), breaks=seq(0,6,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6"),
                     expand = c(0, 0)) +
  #scale_x_discrete(name = "Trustworthiness") +
  scale_fill_manual("legend", values = c("Non-transparent" = "#fc3d03", "150% of parliamentary salary" = "#fca103", "20% of parliamentary salary" = "#fcfc03", "Pro bono" = "#11ab63")) +
  coord_flip()


voteobserved <- ggplot(VIGNVOTE, aes(x=reorder(transp_vignette, desc(mean)), y=mean, fill=transp_vignette_income)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(x=reorder(transp_vignette, desc(mean)), y=mean, ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        #panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0,0), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Electability", 
                     limits = c(0, 6), breaks=seq(0,6,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6"),
                     expand = c(0, 0)) +
  #scale_x_discrete(name = "Trustworthiness") +
  scale_fill_manual("legend", values = c("Non-transparent" = "#fc3d03", "150% of parliamentary salary" = "#fca103", "20% of parliamentary salary" = "#fcfc03", "Pro bono" = "#11ab63")) +
  coord_flip()

# 7) Exporting
completefilename <- paste(getwd(),"/FigureA15_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 12, width = 25, units = "cm",res = 1200)
ggarrange(trustobserved, voteobserved,
          ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off()



#############################
### Table A4 / Figure A16 ###
#############################

# Experimental design imbalances
# 1) Create a version of transp_vignette that does not include gender
SURVEY$transp_vignette_nogender <- gsub("^(F_|M_)", "", SURVEY$transp_vignette)
SURVEY %>% relocate(transp_vignette_nogender, .after=transp_vignette) -> SURVEY

SURVEY$transp_vignette_nogender <- relevel(as.factor(SURVEY$transp_vignette_nogender), ref = "IT_C5")


# 2) Model with only vignettes
# Trust:
lmmodt5a <- lm(transp_trust ~ transp_vignette_nogender+
                 left_right_1 + income2 + isced11 + gender + age + country,
               data=SURVEY)
summary(lmmodt5a)


# Obtain the predicted values
lmmodt5apredict <- ggpredict(lmmodt5a, terms = c("transp_vignette_nogender"))
lmmodt5apredict$group <- lmmodt5apredict$x

lmmodt5apredict$group <- gsub("P1|P5|C1|C5|_", "", lmmodt5apredict$group)

lmmodt5apredict$group <- recode(lmmodt5apredict$group,
                                "IT"="Non-transparent",
                                "T150"="150% of parliamentary salary",
                                "T20"="20% of parliamentary salary",
                                "TPB"="Pro bono")



figurea16vignettestrust <- ggplot(lmmodt5apredict, aes(x=reorder(x, desc(predicted)), y=predicted, fill=group)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(x=reorder(x, desc(predicted)), y=predicted, ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        #panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0,0), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Trustworthiness", 
                     limits = c(0, 6), breaks=seq(0,6,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6"),
                     expand = c(0, 0)) +
  #scale_x_discrete(name = "Trustworthiness") +
  scale_fill_manual("legend", values = c("Non-transparent" = "#fc3d03", "150% of parliamentary salary" = "#fca103", "20% of parliamentary salary" = "#fcfc03", "Pro bono" = "#11ab63")) +
  coord_flip()




figurea16vignettestrust



# Vote intention:
lmmodv5a <- lm(transp_vote ~ transp_vignette_nogender+
                 left_right_1 + income2 + isced11 + gender + age + country,
               data=SURVEY)
summary(lmmodv5a)


# Obtain the predicted values
lmmodv5apredict <- ggpredict(lmmodv5a, terms = c("transp_vignette_nogender"))
lmmodv5apredict$group <- lmmodv5apredict$x


lmmodv5apredict$group <- gsub("P1|P5|C1|C5|_", "", lmmodv5apredict$group)

lmmodv5apredict$group <- recode(lmmodv5apredict$group,
                                "IT"="Non-transparent",
                                "T150"="150% of parliamentary salary",
                                "T20"="20% of parliamentary salary",
                                "TPB"="Pro bono")



figurea16vignettesvote <- ggplot(lmmodv5apredict, aes(x=reorder(x, desc(predicted)), y=predicted, fill=group)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(x=reorder(x, desc(predicted)), y=predicted, ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        #panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0,0), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Electability", 
                     limits = c(0, 6), breaks=seq(0,6,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6"),
                     expand = c(0, 0)) +
  #scale_x_discrete(name = "Trustworthiness") +
  scale_fill_manual("legend", values = c("Non-transparent" = "#fc3d03", "150% of parliamentary salary" = "#fca103", "20% of parliamentary salary" = "#fcfc03", "Pro bono" = "#11ab63")) +
  coord_flip()




figurea16vignettesvote





# Combined figure output
ggarrange(figurea16vignettestrust, figurea16vignettesvote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

completefilename <- paste(getwd(),"/FigureA16_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 12, width = 25, units = "cm",res = 1200)
ggarrange(figurea16vignettestrust, figurea16vignettesvote,
          ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off() 




# Underlying models for the appendix (Table A4)
stargazer(lmmodt5a,lmmodv5a, title="The effect of (non-)transparency by organisational type",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-45pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: Non-transparent, 1 company board seat\\textsuperscript{1}",
                               "MP: Non-transparent, 1 public IG board seat\\textsuperscript{1}",
                               "MP: Non-transparent, 5 public IG board seats\\textsuperscript{1}",
                               "MP: 150\\% side income, 1 company board seat\\textsuperscript{1}",
                               "MP: 20\\% side income, 1 company board seat\\textsuperscript{1}",
                               "MP: 150\\% side income, 5 company board seats\\textsuperscript{1}",
                               "MP: 150\\% side income, 1 public IG board seat\\textsuperscript{1}",
                               "MP: 20\\% side income, 1 public IG board seat\\textsuperscript{1}",
                               "MP: Pro bono, 1 public IG board seat\\textsuperscript{1}",
                               "MP: 150\\% side income, 5 public IG board seats\\textsuperscript{1}",
                               "MP: Pro bono, 5 public IG board seats\\textsuperscript{1}",
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l") # left alignment



# Where do the 95% confidence intervals overlap?

# Trustworthiness:

# Function to check intersection
check_intersection <- function(row1, row2) {
  return(!(row1['conf.high'] < row2['conf.low'] || row2['conf.high'] < row1['conf.low']))
}

# Convert conf.low and conf.high columns to numeric to avoid comparison issues
TRUST <- as.data.frame(lmmodt5apredict)

TRUST$conf.low <- as.numeric(as.character(TRUST$conf.low))
TRUST$conf.high <- as.numeric(as.character(TRUST$conf.high))
TRUST$x <- as.character(TRUST$x)
TRUST$seat <- str_extract(TRUST$x, "C1|C5|P1|P5")
TRUST$income <- str_extract(TRUST$x, "IT|20|150|PB")

TRUST$income = recode(TRUST$income,
                      "IT"="NT",
                      "20"="T_20",
                      "150"="T_150",
                      "PB"="T_PB"
)


TRUST <- TRUST[order(TRUST$seat, TRUST$income),]

# Initialize the result matrix
CIOVERLAPTRUST <- as.data.frame(matrix(FALSE, nrow = nrow(TRUST), ncol = nrow(TRUST)))
rownames(CIOVERLAPTRUST) <- TRUST$x
colnames(CIOVERLAPTRUST) <- TRUST$x

# Populate the matrix
for (i in 1:nrow(TRUST)) {
  for (j in 1:nrow(TRUST)) {
    CIOVERLAPTRUST[i, j] <- check_intersection(TRUST[i,], TRUST[j,])
  }
}

rm(TRUST)


# Electability:


# Convert conf.low and conf.high columns to numeric to avoid comparison issues
VOTE <- as.data.frame(lmmodv5apredict)

VOTE$conf.low <- as.numeric(as.character(VOTE$conf.low))
VOTE$conf.high <- as.numeric(as.character(VOTE$conf.high))
VOTE$x <- as.character(VOTE$x)
VOTE$seat <- str_extract(VOTE$x, "C1|C5|P1|P5")
VOTE$income <- str_extract(VOTE$x, "IT|20|150|PB")

VOTE$income = recode(VOTE$income,
                     "IT"="NT",
                     "20"="T_20",
                     "150"="T_150",
                     "PB"="T_PB"
)


VOTE <- VOTE[order(VOTE$seat, VOTE$income),]

# Initialize the result matrix
CIOVERLAPVOTE <- as.data.frame(matrix(FALSE, nrow = nrow(VOTE), ncol = nrow(VOTE)))
rownames(CIOVERLAPVOTE) <- VOTE$x
colnames(CIOVERLAPVOTE) <- VOTE$x

# Populate the matrix
for (i in 1:nrow(VOTE)) {
  for (j in 1:nrow(VOTE)) {
    CIOVERLAPVOTE[i, j] <- check_intersection(VOTE[i,], VOTE[j,])
  }
}

rm(VOTE)


################
### Table A5 ###
################

# Question: Are our key treatments correlated with certain groups of citizens?
# Solution: Run a multinomial regression model to see whether there is a relationship between citizens' characteristics and assignemnt to our key treatment
balance = multinom(transp_vignette_income ~ left_right_1 + income2 + isced11 + gender + age + country, data=SURVEY)
summary(balance)

# Outputting the balance test with stargazer
stargazer(balance, title="Respondents' characteristics as predictors of treatment assignment",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Transparency status",
          dep.var.labels   = c("150\\% side income", "20\\% side income", "Pro bono"),
          #omit = c("country"),
          #omit.labels = c("Country"),
          #omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen: age",
                               "Belgium",
                               "Germany",
                               "United Kingdom",
                               "France",
                               "Poland",
                               "Netherlands",
                               "Constant"),
          notes.align = "l" # left alignment
)





##############################
### Figure A17 / Table A12 ###
##############################

# Perception Differences Based on MPs' Gender


### Trustworthiness - Non-Transparent


# Plot the means for trustworthiness for IT, 150, 20, and PB based on the politician's gender

# 1) Trustworthiness
a <- SURVEY[which(grepl("IT", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_trust
b <- SURVEY[which(grepl("IT", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_trust

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Male politician, non-transparent vs female politician, non-transparent"
DICHOTEST$dv <- "trustworthiness"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTS1 <- DICHOTEST
rm(a,b,result, DICHOTEST)


# 2) Store means in 1 instead of 2 columns
# a) Empty df
MEANS1 <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("category", "dv", "mean"))

# b) Store values
MEANS1[1,1] <- "Male MP, non-transparent"
MEANS1[1,2] <- "trustworthiness"
MEANS1[1,3] <- mean(SURVEY[which(grepl("IT", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_trust, na.rm = TRUE)

MEANS1[2,1] <- "Female MP, non-transparent"
MEANS1[2,2] <- "trustworthiness"
MEANS1[2,3] <- mean(SURVEY[which(grepl("IT", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_trust, na.rm = TRUE)


# 3) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTS1, select = c("comparison", "dv", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", "")))


# 5) Order the variables' factor levels according to row order
MEANS1 <- mutate(MEANS1, category = factor(MEANS1$category, c("Female MP, non-transparent", "Male MP, non-transparent")))


# 6) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot


figurea17nontranspgendertrust <- ggplot() +
  geom_bar(data=MEANS1, aes(x=category, y=mean),stat="identity", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        # axis.text.x = element_text(colour = "black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0.5,0,0.3), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(#name = "Trustworthiness",position = "right", 
    limits = c(0, 7), breaks=seq(0,7,by = .5), 
    # labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
    expand = c(0, 0)) +
  scale_x_discrete(labels = rev(c("Male MP,\n non-transparent", "Female MP,\n non-transparent"))) +
  coord_flip() +
  ## For the significance tests (IT vs 150)
  annotate("segment", x=1, xend=2,y=MEANS1$mean[2]+0.3, yend=MEANS1$mean[2]+0.3, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1, xend=1,y=MEANS1$mean[2]+0.1, yend=MEANS1$mean[2]+0.3, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2, xend=2,y=MEANS1$mean[1]+0.1, yend=MEANS1$mean[2]+0.3, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANS1$mean[2]+0.5, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE)  # hjust = 0 for left allignment of text

figurea17nontranspgendertrust




### Trustworthiness - 150% side income


# Plot the means for trustworthiness for IT, 150, 20, and PB based on the politician's gender

# 1) Trustworthiness
a <- SURVEY[which(grepl("150", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_trust
b <- SURVEY[which(grepl("150", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_trust

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Male politician, 150% side income vs female politician, 150% side income"
DICHOTEST$dv <- "trustworthiness"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTS2 <- DICHOTEST
rm(a,b,result, DICHOTEST)


# 2) Store means in 1 instead of 2 columns
# a) Empty df
MEANS2 <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("category", "dv", "mean"))

# b) Store values
MEANS2[1,1] <- "Male MP, 150% side income"
MEANS2[1,2] <- "trustworthiness"
MEANS2[1,3] <- mean(SURVEY[which(grepl("150", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_trust, na.rm = TRUE)

MEANS2[2,1] <- "Female MP, 150% side income"
MEANS2[2,2] <- "trustworthiness"
MEANS2[2,3] <- mean(SURVEY[which(grepl("150", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_trust, na.rm = TRUE)


# 3) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTS2, select = c("comparison", "dv", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", ifelse(STARS$pvalue>=0.05, STARS$stars <- "n.s.", ""))))


# 5) Order the variables' factor levels according to row order
MEANS2 <- mutate(MEANS2, category = factor(MEANS2$category, c("Female MP, 150% side income", "Male MP, 150% side income")))


# 6) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot


figurea17income150gendertrust <- ggplot() +
  geom_bar(data=MEANS2, aes(x=category, y=mean),stat="identity", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        # axis.text.x = element_text(colour = "black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0.5,0,0.1), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Trustworthiness",position = "right", 
                     limits = c(0, 7), breaks=seq(0,7,by = .5), 
                     labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = rev(c("Male MP,\n 150% side income", "Female MP,\n 150% side income")))+
  coord_flip() +
  ## For the significance tests (IT vs 150)
  annotate("segment", x=1, xend=2,y=MEANS2$mean[2]+0.3, yend=MEANS2$mean[2]+0.3, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1, xend=1,y=MEANS2$mean[2]+0.1, yend=MEANS2$mean[2]+0.3, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2, xend=2,y=MEANS2$mean[1]+0.1, yend=MEANS2$mean[2]+0.3, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANS2$mean[2]+0.5, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE)  # hjust = 0 for left allignment of text

figurea17income150gendertrust




### Trustworthiness - 20% side income


# Plot the means for trustworthiness for IT, 150, 20, and PB based on the politician's gender

# 1) Trustworthiness
a <- SURVEY[which(grepl("20", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_trust
b <- SURVEY[which(grepl("20", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_trust

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Male politician, 20% side income vs female politician, 20% side income"
DICHOTEST$dv <- "trustworthiness"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTS3 <- DICHOTEST
rm(a,b,result, DICHOTEST)


# 2) Store means in 1 instead of 2 columns
# a) Empty df
MEANS3 <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("category", "dv", "mean"))

# b) Store values
MEANS3[1,1] <- "Male MP, 20% side income"
MEANS3[1,2] <- "trustworthiness"
MEANS3[1,3] <- mean(SURVEY[which(grepl("20", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_trust, na.rm = TRUE)

MEANS3[2,1] <- "Female MP, 20% side income"
MEANS3[2,2] <- "trustworthiness"
MEANS3[2,3] <- mean(SURVEY[which(grepl("20", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_trust, na.rm = TRUE)


# 3) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTS3, select = c("comparison", "dv", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", ifelse(STARS$pvalue>=0.05, STARS$stars <- "n.s.", ""))))


# 5) Order the variables' factor levels according to row order
MEANS3 <- mutate(MEANS3, category = factor(MEANS3$category, c("Female MP, 20% side income", "Male MP, 20% side income")))


# 6) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot


figurea17income20gendertrust <- ggplot() +
  geom_bar(data=MEANS3, aes(x=category, y=mean),stat="identity", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        # axis.text.x = element_text(colour = "black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0.5,0,0.1), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Trustworthiness",position = "right", 
                     limits = c(0, 7), breaks=seq(0,7,by = .5), 
                     labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = rev(c("Male MP,\n 20% side income", "Female MP,\n 20% side income")))+
  coord_flip() +
  ## For the significance tests (IT vs 150)
  annotate("segment", x=1, xend=2,y=MEANS3$mean[2]+0.3, yend=MEANS3$mean[2]+0.3, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1, xend=1,y=MEANS3$mean[2]+0.1, yend=MEANS3$mean[2]+0.3, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2, xend=2,y=MEANS3$mean[1]+0.1, yend=MEANS3$mean[2]+0.3, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANS3$mean[2]+0.5, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE)  # hjust = 0 for left allignment of text

figurea17income20gendertrust




### Trustworthiness - pro bono

# Plot the means for trustworthiness for IT, 150, 20, and PB based on the politician's gender

# 1) Trustworthiness
a <- SURVEY[which(grepl("PB", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_trust
b <- SURVEY[which(grepl("PB", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_trust

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Male politician, pro bono vs female politician, pro bono"
DICHOTEST$dv <- "trustworthiness"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTS4 <- DICHOTEST
rm(a,b,result, DICHOTEST)


# 2) Store means in 1 instead of 2 columns
# a) Empty df
MEANS4 <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("category", "dv", "mean"))

# b) Store values
MEANS4[1,1] <- "Male MP, pro bono"
MEANS4[1,2] <- "trustworthiness"
MEANS4[1,3] <- mean(SURVEY[which(grepl("PB", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_trust, na.rm = TRUE)

MEANS4[2,1] <- "Female MP, pro bono"
MEANS4[2,2] <- "trustworthiness"
MEANS4[2,3] <- mean(SURVEY[which(grepl("PB", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_trust, na.rm = TRUE)


# 3) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTS4, select = c("comparison", "dv", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", ifelse(STARS$pvalue>=0.05, STARS$stars <- "n.s.", ""))))


# 5) Order the variables' factor levels according to row order
MEANS4 <- mutate(MEANS4, category = factor(MEANS4$category, c("Female MP, pro bono", "Male MP, pro bono")))


# 6) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot


figurea17probonogendertrust <- ggplot() +
  geom_bar(data=MEANS4, aes(x=category, y=mean),stat="identity", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Trustworthiness",position = "left", 
                     limits = c(0, 7), breaks=seq(0,7,by = .5), 
                     labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = rev(c("Male MP,\n pro bono", "Female MP,\n pro bono")))+
  coord_flip() +
  ## For the significance tests (IT vs 150)
  annotate("segment", x=1, xend=2,y=MEANS4$mean[2]+0.3, yend=MEANS4$mean[2]+0.3, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1, xend=1,y=MEANS4$mean[2]+0.1, yend=MEANS4$mean[2]+0.3, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2, xend=2,y=MEANS4$mean[1]+0.1, yend=MEANS4$mean[2]+0.3, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANS4$mean[2]+0.5, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE)  # hjust = 0 for left allignment of text

figurea17probonogendertrust







### Vote intention - Non-Transparent


# Plot the means for electability for IT, 150, 20, and PB based on the politician's gender

# 1) Electability
a <- SURVEY[which(grepl("IT", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_vote
b <- SURVEY[which(grepl("IT", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_vote

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Male politician, non-transparent vs female politician, non-transparent"
DICHOTEST$dv <- "electability"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTS5 <- DICHOTEST
rm(a,b,result, DICHOTEST)


# 2) Store means in 1 instead of 2 columns
# a) Empty df
MEANS5 <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("category", "dv", "mean"))

# b) Store values
MEANS5[1,1] <- "Male MP, non-transparent"
MEANS5[1,2] <- "electability"
MEANS5[1,3] <- mean(SURVEY[which(grepl("IT", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_vote, na.rm = TRUE)

MEANS5[2,1] <- "Female MP, non-transparent"
MEANS5[2,2] <- "electability"
MEANS5[2,3] <- mean(SURVEY[which(grepl("IT", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_vote, na.rm = TRUE)


# 3) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTS5, select = c("comparison", "dv", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", ifelse(STARS$pvalue>=0.05, STARS$stars <- "n.s.", ""))))


# 5) Order the variables' factor levels according to row order
MEANS5 <- mutate(MEANS5, category = factor(MEANS5$category, c("Female MP, non-transparent", "Male MP, non-transparent")))


# 6) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot


figurea17nontranspgendervote <- ggplot() +
  geom_bar(data=MEANS5, aes(x=category, y=mean),stat="identity", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text.y = element_text(colour = "black"),
        axis.text.y = element_blank(),
        # axis.text.x = element_text(colour = "black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0.1,0,0.7), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(#name = "Electability",position = "right", 
    limits = c(0, 7), breaks=seq(0,7,by = .5), 
    # labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
    expand = c(0, 0)) +
  scale_x_discrete(labels = rev(c("Male MP,\n non-transparent", "Female MP,\n non-transparent"))) +
  coord_flip() +
  ## For the significance tests (IT vs 150)
  annotate("segment", x=1, xend=2,y=MEANS5$mean[2]+0.3, yend=MEANS5$mean[2]+0.3, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1, xend=1,y=MEANS5$mean[2]+0.1, yend=MEANS5$mean[2]+0.3, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2, xend=2,y=MEANS5$mean[1]+0.1, yend=MEANS5$mean[2]+0.3, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANS5$mean[2]+0.5, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE)  # hjust = 0 for left allignment of text

figurea17nontranspgendervote




### Vote intention - 150% side income


# Plot the means for electability for IT, 150, 20, and PB based on the politician's gender

# 1) Electability
a <- SURVEY[which(grepl("150", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_vote
b <- SURVEY[which(grepl("150", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_vote

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Male politician, 150% side income vs female politician, 150% side income"
DICHOTEST$dv <- "electability"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTS6 <- DICHOTEST
rm(a,b,result, DICHOTEST)


# 2) Store means in 1 instead of 2 columns
# a) Empty df
MEANS6 <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("category", "dv", "mean"))

# b) Store values
MEANS6[1,1] <- "Male MP, 150% side income"
MEANS6[1,2] <- "electability"
MEANS6[1,3] <- mean(SURVEY[which(grepl("150", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_vote, na.rm = TRUE)

MEANS6[2,1] <- "Female MP, 150% side income"
MEANS6[2,2] <- "electability"
MEANS6[2,3] <- mean(SURVEY[which(grepl("150", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_vote, na.rm = TRUE)


# 3) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTS6, select = c("comparison", "dv", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", ifelse(STARS$pvalue>=0.05, STARS$stars <- "n.s.", ""))))


# 5) Order the variables' factor levels according to row order
MEANS6 <- mutate(MEANS6, category = factor(MEANS6$category, c("Female MP, 150% side income", "Male MP, 150% side income")))


# 6) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot


figurea17income150gendervote <- ggplot() +
  geom_bar(data=MEANS6, aes(x=category, y=mean),stat="identity", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text.y = element_text(colour = "black"),
        axis.text.y = element_blank(),
        # axis.text.x = element_text(colour = "black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0.1,0,0.7), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Electability",position = "right", 
                     limits = c(0, 7), breaks=seq(0,7,by = .5), 
                     labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = rev(c("Male MP,\n 150% side income", "Female MP,\n 150% side income"))) +
  coord_flip() +
  ## For the significance tests (IT vs 150)
  annotate("segment", x=1, xend=2,y=MEANS6$mean[2]+0.3, yend=MEANS6$mean[2]+0.3, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1, xend=1,y=MEANS6$mean[2]+0.1, yend=MEANS6$mean[2]+0.3, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2, xend=2,y=MEANS6$mean[1]+0.1, yend=MEANS6$mean[2]+0.3, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANS6$mean[2]+0.5, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE)  # hjust = 0 for left allignment of text

figurea17income150gendervote




### Vote intention - 20% side income


# Plot the means for electability for IT, 150, 20, and PB based on the politician's gender

# 1) Electability
a <- SURVEY[which(grepl("20", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_vote
b <- SURVEY[which(grepl("20", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_vote

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Male politician, 20% side income vs female politician, 20% side income"
DICHOTEST$dv <- "electability"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTS7 <- DICHOTEST
rm(a,b,result, DICHOTEST)


# 2) Store means in 1 instead of 2 columns
# a) Empty df
MEANS7 <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("category", "dv", "mean"))

# b) Store values
MEANS7[1,1] <- "Male MP, 20% side income"
MEANS7[1,2] <- "electability"
MEANS7[1,3] <- mean(SURVEY[which(grepl("20", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_vote, na.rm = TRUE)

MEANS7[2,1] <- "Female MP, 20% side income"
MEANS7[2,2] <- "electability"
MEANS7[2,3] <- mean(SURVEY[which(grepl("20", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_vote, na.rm = TRUE)


# 3) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTS7, select = c("comparison", "dv", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", ifelse(STARS$pvalue>=0.05, STARS$stars <- "n.s.", ""))))


# 5) Order the variables' factor levels according to row order
MEANS7 <- mutate(MEANS7, category = factor(MEANS7$category, c("Female MP, 20% side income", "Male MP, 20% side income")))


# 6) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot


figurea17income20gendervote <- ggplot() +
  geom_bar(data=MEANS7, aes(x=category, y=mean),stat="identity", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text.y = element_text(colour = "black"),
        axis.text.y = element_blank(),
        # axis.text.x = element_text(colour = "black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0.1,0,0.7), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Electability",position = "right", 
                     limits = c(0, 7), breaks=seq(0,7,by = .5), 
                     labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = rev(c("Male MP,\n 20% side income", "Female MP,\n 20% side income"))) +
  coord_flip() +
  ## For the significance tests (IT vs 150)
  annotate("segment", x=1, xend=2,y=MEANS7$mean[2]+0.3, yend=MEANS7$mean[2]+0.3, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1, xend=1,y=MEANS7$mean[2]+0.1, yend=MEANS7$mean[2]+0.3, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2, xend=2,y=MEANS7$mean[1]+0.1, yend=MEANS7$mean[2]+0.3, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANS7$mean[2]+0.5, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE)  # hjust = 0 for left allignment of text

figurea17income20gendervote




### Vote intention - pro bono

# Plot the means for electability for IT, 150, 20, and PB based on the politician's gender

# 1) Electability
a <- SURVEY[which(grepl("PB", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_vote
b <- SURVEY[which(grepl("PB", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_vote

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Male politician, pro bono vs female politician, pro bono"
DICHOTEST$dv <- "electability"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTS8 <- DICHOTEST
rm(a,b,result, DICHOTEST)


# 2) Store means in 1 instead of 2 columns
# a) Empty df
MEANS8 <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("category", "dv", "mean"))

# b) Store values
MEANS8[1,1] <- "Male MP, pro bono"
MEANS8[1,2] <- "electability"
MEANS8[1,3] <- mean(SURVEY[which(grepl("PB", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "M"),]$transp_vote, na.rm = TRUE)

MEANS8[2,1] <- "Female MP, pro bono"
MEANS8[2,2] <- "electability"
MEANS8[2,3] <- mean(SURVEY[which(grepl("PB", SURVEY$transp_vignette_income) & SURVEY$transp_vignette_gender == "F"),]$transp_vote, na.rm = TRUE)


# 3) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTS8, select = c("comparison", "dv", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", ifelse(STARS$pvalue>=0.05, STARS$stars <- "n.s.", ""))))


# 5) Order the variables' factor levels according to row order
MEANS8 <- mutate(MEANS8, category = factor(MEANS8$category, c("Female MP, pro bono", "Male MP, pro bono")))


# 6) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot


figurea17probonogendervote <- ggplot() +
  geom_bar(data=MEANS8, aes(x=category, y=mean),stat="identity", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.text.y = element_text(colour = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black"),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0,0.1,0.1,0.7), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Electability",position = "left", 
                     limits = c(0, 7), breaks=seq(0,7,by = .5), 
                     labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = rev(c("Male MP,\n pro bono", "Female MP,\n pro bono")))+
  coord_flip() +
  ## For the significance tests (IT vs 150)
  annotate("segment", x=1, xend=2,y=MEANS8$mean[2]+0.3, yend=MEANS8$mean[2]+0.3, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1, xend=1,y=MEANS8$mean[2]+0.1, yend=MEANS8$mean[2]+0.3, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2, xend=2,y=MEANS8$mean[1]+0.1, yend=MEANS8$mean[2]+0.3, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANS8$mean[2]+0.5, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE)  # hjust = 0 for left allignment of text

figurea17probonogendervote







### Figure A17, combined



# Exporting the figure as one
grid.draw(cbind(rbind(ggplotGrob(figurea17nontranspgendertrust), ggplotGrob(figurea17income150gendertrust),
                      ggplotGrob(figurea17income20gendertrust), ggplotGrob(figurea17probonogendertrust), size="first"),
                rbind(ggplotGrob(figurea17nontranspgendervote), ggplotGrob(figurea17income150gendervote),
                      ggplotGrob(figurea17income20gendervote), ggplotGrob(figurea17probonogendervote), size="first"), size = "last"))


completefilename <- paste(getwd(),"/FigureA17_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 11.5, width = 29, units = "cm",res = 1200)
grid.draw(cbind(rbind(ggplotGrob(figurea17nontranspgendertrust), ggplotGrob(figurea17income150gendertrust),
                      ggplotGrob(figurea17income20gendertrust), ggplotGrob(figurea17probonogendertrust), size="first"),
                rbind(ggplotGrob(figurea17nontranspgendervote), ggplotGrob(figurea17income150gendervote),
                      ggplotGrob(figurea17income20gendervote), ggplotGrob(figurea17probonogendervote), size="first"), size = "first"))
dev.off() 


# Exporting the underlying data for the appendix
# a) Create df
TESTSFIGA17 <- smartbind(TESTS1, TESTS2, TESTS3, TESTS4, TESTS5, TESTS6, TESTS7, TESTS8)

# b) Change certain character vectors to numeric
TESTSFIGA17$mean1 <- as.numeric((TESTSFIGA17$mean1))
TESTSFIGA17$mean2 <- as.numeric((TESTSFIGA17$mean2))
TESTSFIGA17$t <- as.numeric((TESTSFIGA17$t))
TESTSFIGA17$pvalue <- as.numeric((TESTSFIGA17$pvalue))

# c) Drop the last two columns on the method (can be added as a footnote)
TESTSFIGA17$alternative <- NULL
TESTSFIGA17$method <- NULL

# d) Rename columns
colnames(TESTSFIGA17) <- c("Categories compared", "Variable", "Mean 1", "Mean 2", "t", "p-value")

# e) Fix the p-value column (Source: https://stackoverflow.com/questions/31551822/how-to-display-coefficients-in-scientific-notation-with-stargazer)
replace_numbers = function(x, low=0.01, high=1e3, digits = 3) {
  x.num = as.numeric(x)
  form = paste0('%.', digits, 'e')
  ifelse(
    (abs(x.num) >= low) & (abs(x.num) < high), 
    round(x.num, digits = digits), 
    sprintf(form, x.num) 
  )
}

TESTSFIGA17$`p-value` <- replace_numbers(TESTSFIGA17$`p-value`)

stargazer(TESTSFIGA17, digits=3, summary = FALSE, rownames=FALSE)


rm(TESTS1, TESTS2, TESTS3, TESTS4, TESTS5, TESTS6, TESTS7, TESTS8, MEANS1, MEANS2, MEANS3, MEANS4, MEANS5, MEANS6, MEANS7, MEANS8, STARS, completefilename)



##############################
### Figure A18 / Table A13 ###
##############################

# 1) Model with interaction effects of citizens' gender and transparency:
# Trust:
lmmodt6a <- lm(transp_trust ~ transp_vignette_income*gender + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender+
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodt6a)


# Obtain the predicted values
lmmodt6apredict <- ggpredict(lmmodt6a, terms = c("transp_vignette_income", "gender"))

lmmodt6apredict$x = recode(lmmodt6apredict$x,
                           "IT"="Non-transparent",
                           "150"="150% of parliamentary salary",
                           "20"="20% of parliamentary salary",
                           "PB"="Pro bono")

lmmodt6apredict$group = recode(lmmodt6apredict$group,
                               "m"="Male citizen",
                               "f"="Female citizen")




centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figurea18gendertrust <- ggplot() +
  geom_bar(data=lmmodt6apredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodt6apredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(0, 7.5), breaks=seq(0,7,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6","7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))

figurea18gendertrust




# Vote intention:
lmmodv6a <- lm(transp_vote ~ transp_vignette_income*gender + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodv6a)


# Obtain the predicted values
lmmodv6apredict <- ggpredict(lmmodv6a, terms = c("transp_vignette_income", "gender"))

lmmodv6apredict$x = recode(lmmodv6apredict$x,
                           "IT"="Non-transparent",
                           "150"="150% of parliamentary salary",
                           "20"="20% of parliamentary salary",
                           "PB"="Pro bono")

lmmodv6apredict$group = recode(lmmodv6apredict$group,
                               "m"="Male citizen",
                               "f"="Female citizen")



centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figurea18gendervote <- ggplot() +
  geom_bar(data=lmmodv6apredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodv6apredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(0, 7.5), breaks=seq(0,7,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6","7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))

figurea18gendervote





# Combined figure output
ggarrange(figurea18gendertrust, figurea18gendervote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

completefilename <- paste(getwd(),"/FigureA18_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 8.5, width = 25, units = "cm",res = 1200)
ggarrange(figurea18gendertrust, figurea18gendervote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off() 



##############################
### Figure A19 / Table A14 ###
##############################


# 1) Model with interaction effects of citizens' age and transparency:
# Trust:
lmmodt7a <- lm(transp_trust ~ transp_vignette_income*age  + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender+
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodt7a)


# Obtain the predicted values
lmmodt7apredict <- ggpredict(lmmodt7a, terms = c("age[18:80]", "transp_vignette_income"))

lmmodt7apredict$group = recode(lmmodt7apredict$group,
                               "IT"="Non-transparent",
                               "150"="150% of parliamentary salary",
                               "20"="20% of parliamentary salary",
                               "PB"="Pro bono")


figurea19agetrust <- ggplot(lmmodt7apredict, 
                            aes(x = x, 
                                y = predicted,
                                group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(2.5, 7.5), breaks=seq(3,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' age",position = "bottom", 
                     limits = c(18, 80), breaks=seq(20,80,by = 10), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) # +
#scale_fill_manual(name = "MP's transparency",
#                  values=c("red", "orange", "blue", "green"),
#                  labels = c("Non-transparent", "150% of parliamentary salary", "20% of parliamentary salary", "Pro bono"))

figurea19agetrust




# Vote intention:
lmmodv7a <- lm(transp_vote ~ transp_vignette_income*age  + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodv7a)


# Obtain the predicted values
lmmodv7apredict <- ggpredict(lmmodv7a, terms = c("age[18:80]", "transp_vignette_income"))

lmmodv7apredict$group = recode(lmmodv7apredict$group,
                               "IT"="Non-transparent",
                               "150"="150% of parliamentary salary",
                               "20"="20% of parliamentary salary",
                               "PB"="Pro bono")


# Obtain the predicted values
figurea19agevote <- ggplot(lmmodv7apredict, 
                           aes(x = x, 
                               y = predicted,
                               group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.3,0.1,0.3), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(2.5, 7.5), breaks=seq(3,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' age",position = "bottom", 
                     limits = c(18, 80), breaks=seq(20,80,by = 10), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) # +
#scale_fill_manual(name = "MP's transparency",
#                  values=c("red", "orange", "blue", "green"),
#                  labels = c("Non-transparent", "150% of parliamentary salary", "20% of parliamentary salary", "Pro bono"))

figurea19agevote



# Combined figure output
ggarrange(figurea19agetrust, figurea19agevote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


# Underlying models for the appendix
stargazer(lmmodt7a,lmmodv7a, title="The effect of (non-)transparency by citizens' age",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-45pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$citizen: age",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$citizen: age",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)

completefilename <- paste(getwd(),"/FigureA19_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 8.5, width = 25, units = "cm",res = 1200)
ggarrange(figurea19agetrust, figurea19agevote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off() 




##############################
### Figure A20 / Table A15 ###
##############################


# 1) Model with interaction effects of organisational type and transparency:
# Trust:
lmmodt8a <- lm(transp_trust ~ transp_vignette_income*transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender+
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodt8a)


# Obtain the predicted values
lmmodt8apredict <- ggpredict(lmmodt8a, terms = c("transp_vignette_income", "transp_vignette_orgtype"))

lmmodt8apredict$x = recode(lmmodt8apredict$x,
                           "IT"="Non-transparent",
                           "150"="150% of parliamentary salary",
                           "20"="20% of parliamentary salary",
                           "PB"="Pro bono")

lmmodt8apredict$group = recode(lmmodt8apredict$group,
                               "P"="Public interest group",
                               "C"="Company")




centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figurea20orgtypetrust <- ggplot() +
  geom_bar(data=lmmodt8apredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodt8apredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(0, 6.4), breaks=seq(0,6,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))

figurea20orgtypetrust




# Vote intention:
lmmodv8a <- lm(transp_vote ~ transp_vignette_income*transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodv8a)


# Obtain the predicted values
lmmodv8apredict <- ggpredict(lmmodv8a, terms = c("transp_vignette_income", "transp_vignette_orgtype"))

lmmodv8apredict$x = recode(lmmodv8apredict$x,
                           "IT"="Non-transparent",
                           "150"="150% of parliamentary salary",
                           "20"="20% of parliamentary salary",
                           "PB"="Pro bono")

lmmodv8apredict$group = recode(lmmodv8apredict$group,
                               "P"="Public interest group",
                               "C"="Company")


centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figurea20orgtypevote <- ggplot() +
  geom_bar(data=lmmodv8apredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodv8apredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(0, 6.4), breaks=seq(0,6,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))

figurea20orgtypevote





# Combined figure output
ggarrange(figurea20orgtypetrust, figurea20orgtypevote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

completefilename <- paste(getwd(),"/FigureA20_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 8.5, width = 25, units = "cm",res = 1200)
ggarrange(figurea20orgtypetrust, figurea20orgtypevote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off() 



# Underlying models for the appendix
stargazer(lmmodt8a,lmmodv8a, title="The effect of (non-)transparency by organisational type",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-45pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$work for company",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$work for company",
                               #  "MP: pro bono work\\textsuperscript{1}$\\\times$citizen: female",
                               "Constant"),
          notes.align = "l") # left alignment



##############################
### Figure A21 / Table A16 ###
##############################


# 1) Model with interaction effects of citizens' country and transparency:
# Trust:
lmmodta9 <- lm(transp_trust ~ transp_vignette_income*country + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender+
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodta9)


# Obtain the predicted values
lmmodta9predict <- ggpredict(lmmodta9, terms = c("transp_vignette_income", "country"))

lmmodta9predict$x = recode(lmmodta9predict$x,
                           "IT"="Non-transparent",
                           "150"="150% of parliamentary salary",
                           "20"="20% of parliamentary salary",
                           "PB"="Pro bono")

lmmodta9predict$group = recode(lmmodta9predict$group,
                               "EN"="UK")
lmmodta9predict$group <- factor(lmmodta9predict$group, levels = c("CH", "BE", "DE", "FR", "NL", "PL", "UK"))



centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figurea21countrytrust <- ggplot() +
  geom_bar(data=lmmodta9predict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  scale_fill_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d')) +
  geom_errorbar(data=lmmodta9predict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(0, 7.5), breaks=seq(0,7,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6","7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))+
  guides(fill = guide_legend(nrow = 1)) # Legend in one row


figurea21countrytrust









# Vote intention:
lmmodva9 <- lm(transp_vote ~ transp_vignette_income*country + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
               data=SURVEY)
summary(lmmodva9)


# Obtain the predicted values
lmmodva9predict <- ggpredict(lmmodva9, terms = c("transp_vignette_income", "country"))

lmmodva9predict$x = recode(lmmodva9predict$x,
                           "IT"="Non-transparent",
                           "150"="150% of parliamentary salary",
                           "20"="20% of parliamentary salary",
                           "PB"="Pro bono")

lmmodva9predict$group = recode(lmmodva9predict$group,
                               "EN"="UK")

lmmodva9predict$group <- factor(lmmodva9predict$group, levels = c("CH", "BE", "DE", "FR", "NL", "PL", "UK"))


centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figurea21countryvote <- ggplot() +
  geom_bar(data=lmmodva9predict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  scale_fill_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d')) +
  geom_errorbar(data=lmmodva9predict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(0, 7.5), breaks=seq(0,7,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6","7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))+
  guides(fill = guide_legend(nrow = 1)) # Legend in one row

figurea21countryvote





# Combined figure output
ggarrange(figurea21countrytrust, figurea21countryvote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

completefilename <- paste(getwd(),"/FigureA21_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 8.5, width = 25, units = "cm",res = 1200)
ggarrange(figurea21countrytrust, figurea21countryvote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off() 



# Underlying models for the appendix
stargazer(lmmodta9,lmmodva9, title="The effect of (non-)transparency by country",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-45pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          #omit = c("country"),
          #omit.labels = c("Country"),
          #omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Belgium\\textsuperscript{4}",
                               "Germany\\textsuperscript{4}",
                               "France\\textsuperscript{4}",
                               "Netherlands\\textsuperscript{4}",
                               "Poland\\textsuperscript{4}",
                               "United Kingdom\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Belgium\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Belgium\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Belgium\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Germany\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Germany\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Germany\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$France\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$France\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$France\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Netherlands\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Netherlands\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Netherlands\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Poland\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Poland\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Poland\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$United Kingdom\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$United Kingdom\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$United Kingdom\\textsuperscript{4}",
                               "Constant"),
          notes.align = "l") # left alignment



#################
### Table A17 ###
#################


# 1) Model with categories of income transparency without France (replication of Table A6):
# Trust:
lmmodt1nofr <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                    left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                  data=SURVEY[which(SURVEY$country != "FR"),])
summary(lmmodt1nofr)

# Vote intention:
lmmodv1nofr <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                    left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                  data=SURVEY[which(SURVEY$country != "FR"),])
summary(lmmodv1nofr)


# Output the two models
stargazer(lmmodt1nofr,lmmodv1nofr, title="The effect of (non-)transparency on citizens' perceptions",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)



########################
### Tables A18 - A24 ###
########################

### Separate Models for all Countries

### Belgium ###

# 1) Model with categories of income transparency:
# Trust:
lmmodtBE <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "BE"),])
summary(lmmodtBE)

# Vote intention:
lmmodvBE <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "BE"),])
summary(lmmodvBE)


# Output the two models
stargazer(lmmodtBE,lmmodvBE, title="The effect of (non-)transparency in Belgium",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Respondents' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)




### France ###

# 1) Model with categories of income transparency:
# Trust:
lmmodtFR <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "FR"),])
summary(lmmodtFR)

# Vote intention:
lmmodvFR <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "FR"),])
summary(lmmodvFR)


# Output the two models
stargazer(lmmodtFR,lmmodvFR, title="The effect of (non-)transparency in France",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Respondents' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)



### Germany ###

# 1) Model with categories of income transparency:
# Trust:
lmmodtDE <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "DE"),])
summary(lmmodtDE)

# Vote intention:
lmmodvDE <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "DE"),])
summary(lmmodvDE)


# Output the two models
stargazer(lmmodtDE,lmmodvDE, title="The effect of (non-)transparency in Germany",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Respondents' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)





### Netherlands ###

# 1) Model with categories of income transparency:
# Trust:
lmmodtNL <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "NL"),])
summary(lmmodtNL)

# Vote intention:
lmmodvNL <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "NL"),])
summary(lmmodvNL)


# Output the two models
stargazer(lmmodtNL,lmmodvNL, title="The effect of (non-)transparency in the Netherlands",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Respondents' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)





### Poland ###

# 1) Model with categories of income transparency:
# Trust:
lmmodtPL <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "PL"),])
summary(lmmodtPL)

# Vote intention:
lmmodvPL <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "PL"),])
summary(lmmodvPL)


# Output the two models
stargazer(lmmodtPL,lmmodvPL, title="The effect of (non-)transparency in Poland",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Respondents' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)



### Switzerland ###

# 1) Model with categories of income transparency:
# Trust:
lmmodtCH <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "CH"),])
summary(lmmodtCH)

# Vote intention:
lmmodvCH <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "CH"),])
summary(lmmodvCH)


# Output the two models
stargazer(lmmodtCH,lmmodvCH, title="The effect of (non-)transparency in Switzerland",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Respondents' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)





### United Kingdom ###

# 1) Model with categories of income transparency:
# Trust:
lmmodtEN <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "EN"),])
summary(lmmodtEN)

# Vote intention:
lmmodvEN <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                 left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age,
               data=SURVEY[which(SURVEY$country == "EN"),])
summary(lmmodvEN)


# Output the two models
stargazer(lmmodtEN,lmmodvEN, title="The effect of (non-)transparency in the United Kingdom",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Respondents' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)



###################################
### Tables A25/A26 / Figure A22 ### 
###################################

# Twitter use of respondents

# The Twitter treatment
SURVEY$twitter_use <- as.factor(SURVEY$sm_use_2)
levels(SURVEY$twitter_use) <- c("At least once an hour","Several times a day","Once a day", "At least once a week", "Less than once a week", "Not at all/never")
SURVEY$twitter_use <- relevel(as.factor(SURVEY$twitter_use), ref = "Not at all/never")

st(SURVEY, 
   vars = c("twitter_use"),
   labels = c("Frequency of Twitter use"),
   summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
   summ.names = c('N','Mean','Std. Dev.','Min','Max'),
   out = "latex")


# 1) Model with categories of income transparency:
# Trust:
lmmodt1twitter <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender + twitter_use*transp_vignette_income +
                       left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                     data=SURVEY)
summary(lmmodt1twitter)

# Vote intention:
lmmodv1twitter <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender + twitter_use*transp_vignette_income +
                       left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                     data=SURVEY)
summary(lmmodv1twitter)


# Output the two models
stargazer(lmmodt1twitter,lmmodv1twitter, title="The effect of (non-)transparency on citizens' perceptions",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female",
                               "Citizen: Twitter used at least once an hour",
                               "Citizen: Twitter used several times a day",
                               "Citizen: Twitter used once a day",
                               "Citizen: Twitter used at least once a week",
                               "Citizen: Twitter used less than once a week ",
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Twitter: At least once an hour\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Twitter: At least once an hour\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Twitter: At least once an hour\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Twitter: Several times a day\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Twitter: Several times a day\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Twitter: Several times a day\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Twitter: Once a day\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Twitter: Once a day\\textsuperscript{4}", 
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Twitter: Once a day\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Twitter: At least once a week\\textsuperscript{4}", 
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Twitter: At least once a week\\textsuperscript{4}",
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Twitter: At least once a week\\textsuperscript{4}",
                               "MP: 150\\% side income\\textsuperscript{1}$\\\times$Twitter: Less than once a week\\textsuperscript{4}",
                               "MP: 20\\% side income\\textsuperscript{1}$\\\times$Twitter: Less than once a week\\textsuperscript{4}", 
                               "MP: pro bono work\\textsuperscript{1}$\\\times$Twitter: Less than once a week\\textsuperscript{4}",
                               "Constant"),
          notes.align = "l" # left alignment
)





# Plotting interaction effects of Twitter use: Trustworthiness

# Obtain the predicted values
lmmodt1twitterpredict <- ggpredict(lmmodt1twitter, terms = c("transp_vignette_income", "twitter_use"))

lmmodt1twitterpredict$x = recode(lmmodt1twitterpredict$x,
                                 "IT"="Non-transparent",
                                 "150"="150% of parliamentary salary",
                                 "20"="20% of parliamentary salary",
                                 "PB"="Pro bono")

#lmmodt1twitterpredict$group = recode(lmmodt1twitterpredict$group,
#                               "m"="Male citizen",
#                               "f"="Female citizen")

lmmodt1twitterpredict$group <- factor(lmmodt1twitterpredict$group, levels = c("At least once an hour","Several times a day","Once a day", "At least once a week", "Less than once a week", "Not at all/never"))


centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figuretwittertrust <- ggplot() +
  geom_bar(data=lmmodt1twitterpredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodt1twitterpredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(0, 7.5), breaks=seq(0,7,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6","7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))

figuretwittertrust



# Plotting interaction effects of Twitter use: Vote intention


# Obtain the predicted values
lmmodv1twitterpredict <- ggpredict(lmmodv1twitter, terms = c("transp_vignette_income", "twitter_use"))

lmmodv1twitterpredict$x = recode(lmmodv1twitterpredict$x,
                                 "IT"="Non-transparent",
                                 "150"="150% of parliamentary salary",
                                 "20"="20% of parliamentary salary",
                                 "PB"="Pro bono")

#lmmodv1twitterpredict$group = recode(lmmodv1twitterpredict$group,
#                               "m"="Male citizen",
#                               "f"="Female citizen")

lmmodv1twitterpredict$group <- factor(lmmodv1twitterpredict$group, levels = c("At least once an hour","Several times a day","Once a day", "At least once a week", "Less than once a week", "Not at all/never"))


centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figuretwittervote <- ggplot() +
  geom_bar(data=lmmodv1twitterpredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodv1twitterpredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(0, 7.5), breaks=seq(0,7,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5", "6","7"),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = centeralignlabel(c("Non-transparent","150% of parliamentary salary","20% of parliamentary salary","Pro bono")))

figuretwittervote



# Combined figure output
ggarrange(figuretwittertrust, figuretwittervote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

completefilename <- paste(getwd(),"/FigureA22_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 8.5, width = 25, units = "cm",res = 1200)
ggarrange(figuretwittertrust, figuretwittervote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off() 


######################
### Tables A27/A28 ### 
######################


# Inspection of dropped observations: is failure of the manipulation check
#                                     driven by certain sociodemographics?

## Preparation of data
# More stringent test: only individuals who didn't fail the knowledge question(s)
SURVEYMANI <- SURVEYSAVE

# Remove the few observations where we lack gender information
SURVEYMANI <- SURVEYMANI[which(SURVEYMANI$gender != "prefer not to say"),]

# Recode the transp_manicheck_istransp_correct variable to numeric
table(SURVEYMANI$transp_manicheck_istransp_correct)

SURVEYMANI$transp_manicheck_istransp_correct2 <- NA
SURVEYMANI[which(SURVEYMANI$transp_manicheck_istransp_correct == "correct"),]$transp_manicheck_istransp_correct2 <- 1
SURVEYMANI[which(SURVEYMANI$transp_manicheck_istransp_correct == "incorrect"),]$transp_manicheck_istransp_correct2 <- 0


# Run the analysis (full model)
logmanicheck <- glm(transp_manicheck_istransp_correct2 ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                      left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country, 
                    data = SURVEYMANI, family = binomial)

summary(logmanicheck)



# Output of the model
stargazer(logmanicheck, title="Manipulation check",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Correct recall of transparency status /0/1)"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)


# The reduced models (main analysis with reduced samples)
# Retain only those who got the transparency question right
SURVEYMANI <- SURVEYMANI[which(SURVEYMANI$transp_manicheck_istransp_correct == "correct"),]


# 1) Model with categories of income transparency:
# Trust:
lmmodt1red <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                   left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                 data=SURVEYMANI)
summary(lmmodt1red)

# Vote intention:
lmmodv1red <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype + transp_vignette_seats + transp_vignette_gender +
                   left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                 data=SURVEYMANI)
summary(lmmodv1red)


# Output the two models
stargazer(lmmodt1red,lmmodv1red, title="The effect of (non-)transparency on citizens' perceptions",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "Constant"),
          notes.align = "l" # left alignment
)



##############################
### Figure A23 / Table A29 ###
##############################


# 1) Model with interaction effects of citizens' ideology and interest group type:
# Trust:
lmmodt10a <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype*left_right_1 + transp_vignette_seats + transp_vignette_gender+
                  left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                data=SURVEY)
summary(lmmodt10a)


# Obtain the predicted values
lmmodt10apredict <- ggpredict(lmmodt10a, terms = c("left_right_1[0:10]", "transp_vignette_orgtype"))

lmmodt10apredict$group = recode(lmmodt10apredict$group,
                                "C"="Company",
                                "P"="Public interest group")


figurea26companyrightleantrust <- ggplot(lmmodt10apredict, 
                                         aes(x = x, 
                                             y = predicted,
                                             group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(2, 7.5), breaks=seq(2,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' right-leaningness",position = "bottom", 
                     limits = c(0, 10), breaks=seq(0,10,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0))

figurea26companyrightleantrust




# Vote intention:
lmmodv10a <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype*left_right_1 + transp_vignette_seats + transp_vignette_gender+
                  left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                data=SURVEY)
summary(lmmodv10a)


# Obtain the predicted values
lmmodv10apredict <- ggpredict(lmmodv10a, terms = c("left_right_1[0:10]", "transp_vignette_orgtype"))

lmmodv10apredict$group = recode(lmmodv10apredict$group,
                                "C"="Company",
                                "P"="Public interest group")


# Obtain the predicted values
figurea26companyrightleanvote <- ggplot(lmmodv10apredict, 
                                        aes(x = x, 
                                            y = predicted,
                                            group = group, fill = group, col = group))  +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2,
              linetype = 2) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.3,0.1,0.3), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(2, 7.5), breaks=seq(2,7,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Citizens' right-leaningness",position = "bottom", 
                     limits = c(0, 10), breaks=seq(0,10,by = 1), 
                     #    labels = c("0", "", "1", "", "2", "", "3", "", "4", "", "5", "", "6", "", "7"),
                     expand = c(0, 0)) 

figurea26companyrightleanvote



# Combined figure output
ggarrange(figurea26companyrightleantrust, figurea26companyrightleanvote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


# Underlying models for the appendix
stargazer(lmmodt10a,lmmodv10a, title="The effect of (non-)transparency by Citizens' right-leaningness",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-55pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          omit.yes.no = c("Controlled", "-"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}", "MP: 20\\% side income\\textsuperscript{1}", "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}", "MP: 5 board seats\\textsuperscript{3}", "MP: female", 
                               "Citizen: right-leaning", "Citizen: income", "Citizen: education level",
                               "Citizen: female",
                               "Citizen \\& MP: same gender",
                               "Citizen: age",
                               "MP: work for company\\textsuperscript{2}$\\\times$citizen: right-leaning",
                               "Constant"),
          notes.align = "l" # left alignment
)


completefilename <- paste(getwd(),"/FigureA23_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 8.5, width = 25, units = "cm",res = 1200)
ggarrange(figurea26companyrightleantrust, figurea26companyrightleanvote, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
dev.off() 






##############################
### Figure A24 / Table A30 ###
##############################


# a) Climate change as the most important issue

# Trust:
lmmodtclimchng <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype*mii_climchng + transp_vignette_seats + transp_vignette_gender + transp_vignette_income +
                       left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                     data=SURVEY)
summary(lmmodtclimchng)

# Vote intention:
lmmodvclimchng <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype*mii_climchng + transp_vignette_seats + transp_vignette_gender + transp_vignette_income +
                       left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                     data=SURVEY)
summary(lmmodvclimchng)



# b) Unemployment as the most important issue

# Trust:
lmmodtunemploy <- lm(transp_trust ~ transp_vignette_income + transp_vignette_orgtype*mii_unemploy + transp_vignette_seats + transp_vignette_gender + transp_vignette_income +
                       left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                     data=SURVEY)
summary(lmmodtunemploy)

# Vote intention:
lmmodvunemploy <- lm(transp_vote ~ transp_vignette_income + transp_vignette_orgtype*mii_unemploy + transp_vignette_seats + transp_vignette_gender + transp_vignette_income +
                       left_right_1 + income2 + isced11 + gender + mp_respondent_gender + age + country,
                     data=SURVEY)
summary(lmmodvunemploy)





# Output the two models
stargazer(lmmodtclimchng,lmmodvclimchng,lmmodtunemploy, lmmodvunemploy,
          title="The effect of (non-)transparency on citizens' perceptions",
          digits=3,
          single.row = TRUE, #SE in same row as coefficients
          column.sep.width = "-15pt",
          align=TRUE,
          type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.caption  = "Citizens' assessment of the fictitious MP",
          dep.var.labels   = c("Trustworthiness", "Electability", "Trustworthiness", "Electability"),
          omit = c("country"),
          omit.labels = c("Country"),
          covariate.labels = c("MP: 150\\% side income\\textsuperscript{1}",
                               "MP: 20\\% side income\\textsuperscript{1}",
                               "MP: pro bono work\\textsuperscript{1}",
                               "MP: work for company\\textsuperscript{2}",
                               "Citizen: climate change not important\\textsuperscript{4}",
                               "Citizen: climate change somewhat important\\textsuperscript{4}",
                               "Citizen: climate change very important\\textsuperscript{4}",
                               "Citizen: climate change extremely important\\textsuperscript{4}",
                               "Citizen: unemployment not important\\textsuperscript{4}",
                               "Citizen: unemployment somewhat important\\textsuperscript{4}",
                               "Citizen: unemployment very important\\textsuperscript{4}",
                               "Citizen: unemployment extremely important\\textsuperscript{4}",
                               "MP: 5 board seats\\textsuperscript{3}",
                               "Citizen: female",
                               "Citizen: right-leaning",
                               "Citizen: income",
                               "Citizen: education level",
                               "MP: female",
                               "MP: same gender",
                               "Citizen: age",
                               "MP: work for company\\textsuperscript{2}$\\\times$Citizen: climate change not important\\textsuperscript{4}",
                               "MP: work for company\\textsuperscript{2}$\\\times$Citizen: climate change somewhat important\\textsuperscript{4}",
                               "MP: work for company\\textsuperscript{2}$\\\times$Citizen: climate change very important\\textsuperscript{4}",
                               "MP: work for company\\textsuperscript{2}$\\\times$Citizen: climate change extremely important\\textsuperscript{4}",
                               "MP: work for company\\textsuperscript{2}$\\\times$Citizen: unemployment not important\\textsuperscript{4}",
                               "MP: work for company\\textsuperscript{2}$\\\times$Citizen: unemployment somewhat important\\textsuperscript{4}",
                               "MP: work for company\\textsuperscript{2}$\\\times$Citizen: unemployment very important\\textsuperscript{4}",
                               "MP: work for company\\textsuperscript{2}$\\\times$Citizen: unemployment extremely important\\textsuperscript{4}",
                               "Constant"),
          notes.align = "l" # left alignment
)


### Plotting

# 1) Climate change as the most important issue

# a) Trustworthiness

# Obtain the predicted values
lmmodtclimchngpredict <- ggpredict(lmmodtclimchng, terms = c("mii_climchng", "transp_vignette_orgtype"))

lmmodtclimchngpredict$group = recode(lmmodtclimchngpredict$group,
                                     "C"="Vignette with\ncompany(ies)",
                                     "P"="Vignette with\npublic interest group(s)")

lmmodtclimchngpredict$x = recode(lmmodtclimchngpredict$x,
                                 "Not important at all" = "Not important\nat all",
                                 "Not important" = "Not\nimportant",
                                 "Somewhat important" = "Somewhat\nimportant",
                                 "Very important" = "Very\nimportant",
                                 "Extremely important" = "Extremely\nimportant")

lmmodtclimchngpredict$x <- factor(lmmodtclimchngpredict$x, levels = c("Not important\nat all", "Not\nimportant", "Somewhat\nimportant", "Very\nimportant", "Extremely\nimportant"))




figureclimchngtrust <- ggplot() +
  geom_bar(data=lmmodtclimchngpredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodtclimchngpredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  ggtitle("Importance of climate change") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(0, 5.01), breaks=seq(0,5,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5"),
                     expand = c(0, 0))
figureclimchngtrust




# b) Electability

# Obtain the predicted values
lmmodvclimchngpredict <- ggpredict(lmmodvclimchng, terms = c("mii_climchng", "transp_vignette_orgtype"))

lmmodvclimchngpredict$group = recode(lmmodvclimchngpredict$group,
                                     "C"="Vignette with\ncompany(ies)",
                                     "P"="Vignette with\npublic interest group(s)")

lmmodvclimchngpredict$x = recode(lmmodvclimchngpredict$x,
                                 "Not important at all" = "Not important\nat all",
                                 "Not important" = "Not\nimportant",
                                 "Somewhat important" = "Somewhat\nimportant",
                                 "Very important" = "Very\nimportant",
                                 "Extremely important" = "Extremely\nimportant")

lmmodvclimchngpredict$x <- factor(lmmodvclimchngpredict$x, levels = c("Not important\nat all", "Not\nimportant", "Somewhat\nimportant", "Very\nimportant", "Extremely\nimportant"))



centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figureclimchngvote <- ggplot() +
  geom_bar(data=lmmodvclimchngpredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodvclimchngpredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  #ggtitle("Importance of climate change") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(0, 5.01), breaks=seq(0,5,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5"),
                     expand = c(0, 0))
figureclimchngvote




# 2) Unemployment as the most important issue

# a) Trustworthiness

# Obtain the predicted values
lmmodtunemploypredict <- ggpredict(lmmodtunemploy, terms = c("mii_unemploy", "transp_vignette_orgtype"))

lmmodtunemploypredict$group = recode(lmmodtunemploypredict$group,
                                     "C"="Vignette with\ncompany(ies)",
                                     "P"="Vignette with\npublic interest group(s)")

lmmodtunemploypredict$x = recode(lmmodtunemploypredict$x,
                                 "Not important at all" = "Not important\nat all",
                                 "Not important" = "Not\nimportant",
                                 "Somewhat important" = "Somewhat\nimportant",
                                 "Very important" = "Very\nimportant",
                                 "Extremely important" = "Extremely\nimportant")

lmmodtunemploypredict$x <- factor(lmmodtunemploypredict$x, levels = c("Not important\nat all", "Not\nimportant", "Somewhat\nimportant", "Very\nimportant", "Extremely\nimportant"))



figureunemploytrust <- ggplot() +
  geom_bar(data=lmmodtunemploypredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodtunemploypredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  ggtitle("Importance of unemployment") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted trustworthiness",position = "left", 
                     limits = c(0, 5.01), breaks=seq(0,5,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5"),
                     expand = c(0, 0))
figureunemploytrust




# b) Electability

# Obtain the predicted values
lmmodvunemploypredict <- ggpredict(lmmodvunemploy, terms = c("mii_unemploy", "transp_vignette_orgtype"))

lmmodvunemploypredict$group = recode(lmmodvunemploypredict$group,
                                     "C"="Vignette with\ncompany(ies)",
                                     "P"="Vignette with\npublic interest group(s)")

lmmodvunemploypredict$x = recode(lmmodvunemploypredict$x,
                                 "Not important at all" = "Not important\nat all",
                                 "Not important" = "Not\nimportant",
                                 "Somewhat important" = "Somewhat\nimportant",
                                 "Very important" = "Very\nimportant",
                                 "Extremely important" = "Extremely\nimportant")

lmmodvunemploypredict$x <- factor(lmmodvunemploypredict$x, levels = c("Not important\nat all", "Not\nimportant", "Somewhat\nimportant", "Very\nimportant", "Extremely\nimportant"))



figureunemployvote <- ggplot() +
  geom_bar(data=lmmodvunemploypredict, aes(x=x, y=predicted, fill = group),stat="identity", position="dodge") +
  geom_errorbar(data=lmmodvunemploypredict, aes(x=x, y=predicted, fill = group, ymax = conf.high, ymin=conf.low), position=position_dodge(width=0.9), width=0.25)+
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  #ggtitle("Importance of unemployment") +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        #panel.grid.major.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major = element_blank(),
        #panel.grid.minor.x = element_line(linewidth = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        panel.grid.minor.y = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.5,0.1,0.1), "cm"))  + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Predicted electability",position = "left", 
                     limits = c(0, 5.01), breaks=seq(0,5,by = 1), 
                     labels = c("0", "1", "2", "3", "4", "5"),
                     expand = c(0, 0))
figureunemployvote





# Make sure titles don't distort plot space by adding top margin (or removing titles)
figureclimchngtrust <- figureclimchngtrust +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))
figureclimchngvote <- figureclimchngvote +
  theme(plot.title = element_blank())
figureunemploytrust <- figureunemploytrust +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))
figureunemployvote <- figureunemployvote +
  theme(plot.title = element_blank())

# Create a blank spacer plot with no elements
spacer <- ggplot() + theme_void()

# Combine all four figures
ggarrange(
  figureclimchngtrust, figureclimchngvote,
  spacer, spacer,  # spacer row
  figureunemploytrust, figureunemployvote,
  ncol = 2, nrow = 3,
  heights = c(1, 0.2, 1),  # increase space by setting spacer height
  common.legend = TRUE,
  legend = "bottom",
  align = "hv"
)






completefilename <- paste(getwd(),"/FigureA24_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".png", sep = "")
png(completefilename,height = 17, width = 25, units = "cm",res = 1200)
ggarrange(
  figureclimchngtrust, figureclimchngvote,
  spacer, spacer,  # spacer row
  figureunemploytrust, figureunemployvote,
  ncol = 2, nrow = 3,
  heights = c(1, 0.2, 1),  # increase space by setting spacer height
  common.legend = TRUE,
  legend = "bottom",
  align = "hv"
)
dev.off()

