##  All options are on the table? Manuscript  ##
##  Rotem Dvir  ##
##  December 2020  ##
## R version 4.0.2 (June 2020) ##

# Set Working Directory
setwd("~/Dropbox/TAMU/Diss./Theory/Choice_set//RnR/Dataverse")

# Packages
library(foreign)
library(ggplot2)
library(gplots)
library(ggthemes)
library(plyr)
library(devtools)
library(dplyr)
library(ggpubr)
library(reshape2)

# Set Randomizer
set.seed(2020)

#################################################################################################################
###  Manuscript Figure 7: Preference reversal plot (values based on ANOVA repeated measure models (see SPSS output))
#################################################################################################################

# Create datasets of proportion selecting policy 1 in both combinations of choice-sets
dat.set <- data.frame(d.set=c("Sets A&B", "Sets C&D"), TwoOptionSet=c(0.436, 0.931), ThreeOptionSet=c(0.352, 0.463))
dat.set2 <- melt(dat.set, id.vars = 'd.set')

# Create barplot of final data (figure 7 in main text)
pref.rev.plot <- ggplot(dat.set2, aes(d.set, value)) + 
  geom_bar(aes(fill = variable), width = 0.5, position = position_dodge(width=0.5), stat="identity") +  
  xlab("") +
  ylab("Proportion Support for Policy 1") +
  ggtitle("Preference Reversal") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="bottom", legend.title = 
          element_blank(),axis.title.x=element_text(colour = "black", size = 10, face = "bold"), 
        axis.title.y=element_text(colour = "black", size = 10, face = "bold"),
        legend.background = element_rect(size = 0.5, linetype = "solid", colour = "darkblue"),
        panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', lineend = "butt",
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed', lineend = "butt",
                                        colour = "grey"),
        plot.title = element_text(size = 18, face = "bold.italic")) 

pref.rev.plot  

