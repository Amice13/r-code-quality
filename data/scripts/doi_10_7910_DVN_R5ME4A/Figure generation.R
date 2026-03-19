
rm(list = ls())

setwd("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets")

load("Survival modelling output.RData") #This includes updated content from "Condition modelling output.RData"
load("Random effect simulation output.RData") #This is from the random effect simulation script
load("Condition tables.RData") #This is the table for the condition models
load("Survival table.RData") #Actually has info for reproductive models too
load("Survival table appendix.RData") #For appendix tables
load("Condition plots.RData") #Includes appendix table
load("Fitness figure.RData")
load("Diet figures.RData")
load("ICC.RData")
load("Consistency plots and info.RData")
load("Reproductive numbers.RData") #Reproductive version of dict
load("C:/Users/jbrig/Desktop/Badgers/Badger condition/R Code/GAMM-Repeatability/Bootstrapping output.RData")

library(cowplot)
library(patchwork)
library(dplyr)

# Descriptive condition figure #

save_plot(descriptiveConditionFig, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Condition figure base 2021-06-05.png",
          base_width = 10, base_height = 3.75,
          dpi = 600)

# Diet figure #

save_plot(dietFigure, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Diet figure.png",
          base_width = 10, base_height = 8,
          dpi = 600)

# Fitness figure #

save_plot(fitnessFigure, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Fitness figure base.png",
          base_width = 10, base_height = 12,
          dpi = 600)

# Banana plot #

bananaPlots <- plot_grid(bananaPlot, suppBananaPlot, nrow = 1)

save_plot(bananaPlots, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Banana plot base.png",
          base_width = 10, base_height = 4,
          dpi = 600)

# Consistency figure #

save_plot(linesPlot, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Consistency plot base.png",
          base_width = 10, base_height = 6,
          dpi = 600)

# BCI figure #

save_plot(summerWeatherBCIPlot,
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Seasonal BCI effects base.png",
          base_width = 10, base_height = 7.5,
          dpi = 600)

# Bootstrapping figure #

save_plot(bootFigure, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Bootstrap figure base.png",
          base_width = 10, base_height = 12,
          dpi = 600)

save_plot(bootFigureApp, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Appendix bootstrap figure base.png",
          base_width = 10, base_height = 12,
          dpi = 600)
