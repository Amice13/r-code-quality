rm(list = ls())

library(metafor)
library(clubSandwich)
library(tidyverse)
library(readr)
effects <- read_csv("effects.csv")

effects$ATE <- as.numeric(effects$ATE)

effects$SE <- as.numeric(effects$SE)


effects$ATE_variance <- effects$SE^2

meta <- rma.mv(yi = ATE, V = ATE_variance, random = list(~1 | item_for_clustering), data = effects)
coef_test(meta, vcov = "CR2")

forest(meta, slab = paste0(effects$item_for_clustering), cex = .4, xlab = 'Correction Effect: All Items')


pdf("meta_analysis_forest_plot.pdf", width = 7, height = 7)

forest(meta, 
       slab = paste0(effects$item_for_clustering), 
       cex = 0.7,                        
       xlab = "Fact-Checking Effect: All Items", 
       main = "Fact-Checks Can Cause Reputational Penalty of Low-Familiarity Subjects \n (0-100 Scale)", 
       xlim = c(-20, 20),                
       at = seq(-20, 20, 5),            
       col = "black",                    
       col.diamond = "blue",             
       col.diamond.lines = "blue",       
       lwd = 2)                          

dev.off()





