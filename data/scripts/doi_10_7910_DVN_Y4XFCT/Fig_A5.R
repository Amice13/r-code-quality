##########################################################################
# Figure A5: Density test of the manipulation of running variable
##########################################################################

rm(list=ls())

library(ggplot2)
library(rdrobust)
library(rddensity)
library(here)

electoral <- readRDS(here("data","mayors_municipal_level.rds"))

vector <- electoral$inverted_margin / 1000
rdd <- rddensity(X = vector)

plot.density.1 <- rdplotdensity(rdd, X = vector, CIuniform = TRUE, xlabel = 'Inverted Margin\n(in 1000s reais)')
# ggsave(here('img','Fig_A5.pdf'), plot = plot.density.1$Estplot, device = 'pdf',height = 10, width = 12, units = 'cm')
