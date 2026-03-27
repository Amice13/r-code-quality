
##########################################################################
# Density test
##########################################################################

rm(list=ls())

library(ggplot2)
library(rdrobust)
library(here)
library(rddensity)
library(data.table)

load(here("data","ver_data.RData"))

tt <- unique(ver_b[law_enforcement ==1 & abs(marginal_muni) < 0.01 & p_elected_muni == 0, .(muni_code, year, law_enforcement, marginal_muni, elected_muni, p_elected_muni, sc_before, dv_homicides, y2008, y2012)])

vector <- tt$marginal_muni * 100
rdd <- rddensity(X = vector, massPoints = TRUE)

summary(rdd)
plot.density.1 <- rdplotdensity(rdd, X = vector, xlabel = 'Vote margin (in %)')

ggsave(here("writing","img","fig_A4.pdf"), plot = plot.density.1$Estplot, device = 'pdf',height = 7.5, width = 7.5, units = 'cm')

