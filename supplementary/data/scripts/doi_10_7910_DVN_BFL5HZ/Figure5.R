######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script makes map of temperature change under RCP8.5

######


rm(list=ls())

library(ggplot2)
library(sf)
library(dplyr)

# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)

################################################################################
################################################################################
# 1. Map country-level GDP/capita damages (2099)
################################################################################
################################################################################


# Load Map data
regions <- st_read('data/input/shape/country.shp')

# Load temperature change data
load("data/output/projectionOutput/Temperatures_RCP85.rdata")
dtempdf <- data.frame(GMI_CNTRY=dimnames(dfTemp)[[1]],dtemp = dfTemp[,90]-dfTemp[,1])

# Merge and plot data
regions2 <- left_join(regions,dtempdf,by="GMI_CNTRY")

png(filename = 'tablesandfigures/Figure5.png', width = 1000, height = 600)
ggplot()+
  geom_sf(data = regions2[regions2$GMI_CNTRY!="ATA",], aes(fill = dtemp),color="black") +
  scale_fill_gradient2(low="white",high="#e50000",limits=c(0,6),name = expression('Temperature Change 2010-2100 ('*degree*'C)'),guide = guide_legend()) +
  theme_void(base_size =18) + theme(legend.position="bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.3, "cm")) +
  guides(fill = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))
dev.off()


