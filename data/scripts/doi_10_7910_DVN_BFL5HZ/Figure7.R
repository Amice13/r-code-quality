######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script makes map of steady state growth

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
# 1. Map country-level stead-state growth
################################################################################
################################################################################

# Load Map data
regions <- st_read('data/input/shape/country.shp')

# Read steady state growth estimates
SSGdf <- read.csv("data/output/SteadyStateGrowthPaths_UpdatedData.csv")
SSGdf <- SSGdf[c(2,c(65:361)),c(1,2)]
SSGdf$est1 <- as.numeric(SSGdf$est1)
SSGdf[c(2:298),c(2)] <- -100*SSGdf[c(2:298),c(2)]/SSGdf[c(1),c(2)]
SSGdf <- SSGdf[c(2:298),c(1,2)]
SSGdf$iso <- substr(SSGdf$X,4,6)
SSGdf <- SSGdf[SSGdf$est != 0,]
SSGdf$SSG <- SSGdf$est
colnames(SSGdf)[3] <- "GMI_CNTRY"

# Merge and plot data
regions2 <- left_join(regions,SSGdf,by="GMI_CNTRY")

png(filename = 'tablesandfigures/Figure7.png', width = 1000, height = 600)
ggplot()+
  geom_sf(data = regions2[regions2$GMI_CNTRY!="ATA",], aes(fill = pmin(10,pmax(0,SSG))),color="black") +
  scale_fill_gradient2(low="white",high="#228C22",limits=c(0,10),name = expression('Steady-state growth rate (%)'),guide = guide_legend(),breaks=c(0,2.5,5,7.5,10),labels=c("\u22640","2.5","5","7.5","\u226510")) +
  theme_void(base_size =18) + theme(legend.position="bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.3, "cm")) +
  guides(fill = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))
dev.off()




