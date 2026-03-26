######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script makes map of loss in GDP/capita b/w CC and NoCC in 2099

######


rm(list=ls())


library(ggplot2)
library(sf)
library(dplyr)
"%&%"<-function(x,y)paste(x,y,sep="")  #define a function for easy string pasting

# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)

################################################################################
################################################################################
# 1. Map country-level GDP/capita damages (2099)
################################################################################
################################################################################

# Set SSPs
scens <- "SSP"%&%c(1:5)
# Set Years
yrs <- 2010:2099
# number of models analyzed
nr = 7  
# Set models
mdls <- "Compare"%&%c(1:nr)

# Load Map data
regions <- st_read('data/input/shape/country.shp')


# Loop over SSPs
#for(scen in c(1:5)){
for(scen in c(5)){
  print(scens[scen])
  # Loop over regression models
  #for(mdl in mdls){
  for(mdl in mdls[c(2,4,6,7)]){
    print(mdl)
    
    # Load economic projections
    load("data/output/projectionOutput/UpdatedData/GDPcapCC_RCP85_"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
    load("data/output/projectionOutput/UpdatedData/GDPcapNoCC_RCP85_"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
    
    # Initialize dataframe to store economic losses
    Damagesdf <- data.frame('iso'=dimnames(GDPcapCC)[[1]])
    
    # Append losses in 2099 (% difference between CC and NoCC)
    Damagesdf$'Growth2099' <- (GDPcapCC[,'2099']-GDPcapNoCC[,'2099'])/GDPcapNoCC[,'2099']*100
    Damagesdf$GMI_CNTRY <- Damagesdf$iso
    
    
    
    # Merge and plot data
    regions2 <- left_join(regions,Damagesdf,by="GMI_CNTRY")
    
    if(mdl==mdls[6]){
      dam.map <- ggplot()+
        geom_sf(data = regions2[regions2$GMI_CNTRY!="ATA",], aes(fill = pmin(600,Growth2099)),color="black") +
        # scale_fill_gradientn(breaks=c(-200,-100,0,100,200),colors=rev(brewer.pal(9,"RdBu"))[c(1,2,5,8,9)],limits=c(-200,200)) +
        scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,limits=c(-25,60),name = expression("Difference in GDP per capita (%)"),guide = guide_legend()) +
        theme_void() + theme(legend.text=element_text(size=15),legend.title=element_text(size=15),legend.position="bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.5, "cm")) +
        guides(fill = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))
      pdf(file = 'tablesandfigures/Figure2_'%&%mdl%&%'_'%&%scens[scen]%&%'.pdf', width = 8, height = 5)
      print(dam.map)
      dev.off()
    }else{
      dam.map <- ggplot()+
        geom_sf(data = regions2[regions2$GMI_CNTRY!="ATA",], aes(fill = pmin(600,Growth2099)),color="black") +
        # scale_fill_gradientn(breaks=c(-200,-100,0,100,200),colors=rev(brewer.pal(9,"RdBu"))[c(1,2,5,8,9)],limits=c(-200,200)) +
        scale_fill_gradient2(low="blue",high="red",mid="white",midpoint=0,limits=c(-100,600),name = expression("Difference in GDP per capita (%)"),guide = guide_legend()) +
        theme_void() + theme(legend.text=element_text(size=15),legend.title=element_text(size=15),legend.position="bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.5, "cm")) +
        guides(fill = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))
      pdf(file = 'tablesandfigures/Figure2_'%&%mdl%&%'_'%&%scens[scen]%&%'.pdf', width = 8, height = 5)
      print(dam.map)
      dev.off()
    }
    
    if(mdl==mdls[6]){
      dam.map <- ggplot()+
        geom_sf(data = regions2[regions2$GMI_CNTRY!="ATA",], aes(fill = pmin(600,Growth2099)),color="black") +
        # scale_fill_gradientn(breaks=c(-200,-100,0,100,200),colors=rev(brewer.pal(9,"RdBu"))[c(1,2,5,8,9)],limits=c(-200,200)) +
        scale_fill_gradient2(low="#2D004B",high="#7F3B08",mid="white",midpoint=0,limits=c(-25,60),name = expression("Difference in GDP per capita (%)"),guide = guide_legend()) +
        theme_void() + theme(legend.text=element_text(size=15),legend.title=element_text(size=15),legend.position="bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.5, "cm")) +
        guides(fill = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))
      pdf(file = 'tablesandfigures/Figure2_'%&%mdl%&%'_'%&%scens[scen]%&%'_bw.pdf', width = 8, height = 5, colormodel = "gray")
      print(dam.map)
      dev.off()
    }else{
      dam.map <- ggplot()+
        geom_sf(data = regions2[regions2$GMI_CNTRY!="ATA",], aes(fill = pmin(600,Growth2099)),color="black") +
        # scale_fill_gradientn(breaks=c(-200,-100,0,100,200),colors=rev(brewer.pal(9,"RdBu"))[c(1,2,5,8,9)],limits=c(-200,200)) +
        scale_fill_gradient2(low="#2D004B",high="#7F3B08",mid="white",midpoint=0,limits=c(-100,600),name = expression("Difference in GDP per capita (%)"),guide = guide_legend()) +
        theme_void() + theme(legend.text=element_text(size=15),legend.title=element_text(size=15),legend.position="bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.5, "cm")) +
        guides(fill = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))
      pdf(file = 'tablesandfigures/Figure2_'%&%mdl%&%'_'%&%scens[scen]%&%'_bw.pdf', width = 8, height = 5, colormodel = "gray")
      print(dam.map)
      dev.off()
    }
    
  }
}

