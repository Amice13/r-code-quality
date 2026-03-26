######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot projected global losses (change in avg GDP/cap)


######



rm(list = ls())

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(stringr)
"%&%"<-function(x,y)paste(x,y,sep="")  #define a function for easy string pasting


# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)


################################################################################
################################################################################
# 1. Projected global losses over time
################################################################################
################################################################################

headers <- read.csv("data/output/TauAnalysis_SR_UpdatedData.csv", header = F, nrows = 1, as.is = T)
prj.sr <- read.csv("data/output/TauAnalysis_SR_UpdatedData.csv",row.names=1,skip=2,header=F)
prj.lr <- read.csv("data/output/TauAnalysis_LR_UpdatedData.csv",row.names=1,skip=2,header=F)
colnames(prj.sr) <- colnames(prj.lr) <- headers[,-1]




# Set SSPs
scens <- "SSP"%&%c(1:5)
# Set Years
yrs <- 2010:2099
# Set number of regression models
nr <- 9
# Set models
mdls <- colnames(prj.sr)
mdlnames <- c(mdls,"SSP (2010)","SSP (2100)")
# Get tau values
taus <- as.numeric(str_sub(headers[,-1],4))

# Loop over SSPs
scen= 5
  print("SSP"%&%scen)
  # Initialize dataframes to store outcomes
  avgGDPcapdf <- data.frame('Year' = yrs) # average GDP/capita results
  GDPdf <- data.frame('Year' = yrs) # GDP results
  
  # Loop over regression models
  for(mdl in mdls){
    print(mdl)
    # Load global projection outcomes
    load("data/output/projectionOutput/TauAnalysis/GlobalChanges_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%"_sr.Rdata")
    
    # Get average GDP/capita losses (%)
    avgGDPcapdf[,mdl] <- (tots[,1]/tots[,2]-1)*100 # (avgGDPcapCC/avgGDPcapNoCC-1)*100
    avgGDPcapdf[1,mdl] <- 0 # first year is end of historical, so no change
    # Get GDP losses (%)
    GDPdf[,mdl] <- (tots[,3]/tots[,4]-1)*100 # (TotGDPCC/TotGDPNoCC-1)*100
    GDPdf[1,mdl] <- 0 # first year is end of historical, so no change
  }
  colnames(GDPdf) <- c("Year",mdls)
  
  # Prep dataframes for plotting
  avgGDPcapdf.long <- reshape2::melt(avgGDPcapdf,id.vars=c("Year"))
  GDPdf.long <- reshape2::melt(GDPdf,id.vars=c("Year"))
  colnames(avgGDPcapdf.long) <- c("Year","Model","GlobalLosses (mean GDP/cap)")
  colnames(GDPdf.long) <- c("Year","Model","GlobalLosses (GDP)")
  levels(avgGDPcapdf.long$Model) <- levels(GDPdf.long$Model) <- mdlnames
  avgGDPcapdf.long['tau2'] <- rep(taus,each=90)
  
  
  # Set color palette
  my_palette = c(brewer.pal(9, "Reds")[3:9])
  newcol <- colorRampPalette(my_palette)
  my_palette <- newcol(500)
  
  # Generate plot for respective SSP
  plotdf <- avgGDPcapdf.long
  plot <- ggplot(plotdf) + 
    geom_point(aes(x=Year,y=`GlobalLosses (mean GDP/cap)`,color=tau2),size=1) + 
    geom_abline(intercept=0,slope=0,size=0.1) + 
    xlab("Year") + ylab("% change in average GDP/cap") +
    theme_classic(base_size=15)  +
    scale_y_continuous(limits=c(min(plotdf$`GlobalLosses (mean GDP/cap)`),max(plotdf$`GlobalLosses (mean GDP/cap)`)),breaks=seq(floor(min(plotdf$`GlobalLosses (mean GDP/cap)`)/10)*10,ceiling(max(plotdf$`GlobalLosses (mean GDP/cap)`)/10)*10,10)) +
    scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30)) +
    theme(text = element_text(size=18)) + 
    annotate("text",x=2010,y=20,label='',size=5,hjust=0) +
    scale_colour_gradientn(breaks=seq(0,100,20),limits=c(0,100),colors=palette(my_palette),name=expression(tau),guide = guide_legend()) +
    theme(legend.position = "bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.3, "cm")) +
    guides(color = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))
  
  
  # Export plots
  pdf(file=paste('tablesandfigures/Figure13_',scens[scen],'_sr.pdf',sep=''),height=5,width=8)
  print(plot)
  dev.off()
  
  png(filename=paste('tablesandfigures/Figure13_',scens[scen],'_sr.png',sep=''),height=500,width=800)
  print(plot)
  dev.off()
  



# 
# # Loop over SSPs
# scen = 5
#   print("SSP"%&%scen)
#   # Initialize dataframes to store outcomes
#   avgGDPcapdf <- data.frame('Year' = yrs) # average GDP/capita results
#   GDPdf <- data.frame('Year' = yrs) # GDP results
#   
#   # Loop over regression models
#   for(mdl in mdls){
#     print(mdl)
#     # Load global projection outcomes
#     load("data/output/projectionOutput/UpdatedData/TauAnalysis/GlobalChanges_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%"_lr.Rdata")
#     
#     # Get average GDP/capita losses (%)
#     avgGDPcapdf[,mdl] <- (tots[,1]/tots[,2]-1)*100 # (avgGDPcapCC/avgGDPcapNoCC-1)*100
#     avgGDPcapdf[1,mdl] <- 0 # first year is end of historical, so no change
#     # Get GDP losses (%)
#     GDPdf[,mdl] <- (tots[,3]/tots[,4]-1)*100 # (TotGDPCC/TotGDPNoCC-1)*100
#     GDPdf[1,mdl] <- 0 # first year is end of historical, so no change
#   }
#   colnames(GDPdf) <- c("Year",mdls)
#   
#   # Prep dataframes for plotting
#   avgGDPcapdf.long <- reshape2::melt(avgGDPcapdf,id.vars=c("Year"))
#   GDPdf.long <- reshape2::melt(GDPdf,id.vars=c("Year"))
#   colnames(avgGDPcapdf.long) <- c("Year","Model","GlobalLosses (mean GDP/cap)")
#   colnames(GDPdf.long) <- c("Year","Model","GlobalLosses (GDP)")
#   levels(avgGDPcapdf.long$Model) <- levels(GDPdf.long$Model) <- mdlnames
#   avgGDPcapdf.long['tau2'] <- rep(taus,each=90)
#   
#   
#   # Set color palette
#   my_palette = c(brewer.pal(9, "Reds")[3:9])
#   newcol <- colorRampPalette(my_palette)
#   my_palette <- newcol(50)
#   
#   # Generate plot for respective SSP
#   plotdf <- avgGDPcapdf.long
#   plot <- ggplot(plotdf) + 
#     geom_point(aes(x=Year,y=`GlobalLosses (mean GDP/cap)`,color=tau2),size=1) + 
#     geom_abline(intercept=0,slope=0,size=0.1) + 
#     xlab("Year") + ylab("% change in average GDP/cap") +
#     theme_classic(base_size=15)  +
#     scale_y_continuous(limits=c(min(plotdf$`GlobalLosses (mean GDP/cap)`),max(plotdf$`GlobalLosses (mean GDP/cap)`)),breaks=seq(floor(min(plotdf$`GlobalLosses (mean GDP/cap)`)/10)*10,ceiling(max(plotdf$`GlobalLosses (mean GDP/cap)`)/10)*10,5)) +
#     scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30)) +
#     theme(text = element_text(size=18)) + 
#     annotate("text",x=2010,y=20,label='',size=5,hjust=0) +
#     scale_colour_gradientn(breaks=seq(0,100,20),limits=c(0,100),colors=palette(my_palette),name=expression(tau),guide = guide_legend()) +
#     theme(legend.position = "bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.3, "cm")) +
#     guides(color = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))
#   
#   
#   # Export plots
#   pdf(file=paste('tablesandfigures/Figure13_',scens[scen],'_lr.pdf',sep=''),height=5,width=8)
#   print(plot)
#   dev.off()
#   



