######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot gini coefficient over time across models

######



rm(list = ls())

library(ggplot2)
library(reshape2)
library(RColorBrewer)
"%&%"<-function(x,y)paste(x,y,sep="")  #define a function for easy string pasting


# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)


################################################################################
################################################################################
# 1. Projected global losses over time
################################################################################
################################################################################

# Set SSPs
scens <- "SSP"%&%c(1:5)
# Set Years
yrs <- 2010:2099
# Set number of regression models
nr <- 9
# Set models
mdls <- "Compare"%&%c(1:nr)
#mdlnames <- c("CI-SR-N","CI-SR-L","CI-SR-Q","CC-SR-N","CC-SR-L","CC-LR-N","CC-LR-N-RP")
mdlnames <- c("SSP","Column "%&%c(1:9))

# Load population data
load('data/output/projectionOutput/popProjections.rdata')

# Loop over SSPs
# for(scen in c(1,2,3,4,5)){
for(scen in c(5)){
  print("SSP"%&%scen)
  mdlnames <- c("SSP"%&%scen,"Column "%&%c(1:9))
  
  
  # Load SSP population projection
  popProj <- popProjections[[scen]]
  pop.arr <- as.matrix(popProj[,as.character(c(2010:2099))])
  
  # Initialize dataframes to store outcomes
  gini <- data.frame('Year' = yrs) # Gini coefficients
  
  # Load GDP/capita under SSP
  load("data/output/projectionOutput/UpdatedData/GDPcapNoCC_RCP85_Compare2_"%&%scens[scen]%&%".Rdata")
  # Calculate GDP
  GDPNoCC <- as.matrix(GDPcapNoCC)*pop.arr
  GDPNoCC <- array(GDPNoCC,dim=c(dim(GDPcapNoCC)[1],90),dimnames=dimnames(GDPcapNoCC))
  
  gini.temp <- array(dim=90)
  for(yr in c(1:90)){
    GDPNoCC.temp <- GDPNoCC[order(GDPcapNoCC[,yr]),]
    pop.arr.temp <- pop.arr[order(GDPcapNoCC[,yr]),]
    gdp.cum <- cumsum(GDPNoCC.temp[,yr])/sum(GDPNoCC.temp[,yr])
    pop.frac <- (pop.arr.temp[,yr])/sum(pop.arr.temp[,yr])
    gini.temp[yr] <- 1-2*sum(gdp.cum*pop.frac)
  }
  gini[,"SSP"%&%scen] <- as.numeric(gini.temp)
  
  # Loop over regression models
  for(mdl in c(1:nr)){
    
    print(mdl)
    
    # Load GDP/capita under SSP
    load("data/output/projectionOutput/UpdatedData/GDPcapCC_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
    # Calculate GDP
    GDPCC <- as.matrix(GDPcapCC)*pop.arr
    GDPCC <- array(GDPCC,dim=c(dim(GDPcapCC)[1],90),dimnames=dimnames(GDPcapCC))
    
    gini.temp <- array(dim=90)
    for(yr in c(1:90)){
      GDPCC.temp <- GDPCC[order(GDPcapCC[,yr]),]
      pop.arr.temp <- pop.arr[order(GDPcapNoCC[,yr]),]
      gdp.cum <- cumsum(GDPCC.temp[,yr])/sum(GDPCC.temp[,yr])
      pop.frac <- (pop.arr.temp[,yr])/sum(pop.arr.temp[,yr])
      gini.temp[yr] <- 1-2*sum(1/2*(gdp.cum+c(gdp.cum[2:dim(GDPcapCC)[1]],1))*pop.frac)
    }
    gini[,"Column "%&%mdl] <- as.numeric(gini.temp)
    
  }
  
  
  
  # Prep dataframes for plotting
  gini.long <- melt(gini,id.vars=c("Year"))
  colnames(gini.long) <- c("Year","Model","Gini Coefficient")
  levels(gini.long$Model) <- mdlnames
  
  
  # Set color palette
  my_palette = c(brewer.pal(7, "Set1"))
  my_palette = c("black","#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438")
  my_palette_bw = rev(c("#000000",  "#4D4D4D",  "#999999",  "#CCCCCC",  "#E5E5E5" ))
  
  # Generate plot for respective SSP
  plotdf <- gini.long
  plot <- ggplot(plotdf,aes(x=Year,y=`Gini Coefficient`,colour=Model)) + 
    geom_line(size=1) + 
    xlab("Year") + ylab("Gini Coefficient") +
    theme_classic(base_size=15)  +
    scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.25),expand=c(0,0)) +
    scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30),expand=c(0,0)) +
    theme(text = element_text(size=18)) + 
    scale_color_manual(values=my_palette)
  
  # Export plots
  pdf(file=paste('tablesandfigures/GiniCoefficientOverTime',scens[scen],'.pdf',sep=''),height=5,width=8)
  print(plot)
  dev.off()
  
  # Create slimmer version for text
  if(scen==5){
    
    # Generate plot for respective SSP
    plotdf <- gini.long[gini.long$Model %in% c(mdlnames[c(1,3,5,7,8)]),]
    plottext <- ggplot(plotdf,aes(x=Year,y=`Gini Coefficient`,colour=Model)) + 
      geom_line(size=1) + 
      xlab("Year") + ylab("Gini Coefficient") +
      theme_classic(base_size=15)  +
      scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.25),labels=c("0","0.25","0.5","0.75","1"),expand=c(0,0)) +
      scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30),expand=c(0,0)) +
      theme(text = element_text(size=18)) + 
      scale_color_manual(values=my_palette[c(1,3,5,7,8)])
    
    # Export plots
    pdf(file=paste('tablesandfigures/Figure3c.pdf',sep=''),height=5,width=6.5)
    print(plottext)
    dev.off()
    
    # Generate plot for respective SSP
    plotdf <- gini.long[gini.long$Model %in% c(mdlnames[c(1,3,5,7,8)]),]
    plottext <- ggplot(plotdf,aes(x=Year,y=`Gini Coefficient`,colour=Model)) + 
      geom_line(size=1) + 
      xlab("Year") + ylab("Gini Coefficient") +
      theme_classic(base_size=15)  +
      scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.25),labels=c("0","0.25","0.5","0.75","1"),expand=c(0,0)) +
      scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30),expand=c(0,0)) +
      theme(text = element_text(size=18)) + 
      scale_color_manual(values=my_palette_bw[c(5,1,2,3,4)])
    
    # Export plots
    pdf(file=paste('tablesandfigures/Figure3c_bw.pdf',sep=''),height=5,width=6.5)
    print(plottext)
    dev.off()
  }
  
  # Write results to a .csv file
  write.csv(gini.long,paste('tablesandfigures/GiniCoefficients',scens[scen],'.csv',sep=''))
}






