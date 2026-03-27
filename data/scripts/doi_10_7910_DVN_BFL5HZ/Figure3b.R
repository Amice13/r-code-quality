######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot lorenz curves

######


rm(list = ls())

library(ggplot2)
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
# number of models analyzed
nr = 9  
# Set models
mdls <- "Compare"%&%c(1:nr)

# Load population data
load('data/output/projectionOutput/popProjections.rdata')



# Loop over SSPs
for(scen in c(5)){
  print(scens[scen])
  
  # Load SSP population projection
  popProj <- popProjections[[scen]]
  
  # Load SSP growth projection
  load("data/output/projectionOutput/UpdatedData/GDPcapNoCC_RCP85_Compare2_"%&%scens[scen]%&%".Rdata")
  
  # Retreive GDP data for 2010, 2050, and 2100
  df2010 <- data.frame(iso=popProj$iso,GDPcap=GDPcapNoCC[,1],pop=popProj[,"2010"],Year=2010)
  df2010$GDP <- df2010$GDPcap*df2010$pop
  df2010 <- df2010[order(df2010$GDPcap),]
  df2010$'GDPshare' <- 100*cumsum(df2010$GDP)/sum(df2010$GDP)
  df2010$'Popshare' <- 100*cumsum(df2010$pop)/sum(df2010$pop)
  
  df2050 <- data.frame(iso=popProj$iso,GDPcap=GDPcapNoCC[,40],pop=popProj[,"2050"],Year=2050)
  df2050$GDP <- df2050$GDPcap*df2050$pop
  df2050 <- df2050[order(df2050$GDPcap),]
  df2050$'GDPshare' <- 100*cumsum(df2050$GDP)/sum(df2050$GDP)
  df2050$'Popshare' <- 100*cumsum(df2050$pop)/sum(df2050$pop)
  
  df2100 <- data.frame(iso=popProj$iso,GDPcap=GDPcapNoCC[,90],pop=popProj[,"2099"],Year=2100)
  df2100$GDP <- df2100$GDPcap*df2100$pop
  df2100 <- df2100[order(df2100$GDPcap),]
  df2100$'GDPshare' <- 100*cumsum(df2100$GDP)/sum(df2100$GDP)
  df2100$'Popshare' <- 100*cumsum(df2100$pop)/sum(df2100$pop)
  
  # Merge into single data.frame
  df <- rbind(df2010,df2050,df2100)
  df$Year <- as.factor(df$Year)
  df$Model = scens[scen]
  
  # Loop over regression models
  #for(mdl in mdls){
  for(mdl in c(1:nr)){
    print(mdl)
    
    
    # Load SSP growth projection
    load("data/output/projectionOutput/UpdatedData/GDPcapCC_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
    
    # Retreive GDP data for 2010, 2050, and 2100
    df2010 <- data.frame(iso=popProj$iso,GDPcap=GDPcapCC[,1],pop=popProj[,"2010"],Year=2010)
    df2010$GDP <- df2010$GDPcap*df2010$pop
    df2010 <- df2010[order(df2010$GDPcap),]
    df2010$'GDPshare' <- 100*cumsum(df2010$GDP)/sum(df2010$GDP)
    df2010$'Popshare' <- 100*cumsum(df2010$pop)/sum(df2010$pop)
    
    df2050 <- data.frame(iso=popProj$iso,GDPcap=GDPcapCC[,40],pop=popProj[,"2050"],Year=2050)
    df2050$GDP <- df2050$GDPcap*df2050$pop
    df2050 <- df2050[order(df2050$GDPcap),]
    df2050$'GDPshare' <- 100*cumsum(df2050$GDP)/sum(df2050$GDP)
    df2050$'Popshare' <- 100*cumsum(df2050$pop)/sum(df2050$pop)
    
    df2100 <- data.frame(iso=popProj$iso,GDPcap=GDPcapCC[,90],pop=popProj[,"2099"],Year=2100)
    df2100$GDP <- df2100$GDPcap*df2100$pop
    df2100 <- df2100[order(df2100$GDPcap),]
    df2100$'GDPshare' <- 100*cumsum(df2100$GDP)/sum(df2100$GDP)
    df2100$'Popshare' <- 100*cumsum(df2100$pop)/sum(df2100$pop)
    
    # Merge into single data.frame
    dfCC <- rbind(df2050,df2100)
    dfCC$Year <- as.factor(dfCC$Year)
    dfCC$Model = "Column ("%&%mdl%&%")"
    
    df <- rbind(df,dfCC)
  }
    
  # Turn model column to factor for plotting
  df$Model <- as.factor(df$Model)
  
  # Set colors for plot
  my_palette = c("black","#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438")
  my_palette_bw = rev(c("#000000",  "#4D4D4D",  "#999999",  "#CCCCCC",  "#E5E5E5" ))
  
  
  lp <- ggplot(data = df[df$Model%in%c("SSP5","Column (2)","Column (4)","Column (6)","Column (7)"),],aes(x=Popshare,y=GDPshare,linetype = Year,color=Model)) +
    geom_line(size=1) +
    scale_linetype_manual(name="Year",guide="legend",values=c("solid","dashed","dotted"))+
    scale_colour_manual(name="Model",values=c("SSP5"=my_palette[1],"Column (2)"=my_palette[3],"Column (4)"=my_palette[5],"Column (6)"=my_palette[7],"Column (7)"=my_palette[8]),labels=c("Column (2)","Column (4)","Column (6)","Column (7)","SSP5"))+
    geom_segment(aes(x=0,xend=100,y=0,yend=100),colour='grey',linetype='dotted') +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"),text=element_text(size=14),legend.background = element_rect(fill = 'transparent'),legend.key = element_rect(fill = 'white'),legend.position = c(0.025, 1),legend.justification = c('left','top'),plot.title = element_text(hjust = 0.5)) +
    xlab("Cum. Share of Global Population (%)") + ylab('Cum. Share of Global GDP (%)') + coord_fixed(1) +
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
  
  
  # Save as pdf
  pdf(file = 'tablesandfigures/Figure3b.pdf',height=5,width=5)
  print(lp)
  dev.off()
  
  
  lp <- ggplot(data = df[df$Model%in%c("SSP5","Column (2)","Column (4)","Column (6)","Column (7)"),],aes(x=Popshare,y=GDPshare,linetype = Year,color=Model)) +
    geom_line(size=1) +
    scale_linetype_manual(name="Year",guide="legend",values=c("solid","dashed","dotted"))+
    scale_colour_manual(name="Model",values=c("SSP5"=my_palette_bw[5],"Column (2)"=my_palette_bw[1],"Column (4)"=my_palette_bw[2],"Column (6)"=my_palette_bw[3],"Column (7)"=my_palette_bw[4]),labels=c("Column (2)","Column (4)","Column (6)","Column (7)","SSP5"))+
    geom_segment(aes(x=0,xend=100,y=0,yend=100),colour='grey',linetype='dotted') +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"),text=element_text(size=14),legend.background = element_rect(fill = 'transparent'),legend.key = element_rect(fill = 'white'),legend.position = c(0.025, 1),legend.justification = c('left','top'),plot.title = element_text(hjust = 0.5)) +
    xlab("Cum. Share of Global Population (%)") + ylab('Cum. Share of Global GDP (%)') + coord_fixed(1) +
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
  
  
  # Save as pdf
  pdf(file = 'tablesandfigures/Figure3b_bw.pdf',height=5,width=5)
  print(lp)
  dev.off()
  
}
