######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot growth rates under RCP8.5

######


rm(list=ls())


library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(ggpubr)
"%&%"<-function(x,y)paste(x,y,sep="")  #define a function for easy string pasting

# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)




################################################################################
################################################################################
# 1. Economic Projection Growth Rates
################################################################################
################################################################################

# Set SSPs
scens <- "SSP"%&%c(1:5)
# Set Years
Years <- 2010:2099
# Set number of regression models
nr <- 9
# Set models
mdls <- "Compare"%&%c(1:nr)
#mdlnames <- c("CI-SR-N","CI-SR-L","CI-SR-Q","CC-SR-N","CC-SR-L","CC-LR-N","CC-LR-N-RP","SSP (2010)","SSP (2100)")
mdlnames <- c("SSP (2010)","SSP (2050)","Column "%&%c(1:9)%&%" (2050)","SSP (2100)","Column "%&%c(1:9)%&%" (2100)")


# Loop over SSP (1 plot/SSP)
for(scen in c(1,2,3,4,5)){
  print("SSP"%&%scen)
  
  # Load global economic projections for NoCC (SSP)
  load("data/output/projectionOutput/UpdatedData/GDPcapNoCC_RCP85_"%&%mdls[1]%&%"_"%&%scens[scen]%&%".Rdata")
  # Initialize dataframe to store results
  df <- data.frame(dimnames(GDPcapNoCC)[1])
  colnames(df) <- c("Country")
  # Store growth in 2010-2011 for SSP
  df[,"SSP (2010)"] <- GDPcapNoCC[,2]/GDPcapNoCC[,1]
  df[,"SSP (2050)"] <- GDPcapNoCC[,40]/GDPcapNoCC[,39]
  df[,"SSP (2100)"] <- GDPcapNoCC[,90]/GDPcapNoCC[,89]
  
  for(mdl in c(1:nr)){
    print(mdl)
    # Load GDPcap data
    load("data/output/projectionOutput/UpdatedData/GDPcapCC_RCP85_"%&%mdls[mdl]%&%"_"%&%scens[scen]%&%".Rdata")
    # Add growth in 2098-2099 to master dataframe
    df[,"Model "%&%mdl%&%" (2050)"] <- GDPcapCC[,40]/GDPcapCC[,39]
    df[,"Model "%&%mdl%&%" (2100)"] <- GDPcapCC[,90]/GDPcapCC[,89]
  }
  
  # Prep dataframe for plotting
  df <- df[,c("Country","SSP (2010)","SSP (2050)","Model "%&%c(1:nr)%&%" (2050)","SSP (2100)","Model "%&%c(1:nr)%&%" (2100)")] # Reorder columns for plotting
  df.long <- melt(df,id.vars=c("Country"))
  colnames(df.long) <- c("Country","Modelyear","value")
  levels(df.long$Modelyear) <- mdlnames
  
  df.long[,c("Model")] = c(rep("SSP5",dim(GDPcapCC)[1]*2),rep("Column "%&%c(1:9),each=dim(GDPcapCC)[1]),rep("SSP5",dim(GDPcapCC)[1]),rep("Column "%&%c(1:9),each=dim(GDPcapCC)[1]))
  df.long[,c("Year")] = c(rep("2010",dim(GDPcapCC)[1]),rep("2050",dim(GDPcapCC)[1]*10),rep("2100",dim(GDPcapCC)[1]*10))
  df.long[,c("Model")] = as.factor(df.long[,c("Model")])
  
  # Set color palette
  my_palette2 = c(brewer.pal(7, "Set1"),'black','gray')
  my_palette = c('gray',"black","#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438","black","#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438")
  my_palette_bw = rev(c("#000000",  "#4D4D4D",  "#999999",  "#CCCCCC",  "#E5E5E5" ))
  # 
  # # Generate scatter plot of growth rates
  # scatterGrowthRates <- ggplot(df.long,aes(x=Country,y=value,colour=Modelyear))+
  #   theme_classic(base_size=15) + 
  #   geom_point() +
  #   xlab("Country") + ylab('log(GDP/capita (US$1,000))') +
  #   scale_color_manual(values=my_palette)
  # 
  # Generate box plot of growth rates
  boxplotGrowthRates <- ggplot(df.long[df.long$Year=="2100"|df.long$Modelyear=="SSP (2010)",],aes(x=Modelyear,y=(value-1)*100,colour=Modelyear))+
    theme_classic(base_size=15)+
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.line.x=element_blank()) + 
    geom_boxplot(outlier.shape = NA) +
    geom_hline(yintercept = 0,color="gray",linetype="dashed") +
    # scale_y_continuous(breaks=seq(-5,10,5),limits=c(-7,12)) +
    xlab("Model/Year") + ylab('Country Growth Rate (%)')  +
    scale_color_manual(values=my_palette[c(1:11)])
  # 
  # # Export scatter plot
  # pdf(file=paste('tablesandfigures/ConvergenceScatter',scens[scen],'.pdf',sep=''),height=5,width=8)
  # print(scatterGrowthRates)
  # dev.off()
  
  # Export box plot
  pdf(file=paste('tablesandfigures/Figure9_',scens[scen],'.pdf',sep=''),height=5,width=8)
  print(boxplotGrowthRates)
  dev.off()
  
  
  # Create slimmer plot for text
  if(scen==5){
    
    # Generate box plot of growth rates
    boxplotGrowthRatestext <- ggplot()+
      theme_classic(base_size=15)+
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.line.x=element_blank()) +
      geom_boxplot(data=df.long[df.long$Modelyear %in% c("SSP (2010)","SSP (2050)",mdlnames[c(4,6,8,9)],"SSP (2100)",mdlnames[c(14,16,18,19)]),],aes(x=Modelyear,y=(value-1)*100,colour=Model),outlier.shape = NA) +
      geom_hline(yintercept = 0,color="gray",linetype="dashed") +
      scale_y_continuous(breaks=seq(-30,10,10),limits=c(-33,11)) +
      xlab("Model/Year") + ylab('Country Growth Rate (%)')  +
      # scale_color_manual(values=c(my_palette[c(1,2,4,6,7,2,12,14,15)]) +
      # scale_color_manual(values=c("SSP (2010)"=my_palette[1],mdlnames[4]=my_palette[4],mdlnames[6]=my_palette[6],mdlnames[7]=my_palette[7])) +
      scale_colour_manual(name="Model",values=c("SSP5"=my_palette[2],"Column 2"=my_palette[4],"Column 4"=my_palette[6],"Column 6"=my_palette[8],"Column 7"=my_palette[9]),labels=c("Column (2)","Column (4)","Column (6)","Column (7)","SSP5"))+
      geom_bracket(
        xmin = c(0.5,1.55,6.55), xmax = c(1.45,6.45,12.45), y.position = -32,
        label = c("2010","2050","2100"), tip.length = c(-0.02, -0.02),vjust=2.7
      )
    
    
    # Export text box plot
    pdf(file=paste('tablesandfigures/Figure3a.pdf',sep=''),height=5,width=10)
    print(boxplotGrowthRatestext)
    dev.off()
    
    
    # Generate box plot of growth rates
    boxplotGrowthRatestext <- ggplot()+
      theme_classic(base_size=15)+
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.line.x=element_blank()) +
      geom_boxplot(data=df.long[df.long$Modelyear %in% c("SSP (2010)","SSP (2050)",mdlnames[c(4,6,8,9)],"SSP (2100)",mdlnames[c(14,16,18,19)]),],aes(x=Modelyear,y=(value-1)*100,colour=Model),outlier.shape = NA) +
      geom_hline(yintercept = 0,color="gray",linetype="dashed") +
      scale_y_continuous(breaks=seq(-30,10,10),limits=c(-33,11)) +
      xlab("Model/Year") + ylab('Country Growth Rate (%)')  +
      # scale_color_manual(values=c(my_palette[c(1,2,4,6,7,2,12,14,15)]) +
      # scale_color_manual(values=c("SSP (2010)"=my_palette[1],mdlnames[4]=my_palette[4],mdlnames[6]=my_palette[6],mdlnames[7]=my_palette[7])) +
      scale_colour_manual(name="Model",values=c("SSP5"=my_palette_bw[5],"Column 2"=my_palette_bw[1],"Column 4"=my_palette_bw[2],"Column 6"=my_palette_bw[3],"Column 7"=my_palette_bw[4]),labels=c("Column (2)","Column (4)","Column (6)","Column (7)","SSP5"))+
      geom_bracket(
        xmin = c(0.5,1.55,6.55), xmax = c(1.45,6.45,12.45), y.position = -32,
        label = c("2010","2050","2100"), tip.length = c(-0.02, -0.02),vjust=2.7
      )
    
    
    # Export text box plot
    pdf(file=paste('tablesandfigures/Figure3a_bw.pdf',sep=''),height=5,width=10)
    print(boxplotGrowthRatestext)
    dev.off()
  }
  
  
  
}
