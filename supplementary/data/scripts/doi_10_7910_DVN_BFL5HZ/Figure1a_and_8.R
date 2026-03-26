######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot global economic projections under RCP8.5

######


rm(list=ls())

library(ggplot2)
library(reshape2)
library(RColorBrewer)
"%&%"<-function(x,y)paste(x,y,sep="")  #define a function for easy string pasting

# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)



################################################################################
################################################################################
# Economic Projections
################################################################################
################################################################################

# Set SSPs
scens <- "SSP"%&%c(1:5)
# Set Years
yrs <- 2010:2099
# Set number of regression models
nr <- 9
#mdlnames <- c("CI-SR-N","CI-SR-L","CI-SR-Q","CC-SR-N","CC-SR-L","CC-LR-N","CC-LR-N-RP","SSP")
mdlnames <- c("Column "%&%c(1:9),"SSP")


# Loop over SSP (1 plot/SSP)
for(scen in c(1,2,3,4,5)){
  print("SSP"%&%scen)
  # Load population data
  load('data/output/projectionOutput/popProjections.Rdata')
  Pop <- popProjections[[scen]]
  Pop <- data.matrix(Pop)
  Pop <- Pop[,8:97]
  Pop <- colSums(Pop)
  
  #Initialize dataframe to store results across models
  df <- data.frame('Year'=yrs)
  dfGDP <- data.frame('Year'=yrs)
  
  # Loop over regression models
  for(mdl in "Compare"%&%c(1:nr)){
    print(mdl)
    # Load global economic projections
    load("data/output/projectionOutput/UpdatedData/GlobalChanges_RCP85_"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
    # Append column to master df with global GDP/capita for CC
    df[,mdl] <- tots[,3]/(1e6*Pop)/1000 # multiplying by 1e6 because population is in millions
    dfGDP[,mdl] <- tots[,3]/1e12
  }
  colnames(df) <- colnames(dfGDP) <- c("Year","Model "%&%c(1:nr))
  
  # Load global economic projections for NoCC (SSP)
  load("data/output/projectionOutput/UpdatedData/GlobalChanges_RCP85_"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
  # Append column to master df with global GDP/capita for NoCC
  df[,"SSP"] <- tots[,4]/(1e6*Pop)/1000 # multiplying by 1e6 because population is in millions
  dfGDP[,"SSP"] <- tots[,4]/1e12
  
  
  
  
  # Prep dataframe for plotting
  df.long <- melt(df,id.vars=c("Year"))
  colnames(df.long) <- c("Year","Model","value")
  df.long <- df.long[df.long$Year!=2010,] # Drop 2010 missing values
  levels(df.long$Model) <- mdlnames
  # df.long$value <- log(df.long$value) #convert to log plot
  
  # Set color palette
  my_palette = c("#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438","black")
  my_palette_bw = rev(c("#000000",  "#4D4D4D",  "#999999",  "#CCCCCC",  "#E5E5E5" ))
  
  # Generate plot for respective SSP
  pGDPcap <- ggplot(df.long,aes(x=Year,y=log(value*1000),colour=Model,linetype=Model))+
    theme_classic(base_size=15) + 
    geom_line(size=1) +
    xlab("Year") + ylab('Global GDP/capita (2015USD)') + labs(colour = "Model") +
    scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30),expand=c(0,0)) + 
    scale_y_continuous(limits=c(8.5,(ceiling(max((log(df.long$value*1000)))))),expand=c(0,0),breaks=c(log(10000),log(20000),log(60000),log(160000)),labels=c("10000","20000","60000","160000")) +
    # coord_fixed(89/(10*(round((max(log(1000*df.long$value))/10)+1)))) + 
    expand_limits(y=0) +
    scale_color_manual(values=my_palette)+ 
    scale_linetype_manual(values=rep(1,nr+1))
  
  
  pdf(file=paste('tablesandfigures/Figure8_',scens[scen],'.pdf',sep=''),height=5,width=8)
  print(pGDPcap)
  dev.off()
  
  # Create slimmer plot for text
  if(scen==5){
    pGDPcaptext <- ggplot(df.long[df.long$Model %in% c(mdlnames[c(2,4,6,7)],"SSP"),],aes(x=Year,y=log(value*1000),colour=Model,linetype=Model))+
      theme_classic(base_size=15) + 
      geom_line(linewidth=1) +
      xlab("Year") + ylab('Global GDP/capita (2015USD)') + labs(colour = "Model") +
      scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30),expand=c(0,0)) + 
      scale_y_continuous(limits=c(8.5,(ceiling(max((log(df.long$value*1000)))))),expand=c(0,0),breaks=c(log(10000),log(20000),log(60000),log(160000)),labels=c("10000","20000","60000","160000")) +
      expand_limits(y=0) +
      scale_color_manual(values=my_palette[c(2,4,6,7,10)])+ 
      scale_linetype_manual(values=c(1,1,1,1,1))
    
    
    pdf(file=paste('tablesandfigures/Figure1a.pdf',sep=''),height=5,width=8)
    print(pGDPcaptext)
    dev.off()
    
    pGDPcaptext <- ggplot(df.long[df.long$Model %in% c(mdlnames[c(2,4,6,7)],"SSP"),],aes(x=Year,y=log(value*1000),colour=Model,linetype=Model))+
      theme_classic(base_size=15) + 
      geom_line(linewidth=1) +
      xlab("Year") + ylab('Global GDP/capita (2015USD)') + labs(colour = "Model") +
      scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30),expand=c(0,0)) + 
      scale_y_continuous(limits=c(8.5,(ceiling(max((log(df.long$value*1000)))))),expand=c(0,0),breaks=c(log(10000),log(20000),log(60000),log(160000)),labels=c("10000","20000","60000","160000")) +
      expand_limits(y=0) +
      scale_color_manual(values=my_palette_bw)+ 
      scale_linetype_manual(values=c(1,1,1,1,1))
    
    
    pdf(file=paste('tablesandfigures/Figure1a_bw.pdf',sep=''),height=5,width=8)
    print(pGDPcaptext)
    dev.off()
    
  }
  
  
  
  # Prep dataframe for plotting
  dfGDP.long <- melt(dfGDP,id.vars=c("Year"))
  colnames(dfGDP.long) <- c("Year","Model","value")
  dfGDP.long <- dfGDP.long[dfGDP.long$Year!=2010,] # Drop 2010 missing values
  
  # Set color palette
  # my_palette = c(brewer.pal(7, "Set1"),'black')
  my_palette = c("#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438","black")
  
  # Generate plot for respective SSP
  pGDP <- ggplot(dfGDP.long,aes(x=Year,y=value,colour=Model,linetype=Model))+
    theme_classic(base_size=15) + 
    geom_line() +
    xlab("Year") + ylab('Global GDP ($Trillion)') + labs(colour = "Model") +
    scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30),expand=c(0,0)) + 
    scale_y_continuous(limits=c(0,10*(round((max(dfGDP.long$value)/10)+1))),expand=c(0,0)) + 
    coord_fixed(89/(10*(round((max(dfGDP.long$value)/10)+1)))) + 
    expand_limits(y=0) +
    scale_color_manual(values=my_palette)+ 
    scale_linetype_manual(values=rep(1,nr+1))
  
  
  pdf(file=paste('tablesandfigures/GlobalGDP_',scens[scen],'.pdf',sep=''),height=5,width=8)
  print(pGDP)
  dev.off()
}


