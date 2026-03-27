# @author: Anthony Harding
# Georgia Institute of Technology, School of Public Policy


# Make Lorenz curve for SSP data



rm(list=ls())
library(ggplot2)
library(reshape2)
library(cowplot)
"%&%"<-function(x,y)paste(x,y,sep="")  #define a function for easy string pasting


# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)

#########################################################################################
# Load SSP data
#########################################################################################
# Set SSP
scens <- c("SSP"%&%1:5)

# Load population data
load('data/output/projectionOutput/popProjections.Rdata')


for(scen in c(1:5)){
  print(scens[scen])
  # Load SSP population projection
  popProj <- popProjections[[scen]]
  
  # Load SSP growth projection
  load("data/output/projectionOutput/GDPcapNoCC_RCP85_Compare1_"%&%scens[scen]%&%".Rdata")
  
  df2010 <- data.frame(iso=popProj$iso,GDPcap=GDPcapNoCC[,1],pop=popProj[,"2010"],Year=2010)
  df2010$GDP <- df2010$GDPcap*df2010$pop
  df2010 <- df2010[order(df2010$GDPcap),]
  df2010$'GDPshare' <- 100*cumsum(df2010$GDP)/sum(df2010$GDP)
  df2010$'Popshare' <- 100*cumsum(df2010$pop)/sum(df2010$pop)
  
  df2100 <- data.frame(iso=popProj$iso,GDPcap=GDPcapNoCC[,90],pop=popProj[,"2099"],Year=2100)
  df2100$GDP <- df2100$GDPcap*df2100$pop
  df2100 <- df2100[order(df2100$GDPcap),]
  df2100$'GDPshare' <- 100*cumsum(df2100$GDP)/sum(df2100$GDP)
  df2100$'Popshare' <- 100*cumsum(df2100$pop)/sum(df2100$pop)
  
  df <- rbind(df2010,df2100)
  df$Year <- as.factor(df$Year)
  
  
  lp <- ggplot(data = df,aes(x=Popshare,y=GDPshare,linetype = Year)) +
    geom_line(size=1,show.legend = FALSE) +
    geom_segment(aes(x=0,xend=100,y=0,yend=100),colour='red',linetype='dotted') +
    geom_text(aes(x=65,y=mean(df[Year==2100 & Popshare>70 & Popshare<80, "GDPshare"])+5,label="SSP "%&%scen%&%" (2100)")) +
    geom_text(aes(x=65,y=mean(df[Year==2010 & Popshare>70 & Popshare<80, "GDPshare"])+ifelse(scen==4,-8,5),label="SSP "%&%scen%&%" (2010)")) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),axis.line = element_line(colour = "black"),text=element_text(size=14),legend.background = element_rect(fill = 'transparent'),legend.key = element_rect(fill = 'white'),legend.position = c(0.025, 1),legend.justification = c('left','top'),plot.title = element_text(hjust = 0.5)) +
    xlab("Cum. Share of Global Population (%)") + ylab('Cum. Share of Global GDP') + coord_fixed(1)
  
  
  # Save as pdf
  pdf(file = 'tablesandfigures/Figure4_SSP'%&%scen%&%'.pdf',height=5,width=8)
  print(lp)
  dev.off()
  print(lp)
  
  
}



