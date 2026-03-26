######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot peak growth temperatures across regression models

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
# 1. Optimal Temperature Functions
################################################################################
################################################################################


# Import historical climate-economy regression coefficients
headers <- read.csv("data/output/TauAnalysis_SR_UpdatedData.csv", header = F, nrows = 1, as.is = T)
prj.sr <- read.csv("data/output/TauAnalysis_SR_UpdatedData.csv",row.names=1,skip=2,header=F)
prj.lr <- read.csv("data/output/TauAnalysis_LR_UpdatedData.csv",row.names=1,skip=2,header=F)
colnames(prj.sr) <- colnames(prj.lr) <- headers[,-1]

# Get tau values
taus <- as.numeric(str_sub(headers[,-1],4))

# Initialize dataframe to store marginal effects
df.sr <- df.lr <- data.frame("Temp"=seq(0,30,0.01))
peak.sr <- peak.lr <- array(dim=length(taus))
dimnames(peak.sr) <- dimnames(peak.lr) <- list(colnames(prj.sr))
# Loop over taus to calculate peaks
for(tau in colnames(prj.sr)){
  peak <- prj.sr["temp",tau]/(-2*prj.sr["temp2",tau])
  peak.sr[tau] <- peak
  df.sr[,tau] <- prj.sr["temp",tau]*(df.sr$Temp-peak) + prj.sr["temp2",tau]*(df.sr$Temp^2-peak^2)
  
  peak <- prj.lr["dtemp",tau]/(-2*prj.lr["dtemp2",tau])
  df.lr[,tau] <- prj.lr["dtemp",tau]*(df.lr$Temp-peak) + prj.lr["dtemp2",tau]*(df.lr$Temp^2-peak^2)
  peak.lr[tau] <- peak
  
}


# Prep dataframe for plotting
df.sr.plot <- melt(df.sr,id.vars=c("Temp"))
colnames(df.sr.plot) <- c("Temp","tau","value")
levels(df.sr.plot$tau) <- colnames(prj.sr)
df.sr.plot['tau2'] <- rep(taus,each=3001)

df.lr.plot <- melt(df.lr,id.vars=c("Temp"))
colnames(df.lr.plot) <- c("Temp","tau","value")
levels(df.lr.plot$tau) <- colnames(prj.lr)
df.lr.plot['tau2'] <- rep(taus,each=3001)


my_palette = c(brewer.pal(9, "Reds")[3:9])
newcol <- colorRampPalette(my_palette)
my_palette <- newcol(100)


tempplot.sr <- ggplot(data=df.sr.plot) + 
  geom_point(aes(Temp,y=value,color=tau2),size=0.1) + 
  xlab(expression("Temperature ("*degree*"C)")) + ylab('Change in ln(GDP/capita)') +
  scale_x_continuous(limits=c(0,30),expand=c(0,0)) +
  scale_colour_gradientn(breaks=seq(0,100,20),limits=c(0,100),colors=palette(my_palette),name=expression(tau),guide = guide_legend()) +
  theme_classic(base_size=15) + 
  theme(legend.position = "bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.3, "cm")) +
  guides(color = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))

pdf(file = 'tablesandfigures/Figure12.pdf',width = 7,height=5)
print(tempplot.sr)
dev.off()

png(filename = 'tablesandfigures/Figure12.png',width = 1100,height=800)
print(tempplot.sr)
dev.off()


tempplot.lr <- ggplot(data=df.lr.plot) + 
  geom_point(aes(Temp,y=value,color=tau2),size=0.1) + 
  xlab(expression("Temperature ("*degree*"C)")) + ylab('Change in ln(GDP/capita)') +
  scale_x_continuous(limits=c(0,30),expand=c(0,0)) +
  scale_colour_gradientn(breaks=seq(0,100,20),limits=c(0,100),colors=palette(my_palette),name=expression(tau),guide = guide_legend()) +
  theme_classic(base_size=15) + 
  theme(legend.position = "bottom",legend.key.width = unit(2.5, "cm"),legend.key.height = unit(0.3, "cm")) +
  guides(color = guide_colourbar(title.position = "bottom",title.hjust = .5, label.position = "bottom"))

pdf(file = 'tablesandfigures/TauAnalysis_OptimalTemperature_lr.pdf',width = 7,height=5)
print(tempplot.lr)
dev.off()


