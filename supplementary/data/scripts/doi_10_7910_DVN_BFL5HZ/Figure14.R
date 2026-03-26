######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot projected global losses (change in avg GDP/cap)

# 1st Projected global losses over time

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
# 1. Projected global losses over time (Figure 2b, Figure 10)
################################################################################
################################################################################

# Set SSPs
scens <- "SSP"%&%c(1:5)
# Set Years
yrs <- 2010:2099
# Set number of regression models
nr <- 2
# Set models
mdls <- "Compare"%&%c(2,6)
mdls.old <- "Compare"%&%c(2,5)
#mdlnames <- c("CI-SR-N","CI-SR-L","CI-SR-Q","CC-SR-N","CC-SR-L","CC-LR-N","CC-LR-N-RP")
mdlnames <- c("Column "%&%c(2,6))

# Loop over SSPs
scen = 5
print("SSP"%&%scen)
# Initialize dataframes to store outcomes
avgGDPcapdf <- data.frame('Year' = yrs) # average GDP/capita results
avgGDPcapdfnew <- data.frame('Year' = yrs) # average GDP/capita results
GDPdf <- data.frame('Year' = yrs) # GDP results
GDPdfnew <- data.frame('Year' = yrs) # GDP results

# Loop over regression models
for(mdl in mdls.old){
  print(mdl)
  # Load global projection outcomes
  load("data/output/projectionOutput/GlobalChanges_RCP85_"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
  
  # Get average GDP/capita losses (%)
  avgGDPcapdf[,mdl] <- (tots[,1]/tots[,2]-1)*100 # (avgGDPcapCC/avgGDPcapNoCC-1)*100
  avgGDPcapdf[1,mdl] <- 0 # first year is end of historical, so no change
  # Get GDP losses (%)
  GDPdf[,mdl] <- (tots[,3]/tots[,4]-1)*100 # (TotGDPCC/TotGDPNoCC-1)*100
  GDPdf[1,mdl] <- 0 # first year is end of historical, so no change
}
colnames(GDPdf) <- c("Year","Model "%&%c(2,6))
GDPdf$Data <- "Burke et al. (2015)"
colnames(avgGDPcapdf) <- c("Year","Model "%&%c(2,6))
avgGDPcapdf$Data <- "Burke et al. (2015)"

# Loop over regression models
for(mdl in mdls){
  print(mdl)
  # Load global projection outcomes
  load("data/output/projectionOutput/UpdatedData/GlobalChanges_RCP85_"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
  
  # Get average GDP/capita losses (%)
  avgGDPcapdfnew[,mdl] <- (tots[,1]/tots[,2]-1)*100 # (avgGDPcapCC/avgGDPcapNoCC-1)*100
  avgGDPcapdfnew[1,mdl] <- 0 # first year is end of historical, so no change
  # Get GDP losses (%)
  GDPdfnew[,mdl] <- (tots[,3]/tots[,4]-1)*100 # (TotGDPCC/TotGDPNoCC-1)*100
  GDPdfnew[1,mdl] <- 0 # first year is end of historical, so no change
}
colnames(GDPdfnew) <- c("Year","Model "%&%c(2,6))
GDPdfnew$Data <- "Updated"
colnames(avgGDPcapdfnew) <- c("Year","Model "%&%c(2,6))
avgGDPcapdfnew$Data <- "Updated"

GDPdfCombined <- rbind(GDPdf,GDPdfnew)
GDPdfCombined$Data <- as.factor(GDPdfCombined$Data)
avgGDPcapdfCombined <- rbind(avgGDPcapdf,avgGDPcapdfnew)
avgGDPcapdfCombined$Data <- as.factor(avgGDPcapdfCombined$Data)

# Prep dataframes for plotting
avgGDPcapdf.long <- melt(avgGDPcapdfCombined,id.vars=c("Year","Data"))
GDPdf.long <- melt(GDPdfCombined,id.vars=c("Year","Data"))
colnames(avgGDPcapdf.long) <- c("Year","Data","Model","GlobalLosses (mean GDP/cap)")
colnames(GDPdf.long) <- c("Year","Data","Model","GlobalLosses (GDP)")
levels(avgGDPcapdf.long$Model) <- levels(GDPdf.long$Model) <- mdlnames


# Set color palette
my_palette = c(brewer.pal(7, "Set1"))
my_palette = c("#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438","black")



# Create slimmer version for text

  
# Generate plot for respective SSP
plotdf <- avgGDPcapdf.long
plottext <- ggplot(plotdf,aes(x=Year,y=`GlobalLosses (mean GDP/cap)`,colour=Model,linetype=Data)) + 
  geom_line(size=1) + 
  geom_abline(intercept=0,slope=0,size=0.1) + 
  xlab("Year") + ylab("% change in average GDP/cap") +
  theme_classic(base_size=15)  +
  scale_y_continuous(limits=c(-40,20),breaks=seq(-40,20,10)) +
  scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30)) +
  theme(text = element_text(size=18)) + 
  annotate("text",x=2010,y=20,label='',size=5,hjust=0) +
  scale_color_manual(values=my_palette[c(2,6)])

# Export plots
pdf(file=paste('tablesandfigures/Figure14.pdf',sep=''),height=5,width=8)
print(plottext)
dev.off()



