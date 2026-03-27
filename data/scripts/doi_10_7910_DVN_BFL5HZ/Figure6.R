######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot peak growth temperatures across regression models (Figure 6)

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
# 1. Optimal Temperature Functions
################################################################################
################################################################################

# Set SSPs
scens <- "SSP"%&%c(1:5)
# Set Years
yrs <- 2010:2099

# Import historical climate-economy regression coefficients
prj <- read.csv("data/output/RegressionCoeff_Projections_UpdatedData.csv",row.names=1,header=F,skip=2) # Regression model coefficients
colnames(prj) = read.csv("data/output/RegressionCoeff_Projections_UpdatedData.csv", header = F, nrows = 1, as.is = T)[-1]
nr = dim(prj)[2]  # number of models analyzed
# Set models
mdls <- "Model "%&%c(1:nr)
#mdlnames <- c("CI-SR-N","CI-SR-L","CI-SR-Q","CC-SR-N","CC-SR-L","CC-LR-N","CC-LR-N-RP (Rich)","CC-LR-N-RP (Poor)")
mdlnames <- c("Column "%&%c(1:8),"Column 9 "%&%c("(Rich)","(Poor)"))




#Initialize dataframe to store results across models
df <- data.frame('Temp'=seq(0,30,0.01))


# Loop over models
for(mdl in c(1:nr)){
  if(is.na(prj["dtemp",mdl])){
    # Check if pooled or rich/poor model
    if(is.na(prj["poor_temp",mdl])){
      # Get temp-growth relationship for pooled models
      df[,"Model "%&%mdl] <- prj["temp",mdl]*df$Temp+prj["temp2",mdl]*df$Temp*df$Temp-max(prj["temp",mdl]*df$Temp+prj["temp2",mdl]*df$Temp*df$Temp)
    }else{
      # Get seperate temp-growth relationship for rich/poor models
      df[,"Model "%&%mdl%&%"Rich"] <- prj["temp",mdl]*df$Temp+prj["temp2",mdl]*df$Temp*df$Temp-max(prj["temp",mdl]*df$Temp+prj["temp2",mdl]*df$Temp*df$Temp)
      df[,"Model "%&%mdl%&%"Poor"] <- (prj["temp",mdl]+prj["poor_temp",mdl])*df$Temp+(prj["temp2",mdl]+prj["poor_temp2",mdl])*df$Temp*df$Temp-max((prj["temp",mdl]+prj["poor_temp",mdl])*df$Temp+(prj["temp2",mdl]+prj["poor_temp2",mdl])*df$Temp*df$Temp)
    }
  }else{
    # Check if pooled or rich/poor model
    if(is.na(prj["poor_dtemp",mdl])){
      # Get temp-growth relationship for pooled models
      # df[,"Compare"%&%mdl%&%"Growth"] <- prj["temp",mdl]*df$Temp+prj["temp2",mdl]*df$Temp*df$Temp-max(prj["temp",mdl]*df$Temp+prj["temp2",mdl]*df$Temp*df$Temp)
      df[,"Model "%&%mdl] <- prj["dtemp",mdl]*df$Temp+prj["dtemp2",mdl]*df$Temp*df$Temp-max(prj["dtemp",mdl]*df$Temp+prj["dtemp2",mdl]*df$Temp*df$Temp)
    }else{
      # Get seperate temp-growth relationship for rich/poor models
      # df[,"Compare"%&%mdl%&%"Rich"%&%"Growth"] <- prj["temp",mdl]*df$Temp+prj["temp2",mdl]*df$Temp*df$Temp-max(prj["temp",mdl]*df$Temp+prj["temp2",mdl]*df$Temp*df$Temp)
      # df[,"Compare"%&%mdl%&%"Poor"%&%"Growth"] <- (prj["temp",mdl]+prj["poor_temp",mdl])*df$Temp+(prj["temp2",mdl]+prj["poor_temp2",mdl])*df$Temp*df$Temp-max((prj["temp",mdl]+prj["poor_temp",mdl])*df$Temp+(prj["temp2",mdl]+prj["poor_temp2",mdl])*df$Temp*df$Temp)
      df[,"Model "%&%mdl%&%" (Rich)"] <- prj["dtemp",mdl]*df$Temp+prj["dtemp2",mdl]*df$Temp*df$Temp-max(prj["dtemp",mdl]*df$Temp+prj["dtemp2",mdl]*df$Temp*df$Temp)
      df[,"Model "%&%mdl%&%" (Poor)"] <- (prj["dtemp",mdl]+prj["poor_dtemp",mdl])*df$Temp+(prj["dtemp2",mdl]+prj["poor_dtemp2",mdl])*df$Temp*df$Temp-max((prj["dtemp",mdl]+prj["poor_dtemp",mdl])*df$Temp+(prj["dtemp2",mdl]+prj["poor_dtemp2",mdl])*df$Temp*df$Temp)
    }
  }
}


# Prep dataframe for plotting
df.long <- melt(df,id.vars=c("Temp"))
colnames(df.long) <- c("Temp","Model","value")
levels(df.long$Model) <- mdlnames

# Set color palette
my_palette = c(brewer.pal(7, "Set1"),brewer.pal(7, "Set1")[7])
my_palette = c("#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438","#619438")

# Plot temperature-growth relationships across models
tempplot <- ggplot(data=df.long,aes(x=Temp,y=value,colour=Model,linetype=Model)) + 
  geom_line(size=1) + 
  theme_classic(base_size=15) + 
  xlab(expression("Temperature ("*degree*"C)")) + ylab('Change in ln(GDP/capita)') +
  scale_x_continuous(limits=c(0,30),expand=c(0,0)) +
  scale_colour_manual(values=palette(my_palette)) +
  scale_linetype_manual(values=c(rep(1,9),2))



# Export plot
pdf(file = 'tablesandfigures/Figure6.pdf',width = 9,height=5)
print(tempplot)
dev.off()












