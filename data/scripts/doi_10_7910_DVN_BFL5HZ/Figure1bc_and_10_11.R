######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to plot projected global losses (change in avg GDP/cap)

# 1st Projected global losses over time
# 2nd Projected global losses vs. temperature change

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
# 1. Projected global losses over time (Figure 1b, Figure 10)
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
mdlnames <- c("Column "%&%c(1:9))

# Loop over SSPs
for(scen in c(1,2,3,4,5)){
  print("SSP"%&%scen)
  # Initialize dataframes to store outcomes
  avgGDPcapdf <- data.frame('Year' = yrs) # average GDP/capita results
  GDPdf <- data.frame('Year' = yrs) # GDP results
  
  # Loop over regression models
  for(mdl in mdls){
    print(mdl)
    # Load global projection outcomes
    load("data/output/projectionOutput/UpdatedData/GlobalChanges_RCP85_"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
    
    # Get average GDP/capita losses (%)
    avgGDPcapdf[,mdl] <- (tots[,1]/tots[,2]-1)*100 # (avgGDPcapCC/avgGDPcapNoCC-1)*100
    avgGDPcapdf[1,mdl] <- 0 # first year is end of historical, so no change
    # Get GDP losses (%)
    GDPdf[,mdl] <- (tots[,3]/tots[,4]-1)*100 # (TotGDPCC/TotGDPNoCC-1)*100
    GDPdf[1,mdl] <- 0 # first year is end of historical, so no change
  }
  colnames(GDPdf) <- c("Year","Model "%&%c(1:nr))
  
  # Prep dataframes for plotting
  avgGDPcapdf.long <- melt(avgGDPcapdf,id.vars=c("Year"))
  GDPdf.long <- melt(GDPdf,id.vars=c("Year"))
  colnames(avgGDPcapdf.long) <- c("Year","Model","GlobalLosses (mean GDP/cap)")
  colnames(GDPdf.long) <- c("Year","Model","GlobalLosses (GDP)")
  levels(avgGDPcapdf.long$Model) <- levels(GDPdf.long$Model) <- mdlnames
  
  
  # Set color palette
  my_palette = c(brewer.pal(7, "Set1"))
  my_palette = c("#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438","black")
  my_palette_bw = rev(c("#4D4D4D",  "#999999",  "#CCCCCC",  "#E5E5E5" ))
  
  # Generate plot for respective SSP
  plotdf <- avgGDPcapdf.long
  plot <- ggplot(plotdf,aes(x=Year,y=`GlobalLosses (mean GDP/cap)`,colour=Model)) + 
    geom_line(size=1) + 
    geom_abline(intercept=0,slope=0,size=0.1) + 
    xlab("Year") + ylab("% change in average GDP/cap") +
    theme_classic(base_size=15)  +
    scale_y_continuous(limits=c(-40,20),breaks=seq(-40,20,10)) +
    scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30)) +
    theme(text = element_text(size=18)) + 
    annotate("text",x=2010,y=20,label='',size=5,hjust=0) +
    scale_color_manual(values=my_palette)
  
  # Export plots
  pdf(file=paste('tablesandfigures/Figure10_',scens[scen],'.pdf',sep=''),height=5,width=8)
  print(plot)
  dev.off()
  
  # Create slimmer version for text
  if(scen==5){
    
    # Generate plot for respective SSP
    plotdf <- avgGDPcapdf.long[avgGDPcapdf.long$Model %in% c(mdlnames[c(2,4,6,7)]),]
    plottext <- ggplot(plotdf,aes(x=Year,y=`GlobalLosses (mean GDP/cap)`,colour=Model)) + 
      geom_line(size=1) + 
      geom_abline(intercept=0,slope=0,size=0.1) + 
      xlab("Year") + ylab("% change in average GDP/cap") +
      theme_classic(base_size=15)  +
      scale_y_continuous(limits=c(-40,20),breaks=seq(-40,20,10)) +
      scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30)) +
      theme(text = element_text(size=18)) + 
      annotate("text",x=2010,y=20,label='',size=5,hjust=0) +
      scale_color_manual(values=my_palette[c(2,4,6,7)])
    
    # Export plots
    pdf(file=paste('tablesandfigures/Figure1b.pdf',sep=''),height=5,width=8)
    print(plottext)
    dev.off()
    
    
    # Generate plot for respective SSP
    plotdf <- avgGDPcapdf.long[avgGDPcapdf.long$Model %in% c(mdlnames[c(2,4,6,7)]),]
    plottext <- ggplot(plotdf,aes(x=Year,y=`GlobalLosses (mean GDP/cap)`,colour=Model)) + 
      geom_line(size=1) + 
      geom_abline(intercept=0,slope=0,size=0.1) + 
      xlab("Year") + ylab("% change in average GDP/cap") +
      theme_classic(base_size=15)  +
      scale_y_continuous(limits=c(-40,20),breaks=seq(-40,20,10)) +
      scale_x_continuous(limits=c(2010,2100),breaks=seq(2010,2100,30)) +
      theme(text = element_text(size=18)) + 
      annotate("text",x=2010,y=20,label='',size=5,hjust=0) +
      scale_color_manual(values=my_palette_bw)
    
    # Export plots
    pdf(file=paste('tablesandfigures/Figure1b_bw.pdf',sep=''),height=5,width=8)
    print(plottext)
    dev.off()
  }
  
  # Write results to a .csv file
  write.csv(avgGDPcapdf.long,paste('tablesandfigures/GlobalGDPcapLosses',scens[scen],'.csv',sep=''))
  write.csv(GDPdf.long,paste('tablesandfigures/GlobalGDPLosses',scens[scen],'.csv',sep=''))
}




################################################################################
################################################################################
# 2. Projected global losses vs temperature (Figure 1c, Figure 11)
################################################################################
################################################################################

################################################################################
# Step 1. Compute damage function at different global temp changes
################################################################################
# Read in SSP projection data generated in ComputeMainProjections.R script
load(file="data/output/projectionOutput/popProjections.Rdata")
load(file="data/output/projectionOutput/growthProjections.Rdata")

# Read in projections of future temperature change, generated by the getTemperatureChange.R script
Tchg <- read.csv("data/input/CCprojections/CountryTempChange_RCP85.csv")
Tchg <- merge(popProjections[[1]][,1:3],Tchg,by.x="iso",by.y="GMI_CNTRY")
Tchg <- Tchg[Tchg$CNTRY_NAME%in%c("West Bank","Gaza Strip","Bouvet Island")==F,]  #these have the same iso code as israel and norway, respectively, which messes up the merge
Tconv <- Tchg$Tconv  #these are the "conversion factors":  i.e. what you multiply the global mean temp by to get the country-level change in temp.  again, this is based on RCP8.5 ensemble mean 

# Read in projections of future precipitation change, generated by the getPrecipitationChange.R script
Pchg <- read.csv("data/input/CCprojections/CountryPrecipChange_RCP85.csv")
Pchg <- merge(popProjections[[1]][,1:3],Pchg,by.x="iso",by.y="GMI_CNTRY")
Pchg <- Pchg[Pchg$CNTRY_NAME%in%c("West Bank","Gaza Strip","Bouvet Island")==F,]
Pconv <- Pchg$Pconv # just keep the vector of temperature changes, since sort order is now correct


# Set SSPs
scens <- "SSP"%&%c(1:5)
# Set Years
yrs <- 2010:2099


# Load IAM results
iam <- read.csv("data/input/IAMdata/ProcessedKoppData.csv")
iam$damages <- -1*iam$damages  #flipping signs to match everything else we have
mods <- c("DICE","FUND","PAGE")
iam <- iam[iam$IAM%in%mods,]
inc <- sort(unique(iam$T[iam$T<=6 & iam$T>1]))  #temperatures under which some IAM was run
incs <- c(0.8,inc)  #adding in a zero increase relative to today (=0.8 increase relative to preindustrial)

# Load regression coefficients
prj <- read.csv("data/output/RegressionCoeff_Projections_UpdatedData.csv",row.names=1,header=F,skip=2) # Regression model coefficients
colnames(prj) = read.csv("data/output/RegressionCoeff_Projections_UpdatedData.csv", header = F, nrows = 1, as.is = T)[-1]

# Set number of regression models
nr = dim(prj)[2]  # number of models analyzed
# Set models
mdls <- "Compare"%&%c(1:nr)
#mdlnames <- c("CI-SR-N","CI-SR-L","CI-SR-Q","CC-SR-N","CC-SR-L","CC-LR-N","CC-LR-N-RP")
mdlnames <- c("Column "%&%c(1:9))


# Loop over regression models
for(mdl in c(1:nr)){
  print("Compare"%&%mdl)
  # Initialize array to hold results
  tots = array(dim=c(length(incs),length(scens),2))  #array to hold total global GDP with and without climate change in 2099, for each scenario and temperature increase
  dimnames(tots) <- list(incs,scens,c("avgGDPcapCC","avgGDPcapNoCC"))
  
  # Loop over temerature rises
  for (dt in 1:length(incs)) { 
    print("Temp chg "%&%dt)
    # Get country-level temperature and precipitation changes for given global temperature rise (relative to pre-industrial assuming increase from pre-industrial to present day of 0.8C)
    dtm <- (incs[dt] - 0.8)*Tconv  # country-specific temperature change for the particular global mean increase, relative to pre-industrial
    dtmp <- (incs[dt] - 0.8)*Pconv  # country-specific precipitation change for the particular global mean increase, relative to pre-industrial
    ccd <- dtm/length(yrs)  # rate of increase in temperature per year.
    ccdp <- dtmp/length(yrs)  # rate of increase in precipitation per year.
    
    # Loop over SSPs 
    for (scen in c(1,2,3,4,5)) {
      print(scens[scen])
      # Get SSP projection
      growthproj <- growthProjections[[scen]]
      popproj <- popProjections[[scen]]
      
      # Get mean historical measures
      basegdp = popproj$gdpCap  #baseline GDP/cap
      temp <- popproj$meantemp #baseline temperature
      precip <- popproj$meanprecip # baseline precipitation
      medgdp <- median(basegdp)
      dfGlobalTemp = if(incs[dt]==0.8){array(0*(yrs-yrs[1]))}else{seq(0,(incs[dt] - 0.8),(incs[dt] - 0.8)/length(yrs))}
      dfGlobalPrecip = array(0*(yrs-yrs[1]))
      
      # Initialize arrays to store economic data
      GDPcapCC = GDPcapNoCC = array(dim=c(dim(growthproj)[1],length(yrs)))  #array to fill with GDP/cap for each country
      dimnames(GDPcapCC) <- dimnames(GDPcapNoCC) <- list(growthproj[,1],yrs)
      GDPcapCC[,1] = GDPcapNoCC[,1] = basegdp  #initialize with baseline per cap GDP
      
      # Loop over years
      for (i in 2:length(yrs)) {
        j = i - 1
        y = yrs[i]
        
        # Calculate baseline growth and store (SSP projected growth)
        basegrowth <- growthproj[,which(names(growthproj)==y)]
        GDPcapNoCC[,i] = GDPcapNoCC[,j]*(1+basegrowth)  #last year's per cap GDP times this years growth rate, as projected by scenario
        
        # Check which countries were poor in previous period of climate change world
        poor <- GDPcapCC[,j]<=medgdp
        
        # Calculate baseline contribution of climate to growth
        bg = prj["temp",mdl]*temp + prj["temp2",mdl]*temp*temp + prj["prec",mdl]*precip + prj["prec2",mdl]*precip*precip + ifelse(!is.na(prj["loggdpCAP_wdi_l",mdl]),prj["loggdpCAP_wdi_l",mdl],0)*log(GDPcapNoCC[,j]) + ifelse(!is.na(prj["noaainnovation",mdl]),sum(prj[c(5:10),mdl]*0),0)  #this finds the predicted growth level for each country's temperature for the particular model
        if(!is.na(prj["poor_temp",mdl])){
          bg[poor] = bg[poor] + prj["poor_temp",mdl]*temp[poor] + prj["poor_temp2",mdl]*temp[poor]*temp[poor] + prj["poor_prec",mdl]*precip[poor] + prj["poor_prec2",mdl]*precip[poor]*precip[poor] + ifelse(!is.na(prj["loggdpCAP_wdi_l",mdl]),prj["loggdpCAP_wdi_l",mdl],0)*log(GDPcapNoCC[poor,j]) #this finds the predicted growth level for each country's temperature for the particular model
        }
        
        # Calculate new temperature and precipitation
        newtemp = temp+j*ccd
        newprecip = precip+j*ccdp # added new precip calculation
        newtemp[newtemp>30] = 30 # Constrain temp to below 30
        newprecip[newprecip<0] = 0 # Constrain precip to above 0
        
        # Calculate contribution of new climate to growth
        dg = prj["temp",mdl]*newtemp + prj["temp2",mdl]*newtemp*newtemp + prj["prec",mdl]*precip + prj["prec2",mdl]*precip*precip + ifelse(!is.na(prj["loggdpCAP_wdi_l",mdl]),prj["loggdpCAP_wdi_l",mdl],0)*log(GDPcapCC[,j]) + ifelse(!is.na(prj["noaainnovation",mdl]),sum(prj[c(5:10),mdl]*rev(c(rep(0,-min(0,i-6)),dfGlobalTemp[max(1,i-5):i]))),0)  #this finds the predicted growth level for each country's temperature for the particular model
        if(!is.na(prj["poor_temp",mdl])){
          dg[poor] = dg[poor] + prj["poor_temp",mdl]*newtemp[poor] + prj["poor_temp2",mdl]*newtemp[poor]*newtemp[poor] + prj["poor_prec",mdl]*precip[poor] + prj["poor_prec2",mdl]*precip[poor]*precip[poor] + ifelse(!is.na(prj["loggdpCAP_wdi_l",mdl]),prj["loggdpCAP_wdi_l",mdl],0)*log(GDPcapCC[poor,j]) #this finds the predicted growth level for each country's temperature for the particular model
        }
        
        # Calculate difference in growth rates and store new projected GDP/capita
        diff = dg - bg  #difference between predicted baseline growth and predicted growth under new temp
        GDPcapCC[,i] = GDPcapCC[,j]*(1+basegrowth + diff)  #last year's GDPcap (w/ climate change) times climate-adjusted growth rate for this year
        
        
      }
      # Calculate global economic outcomes
      wt = popproj[,which(names(popproj)==y)]  #population weights 
      tots[dt,scen,1] <- round(weighted.mean(GDPcapCC[,90],wt),0)
      tots[dt,scen,2] <- round(weighted.mean(GDPcapNoCC[,90],wt),0)
      GDPcapCC <- GDPcapNoCC <- NULL
    }  #end scenario loop
  }  # end temperature increase loop
  
  # Export results
  save(tots,file=paste("data/output/projectionOutput/UpdatedData/DamageFunction_",mdls[mdl],".Rdata",sep=''))
} # end model loop



################################################################################
# End Step 1 - End Step 1 - End Step 1 - End Step 1 - End Step 1 - End Step 1 
################################################################################



################################################################################
# Step 2 - Plot global losses at different global temperature changes
################################################################################

# Loop over SSP
for(scen in c(1,2,3,4,5)){
  print(scens[scen])
  # Initialize object to store results
  imp <- NULL
  
  # Loop over regression models
  for (mdl in c(1:nr)) {
    print(mdls[mdl])
    # Load global economic losses projections
    load(file=paste("data/output/projectionOutput/UpdatedData/DamageFunction_",mdls[mdl],".Rdata",sep=''))
    # Calculate global losses (%)
    chg <- (tots[,,1]/tots[,,2] - 1)*100 # (avgGDPcapCC/avgGDPcapNoCC-1)*100
    chg <- data.frame(incs,chg[,scen],rep(mdlnames[mdl],length(incs)))
    imp <- rbind(imp,chg)
  }
  # Label columns
  colnames(imp) <- c("T","damages","Model")
  
  # Set color palette
  my_palette = c(brewer.pal(7, "Set1"),brewer.pal(8, "Set3")[c(1)],brewer.pal(8, "Set2")[c(3,4)])
  my_palette = c("#FBBC9D","#F55F14","#893206","#B57EDC","#83C0EC","#2B93DE","#673AB7","#94C76B","#619438","#FFA3AF","#C2AF32","#A033C1")
                 # brewer.pal(8, "Set3")[c(1)],brewer.pal(8, "Set2")[c(3,4)])
  my_palette_bw = rev(c(  "#4D4D4D",  "#999999",   "#E5E5E5",  "#4D4D4D",  "#999999",   "#E5E5E5" ))
  
  # Generate plot for respective SSP
  plotdf <- imp
  plot <- ggplot() + 
    geom_line(data=plotdf,aes(x=`T`-0.8,y=`damages`,group=Model,colour=Model,linetype=Model),size=1) + 
    geom_line(data=iam[iam$T<=6,],aes(x=`T`-0.8,y=damages,group=IAM,colour=IAM,linetype=IAM),size=1) +
    geom_abline(intercept=0,slope=0,size=0.1)  +
    xlab(expression("Temperature Change ("*degree*"C)")) + ylab("% change in average GDP/cap") +
    theme_classic(base_size=15)  +
    scale_y_continuous(limits=c(10*floor(min(plotdf$damages)/10),10*ceiling(max(plotdf$damages)/10)),breaks=seq(10*floor(min(plotdf$damages)/10),10*ceiling(max(plotdf$damages)/10),10)) +
    scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1)) +
    theme(text = element_text(size=18)) + 
    scale_color_manual(values=my_palette,breaks=c(mdlnames,"DICE","FUND","PAGE"))+ 
    scale_linetype_manual(values=c(rep("solid",9),rep("dashed",3)),breaks=c(mdlnames,"DICE","FUND","PAGE"))
  
  pdf(file=paste('tablesandfigures/Figure11_',scens[scen],'.pdf',sep=''),height=5,width=8)
  print(plot)
  dev.off()
  
  # Make slimmer plot for text
  if(scen==5){
    # Generate plot for respective SSP
    plotdf <- imp[imp$Model %in% c(mdlnames[c(2,6,7)]),]
    plottext <- ggplot() + 
      geom_line(data=plotdf,aes(x=`T`-0.8,y=`damages`,group=Model,colour=Model,linetype=Model),size=1) + 
      geom_line(data=iam[iam$T<=6,],aes(x=`T`-0.8,y=damages,group=IAM,colour=IAM,linetype=IAM),size=1) +
      geom_abline(intercept=0,slope=0,size=0.1)  +
      xlab(expression("Temperature Change ("*degree*"C)")) + ylab("% change in average GDP/cap") +
      theme_classic(base_size=15)  +
      scale_y_continuous(limits=c(-50,10),breaks=seq(-50,10,10)) +
      scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1)) +
      theme(text = element_text(size=18)) + 
      scale_color_manual(values=my_palette[c(2,6,7,10,11,12)],breaks=c(mdlnames[c(2,6,7)],"DICE","FUND","PAGE"))+ 
      scale_linetype_manual(values=c(rep("solid",3),rep("dashed",3)),breaks=c(mdlnames[c(2,6,7)],"DICE","FUND","PAGE"))
    
    pdf(file=paste('tablesandfigures/Figure1c.pdf',sep=''),height=5,width=8)
    print(plottext)
    dev.off()
    
    # Generate plot for respective SSP
    plotdf <- imp[imp$Model %in% c(mdlnames[c(2,6,7)]),]
    plottext <- ggplot() + 
      geom_line(data=plotdf,aes(x=`T`-0.8,y=`damages`,group=Model,colour=Model,linetype=Model),size=1) + 
      geom_line(data=iam[iam$T<=6,],aes(x=`T`-0.8,y=damages,group=IAM,colour=IAM,linetype=IAM),size=1) +
      geom_abline(intercept=0,slope=0,size=0.1)  +
      xlab(expression("Temperature Change ("*degree*"C)")) + ylab("% change in average GDP/cap") +
      theme_classic(base_size=15)  +
      scale_y_continuous(limits=c(-50,10),breaks=seq(-50,10,10)) +
      scale_x_continuous(limits=c(0,5),breaks=seq(0,5,1)) +
      theme(text = element_text(size=18)) + 
      scale_color_manual(values=my_palette_bw,breaks=c(mdlnames[c(2,6,7)],"DICE","FUND","PAGE"))+ 
      scale_linetype_manual(values=c(rep("solid",3),rep("dashed",3)),breaks=c(mdlnames[c(2,6,7)],"DICE","FUND","PAGE"))
    
    pdf(file=paste('tablesandfigures/Figure1c_bw.pdf',sep=''),height=5,width=8)
    print(plottext)
    dev.off()
  }
}



