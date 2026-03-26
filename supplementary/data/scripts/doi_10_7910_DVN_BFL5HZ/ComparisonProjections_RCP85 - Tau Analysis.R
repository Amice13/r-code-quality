######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy

# Script to calculate economic projections under RCP8.5
# Code adapted from replication code for Burke et al. (2015)

# 1st import and prep data
# 2nd generate projections
######


rm(list = ls())

require(data.table)
require(dplyr)
library(ggplot2)
library(xtable)
library(reshape2)
"%&%"<-function(x,y)paste(x,y,sep="")  #define a function for easy string pasting

# Set working Directory
path = "C:/Users/aharding6/Dropbox (GaTech)/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)



################################################################################
################################################################################
# 1. Import and Prep Data
################################################################################
################################################################################

################################################################################
# Step 1. Import Data 
################################################################################
# Import historical data (generated in ComparisonRegressions.do)
dta <- read.csv("data/output/mainDataset.csv") # Udel_precip data is in mm/year 

# Import SSP population and growthprojections
pop <- read.csv("data/input/SSP/SSP_PopulationProjections.csv")
growth <- read.csv("data/input/SSP/SSP_GrowthProjections.csv")

# Import temperature and precipitation projections
Tchg <- read.csv("data/input/CCprojections/CountryTempChange_RCP85.csv")
Pchg <- read.csv("data/input/CCprojections/CountryPrecipChange_RCP85.csv")


# Set projection years
yrs <- 2010:2099

################################################################################
# End Step 1 - End Step 1 - End Step 1 - End Step 1 - End Step 1 - End Step 1 
################################################################################


################################################################################
# Step 2 - Prep historical data
################################################################################
# Calculate mean temp, precip, and gdpcap from 1980-2010
mt <- dta %>%   #the following few lines gets the average temperature in each country for the years we want, using dplyr
  filter(year>=1980 & is.na(temp)==F & is.na(growthWDI)==F & is.na(prec)==F) %>% # ARH - added filter for UDel_precip_popweight
  group_by(iso) %>% 
  summarize(meantemp = mean(temp,na.rm=T), basegrowth = mean(growthWDI, na.rm=T), gdpCap = mean(gdpCAP_wdi,na.rm=T), meanprecip = mean(prec, na.rm=T)) # ARH - added mean(UDel_precip_popweight)
mt <- as.data.frame(mt)

################################################################################
# End Step 2 - End Step 2 - End Step 2 - End Step 2 - End Step 2 - End Step 2 
################################################################################

################################################################################
# Step 3 - Prep SSP growth and population projections 
################################################################################
# FIRST DEFINE FUNCTION TO INTERPOLATE SSP POPULATION AND GROWTH DATASETS.  THESE COME AS 5-YR PROJECTIONS. we want opbs for each year for each projection, so linearly interpolate between 5-yr estimates
#   writing this as a function that spits out a data frame, where the first three columns gives the scenario and country, and the rest give the projections by year
#   growth projections only go through 2095, so repeating the 2095 value for 2096-2099
ipolate <- function(mat) {
  mat1 <- array(dim=c(dim(mat)[1],length(yrs)))
  ys <- as.numeric(unlist(strsplit(names(mat),"X")))
  est <- seq(2010,2100,5)  #the 5yr estimates in the SSP dataset
  for (i in 1:length(yrs)) {
    y = yrs[i]
    if ("X"%&%y %in% names(pop) == T) {  #if the year falls on the 5-yr interval, use their point estimate. otherwise interpolate between nearest endpoints
      mat1[,i]  <- as.numeric(mat[,which(names(mat)=="X"%&%y)])
    } else {
      z <- y-est
      yl = est[which(z==min(z[z>0]))]  #the 5-year endpoint lower than the year
      y5 = yl+5  #the next endpoint
      el <- as.numeric(mat[,which(names(mat)=="X"%&%yl)])  #values at lower endpoint
      eu <- as.numeric(mat[,which(names(mat)=="X"%&%y5)]) #values at upper endpoint
      if (y > max(ys,na.rm=T)) {  mat1[,i] <- el   #this is to account for growth projections ending in 2095  
      }  else { mat1[,i] <- el + (eu-el)*(y-yl)/5 }
    }
  } 
  mat1 <- data.frame(mat[,1:3],mat1)
  names(mat1)[4:dim(mat1)[2]] <- yrs
  mat1[mat1$Region=="COD","Region"] <- "ZAR"  #our code for the DRC
  mat1[mat1$Region=="ROU","Region"] <- "ROM"  #our code for Romania 
  return(mat1)
}

# Initialize lists to store population and growth projections
popProjections <- NULL  #initialize list that we will will with population projections for each scenario
growthProjections <- NULL   #same for growth

# Interpolate SSP data
pop[pop$Scenario=="SSP4d_v9_130115","Scenario"] <- "SSP4_v9_130115"  #renaming one of the scenarios slightly so the loop works
pop1 <- ipolate(pop)  #warning here is just from stringsplit function
growth1 <- ipolate(growth)
growth1[,names(growth1)%in%yrs] = growth1[,names(growth1)%in%yrs]/100
#   First we merge countries in historical database with the growth and pop projections from SSP, restricted to the scenario we want
# we are using growth projections from OECD, which are the only ones with data for every country; population projections are from IIASA
popSSP <- merge(mt,pop1,by.x="iso",by.y="Region")  #merge our data and SSP for population
# length(unique(popproj$iso))  #165 countries that we can match in our data to SSP data
growthSSP <- merge(mt,growth1,by.x="iso",by.y="Region")

for (scen in 1:5) {  #now add each scenario to the list
  # projections for economic growth - using OECD, because they have projections for every country
  pgrow <- growthSSP$Model=="OECD Env-Growth" & growthSSP$Scenario=="SSP"%&%scen%&%"_v9_130325"
  growthProjections[[scen]] <- growthSSP[pgrow,]
  
  # population projections from IIASA
  ppop <- popSSP$Scenario=="SSP"%&%scen%&%"_v9_130115"
  popProjections[[scen]] <- popSSP[ppop,]
}

# SAVE THIS SCENARIO DATA TO BE USED IN CONSTRUCTION OF DAMAGE FUNCTION
save(popProjections,file="data/output/projectionOutput/popProjections.Rdata")
save(growthProjections,file="data/output/projectionOutput/growthProjections.Rdata")

################################################################################
# End Step 3 - End Step 3 - End Step 3 - End Step 3 - End Step 3 - End Step 3
################################################################################


################################################################################
# Step 4 - Prep climate projection
################################################################################

# Finally, read in projections of future temperature change, generated by the getTemperatureChange.R script
#   These are population-weighted country level projections averaged across all CMIP5 models
Tchg <- read.csv("data/input/CCprojections/CountryTempChange_RCP85.csv")
Tchg <- merge(popProjections[[1]][,1:3],Tchg,by.x="iso",by.y="GMI_CNTRY")
Tchg <- Tchg[Tchg$CNTRY_NAME%in%c("West Bank","Gaza Strip","Bouvet Island")==F,]
Tchg <- Tchg$Tchg # just keep the vector of temperature changes, since sort order is now correct


# ARH- Added read in of projections of future precipitation change, generated by the getPrecipitationChange.R script
Pchg <- read.csv("data/input/CCprojections/CountryPrecipChange_RCP85.csv")
Pchg <- merge(popProjections[[1]][,1:3],Pchg,by.x="iso",by.y="GMI_CNTRY")
Pchg <- Pchg[Pchg$CNTRY_NAME%in%c("West Bank","Gaza Strip","Bouvet Island")==F,]
Pchg <- Pchg$Pchg # just keep the vector of temperature changes, since sort order is now correct

# set temperature change for all runs
dtm <- Tchg   #the country-specific changes
scens <- c("SSP"%&%1:5)
ccd <- dtm/length(yrs)  #rate of increase in temperature per year.
GlobalTchg <- read.csv("data/input/CCprojections/GlobalTempChange_RCP85.csv") # Get global temperature change from climate change
GlobalTchg <- GlobalTchg$x # ARH - convert from df to value
GlobalTchgPY <- GlobalTchg/length(yrs)

# ARH - added set precipitation change for all runs
dtmp <- Pchg  #the country-specific changes
ccdp <- dtmp/length(yrs)  #rate of increase in precipitation per year. 
GlobalPchg <- read.csv("data/input/CCprojections/GlobalPrecipChange_RCP85.csv") # Get global precipitation change from climate change
GlobalPchg <- GlobalPchg$x # convert from df to value
GlobalPchgPY <- GlobalPchg/length(yrs)

################################################################################
# End Step 4 - End Step 4 - End Step 4 - End Step 4 - End Step 4 - End Step 4
################################################################################




################################################################################
################################################################################
# 2. Calculate Projections
################################################################################
################################################################################

headers <- read.csv("data/output/TauAnalysis_SR.csv", header = F, nrows = 1, as.is = T)
prj.sr <- read.csv("data/output/TauAnalysis_SR.csv",row.names=1,skip=2,header=F)
prj.lr <- read.csv("data/output/TauAnalysis_LR.csv",row.names=1,skip=2,header=F)
colnames(prj.sr) <- colnames(prj.lr) <- headers[,-1]




prj <- prj.sr
# Loop over models
for (mdl in colnames(prj.sr)){
  print("Compare"%&%mdl)
  # Loop over SSP scenarios
  for (scen in c(5)) {
    print("SSP"%&%scen)
    # Retreive relevent SSP projection
    growthproj <- growthProjections[[scen]]  #select growth projections
    popproj <- popProjections[[scen]]  # select population projections
    
    # Get mean historical measures
    basegdp <- popproj$gdpCap  #baseline GDP/cap
    medgdp <- median(basegdp) # Median GDP/cap avg. used for poor indicator
    temp <- popproj$meantemp  #baseline temperature.
    precip <- popproj$meanprecip # baseline precipitation
    
    # Initialize arrays to store economic data
    GDPcapCC = GDPcapNoCC = array(dim=c(dim(growthproj)[1],length(yrs)))  #array to fill with GDP/cap for each country
    dimnames(GDPcapCC) <- dimnames(GDPcapNoCC) <- list(growthproj[,1],yrs)
    GDPcapCC[,1] = GDPcapNoCC[,1] = basegdp  #initialize with baseline per cap GDP
    tots = array(dim=c(length(yrs),4))  #array to hold average global per cap GDP and total global GDP across scenarios, with and without climate change
    dimnames(tots) <- list(yrs,c("avgGDPcapCC","avgGDPcapNoCC","TotGDPCC","TotGDPNoCC"))
    
    # Initialize arrays to store climate data
    dfTemp = dfPrecip = array(dim=c(dim(growthproj)[1],length(yrs))) # Create array to store temperatures and precipitations
    dimnames(dfTemp) <- dimnames(dfPrecip) <- list(growthproj[,1],yrs) # ARH - set dimension names
    dfTemp[,1] <- growthproj$meantemp # Initialize with initial temps
    dfPrecip[,1] <- popproj$meanprecip # Initialize with initial temps
    dfGlobalTemp = dfGlobalPrecip = array(dim=length(yrs))  # set dimension names
    dimnames(dfGlobalTemp) <- dimnames(dfGlobalPrecip) <- list(yrs) # set dimension names
    dfGlobalTemp[1] <- dfGlobalPrecip[1] <- 0 # Initialize chg in global temp and precip with 0
    
    # Loop over years
    for (i in 2:length(yrs)) {
      j = i - 1
      y = yrs[i]
      
      # Calculate baseline growth and store (SSP projected growth)
      basegrowth <- growthproj[,which(names(growthproj)==y)]  #growth rate without climate change
      GDPcapNoCC[,i] = GDPcapNoCC[,j]*(1+basegrowth)  #last year's per cap GDP times this years growth rate, as projected by scenario
      
      # Check which countries were poor in previous period of climate change world
      poor <- GDPcapCC[,j]<=medgdp
      
      # Calculate baseline contribution of climate to growth
      bg = prj["temp",mdl]*temp + prj["temp2",mdl]*temp*temp + prj["prec",mdl]*precip + prj["prec2",mdl]*precip*precip + 
        ifelse(!is.na(prj["loggdpCAP_wdi_l",mdl]),prj["loggdpCAP_wdi_l",mdl],0)*log(GDPcapNoCC[,j]) + 
        ifelse(!is.na(prj["growthWDI_l",mdl]),prj["growthWDI_l",mdl],0)*(ifelse(j==1,0,log(GDPcapNoCC[,j])-log(GDPcapNoCC[,j-1])))  #this finds the predicted growth level for each country's temperature for the particular model
      if(!is.na(prj["poor_temp",mdl])){
        bg[poor] = bg[poor] + prj["poor_temp",mdl]*temp[poor] + prj["poor_temp2",mdl]*temp[poor]*temp[poor] + prj["poor_prec",mdl]*precip[poor] + prj["poor_prec2",mdl]*precip[poor]*precip[poor] + ifelse(!is.na(prj["poor_loggdpCAP_wdi_l",mdl]),prj["poor_loggdpCAP_wdi_l",mdl],0)*log(GDPcapNoCC[poor,j]) + ifelse(!is.na(prj["poor_growthWDI_l",mdl]),prj["poor_growthWDI_l",mdl],0)*(ifelse(j==1,0,log(GDPcapNoCC[poor,j])-log(GDPcapNoCC[poor,j-1])))  #this finds the predicted growth level for each country's temperature for the particular model
      }
      
      # Calculate new temperature and precipitation
      newtemp = temp+j*ccd
      newprecip = precip+j*ccdp # added new precip calculation
      newtemp[newtemp>30] = 30 # Constrain temp to below 30
      newprecip[newprecip<0] = 0 # Constrain precip to above 0
      oldtemp = temp+(j-1)*ccd
      oldprecip = precip+(j-1)*ccdp # added new precip calculation
      oldtemp[oldtemp>30] = 30 # Constrain temp to below 30
      oldprecip[oldprecip<0] = 0 # Constrain precip to above 0
      
      # Calculate contribution of new climate to growth
      dg = prj["temp",mdl]*newtemp + prj["temp2",mdl]*newtemp*newtemp + prj["prec",mdl]*newprecip + prj["prec2",mdl]*newprecip*newprecip + 
        ifelse(!is.na(prj["dtemp",mdl]),prj["dtemp",mdl]*(newtemp-oldtemp) + prj["dtemp2",mdl]*(newtemp*newtemp-oldtemp*oldtemp) + prj["dprec",mdl]*(newprecip-oldprecip) + prj["dprec2",mdl]*(newprecip*newprecip-oldprecip*oldprecip),0) + 
        ifelse(!is.na(prj["loggdpCAP_wdi_l",mdl]),prj["loggdpCAP_wdi_l",mdl],0)*log(GDPcapCC[,j]) + ifelse(!is.na(prj["growthWDI_l",mdl]),prj["growthWDI_l",mdl],0)*(ifelse(j==1,0,log(GDPcapNoCC[,j])-log(GDPcapNoCC[,j-1])))  #this finds the predicted growth level for each country's temperature for the particular model
      if(!is.na(prj["poor_temp",mdl])){
        dg[poor] = dg[poor] + prj["poor_temp",mdl]*newtemp[poor] + prj["poor_temp2",mdl]*newtemp[poor]*newtemp[poor] + prj["poor_prec",mdl]*newprecip[poor] + prj["poor_prec2",mdl]*newprecip[poor]*newprecip[poor] + ifelse(!is.na(prj["poor_dtemp",mdl]),prj["poor_dtemp",mdl]*(newtemp[poor]-oldtemp[poor]) + prj["poor_dtemp2",mdl]*(newtemp[poor]*newtemp[poor]-oldtemp[poor]*oldtemp[poor]) + prj["poor_dprec",mdl]*(newprecip[poor]-oldprecip[poor]) + prj["poor_dprec2",mdl]*(newprecip[poor]*newprecip[poor]-oldprecip[poor]*oldprecip[poor]),0) + ifelse(!is.na(prj["poor_loggdpCAP_wdi_l",mdl]),prj["poor_loggdpCAP_wdi_l",mdl],0)*log(GDPcapCC[poor,j]) + ifelse(!is.na(prj["poor_growthWDI_l",mdl]),prj["poor_growthWDI_l",mdl],0)*(ifelse(j==1,0,log(GDPcapNoCC[poor,j])-log(GDPcapNoCC[poor,j-1]))) #this finds the predicted growth level for each country's temperature for the particular model
      }
      
      # Calculate difference in growth rates and store new projected GDP/capita
      diff = dg - bg  #difference between predicted baseline growth and predicted growth under new temp
      GDPcapCC[,i] = GDPcapCC[,j]*(1+basegrowth + diff)  #last year's GDPcap (w/ climate change) times climate-adjusted growth rate for this year
      
      # Store changes in climate
      dfTemp[,i] = newtemp # record temps in dataframe
      dfPrecip[,i] = newprecip # record precip in dataframe
      dfGlobalTemp[i] = j*GlobalTchgPY # record chg in global temp in dataframe
      dfGlobalPrecip[i] = j*GlobalPchgPY # record chg in global precip in dataframe
      
      # Store aggregate economic outcomes
      # Calculate global average per cap GDP, weighting by population
      wt = popproj[,which(names(popproj)==y)]  #population weights 
      tots[i,1] <- round(weighted.mean(GDPcapCC[,i],wt),3)  # per cap GDP, with climate change
      tots[i,2] <- round(weighted.mean(GDPcapNoCC[,i],wt),3)  # per cap GDP, no climate change
      # Calculate total GDP with and without climate change. multiplying by 1e6 because population is in millions
      tots[i,3] <- sum(GDPcapCC[,i]*wt*1e6)  #with climate change
      tots[i,4] <- sum(GDPcapNoCC[,i]*wt*1e6) #without climate change
    }
    
    # Round economic outcomes
    GDPcapCC <- round(GDPcapCC,3) #round to nearest dollar
    GDPcapNoCC <- round(GDPcapNoCC,3)  #ditto
    
    # Export Results
    save(GDPcapCC,file="data/output/projectionOutput/TauAnalysis/GDPcapCC_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%"_sr.Rdata")
    save(GDPcapNoCC,file="data/output/projectionOutput/TauAnalysis/GDPcapNoCC_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%"_sr.Rdata")
    save(tots,file="data/output/projectionOutput/TauAnalysis/GlobalChanges_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%"_sr.Rdata")
    GDPcapCC <- GDPcapNoCC <- NULL
  }
}




prj <- prj.lr
# Loop over models
for (mdl in colnames(prj.sr)){
  print("Compare"%&%mdl)
  # Loop over SSP scenarios
  for (scen in c(5)) {
    print("SSP"%&%scen)
    # Retreive relevent SSP projection
    growthproj <- growthProjections[[scen]]  #select growth projections
    popproj <- popProjections[[scen]]  # select population projections
    
    # Get mean historical measures
    basegdp <- popproj$gdpCap  #baseline GDP/cap
    medgdp <- median(basegdp) # Median GDP/cap avg. used for poor indicator
    temp <- popproj$meantemp  #baseline temperature.
    precip <- popproj$meanprecip # baseline precipitation
    
    # Initialize arrays to store economic data
    GDPcapCC = GDPcapNoCC = array(dim=c(dim(growthproj)[1],length(yrs)))  #array to fill with GDP/cap for each country
    dimnames(GDPcapCC) <- dimnames(GDPcapNoCC) <- list(growthproj[,1],yrs)
    GDPcapCC[,1] = GDPcapNoCC[,1] = basegdp  #initialize with baseline per cap GDP
    tots = array(dim=c(length(yrs),4))  #array to hold average global per cap GDP and total global GDP across scenarios, with and without climate change
    dimnames(tots) <- list(yrs,c("avgGDPcapCC","avgGDPcapNoCC","TotGDPCC","TotGDPNoCC"))
    
    # Initialize arrays to store climate data
    dfTemp = dfPrecip = array(dim=c(dim(growthproj)[1],length(yrs))) # Create array to store temperatures and precipitations
    dimnames(dfTemp) <- dimnames(dfPrecip) <- list(growthproj[,1],yrs) # ARH - set dimension names
    dfTemp[,1] <- growthproj$meantemp # Initialize with initial temps
    dfPrecip[,1] <- popproj$meanprecip # Initialize with initial temps
    dfGlobalTemp = dfGlobalPrecip = array(dim=length(yrs))  # set dimension names
    dimnames(dfGlobalTemp) <- dimnames(dfGlobalPrecip) <- list(yrs) # set dimension names
    dfGlobalTemp[1] <- dfGlobalPrecip[1] <- 0 # Initialize chg in global temp and precip with 0
    
    # Loop over years
    for (i in 2:length(yrs)) {
      j = i - 1
      y = yrs[i]
      
      # Calculate baseline growth and store (SSP projected growth)
      basegrowth <- growthproj[,which(names(growthproj)==y)]  #growth rate without climate change
      GDPcapNoCC[,i] = GDPcapNoCC[,j]*(1+basegrowth)  #last year's per cap GDP times this years growth rate, as projected by scenario
      
      # Check which countries were poor in previous period of climate change world
      poor <- GDPcapCC[,j]<=medgdp
      
      # Calculate baseline contribution of climate to growth
      bg = prj["temp",mdl]*temp + prj["temp2",mdl]*temp*temp + prj["prec",mdl]*precip + prj["prec2",mdl]*precip*precip + 
        ifelse(!is.na(prj["loggdpCAP_wdi_l",mdl]),prj["loggdpCAP_wdi_l",mdl],0)*log(GDPcapNoCC[,j]) + 
        ifelse(!is.na(prj["growthWDI_l",mdl]),prj["growthWDI_l",mdl],0)*(ifelse(j==1,0,log(GDPcapNoCC[,j])-log(GDPcapNoCC[,j-1])))  #this finds the predicted growth level for each country's temperature for the particular model
      if(!is.na(prj["poor_temp",mdl])){
        bg[poor] = bg[poor] + prj["poor_temp",mdl]*temp[poor] + prj["poor_temp2",mdl]*temp[poor]*temp[poor] + prj["poor_prec",mdl]*precip[poor] + prj["poor_prec2",mdl]*precip[poor]*precip[poor] + ifelse(!is.na(prj["poor_loggdpCAP_wdi_l",mdl]),prj["poor_loggdpCAP_wdi_l",mdl],0)*log(GDPcapNoCC[poor,j]) + ifelse(!is.na(prj["poor_growthWDI_l",mdl]),prj["poor_growthWDI_l",mdl],0)*(ifelse(j==1,0,log(GDPcapNoCC[poor,j])-log(GDPcapNoCC[poor,j-1])))  #this finds the predicted growth level for each country's temperature for the particular model
      }
      
      # Calculate new temperature and precipitation
      newtemp = temp+j*ccd
      newprecip = precip+j*ccdp # added new precip calculation
      newtemp[newtemp>30] = 30 # Constrain temp to below 30
      newprecip[newprecip<0] = 0 # Constrain precip to above 0
      oldtemp = temp+(j-1)*ccd
      oldprecip = precip+(j-1)*ccdp # added new precip calculation
      oldtemp[oldtemp>30] = 30 # Constrain temp to below 30
      oldprecip[oldprecip<0] = 0 # Constrain precip to above 0
      
      # Calculate contribution of new climate to growth
      dg = prj["temp",mdl]*newtemp + prj["temp2",mdl]*newtemp*newtemp + prj["prec",mdl]*newprecip + prj["prec2",mdl]*newprecip*newprecip + 
        ifelse(!is.na(prj["dtemp",mdl]),prj["dtemp",mdl]*(newtemp-oldtemp) + prj["dtemp2",mdl]*(newtemp*newtemp-oldtemp*oldtemp) + prj["dprec",mdl]*(newprecip-oldprecip) + prj["dprec2",mdl]*(newprecip*newprecip-oldprecip*oldprecip),0) + 
        ifelse(!is.na(prj["loggdpCAP_wdi_l",mdl]),prj["loggdpCAP_wdi_l",mdl],0)*log(GDPcapCC[,j]) + ifelse(!is.na(prj["growthWDI_l",mdl]),prj["growthWDI_l",mdl],0)*(ifelse(j==1,0,log(GDPcapNoCC[,j])-log(GDPcapNoCC[,j-1])))  #this finds the predicted growth level for each country's temperature for the particular model
      if(!is.na(prj["poor_temp",mdl])){
        dg[poor] = dg[poor] + prj["poor_temp",mdl]*newtemp[poor] + prj["poor_temp2",mdl]*newtemp[poor]*newtemp[poor] + prj["poor_prec",mdl]*newprecip[poor] + prj["poor_prec2",mdl]*newprecip[poor]*newprecip[poor] + ifelse(!is.na(prj["poor_dtemp",mdl]),prj["poor_dtemp",mdl]*(newtemp[poor]-oldtemp[poor]) + prj["poor_dtemp2",mdl]*(newtemp[poor]*newtemp[poor]-oldtemp[poor]*oldtemp[poor]) + prj["poor_dprec",mdl]*(newprecip[poor]-oldprecip[poor]) + prj["poor_dprec2",mdl]*(newprecip[poor]*newprecip[poor]-oldprecip[poor]*oldprecip[poor]),0) + ifelse(!is.na(prj["poor_loggdpCAP_wdi_l",mdl]),prj["poor_loggdpCAP_wdi_l",mdl],0)*log(GDPcapCC[poor,j]) + ifelse(!is.na(prj["poor_growthWDI_l",mdl]),prj["poor_growthWDI_l",mdl],0)*(ifelse(j==1,0,log(GDPcapNoCC[poor,j])-log(GDPcapNoCC[poor,j-1]))) #this finds the predicted growth level for each country's temperature for the particular model
      }
      
      # Calculate difference in growth rates and store new projected GDP/capita
      diff = dg - bg  #difference between predicted baseline growth and predicted growth under new temp
      GDPcapCC[,i] = GDPcapCC[,j]*(1+basegrowth + diff)  #last year's GDPcap (w/ climate change) times climate-adjusted growth rate for this year
      
      # Store changes in climate
      dfTemp[,i] = newtemp # record temps in dataframe
      dfPrecip[,i] = newprecip # record precip in dataframe
      dfGlobalTemp[i] = j*GlobalTchgPY # record chg in global temp in dataframe
      dfGlobalPrecip[i] = j*GlobalPchgPY # record chg in global precip in dataframe
      
      # Store aggregate economic outcomes
      # Calculate global average per cap GDP, weighting by population
      wt = popproj[,which(names(popproj)==y)]  #population weights 
      tots[i,1] <- round(weighted.mean(GDPcapCC[,i],wt),3)  # per cap GDP, with climate change
      tots[i,2] <- round(weighted.mean(GDPcapNoCC[,i],wt),3)  # per cap GDP, no climate change
      # Calculate total GDP with and without climate change. multiplying by 1e6 because population is in millions
      tots[i,3] <- sum(GDPcapCC[,i]*wt*1e6)  #with climate change
      tots[i,4] <- sum(GDPcapNoCC[,i]*wt*1e6) #without climate change
    }
    
    # Round economic outcomes
    GDPcapCC <- round(GDPcapCC,3) #round to nearest dollar
    GDPcapNoCC <- round(GDPcapNoCC,3)  #ditto
    
    # Export Results
    save(GDPcapCC,file="data/output/projectionOutput/TauAnalysis/GDPcapCC_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%"_lr.Rdata")
    save(GDPcapNoCC,file="data/output/projectionOutput/TauAnalysis/GDPcapNoCC_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%"_lr.Rdata")
    save(tots,file="data/output/projectionOutput/TauAnalysis/GlobalChanges_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%"_lr.Rdata")
    GDPcapCC <- GDPcapNoCC <- NULL
  }
}




