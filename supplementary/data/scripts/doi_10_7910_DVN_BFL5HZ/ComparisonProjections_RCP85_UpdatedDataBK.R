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
# Import historical data (generated in ComparisonRegressions_Text_UpdatedData.do)
dta <- read.csv("data/output/mainDataset_UpdatedData.csv") # Udel_precip data is in mm/year 

# Import SSP population and growthprojections
pop <- read.csv("data/input/SSP/SSP_PopulationProjections.csv")
growth <- read.csv("data/input/SSP/SSP_GrowthProjections.csv")

# Import temperature and precipitation projections
Tchg_RCP85 <- read.csv("data/input/CCprojections/GlobalTempChange_RCP85.csv")
load("Data/output/MWv5/GlobalTemp.dta")
Tchg_hist <- tc
Tinnov <- read.csv("Data/output/NOAATemperatureInnovation.csv")
Tinnov <- c(Tinnov$NOAAinnovation)
names(Tinnov) <- as.character(c(1850:2024))

# Import historical climate-economy regression coefficients
prj <- read.csv("data/output/BKestimates_country.csv",header=F,skip=2)


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
  filter(year>=1980 & is.na(growthWDI)==F) %>% 
  group_by(iso) %>% 
  summarize(basegrowth = mean(growthWDI, na.rm=T), gdpCap = mean(gdpCAP_wdi,na.rm=T)) # ARH - added mean(UDel_precip_popweight)
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
pop1 <- pop1[pop1$Region!="VIR",]
growth1 <- ipolate(growth)
growth1[,names(growth1)%in%yrs] = growth1[,names(growth1)%in%yrs]/100
#   First we merge countries in historical database with the growth and pop projections from SSP, restricted to the scenario we want
# we are using growth projections from OECD, which are the only ones with data for every country; population projections are from IIASA
popSSP <- merge(mt,pop1,by.x="iso",by.y="Region")  #merge our data and SSP for population
# length(unique(popproj$iso))  #165 countries that we can match in our data to SSP data
growthSSP <- merge(mt,growth1,by.x="iso",by.y="Region")
popSSP <- popSSP[popSSP$iso%in%unique(growthSSP$iso),]

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
# Tchg <- seq(0,Tchg_RCP85$x,Tchg_RCP85$x/89)
Tchg <- c(seq(0,1.2,1.2/40),seq((1.2+0.8/49),2,0.8/50))
Tchg <- c(seq(0,1.5,1.5/29),seq(1.5,2,0.5/19),seq(2,2.5,0.5/39))
# Tchg <- Tinnov[86:175]


################################################################################
# End Step 4 - End Step 4 - End Step 4 - End Step 4 - End Step 4 - End Step 4
################################################################################




################################################################################
################################################################################
# 2. Calculate Projections
################################################################################
################################################################################


# Loop over SSP scenarios
# for (scen in c(1,2,3,4,5)) {
scen = 5
  print("SSP"%&%scen)
  # Retreive relevent SSP projection
  growthproj <- growthProjections[[scen]]  #select growth projections
  popproj <- popProjections[[scen]]  # select population projections
  
  # Get mean historical measures
  basegdp <- popproj$gdpCap  #baseline GDP/cap
  medgdp <- median(basegdp) # Median GDP/cap avg. used for poor indicator
  temp <- 0  #baseline temperature.
  
  # Initialize arrays to store economic data
  GDPcapCC = GDPcapNoCC = array(dim=c(dim(growthproj)[1],length(yrs)))  #array to fill with GDP/cap for each country
  dimnames(GDPcapCC) <- dimnames(GDPcapNoCC) <- list(growthproj[,1],yrs)
  GDPcapCC[,1] = GDPcapNoCC[,1] = basegdp  #initialize with baseline per cap GDP
  tots = array(dim=c(length(yrs),4))  #array to hold average global per cap GDP and total global GDP across scenarios, with and without climate change
  dimnames(tots) <- list(yrs,c("avgGDPcapCC","avgGDPcapNoCC","TotGDPCC","TotGDPNoCC"))
  diff <- array(0,dim=(length(yrs)+11))
  
  # Loop over years
  for (i in 2:length(yrs)) {
    j = i - 1
    y = yrs[i]
    
    # Calculate baseline growth and store (SSP projected growth)
    basegrowth <- growthproj[,which(names(growthproj)==y)]  #growth rate without climate change
    GDPcapNoCC[,i] = GDPcapNoCC[,j]*(1+basegrowth)  #last year's per cap GDP times this years growth rate, as projected by scenario
    
    # Calculate baseline contribution of climate to growth
    bg = 0
    
    # Calculate new temperature and precipitation
    newtemp = Tchg[i]
    newtempl1 = Tchg[i-1]
    newtempl2 = if(i>2){Tchg[i-2]}else{0}
    
    # Calculate contribution of new climate to growth
    dg = array(dim=11)
    # dg[1] <-prj[2,2]*newtemp# + prj[2,1]*growtheffect[i-1] + prj[3,1]*growtheffect[i-2] + prj[4,1]*temps[i-1] + prj[5,1]*temps[i-2]
    # dg[2] <-prj[8,2]*newtemp# + prj[2,2]*growtheffect[i-1] + prj[3,2]*growtheffect[i-2] + prj[4,2]*temps[i-1] + prj[5,2]*temps[i-2]
    # dg[3] <- prj[14,2]*newtemp# + prj[2,3]*growtheffect[i-1] + prj[3,3]*growtheffect[i-2] + prj[4,3]*temps[i-1] + prj[5,3]*temps[i-2]
    # dg[4] <- prj[20,2]*newtemp# + prj[2,4]*growtheffect[i-1] + prj[3,4]*growtheffect[i-2] + prj[4,4]*temps[i-1] + prj[5,4]*temps[i-2]
    # dg[5] <- prj[26,2]*newtemp# + prj[2,5]*growtheffect[i-1] + prj[3,5]*growtheffect[i-2] + prj[4,5]*temps[i-1] + prj[5,5]*temps[i-2]
    # dg[6] <- prj[32,2]*newtemp# + prj[2,6]*growtheffect[i-1] + prj[3,6]*growtheffect[i-2] + prj[4,6]*temps[i-1] + prj[5,6]*temps[i-2]
    # dg[7] <- prj[38,2]*newtemp# + prj[2,7]*growtheffect[i-1] + prj[3,7]*growtheffect[i-2] + prj[4,7]*temps[i-1] + prj[5,7]*temps[i-2]
    # dg[8] <- prj[44,2]*newtemp# + prj[2,8]*growtheffect[i-1] + prj[3,8]*growtheffect[i-2] + prj[4,8]*temps[i-1] + prj[5,8]*temps[i-2]
    # dg[9] <- prj[50,2]*newtemp# + prj[2,9]*growtheffect[i-1] + prj[3,9]*growtheffect[i-2] + prj[4,9]*temps[i-1] + prj[5,9]*temps[i-2]
    # dg[10] <- prj[56,2]*newtemp# + prj[2,10]*growtheffect[i-1] + prj[3,10]*growtheffect[i-2] + prj[4,10]*temps[i-1] + prj[5,10]*temps[i-2]
    # dg[11] <- prj[62,2]*newtemp# + prj[2,11]*growtheffect[i-1] + prj[3,11]*growtheffect[i-2] + prj[4,11]*temps[i-1] + prj[5,11]*temps[i-2]
    # dg[1] <- prj[2,2]*newtemp + prj[3,2]*newtempl1 + prj[4,2]*newtempl2# + prj[2,1]*growtheffect[i-1] + prj[3,1]*growtheffect[i-2]
    # dg[2] <- prj[8,2]*newtemp + prj[9,2]*newtempl1 + prj[10,2]*newtempl2# + prj[2,2]*growtheffect[i-1] + prj[3,2]*growtheffect[i-2]
    # dg[3] <- prj[14,2]*newtemp + prj[15,2]*newtempl1 + prj[16,2]*newtempl2# + prj[2,3]*growtheffect[i-1] + prj[3,3]*growtheffect[i-2]
    # dg[4] <- prj[20,2]*newtemp + prj[21,2]*newtempl1 + prj[22,2]*newtempl2# + prj[2,4]*growtheffect[i-1] + prj[3,4]*growtheffect[i-2]
    # dg[5] <- prj[26,2]*newtemp + prj[27,2]*newtempl1 + prj[28,2]*newtempl2# + prj[2,5]*growtheffect[i-1] + prj[3,5]*growtheffect[i-2]
    # dg[6] <- prj[32,2]*newtemp + prj[33,2]*newtempl1 + prj[34,2]*newtempl2# + prj[2,6]*growtheffect[i-1] + prj[3,6]*growtheffect[i-2]
    # dg[7] <- prj[38,2]*newtemp + prj[39,2]*newtempl1 + prj[40,2]*newtempl2# + prj[2,7]*growtheffect[i-1] + prj[3,7]*growtheffect[i-2]
    # dg[8] <- prj[44,2]*newtemp + prj[45,2]*newtempl1 + prj[46,2]*newtempl2# + prj[2,8]*growtheffect[i-1] + prj[3,8]*growtheffect[i-2]
    # dg[9] <- prj[50,2]*newtemp + prj[51,2]*newtempl1 + prj[52,2]*newtempl2# + prj[2,9]*growtheffect[i-1] + prj[3,9]*growtheffect[i-2]
    # dg[10] <- prj[56,2]*newtemp + prj[57,2]*newtempl1 + prj[58,2]*newtempl2# + prj[2,10]*growtheffect[i-1] + prj[3,10]*growtheffect[i-2]
    # dg[11] <- prj[62,2]*newtemp + prj[63,2]*newtempl1 + prj[64,2]*newtempl2# + prj[2,11]*growtheffect[i-1] + prj[3,11]*growtheffect[i-2]
    # dg[1] <- 0*newtemp# + prj[2,1]*growtheffect[i-1] + prj[3,1]*growtheffect[i-2] + prj[4,1]*temps[i-1] + prj[5,1]*temps[i-2]
    # dg[2] <- -0.025*newtemp# + prj[2,2]*growtheffect[i-1] + prj[3,2]*growtheffect[i-2] + prj[4,2]*temps[i-1] + prj[5,2]*temps[i-2]
    # dg[3] <- -0.03*newtemp# + prj[2,3]*growtheffect[i-1] + prj[3,3]*growtheffect[i-2] + prj[4,3]*temps[i-1] + prj[5,3]*temps[i-2]
    # dg[4] <- -0.025*newtemp# + prj[2,4]*growtheffect[i-1] + prj[3,4]*growtheffect[i-2] + prj[4,4]*temps[i-1] + prj[5,4]*temps[i-2]
    # dg[5] <- -0.021*newtemp# + prj[2,5]*growtheffect[i-1] + prj[3,5]*growtheffect[i-2] + prj[4,5]*temps[i-1] + prj[5,5]*temps[i-2]
    # dg[6] <- -0.017*newtemp# + prj[2,6]*growtheffect[i-1] + prj[3,6]*growtheffect[i-2] + prj[4,6]*temps[i-1] + prj[5,6]*temps[i-2]
    # dg[7] <- -0.013*newtemp# + prj[2,7]*growtheffect[i-1] + prj[3,7]*growtheffect[i-2] + prj[4,7]*temps[i-1] + prj[5,7]*temps[i-2]
    # dg[8] <- -0.009*newtemp# + prj[2,8]*growtheffect[i-1] + prj[3,8]*growtheffect[i-2] + prj[4,8]*temps[i-1] + prj[5,8]*temps[i-2]
    # dg[9] <- -0.003*newtemp# + prj[2,9]*growtheffect[i-1] + prj[3,9]*growtheffect[i-2] + prj[4,9]*temps[i-1] + prj[5,9]*temps[i-2]
    # dg[10] <- -0.001*newtemp# + prj[2,10]*growtheffect[i-1] + prj[3,10]*growtheffect[i-2] + prj[4,10]*temps[i-1] + prj[5,10]*temps[i-2]
    # dg[11] <- -0*newtemp# + prj[2,11]*growtheffect[i-1] + prj[3,11]*growtheffect[i-2] + prj[4,11]*temps[i-1] + prj[5,11]*temps[i-2]
    dg[1] <- -.0193258*newtemp# + prj[2,1]*growtheffect[i-1] + prj[3,1]*growtheffect[i-2] + prj[4,1]*temps[i-1] + prj[5,1]*temps[i-2]
    dg[2] <- .0032484*newtemp# + prj[2,2]*growtheffect[i-1] + prj[3,2]*growtheffect[i-2] + prj[4,2]*temps[i-1] + prj[5,2]*temps[i-2]
    dg[3] <- .0141338*newtemp# + prj[2,3]*growtheffect[i-1] + prj[3,3]*growtheffect[i-2] + prj[4,3]*temps[i-1] + prj[5,3]*temps[i-2]
    dg[4] <- -.0282192*newtemp# + prj[2,4]*growtheffect[i-1] + prj[3,4]*growtheffect[i-2] + prj[4,4]*temps[i-1] + prj[5,4]*temps[i-2]
    dg[5] <- -.0384229*newtemp# + prj[2,5]*growtheffect[i-1] + prj[3,5]*growtheffect[i-2] + prj[4,5]*temps[i-1] + prj[5,5]*temps[i-2]
    dg[6] <- -.0296745*newtemp# + prj[2,6]*growtheffect[i-1] + prj[3,6]*growtheffect[i-2] + prj[4,6]*temps[i-1] + prj[5,6]*temps[i-2]
    dg[7] <- .013944*newtemp# + prj[2,7]*growtheffect[i-1] + prj[3,7]*growtheffect[i-2] + prj[4,7]*temps[i-1] + prj[5,7]*temps[i-2]
    dg[8] <- -0*newtemp# + prj[2,8]*growtheffect[i-1] + prj[3,8]*growtheffect[i-2] + prj[4,8]*temps[i-1] + prj[5,8]*temps[i-2]
    dg[9] <- -0*newtemp# + prj[2,9]*growtheffect[i-1] + prj[3,9]*growtheffect[i-2] + prj[4,9]*temps[i-1] + prj[5,9]*temps[i-2]
    dg[10] <- -0*newtemp# + prj[2,10]*growtheffect[i-1] + prj[3,10]*growtheffect[i-2] + prj[4,10]*temps[i-1] + prj[5,10]*temps[i-2]
    dg[11] <- -0*newtemp# + prj[2,11]*growtheffect[i-1] + prj[3,11]*growtheffect[i-2] + prj[4,11]*temps[i-1] + prj[5,11]*temps[i-2]
    # dg[1] <- -0.0001146*newtemp# + prj[2,1]*growtheffect[i-1] + prj[3,1]*growtheffect[i-2] + prj[4,1]*temps[i-1] + prj[5,1]*temps[i-2]
    # dg[2] <- 0.0125696*newtemp# + prj[2,2]*growtheffect[i-1] + prj[3,2]*growtheffect[i-2] + prj[4,2]*temps[i-1] + prj[5,2]*temps[i-2]
    # dg[3] <- -.0053639*newtemp# + prj[2,3]*growtheffect[i-1] + prj[3,3]*growtheffect[i-2] + prj[4,3]*temps[i-1] + prj[5,3]*temps[i-2]
    # dg[4] <- -.0202447*newtemp# + prj[2,4]*growtheffect[i-1] + prj[3,4]*growtheffect[i-2] + prj[4,4]*temps[i-1] + prj[5,4]*temps[i-2]
    # dg[5] <- -.0141773*newtemp# + prj[2,5]*growtheffect[i-1] + prj[3,5]*growtheffect[i-2] + prj[4,5]*temps[i-1] + prj[5,5]*temps[i-2]
    # dg[6] <- -.0151604*newtemp# + prj[2,6]*growtheffect[i-1] + prj[3,6]*growtheffect[i-2] + prj[4,6]*temps[i-1] + prj[5,6]*temps[i-2]
    # dg[7] <- -.0019783*newtemp# + prj[2,7]*growtheffect[i-1] + prj[3,7]*growtheffect[i-2] + prj[4,7]*temps[i-1] + prj[5,7]*temps[i-2]
    # dg[8] <- -0*newtemp# + prj[2,8]*growtheffect[i-1] + prj[3,8]*growtheffect[i-2] + prj[4,8]*temps[i-1] + prj[5,8]*temps[i-2]
    # dg[9] <- -0*newtemp# + prj[2,9]*growtheffect[i-1] + prj[3,9]*growtheffect[i-2] + prj[4,9]*temps[i-1] + prj[5,9]*temps[i-2]
    # dg[10] <- -0*newtemp# + prj[2,10]*growtheffect[i-1] + prj[3,10]*growtheffect[i-2] + prj[4,10]*temps[i-1] + prj[5,10]*temps[i-2]
    # dg[11] <- -0*newtemp# + prj[2,11]*growtheffect[i-1] + prj[3,11]*growtheffect[i-2] + prj[4,11]*temps[i-1] + prj[5,11]*temps[i-2]
    
    
    # Calculate difference in growth rates and store new projected GDP/capita
    diff[i:(i+10)] = diff[i:(i+10)] + dg - bg  #difference between predicted baseline growth and predicted growth under new temp
    GDPcapCC[,i] = GDPcapCC[,j]*(1+basegrowth*(1+diff[i]))  #last year's GDPcap (w/ climate change) times climate-adjusted growth rate for this year
    
    
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
  # save(GDPcapCC,file="data/output/projectionOutput/UpdatedData/GDPcapCC_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
  # save(GDPcapNoCC,file="data/output/projectionOutput/UpdatedData/GDPcapNoCC_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
  # save(tots,file="data/output/projectionOutput/UpdatedData/GlobalChanges_RCP85_Compare"%&%mdl%&%"_"%&%scens[scen]%&%".Rdata")
  # save(dfTemp,file="data/output/projectionOutput/UpdatedData/Temperatures_RCP85.Rdata") # Save temperatures
  # save(dfGlobalTemp,file="data/output/projectionOutput/UpdatedData/GlobalTemperatures_RCP85.Rdata") # Save Global temperatures
  # save(dfPrecip,file="data/output/projectionOutput/UpdatedData/Precipitations_RCP85.Rdata") # Save precipitations
  # save(dfGlobalPrecip,file="data/output/projectionOutput/UpdatedData/GlobalPrecipitations_RCP85.Rdata") # Save Global Precipitations
  # GDPcapCC <- GDPcapNoCC <- NULL
# }




df <- data.frame("year"=c(2010:2099),"impact"=(tots[,3]-tots[,4])/tots[,4]*100)

ggplot(data=df) +
  geom_line(aes(year,impact))





