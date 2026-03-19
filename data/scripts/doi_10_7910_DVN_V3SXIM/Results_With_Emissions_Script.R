#This code reads the marginal model results and emissions and calculates overall impacts/results
#Author: Ernani F. Choma 

#Please change the working directory path on line 22 before running:
#Line 22 -- Working directory

#Also make sure to check following paths for Inputs and Outputs (i.e., Results):
#Outputs/Results:
#Line 25-26 -- Path to Results: Figures and CSV Supplemental Datasets
#Inputs:
#Lines 42-45,50 -- Results from the marginal damages model (provided with the model)
#Lines 60-63 -- NEI emissions datasets -- output after processing raw data (provided with the model)
#Line 77 -- HHS Population Data (provided with model)
#Line 82 -- County list (FIPS State + County code) (provided with model)
#Line 88 -- USCB County data (not provided with model/needs to be downloaded)
#Line 97 -- USCB Metropolitan Area data (not provided with model/needs to be downloaded)
#Line 106 -- Baseline Ambient PM2.5 data (provided with model, only used to write Supplemental Dataset S5)



#Please change path to your working directory in the next line
setwd("~/US_OnRoad_HealthImpacts/Results_With_Emissions/")

#Creating file paths in the next 2 lines to desired location for Results (Figures and Supplemental Datasets)
Figure.path <- "Results/Figures/"
SupplementalDatasets.path <- "Results/Supplemental_Datasets/"

###### 1. Loading Model Inputs #####
#We will use:
#1.1 Marginal Damages Model Results
#1.2 NEI Emissions data
#1.3 Auxiliary Data: Population, County Lists, Baseline Ambient PM2.5 levels

######    1.1. Loading Results from Marginal Damages Model ######
#We will use the .RData files here, but these are also provided in .csv files.
#The .csv files might be in a slightly different format (e.g. matrices as opposed to 3-dimensional arrays)
#The documentation provides a description of the files

#Marginal Values
#four datasets, where marginal values were calculated using different (from 2008 or 2017) data for baseline ambient PM concentrations and mortality
#Please change path to location of inputs in the next 4 lines
load("Inputs/Marginal_Damages_Model_Results/MV_Damages_BasePM_2008_Mortality_2008.RData")
load("Inputs/Marginal_Damages_Model_Results/MV_Damages_BasePM_2008_Mortality_2017.RData")
load("Inputs/Marginal_Damages_Model_Results/MV_Damages_BasePM_2017_Mortality_2008.RData")
load("Inputs/Marginal_Damages_Model_Results/MV_Damages_BasePM_2017_Mortality_2017.RData")

#Source-Receptor Matrix
#Just one dataset for current (2017) data, i.e. 2017 data for both baseline ambient PM concentrations and mortality.
#Please change path to location of inputs in the next line
load("Inputs/Marginal_Damages_Model_Results/SRM_Damages_BasePM_2017_Mortality_2017.RData")


#Note: We provide the other SRMs (i.e. for other PM and mortality years) but we do not use them to produce our main results
#For previous years, we only use the overall marginal damages
#The SRM would be needed if it were necessary to investigate transfers of impacts across states and metropolitan areas in a counterfactual scenario 
#(i.e. mortality and/or baseline PM levels as they were in 2008)

######    1.2. Loading NEI Emissions Data ######
#Please change path to location of inputs in the next 4 lines
load("Inputs/NEI_Emissions/NEI2008.RData")
load("Inputs/NEI_Emissions/NEI2011.RData")
load("Inputs/NEI_Emissions/NEI2014.RData")
load("Inputs/NEI_Emissions/NEI2017.RData")

######    1.3. Loading Auxiliary Data (Population, County Lists, Baseline Ambient PM2.5 levels)  ######
#a) Population Data
#Population in 2008 and 2017:
Population.2008 <- 302074298
Population.2017 <- 322821446
#Source: 
#[dataset] U.S. Department of Health and Human Services, 2020. 
#United States Department of Health and Human Services (US DHHS), Centers for Disease Control and Prevention (CDC), National Center for Health Statistics (NCHS), 
#Bridged-Race Population Estimates, United States July 1st resident population by state, county, age, sex, bridged-race, and Hispanic origin. 
#Compiled from 1990-1999 bridged-race intercensal population estimates (released by NCHS on 7/26/2004); 
#revised bridged-race 2000-2009 intercensal population estimates (released by NCHS on 10/26/2012); and bridged-race Vintage 2019 (2010-2019) postcensal population estimates (released by NCHS on 7/9/2020). 
#Available on CDC WONDER Online Database. http://wonder.cdc.gov/bridged-race-v2019.html (accessed 30 November 2020).
Aux.Pop.Data <- read.csv("Inputs/Auxiliary/CountyPop.csv")


#b) County List
#Please change path to location of inputs in the next line
County.List <- read.csv("Inputs/Auxiliary/STCOUList.csv")
names(County.List) <- "STCOU"


#c) County Data from the US Census Bureau
#Please change path to location of inputs in the next line
USCB.County.Data <- read.csv("Inputs/Auxiliary/co-est2019-alldata.csv")
#Source: U.S. Census Bureau, 2020
#Available at: https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv
#Accessed on May 03, 2021
#Data Last Modified, according to US Census Bureau: 03/26/2020 09:40
#File Size: 3.5M

#d) Metropolitan Area data from the U.S. Census Bureau
#Please change path to location of inputs in the next line
USCB.Metropolitan.Areas <- read.csv("Inputs/Auxiliary/cbsa-est2019-alldata.csv")
#Data Source: US Census Bureau, 2020 
#Available at: https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.csv
#Accessed: May 03, 2021
#Data Last Modified, according to US Census Bureau: 03/26/2020 09:41
#File Size: 1.2M

#e) Ambient baseline PM2.5 levels (provided with model and only used in this code file to write supplemental dataset S5)
#Please change path to location of inputs in the next line
InputPM25 <- read.csv("Inputs/Auxiliary/Model_Inputs_AmbientPM25.csv")

#NOTE About Manuscript Figure 4 -- Map of PLDV impacts per mile:
#It requires one additional input that needs to be downloaded: a shapefile from the U.S. Census Bureau
#It also requires R packages sp, raster, rgeos, viridis, and gridExtra
#Detailed description, including of required download is available in lines 2015-2031
#This figure is generated at the very end of the code (lines 2013-2087)
#The remainder of the code can be run without this additional shapefile input and packages

###### 2. Processing Population Data and County Lists ######

######    2.1. Processing HHS Population data ######
Aux.Pop.08 <- Aux.Pop.Data[Aux.Pop.Data$AgeGroupCode==0,]
Aux.Pop.08 <- Aux.Pop.08[Aux.Pop.08$Year==2008,]
Aux.Pop.17 <- Aux.Pop.Data[Aux.Pop.Data$AgeGroupCode==0,]
Aux.Pop.17 <- Aux.Pop.17[Aux.Pop.17$Year==2017,]
Aux.Pop.17$STCOU[!Aux.Pop.17$STCOU %in% Aux.Pop.08$STCOU]
Aux.Pop.08$STCOU[!Aux.Pop.08$STCOU %in% Aux.Pop.17$STCOU]

Aux.Pop.08[Aux.Pop.08$STCOU==51515,]
Aux.Pop.08[Aux.Pop.08$STCOU==51019,]
sum(Aux.Pop.08$Population)
Aux.Pop.08$Population[Aux.Pop.08$STCOU==51019] <- sum(Aux.Pop.08$Population[Aux.Pop.08$STCOU %in% c(51019,51515)])
Aux.Pop.08 <- Aux.Pop.08[!Aux.Pop.08$STCOU==51515,]
sum(Aux.Pop.08$Population)
Aux.Pop.17$STCOU[!Aux.Pop.17$STCOU %in% Aux.Pop.08$STCOU]
Aux.Pop.08$STCOU[!Aux.Pop.08$STCOU %in% Aux.Pop.17$STCOU]

names(Aux.Pop.08)
Aux.Pop.08 <- subset(Aux.Pop.08, select=c("STCOU","Population"))
names(Aux.Pop.08) <- c("STCOU","POP08")
Aux.Pop.08 <- Aux.Pop.08[order(Aux.Pop.08$STCOU,decreasing=FALSE),]
sum(Aux.Pop.08$POP08)
sum(Aux.Pop.08$STCOU==NEI2017.REF.EMIS$STCOU)
sum(Aux.Pop.08$STCOU!=NEI2017.REF.EMIS$STCOU)

Aux.Pop.17 <- subset(Aux.Pop.17, select=c("STCOU","Population"))
names(Aux.Pop.17) <- c("STCOU","POP17")
Aux.Pop.17 <- Aux.Pop.17[order(Aux.Pop.17$STCOU,decreasing=FALSE),]
sum(Aux.Pop.17$POP17)
sum(Aux.Pop.17$STCOU==NEI2017.REF.EMIS$STCOU)
sum(Aux.Pop.17$STCOU!=NEI2017.REF.EMIS$STCOU)

######    2.2. Processing USCB County Data ######
County.List.Full <- subset(USCB.County.Data, select=c("STATE","COUNTY","STNAME","CTYNAME"))
County.List.Full$STCOU <- 1000*County.List.Full$STATE + County.List.Full$COUNTY
County.List.Full <- County.List.Full[County.List.Full$STCOU %in% County.List$STCOU,]
County.List.Full <- subset(County.List.Full, select=c("STCOU","CTYNAME","STATE","STNAME"))

######    2.3. Processing USCB Metropolitan Area Data ######
USCB.Metropolitan.Areas <- subset(USCB.Metropolitan.Areas, select=c("CBSA","STCOU","NAME","LSAD","POPESTIMATE2017"))
Filter.MSA.1M <- USCB.Metropolitan.Areas$CBSA[USCB.Metropolitan.Areas$LSAD=="Metropolitan Statistical Area" & USCB.Metropolitan.Areas$POPESTIMATE2017>1000000]
USCB.Metropolitan.Areas <- USCB.Metropolitan.Areas[USCB.Metropolitan.Areas$CBSA %in% Filter.MSA.1M,]
MSA.1M.List <- subset(USCB.Metropolitan.Areas[USCB.Metropolitan.Areas$LSAD=="Metropolitan Statistical Area",], select=c("CBSA","NAME"))
MSA.T10.List <- subset(USCB.Metropolitan.Areas[USCB.Metropolitan.Areas$LSAD=="Metropolitan Statistical Area",], select=c("CBSA","NAME","POPESTIMATE2017"))
MSA.T10.List <- MSA.T10.List[order(MSA.T10.List$POPESTIMATE2017, decreasing=TRUE),]
MSA.T10.List <- MSA.T10.List[1:10,]

MSA.County.List <- subset(USCB.Metropolitan.Areas[USCB.Metropolitan.Areas$LSAD=="County or equivalent",], select=c("CBSA","STCOU","POPESTIMATE2017"))
MSA.County.List <- merge(x=MSA.County.List,y=MSA.1M.List,by="CBSA",all=TRUE)
names(MSA.County.List)[4] <- "CBSA.NAME"
MSA.List <- unique(MSA.County.List$CBSA)
MSA.to.County <- subset(MSA.County.List,select=c("CBSA","STCOU"))
MSA.to.County.T10 <- MSA.to.County[(MSA.to.County$CBSA %in% MSA.T10.List$CBSA),]

#Creating State Lists
State.List <- unique(County.List$STCOU %/% 1000)
State.to.County <- County.List
State.to.County$ST <- State.to.County$STCOU %/% 1000
State.to.County <- State.to.County[,c(2,1)]

rm(USCB.Metropolitan.Areas)
rm(USCB.County.Data)
rm(MSA.1M.List)
rm(Filter.MSA.1M)

###### 3. Defining Variables to read and use NEI data ######

#Vehicle Types in the NEI files provided
MOT.Slices <- 1 #Motorcycles (MOT)
LDV.Slices <- c(2,3,4) #Light-Duty Vehicles (LDV)
PLDV.Slices <- c(2,3) #Passenger LDVs (PLDV)
BUS.Slices <- c(5,6,7) #Buses (BUS)
HDT.Slices <- c(8:13) #Heavy-Duty Trucks (HDT)
ALL.Slices <- c(1:13) #All Vehicles

#Pollutants
Pol.Names <- c("PM25","SO2","NOX","NH3","VOC","CO2","CH4","N2O")
Pol.Columns <- c(4:11) #The pollutants above are in these columns (in order) in the NEI files provided
Pol.Columns.Damages <- c(1:5)

#CO2-eq. weights
C2E.Weights <- c(1,34,298)

#Conversion of Short Tons to grams
Shortton.grams <- 0.45359237 * 2000 * 1000 #grams in a short ton

###### 4. Calculating Results ######


######    4.1. Calculating Emissions ######
Aggregate.Emissions <- array(rep(0,4*7*6),dim=c(4,7,6),
                             dimnames = list(c("2008","2011","2014","2017"),
                                             c("PLDV","LDV", "HDT", "BUS", "MOT", "REF", "VMT.Adj"),
                                             c("PM25", "SO2", "NOX", "NH3", "VOC", "C2E")))
#Dim 1 -- Year (2008, 2011, 2014, 2017)
#Dim 2 -- Vehicle Type (PLDV,LDV, HDT, BUS, MOT, REF, VMT_Adjustment)
#Dim 3 -- Pollutant (Primary PM2.5, SO2, NOx, NH3, VOC, CO2-eq.)

######       4.1.1. For PM2.5 (primary and precursors) ######
for (k in 1:5)
{
  for (i in 1:4)
  {
    aux <- eval(parse(text=paste0("NEI",dimnames(Aggregate.Emissions)[[1]][i],".VTYPE.EF")))
    Aggregate.Emissions[i,1,k] <- sum(aux[,Pol.Columns[k],PLDV.Slices] * aux[,3,PLDV.Slices]) * (1/1e6) #Converting to tonnes
    Aggregate.Emissions[i,2,k] <- sum(aux[,Pol.Columns[k],LDV.Slices] * aux[,3,LDV.Slices]) * (1/1e6) #Converting to tonnes
    Aggregate.Emissions[i,3,k] <- sum(aux[,Pol.Columns[k],HDT.Slices] * aux[,3,HDT.Slices]) * (1/1e6) #Converting to tonnes
    Aggregate.Emissions[i,4,k] <- sum(aux[,Pol.Columns[k],BUS.Slices] * aux[,3,BUS.Slices]) * (1/1e6) #Converting to tonnes
    Aggregate.Emissions[i,5,k] <- sum(aux[,Pol.Columns[k],MOT.Slices] * aux[,3,MOT.Slices])* (1/1e6) #Converting to tonnes

    
    if (i != 1 & k==5)
    {
      aux <- eval(parse(text=paste0("NEI",dimnames(Aggregate.Emissions)[[1]][i],".VTYPE.EF")))
      aux.ref <- eval(parse(text=paste0("NEI",dimnames(Aggregate.Emissions)[[1]][i],".REF.EMIS")))
      Aggregate.Emissions[i,6,k] <- sum(aux.ref[,4]) * Shortton.grams * (1/1e6) #Converting to tonnes
      aux.nonref.VMT17 <- sum(aux[,Pol.Columns[k],1:13] * NEI2017.VTYPE.EF[,3,1:13]) * (1/1e6)
      aux.ref.VMT17 <- sum(aux.ref[,5]*(NEI2017.REF.EMIS[,3])) * (1/1e6)
      if (i != 4) #2017 will not be adjusted to 2017 VMT levels
      {
        Aggregate.Emissions[i,7,k] <- (aux.nonref.VMT17 + aux.ref.VMT17) - sum(Aggregate.Emissions[i,2:6,k])
      }
    } else {
      aux <- eval(parse(text=paste0("NEI",dimnames(Aggregate.Emissions)[[1]][i],".VTYPE.EF")))
      aux.nonref.VMT17 <- sum(aux[,Pol.Columns[k],1:13] * NEI2017.VTYPE.EF[,3,1:13]) * (1/1e6)
      if (i != 4) #2017 will not be adjusted to 2017 VMT levels
      {
        Aggregate.Emissions[i,7,k] <- (aux.nonref.VMT17) - sum(Aggregate.Emissions[i,2:6,k])
      }
    }
  }
}

######       4.1.2. For C2E ######
for (i in 1:4)
{
  aux.vtype <- list(PLDV.Slices,LDV.Slices,HDT.Slices,BUS.Slices,MOT.Slices)
  for (vt in 1:5)
  {
    aux <- eval(parse(text=paste0("NEI",dimnames(Aggregate.Emissions)[[1]][i],".VTYPE.EF")))
    aux.CO2 <- sum(aux[,Pol.Columns[6],aux.vtype[[vt]]] * aux[,3,aux.vtype[[vt]]]) * (1/1e6) #Converting to tonnes
    aux.CH4 <- sum(aux[,Pol.Columns[7],aux.vtype[[vt]]] * aux[,3,aux.vtype[[vt]]]) * (1/1e6) #Converting to tonnes
    aux.N2O <- sum(aux[,Pol.Columns[8],aux.vtype[[vt]]] * aux[,3,aux.vtype[[vt]]]) * (1/1e6) #Converting to tonnes
    Aggregate.Emissions[i,vt,6] <- aux.CO2 * C2E.Weights[1] + aux.CH4 * C2E.Weights[2] + aux.N2O * C2E.Weights[3] #Converting to CO2-eq.
  }
  if (i != 4)
  {
    aux <- eval(parse(text=paste0("NEI",dimnames(Aggregate.Emissions)[[1]][i],".VTYPE.EF")))
    aux.CO2.VMT17 <- sum(aux[,Pol.Columns[6],1:13] * NEI2017.VTYPE.EF[,3,1:13]) * (1/1e6) * C2E.Weights[1]
    aux.CH4.VMT17 <- sum(aux[,Pol.Columns[7],1:13] * NEI2017.VTYPE.EF[,3,1:13]) * (1/1e6) * C2E.Weights[2]
    aux.N2O.VMT17 <- sum(aux[,Pol.Columns[8],1:13] * NEI2017.VTYPE.EF[,3,1:13]) * (1/1e6) * C2E.Weights[3]
    aux.C2E.VMT17 <- aux.CO2.VMT17 + aux.CH4.VMT17 + aux.N2O.VMT17
    Aggregate.Emissions[i,7,6] <- (aux.C2E.VMT17) - sum(Aggregate.Emissions[i,2:5,6])
  }
}

######       4.1.3. Printing Results ######
Aggregate.Emissions
#Values are in tonnes

######    4.2. Calculating Monetized Overall Benefits ######
#SCC Values
SCCAdj <- 1.167186429
AveCO2 <- 42*SCCAdj
HighCO2 <- 123*SCCAdj

VSL.2014 <- 9.3e6
Income.Adjustment <- 351/334
Inflation.Adjustment <- 1.041166717 #World Bank 2015*2016*2017 GDP Deflator

VSL <- VSL.2014 * Income.Adjustment * Inflation.Adjustment
VSL #This is the value used in our marginal damages: $10.18 million (2017 USD)
#Benefits can be simply scaled accordingly is a new VSL is desired

#We also apply EPA's preferred cessation lag:
Cessation.Lag <- c(0.3,rep(0.5/4,4),rep(0.2/15,15))
Discount.Rate <- 0.03
Year <- 1:20

#Present Value:
Present.Value <- (1/Discount.Rate)*(exp(-Discount.Rate*(Year-1))-exp(-Discount.Rate*(Year)))
Sum.Present.Value <- t(Cessation.Lag) %*% Present.Value
Present.Value.Multiplier <- as.numeric(Sum.Present.Value)


######       4.2.1. Calculating Monetized Impacts ######
Model.SCC <- c(AveCO2,HighCO2)
Model.CRF <- c("GEMM","Vodonos.Parametric","Vodonos.Spline","Krewski")

#This Function Returns an Array with Monetized Impacts per year (in Billions of 2017 USD) for:
#Dim 1 -- Vehicle Type (PLDV, HDT, LDV, All Vehicles)
#Dim 2 -- Pollutant (Primary PM2.5, SO2, NOx, NH3, VOC, CO2-eq.)
#Dim 3 -- Emission Factor Year (2008, 2017) 
#Note that year (Dim 3) refers to the year of Emission Factors only (i.e. vehicles driving under 2008 or 2017 EFs), all impacts are assessed for year 2017.
Calculate.Monetized.Impacts <- function(SCC,CRF)
{
  Overall.Monetized.Impacts <- array(rep(0,4*6*2),dim=c(4,6,2),
                                     dimnames = list(c("PLDV","HDT","LDV","ALL"),
                                                     c("PM25", "SO2", "NOX", "NH3", "VOC", "C2E"),
                                                     c("2008", "2017")))
  aux.SCC <- SCC
  aux.CRF <- as.character(CRF)
  if (!aux.CRF %in% Model.CRF)
  {
    stop("Wrong CRF Input -- select 'GEMM', Vodonos.Parametric', 'Vodonos.Spline', or 'Krewski'")
  }
  
  #For PM2.5
  for (p in 1:5)
  {
    aux.vtype <- list(PLDV.Slices,HDT.Slices,LDV.Slices,ALL.Slices)
    for (i in 1:4)
    {
      for (k in 1:2)
      {
        aux <- eval(parse(text=paste0("NEI",dimnames(Overall.Monetized.Impacts)[[3]][k],".VTYPE.EF")))
        aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_2017_Mortality_2017")))
        aux.nonref.VMT17 <- (rowSums(aux[,Pol.Columns[p],aux.vtype[[i]]] * NEI2017.VTYPE.EF[,3,aux.vtype[[i]]]) %*% aux.marg.crf[,Pol.Columns.Damages[p]]) * 1e-6 * 1e-9
        Overall.Monetized.Impacts[i,p,k] <- aux.nonref.VMT17
      }
    }
    
    if (p == 5 & k == 2)
    {
      aux.ref <- (NEI2017.REF.EMIS$EF.VOC * NEI2017.REF.EMIS$VMT) %*% aux.marg.crf[,Pol.Columns.Damages[p]] * 1e-6 * 1e-9
      Overall.Monetized.Impacts[4,p,k] <- Overall.Monetized.Impacts[4,p,k] + aux.ref 
    }
  }
  
  #Now for C2E
  aux.vtype <- list(PLDV.Slices,HDT.Slices,LDV.Slices,ALL.Slices)
  for (i in 1:4)
  {
    for (k in 1:2)
    {
      aux <- eval(parse(text=paste0("NEI",dimnames(Overall.Monetized.Impacts)[[3]][k],".VTYPE.EF")))
      aux.CO2.VMT17 <- sum(aux[,Pol.Columns[6],aux.vtype[[i]]] * NEI2017.VTYPE.EF[,3,aux.vtype[[i]]]) * aux.SCC * (1/1e6) * C2E.Weights[1] * 1e-9
      aux.CH4.VMT17 <- sum(aux[,Pol.Columns[7],aux.vtype[[i]]] * NEI2017.VTYPE.EF[,3,aux.vtype[[i]]]) * aux.SCC * (1/1e6) * C2E.Weights[2] * 1e-9
      aux.N2O.VMT17 <- sum(aux[,Pol.Columns[8],aux.vtype[[i]]] * NEI2017.VTYPE.EF[,3,aux.vtype[[i]]]) * aux.SCC * (1/1e6) * C2E.Weights[3] * 1e-9
      aux.C2E.VMT17 <- aux.CO2.VMT17 + aux.CH4.VMT17 + aux.N2O.VMT17
      Overall.Monetized.Impacts[i,6,k] <- aux.C2E.VMT17
    }
  }
  return(Overall.Monetized.Impacts)
}

Base.Monetized.Impacts <- Calculate.Monetized.Impacts(SCC=Model.SCC[1],CRF=Model.CRF[1])
Low.Monetized.Impacts <- Calculate.Monetized.Impacts(SCC=Model.SCC[1],CRF=Model.CRF[4])
High.Monetized.Impacts <- Calculate.Monetized.Impacts(SCC=Model.SCC[2],CRF=Model.CRF[3])
High.Monetized.Impacts2 <- Calculate.Monetized.Impacts(SCC=Model.SCC[2],CRF=Model.CRF[2])

######       4.2.2. Printing Results ###### 
#Monetized Impacts in 2017 by emissions scenario (2008 EFs, 2017 EFs), pollutant and vehicle class 
#Values in billion USD
Base.Monetized.Impacts #Base case scenario: GEMM + Average SCC
Low.Monetized.Impacts #Low scenario: Krewski + Average SCC
High.Monetized.Impacts #High scenario: Vodonos + High-Impact SCC

#Benefits by pollutant and vehicle class -- values in billion USD
Base.Monetized.Impacts[,,1]-Base.Monetized.Impacts[,,2]
Low.Monetized.Impacts[,,1]-Low.Monetized.Impacts[,,2]
High.Monetized.Impacts[,,1]-High.Monetized.Impacts[,,2]

######    4.3. Calculating PM2.5-attributable deaths ######
Model.SCC <- c(AveCO2,HighCO2)
Model.CRF <- c("GEMM","Vodonos.Parametric","Vodonos.Spline","Krewski")

######       4.3.1 Calculating Attributable Deaths for each emission factor scenario ######
#This Function Returns an Array with Attributable deaths per year for:
#Dim 1 -- Vehicle Type (PLDV,LDV, HDT, BUS, MOT (Motorcycles), REF (Refueling))
#Dim 2 -- Pollutant (Primary PM2.5, SO2, NOx, NH3, VOC)
#Dim 3 -- Emission Factor Year (2008, 2011, 2014, 2017) 
#Note that year (Dim 3) refers to the year of Emission Factors only (i.e. vehicles driving under 2008, 2011, 2014 or 2017 EFs), all impacts are assessed for year 2017.

Calculate.Attributable.Deaths <- function(CRF)
{
  Attributable.Deaths <- array(rep(0,6*5*4),dim=c(6,5,4),
                               dimnames = list(c("PLDV","LDV","HDT","BUS","MOT","REF"),
                                               c("PM25", "SO2", "NOX", "NH3", "VOC"),
                                               c("2008", "2011","2014","2017")))
  aux.CRF <- as.character(CRF)
  if (!aux.CRF %in% Model.CRF)
  {
    stop("Wrong CRF Input -- select 'GEMM', Vodonos.Parametric', 'Vodonos.Spline', or 'Krewski'")
  }
  
  #For PM2.5
  for (p in 1:5)
  {
    aux.vtype <- list(PLDV.Slices,LDV.Slices,HDT.Slices,BUS.Slices,MOT.Slices)
    for (i in 1:5)
    {
      for (k in 1:4)
      {
        aux <- eval(parse(text=paste0("NEI",dimnames(Attributable.Deaths)[[3]][k],".VTYPE.EF")))
        aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_2017_Mortality_2017")))
        aux.nonref.VMT17 <- (rowSums(as.matrix(aux[,Pol.Columns[p],aux.vtype[[i]]] * NEI2017.VTYPE.EF[,3,aux.vtype[[i]]])) %*% aux.marg.crf[,Pol.Columns.Damages[p]]) * 1e-6 * (1/VSL) * (1/Present.Value.Multiplier) 
        #The division by the monetization in the previous line is to get back to attributable deaths
        
        Attributable.Deaths[i,p,k] <- aux.nonref.VMT17
        if (p == 5 & k != 1) #No separate refueling emissions in NEI 2008
        {
          aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_2017_Mortality_2017")))
          aux.ref <- eval(parse(text=paste0("NEI",dimnames(Attributable.Deaths)[[3]][k],".REF.EMIS"))) 
          aux.ref.VMT17 <- ((aux.ref[,5]*NEI2017.REF.EMIS[,3]) %*% aux.marg.crf[,Pol.Columns.Damages[p]]) * (1/1e6) * (1/VSL) * (1/Present.Value.Multiplier)
          #The division by the monetization in the previous line is to get back to attributable deaths
          
          Attributable.Deaths[6,p,k] <-aux.ref.VMT17
        }
      }
    }
  }
  return(Attributable.Deaths)
}

GEMM.Attributable.Deaths <- Calculate.Attributable.Deaths(Model.CRF[1])
Vodonos.Attributable.Deaths <- Calculate.Attributable.Deaths(Model.CRF[3])
Krewski.Attributable.Deaths <- Calculate.Attributable.Deaths(Model.CRF[4])

######       4.3.2 Printing Results ######

GEMM.Attributable.Deaths
Vodonos.Attributable.Deaths
Krewski.Attributable.Deaths
#Attributable deaths in 2017 by emissions scenario (2008 EFs, 2011 EFs, 2014 EFs, 2017 EFs), pollutant, and vehicle class. 
#REF = Refueling (Note: REF=0 in 2008 as refueling emissions are not provided separately in NEI 2008)


######    4.4. Calculating Decomposition of Effects from 2008 to 2017 ######
aux.Pop.i <- (Aux.Pop.17$POP17/Aux.Pop.08$POP08)
aux.vtype <- list(PLDV.Slices,LDV.Slices,HDT.Slices,ALL.Slices)
a.aux.emis.VMT08 <- array(rep(0,3108*5*length(aux.vtype[[i]])),dim=c(3108,5,length(aux.vtype[[i]])))
a.aux.emis.VMT17 <- array(rep(0,3108*5*length(aux.vtype[[i]])),dim=c(3108,5,length(aux.vtype[[i]])))

for (p in 1:5)
{
  a.aux.emis.VMT08[,p,] <- as.matrix(NEI2008.VTYPE.EF[,Pol.Columns[p],aux.vtype[[i]]] * NEI2008.VTYPE.EF[,3,aux.vtype[[i]]]) * 1e-6
  a.aux.emis.VMT17[,p,] <- as.matrix(NEI2008.VTYPE.EF[,Pol.Columns[p],aux.vtype[[i]]] * NEI2017.VTYPE.EF[,3,aux.vtype[[i]]]) * 1e-6
}

#Total VMT in 2008 and 2017 for each vehicle class (LDV, HDT, ALL)
VMT.2008 <- c(sum(NEI2008.VTYPE.EF[,3,PLDV.Slices]),sum(NEI2008.VTYPE.EF[,3,LDV.Slices]),sum(NEI2008.VTYPE.EF[,3,HDT.Slices]),sum(NEI2008.VTYPE.EF[,3,ALL.Slices]))
VMT.2017 <- c(sum(NEI2017.VTYPE.EF[,3,PLDV.Slices]),sum(NEI2017.VTYPE.EF[,3,LDV.Slices]),sum(NEI2017.VTYPE.EF[,3,HDT.Slices]),sum(NEI2017.VTYPE.EF[,3,ALL.Slices]))

i.pm <- 1
i.m <- 1
aux.CRF <- "GEMM"
aux.BasePM <- c("2008","2017")
aux.Mortality <- c("2008","2017")


Effect.Decomposition <- function(CRF)
{
  Effects <- matrix(rep(0,4*10),ncol=10,dimnames = list(c("PLDV","LDV","HDT","ALL"),
                                                        c("2008",
                                                          "Pop.Increase","Mortality.Rates",
                                                          "Total.Vehicle.Class.VMT","Composition.Changes.Within.Class",
                                                          "Lower.Ambient.PM25",
                                                          "VMT.Mortality.Interaction",
                                                          "VMT.PM25.Interaction",
                                                          "Mortality.PM25.Interaction",
                                                          "3.Way.Interaction")))
  aux.CRF <- as.character(CRF)
  if (!aux.CRF %in% Model.CRF)
  {
    stop("Wrong CRF Input -- select 'GEMM', Vodonos.Parametric', 'Vodonos.Spline', or 'Krewski'")
  }
  aux.BasePM <- c("2008","2017")
  aux.Mortality <- c("2008","2017")
  aux.vtype <- list(PLDV.Slices,LDV.Slices,HDT.Slices,ALL.Slices)
  for (i in 1:4)
  {

    aux.emis.VMT08 <- array(rep(0,3108*5*length(aux.vtype[[i]])),dim=c(3108,5,length(aux.vtype[[i]])))
    aux.emis.VMT17 <- array(rep(0,3108*5*length(aux.vtype[[i]])),dim=c(3108,5,length(aux.vtype[[i]])))
    for (p in 1:5)
    {
      aux.emis.VMT08[,p,] <- as.matrix(NEI2008.VTYPE.EF[,Pol.Columns[p],aux.vtype[[i]]] * NEI2008.VTYPE.EF[,3,aux.vtype[[i]]]) * 1e-6
      aux.emis.VMT17[,p,] <- as.matrix(NEI2008.VTYPE.EF[,Pol.Columns[p],aux.vtype[[i]]] * NEI2017.VTYPE.EF[,3,aux.vtype[[i]]]) * 1e-6
    }
    i.pm <- 1
    i.m <- 1
    aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_",aux.BasePM[i.pm],"_Mortality_",aux.Mortality[i.m])))
    Effects[i,1] <- sum(diag(t(rowSums(aux.emis.VMT08,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
    Effects[i,2] <- Effects[i,1] * ((Population.2017/Population.2008)-1)
    aux.calc <- sum(diag(t(rowSums(aux.emis.VMT17,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
    Effects[i,4] <- Effects[i,1] * ((VMT.2017[i]/VMT.2008[i])-1)
    Effects[i,5] <- aux.calc - sum(Effects[i,c(1,4)])
    i.m <- 2
    aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_",aux.BasePM[i.pm],"_Mortality_",aux.Mortality[i.m])))
    aux.calc <- sum(diag(t(rowSums(aux.emis.VMT08,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
    Effects[i,3] <- aux.calc - sum(Effects[i,1:2])
    aux.calc <- sum(diag(t(rowSums(aux.emis.VMT17,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
    Effects[i,7] <- aux.calc - sum(Effects[i,1:5])
    i.m <- 1
    i.pm <- 2
    aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_",aux.BasePM[i.pm],"_Mortality_",aux.Mortality[i.m])))
    aux.calc <- sum(diag(t(rowSums(aux.emis.VMT08,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
    Effects[i,6] <- aux.calc - Effects[i,1]
    aux.calc <- sum(diag(t(rowSums(aux.emis.VMT17,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
    Effects[i,8] <- aux.calc - sum(Effects[i,c(1,4:6)])
    i.m <- 2
    aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_",aux.BasePM[i.pm],"_Mortality_",aux.Mortality[i.m])))
    aux.calc <- sum(diag(t(rowSums(aux.emis.VMT08,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
    Effects[i,9] <- aux.calc - sum(Effects[i,c(1:3,6)])
    aux.calc <- sum(diag(t(rowSums(aux.emis.VMT17,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
    Effects[i,10] <- aux.calc - sum(Effects[i,1:9])
  }
  return(Effects)
}

Base.Case.Effects <- Effect.Decomposition(Model.CRF[1])

######       4.4.1 Printing main decomposition results (shown in Fig 3 and Fig S1) ######
#Base Case Effects by vehicle class.
#Values are attributable deaths
Base.Case.Effects
#For example...the fourth line shows the effects bar shown in Fig 3, panel b)
Base.Case.Effects[4,]
#27,713 deaths occurred in 2008
#Population increase from 2008 to 2017 added another 1,903 PM2.5-attributable deaths in 2017
#Increases in mortality rates from 2008 to 2017 added another 1,702 PM2.5-attributable deaths in 2017
#and so on, as shown in Fig 3, panel b)

#The first line shows the effects bar shown in Fig S1, panel b)
Base.Case.Effects[1,]

######       4.4.2 Printing additional decomposition results ######
aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_",aux.BasePM[i.pm],"_Mortality_",aux.Mortality[i.m])))
a.Effects <- sum(diag(t(rowSums(a.aux.emis.VMT08,dims=2)) %*% aux.marg.crf)) * (1/VSL) * (1/Present.Value.Multiplier)
b.Effects <- sum(diag(t(rowSums(a.aux.emis.VMT08,dims=2)) %*% (aux.marg.crf * aux.Pop.i))) * (1/VSL) * (1/Present.Value.Multiplier)
aux.VMT <- rowSums(NEI2017.VTYPE.EF[,3,ALL.Slices])/rowSums(NEI2008.VTYPE.EF[,3,ALL.Slices])
c.Effects <- sum(diag(t(rowSums(a.aux.emis.VMT08,dims=2)) %*% (aux.marg.crf * aux.VMT))) * (1/VSL) * (1/Present.Value.Multiplier)

b.Effects/a.Effects #Effect of national-level population increase = +7%
((VMT.2017[i]/VMT.2008[i])-1) #Effect of overall national-level VMT increase = +6.6%
c.Effects/a.Effects # Effect of VMT increase if computed on a county-level = +5.5%


######       4.4.2.1 Results under different VMT adjustment (SI Section 2) ######
#Results under different VMT adjustment (SI Section 2)
#This adjustment splits the fleet in 3 types only: motorcycles, cars, and all remaining vehicles
#a) If done on a county level:
#a.i) In 2008 under 2008 EFs:
#For Motorcycles
aux.MOT.impact.08 <- (NEI2008.VTYPE.EF[,Pol.Columns[1:5],1]*Marg.Damages.GEMM_BasePM_2008_Mortality_2008*NEI2008.VTYPE.EF[,3,1]) * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
#For Cars
aux.CAR.impact.08 <- (NEI2008.VTYPE.EF[,Pol.Columns[1:5],2]*Marg.Damages.GEMM_BasePM_2008_Mortality_2008*NEI2008.VTYPE.EF[,3,2]) * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
#For other vehicle types
# aux.OTHER.VMTadjust.08 <- rowSums(NEI2017.VTYPE.EF[,3,3:13])/rowSums(NEI2008.VTYPE.EF[,3,3:13])
aux.OTHER.emissions.08 <- array(rep(0,3108*5*11),dim=c(3108,5,11))
aux.OTHER.impact.08 <- array(rep(0,3108*5*11),dim=c(3108,5,11))
for (j in 1:11)
{
  aux.OTHER.emissions.08[,,j] <- as.matrix(NEI2008.VTYPE.EF[,Pol.Columns[1:5],(j+2)] * NEI2008.VTYPE.EF[,3,(j+2)])
  aux.OTHER.impact.08[,,j] <- as.matrix(aux.OTHER.emissions.08[,,j] * Marg.Damages.GEMM_BasePM_2008_Mortality_2008) * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
}
#Attrib. Deaths in 2008 under 2008 EFs: 27,700
Alt.Adjust.County.2008.Impact <- sum(aux.OTHER.impact.08) + sum(aux.CAR.impact.08) + sum(aux.MOT.impact.08)
Alt.Adjust.County.2008.Impact #27,700

#a.ii) Starting from impacts in 2008 under 2008 EFs and changing only VMT, i.e. estimating VMT effects
#For Motorcycles
aux.MOT.impact.08.VMT <- (NEI2008.VTYPE.EF[,Pol.Columns[1:5],1]*Marg.Damages.GEMM_BasePM_2008_Mortality_2008*NEI2017.VTYPE.EF[,3,1]) * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
#For Cars
aux.CAR.impact.08.VMT <- (NEI2008.VTYPE.EF[,Pol.Columns[1:5],2]*Marg.Damages.GEMM_BasePM_2008_Mortality_2008*NEI2017.VTYPE.EF[,3,2]) * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
#For other vehicle types
aux.OTHER.VMTadjust <- rowSums(NEI2017.VTYPE.EF[,3,3:13])/rowSums(NEI2008.VTYPE.EF[,3,3:13])
aux.OTHER.emissions.08.VMT <- array(rep(0,3108*5*11),dim=c(3108,5,11))
aux.OTHER.impact.08.VMT <- array(rep(0,3108*5*11),dim=c(3108,5,11))
for (j in 1:11)
{
  aux.OTHER.emissions.08.VMT[,,j] <- as.matrix(NEI2008.VTYPE.EF[,Pol.Columns[1:5],(j+2)] * NEI2008.VTYPE.EF[,3,(j+2)])
  aux.OTHER.impact.08.VMT[,,j] <- as.matrix(aux.OTHER.emissions.08.VMT[,,j] * Marg.Damages.GEMM_BasePM_2008_Mortality_2008) * aux.OTHER.VMTadjust * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
}
#Attrib. Deaths in 2008 under 2008 EFs + VMT changes: 32,500
Alt.Adjust.County.2008.Impact.VMT <- sum(aux.OTHER.impact.08.VMT) + sum(aux.CAR.impact.08.VMT) + sum(aux.MOT.impact.08.VMT)
Alt.Adjust.County.2008.Impact.VMT #32,500

#VMT effects: 17%
Alt.Adjust.County.2008.Impact.VMT/Alt.Adjust.County.2008.Impact #17%

#a.iii) Full effects -- impacts in 2017 under 2017 EFs under alternative VMT adjustment
#For Motorcycles
aux.MOT.impact.17 <- (NEI2008.VTYPE.EF[,Pol.Columns[1:5],1]*Marg.Damages.GEMM_BasePM_2017_Mortality_2017*NEI2017.VTYPE.EF[,3,1]) * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
#For Cars
aux.CAR.impact.17 <- (NEI2008.VTYPE.EF[,Pol.Columns[1:5],2]*Marg.Damages.GEMM_BasePM_2017_Mortality_2017*NEI2017.VTYPE.EF[,3,2]) * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
#For other vehicle types
aux.OTHER.emissions.17 <- array(rep(0,3108*5*11),dim=c(3108,5,11))
aux.OTHER.impact.17 <- array(rep(0,3108*5*11),dim=c(3108,5,11))
for (j in 1:11)
{
  aux.OTHER.emissions.17[,,j] <- as.matrix(NEI2008.VTYPE.EF[,Pol.Columns[1:5],(j+2)] * NEI2008.VTYPE.EF[,3,(j+2)])
  aux.OTHER.impact.17[,,j] <- as.matrix(aux.OTHER.emissions.17[,,j] * Marg.Damages.GEMM_BasePM_2017_Mortality_2017) * aux.OTHER.VMTadjust * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
}

#Attrib. Deaths in 2017 under 2017 EFs: 46,500
Alt.Adjust.County.2017.Impact <- sum(aux.OTHER.impact.17) + sum(aux.CAR.impact.17) + sum(aux.MOT.impact.17)
Alt.Adjust.County.2017.Impact #46,500

#b) If done on a national level:
#b.i) Impact In 2008 under 2008 EFs is the same as before (a.i) -- 27,700 attributable deaths
Alt.Adjust.US.2008.Impact <- Alt.Adjust.County.2008.Impact
#b.ii) Starting from impacts in 2008 under 2008 EFs and changing only VMT, i.e. estimating VMT effects
#For Motorcycles
aux.MOT.VMTadjust.US <- sum(NEI2017.VTYPE.EF[,3,1])/sum(NEI2008.VTYPE.EF[,3,1])
aux.MOT.impact.08.VMT.US <- aux.MOT.impact.08 * aux.MOT.VMTadjust.US
#For Cars
aux.CAR.VMTadjust.US <- sum(NEI2017.VTYPE.EF[,3,2])/sum(NEI2008.VTYPE.EF[,3,2])
aux.CAR.impact.08.VMT.US <- aux.CAR.impact.08 * aux.CAR.VMTadjust.US
#For other vehicle types
aux.OTHER.VMTadjust.US <- sum(NEI2017.VTYPE.EF[,3,3:13])/sum(NEI2008.VTYPE.EF[,3,3:13])
aux.OTHER.impact.08.VMT.US <- aux.OTHER.impact.08 * aux.OTHER.VMTadjust.US

#Attrib. Deaths in 2008 under 2008 EFs + VMT changes: 32,700
Alt.Adjust.US.2008.Impact.VMT <- sum(aux.OTHER.impact.08.VMT.US) + sum(aux.CAR.impact.08.VMT.US) + sum(aux.MOT.impact.08.VMT.US)
Alt.Adjust.US.2008.Impact.VMT #32,700
#VMT effects: 18%
Alt.Adjust.US.2008.Impact.VMT/Alt.Adjust.US.2008.Impact #18%


#b.iii) Full effects -- impacts in 2017 under 2017 EFs under alternative VMT adjustment
#For Motorcycles
aux.MOT.impact.17.US <- (NEI2008.VTYPE.EF[,Pol.Columns[1:5],1]*Marg.Damages.GEMM_BasePM_2017_Mortality_2017*NEI2008.VTYPE.EF[,3,1]) * aux.MOT.VMTadjust.US * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
#For Cars
aux.CAR.impact.17.US <- (NEI2008.VTYPE.EF[,Pol.Columns[1:5],2]*Marg.Damages.GEMM_BasePM_2017_Mortality_2017*NEI2008.VTYPE.EF[,3,2]) * aux.CAR.VMTadjust.US * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
#For other vehicle types
aux.OTHER.emissions.17.US <- array(rep(0,3108*5*11),dim=c(3108,5,11))
aux.OTHER.impact.17.US <- array(rep(0,3108*5*11),dim=c(3108,5,11))
for (j in 1:11)
{
  aux.OTHER.emissions.17.US[,,j] <- as.matrix(NEI2008.VTYPE.EF[,Pol.Columns[1:5],(j+2)] * NEI2008.VTYPE.EF[,3,(j+2)])
  aux.OTHER.impact.17.US[,,j] <- as.matrix(aux.OTHER.emissions.17.US[,,j] * Marg.Damages.GEMM_BasePM_2017_Mortality_2017) * aux.OTHER.VMTadjust.US * (1/1e6) * (1/(VSL*Present.Value.Multiplier))
}

#Attrib. Deaths in 2017 under 2017 EFs: 46,500
Alt.Adjust.County.2017.Impact.US <- sum(aux.OTHER.impact.17.US) + sum(aux.CAR.impact.17.US) + sum(aux.MOT.impact.17.US)
Alt.Adjust.County.2017.Impact.US #46,500

######    4.5. Cross-State and Cross-MSA Impacts ######
######       4.5.1 Calculating Cross-State and Cross-MSA Impacts ######
Geography.Options <- c("State","MSA")
Vehicle.Options <- c("ALL","PLDV")

Calculate.Impact.Exports <- function(CRF,Geography,Vehicle)
{

  aux.CRF <- as.character(CRF)
  aux.Geography <- as.character(Geography)
  aux.Vehicle <- as.character(Vehicle)
  if (!aux.CRF %in% Model.CRF)
  {
    stop("Wrong CRF Input -- select 'GEMM', Vodonos.Parametric', 'Vodonos.Spline', or 'Krewski'")
  }
  if (!aux.Geography %in% Geography.Options)
  {
    stop("Wrong Geography Input -- select 'State' or 'MSA'")
  }
  if (!aux.Vehicle %in% Vehicle.Options)
  {
    stop("Wrong Vehicle Input -- select 'ALL' or 'PLDV'")
  }
  Geography.List <- eval(parse(text=paste0(aux.Geography,".List")))
  Geography.to.County <- eval(parse(text=paste0(aux.Geography,".to.County")))
  Vehicle.Slices <- eval(parse(text=paste0(aux.Vehicle,".Slices")))

  Geography.Deaths <- array(rep(0,length(Geography.List)*5*2),dim=c(length(Geography.List),5,2),
                            dimnames = list(c(Geography.List),
                                        c("PM25", "SO2", "NOX", "NH3", "VOC"),
                                        c(paste0("IN.",aux.Geography),paste0("OUT.OF.",aux.Geography))))
  
  aux.sr.crf <- eval(parse(text=paste0("SR.Damages.",aux.CRF,"_BasePM_2017_Mortality_2017")))
  aux.emis <- NEI2017.VTYPE.EF
  aux.ref <- NEI2017.REF.EMIS

  
  for (p in 1:5)
  {
    aux.nonref.VMT17 <- rowSums(as.matrix(aux.emis[,Pol.Columns[p],Vehicle.Slices] * NEI2017.VTYPE.EF[,3,Vehicle.Slices])) * (1/1e6)
    if (p == 5 & aux.Vehicle=="ALL")
    {
      aux.ref.VMT17 <- aux.ref[,5] * aux.ref[,3] * (1/1e6)
      aux.nonref.VMT17 <- aux.nonref.VMT17 + aux.ref.VMT17
    }
    aux.sr.crf.pol <- (aux.sr.crf[,,p]) * aux.nonref.VMT17 * (1/VSL) * (1/Present.Value.Multiplier) 
    for (i in 1:length(Geography.List))
    {
      aux.stcou <- Geography.to.County[,2][Geography.to.County[,1] == Geography.List[i]]
      aux.sr.line <- which(County.List$STCOU %in% aux.stcou)
      Geography.Deaths[i,p,1] <- sum(aux.sr.crf.pol[aux.sr.line,aux.sr.line])
      Geography.Deaths[i,p,2] <- sum(aux.sr.crf.pol[aux.sr.line,-aux.sr.line])
    }
    
  }  
  return(Geography.Deaths)
}

OnRoad.CrossMSA <- Calculate.Impact.Exports(CRF="GEMM",Geography="MSA",Vehicle="ALL")
OnRoad.CrossState <- Calculate.Impact.Exports(CRF="GEMM",Geography="State",Vehicle="ALL")
PLDV.CrossMSA <- Calculate.Impact.Exports(CRF="GEMM",Geography="MSA",Vehicle="PLDV")
PLDV.CrossState <- Calculate.Impact.Exports(CRF="GEMM",Geography="State",Vehicle="PLDV")
#All values in these results are attributable deaths

######       4.5.2 Printing Results ######
#For the entire OnRoad Sector (all vehicles, including refueling):
#In-state
sum(OnRoad.CrossState[,,1])/sum(OnRoad.CrossState[,,]) #76% In-state, 24% Out-of-state
sum(OnRoad.CrossState[,3,2])/sum(OnRoad.CrossState[,,2]) #72% of Out-of-state impacts are due to NOx

#For Passenger LDVs
#In-MSA
sum(PLDV.CrossMSA[,,1])/sum(PLDV.CrossMSA[,,]) #83% In the same MSA, 17% crossing beyond each MSA's lines
sum(PLDV.CrossMSA[,-3,1])/sum(PLDV.CrossMSA[,-3,]) #91% in the same MSA for all pollutants except NOx
sum(PLDV.CrossMSA[,3,1])/sum(PLDV.CrossMSA[,3,]) #67% for NOx

#In-State
sum(PLDV.CrossState[,,1])/sum(PLDV.CrossState[,,]) #77% In-state, 23% Out-of-state
sum(PLDV.CrossState[,3,2])/sum(PLDV.CrossState[,,2]) #66% of Out-of-state PLDV impacta are due to NOX

######    4.6. Calculating Passenger Light-duty vehicle Impacts ######

######       4.6.1 Calculating PLDV Impacts ######

Model.Ambient.PM.Year <- c("2008","2017")
Model.Mortality.Year <- c("2008","2017")
Model.Emissions.Year <- c("2008","2017")
Model.VMT.Year <- c("2008","2017")

Calculate.PLDV.Impacts <- function(CRF,AmbientPM.Year,Mortality.Year,Emissions.Year,VMT.Year)
{
  
  aux.CRF <- as.character(CRF)
  aux.PM.Yr <- as.character(AmbientPM.Year)
  aux.Mortality.Yr <- as.character(Mortality.Year)
  aux.Em.Yr <- as.character(Emissions.Year)
  aux.VMT.Yr <- as.character(VMT.Year)
  if (!aux.CRF %in% Model.CRF)
  {
    stop("Wrong CRF Input -- select 'GEMM', Vodonos.Parametric', 'Vodonos.Spline', or 'Krewski'")
  }
  if (!aux.PM.Yr %in% Model.Ambient.PM.Year)
  {
    stop("Wrong Ambient PM Year Input -- select 2008 or 2017")
  }
  if (!aux.Mortality.Yr %in% Model.Mortality.Year)
  {
    stop("Wrong Mortality Year Input -- select 2008 or 2017")
  }
  if (!aux.Em.Yr %in% Model.Emissions.Year)
  {
    stop("Wrong Emissions Year Input -- select 2008 or 2017")
  }
  if (!aux.VMT.Yr %in% Model.VMT.Year)
  {
    stop("Wrong Emissions Year Input -- select 2008 or 2017")
  }

  PLDV.Monetized <- matrix(rep(0,3108*5),ncol=5,
                           dimnames=list(c(County.List$STCOU),c("PM25", "SO2", "NOX", "NH3", "VOC")))
  aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",aux.CRF,"_BasePM_",aux.PM.Yr,"_Mortality_",aux.Mortality.Yr)))
  aux.emis <- eval(parse(text=paste0("NEI",aux.Em.Yr,".VTYPE.EF")))
  aux.VMT <- eval(parse(text=paste0("NEI",aux.VMT.Yr,".VTYPE.EF")))
  for (p in 1:5)
  {
    aux.nonref.VMT <- rowSums(as.matrix(aux.emis[,Pol.Columns[p],PLDV.Slices] * aux.VMT[,3,PLDV.Slices])) * (1/1e6)
    aux.marg.crf.pol <- aux.marg.crf[,p] * aux.nonref.VMT
    PLDV.Monetized[,p] <- aux.marg.crf.pol
  }  
  return(PLDV.Monetized)
}

#Impacts in 2017:
#GEMM CRF, 2017 EFs emission scenario
GEMM.PLDV <- Calculate.PLDV.Impacts(CRF="GEMM",AmbientPM.Year=2017,Mortality.Year=2017,Emissions.Year=2017,VMT.Year=2017)

#Krewski CRF, 2017 EFs emission scenario
KRE.PLDV <- Calculate.PLDV.Impacts(CRF="Krewski",AmbientPM.Year=2017,Mortality.Year=2017,Emissions.Year=2017,VMT.Year=2017)

#Vodonos CRF, 2017 EFs emission scenario
VOD.PLDV <- Calculate.PLDV.Impacts(CRF="Vodonos.Spline",AmbientPM.Year=2017,Mortality.Year=2017,Emissions.Year=2017,VMT.Year=2017)

#Impacts in 2008, using 2008 emission factors and GEMM CRF:
GEMM.PLDV.08 <- Calculate.PLDV.Impacts(CRF="GEMM",AmbientPM.Year=2008,Mortality.Year=2008,Emissions.Year=2008,VMT.Year=2008)

######       4.6.2. Printing Results ######
#Each of these results (GEMM.PLDV, KRE.PLDV, VOD.PLDV, GEMM.PLDV.08) is a 3,108 x 5 matrix where:
#Each line is a county (denoted by FIPS State + County code)
#Each column is a pollutant
#All values are in USD
GEMM.PLDV

#Sum of monetized PLDV impacts in 2017 under 2017 EFs scenario and using GEMM CRFs and 2017:
sum(GEMM.PLDV) #109 billion USD 
#Attributable deaths can be estimated by dividing monetized impacts by [discounted] VSL
sum(GEMM.PLDV)/(VSL*Sum.Present.Value) #12028 attributable deaths


######       4.6.3. Further processing some results shown in manuscript ######
aux.MSA.counties <- which(County.List$STCOU %in% MSA.County.List$STCOU)
aux.MSA.T10.counties <- which(County.List$STCOU %in% MSA.to.County.T10$STCOU)

#PLDVs -- For the entire US
100*(colSums(GEMM.PLDV)/sum(NEI2017.VTYPE.EF[,3,PLDV.Slices])) #Cents/mi by pollutant for entire US
100*(sum(GEMM.PLDV)/sum(NEI2017.VTYPE.EF[,3,PLDV.Slices])) #4 cents/mi in aggregate

#PLDVs -- For Large MSAs only
MSA.PLDV <- 100*(colSums(GEMM.PLDV[aux.MSA.counties,])/sum(NEI2017.VTYPE.EF[aux.MSA.counties,3,PLDV.Slices])) 
MSA.PLDV #Cents/mi by pollutant for the 53 Large MSAs
100*(sum(GEMM.PLDV[aux.MSA.counties,])/sum(NEI2017.VTYPE.EF[aux.MSA.counties,3,PLDV.Slices])) #5.1 cents/mi in aggregate

#PLDVs -- Outside Large MSAs
OutsideMSA.PLDV <- 100*(colSums(GEMM.PLDV[-aux.MSA.counties,])/sum(NEI2017.VTYPE.EF[-aux.MSA.counties,3,PLDV.Slices])) 
OutsideMSA.PLDV #Cents/mi by pollutant outside the 53 large MSAs
100*(sum(GEMM.PLDV[-aux.MSA.counties,])/sum(NEI2017.VTYPE.EF[-aux.MSA.counties,3,PLDV.Slices])) #3.0 cents/mi in aggregate

#PLDVs -- For 10 largest MSAs only
T10MSA.PLDV <- 100*(colSums(GEMM.PLDV[aux.MSA.T10.counties,])/sum(NEI2017.VTYPE.EF[aux.MSA.T10.counties,3,PLDV.Slices])) 
T10MSA.PLDV #Cents/mi by pollutant for the 10 Largest MSAs
100*(sum(GEMM.PLDV[aux.MSA.T10.counties,])/sum(NEI2017.VTYPE.EF[aux.MSA.T10.counties,3,PLDV.Slices])) #6.2 cents/mi in aggregate


###### 5. Writing Supplemental Results Datasets ######

Pollutant.List <- c("PM25_","SO2_","NOX_","NH3_","VOC_","SUM_")
CRF.List <- c("GEMM","VodonosParametric","VodonosSpline","Krewski")
Outcomes.List <- c("2017Mortality_2017PM_USD_","2017Mortality_2017PM_Deaths_","2017Mortality_2008PM_Deaths_","2008Mortality_2017PM_Deaths_","2008Mortality_2008PM_Deaths_")
Emissions.List <- c("PER_TONNE","PASSENGER_LDV_PER_MILE")
AuxiliaryColumns.List <- c("FIPS_STCOU","COUNTY_NAME","FIPS_STATE","STATE_NAME","CBSA_CODE","CBSA_NAME")

aux.MSA <- subset(MSA.County.List, select=c("STCOU","CBSA","CBSA.NAME"))
names(aux.MSA) <- c("FIPS_STCOU","CBSA_CODE_Metro_Area_Code","CBSA_NAME_Metro_Area_NAME")
names(County.List.Full) <- AuxiliaryColumns.List[1:4]
AuxiliaryColumns <- merge(x=County.List.Full, y=aux.MSA, by="FIPS_STCOU", all=TRUE)
names(AuxiliaryColumns)
AuxiliaryColumns$COUNTY_IN_LARGE_MSA <- "YES"
AuxiliaryColumns$COUNTY_IN_LARGE_MSA[is.na(AuxiliaryColumns$CBSA_CODE_Metro_Area_Code)] <- "NO"
table(AuxiliaryColumns$COUNTY_IN_LARGE_MSA,AuxiliaryColumns$CBSA_CODE_Metro_Area_Code)

AuxiliaryColumns <- subset(AuxiliaryColumns, select=c(names(County.List.Full),"COUNTY_IN_LARGE_MSA",names(aux.MSA[,-1])))
orig.cols <- dim(AuxiliaryColumns)[2]

aux.grid <- expand.grid(Pollutant.List[1:5],Outcomes.List,CRF.List, stringsAsFactors = FALSE)
Column.List <- apply(aux.grid,MARGIN=1,FUN=paste, collapse="")
aux.matrix <- matrix(rep(0,length(AuxiliaryColumns$FIPS_STCOU)*length(Column.List)),ncol=length(Column.List))
colnames(aux.matrix) <- Column.List


######    5.1. Writing Supplemental Datasets S1 and S2 (S1: marginal damages, S2: PLDV impacts in cents/mi) ######
Dataset.Marg.Damages <- cbind(AuxiliaryColumns,aux.matrix)

aux.grid <- expand.grid(Pollutant.List,CRF.List, stringsAsFactors = FALSE)
Column.List <- apply(aux.grid,MARGIN=1,FUN=paste, collapse="")
aux.matrix <- matrix(rep(0,length(AuxiliaryColumns$FIPS_STCOU)*length(Column.List)),ncol=length(Column.List))
colnames(aux.matrix) <- Column.List
Dataset.PLDV.CentsMi <- cbind(AuxiliaryColumns,aux.matrix)


PM.Yr <- c(2017,2017,2008,2017,2008)
Mortality.Yr <- c(2017,2017,2017,2008,2008)
aux.VSL.Mult <- (1/VSL) * (1/Present.Value.Multiplier)
Use.VSL <- c(1,rep(aux.VSL.Mult,4))

for (i in 1:length(Model.CRF))
{
  for (aux.outcome in 1:5)
  {
    aux.PM.Yr <- PM.Yr[aux.outcome]
    aux.Mortality.Yr <- Mortality.Yr[aux.outcome]
    aux.VSL <- Use.VSL[aux.outcome]
    aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",Model.CRF[i],"_BasePM_",aux.PM.Yr,"_Mortality_",aux.Mortality.Yr)))
    Dataset.Marg.Damages[,(orig.cols+5*(aux.outcome-1)+(25*(i-1)))+(1:5)] <- aux.marg.crf * Use.VSL[aux.outcome]
  }
  aux.outcome <- 1
  aux.PM.Yr <- PM.Yr[aux.outcome]
  aux.Mortality.Yr <- Mortality.Yr[aux.outcome]
  aux.PLDV <- Calculate.PLDV.Impacts(CRF=Model.CRF[i],AmbientPM.Year=aux.PM.Yr,Mortality.Year=aux.Mortality.Yr,Emissions.Year=2017,VMT.Year=2017)
  aux.PLDV.VMT <- rowSums(NEI2017.VTYPE.EF[,3,2:3])
  aux.cents.mi <- 100*(aux.PLDV/aux.PLDV.VMT)
  Dataset.PLDV.CentsMi[,(orig.cols+6*(i-1))+(1:5)] <- aux.cents.mi[,1:5]
  Dataset.PLDV.CentsMi[,(orig.cols+6*(i-1))+(6)] <- rowSums(aux.cents.mi)
}

write.csv(Dataset.Marg.Damages,file=paste0(SupplementalDatasets.path,"Dataset_S1.csv"),row.names=FALSE)
write.csv(Dataset.PLDV.CentsMi,file=paste0(SupplementalDatasets.path,"Dataset_S2.csv"),row.names=FALSE)


######    5.2. Writing Supplemental Datasets S3 and S4 (S3: Emission Factors from NEI, S4: Refueling Emission Factors from NEI) ######
NEI.Yr <- c(2008,2011,2014,2017)
aux.data <- NEI2008.VTYPE.EF[1,,1]
Emissions.Dataset <- as.data.frame(matrix(rep(-1,18),ncol=18))
NEI.Names <- c("STCOU","Vehicle_Type_VTYPE","VMT",paste0(Pol.Names,"_EF_grams_per_mile"))
names(Emissions.Dataset) <- c(names(AuxiliaryColumns),"NEI_Data_Year",NEI.Names[-1])

Emissions.Ref.Dataset <- as.data.frame(matrix(rep(-1,12),ncol=12))
NEI.REF.Names <- c("STCOU","Vehicle_Type_VTYPE","VMT_SUM_FROM_ALL_VEHICLE_TYPES","VOC_EMISSIONS_SHORT_TONS","VOC_EF_grams_per_mile")
names(Emissions.Ref.Dataset) <- c(names(AuxiliaryColumns),"NEI_Data_Year",NEI.REF.Names[-1])

for (i in 1:4)
{
  aux.nei <- eval(parse(text=paste0("NEI",NEI.Yr[i],".VTYPE.EF")))
  for (j in 1:13)
  {
    aux.data1 <- aux.nei[,-1,j]
    colnames(aux.data1) <- NEI.Names[-1]
    aux.nei.yr <- matrix(rep(NEI.Yr[i],dim(aux.data1)[1]),ncol=1)
    colnames(aux.nei.yr) <- "NEI_Data_Year"
    aux.data2 <- cbind(AuxiliaryColumns,aux.nei.yr,aux.data1)
    Emissions.Dataset <- rbind(Emissions.Dataset,aux.data2)
  }
  if(i>1)
  {
    aux.nei.ref <- eval(parse(text=paste0("NEI",NEI.Yr[i],".REF.EMIS")))
    aux.data1 <- aux.nei.ref[,-1]
    colnames(aux.data1) <- NEI.REF.Names[-1]
    aux.nei.yr <- matrix(rep(NEI.Yr[i],dim(aux.data1)[1]),ncol=1)
    colnames(aux.nei.yr) <- "NEI_Data_Year"
    aux.data2 <- cbind(AuxiliaryColumns,aux.nei.yr,aux.data1)
    Emissions.Ref.Dataset <- rbind(Emissions.Ref.Dataset,aux.data2)
  }
}
Emissions.Dataset <- Emissions.Dataset[-1,]
Emissions.Ref.Dataset <- Emissions.Ref.Dataset[-1,]

Emissions.Dataset.2 <- Emissions.Dataset[order(Emissions.Dataset$FIPS_STCOU, Emissions.Dataset$NEI_Data_Year, Emissions.Dataset$Vehicle_Type_VTYPE, decreasing=FALSE),]
row.names(Emissions.Dataset.2) <- c()
Emissions.Ref.Dataset.2 <- Emissions.Ref.Dataset[order(Emissions.Ref.Dataset$FIPS_STCOU, Emissions.Ref.Dataset$NEI_Data_Year, Emissions.Ref.Dataset$Vehicle_Type_VTYPE, decreasing=FALSE),]
row.names(Emissions.Ref.Dataset.2) <- c()

write.csv(Emissions.Dataset.2, file=paste0(SupplementalDatasets.path,"Dataset_S3.csv"),row.names=FALSE)
write.csv(Emissions.Ref.Dataset.2, file=paste0(SupplementalDatasets.path,"Dataset_S4.csv"),row.names=FALSE)

######    5.3. Writing Supplemental Datasets S5 (S5: Baseline Ambient PM2.5 levels) ######
sum(AuxiliaryColumns$FIPS_STCOU == InputPM25$STCOU)
sum(AuxiliaryColumns$FIPS_STCOU != InputPM25$STCOU)
Baseline_Ambient_PM25 <- merge(x=AuxiliaryColumns, y=InputPM25, by.x="FIPS_STCOU",by.y="STCOU",all=TRUE)
Baseline_Ambient_PM25 <- Baseline_Ambient_PM25[order(Baseline_Ambient_PM25$FIPS_STCOU, decreasing=FALSE),]
names(Baseline_Ambient_PM25)[8:9] <- c("Ambient_PM25_2008","Ambient_PM25_2017")

write.csv(Baseline_Ambient_PM25, file=paste0(SupplementalDatasets.path,"Dataset_S5.csv"),row.names=FALSE)


###### 6. Plotting Figures ######

######    6.1. Plotting Figure 1 ######
col.vclass <- c("darkorange","magenta","red","yellow","purple4",gray(0.8))

aux.xlim2 <- c(300,40,8000,150,4000,2350)
aux.xax <- list(50*0:6,10*0:4,2000*0:4,25*0:6,1000*0:4,500*0:4)

ylab <- c("2008","2011","2014","2017")
xlab1 <- c("PM","SO","NO","NH","VOC","CO")
xlab2 <- c("2.5","2","x","3","","2")
xlab2b <- c("","","","","","-eq.")
xlab3 <- c("3","3","3","3","3","6")

rec.height <- 0.35
rec.dens <- c(NA,NA,NA,NA,NA,NA)
rec.angle <- c(NA,NA,NA,NA,NA,NA)

aux.adj.f <- 0.8

wd.f1 <- 11/2.54 #Figure witdh
ht.f1 <- 0.7*wd.f1 #Figure height
lwd.f1 <- (wd.f1/10)*aux.adj.f #Figure line width
ps.f1 <- (wd.f1/10)*aux.adj.f #Figure point size

plot(0)
pdf(eval(paste0(Figure.path,"Figure_1.pdf")),height=ht.f1,width=wd.f1, pointsize = ps.f1)

par(mfcol=c(3,2),mar=c(3.8,5,0.5,2), oma=c(0.1,0.1,1.5,0.1), mgp=c(1.8,0.5,0), cex.lab=1.1, xaxs="i", lwd=lwd.f1)
for (i in 1:6)
{

  plot(NA, xlim=c(0,aux.xlim2[i]), ylim=c(0.5,4.5), xaxt="n", yaxt="n",
       ylab="",
       xlab=bquote(bold(paste(.(xlab1[i])[.(xlab2[i])],.(xlab2b[i])," emissions [10"^.(xlab3[i])," tonnes]"))),
       main=NA)
  aux.yb1 <- c(1:4 - rec.height)
  aux.yt1 <- c(1:4 + rec.height)
  
  axis(side=1, at=aux.xax[[i]],labels=aux.xax[[i]], cex.axis=1, font=2, lwd=lwd.f1)
  axis(side=2, at=c(1:4), labels=ylab,las=2, cex=1, font=2, lwd=lwd.f1)
  
  aux.Fig1.slice <- as.matrix(Aggregate.Emissions[,-1,i])
  aux.Fig1.slice <- cbind(rep(0,dim(aux.Fig1.slice)[1]),aux.Fig1.slice)
  
  for(j in 2:7)
  {
    rect(xleft=rowSums(as.matrix(aux.Fig1.slice[,(1:j-1)]))/(10^as.numeric(xlab3[i])),
         xright=rowSums(as.matrix(aux.Fig1.slice[,(1:j)]))/(10^as.numeric(xlab3[i])),
         ytop=(aux.yt1),
         ybottom=(aux.yb1),
         col=col.vclass[j-1],
         border="black",
         density=rec.dens[j-1], angle=rec.angle[j-1])
  }
  
  mtext("Emissions Scenario",side=2,line=3.5,font=2,srt=90, cex=0.8)
  if (i!=5)
  {
    legend("topright",
           c(expression(paste("LDVs",sep="")),
             expression(paste("HDTs",sep="")),
             expression(paste("Buses",sep="")),
             expression(paste("Motorcycles",sep="")),
             expression(paste("2017 VMT Adjust.",sep=""))),
           pch=22, pt.bg=c(col.vclass[c(1:4,6)]), col="black", pt.cex=1.5, cex=0.8, bty="n")
  } else {
    legend("topright",
           c(expression(paste("LDVs",sep="")),
             expression(paste("HDTs",sep="")),
             expression(paste("Buses",sep="")),
             expression(paste("Motorcycles",sep="")),
             expression(paste("Refueling",sep="")),
             expression(paste("2017 VMT Adjust.",sep=""))),
           pch=22, pt.bg=c(col.vclass), col="black", pt.cex=1.5, cex=0.8, bty="n")
  }
  mtext("Emissions by vehicle class and pollutant: 2008-2017", cex=1, outer=TRUE, font=2, line=0.1, side=3)
}
dev.off()

######    6.2. Plotting Figure 2, Figure S4, and Figure S5 ######

col.pol.cars <- c("aquamarine","navy","deepskyblue","chartreuse","darkgreen","darksalmon")
aux.xlim <- list(c(0,600),c(0,1200),c(0,500))
aux.ylim <- c(0,3.5)
rec.height <- 0.7
rec.dens <- c(NA,NA,NA,NA,NA,NA)
rec.angle <- c(NA,NA,NA,NA,NA,NA)
aux.title2 <- c("test","High SCC, CRF from Vodonos et al. [10]","Average SCC, CRF from Krewski et al. [12]")
aux.uppermargin <- c(3,4,4)

aux.Fig2 <- array(rep(0,4*6*6),dim=c(4,6,6))
aux.Fig2[,,1] <- as.matrix(Base.Monetized.Impacts[,,1])
aux.Fig2[,,2] <- as.matrix(Base.Monetized.Impacts[,,2])
aux.Fig2[,,3] <- as.matrix(High.Monetized.Impacts[,,1])
aux.Fig2[,,4] <- as.matrix(High.Monetized.Impacts[,,2])
aux.Fig2[,,5] <- as.matrix(Low.Monetized.Impacts[,,1])
aux.Fig2[,,6] <- as.matrix(Low.Monetized.Impacts[,,2])

aux.adj.f <- 0.8

wd.f2 <- 11/2.54
ht.f2 <- 0.4*wd.f2
lwd.f2 <- (wd.f2/10)*aux.adj.f
ps.f2 <- (wd.f2/10)*aux.adj.f

plot(0)
Plot.names <- paste0(Figure.path,c("Figure_2","Figure_S4","Figure_S5"),".pdf")

for (plot.iter in 1:3)
{
  pdf(bquote(.(Plot.names[plot.iter])),height=ht.f2,width=wd.f2, pointsize = ps.f2)
  par(mar=c(3,4.5,aux.uppermargin[plot.iter],2),mgp=c(1.8,1,0), lwd=lwd.f2)
  plot(NA, xlim=aux.xlim[[plot.iter]], ylim=aux.ylim, xaxt="n", yaxt="n",
       ylab="",xlab=bquote(bold("USD: billions/year in 2017")),
       main="")
  if (plot.iter>1)
  {
    title(main="Social cost of emissions and benefits of emission reductions (2008-2017) in 2017", font=2, line=2.5)
    title(main=aux.title2[plot.iter], font=2, line=1)
  } else {
    title(main="Social cost of emissions and benefits of emission reductions (2008-2017) in 2017", font=2, line=1.5)
  }
  
  aux.yb1 <- c((1:3))-rec.height/2
  aux.yt1 <- c((1:3))+rec.height/2
  
  aux.Fig2.Ben <- as.matrix(aux.Fig2[-1,,((2*plot.iter)-1)] - aux.Fig2[-1,,(2*plot.iter)])
  aux.Fig2.Ben <- cbind(rep(0,dim(aux.Fig2.Ben)[1]),aux.Fig2.Ben)
  aux.Fig2.Imp <- aux.Fig2[-1,,(2*plot.iter)]
  aux.Fig2.Imp <- cbind(rep(0,dim(aux.Fig2.Imp)[1]),aux.Fig2.Imp)
  
  mid.pt <- mean(aux.xlim[[plot.iter]])
  # abline(v=50*0:(2*(mid.pt/50)),lty=2,lwd=0.85,col=c(gray(0)))
  
  
  for(j in 2:7)
  {
    
    rect(xleft=mid.pt+(rowSums(as.matrix(aux.Fig2.Ben[,(1:j-1)]))),
         xright=mid.pt+(rowSums(as.matrix(aux.Fig2.Ben[,(1:j)]))),
         ytop=aux.yt1,
         ybottom=aux.yb1,
         col=col.pol.cars[j-1],
         density=rec.dens[j-1],
         angle=rec.angle[j-1],
         border="black")
    
    rect(xleft=mid.pt-(rowSums(as.matrix(aux.Fig2.Imp[,(1:j-1)]))),
         xright=mid.pt-(rowSums(as.matrix(aux.Fig2.Imp[,(1:j)]))),
         ytop=aux.yt1,
         ybottom=aux.yb1,
         col=col.pol.cars[j-1],
         density=rec.dens[j-1],
         angle=rec.angle[j-1],
         border="black")
  }
  
  legend(expression(bold("Cost of current (2017) emissions")), 
         x=mid.pt, y=0.3,cex=0.8, xjust=1, yjust=0.5, bty="o", bg="white", box.lwd=NA)
  legend(expression(bold("Benefits of emission reductions since 2008")), 
         x=mid.pt, y=0.3, cex=0.8, xjust=0, yjust=0.5, bty="o", bg="white", box.lwd=NA)
  mtext("Vehicle types",side=2,line=3,font=2,srt=90,at=2)
  aux.leg <- c(expression(paste("Primary PM"[2.5],"",sep="")),
               expression(paste("SO"[2],"",sep="")),
               expression(paste("NO"["x"],"",sep="")),
               expression(paste("NH"[3],"",sep="")),
               expression(paste("VOC", "",sep="")),
               expression(paste("CO"[2],"-eq.")))
  legend("right",
         legend=aux.leg,
         pch=c(rep(22,6)), col="black", 
         pt.bg=col.pol.cars, pt.cex=c(rep(1.5,6)), 
         cex=0.7, box.lwd=NA, bg="white", y.intersp = 0.9)
  box(which="plot",lty="solid")
  abline(v=mid.pt,lty=1,lwd=2*lwd.f2,col=gray(0))
  
  par(mgp=c(2,1,0))
  axis(side=2, at=c(1,2,3), labels=c("HDT","LDV","ALL"),las=2, cex.axis=0.8, font=2, lwd=lwd.f2)
  # axis(side=2, at=c(2.85,3.15), labels=c("(All Vehicles)","Road      "),las=2, cex.axis=0.8, font=2, tick=FALSE, lwd=lwd.f2)
  par(mgp=c(0.5,0.5,0))
  if(plot.iter == 2)
  {
    aux.labels <- c((100*(mid.pt/100):1),100*0:(mid.pt/100))
    axis(side=1, at=100*0:(2*(mid.pt/100)), labels=aux.labels, cex.axis=0.8, font=2, lwd=lwd.f2)
  } else {
    aux.labels <- c((50*(mid.pt/50):1),50*0:(mid.pt/50))
    axis(side=1, at=50*0:(2*(mid.pt/50)), labels=aux.labels, cex.axis=0.8, font=2, lwd=lwd.f2)
  }
  
  dev.off()
}

######    6.3. Plotting Figure 3 and Figure S1 ######

aux.ylim <- list(c(-6.3,2.2),c(-4.5,2.2))
aux.ht <- c(10,10*((aux.ylim[[2]][2]-aux.ylim[[2]][1])/(aux.ylim[[1]][2]-aux.ylim[[1]][1])))
aux.wd <- 10
aux.xlim <- list(c(0,71000),c(0,37000))
aux.xaxis <- list(c(10,10),c(5,5))

col.effects <- c("blue","turquoise1","turquoise4","violetred","violetred4","purple",
                 "slategray1","slategray3","slategray4",gray(0.15))
col.pol.cars <- c("aquamarine","navy","deepskyblue","chartreuse","darkgreen","darksalmon")
col.vclass <- c("darkorange","magenta","red","yellow","purple4",gray(0.5))

rec.dens <- c(NA,NA,NA,NA,NA,NA)
rec.angle <- c(NA,NA,NA,NA,NA,NA)

aux.vehicle.select <- list(c(2:6),c(1))
aux.vehicle.select2 <- list(c(4),c(1))

Plot.names <- c(paste0(Figure.path,"Figure_3",".pdf"),paste0(Figure.path,"Figure_S1",".pdf"))
Plot.title.names <- c("on-road transportation", "passenger light-duty vehicles")

aux.adj.f <- 1
aux.cex.adj.f <- 1.5

wd.f3 <- 18/2.54
ht.f3 <- (wd.f3/10)*c(10,7.882353)
lwd.f3 <- (wd.f3/10)*aux.adj.f
ps.f3 <- (wd.f3/10)*aux.adj.f

plot(0)

for (plot.iter in 1:2)
{
  plot(0)
  
  pdf(bquote(.(Plot.names[plot.iter])),height=ht.f3[plot.iter],width=wd.f3, pointsize=ps.f3)
  par(mar=c(5,9.25,3,1),mgp=c(3,1,0), xaxs="i", yaxs="i",lwd=lwd.f3, cex.axis=aux.cex.adj.f)
  plot(NA, xlim=aux.xlim[[plot.iter]], ylim=aux.ylim[[plot.iter]], xaxt="n", yaxt="n",
       ylab="",xlab=bquote(bold(paste("PM"["2.5"],"-attributable deaths in 2017 [thousands]"))),
       main="", cex.lab=1.3*aux.cex.adj.f)
  title(main=bquote(bold(paste("PM"["2.5"],"-attributable deaths in 2017 from ", .(Plot.title.names[plot.iter]) ," in the U.S."))), font=2, line=1,cex.main=1.3*aux.cex.adj.f*0.95)

  aux.yoffset <- 0.2
  aux.ybottom1 <- c(aux.yoffset+(c(3:0)*(0.1+0.3)))
  aux.ytop1 <- aux.ybottom1 + 0.3

  aux.yoffset2 <- 0.5
  aux.ylimb <- -2.4
  aux.ybottom2 <- c(aux.ylimb+aux.yoffset2)
  aux.ytop2 <- c(aux.ybottom2+0.3)

  aux.yoffset3 <- aux.ylimb - 0.5 - 0.3
  aux.ytop3 <- aux.yoffset3 - (c(0:3)*(0.10+0.20))
  aux.ybottom3 <- aux.ytop3 - 0.20

  aux.yoffset4 <- aux.ybottom3[4] - 0.1 - 0.3
  aux.ytop4 <- aux.yoffset4 - (c(0:1)*(0.10+0.20))
  aux.ybottom4 <- aux.ytop4 - 0.2

  aux.yoffset5 <- aux.ybottom4[2] - 0.1 - 0.3
  aux.ytop5 <- aux.yoffset5 - (c(0:1)*(0.10+0.20))
  aux.ybottom5 <- aux.ytop5 - 0.2
  
  aux.veh <- aux.vehicle.select[[plot.iter]]
  aux.veh2 <- aux.vehicle.select2[[plot.iter]]
  if(length(aux.veh)>1)
  {
    Vehicle.Class.Deaths <- colSums(aperm(GEMM.Attributable.Deaths[aux.veh,,], perm=c(2,3,1)))
  } else {
    Vehicle.Class.Deaths <- as.matrix(colSums(GEMM.Attributable.Deaths[aux.veh,,]))
  }
  Vehicle.Class.Deaths <- cbind(rep(0,dim(Vehicle.Class.Deaths)[1]),Vehicle.Class.Deaths)
  Vehicle.Class.Deaths
  
  #Using The SI figure for PLDV as a 'default', then re-calculating if all vehicle classes (i.e., main text figure)
  Pollutant.Deaths <- t(GEMM.Attributable.Deaths[aux.veh[1],,])
  Pollutant.Deaths <- cbind(rep(0,dim(Pollutant.Deaths)[1]),Pollutant.Deaths)
  ALL.Pollutant.Deaths <- Pollutant.Deaths
  if(length(aux.veh)>1)
  {
    for (s in 2:length(aux.veh))
    {
      Pollutant.Deaths <- cbind(Pollutant.Deaths,t(GEMM.Attributable.Deaths[aux.veh[s],,]))
    }
  }
  if(plot.iter == 1)
  {
    ALL.Pollutant.Deaths <- cbind(rep(0,dim(Pollutant.Deaths)[1]),t(colSums(GEMM.Attributable.Deaths[-1,,])))
    LDV.Pollutant.Deaths <- t(GEMM.Attributable.Deaths[2,,c(1,4)])
    LDV.Pollutant.Deaths <- cbind(rep(0,dim(LDV.Pollutant.Deaths)[1]),LDV.Pollutant.Deaths)
    HDT.Pollutant.Deaths <- t(GEMM.Attributable.Deaths[3,,c(1,4)])
    HDT.Pollutant.Deaths <- cbind(rep(0,dim(HDT.Pollutant.Deaths)[1]),HDT.Pollutant.Deaths)
  } 
  
  
  #Pollutant.Deaths
  if(plot.iter==2)
  {
    col.vclass[1] <- "darkgoldenrod1"
  }
  aux.Base.Case.Effects <- c(0,Base.Case.Effects[aux.veh2,])
  Vehicle.Class.Deaths[,1:2]
  for(j in 2:dim(Vehicle.Class.Deaths)[2])
  {
    rect(xleft=rowSums(as.matrix(Vehicle.Class.Deaths[,(1:j-1)])),
         xright=rowSums(as.matrix(Vehicle.Class.Deaths[,(1:j)])),
         ytop=(aux.ytop1),
         ybottom=(aux.ybottom1),
         col=col.vclass[j-1],
         border="black",
         density=rec.dens[j-1], angle=rec.angle[j-1])
  }
  aux.col.pols <- c(rep(col.pol.cars[1:5],5))

  for(j in 2:dim(ALL.Pollutant.Deaths)[2])
  {
    rect(xleft=rowSums(as.matrix(ALL.Pollutant.Deaths[,(1:j-1)])),
         xright=rowSums(as.matrix(ALL.Pollutant.Deaths[,(1:j)])),
         ytop=(aux.ytop3),
         ybottom=(aux.ybottom3),
         col=aux.col.pols[j-1],
         border=NA,
         density=rec.dens[1], angle=rec.angle[1])
    
    if (plot.iter==1)
    {
      rect(xleft=rowSums(as.matrix(LDV.Pollutant.Deaths[,(1:j-1)])),
           xright=rowSums(as.matrix(LDV.Pollutant.Deaths[,(1:j)])),
           ytop=(aux.ytop4),
           ybottom=(aux.ybottom4),
           col=aux.col.pols[j-1],
           border=NA,
           density=rec.dens[1], angle=rec.angle[1])
      
      rect(xleft=rowSums(as.matrix(HDT.Pollutant.Deaths[,(1:j-1)])),
           xright=rowSums(as.matrix(HDT.Pollutant.Deaths[,(1:j)])),
           ytop=(aux.ytop5),
           ybottom=(aux.ybottom5),
           col=aux.col.pols[j-1],
           border=NA,
           density=rec.dens[1], angle=rec.angle[1])
    }
  }
  for (j in 1:length(Base.Case.Effects))
  {
    rect(xleft=sum(aux.Base.Case.Effects[(1:j-1)]),
         xright=sum(aux.Base.Case.Effects[(1:j)]),
         ytop=(aux.ytop2),
         ybottom=(aux.ybottom2),
         col=col.effects[j-1],
         border="black",
         density=rec.dens[1], angle=rec.angle[1])
  }
 
  aux.Base.Case.Effects[2]
  sum(aux.Base.Case.Effects) - aux.Base.Case.Effects[2]
  sum(aux.Base.Case.Effects[-c(1,2)])/aux.Base.Case.Effects[2]
  aux.deaths <- list(as.character(c(30800,21400,69)),as.character(c(15900,10200,64)))

  text(bquote(bold(paste("PM"["2.5"],"-attributable deaths in 2008"))),
       y=aux.ytop2+0.05,x=0.5*aux.Base.Case.Effects[2],
       adj=c(0.5,0),font=2,cex=0.8*aux.cex.adj.f,col=col.effects[1])

  aux.text <- c(as.numeric(100*round(aux.Base.Case.Effects[2]/100,0)),
                paste0("+",round((100*as.numeric(aux.Base.Case.Effects[3:11]/aux.Base.Case.Effects[2])),0),"%"))
  
  aux.text.leg <- c("",
                    expression(bold(paste("Population increase"))),
                    expression(bold(paste("Aging population"))),
                    expression(bold(paste("Overall fleet VMT"))),
                    expression(bold(paste("Fleet composition"))),
                    expression(bold(paste("Lower PM"[2.5]," levels",sep=""))),
                    expression(bold(paste("VMT * Mortality"))),
                    expression(bold(paste("VMT * PM"[2.5],"",sep=""))),
                    expression(bold(paste("Mortality * PM"[2.5],"",sep=""))),
                    expression(bold(paste("VMT * Mortality * PM"[2.5],"",sep=""))))
  
  for (i in 1:length(aux.text))
  {
    if (i == 1)
    {
      aux.adj <- c(0.5,1)
      aux.srt <- 0
      aux.adj2 <- c(0.5,1)
      aux.srt2 <- 0
    } else {
      aux.adj <- c(1,0.5)
      aux.srt <- 90
      aux.adj2 <- c(0,0.5)
      aux.srt2 <- 60
    }
    text(aux.text[i],
         x=(sum(aux.Base.Case.Effects[1:i]) + sum(aux.Base.Case.Effects[1:(i+1)]))/2,
         y=(aux.ybottom2 - 0.05),
         adj=aux.adj,font=2,cex=0.8*aux.cex.adj.f, srt=aux.srt, col=col.effects[i])
    
    if(i<(length(aux.text)-1))
    {
      aux.text.x <- (sum(aux.Base.Case.Effects[1:i]) + sum(aux.Base.Case.Effects[1:(i+1)]))/2
      aux.text.y <- (aux.ytop2+0.05)
    } else {
      if(i==(length(aux.text)-1))
      {
        aux.text.x <- ((sum(aux.Base.Case.Effects[1:i]) + sum(aux.Base.Case.Effects[1:(i+1)]))/2)+200
        aux.text.y <- (aux.ytop2+0.05)
      } else {
        aux.text.x <- sum(aux.Base.Case.Effects[1:(i+1)])+400
        aux.text.y <- (aux.ytop2+aux.ybottom2)/2
      }
      
    }
    
    text(aux.text.leg[i],
         x=aux.text.x,
         y=aux.text.y,
         adj=aux.adj2,font=2,cex=0.8*aux.cex.adj.f, srt=aux.srt2, col=col.effects[i])
  }
  
  aux.leg.x <- c(53500,27500)
  
  if(plot.iter==1)
  {
    aux.legend.text <- c(expression(bold(paste("Vehicle Classes",sep=""))),
                         expression(paste("Light-Duty Vehicles",sep="")),
                         expression(paste("Heavy-Duty Trucks",sep="")),
                         expression(paste("Motorcycles",sep="")),
                         expression(paste("Buses",sep="")),
                         expression(paste("Refueling",sep="")))
    legend(x=aux.leg.x[plot.iter],y=aux.ybottom1[4], xjust=0, yjust=0,
           legend=aux.legend.text,
           pch=c(NA,rep(22,5)), col="black", 
           pt.bg=c(NA,col.vclass), pt.cex=c(NA,rep(1.2,5))*aux.cex.adj.f*1.2, 
           cex=1*aux.cex.adj.f, box.lwd=NA, bg="white", y.intersp = 0.85)
  }
  if(plot.iter==2)
  {
    aux.legend.text <- c(expression(bold(paste("Vehicle Classes",sep=""))),
                         expression(paste("Passenger",sep="")),
                         expression(paste("Light-Duty Vehicles",sep="")))
    
    legend(x=aux.leg.x[plot.iter],y=aux.ybottom1[4], xjust=0, yjust=0,
           legend=aux.legend.text,
           pch=c(NA,rep(22,1),NA), col="black", 
           pt.bg=c(NA,col.vclass[1],NA), pt.cex=c(NA,rep(1.2,1),NA)*aux.cex.adj.f*1.2, 
           cex=1*aux.cex.adj.f, box.lwd=NA, bg="white", y.intersp = 0.85)
  }
  
  legend(x=aux.leg.x[plot.iter],y=aux.ybottom3[4], xjust=0, yjust=0,
         c(expression(bold(paste("Pollutants"))),
           expression(paste("Primary PM"[2.5],"",sep="")),
           expression(paste("SO"[2],"",sep="")),
           expression(paste("NO"["x"],"",sep="")),
           expression(paste("NH"[3],"",sep="")),
           expression(paste("VOC", "",sep=""))),
         pch=c(NA,rep(15,5)), col=c(NA,col.pol.cars), 
         pt.bg=NA, pt.cex=c(NA,rep(1,5))*aux.cex.adj.f*1.2, 
         cex=1*aux.cex.adj.f, box.lwd=NA, bg="white", y.intersp = 0.85)
  
  
  aux.xloc.legf3 <- c(54500,28000)
  aux.ysize <- (aux.ylim[[plot.iter]][2]-aux.ylim[[plot.iter]][1])
  aux.xsize <- (aux.xlim[[plot.iter]][2]-aux.xlim[[plot.iter]][1])
  aux.y.rec.legf3 <- 0.15
  aux.x.rec.legf3 <- aux.y.rec.legf3 * (aux.xsize/aux.ysize) * (aux.ht[plot.iter]/aux.wd)
  
  rect(xleft=aux.xloc.legf3[plot.iter]+aux.x.rec.legf3*c(0:1,0:1,0,0:3),
       xright=aux.xloc.legf3[plot.iter]+aux.x.rec.legf3*(1+c(0:1,0:1,0,0:3)),
       ytop=aux.ybottom2+aux.y.rec.legf3*(1+c(4.5,4.5,3,3,1.5,0,0,0,0)),
       ybottom=aux.ybottom2+aux.y.rec.legf3*c(4.5,4.5,3,3,1.5,0,0,0,0),
       col=col.effects[-1],border="black",density=NA)
  
  aux.text.legf3 <- c(expression(bold(paste("(","Baseline",")"," Mortality",sep=""))),
                      expression(bold(paste("VMT",sep=""))),
                      expression(bold(paste("(","Baseline",")"," PM"[2.5],"",sep=""))),
                      expression(bold(paste("Interactions",sep=""))))
  text(aux.text.legf3,
       x=aux.xloc.legf3[plot.iter]+aux.x.rec.legf3*4.5,
       y=aux.ybottom2+aux.y.rec.legf3*c(5,3.5,2,0.5),
       adj=c(0,0.5),col="black",font=2,cex=0.75*aux.cex.adj.f)
  
  text("Effects",
       x=aux.xloc.legf3[plot.iter]+aux.x.rec.legf3*5,
       y=aux.ybottom2+aux.y.rec.legf3*c(7),
       adj=c(0,0.5),col="black",font=2,cex=1*aux.cex.adj.f)
  
  aux.text <- (100*round(rowSums(as.matrix(Vehicle.Class.Deaths[1:4,]))/100,0))
  aux.text.xloc <- rowSums(as.matrix(Vehicle.Class.Deaths[1:4,])) + 200
  aux.text.col <- "black"
  for (i in 1:4)
  {
    
    text(eval(aux.text[i]),
         y=(aux.ytop1[i]+aux.ybottom1[i])/2,
         x=aux.text.xloc[i],
         adj=c(0,0.5),font=2,cex=1*aux.cex.adj.f)
  }
  mtext("Emissions Scenario",side=2,line=7.25,font=2,srt=90,at=(aux.ylim[[plot.iter]][2]+aux.ylim[[plot.iter]][1])/2,cex=1.5*aux.cex.adj.f)

    box(which="plot",lty="solid")
  
  par(mgp=c(2,1,0))
  aux.ylab <- c("2008 EFs","2011 EFs","2014 EFs","2017 EFs")
  axis(side=2, at=aux.ybottom1+0.15, labels=aux.ylab,las=2, cex.axis=1.2*aux.cex.adj.f*0.8, font=2, lwd = lwd.f3)
  
  aux.ylab <- c("2008 EFs")
  axis(side=2, at=aux.ybottom2+0.15, labels=aux.ylab,las=2, cex.axis=1.2*aux.cex.adj.f*0.8, font=2, lwd=lwd.f3)
  
  aux.ylab <- c("2008 EFs","2011 EFs","2014 EFs","2017 EFs")
  axis(side=2, at=aux.ybottom3+0.1, labels=aux.ylab,las=2, cex.axis=1.2*aux.cex.adj.f*0.8, font=2, lwd=lwd.f3)
  
  if(plot.iter==1)
  {
    aux.ylab <- c("2008 EFs","2017 EFs")
    axis(side=2, at=aux.ybottom4+0.1, labels=aux.ylab,las=2, cex.axis=1.2*aux.cex.adj.f*0.8, font=2, lwd=lwd.f3)
    
    aux.ylab <- c("2008 EFs","2017 EFs")
    axis(side=2, at=aux.ybottom5+0.1, labels=aux.ylab,las=2, cex.axis=1.2*aux.cex.adj.f*0.8, font=2, lwd=lwd.f3)
  }
  
  par(mgp=c(1,1,0))

  aux.axis1.at <- c(5000*0:10)
  aux.axis1.lab <- list(c(0,"",10,"",20,"",30,"",40,"",50),c(0,5,10,15,20,25))
  
  axis(side=1, at=aux.axis1.at[1:((aux.xaxis[[plot.iter]][1])+1)],labels=aux.axis1.lab[[plot.iter]], 
       cex.axis=1.2*aux.cex.adj.f, font=2, lwd=lwd.f3)
  if(plot.iter==2)
  {
    col.vclass[1] <- "darkorange"
  }
  
  aux.text.fig3a <- c(expression(bold(paste("(a) PM"[2.5],"-attributable deaths in 2017 in each emission scenario",sep=""))))
  aux.text.fig3b <- c(expression(bold(paste("(b) Decomposition of effects for 2008 EFs emission scenario",sep=""))))
  aux.text.fig3c <- c(expression(bold(paste("(c) PM"[2.5],"-attributable deaths in 2017 by pollutant",sep=""))))
  
  text(c(aux.text.fig3a),
       x=(1/100)*aux.xlim[[plot.iter]][2],
       y=aux.ylim[[plot.iter]][2]-(2/100)*aux.ysize,col="black",adj=c(0,1),cex=1.2*aux.cex.adj.f)
  
  text(c(aux.text.fig3b),
       x=(1/100)*aux.xsize,
       y=0-(2/100)*aux.ysize,col="black",adj=c(0,1),cex=1.2*aux.cex.adj.f)
  
  text(c(aux.text.fig3c),
       x=(1/100)*aux.xsize,
       y=aux.ylimb-(2/100)*aux.ysize,col="black",adj=c(0,1),cex=1.2*aux.cex.adj.f)
  
  if (plot.iter==1)
  {
    aux.text.fig3c1 <- c(expression(bold(paste("All vehicles",sep=""))))
    aux.text.fig3c2 <- c(expression(bold(paste("Light-duty vehicles",sep=""))))
    aux.text.fig3c3 <- c(expression(bold(paste("Heavy-duty trucks",sep=""))))
    
    text(c(aux.text.fig3c1),
         x=(1/100)*aux.xsize,
         y=aux.ytop3[1]+0.05,col="black",adj=c(0,0),cex=1*aux.cex.adj.f)
    
    text(c(aux.text.fig3c2),
         x=(1/100)*aux.xsize,
         y=aux.ytop4[1]+0.05,col="black",adj=c(0,0),cex=1*aux.cex.adj.f)
    
    text(c(aux.text.fig3c3),
         x=(1/100)*aux.xsize,
         y=aux.ytop5[1]+0.05,col="black",adj=c(0,0),cex=1*aux.cex.adj.f)
    
  }
  
  abline(h=0,lty=1,lwd=2*lwd.f3,col=c(gray(0)))
  abline(h=aux.ylimb,lty=1,lwd=2*lwd.f3,col=c(gray(0)))
  
  dev.off()
}


######    6.4. Plotting Figure S6 ######

#Creating auxiliary dataset for plotting
aux.FigS6.Vehicle <- matrix(rep(0,4*3*6),ncol=6)
aux.FigS6.Vehicle[1:4,-1] <- colSums(aperm(Krewski.Attributable.Deaths[-1,,], perm=c(2,3,1)))
aux.FigS6.Vehicle[5:8,-1] <- colSums(aperm(Vodonos.Attributable.Deaths[-1,,], perm=c(2,3,1)))
aux.FigS6.Vehicle[9:12,-1] <- colSums(aperm(GEMM.Attributable.Deaths[-1,,], perm=c(2,3,1)))

aux.FigS6.Pollutant <- matrix(rep(0,4*3*26),ncol=26)
aux.FigS6.Pollutant[1:4,-1] <- cbind(t(Krewski.Attributable.Deaths[2,,]),
                                     t(Krewski.Attributable.Deaths[3,,]),
                                     t(Krewski.Attributable.Deaths[4,,]),
                                     t(Krewski.Attributable.Deaths[5,,]),
                                     t(Krewski.Attributable.Deaths[6,,]))
aux.FigS6.Pollutant[5:8,-1] <- cbind(t(Vodonos.Attributable.Deaths[2,,]),
                                     t(Vodonos.Attributable.Deaths[3,,]),
                                     t(Vodonos.Attributable.Deaths[4,,]),
                                     t(Vodonos.Attributable.Deaths[5,,]),
                                     t(Vodonos.Attributable.Deaths[6,,]))
aux.FigS6.Pollutant[9:12,-1] <- cbind(t(GEMM.Attributable.Deaths[2,,]),
                                      t(GEMM.Attributable.Deaths[3,,]),
                                      t(GEMM.Attributable.Deaths[4,,]),
                                      t(GEMM.Attributable.Deaths[5,,]),
                                      t(GEMM.Attributable.Deaths[6,,]))

aux.xlim <- c(0,90000)
aux.ylim <- c(0.25,15.25)

col.pol.cars <- c("aquamarine","navy","deepskyblue","chartreuse","darkgreen","darksalmon")
col.vclass <- c("darkorange","magenta","red","yellow","purple4",gray(0.5))
rec.dens <- c(NA,NA,NA,NA,NA,NA)
rec.angle <- c(NA,NA,NA,NA,NA,NA)

Plot.names <- c(paste0(Figure.path,"Figure_S6",".pdf"))

aux.adj.f <- 0.8
aux.cex.adj.f <- 1.3

wd.fs6 <- 18/2.54
ht.fs6 <- 0.7*wd.fs6
lwd.fs6 <- (wd.fs6/10)*aux.adj.f
ps.fs6 <- (wd.fs6/10)*aux.adj.f

for (plot.iter in 1:1)
{
  plot.iter <- 1
  plot(0)
  pdf(bquote(.(Plot.names[plot.iter])),height=ht.fs6,width=wd.fs6, pointsize = ps.fs6)
  par(mar=c(5,8.25,3,1),mgp=c(3,1,0), xaxs="i", yaxs="i", lwd=lwd.fs6)
  plot(NA, xlim=aux.xlim, ylim=aux.ylim, xaxt="n", yaxt="n",
       ylab="",xlab=bquote(bold(paste("PM"["2.5"],"-attributable deaths in 2017 [thousands]"))),
       main="", cex.lab=1.2*aux.cex.adj.f)
  title(main=bquote(bold(paste("PM"["2.5"],"-attributable deaths in 2017 from on-road transportation in the U.S. under different CRFs"))), font=2, line=1,cex.main=1.2*aux.cex.adj.f)
  aux.yoffset <- 1
  aux.ybottom1 <- c(4:1,(aux.yoffset+(8:5)),(2*aux.yoffset+(12:9))) - 0.05
  aux.ytop1 <- c(4:1,(aux.yoffset+(8:5)),(2*aux.yoffset+(12:9))) + 0.35
  aux.ybottom2 <- c(4:1,(aux.yoffset+(8:5)),(2*aux.yoffset+(12:9))) - 0.35
  aux.ytop2 <- c(4:1,(aux.yoffset+(8:5)),(2*aux.yoffset+(12:9))) - 0.1
  
  
  for(j in 2:dim(aux.FigS6.Vehicle)[2])
  {
    rect(xleft=rowSums(as.matrix(aux.FigS6.Vehicle[,(1:j-1)])),
         xright=rowSums(as.matrix(aux.FigS6.Vehicle[,(1:j)])),
         ytop=(aux.ytop1),
         ybottom=(aux.ybottom1),
         col=col.vclass[j-1],
         border="black",
         density=rec.dens[j-1], angle=rec.angle[j-1])
  }
  aux.col.pols <- c(rep(col.pol.cars[1:5],5))
  for(j in 2:dim(aux.FigS6.Pollutant)[2])
  {
    rect(xleft=rowSums(as.matrix(aux.FigS6.Pollutant[,(1:j-1)])),
         xright=rowSums(as.matrix(aux.FigS6.Pollutant[,(1:j)])),
         ytop=(aux.ytop2),
         ybottom=(aux.ybottom2),
         col=aux.col.pols[j-1],
         border=NA,
         density=rec.dens[1], angle=rec.angle[1])
  }
  text("(c) Using the Krewski et al. CRF [12]", x=0, y=4.75,font=2, cex=1*aux.cex.adj.f, pos=4)
  text("(b) Using the Vodonos et al. CRF [10]", x=0, y=8.75+aux.yoffset,font=2, cex=1*aux.cex.adj.f, pos=4)
  text("(a) Using the GEMM CRF [9]", x=0, y=12.75+2*aux.yoffset,font=2, cex=1*aux.cex.adj.f, pos=4)
  abline(h=c(4.25+aux.yoffset,8.25+2*aux.yoffset),lwd=1.5*lwd.fs6,lty=1)
  
  aux.text <- 100*round(rowSums(aux.FigS6.Vehicle)/100,0)
  aux.text.xloc <- rowSums(aux.FigS6.Vehicle) + 400
  text(aux.text,y=0.5*(aux.ytop1+aux.ybottom1),x=aux.text.xloc,adj=c(0,0.5),font=2,cex=1*aux.cex.adj.f)
  
  aux.legend.text <- c(expression(bold(paste("Vehicle Classes",sep=""))),
                       expression(paste("Light-Duty Vehicles",sep="")),
                       expression(paste("Heavy-Duty Trucks",sep="")),
                       expression(paste("Motorcycles",sep="")),
                       expression(paste("Buses",sep="")),
                       expression(paste("Refueling",sep="")))
  
  legend(x=65000,y=10.25+2.5, xjust=0, yjust=0.5,
         legend=aux.legend.text,
         pch=c(NA,rep(22,5)), col="black", 
         pt.bg=c(NA,col.vclass), pt.cex=c(NA,rep(1.2,5))*aux.cex.adj.f, 
         cex=1*aux.cex.adj.f, box.lwd=NA, bg="white", y.intersp = 0.85)
  
  legend(x=65000,y=(0.25+2.5), xjust=0, yjust=0.5,
         c(expression(bold(paste("Pollutants"))),
           expression(paste("Primary PM"[2.5],"",sep="")),
           expression(paste("SO"[2],"",sep="")),
           expression(paste("NO"["x"],"",sep="")),
           expression(paste("NH"[3],"",sep="")),
           expression(paste("VOC", "",sep=""))),
         pch=c(NA,rep(15,5)), col=c(NA,col.pol.cars), 
         pt.bg=NA, pt.cex=c(NA,rep(1,5))*aux.cex.adj.f, 
         cex=1*aux.cex.adj.f, box.lwd=NA, bg="white", y.intersp = 0.85)
  
  mtext("Emissions Scenario",side=2,line=6.5,font=2,srt=90,at=(aux.ylim[1]+aux.ylim[2])/2,cex=1.5*aux.cex.adj.f)
  
  box(which="plot",lty="solid")
  
  par(mgp=c(2,1,0))
  aux.ylab <- c("2008 EFs","2011 EFs","2014 EFs","2017 EFs")
  axis(side=2, at=aux.ybottom1+0.2, labels=rep(aux.ylab,3),las=2, cex.axis=1*aux.cex.adj.f, font=2, lwd=lwd.fs6)
  par(mgp=c(1,1,0))
  
  aux.axis1.at <- c(5000*0:18)
  aux.axis1.lab <- c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90)
  axis(side=1, at=aux.axis1.at,labels=aux.axis1.lab, cex.axis=1*aux.cex.adj.f, font=2, lwd=lwd.fs6)
  
  abline(v=0,lty=1,lwd=1*lwd.fs6,col=c(gray(0),gray(0.5)))
  
  dev.off()
}


######    6.5. Plotting Figure S3 ######
#Creating auxiliary dataset
aux.Fig.S5 <- matrix(rep(0,4*11),ncol=11)
aux.Fig.S5[1,-1] <- as.vector(colSums(PLDV.CrossMSA))
aux.Fig.S5[2,-1] <- as.vector(colSums(OnRoad.CrossMSA))
aux.Fig.S5[3,-1] <- as.vector(colSums(PLDV.CrossState))
aux.Fig.S5[4,-1] <- as.vector(colSums(OnRoad.CrossState))
aux.Fig.S5


rec.dens <- c(rep(NA,5),rep(50,5))
rec.angle <- c(rep(NA,5),rep(45,5))

aux.ylim <- c(0.5,5.5)
aux.xlim <- c(0,22000)

rec.height <- 0.7

col.pol.cars <- c("aquamarine","navy","deepskyblue","chartreuse","darkgreen","darksalmon")
aux.col.pol.cars <- c(col.pol.cars[-6],col.pol.cars[-6])

aux.adj.f <- 1
aux.cex.adj.f <- 1.2

wd.fs3 <- 18/2.54
ht.fs3 <- wd.fs3*(7/12)
lwd.fs3 <- (wd.fs3/12)*aux.adj.f
ps.fs3 <- (wd.fs3/12)*aux.adj.f

Plot.names <- c(paste0(Figure.path,"Figure_S3",".pdf"))
plot(0)
pdf(Plot.names, height=ht.fs3, width=wd.fs3, pointsize = ps.fs3)
par(mar=c(5,8.5,3,2),mgp=c(3,1,0), xaxs="i", yaxs="i", lwd=lwd.fs3)
plot(NA, xlim=aux.xlim, ylim=aux.ylim, xaxt="n", yaxt="n",
     ylab="",xlab=bquote(bold(paste("PM"["2.5"],"-attributable deaths in 2017 [thousands]",sep=""))),
     main="",cex.lab=1.5)
title(main="Air pollution across state and metropolitan area lines", font=2, line=1.5,cex.main=1.8)
aux.ybottom <- c(1,2,3.5,4.5)-rec.height/2
aux.ytop <- c(1,2,3.5,4.5)+rec.height/2

text("(a) Air pollution impacts across state lines", x=0, y=5.25,font=2, cex=1.5, pos=4)
text("(b) Air pollution impacts across metropolian area borders", x=0, y=2.75,font=2, cex=1.5, pos=4)
abline(h=3,lty=1,lwd=1*lwd.fs3,col=c(gray(0)))
for (j in 2:11)
{
  rect(xleft=(rowSums(as.matrix(aux.Fig.S5[,(1:j-1)]))),
       xright=(rowSums(as.matrix(aux.Fig.S5[,(1:j)]))),
       ytop=aux.ytop,
       ybottom=aux.ybottom,
       col=aux.col.pol.cars[j-1],
       density=rec.dens[j-1],
       angle=rec.angle[j-1],
       border="black")
}
mtext("Vehicle",side=2,line=4.5,font=2,srt=90, at=(2+3.5)/2,cex=1.5)
mtext("Classes",side=2,line=3.2,font=2,srt=90, at=(2+3.5)/2,cex=1.5)
legend("bottomright",
       c(expression(paste("Primary PM"[2.5],"",sep="")),
         expression(paste("SO"[2],"",sep="")),
         expression(paste("NO"["x"],"",sep="")),
         expression(paste("NH"[3],"",sep="")),
         expression(paste("VOC", "",sep="")),
         expression("solid: within borders"),
         expression("hashed: across borders")),
       pch=c(rep(22,5),NA,NA), col=c("black",NA,NA), 
       pt.bg=c(aux.col.pol.cars,NA,NA), pt.cex=c(rep(1.5,5)*1.2*aux.cex.adj.f,NA,NA), 
       cex=1.5, box.lwd=NA, bg="white", y.intersp = 0.9)

box(which="plot",lty="solid")
axis(side=2, at=c(1,2,3.5,4.5), labels=rep(c("Passenger LDV","ALL"),2),las=2, cex.axis=1*aux.cex.adj.f, font=2, lwd=lwd.fs3)
axis(side=1, at=2000*0:11,labels=2*0:11, cex.axis=1.2*aux.cex.adj.f, font=2, lwd=lwd.fs3)
abline(v=0,lty=1,lwd=1*lwd.fs3,col=c(gray(0)))

dev.off()

######    6.6. Plotting Figure S2 ######

#Creating auxiliary dataset for plotting:
PLDV.Cents.Mi <- as.data.frame(100*(GEMM.PLDV/rowSums(NEI2017.VTYPE.EF[,3,PLDV.Slices])))
PLDV.Cents.Mi$ZERO <- 0 #Just to simplify plotting
PLDV.Cents.Mi <- cbind(County.List$STCOU,PLDV.Cents.Mi[,c(6,1:5)])
names(PLDV.Cents.Mi)[1] <- "STCOU"
PLDV.Cents.Mi$SUM <- rowSums(PLDV.Cents.Mi[,-1])
PLDV.Cents.Mi$VMT <- rowSums(NEI2017.VTYPE.EF[,3,PLDV.Slices])/1e12 #Divided by 1e12 so that the value is in trillion miles
PLDV.Cents.Mi <- PLDV.Cents.Mi[order(PLDV.Cents.Mi$SUM, decreasing=TRUE),]
PLDV.Cents.Mi$VMT.Left <- 0
PLDV.Cents.Mi$VMT.Right <- 0
PLDV.Cents.Mi$VMT.Right[1] <- PLDV.Cents.Mi$VMT[1]
for (i in 2:length(PLDV.Cents.Mi$VMT))
{
  PLDV.Cents.Mi$VMT.Left[i] <- sum(PLDV.Cents.Mi$VMT[1:(i-1)])
  PLDV.Cents.Mi$VMT.Right[i] <- sum(PLDV.Cents.Mi$VMT[1:(i)])
}
PLDV.Cents.Mi$Total.Impacts.by.County <- (PLDV.Cents.Mi$SUM * PLDV.Cents.Mi$VMT) * 1e12 * 1e-2 * 1e-9 
#Now in total dollars per county
#1e12 to convert back to miles; 1e-2 to convert back to dollars; 1e-9 to make it billions of dollars
PLDV.Cents.Mi$Cumulative.Total.Impacts.Left <- 0
PLDV.Cents.Mi$Cumulative.Total.Impacts.Right <- 0
PLDV.Cents.Mi$Cumulative.Total.Impacts.Right[1] <- PLDV.Cents.Mi$Total.Impacts.by.County[1]
for (i in 2:length(PLDV.Cents.Mi$VMT))
{
  PLDV.Cents.Mi$Cumulative.Total.Impacts.Left[i] <- sum(PLDV.Cents.Mi$Total.Impacts.by.County[1:(i-1)])
  PLDV.Cents.Mi$Cumulative.Total.Impacts.Right[i] <- sum(PLDV.Cents.Mi$Total.Impacts.by.County[1:i])
}


aux.pol.order <- c(3,2,5,4,1)
PLDV.Cents.Mi <- PLDV.Cents.Mi[,c(1:2,(aux.pol.order+2),8:(dim(PLDV.Cents.Mi)[2]))]

col.pol.cars <- c("aquamarine","navy","deepskyblue","chartreuse","darkgreen","darksalmon")
aux.col.pol <- col.pol.cars[aux.pol.order]
aux.col.pol[which(aux.pol.order==1)] <- "cyan"
aux.col.cum <- "magenta"
aux.right.axis.scaling <- 5/25
aux.rec.border <- c(rep(NA,5),"black")

plot(0)
pdf(paste0(Figure.path,"Figure_S2",".pdf"),height=7,width=7)
par(cex.lab=1,cex.main=1.25,cex.axis=0.8, mar=c(5,3.5,3,3.5), mgp=c(2,0.5,0))
plot(NA,xlim=c(0,2.75),ylim=c(0,35),
     xaxt="n",yaxt="n",
     xlab=expression(bold("Cumulative VMT [trillion miles]")),
     ylab=expression(bold("Impact [cents per mile]")),
     main=expression(bold("Passenger LDV 2017 impacts by county [cents per mile]")))


for(j in 3:7)
{
  rect(xleft=PLDV.Cents.Mi$VMT.Left,
       xright=PLDV.Cents.Mi$VMT.Right,
       ybottom=rowSums(as.matrix(PLDV.Cents.Mi[,2:(j-1)])),
       ytop=rowSums(as.matrix(PLDV.Cents.Mi[,2:(j)])),
       border=aux.rec.border[j-2], lwd=0.001, col=aux.col.pol[j-2])
}

segments(x0=0,
         x1=PLDV.Cents.Mi$VMT.Right[3108],
         y0=0,
         y1=aux.right.axis.scaling*PLDV.Cents.Mi$Cumulative.Total.Impacts.Right[3108],
         col=aux.col.cum, lwd=1, lty=2)

segments(x0=PLDV.Cents.Mi$VMT.Left,
         x1=PLDV.Cents.Mi$VMT.Right,
         y0=aux.right.axis.scaling*PLDV.Cents.Mi$Cumulative.Total.Impacts.Left,
         y1=aux.right.axis.scaling*PLDV.Cents.Mi$Cumulative.Total.Impacts.Right,
         col=aux.col.cum, lwd=2, lty=1)

aux.US.Mean <- sum(PLDV.Cents.Mi$SUM*PLDV.Cents.Mi$VMT)/sum(PLDV.Cents.Mi$VMT)
abline(h=aux.US.Mean,col="darkorange",lty=1,lwd=2)
par(mgp=c(2,0.5,0))
axis(side=2,at=c(2.5*0:14),
     labels=c("0","","5","","10","","15","","20","","25","","30","","35"),
     font=2,font.axis=2)


par(mgp=c(2,0.5,0))
axis(side=1,at=c(0.25*0:10,sum(PLDV.Cents.Mi$VMT)),labels=c(0.25*0:10,2.7),font=2,font.axis=2)

aux.legend.text <- c(expression(paste("Primary PM"[2.5],"",sep="")),
                     expression(paste("SO"[2],"",sep="")),
                     expression(paste("NO"["x"],"",sep="")),
                     expression(paste("NH"[3],"",sep="")),
                     expression(paste("VOC", "",sep="")),
                     expression(paste("U.S. VMT-weighted mean (4.0 cents/mile)")),
                     expression(paste("Cumulative impact")))
legend("topright",
       legend=aux.legend.text[c(aux.pol.order,6,7)],
       pch=c(rep(22,5),NA<NA),pt.cex=c(rep(1.2,5),NA,NA),col=c(rep(NA,5),"darkorange",aux.col.cum),
       pt.bg=c(aux.col.pol,NA,NA),lty=c(rep(NA,5),1,1),lwd=c(rep(NA,5),2,2),
       box.lwd=NA, bg="white", cex=1)
box(which="plot",lty="solid")

par(mgp=c(2,0.5,0))
axis(side=4,at=c(2.5*0:10),
     labels=c("0","","25","","50","","75","","100","","125"),
     col.ticks = aux.col.cum, col.axis=aux.col.cum, col=aux.col.cum,font=2,font.axis=2)
mtext("Cumulative Impact in 2017 [USD, billions]",
      side=4,line=2,at=(125/2)*aux.right.axis.scaling, col=aux.col.cum, font=2)

dev.off()

######    6.7. Plotting Figures S7-S10 ######
#Plot options
col.pol.cars <- c("aquamarine","navy","deepskyblue","chartreuse","darkgreen","darksalmon")
aux.xaxis <- list(c(1e3,3e7),c(3e2,1e6),c(3e2,3e5),c(1e3,1e7),c(1e2,3e5))
Plot.titles <- paste0("CDFs of marginal damages for each species -- ",
                      c("GEMM CRF [9]","Not Used","Vodonos et al. CRF [10]","Krewski et al. CRF [12]"))
Plot.titles2 <- c("[weighted by total 2017 U.S. On-Road emissions of each species]")

Plot.titles3 <- paste0("Distribution of marginal damages for each species by county -- ",
                       c("GEMM CRF [9]","Not Used","Vodonos et al. CRF [10]","Krewski et al. CRF [12]"))
Plot.titles4 <- c("[unweighted, for the 3,108 counties in the contiguous U.S.]")

Plot.names <- paste0(Figure.path,c("Figure_S10","Figure_S7",
                                   "Not_Used","Not_Used",
                                   "Not_Used","Figure_S8",
                                   "Not_Used","Figure_S9"),".pdf")

col.crf <- c("green","black","blue","red")

#Figures S7-S9
for (crf in c(1,3,4))
{
  aux.PM.Yr <- 2017
  aux.Mortality.Yr <- 2017
  aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",Model.CRF[crf],"_BasePM_",aux.PM.Yr,"_Mortality_",aux.Mortality.Yr)))
  
  aux.main.text <- c(expression(bold(paste("Primary PM"["2.5"],"",sep=""))),
                     expression(bold(paste("SO"["2"],"",sep=""))),
                     expression(bold(paste("NO"["x"],"",sep=""))),
                     expression(bold(paste("NH"["3"],"",sep=""))),
                     expression(bold(paste("VOC", "",sep=""))))
  
  pdf(Plot.names[(2*crf)],height=10,width=6.5)
  par(mfcol=c(3,2),mgp=c(2,0.5,0),mar=c(3,3,2,0.5),
      cex.axis=0.8,cex.lab=0.8,oma=c(0.1,0.1,3.5,0.1),
      xaxs="i", yaxs="i")
  
  for (aux.pol in c(1,3,2,4,5))
  {
    aux.plot <- as.data.frame(matrix(rep(0,3108*2),ncol=2))
    names(aux.plot) <- c("MD","EMIS")
    aux.plot$MD <- log10(aux.marg.crf[,aux.pol])
    
    hist(aux.plot$MD,breaks=c(0.25*4:29),
         xlim=log10(c(1e2,3e7)),ylim=c(0,1600),
         xaxt="n",yaxt="n",
         xlab=expression(bold("Marginal Damage [$/tonne], log scale")),
         ylab=expression(bold("Number of Counties")),
         main=aux.main.text[aux.pol],col=col.pol.cars[aux.pol],border="black")
    mtext(side=3, outer=TRUE, Plot.titles3[crf], line=1.9, font=2, cex=0.85)
    mtext(side=3, outer=TRUE, Plot.titles4, line=0.6, font=2,cex=0.75)
    
    axis(side=1, at=c(2:7),labels=c("0.1k","1k","10k","100k","1M","10M"),font=2)
    aux.ylab <- 200*0:8
    axis(side=2, at=aux.ylab,
         labels=aux.ylab,
         las=2,font=2)
    
    aux.ylab <- c(0.025,0.1,0.25,0.5,0.75,0.9,0.975)
    aux.x1 <- 10^(c(min(aux.plot$MD),quantile(aux.plot$MD, probs=aux.ylab),max(aux.plot$MD)))
    
    aux.MD.legend <- rep("a",length(aux.x1))
    for (aux.leg in 1:length(aux.x1))
    {
      if(aux.x1[aux.leg]<995000)
      {
        aux.MD.legend[aux.leg] <- paste0(as.character(signif(aux.x1[aux.leg]/1e3,digits=2)),"k")
      } else {
        aux.MD.legend[aux.leg] <- paste0(as.character(signif(aux.x1[aux.leg]/1e6,digits=2)),"M")
      }
    }
    
    aux.MD.legend2 <- c(eval(bquote(expression(bold(paste("Percentiles:  ",sep=""))))),
                        eval(bquote(expression(bold(paste("Min.:  ",.(aux.MD.legend[1]),sep=""))))),
                        eval(bquote(expression(bold(paste("2.5"^"th",":  ",.(aux.MD.legend[2]),sep=""))))),
                        eval(bquote(expression(bold(paste("10"^"th",":  ",.(aux.MD.legend[3]),sep=""))))),
                        eval(bquote(expression(bold(paste("25"^"th",":  ",.(aux.MD.legend[4]),sep=""))))),
                        eval(bquote(expression(bold(paste("50"^"th",":  ",.(aux.MD.legend[5]),sep=""))))),
                        eval(bquote(expression(bold(paste("75"^"th",":  ",.(aux.MD.legend[6]),sep=""))))),
                        eval(bquote(expression(bold(paste("90"^"th",":  ",.(aux.MD.legend[7]),sep=""))))),
                        eval(bquote(expression(bold(paste("97.5"^"th",":  ",.(aux.MD.legend[8]),sep=""))))),
                        eval(bquote(expression(bold(paste("Max.:  ",.(aux.MD.legend[9]),sep=""))))))
    
    legend("topright",
           legend=aux.MD.legend2, bty="n", cex=0.7, 
           pch=NA,col=NA,text.font=2)
    box(which="plot",lty="solid")
  }
  dev.off()
}

#Figure S10
aux.PM.Yr <- 2017
aux.Mortality.Yr <- 2017

aux.main.text <- c(expression(bold(paste("Primary PM"["2.5"],"",sep=""))),
                   expression(bold(paste("SO"["2"],"",sep=""))),
                   expression(bold(paste("NO"["x"],"",sep=""))),
                   expression(bold(paste("NH"["3"],"",sep=""))),
                   expression(bold(paste("VOC", "",sep=""))))

plot(0)
pdf(Plot.names[1],height=10,width=6.5)
par(mfcol=c(3,2),mgp=c(2,0.5,0),mar=c(3,3,2,0.5),
    cex.axis=0.8,cex.lab=0.8,oma=c(0.1,0.1,3.5,0.1))

for (aux.pol in c(1,3,2,4,5))
{
  plot(NA,xlim=log10(c(1e2,3e7)),ylim=c(0,1),
       xaxt="n",yaxt="n",
       xlab=expression(bold("Marginal Damage [$/tonne], log scale")),
       ylab=expression(bold("CDF")),
       main=aux.main.text[aux.pol])

  mtext(side=3, outer=TRUE, "CDFs of marginal damages for each species for all three CRFs considered", line=1.9, font=2)
  mtext(side=3, outer=TRUE, Plot.titles2, line=0.6, font=2,cex=0.8)
  
  axis(side=1, at=c(2:7),labels=c("0.1k","1k","10k","100k","1M","10M"),font=2)
  aux.ylab <- c(0,0.1,0.25,0.5,0.75,0.9,1)
  axis(side=2, at=aux.ylab,
       labels=aux.ylab,
       las=2,font=2)
  
  for (crf in c(1,3,4))
  {
    aux.marg.crf <- eval(parse(text=paste0("Marg.Damages.",Model.CRF[crf],"_BasePM_",aux.PM.Yr,"_Mortality_",aux.Mortality.Yr)))
    aux.plot <- as.data.frame(matrix(rep(0,3108*2),ncol=2))
    names(aux.plot) <- c("MD","EMIS")
    aux.plot$MD <- log10(aux.marg.crf[,aux.pol])
    aux.plot$EMIS <- rowSums(NEI2017.VTYPE.EF[,Pol.Columns[aux.pol],1:13] * NEI2017.VTYPE.EF[,3,1:13]) * (1/1e6)
    aux.plot <- aux.plot[order(aux.plot$MD, decreasing=FALSE),]
    aux.plot$EMIS.Left <- 0
    aux.plot$EMIS.Right <- 0
    aux.plot$EMIS.Right[1] <- aux.plot$EMIS[1]/sum(aux.plot$EMIS)
    for (i in 2:length(aux.plot$MD))
    {
      aux.plot$EMIS.Left[i] <- sum(aux.plot$EMIS[1:(i-1)])/sum(aux.plot$EMIS)
      aux.plot$EMIS.Right[i] <- sum(aux.plot$EMIS[1:(i)])/sum(aux.plot$EMIS)
    }
    
    points(x=aux.plot$MD,y=aux.plot$EMIS.Right, pch=20, col=col.crf[crf], cex=0.3)
    segments(x0=aux.plot$MD[-length(aux.plot$MD)],
             x1=aux.plot$MD[-1],
             y0=aux.plot$EMIS.Right[-length(aux.plot$MD)],
             y1=aux.plot$EMIS.Right[-length(aux.plot$MD)],
             lwd=0.5,col=col.crf[crf])
    
    
    aux.OnRoad.Mean <- sum(10^aux.plot$MD * aux.plot$EMIS)/sum(aux.plot$EMIS)
    aux.over <- which.max(10^aux.plot$MD>aux.OnRoad.Mean)
    aux.over2 <- (10^aux.plot$MD[aux.over] - aux.OnRoad.Mean)/(10^aux.plot$MD[aux.over] - 10^aux.plot$MD[aux.over-1])
    aux.y.OnRoad.Mean <- aux.plot$EMIS.Right[aux.over-1] + ((1-aux.over2) * (aux.plot$EMIS.Right[aux.over] - aux.plot$EMIS.Right[aux.over-1]))
    points(x=log10(aux.OnRoad.Mean),y=aux.y.OnRoad.Mean,col="magenta",pch=8,cex=1)
    
  }
  
}

plot(NA,xlim=c(0,1),ylim=c(0,1),
     xaxt="n",yaxt="n",
     xlab="",
     ylab="",
     main=aux.main.text[aux.pol], bty="n")

legend(c("Krewski et al. [12]","GEMM [9]","Vodonos et al. [10]","CDF Means"),
       pch=c(15,15,15,8), col=c(col.crf[c(4,1,3)],"magenta"),
       x=0.5, y=0.5, xjust=0.5, yjust=0.5, cex=2.5, bty="n")

dev.off()



######    6.8. Plotting Figure 4 ######

#Figure 4 -- Map of PLDV impacts per mile
#This requires the spatial packages sp, raster, and rgeos
#We also use the packages viridis and gridExtra to customize the plot
library("sp")
library("raster")
library("rgeos")
library("viridis")
library("gridExtra")

#It also requires the following download: USCB shapefile for counties.
#Loading shapefile for plotting.
Counties <- shapefile("Inputs/Auxiliary/USCB_County_Shapefile_500k/cb_2019_us_county_500k.shp")
#Source: U.S. Census Bureau
#Available at: https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_county_500k.zip
#Accessed 04 May 2021
#File Last Modified according to USCB: 05/01/2020 13:27
#File Size: 11M

#Creating the auxiliary dataset
PLDV.Cents.Mi <- as.data.frame(100*(GEMM.PLDV/rowSums(NEI2017.VTYPE.EF[,3,PLDV.Slices])))
PLDV.Cents.Mi$SUM <- rowSums(PLDV.Cents.Mi)
PLDV.Cents.Mi$PM25_NH3_VOC <- PLDV.Cents.Mi$PM25 + PLDV.Cents.Mi$NH3 + PLDV.Cents.Mi$VOC

#We will plot on a log scale
PLDV.Cents.Mi$LOG_NOX <- log10(PLDV.Cents.Mi$NOX)
PLDV.Cents.Mi$LOG_PM25_NH3_VOC <- log10(PLDV.Cents.Mi$PM25_NH3_VOC)
PLDV.Cents.Mi <- cbind(County.List$STCOU,PLDV.Cents.Mi)
names(PLDV.Cents.Mi)[1] <- "STCOU"

#Editing spatial data to merge with auxiliary dataset
Counties$STCOU <- as.numeric(as.character(Counties$GEOID))

#Filtering out counties outside the contiguous U.S.
Counties <- merge(x=Counties, y=PLDV.Cents.Mi, by.x="STCOU", by.y="STCOU", all.x=FALSE, all.y=TRUE)

### Creating Color scale
aux.scale.break <- 0.5 
#This will be the threshold for plotting. 
#We will not differentiate any value below 0.5 cents/mi
#That is, any value below 0.5 will be shown as <=0.5
Counties$LOG_NOX[Counties$LOG_NOX <= log10(aux.scale.break)] <- log10(aux.scale.break)
Counties$LOG_PM25_NH3_VOC[Counties$LOG_PM25_NH3_VOC <= log10(aux.scale.break)] <- log10(aux.scale.break)

#Plot options -- color breaks, color scales, and legend
aux.breaks <- c(log10(0.4999),-0.28+0.02*(0:85),log10(1.0001*max(Counties$PM25_NH3_VOC)))
col.scale <- viridis(n=(length(aux.breaks)+1),alpha=1,begin=0,end=1,direction=1)
aux.leg.at <- log10(c(0.5,1,2,3,5,10,20,max(Counties$PM25_NH3_VOC)))
aux.leg.labels <- c("<=0.5","1","2","3","5","10","20","27")

aux.title1 <- list("sp.text",as.vector(c(rowMeans(Counties@bbox)[1],(Counties@bbox[2,2]+1.5))),
                   expression(bold(paste("(a) Primary PM"[2.5],", NH"[3],", and VOCs combined",sep=""))), which=2, cex=1.25)

aux.title2 <- list("sp.text",as.vector(c(rowMeans(Counties@bbox)[1],(Counties@bbox[2,2]+1.5))),
                   expression(bold(paste("(b) NO"["x"],"",sep=""))), which=1, cex=1.25)

wd.f4 <- 18/2.54
ht.f4 <- 18/2.54

pdf(file=paste0(Figure.path,"Figure_4",".pdf"), height=ht.f4, width=wd.f4)
spplot(Counties, c("LOG_NOX","LOG_PM25_NH3_VOC"), lwd=NA,
       col.regions = col.scale, at=aux.breaks,

       ylim=c(Counties@bbox[2,1],(Counties@bbox[2,2]+3)),
       xlim=Counties@bbox[1,],
       strip=FALSE,
       sp.layout=list(aux.title1, aux.title2),

       main=list(label=expression(bold(paste("Passenger LDV 2017 impacts by county for each pollutant [cents per mile]"))), cex=1.2),
       xlab=list(label=expression(bold("cents per mile (colors on a log scale)", cex=0.875))),
       colorkey=list(height=1, width=3, labels=list(labels=aux.leg.labels,at=aux.leg.at,cex=1,font.axis=2)))
dev.off()

###### END CODE ######