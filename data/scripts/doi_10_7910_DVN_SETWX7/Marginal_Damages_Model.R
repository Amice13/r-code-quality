#Code to estimate marginal damages of emissions.
#Author: Ernani F. Choma 

options(warn = 1)

#Please change the following file paths accordingly before running:
#Lines 21-22 -- ncdf4 and mgcv package locations
#Line 27 -- Working directory
#Inputs:
#Lines 31-32 -- Data Inputs
#Line 61 -- ISRM file path (needs to be downloaded)
#Line 70 -- Vodonos file path (needs to be downloaded)
#Outputs:
#Line 466 -- Mortality output files in .csv format
#Lines 496,502 -- Mortality output files in .RData format
#Line 526 -- Asthma output files in .csv format
#Lines 558,564 -- Asthma output files in .RData format

###### 1. Loading Required R packages ######
#Please change following line to correct path to ndcf4 package location
library("ncdf4")
library("mgcv")
#requires nlme!

###### 2. Loading Inputs ######
#Please change path to work directory
setwd("~/Ernani_spengler_lab/School_Bus/PNAS_MDM/Marginal_Damages_Model/")


#2.1) Loading Inputs provided with the model:
load("Inputs/Model_Inputs.RData")
load("Inputs/Model_Inputs_Asthma.RData")



#a) Ambient Baseline PM2.5 Data
#     Variable name: Ambient.PM25

#b) Baseline Mortality Data. 
#     Four datasets with variable names:
#     Deaths.AllCause.2008
#     Deaths.AllCause.2017
#     Deaths.GEMM.2008
#     Deaths.GEMM.2017

#c) GEMM Coefficients
#     Variable name: GEMM.Coefficients

#d) InMAP to county population mapping
#     Variable name: InMAP.Cells.by.County


#2.2 Inputs available online that need to be downloaded:
#a) ISRM
#Loading InMAP data. This dataset are available at:
#https://doi.org/10.5281/zenodo.2589760 and were accessed on March 03, 2021
#The "ISRM" variable is the isrm_v1.2.1.zip file after unzipping

# Please change directory and run following line to open the ISRM.
# ISRM <- nc_open ("/n/holyscratch01/spengler_lab/ISRM_071223/isrm_v1.2.1.ncf", write=FALSE)
ISRM <- nc_open ("~/Ernani_spengler_lab/ISRMUnzip/isrm_v1.2.1.ncf", write=FALSE)
# print(ISRM)

#b) Vodonos Coefficients
#This dataset can be downloaded from 
#https://github.com/AlinaVod/meta_regression_pm2.5 
#file: "metaregression.RData.Rdata", acessed March 09, 2021)

# Please change directory and run following line to load the Vodonos et al. data
load("Inputs/Vodonos_et_al/metaregression.RData.Rdata")


###### 3. Creating functions for the risk ratios ######
#These return the percentage increase in mortality per 1 ug/m3 
#in the case of GEMM, NCD+LRI mortality; in the case of Vodonos et al. and Krewski et al., all-cause mortality
#The input is a vector of baseline ambient PM2.5 levels with N rows

#The outputs are:
#For GEMM
#a matrix of N rows x 13 columns, where each column is an age group and each row is one ambient baseline PM2.5 level
#age groups are in order from youngest (25-29), in column 1, to oldest (85+), in column 12
#column 13 is for all adults which we do not use here.

#For Vodonos
#A vector of N rows, representing the N ambient levels

#Krewski's slope is constant so there is no need to create a function.

GEMM.NCDLRI <- function(PM)
{
  auxGEMM <- array(rep(-1,length(PM)*length(GEMM.Coefficients$age)*3),dim=c(length(PM),length(GEMM.Coefficients$age),3))
  auxGEMM[,,1] <- t(exp((GEMM.Coefficients$th_w*log(t((PM-2.4) %*% t((1/GEMM.Coefficients$al_w)))+1))/(1+exp(-(t((PM-2.4) %*% t(rep(1,length(GEMM.Coefficients$age))))-GEMM.Coefficients$mu_w)/GEMM.Coefficients$v_w))))
  auxGEMM[,,2] <- t(exp((GEMM.Coefficients$th_w*log(t(((PM+0.00001)-2.4) %*% t((1/GEMM.Coefficients$al_w)))+1))/(1+exp(-(t(((PM+0.00001)-2.4) %*% t(rep(1,length(GEMM.Coefficients$age))))-GEMM.Coefficients$mu_w)/GEMM.Coefficients$v_w))))
  auxGEMM[,,3] <- ((auxGEMM[,,2] - auxGEMM[,,1])/auxGEMM[,,1])/0.00001 
  returnarray <- as.matrix(auxGEMM[,,3])
  return(returnarray)
}


Vodonos <- function(PM)
{
  auxVodonos <- matrix(rep(-1,length(PM)*2),ncol=2)
  VB0 <- 0.0059
  VB1 <- 0.0705
  auxVodonos[,1] <- VB0 + VB1*(1/PM)
  aux.spline <- as.data.frame(PM)
  names(aux.spline) <- "pm25"
  aux.spline$Outcome <- 1
  aux.spline$elderly <- 0
  aux.spline$female <- 0
  pred.aux.spline <- predict(mod1$gam, type = "response", aux.spline, se=T)
  auxVodonos[,2] <- as.vector(pred.aux.spline$fit)
  return(auxVodonos)
}

###### 4. VSL & Cessation lag multiplier ######
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
Present.Value <- (1/Discount.Rate)*(exp(-Discount.Rate*(Year-1))-exp(-Discount.Rate*(Year)))
Sum.Present.Value <- t(Cessation.Lag) %*% Present.Value
Present.Value.Multiplier <- as.numeric(Sum.Present.Value)

Present.Value.Multiplier #Our marginal damages are multiplied by ~0.89.
#Undiscounted values can be obtained by removing this step, i.e. setting the Present.Value.Multiplier to 1
#Present.Value.Multiplier <- 1 #For undiscounted damages

k.VSL <- VSL * Present.Value.Multiplier
k.VSC.Asthma <- 610000 #Appere et al. (2023)

###### 5. Creating dataset names ######
#Model CRF Names
Model.CRF <- c("GEMM","Vodonos.Parametric","Vodonos.Spline","Krewski")
Model.CRF.csv <- c("GEMM","Vodonos_Parametric","Vodonos_Spline","Krewski")

#Model CRF names of mortality datasets
#GEMM's risk ratios apply to age & cause-specific mortality
#The other CRFs apply to all-cause mortality
Model.CRF.Mortality <- c("GEMM","AllCause","AllCause","AllCause")

#ISRM pollutant variable names
Pollutant.Variables <- c("PrimaryPM25","pSO4","pNO3","pNH4","SOA")
Pollutant.Names <- c("Primary PM25","SO2","NOx","NH3","VOC")
Pollutant.Names.csv <- c("PrimaryPM25","SO2","NOx","NH3","VOC")

#Variable names (used when saving files)
Model.CRF.Names.SRM <- paste0("SR.Damages.",Model.CRF)
Model.CRF.Names.MV <- paste0("Marg.Damages.",Model.CRF)
# Model.CRF.Names.SRM <- paste0("SR.Damages.",Model.CRF)
# Model.CRF.Names.MV <- paste0("Marg.Damages.",Model.CRF)


#Base year of demographics (Mortality) and baseline ambient PM 2.5 data
PM.Year <- c(2008,2017)
Mortality.Year <- c(2008,2017) 
Asthma.Age <- c("04","511","1217","")

#Creating datasets to store results
aux.grid <- expand.grid(Model.CRF, paste0("M",Mortality.Year), paste0("PM",PM.Year), Pollutant.Variables)
aux.grid <- aux.grid[,4:1]
Slice.Names <- apply(aux.grid, MARGIN=1, FUN=paste, collapse=".")
Slice.Names <- c(Slice.Names,rep("a",20))

for(aux.asthma.age in 1:4)
{
  for(p in 1:5)
  {
    Slice.Names[80+((aux.asthma.age-1)*5)+p] <- paste(Pollutant.Variables[p],
                                                      paste0("PM",PM.Year[2]),
                                                      paste0("Age",Asthma.Age[aux.asthma.age]),
                                                      sep=".")
  }
}


#For source-receptor matrix
aux.Store.SRM <- array(rep(0,3108*3108*100),dim=c(3108,3108,100),
                       dimnames = list(c(paste0("Source STCOU: ",Ambient.PM25$STCOU)),
                                       c(paste0("Receptor STCOU: ",Ambient.PM25$STCOU)),
                                       c(Slice.Names)))
#For marginal values
aux.Store.MV <- matrix(rep(0,3108*100),ncol=100)
rownames(aux.Store.SRM) <- c(paste0("Source STCOU: ",Ambient.PM25$STCOU))
colnames(aux.Store.MV) <- Slice.Names



#Creating matrix to store ISRM variable
ISRM.Store.Var <- matrix(rep(-1,52411*52411),ncol=52411)

#Creating constant to convert changes in concentrations given by ISRM from 1 ug/s to 1 tonne/yr
k.tonne.yr <- 1e12/(24*3600*365) #1 tonne/yr = (1e12/(24*3600*365))*1ug/s


###### 6. Model Calculation ######
#This step calculates:
#(i) the marginal values (MV); and (ii) the source receptor matrix (SRM).
#both for 1 tonne of emissions of each species in each source county
#(i) The marginal values are vectors of length 3,108 containing marginal values (in 2017 USD for Mortality, 2022 for childhood asthma) per tonne of emissions of each species occurring in each county

#(ii) The source-receptor matrix (SRM) are 3,108 x 3,108 matrices that further identify where these marginal damages are occurring,
#i.e. damages occurring in each of 3,108 counties [receptors] as a function of 1 tonne of emissions occurring in each of the 3,108 counties [sources]
#Sources are the lines and receptor the columns, so the row sums of the SRM are identical to the marginal values
#All values are in 2017 USD (mortality) or 2022 USD (childhood asthma)

#There are 100 sets of MV and SRM for:
#For Mortality
#Each of 5 species x each of 4 CRFs x each of 2 baseline mortality years x each of 2 baseline ambient PM 2.5 concentration years
#The names of the datasets are provided in the Slice.Names variable, following the format:
#[Species].[PMYear].[MortalityYear].[CRF] (without the square brackets)

#For Childhood Asthma
#Each of 5 species x 4 Age groups (aged 0-4 years; 5-11 years; 12-17 years; and the sum of all ages between 0 and 17 years, represented simply by "Age" with no numbers)
#The names of the datasets are provided in the Slice.Names variable, following the format:
#[Species].[PMYear].[AsthmaAge] (without the square brackets)

# p<-1
# aux.pm.year<-2


for (p in 1:5) #For 5 pollutant/species
{
  for (i in 1:52411) #ISRM is 52411 x 52411
  {
    #Loading the ISRM variable for the respective pollutant/species (ground-level)
    aux.ISRM.Line <- ncvar_get(ISRM, eval(Pollutant.Variables[p]), start=c(1,i,1), count=c(52411,1,1))
    ISRM.Store.Var[i,] <- aux.ISRM.Line
    if (i %% 2000 == 0)
    {
      print(paste0("Loading ISRM: Ground-level ", Pollutant.Names[p], " -- Line ",i," of 52411"))
    }
  }
  ISRM.Store.Var <- k.tonne.yr * ISRM.Store.Var #scaling ISRM
  
  for (aux.pm.year in c(1,2)) #For the two baseline ambient PM2.5 levels, ie. 2008 or 2017 levels 
  {
    #Loading PM data
    aux.BaselinePM <- Ambient.PM25[,which(names(Ambient.PM25)==paste0("Ambient.PM25.",PM.Year[aux.pm.year]))]
    
    #Determine counties with baseline levels below the counterfactual of 2.4
    Which.Below.Counterfactual <- which(aux.BaselinePM<2.4)
    #These counties will receive a value of 0
    
    #Calculating Slopes
    Slope.GEMM <- GEMM.NCDLRI(aux.BaselinePM)
    Slope.GEMM <- as.matrix(Slope.GEMM[,-13]) #The 13th column is the risk for all adults combined (not used as we are using the age-group-specific risks)
    Slope.GEMM[Which.Below.Counterfactual,] <- 0 #Risks set to 0 if baseline ambient level below counterfactual
    
    Slope.Vodonos <- Vodonos(aux.BaselinePM)
    Slope.Vodonos[Which.Below.Counterfactual,] <- 0
    Slope.Vodonos.Parametric <- Slope.Vodonos[,1]
    Slope.Vodonos.Spline <- Slope.Vodonos[,2]
    
    Slope.Krewski <- log(1.06)/10
    Slope.Krewski <- rep(Slope.Krewski,length(aux.BaselinePM))
    Slope.Krewski[Which.Below.Counterfactual] <- 0
    
    Slope.Asthma.aux <- log(1.33)/6.53
    Slope.Asthma <- rep(Slope.Asthma.aux,length(aux.BaselinePM))
    
    Slope.Asthma.All <- matrix(rep(Slope.Asthma.aux,length(aux.BaselinePM)*3),ncol=3)
    
    for (aux.m.year in c(1,2)) #For the two sets of baseline mortality data, ie. 2008 or 2017 levels
    {
      
      #Loading mortality data
      aux.Deaths.GEMM <- as.matrix(eval(parse(text=paste0("Deaths.GEMM.",Mortality.Year[aux.m.year])))[,-1])
      aux.Deaths.AllCause <- as.matrix(eval(parse(text=paste0("Deaths.AllCause.",Mortality.Year[aux.m.year])))[,-1])
      #Note: the first column was removed because it is the FIPS State + County code
      

      for (aux.crf in 1:4) #for each CRF
      {
        
        aux.Slice.Name <- paste(Pollutant.Variables[p],
                                paste0("PM",PM.Year[aux.pm.year]),
                                paste0("M",Mortality.Year[aux.m.year]),
                                Model.CRF[aux.crf],
                                sep=".")
        aux.slice <- which(Slice.Names==aux.Slice.Name)
        
        print(paste0("Starting Slice No. ",aux.slice," of 100 -- ",aux.Slice.Name))
        
        aux.Deaths <- eval(parse(text=paste0("aux.Deaths.",Model.CRF.Mortality[aux.crf])))
        aux.Slope <- eval(parse(text=paste0("Slope.",Model.CRF[aux.crf]))) 
        
        #The mortality matrix (M)
        aux.Mortality <- diag(diag(aux.Deaths %*% t(aux.Slope)))
        
        #Note that we aim to assess P x ISRM x P' x M
        #Where:
        #P is InMAP.Cells.by.County
        #ISRM is ISRM.Store.Var
        #M is aux.Mortality
        
        #Nevertheless, it is much cheaper computationally to calculate
        #((ISRM x (P' x M))' x P')'
        #i.e. (ISRM' x P')' is ~2.5 orders of magnitude faster than P x ISRM
        aux.SRM <- t(t(ISRM.Store.Var %*% (t(InMAP.Cells.by.County) %*% aux.Mortality)) %*% t(InMAP.Cells.by.County))
        aux.Store.SRM[,,aux.slice] <- as.matrix(k.VSL*aux.SRM)
        aux.Store.MV[,aux.slice] <- k.VSL*rowSums(aux.SRM)

      }
    }
    if (aux.pm.year==2)
    {
      for (aux.asthma.age in 1:4)
      {
        # aux.asthma.age<-4
        #Loading Asthma incidence data (which is still called deaths because it was the old variable name)
        aux.Deaths <- as.matrix(eval(parse(text=paste0("Model_Inputs_Asthma",Asthma.Age[aux.asthma.age],"_2019")))[,-1])
        #Note: the first column was removed because it is the FIPS State + County code
        
        aux.Slice.Name <- paste(Pollutant.Variables[p],
                                paste0("PM",PM.Year[aux.pm.year]),
                                paste0("Age",Asthma.Age[aux.asthma.age]),
                                sep=".")
        aux.slice <- which(Slice.Names==aux.Slice.Name)
        
        print(paste0("Starting Slice No. ",aux.slice," of 100 -- ",aux.Slice.Name))
        
        # aux.Deaths <- eval(parse(text=paste0("aux.Deaths.",Model.CRF.Mortality[aux.crf])))
        aux.Slope <- Slope.Asthma
        if(aux.asthma.age==4)
        {
          aux.Slope <- Slope.Asthma.All
        }

        
        #The mortality matrix (M) (I am still calling it mortality though for Asthma this is incidence)
        aux.Mortality <- diag(diag(aux.Deaths %*% t(aux.Slope)))
        
        #Note that we aim to assess P x ISRM x P' x M
        #Where:
        #P is InMAP.Cells.by.County
        #ISRM is ISRM.Store.Var
        #M is aux.Mortality
        
        #Nevertheless, it is much cheaper computationally to calculate
        #((ISRM x (P' x M))' x P')'
        #i.e. (ISRM' x P')' is ~2.5 orders of magnitude faster than P x ISRM
        aux.SRM <- t(t(ISRM.Store.Var %*% (t(InMAP.Cells.by.County) %*% aux.Mortality)) %*% t(InMAP.Cells.by.County))
        aux.Store.SRM[,,aux.slice] <- as.matrix(k.VSC.Asthma*aux.SRM)
        aux.Store.MV[,aux.slice] <- k.VSC.Asthma*rowSums(aux.SRM)
      }
    }
 
  }
}

###### 7. Saving Results ######
#Results will be saved in .RData format and in .csv format
#For .RData, there are 10 files, 8 for mortality and 2 for childhood asthma:
# For Mortality:
#     SRM_BasePM_[PMYear]_Mortality_[MYear].RData
#     MV_Damages_BasePM_[PMYear]_Mortality_[MYear].RData
#     Where:
#           PMYear is the year of baseline ambient PM 2.5 concentrations data, and can be 2008 or 2017
#           MYear is the year of baseline mortality data, and can be 2008 or 2017

#     Each of the .RData files contains 4 datasets, one for each CRF:
#     For SRM, each dataset is an arrays with dimension 3,108 x 3,108 x 5
#           The rows are sources (3,108 counties); 
#           The columns are receptors (3,108 counties); and
#           The 5 slices are one for each pollutant. In order: Primary PM2.5, SO2, NOX, NH3, and VOC

#     For MV, each dataset is an matrix with dimension 3,108 x 5
#           The rows are sources (3,108 counties); 
#           The 5 columns are one for each pollutant. In order: Primary PM2.5, SO2, NOX, NH3, and VOC

# For Childhood Asthma:
#     SRM_BasePM_[PMYear]_Asthma_[MYear].RData
#     MV_Damages_BasePM_[PMYear]_Asthma_[MYear].RData
#     Where:
#           PMYear is the year of baseline ambient PM 2.5 concentrations data, which is only 2017 for Asthma
#           MYear is the year of baseline mortality data, which is only 2019 for Asthma

#     Each of the .RData files contains only one dataset:
#     For SRM, the dataset is an array with dimension 3,108 x 3,108 x 20
#           The rows are sources (3,108 counties); 
#           The columns are receptors (3,108 counties); and
#           The 20 slices are one for each pollutant, repeated 4 times for the 4 age groups. 
#           1-5: In order: Primary PM2.5, SO2, NOX, NH3, and VOC for Ages 0-4
#           6-10: In order: Primary PM2.5, SO2, NOX, NH3, and VOC for Ages 5-11
#           11-15: In order: Primary PM2.5, SO2, NOX, NH3, and VOC for Ages 12-17
#           16-20: In order: Primary PM2.5, SO2, NOX, NH3, and VOC for all ages under 17 (Ages 0-17)

#     For MV, each dataset is an matrix with dimension 3,108 x 29
#           The rows are sources (3,108 counties); 
#           The 20 columns are one for each pollutant, repeated 4 times for the 4 age groups. 
#           1-5: In order: Primary PM2.5, SO2, NOX, NH3, and VOC for Ages 0-4
#           6-10: In order: Primary PM2.5, SO2, NOX, NH3, and VOC for Ages 5-11
#           11-15: In order: Primary PM2.5, SO2, NOX, NH3, and VOC for Ages 12-17
#           16-20: In order: Primary PM2.5, SO2, NOX, NH3, and VOC for all ages under 17 (Ages 0-17)

#For the .csv result files, there are:
      #Each of the 100 SRM (80 for mortality and 20 for childhood asthma) and will be saved as an individual .csv file.
#           Each file is a 3,108 x 3,108 matrix, where rows are the sources (3,108 counties) and columns the receptors (3,108 counties)
#           Filenames are:
#           For Mortality:
#           SRM_Damages_BasePM_[PMYear]_Mortality_[MYear]_[CRF]_[Pollutant].csv
#           Where:
#                 PMYear is the year of baseline ambient PM 2.5 concentrations data, and can be 2008 or 2017
#                 MYear is the year of baseline mortality data, and can be 2008 or 2017
#                 CRF is the CRF used (GEMM, Vodonos_Parametric, Vodonos_Spline, or Krewski)
#                 Pollutant is one of Primary_PM2.5, SO2, NOX, NH3, or VOC
#           For Asthma:
#           SRM_Damages_BasePM_[PMYear]_Asthma_[AgeGroup]_[CRF]_[Pollutant].csv
#           Where:
#                 PMYear is the year of baseline ambient PM 2.5 concentrations data, which is 2017 for asthma
#                 AgeGroup is the age group, which can be 'Age04' for 0-4 years old, 'Age511' for 5-11 years old, 'Age1217' for 12-17 years old, and 'Age' for all under 17 years old (0-17 years old)
#                 CRF is the CRF used (Tetreault)
#                 Pollutant is one of Primary_PM2.5, SO2, NOX, NH3, or VOC

#     The 100 MVs will be saved as a single 3,108 x 101 matrix (the first column is the row.number)
#     Filename: MarginalValues_AllResults.csv
#           Rows are the sources (3,108 counties)
#           For Mortality (columns 2-81)
#           Columns are the combination of baseline PM Year, baseline mortality, CRF, and Pollutant, named:
#           MV_Damages_BasePM_[PMYear]_Mortality_[MYear]_[CRF]_[Pollutant]
#           Where:
#                 PMYear is the year of baseline ambient PM 2.5 concentrations data, and can be 2008 or 2017
#                 MYear is the year of baseline mortality data, and can be 2008 or 2017
#                 CRF is the CRF used (GEMM, Vodonos_Parametric, Vodonos_Spline, or Krewski)
#                 Pollutant is one of Primary_PM2.5, SO2, NOX, NH3, or VOC
#           For Asthma(columns 82-101)
#           Columns are the combination of pollutant and age group
#           SRM_Damages_BasePM_[PMYear]_Asthma_[AgeGroup]_[CRF]_[Pollutant].csv
#           Where:
#                 PMYear is the year of baseline ambient PM 2.5 concentrations data, which is 2017 for asthma
#                 AgeGroup is the age group, which can be 'Age04' for 0-4 years old, 'Age511' for 5-11 years old, 'Age1217' for 12-17 years old, and 'Age' for all under 17 years old (0-17 years old)
#                 CRF is the CRF used (Tetreault)
#                 Pollutant is one of Primary_PM2.5, SO2, NOX, NH3, or VOC


# aux.m.year<-1
# aux.crf <- 1
# rm(ISRM.Store.Var)
Store.MV <- aux.Store.MV
for (aux.pm.year in c(1,2))
{
  for (aux.m.year in c(1,2))
  {
    for (aux.crf in 1:4)
    {
      aux.slice.name <- paste(paste0("PM",PM.Year[aux.pm.year]),
                              paste0("M",Mortality.Year[aux.m.year]),
                              Model.CRF[aux.crf],
                              sep=".")
      aux.slice.number <- grep(aux.slice.name,Slice.Names,fixed=TRUE)
      aux.slice.number <- aux.slice.number[order(aux.slice.number,decreasing=FALSE)]
      assign(paste0(Model.CRF.Names.SRM[aux.crf],"_BasePM_",PM.Year[aux.pm.year],"_Mortality_",Mortality.Year[aux.m.year]),eval(aux.Store.SRM[,,aux.slice.number]))
      assign(paste0(Model.CRF.Names.MV[aux.crf],"_BasePM_",PM.Year[aux.pm.year],"_Mortality_",Mortality.Year[aux.m.year]),eval(as.matrix(aux.Store.MV[,aux.slice.number])))
      
      #Writing SRM .csv file
      #Please change file path on aux.csv.filepath variable
      aux.csv.filepath <- "~/Ernani_spengler_lab/School_Bus/Store_Results_080623/CSV/"
      aux.csv.filename <- paste0("SRM_Damages_BasePM_",
                                 PM.Year[aux.pm.year],
                                 "_Mortality_",
                                 Mortality.Year[aux.m.year],
                                 "_CRF_",
                                 Model.CRF.csv[aux.crf])
      for (aux.pol in 1:5)
      {
        write.csv(as.matrix(aux.Store.SRM[,,aux.slice.number[aux.pol]]),
                  file=paste0(aux.csv.filepath,
                              aux.csv.filename,
                              "_",
                              Pollutant.Names.csv[aux.pol],
                              ".csv"))
        
        #Standardizing MV colnames
        colnames(Store.MV)[aux.slice.number[aux.pol]] <- paste0("MV_Damages_BasePM_",
                                                                PM.Year[aux.pm.year],
                                                                "_Mortality_",
                                                                Mortality.Year[aux.m.year],
                                                                "_CRF_",
                                                                Model.CRF.csv[aux.crf],Pollutant.Names.csv[aux.pol])
      }


    }
    #Full S-R matrix
    #Please change file path on File.Save.Name.SRM variable
    File.Save.Name.SRM <- paste0("~/Ernani_spengler_lab/School_Bus/Store_Results_080623/RData/SRM_BasePM_",PM.Year[aux.pm.year],"_Mortality_",Mortality.Year[aux.m.year],".RData")
    save(list=paste0(Model.CRF.Names.SRM[1:4],"_BasePM_",PM.Year[aux.pm.year],"_Mortality_",Mortality.Year[aux.m.year]),
         file=File.Save.Name.SRM)
    
    #Marginal values
    #Please change file path on File.Save.Name.Marg variable
    File.Save.Name.Marg <- paste0("~/Ernani_spengler_lab/School_Bus/Store_Results_080623/RData/MV_Damages_BasePM_",PM.Year[aux.pm.year],"_Mortality_",Mortality.Year[aux.m.year],".RData")
    save(list=paste0(Model.CRF.Names.MV[1:4],"_BasePM_",PM.Year[aux.pm.year],"_Mortality_",Mortality.Year[aux.m.year]),
         file=File.Save.Name.Marg)
    
  }
  if(aux.pm.year==2)
  {

    paste(Pollutant.Variables[p],
          paste0("PM",PM.Year[aux.pm.year]),
          paste0("Age",Asthma.Age[aux.asthma.age]),
          sep=".")
    
    aux.slice.name <- paste(paste0("PM",PM.Year[aux.pm.year]),
                            paste0("Age"),
                            sep=".")
    
    aux.slice.number <- grep(aux.slice.name,Slice.Names,fixed=TRUE)
    aux.slice.number <- aux.slice.number[order(aux.slice.number,decreasing=FALSE)]
    assign(paste0("SR.Damages.Tetreault","_BasePM_",PM.Year[aux.pm.year],"_Asthma_2019"),eval(aux.Store.SRM[,,aux.slice.number]))
    assign(paste0("Marg.Damages.Tetreault","_BasePM_",PM.Year[aux.pm.year],"_Asthma_2019"),eval(as.matrix(aux.Store.MV[,aux.slice.number])))
    
    
    #Writing SRM .csv file
    #Please change file path on aux.csv.filepath variable
    aux.csv.filepath <- "~/Ernani_spengler_lab/School_Bus/Store_Results_080623/CSV/"
    for (aux.asthma.age in 1:4)
    {
      aux.csv.filename <- paste0("SRM_Damages_BasePM_",
                                 PM.Year[aux.pm.year],
                                 "_Asthma_Age",
                                 Asthma.Age[aux.asthma.age],
                                 "_CRF_Tetreault")
      for (aux.pol in 1:5)
      {
        aux.asthma.slice <- ((aux.asthma.age-1)*5+aux.pol)
        write.csv(as.matrix(aux.Store.SRM[,,aux.slice.number[aux.asthma.slice]]),
                  file=paste0(aux.csv.filepath,
                              aux.csv.filename,
                              "_",
                              Pollutant.Names.csv[aux.pol],
                              ".csv"))
        
        #Standardizing MV colnames
        
        colnames(Store.MV)[aux.slice.number[aux.asthma.slice]] <- paste0("SRM_Damages_BasePM_",
                                                                         PM.Year[aux.pm.year],
                                                                         "_Asthma_Age",
                                                                         Asthma.Age[aux.asthma.age],
                                                                         "_CRF_Tetreault",Pollutant.Names.csv[aux.pol])
        
      }
    }
    
    #Full S-R matrix
    #Please change file path on File.Save.Name.SRM variable
    File.Save.Name.SRM <- paste0("~/Ernani_spengler_lab/School_Bus/Store_Results_080623/RData/SRM_BasePM_",PM.Year[aux.pm.year],"_Asthma_2019.RData")
    save(list=paste0("SR.Damages.Tetreault","_BasePM_",PM.Year[aux.pm.year],"_Asthma_2019"),
         file=File.Save.Name.SRM)
    
    #Marginal values
    #Please change file path on File.Save.Name.Marg variable
    File.Save.Name.Marg <- paste0("~/Ernani_spengler_lab/School_Bus/Store_Results_080623/RData/MV_Damages_BasePM_",PM.Year[aux.pm.year],"_Asthma_2019.RData")
    save(list=paste0("Marg.Damages.Tetreault","_BasePM_",PM.Year[aux.pm.year],"_Asthma_2019"),
         file=File.Save.Name.Marg)
  }
}

warnings()
#Writing MV .csv file
write.csv(Store.MV, file=paste0(aux.csv.filepath,"MarginalValues_AllResults.csv"))
