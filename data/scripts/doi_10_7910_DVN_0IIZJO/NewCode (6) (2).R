####################### ** Initialize #################################################################
cat("\014")  
#install.packages("readxl")
library(readxl)
Rady_for_R_GBPUSD = read_excel("C:\\Users\\Teona\\Desktop\\Exchange Rate Project\\Datasets\\Rady_for_R_GBPUSD.xlsx",sheet = "RadyGBPUSD") 
withpositiveentrophies <- read.csv("C:/Users/Teona/Desktop/Exchange Rate Project/Datasets/withpositiveentrophies.csv", sep=";")
#frompython <- read.csv("C:/Users/User/Desktop/Book1.csv", sep=",")
###frompython <- read_excel("C:\\Users\\User\\Desktop\\Book1.xlsx",sheet = "Book1")
#frompython_EU <- read.csv("C:/Users/User/Desktop/Book1_EU.csv", sep=",")
frompython_CA <- read.csv("C:/Users/User/Desktop/CAD.csv", sep=",")
frompython_NO <- read.csv("C:/Users/User/Desktop/NOK.csv", sep=",")
frompython_CN <- read.csv("C:/Users/User/Desktop/CNY.csv", sep=",")
frompython<- read.csv("C:/Users/User/Desktop/GBPUSD.csv", sep=",")
frompython<- read.csv("C:/Users/Tshug/OneDrive/Desktop/GBPUSD.csv", sep=",")
frompython_EU <- read.csv("C:/Users/User/Desktop/EURUSD.csv", sep=",")
#EPU <- read.csv("C:/Users/User/Desktop/EPU.csv", sep=",")
#EPU_CA <- read.csv("C:/Users/User/Desktop/EPU_CA.csv", sep=",")
#EPU_EU <- read.csv("C:/Users/User/Desktop/EPU_EU.csv", sep=",")
#here we attach with logarithmized data
##EPU <- read.csv("C:/Users/Tshug/OneDrive/Desktop/EPU.csv", sep=",")
#EPU <- read.csv("C:/Users/User/Desktop/FX/EPU/GBP/EPU.csv", sep=",")
#EPU_CA <- read.csv("C:/Users/User/Desktop/FX/EPU/CAD/EPU_CA.csv", sep=",")
#EPU_EU <- read.csv("C:/Users/User/Desktop/FX/EPU/EUR/EPU_EU.csv", sep=",")

UStoGBP2017.21 <- read.csv("C:/Users/Teona/Desktop/Exchange Rate Project/Datasets/UStoGBP2017-21.csv", sep=";")
attach(Rady_for_R_GBPUSD)
attach(frompython)
attach(withpositiveentrophies)
attach(UStoGBP2017.21)
#attach(merge_exchangedialy)
library(forecast)
library(zoo)
library("vars", lib.loc="~/R/win-library/3.5") 
library("forecast", lib.loc="~/R/win-library/3.5")
library("cointReg")
library("aTSA", lib.loc="~/R/win-library/3.5")
library("car", lib.loc="~/R/win-library/3.5")
library("tseries", lib.loc="~/R/win-library/3.5")
library("deseasonalize", lib.loc="~/R/win-library/3.5")
library("EnvStats", lib.loc="~/R/win-library/3.5")
library("FinTS", lib.loc="~/R/win-library/3.5")
library("mFilter", lib.loc="~/R/win-library/3.5")
library("lmtest", lib.loc="~/R/win-library/3.5")
library(tsDyn)
library(vars)

####################### Create time series data UK #################################################################
Monthly_USD_to_UK.ts<- ts(Rady_for_R_GBPUSD$Monthly_USD_to_UK[13:224], start = c(2000, 1), frequency=12)
Real_Monthly_USD_to_UK.ts<- ts(Rady_for_R_GBPUSD$Real_USGBP[13:224], start = c(2000, 1), frequency=12)
#forSMA.ts<- ts(Rady_for_R_GBPUSD$Monthly_USD_to_UK[1:224], start = c(2000, 1), frequency=12)
#Monthly_USD_to_UK.ts <- SMA(forSMA.ts,n=12)
log_Monthly_USD_to_UK.ts = ts(log(Monthly_USD_to_UK.ts), start = c(2000, 1), frequency=12)
M2_US.ts<- ts(Rady_for_R_GBPUSD$M2_US[13:224], start = c(2000, 1), frequency=12)
M2_UK.ts<- ts(Rady_for_R_GBPUSD$M2_UK[13:224], start = c(2000, 1), frequency=12)
Inflation_gap_US.ts<- ts(Rady_for_R_GBPUSD$Inflation_gap_US[13:224], start = c(2000, 1), frequency=12)
Inflation_gap_UK.ts<- ts(Rady_for_R_GBPUSD$Inflation_gap_UK[13:224], start = c(2000, 1), frequency=12)
Inflation_US.ts<- ts(Rady_for_R_GBPUSD$CPI_US[13:224], start = c(2000, 1), frequency=12)
Inflation_UK.ts<- ts(Rady_for_R_GBPUSD$CPI_UK[13:224], start = c(2000, 1), frequency=12)
Money_Market_Rate_US.ts<- ts(Rady_for_R_GBPUSD$Money_Market_Rate_US[13:224], start = c(2000, 1), frequency=12)
Money_Market_Rate_UK.ts<- ts(Rady_for_R_GBPUSD$Money_Market_Rate_UK[13:224], start = c(2000, 1), frequency=12)
############ News Data  from dataset withpositiveentrophies
monthlyentrophy_Stock_Market.ts <-ts(withpositiveentrophies$mean_EntroCommodity_Oil, frequency = 12, start = c(2000, 1))
monthlyentrophy_Economic_Development.ts <-ts(withpositiveentrophies$mean_EntrocentralBank_MontoryPol, frequency = 12, start = c(2000, 1))
monthlyentrophy_CentrlBank.ts <-ts(withpositiveentrophies$mean_EntroFX_CentrlBank, frequency = 12, start = c(2000, 1))
monthlyentrophy_Micro_Finance.ts <-ts(withpositiveentrophies$mean_EntroMicro_Finance, frequency = 12, start = c(2000, 1))
monthlyentrophy_Export_import.ts <-ts(withpositiveentrophies$mean_EntroExport_import, frequency = 12, start = c(2000, 1))

monthlyentrophy_pub_policy.ts<-ts(withpositiveentrophies$mean_Entroconomy_pub_policy, frequency = 12, start = c(2000, 1))
monthlyentrophy_Stock_market1.ts<-ts(withpositiveentrophies$mean_EntroStock_market, frequency = 12, start = c(2000, 1))

lagMonthly_USD_to_UK.ts<- ts(Rady_for_R_GBPUSD$Monthly_USD_to_UK[12:223], start = c(2000, 1), frequency=12)
log_Monthly_USD_to_UK_l1.ts = ts(log(lagMonthly_USD_to_UK.ts), start = c(2000, 1), frequency=12)
Monthly_USD_to_UK.ts = ts((log_Monthly_USD_to_UK.ts-log_Monthly_USD_to_UK_l1.ts), start = c(1999, 12), frequency=12)
Monthly_USD_to_UK.ts = ts((Monthly_USD_to_UK.ts-lagMonthly_USD_to_UK.ts), start = c(1999, 12), frequency=12)
Monthly_USD_to_UK.ts = ts((log_Monthly_USD_to_UK.ts-log_Monthly_USD_to_UK_l1.ts), start = c(1999, 12), frequency=12)

#monthlyentrophy_Economy_info_pos.ts<-ts(withpositiveentrophies$monthlyentrophy_Economy_info_pos, frequency = 12, start = c(2000, 1))
####################### Create lagged macro time series ################
Inflation_gap_US.ts<- ts(Rady_for_R_GBPUSD$Inflation_gap_US[12:223], start = c(1999, 12), frequency=12)
Inflation_gap_UK.ts<- ts(Rady_for_R_GBPUSD$Inflation_gap_UK[12:223], start = c(1999, 12), frequency=12)
Inflation_US.ts<- ts(Rady_for_R_GBPUSD$Inflation_US[12:223], start = c(1999, 12), frequency=12)
Inflation_UK.ts<- ts(Rady_for_R_GBPUSD$Inflation_UK[12:223], start = c(1999, 12), frequency=12)
#Money_Market_Rate_US.ts<- ts(Rady_for_R_GBPUSD$Money_Market_Rate_US[12:223], start = c(1999, 12), frequency=12)
#Money_Market_Rate_UK.ts<- ts(Rady_for_R_GBPUSD$Money_Market_Rate_UK[12:223], start = c(1999, 12), frequency=12)
Real_Monthly_USD_to_UK.ts<- ts(Rady_for_R_GBPUSD$Real_USGBP[12:223], start = c(1999, 12), frequency=12)
Money_Market_Rate_US.ts<- ts(Rady_for_R_GBPUSD$Money_Market_Rate_US[11:222], start = c(1999, 11), frequency=12)
Money_Market_Rate_UK.ts<- ts(Rady_for_R_GBPUSD$Money_Market_Rate_UK[11:222], start = c(1999, 11), frequency=12)

Real_Monthly_USD_to_UK.ts<- ts(lag(Real_Monthly_USD_to_UK.ts), start = c(2000, 1), frequency=12)
Inflation_gap_US.ts<- ts(lag(Inflation_gap_US.ts), start = c(2000, 1), frequency=12)
Inflation_gap_UK.ts<- ts(lag(Inflation_gap_UK.ts), start = c(2000, 1), frequency=12)
Inflation_US.ts<- ts(lag(Inflation_US.ts), start = c(2000, 1), frequency=12)
Inflation_UK.ts<- ts(lag(Inflation_UK.ts), start = c(2000, 1), frequency=12)
Money_Market_Rate_US.ts<- ts(lag(lag(Money_Market_Rate_US.ts)), start = c(2000, 1), frequency=12)
Money_Market_Rate_UK.ts<- ts(lag(lag(Money_Market_Rate_UK.ts)), start = c(2000, 1), frequency=12)
########################1) Economic Activity (monthly GDP) ######################################################
EA_US.ts<- ts(Rady_for_R_GBPUSD$GDP_US[13:223], start = c(2000, 1), frequency=12)
EA_UK.ts<- ts(Rady_for_R_GBPUSD$GDP_UK[13:223], start = c(2000, 1), frequency=12)

#
#
#EA_US <-hpfilter(EA_US.ts,freq=14400,type="lambda",drift=FALSE)
#EA_US <-EA_US[["trend"]]
#EA_UK <-hpfilter(EA_UK.ts,freq=14400,type="lambda",drift=TRUE)
#EA_UK <-EA_UK[["trend"]]

#EA_US.ts<- ts(EA_US, start = c(2000, 1), frequency=12)
#EA_UK.ts<- ts(EA_UK, start = c(2000, 1), frequency=12)
############################################ 2) Economic Activity Index (from OECD)###################################
EAI_US.ts <-ts(Rady_for_R_GBPUSD$GDP_Index_US[13:223], start = c(2000, 1), frequency=12)
EAI_UK.ts <-ts(Rady_for_R_GBPUSD$GDP_Index_UK[13:223], start = c(2000, 1), frequency=12)

#EA_US <-EAI_US.ts 
#EA_UK <-EAI_UK.ts 
EA_US.ts<- ts(EAI_US.ts, start = c(2000, 1), frequency=12)
EA_UK.ts<- ts(EAI_UK.ts, start = c(2000, 1), frequency=12)

########################## 3) Economic Activity Gap (GDP-PotentialGDP)/PotentialGDP###################################
Potential_EA_US.ts<- ts(Rady_for_R_GBPUSD$Potential_GDP_US[13:223], start = c(2000, 1), frequency=12)
#decomp <- stl(Potential_EA_US.ts, s.window="periodic")
#Potential_EA_US.ts <-seasadj(decomp)

Potential_EA_UK.ts<- ts(Rady_for_R_GBPUSD$Potential_GDP_UK[13:223], start = c(2000, 1), frequency=12)
#decomp <- stl(Potential_EA_UK.ts, s.window="periodic")
#Potential_EA_UK.ts <-seasadj(decomp)

EA_Gap_US.ts<- ((EA_US.ts-Potential_EA_US.ts)/Potential_EA_US.ts)
EA_Gap_UK.ts<- ((EA_UK.ts-Potential_EA_UK.ts)/Potential_EA_UK.ts)

EA_US.ts <-EA_Gap_US.ts 
EA_UK.ts <-EA_Gap_UK.ts 

############################ 4) Output from filtered IPI ##############################################
#EA_US.ts<-(EA_US.ts$trend)
EA_US.ts<- ts(Rady_for_R_GBPUSD$IPI_US[13:223], start = c(2000, 1), frequency=12)
EA_UK.ts<- ts(Rady_for_R_GBPUSD$IPI_UK[13:223], start = c(2000, 1), frequency=12)
GDP_US.ts<- ts(Rady_for_R_GBPUSD$IPI_US[13:223]/100, start = c(2000, 1), frequency=12)
GDP_UK.ts<- ts(Rady_for_R_GBPUSD$IPI_UK[13:223]/100, start = c(2000, 1), frequency=12)
#plot(GDP_Gap_US.ts$x)
#plot(GDP_Gap_US.ts$trend)

# #library("mFilter", lib.loc="~/R/win-library/3.5")
# EA_US <-hpfilter(GDP_US.ts,freq=14400,type="lambda",drift=FALSE)
# EA_US <-EA_US[["trend"]]
# EA_UK <-hpfilter(GDP_UK.ts,freq=14400,type="lambda",drift=TRUE)
# EA_UK <-EA_UK[["trend"]]

EA_US.ts<- ts(EA_US, start = c(2000, 1), frequency=12)
EA_UK.ts<- ts(EA_UK, start = c(2000, 1), frequency=12)

############################ 5) Output from filtered IPI GAP.  run 4) first##############################################
Potential_EA_US.ts<- ts(Rady_for_R_GBPUSD$Potential_GDP_US[13:223], start = c(2000, 1), frequency=12)
Potential_EA_UK.ts<- ts(Rady_for_R_GBPUSD$Potential_GDP_UK[13:223], start = c(2000, 1), frequency=12)
EA_Gap_US.ts<- ((EA_US.ts-Potential_EA_US.ts)/Potential_EA_US.ts)*100
EA_Gap_UK.ts<- ((EA_UK.ts-Potential_EA_UK.ts)/Potential_EA_UK.ts)*100
EA_US <-EA_Gap_US.ts 
EA_UK <-EA_Gap_UK.ts 

############################ EURO Bind Data old ############################################## 
#Rady_for_R_EURUSD = read_excel("D:/Teo computer acer/Exchange Rate Project/Datasets/Rady_for_R_EURUSD.xlsx",sheet = "Sheet1") 
Rady_for_R_EURUSD <- read.csv("D:/Teo computer acer/Exchange Rate Project/Datasets/Rady_for_R_EURUSD.csv", sep=";", quote="", stringsAsFactors=FALSE)
#attach(Rady_for_R_EURUSD)
Monthly_USD_to_EUR.ts<- ts(Rady_for_R_EURUSD$Monthly_USD_to_EUR[13:223], start = c(2000, 1), frequency=12)
EA_EU.ts <-ts(Rady_for_R_EURUSD$GDP_Index_EU[13:223], start = c(2000, 1), frequency=12)
M2_EU.ts<- ts(Rady_for_R_EURUSD$M2_Europe[13:223], start = c(2000, 1), frequency=12)
log_Monthly_USD_to_EU.ts = ts(log(Monthly_USD_to_EU.ts), start = c(2000, 1), frequency=12)
Inflation_US.ts<- ts(Rady_for_R_EURUSD$Inflation_US[13:223], start = c(2000, 1), frequency=12)
Inflation_EU.ts<- ts(Rady_for_R_EURUSD$Inflation_EU[13:223], start = c(2000, 1), frequency=12)
Money_Market_Rate_US.ts<- ts(Rady_for_R_EURUSD$Money_Market_Rate_US[13:223], start = c(2000, 1), frequency=12)
Money_Market_Rate_EU.ts<- ts(Rady_for_R_EURUSD$Money_Market_Rate_EU[13:223], start = c(2000, 1), frequency=12)
#Monthly_USD_to_UK.ts<- var(Monthly_USD_to_UK.ts)
#lagMonthly_USD_to_UK.ts<- ts(frompython$Monthly_USD_to_UK[12:223], start = c(1999, 12), frequency=12)
#Monthly_USD_to_UK.ts<-Monthly_USD_to_UK.ts-lagMonthly_USD_to_UK.ts

ywithnews=cbind(Monthly_USD_to_EUR.ts, Inflation_US.ts, Inflation_EU.ts, Money_Market_Rate_US.ts, Money_Market_Rate_EU.ts, EA_US.ts,EA_EU.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts)
colnames(ywithnews) <- c("USDEUR","Inflation_US","Inflation_EU","Money_Market_Rate_US","Money_Market_Rate_EU","Economic_Activity_US","Economic_Activity_EU","Entropy_Stock_Market","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade") 
shortcolnames=c('USDEUR','Inflation US','Inflation EU','Interest Rate US','Interest Rate EU','Economic Activity US','Economic Activity EU','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
# # bind whitout news
# ywithnews=cbind(Monthly_USD_to_EUR.ts, Inflation_US.ts, Inflation_EU.ts, Money_Market_Rate_US.ts, Money_Market_Rate_EU.ts, EA_US.ts,EA_EU.ts)
# colnames(ywithnews) <- c("USDEUR","Inflation_US","Inflation_EU","Money_Market_Rate_US","Money_Market_Rate_EU","Economic_Activity_US","Economic_Activity_EU")
# shortcolnames=c('GBPUSD','Inflation US','Inflation EU','Interest Rate US','Interest Rate EU','Economic Activity US','Economic Activity EU')

# #with M2
# ywithnews=cbind(Monthly_USD_to_EUR.ts, Inflation_US.ts, Inflation_EU.ts, Money_Market_Rate_US.ts, Money_Market_Rate_EU.ts, EA_US.ts, EA_EU.ts, M2_US.ts, M2_EU.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts) #, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts
# colnames(ywithnews) <- c("USDEUR","Inflation_US","Inflation_EU","Money_Market_Rate_US","Money_Market_Rate_EU","Economic_Activity_US","Economic_Activity_EU","M2_US", "M2_EU", "Entropy_Stock_Market","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade") #
# shortcolnames=c('GBPUSD','Inflation US','Inflation EU','Interest Rate US','Interest Rate EU','Economic Activity US','Economic Activity EU','M2_US','M2_EU','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News') #

# gadaadgilebuli 
ywithnews=cbind(Monthly_USD_to_EUR.ts, Money_Market_Rate_US.ts, Money_Market_Rate_EU.ts, EA_US.ts,EA_EU.ts, Inflation_US.ts, Inflation_EU.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts)
colnames(ywithnews) <- c("USDEUR","Inflation_US","Inflation_EU","Money_Market_Rate_US","Money_Market_Rate_EU","Economic_Activity_US","Economic_Activity_EU","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade") 
shortcolnames=c('USDEUR','Inflation US','Inflation EU','Interest Rate US','Interest Rate EU','Economic Activity US','Economic Activity EU','Economic Development News','FED News','Micro Finance News','International Trade News')
# # bind whitout news

# Clean from NAs
which(is.na(ywithnews))
ywithnews <- na.omit(ywithnews) 
which(is.na(ywithnews))

####################### ** Create time series data UK python #################################################################
######################Monthly_USD_to_UK.ts<- ts(frompython$GBPUSD, start = c(2000, 1), frequency=12)
Monthly_UK_to_USD.ts<- ts(frompython$GBPUSD, start = c(2000, 1), frequency=12)
#Real_Monthly_USD_to_UK.ts<- ts(frompython$Real_USGBP, start = c(2000, 1), frequency=12)
#forSMA.ts<- ts(Rady_for_R_GBPUSD$Monthly_USD_to_UK[1:224], start = c(2000, 1), frequency=12)
#Monthly_USD_to_UK.ts <- SMA(forSMA.ts,n=12)
#log_Monthly_USD_to_UK.ts = ts(log(Monthly_USD_to_UK.ts), start = c(2000, 1), frequency=12)
M2_US.ts<- ts(frompython$M2_US, start = c(2000, 1), frequency=12)
M2_UK.ts<- ts(frompython$M2_UK, start = c(2000, 1), frequency=12)
EA_US.ts<- ts(frompython$IPI_US, start = c(2000, 1), frequency=12)
EA_UK.ts<- ts(frompython$IPI_UK, start = c(2000, 1), frequency=12)
Inflation_US.ts<- ts(frompython$CPI_US, start = c(2000, 1), frequency=12)
Inflation_UK.ts<- ts(frompython$CPI_UK, start = c(2000, 1), frequency=12)
Money_Market_Rate_US.ts<- ts(frompython$Money_Market_Rate_US, start = c(2000, 1), frequency=12)
Money_Market_Rate_UK.ts<- ts(frompython$Money_Market_Rate_UK, start = c(2000, 1), frequency=12)
############ News Data  from dataset withpositiveentrophies
monthlyentrophy_Stock_Market.ts <-ts(frompython$Stock_Market_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Economic_Development.ts <-ts(frompython$Economic_Development_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_CentrlBank.ts <-ts(frompython$FED_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Micro_Finance.ts <-ts(frompython$Micro_Finance_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Export_import.ts <-ts(frompython$International_Trade_News, frequency = 12, start = c(2000, 1))
EPU_US.ts <-ts(frompython$EPU_US, frequency = 12, start = c(2000, 1))
EPU_UK.ts <-ts(frompython$EPU_UK, frequency = 12, start = c(2000, 1))

#monthlyentrophy_Economy_info_pos.ts<-ts(withpositiveentrophies$monthlyentrophy_Economy_info_pos, frequency = 12, start = c(2000, 1))

############################ *  Bind UK Data ############################################## 
#with M2
# ywithnews=cbind(Monthly_USD_to_UK.ts, Inflation_US.ts, Inflation_UK.ts, Money_Market_Rate_US.ts, Money_Market_Rate_UK.ts, EA_US.ts, EA_UK.ts, M2_US.ts, M2_UK.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts) #, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts
# colnames(ywithnews) <- c("GBPUSD","Inflation_US","Inflation_UK","Money_Market_Rate_US","Money_Market_Rate_UK","Economic_Activity_US","Economic_Activity_UK","M2_US", "M2_UK", "Entropy_Stock_Market","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade") #
# shortcolnames=c('GBPUSD','Inflation US','Inflation UK','Interest Rate US','Interest Rate UK','Economic Activity US','Economic Activity UK','M2 US','M2 UK','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News') #

################ GBPUSD
ywithnews=cbind(Monthly_UK_to_USD.ts, Inflation_US.ts, Inflation_UK.ts, Money_Market_Rate_US.ts, Money_Market_Rate_UK.ts, EA_US.ts, EA_UK.ts, M2_US.ts, M2_UK.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts, EPU_US.ts, EPU_UK.ts) ##, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts
colnames(ywithnews) <- c("GBPUSD","Inflation_US","Inflation_UK","Money_Market_Rate_US","Money_Market_Rate_UK","Economic_Activity_US","Economic_Activity_UK","M2_US", "M2_UK", "Entropy_Stock_Market","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade", "EPU_US", "EPU_UK") #
shortcolnames=c('GBPUSD','Inflation US','Inflation UK','Interest Rate US','Interest Rate UK','Economic Activity US','Economic Activity UK','M2 US','M2 UK','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News', "EPU_US", "EPU_UK") #

## Clean from NAs
which(is.na(ywithnews))
ywithnews <- na.omit(ywithnews) 
which(is.na(ywithnews))

####################### Create time series data EU python #################################################################
#Monthly_USD_to_EUR.ts<- ts(frompython_EU$USDEUR, start = c(2000, 1), frequency=12)
Monthly_EUR_to_USD.ts<- ts(frompython_EU$EURUSD, start = c(2000, 1), frequency=12)

#Real_Monthly_USD_to_UK.ts<- ts(frompython$Real_USGBP, start = c(2000, 1), frequency=12)
#forSMA.ts<- ts(Rady_for_R_GBPUSD$Monthly_USD_to_UK[1:224], start = c(2000, 1), frequency=12)
#Monthly_USD_to_UK.ts <- SMA(forSMA.ts,n=12)
log_Monthly_USD_to_EU.ts = ts(log(Monthly_USD_to_EU.ts), start = c(2000, 1), frequency=12)
M2_US.ts<- ts(frompython_EU$M2_US, start = c(2000, 1), frequency=12)
M2_EU.ts<- ts(frompython_EU$M2_EU, start = c(2000, 1), frequency=12)
EA_US.ts<- ts(frompython_EU$IPI_US, start = c(2000, 1), frequency=12)
EA_EU.ts<- ts(frompython_EU$IPI_EU, start = c(2000, 1), frequency=12)
Inflation_US.ts<- ts(frompython_EU$CPI_US, start = c(2000, 1), frequency=12)
Inflation_EU.ts<- ts(frompython_EU$CPI_EU, start = c(2000, 1), frequency=12)
Money_Market_Rate_US.ts<- ts(frompython_EU$Money_Market_Rate_US, start = c(2000, 1), frequency=12)
Money_Market_Rate_EU.ts<- ts(frompython_EU$Money_Market_Rate_EU, start = c(2000, 1), frequency=12)
############ News Data  from dataset withpositiveentrophies
monthlyentrophy_Stock_Market.ts <-ts(frompython$Stock_Market_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Economic_Development.ts <-ts(frompython$Economic_Development_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_CentrlBank.ts <-ts(frompython$FED_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Micro_Finance.ts <-ts(frompython$Micro_Finance_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Export_import.ts <-ts(frompython$International_Trade_News, frequency = 12, start = c(2000, 1))

#monthlyentrophy_Economy_info_pos.ts<-ts(withpositiveentrophies$monthlyentrophy_Economy_info_pos, frequency = 12, start = c(2000, 1))


##############  EURO Bind Data Python #######
# #with M2
# ywithnews=cbind(Monthly_USD_to_EUR.ts, Inflation_US.ts, Inflation_EU.ts, Money_Market_Rate_US.ts, Money_Market_Rate_EU.ts, EA_US.ts, EA_EU.ts, M2_US.ts, M2_EU.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts) #, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts
# colnames(ywithnews) <- c("USDEUR","Inflation_US","Inflation_EU","Money_Market_Rate_US","Money_Market_Rate_EU","Economic_Activity_US","Economic_Activity_EU","M2_US", "M2_EU", "Entropy_Stock_Market","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade") #
# shortcolnames=c('USDEUR','Inflation US','Inflation EU','Interest Rate US','Interest Rate EU','Economic Activity US','Economic Activity EU','M2_US','M2_EU','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News') #

#with M2
ywithnews=cbind(Monthly_EUR_to_USD.ts, Inflation_US.ts, Inflation_EU.ts, Money_Market_Rate_US.ts, Money_Market_Rate_EU.ts, EA_US.ts, EA_EU.ts, M2_US.ts, M2_EU.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts) #, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts
colnames(ywithnews) <- c("EURUSD","Inflation_US","Inflation_EU","Money_Market_Rate_US","Money_Market_Rate_EU","Economic_Activity_US","Economic_Activity_EU","M2_US", "M2_EU", "Entropy_Stock_Market","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade") #
shortcolnames=c('EURUSD','Inflation US','Inflation EU','Interest Rate US','Interest Rate EU','Economic Activity US','Economic Activity EU','M2_US','M2_EU','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News') #

# Clean from NAs
which(is.na(ywithnews))
ywithnews <- na.omit(ywithnews) 
which(is.na(ywithnews))



###EURUSD  ###########
fevd=fevd(fitVARGBP1, n.ahead=48) #forecast error variance decomposition
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('fevd_EURUSD.pdf')
ts.plot(fevd$`EURUSD`, col=c(legend_order), ylab= "percentage", main= "EUR/USD", lwd=2) #cex.main=2
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()

legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", '#FFFF00') #808000 olive
pdf('fevd_EURUSD_AllNews.pdf')
z=cbind(fevd$EURUSD[,1], fevd$EURUSD[,2], fevd$EURUSD[,3], fevd$EURUSD[,4], fevd$EURUSD[,5], fevd$EURUSD[,6], fevd$EURUSD[,7], fevd$EURUSD[,8], fevd$EURUSD[,9], fevd$EURUSD[,10]+fevd$EURUSD[,11]+fevd$EURUSD[,12]+fevd$EURUSD[,13]+fevd$EURUSD[,14])
ts.plot(z*100, ylab= "Contribution (%) to EUR/USD", main= "FEVD for EUR/USD", col=c(legend_order), lwd=2) #cex.main=2
sc=c('EURUSD','Inflation US','Inflation EU','Interest Rate US','Interest Rate EU','Economic Activity US','Economic Activity EU','M2 US','M2 EU','All News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order)) #col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(10, 30, 50, 70, 90), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()


# #EURUSD#####################################################################
legend_order_news <- c("#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('Allfx_EU_EURUSD_together.pdf')
z=cbind(fevd$EURUSD[,10], fevd$EURUSD[,11], fevd$EURUSD[,12], fevd$EURUSD[,13], fevd$EURUSD[,14])
ts.plot(z*100, type='l', xlab="time", ylab = "Contribution (%) to EUR/USD", col=c(legend_order_news), main = "FEVD of EUR/USD",lwd=2, ylim= c(0,5.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
sc=c('Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order_news)) #col=c(legend_order),ncol=1, inset = c(0, 0))
#abline(h = c(1, 3, 5, 7, 9), col = "gray", lty = 3)
#abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
abline(h = c(0.2, 0.4, 0.6, 0.8, 1.2, 1.4, 1.6, 1.8, 2.2, 2.4, 2.6, 2.8, 3.2, 3.4, 3.6, 3.8, 4.2, 4.4, 4.6, 4.8), col = "gray", lty = 3)
abline(v =c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
grid()
dev.off()
#EURUSD
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
y = names(fevd)[1]
pdf('10fx_EURUSD.pdf')
plot(100*fevd$EURUSD[,10], type='l', xlab="time", ylab = "Contribution (%) to EUR/USD", col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()
#EURUSD
pdf('11fx_EURUSD.pdf')
plot(100*fevd$EURUSD[,11], type='l', xlab="time", ylab = "Contribution (%) to EUR/USD", col=c(legend_order[[11]]), main = "Economic Development News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()
#EURUSD
pdf('12fx_EURUSD.pdf')
plot(100*fevd$EURUSD[,12], type='l', xlab="time", ylab = "Contribution (%) to EUR/USD", col=c(legend_order[[12]]), main = "FED News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()
#EURUSD
pdf('13fx_EURUSD.pdf')
plot(100*fevd$EURUSD[,13], type='l', xlab="time", ylab = "Contribution (%) to EUR/USD", col=c(legend_order[[13]]), main = "Micro Finance News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()
#EURUSD
pdf('14fx_EURUSD.pdf')
plot(100*fevd$EURUSD[,14], type='l', xlab="time", ylab = "Contribution (%) to EUR/USD", col=c(legend_order[[14]]), main = "International Trade News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

####################### Create time series data CAD python #################################################################
#Monthly_USD_to_CAD.ts<- ts(frompython_CAD$USDCAD, start = c(2000, 1), frequency=12)
Monthly_CAD_to_USD.ts<- ts(frompython_CA$CADUSD, start = c(2000, 1), frequency=12)

#Real_Monthly_USD_to_UK.ts<- ts(frompython$Real_USGBP, start = c(2000, 1), frequency=12)
#forSMA.ts<- ts(Rady_for_R_GBPUSD$Monthly_USD_to_UK[1:224], start = c(2000, 1), frequency=12)
#Monthly_USD_to_UK.ts <- SMA(forSMA.ts,n=12)
#log_Monthly_USD_to_CAD.ts = ts(log(Monthly_USD_to_CAD.ts), start = c(2000, 1), frequency=12)
M2_US.ts<- ts(frompython_CA$M2_US, start = c(2000, 1), frequency=12)
M2_CA.ts<- ts(frompython_CA$M2_CA, start = c(2000, 1), frequency=12)
EA_US.ts<- ts(frompython_CA$IPI_US, start = c(2000, 1), frequency=12)
EA_CA.ts<- ts(frompython_CA$IPI_CA, start = c(2000, 1), frequency=12)
Inflation_US.ts<- ts(frompython_CA$CPI_US, start = c(2000, 1), frequency=12)
Inflation_CA.ts<- ts(frompython_CA$CPI_CA, start = c(2000, 1), frequency=12)
Money_Market_Rate_US.ts<- ts(frompython_CA$Money_Market_Rate_US, start = c(2000, 1), frequency=12)
Money_Market_Rate_CA.ts<- ts(frompython_CA$Money_Market_Rate_CA, start = c(2000, 1), frequency=12)
############ News Data  from dataset withpositiveentrophies
monthlyentrophy_Stock_Market.ts <-ts(frompython_CA$Stock_Market_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Economic_Development.ts <-ts(frompython_CA$Economic_Development_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_CentrlBank.ts <-ts(frompython_CA$FED_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Micro_Finance.ts <-ts(frompython_CA$Micro_Finance_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Export_import.ts <-ts(frompython_CA$International_Trade_News, frequency = 12, start = c(2000, 1))

#monthlyentrophy_Economy_info_pos.ts<-ts(withpositiveentrophies$monthlyentrophy_Economy_info_pos, frequency = 12, start = c(2000, 1))

############## CAD Bind Data Python #######
#with M2
ywithnews=cbind(Monthly_CAD_to_USD.ts, Inflation_US.ts, Inflation_CA.ts, Money_Market_Rate_US.ts, Money_Market_Rate_CA.ts, EA_US.ts, EA_CA.ts, M2_US.ts, M2_CA.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts) #, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts
colnames(ywithnews) <- c("CADUSD","Inflation_US","Inflation_CA","Money_Market_Rate_US","Money_Market_Rate_CA","Economic_Activity_US","Economic_Activity_CA","M2_US", "M2_CA", "Entropy_Stock_Market","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade") #
shortcolnames=c('CADUSD','Inflation US','Inflation CA','Interest Rate US','Interest Rate CA','Economic Activity US','Economic Activity CA','M2 US','M2 CA','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News') #

# Clean from NAs
which(is.na(ywithnews))
ywithnews <- na.omit(ywithnews) 
which(is.na(ywithnews))


########## CAD fevd plot ###############################
fevd=fevd(fitVARGBP1, n.ahead=48) #forecast error variance decomposition

win.graph(width=20,height=20)
plot(fevd, col=2:15)
grid(NULL, NULL) #ny = 10, nx= 48
#legend_order <- c("#005EB8", "#0091DA", "#483698", "#470A68", "#6D2077", "#00A3A1", "#009A44", "#EAAA00", "#F68D2E", "#BC204B", "#333333", "#EAAA00", "#F68D2E", "#BC204B")
dev.off()


legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('fevd_CADUSD.pdf')
#par(mar=c(0,4,5,4),xpd=FALSE)
#ts.plot(fevd$`GBPUSD`, col=cols[legend_order], ylab= "percentage", main= "GBPUSD", lwd=2) #cex.main=2
ts.plot(fevd$`CADUSD`, col=c(legend_order), ylab= "percentage", main= "FEVD for CAD/USD", lwd=2) #cex.main=2
#ts.plot(fevd$`GBPUSD`, col=c(cols[2:13], 'darkseegreen4', 'coral4'), ylab= "percentage", main= "GBPUSD", lwd=2) #cex.main=2
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(legend_order),ncol=1, inset = c(0, 0))
#legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(cols[2:13]),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()

# fevd with ALL NEWS
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", '#FFFF00') #808000 olive
pdf('fevd_CADUSD_AllNews.pdf')
z=cbind(fevd$CADUSD[,1], fevd$CADUSD[,2], fevd$CADUSD[,3], fevd$CADUSD[,4], fevd$CADUSD[,5], fevd$CADUSD[,6], fevd$CADUSD[,7], fevd$CADUSD[,8], fevd$CADUSD[,9], fevd$CADUSD[,10]+fevd$CADUSD[,11]+fevd$CADUSD[,12]+fevd$CADUSD[,13]+fevd$CADUSD[,14])
ts.plot(z*100, ylab= "Contribution (%)", main= "FEVD for CAD/USD", col=c(legend_order), lwd=2) #cex.main=2
sc=c('CADUSD','Inflation US','Inflation CA','Interest Rate US','Interest Rate CA','Economic Activity US','Economic Activity CA','M2 US','M2 CA','All News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order)) #col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(10, 30, 50, 70, 90), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()

####### fevd CAD responses to News shocks ###########
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")

y = names(fevd)[1]
pdf('AllCAD.pdf')
ts.plot(fevd$CADUSD[,10]+fevd$CADUSD[,11]+fevd$CADUSD[,12]+fevd$CADUSD[,13]+fevd$CADUSD[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col='#6D2077', main = "All News", ylim = c(0,0.3)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

legend_order_news <- c("#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('Allfx_CA_together.pdf')
z=cbind(fevd$CADUSD[,10], fevd$CADUSD[,11], fevd$CADUSD[,12], fevd$CADUSD[,13], fevd$CADUSD[,14])
ts.plot(z*100, type='l', xlab="time", ylab = "Contribution (%) to CAD/USD", col=c(legend_order_news), main = "FEVD of CAD/USD",lwd=2, ylim= c(0,10)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
sc=c('Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order_news)) 
abline(h = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8.2, 8.4, 8.6, 8.8, 9, 9.2, 9.4, 9.6, 9.8), col = "gray", lty = 3) 
abline(v =c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
grid()
dev.off()

y = names(fevd)[1]
pdf('10CAD.pdf')
plot(fevd$CADUSD[,10], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()


pdf('11CAD.pdf')
plot(fevd$CADUSD[,11], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[11]]), main = "Economic Development News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('12CAD.pdf')
plot(fevd$CADUSD[,12], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[12]]), main = "FED News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('13CAD.pdf')
plot(fevd$CADUSD[,13], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[13]]), main = "Micro Finance News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('14CAD.pdf')
plot(fevd$CADUSD[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[14]]), main = "International Trade News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()







####################### Create time series data NOK python #################################################################
#Monthly_USD_to_CAD.ts<- ts(frompython_CAD$USDCAD, start = c(2000, 1), frequency=12)
Monthly_NOK_to_USD.ts<- ts(frompython_NO$NOKUSD, start = c(2000, 1), frequency=12)

#Real_Monthly_USD_to_UK.ts<- ts(frompython$Real_USGBP, start = c(2000, 1), frequency=12)
#forSMA.ts<- ts(Rady_for_R_GBPUSD$Monthly_USD_to_UK[1:224], start = c(2000, 1), frequency=12)
#Monthly_USD_to_UK.ts <- SMA(forSMA.ts,n=12)
#log_Monthly_USD_to_CAD.ts = ts(log(Monthly_USD_to_CAD.ts), start = c(2000, 1), frequency=12)
M2_US.ts<- ts(frompython_NO$M2_US, start = c(2000, 1), frequency=12)
M2_NO.ts<- ts(frompython_NO$M2_NO, start = c(2000, 1), frequency=12)
EA_US.ts<- ts(frompython_NO$IPI_US, start = c(2000, 1), frequency=12)
EA_NO.ts<- ts(frompython_NO$IPI_NO, start = c(2000, 1), frequency=12)
Inflation_US.ts<- ts(frompython_NO$CPI_US, start = c(2000, 1), frequency=12)
Inflation_NO.ts<- ts(frompython_NO$CPI_NO, start = c(2000, 1), frequency=12)
Money_Market_Rate_US.ts<- ts(frompython_NO$Money_Market_Rate_US, start = c(2000, 1), frequency=12)
Money_Market_Rate_NO.ts<- ts(frompython_NO$Money_Market_Rate_NO, start = c(2000, 1), frequency=12)
############ News Data  from dataset withpositiveentrophies
monthlyentrophy_Stock_Market.ts <-ts(frompython$Stock_Market_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Economic_Development.ts <-ts(frompython$Economic_Development_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_CentrlBank.ts <-ts(frompython$FED_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Micro_Finance.ts <-ts(frompython$Micro_Finance_News, frequency = 12, start = c(2000, 1))
monthlyentrophy_Export_import.ts <-ts(frompython$International_Trade_News, frequency = 12, start = c(2000, 1))

#monthlyentrophy_Economy_info_pos.ts<-ts(withpositiveentrophies$monthlyentrophy_Economy_info_pos, frequency = 12, start = c(2000, 1))

############## NOK Bind Data Python #######
#with M2
ywithnews=cbind(Monthly_NOK_to_USD.ts, Inflation_US.ts, Inflation_NO.ts, Money_Market_Rate_US.ts, Money_Market_Rate_NO.ts, EA_US.ts, EA_NO.ts, M2_US.ts, M2_NO.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts) #, monthlyentrophy_Micro_Finance.ts, monthlyentrophy_Export_import.ts
colnames(ywithnews) <- c("NOKUSD","Inflation_US","Inflation_NO","Money_Market_Rate_US","Money_Market_Rate_NO","Economic_Activity_US","Economic_Activity_NO","M2_US", "M2_NO", "Entropy_Stock_Market","Entropy_Economic_Development","Entropy_FED","Entropy_Micro_Finance","Entropy_International_Trade") #
shortcolnames=c('GBPUSD','Inflation US','Inflation NO','Interest Rate US','Interest Rate NO','Economic Activity US','Economic Activity NO','M2_US','M2_NO','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News') #

# Clean from NAs
which(is.na(ywithnews))
ywithnews <- na.omit(ywithnews) 
which(is.na(ywithnews))


########## NOK fevd plot ###############################
fevd=fevd(fitVARGBP1, n.ahead=48) #forecast error variance decomposition

win.graph(width=20,height=20)
plot(fevd, col=2:15)
grid(NULL, NULL) #ny = 10, nx= 48
#legend('topright',legend = c(colnames(ywithnews)),col = 2:13, lwd = 3, xpd = FALSE, horiz = TRUE, inset = c(2, 1)) #cex = 1, seg.len=1, bty = 'n', 
#write.xlsx(data.frame(fevd))
#shortcolnames=c('GBPUSD','Inflation US','Inflation UK','Interest Rate US','Interest Rate UK','Economic Activity US','Economic Activity UK','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
#legend_order <- matrix(2:15,ncol=1,byrow = TRUE)
#legend_order <- c("#005EB8", "#0091DA", "#483698", "#470A68", "#6D2077", "#00A3A1", "#009A44", "#EAAA00", "#F68D2E", "#BC204B", "#333333", "#EAAA00", "#F68D2E", "#BC204B")
dev.off()


legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('fevd_NOKUSD.pdf')
ts.plot(fevd$`NOKUSD`, col=c(legend_order), ylab= "percentage", main= "NOK/USD", lwd=2) #cex.main=2
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()


# fevd with ALL NEWS
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", '#FFFF00') #808000 olive
pdf('fevd_NOKUSD_AllNews.pdf')
z=cbind(fevd$NOKUSD[,1], fevd$NOKUSD[,2], fevd$NOKUSD[,3], fevd$NOKUSD[,4], fevd$NOKUSD[,5], fevd$NOKUSD[,6], fevd$NOKUSD[,7], fevd$NOKUSD[,8], fevd$NOKUSD[,9], fevd$NOKUSD[,10]+fevd$NOKUSD[,11]+fevd$NOKUSD[,12]+fevd$NOKUSD[,13]+fevd$NOKUSD[,14])
ts.plot(z*100, ylab= "Contribution (%)", main= "FEVD for NOKUSD", col=c(legend_order), lwd=2) #cex.main=2
sc=c('NOKUSD','Inflation US','Inflation NO','Interest Rate US','Interest Rate NO','Economic Activity US','Economic Activity NO','M2 US','M2 NO','All News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order)) #col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(10, 30, 50, 70, 90), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()


####### fevd NOK responses to News shocks ###########
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
y = names(fevd)[1]
pdf('AllNOK.pdf')
ts.plot(fevd$NOKUSD[,10]+fevd$NOKUSD[,11]+fevd$NOKUSD[,12]+fevd$NOKUSD[,13]+fevd$NOKUSD[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=rainbow(100), main = "All News", ylim = c(0,0.3)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

legend_order_news <- c("#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('Allfx_NO_together.pdf')
z=cbind(fevd$NOKUSD[,10], fevd$NOKUSD[,11], fevd$NOKUSD[,12], fevd$NOKUSD[,13], fevd$NOKUSD[,14])
ts.plot(z*100, type='l', xlab="time", ylab = "Contribution (%) to NOK/USD", col=c(legend_order_news), main = "FEVD of NOK/USD",lwd=2, ylim= c(0,6.8)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
sc=c('Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order_news)) 
abline(h = c(0.2, 0.4, 0.6, 0.8, 1.2, 1.4, 1.6, 1.8, 2.2, 2.4, 2.6, 2.8, 3.2, 3.4, 3.6, 3.8, 4.2, 4.4, 4.6, 4.8, 5.2, 5.4, 5.6, 5.8, 6.2, 6.4, 6.6, 6.8), col = "gray", lty = 3) 
abline(v =c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
grid()
dev.off()

y = names(fevd)[1]
pdf('10NOK.pdf')
plot(fevd$NOKUSD[,10], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()


pdf('11NOK.pdf')
plot(fevd$NOKUSD[,11], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[11]]), main = "Economic Development News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('12NOK.pdf')
plot(fevd$NOKUSD[,12], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[12]]), main = "FED News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('13NOK.pdf')
plot(fevd$NOKUSD[,13], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[13]]), main = "Micro Finance News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('14NOK.pdf')
plot(fevd$NOKUSD[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[14]]), main = "International Trade News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()






####### outliers #######
remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75))
  H <- 1.5 * IQR(x)
  #y <- x
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  x
}

#ywithnews[,1] = remove_outliers(ywithnews[,1])
for (col in ncol(ywithnews)) {
  ywithnews[,col] = remove_outliers(ywithnews[,col])
}

which(is.na(ywithnews))

######## Impute Outliers
impute_outliers <- function(data, cols = ncol(data)) {
  for (col in cols) {
    mean <- mean(data[,col], na.rm=TRUE)
    print(mean)
    data[,col].fillna(mean, inplace=True)
    }
  data
}

ywithnews = impute_outliers(ywithnews)
which(is.na(ywithnews))

####### outliers old ##########
detect_outlier < - function(x) {
  Quantile1 < - quantile(x, probs=.25)
  Quantile3 < - quantile(x, probs=.75)
  # calculate inter quartile range
  IQR = Quantile3-Quantile1
  # return true or false
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

# create remove outlier function
remove_outlier < - function(dataframe,
                            columns=names(dataframe)) {
  # for loop to traverse in columns vector
  for (col in columns) {
    # remove observation if it satisfies outlier function
    dataframe < - dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  # return dataframe
  print("Remove outliers")
  print(dataframe)
}

remove_outlier(ywithnews, c('GBPUSD', 'Inflation_US', 'Inflation_UK'))
############################ Find outliers ########
# Outliers with Boxplot
# for (i in ncol(ywithnews)){
#     outliers <- boxplot.stats(ywithnews[,i])$out # points outside the whiskers, 1.5*Inter Quartile Range
#     View(outliers) # we have outliers in inflation_US, Economic Development News and FED news.
# }
# outliers <- function(x) x %in% boxplot.stats(x)$out
# remove_outliers <- function(data, cols = ncol(data)) {
#   for (col in cols) {
#     is.na(data[,col]) <- outliers(data[,col])
#     data <- data[!data %in% outliers(data[,col])]
#     #data[is.na(data)] <- 0
#   }
#   data
# }

for (i in ncol(ywithnews)){
  Q1 <- quantile(ywithnews[,i], .25)
  Q3 <- quantile(ywithnews[,i], .75)
  IQR <- IQR(ywithnews[,i])
  print(IQR)
  outliers <- ywithnews[,i] > (Q1 - 1.5*IQR) & ywithnews[,i] < (Q3 + 1.5*IQR)
  View(outliers)
}
remove_outliers <- function(data, cols = ncol(data)) {
  for (col in cols) {
    #is.na(data[,col]) <- outliers(data[,col])
    #data <- data[!data %in% outliers(data[,col])]
    if(outliers(data[,col])==TRUE) {
      is.na(data[,col])}
    #data <- data[!data %in% outliers(data[,col])]
  }
  data
}

ywithnews = remove_outliers(ywithnews)
print(ywithnews)
which(is.na(ywithnews))
#ywithnews <- amelia(ywithnews,14) #C:\Users\User\AppData\Local\Temp\RtmpcvL6rT\downloaded_packages

# Impute Outliers
impute_outliers <- function(data, cols = ncol(data)) {
  for (col in cols) {
    mean <- mean(data[,col], na.rm=TRUE)
    print(mean)
    data[ ,col].fillna(mean, inplace=True)
  }
  data
}

ywithnews = impute_outliers(ywithnews)
which(is.na(ywithnews))

####################### First Differencing##############################################
# for (i in 1:ncol(ywithnews)) {
#   print (colnames(ywithnews)[i])
#   #kpss.test(ywithnews[,i], null="Trend")
#   #print(nsdiffs(ywithnews[,i])) #number of seasonal differences necessary
#   print(ndiffs(ywithnews[,i])) #number of first differences necessary
# }

ywithnewsdiff<-data.frame(matrix(nrow = nrow(ywithnews)-1, ncol = ncol(ywithnews)))
#colnames(ywithnewsdiff) <- c("GBPUSD","Inflation_US", "Inflation_UK",  "Money_Market_Rate_US", "Money_Market_Rate_UK", "Economic_Activity_US", "Economic_Activity_UK", "Entropy_Stock_Market", "Entropy_Economic_Development", "Entropy_FED", "Entropy_Micro_Finance", "Entropy_International_Trade") 
colnames(ywithnewsdiff) <- colnames(ywithnews) 

for (i in 1:ncol(ywithnews)) {
  ywithnewsdiff[,i] <-diff(ywithnews[,i], difference=1)
}
ywithnews =ywithnewsdiff
#colnames(ywithnews) <- c("GBPUSD","Inflation_US", "Inflation_UK",  "Money_Market_Rate_US", "Money_Market_Rate_UK", "Economic_Activity_US", "Economic_Activity_UK", "Entropy_Stock_Market", "Entropy_Economic_Development", "Entropy_FED", "Entropy_Micro_Finance", "Entropy_International_Trade") 
colnames(ywithnews) <- colnames(ywithnewsdiff) 

#################### Cointegration Test Johansen procedure -> There is no cointegration #####

jotest=ca.jo(ywithnews, type="trace", K=6, ecdet="none", spec="longrun")  # #4
summary(jotest) 
#w= 1.00000*ywithnews[, 1] -66.173164*ywithnews[, 2]-103.63450 *ywithnews[, 3]-100.51285*ywithnews[, 4]-504.64415*ywithnews[, 5]-398.67916*ywithnews[, 6]+173.63292*ywithnews[, 7]+249.84530*ywithnews[, 8]-782.03161*ywithnews[, 9]+976.47947*ywithnews[, 10]-106.80083*ywithnews[, 11]+160.08961*ywithnews[, 12] 
#w= 1.00000*ywithnews[, 1] -9.245544*ywithnews[, 2]-13.626842 *ywithnews[, 3]-8.345889*ywithnews[, 4]-108.498779*ywithnews[, 5]-74.841752*ywithnews[, 6]+33.986365*ywithnews[, 7]+54.015070*ywithnews[, 8]-110.470146*ywithnews[, 9]+161.756154*ywithnews[, 10]+5.257558*ywithnews[, 11]+50.294491*ywithnews[, 12] 
#latest w below 18.03.24
#w= 1.00000*ywithnews[, 1] -82.048806 *ywithnews[, 2] -4.423535 *ywithnews[, 3]+58.502502*ywithnews[, 4]-22.693187*ywithnews[, 5]+147.268948*ywithnews[, 6]+104.438114*ywithnews[, 7]-84.280949*ywithnews[, 8]+731.751109*ywithnews[, 9]-196.829850*ywithnews[, 10]+198.998724*ywithnews[, 11] -167.498385*ywithnews[, 12] 
#latest 4lags
#w = 1.000000 * ywithnews[, 1] - 4.507364 * ywithnews[, 2] + 1.239260 * ywithnews[, 3] - 0.078895 * ywithnews[, 4] - 0.031615 * ywithnews[, 5] - 0.267858 * ywithnews[, 6] + 0.166605 * ywithnews[, 7] - 0.000028 * ywithnews[, 8] + 0.000004 * ywithnews[, 9] - 5.108912 * ywithnews[, 10] + 0.280642 * ywithnews[, 11] + 36.538940 * ywithnews[, 12] - 30.943740 * ywithnews[, 13] + 3.264700 * ywithnews[, 14]

#for 6 algs
w = 1.000000 * ywithnews[, 1] + 802.1769 * ywithnews[, 2] - 572.4314 * ywithnews[, 3] - 0.03954655 * ywithnews[, 4] +37.72297 * ywithnews[, 5] +144.4919 * ywithnews[, 6] - 142.5813 * ywithnews[, 7] +0.01644418 * ywithnews[, 8] + 0.00001548652 * ywithnews[, 9] - 8312.689 * ywithnews[, 10] -1706.767 * ywithnews[, 11] + 1003.942 * ywithnews[, 12] - 919.0407 * ywithnews[, 13] - 234.6682 * ywithnews[, 14] - 0.01478644 * ywithnews[, 15] - 0.003823252 * ywithnews[, 16]

# # #with lagged macro -> also non stationary
# w= 1.00000*ywithnews[, 1]  -171.53405 *ywithnews[, 2] +97.27416 *ywithnews[, 3]+163.67320*ywithnews[, 4]+11.37666*ywithnews[, 5]+ 255.97756*ywithnews[, 6]+147.96789*ywithnews[, 7]-220.48438*ywithnews[, 8]+777.13227*ywithnews[, 9]-486.71235*ywithnews[, 10]+190.48244*ywithnews[, 11]-332.52393*ywithnews[, 12] 
w.ts <-ts(w, frequency = 12, start = c(2000, 1)) #cajorls(jotest)$beta*
#adf.test(Monthly_USD_to_UK.ts-w.ts)  # p value > 0.05 time series is non-stationary. # if residuals of the cointegration regression are stationary, i.e. Fx and other variables are cointegrated! so we should go for VECM model
adf.test(w.ts)
summary(ur.df(w.ts, type = "trend", lags = 2))
adf.test(ywithnews[, 1]-w.ts)
#adf.test(ywithnews[, 1]-cajoresids) 
#plotres(jotest)


# Estimate the error correction model (ECM)
# Construct lagged matrix for independent variables
lagged_matrix <- cbind(ywithnews[,-1], lag(ywithnews[,-1], 1))  # Assuming you want to include one lag
# Construct lagged dependent variable
lagged_y <- lag(ywithnews[, 1], 1)
# Remove NAs resulting from lagging
lagged_matrix <- lagged_matrix[-1, ]
lagged_y <- lagged_y[-1]
# Fit the ECM
ecm_model <- lm(ywithnews[-1, 1] ~ lagged_matrix + lagged_y)
# Extract residuals from the ECM
residuals_ecm <- residuals(ecm_model)


#### cointegration test for EU
jotest=ca.jo(ywithnews, type="trace", K=2, ecdet="none", spec="transitory")  #longrun #const, trend
summary(jotest) 
w= 1.00000*ywithnews[, 1] -492.448355*ywithnews[, 2]+320.554067 *ywithnews[, 3]+2.122344*ywithnews[, 4]+9.705508*ywithnews[, 5]+578.775877*ywithnews[, 6]-3.886523*ywithnews[, 7]-199.523443*ywithnews[, 8]+1727.466995*ywithnews[, 9]+69.284141*ywithnews[, 10]+262.419793*ywithnews[, 11]-1132.706357*ywithnews[, 12] 
w.ts <-ts(w, frequency = 12, start = c(2000, 1))
adf.test(Monthly_UK_to_USD.ts-w.ts)  # p value > 0.05 time series is non-stationary. # if residuals of the cointegration regression are stationary, i.e. Fx and other variables are cointegrated! so we should go for VECM model

############################ ***  *** *** Fit VAR Model ##############################################
VARselect(ywithnews)
#Residual Serial Correlation Lagrange Multiplier (LM) test
for(i in 1:15){
  print(bgtest(ywithnews[,1]~ywithnews[,2]+ywithnews[,3]+ywithnews[,4]+ywithnews[,5]+ywithnews[,6]+ywithnews[,7]+ywithnews[,8]+ywithnews[,9]+ywithnews[,10]+ywithnews[,11]+ywithnews[,12], order=i, data=ywithnews))
}

#fitVARGBP1<-VAR(ywithnews[1:7], p =5, type = "both", season = NULL, exogen = ywithnews[8:12], lag.max = 4, ic = "HQ") #const ic = "AIC"
fitVARGBP1<-VAR(ywithnews, p =6, type = "both", season = NULL, exogen = NULL, ic = "HQ") #const ic = "AIC" # for p=5 and lag max 14 vecm produced important respond to FED news

normality.test(fitVARGBP1)
residuals(ywithnews)
#shapiro.test(residuals(fitVARGBP1)) # p<0.05 not normal
serial.test(fitVARGBP1,lags.pt=8, type="BG")  # p-value<0.05 -> autocorrelated #lags.pt=14, "BG"
acf(fitVARGBP1[["varresult"]][["GBPUSD"]][["residuals"]])
# Density plot
# ggdensity(ToothGrowth$len, fill = "lightgray")
# QQ plot
# qqplot(ToothGrowth$len)
stability = stability(fitVARGBP1, type = "OLS-CUSUM")
win.graph(width=20,height=20)
plot(stability)
plot(stability[["stability"]][["GBPUSD"]])
nhor = 48
ForecastGBP.ts = predict(fitVARGBP1, n.ahead=nhor, ci=0.99)
########## * fevd plot ###############################
fevd=fevd(fitVARGBP1, n.ahead=48) #forecast error variance decomposition

win.graph(width=20,height=20)
plot(fevd, col=2:15)
grid(NULL, NULL) #ny = 10, nx= 48
#legend('topright',legend = c(colnames(ywithnews)),col = 2:13, lwd = 3, xpd = FALSE, horiz = TRUE, inset = c(2, 1)) #cex = 1, seg.len=1, bty = 'n', 
fevd1=fevd$GBPUSD
#write.xlsx(data.frame(fevd))
#shortcolnames=c('GBPUSD','Inflation US','Inflation UK','Interest Rate US','Interest Rate UK','Economic Activity US','Economic Activity UK','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
#legend_order <- matrix(2:15,ncol=1,byrow = TRUE)
#legend_order <- c("#005EB8", "#0091DA", "#483698", "#470A68", "#6D2077", "#00A3A1", "#009A44", "#EAAA00", "#F68D2E", "#BC204B", "#333333", "#EAAA00", "#F68D2E", "#BC204B")
dev.off()


legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('fevd_GBPUSD.pdf')
#par(mar=c(0,4,5,4),xpd=FALSE)
#ts.plot(fevd$`GBPUSD`, col=cols[legend_order], ylab= "percentage", main= "GBPUSD", lwd=2) #cex.main=2
#ts.plot(fevd$`GBPUSD`, col=c(legend_order), ylab= "percentage", main= "GBPUSD", lwd=2) #cex.main=2
ts.plot(fevd$`GBPUSD`, col=c(legend_order), ylab= "percentage", main= "GBPUSD", lwd=2) #cex.main=2
#ts.plot(fevd$`GBPUSD`, col=c(cols[2:13], 'darkseegreen4', 'coral4'), ylab= "percentage", main= "GBPUSD", lwd=2) #cex.main=2
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(legend_order),ncol=1, inset = c(0, 0))
#legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(cols[2:13]),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()

# legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", '#FFFF00') #808000 olive
# pdf('fevd_GBPUSD_AllNews.pdf')
# z=cbind(fevd$GBPUSD[,1], fevd$GBPUSD[,2], fevd$GBPUSD[,3], fevd$GBPUSD[,4], fevd$GBPUSD[,5], fevd$GBPUSD[,6], fevd$GBPUSD[,7], fevd$GBPUSD[,8], fevd$GBPUSD[,9], fevd$GBPUSD[,10]+fevd$GBPUSD[,11]+fevd$GBPUSD[,12]+fevd$GBPUSD[,13]+fevd$GBPUSD[,14])
# ts.plot(z*100, ylab= "Contribution (%)", main= "FEVD for USD/GBP", col=c(legend_order), lwd=2) #cex.main=2
# sc=c('GBPUSD','Inflation US','Inflation UK','Interest Rate US','Interest Rate UK','Economic Activity US','Economic Activity UK','M2 US','M2 UK','All News')
# legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order)) #col=c(legend_order),ncol=1, inset = c(0, 0))
# grid() #ny = 10, nx= 48
# abline(h = c(10, 30, 50, 70, 90), col = "gray",lty = 3)
# abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
# dev.off()

### * GBPUSD  ###########
fevd=fevd(fitVARGBP1, n.ahead=48) #forecast error variance decomposition
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('fevd_GBPUSD.pdf')
ts.plot(fevd$`GBPUSD`, col=c(legend_order), ylab= "percentage", main= "GBP/USD", lwd=2) #cex.main=2
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()

legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", '#FFFF00') #808000 olive
pdf('fevd_GBPUSD_AllNews.pdf')
z=cbind(fevd$GBPUSD[,1], fevd$GBPUSD[,2], fevd$GBPUSD[,3], fevd$GBPUSD[,4], fevd$GBPUSD[,5], fevd$GBPUSD[,6], fevd$GBPUSD[,7], fevd$GBPUSD[,8], fevd$GBPUSD[,9], fevd$GBPUSD[,10]+fevd$GBPUSD[,11]+fevd$GBPUSD[,12]+fevd$GBPUSD[,13]+fevd$GBPUSD[,14])
ts.plot(z*100, ylab= "Contribution (%) to GBP/USD", main= "FEVD for GBP/USD", col=c(legend_order), lwd=2) #cex.main=2
sc=c('GBPUSD','Inflation US','Inflation UK','Interest Rate US','Interest Rate UK','Economic Activity US','Economic Activity UK','M2 US','M2 UK','All News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order)) #col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(10, 30, 50, 70, 90), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()

####### EUR ####
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('fevd_USDEUR.pdf')
ts.plot(fevd$`USDEUR`, col=c(legend_order), ylab= "percentage", main= "USDEUR", lwd=2) #cex.main=2
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()


legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", '#FFFF00') #808000 olive
pdf('fevd_USDEUR_AllNews.pdf')
z=cbind(fevd$USDEUR[,1], fevd$USDEUR[,2], fevd$USDEUR[,3], fevd$USDEUR[,4], fevd$USDEUR[,5], fevd$USDEUR[,6], fevd$USDEUR[,7], fevd$USDEUR[,8], fevd$USDEUR[,9], fevd$USDEUR[,10]+fevd$USDEUR[,11]+fevd$USDEUR[,12]+fevd$USDEUR[,13]+fevd$USDEUR[,14])
ts.plot(z*100, ylab= "Contribution (%)", main= "FEVD for USD/EUR", col=c(legend_order), lwd=2) #cex.main=2
sc=c('USDEUR','Inflation US','Inflation EU','Interest Rate US','Interest Rate EU','Economic Activity US','Economic Activity EU','M2 US','M2 EU','All News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order)) #col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(10, 30, 50, 70, 90), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()


##

win.graph(width=20,height=20)
pdf('fevd_Inflation_US.pdf')
ts.plot(fevd$`Inflation_US`, col=cols[legend_order], ylab= "percentage", main= "Inflation US", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_Inflation_UK.pdf')
ts.plot(fevd$`Inflation_UK`, col=cols[legend_order], ylab= "percentage", main= "Inflation UK", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid()
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_MMR_US.pdf')
ts.plot(fevd$`Money_Market_Rate_US`, col=cols[legend_order], ylab= "percentage", main= "Interest Rate US", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(c(0, 0))) #c(-0.2, 0)
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_MMR_UK.pdf')
ts.plot(fevd$`Money_Market_Rate_UK`, col=cols[legend_order], ylab= "percentage", main= "Interest Rate UK", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_EA_US.pdf')
ts.plot(fevd$`Economic_Activity_US`, col=cols[legend_order], ylab= "percentage", main= "Economic Activity US", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid()
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_EA_UK.pdf')
ts.plot(fevd$`Economic_Activity_UK`, col=cols[legend_order], ylab= "percentage", main= "Economic Activity UK", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_SMN.pdf')
ts.plot(fevd[["Entropy_Stock_Market"]], col=cols[legend_order], ylab= "percentage", main= "Stock Market News", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid()
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_EDN.pdf')
ts.plot(fevd$`Entropy_Economic_Development`, col=cols[legend_order], ylab= "percentage", main= "Economic Development News", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid()
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_FED.pdf')
ts.plot(fevd$`Entropy_FED`, col=cols[legend_order], ylab= "percentage", main= "FED News", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid()
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_MFN.pdf')
ts.plot(fevd$`Entropy_Micro_Finance`, col=cols[legend_order], ylab= "percentage", main= "Micro Finance News", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid()
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
pdf('fevd_ITN.pdf')
ts.plot(fevd$`Entropy_International_Trade`, col=cols[legend_order], ylab= "percentage", main= "International Trade News", lwd=2)
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=cols[legend_order],ncol=1, inset = c(0, 0))
grid()
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()
#dev.off()

###my plot
dev.new(width=10, height=10)
#par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
#legend('bottom',legend = c(colnames(ywithnews)),col = 2:13,  lwd = 5, xpd = FALSE, horiz = TRUE, cex = 0.8, seg.len=0.2, bty = 'n') #cex = 1, seg.len=1, bty = 'n', 

#ts.plot(fevd$`Inflation_US`, col=1:13, ylab= "percentage")#col = "red"
legend_order <- matrix(1:12,ncol=1,byrow = TRUE)
ts.plot(fevd$`GBPUSD`, col=cols[legend_order], ylab= "percentage", lwd= 2.0)#col = "red"col=1:12,
ts.plot(fevd$`USDEUR`, col=cols[legend_order], ylab= "percentage", lwd= 2.0)#col = "red"col=1:12,
legend("topright", c(shortcolnames)[legend_order],
       lwd=2, bty="n", xpd=FALSE,
       col=cols[legend_order],
       ncol=2)
abline(h = 0, col = "gray", lwd = 2, lty = 4)
grid() #ny = 50
#dev.off() 
####### fevd FX responses to News shocks ###########

# for (i in fevd){
#   y = names(fevd)[1]
#   pdf('10fx.pdf')
#   plot(i[,10], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
#   abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
#   abline(h = 0, col = "black", lwd = 2, lty = 4)
#   abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
#   grid()
#   dev.off()
# }

# y = names(fevd)[1]
# pdf('Allfx.pdf')
# plot(fevd$GBPUSD[,10]+fevd$GBPUSD[,11]+fevd$GBPUSD[,12]+fevd$GBPUSD[,13]+fevd$GBPUSD[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
# abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
# abline(h = 0, col = "black", lwd = 2, lty = 4)
# abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
# grid()
# dev.off()

legend_order_news <- c("#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('Allfx_UK_together.pdf')
z=cbind(fevd$GBPUSD[,10], fevd$GBPUSD[,11], fevd$GBPUSD[,12], fevd$GBPUSD[,13], fevd$GBPUSD[,14])
ts.plot(z*100, type='l', xlab="time", ylab = "Contribution (%) to USD/GBP", col=c(legend_order_news), main = "FEVD of USD/GBP",lwd=2, ylim= c(0,5.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
sc=c('Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order_news)) #col=c(legend_order),ncol=1, inset = c(0, 0))
#abline(h = c(1, 3, 5, 7, 9), col = "gray", lty = 3)
#abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
abline(h = c(0.2, 0.4, 0.6, 0.8, 1.2, 1.4, 1.6, 1.8, 2.2, 2.4, 2.6, 2.8, 3.2, 3.4, 3.6, 3.8, 4.2, 4.4, 4.6, 4.8), col = "gray", lty = 3)
abline(v =c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
grid()
dev.off()

y = names(fevd)[1]
pdf('10fx.pdf')
plot(fevd$GBPUSD[,10], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()


pdf('11fx.pdf')
plot(fevd$GBPUSD[,11], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[11]]), main = "Economic Development News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('12fx.pdf')
plot(fevd$GBPUSD[,12], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[12]]), main = "FED News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('13fx.pdf')
plot(fevd$GBPUSD[,13], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[13]]), main = "Micro Finance News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('14fx.pdf')
plot(fevd$GBPUSD[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[14]]), main = "International Trade News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()


# # * GBPUSD#####################################################################
legend_order_news <- c("#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('Allfx_UK_GBPUSD_together.pdf')
z=cbind(fevd$GBPUSD[,10], fevd$GBPUSD[,11], fevd$GBPUSD[,12], fevd$GBPUSD[,13], fevd$GBPUSD[,14])
ts.plot(z*100, type='l', xlab="time", ylab = "Contribution (%) to GBP/USD", col=c(legend_order_news), main = "FEVD of GBP/USD",lwd=2, ylim= c(0,6)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
sc=c('Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order_news)) #col=c(legend_order),ncol=1, inset = c(0, 0))
#abline(h = c(1, 3, 5, 7, 9), col = "gray", lty = 3)
#abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
abline(h = c(0.2, 0.4, 0.6, 0.8, 1.2, 1.4, 1.6, 1.8, 2.2, 2.4, 2.6, 2.8, 3.2, 3.4, 3.6, 3.8, 4.2, 4.4, 4.6, 4.8, 5.2, 5.4, 5.6, 5.8), col = "gray", lty = 3)
abline(v =c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
grid()
dev.off()
#GBPUSD
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
y = names(fevd)[1]
pdf('10fx_GBPUSD.pdf')
plot(100*fevd$GBPUSD[,10], type='l', xlab="time", ylab = "Contribution (%) to GBP/USD", col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()
#GBPUSD
pdf('11fx_GBPUSD.pdf')
plot(100*fevd$GBPUSD[,11], type='l', xlab="time", ylab = "Contribution (%) to GBP/USD", col=c(legend_order[[11]]), main = "Economic Development News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()
#GBPUSD
pdf('12fx_GBPUSD.pdf')
plot(100*fevd$GBPUSD[,12], type='l', xlab="time", ylab = "Contribution (%) to GBP/USD", col=c(legend_order[[12]]), main = "FED News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()
#GBPUSD
pdf('13fx_GBPUSD.pdf')
plot(100*fevd$GBPUSD[,13], type='l', xlab="time", ylab = "Contribution (%) to GBP/USD", col=c(legend_order[[13]]), main = "Micro Finance News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()
#GBPUSD
pdf('14fx_GBPUSD.pdf')
plot(100*fevd$GBPUSD[,14], type='l', xlab="time", ylab = "Contribution (%) to GBP/USD", col=c(legend_order[[14]]), main = "International Trade News", ylim = c(0,5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(1, 3, 5), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()









# for (i in fevd){
#   y = names(fevd)[1]
#   pdf('11fx.pdf')
#   plot(i[,11], type='l', xlab="time", ylab = paste(y, sep = ""), c(legend_order[[11]]), main = "Economic Developmen News", ylim = c(0,0.1)) #, ylim = c(0,0.1)
#   abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
#   #abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
#   abline(h = 0, col = "black", lwd = 2, lty = 4)
#   abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
#   grid()
#   dev.off()
# }
# 
# 
# for (i in fevd){
#   y = names(fevd)[1]
#   pdf('12fx.pdf')
#   plot(i[,12], type='l', xlab="time", ylab = paste(y, sep = ""), c(legend_order[[12]]), main = "FED News", ylim = c(0,0.1)) #, ylim = c(0,0.1)
#   abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
#   abline(h = 0, col = "black", lwd = 2, lty = 4)
#   abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
#   grid()
#   dev.off()
# }
# 
# 
# for (i in fevd){
#   y = names(fevd)[1]
#   pdf('13fx.pdf')
#   plot(i[,13], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[13]]), main = "Microeconomic Finance News", ylim = c(0,0.18)) #, ylim = c(0,0.1)
#   abline(h = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18), col = "gray", lty = 3)
#   abline(h = 0, col = "black", lwd = 2, lty = 4)
#   abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
#   grid()
#   dev.off()
# }
# 
# 
# for (i in fevd){
#   y = names(fevd)[1]
#   pdf('14fx.pdf')
#   plot(i[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[14]]), main = "International Trade News", ylim = c(0,0.1)) #, ylim = c(0,0.1)
#   abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
#   abline(h = 0, col = "black", lwd = 2, lty = 4)
#   abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
#   grid()
#   dev.off()
# }

####
# for (i in fevd){
#   y = names(fevd)[1]
#   pdf('8fxreal.pdf')
#   plot(exp(i[,8]), type='l', xlab="time", ylab = paste(y, sep = ""), col=cols[legend_order[8]], main = "", ylim = c(0,0.1)) #, ylim = c(0,0.1)
#   abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
#   abline(h = 0, col = "black", lwd = 2, lty = 4)
#   abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
#   grid()
#   dev.off()
# }
# 
# for (i in fevd){
#   y = names(fevd)[1]
#   pdf('9fxreal.pdf')
#   plot(exp(i[,9]), type='l', xlab="time", ylab = paste(y, sep = ""), col=cols[legend_order[9]], main = "", ylim = c(0,0.1)) #, ylim = c(0,0.1)
#   abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
#   abline(h = 0, col = "black", lwd = 2, lty = 4)
#   abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
#   grid()
#   dev.off()
# }
# 
# for (i in fevd){
#   y = names(fevd)[1]
#   pdf('9fxreal.pdf')
#   plot(1/(i[,9]), type='l', xlab="time", ylab = paste(y, sep = ""), col=cols[legend_order[9]], main = "") #, ylim = c(0,0.1)
#   abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
#   abline(h = 0, col = "black", lwd = 2, lty = 4)
#   abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
#   grid()
#   dev.off()
# }
# 

####### fevd FX responses to News shocks EU ###########
y = names(fevd)[1]
pdf('Allfx_EU.pdf')
plot(fevd$USDEUR[,10]+fevd$USDEUR[,11]+fevd$USDEUR[,12]+fevd$USDEUR[,13]+fevd$USDEUR[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()


legend_order_news <- c("#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('Allfx_EU_together.pdf')
z=cbind(fevd$USDEUR[,10], fevd$USDEUR[,11], fevd$USDEUR[,12], fevd$USDEUR[,13], fevd$USDEUR[,14])
ts.plot(z*100, type='l', xlab="time", ylab = "Contribution (%) to USD/EUR", col=c(legend_order_news), main = "FEVD of USD/EUR",lwd=2) #,col=cols[legend_order[10]], ylim = c(0,0.1)
sc=c('Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order_news)) #col=c(legend_order),ncol=1, inset = c(0, 0))
#abline(h = c(1, 3, 5, 7, 9), col = "gray", lty = 3)
#abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
abline(h = c(0.2, 0.4, 0.6, 0.8, 1.2, 1.4, 1.6, 1.8, 2.2, 2.4, 2.6, 2.8, 3.2, 3.4, 3.6, 3.8, 4.2, 4.4, 4.6, 4.8), col = "gray", lty = 3) 
abline(v =c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
grid()
dev.off()


###

y = names(fevd)[1]
pdf('10fx_EU.pdf')
plot(fevd$USDEUR[,10], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[10]]), main = "Stock Market News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()


pdf('11fx_EU.pdf')
plot(fevd$USDEUR[,11], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[11]]), main = "Economic Development News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('12fx_EU.pdf')
plot(fevd$USDEUR[,12], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[12]]), main = "FED News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('13fx_EU.pdf')
plot(fevd$USDEUR[,13], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[13]]), main = "Micro Finance News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()

pdf('14fx_EU.pdf')
plot(fevd$USDEUR[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=c(legend_order[[14]]), main = "International Trade News", ylim = c(0,0.1)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
abline(h = c(0.01, 0.03, 0.05, 0.07, 0.09), col = "gray", lty = 3)
abline(h = 0, col = "black", lwd = 2, lty = 4)
abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
grid()
dev.off()


################fevd for each news shock #############
par(mfrow=c(3,4))
j=0
for (i in fevd){
  j= j+1
  #print(names(fevd)[j])
  y = names(fevd)[j]
  #print(i[,8])
  pdf('10.pdf')
  plot(i[,10], type='l', xlab="time", ylab = paste(y, sep = ""), col=cols[legend_order[10]], main = "", ylim = c(0,0.1)) #, ylim = c(0,0.1)
  abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
  abline(h = 0, col = "black", lwd = 2, lty = 4)
  abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
  grid()
  #plot(fevd$i[,8])
  dev.off()
}

par(mfrow=c(3,4))
j=0
for (i in fevd){
  j= j+1
  #print(names(fevd)[j])
  y = names(fevd)[j]
  #print(i[,8])
  pdf('11.pdf')
  plot(i[,11], type='l', xlab="time", ylab = paste(y, sep = ""), col=cols[legend_order[11]], main = "", ylim = c(0,0.1)) #, ylim = c(0,0.1)
  abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
  abline(h = 0, col = "black", lwd = 2, lty = 4)
  abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
  grid()
  #plot(fevd$i[,8])
  dev.off()
}

par(mfrow=c(3,4))
j=0
for (i in fevd){
  j= j+1
  #print(names(fevd)[j])
  y = names(fevd)[j]
  #print(i[,8])
  pdf('12.pdf')
  plot(i[,12], type='l', xlab="time", ylab = paste(y, sep = ""), col=cols[legend_order[12]], main = "", ylim = c(0,0.1)) #, ylim = c(0,0.1)
  abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
  abline(h = 0, col = "black", lwd = 2, lty = 4)
  abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
  grid()
  #plot(fevd$i[,8])
  dev.off()
}

par(mfrow=c(3,4))
j=0
for (i in fevd){
  j= j+1
  #print(names(fevd)[j])
  y = names(fevd)[j]
  #print(i[,8])
  pdf('13.pdf')
  plot(i[,13], type='l', xlab="time", ylab = paste(y, sep = ""), col=cols[legend_order[13]], main = "", ylim = c(0,0.1)) #, ylim = c(0,0.1)
  abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
  abline(h = 0, col = "black", lwd = 2, lty = 4)
  abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
  grid()
  #plot(fevd$i[,8])
  dev.off()
}

par(mfrow=c(3,4))
j=0
for (i in fevd){
  j= j+1
  #print(names(fevd)[j])
  y = names(fevd)[j]
  #print(i[,8])
  pdf('14.pdf')
  plot(i[,14], type='l', xlab="time", ylab = paste(y, sep = ""), col=cols[legend_order[14]], main = "", ylim = c(0,0.1)) #, ylim = c(0,0.1)
  abline(h = 0.01, col = "gray", lwd = 2, lty = 4)
  abline(h = 0, col = "black", lwd = 2, lty = 4)
  abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
  grid()
  #plot(fevd$i[,8])
  dev.off()
}

#
#
#
########## bgvar#####
library(BGVAR)
model.1<-bgvar(Data=ywithnews,
               W=W.trade0012,
               draws=100,
               burnin=100,
               plag=1,
               prior="NG",
               hyperpara=NULL, 
               SV=TRUE,
               thin=1,
               trend=TRUE,
               hold.out=0,
               eigen=1
)
############################ Fit SVAR Model ##############################################
fitVARGBP1 = BQ(fitVARGBP1)
#summary(SVAR)
#SVAR$B
summary(fitVARGBP1)[["Sigma.U"]]
shapiro.test(fitVARGBP1[["Sigma.U"]])
#ForecastGBP.ts = predict(fitVARGBP1, n.ahead=nhor, ci=0.99)
########################### Fit handwritten SVAR to the data with news ##############################################
newsamat <- diag(12)
diag(newsamat) <- NA
newsamat[lower.tri(amat)] <- NA
newsamat[4,3]=0
newsamat[5,2]=0
newsamat[5,4]=0
newsamat[6,3]=0
newsamat[6,5]=0
newsamat[7,2]=0
newsamat[7,4]=0
newsamat[7,6]=0

newsamat[7,8]=0
newsamat[7,9]=0
newsamat[7,10]=0
newsamat[7,11]=0
newsamat[7,12]=0
newsbmat =diag(12)

SVAR <- SVAR(fitVARGBP1, estmethod = "scoring", Amat = newsamat, Bmat = NULL, hessian = TRUE, method="BFGS")
SVAR_residuals <- resid(SVAR)
summary(SVAR)
# Inverse Covariance matrix of reduced form residuals
#solve(summary(SVAR)$corres)
summary(SVAR)[["Sigma.U"]]
#shapiro.test(SVAR[["Sigma.U"]])
############################ Fit VECM Model ##############################################
ywithnewsmin=cbind(Monthly_USD_to_UK.ts, Inflation_US.ts, Inflation_UK.ts, Money_Market_Rate_US.ts, Money_Market_Rate_UK.ts, EA_US.ts,EA_UK.ts, monthlyentrophy_Stock_Market.ts, monthlyentrophy_Economic_Development.ts, monthlyentrophy_CentrlBank.ts)
jotest=ca.jo(ywithnewsmin, type="trace", K=6, ecdet="none", spec="transitory")  #longrun
summary(jotest)

#VARselect(ywithnews, type= "both", lag.max = 10)
library(ggplot2)
library(tsDyn)
#fitVARGBP1 <- VECM(ywithnews, lag =13, r=5, estim = "ML") #"2OLS" #lag=1 lag2 4
fitVARGBP1 <- ca.jo(ywithnews, type="trace", K=4, ecdet="none", spec="transitory") #lag K=2 K=5 
fitVARGBP1 <-vec2var(fitVARGBP1, r=4) # r=K
#summary(fitVARGBP1)


## Fit VECM model
#library(urca)
vecm_model <- ca.jo(ywithnews, type = "trace", K = 6, ecdet = "none", spec = "longrun") #transitory
## Summary of the VECM model
summary(vecm_model)
## Convert 'ca.jo' object to 'vec2var' object
vec2var_model <- vec2var(vecm_model)
fitVARGBP1 <- vec2var_model
#summary(fitVARGBP1)
#no autocorr for 26 lags for 4) 
summary(vec2var_model)
## Calculate FEVD
fevd <- fevd(vec2var_model, n.ahead=48)

par(mar = c(5, 4, 4, 2) + 0.1)
win.graph(width=20,height=20)
shortcolnames=c('GBPUSD','Inflation US','Inflation UK','Interest Rate US','Interest Rate UK','Economic Activity US','Economic Activity UK','M2 US','M2 UK','Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News', 'EPU_US', 'EPU_UK') #
legend_order <- c("#00338D", "#005EB8", "#0091DA", "#483698", "#470A68", "#009A44","#43B02A", "#765341", "#C6007E", "#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333", "#EAAA00", "#F68D2E")
pdf('fevd_GBPUSD.pdf')
ts.plot(fevd$`GBPUSD`, col=c(legend_order), ylab= "percentage", main= "GBP/USD", lwd=2) #cex.main=2
legend("topright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(legend_order),ncol=1, inset = c(0, 0))
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 40, 38, 42, 44, 46, 48), col = "gray",lty = 3)
dev.off()

barplot(t(fevd$`GBPUSD`), col=c(legend_order), ylab= "Percentage", xlab = "Forecast Horizon", main= "GBP/USD", lwd=2, names.arg = rep(1:48, each = ncol(t(fevd$`GBPUSD`)))) #cex.main=2 #, ylim = c(0.84, 1)
legend("bottomright",shortcolnames,lwd=2, bty="n", xpd=FALSE,col=c(legend_order),ncol=1, inset = c(0, 0), text.col = "white")
grid() #ny = 10, nx= 48
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col = "gray",lty = 3)
abline(v = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 40, 38, 42, 44, 46, 48), col = "gray",lty = 3) 

#par(mar = c(5, 5, 2, 2))  # Set the margins to c(bottom, left, top, right)
#plot(fevd, col = legend_order)
#par(mar = c(5, 5, 2, 5), plt = c(0.1, 0.9, 0.1, 0.9))  # Set the margins and plot size
#plot(fevd_result$GBPUSD, col = colors, main = "FEVD for GBPUSD", xlab = "Forecast Horizon", ylab = "Percentage")
par(mar = c(5, 4, 4, 2) + 0.1)
legend_order_news <- c("#6D2077","#00A3A1", "#ff9933", "#BC204B", "#333333")
pdf('Allfx_UK_together_VECM.pdf')
z=cbind(fevd$GBPUSD[,10], fevd$GBPUSD[,11], fevd$GBPUSD[,12], fevd$GBPUSD[,13], fevd$GBPUSD[,14])
ts.plot(z*100, type='l', xlab="time", ylab = "Contribution (%) to USD/GBP", col=c(legend_order_news), main = "FEVD of USD/GBP",lwd=2, ylim= c(0,0.5)) #,col=cols[legend_order[10]], ylim = c(0,0.1)
sc=c('Stock Market News','Economic Development News','FED News','Micro Finance News','International Trade News')
legend("topright",sc,lwd=2, bty="n", xpd=FALSE, col=c(legend_order_news)) #col=c(legend_order),ncol=1, inset = c(0, 0))
#abline(h = c(1, 3, 5, 7, 9), col = "gray", lty = 3)
#abline(v = c(5, 15, 25, 35, 45), col = "gray",lty = 3)
abline(h = c(0.2, 0.4, 0.6, 0.8, 1.2, 1.4, 1.6, 1.8, 2.2, 2.4, 2.6, 2.8, 3.2, 3.4, 3.6, 3.8, 4.2, 4.4, 4.6, 4.8), col = "gray", lty = 3)
abline(v =c(2, 4, 6, 8, 12, 14, 16, 18, 22, 24, 26, 28, 32, 34, 36, 38, 42, 44, 46, 48), col = "gray",lty = 3)
grid()
dev.off()

############################ Check VECM Model ##############################################
resid_VECM_US_UK = residuals(fitVARGBP1)
#ts.plot(resid_VECM_US_UK)
jarque.bera.test(resid_VECM_US_UK[, 1]) 
shapiro.test(resid_VECM_US_UK[, 1]) # p>0.05 normal!
acf(resid_VECM_US_UK[, 1], type="correlation")
#install.packages("EnvStats")
serialCorrelationTest(resid_VECM_US_UK[, 1])
serial.test(fitVARGBP1, type="PT.asymptotic") # p-value<0.05 -> autocorrelated

# Forecast from ca.jo() using vec2var()
#----------------
nhor = 120
pred_vec2var_ca.jo = predict(fitVARGBP1, n.ahead=nhor, ci=0.99)
# x11(); par(mai=rep(0.4, 4)); plot(pred_vec2var_ca.jo)
# x11(); par(mai=rep(0.4, 4)); fanchart(pred_vec2var_ca.jo)
# m.pred_vec2var_ca.jo = cbind(
#   pred_vec2var_ca.jo$fcst$lrm1[,1], 
#   pred_vec2var_ca.jo$fcst$lny[,1],
#   pred_vec2var_ca.jo$fcst$lnmr[,1], 
#   pred_vec2var_ca.jo$fcst$difp[,1])
# colnames(m.pred_vec2var_ca.jo) = colnames(ywithnews)
# m.pred_vec2var_ca.jo
ForecastGBP.ts =pred_vec2var_ca.jo
ForecastGBP.ts<-ts(pred_vec2var_ca.jo[["fcst"]][["GBPUSD"]][,1],start=c(2017, 10), end=c(2023, 10), frequency=12) #pred_vec2var_ca.jo$fcst$GBPUSD

### compare to forecast ###########
windows()
Monthly_USD_to_UK.2017.21.ts <-ts(UStoGBP2017.21$Monthly_USD_to_UK, start = c(2017, 10), frequency=12)
#Monthly_USD_to_UK.2017.21.ts <- SMA(Monthly_USD_to_UK.2017.21.ts,n=12) #smoothing using a simple moving average to estimate trend component
ts.plot(Monthly_USD_to_UK.2017.21.ts, ForecastGBP.ts, lty = c(1,3), col=c("blue",2)) #ForecastGBP.ts[, 1]
legend("topright", c("USD/GBP", "Forecast of USD/GBP"), lty = 1, col = c("blue",2))
title("USD/GBP VS Forecast of USD/GBP")
#dev.off()

############################ ARIMA ##############################################
library(marima)
arimamodel = define.model(kvar = 12, ar = c(1:8, 15), ma = c(1, 12), rem.var = 1, reg.var = 2:12,
             no.dep = NULL, print = 0, ar.fill = NULL, ar.rem = NULL,
             ma.fill = NULL, ma.rem = NULL, indep = NULL)
Marima = marima(DATA = ywithnews, ar.pattern = arimamodel$ar.pattern, ma.pattern =arimamodel$ma.pattern, max.iter = 50, penalty = 0, weight = 0.33, Plot = "none", Check = FALSE) #means = 1,
nstart <- 120
nstep <- 90
ForecastGBP <- arma.forecast(series=ts(ywithnews), marima=Marima,
                           nstart=nstart, nstep=nstep)
ForecastGBP.ts <- ts(ForecastGBP[["fcst"]][["GBPUSD"]][,1],start=c(2017, 10), end=c(2023, 10), frequency=12)
########obtain system response to news ############################
set.seed(5)
par(mfrow=c(2,2))
plot(irf1)
plot(irf1inflation)
plot(irf1EA, main = "plot 3")
plot(irf1MMR, main = "plot 4")

################################# Obtain IRFs of GBPUSD############################
##IRFs of GBPUSD
# 1)Stock Market, 2)Economic Development, 3)FED, 4) Micro Finance, 5) International Trade

pdf("GIRF from Stock Market News to GBPUSD.pdf",width=6,height=4,paper='special')
irf1 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 1000)
plot(irf1, main ="Generalized Impulse Response from Stock Market News", pch=25, col="red")
dev.off()

#plotIrf(irf1, name = NULL, ylab = NULL, alpha = 0.3, n.ahead = NULL, filename = NULL, width = 10, height = 6)


pdf("GIRF from Economic Development News to GBPUSD.pdf",width=6,height=4,paper='special')
irf2 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 1000)
plot(irf2, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to GBPUSD.pdf",width=6,height=4,paper='special')
irf3 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 2500)
plot(irf3, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to GBPUSD.pdf",width=6,height=4,paper='special')
irf4 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from International Trade News to GBPUSD.pdf",width=6,height=4,paper='special')
irf5 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()
################################# * Obtain  NEW IRFs of GBPUSD ############################
#### irf1    ###########################
pdf("GIRF from Stock Market News to GBPUSD.pdf",width=6,height=4,paper='special')
irf1 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 1000)
plot(irf1$irf$Entropy_Stock_Market, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nStock Market News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/GBP", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf1$Upper$Entropy_Stock_Market), length(irf1$Lower$Entropy_Stock_Market):1),
  c(irf1$Upper$Entropy_Stock_Market, rev(irf1$Lower$Entropy_Stock_Market)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf1$irf$Entropy_Stock_Market, col = "Darkblue", lwd=2) #darkgeen
lines(irf1$Upper$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf1$Lower$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
#### irf 2 ###########################
pdf("GIRF from Economic Development News to GBPUSD.pdf",width=6,height=4,paper='special')
irf2 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2$irf$Entropy_Economic_Development, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", ylim = c(-.026, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nEconomic Development News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf2$Upper$Entropy_Economic_Development), length(irf2$Lower$Entropy_Economic_Development):1),
  c(irf2$Upper$Entropy_Economic_Development, rev(irf2$Lower$Entropy_Economic_Development)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf2$irf$Entropy_Economic_Development, col = "Darkblue", lwd=2) #darkgeen
lines(irf2$Upper$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf2$Lower$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#### irf 3   ###########################
##
pdf("GIRF from FED News to GBPUSD.pdf",width=6,height=4,paper='special')
irf3 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3$irf$Entropy_FED, type="l", col="Darkblue", ylab = "Response of USD/USD", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nFED News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf3$Upper$Entropy_FED), length(irf3$Lower$Entropy_FED):1),
  c(irf3$Upper$Entropy_FED, rev(irf3$Lower$Entropy_FED)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf3$irf$Entropy_FED, col = "Darkblue", lwd=2) #darkgeen
lines(irf3$Upper$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf3$Lower$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#### irf 4   ###########################
##
pdf("GIRF from Microeconomic News to GBPUSD.pdf",width=6,height=4,paper='special')
irf4 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf4$irf$Entropy_Micro_Finance, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nMicroeconomic News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf4$Upper$Entropy_Micro_Finance), length(irf4$Lower$Entropy_Micro_Finance):1),
  c(irf4$Upper$Entropy_Micro_Finance, rev(irf4$Lower$Entropy_Micro_Finance)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf4$irf$Entropy_Micro_Finance, col = "Darkblue", lwd=2) #darkgeen
lines(irf4$Upper$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf4$Lower$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
### irf 5   ###########################
pdf("GIRF from International Trade News to GBPUSD.pdf",width=6,height=4,paper='special')
irf5 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5$irf$Entropy_International_Trade, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nInternational Trade News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf5$Upper$Entropy_International_Trade), length(irf5$Lower$Entropy_International_Trade):1),
  c(irf5$Upper$Entropy_International_Trade, rev(irf5$Lower$Entropy_International_Trade)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf5$irf$Entropy_International_Trade, col = "Darkblue", lwd=2) #darkgeen
lines(irf5$Upper$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf5$Lower$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
################################ IRF GBPUSD to inflation
pdf("GIRF from Inflation to GBPUSD.pdf",width=6,height=4,paper='special')
irfInflGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Inflation_US", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfInflGBPUSD, main ="Generalized Impulse Response from Inflation", pch=25)
dev.off()

pdf("GIRF from MMR to GBPUSD.pdf",width=6,height=4,paper='special')
irfMMRGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Money_Market_Rate_US", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfMMRGBPUSD, main ="Generalized Impulse Response from Money Market Rate", pch=25)
dev.off()

pdf("GIRF from EA to GBPUSD.pdf",width=6,height=4,paper='special')
irfEAGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Economic_Activity_US", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfEAGBPUSD, main ="Generalized Impulse Response from Economic Activity in USA", pch=25)
dev.off()

pdf("all responses to stock.pdf",width=6,height=4,paper='special')
par(mfrow=c(3,2))
plot1 <- plot(irf1$irf$Entropy_Stock_Market, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", main ="Generalized Impulse Response from \nStock Market News") # ylim = c(-.003, .003),xlim= c(2, 36)
#plot2=plot(irf2$irf$Entropy_Economic_Development, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", main ="Generalized Impulse Response from \nEconomic Development News") # ylim = c(-.003, .003),xlim= c(2, 3
plot1
################################ NEW IRF GBPUSD to inflation, EA, MMR ################ 
pdf("GIRF from Inflation to GBPUSD.pdf",width=6,height=4,paper='special')
irfInflGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Inflation_US", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 1000)
plot(irfInflGBPUSD$irf$Inflation_US, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nUS Inflation") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/GBP", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irfInflGBPUSD$Upper$Inflation_US), length(irfInflGBPUSD$Lower$Inflation_US):1),
  c(irfInflGBPUSD$Upper$Inflation_US, rev(irfInflGBPUSD$Lower$Inflation_US)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irfInflGBPUSD$irf$Inflation_US, col = "Darkblue", lwd=2) #darkgeen
lines(irfInflGBPUSD$Upper$Inflation_US, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irfInflGBPUSD$Lower$Inflation_US, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
###

pdf("GIRF from MMR to GBPUSD.pdf",width=6,height=4,paper='special')
irfMMRGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Money_Market_Rate_US", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfMMRGBPUSD$irf$Money_Market_Rate_US, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nUS Money Market Rate") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/GBP", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irfMMRGBPUSD$Upper$Money_Market_Rate_US), length(irfMMRGBPUSD$Lower$Money_Market_Rate_US):1),
  c(irfMMRGBPUSD$Upper$Money_Market_Rate_US, rev(irfMMRGBPUSD$Lower$Money_Market_Rate_US)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irfMMRGBPUSD$irf$Money_Market_Rate_US, col = "Darkblue", lwd=2) #darkgeen
lines(irfMMRGBPUSD$Upper$Money_Market_Rate_US, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irfMMRGBPUSD$Lower$Money_Market_Rate_US, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
####
pdf("GIRF from EA to GBPUSD.pdf",width=6,height=4,paper='special')
irfEAGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Economic_Activity_US", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfEAGBPUSD$irf$Economic_Activity_US, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nUS Economic Activity") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/GBP", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irfEAGBPUSD$Upper$Economic_Activity_US), length(irfEAGBPUSD$Lower$Economic_Activity_US):1),
  c(irfEAGBPUSD$Upper$Economic_Activity_US, rev(irfEAGBPUSD$Lower$Economic_Activity_US)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irfEAGBPUSD$irf$Economic_Activity_US, col = "Darkblue", lwd=2) #darkgeen
lines(irfEAGBPUSD$Upper$Economic_Activity_US, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irfEAGBPUSD$Lower$Economic_Activity_US, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#####Obtain IRFs of GBPUSD to UK 
pdf("GIRF from UK Inflation to GBPUSD.pdf",width=6,height=4,paper='special')
irfUKInflGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Inflation_UK", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfUKInflGBPUSD, main ="Generalized Impulse Response from Inflation in UK", pch=25)
dev.off()

pdf("GIRF from UK MMR to GBPUSD.pdf",width=6,height=4,paper='special')
irfUKMMRGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Money_Market_Rate_UK", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfUKMMRGBPUSD, main ="Generalized Impulse Response from Money Market Rate in UK", pch=25)
dev.off()

pdf("GIRF from UK EA to GBPUSD.pdf",width=6,height=4,paper='special')
irfUKEAGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Economic_Activity_UK", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfUKEAGBPUSD, main ="Generalized Impulse Response from Economic Activity in UK", pch=25)
dev.off()
# library(ggplot2)
# grid.arrange(plot(irf1), plot(irf2), ncol=2,top="Main Title") 

#library("graphics", lib.loc="C:/Program Files/R/R-3.5.1/library")
par(mfrow = c(3, 1))    
plot(irf1, main = "plot 1")
plot(irf2, main = "plot 2")
plot(irf3, main = "plot 3")

################################# Obtain IRFs of Inflation############################

pdf("GIRF from Stock Market News to Inflation.pdf",width=6,height=4,paper='special')
irf1inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1inflation, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to Inflation.pdf",width=6,height=4,paper='special')
irf2inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2inflation, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to Inflation.pdf",width=6,height=4,paper='special')
irf3inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3inflation, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to Inflation.pdf",width=6,height=4,paper='special')
irf4inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4inflation, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from International Trade News to Inflation.pdf",width=6,height=4,paper='special')
irf5inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5inflation, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()
############################### Obtain IRFs of UK Inflation #############################

pdf("GIRF from Stock Market News to UK Inflation.pdf",width=6,height=4,paper='special')
irf1inflationUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Inflation_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1inflationUK, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to UK Inflation.pdf",width=6,height=4,paper='special')
irf2inflationUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Inflation_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2inflationUK, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to UK Inflation.pdf",width=6,height=4,paper='special')
irf3inflationUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Inflation_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3inflationUK, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to UK Inflation.pdf",width=6,height=4,paper='special')
irf4inflationUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Inflation_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4inflationUK, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from International Trade News to UK Inflation.pdf",width=6,height=4,paper='special')
irf5inflationUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Inflation_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5inflationUK, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()
############################### Obtain IRFs of EU Inflation #############################

pdf("GIRF from Stock Market News to EU Inflation.pdf",width=6,height=4,paper='special')
irf1inflationEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Inflation_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1inflationEU, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to UK Inflation.pdf",width=6,height=4,paper='special')
irf2inflationEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Inflation_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2inflationEU, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to EU Inflation.pdf",width=6,height=4,paper='special')
irf3inflationEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Inflation_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3inflationEU, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to EU Inflation.pdf",width=6,height=4,paper='special')
irf4inflationEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Inflation_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4inflationEU, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from International Trade News to EU Inflation.pdf",width=6,height=4,paper='special')
irf5inflationEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Inflation_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5inflationEU, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()

################################# Obtain IRFs of Economic Activity############################
pdf("GIRF from Stock Market News to Economic Activity.pdf",width=6,height=4,paper='special')
irf1EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1EA, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to Economic Activity.pdf",width=6,height=4,paper='special')
irf2EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2EA, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to Economic Activity.pdf",width=6,height=4,paper='special')
irf3EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3EA, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to Economic Activity.pdf",width=6,height=4,paper='special')
irf4EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4EA, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from International Trade News to Economic Activity.pdf",width=6,height=4,paper='special')
irf5EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5EA, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()

pdf("GIRF from FX to Economic Activity.pdf",width=6,height=4,paper='special')
irf1EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "GBPUSD", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1EA, main ="Generalized Impulse Response from FX to Economic Activity", pch=25)
dev.off()
################################# Obtain IRFs of UK Economic Activity############################
pdf("GIRF from Stock Market News to UK Economic Activity.pdf",width=6,height=4,paper='special')
irf1EAUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Economic_Activity_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1EAUK, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to UK Economic Activity.pdf",width=6,height=4,paper='special')
irf2EAUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Economic_Activity_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2EAUK, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to UK Economic Activity.pdf",width=6,height=4,paper='special')
irf3EAUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Economic_Activity_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3EAUK, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to UK Economic Activity.pdf",width=6,height=4,paper='special')
irf4EAUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Economic_Activity_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4EAUK, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from International Trade News to UK Economic Activity.pdf",width=6,height=4,paper='special')
irf5EAUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Economic_Activity_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5EAUK, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()

pdf("GIRF from FX to UK Economic Activity.pdf",width=6,height=4,paper='special')
irf1EAUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "GBPUSD", response = "Economic_Activity_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1EAUK, main ="Generalized Impulse Response from FX to Economic Activity", pch=25)
dev.off()

################################# Obtain IRFs of EU Economic Activity############################
pdf("GIRF from Stock Market News to EU Economic Activity.pdf",width=6,height=4,paper='special')
irf1EAEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Economic_Activity_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1EAEU, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to EU Economic Activity.pdf",width=6,height=4,paper='special')
irf2EAEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Economic_Activity_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2EAEU, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to EU Economic Activity.pdf",width=6,height=4,paper='special')
irf3EAEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Economic_Activity_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3EAEU, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to EU Economic Activity.pdf",width=6,height=4,paper='special')
irf4EAEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Economic_Activity_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4EAEU, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from International Trade News to EU Economic Activity.pdf",width=6,height=4,paper='special')
irf5EAEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Economic_Activity_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5EAEU, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()

pdf("GIRF from FX to EU Economic Activity.pdf",width=6,height=4,paper='special')
irf1EAEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "GBPUSD", response = "Economic_Activity_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1EAEU, main ="Generalized Impulse Response from FX to Economic Activity", pch=25)
dev.off()


################################# Obtain IRFs of Money Market Rate (interest rate)############################

pdf("GIRF from Stock Market News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf1MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1MMR, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf2MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2MMR, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf3MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3MMR, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf4MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4MMR, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from Trade News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf5MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5MMR, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()


################################# Obtain IRFs of UK Money Market Rate (interest rate)############################

pdf("GIRF from Stock Market News to UK Money Market Rate.pdf",width=6,height=4,paper='special')
irf1MMRUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Money_Market_Rate_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1MMRUK, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to UK Money Market Rate.pdf",width=6,height=4,paper='special')
irf2MMRUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Money_Market_Rate_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2MMRUK, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to UK Money Market Rate.pdf",width=6,height=4,paper='special')
irf3MMRUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Money_Market_Rate_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3MMRUK, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to UK Money Market Rate.pdf",width=6,height=4,paper='special')
irf4MMRUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Money_Market_Rate_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4MMRUK, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from Trade News to UK Money Market Rate.pdf",width=6,height=4,paper='special')
irf5MMRUK <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Money_Market_Rate_UK", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5MMRUK, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()

################################# Obtain IRFs of EU Money Market Rate (interest rate)############################

pdf("GIRF from Stock Market News to EU Money Market Rate.pdf",width=6,height=4,paper='special')
irf1MMREU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Money_Market_Rate_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1MMREU, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to EU Money Market Rate.pdf",width=6,height=4,paper='special')
irf2MMREU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Money_Market_Rate_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2MMREU, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to EU Money Market Rate.pdf",width=6,height=4,paper='special')
irf3MMREU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Money_Market_Rate_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3MMREU, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to EU Money Market Rate.pdf",width=6,height=4,paper='special')
irf4MMREU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Money_Market_Rate_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4MMREU, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from Trade News to EU Money Market Rate.pdf",width=6,height=4,paper='special')
irf5MMREU <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Money_Market_Rate_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5MMREU, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()


################################# Obtain NEW IRFs of Inflation###########################
############## irf1inflation ##############
pdf("GIRF from Stock Market News to Inflation.pdf",width=6,height=4,paper='special')
irf1inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1inflation$irf$Entropy_Stock_Market, type="l", col="Darkblue", ylab = "Response of US inflation", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nStock Market News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/GBP", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf1inflation$Upper$Entropy_Stock_Market), length(irf1inflation$Lower$Entropy_Stock_Market):1),
  c(irf1inflation$Upper$Entropy_Stock_Market, rev(irf1inflation$Lower$Entropy_Stock_Market)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf1inflation$irf$Entropy_Stock_Market, col = "Darkblue", lwd=2) #darkgeen
lines(irf1inflation$Upper$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf1inflation$Lower$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
########################### irf2inflation ##############
pdf("GIRF from Economic Development News to Inflation.pdf",width=6,height=4,paper='special')
irf2inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2inflation$irf$Entropy_Economic_Development, type="l", col="Darkblue", ylab = "Response of US inflation", xlab = "Time periods after shock", ylim = c(-.025, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nEconomic Development News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf2inflation$Upper$Entropy_Economic_Development), length(irf2inflation$Lower$Entropy_Economic_Development):1),
  c(irf2inflation$Upper$Entropy_Economic_Development, rev(irf2inflation$Lower$Entropy_Economic_Development)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf2inflation$irf$Entropy_Economic_Development, col = "Darkblue", lwd=2) #darkgeen
lines(irf2inflation$Upper$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf2inflation$Lower$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
###############################irf3inflation ##############

pdf("GIRF from FED News to Inflation.pdf",width=6,height=4,paper='special')
irf3inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3inflation$irf$Entropy_FED, type="l", col="Darkblue", ylab = "Response of US inflation", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nFED News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf3inflation$Upper$Entropy_FED), length(irf3inflation$Lower$Entropy_FED):1),
  c(irf3inflation$Upper$Entropy_FED, rev(irf3inflation$Lower$Entropy_FED)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf3inflation$irf$Entropy_FED, col = "Darkblue", lwd=2) #darkgeen
lines(irf3inflation$Upper$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf3inflation$Lower$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#####################irf4inflation ##############

pdf("GIRF from Microeconomic News to Inflation.pdf",width=6,height=4,paper='special')
irf4inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf4inflation$irf$Entropy_Micro_Finance, type="l", col="Darkblue", ylab = "Response of US inflation", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nMicroeconomic News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf4inflation$Upper$Entropy_Micro_Finance), length(irf4inflation$Lower$Entropy_Micro_Finance):1),
  c(irf4inflation$Upper$Entropy_Micro_Finance, rev(irf4inflation$Lower$Entropy_Micro_Finance)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf4inflation$irf$Entropy_Micro_Finance, col = "Darkblue", lwd=2) #darkgeen
lines(irf4inflation$Upper$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf4inflation$Lower$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

###############irf5inflation ##############

pdf("GIRF from International Trade News to Inflation.pdf",width=6,height=4,paper='special')
irf5inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5inflation$irf$Entropy_International_Trade, type="l", col="Darkblue", ylab = "Response of US inflation", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nInternational Trade News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf5inflation$Upper$Entropy_International_Trade), length(irf5inflation$Lower$Entropy_International_Trade):1),
  c(irf5inflation$Upper$Entropy_International_Trade, rev(irf5inflation$Lower$Entropy_International_Trade)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf5inflation$irf$Entropy_International_Trade, col = "Darkblue", lwd=2) #darkgeen
lines(irf5inflation$Upper$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf5inflation$Lower$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

############## irfGBPUSD to Inflation ##############
pdf("GIRF from GBPUSD to Inflation.pdf",width=6,height=4,paper='special')
irf1inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "GBPUSD", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1inflation$irf$Entropy_Stock_Market, type="l", col="Darkblue", ylab = "Response of US inflation", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \n USD/GBP") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/GBP", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf1inflation$Upper$Entropy_Stock_Market), length(irf1inflation$Lower$Entropy_Stock_Market):1),
  c(irf1inflation$Upper$Entropy_Stock_Market, rev(irf1inflation$Lower$Entropy_Stock_Market)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf1inflation$irf$Entropy_Stock_Market, col = "Darkblue", lwd=2) #darkgeen
lines(irf1inflation$Upper$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf1inflation$Lower$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

################################# Obtain IRFs of Economic Activity############################
pdf("GIRF from Stock Market News to Economic Activity.pdf",width=6,height=4,paper='special')
irf1EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1EA, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to Economic Activity.pdf",width=6,height=4,paper='special')
irf2EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2EA, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to Economic Activity.pdf",width=6,height=4,paper='special')
irf3EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3EA, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to Economic Activity.pdf",width=6,height=4,paper='special')
irf4EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4EA, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from International Trade News to Economic Activity.pdf",width=6,height=4,paper='special')
irf5EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5EA, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()

pdf("GIRF from FX to Economic Activity.pdf",width=6,height=4,paper='special')
irf1EA <- irf(fitVARGBP1, n.ahead = 36, impulse = "GBPUSD", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1EA, main ="Generalized Impulse Response from FX to Economic Activity", pch=25)
dev.off()
################################# Obtain IRFs of Money Market Rate (interest rate)############################

pdf("GIRF from Stock Market News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf1MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1MMR, main ="Generalized Impulse Response from Stock Market News", pch=25)
dev.off()

pdf("GIRF from Economic Development News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf2MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2MMR, main ="Generalized Impulse Response from Economic Development News", pch=25)
dev.off()

pdf("GIRF from FED News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf3MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3MMR, main ="Generalized Impulse Response from FED News", pch=25)
dev.off()

pdf("GIRF from Microeconomic News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf4MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
#png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
plot(irf4MMR, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
dev.off()

pdf("GIRF from Trade News to Money Market Rate.pdf",width=6,height=4,paper='special')
irf5MMR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "Money_Market_Rate_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5MMR, main ="Generalized Impulse Response from International Trade News", pch=25)
dev.off()

############################ compare Models############################ 
VECM <- VECM(ywithnews, lag =6, r=9, estim = "ML", LRinclude = 'trend') #estim ="2OLS" # include ="const" 
summary(rank.test(VECM))
summary(VECM)
#wirte.xlsx(summary(VECM), vecm.xlsx)
#
library("lmtest", lib.loc="~/R/win-library/3.5")
grangertest(Monthly_USD_to_UK.ts[13:224] ~ monthlyentrophy_Stock_Market.ts, order = 1)
grangertest(Monthly_USD_to_UK.ts[13:224] ~ monthlyentrophy_Economic_Development.ts, order = 1)
grangertest(Monthly_USD_to_UK.ts[13:224] ~ monthlyentrophy_CentrlBank.ts, order = 1)
grangertest(Monthly_USD_to_UK.ts[13:224] ~ monthlyentrophy_Micro_Finance.ts, order = 1)
grangertest(Monthly_USD_to_UK.ts[13:224] ~ monthlyentrophy_Export_import.ts, order = 1)
for (i in 1:ncol(ywithnews)) {
  print(colnames(ywithnews)[i]) # y 13:223
  print(summary(ur.df(ywithnews[,i], selectlags = c("AIC"), type="drift")))
  #print(summary(adf.test(y[,i]),nlag = NULL, output = TRUE))
}

for (i in 1:ncol(ywithnews)) {
  print(colnames(ywithnews)[i]) # y 13:223
  print(summary(ur.df(ywithnews[,i], selectlags = c("AIC"), type="none")))
  #print(summary(adf.test(y[,i]),nlag = NULL, output = TRUE))
}



################################# Obtain NEW IRFs of USDEUR############################
##IRFs of USDEUR
# 1)Stock Market, 2)Economic Development, 3)FED, 4) Micro Finance, 5) International Trade
#irf1   ###########################
# pdf("GIRF from Stock Market News to USDEUR.pdf",width=6,height=4,paper='special')
# irf1 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 1000)
# plot(irf1, main ="Generalized Impulse Response from Stock Market News", pch=25, col="red")
# dev.off()
####
pdf("GIRF from Stock Market News to USDEUR.pdf",width=6,height=4,paper='special')
irf1 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 1000)
plot(irf1$irf$Entropy_Stock_Market, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nStock Market News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf1$Upper$Entropy_Stock_Market), length(irf1$Lower$Entropy_Stock_Market):1),
  c(irf1$Upper$Entropy_Stock_Market, rev(irf1$Lower$Entropy_Stock_Market)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf1$irf$Entropy_Stock_Market, col = "Darkblue", lwd=2) #darkgeen
lines(irf1$Upper$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf1$Lower$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#irf2   ###############
# pdf("GIRF from Economic Development News to USDEUR.pdf",width=6,height=4,paper='special')
# irf2 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
# plot(irf2, main ="Generalized Impulse Response from Economic Development News", pch=25)
# dev.off()
####
pdf("GIRF from Economic Development News to USDEUR.pdf",width=6,height=4,paper='special')
irf2 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf2$irf$Entropy_Economic_Development, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", ylim = c(-.024, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nEconomic Development News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf2$Upper$Entropy_Economic_Development), length(irf2$Lower$Entropy_Economic_Development):1),
  c(irf2$Upper$Entropy_Economic_Development, rev(irf2$Lower$Entropy_Economic_Development)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf2$irf$Entropy_Economic_Development, col = "Darkblue", lwd=2) #darkgeen
lines(irf2$Upper$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf2$Lower$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#irf3   ################################
# pdf("GIRF from FED News to USDEUR.pdf",width=6,height=4,paper='special')
# irf3 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
# plot(irf3, main ="Generalized Impulse Response from FED News", pch=25)
# dev.off()
##
pdf("GIRF from FED News to USDEUR.pdf",width=6,height=4,paper='special')
irf3 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf3$irf$Entropy_FED, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nFED News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf3$Upper$Entropy_FED), length(irf3$Lower$Entropy_FED):1),
  c(irf3$Upper$Entropy_FED, rev(irf3$Lower$Entropy_FED)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf3$irf$Entropy_FED, col = "Darkblue", lwd=2) #darkgeen
lines(irf3$Upper$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf3$Lower$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#irf4  #########################################
# pdf("GIRF from Microeconomic News to USDEUR.pdf",width=6,height=4,paper='special')
# irf4 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
# #png(file="C:\Users\Teona\Documents\Generalized_Impulse_Response_from_Microeconomic_Finance_News.png", width=600, height=350)
# plot(irf4, xlab="Periods after shock", main ="Generalized Impulse Response from Microeconomic Finance News", pch=25)
# dev.off()
#  
pdf("GIRF from Microeconomic News to USDEUR.pdf",width=6,height=4,paper='special')
irf4 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf4$irf$Entropy_Micro_Finance, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nMicroeconomic News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf4$Upper$Entropy_Micro_Finance), length(irf4$Lower$Entropy_Micro_Finance):1),
  c(irf4$Upper$Entropy_Micro_Finance, rev(irf4$Lower$Entropy_Micro_Finance)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf4$irf$Entropy_Micro_Finance, col = "Darkblue", lwd=2) #darkgeen
lines(irf4$Upper$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf4$Lower$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#irf5  #########################################

# pdf("GIRF from International Trade News to USDEUR.pdf",width=6,height=4,paper='special')
# irf5 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
# plot(irf5, main ="Generalized Impulse Response from International Trade News", pch=25)
# dev.off()

# 
pdf("GIRF from International Trade News to USDEUR.pdf",width=6,height=4,paper='special')
irf5 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf5$irf$Entropy_International_Trade, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", ylim = c(-.02, .02), xlim= c(2.2, 36), main ="Generalized Impulse Response from \nInternational Trade News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf5$Upper$Entropy_International_Trade), length(irf5$Lower$Entropy_International_Trade):1),
  c(irf5$Upper$Entropy_International_Trade, rev(irf5$Lower$Entropy_International_Trade)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf5$irf$Entropy_International_Trade, col = "Darkblue", lwd=2) #darkgeen
lines(irf5$Upper$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf5$Lower$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

################################# Obtain IRFs of USDEUR Inflation EA and MMR ################################# 
################  IRF from inflation
pdf("GIRF from Inflation to USDEUR.pdf",width=6,height=4,paper='special')
irfInflUSDEUR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Inflation_US", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfInflUSDEUR, main ="Generalized Impulse Response from Inflation", pch=25)
dev.off()

pdf("GIRF from MMR to USDEUR.pdf",width=6,height=4,paper='special')
irfMMRUSDEUR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Money_Market_Rate_US", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfMMRUSDEUR, main ="Generalized Impulse Response from Money Market Rate", pch=25)
dev.off()

pdf("GIRF from EA to USDEUR.pdf",width=6,height=4,paper='special')
irfEAUSDEUR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Economic_Activity_US", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfEAUSDEUR, main ="Generalized Impulse Response from Economic Activity in USA", pch=25)
dev.off()
#####################  Obtain IRFs of USDEUR to EU Inflation, Economic Activity and MMR
pdf("GIRF from EU Inflation to USDEUR.pdf",width=6,height=4,paper='special')
irfEUInflUSDEUR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Inflation_EU", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfEUInflUSDEUR, main ="Generalized Impulse Response from Inflation in EU", pch=25)
dev.off()

pdf("GIRF from EU MMR to USDEUR.pdf",width=6,height=4,paper='special')
irfEUMMRUSDEUR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Money_Market_Rate_EU", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfEUMMRUSDEUR, main ="Generalized Impulse Response from Money Market Rate in EU", pch=25)
dev.off()

pdf("GIRF from EU EA to USDEUR.pdf",width=6,height=4,paper='special')
irfEUEAUSDEUR <- irf(fitVARGBP1, n.ahead = 36, impulse = "Economic_Activity_EU", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irfEUEAUSDEUR, main ="Generalized Impulse Response from Economic Activity in EU", pch=25)
dev.off()
# library(ggplot2)
# grid.arrange(plot(irf1), plot(irf2), ncol=2,top="Main Title") 

#library("graphics", lib.loc="C:/Program Files/R/R-3.5.1/library")
par(mfrow = c(3, 1))    
plot(irf1, main = "plot 1")
plot(irf2, main = "plot 2")
plot(irf3, main = "plot 3")
##################################### inflation, EA to USDEUR
pdf("GIRF from USDEUR to Inflation.pdf",width=6,height=4,paper='special')
irf1inflation <- irf(fitVARGBP1, n.ahead = 36, impulse = "USDEUR", response = "Inflation_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irf1inflation$irf$USDEUR, type="l", col="Darkblue", ylab = "Response of US inflation", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Generalized Impulse Response from \n USD/EUR") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irf1inflation$Upper$USDEUR), length(irf1inflation$Lower$USDEUR):1),
  c(irf1inflation$Upper$USDEUR, rev(irf1inflation$Lower$USDEUR)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irf1inflation$irf$USDEUR, col = "Darkblue", lwd=2) #darkgeen
lines(irf1inflation$Upper$USDEUR, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irf1inflation$Lower$USDEUR, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
###############of US EA to USD/EUR
pdf("GIRF from USDEUR to Economic Activity US.pdf",width=6,height=4,paper='special')
irftoFXEA <- irf(fitVARGBP1, n.ahead = 36, impulse = "USDEUR", response = "Economic_Activity_US", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irftoFXEA$irf$USDEUR, type="l", col="Darkblue", ylab = "Response of US Economic Activity", xlab = "Time periods after shock", ylim = c(-.04, .05), xlim= c(2.2, 36), main ="Generalized Impulse Response from \n USD/EUR") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irftoFXEA$Upper$USDEUR), length(irftoFXEA$Lower$USDEUR):1),
  c(irftoFXEA$Upper$USDEUR, rev(irftoFXEA$Lower$USDEUR)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irftoFXEA$irf$USDEUR, col = "Darkblue", lwd=2) #darkgeen
lines(irftoFXEA$Upper$USDEUR, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irftoFXEA$Lower$USDEUR, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

###############of EU EA to USD/EUR
pdf("GIRF from USDEUR to Economic Activity EU.pdf",width=6,height=4,paper='special')
irftoFXEAEU <- irf(fitVARGBP1, n.ahead = 36, impulse = "USDEUR", response = "Economic_Activity_EU", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(irftoFXEAEU$irf$USDEUR, type="l", col="Darkblue", ylab = "Response of EU Economic Activity", xlab = "Time periods after shock", ylim = c(-.1, .09), xlim= c(2.2, 36), main ="Generalized Impulse Response from \n USD/EUR") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(irftoFXEAEU$Upper$USDEUR), length(irftoFXEAEU$Lower$USDEUR):1),
  c(irftoFXEAEU$Upper$USDEUR, rev(irftoFXEAEU$Lower$USDEUR)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(irftoFXEAEU$irf$USDEUR, col = "Darkblue", lwd=2) #darkgeen
lines(irftoFXEAEU$Upper$USDEUR, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(irftoFXEAEU$Lower$USDEUR, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

##################################  * Obtain  NEW Cumulative GIRFs of GBPUSD ############################
### cirf1    ###########################
pdf("Cumulative GIRF from Stock Market News to GBPUSD.pdf",width=6,height=4,paper='special')
cirf1 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "GBPUSD", cumulative = TRUE, ortho = TRUE, runs = 1000)
plot(cirf1$irf$Entropy_Stock_Market, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Cumulative Generalized Impulse Response from \nStock Market News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/GBP", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf1$Upper$Entropy_Stock_Market), length(cirf1$Lower$Entropy_Stock_Market):1),
  c(cirf1$Upper$Entropy_Stock_Market, rev(cirf1$Lower$Entropy_Stock_Market)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf1$irf$Entropy_Stock_Market, col = "Darkblue", lwd=2) #darkgeen
lines(cirf1$Upper$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf1$Lower$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
#### cirf 2 ###########################
pdf("Cumulative GIRF from Economic Development News to GBPUSD.pdf",width=6,height=4,paper='special')
cirf2 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "GBPUSD", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirf2$irf$Entropy_Economic_Development, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Cumulative Generalized Impulse Response from \nEconomic Development News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf2$Upper$Entropy_Economic_Development), length(cirf2$Lower$Entropy_Economic_Development):1),
  c(cirf2$Upper$Entropy_Economic_Development, rev(cirf2$Lower$Entropy_Economic_Development)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf2$irf$Entropy_Economic_Development, col = "Darkblue", lwd=2) #darkgeen
lines(cirf2$Upper$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf2$Lower$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#### cirf 3   ###########################
##
pdf("Cumulative GIRF from FED News to GBPUSD.pdf",width=6,height=4,paper='special')
cirf3 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "GBPUSD", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirf3$irf$Entropy_FED, type="l", col="Darkblue", ylab = "Response of USD/USD", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Cumulative Generalized Impulse Response from \nFED News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf3$Upper$Entropy_FED), length(cirf3$Lower$Entropy_FED):1),
  c(cirf3$Upper$Entropy_FED, rev(cirf3$Lower$Entropy_FED)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf3$irf$Entropy_FED, col = "Darkblue", lwd=2) #darkgeen
lines(cirf3$Upper$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf3$Lower$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#### cirf 4   ###########################
##
pdf("Cumulative GIRF from Microeconomic News to GBPUSD.pdf",width=6,height=4,paper='special')
cirf4 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "GBPUSD", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirf4$irf$Entropy_Micro_Finance, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Cumulative Generalized Impulse Response from \nMicroeconomic News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf4$Upper$Entropy_Micro_Finance), length(cirf4$Lower$Entropy_Micro_Finance):1),
  c(cirf4$Upper$Entropy_Micro_Finance, rev(cirf4$Lower$Entropy_Micro_Finance)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf4$irf$Entropy_Micro_Finance, col = "Darkblue", lwd=2) #darkgeen
lines(cirf4$Upper$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf4$Lower$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
### cirf 5   ###########################
pdf("Cumulative GIRF from International Trade News to GBPUSD.pdf",width=6,height=4,paper='special')
cirf5 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "GBPUSD", cumulative = FALSE, ortho = TRUE, runs = 500)
plot(cirf5$irf$Entropy_International_Trade, type="l", col="Darkblue", ylab = "Response of USD/GBP", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Cumulative Generalized Impulse Response from \nInternational Trade News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf5$Upper$Entropy_International_Trade), length(cirf5$Lower$Entropy_International_Trade):1),
  c(cirf5$Upper$Entropy_International_Trade, rev(cirf5$Lower$Entropy_International_Trade)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf5$irf$Entropy_International_Trade, col = "Darkblue", lwd=2) #darkgeen
lines(cirf5$Upper$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf5$Lower$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()
################################ IRF GBPUSD to inflation
pdf("Cumulative GIRF from Inflation to GBPUSD.pdf",width=6,height=4,paper='special')
cirfInflGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Inflation_US", response = "GBPUSD", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirfInflGBPUSD, main ="Cumulative Generalized Impulse Response from Inflation", pch=25)
dev.off()

pdf("Cumulative GIRF from MMR to GBPUSD.pdf",width=6,height=4,paper='special')
cirfMMRGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Money_Market_Rate_US", response = "GBPUSD", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirfMMRGBPUSD, main ="Cumulative Generalized Impulse Response from Money Market Rate", pch=25)
dev.off()

pdf("Cumulative GIRF from EA to GBPUSD.pdf",width=6,height=4,paper='special')
cirfEAGBPUSD <- irf(fitVARGBP1, n.ahead = 36, impulse = "Economic_Activity_US", response = "GBPUSD", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirfEAGBPUSD, main ="Cumulative Generalized Impulse Response from Economic Activity in USA", pch=25)
dev.off()


################################# Obtain NEW Cumulative IRFs of USDEUR############################
##IRFs of USDEUR
# 1)Stock Market, 2)Economic Development, 3)FED, 4) Micro Finance, 5) International Trade
#cirf1   ###########################
####
pdf("Cumulative GIRF from Stock Market News to USDEUR.pdf",width=6,height=4,paper='special')
cirf1 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Stock_Market", response = "USDEUR", cumulative = TRUE, ortho = TRUE, runs = 1000)
plot(cirf1$irf$Entropy_Stock_Market, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Cumulative Generalized Impulse Response from \nStock Market News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "topright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf1$Upper$Entropy_Stock_Market), length(cirf1$Lower$Entropy_Stock_Market):1),
  c(cirf1$Upper$Entropy_Stock_Market, rev(cirf1$Lower$Entropy_Stock_Market)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf1$irf$Entropy_Stock_Market, col = "Darkblue", lwd=2) #darkgeen
lines(cirf1$Upper$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf1$Lower$Entropy_Stock_Market, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#cirf2   ###############
####
pdf("Cumulative GIRF from Economic Development News to USDEUR.pdf",width=6,height=4,paper='special')
cirf2 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Economic_Development", response = "USDEUR", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirf2$irf$Entropy_Economic_Development, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Generalized Impulse Response from \nEconomic Development News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "topright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf2$Upper$Entropy_Economic_Development), length(cirf2$Lower$Entropy_Economic_Development):1),
  c(cirf2$Upper$Entropy_Economic_Development, rev(cirf2$Lower$Entropy_Economic_Development)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf2$irf$Entropy_Economic_Development, col = "Darkblue", lwd=2) #darkgeen
lines(cirf2$Upper$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf2$Lower$Entropy_Economic_Development, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#cirf3   ################################

pdf("Cumulative GIRF from FED News to USDEUR.pdf",width=6,height=4,paper='special')
cirf3 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_FED", response = "USDEUR", cumulative = TRUE, ortho = FALSE, runs = 500)
plot(cirf3$irf$Entropy_FED, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Cumulative Generalized Impulse Response from \nFED News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "bottomright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf3$Upper$Entropy_FED), length(cirf3$Lower$Entropy_FED):1),
  c(cirf3$Upper$Entropy_FED, rev(cirf3$Lower$Entropy_FED)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf3$irf$Entropy_FED, col = "Darkblue", lwd=2) #darkgeen
lines(cirf3$Upper$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf3$Lower$Entropy_FED, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#cirf4  #########################################

pdf("Cumulative GIRF from Microeconomic News to USDEUR.pdf",width=6,height=4,paper='special')
cirf4 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_Micro_Finance", response = "USDEUR", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirf4$irf$Entropy_Micro_Finance, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock",xlim= c(2.2, 36),  ylim= c(-0.07, 0.025),main ="Cumulative Generalized Impulse Response from \nMicroeconomic News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "topright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf4$Upper$Entropy_Micro_Finance), length(cirf4$Lower$Entropy_Micro_Finance):1),
  c(cirf4$Upper$Entropy_Micro_Finance, rev(cirf4$Lower$Entropy_Micro_Finance)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf4$irf$Entropy_Micro_Finance, col = "Darkblue", lwd=2) #darkgeen
lines(cirf4$Upper$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf4$Lower$Entropy_Micro_Finance, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()

#cirf5  #########################################

# pdf("GIRF from International Trade News to USDEUR.pdf",width=6,height=4,paper='special')
# irf5 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "USDEUR", cumulative = FALSE, ortho = TRUE, runs = 500)
# plot(irf5, main ="Generalized Impulse Response from International Trade News", pch=25)
# dev.off()

# 
pdf("Cumulative GIRF from International Trade News to USDEUR.pdf",width=6,height=4,paper='special')
cirf5 <- irf(fitVARGBP1, n.ahead = 36, impulse = "Entropy_International_Trade", response = "USDEUR", cumulative = TRUE, ortho = TRUE, runs = 500)
plot(cirf5$irf$Entropy_International_Trade, type="l", col="Darkblue", ylab = "Response of USD/EUR", xlab = "Time periods after shock", xlim= c(2.2, 36), main ="Cumulative Generalized Impulse Response from \nInternational Trade News") # ylim = c(-.003, .003),xlim= c(2, 36)
abline(h=0, lty = 2)

# Add a legend
legend(x = "topright", legend=c("Response of USD/EUR", "95% CB of the Response"), 
       col=c("Darkblue", "darkgreen"), lty=1:2, cex=0.8, lwd=2) #inset = c(0, -0.2), xpd = FALSE,

# draw the filled polygon for confidence intervals
polygon(
  c(1:length(cirf5$Upper$Entropy_International_Trade), length(cirf5$Lower$Entropy_International_Trade):1),
  c(cirf5$Upper$Entropy_International_Trade, rev(cirf5$Lower$Entropy_International_Trade)), 
  col = "grey90", border = NA)

grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "grey70", # Grid line color
     lwd = 1)      # Grid line width

# add coefficient estimate line
lines(cirf5$irf$Entropy_International_Trade, col = "Darkblue", lwd=2) #darkgeen
lines(cirf5$Upper$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
lines(cirf5$Lower$Entropy_International_Trade, col = "darkgreen", lty=2, lwd=2) #darkgeen
abline(h=0, lty = 2)
dev.off()


#plot Entropies over time #######
pdf("Monthly Entropy to the Topic Stock Market.pdf",width=6,height=4,paper='special')
plot(monthlyentrophy_Stock_Market.ts, type="l", col="Darkblue", ylab = "Monthly Entropy to the Topic Stock Market", xlab = "Time", main =" Monthly Attention to the Topic 1\n Stock Market")
dev.off()

pdf("Monthly Entropy to the Topic Economic Development.pdf",width=6,height=4,paper='special')
plot(monthlyentrophy_Economic_Development.ts, type="l", col="Darkblue", ylab = "Monthly Entropy to the Topic Economic Development", xlab = "Time", main =" Monthly Attention to the Topic 2\n Economic Development")
dev.off()

pdf("Monthly Entropy to the Topic ECB.pdf",width=6,height=4,paper='special')
plot(monthlyentrophy_CentrlBank.ts, type="l", col="Darkblue", ylab = "Monthly Entropy  to the Topic ECB", xlab = "Time", main =" Monthly Attention to the Topic 3\n ECB")
dev.off()

pdf("Monthly Entropy to the Topic Microeconomics.pdf",width=6,height=4,paper='special')
plot(monthlyentrophy_Micro_Finance.ts, type="l", col="Darkblue", ylab = "Monthly Entropy  to the Topic Microeconomics", xlab = "Time", main =" Monthly Attention to the Topic 4\n Microeconomics")
dev.off()

pdf("Monthly Entropy to the Topic Trade",width=6,height=4,paper='special')
plot(monthlyentrophy_Export_import.ts, type="l", col="Darkblue", ylab = "Monthly Entropy to the Topic International Trade", xlab = "Time", main =" Monthly Attention to the Topic 5\n International Trade")
dev.off()