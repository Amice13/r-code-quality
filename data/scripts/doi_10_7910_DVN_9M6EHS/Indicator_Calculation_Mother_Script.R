#-------------------------------------------------------------------------------------------------
# Master Script for RHoMIS Indicator Calculations
#-------------------------------------------------------------------------------------------------
#### Libraries ####
library(plyr)
library(stringr)
library(readr)
#-------------------------------------------------------------------------------------------------
#' project_ID made up of: 
#' 2 letter country code 
#' 3 letter project code 
#' the year most of the interviews were conducted
# 'TZ_CFM_2015',
# 'GT_S4N_2015', 
# 'SV_S4N_2015', 
# 'HN_S4N_2015',
# 'ML_CW1_2015',
# 'BF_CW2_2015',
# 'MW_CFG_2015',
# 'KE_CM1_2016',
# 'KE_CM2_2016',
# 'BF_SIL_2016',
# 'IN_CM3_2016',
# 'KH_HAM_2016',
# 'VM_HAM_2016',
# 'LA_HAM_2016',
# 'TZ_SRL_2016',
# 'ET_SRL_2016',
# 'KE_SRL_2016',
# 'TZ_GLV_2017',
# 'CD_FRT_2017',
# 'KE_SCN_2017',
# 'ZM_SCN_2017',
# 'CD_CLP_2017',
# 'ET_TA1_2017',
# 'GH_TA2_2017',
# 'ML_TA3_2017',
# 'UG_CWU_2017',
# 'KE_VCD_2017',
# 'CR_IND_2017',
# 'BI_CLP_2018',
# 'BF_TA4_2018',
# 'TZ_CRA_2018',
# 'PE_MKP_2018',
# 'NI_CFN_2018',
# 'BF_TA5_2018',
# 'ET_TA6_2018',
# 'GH_TA7_2018'

# Select which project you are processing

#SEARCH_SURVEY_ID
#project_ID<-"KE_CM2_2016"




#SEARCH_YEAR
YEAR<- gsub('.*_','', project_ID)
#SEARCH_ID_COUNTRY
COUNTRY_CODE<- gsub('_.*','', project_ID)

#SEARCH_ID_PROJ
PROJECT<- gsub(paste0('_',unique(YEAR)), '', project_ID)
PROJECT<- gsub(paste0(unique(COUNTRY_CODE),'_'), '', PROJECT)
#SEARCH_ITERATION
ITERATION<-'1'





#-------------------------------------------------------------------------------------------------
#### Reading in data ####

# The Core data, containing all of the core RHoMIS information


dat_all<- data.frame(read_csv(paste0("Data/Core_Data.csv"),na=c('n/a','<NA>', '-999')), check.names = F)
dat_all <- data.frame(lapply(dat_all, as.character), stringsAsFactors=FALSE, check.names = F)
#  replacing new spellings
source("../../Most_Recent_Script_Update/Cleaning/Cleanmotherdatabase.R")

#SEARCH_ID_HH
ID_HH<- paste0(COUNTRY_CODE,'_', YEAR,'_',PROJECT,'_', row.names(dat_all), '_',ITERATION)

#-------------------------------------------------------------------------------------------------
# Cleaning country names for consistency
# dat_all$country<- gsub("ethiopia","eth", dat_all$country)
# dat_all$country<- gsub("eth","ethiopia", dat_all$country)
# dat_all$country<- gsub("tnz","tanzania", dat_all$country)
# dat_all$country<- gsub("burkina faso","burkina", dat_all$country)
# dat_all$country<- gsub("burkina","Burkina Faso", dat_all$country)
# dat_all$country_wb<-gsub("Burkina_Faso", "Burkina Faso", dat_all$country)
# dat_all$country<- gsub("comoros","Comoros", dat_all$country)
country_names_list<- data.frame(read_csv("../../Units_and_Cleaning_Data/Cleaning/Country_Names.csv",na=c('n/a','<NA>')), check.names=F)#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)


#SEARCH_Country
dat_all$country<- country_names_list$Country_Name[country_names_list$ISO_Country_Code==COUNTRY_CODE]
dat_all$country_wb<- country_names_list$Country_WB[country_names_list$ISO_Country_Code==COUNTRY_CODE]

dat_all$year<- YEAR
#-------------------------------------------------------------------------------------------------
# Reading in literature values
# Crop Energy conversions
#'SEARCH_Food_Availability_kCal_MAE_day
#SEARCH_Food_Self_Sufficiency_kCal_MAE_day
crop_energy_table<- data.frame(read_csv("../../Indicator_Calculations/Literature_Values_and_Resources/CropEnergy.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
# Conversion for PPI Scores to PPI likelihood
#SEARCH_PPI_Score
PPI_Scorecard<- data.frame(read_csv("../../Indicator_Calculations/Literature_Values_and_Resources/PPI_ScoreCard.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)

#SEARCH_PPI_Threshold
PPI_Threshold_Sheet<- data.frame(read_csv("../../Indicator_Calculations/Literature_Values_and_Resources/PPI_Limits.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)

# World Bank table for international PPP$ conversions
#SEARCH_currency_conversion_factor

wb_table<- data.frame(read_csv("../../Indicator_Calculations/Literature_Values_and_Resources/World_Bank_Private_PPP_Conversion.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="latin1", stringsAsFactors = FALSE)
colnames(wb_table)<- gsub("ï..","", colnames(wb_table), fixed=T)
colnames(wb_table)<- gsub("."," ", colnames(wb_table), fixed=T)
colnames(wb_table)<- gsub("X","", colnames(wb_table), fixed=T)
#prices_table<- data.frame(read.csv("Indicator_Calculations/Literature_Values_and_Resources/Prices_Reasonable_Limits.csv",na=c('n/a','<NA>')))#,na.strings = c("n/a","<NA>"), fileEncoding="UTF-8", stringsAsFactors = FALSE)
#-------------------------------------------------------------------------------------------------
#### Sourcing initial Scripts and Creating Functions ####
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/TRUE_FALSE_SCRIPT.R") # The description of this function can be found within the script file

# A function used to capitalise the first letter of every word in a character string
# A function found online at: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
} 
#-------------------------------------------------------------------------------------------------


#### Further cleaning of variables for easier analysis ####
dat_all$landowned <- as.numeric(dat_all$landowned)
dat_all$landcultivated<- as.numeric(dat_all$landcultivated)

colnames(wb_table)<- gsub("X", "", colnames(wb_table))
colnames(wb_table)<- gsub("\\.", " ", colnames(wb_table))

#### The countries "Guatemala", "El_Salvador", and "Honduras" were not in the country data set, so I identified them based on the currency spent in that country
dat_all[dat_all$local_currency=="quetzales", colnames(dat_all)=="country"] <- "Guatemala"
dat_all[dat_all$local_currency=="dolares", colnames(dat_all)=="country"] <- "El_Salvador"
dat_all[dat_all$local_currency=="lempiras", colnames(dat_all)=="country"] <- "Honduras"



#Changing all of the livestock head counts to numeric instead of factor
x<- grep("livestock_heads_",colnames(dat_all)) 
dat_all[,x] <- apply(dat_all[,x], 2, function(x) as.numeric(x))

#Finding the columns with PPI questions
#SEARCH_PPI_Score
PPI_columns<- grep("PPI_",colnames(dat_all)) #Identifies the column numbers of all of the PPI_1, PPI_2 ... entries
PPI_score_data<- as.data.frame(lapply(dat_all[,PPI_columns], function (x)  as.numeric(gsub("\\D", "", x)))) #Extracts only the numeric values of each PPI variable eg. "aa0" goes to "0"

#-------------------------------------------------------------------------------------------------
# Extracting the year from the project ID
#dat_all$year<- rep(as.numeric(sub('.*_','', project_ID)), nrow(dat_all))

# # Creating a country variable which matches the country names in the world bank conversion sheet
# dat_all$country_wb<- dat_all$country
# dat_all$country_wb<-gsub("drc", "Congo, Dem. Rep.", dat_all$country_wb)
# dat_all$country_wb<-gsub("laopdr", "laos", dat_all$country_wb)
# dat_all$country_wb<-gsub("india_bihar_vaishali", "india", dat_all$country_wb)
# dat_all$country_wb<- firstup(dat_all$country_wb)
# dat_all$country_wb<-gsub("Burkina faso", "Burkina Faso", dat_all$country_wb)
# dat_all$country_wb<-gsub("Laos", "Lao PDR", dat_all$country_wb)
# dat_all$country_wb<-gsub("El_Salvador", "El Salvador", dat_all$country_wb)




#Selecting the staple crop to use for the calculations relating to food availability.
staple_crop<- "maize" 
if (dat_all$country_wb=="Lao PDR" || dat_all$country_wb=="India" || dat_all$country_wb=="Cambodia" || dat_all$country_wb=="Vietnam")
{
  staple_crop<- "rice" 
}

if (unique(dat_all$country_wb)=="Niger")
{
  staple_crop<- "millet" 
}

#A script which converts all incomes from LCU to PPP$
#SEARCH_currency_conversion_factor
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Currency_Conversion.R")

# A script for sourcing unit conversions, coefficients and constants
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Initialise_Parameters.R")

# A script for household basics: Household ID, Household size variables, Education Levels and land related variables 
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Household_Basics.R")

# A script for food security, FIES or HFIAS
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Food_Security.R")

# A script used to calculate household dietary diversity scores for various times of the year
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/HDDS.R")

#' A script which calculates crop yield -kg-, crop sold -kg-, crop consumed -kg-, crop used to feed livestock -kg- and income from crops -PPP $ / year-
#' This script also calculates the amounts, of the variables listed above, controlled by female, female_youth, male, male_youth



if (project_ID!='KE_GLT_2019')
{
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Crop_Calculations.R")
}

if (project_ID=='KE_GLT_2019')
{
  source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/GLTEN_Crop_Calcs_Integrated.R")
}

#' A script calculating the numbers and income relating to whole livestock
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Whole_Livestock_Calculations.R")

#' A script calculating the amount produced, sold and the income from sales for livestock products. 
#' The specific units relating to each livestock products are located within the script file
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Livestock_Product_Calculations.R")

# A script to combine all the incomes
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Total_Incomes.R")


#### ONLY UNCOMMENT THESE SCRIPTS IF YOU WANT OLD PRICE CALCULATIONS TO BE OVERWRITTEN ####
# A script for working out the prices of each product, including crops, whole livestock and livestock products
#source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Prices.R")

# Reading in the verified prices.
crop_price_means<-data.frame(read_csv("../../Indicator_Calculations/Prices/crop_prices_per_kg.csv",na=c('n/a','<NA>')), check.names = F)
crop_price_means<- crop_price_means[crop_price_means$SURVEY_ID==project_ID,which(colnames(crop_price_means)%in% c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'SURVEY_ID')==F)]
crop_price_means<- data.frame(lapply(crop_price_means,function(x) as.numeric(gsub(",",".",as.character(x)))))
colnames(crop_price_means)<- gsub("."," ", colnames(crop_price_means), fixed = T)

livestock_price_means<- data.frame(read_csv("../../Indicator_Calculations/Prices/whole_livestock_prices.csv",na=c('n/a','<NA>')), check.names = F)
livestock_price_means<- livestock_price_means[livestock_price_means$SURVEY_ID==project_ID,which(colnames(livestock_price_means)%in% c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'SURVEY_ID')==F)]
livestock_price_means<- data.frame(lapply(livestock_price_means,function(x) as.numeric(gsub(",",".",as.character(x)))))
colnames(livestock_price_means)<- gsub("."," ", colnames(livestock_price_means), fixed = T)

meat_price_means<- data.frame(read_csv("../../Indicator_Calculations/Prices/meat_prices_per_kg.csv",na=c('n/a','<NA>')), check.names = F)
meat_price_means<- meat_price_means[meat_price_means$SURVEY_ID==project_ID,which(colnames(meat_price_means)%in% c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'SURVEY_ID')==F)]
meat_price_means<- data.frame(lapply(meat_price_means,function(x) as.numeric(gsub(",",".",as.character(x)))))
colnames(meat_price_means)<- gsub("."," ", colnames(meat_price_means), fixed = T)

milk_price_means<-data.frame(read_csv("../../Indicator_Calculations/Prices/milk_prices_per_l.csv",na=c('n/a','<NA>')), check.names = F)
milk_price_means<- milk_price_means[milk_price_means$SURVEY_ID==project_ID,which(colnames(milk_price_means)%in% c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'SURVEY_ID')==F)]
milk_price_means<- data.frame(lapply(milk_price_means,function(x) as.numeric(gsub(",",".",as.character(x)))))
colnames(milk_price_means)<- gsub("."," ", colnames(milk_price_means), fixed = T)

eggs_price_means<- data.frame(read_csv("../../Indicator_Calculations/Prices/egg_price_per_egg.csv",na=c('n/a','<NA>')), check.names = F)
eggs_price_means<- eggs_price_means[eggs_price_means$SURVEY_ID==project_ID,which(colnames(eggs_price_means)%in% c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'SURVEY_ID')==F)]
eggs_price_means<- data.frame(lapply(eggs_price_means,function(x) as.numeric(gsub(",",".",as.character(x)))))
colnames(eggs_price_means)<- gsub("."," ", colnames(eggs_price_means), fixed = T)

honey_price_means<- data.frame(read_csv("../../Indicator_Calculations/Prices/honey_price_per_litre.csv",na=c('n/a','<NA>')), check.names = F)
honey_price_means<- honey_price_means[honey_price_means$SURVEY_ID==project_ID,which(colnames(honey_price_means)%in% c('ID_PROJ', 'ID_COUNTRY', 'YEAR', 'ITERATION', 'SURVEY_ID')==F)]
honey_price_means<- data.frame(lapply(honey_price_means,function(x) as.numeric(gsub(",",".",as.character(x)))))
colnames(honey_price_means)<- gsub("."," ", colnames(honey_price_means), fixed = T)

# cheese_price_means<- data.frame(read_csv(paste0("../../Indicator_Calculations/Prices/",project_ID,"/cheese_price_per_kg.csv"),na=c('n/a','<NA>')))#, encoding = 'UTF-8')
# cheese_price_means<- data.frame(lapply(cheese_price_means,function(x) as.numeric(gsub(",",".",as.character(x)))))
# colnames(cheese_price_means)<- gsub("."," ", colnames(cheese_price_means), fixed = T)
# 
# butter_price_means<- data.frame(read_csv(paste0("../../Indicator_Calculations/Prices/",project_ID,"/butter_price_per_kg.csv"),na=c('n/a','<NA>')))#, encoding = 'UTF-8')
# butter_price_means<- data.frame(lapply(butter_price_means,function(x) as.numeric(gsub(",",".",as.character(x)))))
# colnames(butter_price_means)<- gsub("."," ", colnames(butter_price_means), fixed = T)

# A script to calculate values for crops consumed, crop yield, livestock held, livestock products consumed etc...
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Value_Calculations.R")

# A script to work out the energies associated with consumption, production and income 
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Energy_Calculations.R")

# A script containing Calculations relating to fertiliser and GHG emissions
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Fertiliser_and_Emissions.R")

# A script calculating livestock and crop diversity
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Crop_and_Livestock_Diversity.R")


# Calcularions of total_value of Activities per day
#SEARCH_TVA_USD_PPP_pmae_pday
TVA_pmae_pday <- rowSums(data.frame((value_crop_consumed[,1]/HHsizeMAE/365), (valuelivestockprodconsumed[,1]/HHsizeMAE/365), (cropsales[,1]/HHsizeMAE/365), (livestockprodsales[,1]/HHsizeMAE/365),(off_farm_income[,1]/HHsizeMAE/365)), na.rm=T)
TVA_pmae_pday<- as.numeric(gsub("Inf", NA, as.character(TVA_pmae_pday)))
TVA_pmae_pday<- as.numeric(gsub("NaN", NA, as.character(TVA_pmae_pday)))


# Determining whether HFIAS or FIES was used, if either was not used a vector of NAs is created for that variable
if (exists('HFIAS_status')==F)
{
  HFIAS_status<- rep(NA, nrow(dat_all))
}

if (exists('FIES_Score')==F)
{
  FIES_Score<- rep(NA, nrow(dat_all))
}


#### Creating the indicator sheet
indicator_sheet_temp<-data.frame(#"HHid"= dat_all$HouseholdID,
  "HHid"= dat_all[,grep('hhid|householdid', colnames(dat_all), ignore.case = T)],
  "Country"=dat_all$country,
  #SEARCH_Region
  "Region"=dat_all$sublocation,
  "Village"= dat_all[,grep('village', colnames(dat_all), ignore.case = T)],
  #SEARCH_GPS_LAT
  "Latitude"=dat_all$X_GPS_latitude,
  #SEARCH_GPS_LON
  "Longitude"=dat_all$X_GPS_longitude,
  #SEARCH_GPS_ALT
  "Altitude"=dat_all$X_GPS_altitude,
  "HHsizemembers"=HHsizemembers,
  "HHsizeMAE"=HHsizeMAE,
  #SEARCH_HouseholdType
  "HouseholdType"=dat_all$household_type,
  "Head_EducationLevel"=Head_EducationLevel,
  "LandOwned"=LandOwned,
  "LandCultivated"=LandCultivated,
  "LivestockHoldings"=LivestockHoldings,
  "WorstFoodSecMonth"=WorstFoodSecMonth,
  "BestFoodSecMonth"= BestFoodSecMonth,
  "NrofMonthsFoodInsecure"= NrofMonthsFoodInsecure,
  "PPI_Threshold"=PPI_Threshold[,1],
  "PPI_Likelihood"=PPI_Likelihood[,1],
  "HFIAS_status"= HFIAS_status,
  "FIES_Score"= FIES_Score,
  "score_HDDS_GoodSeason"=score_HDDS_goodseason[,1],
  "score_HDDS_farmbasedGoodSeason"=score_HDDS_farmbasedGoodSeason[,1],
  "score_HDDS_purchasedGoodSeason"=score_HDDS_purchasedGoodSeason[,1],
  "score_HDDS_BadSeason"=score_HDDS_badseason[,1],
  "score_HDDS_farmbasedBadSeason"=score_HDDS_farmbasedBadSeason[,1],
  "score_HDDS_purchasedBadSeason"=score_HDDS_purchasedBadSeason[,1],
  'TVA_pmae_pday'=TVA_pmae_pday,
  'currency_conversion_factor'=conversion_factor[,1],
  "total_income"=total_income[,1],
  "offfarm_income"=off_farm_income[,1],
  "farm_income"=farm_income[,1],
  "valuefarmproduce"=valuefarmproduce[,1],
  "cropsales"=cropsales[,1],
  "valuecropproduce"= value_crop_produce[,1],
  "valuecropconsumed"=value_crop_consumed[,1],
  "livestockprodsales"=livestockprodsales[,1],
  "valuelivestockproduction"=valuelivestockproduce[,1],
  "valuelivestockprodconsumed"=valuelivestockprodconsumed[,1],
  "FoodAvailability"= FoodAvailability,
  "FoodSelfSufficiency"= foodselfsufficiency,
  "TotalEnergyAvailable"=TotalEnergyAvailable,
  "FAEnergyBought"= FAEnergyBought,
  "FAEnergyOffFarm"=FAEnergyOffFarm,
  "FAEnergyCropConsumption"=FAEnergyCropConsumption,
  "FAEnergyCropSales"=FAEnergyCropSales,
  "FAEnergyLivestockConsumption"= FAEnergyLivestockConsumption,
  "FAEnergyLivestockSales"=FAEnergyLivestockSales,
  "FAEnergyFarmBased"= FAEnergyFarmBased[,1],
  "FAMarketOrientation"=FAMarketOrientation,
  "FALivestockOrientation"= FALivestockOrientation,
  "RelImpWildFoods"=wild_food_prop_text,
  "NrofMonthsWildFoodCons"= NrofMonthsWildFoodCons,
  "GHGEmissions"= Emissions_total,
  "Gender_MaleControl"= Gender_male_control,
  "Gender_FemaleControl"= Gender_Female_control,
  "Gender_MaleYouthControl"= Gender_male_youth_control,
  "Gender_FemaleYouthControl"= Gender_Female_Youth_Control,
  "NFertInput"=fertiliser_input[,1],
  #"CropDiv"=cropdiv_nr,
  "CropDiv"=dat_all$crop_count,
  #"LivestockDiv"=livestock_div,
  "LivestockDiv"=dat_all$livestock_count)

write_csv(indicator_sheet_temp,"Data/indicator_sheet.csv")

# A script used to generate some extra outputs
source("../../Most_Recent_Script_Update/Indicator_Calculations/Scripts/Extra_Outputs.R")


