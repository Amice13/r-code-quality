# basic nielsen data processing
#
# Description: create annual files with minimal processing from Consumer Panel data
#
# INPUT FILES: raw nielsen files (products master; annual purchases, trips, panelists)
# OUTPUT FILES: datacompile_sums_nat_upcYear_YYYY.rds; upcModLink_simple.rds; modDesc.rds
 
# Libraries
rm(list=ls())
library(data.table)
library(bit64)
library(xlsx)
library(openxlsx)

# Directory / File Structure
# dataDir = where nielsen files live [.../nielsen_extracts/HMS/]
# rdsDir = where you want output rds files saved
dataDir <- "E:/Nilesen/Consumer_Panel/HMS/"
rdsDir <- "C:/Users/pengl/Dropbox/Code/R_intermediate/Neilsen_processed/"

# load in products.tsv; record which UPCs are in which modules
# and store module descriptions as well
prodFile <- paste0(dataDir,"Master_Files/Latest/products.tsv")
prodData <- fread(prodFile,select=c("upc"="integer64","upc_ver_uc"="integer",
                                    "product_module_code"="integer",
                                    "product_module_descr"="character"),quote="")

upcModLink <- prodData[, .SD, .SDcols=c("upc","upc_ver_uc","product_module_code")]
modInfo <- as.data.table(read.xlsx(paste0(dataDir,"reference_documentation/Product_Hierarchy_01.2021.xlsx"),
                                   sheetIndex=1L,rowIndex=seq(2,1409),colIndex=seq(1,10),header=FALSE))
setnames(modInfo,paste0("X",seq(1,10)),c("product_module_code","product_module_descr","product_group_code","product_group_descr",
                                         "department_code","department_descr","deferred","scanner_years","onlyPanel","emptyYears"))
# If a different version of Product_Hierarchy file is used, the researcher may need to customize it.
upcModLink <- merge.data.table(upcModLink,modInfo,by="product_module_code")
upcModLink[, keepMod := TRUE]
upcModLink[product_module_code<1000 | product_module_code>9000, keepMod := FALSE]
upcModLink[deferred=="X", keepMod := FALSE]
upcModLink <- upcModLink[, .SD, .SDcols=c("upc","upc_ver_uc","product_module_code","keepMod")]
modDesc <- prodData[prodData[, .I[1], by="product_module_code"]$V1, .SD, .SDcols=c("product_module_code","product_module_descr")]
saveRDS(upcModLink,paste0(rdsDir,"upcModLink_simple.rds"))
saveRDS(modDesc,paste0(rdsDir,"modDesc.rds"))
rm(list=c("prodFile","prodData","upcModLink","modDesc"))

# Process data files for years startYear to endYear
count <- 0
startYear <- 2004L
endYear <- 2019L

# Loop over years, applying data cleaning procedure
# --> collapse UPC-HH-TRIP (raw purchases file) to UPC-YEAR at national geography
# --> data cleaning steps: 
# --> (1) drop transactions (UPC-HH-TRIP) with <=0 quantity or purchase value
# --> (2) drop UPC-HH-TRIP observations that are extreme outliers within relevant UPC-MKT-QTR
for (yearUse in seq(startYear,endYear,1)) {
  
  count = count+1
  
  yearStr <- toString(yearUse)
  tripFile <- paste0(dataDir,yearStr,"/Annual_Files/trips_",yearStr,".tsv")
  purchFile <- paste0(dataDir,yearStr,"/Annual_Files/purchases_",yearStr,".tsv")
  panelFile <- paste0(dataDir,yearStr,"/Annual_Files/panelists_",yearStr,".tsv")
  
  print(paste0("Now processing year: ",yearStr))
  
  # read in purchases file, construct actual price paid, drop coupon value
  print("     purchases file")
  purchData <- fread(purchFile,select=c("trip_code_uc"="integer","upc"="integer64",
                                        "upc_ver_uc"="integer","quantity"="numeric",
                                        "total_price_paid"="numeric","coupon_value"="numeric"))
  purchData[, actualPaid := total_price_paid-coupon_value]
  dropVars <- c("coupon_value","total_price_paid")
  purchData <- purchData[, (dropVars) := NULL]
  
  # --> keep only observation where actualPaid and quantity is greater than zero
  index1 <- purchData$actualPaid>0 & !is.na(purchData$actualPaid)
  index2 <- purchData$quantity>0 & !is.na(purchData$quantity)
  purchData <- purchData[index1 & index2,]
  rm("index1","index2")  
  
  # read in trip data
  print("     trips file")  
  tripData <- fread(tripFile,select=c("trip_code_uc"="integer","household_code"="integer","purchase_date"="character"))
  
  # extract year and quarter from purchase_date string
  tripData[, yrVal := as.integer(as.integer(paste0(substr(purchase_date,1,4))))]
  tripData[, qtrVal := as.integer(ceiling(as.integer(substr(purchase_date,6,7))/3))]
  tripData[, purchase_date := NULL]
  
  # if yrVal is from prior year, bump up to current year q1
  tripData[yrVal==yearUse-1L, qtrVal := 1L]
  tripData[yrVal==yearUse-1L, yrVal := yearUse]
  tripData[, yqVal := yrVal*10L+qtrVal]
  
  # merge purchData and tripData to get household_code and purchase dates for each transaction
  purchData <- merge.data.table(purchData,tripData,by="trip_code_uc")
  rm("tripData")
  purchData[, trip_code_uc := NULL]
  
  print("     collapse to upc-household-quarter")  
  grpUpcHhQtr <- c("upc","upc_ver_uc","yqVal","household_code")
  purchData <- purchData[, .(quantity = sum(quantity), actualPaid = sum(actualPaid)), by=grpUpcHhQtr]
  
  # read in panelist file, merge with purch/trip data
  # conform variable names to pre-2016 standard
  print("     panelist file")    
  if (yearUse < 2016L) {
    panelData <- fread(panelFile,select=c("household_code"="integer","projection_factor"="numeric",
                                          "scantrack_market_code"="integer"))
  }
  else {
    panelData <- fread(panelFile,select=c("Household_Cd"="integer","Projection_Factor"="numeric",
                                          "Scantrack_Market_Identifier_Cd"="integer"))
    setnames(panelData,c("Household_Cd","Projection_Factor","Scantrack_Market_Identifier_Cd"),
             c("household_code","projection_factor","scantrack_market_code"))
  }
  purchData <- merge.data.table(purchData,panelData,by="household_code")
  rm("panelData")
  purchData[, household_code := NULL]
  
  # data cleaning
  # --> drop observations with more than 3-times the median market-quarter price
  # --> drop observation with less than 1/3 the median market-quarter price
  # --> drop observation with more than 24 times the median market-quarter quantity
  # (currently an "observation" is a trip-upc)
  print("     drop outlier household-upc within household-upc-market-qtr")
  grpUpcMktQtr <- c("upc","upc_ver_uc","scantrack_market_code","yqVal")
  setcolorder(purchData,grpUpcMktQtr)  
  purchData[, price := actualPaid/quantity]
  grpUpcMktQtr <- c("upc","upc_ver_uc","scantrack_market_code","yqVal")
  setkeyv(purchData,c(grpUpcMktQtr))
  tmp1 <- purchData[, .(medquant = median(quantity), medprice = median(price)), by=grpUpcMktQtr]
  purchData <- merge.data.table(purchData,tmp1,by=grpUpcMktQtr)
  rm("tmp1")
  purchData[, dropIndex := price>3*medprice | price<(1/3)*medprice | quantity>24*medquant]
  purchData <- purchData[dropIndex==FALSE, ]
  dropVars <- c("price","medprice","medquant","dropIndex")
  purchData[, (dropVars) := NULL]
  
  # collapse cleaned UPC-HH-TRIP down to UPC-QTR (national sums)
  # also include some summary data on the UPC-QTR
  print("     collapse on upc (quarterly)")
  sum_stats_qtr <- purchData[, .(nmkt = length(unique(scantrack_market_code)),
                             nhh = .N), by=c("upc","upc_ver_uc","yqVal")]
  qtrData <- purchData[, .(rawsum_quant = sum(quantity), rawsum_actualPaid = sum(actualPaid),
                             pfsum_quant = sum(projection_factor*quantity),
                             pfsum_actualPaid = sum(projection_factor*actualPaid),
                             pfsum = sum(projection_factor)), by=c("upc","upc_ver_uc","yqVal")]
  
  qtrData <- merge.data.table(qtrData,sum_stats_qtr,by=c("upc","upc_ver_uc","yqVal"))
  
  # collapse cleaned UPC-HH-TRIP down to UPC-YEAR (national sums)
  # also include some summary data on the UPC-YEAR
  print("     collapse on upc (annual)")
  sum_stats <- purchData[, .(nqtr = length(unique(yqVal)), nmkt = length(unique(scantrack_market_code)),
                             nhh = .N), by=c("upc","upc_ver_uc")]
  purchData <- purchData[, .(rawsum_quant = sum(quantity), rawsum_actualPaid = sum(actualPaid),
                             pfsum_quant = sum(projection_factor*quantity),
                             pfsum_actualPaid = sum(projection_factor*actualPaid),
                             pfsum = sum(projection_factor)), by=c("upc","upc_ver_uc")]
  
  purchData <- merge.data.table(purchData,sum_stats,by=c("upc","upc_ver_uc"))
  purchData[, year := yearUse]
  
  print("     saving rds file")  
  saveRDS(purchData,paste0(rdsDir,"datacompile_natsums_annual_",yearStr,".rds"))
  rm(list=c("purchData","qtrData"))
  
} # end loop over years




