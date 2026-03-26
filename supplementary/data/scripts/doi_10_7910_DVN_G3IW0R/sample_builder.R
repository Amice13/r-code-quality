# sample_builder
#
# Description: Define the set of continuing goods for different samples / 
# price index calculations
# 
# Define whether goods are "continuing" goods
# (1) Keep goods that are available in all four quarters of each year
# (2) Define "price" as the annual unit value (expenditure / quantity)
# (3) Calculate all log-price and log-expenditure changes for each one-year step
# (4) Define a good as "continuing" between two adjacent years if the log-price
#     changes and, resp., log-expenditure changes, is within the 1st and 99th
#     percentiles of all one-year log-price (res. log-expenditure changes)
#
# --> remaining observations after step (4) define goods that are "continuing"
#     for each one-year step
#
# (5) Define a good as a "long-continuing good" from some initial year 
#     (e.g. 2012) to the "base" year (2019) if it is "continuing" in step (4)
#     in all intervening one-year steps
#
# INPUTS: datacompile_sums_nat_upcYear_YYYY.rds; upcModLink_simple.rds
# OUTPUT: upcModLink.rds; upc_sample_defs.rds
#

rm(list=ls())
library(data.table) # augmented data frame, useful for large # of groups
library(bit64) # to allow large integer values (necessary for upcs)
library(matrixStats) # fast row-wise calculations
rdsDir <- "C:/Users/pengl/Dropbox/Code/R_intermediate/Neilsen_processed/"

startYear <- 2004L
endYear <- 2019L
yearLoop <- seq(startYear,endYear,by=1L)

# compile data file for all years into single data table
count <- 0L
for (iyr in yearLoop) {
  
  keepVars <- c("upc","upc_ver_uc","nqtr","pfsum_quant","pfsum_actualPaid","year")
  count <- count + 1L
  if (count == 1L) {
    mainData <- readRDS(paste0(rdsDir,"datacompile_natsums_annual_",iyr,".rds"))
    mainData <- mainData[, .SD, .SDcols=keepVars]
  } else {
    newData <- readRDS(paste0(rdsDir,"datacompile_natsums_annual_",iyr,".rds"))
    newData <- newData[, .SD, .SDcols=keepVars]
    mainData <- rbind(mainData,newData)    
  }

}

# calculate price = spend / quant
# (annual unit value)
mainData[, price := pfsum_actualPaid/pfsum_quant]

# append lead values
mainData[, year_tp1 := year+1L]
mainData[year_tp1>endYear, year_tp1 := NA_real_]
lagData <- mainData[, .SD, .SDcols=c("upc","upc_ver_uc","year","price","pfsum_actualPaid")]
setnames(lagData,c("year","price","pfsum_actualPaid"),c("year_tp1","price_tp1","pfsum_actualPaid_tp1"))
mainData <- merge.data.table(mainData,lagData,by=c("upc","upc_ver_uc","year_tp1"),all.x=TRUE)
setcolorder(mainData,c("upc","upc_ver_uc","year"))
setkeyv(mainData,c("upc","upc_ver_uc","year"))

# calculate change in price and total expenditure
mainData[, dlprice := log(price_tp1/price)]
mainData[, dlexp := log(pfsum_actualPaid_tp1/pfsum_actualPaid)]

# drop observations where nqtr is not equal to 4
mainData[nqtr<4, dlprice := NA_real_]
mainData[nqtr<4, dlexp := NA_real_]

# drop observations with extreme price/expenditure changes
mainData[, dlprice_p01 := quantile(dlprice,probs=0.01,na.rm=TRUE)]
mainData[, dlprice_p99 := quantile(dlprice,probs=0.99,na.rm=TRUE)]
mainData[, dlexp_p01 := quantile(dlexp,probs=0.01,na.rm=TRUE)]
mainData[, dlexp_p99 := quantile(dlexp,probs=0.99,na.rm=TRUE)]
mainData[, dropIndex := (dlprice<dlprice_p01 | dlprice>dlprice_p99 | dlexp<dlexp_p01 | dlexp>dlexp_p99) ]
mainData[dropIndex==TRUE, dlprice := NA_real_]
mainData[dropIndex==TRUE, dlexp := NA_real_]

# construct lists of upcs for the 2yr-continuing sample
# i.e. sample2yr_YEAR = TRUE if the upc had a valid link from year to year+1
mainData[, logical := TRUE]
upcList <- mainData[mainData[, .I[1], by=c("upc","upc_ver_uc")]$V1, .SD, .SDcols=c("upc","upc_ver_uc")]

for (iyr in yearLoop[1:length(yearLoop)-1]) {
  upcList <- merge.data.table(upcList,mainData[year==iyr & !is.na(dlprice) & !is.na(dlexp),
                                               .SD, .SDcols=c("upc","upc_ver_uc","logical")],all.x=TRUE)
  upcList[is.na(logical), logical := FALSE]
  setnames(upcList,"logical",paste0("sample2yr_",iyr))
}

# construct lists of upcs for long-chained-sample
# i.e. sampleChnLong_YEAR_endYr = TRUE if upc is "continuing" by the 2yr-continuing 
#     criteria (including the not-too-large change condition)
#     for each period from YEAR to the final year (2019)
count <- 0L
for ( iyr in yearLoop[1:(length(yearLoop)-1)] ) {
  count <- count+1L
  allLinks <- paste0("sample2yr_",yearLoop[count:(length(yearLoop)-1)])
  newVar <- paste0("sampleChnLong_",iyr,"_",endYear)
  upcList[, (newVar) := rowAlls(as.matrix(.SD)), .SDcols=allLinks]
}

# construct sampleChnLong_startYr_YEAR = TRUE if upc is "continuing" by 2yr-continuing
#       criteria (including the not-too-large change condition)
#       for each period from initial year (2004) to YEAR
count <- 0
for ( iyr in yearLoop[1:(length(yearLoop)-1)] ) {
  count <- count+1L
  allLinks <- paste0("sample2yr_",yearLoop[1:count])
  newVar <- paste0("sampleChnLong_",startYear,"_",iyr+1)
  upcList[, (newVar) := rowAlls(as.matrix(.SD)), .SDcols=allLinks]
}

# construct list of upcs for raw-chained sample
# i.e. sampleRawLong_YEAR = TRUE if upc is in all 4 quarters of YEAR and
#      final year, dropping goods with extreme price / share changes
yearLoop <- seq(startYear,endYear,by=1L)
upcModLink <- readRDS(paste0(rdsDir,"upcModLink_simple.rds"))
for (baseYear in c(startYear,endYear)) {
  
  # set up baseData and rename variables
  baseData <- mainData[year==baseYear, .SD, .SDcols=c("upc","upc_ver_uc","nqtr","price","pfsum_actualPaid")]
  setnames(baseData,c("nqtr","price","pfsum_actualPaid"),c("nqtr_base","price_base","pfsum_actualPaid_base"))
  
  # loop over years for other end of the long difference
  for (iyr in yearLoop) {
    
    # check if current iyr / baseYear combination should be run
    # do not run if (a) iyr == baseyear, or (b) an iyr / baseYear combination has already been run
    if (iyr==baseYear) { 
      doCalc <- FALSE
    } else if ( paste0("sampleRawLong_",min(baseYear,iyr),"_",max(baseYear,iyr)) %in% names(upcList) ) {
      doCalc <- FALSE
    } else {
      doCalc <- TRUE
    }

    # if doCalc is true, find upcs for iyr_baseYear long difference
    if (doCalc) {
  
      # merge base year and current loop year data
      # merge in upcModLink
      loopData <- mainData[year==iyr, .SD, .SDcols=c("upc","upc_ver_uc","nqtr","price","pfsum_actualPaid")]
      mergeData <- merge.data.table(baseData,loopData,by=c("upc","upc_ver_uc"),all=TRUE)
      mergeData <- merge.data.table(mergeData,upcModLink,by=c("upc","upc_ver_uc"),all.x=TRUE)
  
      # define continuing goods before making price / share cut
      if (baseYear > iyr) {
        mergeData[, dlprice := log(price_base/price)]
      } else {
        mergeData[, dlprice := -log(price_base/price)]
      }
      mergeData[nqtr<4 | nqtr_base<4, dlprice := NA_real_]
      mergeData[, cnt_tmp := !is.na(dlprice), by="product_module_code"]
  
      # calculate continuing goods shares (prior to extreme price / share cut)
      newVars <- c("modExpC_tmp","modExpC_base_tmp")
      mergeData[, (newVars) := lapply(.SD, function(x) sum(cnt_tmp*x,na.rm=TRUE)),
            by="product_module_code", .SDcols=c("pfsum_actualPaid","pfsum_actualPaid_base")]
      mergeData[, .(shrC_tmp = pfsum_actualPaid/modExpC_tmp, shrC_base_tmp = pfsum_actualPaid_base/modExpC_base_tmp)]
      mergeData[cnt_tmp==FALSE, shrC_tmp := NA_real_]
      mergeData[cnt_tmp==FALSE, shrC_tmp_base := NA_real_]
      if (baseYear > iyr) {
        mergeData[, dlshrC_tmp := log(shrC_tmp_base/shrC_tmp)]
      } else {
        mergeData[, dlshrC_tmp := -log(shrC_tmp_base/shrC_tmp)]    
      }
  
      # find 1st and 99th percentiles
      mergeData[, dlprice_p01 := quantile(dlprice,probs=0.01,na.rm=TRUE)]
      mergeData[, dlprice_p99 := quantile(dlprice,probs=0.99,na.rm=TRUE)]
      mergeData[, dlshrC_p01 := quantile(dlshrC_tmp,probs=0.01,na.rm=TRUE)]
      mergeData[, dlshrC_p99 := quantile(dlshrC_tmp,probs=0.99,na.rm=TRUE)]
  
      # drop if observation has extreme price or share changes
      mergeData[, dropIndex := (dlprice<dlprice_p01 | dlprice>dlprice_p99 | dlshrC_tmp<dlshrC_p01 | dlshrC_tmp>dlshrC_p99)]
      mergeData[dropIndex==TRUE, cnt_tmp := FALSE]
  
      # keep only UPCs that are continuing after the price / share cut
      upcList <- merge.data.table(upcList,mergeData[cnt_tmp==TRUE, .SD, .SDcols=c("upc","upc_ver_uc","cnt_tmp")],all.x=TRUE)
      upcList[is.na(cnt_tmp), cnt_tmp := FALSE]  
      setnames(upcList,"cnt_tmp",paste0("sampleRawLong_",min(baseYear,iyr),"_",max(baseYear,iyr)))
      
    } # end if (doCalc)
  } # end loop over iyr
} # end loop over baseYear

upcList <- merge.data.table(upcList,upcModLink,by=c("upc","upc_ver_uc"))
upcList[, testMod := sum(sampleChnLong_2004_2019), by="product_module_code"]
keepMods <- upcList[testMod>1, .SD, .SDcols=c("product_module_code")]
dropVars <- c("testMod","product_module_code","keepMod")
upcList[, (dropVars) := NULL]
keepMods <- keepMods[keepMods[, .I[1],by="product_module_code"]$V1, ]
keepMods[, fullSample := TRUE]
upcModLink <- merge.data.table(upcModLink,keepMods,by="product_module_code",all.x=TRUE)
upcModLink[is.na(fullSample), fullSample := FALSE]
upcModLink[, keepMod := (keepMod & fullSample)]
upcModLink[, fullSample := NULL]

saveRDS(upcList,paste0(rdsDir,"upc_sample_defs.rds"))
saveRDS(upcModLink,paste0(rdsDir,"upcModLink.rds"))


