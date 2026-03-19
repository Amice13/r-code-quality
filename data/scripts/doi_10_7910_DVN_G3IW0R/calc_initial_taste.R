#
# Description: calc supply/demand shock correlations and "iterated" CES price indices
# using long-differences
#
# includes estimates based on varying a sigma parameter that is common to each module
# (e.g. assume all modules have a CES sigma of 1.5, or 2, or 2.5, etc.)
#
# NOTE: all module --> national aggregation uses initial year exp weights (assuming cobb-douglas for across modules: welfare relevant and chained index)
# among continuing goods.
# INPUTS: datacompile_natsums_annual_YYYY.rds; upcModLink.rds upc_sample_defs.rds
# OUTPUT: initial_taste.xlsx


rm(list=ls())
library(data.table)
library(bit64)
library(openxlsx)

# Directory Structure
rdsDir <- "C:/Users/pengl/Dropbox/Code/R_intermediate/Neilsen_processed/"
resultsDir <- "C:/Users/pengl/Dropbox/Code/figdata/"


# read in upc-module link and sample definitions
upcModLink <- readRDS(paste0(rdsDir,"upcModLink.rds"))
upcSampleList <- readRDS(paste0(rdsDir,"upc_sample_defs.rds"))

# variables that define a "product"
productVars <- c("upc","upc_ver_uc")
grpMod <- "product_module_code"

# set betaLoop values
sigmaVals <- seq(1.5,8,by=0.5)
betaLoop <- 1-sigmaVals

# set year values
yearLoop <- seq(2004L,2018L,by=1L)
baseYear <- 2019L

# read in base data
baseData <- readRDS(paste0(rdsDir,"datacompile_natsums_annual_",baseYear,".rds"))
baseData <- baseData[, .SD, .SDcols=c("upc","upc_ver_uc","year","pfsum_actualPaid","pfsum_quant")]

# calculate price
baseData[, price := pfsum_actualPaid/pfsum_quant]

# reset names for base-year value
resetVars <- c("year","pfsum_quant","pfsum_actualPaid","price")
setnames(baseData,resetVars,paste0(resetVars,"_base"))

# loop over prior years
count <- 0L
for (iyr in yearLoop) {
  
  print(paste0("Running year ",iyr))
  count <- count+1L
  
  # read in prior-year data
  priorData <- readRDS(paste0(rdsDir,"datacompile_natsums_annual_",iyr,".rds"))
  priorData[, .SD, .SDcols=c("upc","upc_ver_uc","pfsum_actualPaid","pfsum_quant")]
  priorData[, price := pfsum_actualPaid/pfsum_quant]

  # merge base and priorData
  mergeData <- merge.data.table(baseData,priorData,by=productVars,all=TRUE)
  rm("priorData")
  mergeData[, year_base := NULL]
  mergeData <- merge.data.table(mergeData,upcModLink,by=productVars,all.x=TRUE)
  mergeData <- mergeData[keepMod==TRUE, ]
  
  # link in continuing goods definition from sample definitions
  useSample <- paste0("sampleChnLong_",iyr,"_2019")
  mergeData <- merge.data.table(mergeData,upcSampleList[, .SD, .SDcols=c(productVars,useSample)],all.x=TRUE)
  setnames(mergeData,useSample,"continuing")
  
  # construct dlmodshrC (upc share within module)
  mergeData[, modExpTtl := sum(pfsum_actualPaid,na.rm=TRUE), by=grpMod]
  mergeData[, modExpTtl_base := sum(pfsum_actualPaid_base,na.rm=TRUE), by=grpMod]
  mergeData[, modExpC := sum(pfsum_actualPaid*continuing,na.rm=TRUE), by=grpMod]
  mergeData[, modExpC_base := sum(pfsum_actualPaid_base*continuing,na.rm=TRUE), by=grpMod]
  mergeData[continuing==TRUE, modShrC := pfsum_actualPaid/modExpC]
  mergeData[continuing==TRUE, modShrC_base := pfsum_actualPaid_base/modExpC_base]
  mergeData[, dlmodshrC := log(modShrC_base/modShrC)]
  mergeData[is.nan(dlmodshrC), dlmodshrC := NA_real_]
  
  # calculate long-difference for price
  mergeData[, dlprice := log(price_base/price)]
  mergeData[continuing==FALSE, dlprice := NA_real_]
  
  # check that module has more than 1 upc
  mergeData[, nprod := sum(continuing,na.rm=TRUE), by="product_module_code"]

  # long-difference logpaas, loglasp, and torn indices
  mergeData[, dlmodCESP_paas := sum(modShrC_base*dlprice,na.rm=TRUE), by=grpMod]
  mergeData[, dlmodCESP_lasp := sum(modShrC*dlprice,na.rm=TRUE), by=grpMod]
  mergeData[, dlmodCESP_torn := (1/2)*(dlmodCESP_paas+dlmodCESP_lasp)]
  
  # construct the long-difference Sato Vartia index
  mergeData[, wsv_mod_num := (modShrC_base-modShrC)/dlmodshrC]
  mergeData[continuing==TRUE & nprod==1, wsv_mod_num := 1]
  mergeData[nprod==0, wsv_mod_num := NA_real_]
  mergeData[, dlmodCESP_sv := sum(wsv_mod_num*dlprice,na.rm=TRUE)/sum(wsv_mod_num,na.rm=TRUE), by=grpMod] # sato-Vertia 
  mergeData[nprod==0, dlmodCESP_sv := NA_real_]  
  
  # calculate the closed form solution for CES (conditional on sigma)
  dlmodCESP_closed_inner <- paste0("dlmodCESP_closed_inner_",sigmaVals*10)
  dlmodCESP_closed <- paste0("dlmodCESP_closed_",sigmaVals*10)  
  mergeData[, (dlmodCESP_closed_inner) := lapply(sigmaVals, function(x) sum(modShrC*(exp((1-x)*dlprice)),na.rm=TRUE)), by=grpMod]
  mergeData[, (dlmodCESP_closed) := lapply(seq(1,length(sigmaVals)), 
                                           function(x) ifelse(.SD[[x]]>0, (1/(1-sigmaVals[x]))*log(.SD[[x]]),NA_real_)),
            .SDcols=dlmodCESP_closed_inner]  
  mergeData[, (dlmodCESP_closed_inner) := NULL]  
  
  # extract module level data
  aggVars_upcMod <- c("dlmodCESP_sv","dlmodCESP_paas","dlmodCESP_torn",dlmodCESP_closed)
  modData <- mergeData[mergeData[, .I[1], by=grpMod]$V1, .SD,
                       .SDcols=c(grpMod,"modExpC","modExpTtl",aggVars_upcMod)]
  
  # collapse nat-module to national 
  aggVars_ttl <- paste0(aggVars_upcMod,"_aggTtl")
  modData[, ttlMktExpC := sum(modExpC)]
  modData[, ttlMktExp := sum(modExpTtl)]  
  modData[, modShrC := modExpC/ttlMktExpC]
  modData[, modShrTtl := modExpTtl/ttlMktExp]    
  modData[, (aggVars_ttl) := lapply(1:length(aggVars_ttl), function(x) sum(modShrTtl*.SD[[x]],na.rm=TRUE)),
          .SDcols=aggVars_upcMod]  
  
  # extract national
  annualData <- modData[1, .SD, .SDcols=aggVars_ttl]
  annualData[, year := iyr]
  
  # compile annual numbers for each iteration of loop  
  if (count==1L) {
    finalData <- annualData
  } else {
    finalData <- rbind(finalData,annualData)
  }
  rm(list=c("mergeData","modData","annualData"))
  gc()
  
} # end loop over prior years

setcolorder(finalData,"year")
setorderv(finalData,cols="year",order=1L)
library(xlsx)
write.xlsx(finalData,file=paste0(resultsDir,"initial_taste.xlsx"),rowNames=FALSE)
