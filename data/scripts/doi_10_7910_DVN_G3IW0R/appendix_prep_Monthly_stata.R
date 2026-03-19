# Producing Monthly Data for Stata

rm(list=ls())
library(data.table) # augmented data frame, useful for large # of groups
library(bit64) # to allow large integer values (necessary for upcs)
library(xlsx) # for saving to excel at end
library(foreign)
rdsDir <- "C:/Users/pengl/Dropbox/Code/R_intermediate/Neilsen_processed/" 
resultsDir <- "C:/Users/pengl/Dropbox/Code/Stata_intermediate/Raw_data_stata/"

print("Reading in UPC / Brand / Module link")
upcModLink <- readRDS(paste0(rdsDir,"upcModLink.rds"))

# set year values
yearLoop <- seq(2004L,2019L,by=1L)
qLoop <- seq(1L,4L,by=1L)
for (iyr in yearLoop) {
  yearStr <- toString(iyr)

    yqrdata <- readRDS(paste0(rdsDir,"datacompile_natsums_monthly_",iyr,".rds"))
    # year and quarter
    yqrdata[, price := pfsum_actualPaid/pfsum_quant]
    # year and quarter
    yqrdata[, year := yqVal %/% 100]
    yqrdata[, quarter := (yqVal %% 100 - 1)+1]
    yqrdata[, upc := as.character(upc)]

    write.dta(yqrdata, paste0(resultsDir,"datacompile_natsums_monthly_",yearStr,".dta"))

}
upcModLink[, upc := as.character(upc)]
write.dta(upcModLink, paste0(resultsDir,"upcModLinkM.dta"))

