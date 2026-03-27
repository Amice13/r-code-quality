# CFI Model ICD10
# Versioned in R based upon original SAS code provided by Dae Hyun Kim
# Authored by: Doug Bedell at Westat
# 17Jun2021: Updated to match coding corrections made to 22May2021 SAS program.

# This code reads in relevant CFI datasets from provided code base,
# and outputs a CFI Measure for each patient.

# Load SQL library for ranged lookups
# If SQLDF not installed, install first with install.packages("sqldf")
library(sqldf)

# Regression model intercept becomes default frailty score for those with no
# diagnoses or procedures- derived from trained model
ModelIntercept = 0.10288
# Set input/output path for files
setwd("C:/CFI-R/")

# Function: Lookup HCPCS/CPT disease number
lookup_pxdisease <- function(data, lookup)
{
  data <- sqldf("select A.*, B.disease_number from
                 data A left join lookup B 
                 ON (A.px >= B.start and A.px <= B.stop)")
}

# Read in all study IDs (patid)
iddata <- read.delim('ids.txt', sep='\t', header=TRUE)

# Read in ICD-9 DX data (patid, dx)
dx9data <- read.delim('dx09.txt', sep='\t', header=TRUE)

# Read in ICD-9 DX lookup data.
dx9lookup <- read.delim('CFI_ICD9CM_V32.csv', sep=',', header=TRUE)

# Read in ICD-10 DX data (patid, dx)
dx10data <- read.delim('dx10.txt', sep='\t', header=TRUE)

# Read in ICD-10 DX lookup data
dx10lookup <- read.delim('CFI_ICD10CM_V2020.csv', sep=',', header=TRUE)

# Read in PX data (patid, px)
pxdata <- read.delim('px.txt', sep='\t', header=TRUE)

# Read in PX lookup data. Implements PX section of SAS codes_org.
pxlookup <- read.delim('pxlookup.txt', sep='\t', header=TRUE)

# Read in model disease weights lookup
weightlookup <- read.delim('disease_weight.txt', sep='\t', header=TRUE)

# NOTE: Since we only need to score each DX/PX once per patient, remove duplicates
# prior to any lookup.
# Get disease numbers for ICD-9 diagnoses file. Set any NA's to 0.
dx9data <- unique(dx9data)
dx9data <- merge(dx9data, dx9lookup, all.x=TRUE)
dx9data[is.na(dx9data)] <- 0
dx9data <- dx9data[order(dx9data$patid,dx9data$dx),]

# Get disease numbers for ICD-10 diagnoses file
dx10data <- unique(dx10data)
dx10data <- merge(dx10data, dx10lookup, all.x=TRUE)
dx10data[is.na(dx10data)] <- 0
dx10data <- dx10data[order(dx10data$patid,dx10data$dx),]

# Get disease numbers for procedures file
pxdata <- unique(pxdata)
pxdata <- lookup_pxdisease(pxdata, lookup=pxlookup)
pxdata[is.na(pxdata)] <- 0
# If a PX value isn't 5 characters or if last character
# isn't a number, PX should not be scored. Set to 0.
pxdata <- within(pxdata, disease_number[nchar(px) != 5 | grepl("[0-9]", substr(px, nchar(px), nchar(px))) == FALSE] <- 0)

# Assign dummy disease_number = 0 for all study IDs. This will have the effect of assigning the
# default weight (ModelIntercept) for any PatID that is not included in the DX9, DX10 or PX file
iddata <- unique(iddata)
iddata['disease_number'] = 0

# Combine the data, keeping only patient ID and disease number
diseasedata <- data.frame()
base_names <- names(iddata)
list_df <- list(dx9data, dx10data, pxdata, iddata)
for(item in list_df)
{
  items <- item[, base_names]
  diseasedata <- rbind(diseasedata, items)
}

# Remove duplicates. Each DX/PX should only be weighted once.
diseasedata <- unique(diseasedata)
diseasedatasort <- diseasedata[order(diseasedata$patid,diseasedata$disease_number),]

# Assign weights
# Merge the disease weights on to the disease data and fill non-matches (NA) with 0
diseasedatasort <- merge(diseasedatasort, weightlookup, all.x=TRUE)
diseasedatasort[is.na(diseasedatasort)] <- 0
diseasedatasort <- diseasedatasort[order(diseasedatasort$patid,diseasedatasort$disease_number),]

# Calculate frailty scores by summing the weights of records grouped by patient ID.
# ModelIntercept value added to every score. Default score for those with no DX/PX.
scores <- aggregate(diseasedatasort$weight, by=list(patid=diseasedatasort$patid), FUN=sum)
scores$x <- scores$x + ModelIntercept
colnames(scores) <- c('patid', 'frailty_index')

# Write to CSV
write.csv(scores, "frailty_output_r.csv", row.names=FALSE)
