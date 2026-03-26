rm(list=ls())
library(readxl)

load("Data/EUTL2021.Rdata")

## Add firm ownership and sectoral information (from Orbis)
d <- as.data.frame(read_xls("Data/EU_ETS_Acccount_to_Firm_Matching_Incl_NACE.xls"))	# EUTL-Orbis link downloaded May 6, 2021, from http://hdl.handle.net/1814/64596.
d <- d[!is.na(d$installation_ID),]	# Remove observations not linked to any installation. These are Personal Holding Accounts (PHAs), not Operator Holding Accounts (OHAs).

d$mergeID <- paste(d$country_code_OHA_PHA, d$installation_ID)
ds$nace <- ds$bvdidnumber <- NA
for (i in unique(d$mergeID)) {
	ds$bvdidnumber[substr(ds$NationalAdministratorCodeCorrected,1,2)==sub(" .*", "", i) & ds$InstallationOrAircraftOperatorID==as.numeric(sub(".* ", "", i))] <- d$GUO_past_BVD_ID[d$mergeID==i]	# Firm
	ds$nace[substr(ds$NationalAdministratorCodeCorrected,1,2)==sub(" .*", "", i) & ds$InstallationOrAircraftOperatorID==as.numeric(sub(".* ", "", i))] <- d$"NACE Rev2"[d$mergeID==i]	# Sector
}


## Create indicator for domestically owned firms
ds$domestic <- NA
ds$domestic[substr(ds$NationalAdministratorCodeCorrected,1,2)==substr(ds$bvdidnumber,1,2) & !is.na(substr(ds$bvdidnumber,1,2))] <- 1
ds$domestic[substr(ds$NationalAdministratorCodeCorrected,1,2)!=substr(ds$bvdidnumber,1,2) & !is.na(substr(ds$bvdidnumber,1,2))] <- 0


## Add firm listing information (from Orbis and Bloomberg)
### Write BvD ID numbers to file that's used to identify listing status in Orbis.
out <- data.frame(bvdidnumber=unique(ds$bvdidnumber), ets=1)
write.csv(out, "Data/EUIETSbvdid.txt", row.names = FALSE)
rm(out)

### Load listing information from Orbis
listing <- as.data.frame(read_xlsx("Data/EUIETSlistings.xlsx", sheet = 2))		# Orbis listing information for ETS companies, downloaded May 6, 2021.
listing <- listing[!is.na(listing$"BvD ID number"),]	# Remove Orbis downloads for which the BvD ID number is missing
listing$"Delisting date" <- as.Date(as.numeric(listing$"Delisting date"), origin="1899-12-30") # Format delisting dates

### Where IPO and delisting dates are missing, complement from Bloomberg terminal (downloaded on April 1, 2021)
# ** Date of earliest Additional Equity Offering, rather than IPO. If IPO date is not available, but the earliest additional offering preceeds the EU ETS, this date is sufficient to establish it was a listed company at the start.
# listing[listing$"Listing status"=="Listed" & is.na(listing$"IPO date"),] # All checked
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA" # **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **

# listing[listing$"Listing status"=="Delisted" & is.na(listing$"IPO date"),] # All checked
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA" # **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA" # **

# listing[listing$"Listing status"=="Delisted" & is.na(listing$"Delisting date"),] # All checked
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$BvD.ID.number=="NA"] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"

# parent[listing$"Listing status"=="Listed" & is.na(listing$"IPO date"),] # All checked
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# ** 
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA" # **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA" # **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA" # **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **

# parent[listing$"Listing status"=="Delisted" & is.na(listing$"IPO date"),] # All checked
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA" # **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **
listing$"IPO date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"	# **

# parent[listing$"Listing status"=="Delisted" & is.na(listing$"Delisting date"),] # All checked
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$BvD.ID.number=="NA"] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"
listing$"Delisting date"[listing$"Ticker symbol"=="NA" & !is.na(listing$"Ticker symbol")] <- "NA"

### Extract listing and delisting years
listing$IPO.year <- as.numeric(substr(listing$"IPO date", 1,4))
listing$Delisting.year <- as.numeric(substr(listing$"Delisting date", 1,4))

### Create indicator for listing status in EUTL database
ds$listed <- NA
for (i in unique(ds$Year)) {
	ds$listed[ds$Year==i & ds$bvdidnumber %in% listing$"BvD ID number"[listing$"Listing status"=="Listed" & listing$IPO.year<i & !is.na(listing$IPO.year)]] <- 1	#
	ds$listed[ds$Year==i & ds$bvdidnumber %in% listing$"BvD ID number"[listing$"Listing status"=="Delisted" & listing$IPO.year<i & !is.na(listing$IPO.year) & listing$Delisting.year>i & !is.na(listing$Delisting.year)]] <- 1	#

	ds$listed[ds$Year==i & ds$bvdidnumber %in% listing$"BvD ID number"[listing$"Listing status"=="Unlisted"]] <- 0
	ds$listed[ds$Year==i & ds$bvdidnumber %in% listing$"BvD ID number"[listing$"Listing status"=="Listed" & listing$IPO.year>i & !is.na(listing$IPO.year)]] <- 0	#
	ds$listed[ds$Year==i & ds$bvdidnumber %in% listing$"BvD ID number"[listing$"Listing status"=="Delisted" & listing$Delisting.year>i & !is.na(listing$Delisting.year)]] <- 0	#
}

save(ds, file = "Data/EUTL2021.Rdata")