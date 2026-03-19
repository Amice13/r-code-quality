rm(list=ls())
library("XML")
library("methods")
library("plyr")

exports <- list.files("Data/EUTL2021/", full.names=TRUE)  # EUTL data exported on May 4, 2021, from https://ec.europa.eu/clima/ets/.

ds <- data.frame()
for (j in seq(exports)) {
	doc <- xmlParse(exports[j])
	d <- getNodeSet(doc, "//Compliance")
	d <- xmlToDataFrame(doc, nodes=d, stringsAsFactors = FALSE)	# Annual compliance information

	account = do.call("rbind", xpathApply(doc, "//Account", function(N) {		# Account holder-level information
	   data.frame(
		   InstallationOrAircraftOperatorID = xmlValue(N[["InstallationOrAircraftOperatorID"]]),
		   AccountHolderName = xmlValue(N[["AccountHolderName"]]),
		   NationalAdministrator = xmlValue(N[["NationalAdministrator"]]),
		   AccountTypeCode = xmlValue(N[["AccountTypeCode"]]),
		   AccountStatus = xmlValue(N[["AccountStatus"]]),
		   NationalAdministratorCode = xmlValue(N[["NationalAdministratorCode"]]),
		   AccountTypeCodeLookup = xmlValue(N[["AccountTypeCodeLookup"]])
		   )
	    }))
	account <- account[-1,]

	installation = do.call("rbind", xpathApply(doc, "//Installation", function(N) {		# Installation-level information
	   data.frame(
		   LastYearOfEmissions = xmlValue(N[["LastYearOfEmissions"]]),
		   PermitOrPlanDate = xmlValue(N[["PermitOrPlanDate"]]),
		   FirstYearOfEmissions = xmlValue(N[["FirstYearOfEmissions"]]),
	       InstallationNameOrAircraftOperatorCode = xmlValue(N[["InstallationNameOrAircraftOperatorCode"]]),
		   Address1 = xmlValue(N[["Address1"]]),
		   CallSign = xmlValue(N[["CallSign"]]),
		   MainActivityTypeCode = xmlValue(N[["MainActivityTypeCode"]]),
		   PermitOrPlanID = xmlValue(N[["PermitOrPlanID"]]),
		   City = xmlValue(N[["City"]]),
		   NationalAdministratorCode = xmlValue(N[["NationalAdministratorCode"]]),
		   InstallationOrAircraftOperatorID = xmlValue(N[["InstallationOrAircraftOperatorID"]]),
		   MainActivityTypeCodeLookup = xmlValue(N[["MainActivityTypeCodeLookup"]]),
		   ZipCode = xmlValue(N[["ZipCode"]])
		   )
	   }))

	person = do.call("rbind", xpathApply(doc, "//RelatedPerson", function(N) {		# Related person-level information
	   data.frame(
		   Name = xmlValue(N[["Name"]]),
		   CompanyRegistrationNo = xmlValue(N[["CompanyRegistrationNo"]]),
		   Address1 = xmlValue(N[["Address1"]]),
		   City = xmlValue(N[["City"]]),
		   RelationshipTypeCode = xmlValue(N[["RelationshipTypeCode"]]),
		   CountryCodeLookup = xmlValue(N[["CountryCodeLookup"]]),
		   CountryCode = xmlValue(N[["CountryCode"]]),
		   RelationshipTypeCodeLookup = xmlValue(N[["RelationshipTypeCodeLookup"]]),
		   ZipCode = xmlValue(N[["ZipCode"]])
		   )
	   }))
	for (i in seq(1,nrow(person),by=2)) {
	   	person$Address1[i] = person$Address1[i+1]
	   	person$City[i] = person$City[i+1]
	   	person$RelationshipTypeCode[i] = person$RelationshipTypeCode[i+1]
	   	person$CountryCodeLookup[i] = person$CountryCodeLookup[i+1]
	   	person$CountryCode[i] = person$CountryCode[i+1]
	   	person$RelationshipTypeCodeLookup[i] = person$RelationshipTypeCodeLookup[i+1]
	   	person$ZipCode[i] = person$ZipCode[i+1]
	}
	person <- person[seq(1,nrow(person),by=2),]

	x <- cbind(account, installation, person)
	x <- x[rep(1:nrow(x), each=length(unique(d$Year))),]
	x <- cbind(x, d)
	ds <- rbind.fill(ds, x)
}

# Clean up formatting
ds$ZipCode <- as.numeric(as.character(ds$ZipCode))
ds$NationalAdministratorCode <- as.character(ds$NationalAdministratorCode)
ds$MainActivityTypeCode<- as.numeric(as.character(ds$MainActivityTypeCode)) 
ds$Year <- as.numeric(ds$Year)
ds$AccountStatus <- as.character(ds$AccountStatus)
ds$FirstYearOfEmissions <- as.numeric(as.character(ds$FirstYearOfEmissions))
ds$LastYearOfEmissions <- as.numeric(as.character(ds$LastYearOfEmissions))	# Still emitting is coded as 0.
ds$AllowanceInAllocation <- as.numeric(ds$AllowanceInAllocation)
ds$CumulativeSurrenderedUnits <- as.numeric(ds$CumulativeSurrenderedUnits)
ds$CumulativeVerifiedEmissions <- as.numeric(ds$CumulativeVerifiedEmissions)
ds$VerifiedEmissions <- as.numeric(ds$VerifiedEmissions)
ds$UnitsSurrendered <- as.numeric(ds$UnitsSurrendered)
ds$SurrenderedAllowances <- as.numeric(ds$SurrenderedAllowances)

# Create corrected code for National administrator
ds$NationalAdministratorCodeCorrected <- ds$NationalAdministratorCode
ds$NationalAdministratorCodeCorrected[ds$NationalAdministratorCode=="XI"] <- "GB"	# Northern Ireland did not have it's own national administrator prior to Brexit.
ds$NationalAdministratorCodeCorrected[ds$NationalAdministratorCode=="BE" & ds$ZipCode %in% c(1000:1299)] <- "BE.Bru" # Belgium is administered by three regional administrators. Zip codes in this range indicate Brussels.
ds$NationalAdministratorCodeCorrected[ds$NationalAdministratorCode=="BE" & ds$ZipCode %in% c(1300:1499, 4000:7999)] <- "BE.W" # Belgium is administered by three regional administrators. Zip codes in this range indicate Wallonia.
ds$NationalAdministratorCodeCorrected[ds$NationalAdministratorCode=="BE" & ds$ZipCode %in% c(1500:3999, 8000:9999)] <- "BE.Fl" # Belgium is administered by three regional administrators. Zip codes in this range indicate Flanders.
ds$NationalAdministratorCodeCorrected[ds$NationalAdministratorCode=="BE" & ds$ZipCode==9999] <- "BE.W" # The nuclear plant in Tihange is in Wallonia, not Flanders, despite it having a zip code of 9999.

# Exclude aircraft operators
ds <- ds[ds$MainActivityTypeCode!=10,]

# Exclude countries prior to entry to the EU ETS
ds <- ds[!(ds$NationalAdministratorCodeCorrected %in% c("BG","RO") & ds$Year %in% 2005:2006),]		# Bulgaria and Romania entered the EU ETS in 2007.
ds <- ds[!(ds$NationalAdministratorCodeCorrected %in% c("NO","LI") & ds$Year %in% 2005:2007),]		# Norway and Liechtenstein entered EU ETS in 2008.
ds <- ds[!(ds$NationalAdministratorCodeCorrected %in% c("HR","IS") & ds$Year %in% 2005:2012),]		# Croatia entered EU ETS in 2013. Iceland officially entered earlier, but stationary installations only joined in 2013.

# Exclude Phase 4
ds <- ds[!(ds$ETSPhase=="2021-2030"),]

# Exclude installations prior to first year of operation and after account closure.
ds$EID <- paste(ds$NationalAdministratorCodeCorrected, ds$InstallationOrAircraftOperatorID)	# Create a European ID that uniquely identifies installations across Europe
for (i in unique(ds$EID)) {
	x <- ds[ds$EID==i,]
	y <- x$Year[!is.na(x$ComplianceCode) | !is.na(x$AllowanceInAllocation) | !is.na(x$CumulativeSurrenderedUnits) | !is.na(x$CumulativeVerifiedEmissions) | !is.na(x$VerifiedEmissions) | !is.na(x$UnitsSurrendered) | !is.na(x$SurrenderedAllowances)]	# Years with emissions-related data
	firstyear <- min(min(y, na.rm=TRUE), min(x$FirstYearOfEmissions, na.rm=TRUE), na.rm=TRUE)		# First year of operation
	lastyear <- max(max(y, na.rm=TRUE), max(x$LastYearOfEmissions, na.rm=TRUE), na.rm=TRUE)			# Last year of operation
	if (firstyear %in% 2005:2020) { ds <- ds[!(ds$EID==i & ds$Year<firstyear),]	}					# Exclude installations prior to first year of operation
	if (lastyear %in% 2005:2020) { ds <- ds[!(ds$EID==i & ds$Year>lastyear & (ds$AccountStatus %in% c("closed","Closure Pending"))),] }	# Exclude installations after account closure
}

save(ds, file = "Data/EUTL2021.Rdata")