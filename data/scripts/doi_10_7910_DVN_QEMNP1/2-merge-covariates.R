######################################################
# Policy Diffusion: The Issue-Definition Stage
# Fabrizio Gilardi, Charles R. Shipan & Bruno Wueest
# Merge covariates
# 2019-11-18
######################################################

rm(list = ls())

setwd("/Users/fgilardi/Downloads/AJPS-final/")

# sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.1

# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# loaded via a namespace (and not attached):
# [1] compiler_3.6.1


state_covars <- read.csv("./covariates/raw-data/state-covariates.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(state_covars)[1] <- "state"

newsp_covars <- read.table("./covariates/raw-data/newspaper-covariates.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

newsp_covars$state <- gsub("[[:space:]]", "", newsp_covars$state)
newsp_covars$newspaper <- gsub("[[:space:]]", "", newsp_covars$newspaper)
newsp_covars$year <- sapply(strsplit(newsp_covars$month, "-"), "[[", 1)

covars <- merge(state_covars, newsp_covars, by = c("state", "year"))
covars$newspaper[covars$newspaper == "SeattleTimes/Post-Intelligencer"] <- "PostIntelligencer"
covars$newspaper[covars$newspaper == "TopekaCapitalJournal"] <- "TheTopekaCapitalJournal"
covars$newspaper[covars$newspaper == "WilmingtonStarNews"] <- "MorningStar"
covars$newspaper[covars$newspaper == "BismarckTribune2"] <- "TheBismarckTribune"
covars$newspaper[covars$newspaper == "AustinAmerican-Statesman"] <- "AustinAmericanStatesman"
covars$newspaper[covars$newspaper == "RichmondTimes-Dispatch"] <- "RichmondTimesDispatch"

media_slant <- read.table("./covariates/raw-data/media-slant.txt", sep = "\t", header = F, stringsAsFactors = F, quote = "")
media_slant <- subset(media_slant , select = c("V1", "V4"))
colnames(media_slant) <- c("newspaper", "slant")

# both Forum and Democrat-Gazette are described as "Moderately conservative" on wikipedia, so about the value of the Wall Street Journal is added for them
mc <- mean(media_slant$slant)
media_slant <- rbind(media_slant, c("IdahoFallsPostRegister", mc))
media_slant <- rbind(media_slant, c("Democrat-Gazette", mc))
media_slant$slant <- as.numeric(media_slant$slant)
covars <- merge(covars, media_slant, by = "newspaper", all.x = T)

#standardize newspaper names
covars$newspaper[covars$newspaper == "AlbuquerqueJournal"] <- "Albuquerque Journal"
covars$newspaper[covars$newspaper == "AnchorageDailyNews"] <- "Anchorage Daily News"
covars$newspaper[covars$newspaper == "ArgusLeader"] <- "Argus Leader"
covars$newspaper[covars$newspaper == "ArizonaRepublic"] <- "Arizona Republic"
covars$newspaper[covars$newspaper == "AtlantaJournal-Constitution"] <- "Atlanta Journal-Constitution"
covars$newspaper[covars$newspaper == "AustinAmericanStatesman"] <- "Austin American-Statesman"
covars$newspaper[covars$newspaper == "BaltimoreSun"] <- "Baltimore Sun"
covars$newspaper[covars$newspaper == "BillingsGazette"] <- "Billings Gazette"
covars$newspaper[covars$newspaper == "BirminghamNews"] <- "Birmingham News"
covars$newspaper[covars$newspaper == "BostonGlobe"] <- "Boston Globe"
covars$newspaper[covars$newspaper == "CharlestonGazette"] <- "Charleston Gazette-Mail"
covars$newspaper[covars$newspaper == "ChicagoTribune"] <- "Chicago Tribune"
covars$newspaper[covars$newspaper == "ClarionLedger"] <- "Clarion-Ledger"
covars$newspaper[covars$newspaper == "Courier-Journal"] <- "Courier-Journal"
covars$newspaper[covars$newspaper == "DailyNews"] <- "Daily News"
covars$newspaper[covars$newspaper == "DailyOklahoman"] <- "Oklahoman"
covars$newspaper[covars$newspaper == "Democrat-Gazette"] <- "Democrat-Gazette"
covars$newspaper[covars$newspaper == "DenverPost"] <- "Denver Post"
covars$newspaper[covars$newspaper == "DesMoinesRegister"] <- "Des Moines Register"
covars$newspaper[covars$newspaper == "DetroitFreePress"] <- "Detroit Free Press"
covars$newspaper[covars$newspaper == "FreePress"] <- "Burlington Free Press"
covars$newspaper[covars$newspaper == "HartfordCourant"] <- "Hartford Courant"
covars$newspaper[covars$newspaper == "HonoluluAdvertiser"] <- "Honolulu Star-Advertiser"
covars$newspaper[covars$newspaper == "IdahoFallsPostRegister"] <- "Idaho Falls Post Register"
covars$newspaper[covars$newspaper == "IndianapolisStar"] <- "Indianapolis Star"
covars$newspaper[covars$newspaper == "JournalSentinel"] <- "Journal Sentinel"
covars$newspaper[covars$newspaper == "LasVegasReviewJournal"] <- "Las Vegas Review-Journal"
covars$newspaper[covars$newspaper == "LosAngelesTimes"] <- "Los Angeles Times"
covars$newspaper[covars$newspaper == "MorningStar"] <- "Wilmington Star-News"
covars$newspaper[covars$newspaper == "NewsJournal"] <- "The News Journal"
covars$newspaper[covars$newspaper == "NewYorkTimes"] <- "New York Times"
covars$newspaper[covars$newspaper == "OmahaWorld-Herald"] <- "Omaha World-Herald"
covars$newspaper[covars$newspaper == "Oregonian"] <- "Oregonian"
covars$newspaper[covars$newspaper == "PhiladelphiaInquirer"] <- "Philadelphia Inquirer"
covars$newspaper[covars$newspaper == "PlainDealer"] <- "Plain Dealer"
covars$newspaper[covars$newspaper == "PostIntelligencer"] <- "Seattle Times"
covars$newspaper[covars$newspaper == "PressHerald"] <- "Portland Press Herald"
covars$newspaper[covars$newspaper == "ProvidenceJournal"] <- "Providence Journal"
covars$newspaper[covars$newspaper == "Record"] <- "Record"
covars$newspaper[covars$newspaper == "RichmondTimesDispatch"] <- "Richmond Times-Dispatch"
covars$newspaper[covars$newspaper == "St.LouisPost-Dispatch"] <- "St.Louis Post-Dispatch"
covars$newspaper[covars$newspaper == "St.PetersburgTimes"] <- "Tampa Bay Times"
covars$newspaper[covars$newspaper == "StarTribuneMinneapolis"] <- "Star Tribune"
covars$newspaper[covars$newspaper == "Tennessean"] <- "Tennessean"
covars$newspaper[covars$newspaper == "TheBismarckTribune"] <- "Bismarck Tribune"
covars$newspaper[covars$newspaper == "TheState"] <- "State"
covars$newspaper[covars$newspaper == "TheTopekaCapitalJournal"] <- "Topeka Capital-Journal"
covars$newspaper[covars$newspaper == "Times-Picayune"] <- "Times-Picayune"
covars$newspaper[covars$newspaper == "Tribune-Eagle"] <- "Wyoming Tribune Eagle"
covars$newspaper[covars$newspaper == "Tribune/DeseretNews"] <- "Deseret News"
covars$newspaper[covars$newspaper == "UnionLeader"] <- "Union Leader"
covars$newspaper[covars$newspaper == "USAToday"] <- "USA Today"
covars$newspaper[covars$newspaper == "WallStreetJournal"] <- "Wall Street Journal"
covars$newspaper[covars$newspaper == "WashingtonPost"] <- "Washington Post"

covars$lowDemocrats <- NULL
covars$lowTotal <- NULL

bans_enacted <- read.csv("./covariates/raw-data/smokefree-policies-with-spatial-lags.csv")

# names(bans_enacted)
bans_enacted <- subset(bans_enacted, !Year %in% c(1994,1995,2014))
bans_enacted$month <- gsub("-01$", "", bans_enacted$Date)
vars <- c("State", "month", "Policy_Enacted_This_Month", "Policy_Months_Before_After_Enacted", "Policy_Spatial_Lag_Neighbors", "Policy_Spatial_Lag_Network_Pcent", "Policy_Spatial_Lag_All")
bans_enacted <- subset(bans_enacted, select = vars)
covars_enacted <- merge(covars, bans_enacted, by.x = c("state_abbr", "month"), by.y = c("State", "month"), all.x = T)
summary(covars_enacted)

write.csv(covars_enacted, "./covariates/covariates.csv", row.names = FALSE)
 