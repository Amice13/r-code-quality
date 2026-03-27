# Code updated: 2021 April 27

#### DESCRIPTION ####
#
# Compiles data files using the resulting data compilations from "001_BG_DataPrep.R"
#
# Files:
# (1) A cross-section of households including:
#     - Residential characteristics such as irrigable area and household size - and an indicator if/when any unobserved change was made
#     - Selected details of the first WaterSmart report if one was sent to the household before 2016-06-12 (when some of control began treatment)
#     - Indicator for automated violation notice and indicator for any pre-treatment non-automated violation notice (through June 2015)
#     - The RD running variable in gallons and in recentered gallons (-124.5) with determination June 17-23, 2015
#     - An indicator for whether pre-treatment consumption is observed for the household (through 2015-04-30)
#
# (2) A daily panel of consumption measures using AMI data only: 2014-02-01 through 2016-09-30
#     - Total daily gallons; daily gallons pre-9AM or post-6PM
#
# (3) An unbalanced panel including all irrigation violation warnings in Burbank, by type (automated, 1st, 2nd)
#
# Notes:
# 1. In addition, all three analysis files include an indicator variable for whether the household is 'insample' using sample restrictions: 
#    1. Burbank household in WaterSmart 'experimental' or 'control' arms (b1, b0)
#    2. Nonmissing pretreatment consumption: first observed no later than 2014-04-01
#    3. Nonmissing posttreatment consumption: is observed during 2015-05-01 through 2015-10-31
#    4. Nonmissing running variable for irrigation violation warnings 
#

#### PREAMBLE ####
rm(list = ls()) # Clear workspace
wd = "/raid/data/WATER/data" ; setwd(wd) # * SET THIS * Directory containing compiled WaterSmart data
pl = c('data.table', 'fasttime', 'zoo') ; lapply(pl, require, character.only=T) # R packages to load

#### READ IN DATA ####

# Hourly consumption data
hourly <- fread(file.path(wd, 'Burbank_Hourly_Use.csv'))
hourly[, readdate := as.Date(fastPOSIXct(read_datetime, tz = 'PDT'))]
hourly[, readhour := as.integer(substr(read_datetime, 12, 13))]
hourly <- hourly[, .(residence_id, readdate, readhour, gallons)]

# Irrigation violation notices, both automated and manual
autoviol <- fread(file.path(wd, 'bwp_10232017', '3ormore_irrigation_days_2015_burbank_dev.csv'))
manualviol <- fread(file.path(wd, 'bwp_10232017', 'water_waste_details_burbank_dev.csv'))

# Residential characteristics 
residence <- fread(file.path(wd, 'BG_Arms_Residence_Details.csv'))
residence <- residence[city == 'Burbank'] 

# Occupancy changes (indicator and date)
cohortchgs <- fread(file.path(wd, 'bwp_11102016', 'occupant_changes_burbank_live.csv'))

# WaterSmart reports for treated households
reports <- fread(file.path(wd, 'BG_Arms_Reports_Recs.csv'))
reports <- reports[city == 'Burbank']

#### PROCESS FILES TO BE COMBINED INTO CROSS-SECTION OF BURBANK HOUSEHOLDS ####

# Subset residence file to a single record per household (currently contains multiple 'versions')
setkey(residence, residence_id, version) ; residence <- residence[, .SD[1], by = residence_id]
residence[, versiondate := as.Date(paste(substr(version, 1, 4), substr(version, 5, 6), substr(version, 7, 8), sep = '-'))]
table(residence$version, residence$arm) # Understandably all b0 and b1 are in the earliest version, 2015-10-13

# Define cohort bins
residence[`Irrigable Area (SqFt)` >= 0 & `Irrigable Area (SqFt)` <= 2000, irrigable_area_bucket_id := 1]
residence[`Irrigable Area (SqFt)` >= 2001 & `Irrigable Area (SqFt)` <= 3000, irrigable_area_bucket_id := 2]
residence[`Irrigable Area (SqFt)` >= 3001 & `Irrigable Area (SqFt)` <= 4000, irrigable_area_bucket_id := 3]
residence[`Irrigable Area (SqFt)` >= 4001 & `Irrigable Area (SqFt)` <= 5000, irrigable_area_bucket_id := 4]
residence[`Irrigable Area (SqFt)` >= 5001 & `Irrigable Area (SqFt)` <= 999999999, irrigable_area_bucket_id := 5]
residence[, occupant_bucket_id := ifelse(`Num Occupants` > 5, 5, `Num Occupants`)]

# Determine residences that made change to occupancy before we observe the value (2015-10-13 in most cases)
table(cohortchgs$question_code) # Ignore 10 lot changes total
cohortchgs <- cohortchgs[question_code == 'numberOccupants', 
                         .(residence_id, changeoccdate = as.Date(substr(response_timestamp, 1, 10), format = '%Y-%m-%d'))]
setkey(cohortchgs, residence_id) ; setkey(residence, residence_id)
residence <- merge(residence, cohortchgs, all.x = T)

residence[, changedocc := ifelse(is.na(changeoccdate) | changeoccdate > versiondate, 0, 1)]
table(residence$changedocc) # 1249 changes
table(residence$changedocc, residence$arm) # 40 in b0, 1047 in b1 (1.32 and 7.34% respectively)
rm(cohortchgs)

# Subset variables to keep in residence file: 
#   drop {city, version, fee_code, Occupants Source, Irrigable Area Source, Residence Type, Has Logged In, versiondate}
residence <- residence[, .(residence_id, arm, `Num Occupants`, `Home Size (SqFt)`, `Num Floors`, `Lot Size (SqFt)`, 
                           `Irrigable Area (SqFt)`, `Year Home Built`, `Num Bedrooms`, `Num Bathrooms`, 
                           irrigable_area_bucket_id, occupant_bucket_id, changeoccdate, changedocc)]

# Subset variables in WaterSmart reports, keeping first report for each household only
reports[,  wsdate := as.Date(substr(send_timestamp, 1, 10))]
# Test that version doesn't matter for content: TRUE
length(unique(reports[,.(residence_id, wsdate)], by = NULL)$residence_id) == length(unique(reports[,.(residence_id, wsdate, version)], by = NULL)$residence_id)
reports <- unique(reports[!is.na(wsdate), .(residence_id, arm, wsdate, waterscore, wsgpd = gpd)], by = NULL)
setkey(reports, residence_id, wsdate)
reports <- reports[, .SD[1], by = .(residence_id)] # Keep only first report for each treated household
reports <- reports[wsdate < '2016-06-12'] # Keep only reports before 2016-06-12 (when some b0 were treated)

# Compile (unbalanced) panel of irrigation violation warnings by type
autoviol <- autoviol[, .(residence_id, violdate = as.Date('2015-07-01'), violtype = 'Automated', violnotice = 1)]
manualviol[, violdate := as.Date(`created on`, format = '%m/%d/%y')]
manualviol[type == 'Water Waste - First Notice', violtype := 'Manual: 1st notice']
manualviol[type == 'Water Waste - Second Notice ', violtype := 'Manual: 2nd notice']
manualviol <- manualviol[!is.na(violtype), .(residence_id, violdate, violtype, violnotice = 1)]
violations <- rbind(autoviol, manualviol) ; rm(autoviol, manualviol) ; setkey(violations, residence_id, violdate, violtype)

# Compute RD running variable
runcalc <- hourly[readdate >= '2015-06-17' & readdate <= '2015-06-23' & !is.na(gallons)]
setkey(runcalc, residence_id, readdate, gallons)
runcalc <- runcalc[, .SD[.N], by = .(residence_id, readdate)] # Peak hour per day
setkey(runcalc, residence_id, gallons) ; runcalc <- runcalc[, .SD[.N-1], by = residence_id] # 2nd-highest day's peak hour
runcalc <- runcalc[, .(residence_id, hasrunningv = 1, runninggal = gallons, runningv = gallons - 124.5)]

#### COMPILE PANEL OF DAILY CONSUMPTION MEASURES ####

# AMI data only 2014-02-01 through 2016-09-30: total daily gallons and daily gallons pre-9AM / post-6PM

#summary(hourly$readdate)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2014-01-13" "2014-09-20" "2015-05-20" "2015-05-24" "2016-01-29" "2016-10-05" 

setkey(hourly, residence_id, readdate, readhour)
hourly[, b9gal := 0L][, a6gal := 0]
hourly[readhour < 9, b9gal := gallons]
hourly[readhour >= 18, a6gal := gallons]
daily <- hourly[readdate >= '2014-02-01' & readdate <= '2016-09-30', 
                .(dailygal = sum(gallons), b9gal = sum(b9gal), a6gal = sum(a6gal)), by = .(residence_id, readdate)]
setkey(daily, residence_id, readdate)
rm(hourly)

#### MERGE CROSS-SECTION FILES ####

# 1. Residence details
# 2. WaterSmart reports (initial report for household)
# 3. Whether sent automated irrigation violation notice and whether sent pre-treatment violation notice (through June 2015)
# 4. RD running variable in gallons and in recentered gallons (-124.5)
# 5. Indicator for whether household has pre-treatment consumption: first observed no later than 2014-04-01

# Merge residence to WaterSmart reports
setkey(residence, residence_id, arm) ; setkey(reports, residence_id, arm)
cs <- merge(residence, reports, all = T)
cs[, everws := ifelse(is.na(waterscore), 0, 1)] # Whether household was ever sent WaterSmart report (before 2016-06-12)
table(cs$everws, cs$arm)

# Merge residence and reports to violation notices
cs.violations <- copy(violations)
cs.violations[, autoviol := ifelse(violtype == 'Automated', 1, 0)]
cs.violations[, pretviol := ifelse(violdate < '2015-07-01', 1, 0)]
cs.violations <- cs.violations[, .(autoviol = max(autoviol), pretviol = max(pretviol)), by = residence_id]
setkey(cs.violations, residence_id) ; setkey(cs, residence_id)
cs <- merge(cs, cs.violations, all.x = T)
cs[is.na(autoviol), autoviol := 0L]
cs[is.na(pretviol), pretviol := 0L]

summary(lm(pretviol~as.factor(arm), data  = cs))
summary(lm(pretviol~as.factor(arm), data  = cs[autoviol == 1]))

# Merge residence, reports, and violation notices to running variable
setkey(runcalc, residence_id)
cs <- merge(cs, runcalc, all = T)
cs[is.na(hasrunningv), hasrunningv := 0L]

summary(lm(hasrunningv ~ as.factor(arm), data = cs))

# Merge residence, reports, violation notices, and running variable to indicator for whether pre-treatment consumption is observed
pretcon <- daily[readdate < '2015-04-01', .(residence_id, readdate)]
pretcon[, minpredate := min(readdate), by = .(residence_id)]
pretcon[, maxpredate := max(readdate), by = .(residence_id)]
pretcon <- unique(pretcon[minpredate <= '2014-04-01' & maxpredate >= '2014-04-01', .(residence_id, hasprecon = 1L)], by = NULL)
setkey(pretcon, residence_id)
cs <- merge(cs, pretcon, all.x = T)
cs[is.na(hasprecon), hasprecon := 0L]
cs[residence_id == 10699, hasprecon := 0L] # This one residence has a gap in consumption spanning 2014-04-26 through 2015-06-08
table(cs$hasprecon, cs$arm) # 28 households in b0 and 160 in b1 do not have observed pre-April 2015 consumption

rm(residence, reports, cs.violations, runcalc, pretcon)

#### DEFINE IN-SAMPLE INDICATOR FOR THE THREE ANALYSIS FILES ####

# Define 'insample' indicator
#    1. Burbank household in WaterSmart 'experimental' or 'control' arms (b1, b0)
#    2. Nonmissing pretreatment consumption: first observed no later than 2014-04-01
#    3. Nonmissing posttreatment consumption: is observed during 2015-05-01 through 2015-10-31
#    4. Nonmissing running variable for irrigation violation warnings 

# Determine households that have nonmissing post-treatment consumption: is observed during 2015-05-01 through 2015-10-31

postcon <- unique(daily[readdate >= '2015-05-01' & readdate <= '2015-10-31', .(residence_id, haspostcon = 1L)], by = NULL)
setkey(postcon, residence_id)
setkey(cs, residence_id)
cs <- merge(cs, postcon, all.x = T)
cs[is.na(haspostcon), haspostcon := 0L]
rm(postcon)

# Define insample indicator
cs[, insample := as.integer(ifelse(arm %in% c('b0', 'b1') & hasprecon == 1 & haspostcon == 1 & hasrunningv == 1, 1, 0))]
table(cs$insample, cs$arm)
prop.table(table(cs$insample, cs$arm), 2)
summary(lm(insample ~ as.factor(arm), data = cs[arm %in% c('b0', 'b1')]))

# Merge 'insample' and arm to violations panel
insam <- cs[, .(residence_id, insample, arm)] ; setkey(insam, residence_id)
setkey(violations, residence_id) ; violations <- merge(violations, insam, all.x = T)
setcolorder(violations, c('residence_id', 'insample', 'arm', 'violdate', 'violtype', 'violnotice'))
violations[is.na(insample), insample := 0L]
violations <- violations[!is.na(arm)] # There are 67 residence_id that do not have any arm, including 'bNA'

# Merge 'insample' and arm to daily consumption panel
setkey(daily, residence_id) ; daily <- merge(daily, insam, all.x = T)
setcolorder(daily, c('residence_id', 'insample', 'arm', 'readdate', 'dailygal', 'b9gal', 'a6gal'))
rm(insam)

# Set column order for cross-sectional analysis file
setcolorder(cs, c('residence_id', 'insample', 'arm', 'hasprecon', 'haspostcon', 'hasrunningv', 'runningv', 'runninggal', 'autoviol', 'pretviol', 
                  'everws', 'wsdate', 'waterscore', 'wsgpd', 'irrigable_area_bucket_id', 'occupant_bucket_id', 'changedocc', 'changeoccdate', 
                  'Irrigable Area (SqFt)', 'Num Occupants', 'Home Size (SqFt)', 'Lot Size (SqFt)', 'Num Floors', 'Year Home Built', 
                  'Num Bedrooms', 'Num Bathrooms' ))

#### SAVE FILES AS .csv ####

fwrite(cs, file.path(wd, 'Analysis_Burbank_Cross_Section.csv'))
fwrite(violations, file.path(wd, 'Analysis_Burbank_Violations.csv'))
fwrite(daily, file.path(wd, 'Analysis_Burbank_Daily_Consumption.csv'))

## END
