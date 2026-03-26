# ### Replication Data for "Longer Trips to Court Cause Evictions" by Hoffman and Strezhnev
### Philadelphia

### Packages

# Tidyverse
library(tidyverse)
library(ggplot2)
library(haven)
library(lubridate)
library(estimatr)
library(viridis)
library(fixest)

# Geocoding/mapping
library(estimatr)
library(geosphere)
library(sf)
library(jsonlite)
library(esri2sf)
library(gridExtra)
library(xtable)
library(ggiraph)
library(ggiraphExtra)
library(rgdal)
library(ggpubr)
library(ggstar)
library(Xmisc)
library(texreg)
library(hexbin)


# BinScatter
source("stat_binscatter.R")


################################
## Load in case level data + commuting distances
summary_table_full <- read_csv("Data/summary_table_philadelphia.csv")

############################
## Geography stuff
###########################

### Hexbin code from Andrew Wheeler
### https://andrewpwheeler.com/2019/08/07/making-a-hexbin-map-in-ggplot/

#get width given height
wd_hex <- function(height){
  tri_side <- height/2
  sma_side <- height/4
  width <- 2*sqrt(tri_side^2 - sma_side^2)
  return(width)
}

#now to figure out the area if you want
#side is simply height/2 in geom_hex
hex_area <- function(side){
  area <- 6 * (  (sqrt(3)*side^2)/4 )
  return(area)
}

#So if you want your hexagon to have a regular area need the inverse function
#Gives height and width if you want a specific area
hex_dim <- function(area){
  num <- 4*area
  den <- 6*sqrt(3)
  vert <- 2*sqrt(num/den)
  horz <- wd_hex(vert)
  return(c(vert,horz))
}

## Truncated mean function - returns mean if vector length is above minBin otherwise NA
mean_trunc <- function(x, minBin=25){
  if (length(x) >= minBin){
    return(mean(x))
  }else{
    return(NA)
  }
}

# Mapping theme
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank()
    #panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

## For plotting downtown as a star
pmc_dataframe = data.frame(latitude = 39.950010, longitude = -75.157070)

### Read in the philly map
phillyMap <- read_sf(dsn="Data/Census Tracts", layer="Census_Tracts_2010")
phillyMap$TRACT11 <- paste(phillyMap$STATEFP10, phillyMap$COUNTYFP10, phillyMap$TRACTCE10, sep="")

### Read in the philly block map
phillyBlockMap <- read_sf(dsn="Data/Census Block", layer="Census_Block")
phillyBlockMap <- st_make_valid(phillyBlockMap)
phillyBlockMap$BLOCK_ID <- paste("1000000US", phillyBlockMap$GEOID10, sep="")

### Get point-in-shape google tracts
summary_table_geo <- st_as_sf(summary_table_full, coords=c("google_longitude", "google_latitude"), crs="WGS84")
summary_join <- st_join(summary_table_geo, phillyMap)

# Join on Blocks
summary_block_join <- st_join(summary_table_geo, phillyBlockMap)

# Fixed the Tracts
summary_table_full$TRACT11 <- summary_join$TRACT11
summary_table_full$BLOCK_ID <- summary_block_join$BLOCK_ID

rm(summary_block_join, summary_table_geo, summary_join)

##########################################
### Appendix 1 - Distribution of pew properties vs. properties w/ evictions

## Summary data file (number of pew properties per tract; number matched to our dataset)
pew_summary <- read_csv("Data/pew_summary.csv")
pew_summary$TRACT11 <- as.character(pew_summary$TRACT11)

# Join to map
phillyMapGeo <-  phillyMap %>% left_join(pew_summary, by = "TRACT11")

# suppress if < 25 pew properties
phillyMapGeo$numEviction[phillyMapGeo$numPew < 25] <- NA
phillyMapGeo$shareFound[phillyMapGeo$numPew < 25] <- NA
phillyMapGeo$numPew[phillyMapGeo$numPew < 25] <- NA

## Generate maps from the appendix
philly_plot_pew <- phillyMapGeo  %>% ggplot() +
  geom_sf(aes(fill=numPew)) + scale_fill_viridis("Count of properties\n(Pew data)", na.value="white", option = "cividis") +
  mapTheme()

ggsave("Output/Figures/Appendix/a1_pew_dist.pdf", plot=philly_plot_pew, width=6, height=5)

philly_plot_final_data <- phillyMapGeo  %>% ggplot() +
  geom_sf(aes(fill=numEviction)) + scale_fill_viridis("Count of properties\n(Final eviction dataset)", na.value="white", option = "cividis") +
  mapTheme()

ggsave("Output/Figures/Appendix/a1_final_data_dist.pdf", plot=philly_plot_final_data, width=6, height=5)

philly_plot_share_data <- phillyMapGeo  %>% ggplot() +
  geom_sf(aes(fill=shareFound)) + scale_fill_viridis("Share of Pew\nproperties found", na.value="white", option = "cividis") +
  mapTheme()

ggsave("Output/Figures/Appendix/a1_share_data_dist.pdf", plot=philly_plot_share_data, width=6, height=5)

#########################
## Load in ACS Data
#########################

### Census Block race
census_race <- read_csv("Data/ACS/Block/2010_block_race.csv")
census_race <- census_race %>% select(BLOCK_ID = GEO_ID, totalpopBlock = P001001, totalwhiteBlock = P001003, totalblackBlock = P001004)

summary_table_full <- merge(summary_table_full, census_race, by="BLOCK_ID", all.x=T)
summary_table_full$shareWhiteBlock = summary_table_full$totalwhiteBlock/summary_table_full$totalpopBlock
summary_table_full$shareBlackBlock = summary_table_full$totalblackBlock/summary_table_full$totalpopBlock

### Census Block Hispanic
census_hispanic <- read_csv("Data/ACS/Block/2010_block_hispanic.csv")
census_hispanic <- census_hispanic %>% select(BLOCK_ID = GEO_ID, totalpopBlockHispanic = P002001, totalHispanicBlock = P002002)

summary_table_full <- merge(summary_table_full, census_hispanic, by="BLOCK_ID", all.x=T)
summary_table_full$shareHispanicBlock = summary_table_full$totalHispanicBlock/summary_table_full$totalpopBlockHispanic

### ACS Income
acs_income <- read_csv("Data/ACS/acs_income.csv", na = c("", "NA", "-"))

## Grab median household income
acs_household_income <- acs_income %>% select(TRACT11=GEO.id2, medianIncome=HC01_EST_VC13)

### ACS Housing value
acs_housing <- read_csv("Data/ACS/acs_housing.csv",na = c("", "NA", "-"))

## Grab median household income
acs_housing_info <- acs_housing %>% select(TRACT11=GEO.id2, medianGrossRent =HC01_VC191, numRenterOccupied = HC01_VC66)

### ACS
acs_contract <- read_csv("Data/ACS/acs_contractRent.csv",na = c("", "NA", "-"))

## Grab median household income
acs_contract_info <- acs_contract%>% select(TRACT11=GEO.id2, medianContractRent =HD01_VD01)

### ACS Race
acs_race <- read_csv("Data/ACS/acs_race.csv",na = c("", "NA", "-"))

## Grab median household income
acs_race_subset <- acs_race %>% select(TRACT11=GEO.id2, totalPopulation =HC01_VC48,  whitepct = HC03_VC54, blackpct = HC03_VC55, hispanicpct = HC03_VC93)

############################
## Merge ACS with individual-level evictions
summary_table_full <- merge(summary_table_full, acs_household_income, by="TRACT11", all.x=T)
summary_table_full <- merge(summary_table_full, acs_race_subset, by="TRACT11", all.x=T)
summary_table_full <- merge(summary_table_full, acs_contract_info, by="TRACT11", all.x=T)
summary_table_full <- merge(summary_table_full, acs_housing_info, by="TRACT11", all.x=T)

summary_table_full <- summary_table_full %>% filter(!is.na(summary_table_full$medianIncome)&!is.na(summary_table_full$medianContractRent)&!is.na(summary_table_full$whitepct)&!is.na(summary_table_full$shareWhiteBlock))

###################
## Check in on sample sizes
nrow(summary_table_full)
length(unique(summary_table_full$pm.building))
length(unique(summary_table_full$LandlordG))

###########################
### Get the defendant-level outcomes from the docket itself

defendant_outcomes_full <- read_csv("Data/docket_defendant_outcomes.csv")

### Recode outcomes based on what happened into larger categories
defendant_outcomes_full <- defendant_outcomes_full %>% mutate(outcomeCombined = case_when(
  grepl("Dismissed", outcome, ignore.case=T) ~ "Dismissed",
  grepl("Withdrawn", outcome, ignore.case=T) ~ "Withdrawn",
  grepl("Settled", outcome, ignore.case=T) ~ "Settled",
  grepl("by Agreement", outcome, ignore.case=T) ~ "Judgment by Agreement",
  grepl("Mediation Agreement", outcome, ignore.case=T) ~ "Judgment by Agreement",
  grepl("Judgment for Plaintiff by default",outcome, ignore.case=T) ~ "For Plaintiff by Default",
  grepl("Judgment for Defendant by default",outcome, ignore.case=T) ~ "For Defendant by Default",
  grepl("Judgment for Plaintiff",outcome, ignore.case=T) ~ "For Plaintiff",
  grepl("Judgment for Defendant",outcome, ignore.case=T) ~ "For Defendant",
  grepl("Judgment for Possession Only", outcome, ignore.case=T) ~ "For Defendant",
  grepl("Judgment for Rent Only", outcome, ignore.case=T) ~ "For Defendant",
  outcome == "Disposition - Action Withdrawn Judgment Stands" ~ "Other",
  outcome == "Disposition - Adjudication Deferred" ~ "Other",
  outcome == "Disposition - Deferred due to Bankruptcy filing" ~ "Other",
  outcome == "Disposition - Held Under Advisement" ~ "Other",
  outcome == "No Disposition" ~ "Other",
  outcome == "Disposition - Unusual Event" ~ "Other",
  outcome == "Disposition - Resolved by Arbitration Order" ~ "Other",
  grepl("Resolved by Arbitration Order", outcome, ignore.case=T) ~ "Other",
  outcome == "Disposition - Dismiss Additional Service Address" ~ "Other",
  outcome == "Disposition - Case to be Consolidated" ~ "Other",
  outcome == "Disposition - Case Amended, See Remarks:" ~ "Other",
  outcome == "Disposition - Fine Not Paid" ~ "Other",
  TRUE ~ "ERROR: MISSING"))

### Merge to summary data
defendant_outcomes_merged <- defendant_outcomes_full %>% left_join(summary_table_full, by = "id")

### Fix rent
defendant_outcomes_merged$ongoing_rent[defendant_outcomes_merged$ongoing_rent ==11650.00] <- 1165
defendant_outcomes_merged$ongoing_rent[defendant_outcomes_merged$ongoing_rent ==9500.00] <- 950

## Drop if we don't have driving duration
defendant_outcomes_merged <- defendant_outcomes_merged %>% filter(!is.na(driving_duration))
defendant_outcomes_merged <- as.data.frame(defendant_outcomes_merged)

# Drop if we don't have the complaint
defendant_outcomes_merged <- defendant_outcomes_merged %>% filter(!is.na(complaint))

length(unique(defendant_outcomes_merged$id))
length(unique(defendant_outcomes_merged$pm.building))
length(unique(defendant_outcomes_merged$Landlord))
nrow(defendant_outcomes_merged)

rm(defendant_outcomes_full)

###################################################################
#### Code the outcome data

### Figure out the settlement outcomes
defendant_outcomes_merged <- defendant_outcomes_merged %>% mutate(outcomePossession = case_when(outcomeCombined == "Dismissed" ~ "No decision: No judgment for possession",
                                                                                                outcomeCombined == "Withdrawn" ~ "No decision: No judgment for possession",
                                                                                                outcomeCombined == "Settled" ~ "No decision: No judgment for possession",
                                                                                                TRUE ~ "OTHER"))

### If default for plaintiff, is there possession
defendant_outcomes_merged$outcomePossession[defendant_outcomes_merged$outcomeCombined == "For Plaintiff by Default"] <- ifelse(!grepl("money judgment", defendant_outcomes_merged$details[defendant_outcomes_merged$outcomeCombined == "For Plaintiff by Default"], ignore.case=T),
                                                                                                                               "Plaintiff wins: Judgment for possession", "Plaintiff wins: No judgment for possession")

### If decision for plaintiff, is there possession
defendant_outcomes_merged$outcomePossession[defendant_outcomes_merged$outcomeCombined == "For Plaintiff"]  <- ifelse(!grepl("money judgment", defendant_outcomes_merged$details[defendant_outcomes_merged$outcomeCombined == "For Plaintiff"], ignore.case=T),
                                                                                                                     "Plaintiff wins: Judgment for possession", "Plaintiff wins: No judgment for possession")

### If defendant wins - no possession
defendant_outcomes_merged$outcomePossession[defendant_outcomes_merged$outcomeCombined == "For Defendant"] <- "Defendant wins: No judgment for possession"
defendant_outcomes_merged$outcomePossession[defendant_outcomes_merged$outcomeCombined == "For Defendant by Default"] <- "Defendant wins: No judgment for possession"

### If it's a judgment by agreement?
defendant_outcomes_merged$outcomePossession[defendant_outcomes_merged$outcomeCombined == "Judgment by Agreement"]  <- ifelse(grepl("possession", defendant_outcomes_merged$details[defendant_outcomes_merged$outcomeCombined == "Judgment by Agreement"], ignore.case=T),
                                                                                                                             "Agreement: Judgment for possession", "Agreement: No judgment for possession")

### Binary - was there possession
defendant_outcomes_merged$outcomePossessionBin <- ifelse(grepl("no judgment", defendant_outcomes_merged$outcomePossession, ignore.case=T), 0, 1)

### Binary - did plaintiff win
defendant_outcomes_merged$plaintiffWin <- ifelse(grepl("Plaintiff wins", defendant_outcomes_merged$outcomePossession, ignore.case=T), 1, 0)

### Binary - did plaintiff win by default
defendant_outcomes_merged$plaintiffWinDefault <- ifelse(grepl("For Plaintiff by Default", defendant_outcomes_merged$outcomeCombined, ignore.case=T), 1, 0)

### Was there a settlement
defendant_outcomes_merged$jba <- ifelse(grepl("Judgment by Agreement", defendant_outcomes_merged$outcomeCombined, ignore.case=T), 1, 0)

### Did the defendant win?
defendant_outcomes_merged$defendantWin <- ifelse(grepl("Defendant wins", defendant_outcomes_merged$outcomePossession, ignore.case=T), 1, 0)

### Did the defendant win by default?
defendant_outcomes_merged$defendantWinDefault <- ifelse(grepl("For Defendant by Default", defendant_outcomes_merged$outcomeCombined, ignore.case=T), 1, 0)

### Was the case withdrawn
defendant_outcomes_merged$noDecision <- ifelse(grepl("No decision", defendant_outcomes_merged$outcomePossession, ignore.case=T), 1, 0)

### Was the case withdrawn
defendant_outcomes_merged$caseWithdrawn <- ifelse(grepl("Withdrawn", defendant_outcomes_merged$outcomeCombined, ignore.case=T), 1, 0)

### Was a writ issued?
defendant_outcomes_merged$writ <- as.numeric(defendant_outcomes_merged$writIssued)

### Was an alias writ issued?
defendant_outcomes_merged$aliasWrit <- as.numeric(defendant_outcomes_merged$aliasWritIssued)

defendant_outcomes_merged$aliasServed <- as.numeric(defendant_outcomes_merged$aliasWritServed)

# What are the other outcomes
defendant_outcomes_merged$otheroutcome <- as.numeric(defendant_outcomes_merged$jba == 0&defendant_outcomes_merged$caseWithdrawn == 0&defendant_outcomes_merged$plaintiffWinDefault == 0)

###########################
## Rent via ongoing rent
###########################

defendant_outcomes_merged$ongoing_rent_recoded <- defendant_outcomes_merged$ongoing_rent
defendant_outcomes_merged$ongoing_rent_recoded[defendant_outcomes_merged$ongoing_rent_recoded >= 10000] <- 0
defendant_outcomes_merged$ongoing_rent_recoded[is.na(defendant_outcomes_merged$ongoing_rent_recoded)] <- 0

defendant_outcomes_merged$ongoing_rent_missing <- as.numeric(defendant_outcomes_merged$ongoing_rent_recoded == 0)

########################################
### Hearing times
#######################################

##########
## Get the hearing date and time
defendant_outcomes_merged$time <- regmatches(defendant_outcomes_merged$complaint, regexpr("Hearing Scheduled: [0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9] [0-9][0-9]?:[0-9][0-9] [A-Z][A-Z]", defendant_outcomes_merged$complaint, perl=T))

defendant_outcomes_merged$hourtime <- regmatches(defendant_outcomes_merged$time, regexpr("[0-9][0-9]?:[0-9][0-9] [A-Z][A-Z]", defendant_outcomes_merged$time, perl=T))
defendant_outcomes_merged$hourtime <- lstrip(defendant_outcomes_merged$hourtime, "0")

defendant_outcomes_merged$hearingdate <- mdy(regmatches(defendant_outcomes_merged$time, regexpr("[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]", defendant_outcomes_merged$time, perl=T)))

# 8:45 call time
defendant_outcomes_merged <- defendant_outcomes_merged %>% mutate(earlymorning = case_when(grepl("8:45 AM", hourtime) ~ 1,
                                                                                           TRUE ~ 0))

defendant_outcomes_merged$monthHearing <- as.factor(month(defendant_outcomes_merged$hearingdate))
defendant_outcomes_merged$yearHearing <- as.factor(year(defendant_outcomes_merged$hearingdate))
defendant_outcomes_merged$monthYearHearing = paste(defendant_outcomes_merged$monthHearing, defendant_outcomes_merged$yearHearing, sep="-")

####################################################
## Summary of the defendant outcomes data
nrow(defendant_outcomes_merged)
length(unique(defendant_outcomes_merged$id))
length(unique(defendant_outcomes_merged$pm.building))
length(unique(defendant_outcomes_merged$Landlord))

#######################################################
### Landlord summaries

landlord_counts <- defendant_outcomes_merged %>% filter(!duplicated(id)) %>% filter(publichousing == 0) %>% group_by(LandlordG) %>% summarize(nCases = n(), nBuildings = length(unique(pm.building)))
landlord_counts$evictionPerBuilding <- landlord_counts$nCases/landlord_counts$nBuildings

###################################################
### Philadelphia Municipal Court location:
pmc_latitude = 39.950010
pmc_longitude = -75.157070

## Calculate distance from Philadelphia Municipal Court
defendant_outcomes_merged$distToCourt <- distGeo(p1=defendant_outcomes_merged[,c("google_longitude","google_latitude")], p2=c(pmc_longitude, pmc_latitude))

## Calculate 1 KM rings
defendant_outcomes_merged$distKM <- defendant_outcomes_merged$distToCourt/1000

## Ring
defendant_outcomes_merged$distRing <- floor(defendant_outcomes_merged$distKM)
defendant_outcomes_merged$distRing2k <- floor(defendant_outcomes_merged$distKM/2)
defendant_outcomes_merged$distRing5k <- floor(defendant_outcomes_merged$distKM/5)

## Ring
defendant_outcomes_merged$transitDistRing <- floor(defendant_outcomes_merged$transit_distance/1000)
defendant_outcomes_merged$transitDistRing2k <- floor(defendant_outcomes_merged$transit_distance/2000)
defendant_outcomes_merged$transitDistRing5k <- floor(defendant_outcomes_merged$transit_distance/5000)

## Ring
defendant_outcomes_merged$drivingDistRing <- floor(defendant_outcomes_merged$driving_distance/1000)
defendant_outcomes_merged$drivingDistRing2k <- floor(defendant_outcomes_merged$driving_distance/2000)
defendant_outcomes_merged$drivingDistRing5k <- floor(defendant_outcomes_merged$driving_distance/5000)

## Strata
defendant_outcomes_merged <- defendant_outcomes_merged  %>% mutate(distStrata = case_when(distRing5k == 0 ~ "0-5km",
                                                                  distRing5k == 1 ~ "5-10km",
                                                                  distRing5k  == 2 ~ "10-15km",
                                                                  distRing5k >= 3 ~ "15+km"))
defendant_outcomes_merged$distStrata <- factor(defendant_outcomes_merged$distStrata, levels=c("0-5km", "5-10km", "10-15km","15+km"))


## Transit to minutes
defendant_outcomes_merged <- defendant_outcomes_merged %>% mutate(transit_duration_min = transit_duration/60,
                                                                  driving_duration_min = driving_duration/60,
                                                                  transit_duration_weekend_min = transit_duration_weekend/60)


### Split pre- and post-covid
defendant_outcomes_merged <- defendant_outcomes_merged %>% filter(hearingdate < ymd("2021-12-31"))
defendant_outcomes_merged <- defendant_outcomes_merged %>% mutate(precovid = case_when(d_filing < ymd("2020-04-01") ~ 1,
                                                                                       d_filing > ymd("2020-04-01") ~ 0,
                                                                                       TRUE ~ NA_real_))

### Summarize data counts
nrow(defendant_outcomes_merged)
length(unique(defendant_outcomes_merged$id))
length(unique(defendant_outcomes_merged$pm.building))

### Correlation between transit and driving durations
cor(defendant_outcomes_merged$transit_duration_min, defendant_outcomes_merged$driving_duration_min)

# Drop if no outcome date or if outcome date is completely off (prior to hearing date)
defendant_outcomes_merged <-  defendant_outcomes_merged %>% filter(!is.na(defendant_outcomes_merged$outcomeDate)) %>% filter(!(outcomeDate < hearingdate)) %>%
  filter(!(d_filing > hearingdate))

# What type of housing?
defendant_outcomes_merged <- defendant_outcomes_merged %>% mutate(`Housing Type` = case_when(publichousing ~ "Philadelphia Housing\nAuthority",
                                                                                             !publichousing ~ "Non-Public"))

# Summarize sample sizes
nrow(defendant_outcomes_merged)
length(unique(defendant_outcomes_merged$id))
length(unique(defendant_outcomes_merged$pm.building))
length(unique(defendant_outcomes_merged$LandlordG))


###############################
## Commuting histogram

defendant_outcomes_merged %>% ggplot(aes(x=transit_duration_min)) + geom_histogram() +
  theme_bw() + xlab("Estimated transit commuting time to\nPhiladelphia Municipal Courthouse") + ylab("Number of defendants")

ggsave("Output/Figures/commuting_histogram_transit.pdf", width=4, height=4)


##########################################################################################
### Case outcomes

defendant_outcomes_merged  %>%
  count(outcomeCombined = factor(outcomeCombined)) %>%
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x = fct_reorder(outcomeCombined,-pct), y = pct, label = scales::percent(pct))) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  scale_y_continuous(labels = scales::percent) + theme_bw() + xlab("") +
  ylab("Percentage of judgments") + theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))

ggsave("Output/Figures/eviction_outcomes_summary.pdf", width=5, height=5)

###################################
### Alias writ served
defendant_outcomes_merged  %>% group_by(outcomeCombined) %>% summarize(aliasWrit = mean(aliasServed)) %>%
  ggplot(aes(x = fct_reorder(outcomeCombined,-aliasWrit), y = aliasWrit, label = scales::percent(aliasWrit))) +
  geom_col(position = 'dodge') +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  scale_y_continuous(labels = scales::percent) + theme_bw() + xlab("") +
  ylab("Share of cases with served alias writs of possession") + theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))

ggsave("Output/Figures/Appendix/a6_eviction_outcomes_aliaswritserved.pdf", width=5, height=5)

########################################
# Time to writs
# Fix incorrect date
defendant_outcomes_merged$aliasWritServedDate[defendant_outcomes_merged$aliasWritServedDate == ymd("2206-05-30")] <- ymd("2006-05-30")

defendant_outcomes_merged <- defendant_outcomes_merged %>% mutate(timeToWrit = as.numeric(aliasWritServedDate - outcomeDate))

defendant_outcomes_merged  %>% group_by(outcomeCombined) %>% filter(timeToWrit >= 0&!is.na(timeToWrit)) %>% summarize(timeToWrit = mean(timeToWrit)) %>%
  ggplot(aes(x = fct_reorder(outcomeCombined,timeToWrit), y = timeToWrit), label=scales::comma(timeToWrit)) +
  geom_col(position = 'dodge') +
  #geom_text(position = position_dodge(width = .9),    # move to center of bars
  #          vjust = -0.5,    # nudge above top of bar
  #          size = 3) +
  scale_y_continuous(labels = scales::comma) + theme_bw() + xlab("") +
  ylab("Average number of days to alias writ served") + theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))

ggsave("Output/Figures/Appendix/a6_eviction_outcomes_time_to_served.pdf", width=5.5, height=5)

################################

###########################################################################################
## Pre-covid summaries:

defendant_outcomes_merged %>% group_by(precovid) %>% summarize(mean(plaintiffWinDefault))
nrow(defendant_outcomes_merged %>% filter(precovid ==1))
length(unique(defendant_outcomes_merged %>% filter(precovid == 1) %>% pull(id)))
length(unique(defendant_outcomes_merged %>% filter(precovid == 1&publichousing==0) %>% pull(id)))

# Plot defaults over time
defendant_outcomes_merged %>% filter(precovid ==1) %>% ggplot(aes(x=hearingdate, y=plaintiffWinDefault, colour=`Housing Type`)) + geom_smooth(se=F, span=1) + stat_binscatter() +
  theme_bw() + xlab("Date of first hearing") + ylab("Share of cases with default judgments\nagainst the defendant") + scale_colour_manual(values=c("indianred", "dodgerblue")) +
  theme(legend.position="bottom", legend.box = "horizontal") + guides(color = guide_legend(title.position="top", title.hjust = 0.5))

ggsave("Output/Figures/default_rate_time.pdf", width=5, height=4)

##########################
## More covid-period summary statistics (filings + zoom hearings)

length(unique(defendant_outcomes_merged %>% filter(precovid == 0&d_filing < ymd("2021-1-1"))  %>% pull(id)))
length(unique(defendant_outcomes_merged %>% filter(precovid == 1&d_filing >= ymd("2019-7-1")&d_filing < ymd("2020-1-1"))  %>% pull(id)))

defendant_outcomes_merged %>% filter(precovid==0) %>% filter(!duplicated(id)) %>% summarize(mean(zoom_hearing), mean(plaintiffWinDefault))

###############################################
## Subset more of the data for the regressions!

## Convert transit durations into other increments
defendant_outcomes_merged$transit_duration_10min <- defendant_outcomes_merged$transit_duration_min/10
defendant_outcomes_merged$transit_duration_hours <- defendant_outcomes_merged$transit_duration_min/60

defendant_outcomes_merged$transit_duration_weekend_hours <- defendant_outcomes_merged$transit_duration_weekend_min/60

## Only use those cases where the date of the hearing is the date at which we observe an outcome.
defendant_outcomes_use <- defendant_outcomes_merged %>% filter(!is.na(transit_duration))  %>% filter(hearingdate == outcomeDate) 

## Summarize number of defendants/cases
nrow(defendant_outcomes_use)
length(unique(defendant_outcomes_use$id))

############################################

### Subset down to non-public housing
defendant_outcomes_use <- defendant_outcomes_merged %>% filter(!is.na(transit_duration))  %>% filter(hearingdate == outcomeDate) %>% filter(publichousing == 0) %>%
  filter(hearingdate == outcomeDate)  %>%
  filter(!is.na(log(medianIncome))&!is.na(log(medianContractRent))&!is.na(log(totalPopulation))&!is.na(whitepct)&!is.na(log(distToCourt))&!is.na(first_date_heard)&!is.na(bld_typ))

# Covid and non-covid
defendant_outcomes_covid <- defendant_outcomes_use %>% filter(precovid == 0)
defendant_outcomes_use_precovid <- defendant_outcomes_use %>% filter(precovid==1)

### Subset down to public housing
defendant_outcomes_public <- defendant_outcomes_merged %>% filter(!is.na(transit_duration))  %>% filter(hearingdate == outcomeDate) %>% filter(publichousing == 1) %>%
  filter(hearingdate == outcomeDate)  %>%
  filter(!is.na(log(medianIncome))&!is.na(log(medianContractRent))&!is.na(log(totalPopulation))&!is.na(whitepct)&!is.na(log(distToCourt))&!is.na(first_date_heard)&!is.na(bld_typ))

# Covid and non-covid
defendant_outcomes_public_covid <- defendant_outcomes_public %>% filter(precovid == 0)
defendant_outcomes_public_precovid <- defendant_outcomes_public %>% filter(precovid==1)

###########################################################3
## Sample size summaries

# Pre-covid, non-public
nrow(defendant_outcomes_use_precovid)
length(unique(defendant_outcomes_use_precovid$id))
length(unique(defendant_outcomes_use_precovid$pm.building))
length(unique(defendant_outcomes_use_precovid$LandlordG))

# Covid, non-public
nrow(defendant_outcomes_covid)
length(unique(defendant_outcomes_covid$id))
length(unique(defendant_outcomes_covid$pm.building))
length(unique(defendant_outcomes_covid$LandlordG))

# Pre-covid, public
nrow(defendant_outcomes_public_precovid)
length(unique(defendant_outcomes_public_precovid$id))
length(unique(defendant_outcomes_public_precovid$pm.building))
length(unique(defendant_outcomes_public_precovid$LandlordG))

# How much of the total data is precovid?
nrow(defendant_outcomes_merged %>% filter(precovid ==1))
length(unique(defendant_outcomes_merged %>% filter(precovid == 1) %>% pull(id)))

# How common are reopening petitions
mean(defendant_outcomes_use$reopen_petition)

# How common are zoom hearings?
mean(defendant_outcomes_covid$zoom_hearing)

#####################################################

# Regressions with weekend data (pre-covid)
defendant_outcomes_use_precovid_weekend <- defendant_outcomes_use_precovid %>% filter(!is.na(weekend_gap))
defendant_outcomes_use_precovid_weekend$transit_diff <- defendant_outcomes_use_precovid_weekend$transit_duration_hours - defendant_outcomes_use_precovid_weekend$transit_duration_weekend_hours

nrow(defendant_outcomes_use_precovid_weekend)
length(unique(defendant_outcomes_use_precovid_weekend$id))
length(unique(defendant_outcomes_use_precovid_weekend$pm.building))
length(unique(defendant_outcomes_use_precovid_weekend$LandlordG))


#################################
## Additional descriptive statistics - Appendix 4 
##################################

# Median income

tract_rates <- defendant_outcomes_use_precovid %>% group_by(TRACT11) %>% summarize(n=n(), default_rate = mean(plaintiffWinDefault),
                                                                                   whitepct = median(whitepct),
                                                                                   medianIncome = median(medianIncome),
                                                                                   commuteTime = median(transit_duration_min),
                                                                                   distance = median(distKM))

tract_rates %>% filter(n >= 10) %>% ggplot(aes(x=medianIncome, y=default_rate)) + geom_smooth(method="loess") + stat_binscatter() + theme_bw()+ scale_x_log10(labels=scales::comma) +
  xlab("Census Tract Median Income") + ylab("Share of cases with defaults\njudgments against the defendant")

ggsave("Output/Figures/Appendix/a4_default_by_income.pdf", width=5, height=4)

tract_rates %>% filter(n >= 10) %>% ggplot(aes(x=medianIncome, y=commuteTime)) + geom_smooth(method="loess") + stat_binscatter() + theme_bw() + scale_x_log10(labels=scales::comma) +
  xlab("Census Tract Median Income") + ylab("Median tract commuting time to\nPhiladelphia Municipal Courthouse (minutes)")

ggsave("Output/Figures/Appendix/a4_commute_by_income.pdf", width=5, height=4)


### Race

block_rates <- defendant_outcomes_use_precovid  %>% group_by(BLOCK_ID) %>% summarize(n=n(), default_rate = mean(plaintiffWinDefault),
                                                                                     whitepct = median(shareWhiteBlock),
                                                                                     blackpct = median(shareBlackBlock),
                                                                                     hispanicpct = median(shareHispanicBlock),
                                                                                     commuteTime = median(transit_duration_min),
                                                                                     distance = median(distKM))

block_rates %>% ggplot(aes(x=whitepct, y=default_rate)) + geom_smooth(method="loess") + stat_binscatter() + theme_bw()+ scale_x_continuous(labels=scales::comma) +
  xlab("Census Block % White") + ylab("Share of cases with default\njudgments against the defendant")

ggsave("Output/Figures/Appendix/a4_default_by_race_block.pdf", width=5, height=4)


block_rates %>% filter(n>=10) %>% ggplot(aes(x=whitepct, y=commuteTime)) + geom_smooth(method="loess") + stat_binscatter() + theme_bw()+ scale_x_continuous(labels=scales::comma) +
  xlab("Census Tract % White") + ylab("Median block commuting time to\nPhiladelphia Municipal Courthouse (minutes)")

ggsave("Output/Figures/Appendix/a4_commute_by_race_block.pdf", width=5, height=4)


block_rates %>% ggplot(aes(x=blackpct, y=default_rate)) + geom_smooth(method="loess") + stat_binscatter() + theme_bw()+ scale_x_continuous(labels=scales::comma) +
  xlab("Census Block % Black") + ylab("Share of cases with default\njudgments against the defendant")

ggsave("Output/Figures/Appendix/a4_default_by_race_black_block.pdf", width=5, height=4)

block_rates %>% filter(n>=10) %>% ggplot(aes(x=blackpct, y=commuteTime)) + geom_smooth(method="loess") + stat_binscatter() + theme_bw()+ scale_x_continuous(labels=scales::comma) +
  xlab("Census Tract % Black") + ylab("Median block commuting time to\nPhiladelphia Municipal Courthouse (minutes)")

ggsave("Output/Figures/Appendix/a4_commute_by_race_black_block.pdf", width=5, height=4)


block_rates %>% ggplot(aes(x=hispanicpct, y=default_rate)) + geom_smooth(method="loess") + stat_binscatter() + theme_bw()+ scale_x_continuous(labels=scales::comma) +
  xlab("Census Block % Hispanic") + ylab("Share of cases with default\njudgments against the defendant")

ggsave("Output/Figures/Appendix/a4_default_by_hispanic_block.pdf", width=5, height=4)


block_rates %>% filter(n>=10) %>% ggplot(aes(x=hispanicpct, y=commuteTime)) + geom_smooth(method="loess") + stat_binscatter() + theme_bw()+ scale_x_continuous(labels=scales::comma) +
  xlab("Census Tract % Hispanic") + ylab("Median block commuting time to\nPhiladelphia Municipal Courthouse (minutes)")

ggsave("Output/Figures/Appendix/a4_commute_by_hispanic_block.pdf", width=5, height=4)

### Apartments
building_rates <- defendant_outcomes_use_precovid  %>% group_by(pm.building) %>% summarize(n=n(), default_rate = mean(plaintiffWinDefault),
                                                                                           apartment = as.numeric(bld_typ[1] == 6|bld_typ[1]==7|bld_typ[1]==8),
                                                                                           hispanicpct = median(shareHispanicBlock),
                                                                                           commuteTime = median(transit_duration_min),
                                                                                           distance = median(distKM))

building_rates %>% ggplot(aes(x=commuteTime, y=apartment)) + geom_smooth() + stat_binscatter() + theme_bw()+ scale_x_continuous(labels=scales::comma) +
  xlab("Median commuting time to\nPhiladelphia Municipal Courthouse (minutes)") + ylab("Share of apartments")

ggsave("Output/Figures/Appendix/a4_commute_by_apartment.pdf", width=5, height=4)

lm_robust(default_rate ~ apartment, data=building_rates)

building_rates %>% ggplot(aes(y=default_rate, x=apartment)) + geom_col() +
  xlab("Apartment building") + ylab("Share of cases with defaults\njudgments against the defendant")

ggsave("Output/Figures/Appendix/a4_default_by_apartment.pdf", width=5, height=4)


### Ongoing rent
defendant_outcomes_use_precovid %>% filter(ongoing_rent_missing != 1) %>% ggplot(aes(y= transit_duration_min, x = ongoing_rent_recoded))  +
  geom_smooth(method="lm_robust")  + stat_binscatter(scalefactor=1e-4, scalepoints = T) +
  xlab("Ongoing rent ($50-$5000)") + ylab("Commuting time to\nPhiladelphia Municipal Courthouse (minutes)")  + theme_bw()

ggsave("Output/Figures/Appendix/a4_commute_by_rent.pdf", width=5, height=3.25)

defendant_outcomes_use_precovid %>% filter(ongoing_rent_missing != 1) %>% ggplot(aes(y= plaintiffWinDefault, x = ongoing_rent_recoded))  +
  geom_smooth(method="lm_robust") + stat_binscatter(scalefactor=1e-4, scalepoints = T) +
  xlab("Ongoing rent ($50-$5000)") + ylab("Probability of Plaintiff Default")  + theme_bw()

ggsave("Output/Figures/Appendix/a4_default_by_rent.pdf", width=5, height=3.25)

###########################################
########## Reopening petitions (Appendix 3)
###########################################

defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1) %>% summarize(n(), sum(reopen_petition), sum(reopen_outcome == "Granted", na.rm=T))

defendant_outcomes_use_precovid %>% filter(precovid ==1&plaintiffWinDefault==1) %>% ggplot(aes(x=hearingdate, y=reopen_petition)) + geom_smooth(se=F) + stat_binscatter() +
  theme_bw() + xlab("Date of first hearing") + ylab("Share of defaulting defendants filing petitions to reopen") + ggtitle("Reopening filing")

ggsave("Output/Figures/Appendix/a3_reopen_rate_time.pdf", width=4, height=4)

defendant_outcomes_use_precovid %>% filter(precovid ==1&plaintiffWinDefault==1&reopen_petition==1) %>% ggplot(aes(x=hearingdate, y=as.numeric(reopen_outcome == "Granted"))) + geom_smooth(se=F) + stat_binscatter() +
  theme_bw() + xlab("Date of first hearing") + ylab("Share of reopen petitions granted") + ggtitle("Reopening success")

ggsave("Output/Figures/Appendix/a3_reopen_success_rate_time.pdf", width=4, height=4)

# Some regressions for reopening (initiation + success)
reg_1_reopen = lm_robust(reopen_petition ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing +  ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                          shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                        fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault==1), cluster = pm.building,
                        se_type = "CR0")

reg_1_reopen_success = lm_robust(I(reopen_outcome == "Granted"&reopen_petition==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) +  ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                           shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                         fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault==1), cluster = pm.building,
                         se_type = "CR0")

results_plot_reopen <- bind_rows(
  tidy(reg_1_reopen) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Reopening filing and success\nPre-Covid, non-public housing\ntenant default",  model = "Petition to reopen", nObs = nrow(defendant_outcomes_use_precovid  %>% filter(plaintiffWinDefault==1)),
                                                                             nBuilding = length(unique(defendant_outcomes_use_precovid  %>% filter(plaintiffWinDefault==1) %>% pull(pm.building)))),
  tidy(reg_1_reopen_success) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Reopening filing and success\nPre-Covid, non-public housing\ntenant default", model = "Successful\npetition to reopen", nObs = nrow(defendant_outcomes_use_precovid  %>% filter(plaintiffWinDefault==1)),
                                                                                     nBuilding = length(unique(defendant_outcomes_use_precovid  %>% filter(plaintiffWinDefault==1) %>% pull(pm.building)))))


results_plot_reopen$sample_full <- paste(results_plot_reopen$sample, "\nN Tenants = ", results_plot_reopen$nObs, ", N Buildings = ", results_plot_reopen$nBuilding, sep="")

results_plot_reopen$color <- ifelse(results_plot_reopen$p.value < .05, "black", "grey50")

reopen_plot <- results_plot_reopen %>% ggplot(aes(y=estimate, ymin=conf.low, ymax=conf.high, x=model, colour = color))  + geom_hline(yintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() + facet_wrap(~sample_full, scales="free", ncol=2) + theme_bw() + xlab("") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("Estimated effect of a 1 hour increase in commuting time")

ggsave("Output/Figures/Appendix/a3_reopen_results.pdf", plot = reopen_plot, width=5, height=5)

#######################################
### Date and Time of hearings (summary)
nrow(defendant_outcomes_merged %>% filter(!duplicated(id)) %>% filter(precovid == 1&publichousing == 0))
defendant_outcomes_merged %>% filter(!duplicated(id)) %>% filter(precovid == 1&publichousing == 0) %>% summarize(mean(earlymorning))

######################################################################
### Commuting times

### Make a hex map of commuting times
philly_plot_transit <- phillyMap %>%  ggplot() +
  geom_sf() + stat_summary_hex(aes(x= google_longitude, y=google_latitude, z=transit_duration_min),alpha=.85, color=NA, linetype = "blank", size=NA,
                               binwidth=hex_dim(0.008983^2), fun="mean", data=defendant_outcomes_use_precovid) +
  geom_star(aes(x = longitude, y=latitude), data=pmc_dataframe, color="red", fill="red", size=3) +
  scale_fill_viridis("Commuting Time by Transit (mins)") + mapTheme() +theme(legend.position="bottom")

philly_plot_transit
ggsave("Output/Figures/commuting_map_transit_precovid.pdf", width=6, height=6)

### Make a hex map of default times
philly_plot_transit2 <- phillyMap %>%  ggplot() +
  geom_sf() + stat_summary_hex(aes(x= google_longitude, y=google_latitude, z=plaintiffWinDefault),alpha=.85, color=NA, linetype = "blank", size=NA,
                               binwidth=hex_dim((1*0.008983)^2), fun="mean_trunc", data=defendant_outcomes_use_precovid ) +
  geom_star(aes(x = longitude, y=latitude), data=pmc_dataframe, color="red", fill="red", size=3) +
  scale_fill_viridis("Default rate", limits = c(0.3, .7), oob = scales::squish) + mapTheme() +theme(legend.position="bottom")

philly_plot_transit2

ggsave("Output/Figures/default_map_precovid.pdf", width=6, height=6)

########################################
## Appendix 5: Weekday vs. weekend commuting times

### Histogram of the weekday/weekend gap
philly_plot_transit_histogram <- defendant_outcomes_use_precovid_weekend %>% ggplot(aes(x=transit_diff*60)) + geom_histogram() + geom_vline(xintercept=0, lty=2, col="red") +
  theme_bw() + xlab("Difference in weekday vs. weekend commuting time (mins)") + ylab("Number of defendants")

ggsave("Output/Figures/Appendix/a5_gap_histogram.pdf", plot=philly_plot_transit_histogram, width=6, height=6)

# What's the share of evictions where there's any difference in transit between weekday/weekend
mean(defendant_outcomes_use_precovid_weekend$transit_diff == 0)

# Make it a map
philly_plot_transit_gap <- phillyMap %>%  ggplot() +
  geom_sf() + stat_summary_hex(aes(x= google_longitude, y=google_latitude, z=transit_diff*60),alpha=.85, color=NA, linetype = "blank", size=NA,
                               binwidth=hex_dim(0.008983^2), fun="mean", data=defendant_outcomes_use_precovid_weekend) +
  geom_star(aes(x = longitude, y=latitude), data=pmc_dataframe, color="red", fill="red", size=3) +
  scale_fill_viridis("Difference in weekday vs. weekend commuting time (mins)") + mapTheme() +theme(legend.position="bottom")

philly_plot_transit_gap

ggsave("Output/Figures/Appendix/a5_commuting_map_transit_precovid_gap.pdf", width=6, height=6)


##########################################################################################
##########################################################################################
### Regression analysis
##########################################################################################
##########################################################################################

### Get colors from colorbrewer
palette_use <- RColorBrewer::brewer.pal(4, "Set1")

# Assign labels to the distance bins
distance_bin_labels <- c(paste("0-5km\n(N = ", sum(defendant_outcomes_use_precovid$distStrata == "0-5km"), ")", sep=""),
                         paste("5-10km\n(N = ", sum(defendant_outcomes_use_precovid$distStrata == "5-10km"), ")", sep=""),
                         paste("10-15km\n(N = ", sum(defendant_outcomes_use_precovid$distStrata == "10-15km"), ")", sep=""),
                         paste("15+km\n(N = ", sum(defendant_outcomes_use_precovid$distStrata == "15+km"), ")", sep=""))


### Simple bivariate regression of defaults on commute
defendant_outcomes_use_precovid %>% ggplot(aes(y= plaintiffWinDefault, x = transit_duration_min, colour = palette_use[2])) + scale_colour_identity() +
  geom_smooth(method = "lm_robust", lwd = 2, method.args = list(cluster = defendant_outcomes_use_precovid$pm.building, se_type = "CR0")) + stat_binscatter(scalefactor=1e-4, scalepoints = T) + coord_cartesian(ylim=c(.35, .55)) +
  xlab("Transit commuting time (minutes))") + ylab("Probability of Plaintiff Default")  + theme_bw()

ggsave("Output/Figures/simple_bivariate_reg.pdf", width=5, height=3.25)

### Regression of defaults on commute within 5km-wide rings
defendant_outcomes_use_precovid %>% ggplot(aes(y= plaintiffWinDefault, x = transit_duration_min, color = as.factor(distStrata))) +
 stat_binscatter(bins=5, scalefactor=2e-4, scalepoints = T) + scale_colour_manual("Transit Distance", values = palette_use, labels=distance_bin_labels) +
  geom_smooth(alpha = .2, lwd = 2, fill = palette_use[1],  colour = palette_use[1], method = "lm_robust", method.args = list(cluster = (defendant_outcomes_use_precovid %>% filter(distStrata == "0-5km"))$pm.building, se_type = "CR0"), data = defendant_outcomes_use_precovid %>% filter(distStrata == "0-5km")) +
  geom_smooth(alpha = .2, lwd = 2, fill = palette_use[2],  colour = palette_use[2], method = "lm_robust", method.args = list(cluster = (defendant_outcomes_use_precovid %>% filter(distStrata == "5-10km"))$pm.building, se_type = "CR0"), data = defendant_outcomes_use_precovid %>% filter(distStrata == "5-10km")) +
  geom_smooth(alpha = .2, lwd = 2, fill = palette_use[3],  colour = palette_use[3], method = "lm_robust", method.args = list(cluster = (defendant_outcomes_use_precovid %>% filter(distStrata == "10-15km"))$pm.building, se_type = "CR0"), data = defendant_outcomes_use_precovid %>% filter(distStrata == "10-15km")) +
  geom_smooth(alpha = .2, lwd = 2, fill = palette_use[4],  colour = palette_use[4], method = "lm_robust", method.args = list(cluster = (defendant_outcomes_use_precovid %>% filter(distStrata == "15+km"))$pm.building, se_type = "CR0"), data = defendant_outcomes_use_precovid %>% filter(distStrata == "15+km")) +
  xlab("Transit commuting time (minutes)") + ylab("Probability of Default judgment for Landlord") + theme_bw() +
  theme(legend.position="bottom", legend.box = "horizontal") + guides(color = guide_legend(title.position="top", title.hjust = 0.5))

ggsave("Output/Figures/bivariate_by_ring.pdf", width=5, height=4)

### Alias writs served
defendant_outcomes_use_precovid <- defendant_outcomes_use_precovid %>% mutate(compressedOutcome = case_when(plaintiffWinDefault == 1 ~ "Default",
                                                                                                            jba == 1 ~ "JBA",
                                                                                                            caseWithdrawn == 1 ~ "Withdrawn"))

defendant_outcomes_use_precovid %>% filter(!is.na(compressedOutcome)) %>% ggplot(aes(y= aliasServed, x = transit_duration_min, color = as.factor(compressedOutcome))) +
  stat_binscatter(bins=5, scalefactor=2e-4, scalepoints = T) + scale_colour_manual("Judgment", values = palette_use, labels=c("Defendant\nDefaults","Judgment by\nAgreement","Case\nWithdrawn")) +
  geom_smooth(alpha = .2, lwd = 2, fill = palette_use[1],  colour = palette_use[1], method = "lm_robust", method.args = list(cluster = (defendant_outcomes_use_precovid %>% filter(!is.na(compressedOutcome)&compressedOutcome=="Default"))$pm.building, se_type = "CR0"), data = defendant_outcomes_use_precovid %>% filter(!is.na(compressedOutcome)&compressedOutcome=="Default")) +
  geom_smooth(alpha = .2, lwd = 2, fill = palette_use[2],  colour = palette_use[2], method = "lm_robust", method.args = list(cluster = (defendant_outcomes_use_precovid %>% filter(!is.na(compressedOutcome)&compressedOutcome=="JBA"))$pm.building, se_type = "CR0"), data = defendant_outcomes_use_precovid %>% filter(!is.na(compressedOutcome)&compressedOutcome=="JBA")) +
  geom_smooth(alpha = .2, lwd = 2, fill = palette_use[3],  colour = palette_use[3], method = "lm_robust", method.args = list(cluster = (defendant_outcomes_use_precovid %>% filter(!is.na(compressedOutcome)&compressedOutcome=="Withdrawn"))$pm.building, se_type = "CR0"), data = defendant_outcomes_use_precovid %>% filter(!is.na(compressedOutcome)&compressedOutcome=="Withdrawn")) +
  xlab("Transit commuting time (minutes)") + ylab("Probability of Alias Writ Served") + theme_bw() +
  theme(legend.position="bottom", legend.box = "horizontal") + guides(color = guide_legend(title.position="top", title.hjust = 0.5))

ggsave("Output/Figures/Appendix/a6_alias_writ_served_by_commute.pdf", width=5, height=4)

######################################################
## Main regression results
######################################################

# For fixest - use only 1 thread
setFixest_nthreads(nthreads=1, save = FALSE)

# No controls
reg_0_nocontrols = feols(plaintiffWinDefault ~ transit_duration_hours, data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Controls
reg_1_plain = feols(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                          shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing),
                    data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Ring fixed-effects
reg_2_ringFE = feols(plaintiffWinDefault  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                           shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k),
                         data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Landlord fixed-effects
reg_3_landlordFE = feols(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                             data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc() # Garbage collect

#################
### Main results, regression tables
texreg(list(extract(reg_0_nocontrols, include.proj.stats = F),extract(reg_1_plain, include.proj.stats=F),extract(reg_2_ringFE, include.proj.stats=F), extract(reg_3_landlordFE, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)


# Placebo: Public housing
reg_0_nocontrols_public = feols(plaintiffWinDefault ~ transit_duration_hours, data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_public = feols(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8)| as.factor(monthYearHearing), 
                           data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_public = feols(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                  shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                            data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

gc()

# Placebo, public housing regression tables
texreg(list(extract(reg_0_nocontrols_public, include.proj.stats = F),extract(reg_1_plain_public, include.proj.stats=F),extract(reg_2_ringFE_public, include.proj.stats=F)),
       stars=c(.01, .05), digits = 4, include.ci=F)

# Placebo: Covid
reg_0_nocontrols_covid = feols(plaintiffWinDefault ~ transit_duration_hours, data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_covid = feols(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), 
                          data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_covid = feols(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                           data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_landlordFE_covid = feols(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) |  as.factor(monthYearHearing) + as.factor(LandlordG), 
                               data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

# Regression tables
texreg(list(extract(reg_0_nocontrols_covid, include.proj.stats = F),extract(reg_1_plain_covid, include.proj.stats=F),extract(reg_2_ringFE_covid, include.proj.stats=F), extract(reg_3_landlordFE_covid, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

# Regressions with weekend
reg_0_weekend_nocontrols = feols(plaintiffWinDefault ~ transit_duration_hours + transit_duration_weekend_hours, data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_weekend_plain = feols(plaintiffWinDefault ~  transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                          shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_weekend_ringFE = feols(plaintiffWinDefault ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                           shareWhiteBlock + I(shareWhiteBlock^2)  + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                           data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_weekend_landlordFE = feols(plaintiffWinDefault ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                               data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

texreg(list(extract(reg_0_weekend_nocontrols, include.proj.stats = F),extract(reg_1_weekend_plain, include.proj.stats=F),extract(reg_2_weekend_ringFE, include.proj.stats=F), extract(reg_3_weekend_landlordFE , include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

#####################################
### Make combined plot
results_plot_main <- bind_rows(
tidy(reg_0_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid),
                                                                               nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
tidy(reg_1_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                               nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
tidy(reg_2_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid),
                                                                          nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
tidy(reg_3_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                           nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
tidy(reg_0_nocontrols_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "No Controls", nObs = nrow(defendant_outcomes_public_precovid),
                                                                               nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
tidy(reg_1_plain_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_public_precovid),
                                                                          nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
tidy(reg_2_ringFE_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_public_precovid),
                                                                           nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
tidy(reg_0_nocontrols_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_covid),
                                                                                      nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
tidy(reg_1_plain_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_covid),
                                                                                 nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
tidy(reg_2_ringFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_covid),
                                                                                  nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
tidy(reg_3_landlordFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_covid),
                                                                                 nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
tidy(reg_0_weekend_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                     nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
tidy(reg_1_weekend_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
tidy(reg_2_weekend_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
tidy(reg_3_weekend_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                     nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building)))

)

## Make Asymptotic CIs
results_plot_main$conf.low <- results_plot_main$estimate - abs(qnorm(.025))*results_plot_main$std.error
results_plot_main$conf.high <- results_plot_main$estimate + abs(qnorm(.025))*results_plot_main$std.error

results_plot_main$sample_full <- paste(results_plot_main$sample, "\nN Tenants = ", results_plot_main$nObs, ", N Buildings = ", results_plot_main$nBuilding, sep="")

results_plot_main$model <- factor(results_plot_main$model, levels=rev(c("No Controls", "Controls +\nMonth-Year FE", "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)",  "Controls +\nMonth-Year FE +\nLandlord FE")))
results_plot_main$color <- ifelse(results_plot_main$p.value < .05, "black", "grey50")

main_plot <- results_plot_main %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() + facet_wrap(~sample_full, scales="free", ncol=1) + theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of default") + ylab("")

ggsave("Output/Figures/main_results_and_placebos.pdf", plot = main_plot, width=5, height=9)

##########################################
### For presentations - separate the facets out

subset_plot <- results_plot_main %>% filter(sample == "Main Analysis\nPre-Covid, non-public housing") %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() + facet_wrap(~sample_full, scales="free", ncol=1) +  theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0, size=10)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of default") + ylab("") 

ggsave("Output/Figures/Presentation/main_results_presentation.pdf", plot = subset_plot, width=5, height=4)

subset_plot <- results_plot_main %>% filter(sample == "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing") %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() +  facet_wrap(~sample_full, scales="free", ncol=1) +  theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0, size=10)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of default") + ylab("") 

ggsave("Output/Figures/Presentation/main_results_presentation_weekend.pdf", plot = subset_plot, width=5, height=4)

subset_plot <- results_plot_main %>% filter(sample == "Placebo\nPost-Covid, non-public housing") %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() +  facet_wrap(~sample_full, scales="free", ncol=1) +  theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0, size=10)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of default") + ylab("") 

ggsave("Output/Figures/Presentation/main_results_presentation_covidplacebo.pdf", plot = subset_plot, width=5, height=4)

subset_plot <- results_plot_main %>% filter(sample == "Placebo\nPre-Covid, public housing") %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() +  facet_wrap(~sample_full, scales="free", ncol=1) +  theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0, size=10)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of default") + ylab("") 

ggsave("Output/Figures/Presentation/main_results_presentation_phplacebo.pdf", plot = subset_plot, width=5, height=4)

######################################################################################

## Prediction task
defendant_outcomes_use_precovid$transit_duration_hours_min = sapply(defendant_outcomes_use_precovid$transit_duration_hours, function (x) min(x, 1/6))

defendant_outcomes_use_precovid$transit_duration_hours_chg = defendant_outcomes_use_precovid$transit_duration_hours_min - defendant_outcomes_use_precovid$transit_duration_hours

defendant_outcomes_use_precovid$effect_transit = coef(reg_3_landlordFE)[1]*defendant_outcomes_use_precovid$transit_duration_hours_chg
defendant_outcomes_use_precovid$effect_transit2 = coef(reg_1_plain)[1]*defendant_outcomes_use_precovid$transit_duration_hours_chg
mean(defendant_outcomes_use_precovid$effect_transit)
sum(defendant_outcomes_use_precovid$effect_transit)

mean(defendant_outcomes_use_precovid$effect_transit2)
sum(defendant_outcomes_use_precovid$effect_transit2)

##################
## Heterogeneity by distance
##################

reg_transit_0_5km = lm_robust(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                              fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(distStrata == "0-5km"), cluster = pm.building,
                              se_type = "CR0")
reg_transit_5_10km = lm_robust(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                               fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(distStrata == "5-10km"), cluster = pm.building,
                               se_type = "CR0")
reg_transit_10_15km = lm_robust(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                  shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                                fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(distStrata == "10-15km"), cluster = pm.building,
                                se_type = "CR0")
reg_transit_15km = lm_robust(plaintiffWinDefault  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                             fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(distStrata == "15+km"), cluster = pm.building,
                             se_type = "CR0")


results_plot_strata <- results_plot_main <- bind_rows(
  tidy(reg_transit_0_5km) %>% filter(term == "transit_duration_hours") %>% mutate(group = "1) 0-5 KM", nObs = nrow(defendant_outcomes_use_precovid %>% filter(distStrata == "0-5km")),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid %>% filter(distStrata == "0-5km") %>% pull(pm.building)))),
  tidy(reg_transit_5_10km) %>% filter(term == "transit_duration_hours") %>% mutate(group = "2) 5-10 KM", nObs = nrow(defendant_outcomes_use_precovid %>% filter(distStrata == "5-10km")),
                                                                                   nBuilding = length(unique(defendant_outcomes_use_precovid %>% filter(distStrata == "5-10km") %>% pull(pm.building)))),
  tidy(reg_transit_10_15km) %>% filter(term == "transit_duration_hours") %>% mutate(group = "3) 10-15 KM", nObs = nrow(defendant_outcomes_use_precovid %>% filter(distStrata == "10-15km")),
                                                                                    nBuilding = length(unique(defendant_outcomes_use_precovid %>% filter(distStrata == "10-15km") %>% pull(pm.building)))),
  tidy(reg_transit_15km) %>% filter(term == "transit_duration_hours") %>% mutate(group = "4) 15+ KM", nObs = nrow(defendant_outcomes_use_precovid %>% filter(distStrata == "15+km")),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid %>% filter(distStrata == "15+km") %>% pull(pm.building)))),
)


results_plot_strata$group_combined <- paste(results_plot_strata$group, "\nN Tenants = ", results_plot_strata$nObs, "\nN Buildings = ", results_plot_strata$nBuilding, sep="")
results_plot_strata$color <- ifelse(results_plot_strata$p.value < .05, "black", "grey")
results_plot_strata$group_combined <- factor(results_plot_strata$group_combined, levels=rev(unique(results_plot_strata$group_combined)))

subgroup_plot <- results_plot_strata %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=group_combined, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange()  + theme_bw()  + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0)) +
  ylab("") + xlab("Estimated effect of a 1 hour increase in commuting time\non probability of default")

ggsave("Output/Figures/subgroup_by_ring.pdf", plot = subgroup_plot, width=5, height=2.5)


### Appendix 7
### Seasonality regressions

defendant_outcomes_use_precovid <- defendant_outcomes_use_precovid %>% mutate(season = case_when(month(hearingdate) %in% c(3,4,5) ~ "Spring",
                                                             month(hearingdate) %in% c(6,7,8) ~ "Summer",
                                                             month(hearingdate) %in% c(9,10,11) ~ "Fall",
                                                             month(hearingdate) %in% c(12, 1, 2) ~ "Winter"))

reg_transit_seasons = lm_robust(plaintiffWinDefault ~ transit_duration_hours*I(season == "Summer") + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                               fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid, cluster = pm.building,
                               se_type = "CR0")

reg_transit_spring = lm_robust(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                              fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(3,4,5)), cluster = pm.building,
                              se_type = "CR0")
reg_transit_summer = lm_robust(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                               fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid%>% filter(month(hearingdate) %in% c(6,7,8)), cluster = pm.building,
                               se_type = "CR0")
reg_transit_fall = lm_robust(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                  shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                                fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(9,10,11)), cluster = pm.building,
                                se_type = "CR0")
reg_transit_winter = lm_robust(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8),
                             fixed_effects = ~ as.factor(monthYearHearing), data= defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(12,1,2)), cluster = pm.building,
                             se_type = "CR0")


results_plot_strata <- results_plot_main <- bind_rows(
  tidy(reg_transit_spring) %>% filter(term == "transit_duration_hours") %>% mutate(group = "Spring (Mar-May)", nObs = nrow(defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(3,4,5))),
                                                                                  nBuilding = length(unique(defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(3,4,5)) %>% pull(pm.building)))),
  tidy(reg_transit_summer) %>% filter(term == "transit_duration_hours") %>% mutate(group = "Summer (Jun-Aug)", nObs = nrow(defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(6,7,8))),
                                                                                   nBuilding = length(unique(defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(6,7,8)) %>% pull(pm.building)))),
  tidy(reg_transit_fall) %>% filter(term == "transit_duration_hours") %>% mutate(group = "Fall (Sep-Nov)", nObs = nrow(defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(9,10,11))),
                                                                                    nBuilding = length(unique(defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(9,10,11)) %>% pull(pm.building)))),
  tidy(reg_transit_winter) %>% filter(term == "transit_duration_hours") %>% mutate(group = "Winter (Dec-Feb)", nObs = nrow(defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(12,1,2))),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid %>% filter(month(hearingdate) %in% c(12,1,2)) %>% pull(pm.building)))),
)


results_plot_strata$group_combined <- paste(results_plot_strata$group, "\nN Tenants = ", results_plot_strata$nObs, "\nN Buildings = ", results_plot_strata$nBuilding, sep="")
results_plot_strata$color <- ifelse(results_plot_strata$p.value < .05, "black", "grey")
results_plot_strata$group_combined <- factor(results_plot_strata$group_combined, levels=rev(unique(results_plot_strata$group_combined)))

subgroup_plot <- results_plot_strata %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=group_combined, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange()  + theme_bw()  + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0)) +
  ylab("") + xlab("Estimated effect of a 1 hour increase in commuting time\non probability of default")

ggsave("Output/Figures/Appendix/a7_subgroup_by_season.pdf", plot = subgroup_plot, width=5, height=2.5)


###########################3
### Sensitivity analysis

library(sensemakr)

reg_1_lm = lm(plaintiffWinDefault ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                          shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  +
                as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) + as.factor(monthYearHearing),
                         data= defendant_outcomes_use_precovid)

sense_reg1 <- sensemakr(reg_1_lm, "transit_duration_hours",
                        benchmark_covariates = list("Tract + Block\nDemographics" = c("log(medianIncome)", "log(medianContractRent)", "shareWhiteBlock", "I(shareWhiteBlock^2)", "shareHispanicBlock", "I(shareHispanicBlock^2)"),
                                                    "Apartment" = c("as.numeric(bld_typ == 6 | bld_typ == 7 | bld_typ == 8)")))

pdf("Output/Figures/Appendix/a2_sens_analysis.pdf", width=6, height=6)
plot(sense_reg1, lim = .2, lim.y=.2, label.bump.x=0.005, nlevels=4)
dev.off()

###################################
### Appendix 6 analyses
################################

####
### Withdrawn or JBA
####

# No controls
reg_0_nocontrols = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours, data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Controls
reg_1_plain = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                      shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing),
                    data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Ring fixed-effects
reg_2_ringFE = feols(I(caseWithdrawn==1|jba==1)   ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                       shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k),
                     data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Landlord fixed-effects
reg_3_landlordFE = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                           shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                         data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc() # Garbage collect

#################
### Main results, regression tables
texreg(list(extract(reg_0_nocontrols, include.proj.stats = F),extract(reg_1_plain, include.proj.stats=F),extract(reg_2_ringFE, include.proj.stats=F), extract(reg_3_landlordFE, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)


# Placebo: Public housing
reg_0_nocontrols_public = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours, data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_public = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8)| as.factor(monthYearHearing), 
                           data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_public = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                            data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

gc()

# Placebo, public housing regression tables
texreg(list(extract(reg_0_nocontrols_public, include.proj.stats = F),extract(reg_1_plain_public, include.proj.stats=F),extract(reg_2_ringFE_public, include.proj.stats=F)),
       stars=c(.01, .05), digits = 4, include.ci=F)

# Placebo: Covid
reg_0_nocontrols_covid = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours, data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_covid = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                            shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), 
                          data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_covid = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                           data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_landlordFE_covid = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) |  as.factor(monthYearHearing) + as.factor(LandlordG), 
                               data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

# Regression tables
texreg(list(extract(reg_0_nocontrols_covid, include.proj.stats = F),extract(reg_1_plain_covid, include.proj.stats=F),extract(reg_2_ringFE_covid, include.proj.stats=F), extract(reg_3_landlordFE_covid, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

# Regressions with weekend
reg_0_weekend_nocontrols = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + transit_duration_weekend_hours, data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_weekend_plain = feols(I(caseWithdrawn==1|jba==1)  ~  transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_weekend_ringFE = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)  + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                             data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_weekend_landlordFE = feols(I(caseWithdrawn==1|jba==1)  ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                   shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                                 data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

texreg(list(extract(reg_0_weekend_nocontrols, include.proj.stats = F),extract(reg_1_weekend_plain, include.proj.stats=F),extract(reg_2_weekend_ringFE, include.proj.stats=F), extract(reg_3_weekend_landlordFE , include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

#####################################
### Make combined plot
results_plot_main <- bind_rows(
  tidy(reg_0_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_1_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                            nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_2_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid),
                                                                             nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_3_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_0_nocontrols_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "No Controls", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                        nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_1_plain_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                   nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_2_ringFE_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                    nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_0_nocontrols_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_covid),
                                                                                       nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_1_plain_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_covid),
                                                                                  nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_2_ringFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_covid),
                                                                                   nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_3_landlordFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_covid),
                                                                                       nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_0_weekend_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                         nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_1_weekend_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                    nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_2_weekend_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                     nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_3_weekend_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                         nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building)))
  
)

## Make Asymptotic CIs
results_plot_main$conf.low <- results_plot_main$estimate - abs(qnorm(.025))*results_plot_main$std.error
results_plot_main$conf.high <- results_plot_main$estimate + abs(qnorm(.025))*results_plot_main$std.error

results_plot_main$sample_full <- paste(results_plot_main$sample, "\nN Tenants = ", results_plot_main$nObs, ", N Buildings = ", results_plot_main$nBuilding, sep="")

results_plot_main$model <- factor(results_plot_main$model, levels=rev(c("No Controls", "Controls +\nMonth-Year FE", "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)",  "Controls +\nMonth-Year FE +\nLandlord FE")))
results_plot_main$color <- ifelse(results_plot_main$p.value < .05, "black", "grey50")

main_plot <- results_plot_main %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() + facet_wrap(~sample_full, scales="free", ncol=1) + theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of withdrawal or JBA") + ylab("")

ggsave("Output/Figures/Appendix/a6_main_results_and_placebos_withdrawal_plus_JBA.pdf", plot = main_plot, width=5, height=9)


###############
### Withdrawn only
#################

# No controls
reg_0_nocontrols = feols(I(caseWithdrawn==1) ~ transit_duration_hours, data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Controls
reg_1_plain = feols(I(caseWithdrawn==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                      shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing),
                    data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Ring fixed-effects
reg_2_ringFE = feols(I(caseWithdrawn==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                       shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k),
                     data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Landlord fixed-effects
reg_3_landlordFE = feols(I(caseWithdrawn==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                           shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                         data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc() # Garbage collect

#################
### Main results, regression tables
texreg(list(extract(reg_0_nocontrols, include.proj.stats = F),extract(reg_1_plain, include.proj.stats=F),extract(reg_2_ringFE, include.proj.stats=F), extract(reg_3_landlordFE, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)


# Placebo: Public housing
reg_0_nocontrols_public = feols(I(caseWithdrawn==1) ~ transit_duration_hours, data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_public = feols(I(caseWithdrawn==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8)| as.factor(monthYearHearing), 
                           data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_public = feols(I(caseWithdrawn==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                            data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

gc()

# Placebo, public housing regression tables
texreg(list(extract(reg_0_nocontrols_public, include.proj.stats = F),extract(reg_1_plain_public, include.proj.stats=F),extract(reg_2_ringFE_public, include.proj.stats=F)),
       stars=c(.01, .05), digits = 4, include.ci=F)

# Placebo: Covid
reg_0_nocontrols_covid = feols(I(caseWithdrawn==1) ~ transit_duration_hours, data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_covid = feols(I(caseWithdrawn==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                            shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), 
                          data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_covid = feols(I(caseWithdrawn==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                           data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_landlordFE_covid = feols(I(caseWithdrawn==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) |  as.factor(monthYearHearing) + as.factor(LandlordG), 
                               data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

# Regression tables
texreg(list(extract(reg_0_nocontrols_covid, include.proj.stats = F),extract(reg_1_plain_covid, include.proj.stats=F),extract(reg_2_ringFE_covid, include.proj.stats=F), extract(reg_3_landlordFE_covid, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

# Regressions with weekend
reg_0_weekend_nocontrols = feols(I(caseWithdrawn==1) ~ transit_duration_hours + transit_duration_weekend_hours, data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_weekend_plain = feols(I(caseWithdrawn==1) ~  transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_weekend_ringFE = feols(I(caseWithdrawn==1) ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)  + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                             data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_weekend_landlordFE = feols(I(caseWithdrawn==1) ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                   shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                                 data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

texreg(list(extract(reg_0_weekend_nocontrols, include.proj.stats = F),extract(reg_1_weekend_plain, include.proj.stats=F),extract(reg_2_weekend_ringFE, include.proj.stats=F), extract(reg_3_weekend_landlordFE , include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

#####################################
### Make combined plot
results_plot_main <- bind_rows(
  tidy(reg_0_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_1_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                            nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_2_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid),
                                                                             nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_3_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_0_nocontrols_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "No Controls", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                        nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_1_plain_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                   nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_2_ringFE_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                    nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_0_nocontrols_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_covid),
                                                                                       nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_1_plain_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_covid),
                                                                                  nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_2_ringFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_covid),
                                                                                   nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_3_landlordFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_covid),
                                                                                       nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_0_weekend_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                         nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_1_weekend_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                    nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_2_weekend_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                     nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_3_weekend_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                         nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building)))
  
)

## Make Asymptotic CIs
results_plot_main$conf.low <- results_plot_main$estimate - abs(qnorm(.025))*results_plot_main$std.error
results_plot_main$conf.high <- results_plot_main$estimate + abs(qnorm(.025))*results_plot_main$std.error

results_plot_main$sample_full <- paste(results_plot_main$sample, "\nN Tenants = ", results_plot_main$nObs, ", N Buildings = ", results_plot_main$nBuilding, sep="")

results_plot_main$model <- factor(results_plot_main$model, levels=rev(c("No Controls", "Controls +\nMonth-Year FE", "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)",  "Controls +\nMonth-Year FE +\nLandlord FE")))
results_plot_main$color <- ifelse(results_plot_main$p.value < .05, "black", "grey50")


main_plot <- results_plot_main %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() + facet_wrap(~sample_full, scales="free", ncol=1) + theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of withdrawal") + ylab("")

ggsave("Output/Figures/Appendix/a6_main_results_and_placebos_withdrawal.pdf", plot = main_plot, width=5, height=9)

### JBA only

# No controls
reg_0_nocontrols = feols(I(jba==1) ~ transit_duration_hours, data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Controls
reg_1_plain = feols(I(jba==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                      shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing),
                    data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Ring fixed-effects
reg_2_ringFE = feols(I(jba==1)  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                       shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k),
                     data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Landlord fixed-effects
reg_3_landlordFE = feols(I(jba==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                           shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                         data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc() # Garbage collect

#################
### Main results, regression tables
texreg(list(extract(reg_0_nocontrols, include.proj.stats = F),extract(reg_1_plain, include.proj.stats=F),extract(reg_2_ringFE, include.proj.stats=F), extract(reg_3_landlordFE, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)


# Placebo: Public housing
reg_0_nocontrols_public = feols(I(jba==1) ~ transit_duration_hours, data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_public = feols(I(jba==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8)| as.factor(monthYearHearing), 
                           data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_public = feols(I(jba==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                            data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

gc()

# Placebo, public housing regression tables
texreg(list(extract(reg_0_nocontrols_public, include.proj.stats = F),extract(reg_1_plain_public, include.proj.stats=F),extract(reg_2_ringFE_public, include.proj.stats=F)),
       stars=c(.01, .05), digits = 4, include.ci=F)

# Placebo: Covid
reg_0_nocontrols_covid = feols(I(jba==1) ~ transit_duration_hours, data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_covid = feols(I(jba==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                            shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), 
                          data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_covid = feols(I(jba==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                           data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_landlordFE_covid = feols(I(jba==1) ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) |  as.factor(monthYearHearing) + as.factor(LandlordG), 
                               data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

# Regression tables
texreg(list(extract(reg_0_nocontrols_covid, include.proj.stats = F),extract(reg_1_plain_covid, include.proj.stats=F),extract(reg_2_ringFE_covid, include.proj.stats=F), extract(reg_3_landlordFE_covid, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

# Regressions with weekend
reg_0_weekend_nocontrols = feols(I(jba==1) ~ transit_duration_hours + transit_duration_weekend_hours, data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_weekend_plain = feols(I(jba==1) ~  transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_weekend_ringFE = feols(I(jba==1) ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)  + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                             data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_weekend_landlordFE = feols(I(jba==1) ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                   shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                                 data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

texreg(list(extract(reg_0_weekend_nocontrols, include.proj.stats = F),extract(reg_1_weekend_plain, include.proj.stats=F),extract(reg_2_weekend_ringFE, include.proj.stats=F), extract(reg_3_weekend_landlordFE , include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

#####################################
### Make combined plot
results_plot_main <- bind_rows(
  tidy(reg_0_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_1_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                            nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_2_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid),
                                                                             nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_3_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_0_nocontrols_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "No Controls", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                        nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_1_plain_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                   nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_2_ringFE_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                    nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_0_nocontrols_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_covid),
                                                                                       nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_1_plain_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_covid),
                                                                                  nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_2_ringFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_covid),
                                                                                   nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_3_landlordFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_covid),
                                                                                       nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_0_weekend_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                         nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_1_weekend_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                    nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_2_weekend_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                     nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_3_weekend_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                         nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building)))
  
)

## Make Asymptotic CIs
results_plot_main$conf.low <- results_plot_main$estimate - abs(qnorm(.025))*results_plot_main$std.error
results_plot_main$conf.high <- results_plot_main$estimate + abs(qnorm(.025))*results_plot_main$std.error

results_plot_main$sample_full <- paste(results_plot_main$sample, "\nN Tenants = ", results_plot_main$nObs, ", N Buildings = ", results_plot_main$nBuilding, sep="")

results_plot_main$model <- factor(results_plot_main$model, levels=rev(c("No Controls", "Controls +\nMonth-Year FE", "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)",  "Controls +\nMonth-Year FE +\nLandlord FE")))
results_plot_main$color <- ifelse(results_plot_main$p.value < .05, "black", "grey50")

main_plot <- results_plot_main %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() + facet_wrap(~sample_full, scales="free", ncol=1) + theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of judgment by agreement") + ylab("")

ggsave("Output/Figures/Appendix/a6_main_results_and_placebos_JBA.pdf", plot = main_plot, width=5, height=9)

### Alias Served

# No controls
reg_0_nocontrols = feols(aliasServed ~ transit_duration_hours, data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Controls
reg_1_plain = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                      shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing),
                    data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Ring fixed-effects
reg_2_ringFE = feols(aliasServed  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                       shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k),
                     data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Landlord fixed-effects
reg_3_landlordFE = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                           shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                         data= defendant_outcomes_use_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc() # Garbage collect

#################
### Main results, regression tables
texreg(list(extract(reg_0_nocontrols, include.proj.stats = F),extract(reg_1_plain, include.proj.stats=F),extract(reg_2_ringFE, include.proj.stats=F), extract(reg_3_landlordFE, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)


# Placebo: Public housing
reg_0_nocontrols_public = feols(aliasServed ~ transit_duration_hours, data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_public = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8)| as.factor(monthYearHearing), 
                           data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_public = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                            data= defendant_outcomes_public_precovid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

gc()

# Placebo, public housing regression tables
texreg(list(extract(reg_0_nocontrols_public, include.proj.stats = F),extract(reg_1_plain_public, include.proj.stats=F),extract(reg_2_ringFE_public, include.proj.stats=F)),
       stars=c(.01, .05), digits = 4, include.ci=F)

# Placebo: Covid
reg_0_nocontrols_covid = feols(aliasServed ~ transit_duration_hours, data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_covid = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                            shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), 
                          data= defendant_outcomes_covid, cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_covid = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                           data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_landlordFE_covid = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) |  as.factor(monthYearHearing) + as.factor(LandlordG), 
                               data= defendant_outcomes_covid,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

# Regression tables
texreg(list(extract(reg_0_nocontrols_covid, include.proj.stats = F),extract(reg_1_plain_covid, include.proj.stats=F),extract(reg_2_ringFE_covid, include.proj.stats=F), extract(reg_3_landlordFE_covid, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

# Regressions with weekend
reg_0_weekend_nocontrols = feols(aliasServed ~ transit_duration_hours + transit_duration_weekend_hours, data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_weekend_plain = feols(aliasServed ~  transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_weekend_ringFE = feols(aliasServed ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)  + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                             data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_weekend_landlordFE = feols(aliasServed ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                   shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                                 data= defendant_outcomes_use_precovid_weekend,  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

texreg(list(extract(reg_0_weekend_nocontrols, include.proj.stats = F),extract(reg_1_weekend_plain, include.proj.stats=F),extract(reg_2_weekend_ringFE, include.proj.stats=F), extract(reg_3_weekend_landlordFE , include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

#####################################
### Make combined plot
results_plot_main <- bind_rows(
  tidy(reg_0_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_1_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                            nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_2_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid),
                                                                             nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_3_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid),
                                                                                 nBuilding = length(unique(defendant_outcomes_use_precovid$pm.building))),
  tidy(reg_0_nocontrols_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "No Controls", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                        nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_1_plain_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                   nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_2_ringFE_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_public_precovid),
                                                                                    nBuilding = length(unique(defendant_outcomes_public_precovid$pm.building))),
  tidy(reg_0_nocontrols_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_covid),
                                                                                       nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_1_plain_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_covid),
                                                                                  nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_2_ringFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_covid),
                                                                                   nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_3_landlordFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_covid),
                                                                                       nBuilding = length(unique(defendant_outcomes_covid$pm.building))),
  tidy(reg_0_weekend_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                         nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_1_weekend_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                    nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_2_weekend_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                     nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building))),
  tidy(reg_3_weekend_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid_weekend),
                                                                                         nBuilding = length(unique(defendant_outcomes_use_precovid_weekend$pm.building)))
  
)

## Make Asymptotic CIs
results_plot_main$conf.low <- results_plot_main$estimate - abs(qnorm(.025))*results_plot_main$std.error
results_plot_main$conf.high <- results_plot_main$estimate + abs(qnorm(.025))*results_plot_main$std.error

results_plot_main$sample_full <- paste(results_plot_main$sample, "\nN Tenants = ", results_plot_main$nObs, ", N Buildings = ", results_plot_main$nBuilding, sep="")

results_plot_main$model <- factor(results_plot_main$model, levels=rev(c("No Controls", "Controls +\nMonth-Year FE", "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)",  "Controls +\nMonth-Year FE +\nLandlord FE")))
results_plot_main$color <- ifelse(results_plot_main$p.value < .05, "black", "grey50")

main_plot <- results_plot_main %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() + facet_wrap(~sample_full, scales="free", ncol=1) + theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0)) +
  xlab("Estimated effect of a 1 hour increase in commuting\ntime on probability of alias writ served") + ylab("")

ggsave("Output/Figures/Appendix/a6_main_results_and_placebos_alias.pdf", plot = main_plot, width=5, height=9)

### Alias Served - Post-treatment

# No controls
reg_0_nocontrols = feols(aliasServed ~ transit_duration_hours, data= defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Controls
reg_1_plain = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                      shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing),
                    data= defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Ring fixed-effects
reg_2_ringFE = feols(aliasServed  ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                       shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k),
                     data= defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

# Landlord fixed-effects
reg_3_landlordFE = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                           shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                         data= defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc() # Garbage collect

#################
### Main results, regression tables
texreg(list(extract(reg_0_nocontrols, include.proj.stats = F),extract(reg_1_plain, include.proj.stats=F),extract(reg_2_ringFE, include.proj.stats=F), extract(reg_3_landlordFE, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)


# Placebo: Public housing
reg_0_nocontrols_public = feols(aliasServed ~ transit_duration_hours, data= defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_public = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8)| as.factor(monthYearHearing), 
                           data= defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_public = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                            data= defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

gc()

# Placebo, public housing regression tables
texreg(list(extract(reg_0_nocontrols_public, include.proj.stats = F),extract(reg_1_plain_public, include.proj.stats=F),extract(reg_2_ringFE_public, include.proj.stats=F)),
       stars=c(.01, .05), digits = 4, include.ci=F)

# Placebo: Covid
reg_0_nocontrols_covid = feols(aliasServed ~ transit_duration_hours, data= defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_plain_covid = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                            shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), 
                          data= defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1), cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_ringFE_covid = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                             shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                           data= defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1),  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_landlordFE_covid = feols(aliasServed ~ transit_duration_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                 shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) |  as.factor(monthYearHearing) + as.factor(LandlordG), 
                               data= defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1),  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

# Regression tables
texreg(list(extract(reg_0_nocontrols_covid, include.proj.stats = F),extract(reg_1_plain_covid, include.proj.stats=F),extract(reg_2_ringFE_covid, include.proj.stats=F), extract(reg_3_landlordFE_covid, include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

# Regressions with weekend
reg_0_weekend_nocontrols = feols(aliasServed ~ transit_duration_hours + transit_duration_weekend_hours, data= defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1),  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_1_weekend_plain = feols(aliasServed ~  transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                              shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing), data= defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1),  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_2_weekend_ringFE = feols(aliasServed ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                               shareWhiteBlock + I(shareWhiteBlock^2)  + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(distRing2k), 
                             data= defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1),  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)

reg_3_weekend_landlordFE = feols(aliasServed ~ transit_duration_hours + transit_duration_weekend_hours + log(medianIncome) + log(medianContractRent) + ongoing_rent_missing + ongoing_rent_recoded:I(1-ongoing_rent_missing) +
                                   shareWhiteBlock + I(shareWhiteBlock^2)   + shareHispanicBlock + I(shareHispanicBlock^2)  + as.numeric(bld_typ == 6|bld_typ==7|bld_typ==8) | as.factor(monthYearHearing) + as.factor(LandlordG), 
                                 data= defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1),  cluster = ~pm.building, ssc = ssc(adj = FALSE, cluster.adj = FALSE), demeaned=T)
gc()

texreg(list(extract(reg_0_weekend_nocontrols, include.proj.stats = F),extract(reg_1_weekend_plain, include.proj.stats=F),extract(reg_2_weekend_ringFE, include.proj.stats=F), extract(reg_3_weekend_landlordFE , include.proj.stats=F)), 
       stars=c(.01, .05), digits = 4, include.ci=F)

#####################################
### Make combined plot
results_plot_main <- bind_rows(
  tidy(reg_0_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid  %>% filter(plaintiffWinDefault == 1)),
                                                                                 nBuilding = length(unique((defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_1_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1)),
                                                                            nBuilding = length(unique((defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_2_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1)),
                                                                             nBuilding = length(unique((defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_3_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1)),
                                                                                 nBuilding = length(unique((defendant_outcomes_use_precovid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_0_nocontrols_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "No Controls", nObs = nrow(defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1)),
                                                                                        nBuilding = length(unique((defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_1_plain_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1)),
                                                                                   nBuilding = length(unique((defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_2_ringFE_public) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPre-Covid, public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1)),
                                                                                    nBuilding = length(unique((defendant_outcomes_public_precovid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_0_nocontrols_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1)),
                                                                                       nBuilding = length(unique((defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_1_plain_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1)),
                                                                                  nBuilding = length(unique((defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_2_ringFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1)),
                                                                                   nBuilding = length(unique((defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_3_landlordFE_covid) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Placebo\nPost-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1)),
                                                                                       nBuilding = length(unique((defendant_outcomes_covid %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_0_weekend_nocontrols) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "No Controls", nObs = nrow(defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1)),
                                                                                         nBuilding = length(unique((defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_1_weekend_plain) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE", nObs = nrow(defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1)),
                                                                                    nBuilding = length(unique((defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_2_weekend_ringFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)", nObs = nrow(defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1)),
                                                                                     nBuilding = length(unique((defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1))$pm.building))),
  tidy(reg_3_weekend_landlordFE) %>% filter(term == "transit_duration_hours") %>% mutate(sample = "Main Analysis; conditioning on weekend time\nPre-Covid, non-public housing", model = "Controls +\nMonth-Year FE +\nLandlord FE", nObs = nrow(defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1)),
                                                                                         nBuilding = length(unique((defendant_outcomes_use_precovid_weekend %>% filter(plaintiffWinDefault == 1))$pm.building)))
  
)

## Make Asymptotic CIs
results_plot_main$conf.low <- results_plot_main$estimate - abs(qnorm(.025))*results_plot_main$std.error
results_plot_main$conf.high <- results_plot_main$estimate + abs(qnorm(.025))*results_plot_main$std.error

results_plot_main$sample_full <- paste(results_plot_main$sample, "\nN Tenants = ", results_plot_main$nObs, ", N Buildings = ", results_plot_main$nBuilding, sep="")

results_plot_main$model <- factor(results_plot_main$model, levels=rev(c("No Controls", "Controls +\nMonth-Year FE", "Controls +\nMonth-Year FE +\nDist. Ring FE (2km)",  "Controls +\nMonth-Year FE +\nLandlord FE")))
results_plot_main$color <- ifelse(results_plot_main$p.value < .05, "black", "grey50")

main_plot <- results_plot_main %>% ggplot(aes(x=estimate, xmin=conf.low, xmax=conf.high, y=model, colour = color))  + geom_vline(xintercept = 0, lty=2)  + scale_colour_identity() +
  geom_pointrange() + facet_wrap(~sample_full, scales="free", ncol=1) + theme_bw() + theme(axis.text = element_text(size = 6)) + theme(axis.text.y=element_text(vjust=.8, hjust=0)) +
  xlab("Estimated 'effect' of a 1 hour increase in commuting\ntime on probability of alias writ served\nconditional on default") + ylab("")

ggsave("Output/Figures/Appendix/a6_main_results_and_placebos_alias_posttreatment.pdf", plot = main_plot, width=5, height=9)



