library(haven)
library(car)
library(ggplot2)
library(dplyr)

# Bring in reduced version of NC voter file snapshot from election day 2016. This file is subsetted to Active and Inactive voters and includes only fields necessary for our analysis (to keep the file size manageable)
dat <- read_dta("VR_Snapshot_20161108_Reduced.dta")

# Create indicators for white and total voters
dat$whites <- 0
dat$whites[dat$race_code=="W" & dat$ethnic_code!="HL"] <- 1
dat$total <- 1

# Collapse data creating counts of RVs and white RVs by precinct
file <- dat %>%
  dplyr::group_by(county_id, precinct_abbrv)  %>%
  dplyr::summarise(total = sum(total),
              whites = sum(whites)) 

# Bring in official precinct vote tallies and rename variables
counts <- read_dta("results_sort_20161108.dta")
counts <- counts %>% 
  dplyr::rename(
    county_id = ïcounty_id,
    pct_label = precinct_code
  )

# Bring in the list of 2016 general election voters from the NC Vote History file. To keep the file size manageable, this file only includes fields needed for the analysis.
voters <- read_dta("nc2016voters_reduced.dta")

# Collapse to give voter count by precinct
voters$vf_voters <- 1  
voters <- voters %>%
  dplyr::group_by(county_id, pct_label)  %>%
  dplyr::summarise(vf_voters = sum(vf_voters)) 

# Merge voter counts from voter file with precinct tallies
countcompare <- merge(counts,voters,by=c("county_id","pct_label"), all.x=T) 

# Merge in data from voter file on racial composition by precinct from voter file
countcompare <- merge(countcompare,file,by.x=c("county_id","pct_label"), by.y=c("county_id", "precinct_abbrv"))      

# Create variable for the proportion of RVs in precint who are white
countcompare$propwhite <- (countcompare$whites/countcompare$total)*100

# Create variable for deviation of voter file count from precinct tallies count
countcompare$deviation <- ((countcompare$vf_voters-countcompare$votes)/countcompare$votes)*100

# Create Figure A.3
tiff(file="nc_pct_dev_2016.tiff",width = 3200, height = 2800, units = "px", res = 500)
ggplot(countcompare, aes(x=propwhite, y=deviation)) + geom_point(aes(size=votes), alpha=.4) + geom_smooth(method="lm") + geom_hline(yintercept=0) + theme_bw() + ylab("Deviation in vote tally") + xlab("Proportion of precinct white") + theme(legend.position="none") + annotate("text", x = 40, y = -40, label = "deviation = -4 - .008*%white")
dev.off()

summary(lm(deviation ~ propwhite, data=countcompare))
