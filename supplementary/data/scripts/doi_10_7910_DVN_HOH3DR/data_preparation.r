rm(list=ls(all=TRUE))

library(dplyr)

################## ##################
#### DEFINE FUNCTIONS
################## ##################

#### function for defining deployment variables and adding state-level covariates
calculate_deploy_vars <- function(sample) {
  
  # calculating state-normalized deployment, following Sunter et al.
  sample <- sample %>%
    mutate(SunroofDeployment = existing_installs_count/(count_qualified),
           SunroofSeeded = ifelse(existing_installs_count > 0, 1, 0)
    ) %>%
    group_by(state_name) %>%
    mutate(
      weighted_SunroofDeployment = (total_population/sum(total_population, na.rm = TRUE))*SunroofDeployment,
      normed_SunroofDeployment = SunroofDeployment/ sum(weighted_SunroofDeployment, na.rm = TRUE),
      
    ) %>%
    select(-c(weighted_SunroofDeployment)) %>%
    ungroup()
  
  # calculating state-level statistics, to be used for conducting state-level analysis
  state_stats <- sample %>%
    group_by(state_name) %>%
    summarize(state_ave =  mean(SunroofDeployment, na.rm = TRUE),
              state_installs_total = sum(existing_installs_count)) %>%
    ungroup() %>%
    arrange(state_ave) %>%
    mutate(state_rank = row_number(),
           quartile = ntile(state_ave, 4))
  
  sample <- sample %>%
    left_join(state_stats, by = "state_name")
  sample
}

################################################################################
########################## End Functions ##########################
################################################################################


################## ################## ################## ################## 
#### READ, CLEAN, AND MERGE DATA 
################## ################## ################## ################## 

### read ACS DATA
acs_df <- readRDS("data/acs_df_2013.rds")

################## Create new ACS variables
acs_df <- acs_df %>%
  mutate(
    pop_dens = total_population/(ALAND/2589988.110336), #converting aland to sq_mi, so pop_dens is pop/sq_mi
    perc_white = white_non_hisp/total_race*100,
    perc_black = black_non_hisp/total_race*100,
    perc_asian = (asian_alone)/ total_race*100,
    perc_native = (native_american_non_hisp+native_american_hisp)/ total_race*100,
    perc_hispanic = hispanic/total_race*100,
    perc_hs = (Regular_high_school_diploma+ GED_or_alternative_credential)/total_edu*100,
    perc_some_col = (Some_college_less_than_1_year +Some_college_1_or_more_years_no_degree)/ total_edu*100,
    perc_bachelor = (Bachelors_degree)/total_edu*100,
    perc_renter = renter_occupied/total_tenure*100,
    perc_homeowner = owner_occupied/total_tenure*100
  ) %>%
  rename( "median_income" = "Median_household_income_(2018_dollars)",
          "state_fips" = "STATEFP",
          "county_fips" = "COUNTYFP",
          "geo_id" = "GEOID",
          "name" = "NAME") %>%
  select( c(state_fips, county_fips, geo_id, name, total_population, pop_dens, perc_white, perc_black, perc_asian, perc_native, perc_hispanic,
            perc_hs, perc_some_col, perc_bachelor, perc_renter, perc_homeowner, median_income,
            below_poverty))

# create majority status variable
acs_df <-acs_df %>%
  mutate(
    majority_status = ifelse(perc_white > 50, "white", 0),
    majority_status = ifelse(perc_black > 50, "black", majority_status),
    majority_status = ifelse(perc_asian > 50, "asian", majority_status),
    majority_status = ifelse(perc_hispanic > 50, "hisp", majority_status),
    majority_status = ifelse(perc_white <= 50 & perc_black <= 50 & perc_asian <= 50 & perc_hispanic <= 50, "no_majority", majority_status)
  )

################## LOAD THE PROJECT SUNROOF DATA
## this file is from September 2017 and contains the same set of tracts that is in SCK's file
## Source: https://www.kaggle.com/siddhantss/google-sunroof-eda/data?select=sunroof_solar_potential_by_censustract.csv
sunroof <- read.csv("data/sunroof_solar_potential_by_censustract(09082017).csv", header = TRUE, sep = "," ,  quote="\"", stringsAsFactors = FALSE, na.strings= 'NULL', colClasses = c('region_name' = "character"))

# A small number of tracts (7) in the Sunroof dataset could not be linked to the ACS
bad_tract_names <- sunroof[which(!(sunroof$region_name %in% acs_df$geo_id) ),]

# We were able to link 5 of the tracts, by consulting the Census documentation (link: https://www2.census.gov/geo/pdfs/reference/Geography_Notes.pdf). 
# In the 5 cases, the Sunroof tracts had outdated geo_id's, which we updated using information in the documentation.
# One tract in California (06037930401) was identified in the documentation as being incorrect and we drop it from the analysis.
# One tract (04019410501) could not be linked to the ACS even after updating the ID based on the documentation.

sunroof <- sunroof %>%
  mutate(region_name = replace(region_name, region_name=="04019002701", "04019002704"),
         region_name = replace(region_name, region_name=="04019002903", "04019002906"),
         region_name = replace(region_name, region_name=="04019410502", "04019004121"),
         region_name = replace(region_name, region_name=="04019410501", "040194004118"), #this tract not present in 2013 ACS 5-year estimates
         region_name = replace(region_name, region_name=="04019470500", "04019005300"),
         region_name = replace(region_name, region_name=="04019410503", "04019004125")
  ) %>%
  filter(region_name != "06037930401") #dropping tract in CA that was noted as being incorrect by the Census (see https://www2.census.gov/geo/pdfs/reference/Geography_Notes.pdf)

acs_df$in_sunroof <- as.factor(ifelse(acs_df$geo_id %in% unique(sunroof$region_name), 1, 0))

## merge ACS and SunRoof datasets and select variables
combined_df <- acs_df %>%
  left_join(sunroof, by = c("geo_id" = "region_name")) %>% #join with sunroof
  
  #group_by(geo_id) %>% #delete the Sunroof duplicate census tracts
  #arrange(count_qualified) %>%
  #filter(row_number()==n()) %>%
  #ungroup() %>%
  select(state_fips, county_fips, geo_id, name, total_population, perc_white, perc_black,
         perc_asian, perc_native, perc_hispanic, perc_bachelor, perc_renter, median_income,
         majority_status, in_sunroof, count_qualified, existing_installs_count, lat_avg,
         lng_avg, percent_covered, percent_qualified, state_name, kw_median, yearly_sunlight_kwh_total)

################################################################################
###################### Create alternative analytical samples ######################

# limit to the sample criteria from Sunter et al.
## ANALYSES BASED ON THIS SAMPLE ARE REPORTED IN THE SI OF THE MATTERS ARISING COMMENT
combined_sunter_sample <- combined_df %>%
  filter(
    in_sunroof ==1,
    median_income > 23834, #median income above poverty rate (per Sunter et al.)
    percent_covered >95,
    !is.na(kw_median) #select tracts missing all potential info, so exclude
  )

combined_sunter_sample <- calculate_deploy_vars(combined_sunter_sample)

print(nrow(combined_sunter_sample)) #compare to 34,156 tracts in Sunter et al.
sum(combined_sunter_sample$yearly_sunlight_kwh_total)/1e+09 #compare to reported 829 TWh yr−1 in Sunter et al.

# Additionally, filter out if percent_covered > 100
sample_filter_perc_covered <- combined_df %>%
  filter(
    in_sunroof ==1,
    median_income > 23834, #median income above poverty rate (per Sunter et al.)
    percent_covered >95,
    !is.na(kw_median), #select tracts missing all potential info, so exclude
    percent_covered <=100
  )

sample_filter_perc_covered <- calculate_deploy_vars(sample_filter_perc_covered)

print(nrow(sample_filter_perc_covered))
sum(sample_filter_perc_covered$yearly_sunlight_kwh_total)/1e+09 #compare to reported 829 TWh yr−1 in Sunter et al.

# Additionally, filter out if too few count_qualified
## ANALYSES BASED ON THIS SAMPLE ARE REPORTED IN THE MAIN TEXT OF THE MATTERS ARISING COMMENT
sample_filter_count_200 <- combined_df %>%
  filter(
    in_sunroof ==1,
    median_income > 23834, #median income above poverty rate (per Sunter et al.)
    percent_covered >95,
    !is.na(kw_median), #select tracts missing all potential info, so exclude
    percent_covered <=100,
    count_qualified > 200)

sample_filter_count_200 <- calculate_deploy_vars(sample_filter_count_200)

print(nrow(sample_filter_count_200))
sum(sample_filter_count_200$yearly_sunlight_kwh_total)/1e+09 #compare to reported 829 TWh yr−1 in Sunter et al.

saveRDS(combined_sunter_sample, file = "data/combined_sunter_sample.rds")
saveRDS(sample_filter_count_200, file = "data/sample_filter_count_200.rds")

