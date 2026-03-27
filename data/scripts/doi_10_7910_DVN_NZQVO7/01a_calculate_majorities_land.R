####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 01a_calculate_majorities_land.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################

## script to aggregate the opinion polls into coalition probabilities 
## and to calculate Pedersen's volatility index for the Land elections
## Note: The polls are scraped from wahlrecht.de in June 2018


## load required packages

library(readr) # Read Rectangular Text Data, CRAN v1.3.1
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(coalitions) # Bayesian "Now-Cast" Estimation of Event Probabilities in Multi-Party Democracies, CRAN v0.6.15
library(tidyr) # Tidy Messy Data, CRAN v1.1.2
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(purrr) # Functional Programming Tools, CRAN v0.3.4

## load dataset with rawe polls
dta_polls <- read_csv("data_polls_land_raw.csv") %>% 
    mutate(date = as.Date(date, "%d/%m/%y"))

dta_polls <- dta_polls %>% 
    mutate(cdu = as.numeric(cdu),
           spd = as.numeric(spd),
           greens = as.numeric(greens),
           left = as.numeric(left),
           fdp = as.numeric(fdp),
           afd = as.numeric(afd),
           ssw = as.numeric(ssw),
           pirates = as.numeric(piraten),
           fw = as.numeric(fw),
           other = as.numeric(other)) %>% 
    filter(spd < 100) %>% ## three polls with errors
    mutate(cdu = ifelse(!is.na(csu), csu, cdu)) %>% 
    filter(!is.na(date))

## change missing respondents values to 1000 (usual sample size)

dta_polls <- dta_polls %>% 
    mutate(respondents = if_else(is.na(respondents), 1000, respondents))

table(dta_polls$land)   


## import files with metadata and election dates

dta_election_results <- readRDS(file = "election_results_laender.rds") %>% 
    mutate(year = lubridate::year(election_date))

dta_election_dates <- dta_election_results %>% 
    select(election_date, year, bundesland, starts_with("votes_lag")) %>% 
    unique()

dta_survey_dates <- read_csv("data_survey_dates_land.csv")

dta_survey_dates <- dta_survey_dates %>% 
    separate(survey_dates, into = c("date_survey_start", "date_survey_end"), 
             sep = "\\-",
             remove = FALSE) %>% 
    mutate(date_survey_start = as.Date(date_survey_start, "%d.%m.%Y")) %>% 
    mutate(date_survey_end = as.Date(date_survey_end, "%d.%m.%Y")) %>% 
    mutate(length_survey_days = date_survey_end - date_survey_start) %>% 
    mutate(bundesland = str_to_lower(bundesland))

dta_survey_dates_subset <- dta_survey_dates %>% 
    select(bundesland, year, date_survey_start, date_survey_end,
           length_survey_days) %>% 
    mutate(bundesland = str_to_lower(bundesland)) %>% 
    unique()

nrow(dta_survey_dates_subset)
nrow(dta_election_dates)

## merge survery dates and election results
dta_survey_metadata_elections <- left_join(dta_survey_dates_subset,
                                           dta_election_results,
                                           by = c("bundesland", "year")) %>% 
    mutate(election_id = paste(year, bundesland))

table(dta_survey_metadata_elections$election_id)


## now select only polls for a specific frame and calculate average

dta_election_dates <- dta_election_results %>% 
    select(bundesland, year, election_date) %>% 
    unique()

dta_survey_metadata_unique <- dta_survey_metadata_elections %>%
    select(bundesland, year, election_id, date_survey_start, election_date) %>% 
    unique() 

table(dta_survey_metadata_elections$bundesland)

## function to calculate coalition probabilities for land elections
prob_land <- function(election_year, land_select,
                      add_coalitions = FALSE,
                      list_coalitions, days) {
    
    dta_survey_metadata_select <- dta_survey_metadata_elections %>%
        select(bundesland, year, date_survey_start) %>% 
        unique() %>% 
        mutate(year = as.numeric(year),
               date_survey_start = as.Date(date_survey_start))
    
    dta_date_election <- dta_survey_metadata_select %>% 
        mutate(year = as.numeric(year)) %>% 
        filter(year == election_year & 
                   bundesland == as.character(land_select))
    
    date_survey_start <-  as.Date(dta_date_election$date_survey_start)
    
    date_polls_start <- date_survey_start - as.numeric(days)
    
    ## now subset polls
    
    dta_polls_relevant_subset <- dta_polls %>% 
        mutate(date = as.Date(date)) %>% 
        filter(land == land_select) %>% 
        filter(date > date_polls_start) %>% 
        filter(date < date_survey_start) %>% 
        select(cdu, spd, fdp, left, greens, everything())
    
    elong <- collapse_parties(dta_polls_relevant_subset) 
    
    elong
    set.seed(1) ## for reproducibility
    
    elong <- elong %>%
        mutate(draws = map(survey, 
                           draw_from_posterior, 
                           nsim = 10, correction = 0.005))
    
    elong_seats <- elong %>%
        mutate(seats = map2(draws, survey, get_seats, distrib.fun=sls))
    
    coalitions <- list(
        c("cdu"),
        c("cdu", "fdp"),
        c("cdu", "greens"),
        c("cdu", "fdp", "greens"),
        c("spd"),
        c("spd", "fdp", "greens"),
        c("cdu", "afd"),
        c("spd", "left"),
        c("spd", "greens"),
        c("cdu", "fw"),
        c("spd", "greens", "fw"),
        c("spd", "fdp"),
        c("spd", "left", "greens"),
        c("cdu", "spd"))
    
    if (add_coalitions == TRUE) {
        
        list_list_coalitions <- list(list_coalitions)
        coalitions <- c(coalitions, list_list_coalitions)
    }
    
    elong_majorities <- elong_seats %>%
        mutate(majorities = map(seats, 
                                have_majority, coalitions=coalitions))
    
    ## coalition probabilities
    
    elong_probabilities <- elong_majorities %>%
        mutate(
            probabilities = map(majorities, 
                                calculate_probs, 
                                coalitions=coalitions))
    ## one row per coalition
    elong_probabilities_unnested <- elong_probabilities %>% 
        slice(1) %>% 
        select(probabilities) %>% 
        unnest() 
    
    dta <- elong_probabilities_unnested %>% 
        mutate(year = election_year,
               bundesland = land_select,
               date_polls_start = date_polls_start,
               date_survey_start = date_survey_start,
               number_polls = as.numeric(nrow(dta_polls_relevant_subset)),
               probability_sd = sd(probability, na.rm = TRUE),
               probability_mean = mean(probability, na.rm = TRUE))
    
    
    dta$number_coa_larger_0 <- sum(dta$probability > 0, na.rm = TRUE)
    dta$number_coa_larger_30 <- sum(dta$probability > 30, na.rm = TRUE)
    dta$number_coa_larger_50 <- sum(dta$probability > 50, na.rm = TRUE)
    dta$number_coa_larger_100 <- sum(dta$probability > 80, na.rm = TRUE)
    
    
    ## the majorities table for each date will have 1 row per simulation
    ## and one column per coalition
    
    elong_majorities_df <- elong_majorities %>% 
        slice(1) %>% 
        select(majorities) %>% 
        unnest()
    
    probs_surplus <- calculate_probs(elong_majorities_df, 
                                     list('cdu', 'cdu_fdp', 
                                          'cdu_greens',
                                          'cdu_fdp_greens',
                                          'spd',
                                          'fdp_greens_spd',
                                          'afd_cdu',
                                          'left_spd',
                                          'greens_spd',
                                          'cdu_fw',
                                          'fw_greens_spd',
                                          'fdp_spd',
                                          'greens_left_spd',
                                          'cdu_spd'), exclude_superior = FALSE) %>% 
        dplyr::rename(coalition = coalition,
                      probability_surplus = probability) %>% 
        mutate(year = election_year,
               bundesland = land_select)
    
    probs_surplus
    
    ## calculate seat shares and ENP
    elong <- elong %>%
        mutate(draws = map(survey, draw_from_posterior, nsim=10, correction=0.005))
    
    elong <- elong %>% slice(1) %>% select(draws) %>% unnest()
    elong <- elong %>%
        summarise_all(funs(mean))
    
    elong_sum <- elong %>%
        mutate(year = election_year,
               bundesland = land_select)
    
    prepare_enp <- elong^2
    
    prepare_enp
    
    elong_sum$enp <- 1 / rowSums(prepare_enp)
    
    elong_sum
    
    dta_full <- left_join(dta, elong_sum, by = c("year", "bundesland"))
    
    dta_full <- left_join(dta_full, probs_surplus, by = c("year", "bundesland", "coalition"))
    dta_full
}


list_elections <- unique(dta_survey_metadata_unique$election_id)
list_elections

## use polls published within a window of 
## 365 days prior to the start of the GLES survey

prob_2010_nrw <- prob_land(election_year = 2010, days = 365,
                           land_select = "nordrhein-westfalen")

prob_2011_sachsen_anhalt <- prob_land(election_year = 2011, days = 365,
                                      land_select = "sachsen-anhalt")

prob_2011_rheinland_pfalz <- prob_land(election_year = 2011, days = 365,
                                       land_select = "rheinland-pfalz")


prob_2011_baden_wuerttemberg <- prob_land(election_year = 2011, days = 365, 
                                          land_select = "baden-wuerttemberg")

prob_2011_berlin <- prob_land(election_year = 2011, days = 365, 
                              land_select = "berlin")

prob_2011_mecklenburg_vorpommern <- prob_land(election_year = 2011, days = 365,
                                              land_select = "mecklenburg-vorpommern")


prob_2012_nrw <- prob_land(election_year = 2012, days = 365, 
                           land_select = "nordrhein-westfalen")


prob_2012_schleswig_holstein <- prob_land(election_year = 2012, days = 365, 
                                          land_select = "schleswig-holstein")

prob_2013_niedersachsen <- prob_land(election_year = 2013, days = 365,
                                     land_select = "niedersachsen")

prob_2013_hessen <- prob_land(election_year = 2013, days = 365, 
                              land_select = "hessen")

prob_2013_bayern <- prob_land(election_year = 2013, days = 365, 
                              land_select = "bayern")

prob_2014_sachsen <- prob_land(election_year = 2014, days = 365,
                               land_select = "sachsen")

prob_2014_brandenburg <- prob_land(election_year = 2014, days = 365, 
                                   land_select = "brandenburg")

prob_2014_thueringen<- prob_land(election_year = 2014, days = 365, 
                                 land_select = "thueringen")

prob_2016_baden_wuerttemberg <- prob_land(election_year = 2016, days = 365,  
                                          land_select = "baden-wuerttemberg")

prob_2016_rheinland_pfalz <- prob_land(election_year = 2016, days = 365, 
                                       land_select = "rheinland-pfalz")

prob_2016_mecklenburg_vorpommern <- prob_land(election_year = 2016, days = 365, 
                                              land_select = "mecklenburg-vorpommern")

prob_2017_schleswig_holstein <- prob_land(election_year = 2017, days = 365, 
                                          land_select = "schleswig-holstein")

prob_2017_nrw <- prob_land(election_year = 2017, days = 365, 
                           land_select = "nordrhein-westfalen")



prob_combined_one_year <- bind_rows(prob_2010_nrw,
                                    prob_2011_baden_wuerttemberg,
                                    prob_2011_berlin,
                                    prob_2011_mecklenburg_vorpommern,
                                    prob_2011_rheinland_pfalz,
                                    prob_2011_sachsen_anhalt,
                                    prob_2012_nrw,
                                    prob_2012_schleswig_holstein,
                                    prob_2013_bayern,
                                    prob_2013_hessen,
                                    prob_2013_niedersachsen,
                                    prob_2014_brandenburg,
                                    prob_2014_sachsen,
                                    prob_2014_thueringen,
                                    prob_2016_baden_wuerttemberg,
                                    prob_2016_mecklenburg_vorpommern,
                                    prob_2016_rheinland_pfalz,
                                    prob_2017_nrw,
                                    prob_2017_schleswig_holstein) %>% 
    arrange(year, bundesland)



## repeat analysis for only the last 180 days before the election

prob_2010_nrw_halfyear <- prob_land(election_year = 2010, days = 182,
                                    land_select = "nordrhein-westfalen")

prob_2011_sachsen_anhalt_halfyear <- prob_land(election_year = 2011, days = 182,
                                               land_select = "sachsen-anhalt")

prob_2011_rheinland_pfalz_halfyear <- prob_land(election_year = 2011,days = 182, 
                                                land_select = "rheinland-pfalz")


prob_2011_baden_wuerttemberg_halfyear <- prob_land(election_year = 2011, days = 182,
                                                   land_select = "baden-wuerttemberg")

prob_2011_berlin_halfyear <- prob_land(election_year = 2011, days = 182,
                                       land_select = "berlin")

prob_2011_mecklenburg_vorpommern_halfyear <- prob_land(election_year = 2011,days = 182,
                                                       land_select = "mecklenburg-vorpommern")


prob_2012_nrw_halfyear <- prob_land(election_year = 2012, days = 182,
                                    land_select = "nordrhein-westfalen")


prob_2012_schleswig_holstein_halfyear <- prob_land(election_year = 2012, days = 182,
                                                   land_select = "schleswig-holstein")

prob_2013_niedersachsen_halfyear <- prob_land(election_year = 2013, days = 182,
                                              land_select = "niedersachsen")

prob_2013_hessen_halfyear <- prob_land(election_year = 2013, days = 182,
                                       land_select = "hessen")

prob_2013_bayern_halfyear <- prob_land(election_year = 2013, days = 182,
                                       land_select = "bayern")

prob_2014_sachsen_halfyear <- prob_land(election_year = 2014,days = 182,
                                        land_select = "sachsen")

prob_2014_brandenburg_halfyear <- prob_land(election_year = 2014, days = 182,
                                            land_select = "brandenburg")

prob_2014_thueringen_halfyear <- prob_land(election_year = 2014, days = 182,
                                           land_select = "thueringen")

prob_2016_baden_wuerttemberg_halfyear <- prob_land(election_year = 2016, days = 182,
                                                   land_select = "baden-wuerttemberg")

prob_2016_rheinland_pfalz_halfyear <- prob_land(election_year = 2016,days = 182, 
                                                land_select = "rheinland-pfalz")

prob_2016_mecklenburg_vorpommern_halfyear <- prob_land(election_year = 2016, days = 182,
                                                       land_select = "mecklenburg-vorpommern")

prob_2017_schleswig_holstein_halfyear <- prob_land(election_year = 2017, days = 182,
                                                   land_select = "schleswig-holstein")

prob_2017_nrw_halfyear <- prob_land(election_year = 2017, days = 182,
                                    land_select = "nordrhein-westfalen")



prob_combined_half_year <- bind_rows(prob_2010_nrw_halfyear,
                                     prob_2011_baden_wuerttemberg_halfyear,
                                     prob_2011_berlin_halfyear,
                                     prob_2011_mecklenburg_vorpommern_halfyear,
                                     prob_2011_rheinland_pfalz_halfyear,
                                     prob_2011_sachsen_anhalt_halfyear,
                                     prob_2012_nrw_halfyear,
                                     prob_2012_schleswig_holstein_halfyear,
                                     prob_2013_bayern_halfyear,
                                     prob_2013_hessen_halfyear,
                                     prob_2013_niedersachsen_halfyear,
                                     prob_2014_brandenburg_halfyear,
                                     prob_2014_sachsen_halfyear,
                                     prob_2014_thueringen_halfyear,
                                     prob_2016_baden_wuerttemberg_halfyear,
                                     prob_2016_mecklenburg_vorpommern_halfyear,
                                     prob_2016_rheinland_pfalz_halfyear,
                                     prob_2017_nrw_halfyear,
                                     prob_2017_schleswig_holstein_halfyear) %>% 
    arrange(year, bundesland) %>% 
    dplyr::select(coalition, year, bundesland, probability, enp) %>% 
    dplyr::rename(probability_half_year = probability,
                  enp_half_year = enp)


prob_combined <- left_join(prob_combined_one_year, prob_combined_half_year, 
                           by = c("year", "bundesland", "coalition"))


## check correlation between surveys from one year or half a year before the election
cor.test(prob_combined$probability_half_year, prob_combined$probability)

## get unique number of polls
polls_unique <- prob_combined %>% 
    select(bundesland, year, number_polls) %>%
    unique()

sum(polls_unique$number_polls)

## recode coalition into GLES format (change order)
recode_coalition_gles <- c("'greens_left_spd'='spd_left_greens';
                           'greens_spd'='spd_greens';'fw_greens_spd'='spd_greens_fw';
                           'afd_cdu'='cdu_afd';'fdp_spd'='spd_fdp';
                           'fdp_greens_spd'='spd_fdp_greens';
                           'left_spd'='spd_left'")

prob_combined <- prob_combined %>% 
    mutate(coalition = car::recode(coalition, recode_coalition_gles)) %>% 
    mutate(spd_in_coa = stringr::str_detect(coalition, "spd")) %>% 
    mutate(cdu_in_coa = stringr::str_detect(coalition, "cdu"))


cor.test(prob_combined$probability,
         prob_combined$probability_surplus)


## merge election variables

dta_votes_last <- dta_election_results %>% 
    select(bundesland, year, starts_with("votes_lag")) %>% 
    unique()

prob_combined_merged <- left_join(prob_combined, dta_votes_last,
                                  by = c("bundesland", "year"))


## get dataset to calculate Pedersen's volatility index for each Land election
prob_pedersen_land <- prob_combined_merged %>% 
    select(bundesland, year, cdu:afd, starts_with("votes_lag"), -c(starts_with("prob"))) %>% 
    unique()


## replace NA with 0 (doesn't make a difference in the analysis)
prob_pedersen_land[is.na(prob_pedersen_land)] <- 0 

## see Pedersen (1979, p. 4)
prob_pedersen_land <- prob_pedersen_land %>% 
    mutate(pedersen_vol = 100 * (0.5 * (abs(cdu - votes_lag_cdu) +
                                            abs(spd - votes_lag_spd) + 
                                            abs(greens - votes_lag_greens) + 
                                            abs(left - votes_lag_left) +
                                            abs(fdp - votes_lag_fdp) + 
                                            abs(pirates - votes_lag_piraten) + 
                                            abs(fw - votes_lag_fw) + 
                                            abs(afd - votes_lag_afd))))

## select only relevant variables
prob_pedersen_land_small <- prob_pedersen_land %>% 
    select(pedersen_vol, bundesland, year)

## merge results back to the main data frame
prob_combined_merged <- left_join(prob_combined_merged, 
                                  prob_pedersen_land_small, 
                                  by = c("bundesland", "year"))

stopifnot(nrow(prob_combined_merged) == nrow(prob_combined))


## save dataset of coalition probabilities for federal elections
saveRDS(prob_combined_merged, file = "data_coalition_predictions_land.rds")

