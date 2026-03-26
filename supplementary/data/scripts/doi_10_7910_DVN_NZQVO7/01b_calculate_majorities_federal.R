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
## and to calculate Pedersen's volatility index for the federal elections
## Note: The polls are scraped from wahlrecht.de in June 2018


## load required packages

library(readr) # Read Rectangular Text Data, CRAN v1.3.1
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(coalitions) # Bayesian "Now-Cast" Estimation of Event Probabilities in Multi-Party Democracies, CRAN v0.6.15
library(tidyr) # Tidy Messy Data, CRAN v1.1.2
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(purrr) # Functional Programming Tools, CRAN v0.3.4


## load polling data

dta_polls <- read_csv("data_polls_federal_raw.csv") %>% 
    mutate(date = as.Date(date, "%d/%m/%y"))

remove_percentage <- function(x){
    str_replace(x, 
                pattern = fixed('%'), '')
    
}


dta_polls <- dta_polls %>% 
    mutate_all(funs(remove_percentage)) %>% 
    mutate(cdu = as.numeric(cdu),
           spd = as.numeric(spd),
           greens = as.numeric(greens),
           left = as.numeric(left),
           fdp = as.numeric(fdp),
           afd = as.numeric(afd),
           pirates = as.numeric(pirates),
           other = as.numeric(other)) %>% 
    filter(!is.na(date)) %>% 
    select(-c(Befragte, Zeitraum))


## change missing respondents values to 1000 (the usual sample size)
dta_polls_relevant <- dta_polls %>% 
    mutate(respondents = 1000) %>% 
    mutate(land = "federal election")

dta_election_dates <- read_csv(file = "data_survey_dates_federal.csv") %>% 
    mutate(election_date = as.Date(election_date, "%d/%m/%y")) %>% 
    mutate(year = election_year) %>% 
    separate(survey_dates, into = c("date_survey_start", "date_survey_end"), 
             sep = "\\-",
             remove = FALSE) %>% 
    mutate(date_survey_start = as.Date(date_survey_start, "%d.%m.%Y")) %>% 
    mutate(date_survey_end = as.Date(date_survey_end, "%d.%m.%Y")) %>% 
    mutate(length_survey_days = date_survey_end - date_survey_start)

dta_survey_metadata_elections <- dta_election_dates %>% 
    select(bundesland, year, date_survey_start, date_survey_end,
           length_survey_days) %>% 
    mutate(bundesland = str_to_lower(bundesland)) %>% 
    mutate(election_id = paste(year, bundesland))


## function to estimate coalition probabilities
prob_federal <- function(election_year, land_select, days,
                      list_coalitions) {
    
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

    date_polls_start <- date_survey_start - days
    
    
    ## now subset polls
    
    dta_polls_relevant_subset <- dta_polls_relevant %>% 
        mutate(date = as.Date(date)) %>% 
        filter(land == land_select) %>% ## only select relevant Land
        filter(date > date_polls_start) %>%  ## limit range
        filter(date < date_survey_start) %>% 
        select(cdu, spd, fdp, left, greens, everything())
    
    elong <- collapse_parties(dta_polls_relevant_subset) 
    
    elong
    set.seed(1)     ## for reproducibility
    
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
        c("spd", "fdp"),
        c("spd", "left", "greens"),
        c("cdu", "spd"))
    
    
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
    
    
    ## the majorities table for each date will have 1 row per simulation
    ## and one column per coalition
    
    elong_majorities_df <- elong_majorities %>% 
        slice(1) %>% 
        select(majorities) %>% 
        unnest()
    
    elong_majorities_df
    
    probs_surplus <- calculate_probs(elong_majorities_df,
                                     list('cdu', 'cdu_fdp', 'cdu_greens',
                                          'cdu_fdp_greens', 'spd', 
                                          'fdp_greens_spd', 'afd_cdu', 
                                          'left_spd', 'greens_spd', 
                                          'fdp_spd', 'greens_left_spd', 'cdu_spd'), 
                                     exclude_superior = FALSE) %>%
        rename(coalition = coalition,
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


list_elections <- unique(dta_survey_metadata_elections$election_id)
list_elections

## estimate probabilities by using surveys published within one year (365 days)
## before the start date of the survey

prob_2017_federal <- prob_federal(election_year = 2017, days = 365,
                           land_select = "federal election")

prob_2013_federal <- prob_federal(election_year = 2013, days = 365,
                                  land_select = "federal election")

prob_2009_federal <- prob_federal(election_year = 2009, days = 365,
                                  land_select = "federal election")


prob_combined_federal_one_year <- bind_rows(prob_2009_federal,
                                   prob_2013_federal,
                                   prob_2017_federal) %>% 
    arrange(year, bundesland)


## use polls from half a year before the survey start date (days = 182)
## as a robustness test

prob_2017_federal_halfyear <- prob_federal(election_year = 2017, days = 182,
                                  land_select = "federal election")

prob_2013_federal_halfyear <- prob_federal(election_year = 2013, days = 182,
                                  land_select = "federal election")

prob_2009_federal_halfyear <- prob_federal(election_year = 2009, days = 182,
                                  land_select = "federal election")


prob_combined_federal_half_year <- bind_rows(prob_2009_federal_halfyear,
                                   prob_2013_federal_halfyear,
                                   prob_2017_federal_halfyear) %>% 
    arrange(year, bundesland) %>% 
    dplyr::select(coalition, year, bundesland, probability, enp) %>% 
    dplyr::rename(probability_half_year = probability,
                  enp_half_year = enp)


## merge both data frames
prob_combined_federal <- left_join(prob_combined_federal_one_year, 
                                   prob_combined_federal_half_year, 
                                   by = c("year", "bundesland", "coalition"))

## check correlation between surveys from one year or half a year before the election
cor.test(prob_combined_federal$probability_half_year, prob_combined_federal$probability)


polls_unique <- prob_combined_federal %>% 
    select(bundesland, year, number_polls) %>%
    unique()

sum(polls_unique$number_polls)

## recode coalition into GLES format (change order)
recode_coalition_gles <- c("'greens_left_spd'='spd_left_greens';
                           'greens_spd'='spd_greens';'fw_greens_spd'='spd_greens_fw';'afd_cdu'='cdu_afd';'fdp_spd'='spd_fdp';'fdp_greens_spd'='spd_fdp_greens';
                           'left_spd'='spd_left'")

prob_combined_federal <- prob_combined_federal %>% 
    mutate(coalition = car::recode(coalition, recode_coalition_gles)) %>% 
    mutate(spd_in_coa = stringr::str_detect(coalition, "spd")) %>% 
    mutate(cdu_in_coa = stringr::str_detect(coalition, "cdu"))

cor.test(prob_combined_federal$probability,
         prob_combined_federal$probability_surplus)


## load election results from previous election

election_results_federal <- read_csv("data_election_results_federal.csv")

prob_pedersen_federal <- prob_combined_federal %>% 
    left_join(election_results_federal, by = c("bundesland", "year")) %>% 
    select(bundesland, year, cdu:afd, starts_with("votes_lag"), -c(starts_with("prob"))) %>% 
    unique()

## replace NA with 0 (doesn't make a difference)

prob_pedersen_federal[is.na(prob_pedersen_federal)] <- 0 

prob_pedersen_federal <- prob_pedersen_federal %>% 
    mutate(votes_lag_piraten = 0,
           votes_lag_afd = 0)

## see Pedersen (1979, p. 4)
prob_pedersen_federal <- prob_pedersen_federal %>% 
    mutate(pedersen_vol = 100 * (0.5 * (abs(cdu - votes_lag_cdu) +
                                            abs(spd - votes_lag_spd) + 
                                            abs(greens - votes_lag_greens) + 
                                            abs(left - votes_lag_left) +
                                            abs(fdp - votes_lag_fdp) + 
                                            abs(afd - votes_lag_afd))))

prob_pedersen_federal_small <- prob_pedersen_federal %>% 
    select(pedersen_vol, bundesland, year)

## join Pedersen's volatility index and coalition predictions
prob_combined_federal <- left_join(prob_combined_federal, 
                                   prob_pedersen_federal_small, 
                                   by = c("bundesland", "year"))


## save dataset of coalition probabilities for federal elections
saveRDS(prob_combined_federal, file = "data_coalition_predictions_federal.rds")


