# install dgo and dependencies
#install.packages("https://cran.r-project.org/src/contrib/Archive/dgodata/dgodata_0.0.2.tar.gz", repos=NULL, type="source")
#install.packages("https://cran.r-project.org/src/contrib/Archive/dgo/dgo_0.2.15.tar.gz", repos = NULL, type = "source")
#devtools::install_version("rstan", version = "2.21.2")
#install.packages("assertthat")

library(dplyr); library(magrittr); library(hrbrthemes); library(dgo); library(rstan)
library(dgodata); library(readr); library(dplyr); library(tidyr)
rstan_options(auto_write = TRUE)
options(mc.cores = 10)

# Load Nationscape
load("auxiliary_analyses/ns_df.RData")

# levels of lucid ethnicity variable
hisp.levels <- c("Not Hispanic", "Mexican", "Cuban",
                 "Argentinian", "Colombian", "Ecuadorian",
                 "Salvadoran", "Guatemalan","Nicaraguan",
                 "Panamanian","Peruvian","Spanish",
                 "Venezuelan", "Other Hispanic","Puerto Rican")

# recoding nationscape
ns.df <- within(ns.df, {
 biden_fav <- ifelse(cand_favorability_biden == 999, NA, cand_favorability_biden)
 trump_biden <- ifelse(trump_biden > 3, NA, trump_biden)
 vote_2020 <- ifelse(vote_2020 > 3, NA, vote_2020)
 vote_2020_retro <- ifelse(vote_2020_retro > 3, NA, vote_2020_retro)
 hispanic_og <- hispanic
 hispanic <- plyr::mapvalues(hispanic, 1:15, hisp.levels)
 biden_fav_discretized <- as.numeric(biden_fav < 3)
 month.year <- as.numeric(paste(lubridate::year(start_date),
                                stringr::str_pad(lubridate::month(start_date),
                                                 2, pad = "0"), sep = ""))
 bi.weekly <- cut(ns.df$wave, 38)}) %>%
 filter(registration == 1 & state != "") %>%
 mutate(vote_2020 = plyr::mapvalues(vote_2020, c(2,1), c(1,2))) %>%
 mutate(vote_2020_retro = plyr::mapvalues(vote_2020_retro, c(2,1), c(1,2))) %>%
 mutate(biden_all = as.character(paste(trump_biden, vote_2020, vote_2020_retro, sep = ""))) %>%
 mutate(biden_all = ifelse(biden_all == "NANANA", NA, biden_all)) %>%
 mutate(biden_all = as.numeric(ifelse(is.na(biden_all), NA, grepl("1", biden_all)))) %>%
 mutate(bi.weekly = as.numeric(bi.weekly))

ns.df %>%
 filter(!(hispanic_og == 1 & race_ethnicity > 1)) %>%
 mutate(hispanic_set1 = paste(as.numeric(hispanic != "Not Hispanic"))) -> ns.df

biden_choice_set1 <- shape(ns.df,
                           item_names = "biden_all",
                           time_name = "month.year",
                           geo_name = "state",
                           group_names = "hispanic_set1")

fit_biden_choice_set1 <- dgmrp(biden_choice_set1, iter = 2000, chains = 4,
                               cores = 10, seed = 47)

dgo::summarize(fit_biden_choice_set1) -> state.monthly.predictions.choice_set1
covid_df <- read_csv("auxiliary_analyses/covid_data.csv")

covid_df %>%
 mutate(date = paste(date)) %>%
 left_join(.,
           state.monthly.predictions.choice_set1 %>%
            mutate(date = paste(lubridate::ym(month.year)),
                   hispanic_set1 = plyr::mapvalues(hispanic_set1,
                                                   c("0", "1"),
                                                   c("Not Hispanic", "Hispanic"))),
           by = c("state", "date")
 ) %>%
 filter(date < "2020-12-01") %>%
 group_by(state, hispanic_set1) %>%
 mutate(diff = median-dplyr::lag(median, order_by = hispanic_set1),
        case_change = cases-dplyr::lag(log(cases+.001), order_by = hispanic_set1),
        death_change = deaths-dplyr::lag(log(deaths+.001), order_by = hispanic_set1),
        retail = retail/100,
        grocery = grocery/100,
        parks = parks/100,
        transit = transit/100,
        workplaces = workplaces/100,
        residences = residences/100,
        cases = log(cases+1),
        deaths = log(deaths+1)) %>%
 mutate(change_wording = as.numeric(date >= "2020-09-01")) -> biden_df

biden_df$lockdown_scale <- -as.vector(psych::alpha(biden_df[, c("retail", "grocery", "transit", "workplaces", "residences")], check.keys = TRUE)$scores)
biden_df$deaths[is.na(biden_df$deaths)] <- 0
