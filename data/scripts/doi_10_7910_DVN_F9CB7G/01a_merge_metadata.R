#' ---
#' title: "Legislating Landlords: Private Interests, Issue Emphasis, and Policy Positions (LSQ)"
#' subtitle: "01a_merge_metadata.R"
#' author: "Authors: Stefan Mueller and Jihed Ncib"
#' date: "Note: Code compiled successfully on `r format(Sys.time(), '%d %B %Y')`"
#' ---

# script to merge and harmonise the text corpora and datasets on legislators
# and their interests

# If the code does not run, one or more packages may have been 
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the 
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2024-05-06")
# Instead of adjusting the library() function for each package, 
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2024-05-06")
# More details are available at: https://groundhogr.com/using/

# load packages

library(tidyverse) # CRAN v2.0.0 
library(cowplot)   # CRAN v1.1.1
library(scales)    # CRAN v1.2.1

# load custom ggplot2 theme
source("function_theme_base.R")

# load harmonised dataset
dat_harmonised_raw <- readRDS("harmonisedlegislator.rds")

# remove unnecessary whitespaces around names
dat_harmonised_raw <- dat_harmonised_raw |> 
    mutate(candidate = str_squish(candidate))


# select relevant columns and recode election year
dat_harmonised <- dat_harmonised_raw |> 
    mutate(election_year = as.integer(election_year)) |> 
    dplyr::select(harmonised, 
           candidate, 
           election_year,
           birth, seniority, 
           constituency_name, 
           party, first_pref_share, 
           gender, district_magnitude_man,
           running_sum, elected_sum)


# save names of constituencies for later use (change in property prices)
dat_constituencies <- dat_harmonised |> 
    ungroup() |> 
    mutate(constituency_name = str_squish(constituency_name)) |> 
    select(constituency_name) |> 
    unique() |> 
    arrange(constituency_name) |> 
    mutate(county = "")
# write_csv(dat_constituencies, "data_constituencies.csv")

# load ownership data
dat_ownership <- readRDS("data_ownership.rds")

table(dat_ownership$year)


# recode ownership categories

dat_ownership <- dat_ownership |> 
    mutate(ownership_clean = 
               dplyr::recode(
                   ownership,
                   "0" = "No private residence/ownership",
                   "1" = "Personal residence",
                   "5" = "Personal residence",
                   "2"  = "Rental property",
                   "3" = "Rental property",
                   "6" = "Rental property",
                   "4" = "Other",
                   "7" = "Other",
                   "8" = "Rental property")) |> 
    mutate(ownership_3 = ifelse(
        ownership %in% c(1,5), "Homeowner Only", 
        ifelse(ownership %in% c(2,3,6,8), "Landlord", 
               "Not Homeowner or Landlord"))) |> 
    mutate(ownership_2 = ifelse(
        ownership %in% c(1,2,3,5,6,8), 
        "Homeowner or Landlord", "Not Homeowner or Landlord"))


table(dat_ownership$propnum,
      dat_ownership$ownership_3)

prop.table(table(dat_ownership$ownership_3))

table(dat_ownership$ownership)

# merge harmonised and ownership data by cycle
# this requires selecting only relevant years (electoral cycles)

dat_ownership_1115 <- dat_ownership |> 
    filter(between(year, 2011, 2015))

dat_ownership_1619 <- dat_ownership |> 
    filter(between(year, 2016, 2019))

dat_ownership_20 <- dat_ownership |> 
    filter(year >= 2020)

# add data on housing committee membership and connect with main dataset
dat_committee_1620 <- read_csv("data_housing_committee_2016-2020.csv") |> 
    rename(harmonised = mpname)

dat_committee_1620$housing_committee <- 1

# add data on housing committee membership and connect with main dataset
dat_committee_20 <- read_csv("data_housing_committee_2020-.csv") |> 
    rename(harmonised = mpname)

dat_committee_20$housing_committee <- 1
dat_committee_20$harmonised <- str_squish(dat_committee_20$harmonised)

dat_ownership_1619$harmonised <- str_squish(dat_ownership_1619$harmonised)
dat_committee_1620$harmonised <- str_squish(dat_committee_1620$harmonised)


dat_ownership_1619 <- left_join(dat_ownership_1619,
                                dat_committee_1620,
                                by = "harmonised")


dat_ownership_1619 |> 
    filter(housing_committee == 1) |> 
    select(harmonised) |> 
    unique() |> 
    nrow()

# note: needs to be 16 members


dat_ownership_20 <- left_join(dat_ownership_20,
                              dat_committee_20,
                              by = "harmonised")


dat_ownership_20 |> 
    filter(housing_committee == 1) |> 
    select(mpname) |> 
    unique() |> 
    nrow()
# needs to be 9 members



# filter candidates from each election
dat_cand_11 <- filter(dat_harmonised, between(election_year, 2011, 2015))
dat_cand_16 <- filter(dat_harmonised, between(election_year, 2016, 2019))
dat_cand_20 <- filter(dat_harmonised, between(election_year, 2020, 2025))



# 2011-2015: check differences

setdiff(dat_ownership_1115$harmonised,
        dat_cand_11$harmonised)

setdiff(dat_cand_11$harmonised,
        dat_ownership_1115$harmonised)


setdiff(dat_ownership_1619$harmonised,
        dat_cand_16$harmonised)

setdiff(dat_cand_16$harmonised,
        dat_ownership_1619$harmonised)


# 2020--2022: check differences

setdiff(dat_cand_20$harmonised,
        dat_ownership_20$harmonised)

dat_ownership_20 |> 
    filter(str_detect(harmonised, "devlin"))

setdiff(str_squish(dat_ownership_20$harmonised),
        dat_cand_20$harmonised)


# merge ownership data and election information
dat_meta_1116 <- left_join(dat_ownership_1115, 
                           dat_cand_11,
                           by = c("harmonised")) |> 
    mutate(term = "2011-2016") |> 
    mutate(housing_committee = NA)


# check for missing
dat_meta_1116 |> 
    filter(is.na(candidate)) |> 
    nrow()


dat_meta_1619 <- left_join(dat_ownership_1619, 
                           dat_cand_16,
                           by = "harmonised") |> 
    mutate(term = "2016-2020") |> 
    mutate(housing_committee = ifelse(is.na(housing_committee), 0, housing_committee))

table(dat_meta_1619$housing_committee)

# check for missing
dat_meta_1619 |> 
    filter(is.na(candidate)) |> 
    nrow()


dat_meta_20 <- left_join(dat_ownership_20, 
                         dat_cand_20,
                         by = "harmonised") |> 
    filter(mpname != "deasy john") |>  # remove politician who was not elected in 2020
    mutate(term = "2020-") |> 
    mutate(housing_committee = ifelse(is.na(housing_committee), 0, housing_committee))

table(dat_meta_20$party, useNA = "always")

table(dat_meta_20$housing_committee)

dat_meta_20 |> 
    filter(is.na(candidate)) |> 
    select(harmonised)


# bind all cycle-level metadata
dat_meta_all_raw <- bind_rows(
    dat_meta_1116,
    dat_meta_1619,
    dat_meta_20 
)

nrow(dat_meta_all_raw)

dat_meta_all_raw |> unique() |> nrow()

# load dataset on development of house prices
dat_houseprices <- readRDS("data_housprices_county.rds")

dat_constituences_recoded <- read_csv("data_constituencies.csv")

dat_constituences_recoded <- rename(dat_constituences_recoded, county_recoded = county)

# remove whitespace in constituency names
dat_meta_all_raw$constituency_name <- str_squish(dat_meta_all_raw$constituency_name)

# merge broader constituencies corresponding to county boundaries
dat_meta_all_const <- left_join(dat_meta_all_raw, 
                                dat_constituences_recoded,
                                by = "constituency_name")

nrow(dat_meta_all_const)

# merge changes in houseprices with metadata
dat_meta_all <- left_join(
    dat_meta_all_const,
    dat_houseprices, 
    by = c("year", "county_recoded"))

nrow(dat_meta_all)

table(dat_meta_all$year,
      dat_meta_all$ownership_2)


## recode party affiliation
dat_meta_all <- dat_meta_all |> 
    mutate(party = ifelse(str_detect(party, "Boxer"), "IND", party)) |> 
    mutate(party_recoded = dplyr::recode(
        party, "FF" = "Fianna Fáil",
        "FG" = "Fine Gael",
        "SF" = "Sinn Féin",
        "GP" = "Green Party",
        "AAA" = "AAA/S-PBP",
        "Aon" = "Other/Independents",
        "GP" = "Green Party",
        "LAB" = "Labour",
        "I4C" = "Other/Independents",
        "IA" = "Other/Independents",
        "IND" = "Other/Independents",
        "PBP" = "AAA/S-PBP",
        "WUAG" = "Other/Independents",
        "SOC" = "Other/Independents",
        "AAA-PBP" = "AAA/S-PBP",
        "SOL-PBP" = "AAA/S-PBP",
        "SD" = "Social Democrats"))


dat_meta_all <- dat_meta_all |> 
    mutate(party_broad = dplyr::recode(party_recoded,
                                       "AAA/S-PBP" = "Greens and\nLeft Bloc",
                                       "Green Party" = "Greens and\nLeft Bloc",
                                       "Labour" = "Greens and\nLeft Bloc",
                                       "Social Democrats" = "Greens and\nLeft Bloc"))


table(dat_meta_all$ownership_2)

# check whether ownership has changed
dat_meta_all <- dat_meta_all |> 
    arrange(mpname, year) |> 
    group_by(mpname) |> 
    mutate(ownership_2_lag = lag(ownership_2)) |> 
    mutate(change_ownership = ifelse(ownership_2_lag != ownership_2, 1, 0)) |> 
    mutate(change_became_owner = ifelse(ownership_2_lag == "No Homeowner or Landlord" 
                                        & ownership_2 == "Homeowner or Landlord", 1, 0)) |> 
    
    ungroup()

table(dat_meta_all$change_ownership)

table(dat_meta_all$change_ownership,
      dat_meta_all$change_became_owner)

table(dat_meta_all$change_ownership)

# ownership status changed for 95 TDs
dat_meta_all |> 
    ungroup() |> 
    filter(change_ownership == 1) |> 
    select(mpname) |> 
    nrow()

dat_meta_all |> 
    mutate(ownership_num = ifelse(ownership_2 == "Homeowner or Landlord", 1, 0)) |> 
    group_by(year) |> 
    summarise(prop_ownership = mean(ownership_num, 
                                    na.rm = TRUE),
              sum_homeowners = sum(ownership_num, 
                                   na.rm = TRUE),
              tds = n()) 


dat_compare_ownership <- dat_meta_all |> 
    mutate(ownership_num = ifelse(ownership_2 == "Homeowner or Landlord", 1, 0)) |> 
    group_by(party_recoded, party_broad, year) |> 
    summarise(prop_ownership = mean(ownership_num, 
                                    na.rm = TRUE),
              sum_homeowners = sum(ownership_num, 
                                   na.rm = TRUE),
              tds = n()) |> 
    filter(!is.na(party_broad))


dat_compare_ownership <- dat_compare_ownership |> 
    mutate(party_broad = str_replace_all(party_broad, "Independents", "Ind.")) |> 
    mutate(party_broad = factor(party_broad, 
                                levels = c("Fine Gael",
                                           "Fianna Fáil",
                                           "Other/Ind.",
                                           "Greens and\nLeft Bloc",
                                           "Sinn Féin")))


table(dat_compare_ownership$party_recoded)

order_parties <- c("Fine Gael",
                   "Fianna Fáil",
                   "Sinn Féin", 
                   "Labour", 
                   "Green Party",
                   "Social Democrats",
                   "AAA/S-PBP",
                   "Other/Independents")


dat_compare_ownership <- dat_compare_ownership |> 
    mutate(party_recoded = factor(party_recoded, levels = order_parties))


dat_mean_ownership <- dat_compare_ownership |> 
    group_by(party_recoded) |> 
    summarise(mean_ownership = mean(prop_ownership))

# Figure A04 ----
ggplot(dat_compare_ownership, aes(x = year,
                                  y = prop_ownership,
                                  colour = party_recoded)) +
    geom_hline(data = dat_mean_ownership,
               aes(yintercept = mean_ownership),
               linetype = "dashed",
               colour = "grey50") +
    geom_point(size = 5) +
    geom_line(group = 1) +
    scale_x_continuous(breaks = c(seq(2013, 2022, 1))) +
    facet_wrap(~party_recoded, nrow = 2) +
    geom_text(aes(label = sum_homeowners), colour = "white",
              size = 3) +
    scale_y_continuous(limits = c(-0.1, 1),
                       breaks = c(seq(0, 1, 0.2)),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(
        values = c("Sinn Féin" = "#326760",
                   "Fine Gael" = "#009FF3",
                   "Fianna Fáil" = "#66BB66",
                   "AAA/S-PBP" = "#660000",
                   "Social Democrats" = "#752F8B",
                   "Aontú" = "#44532A",
                   "Labour" = "#CC0000",
                   "Other/Independents" = "grey50",
                   "Green Party" = "99CC33")) +
    labs(x = NULL, y = "Percentage of Landlords") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 0)) 
ggsave("fig_a04.pdf",
       width = 9, height = 5)
ggsave("fig_a04.eps",
       width = 9, height = 5)

# repeat for gov_opp
dat_compare_ownership_govopp <- dat_meta_all |> 
    mutate(gov_opp = case_when(
        term == "2011-2016" & party %in% c("FG", "LAB") ~ "Government",
        term == "2016-2020" & party %in% c("FG") ~ "Government",
        term == "2020-" & party %in% c("FG", "FF", "GP") ~ "Government",
        TRUE ~ "Opposition"
    )) |> 
    mutate(ownership_num = ifelse(ownership_2 == "Homeowner or Landlord", 1, 0)) |> 
    group_by(gov_opp, year) |> 
    summarise(prop_ownership = mean(ownership_num, 
                                    na.rm = TRUE),
              sum_homeowners = sum(ownership_num, 
                                   na.rm = TRUE),
              tds = n()) |> 
    filter(!is.na(gov_opp))

dat_mean_ownership_govopp <- dat_compare_ownership_govopp |> 
    group_by(gov_opp) |> 
    summarise(mean_ownership = mean(prop_ownership))

dat_mean_ownership_govopp  
    

dat_compare_ownership_govopp <- dat_compare_ownership_govopp |>  
    mutate(type = "Landlord or Owner of Multiple Properties")


# Figure 01 ----
p_broad <- ggplot(dat_compare_ownership_govopp, aes(x = year,
                                  y = prop_ownership,
                                  colour = gov_opp,
                                  shape = gov_opp)) +
    geom_point(size = 5) +
    geom_line() +
    scale_x_continuous(breaks = c(seq(2013, 2022, 1))) +
    scale_y_continuous(limits = c(0, 0.6),
                       breaks = c(seq(0, 1, 0.2)),
                       labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~type) +
    labs(x = NULL, y = "Percentage of TDs",
         title = "(a) Broad Classification") +
    scale_colour_manual(values = c("black", "grey60")) +
    scale_shape_manual(values = c(16, 15)) +
    theme(legend.position = c(0.1, 0.2),
          strip.text.x = element_text(face = "plain"),
          legend.title = element_blank()) 

# 3-category measure
dat_compare_ownership_govopp3 <- dat_meta_all |> 
    mutate(gov_opp = case_when(
        term == "2011-2016" & party %in% c("FG", "LAB") ~ "Government",
        term == "2016-2020" & party %in% c("FG") ~ "Government",
        term == "2020-" & party %in% c("FG", "FF", "GP") ~ "Government",
        TRUE ~ "Opposition"
    )) |> 
    group_by(gov_opp, ownership_3, year) |> 
    count() |> 
    filter(!is.na(gov_opp)) |> 
    group_by(gov_opp, year) |> 
    mutate(prop = n / sum(n))

dat_compare_ownership_govopp3 <- dat_compare_ownership_govopp3 |> 
    mutate(ownership_3 = dplyr::recode(ownership_3, 
                                       "Homeowner Only" = "Owner of Multiple Properties",
                                       "Landlord" = "Landlord with Rental Income")) |> 
    filter(ownership_3 != "Not Homeowner or Landlord")



p_detailed <- ggplot(dat_compare_ownership_govopp3, aes(x = year,
                                         y = prop,
                                         colour = gov_opp,
                                         shape = gov_opp)) +
    geom_point(size = 5) +
    geom_line() +
    facet_wrap(~ownership_3) +
    scale_x_continuous(breaks = c(seq(2013, 2022, 1))) +
    scale_y_continuous(limits = c(0, 0.6),
                       breaks = c(seq(0, 1, 0.2)),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Percentage of TDs",
         title = "(b) Detailed Classification") +
    # scale_colour_manual(values = c("darkgreen", "darkred")) +
    scale_colour_manual(values = c("black", "grey60")) +
    scale_shape_manual(values = c(16, 15)) +
    theme(legend.position = c(0.1, 0.2),
          strip.text.x = element_text(face = "plain"),
          axis.text.x = element_text(angle = 90),
          legend.title = element_blank()) 

# combine to Figure 01 
plot_grid(p_broad, NULL,
          p_detailed, nrow = 3,
          rel_heights = c(1, 0.05, 1))
ggsave("fig_01.pdf",
       width = 9, height = 7)
ggsave("fig_01.eps",
       width = 9, height = 7)


# some summary stats for papers
dat_compare_ownership_govopp3 |> 
    group_by(gov_opp, year) |> 
    summarise(sum = sum(prop))

dat_compare_ownership_govopp |> 
    group_by(gov_opp) |> 
    filter(prop_ownership == min(prop_ownership))


dat_compare_ownership_govopp |> 
    group_by(gov_opp) |> 
    filter(prop_ownership == max(prop_ownership))

dat_compare_ownership_govopp3 |> 
    group_by(gov_opp, ownership_3) |> 
    filter(prop == min(prop))

dat_compare_ownership_govopp3 |> 
    group_by(gov_opp, ownership_3) |> 
    summarise(mean = mean(prop))

dat_compare_ownership_govopp3 |>
    group_by(gov_opp) |> 
    summarise(mean = mean(prop))

dat_compare_ownership_govopp3 |> 
    group_by(gov_opp, ownership_3) |> 
    filter(prop == max(prop))


dat_count_owner <- dat_meta_all |> 
    group_by(ownership_3, year) |> 
    count() |> 
    group_by(year) |> 
    mutate(prop_cat = n / sum(n)) |> 
    mutate(n_prop = paste0(n, " (", 100 * round(prop_cat, 3), "%)")) |> 
    select(ownership_3, Year = year, n_prop) |> 
    pivot_wider(
        names_from = ownership_3,
        values_from = c(n_prop)
    ) |> 
    rename(Neither = `Not Homeowner or Landlord`) |> 
    select(Year, Landlord, everything())


min(dat_meta_all$year)

table(dat_meta_all$year)


dat_meta_all <- dat_meta_all |> 
    mutate(dublin_dummy = ifelse(str_detect(constituency_name, "Dublin"),
                                 "Dublin", "Other"))

# recode government/opposition status
dat_meta_all <- dat_meta_all |> 
    mutate(gov_opp = case_when(
        term == "2011-2016" & party %in% c("FG", "LAB") ~ "Government",
        term == "2016-2020" & party %in% c("FG") ~ "Government",
        term == "2020-" & party %in% c("FG", "FF", "GP") ~ "Government",
        TRUE ~ "Opposition"
    ))

# save dataset
saveRDS(dat_meta_all, "dat_meta_all.rds")

