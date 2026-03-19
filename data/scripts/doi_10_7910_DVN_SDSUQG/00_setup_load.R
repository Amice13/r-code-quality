suppressMessages({
    library(tidyverse)
    library(scales)
    library(patchwork)
    library(wacolors)
    library(here)
    library(Hmisc)
    library(nbhdmodel)
    library(s2)
    library(posterior)

    summarize <- dplyr::summarize
})
invisible(lapply(Sys.glob(here("replication/utils/*.R")), source)) # load in utilities

M_PER_MI = 1609.34

# colors and themes
options(
    ggplot2.continuous.colour=scale_color_wa_c,
    ggplot2.continuous.fill=scale_fill_wa_c,
    ggplot2.discrete.colour=scale_color_wa_d,
    ggplot2.discrete.fill=scale_fill_wa_d
)
theme_paper = function() theme_bw(base_size=10, base_family="Times")
PAL_CITY = c(Miami="#465177", NYC="#E4C22B", Phoenix="#965127")
lbl_city = function(x) c(miami="Miami", `new-york`="NYC", phoenix="Phoenix", pooled="Pooled")[x]
GOP_DEM = c("#A0442C", "#B25D4C", "#C27568", "#D18E84", "#DFA8A0",
            "#EBC2BC", "#F6DCD9", "#F9F9F9", "#DAE2F4", "#BDCCEA",
            "#9FB6DE", "#82A0D2", "#638BC6", "#3D77BB", "#0063B1")

# Load data ---------
resp_d = get_clean_responses(here("data/clean_responses.rds"))
block_d = get_block_data()
adj_gr = get_graphs()

raw_model_d = make_model_frame(resp_d, block_d, adj_gr)

# Define model formulae ---------
form_full = ~ is_church + log(dist_church) + is_park +
    is_school*children_home + log(dist_school)*children_home +
    same_bg + same_tract * same_rr +
    sqrt(pop/1e4) + sqrt(area) +
    pct_race + pct_race:minority +
    pct_party + pct_party:party +
    pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    log(med_inc) + log(med_inc):educ_grp +
    age + educ_grp + retired + sqrt(tenure) +
    party + minority + homeowner + children_home

form_baseline = ~ is_church + log(dist_church) + is_park +
    is_school*children_home + log(dist_school)*children_home +
    same_bg + same_tract * same_rr +
    sqrt(pop/1e4) + sqrt(area) +
    age + educ_grp + retired + sqrt(tenure) +
    party + minority + homeowner + children_home

form_baseline_nodemg = ~ is_church + log(dist_church) + is_park +
    is_school + log(dist_school) +
    same_bg + same_tract * same_rr +
    sqrt(pop/1e4) + sqrt(area)

#' Pre-process raw model data
#'
#' Designed to work with our particular survey responses; not a general-purpose
#' function.
#'
#' @param d the raw model data, from [make_model_frame()]
#'
#' @return a processed data.frame
preproc = function(d) {
    d %>%
    mutate(block = fips,
           block_group = str_sub(block, 1, 12),
           tract = str_sub(block, 1, 11),
           is_school = coalesce(type == "school", FALSE),
           is_church = coalesce(type == "church", FALSE),
           is_park = coalesce(type == "park", FALSE),
           area = area / M_PER_MI^2,
           pop_race = case_when(race == "white" ~ pop_white,
                                race == "black" ~ pop_black,
                                race == "hisp" ~ pop_hisp,
                                T ~ pop - pop_white - pop_black - pop_hisp),
           pop_race = pmax(0, pop_race),
           race = if_else(race %in% c("white", "black", "hisp"), race, "other"),
           party = str_sub(party_combined, 1, 3),
           party_strong = party_combined %in% c("dem_strong", "rep_strong"),
           reg_party = case_when(
               party_combined %in% c("dem_strong", "dem_lean") ~ democrats,
               party_combined %in% c("rep_strong", "rep_lean") ~ republicans,
               T ~ registrants - democrats - republicans),
           pct_race = pop_race / (pop + 0.001),
           pct_party = reg_party / (registrants + 0.001),
           minority = !(race == "white"),
           income_est  = case_when(income == "Less than $30,000" ~ 15e3,
                                   income == "$30,000 – 39,999" ~ 35e3,
                                   income == "$40,000 – 49,999" ~ 45e3,
                                   income == "$50,000 – 59,999" ~ 55e3,
                                   income == "$60,000 – 69,999" ~ 65e3,
                                   income == "$70,000 – 79,999" ~ 75e3,
                                   income == "$80,000 – 89,999" ~ 85e3,
                                   income == "$90,000 – 99,999" ~ 95e3,
                                   income == "$100,000 -119,999" ~ 110e3,
                                   income == "$120,000  or More" ~ 160e3),
           income_dist = abs(log(income_est) - log(med_inc)) /
               mean(log(med_inc), na.rm=T),
           educ_grp = case_when(education %in% c("no_hs", "some_hs",
                                                 "hs", "some_coll") ~ "no_coll",
                                education %in% c("grad_2yr", "grad_4yr", "postgrad") ~ "coll"),
           pct_educ = if_else(educ_grp == "coll", pct_college, 1 - pct_college),
           pct_own = if_else(homeowner == "Yes", pct_homeown, 1 - pct_homeown),
           age_grp = case_when(age <= 40 ~ "00_40",
                               age > 40 & age <= 55 ~ "41_55",
                               age > 55 & age <= 65 ~ "56_65",
                               age > 65 & age <= 75 ~ "66_75",
                               age > 75 ~ "76_99")) %>%
    select(city, block, block_group, tract, rr_id,
           is_school, is_church, is_park, dist_school, dist_church,
           id, incl, pop, race, pop_race,
           reg=registrants, reg_party, pct_race,
           pct_party, party, party_combined, party_strong,
           gender, minority, age, age_grp, educ_grp, pct_educ,
           med_inc, income_est, income_dist,
           tenure=housing_tenure, homeowner, retired,
           children_home, pct_own, pct_homeown, pct_college,
           area, any_of("ring"), dist,
           trust_1, trust_2, trust_3, housing,vote_2020) %>%
    group_by(id) %>%
    mutate(same_bg = block_group == block_group[1],
           same_tract = tract == tract[1],
           same_rr = rr_id == rr_id[1])
}
