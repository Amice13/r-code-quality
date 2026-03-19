suppressMessages({
    library(tidyverse)
    library(scales)
    library(patchwork)
    library(wacolors)
    library(here)
    library(Hmisc)
    library(nbhdmodel)
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


get_clean_responses_nycc = function(
        path = here("data/clean_responses.rds"),
        raw_path = Sys.glob(here("data-raw/responses*.csv")),
        distribution = c("email", "anonymous")) {
    if (file.exists(path)) {
        read_rds(path)
    } else if (distribution == "email") {
        cli::cli_alert_info("Reading raw response file.")
        if (length(raw_path) > 1) {
            choices = str_glue("{basename(raw_path)} (Last modified {file.mtime(raw_path)})")
            choice = menu(choices, title="Which response file to use?")
            if (choice == 0) stop("Must select a file.")

            raw_path = raw_path[choice]
            cat("Using ", basename(raw_path), "\n")
        }
        dir.create(dirname(path), showWarnings = FALSE)

        raw_headings = read_csv(raw_path, n_max=1) %>%
            suppressMessages() %>%
            clean_names()
        raw = read_csv(raw_path, skip=3, col_names=colnames(raw_headings)) %>%
            suppressMessages()%>%
            select(-neighborhood)%>%
            bind_cols(read_csv(raw_path,  col_names=colnames(raw_headings))%>%
                          select(neighborhood)%>%
                          slice(4:n()))


        d = raw %>%
            filter(distribution_channel == distribution & recipient_email!='jrbrown@g.harvard.edu') %>%
            remove_constant() %>%
            rename(
                latitude=location_latitude, longitude=location_longitude,
                   duration = duration_in_seconds, intended_respondent=are_you,
                   party=pid, mayor=mayor_2021,
                   map_clicks = map_timing_click_count) %>%
            mutate(id = seq_len(n()),
                   date = as.Date(start_date),
                   map_time = map_timing_page_submit - map_timing_first_click,
                   neighborhood = str_split(neighborhood, ","),
                   .before = progress,
                   city='new-york') %>%
            mutate(party_combined = paste(dplyr::coalesce(#dem_strong, rep_strong,
                pid_lean, ""), party),
                party_combined = fct_collapse(
                    na_if(party_combined, " NA"),
                    dem_strong = "Strong Democrat",
                    dem_lean = c("Not very strong Democrat", "Closer to Democrat Independent",
                                 "Closer to Democrat Other party (please specify)", " Democrat"),
                    independent = c(" Independent", "Neither Independent",
                                    "Neither Other party (please specify)"),
                    rep_lean = c("Not very strong Republican", "Closer to Republican Independent",
                                 "Closer to Republican Other party (please specify)", " Republican"),
                    rep_strong = "Strong Republican") %>%
                    fct_relevel(c("dem_strong", "dem_lean", "independent",
                                  "rep_lean", "rep_strong")),
                vote_2021 = dplyr::coalesce(str_starts(vote_2021, "I am sure I voted"), F),
                #  president = case_when(
                #     president == "Donald Trump" ~ "trump",
                #    president == "Joe Biden" ~ "biden",
                #   president == "Somebody else" ~ "other",
                #  TRUE ~ NA_character_),
                gender = na_if(str_to_lower(gender), "other/prefer not to answer"),
                education = case_when(
                    str_detect(education, "Attended college") ~ "some_coll",
                    str_detect(education, "Attended high") ~ "some_hs",
                    str_detect(education, "Did not attend") ~ "no_hs",
                    str_detect(education, "2-year") ~ "grad_2yr",
                    str_detect(education, "4-year") ~ "grad_4yr",
                    str_detect(education, "post-graduate") ~ "postgrad",
                    str_detect(education, "Graduated from high") ~ "hs",
                    str_detect(education, "Other/Prefer") ~ NA_character_) %>%
                    fct_relevel(c("no_hs", "some_hs", "hs", "some_coll",
                                  "grad_2yr", "grad_4yr", "postgrad")),
                race = case_when(
                    hispanic == "Yes" ~ "hisp",
                    str_detect(race, "African") ~ "black",
                    str_detect(race, "Asian") ~ "aapi",
                    str_detect(race, "More than") ~ "multi",
                    str_detect(race, "Native") ~ "indig",
                    str_detect(race, "White") ~ "white",
                    TRUE ~ NA_character_),
                across(starts_with("trust_"), parse_number),
                housing_tenure = str_replace(housing_tenure,'Less than a year', '0'),
                housing_tenure = suppressWarnings(parse_number(housing_tenure)),
           #     neighborhood = map2(neighborhood, city, recover_fips),
                #   housing = housing == "Support",
                     .after="intended_respondent"
            ) %>%
            select(-ends_with("_date"), -ip_address, -finished, -recorded_date,
                   -response_id,  -starts_with("recipient_"),
                   -party, intended_respondent,
                   -pid_lean, -pid_4_text,
                   -children_home_do, -marital_status_7_text, -hispanic,
                   -starts_with("map_timing_"), -fl_11_do, -fl_22_do)

        write_rds(d, path, compress="xz")
        d
    } else if (distribution == "anonymous") {
        cli::cli_alert_info("Reading raw response file.")
        if (length(raw_path) > 1) {
            choices = str_glue("{basename(raw_path)} (Last modified {file.mtime(raw_path)})")
            choice = menu(choices, title="Which response file to use?")
            if (choice == 0) stop("Must select a file.")

            raw_path = raw_path[choice]
            cat("Using ", basename(raw_path), "\n")
        }
        dir.create(dirname(path), showWarnings = FALSE)

        raw_headings = read_csv(raw_path, n_max=1) %>%
            suppressMessages() %>%
            clean_names()
        raw = read_csv(raw_path, skip=3, col_names=colnames(raw_headings)) %>%
            suppressMessages()%>%
            select(-neighborhood)%>%
            bind_cols(read_csv(raw_path,  col_names=colnames(raw_headings))%>%
                          select(neighborhood)%>%
                          slice(4:n()))



        d = raw %>%
            filter(distribution_channel == distribution) %>%
            remove_constant() %>%
            rename(latitude=location_latitude, longitude=location_longitude,
                   duration = duration_in_seconds,
                   party=pid, mayor=mayor_2021,
                   map_clicks = map_timing_click_count) %>%
            mutate(id = seq_len(n()),
                   date = as.Date(start_date),
                   map_time = map_timing_page_submit - map_timing_first_click,
                   neighborhood = str_split(neighborhood, ","),
                   .before = progress,
                   city='new-york') %>%
            mutate(party_combined = paste(dplyr::coalesce(#dem_strong, rep_strong,
                                                   pid_lean, ""), party),
                   party_combined = fct_collapse(
                       na_if(party_combined, " NA"),
                       dem_strong = "Strong Democrat",
                       dem_lean = c("Not very strong Democrat", "Closer to Democrat Independent",
                                    "Closer to Democrat Other party (please specify)", " Democrat"),
                       independent = c(" Independent", "Neither Independent",
                                       "Neither Other party (please specify)"),
                       rep_lean = c("Not very strong Republican", "Closer to Republican Independent",
                                    "Closer to Republican Other party (please specify)", " Republican"),
                       rep_strong = "Strong Republican") %>%
                       fct_relevel(c("dem_strong", "dem_lean", "independent",
                                     "rep_lean", "rep_strong")),
                   vote_2021 = dplyr::coalesce(str_starts(vote_2021, "I am sure I voted"), F),
                 #  president = case_when(
                  #     president == "Donald Trump" ~ "trump",
                   #    president == "Joe Biden" ~ "biden",
                    #   president == "Somebody else" ~ "other",
                     #  TRUE ~ NA_character_),
                   gender = na_if(str_to_lower(gender), "other/prefer not to answer"),
                   education = case_when(
                       str_detect(education, "Attended college") ~ "some_coll",
                       str_detect(education, "Attended high") ~ "some_hs",
                       str_detect(education, "Did not attend") ~ "no_hs",
                       str_detect(education, "2-year") ~ "grad_2yr",
                       str_detect(education, "4-year") ~ "grad_4yr",
                       str_detect(education, "post-graduate") ~ "postgrad",
                       str_detect(education, "Graduated from high") ~ "hs",
                       str_detect(education, "Other/Prefer") ~ NA_character_) %>%
                       fct_relevel(c("no_hs", "some_hs", "hs", "some_coll",
                                     "grad_2yr", "grad_4yr", "postgrad")),
                   race = case_when(
                       hispanic == "Yes" ~ "hisp",
                       str_detect(race, "African") ~ "black",
                       str_detect(race, "Asian") ~ "aapi",
                       str_detect(race, "More than") ~ "multi",
                       str_detect(race, "Native") ~ "indig",
                       str_detect(race, "White") ~ "white",
                       TRUE ~ NA_character_),
                   across(starts_with("trust_"), parse_number),
                 housing_tenure = str_replace(housing_tenure,'Less than a year', '0'),
                 housing_tenure = suppressWarnings(parse_number(housing_tenure))#,
            #       neighborhood = map2(neighborhood, city, recover_fips),
                #   housing = housing == "Support",
              #     .after="intended_respondent"
                ) %>%
            select(-ends_with("_date"), -ip_address, -finished, -recorded_date,
                   -response_id,  -starts_with("recipient_"),
                    -party,
                  -pid_lean, -pid_4_text,
                   -children_home_do, -marital_status_7_text, -hispanic,
                   -starts_with("map_timing_"), -fl_11_do, -fl_22_do)

        write_rds(d, path, compress="xz")
        d
    }
}


resp_d = bind_rows(
    meta = get_clean_responses_nycc(path = "data/nycc_meta_clean_responses.rds",
                                    raw_path = "data-raw/responses_nycc_fb_2023-02-21.csv",
                                    distribution = "anonymous")%>%
        mutate(id = paste0('meta_',id)),
    email = get_clean_responses_nycc(path = "data/nycc_vf_clean_responses.rds",
                                     raw_path = "data-raw/responses_nycc_email_2023-02-21.csv",
                                     distribution = "email")%>%
        mutate(id = paste0('email_',id)),
    .id="survey"
) %>%
    mutate(why_map=NA)


# make block data for NYC
if (!file.exists(path <- here("data/nycc_block_data.rds"))) {
    block_d = get_block_data(cities="new-york") %>%
        filter(str_sub(fips, 1, 5) %in% c("36061", "36047", "36081", "36085", "36005"))
    xw = PL94171::pl_crosswalk("NY")

    # join XW and aggregate each type of variable differently, then combine
    block_d_20 = block_d %>%
        left_join(xw, by=c("fips"="GEOID"), multiple="all") %>%
        filter(int_land > 0)
    block_d_20_chr = block_d_20 %>%
        select(city, GEOID_to, type, rr_id, int_land) %>%
        fgroup_by(GEOID_to) %>%
        fmode(w=int_land, keep.w=FALSE) %>%
        rename(fips=GEOID_to)
    block_d_20_mn = block_d_20 %>%
        mutate(x = st_coordinates(centroid)[, 1],
               y = st_coordinates(centroid)[, 2]) %>%
        select(GEOID_to, pct_dem:pct_homeown, x:y, dist_school:dist_church, int_land) %>%
        fgroup_by(GEOID_to) %>%
        fmean(w=int_land, keep.w=FALSE) %>%
        mutate(centroid = st_as_sf(select(., x, y), coords=c("x", "y"))$geometry) %>%
        select(-x, -y, -GEOID_to)
    block_d_20_agg = block_d_20 %>%
        select(GEOID_to, pop:pop_hisp, registrants:republicans, area, int_land) %>%
        fgroup_by(GEOID_to) %>%
        fsum(w=int_land, keep.w=FALSE) %>%
        select(-GEOID_to)
    block_d_20 = bind_cols(block_d_20_chr, block_d_20_mn, block_d_20_agg) %>%
        select(names(block_d))
    write_rds(block_d_20, path, compress="xz")
} else {
    block_d_20 = read_rds(path)
}

# make adj graph for NYC
adj_gr = if (!file.exists(path <- here("data/nycc_graph.rds"))) {
    adj_gr = list(`new-york`=list(graph=NULL, blocks=NULL))
    d_blocks = tigris::blocks("NY", county=c("New York", "Bronx", "Kings", "Queens", "Richmond"), year=2020)
    d_blocks = d_blocks %>%
        semi_join(block_d_20, by=c(GEOID20="fips"))
    adj_gr$`new-york`$graph = geomander::adjacency(d_blocks)
    adj_gr$`new-york`$blocks = d_blocks$GEOID20
    write_rds(adj_gr, path, compress="xz")
    adj_gr
} else {
    read_rds(path)
}

# align block data and adj graph
block_d = map_dfr(adj_gr, function(x) tibble(fips=x$blocks), .id="city") %>%
    left_join(block_d_20, by=c("city", "fips")) %>%
    mutate(centroid = st_set_crs(centroid, 4326L))
rm(block_d_20)

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
               is_school = dplyr::coalesce(type == "school", FALSE),
               is_church = dplyr::coalesce(type == "church", FALSE),
               is_park = dplyr::coalesce(type == "park", FALSE),
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
               income_est  = case_when( income == "Less than $25,000" ~ 20e3,
                                        income == "$25,000 – 49,999" ~ 37.5e3,
                                        income == "$50,000 – 74,999" ~ 62.5e3,
                                        income == "$75,000 – 119,999" ~ 97.5e3,
                                        income == "$120,000  or More" ~ 160e3),
               income_dist = abs(log(income_est) - log(med_inc)) /
                   mean(log(med_inc), na.rm=T),
               educ_grp = case_when(education %in% c("no_hs", "some_hs",
                                                     "hs", "some_coll") ~ "no_coll",
                                    education %in% c("grad_2yr", "grad_4yr", "postgrad") ~ "coll"),
               pct_educ = if_else(educ_grp == "coll", pct_college, 1 - pct_college),
               pct_own = if_else(homeowner == "Homeowner", pct_homeown, 1 - pct_homeown),
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
               area, any_of("ring"), dist, vote_2021) %>%
        group_by(id) %>%
        mutate(same_bg = block_group == block_group[1],
               same_tract = tract == tract[1],
               same_rr = rr_id == rr_id[1])
}

