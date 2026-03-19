library(janitor)

#' Clean Qualtrics response data and save to an RDS file
#'
#' This function performs the following cleaning steps, then saves the data to
#' an RDS file:
#' 1. Convert all column names to `snake_case`
#' 1. Rename several variables to be more concise/informative
#' 1. Calculate `map_time` from start and end timestamps
#' 1. Parse `neighborhood` column into list of blocks and re-prepend state FIPS codes
#' 1. Drop personally identifying variables, auxiliary variables, and most
#' free-response variables
#' 1. Recode categorical variables with more concise labels
#'
#' @param path the RDS to save/cache
#' @param raw_path the path to the Qualtrics CSV export
#'
#' @return A data frame containing the cleaned data
get_clean_responses = function(path = here("data/clean_responses.rds"),
                               raw_path = Sys.glob(here("data-raw/responses*.csv"))) {
    if (file.exists(path)) {
        read_rds(path)
    } else {
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
        raw = read_csv(raw_path, skip=7, col_names=colnames(raw_headings)) %>%
            suppressMessages()

        d = raw %>%
            remove_constant() %>%
            filter(distribution_channel == "email") %>%
            rename(latitude=location_latitude, longitude=location_longitude,
                   duration = duration_in_seconds, intended_respondent=are_you,
                   party=pid, president=president_intent, why_map=open,
                   map_clicks = map_timing_click_count, city=city_group) %>%
            mutate(id = seq_len(n()),
                   date = as.Date(start_date),
                   map_time = map_timing_page_submit - map_timing_first_click,
                   neighborhood = str_split(neighborhood, ","),
                   .before = progress) %>%
            mutate(party_combined = paste(coalesce(dem_strong, rep_strong,
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
                   vote_2020 = coalesce(str_starts(vote_2020, "I am sure I voted"), F),
                   president = case_when(
                       president == "Donald Trump" ~ "trump",
                       president == "Joe Biden" ~ "biden",
                       president == "Somebody else" ~ "other",
                       TRUE ~ NA_character_),
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
                   housing_tenure = suppressWarnings(parse_number(housing_tenure)),
                   housing_tenure = if_else(housing_tenure >= 1900,
                                            2021 - housing_tenure, housing_tenure),
                   housing_tenure = if_else(housing_tenure >= 100, NA_real_, housing_tenure),
                   neighborhood = map2(neighborhood, city, recover_fips),
                   housing = housing == "Support",
                   .after="intended_respondent") %>%
            select(-ends_with("_date"), -ip_address, -finished, -recorded_date,
                   -response_id, -distribution_channel, -starts_with("recipient_"),
                   -external_reference, -your_name, -relation, -party,
                   -dem_strong, -rep_strong, -pid_lean, -pid_4_text,
                   -children_home_do, -marital_status_7_text, -hispanic,
                   -starts_with("map_timing_"), -fl_11_do, -fl_22_do)

        write_rds(d, path, compress="xz")
        d
    }
}
