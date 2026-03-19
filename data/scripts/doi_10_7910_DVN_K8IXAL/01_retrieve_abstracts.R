#' ---
#' title: "Catalysts for progress? Mapping policy insights from energy research (ERSS)"
#' subtitle: "01_retrieve_abstracts.R"
#' author: "Authors: Brian Boyle, Yen-Chieh Liao, Sarah King, Robin Rauner, and Stefan Müller"
#' ---

# load required R packages
library(tidyverse) # CRAN v2.0.0
library(here) # CRAN v1.0.1
library(rscopus) # CRAN v0.6.6
library(data.table) # CRAN v1.16.0


# If the code does not run, one or more packages may have been
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2025-01-01")
# Instead of adjusting the library() function for each package,
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2025-01-01")
# More details are available at: https://groundhogr.com/using/


# Clean raw file containing list of energy journals
# Prepare data for use in Scopus API calls
# Contains journal name, issn, and data availability information
# Can use this information to create API requests
# Data taken from: https://www.scimagojr.com/journalrank.php?area=2100&wos=true

energy_journals <- read.csv("energy_journals_2023_scimago.csv")

# Manually update any issns with incorrect codes
energy_journals$Issn[energy_journals$Issn == "13640321, 18790690"] <- "13640321"

# If multiple issns are included, use first
# If issn is < 8 characters long, pad the start with zeros until correct length
journal_master <- energy_journals |>
  mutate(
    issn_1 = str_extract(Issn, "^[^,]*"),
    issn_2 = str_extract(Issn, "(?<=, ).*"),
    journal_title_lower = str_replace_all(tolower(Title), "[^\\w\\s]", " ") |>
      str_replace_all("  ", " ") |>
      str_replace_all(" ", "_"),
    issn_1 = case_when(
      nchar(issn_1) == 8 ~ issn_1,
      nchar(issn_1) == 7 ~ paste0("0", issn_1),
      nchar(issn_1) == 6 ~ paste0("00", issn_1)
    ),
    issn_2 = case_when(
      nchar(issn_2) == 8 ~ issn_2,
      nchar(issn_2) == 7 ~ paste0("0", issn_2),
      nchar(issn_2) == 6 ~ paste0("00", issn_2)
    ),
    issn_multi = case_when(
      is.na(issn_2) ~ issn_1,
      !is.na(issn_2) ~ paste(issn_1, "OR", issn_2)
    ),
    journal_title_50 = substr(journal_title_lower, 1, 50)
  )

# Manually change issn's for journals with incorrect info
journal_master$issn_1[journal_master$issn_1 == "22106715"] <- "22106707"
journal_master$issn_1[journal_master$issn_1 == "20954719"] <- "20523289"


# Collect journals

# Load custom functions for scopus data collection
source("scopus_journal_functions_cursor.r")

# Setup API key

# Manually add your Scopus API key to this object
key <- read.csv("", header = FALSE)[1, 1]

# The following function can be used to assign your API key
rscopus::set_api_key(key)

# Check how many articles are returned from each search request
# Can manually input journal details, or use the journal master list

# # Example using the data from the 'master list'
# journal_search_results <-
# journal_date_results(issn = journal_master$issn_multi[c(1:100)],
#                      date = '2010-2023')
# journal_search_results
#
#
# # Export number of returned results
# write.csv(journal_search_results,
# here('data', 'article_freq_energy_journals_2010-2023.csv'), row.names = F)


# Example using the issn for 'Political Analysis'
# journal_search_results <-
# journal_date_results(issn = '09596526 OR 18791786',
#                      date = '2023')
# journal_search_results

# Check which journals have already been collected

# Import list of journals and number of articles
journal_results <- read.csv("article_freq_energy_journals_2010-2023.csv")

# Convert journal name into same format as the folder names
journal_results$journal_name_lower <-
  str_replace_all(unique(
    tolower(journal_results$journal_name)
  ), "[^\\w\\s]", " ") |>
  str_replace_all("  ", " ") |>
  str_replace_all(" ", "_")

# Check which journals have already been collected
collected_df <- data.frame(
  journal_rank = journal_master$Rank[1:100],
  journal_name = journal_master$Title[1:100],
  journal_name_lower = journal_results$journal_name_lower[1:100],
  collected = as.numeric(
    journal_results$journal_name_lower[1:100] %in% dir("./journal_year")
  )
)

# Collect all articles from a specific journal over a set time period
# Can manually input journal details, or use the journal master list

# Looping through years rather than supplying date ranges means that the
# output will be exported after each year, saving as we go.
dates <- as.character(2010:2023)

# Outer loop - journals
for (j in c(90:100)) {
  # Inner loop - years
  # for(i in seq_along(dates)){
  for (i in seq_along(dates)) {
    date_range <- dates[i]

    # API call
    api_output <-
      journal_date_search(
        issn = journal_master$issn_multi[j], date = date_range,
        api_key = rscopus::get_api_key()
      )

    # If there are any results for that journal-year query, then process them
    if (any(is.na(api_output) == FALSE)) {
      # Parse XML into dataframe
      parsed_output <-
        api_output |>
        parse_scopus_output()

      # Clean dataset and save to file
      parsed_output |>
        tidy_output() |>
        save_entries(
          path = "./journal_year",
          abbrev = journal_master$journal_title_50[j],
          overwrite = T
        )
    } # else statement
  }
}

# To do - tidy_output() systematically handle empty columns

# Notes
# journal 13 - issn search returns multiple journals
# journal 40 and 48 partially collected, reached max quota limit

# Check remaining quota left on API key
check_limits <- rscopus::generic_elsevier_api(
  query = "issn(12345678)",
  type = "search",
  search_type = "scopus",
  date = 2010,
  max_count = Inf,
  view = "COMPLETE",
  api_key = rscopus::get_api_key(),
  verbose = F
)

reset_date <- as.POSIXct(
  as.numeric(check_limits$get_statement$headers$`x-ratelimit-reset`),
  origin = "1970-01-01", tz = "UCT"
)

cat(c(
  "Limit:",
  check_limits$get_statement$headers$`x-ratelimit-limit`,
  "\nLimit Remaining:",
  check_limits$get_statement$headers$`x-ratelimit-remaining`,
  "\nRestart time:", as.character(reset_date)
))

# Check which journals have already been scraped
journal_master$journal_title_lower %in% dir("./journal_year")

# Combine articles into single dataset (top 50 journals)


# Select which journals we want to select and combine the results for
# (e.g. top 50 journals)
target_range <- 1:50

# Create a string with all the required journal scopus_ids
# joined by OR operators
combine_ids <- paste0("s", journal_master$Sourceid)
combine_ids <- combine_ids[target_range] |> paste(collapse = "|")

# Get filepaths for all journal-year files
file_paths <- list.files(list.files(
  "./journal_year",
  full.names = T
), full.names = T)

# Only keep files that include any of our selected scopus_ids
selected_files <- file_paths[grep(combine_ids, file_paths)]

# Import and merge all selected files
scopus_df <- map_dfr(selected_files, readRDS, .progress = T)

# We can also merge in the journal rankings
# based on our initial journal information

# Select variables for merging
journal_rank_merge <- energy_journals |>
  select(source_id = Sourceid, rank = Rank) |>
  mutate_all(as.character)

# Merge into scopus dataset
scopus_df_001_050 <- scopus_df |>
  left_join(journal_rank_merge, by = "source_id") |>
  mutate(rank = as.numeric(rank)) |>
  mutate(journal_name = stringr::str_to_title(journal_name))

#  Export dataset

saveRDS(
  scopus_df_001_050,
  "data_dontshare/scopus_energy_journals_1-50_2010-2023_combined_dontshare.rds"
)

# Combine articles into single dataset (top 51-100 journals)

# Select which journals we want to select and combine the results for
# (e.g. ranked 51-100 journals)
target_range <- 51:100

# Create a string with all the required journal scopus_ids
# joined by OR operators
combine_ids <- paste0("s", journal_master$Sourceid)
combine_ids <- combine_ids[target_range] |> paste(collapse = "|")

# Get filepaths for all journal-year files held within the data directory
file_paths <- list.files(list.files(
  "./journal_year",
  full.names = T
), full.names = T)

# Only keep files that include any of our selected scopus_ids
selected_files <- file_paths[grep(combine_ids, file_paths)]

# Import and merge all selected files
scopus_df <- map_dfr(selected_files, readRDS, .progress = T)

# We can also merge in the journal rankings
# based on our initial journal information

# Select variables for merging
journal_rank_merge <- energy_journals |>
  select(source_id = Sourceid, rank = Rank) |>
  mutate_all(as.character)

# Merge into scopus dataset
scopus_df_051_100 <- scopus_df |>
  left_join(journal_rank_merge, by = "source_id") |>
  mutate(rank = as.numeric(rank))

# Manual cleaning

# Change journal names to title case for consistency
# Remove one journal returned in error (assume scopus indexing error)
scopus_df_051_100 <- scopus_df_051_100 |>
  mutate(journal_name = stringr::str_to_title(journal_name)) |>
  mutate(journal_name = stringr::str_replace_all(
    journal_name,
    "Aapg Bulletin",
    "American Association Of Petroleum Geologists Bulletin"
  )) |>
  filter(journal_name != "The Leading Edge")

#  Export dataset
saveRDS(
  scopus_df_051_100,
  "scopus_energy_journals_51-100_2010-2023_combined_dontshare.rds"
)

df1 <- readRDS("scopus_energy_journals_1-50_2010-2023_combined_dontshare.rds")
df2 <- readRDS("scopus_energy_journals_51-100_2010-2023_combined_dontshare.rds")

# Data inspection

# Can check how many articles meet the different filtering criteria we will use
# scopus_df |> count(publication_type_sub)
# scopus_df$article_doi         |> is.na() |> sum()
# scopus_df$article_title       |> is.na() |> sum()
# scopus_df$article_abstract    |> is.na() |> sum()
# sum(nchar(scopus_df$affil_country) == 0)
# sum(nchar(scopus_df$author_name) == 0)

energy_articles <- scopus_df_051_100 |>
  filter(
    publication_type_agg %in% c("Journal"),
    publication_type_sub %in% c("Article"),
    !is.na(article_doi),
    !is.na(article_title),
    !is.na(article_abstract),
    affil_country != "",
    author_name != ""
  )

energy_articles |>
  count(rank, journal_name) |>
  View()

# Check that the number of collected articles matches the number expected
# from the initial search query.
scopus_df_001_050 |>
  count(rank, journal_name) |>
  left_join(
    journal_results |>
      mutate(rank = 1:100) |>
      select(rank, results_total, journal_name_lower),
    by = "rank"
  ) |>
  View()

scopus_df_051_100 |>
  count(rank, journal_name) |>
  left_join(
    journal_results |>
      mutate(rank = 1:100) |>
      select(rank, results_total, journal_name_lower),
    by = "rank"
  ) |>
  View()

# Combine journals
# load full dataset
dat_raw_1_50 <- readRDS(
  "data_dontshare/scopus_energy_journals_1-50_2010-2023_combined_dontshare.rds"
)
dat_raw_51_100 <- readRDS(
  "data_dontshare/scopus_energy_journals_51-100_2010-2023_combined_dontshare.rds"
)

# bind data
dat_raw <- bind_rows(dat_raw_1_50, dat_raw_51_100)

# recode some journals
dat_raw <- dat_raw |>
  mutate(journal_name = dplyr::recode(journal_name,
    "Annual review of chemical and biomolecular engineering" = "Annual Review of Chemical and Biomolecular Engineering",
    "Bioresource technology" = "Bioresource Technology"
  ))

table(dat_raw$rank)

length(unique(dat_raw$journal_name))

# only keep journal articles with DOIs, titles, abstracts, affiliations, and authors
dat_relevant <- dat_raw |>
  filter(
    publication_type_agg %in% c("Journal"),
    publication_type_sub %in% c("Article"),
    !is.na(article_doi),
    !is.na(article_title),
    !is.na(article_abstract),
    affil_country != "",
    author_name != ""
  )

# 02. Merge in subject area codes

# The scoupus API output does not include subject area tags for each publication.
# They do have a list of journals and associated subject area codes, that can
# be merged with the publication output: 'All Science Journal Classification' codes.

# List of all codes:
# https://service.elsevier.com/app/answers/detail/a_id/15181/supporthub/scopus/

# Scopus source list (instructions to get source data with ASJC codes):
# https://service.elsevier.com/app/answers/detail/a_id/14195/supporthub/scopus/related/1/

# Import merged dataset of journal IDs, and associated subject area codes
# (For journals with multiple codes attached, the first code is used)
asjc_journal_codes <- readRDS("asjc_journal_codes.rds")

# Merge in subject area information
# Create dummy variable indicating if subject code present
dat_relevant <- dat_relevant |>
  left_join(asjc_journal_codes, by = "source_id") |>
  mutate(asjc_dum = as.numeric(!is.na(asjc_code_first)))

# Check if any articles/journals do not have a matching ASJC code
count(dat_relevant, asjc_dum)

# Remove dummy variable used for checking missing cases
dat_relevant <- select(dat_relevant, -asjc_dum)

# 03. Aggregate ASJC subject area codes

# Information on ASJC codes:
#  https://service.elsevier.com/app/answers/detail/a_id/12007/supporthub/scopus/

# There are 4 digit ASJC codes for each specific topic
# These are grouped by 100's into 27 general subject areas
# And these are then also grouped into 5 major areas

# Create key with the 27 aggregated ASJC subject areas
asjc_areas_agg <- asjc_journal_codes |>
  select(asjc_code_first, asjc_cat_first) |>
  distinct() |>
  filter(str_detect(asjc_code_first, "00$")) |>
  mutate(
    asjc_area_code = as.numeric(asjc_code_first),
    asjc_area = str_replace(asjc_cat_first, "General ", "")
  ) |>
  select(-asjc_code_first, -asjc_cat_first) |>
  arrange(asjc_area_code)

# Round the ASJC code to the nearest hundred
# and merge in the subject area label
# Rename the broader research field label

dat_relevant <- dat_relevant |>
  mutate(asjc_area_code = round(as.numeric(asjc_code_first), -2)) |>
  left_join(asjc_areas_agg, by = "asjc_area_code") |>
  mutate(asjc_field = asjc_area_first) |>
  select(-asjc_area_first)

# We now have information on:
# asjc_field = broad subject field (5 options)
# asjc_area  = narrower subject area within field (27 options)
# ajcs_cat_first = specific subject category (100+ options)

# For journals classed under multiple asjc codes, the first code is used
# asjc_code_all contains the full list of codes for each journal if needed

nrow(dat_raw)
# > nrow(dat_raw)
# [1] 512096

nrow(dat_relevant)
# > nrow(dat_relevant)
# [1] 461894

saveRDS(dat_relevant, "data_dontshare/data_scopus_1_100_2010_2023_dontshare.rds")
