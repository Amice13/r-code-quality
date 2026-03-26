##########################################
# Data Preparation
##########################################
library(tidyverse)
library(googlesheets4)
library(sf)
library(geosphere)

ss_id ='https://docs.google.com/spreadsheets/d/1Y1xGVuFqMzaKnbQAQFgaIdgVJ1Ciik3UV5ougUzxXXE/edit?gid=1701903396#gid=1701903396'
# outlets_with_distances <- readRDS('data/features/outlets_with_distances.rds')

# Load datasets
analysis_data <- readRDS('replication_data_geo_newsmapper/analysis_data.rds')
# analysis_data |> head(50) |> write_sheet(ss_id, sheet = 'location_level_sample')

pinf <- read_sheet(ss_id, sheet = 'Domains_PINF_cluster')
districts <- read_sheet(ss_id, sheet = 'Districts')
publishers <- read_sheet(ss_id, sheet = 'Publisher')
pop <- read_sheet(ss_id, sheet = 'Pop')
dispersion_stats <- readRDS('data/features/dispersion_stats.rds')
rural <- read_sheet(ss_id, 'RURAL_URBAN_clean')


dispersion_stats <- readRDS('data/features/dispersion_stats.rds')
mean(dispersion_stats$median_distance_km)
median(dispersion_stats$median_distance_km)
sd(dispersion_stats$median_distance_km)
max(dispersion_stats$median_distance_km)
min(dispersion_stats$median_distance_km)

# Join datasets to create outlet-level dataset
outlets <- analysis_data %>%
  # Get basic domain data
  group_by(domain) %>%
  mutate(
    total_articles = n_distinct(unique_article_id)
  ) %>%
  ungroup() %>% 
  # Join with PINF data for outlet info
  left_join(pinf %>% select(TITLE, domain, INDEPENDENT, OFFICE_ADDRESS, REGISTERED_ADDRESS, OWNER_PINF), by = "domain") %>%
  # Add title and ownership information
  mutate(
    TITLE = TITLE,
    ownership_type = if_else(INDEPENDENT == "Yes", "Independent", "Consolidated"),
    newsroom_address = if_else(!is.na(OFFICE_ADDRESS), OFFICE_ADDRESS, REGISTERED_ADDRESS),
    newsroom_address = if_else(is.na(newsroom_address), primary_location, newsroom_address)
  ) %>%
  # Add LAD centroid coordinates
  left_join(districts %>% select(LAD, Latitude, Longitude) %>%rename(
    coverage_lad_centroid_lat = Latitude,
    coverage_lad_centroid_lon = Longitude
  ), by = c("main_LAD" = "LAD")) %>%
  # Add publisher group size
  left_join(publishers %>% select(owner, n_domains), by = "owner") %>%
  rename(outlets_in_group = n_domains) %>%
  left_join(rural |> select(LAD, `ruc-cluster-label`), by = c("main_LAD" = "LAD"))
  
# Function to get geocode from Mapbox API
get_geocode <- function(address) {
  if (any(!is.na(address))) {  # Check if any address is not NA
    mapbox_access_token <- "pk.eyJ1Ijoic2ltb25hYmlzaWFuaSIsImEiOiJjbHRvaHZyZXowMTRmMmtycDlmZHB4N2h2In0.EdVXs5b__rvnSkgbByNEgg"
    base_url <- "https://api.mapbox.com/geocoding/v5/mapbox.places/"
    query <- list(
      bbox = "-8,49,2,61",
      access_token = mapbox_access_token
    )
    url <- paste0(base_url, URLencode(address), ".json")
    response <- httr::GET(url, query = query)
    if (httr::status_code(response) == 200) {
      result <- httr::content(response, as = "text", encoding = "UTF-8")
      json_data <- jsonlite::fromJSON(result)
      if (length(json_data$features) > 0) {
        return(json_data$features$geometry$coordinates[[1]])
      }
    }
  }
  return(c(NA, NA))
}

# Geocode data
library(purrr)
library(stringr)
library(tidyr)
geocoded_data <- outlets |> 
  select(domain, OWNER_PINF, newsroom_address) |> 
  distinct() |> 
  rowwise() |>
  mutate(
    LOCATION = list(get_geocode(newsroom_address))) |> 
  mutate(
    newsroom_lon = LOCATION[[1]],
    newsroom_lat = LOCATION[[2]]
  ) |>
  select(-LOCATION) |> 
  ungroup()

outlets <- outlets |>
  left_join(geocoded_data, by = c('domain', 'OWNER_PINF', 'newsroom_address')) |> 
  # Calculate newsroom distance
  rowwise() %>%
  mutate(
    newsroom_distance_km = if_else(
      !is.na(newsroom_lat) & !is.na(outlet_lat),
      distHaversine(c(newsroom_lon, newsroom_lat), 
                    c(outlet_lon, outlet_lat)) / 1000,
      NA_real_
    )
  ) %>%
  ungroup() |> 
  rename(lad_urban_rural = `ruc-cluster-label`)

# Calculate geographic fidelity
geographic_fidelity <- analysis_data %>%
  select(unique_article_id, domain, chosen_LAD24NM, main_LAD) |> 
  distinct() |> 
  group_by(domain) %>%
  summarize(
    total_articles = n_distinct(unique_article_id),
    articles_in_main_lad = sum(chosen_LAD24NM == main_LAD, na.rm = TRUE),
    geographic_fidelity = articles_in_main_lad / total_articles
  ) |> select(-total_articles, - articles_in_main_lad)

  gf_stats <- geographic_fidelity$geographic_fidelity
  cat("N:", length(gf_stats), "\n",
      "Mean:", mean(gf_stats), "\n", 
      "Median:", median(gf_stats), "\n",
      "SD:", sd(gf_stats), "\n",
      "Min:", min(gf_stats), "\n",
      "Max:", max(gf_stats))

# Calculate syndication rate
syndication_rate <- analysis_data %>%
  select(unique_article_id, duplicate_group, domain) |> 
  distinct() |> 
  group_by(domain) %>%
  summarize(
    total_articles = n_distinct(unique_article_id),
    duplicate_groups = n_distinct(duplicate_group[!is.na(duplicate_group)]),
    syndication_rate = duplicate_groups / total_articles
  ) |> select(-total_articles, -duplicate_groups)

# Calculate yearly article counts
yearly_counts <- analysis_data %>%
  select(domain, unique_article_id, year) |> 
  distinct() |> 
  group_by(domain, year) %>%
  summarize(article_count = n_distinct(unique_article_id)) %>%
  pivot_wider(names_from = year, values_from = article_count, names_prefix = "articles_", values_fill = 0
)

# Combine all metrics
final_dataset <- outlets %>%
  select(domain, 
    TITLE, 
    OWNER_PINF,
    main_LAD,
    coverage_lad_centroid_lat,
    coverage_lad_centroid_lon,
    outlet_lat,
    outlet_lon,
    newsroom_address,
    newsroom_lat,
    newsroom_lon,
    newsroom_distance_km,
    ownership_type,
    outlets_in_group,
    total_articles,
    # articles_with_locations,
    # geocoding_success_rate,
    lad_urban_rural) |> 
  distinct() |>
  left_join(geographic_fidelity, by = "domain") %>%
  left_join(syndication_rate, by = "domain") %>%
  left_join(yearly_counts, by = "domain") %>%
  left_join(dispersion_stats %>% select(domain, median_distance_km), by = "domain") %>%
  # Add population density
  left_join(pop %>% select(LAD, area_sq_km,	pop_mid2022, pop_per_sq_km), by = c("main_LAD" = "LAD")) 

##################################################
# For article-level enhancements
##################################################
article_data <- analysis_data %>%
  select(domain, article_text, resolved_url, tweet_date, year, quarter, day_of_week,
         constructed_week, duplicate_group, unique_article_id, chosen_LAD24NM, main_LAD, like_count, retweet_count, reply_count, quote_count, impression_count) %>%
  group_by(unique_article_id) %>%
  mutate(
    num_locations = n(),  # number of locations per article
    has_local_location = any(chosen_LAD24NM == main_LAD, na.rm = TRUE),  # TRUE if any match
    engagement = as.numeric(str_remove(like_count, ';.*')) + as.numeric(str_remove(retweet_count, ';.*')) + as.numeric(str_remove(reply_count, ';.*')) + as.numeric(str_remove(quote_count, ';.*')),
    impressions = as.numeric(str_remove(impression_count, ';.*'))
  ) %>%
  ungroup() |> 
  select(-c(chosen_LAD24NM, like_count, retweet_count, reply_count, quote_count, impression_count)) |> 
  distinct() |> 
  mutate(
        # 1. Word count from article_text
        word_count = str_count(article_text, "\\S+"),      
        # 2. is_duplicate: TRUE if duplicate_group is not empty/NA
        is_duplicate = !is.na(duplicate_group) & duplicate_group != ""
      ) |> 
  select(-article_text, -main_LAD, -duplicate_group)

# saveRDS(article_data, "data/article_level_analysis_data.rds")
##################################################
# Geocoding success rate stats
##################################################
articles <- read_csv('replication_data_geo_newsmapper/articles_sample.csv.zip')

# Step 2: total articles per domain
total_articles <- articles %>%
  group_by(domain) %>%
  summarise(all_articles = n_distinct(unique_article_id), .groups = "drop")

nrow(total_articles)

# Step 3: join and compute success rate
final_dataset <- final_dataset %>%
  left_join(total_articles, by = "domain") %>%
  mutate(
    geocoding_success_rate = total_articles / all_articles
  )

# write_sheet(final_dataset, ss_id, 'beyond_the_masthead_outlets')
# please do not override this as I have made some manual changes to the column OWNER_PINF, adjusting inconsistencies in spelling and adding a new var called main_LAD recoded to adjust for boundary (Cumberland and Westmorland and Furness) changes. Essentially recoded these columns to attribute original main LAD. The rural urban column can be inherited, as it's Rural for both (and presumably the subunits). [I have though made a copy called _recoded - see below]
###############################################################################
# Covid
###############################################################################
outlets <- read_sheet(ss_id, sheet = 'beyond_the_masthead_outlets_recoded')
covid_data <- read_sheet(ss_id, sheet = 'Covid')

# Population data
hackney_pop <- 261491
city_pop <- 10847
cornwall_pop <- 575413
scilly_pop <- 2281

# Calculate population proportions (fix the parentheses!)
hackney_prop <- hackney_pop / (hackney_pop + city_pop)
city_prop <- city_pop / (hackney_pop + city_pop)
cornwall_prop <- cornwall_pop / (cornwall_pop + scilly_pop)
scilly_prop <- scilly_pop / (cornwall_pop + scilly_pop)

pop <- read_sheet(ss_id, sheet = 'Pop')

# Your existing pivot and interpolation code
covid_data <- covid_data %>%
  pivot_longer(cols = -LAD, names_to = 'quarter', values_to = 'covid_cases') |> 
  arrange(LAD, quarter) |> 
  group_by(LAD) %>%
  mutate(
    covid_cases_imputed = zoo::na.approx(covid_cases, na.rm = FALSE, rule = 2)) %>%
  ungroup() |> 
  pivot_wider(id_cols = LAD, names_from = 'quarter', values_from = 'covid_cases_imputed', names_prefix = 'Covid_')

# Now allocate the duplicated rows proportionally
covid_data <- covid_data %>%
  mutate(
    across(starts_with("Covid_"), ~ case_when(
      LAD == "Hackney" ~ .x * hackney_prop,
      LAD == "City of London" ~ .x * city_prop,
      LAD == 'Cornwall' ~.x * cornwall_prop,
      LAD == 'Isles of Scilly' ~.x * scilly_prop,
      TRUE ~ .x  # keep other areas unchanged
    ))
  )

# sum into Buckinghamshire if LAD is Aylesbury Vale, Chiltern, South Bucks or Wycombe, same for Northamptonshire uniary authorities
covid_data <- covid_data %>%
  # Create consolidated LAD names
  mutate(
    LAD_consolidated = case_when(
      # Buckinghamshire
      LAD %in% c('Aylesbury Vale', 'Chiltern', 'South Bucks', 'Wycombe') ~ "Buckinghamshire",
      # North Northamptonshire  
      LAD %in% c('Corby', 'East Northamptonshire', 'Kettering', 'Wellingborough') ~ "North Northamptonshire",
      # North Yorkshire  
      LAD %in% c("Craven", "Hambleton", "Harrogate", "Richmondshire", "Ryedale", "Scarborough", "Selby") ~ "North Yorkshire",
      # West Northamptonshire
      LAD %in% c('Daventry', 'Northampton', 'South Northamptonshire') ~ "West Northamptonshire",
      TRUE ~ LAD
    )
  ) %>%
  # Group and sum
  group_by(LAD_consolidated) %>%
  summarise(across(starts_with("Covid_"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(LAD = LAD_consolidated)

covid_data <- covid_data |> 
  left_join(pop, by = "LAD")%>%
  # Convert case counts to rates per 100k
  mutate(
    across(starts_with("Covid_"), ~ (.x / pop_mid2022) * 100000, .names = "{.col}_rate")
  )

# write_sheet(covid_data, ss_id, 'covid_w_pop')
# covid_data <- read_sheet(ss_id, sheet = 'covid_w_pop')

# I need to keep everything from final_dataset, but a few vars from outlets: 
outlets <- read_sheet(ss_id, sheet = 'beyond_the_masthead_outlets_recoded') |> 
  mutate(LAD = if_else(!is.na(main_LAD_recoded), main_LAD_recoded, main_LAD)) 

final_dataset_analysis <- final_dataset |> 
  left_join(outlets |> select(domain, LAD), by = c('domain')) |> 
  select(-c(area_sq_km, pop_mid2022, pop_per_sq_km, main_LAD)) |> 
  left_join(covid_data |> select(LAD, area_sq_km, pop_mid2022, pop_per_sq_km, ends_with('rate')), by = "LAD")

write_sheet(final_dataset_analysis, ss_id, 'beyond_the_masthead_final')
# geographic fidelity is based on the 2023 lad borders as this is what was implemented during disambiguation. there are 6 districts affected, where fidelity is 'more generous' than it would be otherwise (because the new borders are wider)

#######################################################
# Fix ownership
#######################################################
outlets <- read_sheet(ss_id, sheet = 'beyond_the_masthead_final')
pinf <- read_sheet(ss_id, sheet = 'Domains_PINF_cluster') 

# THERE ARE TWO TASKS TO BE ADDRESSED:
# recode HIGHLAND NEWS AND MEDIA LIMITED to ILIFFE MEDIA LIMITED, including owner, ownership_status, and registered_address
# do the same with NEW MILTON NEWS AND MEDIA LTD
outlets_recoded <- outlets |> 
  mutate(
    OWNER_PINF = case_when(
      OWNER_PINF == 'HIGHLAND NEWS AND MEDIA LIMITED' ~ 'ILIFFE MEDIA LIMITED',
      OWNER_PINF == 'NEW MILTON NEWS AND MEDIA LTD' ~ 'ILIFFE MEDIA LIMITED',
      TRUE ~ OWNER_PINF
    )) |> 
  left_join(pinf |> select(domain, REGISTERED_ADDRESS), by = 'domain') |>
  mutate(
    REGISTERED_ADDRESS = case_when(
      OWNER_PINF == 'ILIFFE MEDIA LIMITED' ~ 'Winship Road, Milton, Cambridge, CB24 6PP, Cambridgeshire',
      TRUE ~ REGISTERED_ADDRESS
    )
  ) |> 
mutate(outlets_in_group = if_else(OWNER_PINF == 'ILIFFE MEDIA LIMITED', 25L, outlets_in_group))  |> 
mutate(ownership_type = if_else(OWNER_PINF == 'ILIFFE MEDIA LIMITED', "Consolidated", ownership_type)) 

write_sheet(outlets_recoded, ss_id, 'beyond_the_masthead_final_recoded')
#######################################################
# Compute alternative ownership metrics
#######################################################

# Our primary ownership consolidation measure is outlets_in_group. We test the robustness of this metric by testing alternative formulations: (1) PINF's binary classification (independent if turnover < £2m; otherwise consolidated \cite{pinf_deserts_2023}); (2) outlets owned within the UKTwitNewsCor sample; (3) non-local status (registered address outside newsroom LAD).

# # Geocode data
# geocoded_data <- outlets_recoded |> 
#   left_join(pinf |> select(domain, primary_location), by = 'domain') |>
#   mutate(REGISTERED_ADDRESS = if_else(is.na(REGISTERED_ADDRESS), primary_location, REGISTERED_ADDRESS)) |> 
#   select(REGISTERED_ADDRESS, domain) |> 
#   distinct() |> 
#   rowwise() |>
#   mutate(
#     LOCATION = list(get_geocode(REGISTERED_ADDRESS))) |> 
#   mutate(
#     registered_lon = LOCATION[[1]],
#     registered_lat = LOCATION[[2]]
#   ) |>
#   select(-c(LOCATION, REGISTERED_ADDRESS)) |> 
#   ungroup()

# outlets_recoded <- outlets_recoded |>
#   left_join(geocoded_data, by = 'domain')

# outlets_w_ownership_vars <- outlets_recoded |>   
#   mutate(
#     ownership_binary_pinf = if_else(ownership_type == "Independent", 0L, 1L),
#     non_local_registered_address = # calculate if registered address is inside LAD using sf package and LAD shapefile
#   )

# # Load UK districts shapefile
# uk_districts <- st_read("Local_Authority_Districts_December_2021_GB_BGC_2022_7766355490887282253.zip")
# uk_districts <- st_transform(uk_districts, crs = 4326)  # Ensure it's in WGS84
# outlets_recoded_sf <- st_as_sf(outlets_recoded |> select(domain, registered_lat, registered_lon, LAD), coords = c("registered_lon", "registered_lat"), crs = 4326)

# # Perform spatial join to find which district each point falls into
# which_district <- st_join(outlets_recoded_sf, uk_districts, join = st_within)

# missing code about northern ireland! to be recuperated
# ended with saving data/outlets.rds

###################################################
# Typology (not used)
###################################################

# pinf <- read_sheet('https://docs.google.com/spreadsheets/d/1JmQlgtLaSXpdZ0lhoszsu-QHFgi6jJYjS5Fl5LKH6IQ/edit?gid=1414399196#gid=1414399196', sheet = 'OUTLETS_all') |> select(TITLE, URL, MEDIA_TYPE)

# patterns <- c('https://', 'http://', 'www.', '/', "http://www.", "https://www.")
# pinf <- pinf |> 
#   mutate(domain = str_remove(URL, paste(patterns, collapse = '|'))) |> 
#   mutate(domain = str_remove(domain, '/.*'),
#         domain = str_remove(domain, 'www.')) |> 
#   select(-TITLE, -URL) |> 
#   distinct() |> 
#   group_by(domain) |> 
#   summarise(MEDIA_TYPE = paste(unique(MEDIA_TYPE), collapse = '; '), .groups = 'drop')

# outlets |> select(TITLE, domain) |>
#   anti_join(pinf, by = 'domain')

# media_type_map <- c(
#   "london-se1.co.uk"       = "Online",
#   "mywelshpool.co.uk"      = "Online",
#   "dissmercury.co.uk"      = 'Print and Online',
#   "ipswichstar.co.uk"      = "Print and Online",
#   "buckinghamshirelive.com"= "Print and Online",
#   "getreading.co.uk"       = "Print and Online",
#   "wokingnewsandmail.co.uk"= 'Print and Online',
#   "wrexham.com"            = "Online",
#   "yourharlow.com"         = "Online",
#   "donegalnews.com"        = "Print and Online"
# )

# outlets$MEDIA_TYPE <- media_type_map[outlets$domain]
# outlets <- outlets |> left_join(pinf, by = 'domain')
# outlets <- outlets |> 
#   mutate(MEDIA_TYPE = coalesce(MEDIA_TYPE.x, MEDIA_TYPE.y)) |>
#   select(-c(MEDIA_TYPE.x, MEDIA_TYPE.y))

#########################################
# Panel Data Preparation (skip to next section to directly load data)
#########################################
articles <- read_rds('data/article_level_analysis_data.rds')
outlets <- read_rds('data/outlets.rds')
analysis_data <- read_rds("replication_data_geo_newsmapper/analysis_data.rds")

analysis_data %>%
  mutate(distance_from_outlet = geosphere::distHaversine(
    cbind(Longitude, Latitude), 
    cbind(outlet_lon, outlet_lat)) / 1000) %>%
  group_by(domain) %>%
  summarise(coverage_distance = median(distance_from_outlet, na.rm = TRUE)) |> 
  summarise(mean = mean(coverage_distance), median(coverage_distance), sd(coverage_distance), min(coverage_distance), max(coverage_distance))

# Calculate article-level distances and aggregate to outlet-quarter
panel_distance <- analysis_data %>%
  mutate(distance_from_outlet = geosphere::distHaversine(
    cbind(Longitude, Latitude), 
    cbind(outlet_lon, outlet_lat)) / 1000) %>%
  group_by(domain, year, quarter) %>%
  summarise(coverage_distance = median(distance_from_outlet, na.rm = TRUE),
            coverage_sd = sd(distance_from_outlet, na.rm = TRUE),
            .groups = "drop")

panel_data <- articles %>%
  group_by(domain, year, quarter) %>%
  summarise(
    geographic_fidelity = mean(has_local_location, na.rm = TRUE),
    syndication_rate = mean(is_duplicate, na.rm = TRUE),
    local_count = sum(has_local_location, na.rm = TRUE),
    dup_count = sum(is_duplicate, na.rm = TRUE),
    total_articles = n(),
    .groups = "drop"
  ) %>%
  # Join outlet-level characteristics (excluding COVID columns for now)
  left_join(outlets %>% 
            select(-c(geographic_fidelity, syndication_rate, median_distance_km, total_articles,
                     starts_with("Covid_"))), 
            by = "domain") %>%
  # Join quarterly coverage distance
  left_join(panel_distance, by = c("domain", "year", "quarter")) %>%
  # Add COVID rates by matching year and quarter
  left_join(
    outlets %>%
      select(domain, starts_with("Covid_")) %>%
      pivot_longer(cols = starts_with("Covid_"), 
                   names_to = "covid_period", 
                   values_to = "covid_rate") %>%
      separate(covid_period, into = c("temp", "year_quarter", "temp2"), sep = "_") %>%
      separate(year_quarter, into = c("year", "quarter"), sep = "Q") %>%
      select(-temp, -temp2) %>%
      mutate(year = as.numeric(year),
             quarter = as.numeric(quarter)),
    by = c("domain", "year", "quarter")
  ) %>%
  # Select and rename key variables
  select(
    domain, OWNER_PINF, year, quarter, geographic_fidelity, syndication_rate, coverage_distance, coverage_sd, total_articles, outlets_in_group, ownership_type, newsroom_distance_km, pop_per_sq_km, area_sq_km, lad_urban_rural, geocoding_success_rate, LAD, local_count, dup_count,covid_rate, 
  outlets_in_group, outlets_in_group_corpus, ownership_binary_pinf, ownership_binary_sample, non_local_registered_address) %>%
  # Create analysis variables
  mutate(
    # Log transformations
    log_articles = log(total_articles + 1),
    log_pop_density = log(pop_per_sq_km + 1),
    log_area = log(area_sq_km + 1),
    log_newsroom_dist = log(newsroom_distance_km + 1),
    log_coverage_dist = log(coverage_distance + 1),
    # Create time factors
    time_period = paste(year, "Q", quarter, sep = ""),
    time_factor = as.factor(time_period),  # Combined quarter-year factor (12 levels)
    year_factor = as.factor(year),         # Separate year factor (3 levels)
    quarter_factor = as.factor(quarter),    # Separate quarter factor (4 levels)
    domain = as.factor(domain),
           time_period = as.factor(time_period),
           lad_urban_rural = factor(lad_urban_rural, 
            levels = c("Urban", 
                      "Urban with rural areas",
                      "Rural", 
                      "Sparse and rural")))

# mark change in ownership, across quarters, for these domains: fetch from google sheets, sheet = 'ownership_change_GPT', where column 'former' is not NA. change ownership for quarter based on column 'when'. Before and up until 'when' is 'former', after 'when' remains as it was.
ownership_change <- read_sheet(ss_id, sheet = "ownership_change_GPT") |> 
  filter(!is.na(former)) |> 
  mutate(
    # standardise and parse the 'when' column
    when = str_replace_all(when, " ", ""),       # remove spaces like "2020 Q1" → "2020Q1"
    year_when = as.numeric(str_extract(when, "^[0-9]{4}")),  # extract year
    quarter_when = as.numeric(str_extract(when, "(?<=Q)[0-9]")) # extract quarter
  )

panel_data <- panel_data |>
    mutate(
      time_period_str = str_replace_all(as.character(time_period), " ", ""),
      year_num = as.numeric(str_extract(time_period_str, "^[0-9]{4}")),
      quarter_num = as.numeric(str_extract(time_period_str, "(?<=Q)[0-9]"))
    ) |>
    left_join(
      ownership_change |> select(domain, year_when, quarter_when, former),
      by = "domain"
    ) |>
    mutate(
      OWNER_PINF = case_when(
        !is.na(former) &
          (year_num < year_when | (year_num == year_when & quarter_num <= quarter_when)) ~ former,
        TRUE ~ OWNER_PINF
      ),
  
      # Adjust outlets_in_group_corpus
      outlets_in_group_corpus = if_else(OWNER_PINF == "ARCHANT", 37L, outlets_in_group_corpus),
      outlets_in_group_corpus = if_else(
        OWNER_PINF %in% c("Andy Barr", "Bullimore family", "Curry family"),
        1L, outlets_in_group_corpus),
      outlets_in_group_corpus = if_else(OWNER_PINF == "NEWSQUEST MEDIA GROUP LIMITED" &
        (year_num < 2022 | (year_num == 2022 & quarter_num <= 1)), 98L, outlets_in_group_corpus),
  
      # Set group sizes
      outlets_in_group = case_when(
        OWNER_PINF == "ARCHANT" ~ 37L, # includes Clear Sky
        OWNER_PINF %in% c("Andy Barr", "Bullimore family", "Curry family") ~ 1L,
        OWNER_PINF == "NEWSQUEST MEDIA GROUP LIMITED" &
          (year_num < 2022 | (year_num == 2022 & quarter_num <= 1)) ~ 156L,
        TRUE ~ outlets_in_group
      ),
    ownership_binary_sample = if_else(outlets_in_group <= 2, 0L, 1L)
  ) |>
    select(-former, -year_when, -quarter_when, -time_period_str, -year_num, -quarter_num)

# let's add cluster names
cluster <- read_sheet(ss_id, sheet = 'clustering_results') |> 
  select(Domain, New_cluster_name)

panel_data <- left_join(panel_data, cluster, by = c("domain" = "Domain"))
write_rds(panel_data, 'data/panel_data.rds')

# let's add cluster names
cluster <- read_sheet(ss_id, sheet = 'clustering_results') |> 
  select(Domain, New_cluster_name)

outlets <- left_join(outlets, cluster, by = c("domain" = "Domain"))
write_rds(outlets, 'data/outlets.rds')

# ##################################################
# # outlet-level non geographic data (not used)
# ##################################################
# article_sample <- read_csv('../6_geographic-local-media-classifier/replication_data_geo_newsmapper/articles_sample.csv.zip')

# # Calculate number of articles successfully geocoded
# pre_geo_counts <- outlets %>%
#   mutate(
#     articles_geocoded = total_articles * geocoding_success_rate
#   ) |> select(domain,geocoding_success_rate, articles_geocoded, total_articles, ownership_binary_pinf)

# pre_geo_counts |>
#     ggplot(aes(x = geocoding_success_rate, y = ownership_binary_pinf)) +
#     ggbeeswarm::geom_quasirandom(
#       size = 0.6,
#       alpha = 0.5,
#       color = "grey",
#       width = 0.4
#     ) +
#     geom_boxplot(width = 0.4,
#                  alpha = 0.5,
#                  outlier.shape = NA) +  # Add boxplot for median/IQR
#     theme_minimal() +
#     scale_x_log10(limits = c(10, 11000), position = "top") +
#     labs(title = "(a)") +
#     theme(
#       plot.title = element_text(hjust = 0.5),
#       axis.title.y = element_blank(),
#       axis.text.y = element_text(size = 10)
#     )
