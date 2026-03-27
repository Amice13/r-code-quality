# Data generation and analysis file for
# Clark and Zucker, "Climate Cascades," American Journal of Political Science.
# See codebook.pdf for descriptions of variables used in analyses.

# Loading packages (versions noted)
require(pacman) # 0.5.1
p_load(
  tidyverse, # 1.3.2
  broom, # 1.0.2
  ggrepel, # 0.9.2
  haven, # 2.5.1
  janitor, # 2.1.0
  lfe, # 2.8-8
  magrittr, # 2.0.3
  MASS, # 7.3-58.1
  multiwayvcov, # 1.2.3
  readxl, # 1.4.1
  stargazer, # 5.2.3
  pdftools, # 3.3.2
  tm, # 0.7-8,
  lmtest, # 0.9-40
  htmlwidgets # 1.6.2
               )

# Setting working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ################## #
# ****************** #
#### DATA LOADING ####
# ****************** #
# ################## #

# — IMF bureaucrat and climate mention data ####

imf_cy <- read_csv("data/article_iv_coding.csv") %>% # country-year Article IV data (hand-coded) -- CLIMATE MENTIONS
  mutate(climate = replace_na(climate, 0)) # replaced NAs with zeroes, denoting no climate mention (1-climate mentions already coded)

.imf_cy_slim <- imf_cy %>%
  dplyr::select(ctry, year, climate) # abridged version of country-year Article IV dataset (for merging)

imf_ry <- read_csv("data/article_iv_coding2.csv") %>% # country-year RR data (hand-coded career paths) -- BUREAUCRAT TRAJECTORIES
  distinct(rep, ctry, year, .keep_all = TRUE) %>% # country-resident rep-year data (limited to RRs who mention climate)
  left_join(.imf_cy_slim, by = c("ctry", "year")) # linking climate mentions to RR postings

imf_ry_history <- imf_ry %>%
  mutate(climate = replace_na(climate, 0)) %>% # replaced NAs with zeroes, denoting no climate mention (1-climate mentions already coded)
  group_by(rep, year) %>%
  summarize(climate = sum(climate, na.rm = TRUE)) %>% # summing climate mentions across postings (some RRs posted in multiple countries at once)
  ungroup() %>%
  mutate(year = year + 1) %>% # lagging dataset by one year
  group_by(rep) %>%
  arrange(year, .by_group = TRUE) %>% # sorting by year (ascending) within RR
  mutate(climate_cumul_l1 = cumsum(climate)) %>% # cumulative climate mentions in Article IV reports associated with each RR, prior to given year (per lag above)
  ungroup() %>%
  dplyr::select(-climate) # removing unnecessary column

imf_ry_history2 <- imf_ry %>%
  mutate(climate = replace_na(climate, 0)) %>% # replaced NAs with zeroes, denoting no climate mention (1-climate mentions already coded)
  group_by(rep, ctry) %>%
  arrange(year, .by_group = TRUE) %>% # sorting by year (ascending) with RR's country postings
  mutate(climate_cumul_incountry_l1 = cumsum(climate)) %>% # cumulative climate mentions in A4 reports in each country associated with each RR
  ungroup() %>%
  mutate(year = year + 1) %>% # lagging by one year (cumulative in-country mentions prior to current year)
  dplyr::select(-climate) # removing unnecessary column

imf_ry_country <- imf_ry %>%
  left_join(imf_ry_history, by = c("rep", "year")) %>% # merging cumulative mention data into main RR file
  left_join(imf_ry_history2, by = c("rep", "ctry", "year")) %>% # ditto (for in-country mentions)
  mutate(climate_cumul_other_l1 = climate_cumul_l1 - climate_cumul_incountry_l1) %>% # cumulative climate mentions in other countries (distinct from RR's current posting)
  mutate(climate_cumul_other_l1 = replace_na(climate_cumul_other_l1, 0)) %>% # replaced NAs with zeroes, denoting no climate mention (1-climate mentions already coded)
  group_by(ctry, year) %>%
  summarize(climate_cumul_l1 = sum(climate_cumul_l1, na.rm = TRUE), # cumulative prior mentions by bureaucrats in country
            climate_cumul_other_l1 = sum(climate_cumul_other_l1, na.rm = TRUE)) %>% # cumulative prior mentions by bureaucrats in country (excluding that current country)
  ungroup()

imf_cy_ry <- imf_cy %>% 
  left_join(imf_ry_country, by = c("ctry", "year")) %>% # merging country-specific bureaucrat data into contemporaneous climate mention data
  mutate(climate_cumul_l1 = replace_na(climate_cumul_l1, 0), # replaced NAs with zeroes, denoting no climate mention (1-climate mentions already coded)
         climate_cumul_other_l1 = replace_na(climate_cumul_other_l1, 0)) %>% # replaced NAs with zeroes, denoting no climate mention (1-climate mentions already coded)
  mutate(country_code = countrycode::countrycode(sourcevar = ctry,
                                                 origin = "country.name",
                                                 destination = "wb")) %>% # creating new country code column (World Bank codes)
  mutate(country_code = ifelse(ctry == "Micronesia",
                               "FSM",
                               country_code)) %>% # manually adding WB code for Micronesia given warning message
  mutate(country_code = ifelse(ctry == "Somoa",
                               "WSM",
                               country_code)) %>% # manually adding WB code for Samoa
  filter(year < 2019) # limiting to years before 2019

# — WDI data ####

wdi <- read_csv("data/wdi_data_full.csv") %>% # loading full World Bank WDI dataset (dated January 30, 2019)
  clean_names() %>% # cleaning up column names
  pivot_longer(cols = starts_with("x"),
               names_to = "year", 
               values_to = "value") %>% # converting columns (each variable) to rows (wide to long format)
  mutate(year = as.numeric(gsub(pattern = "x", replacement = "", year))) %>% # cleaning years
  dplyr::select(country_code, indicator_name, year, value) %>% # condensing data
  pivot_wider(names_from = indicator_name,
              values_from = value) %>% # converting country-year-variable format to country-year format
  clean_names() %>% # cleaning column names
  mutate(year = year + 1) %>% # lagging WDI by one year
  dplyr::select(country_code, year, gdp_per_capita_constant_2010_us)

imf_cy_ry <- imf_cy_ry %>%
  left_join(wdi, by = c("country_code", "year")) # merging WDI data into main dataset (imf_cy_ry)

# — Climate disaster data ####

emdat <- read_excel("FILE PATH TO EM-DAT DATA / SEE CODEBOOK FOR DOWNLOAD DETAILS") %>% # loading EM-DAT data (dated May 14, 2021); “EM-DAT, CRED / UCLouvain, Brussels, Belgium – www.emdat.be”
  slice(-c(1:6)) %>% # removing EM-DAT Excel file metadata rows
  group_by(ISO, Year) %>%
  summarize(climate_disaster = sum(`Disaster Subgroup` == "Climatological")) %>% # summing number of climatological disasters by country-year
  ungroup() %>%
  mutate(country_code = countrycode::countrycode(sourcevar = ISO,
                                                 origin = "iso3c",
                                                 destination = "wb")) %>% # converting ISO3 (character) country codes to WB codes
  mutate(Year = as.numeric(Year) + 1) %>% # lagging climate disaster data by one year
  rename(year = Year) %>% # standardizing year name
  dplyr::select(-ISO)
# disregard warnings (irrelevant to analyses)

emdat2 <- read_excel("FILE PATH TO EM-DAT DATA / SEE CODEBOOK FOR DOWNLOAD DETAILS") %>% # loading EM-DAT data (dated May 14, 2021); “EM-DAT, CRED / UCLouvain, Brussels, Belgium – www.emdat.be”
  slice(-c(1:6)) %>% # removing EM-DAT Excel file metadata rows
  group_by(ISO, Year) %>%
  summarize(meteorological_disaster = sum(`Disaster Subgroup` == "Meteorological")) %>%
  ungroup() %>%
  mutate(country_code = countrycode::countrycode(sourcevar = ISO,
                                                 origin = "iso3c",
                                                 destination = "wb")) %>%
  mutate(Year = as.numeric(Year) + 1) %>%
  rename(year = Year) %>%
  dplyr::select(-ISO)
# disregard warnings (irrelevant to analyses)

imf_cy_ry <- imf_cy_ry %>% # merging climatological/meteorological disaster data into main dataset
  left_join(emdat, by = c("country_code", "year")) %>%
  left_join(emdat2, by = c("country_code", "year"))

imf_cy_ry <- imf_cy_ry %>% # replacing NAs for natural disasters with zeroes (positive values already coded)
  mutate(climate_disaster = replace_na(climate_disaster, 0)) %>%
  mutate(meteorological_disaster = replace_na(meteorological_disaster, 0))

# — Polity2 data ####

polity2 <- read_excel("data/polity.xls") %>% # loading in Polity2 data (from 2018)
  mutate(country_code = countrycode::countrycode(sourcevar = country,
                                                 origin = "country.name",
                                                 destination = "wb")) %>% # creating WB country code data
  dplyr::select(country_code, year, polity2) %>%
  mutate(year = year + 1) # lagging by a year
# disregard warnings (irrelevant to analyses)

imf_cy_ry <- imf_cy_ry %>% # merging Polity2 data into main dataset
  left_join(polity2, by = c("country_code", "year"))

# — IMF program participation data ####

imf_programs <- read_csv("data/imf_programs.csv") %>% # IMF program participation from Kentikelenis, Stubbs, and King (2016)
  mutate(year = year + 1) %>% # lagging by one year
  mutate(ccode = countrycode::countrycode(sourcevar = ccode,
                                          origin = "iso3c",
                                          destination = "wb")) # converting ISO3 (character) to WB codes
imf_programs <- unique(imf_programs) # removing duplicate rows (some countries have multiple IMF program units in a year)

imf_cy_ry <- imf_cy_ry %>% # merging IMF data into main dataset
  left_join(imf_programs, by = c("country_code" = "ccode", "year")) %>%
  mutate(IMF = replace_na(IMF, 0)) # replacing NAs for IMF program participation with zeroes

# — UN voting data ####

un_votes <- read_csv("data/un_voting.csv") %>% # UNGA voting data (March 2021 version) from Bailey, Strezhnev, and Voeten (2017)
  filter(ccode1 == 2) %>% # limiting one-half of dyad to US
  mutate(ccode2 = countrycode::countrycode(ccode2,
                                           "cown",
                                           "wb")) %>% # converting COW country codes to WB
  dplyr::select(ccode2, year, IdealPointDistance) %>% # ideal point distance between country and US for each year
  mutate(year = year + 1) # lagging by one year

imf_cy_ry <- imf_cy_ry %>% # merging UN voting data into main datset
  left_join(un_votes, by = c("country_code" = "ccode2", "year"))

# — IMF managing director data ####

managing_directors <- read_excel("Data/imf_directors.xlsx") # managing director terms (hand coded based on public data)

imf_cy_ry <- imf_cy_ry %>% # merging managing director terms into main dataset by year
  left_join(managing_directors, by = "year")

# — Lagged Article IV mention DV ####

imf_cy_ry %<>%
  group_by(country_code) %>%
  mutate(climate_lagged_dv = dplyr::lag(climate, n = 1)) %>% # creating lagged count of climate mentions in Article IV reports
  ungroup()

# — Bureaucrat-level data ####

imf_ry2 <- read_csv("data/article_iv_coding2.csv")  %>% # reloading RR career trajectory data
  mutate(ctry = case_when(ctry == "Somoa" ~ "Samoa", # fixing misspellings
                          ctry == "Marshal Islands" ~ "Marshall Islands",
                          TRUE ~ ctry))
imf_ry2$country_code <- countrycode::countrycode(imf_ry2$ctry, 
                                                 origin = "country.name", 
                                                 destination = "wb") # creating WB country code column

imf_ry2 <- merge(unique(imf_ry2), 
                 unique(emdat),
                 c("country_code", "year"), all.x = TRUE) # merging climatological disaster data into RR career trajectory dataset
imf_ry2$climate_disaster[is.na(imf_ry2$climate_disaster)] <- 0 # replacing natural disaster NAs with zeroes
imf_ry2 <- merge(unique(imf_ry2), 
                 unique(emdat2), 
                 c("country_code", "year"), all.x = TRUE) # merging meteorological disaster data into RR career trajectory dataset
imf_ry2$meteorological_disaster[is.na(imf_ry2$meteorological_disaster)] <- 0

imf_ry2_history <- imf_ry2 %>%
  mutate(climate_disaster = replace_na(climate_disaster, 0)) %>% # replacing climatological disaster NAs with zeroes
  mutate(meteorological_disaster = replace_na(meteorological_disaster, 0)) %>% # replacing meteorological disaster NAs with zeroes
  group_by(rep, year) %>%
  summarize(climate_disaster = sum(climate_disaster, na.rm = TRUE), # summing disasters by RR-year
            meteorological_disaster = sum(meteorological_disaster, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = year + 1) %>% # lagging dataset by a year (each RR's history of exposure to natural disasters)
  group_by(rep) %>%
  arrange(year, .by_group = TRUE) %>% # sorting by year (ascending) within RR
  mutate(climate_disaster_cumul_l1 = cumsum(climate_disaster)) %>% # each RR's cumulative prior exposure to climatological disasters
  mutate(meteorological_disaster_cumul_l1 = cumsum(meteorological_disaster)) %>% # each RR's cumulative prior exposure to meteorological disasters
  ungroup() %>%
  dplyr::select(-climate_disaster) # cleaning up unnecessary column

imf_ry2_history2 <- imf_ry2 %>%
  mutate(climate_disaster = replace_na(climate_disaster, 0)) %>% # replacing NAs with zeroes
  mutate(meteorological_disaster = replace_na(meteorological_disaster, 0)) %>% # replacing NAs with zeroes
  group_by(rep, ctry) %>%
  arrange(year, .by_group = TRUE) %>% # sorting by year (ascending) within RR-country posting
  mutate(climate_disaster_cumul_incountry_l1 = cumsum(climate_disaster)) %>% # cumulative climatological disasters during an RR's time in a given country
  mutate(meteorological_disaster_cumul_incountry_l1 = cumsum(meteorological_disaster)) %>% # cumulative meteorological disasters during an RR's time in a given country
  ungroup() %>%
  mutate(year = year + 1) %>% # lagging by one year
  dplyr::select(-climate_disaster, -meteorological_disaster)

imf_ry2_country <- imf_ry2 %>%
  left_join(imf_ry2_history, by = c("rep", "year")) %>% # merging in RR's prior exposure to disasters (all postings)
  left_join(imf_ry2_history2, by = c("rep", "ctry", "year")) %>% # merging in RR's prior exposure to disasters (current posting)
  mutate(climate_disaster_cumul_other_l1 = climate_disaster_cumul_l1 - climate_disaster_cumul_incountry_l1) %>% # exposure to climatological disasters in other countries
  mutate(climate_disaster_cumul_other_l1 = replace_na(climate_disaster_cumul_other_l1, 0)) %>% # replacing NAs with zeroes
  mutate(meteorological_disaster_cumul_other_l1 = meteorological_disaster_cumul_l1 - meteorological_disaster_cumul_incountry_l1) %>% # exposure to meteorological disasters in other countries
  mutate(meteorological_disaster_cumul_other_l1 = replace_na(meteorological_disaster_cumul_other_l1, 0)) %>% # replacing NAs with zeroes
  group_by(ctry, year) %>% # summing disaster counts by country-year
  summarize(climate_disaster_cumul_l1 = sum(climate_disaster_cumul_l1, na.rm = TRUE),
            climate_disaster_cumul_other_l1 = sum(climate_disaster_cumul_other_l1, na.rm = TRUE),
            meteorological_disaster_cumul_l1 = sum(meteorological_disaster_cumul_l1, na.rm = TRUE),
            meteorological_disaster_cumul_other_l1 = sum(meteorological_disaster_cumul_other_l1, na.rm = TRUE)) %>%
  ungroup()

imf_cy_ry2 <- left_join(imf_cy_ry, imf_ry2_country, by = c("ctry", "year")) # merging RR disaster exposure data into main dataset (imf_cy_ry)

imf_ry2_history2 <- imf_ry2_history2 %>% # merging datasets
  left_join(imf_cy_ry2, by = c("country_code", "year"))

imf_ry2_history3a1 <- imf_ry %>%
  mutate(climate = ifelse(is.na(climate), 0, climate)) %>% # replacing NAs with zeroes
  group_by(rep) %>%
  arrange(year) %>% # sorting by year (ascending) within RR
  mutate(any_climate_history_rr = as.numeric(cumsum(climate) > 0)) %>% # binary for whether RR mentioned climate at any point in associated Article IV report
  dplyr::select(-climate) # cleaning up

imf_ry2_history3 <- imf_ry2_history2 %>%
  mutate(any_climate_history_rr = climate_cumul_l1 + climate) %>% # cumulative sum of climate mentions by each bureaucrat
  mutate(any_climate_history_rr = as.numeric(any_climate_history_rr > 0)) # binary for whether RR mentioned climate at any point in associated Article IV report

imf_ry2_history3b <- imf_ry2_history3 %>%
  dplyr::select(rep.x, any_climate_history_rr, year, country_code, climate_cumul_l1, climate_cumul_other_l1) %>% # condensing
  mutate(year = year + 1) # lagging by one year

imf_ry2_history3 %<>% # merging data (merging current and lagged variables)
  left_join(imf_ry2_history3b, by = c("rep.x", "year", "country_code"), suffix = c("", "_l1"))

imf_ry2_history3 <- unique(imf_ry2_history3) # dropping duplicate rows

imf_ry2_history2_to_merge <- imf_ry2_history2 %>% # creating condensed RR dataset for merging
  dplyr::select(meteorological_disaster_cumul_incountry_l1,
                climate_disaster,
                climate_disaster_cumul_incountry_l1,
                polity2,
                gdp_per_capita_constant_2010_us,
                IMF,
                IdealPointDistance,
                year,
                country_code,
                rep.x) %>%
  rename(rep = rep.x)

imf_ry2_history3a2 <- imf_ry2_history3a1 %>%
  mutate(ctry = case_when(ctry == "Somoa" ~ "Samoa", # fixing misspellings
                          ctry == "Marshal Islands" ~ "Marshall Islands",
                          TRUE ~ ctry)) %>%
  mutate(country_code = countrycode::countrycode(ctry, "country.name", "iso3c")) %>% # creating country code column
  mutate(country_code = ifelse(ctry == "Micronesia",
                               "FSM",
                               country_code)) %>% # manually adding country code for Micronesia given warning
  left_join(imf_ry2_history2_to_merge, by = c("rep", "year", "country_code")) # merging

imf_ry2_history3_learning <- imf_ry2_history3 %>%
  mutate(ctry.x = case_when(ctry.x == "Somoa" ~ "Samoa", # fixing misspellings
                            ctry.x == "Marshal Islands" ~ "Marshall Islands",
                            TRUE ~ ctry.x)) %>%
  mutate(country_code = countrycode::countrycode(ctry.x, "country.name", "iso3c")) %>% # creating country code column
  mutate(country_code = ifelse(ctry.x == "Micronesia",
                               "FSM",
                               country_code)) %>% # manually adding country code for Micronesia given warning
  left_join(imf_ry2_history2_to_merge, by = c("rep.x" = "rep", "year", "country_code"),
            suffix = c("_old", "")) # distinguishing duplicate variables

# — Rotation-by-dyad data ###

unique_bureaucrats <- unique(imf_ry$rep) # creating list of RRs who mentioned climate at some point
unique_countries <- unique(imf_ry$ctry) # creating list of countries with Article IV reports

bureaucrat_country_matrix <- matrix(nrow = length(unique_bureaucrats), # creating RR-country matrix
                                    ncol = length(unique_countries))

for (i in 1:nrow(bureaucrat_country_matrix)){
  .bureaucrat_countries_assigned_to <- unique(imf_ry$ctry[imf_ry$rep == unique_bureaucrats[i]]) # identifying countries in which each RR was stationed
  for (j in 1:ncol(bureaucrat_country_matrix)){
    bureaucrat_country_matrix[i,j] <- ifelse(unique_countries[[j]] %in% .bureaucrat_countries_assigned_to, # coding 0 if bureaucrat never in country, 1 if bureaucrat in country at some point
                                             1,
                                             0)
    #print(paste0(i,"-",j)) #uncomment for progress indicator
  }
}

country_country_matrix <- matrix(nrow = length(unique_countries), # creating country-country matrix
                                 ncol = length(unique_countries))
.overlap_count_holder <- rep(NA, nrow(bureaucrat_country_matrix))

for (i in 1:nrow(country_country_matrix)){ # creating loop to quantify total RR rotations within country dyads
  for (j in 1:ncol(country_country_matrix)){
    for (k in 1:nrow(bureaucrat_country_matrix)){
      .overlap_count_holder[k] <- ifelse(bureaucrat_country_matrix[k, i] == 1 && bureaucrat_country_matrix[k, j] == 1,
                                         1,
                                         0)
      country_country_matrix[i,j] <- sum(.overlap_count_holder, na.rm = TRUE)
      
      #print(paste0(i,"-",j,"-",k)) #uncomment for progress indicator
    }
  }
}
diag(country_country_matrix) <- 0 # coding diagonal as zero (no movement of bureaucrats within same country)
country_country_matrix2 <- as.data.frame(country_country_matrix) # converting matrix to dataframe

# write_csv(country_country_matrix2, "overlap_matrix.csv") ### NOTE: manually add first col with empty header, indexing rows 1, 2, 3... ## exporting this for use in Mathematica

country_country_table <- expand_grid(ctry1 = 1:length(unique_countries), # creating country dyad dataset
                                     ctry2 = 1:length(unique_countries))
country_country_table$overlaps <- NA # holder column

for (i in 1:nrow(country_country_table)){ # summing bureaucrat movements within each country dyad
  country_country_table$overlaps[i] <- country_country_matrix[country_country_table$ctry1[i], country_country_table$ctry2[i]]
  #print(i) #uncomment for progress indicator
}

country_correspondence <- tibble(number = 1:length(unique_countries),
                                 country = unique_countries) # associating countries with numerical indices (country name - index correspondence)

# — IMF staff reports (scraping) ####
numbering <- read_delim("data/numbering.txt",
                        delim = "\t") %>% # list with highest report ID by year
  group_by(year) %>%
  summarize(max = as.integer(max(max))) %>%
  ungroup() %>%
  filter(year >= 2003)

url_formatter <- function(year, number){ # function for generating URL format
  .url_format <- paste0("https://www.imf.org/-/media/Websites/IMF/imported-full-text-pdf/external/pubs/ft/wp/",
                        year,
                        "/_wp",
                        substr(year, 3, 4),
                        number,
                        ".ashx")
}

url_formatter2 <- function(year, number){ # alternative function for generating URL format
  .url_format <- paste0("https://www.imf.org/-/media/Files/Publications/WP/",
                        "wp",
                        substr(year, 3, 4),
                        number,
                        ".ashx")
}

url_formatter2b <- function(year, number){ # alternative function for generating URL format
  .url_format <- paste0("https://www.imf.org/-/media/Files/Publications/WP/",
                        year,
                        "/wp",
                        substr(year, 3, 4),
                        number,
                        ".ashx")
  print(.url_format)
}

url_list2 <- tibble(year = rep(numbering$year, numbering$max)) %>% # creating holder table
  group_by(year) %>%
  mutate(id = row_number()) %>%
  mutate(id = ifelse(nchar(id) == 1, str_pad(string = id,
                                             width = 2,
                                             side = "left",
                                             pad = "0"),
                     id)) %>%
  ungroup()
url_list2$url <- ifelse(url_list2$year <= 2016, # coding URLs up to 2016
                        url_formatter(url_list2$year, url_list2$id),
                        url_formatter2(url_list2$year, url_list2$id))
url_list2$url <- ifelse(url_list2$year >= 2017 & as.integer(url_list2$id) >= 35, # coding URLs post-2016
                        url_formatter2b(url_list2$year, url_list2$id),
                        url_list2$url)

skip_with_message = simpleError('Did not work out')
for(i in seq_along(url_list2$url)){ ## downloading files from IMF website (**MAY TAKE A WHILE**)
  tryCatch(download.file(url = url_list2$url[i], 
                         destfile = paste0("data/IMF Working Papers/", paste0(url_list2$year[i],
                                                                              "_",
                                                                              gsub(pattern = ".ashx",
                                                                                   replacement = ".pdf",
                                                                                   x = basename(url_list2$url[i])))),
                         mode = "wb"),
           error = function(e) skip_with_message)
  
  #print(i) #uncomment for progress indicator
}

# ################## #
# ****************** #
#### DESCRIPTIVES ####
# ****************** #
# ################## #

# — Countries entering climate circle ####

.imf_cy_slim %>% # plotting number of countries with climate mentions in Article IV reports over time
  group_by(year) %>%
  summarize(unique_countries = n_distinct(ctry[climate > 0], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cumul_countries = cumsum(unique_countries)) %>%
  filter(year < 2019) %>%
  ggplot(aes(x = year, y = unique_countries)) +
  geom_line() +
  theme_classic() +
  ylab("Countries with climate mentions") +
  xlab("Year")
ggsave(filename = "countries_mentioning_climate.pdf", # exporting plot (Figure 1a)
       device = "pdf",
       height = 2.5, width = 4, unit = "in")

# — Climate mentions over time ####

.imf_cy_slim %>% # plotting cumulative climate mentions in Article IV reports over time
  group_by(year) %>%
  summarize(climate = sum(climate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cumul_climate = cumsum(climate)) %>%
  filter(year < 2019) %>%
  ggplot(aes(x = year, y = cumul_climate)) +
  geom_line() +
  theme_classic() +
  ylab("Cumulative climate mentions") +
  xlab("Year")
ggsave(filename = "cumulative_climate_mentions.pdf", # exporting plot of cumulative climate mentions by year (Figure 1b)
       device = "pdf",
       height = 2.5, width = 4, unit = "in")

# — Mentions in IMF staff reports ####

wp_file_list <- list.files("data/IMF Working Papers/") # loading list of downloaded IMF staff reports

wp_all <- list() # empty list

skip_with_message = simpleError('Did not work out')
for (i in 1:length(wp_file_list)){ # loop to analyze each downloaded IMF staff report, culminating in word frequency list
  tryCatch({.wp0001 <- pdf_text(paste0("data/IMF Working Papers/", wp_file_list[[i]]))
  .wp0001_df <- .wp0001 %>% enframe()
  .wp0001_corpus <- Corpus(VectorSource(.wp0001_df$value))
  .wp0001_corpus <- tm_map(.wp0001_corpus, content_transformer(tolower))
  .wp0001_corpus <- tm_map(.wp0001_corpus, removeNumbers)
  .wp0001_corpus <- tm_map(.wp0001_corpus, removeWords, stopwords('english'))
  .wp0001_corpus <- tm_map(.wp0001_corpus, removePunctuation)
  .wp0001_corpus <- tm_map(.wp0001_corpus, removeWords, c("–", "can", "“", "also", "new", "many"))
  .wp0001_corpus <- tm_map(.wp0001_corpus, stripWhitespace)
  .wp0001_tdm <- TermDocumentMatrix(.wp0001_corpus)
  .wp0001_tdm_mat <- as.matrix(.wp0001_tdm)
  .wp0001_wordfreq <- sort(rowSums(.wp0001_tdm_mat), decreasing = T)
  .wp0001_wordfreq_list <- tibble(word = names(.wp0001_wordfreq), freq = .wp0001_wordfreq) %>%
    filter(! word %in% c("–", "“", "−", "•", "*")) %>%
    mutate(year = substr(wp_file_list[[i]], 1, 4))
  
  wp_all[[i]] <- .wp0001_wordfreq_list},
  error = function(e) skip_with_message)
  
  #print(i) #uncomment for progress indicator
  
}

wp_all <- do.call(rbind, wp_all) %>% # binding outputs of above loop together
  group_by(year, word) %>%
  summarize(freq = sum(freq, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(-freq)

wp_all %<>% group_by(year) %>% # summing word frequency across reports by year
  mutate(total = sum(freq, na.rm = TRUE)) %>%
  ungroup()

wp_all %>% # plotting mentions of "climate" over time
  filter(word == "climate") %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(x = year, y = freq/total, group = word)) +
  geom_line() +
  theme_classic() +
  ylab("Relative frequency: climate") +
  xlab("Year")
ggsave(filename = "staffReport_climate_mentions.pdf", # exporting Figure 1c
       device = "pdf",
       height = 2.5, width = 4, unit = "in")

wp_all %>% # plotting mentions of "climate" over time
  filter(word == "carbon") %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(x = year, y = freq/total, group = word)) +
  geom_line() +
  theme_classic() +
  ylab("Relative frequency: carbon") +
  xlab("Year")
ggsave(filename = "staffReport_carbon_mentions.pdf", # exporting Figure 1d
       device = "pdf",
       height = 2.5, width = 4, unit = "in")

# — Diffusion by country ####

diff_by_country <- .imf_cy_slim %>%
  filter(climate > 0) %>% # identifying country-years with climate mentions in Article IV report
  arrange(year, -climate) %>% # sorting by climate mentions
  group_by(year) %>%
  mutate(rank = rank(climate, ties.method = "random")) # ranking countries by climate mentions

ctry_names <- diff_by_country$ctry # country list
ctry_names <- str_replace(string = ctry_names, pattern = "Islands", replacement = "Is.") # shortening country names for plotting purposes
ctry_names <- str_replace(string = ctry_names, pattern = "Republic", replacement = "Rep.") # shortening country names for plotting purposes
ctry_names <- str_replace(string = ctry_names, pattern = "The ", replacement = "") # shortening country names for plotting purposes
ctry_names <- str_replace(string = ctry_names, pattern = " New Guinea", replacement = "") # shortening country names for plotting purposes
ctry_names <- str_replace(string = ctry_names, pattern = "Antigua and Barbuda", replacement = "Antigua & Barbuda") # shortening country names for plotting purposes

diff_by_country$ctry2 <- ctry_names

diff_by_country %>% # plotting list of countries with climate mentions in each year
  filter(year <= 2018) %>%
  ggplot(aes(x = year, label = ctry2)) +
  geom_text(aes(y = rank), size = 2) +
  xlab("Year") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) -> diff_by_country

ggsave(diff_by_country, # exporting Figure 2
       filename = "diff_by_country.pdf",
       device = "pdf",
       height = 3, width = 7.25, unit = "in")

# ############## #
# ************** #
#### ANALYSES ####
# ************** #
# ############## #

# — Table 1: Climate Attentiveness (Learning) ####

climate_correlate01 <- felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                                         climate_disaster_cumul_incountry_l1) | rep.x + year | 0 | rep.x + country_code, 
                            data = imf_ry2_history3_learning, 
                            subset = year >= 2000) # continuous treatment -- did RR ever mention climate, as function of current-country exposure to climate-related disasters?
tidy(climate_correlate01)

climate_correlate02 <- felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                                         climate_disaster_cumul_incountry_l1) +
                              polity2 + 
                              log1p(gdp_per_capita_constant_2010_us) + 
                              IMF + 
                              IdealPointDistance | rep.x + year | 0 | rep.x + country_code, 
                            data = imf_ry2_history3_learning,
                            subset = year >= 2000) # same as above, with standard covariate set
tidy(climate_correlate02)

# — Table 2: Climate Mentions (Spread) ####

count_ols01b_bin <- felm(climate ~ I(climate_cumul_other_l1 > 0) | country_code + year | 0 | country_code, 
                         data = imf_cy_ry) # binary treatment, continuous outcome
tidy(count_ols01b_bin)

count_ols02b_bin <- felm(climate ~ I(climate_cumul_other_l1 > 0) +
                           polity2 + 
                           log1p(gdp_per_capita_constant_2010_us) + 
                           I(climate_disaster+meteorological_disaster) +
                           IMF + 
                           IdealPointDistance | country_code + year | 0 | country_code,
                         data = imf_cy_ry) # binary treatment, continuous outcome, with controls
tidy(count_ols02b_bin)

binary_ols01b_bin <- felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) | country_code + year | 0 | country_code, 
                          data = imf_cy_ry) # binary treatment, binary outcome
tidy(binary_ols01b_bin)

binary_ols02b_bin <- felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
                            polity2 + 
                            log1p(gdp_per_capita_constant_2010_us) + 
                            I(climate_disaster+meteorological_disaster) +
                            IMF + 
                            IdealPointDistance | country_code + year | 0 | country_code,
                          data = imf_cy_ry) # binary treatment, binary outcome, with controls
tidy(binary_ols02b_bin)

# — Footnote 25: Climate Attentiveness Predicting Exposure to Climate Disasters ####

felm(I(climate_disaster + climate_disaster_cumul_incountry_l1 +
         meteorological_disaster + meteorological_disaster_cumul_incountry_l1) ~ I(climate_cumul_other_l1 > 0) | 
       rep.x + year | 0 | rep.x,
     data = imf_ry2_history3) %>% # regressing cumulative climate-related disasters experienced in country by bureaucrat through current year, on binary indicator of whether bureaucrat had discussed climate in early country posting
  tidy()

# — Footnote 33: Climate Mentions and Report Length ####

a4_file_list <- list.files("data/Article IV Reports/")

a4_holder <- tibble(doc = a4_file_list, 
                    number = gsub("cr|_|.pdf", "", doc),
                    cc_mention = NA,
                    word_count = NA) %>%
  mutate(number = gsub("(.*)-(.*)", "\\1", number)) %>%
  filter(nchar(number) <= 5) %>%
  mutate(number_numeric = as.integer(number)) %>%
  arrange(number_numeric) %>%
  mutate(no = row_number()) %>%
  dplyr::select(doc, no, cc_mention, word_count)

skip_with_message = simpleError('Did not work out')
for (i in seq_along(a4_holder$doc)){ # loading Article IV reports and counting climate mentions
  tryCatch(
    .wp0001 <- pdf_text(paste0("data/Article IV Reports/", a4_file_list[[i]])),
    error = function(e) skip_with_message
  )
  .wp0001 <- tolower(.wp0001)
  a4_holder$cc_mention[i] <- sum(str_detect(.wp0001, "climate"))
  a4_holder$word_count[i] <- sum(lengths(strsplit(.wp0001, "\\W+")))
  
  #print(i) #uncomment for progress indicator
}

felm(word_count ~ cc_mention | 0 | 0 | 0,
     data = a4_holder) %>% # regressing word count on number of climate mentions in each report
  tidy()
306/mean(a4_holder$word_count) # change in word count divided by mean word count

# ################### #
# ******************* #
#### TABLE EXPORTS ####
# ******************* #
# ################### #

# — Table 1: Climate Attentiveness (Learning) ####

stargazer(climate_correlate01,
          climate_correlate02,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Democracy (Polity2)",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "rr_acclimation_results.tex")

stargazer(climate_correlate01,
          climate_correlate02,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Democracy (Polity2)",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = FALSE,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"))

# — Table 2: Climate Mentions (Spread) ####

stargazer(count_ols01b_bin,
          count_ols02b_bin,
          binary_ols01b_bin,
          binary_ols02b_bin,
          dep.var.labels = c("\\# Climate Mentions", "Any Climate Mention"),
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions (other countries)",
                               "Democracy (Polity2)",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "baseline2_results.tex")

stargazer(count_ols01b_bin,
          count_ols02b_bin,
          binary_ols01b_bin,
          binary_ols02b_bin,
          dep.var.labels = c("Number of Climate Mentions", "Any Climate Mention"),
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions (other countries)",
                               "Democracy (Polity2)",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = FALSE,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"))

# ################ #
# **************** #
#### APPENDICES ####
# **************** #
# ################ #

# — Appendix A: Bureaucrats in Sample ####

service_length <- read_csv("data/article_iv_coding2.csv") %>% # bureaucrat career data
  dplyr::select(rep, year) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(rep) %>%
  summarize(min_year = min(year), # first year in data
            max_year = max(year)) %>% # last year in data
  ungroup()

service_length %>%
  mutate(length = max_year - min_year + 1) %$% # number of years in data (years between min and max year, inclusive)
  hist(length, xlab = "Time in Sample", main = "") # histogram of bureaucrat service length

service_length %>%
  mutate(id = row_number()) %>%
  ggplot() +
  geom_segment(aes(x = min_year,
                   xend = max_year,
                   y = id,
                   yend = id)) +
  xlim(2000, 2018) +
  ylab("Bureaucrat ID") +
  xlab("Year") +
  ggtitle("Tenures of Bureaucrats Who Discussed Climate") +
  theme_classic()

median(service_length$max_year - service_length$min_year + 1) # descriptive stats on tenures
quantile(service_length$max_year - service_length$min_year + 1)

# — Appendix B: States by Centrality to Bureaucrat Movement ####

centralities <- read_csv("centralityList2.csv",
                         col_names = FALSE) %>% # reading in eigenvector centrality data from Mathematica file (see: 02_network.nb)
  rename(ctry = X1,
         eig_cent = X2) %>% # renaming columns
  arrange(-eig_cent) %>% # sorting by eigenvector centrality
  mutate(index = row_number()) %>%
  left_join(country_correspondence,
            by = c("ctry" = "number"))

centralities %>% # plotting eigenvector centralities by country
  mutate(country = ifelse(index <= 5, country, NA)) %>% # limiting labels to five most central countries
  ggplot(aes(x = index, y = eig_cent, label = country)) +
  geom_point() +
  theme_classic() +
  ylab("Eigenvector centrality") +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_label_repel()
ggsave(filename = "eigenvector_centralities.pdf", # exporting plot of eigenvector centralities (Figure B1)
       device = "pdf",
       height = 4, width = 3.5, unit = "in")

# — Appendix C: Network Communities ####

# See 02_network.nb

# — Appendix D: Logit Specification: Learning ####

climate_correlate_glm0 <- glm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                                            climate_disaster_cumul_incountry_l1) +
                                as.factor(rep.x) + as.factor(year),
                              data = imf_ry2_history3,
                              family = "binomial") # learning model estimated via binomial logistic regression
tidy(climate_correlate_glm0)

climate_correlate_glm1 <- glm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                                            climate_disaster_cumul_incountry_l1) +
                                polity2 + 
                                log1p(gdp_per_capita_constant_2010_us) + 
                                IMF + 
                                IdealPointDistance +
                                as.factor(rep.x) + as.factor(year),
                              data = imf_ry2_history3,
                              family = "binomial")  # learning model estimated via binomial logistic regression, with controls
tidy(climate_correlate_glm1)

stargazer(climate_correlate_glm0,
          climate_correlate_glm1,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Polity2",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          omit = "as.factor",
          out = "rr_acclimation_results_glm.tex")

# — Appendix E: Clustering by Country ####

climate_correlate01_seCountryCluster <- felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                                                          climate_disaster_cumul_incountry_l1) | rep.x + year | 0 | country_code, 
                                             data = imf_ry2_history3_learning,
                                             subset = year >= 2000)
tidy(climate_correlate01_seCountryCluster)

climate_correlate02_seCountryCluster <- felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                                                          climate_disaster_cumul_incountry_l1) +
                                               polity2 + 
                                               log1p(gdp_per_capita_constant_2010_us) + 
                                               IMF + 
                                               IdealPointDistance | rep.x + year | 0 | country_code, 
                                             data = imf_ry2_history3_learning,
                                             subset = year >= 2000)
tidy(climate_correlate02_seCountryCluster)

stargazer(climate_correlate01_seCountryCluster,
          climate_correlate02_seCountryCluster,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Polity2",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "rr_acclimation_results_seCountryCluster.tex")

# — Appendix F: Managing Directors and Learning ####

climate_correlate01md <- felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                                           climate_disaster_cumul_incountry_l1) | rep.x + name | 0 | rep.x + country_code, 
                              data = imf_ry2_history3,
                              subset = year >= 2000) # including managing director ("name") fixed effects
tidy(climate_correlate01md)

climate_correlate02md <- felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                                           climate_disaster_cumul_incountry_l1) +
                                polity2 + 
                                log1p(gdp_per_capita_constant_2010_us) + 
                                IMF + 
                                IdealPointDistance | rep.x + name | 0 | rep.x + country_code, 
                              data = imf_ry2_history3,
                              subset = year >= 2000) # including managing director ("name") fixed effects
tidy(climate_correlate02md)

stargazer(climate_correlate01md,
          climate_correlate02md,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Polity2",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "rr_acclimation_results_mdFEs.tex")

imf_cy_ry %<>%
  group_by(name, country_code) %>%
  arrange(year) %>%
  mutate(any_climate_history_md_ctry = as.numeric(cumsum(climate) > 0)) %>% # identifying whether climate had ever been discussed for a certain country during a given managing director's tenure (continuous)
  mutate(any_climate_history_md_ctry = as.numeric(any_climate_history_md_ctry > 0)) %>% # converting to binary
  ungroup()
md_ctry_mention <- imf_cy_ry %>% # condensing for merging
  dplyr::select(name, ctry, year, any_climate_history_md_ctry)

imf_ry2_history3a2 %>%
  left_join(md_ctry_mention, by = c("ctry", "year")) %>%
  mutate(any_climate_history_md_ctry = replace_na(any_climate_history_md_ctry, 0)) %>%
  filter((name == "Lagarde" & any_climate_history_md_ctry == 0) | name != "Lagarde") %>% # limiting to countries that either had not previously had a climate mention under Lagarde, or country-years not coinciding with Lagarde's directorship
  dplyr::select(any_climate_history_rr,
                meteorological_disaster_cumul_incountry_l1,
                climate_disaster_cumul_incountry_l1,
                polity2,
                gdp_per_capita_constant_2010_us,
                IMF,
                IdealPointDistance,
                rep,
                year,
                country_code) -> lagarde_data
  
lagarde_data %>% 
  felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                    climate_disaster_cumul_incountry_l1) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         IMF + 
         IdealPointDistance | rep + year | 0 | country_code, 
       data = .,
       subset = year >= 2000) -> no_lagarde_interest_reg
stargazer(no_lagarde_interest_reg,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Polity2",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "no_lagarde_interest_reg.tex")

# — Appendix G: Stock of Working Papers and Learning ####

imf_file_list <- list.files("data/IMF Working Papers/")

imf_cc_mention_holder <- tibble(doc = imf_file_list, 
                                year = as.integer(substr(doc, 1, 4)),
                                cc_mention = NA)

skip_with_message = simpleError('Did not work out')
for (i in seq_along(imf_file_list)){
  tryCatch(
    .wp0001 <- pdf_text(paste0("data/IMF Working Papers/", imf_file_list[[i]])),
    error = function(e) skip_with_message
  )
  .wp0001 <- tolower(.wp0001)
  imf_cc_mention_holder$cc_mention[i] <- sum(str_detect(.wp0001, "climate change|global warming")) # loading IMF working papers and counting number of "climate change" or "global warming" mentions

  #print(i) #uncomment for progress indicator
}

imf_cc_mention_holder2 <- imf_cc_mention_holder %>%
  group_by(year) %>%
  summarize(cc_mention2 = sum(cc_mention > 1, na.rm = TRUE)) %>% # counting number of papers with multiple "climate change" or "global warming" mentions
  ungroup() %>%
  mutate(cumulative_cc_count2 = cumsum(cc_mention2)) %>% # cumulative sum of above variable
  dplyr::select(-cc_mention2) %>%
  mutate(year = year + 1) # lagging by one year

imf_ry2_history3_learning %>%
  left_join(imf_cc_mention_holder2, by = "year") %>%
  mutate(cumulative_cc_count2 = replace_na(cumulative_cc_count2, 0)) %>%
  dplyr::select(any_climate_history_rr,
                meteorological_disaster_cumul_incountry_l1,
                climate_disaster_cumul_incountry_l1,
                polity2,
                gdp_per_capita_constant_2010_us,
                IMF,
                IdealPointDistance,
                cumulative_cc_count2,
                rep.x,
                year,
                country_code) -> stock_test_data
  
stock_test_data %>%
  felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                    climate_disaster_cumul_incountry_l1) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         IMF + 
         IdealPointDistance +
         log1p(cumulative_cc_count2) | rep.x | 0 | rep.x + country_code, 
       data = .,
       subset = year >= 2004) -> 
  learning_with_mention_stock # learning model, controlling for natural log of cumulative stock of climate papers

stargazer(learning_with_mention_stock,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Polity2",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance",
                               "Stock of climate working papers (ln)"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "learning_with_mention_stock.tex")

# — Appendix H: Powerful States and Learning ####

voteshare <- read_csv("data/powerasymm.csv") %>% 
  dplyr::select(ccode, year, voteshare) # data on vote share asymmetry comes from Carnegie and Clark 2022 ``Reforming Global Governance''

laws <- read_csv("data/laws_and_policies_23092022.csv") %>% # Climate Change Laws of the World data
  clean_names() %>%
  filter(geography_iso %in% c("USA", "GBR", "FRA", "DEU", "CHN", "JPN")) %>%
  mutate(year = as.integer(substr(events, 7, 10))) %>%
  mutate(document_types %in% c("Act", "Decree",
                               "Decree/Order/Ordinance",
                               "Directive", "Law",
                               "Law;Act", "Policy",
                               "Programme", "Programme;Decree/Order/Ordinance",
                               "Regulation/Rules")) %>%
  group_by(geography_iso, year) %>%
  summarize(count_laws_all = n(),
            count_laws_exec = sum(type == "executive", na.rm = TRUE)) %>%
  ungroup()

laws_all <- expand_grid(geography_iso = c("USA", "GBR", "FRA", "DEU", "CHN", "JPN"),
                        year = 1999:2017) %>%
  left_join(laws, by = c("geography_iso", "year")) %>%
  mutate_at(vars(count_laws_all:count_laws_exec), ~ replace_na(., 0)) %>%
  left_join(voteshare, by = c("geography_iso" = "ccode", "year")) %>%
  group_by(year) %>%
  summarize(n_laws_all = sum(count_laws_all, na.rm = TRUE),
            weightedMean_laws_all = weighted.mean(x = count_laws_all,
                                                  w = voteshare),
            weightedBin_laws_all = weighted.mean(x = as.integer(count_laws_all > 0),
                                                 w = voteshare),
            weightedBin_laws_exec = weighted.mean(x = as.integer(count_laws_exec > 0),
                                                  w = voteshare)) %>%
  ungroup() %>%
  mutate(year = year + 1)

laws_all2 <- laws %>%
  mutate_at(vars(count_laws_all:count_laws_exec), ~ replace_na(., 0)) %>%
  left_join(voteshare, by = c("geography_iso" = "ccode", "year")) %>%
  group_by(year) %>%
  summarize(n_laws_all = sum(count_laws_all, na.rm = TRUE),
            weightedMean_laws_all = weighted.mean(x = count_laws_all,
                                                  w = voteshare),
            weightedBin_laws_all = weighted.mean(x = as.integer(count_laws_all > 0),
                                                 w = voteshare),
            weightedBin_laws_exec = weighted.mean(x = as.integer(count_laws_exec > 0),
                                                  w = voteshare)) %>%
  ungroup() %>%
  mutate(year = year + 1)

imf_ry2_history3_learning %>%
  left_join(laws_all, by = "year") %>%
  felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                    climate_disaster_cumul_incountry_l1) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         IMF + 
         IdealPointDistance +
         n_laws_all | rep.x | 0 | rep.x + country_code, 
       data = .,
       subset = year >= 2000) -> learnpower1 # Table H1, Model 1
imf_ry2_history3_learning %>%
  left_join(laws_all, by = "year") %>%
  felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                    climate_disaster_cumul_incountry_l1) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         IMF + 
         IdealPointDistance +
         weightedMean_laws_all | rep.x | 0 | rep.x + country_code, 
       data = .,
       subset = year >= 2000) -> learnpower2 # Table H1, Model 2
imf_ry2_history3_learning %>%
  left_join(laws_all, by = "year") %>%
  felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                    climate_disaster_cumul_incountry_l1) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         IMF + 
         IdealPointDistance +
         weightedBin_laws_all | rep.x | 0 | rep.x + country_code, 
       data = .,
       subset = year >= 2000) -> learnpower3 # Table H1, Model 3
imf_ry2_history3_learning %>%
  left_join(laws_all, by = "year") %>%
  felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                    climate_disaster_cumul_incountry_l1) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         IMF + 
         IdealPointDistance +
         weightedBin_laws_exec | rep.x | 0 | rep.x + country_code, 
       data = .,
       subset = year >= 2000) -> learnpower4 # Table H1, Model 4

stargazer(learnpower1, 
          learnpower2,
          learnpower3,
          learnpower4,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Polity2",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance",
                               "Climate policies (count)",
                               "Climate policies (weighted mean)",
                               "Climate policies (weighted binary)",
                               "Climate policies, executive only (weighted binary)"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "rr_acclimation_results_powerful_states.tex")

# — Appendix I: Bureaucrat Selection and Learning ####

time_at_imf_data <- imf_ry %>% # bureaucrat career data
  mutate(climate = replace_na(climate, 0)) %>%
  group_by(rep, year) %>%
  summarize(climate = sum(climate, na.rm = TRUE)) %>% # aggregating to bureaucrat-year level
  ungroup() %>%
  arrange(rep, year) %>%
  group_by(rep) %>%
  mutate(first_year = first(year), # identifying each bureaucrat's first year in data
         which_year_at_imf = (year + 1) - first_year) %>% # index of each bureaucrat's time at fund
  ungroup()

time_at_imf_data %>%
  filter(climate > 0 & which_year_at_imf == 1) %>% # limiting to bureaucrat's who discuss climate in first year
  pull(rep) -> rr_who_mention_climate_at_first_appt # just 9 of 73 RRs in data mentioned climate in first year on overseas assignment

felm(any_climate_history_rr ~ I(meteorological_disaster_cumul_incountry_l1+
                                  climate_disaster_cumul_incountry_l1) +
       polity2 + 
       log1p(gdp_per_capita_constant_2010_us) + 
       IMF + 
       IdealPointDistance | rep + year | 0 | rep + country_code, 
     data = imf_ry2_history3a2,
     subset = year >= 2000 & (! rep %in% rr_who_mention_climate_at_first_appt)) -> learning_no_first_mentioners # learning model excluding bureaucrats who mentioned climate in first year on overseas assignment

stargazer(learning_no_first_mentioners,
          dep.var.labels = c("Climate Attuned"),
          float = FALSE,
          covariate.labels = c("Climate-related disasters in current country",
                               "Polity2",
                               "GDP per capita (ln)",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "learning_no_first_mentioners.tex")

# — Appendix J: Mediation Analysis ####

imf_ry_fixed <- imf_ry %>%
  dplyr::select(rep, ctry, year) %>%
  mutate(ctry = case_when(ctry == "Somoa" ~ "Samoa", # fixing misspellings
                          ctry == "Marshal Islands" ~ "Marshall Islands",
                          TRUE ~ ctry)) %>%
  rename(rep_NEW = rep)

imf_cy_ry2_with_all_reps <- imf_cy_ry2 %>%
  left_join(imf_ry_fixed, by = c("ctry", "year")) %>%
  filter(!is.na(rep_NEW))

# replacing NAs with zeroes
imf_cy_ry2_with_all_reps$climate_disaster_cumul_other_l1[is.na(imf_cy_ry2_with_all_reps$climate_disaster_cumul_other_l1)] <- 0
imf_cy_ry2_with_all_reps$meteorological_disaster_cumul_other_l1[is.na(imf_cy_ry2_with_all_reps$meteorological_disaster_cumul_other_l1)] <- 0

# mediation treatment is natural log of cumulative climate-related disasters experienced by bureaucrat in prior country postings
imf_cy_ry2_with_all_reps$med_treatment1 <- log1p(imf_cy_ry2_with_all_reps$climate_disaster_cumul_other_l1+imf_cy_ry2_with_all_reps$meteorological_disaster_cumul_other_l1)

# mediation mediator is binary indicator of whether bureaucrat ever mentioned climate on prior assignment
imf_cy_ry2_with_all_reps$med_mediator1 <- as.integer(imf_cy_ry2_with_all_reps$climate_cumul_other_l1 > 0)

# creating natural log of GDP per capita variable
imf_cy_ry2_with_all_reps$gdp_pc_log <- log1p(imf_cy_ry2_with_all_reps$gdp_per_capita_constant_2010_us)

# creating year fixed effects term
imf_cy_ry2_with_all_reps$year_fe <- as.factor(imf_cy_ry2_with_all_reps$year)

imf_cy_ry2_with_all_reps_med <- imf_cy_ry2_with_all_reps %>%
  dplyr::select(med_mediator1,
                med_treatment1,
                climate,
                polity2,
                gdp_pc_log,
                climate_disaster,
                IMF,
                IdealPointDistance,
                year_fe,
                ctry)
imf_cy_ry2_with_all_reps_med <- imf_cy_ry2_with_all_reps_med[complete.cases(imf_cy_ry2_with_all_reps_med), ]
imf_cy_ry2_with_all_reps_med$ctry_fe <- as.factor(as.character(imf_cy_ry2_with_all_reps_med$ctry))
# fixing fixed effect factors
levels(imf_cy_ry2_with_all_reps_med$ctry_fe) <- droplevels(imf_cy_ry2_with_all_reps_med$ctry_fe)
levels(imf_cy_ry2_with_all_reps_med$year_fe) <- droplevels(imf_cy_ry2_with_all_reps_med$year_fe)

med.fit01 <- lm(med_mediator1 ~ med_treatment1 +
                  polity2 + 
                  gdp_pc_log + 
                  climate_disaster +
                  IMF + 
                  IdealPointDistance +
                  ctry_fe + year_fe
                ,
                data = imf_cy_ry2_with_all_reps_med,
                na.action = na.omit)
out.fit01 <- lm(climate ~ med_mediator1 + med_treatment1 +
                  polity2 + 
                  gdp_pc_log + 
                  climate_disaster +
                  IMF + 
                  IdealPointDistance +
                  ctry_fe + year_fe
                ,
                data = imf_cy_ry2_with_all_reps_med,
                na.action = na.omit)
set.seed(211)

med.out01 <- mediation::mediate(med.fit01, out.fit01, 
                                treat = "med_treatment1",
                                control.value = 0,
                                mediator = "med_mediator1",
                                robustSE = TRUE,
                                sims = 100)
summary(med.out01)

# — Appendix K: Poisson and Logit Specifications: Spread of Climate Concerns ####

glm01 <- glm(climate ~ I(climate_cumul_other_l1 > 0) + as.factor(country_code),
             data = imf_cy_ry,
             family = poisson(link = "log"),
             control=glm.control(maxit=20))
glm01_vcovform <- multiwayvcov::cluster.vcov(glm01, ~ country_code)
(glm01_coef <- coeftest(glm01, glm01_vcovform)) # poisson no controls

glm02 <- glm(climate ~ I(climate_cumul_other_l1 > 0) +
               polity2 + 
               log1p(gdp_per_capita_constant_2010_us) + 
               I(climate_disaster+meteorological_disaster) + 
               IMF + 
               IdealPointDistance 
             + as.factor(country_code),
             data = imf_cy_ry,
             family = poisson(link = "log"),
             control=glm.control(maxit=20))
glm02_vcovform <- multiwayvcov::cluster.vcov(glm02, ~ country_code)
(glm02_coef <- coeftest(glm02, glm02_vcovform)) # poisson with controls

logit01 <- glm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
                 as.factor(country_code),
               data = imf_cy_ry,
               family = "binomial")
logit01_vcovform <- multiwayvcov::cluster.vcov(logit01, ~ country_code)
(logit01_coef <- coeftest(logit01, logit01_vcovform)) # logit no controls

logit02 <- glm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
                 polity2 + 
                 log1p(gdp_per_capita_constant_2010_us) +  
                 I(climate_disaster+meteorological_disaster) + 
                 IMF +
                 IdealPointDistance + as.factor(country_code),
               data = imf_cy_ry,
               family = "binomial")
logit02_vcovform <- multiwayvcov::cluster.vcov(logit02, ~ country_code)
(logit02_coef <- coeftest(logit02, logit02_vcovform)) # logit all covs

stargazer(glm01,
          glm02,
          dep.var.labels = c("\\# Climate Mentions"),
          omit = "as.factor",
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions",
                               "Polity2",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f", "aic", "ll", "theta"),
          no.space = TRUE,
          out = "baseline_results_glm_jan2022.tex")

stargazer(logit01,
          logit02,
          dep.var.labels = c("Any Climate Mention"),
          omit = "as.factor",
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions",
                               "Polity2",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f", "aic", "ll", "theta"),
          no.space = TRUE,
          out = "baseline_results_logit_jan2022.tex")

# — Appendix L: Managing Directors and Spread of Climate Concerns ####

imf_cy_ry %<>%
  mutate(md_ctry_fe = paste0(name, "-", country_code)) # combining managing director and country terms for fixed effects factor

felm(climate ~ I(climate_cumul_other_l1 > 0) +
       polity2 + 
       log1p(gdp_per_capita_constant_2010_us) + 
       I(climate_disaster+meteorological_disaster) +
       IMF + 
       IdealPointDistance | md_ctry_fe | 0 | country_code,
     data = imf_cy_ry) -> diffusion_md_ctry_fe1 # Table L1, Model 1
felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
       polity2 + 
       log1p(gdp_per_capita_constant_2010_us) + 
       I(climate_disaster+meteorological_disaster) +
       IMF + 
       IdealPointDistance | md_ctry_fe | 0 | country_code,
     data = imf_cy_ry) -> diffusion_md_ctry_fe2 # Table L1, Model 2

stargazer(diffusion_md_ctry_fe1,
          diffusion_md_ctry_fe2,
          dep.var.labels = c("\\# Climate Mentions",
                             "Any Climate Mention"),
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions",
                               "Polity2",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "diffusion_md_ctry_fe.tex")

# — Appendix M: Lagged DV and Spread of Climate Concerns ####

count_ols02b_bin1 <- felm(climate ~ I(climate_cumul_other_l1 > 0) +
                            climate_lagged_dv + # adding lagged dependent variable
                            polity2 + 
                            log1p(gdp_per_capita_constant_2010_us) + 
                            I(climate_disaster+meteorological_disaster) +
                            IMF + 
                            IdealPointDistance | country_code + year | 0 | country_code,
                          data = imf_cy_ry) # Table M, Model 1
tidy(count_ols02b_bin1)

binary_ols02b_bin2 <- felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
                             I(climate_lagged_dv > 0) + # adding lagged dependent variable
                             polity2 + 
                             log1p(gdp_per_capita_constant_2010_us) + 
                             I(climate_disaster+meteorological_disaster) +
                             
                             IMF + 
                             IdealPointDistance | country_code + year | 0 | country_code,
                           data = imf_cy_ry) # Table M, Model 2
tidy(binary_ols02b_bin2)

# — Appendix N: Stock of Working Papers and Spread of Climate Concerns ####

imf_cy_ry %>%
  left_join(imf_cc_mention_holder2, by = c("year")) %>%
  mutate(cumulative_cc_count2 = replace_na(cumulative_cc_count2, 0)) %>% # cumulative stock of papers discussing climate multiple times
  felm(climate ~ I(climate_cumul_other_l1 > 0) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         I(climate_disaster+meteorological_disaster) +
         IMF + 
         IdealPointDistance +
         log1p(cumulative_cc_count2) | ctry | 0 | country_code,
       data = .) -> diffusion_mention_stock # Table N, Model 1
imf_cy_ry %>%
  left_join(imf_cc_mention_holder2, by = c("year")) %>%
  mutate(cumulative_cc_count2 = replace_na(cumulative_cc_count2, 0)) %>% # cumulative stock of papers discussing climate multiple times
  felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         I(climate_disaster+meteorological_disaster) +
         IMF + 
         IdealPointDistance +
         log1p(cumulative_cc_count2) | ctry | 0 | country_code,
       data = .) -> diffusion_mention_stock2 # Table N, Model 2

stargazer(diffusion_mention_stock,
          diffusion_mention_stock2,
          dep.var.labels = c("\\# Climate Mentions",
                             "Any Climate Mention"),
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions",
                               "Polity2",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance",
                               "Stock of climate working papers (ln)"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "diffusion_with_mention_stock.tex")

# — Appendix O: Powerful States and Spread of Climate Concerns ####

imf_cy_ry %>%
  left_join(laws_all, by = "year") %>%
  felm(climate ~ I(climate_cumul_other_l1 > 0) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         I(climate_disaster+meteorological_disaster) + 
         IMF + 
         IdealPointDistance + 
         n_laws_all
       | country_code | 0 | country_code,
       data = .) -> difCountpow1 # Table O1, Model 1
imf_cy_ry %>%
  left_join(laws_all, by = "year") %>%
  felm(climate ~ I(climate_cumul_other_l1 > 0) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         I(climate_disaster+meteorological_disaster) + 
         IMF + 
         IdealPointDistance + 
         weightedMean_laws_all
       | country_code | 0 | country_code,
       data = .) -> difCountpow2 # Table O1, Model 2
imf_cy_ry %>%
  left_join(laws_all, by = "year") %>%
  felm(climate ~ I(climate_cumul_other_l1 > 0) +
         polity2 +
         log1p(gdp_per_capita_constant_2010_us) +  
         I(climate_disaster+meteorological_disaster) + 
         IMF + 
         IdealPointDistance + 
         weightedBin_laws_all
       | country_code | 0 | country_code,
       data = .) -> difCountpow3 # Table O1, Model 3
imf_cy_ry %>%
  left_join(laws_all, by = "year") %>%
  felm(climate ~ I(climate_cumul_other_l1 > 0) +
         polity2 +
         log1p(gdp_per_capita_constant_2010_us) +  
         I(climate_disaster+meteorological_disaster) + 
         IMF + 
         IdealPointDistance + 
         weightedBin_laws_exec
       | country_code | 0 | country_code,
       data = .) -> difCountpow4 # Table O1, Model 4

stargazer(difCountpow1, 
          difCountpow2,
          difCountpow3,
          difCountpow4,
          dep.var.labels = c("\\# Climate Mentions"),
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions",
                               "Polity2",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance",
                               "Climate policies (count)",
                               "Climate policies (weighted mean)",
                               "Climate policies (weighted binary)",
                               "Climate policies, executive only (weighted binary)"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "diffusion_count_powerful_states.tex")

imf_cy_ry %>%
  left_join(laws_all, by = "year") %>%
  felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         I(climate_disaster+meteorological_disaster) + 
         IMF + 
         IdealPointDistance + 
         n_laws_all
       | country_code | 0 | country_code,
       data = .) -> difBinpow1 # Table O2, Model 1
imf_cy_ry %>%
  left_join(laws_all, by = "year") %>%
  felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         I(climate_disaster+meteorological_disaster) + 
         IMF + 
         IdealPointDistance + 
         weightedMean_laws_all
       | country_code | 0 | country_code,
       data = .) -> difBinpow2 # Table O2, Model 2
imf_cy_ry %>%
  left_join(laws_all, by = "year") %>%
  felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         I(climate_disaster+meteorological_disaster) + 
         IMF + 
         IdealPointDistance + 
         weightedBin_laws_all
       | country_code | 0 | country_code,
       data = .) -> difBinpow3 # Table O2, Model 3
imf_cy_ry %>%
  left_join(laws_all, by = "year") %>%
  felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
         polity2 + 
         log1p(gdp_per_capita_constant_2010_us) + 
         I(climate_disaster+meteorological_disaster) + 
         IMF + 
         IdealPointDistance + 
         weightedBin_laws_exec
       | country_code | 0 | country_code,
       data = .) -> difBinpow4 # Table O2, Model 4

stargazer(difBinpow1, 
          difBinpow2,
          difBinpow3,
          difBinpow4,
          dep.var.labels = c("Any Climate Mention"),
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions",
                               "Polity2",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance",
                               "Climate policies (count)",
                               "Climate policies (weighted mean)",
                               "Climate policies (weighted binary)",
                               "Climate policies, executive only (weighted binary)"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "diffusion_bin_powerful_states.tex")

# — Appendix P: Bureaucrat Selection and Spread of Climate Concerns ####

imf_ry_fixed <- imf_ry %>%
  dplyr::select(rep, ctry, year) %>%
  mutate(ctry = case_when(ctry == "Somoa" ~ "Samoa", # fixing misspellings
                          ctry == "Marshal Islands" ~ "Marshall Islands",
                          TRUE ~ ctry)) %>%
  rename(rep_NEW = rep)

imf_cy_ry_with_reps <- imf_cy_ry %>%
  left_join(imf_ry_fixed, by = c("ctry", "year"))

felm(climate ~ I(climate_cumul_other_l1 > 0) +
       polity2 + 
       log1p(gdp_per_capita_constant_2010_us) + 
       I(climate_disaster+meteorological_disaster) +
       IMF + 
       IdealPointDistance | country_code + year | 0 | country_code,
     data = imf_cy_ry_with_reps,
     subset = ! rep_NEW %in% rr_who_mention_climate_at_first_appt) -> diffusion_no_first_mentioners1 # test of spread, excluding bureaucrats who mentioned climate in first year on overseas assignment
felm(I(climate > 0) ~ I(climate_cumul_other_l1 > 0) +
       polity2 + 
       log1p(gdp_per_capita_constant_2010_us) + 
       I(climate_disaster+meteorological_disaster) +
       IMF + 
       IdealPointDistance | country_code + year | 0 | country_code,
     data = imf_cy_ry_with_reps,
     subset = ! rep_NEW %in% rr_who_mention_climate_at_first_appt) -> diffusion_no_first_mentioners2 # test of spread, excluding bureaucrats who mentioned climate in first year on overseas assignment

stargazer(diffusion_no_first_mentioners1,
          diffusion_no_first_mentioners2,
          dep.var.labels = c("\\# Climate Mentions",
                             "Any Climate Mention"),
          float = FALSE,
          covariate.labels = c("Prior bureaucrat climate mentions",
                               "Polity2",
                               "GDP per capita (ln)",
                               "Climate-related disaster",
                               "In IMF program",
                               "UN ideal point distance"),
          style = "ajps",
          omit.stat = c("rsq", "ser", "f"),
          no.space = TRUE,
          out = "diffusion_no_first_mentioners.tex")
