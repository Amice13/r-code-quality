
# Set up ----------------
# Checks if package is installed, installs if necessary, and loads package for current session

pacman::p_load(
  tidyverse,      # data management and visualization  
  mapview,        # to create test map of latitutde/longitude data
  writexl)        # to write excel versions of data

# this script was last run on R version 4.1.2 (2021-11-01), 
# tidyverse 1.3.2, mapview 2.11.0, and writexl 1.4.0
version
packageVersion("tidyverse")
packageVersion("mapview")
packageVersion("writexl")

# If you are having trouble running this file on more recent versions or R or packages, 
# consider using renv or the academic project Rocker (https://rocker-project.org/images/), 
# which offers Docker containers of version-controlled R environments. See discussion 
# of this in the preprint "The Rockerverse: Packages and Applications for 
# Containerization with R", https://doi.org/10.48550/arXiv.2001.10641

# Import Data ------------
# Data is updated directly in Drupal every time a class is taught. After the class is 
# finished, data is exported as csvs from SQL Views in the Drupal database Admin
# interface.

# theaters data view has
theaters <- read_csv("data/theaters_2022-08-24.csv", 
                                col_types = cols(id = col_integer(), 
                                                 start_date_of_operation = col_date(format = "%m/%d/%Y"), 
                                                 start_year = col_integer(), 
                                                 end_date_of_operation = col_date(format = "%m/%d/%Y"), 
                                                 end_year = col_integer()))

theaters_html <- read_csv("data/theaters_plus_html_2022-08-22.csv", 
                                          col_types = cols(id = col_integer(), 
                                                           start_date_of_operation = col_date(format = "%m/%d/%Y"), 
                                                           start_year = col_integer(), 
                                                           end_date_of_operation = col_date(format = "%m/%d/%Y"), 
                                                           end_year = col_integer()))

owners <- read_csv("data/owners_2022-08-22.csv",
                   col_types = cols(id = col_integer()))

articles <- read_csv("data/articles_2022-08-22.csv", 
                     col_types = cols(id = col_integer()))

articles_html <- read_csv("data/articles_html_2022-08-22.csv", 
                          col_types = cols(id = col_integer()))

# Test Data Quality ----

# print variable names to confirm and/or add to README file
print(paste(names(theaters), collapse = ", "))
print(paste(names(theaters_html), collapse = ", "))
print(paste(names(articles), collapse = ", "))
print(paste(names(articles_html), collapse = ", "))
print(paste(names(owners), collapse = ", "))

# Confirm unique id in theaters data sets
dim(theaters)
dim(theaters)[1] == length(unique(theaters$id)) # should return TRUE

dim(theaters_html)
dim(theaters_html)[1] == length(unique(theaters_html$id)) # should return TRUE

# confirm that theaters and theaters_html have the same number of columns
dim(theaters)[1] == dim(theaters_html)[1] # should return TRUE

dim(articles)
dim(articles_html)
dim(articles)[1] == dim(articles_html)[1] # should return TRUE

dim(owners)

# Tidy Data ------------

# remove extra white space and special characters from some columns
theaters <- theaters %>% 
  mutate(across(c(body, additional_facts, works_cited), str_squish))

articles <-  articles %>% 
  mutate(across(c(body), str_squish))

# commenting this below where I removed spaces and special characters from all columns 
# because it converts to array and messes with my data types
# see possible alternative at https://stackoverflow.com/questions/18503177/r-apply-function-on-specific-dataframe-columns
# owners <- as_tibble(apply(owners, 2, str_squish))
# articles <- as_tibble(apply(articles, 2, str_squish))

# separate city and state
theaters <- separate(data = theaters,
                     col = city_state, 
                     into = c("city", "state"), 
                     sep = ", ",
                     remove = FALSE) 

# separate location into lat and long 
theaters <- separate(data = theaters, 
                     col = location, 
                     into = c("latitude", "longitude"), 
                     sep =", ",
                     convert = TRUE)

# Clean owners data
# note that owners were not all entered in a format that separates owners or managers when a 
# theater has more than one name
owners_names = c("id", "theater_name", "city_state", "owner_and_manager_names")

names(owners) <- owners_names
rm(owners_names)

owners <- owners %>% 
  filter(!(is.na(city_state)))

# Test GIS Data Quality ---------
# test lat/long using mapview
# create a second dataframe with no blank lat/longs to avoid errors in mapping the data
theaters_for_gis_test <- theaters %>% filter(!(is.na(latitude)))

# mapview basic view of theaters
mapview(theaters_for_gis_test, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
rm(theaters_for_gis_test)

# Join HTML data into larger data sets -----

theaters_join <- left_join(theaters,
                           theaters_html,
                           by = "id",
                           copy = FALSE,
                           suffix = c("", "_html"),
                           keep = FALSE)

theaters <- theaters_join[, (c(colnames(theaters), 
                               "body_html", 
                               "additional_facts_html", 
                               "works_cited_html"))]

rm(theaters_join, theaters_html)

articles_join <- left_join(articles,
                           articles_html,
                           by = "id",
                           copy = FALSE,
                           suffix = c("", "_html"),
                           keep = FALSE)

articles <- articles_join[, (c(colnames(articles), 
                               "body_html"))]

rm(articles_join, articles_html)


# cells in Excel can't have more than 32,000 characters. Because some of the html columns
# have cells over this limit, we include a version of the data set for excel users that 
# removes the body_html column
articles_excel <- articles %>% 
  mutate(body_html_length = nchar(body_html)) %>% 
  select(-body_html)

articles_excel$body_html_length > 32000 

theaters_excel <- theaters %>% 
  mutate(body_html_length = nchar(body_html)) %>% 
  select(-body_html)

theaters_excel$body_html_length > 32000

# Export to xlsx for Excel users-----
write_xlsx(theaters_excel, "results/theaters_excel_202210.xlsx")

write_xlsx(articles_excel, "results/articles_excel_202210.xlsx")

write_xlsx(owners, "results/owners_excel_202210.xlsx")

# Export to csv for other users

write_csv(theaters, "results/theaters_202210.csv")

write_csv(articles, "results/articles_202210.csv")

write_csv(owners, "results/owners_202210.csv")
