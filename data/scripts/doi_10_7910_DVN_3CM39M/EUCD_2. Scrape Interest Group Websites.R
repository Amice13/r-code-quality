# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readxl, rvest, XML, dplyr, stringr, tidyr, R.utils, crayon, padr, pdftools, writexl, cowplot, quanteda, readtext, 
       bit64, igraph, ggraph, rtweet)

# The goal of this script is to scrape and download statements from the websites of interest groups interested in EU Copyright legislation.
# Those interest groups were identified in the previous script `1. Identify IGs and scrape IG contact info and inside lobbying data`.

# I use the urls from the interest group websites (mainpages) to build a custom google search and query that search with an API.
# I search for subpages and documents referring to EU copyright. After identifying the links to these subpages I scrape the html 
# contents of the links and/or download the linked files. If the link neither refers to html or a file, I copy and paste the text 
# from the link to a txt file. Finally, some webpages from a manual search are added.

################## 1. Retrieve the links to subpages referring to EU copyright from IG mainpages ###################
##### a) Build functions to query the Google Custom Search API #####

# Identify with API and search engine id
google.key = "AIzaSyD1XwCviC7oTGf4vZJ16ampfukljmF-HYw" #API key
google.cx = "4fc774220f724725e" #search engine id

# define a function to query the custom search engine
Google.Search.API <- function(keyword, google.key, google.cx, country = "de", start_no = 1, daterange) # function to query and parse api
{
  url <- paste0("https://www.googleapis.com/customsearch/v1?"
                , "key=", google.key
                , "&q=", gsub(" ", "+", keyword)
                , "&gl=", country         # Country
                , "&hl=en"                # Language from Browser, english
                , "&cx=", google.cx
                , "&start=", as.character(start_no)
                , "&sort=date:r:", daterange
                #, "&sort=date:", order
                #, "&dateRestrict=", dates
                , "&fields=items()"
  )
  
  d2 <- url %>%
    httr::GET(ssl.verifypeer=TRUE) %>%
    httr::content(.) %>% .[["items"]] %>%
    data.table::rbindlist(., fill = T) %>%
    mutate(keyword, SERP = row_number(), search.engine = "Google API")
  
  pause <- round(runif(1, min = 1.1, max = 5), 1)
  if(nrow(d2) == 0)
  {cat("\nPausing", pause, "seconds. Failed for:", keyword)} else
  {cat("\nPausing", pause, "seconds. Successful for:", keyword)}
  
  Sys.sleep(pause)
  rm(keyword, country, pause, url, google.key, google.cx)
  return(d2)
}


# define a function to feed the `Google.Search.API` function with a query that can be limited to a specific website and daterange
# the custom search api does only return results from the first ten pages. 
# It is therefore necessary to limit the date range insofar that the results do fill less than ten pages.

search_IG_websites = function(search_term, date_range, site) {
  
  query = paste0(search_term, " site:", site)
  
  urls = vector(mode = "list", length = 0) # initiate an empty list
  
  # Loop over up to ten pages for all results before January 01, 2014.
  for (i in 0:10){
    if(i == 0){
      urls[[i+1]] = Google.Search.API(keyword = query, google.key, google.cx, country = "de", start_no = i, daterange = date_range)
    } else if(i > 0 & i <=9) {
      if(nrow(urls[[i]]) != 0)
      {
        urls[[i+1]] = Google.Search.API(keyword = query, google.key, google.cx, country = "de", start_no = i*10+1, daterange = date_range)
      } else {
        break
      }
    }
    else {
      cat(green("\n Ran fully through"))
    }
  }
  if(nrow(urls[[1]]) == 0) {
    cat(cyan(paste0("no result for: ", site)))
  }
  return(urls)
}

# define a function to process the search results
# the function returns a dataframe including the transparency register id, title, snippet, source url, creation date, and publishing date, etc.
process_search = function(urls){
  
  urls = urls %>% 
    rename(source = link) %>%
    select(regid, title, snippet, htmlSnippet, search.engine, keyword, SERP, source, pagemap)
  
  article_pub_date = vector(nrow(urls), mode = "list")
  creationdate = vector(nrow(urls), mode = "list")
  
  for (i in 1:nrow(urls)){
    if (length(urls$pagemap[[i]][[1]]$`article:published_time`) > 0){
      article_pub_date[[i]] = urls$pagemap[[i]][[1]]$`article:published_time`
    }
    else{
      article_pub_date[[i]] = NA
    }
  }
  
  for (i in 1:nrow(urls)){
    if (length(urls$pagemap[[i]][[1]]$creationdate) > 0){
      creationdate[[i]] = creationdate[[i]] = urls$pagemap[[i]][[1]]$creationdate
    }
    else{
      creationdate[[i]] = NA
    }
  }
  
  urls$article_pub_date = unlist(article_pub_date)
  urls$creationdate = unlist(creationdate)
  
  urls_2 = select(urls, -c(pagemap, creationdate, article_pub_date, SERP))
  urls_2 = unique(urls_2)
  
  urls_date = subset(urls, is.na(urls$creationdate) == F | is.na(urls$article_pub_date) == F)
  urls_date = select(urls_date, c(source, creationdate, article_pub_date))
  
  urls_final = left_join(urls_2, urls_date, by = "source")
  
  return(urls_final)
}

##### b) apply the functions to query the API and retrieve links to subpages #####

# read in the dataframe cotaining the urls to interest group websites and other information on interest groups involved in copyright legislation
ig_info_full = read_xlsx("EU Copyright_IGs_Klassifizierung.xlsx")
ig_info = subset(ig_info_full, is.na(ig_info_full$website) == F)

# Apply the functions and query websites for each of the following languages and for each year between 2014 and 2019
###### i) English ######
# 2014

# initiate empty lists to store results
ig_website_14 = vector(mode = "list", length = 0)
ig_id_14 = vector(mode = "list", length = 0)

# loop over all IG urls for site restricted search
for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '(copyright OR "intellectual property") AROUND(15) (EU OR Europe OR directive OR reform)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}

# create empty list
ig_web_id_14 = vector(mode = "list", length = 0)

# append interest group registration id to the search results
for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

# bind everything into a dataframe
ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '(copyright OR "intellectual property") AROUND(15) (EU OR Europe OR directive OR reform)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '(copyright OR "intellectual property") AROUND(15) (EU OR Europe OR directive OR reform)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '(copyright OR "intellectual property") AROUND(15) (EU OR Europe OR directive OR reform)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '(copyright OR "intellectual property") AROUND(15) (EU OR Europe OR directive OR reform)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '(copyright OR "intellectual property") AROUND(15) (EU OR Europe OR directive OR reform)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites

ig_web_id_en = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_en = process_search(ig_web_id_en)

write.csv(ig_web_id_en, "ig_web_id_en.csv")

###### ii) German ###### 
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '((urheberrechtsreform OR urheberrechtsrichtlinie) AROUND(15) (EU OR Europa)) OR ((urheberrecht OR "geistiges eigentum") AROUND(15) (EU OR Europa OR reform OR richtlinie OR direktive))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '((urheberrechtsreform OR urheberrechtsrichtlinie) AROUND(15) (EU OR Europa)) OR ((urheberrecht OR "geistiges eigentum") AROUND(15) (EU OR Europa OR reform OR richtlinie OR direktive))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '((urheberrechtsreform OR urheberrechtsrichtlinie) AROUND(15) (EU OR Europa)) OR ((urheberrecht OR "geistiges eigentum") AROUND(15) (EU OR Europa OR reform OR richtlinie OR direktive))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '((urheberrechtsreform OR urheberrechtsrichtlinie) AROUND(15) (EU OR Europa)) OR ((urheberrecht OR "geistiges eigentum") AROUND(15) (EU OR Europa OR reform OR richtlinie OR direktive))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '((urheberrechtsreform OR urheberrechtsrichtlinie) AROUND(15) (EU OR Europa)) OR ((urheberrecht OR "geistiges eigentum") AROUND(15) (EU OR Europa OR reform OR richtlinie OR direktive))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '((urheberrechtsreform OR urheberrechtsrichtlinie) AROUND(15) (EU OR Europa)) OR ((urheberrecht OR "geistiges eigentum") AROUND(15) (EU OR Europa OR reform OR richtlinie OR direktive))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites

ig_web_id_de = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_de = process_search(ig_web_id_de)

write.csv(ig_web_id_de, "ig_web_id_de.csv")

###### iii) Spanish ###### 
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '("derechos de autor" OR "propiedad intelectual") AROUND(15) (directiva OR reform OR europa or UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '("derechos de autor" OR "propiedad intelectual") AROUND(15) (directiva OR reform OR europa or UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '("derechos de autor" OR "propiedad intelectual") AROUND(15) (directiva OR reform OR europa or UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '("derechos de autor" OR "propiedad intelectual") AROUND(15) (directiva OR reform OR europa or UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '("derechos de autor" OR "propiedad intelectual") AROUND(15) (directiva OR reform OR europa or UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '("derechos de autor" OR "propiedad intelectual") AROUND(15) (directiva OR reform OR europa or UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites

ig_web_id_es = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_es = process_search(ig_web_id_es)

write.csv(ig_web_id_es, "ig_web_id_es.csv")

###### iv) French ###### 
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '("droits d\'auteur" OR "propri?t? intellectuelle") AROUND(15) (directif OR r?forme OR europe OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '("droits d\'auteur" OR "propri?t? intellectuelle") AROUND(15) (directif OR r?forme OR europe OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '("droits d\'auteur" OR "propri?t? intellectuelle") AROUND(15) (directif OR r?forme OR europe OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '("droits d\'auteur" OR "propri?t? intellectuelle") AROUND(15) (directif OR r?forme OR europe OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '("droits d\'auteur" OR "propri?t? intellectuelle") AROUND(15) (directif OR r?forme OR europe OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '("droits d\'auteur" OR "propri?t? intellectuelle") AROUND(15) (directif OR r?forme OR europe OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites

ig_web_id_fr = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_fr = process_search(ig_web_id_fr)

write.csv(ig_web_id_fr, "ig_web_id_fr.csv")

###### v) Italian ######
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '("diritto d\'autore" OR "propriet? intellettuale") AROUND(15) (direttiva OR riforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '("diritto d\'autore" OR "propriet? intellettuale") AROUND(15) (direttiva OR riforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '("diritto d\'autore" OR "propriet? intellettuale") AROUND(15) (direttiva OR riforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '("diritto d\'autore" OR "propriet? intellettuale") AROUND(15) (direttiva OR riforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '("diritto d\'autore" OR "propriet? intellettuale") AROUND(15) (direttiva OR riforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '("diritto d\'autore" OR "propriet? intellettuale") AROUND(15) (direttiva OR riforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites

ig_web_id_it = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_it = process_search(ig_web_id_it)

write.csv(ig_web_id_it, "ig_web_id_it.csv")

###### vi) Dutch ######
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '(auteursrecht OR "intellectueel eigendom") AROUND(15) (hervormen OR richtlijn OR EU OR europa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '(auteursrecht OR "intellectueel eigendom") AROUND(15) (hervormen OR richtlijn OR EU OR europa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '(auteursrecht OR "intellectueel eigendom") AROUND(15) (hervormen OR richtlijn OR EU OR europa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '(auteursrecht OR "intellectueel eigendom") AROUND(15) (hervormen OR richtlijn OR EU OR europa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '(auteursrecht OR "intellectueel eigendom") AROUND(15) (hervormen OR richtlijn OR EU OR europa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '(auteursrecht OR "intellectueel eigendom") AROUND(15) (hervormen OR richtlijn OR EU OR europa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites
ig_web_id_nl = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_nl = process_search(ig_web_id_nl)

write.csv(ig_web_id_nl, "ig_web_id_nl.csv")

###### vii) Portuguese ######
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '("direito autoral" OR "propriedade intelectual") AROUND(15) (diretiva OR reforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '("direito autoral" OR "propriedade intelectual") AROUND(15) (diretiva OR reforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '("direito autoral" OR "propriedade intelectual") AROUND(15) (diretiva OR reforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '("direito autoral" OR "propriedade intelectual") AROUND(15) (diretiva OR reforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '("direito autoral" OR "propriedade intelectual") AROUND(15) (diretiva OR reforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '("direito autoral" OR "propriedade intelectual") AROUND(15) (diretiva OR reforma OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites
ig_web_id_pt = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_pt = process_search(ig_web_id_pt)

write.csv(ig_web_id_pt, "ig_web_id_pt.csv")


###### viii) Polish ###### 
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '("prawo autorskie" OR "wlasnosc intelektualna") AROUND(15) (reforma OR dyrektywa OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '("prawo autorskie" OR "wlasnosc intelektualna") AROUND(15) (reforma OR dyrektywa OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '("prawo autorskie" OR "wlasnosc intelektualna") AROUND(15) (reforma OR dyrektywa OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '("prawo autorskie" OR "wlasnosc intelektualna") AROUND(15) (reforma OR dyrektywa OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '("prawo autorskie" OR "wlasnosc intelektualna") AROUND(15) (reforma OR dyrektywa OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '("prawo autorskie" OR "wlasnosc intelektualna") AROUND(15) (reforma OR dyrektywa OR europa OR UE)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites
ig_web_id_pl = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_pl = process_search(ig_web_id_pl)

write.csv(ig_web_id_pl, "ig_web_id_pl.csv")

###### ix) Swedish ######
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '(Upphovsr?ttsdirektiv AROUND(15) (EU OR europa)) OR ((upphovsr?tt OR immaterialr?tt) AROUND(15) (reform OR direktiv OR EU OR europa))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '(Upphovsr?ttsdirektiv AROUND(15) (EU OR europa)) OR ((upphovsr?tt OR immaterialr?tt) AROUND(15) (reform OR direktiv OR EU OR europa))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '(Upphovsr?ttsdirektiv AROUND(15) (EU OR europa)) OR ((upphovsr?tt OR immaterialr?tt) AROUND(15) (reform OR direktiv OR EU OR europa))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '(Upphovsr?ttsdirektiv AROUND(15) (EU OR europa)) OR ((upphovsr?tt OR immaterialr?tt) AROUND(15) (reform OR direktiv OR EU OR europa))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '(Upphovsr?ttsdirektiv AROUND(15) (EU OR europa)) OR ((upphovsr?tt OR immaterialr?tt) AROUND(15) (reform OR direktiv OR EU OR europa))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '(Upphovsr?ttsdirektiv AROUND(15) (EU OR europa)) OR ((upphovsr?tt OR immaterialr?tt) AROUND(15) (reform OR direktiv OR EU OR europa))', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites
ig_web_id_sw = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_sw = process_search(ig_web_id_sw)

write.csv(ig_web_id_sw, "ig_web_id_sw.csv")

###### x) Magyar (Hungary) ###### 
# 2014
ig_website_14 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_14[[i]] = search_IG_websites(search_term = '("szerzoij jog" OR "szellemi tulajdon") AROUND(15) (ir?nyelv OR reform OR EU OR eur?pa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20140101:20150101")
  ig_id_14[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_14 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_14[[i]][[1]]) > 0){
    ig_website_14[[i]][[1]]$regid = ig_id_14[[i]]
    ig_web_id_14[[i]] = ig_website_14[[i]][[1]]
  }
}

ig_web_id_14 = bind_rows(ig_web_id_14)


# 2015
ig_website_15 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_15[[i]] = search_IG_websites(search_term = '("szerzoij jog" OR "szellemi tulajdon") AROUND(15) (ir?nyelv OR reform OR EU OR eur?pa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20150101:20160101")
  ig_id_15[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_15 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_15[[i]][[1]]) > 0){
    ig_website_15[[i]][[1]]$regid = ig_id_15[[i]]
    ig_web_id_15[[i]] = ig_website_15[[i]][[1]]
  }
}

ig_web_id_15 = bind_rows(ig_web_id_15)

# 2016
ig_website_16 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_16[[i]] = search_IG_websites(search_term = '("szerzoij jog" OR "szellemi tulajdon") AROUND(15) (ir?nyelv OR reform OR EU OR eur?pa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20160101:20170101")
  ig_id_16[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_16 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_16[[i]][[1]]) > 0){
    ig_website_16[[i]][[1]]$regid = ig_id_16[[i]]
    ig_web_id_16[[i]] = ig_website_16[[i]][[1]]
  }
}

ig_web_id_16 = bind_rows(ig_web_id_16)

# 2017
ig_website_17 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_17[[i]] = search_IG_websites(search_term = '("szerzoij jog" OR "szellemi tulajdon") AROUND(15) (ir?nyelv OR reform OR EU OR eur?pa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20170101:20180101")
  ig_id_17[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_17 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_17[[i]][[1]]) > 0){
    ig_website_17[[i]][[1]]$regid = ig_id_17[[i]]
    ig_web_id_17[[i]] = ig_website_17[[i]][[1]]
  }
}

ig_web_id_17 = bind_rows(ig_web_id_17)

# 2018
ig_website_18 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_18[[i]] = search_IG_websites(search_term = '("szerzoij jog" OR "szellemi tulajdon") AROUND(15) (ir?nyelv OR reform OR EU OR eur?pa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20180101:20190101")
  ig_id_18[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_18 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_18[[i]][[1]]) > 0){
    ig_website_18[[i]][[1]]$regid = ig_id_18[[i]]
    ig_web_id_18[[i]] = ig_website_18[[i]][[1]]
  }
}

ig_web_id_18 = bind_rows(ig_web_id_18)

#2019
ig_website_19 = vector(mode = "list", length = 0) # initiate an empty list
ig_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)) {
  ig_website_19[[i]] = search_IG_websites(search_term = '("szerzoij jog" OR "szellemi tulajdon") AROUND(15) (ir?nyelv OR reform OR EU OR eur?pa)', 
                                          site = ig_info$website[[i]], 
                                          date_range = "20190101:20200101")
  ig_id_19[[i]] = ig_info$regid[[i]]
  print(paste0("page: ", i))
}


ig_web_id_19 = vector(mode = "list", length = 0)

for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_19[[i]][[1]]) > 0){
    ig_website_19[[i]][[1]]$regid = ig_id_19[[i]]
    ig_web_id_19[[i]] = ig_website_19[[i]][[1]]
  }
}

ig_web_id_19 = bind_rows(ig_web_id_19)

# Process websites
ig_web_id_mg = rbind(ig_web_id_14, ig_web_id_15, ig_web_id_16, ig_web_id_17, ig_web_id_18, ig_web_id_19)

ig_web_id_mg = process_search(ig_web_id_mg)

write.csv(ig_web_id_mg, "ig_web_id_mg.csv")

ig_web_id_non_en = rbind(ig_web_id_de, ig_web_id_nl, ig_web_id_sw)
write_xlsx(ig_web_id_non_en, "ig_web_id_non_en.xlsx")


#load cleaned data
ig_web_id_en = read_xlsx("ig_web_id_en.xlsx")
ig_web_id_non_en = read_xlsx("ig_web_id_non_en.xlsx")
ig_web_id = rbind(ig_web_id_en, ig_web_id_non_en)

# drop hand cleaned websites and duplicate links
ig_web_id = subset(ig_web_id, drop != 1)
ig_web_id = subset(ig_web_id, index != "612" & index != "611" & index != "337" & index != "905" & index != "1124" & index != "1258" & index != "1259" & index != "1682")
ig_web_id = unique(ig_web_id, by = "source")

# extract date from snippets
ig_web_id$snip_date = NA

for (i in 1:nrow(ig_web_id)){
  ig_web_id$snip_date[i] = str_extract(ig_web_id$snippet[i], "[0-9]{1,2} (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]{4}")
  ig_web_id$snip_date[i] = sub(" Jan ", ".01.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Feb ", ".02.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Mar ", ".03.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Apr ", ".04.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" May ", ".05.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Jun ", ".06.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Jul ", ".07.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Aug ", ".08.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Sep ", ".09.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Oct ", ".10.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Nov ", ".11.", ig_web_id$snip_date[i])
  ig_web_id$snip_date[i] = sub(" Dec ", ".12.", ig_web_id$snip_date[i])
}

# merge different info on date
# prefer article publication date over snippet date and snippet date over creation date (because creation can happen before publication)
ig_web_id$date = NA
date_list = vector(length = nrow(ig_web_id), mode = "list")

for (i in 1:nrow(ig_web_id)){
  if(is.na(ig_web_id$article_pub_date[i]) == F){
    date_list[i] = str_split(ig_web_id$article_pub_date[i], "-|T", n = Inf, simplify = FALSE)
    ig_web_id$date[i] = paste0(date_list[[i]][3], ".", date_list[[i]][2], ".", date_list[[i]][1])
  }
}

for (i in 1:nrow(ig_web_id)){
  if(is.na(ig_web_id$date[i]) == T){
    ig_web_id$date[i] = ig_web_id$snip_date[i]
  }
}

for (i in 1:nrow(ig_web_id)){
  if(is.na(ig_web_id$date[i]) == T & is.na(ig_web_id$creationdate[i]) == F){
    ig_web_id$date[i] = paste0(str_extract(ig_web_id$creationdate[i], "(?<=[0-9]{6})([0-9]{2})"), ".", str_extract(ig_web_id$creationdate[i], "(?<=[0-9]{4})([0-9]{2})"), ".", str_extract(ig_web_id$creationdate[i], "[0-9]{4}"))
  }
}

ig_web_id$date = as.Date(ig_web_id$date, "%d.%m.%Y")


# Plot
ig_web_id$count = 1 

test = select(ig_web_id, c(date, count))

test = subset(test, date >= "2014-01-01" & date < "2020-01-01")

test = test %>% pad %>% fill_by_value(count)

test = test %>% 
  group_by(week = format(date, '%Y-%W')) %>% 
  summarise_if(is.numeric, sum)

ggplot(data = subset(test, is.na(test$week) == F), aes(x = week, y = count)) +
  geom_point()

################## 2. Scrape the websites and load the documents ################## 
##### a) scrape html websites #####
# Now that I have all the urls leading to the IG websites that contain the search terms it is time to scrape the relevant 
# passages from the websites. I do this in two steps according to the format of the website. 

# 1. For urls leading to a html website, I scrape the relevant paragraphs directly from the web.

# define a function to search for relevant keywords in paragraphs
search_html = function(strings){
  
  clean_string = vector(mode = "list") # initiate empty list
  
  # loop over each paragraph returned by the `scrape_html function` and search for keywords
  
  for(i in 1:length(strings)){
    if (grepl("Copyright|Intellectual Property|Urheberrecht|Geistiges Eigentum|upphovsr?tt|immaterialr?tt|auteursrecht|intellectueel eigendom|derechos de autor|propiedad intellectual|droits d'auteur|propri?t? intellectuelle|diritto d'autore|propriet? intellettuale|direito autoral|propriedade intelectual|prawo autorskie|wlasnosc intelektualna|szerzoij jog|szellemi tulajdon", strings[i], ignore.case = T) == T & (grepl("Europ|Directi|Reform|Richtli|Direktiv|Direttiv|riform|hervorm|diretiv|dyrektyw|ir?nyelv", strings[i], ignore.case = T) == T | grepl("EU|UE", strings[i], ignore.case = F) == T)) {
      clean_string[i] = strings[i]
    } else {
      clean_string[i] = NA
    }
  }
  return(clean_string)
}

# define a function to 
scrape_html = function(data){
  
  # initiate empty lists to store: a) all paragraphs of the html website, b) the type of the url (html, pdf, doc/x), c) whether an error
  # occurred and type of error, d) the source url
  result_list = vector(mode = "list", length = nrow(data))
  type = vector(mode = "list", length = nrow(data))
  errors = vector(mode = "list", length = nrow(data))
  source = vector(mode = "list", length = nrow(data))
  
  for(i in 1:nrow(data)){
    
    setTimeLimit(elapsed = Inf) # set the time limit to infinite again
    
    # set errors to NA as default; extract the source url
    errors[[i]] = NA
    source[[i]] = data$source[i]
    
    # logical if error occured and loop should skip to next iteration
    skip_to_next = F
    skip_to_next1 = F
    
    # print index for each iteration
    print(i)
    
    # if the url leads neither to a pdf or doc/x file, then the html should be scraped
    if(grepl(".pdf", data$source[i], ignore.case = T) == F & grepl(".doc", data$source[i], ignore.case = T) == F){
      type[[i]] = "HTML" # set type to html
      
      html = tryCatch({
        setTimeLimit(elapsed = 60) # set time limit of call to 1 minute to accomodate broken links
        read_html(data$source[[i]]) # scrape website
      }, error = function(e){ # if an error occurs, print the error message and set the skip logical to true
        print(e)
        skip_to_next <<- T
      })
      
      if(skip_to_next) { # if an error occured set the type of error and skip url
        result_list[[i]] = NA
        errors[[i]] = "CONNECTION ERROR" 
        next
      } else { # if no error has occured process the html
        html = html %>% 
          html_nodes('body') %>%
          html_text('p') # get the paragraphs from the html websites body
        
        html = str_split(html, "\n", simplify = F) # split the vector into separate paragraphs
        
        html = html[[1]] # flatten
        
        html = tryCatch({ # try whether html is longer than 10 characters
          html[nchar(html) > 10] 
        }, error = function(e){
          print(e) # if not print the error message and set the skip logical true
          skip_to_next1 <<- T
        })
        
        if(skip_to_next1) { # skip to next html file and set the error to format error if html is shorter than 10 paragraphs
          result_list[[i]] = NA
          errors[[i]] = "FORMAT ERROR"
          next
        } else { # if not do the following:
          
          for (j in 1:length(html)){
            html[j] = str_squish(html[j]) # delete repetitive whitespaces
          }
          
          html = search_html(html) # search the paragraphs for keywords (see function above)
          
          html = html[is.na(html) == F] # delete all empty paragraphs
          
          html = unlist(html) # unlist
          
          html = paste(html, collapse = " \n ") # merge all paragraphs into a single string and indicate start of new paragraph by /n
          
          result_list[[i]] = html # put everything into the result list
        }
      }
      
    } else if (grepl(".pdf", data$source[i], ignore.case = T) == T) { # if its an pdf file indicate it and return NA as result
      type[[i]] = "PDF"
      result_list[[i]] = NA
    } else if (grepl(".doc", data$source[i], ignore.case = T) == T) { # same as above for doc/x files
      type[[i]] = "DOC"
      result_list[[i]] = NA
    } else { # if neither pdf / doc/x / html, then set type and result to NA (should not happen)
      type[[i]] = NA
      result_list[[i]] = NA
    }
  }
  
  #put everything into a dataframe
  type = data.frame(do.call(rbind, type))
  result_list = data.frame(do.call(rbind, result_list))
  source = data.frame(do.call(rbind, source))
  errors = data.frame(do.call(rbind, errors))
  result_df = cbind(type, result_list, source, errors)
  
  # name dataframe columns
  names(result_df)[1] <- "Format"
  names(result_df)[2] <- "html_string"
  names(result_df)[3] <- "source"
  names(result_df)[4] <- "errors"
  
  return(result_df)
}

# apply the function
ig_web_id_html = scrape_html(ig_web_id)

# set time limit to infinite again
setTimeLimit(elapsed = Inf)

# merge html scraping results and the search result into a dataframe
ig_web_id_html = unique(inner_join(ig_web_id, ig_web_id_html, by = "source"))

# add an index
ig_web_id_html$index = row.names(ig_web_id_html)

# export/import to save the data
write.csv(ig_web_id_html, "ig_web_id_html.csv")
ig_web_id_html = read.csv("ig_web_id_html.csv")

##### b) Download files from urls leading to pdf or doc/x ######

# initate a list to report errors
reporting = vector(mode = "list", length = nrow(ig_web_id_html))

# loop over all urls
for (i in 1:nrow(ig_web_id_html)) {
  
  print(i) # print the index 
  skip_to_next = F # define logical to indicate whether to skip to next iteration of the loop
  reporting[[i]] = NA # set reporting to NA by default
  
  tryCatch({ # try to download files if the url leads to a pdf or doc/x file (the format was determined by the `scrape_html` function)
    if(ig_web_id_html$Format[i] == "PDF"){
      download.file(ig_web_id_html$source[i], # url
                    paste0("Interest_Groups_Downloaded_Files/",ig_web_id_html$regid[i],"_index",i,".pdf"), # indicate storage path and create filename in this pattern: 'regid_index'
                    mode = "wb") # needed for windows machines to not fuck up file format
    } else if(ig_web_id_html$Format[i] == "DOC") { # the same for doc/x files
      download.file(ig_web_id_html$source[i], paste0("Interest_Groups_Downloaded_Files/",ig_web_id_html$regid[i],"_index",i,".doc"), mode = "wb")
    }
  }, error = function(e){ # if the link contains '.pdf' or '.doc' but does not lead to a pdf or doc file, print the error message and set skip to next true
    print(e)
    skip_to_next <<- T
  })
  
  if(skip_to_next) { # skip to next and store the index of the url returning the error
    reporting[[i]] = i
    next}
}

reporting = reporting[is.na(reporting) == F] # delete all NA's from reporting list

ig_web_id_html$download_error = NA # set download_error to NA as default

for(i in 1:length(reporting)){ # set download_error to 1 for all urls that lead to an error
  ig_web_id_html$download_error[[reporting[[i]]]] = 1
}

# follow-up manually on errors
error_links = subset(ig_web_id_html, is.na(download_error) == F | is.na(errors) == F)
error_links = select(error_links, c(index, regid, source, download_error, errors))

# manually download files from non-corrupted urls that were not classified as leading to a pdf / doc/x file but do so nonetheless 
# (returned a format error in the html_scrape function) 

# drop links to websites that do not exist anymore (broken urls)
error_links = subset(error_links,  index == "140" | index == "141" | index == "142" | index == "143" | index == "144" | index == "449" | index == "766" |
                       index == "1301" | index == "1384" | index == "1385" | index == "1386" | index == "1387" | index == "1767" | index == "1768" | 
                       index == "1770" | index == "1771")

# before reading in the downloaded files some manual cleaning of formats is necessary: some docx are downloaded as doc; 
# some links are downloaded as doc but aren't a word document, those are copy-pasted into a txt file

##### c) Load downloaded & txt files #####
# manually copy paste the text from the remaining websites that returned an error into a txt file (separate file per website)

directory = getwd()

## read text files but skip and log errors. Function from: https://stackoverflow.com/questions/63305428/ignore-errors-in-readtext-r

readtext_safe = function(path, no) {
  out = tryCatch({readtext(path)}, 
                 error = function(e) "fail",
                 warning = function(e) "fail")
  
  if (isTRUE("fail" == out)) {
    write(path, paste0("errored_files", no, ".txt"), append = TRUE)
  } else {
    return(out)
  }
}

# R tends to break down when running the function on all files at once. Therefore, I separated the files into 4 chunks of 200 and one of 60
#create a character vector with the path of each files
files_1 = list.files(path = "Interest_Groups_Downloaded_Files_1/", ignore.case = TRUE, full.names = TRUE)
files_2 = list.files(path = "Interest_Groups_Downloaded_Files_2/", ignore.case = TRUE, full.names = TRUE)
files_3 = list.files(path = "Interest_Groups_Downloaded_Files_3/", ignore.case = TRUE, full.names = TRUE)
files_4 = list.files(path = "Interest_Groups_Downloaded_Files_4/", ignore.case = TRUE, full.names = TRUE)
files_5 = list.files(path = "Interest_Groups_Downloaded_Files_5/", ignore.case = TRUE, full.names = TRUE)


# apply function to read in files
ig_web_id_text_1 = lapply(files_1, no = 1, readtext_safe)
ig_web_id_text_2 = lapply(files_2, no = 2, readtext_safe)
ig_web_id_text_3 = lapply(files_3, no = 3, readtext_safe)
ig_web_id_text_4 = lapply(files_4, no = 4, readtext_safe)
ig_web_id_text_5 = lapply(files_5, no = 5, readtext_safe)

# bind list into dataframe
ig_web_id_text_1 = data.frame(do.call(rbind, ig_web_id_text_1))
ig_web_id_text_2 = data.frame(do.call(rbind, ig_web_id_text_2))
ig_web_id_text_3 = data.frame(do.call(rbind, ig_web_id_text_3))
ig_web_id_text_4 = data.frame(do.call(rbind, ig_web_id_text_4))
ig_web_id_text_5 = data.frame(do.call(rbind, ig_web_id_text_5))

ig_web_id_text = rbind(ig_web_id_text_1, ig_web_id_text_2, ig_web_id_text_3, ig_web_id_text_4, ig_web_id_text_5)

# look at the files with errors
readLines("errored_files1.txt")
readLines("errored_files3.txt")
readLines("errored_files4.txt")

# processing
# extract regid and index number and add to dataframe
ig_web_id_text$index = gsub(".*index(.*)\\..*", "\\1", ig_web_id_text$doc_id)
ig_web_id_text$regid = gsub("(.*)_index.*", "\\1", ig_web_id_text$doc_id)

# define function to process downloaded files

process_files = function(files){
  
  #initiate empty lists
  result_list = vector(mode = "list", length = nrow(files))
  regid = vector(mode = "list", length = nrow(files))
  index = vector(mode = "list", length = nrow(files))
  
  # loop over downloaded files and process
  for(i in 1:nrow(files)){
    pdf = str_split(files$text[i], "\n", simplify = F) # split into paragraphs
    
    pdf = pdf[[1]] # flatten
    
    pdf = search_html(pdf)  # extract only paragraphs that contain keywords
    
    pdf = pdf[is.na(pdf) == F] # drop empty paragraphs
    
    pdf = unlist(pdf) # unlist
    
    pdf = paste(pdf, collapse = " \n ") # paste into a single string and insert \n at the end of each paragraph
    
    # put everything in the lists
    result_list[[i]] = pdf
    regid[[i]] = files$regid[i]
    index[[i]] = files$index[i]
  }
  
  # convert lists to a single dataframe
  result_list = data.frame(do.call(rbind, result_list))
  regid = data.frame(do.call(rbind, regid))
  index = data.frame(do.call(rbind, index))
  result_df = cbind(result_list, regid, index)
  
  # name the columns in the dataframe
  names(result_df)[1] <- "file_string"
  names(result_df)[2] <- "regid"
  names(result_df)[3] <- "index"
  
  return(result_df)
}

# apply the processing function
ig_web_id_text = process_files(ig_web_id_text)
ig_web_id_text$index = as.integer(ig_web_id_text$index)

ig_web_id = unique(full_join(ig_web_id_text, ig_web_id_html, by = c("regid", "index")))

# check whether there is a case that returned text from html and from pdf / doc/x
subset(ig_web_id, is.na(ig_web_id$file_string) == F & is.na(ig_web_id$html_string) == F)

# put text from html and from files into single column
ig_web_id$text_web = NA

for (i in 1:nrow(ig_web_id)){
  if(is.na(ig_web_id$html_string[i]) == F & nchar(ig_web_id$html_string[i]) > 0){
    ig_web_id$text_web[i] = ig_web_id$html_string[i]
  } else if(is.na(ig_web_id$file_string[i]) == F & nchar(ig_web_id$file_string[i]) > 0){
    ig_web_id$text_web[i] = ig_web_id$file_string[i]
  }
}

################## 3. Text processing ################## 
# Since paragraphs are the unit of analysis for the quantitative text analysis, I create a new dataframe with paragraphs as rows.

# split text into list of paragraphs
ig_web_id$para_text_web = str_split(ig_web_id$text_web, "\n")

# initiate empty outer lists to store information from the original dataset
para_text_ol = vector(mode = "list")
regid_ol = vector(mode = "list")
doc_id_ol = vector(mode = "list")
date_ol = vector(mode = "list")
source_ol = vector(mode = "list")
snippet_ol = vector(mode = "list")
keyword_ol = vector(mode = "list")
text_ol = vector(mode = "list")
para_id_ol = vector(mode = "list")

# loop over each row of the original dataset 
for (i in 1:nrow(ig_web_id)){
  
  # initiate some empty inner lists to store information from the original dataset on the paragraph level for each row
  para_text_il = vector(mode = "list")
  regid_il = vector(mode = "list")
  doc_id_il = vector(mode = "list")
  date_il = vector(mode = "list")
  source_il = vector(mode = "list")
  snippet_il = vector(mode = "list")
  keyword_il = vector(mode = "list")
  text_il = vector(mode = "list")
  para_id_il = vector(mode = "list")
  
  # loop over each paragraph in a row's text and extract the paragraph and additional information from the row
  for (j in 1:length(ig_web_id$para_text_web[i][[1]])){
    para_text_il[j] = ig_web_id$para_text_web[i][[1]][j]
    regid_il[j] = ig_web_id$regid[i]
    doc_id_il[j] = ig_web_id$index[i]
    date_il[j] = ig_web_id$date[i]
    source_il[j] = ig_web_id$source[i]
    snippet_il[j] = ig_web_id$snippet[i]
    keyword_il[j] = ig_web_id$keyword[i]
    text_il[j] = ig_web_id$text_web[i]
    para_id_il[j] = j
  }
  # append the inner lists storing paragraph level results to the outer lists
  para_text_ol[[i]] = para_text_il
  regid_ol[[i]] = regid_il
  doc_id_ol[[i]] = doc_id_il
  date_ol[[i]] = date_il
  source_ol[[i]] = source_il
  snippet_ol[[i]] = snippet_il
  keyword_ol[[i]] = keyword_il
  text_ol[[i]] = text_il
  para_id_ol[[i]] = para_id_il
}

# bind everything in a dataframe with each row being a paragraph
ig_para = data.frame(cbind(unlist(para_text_ol), unlist(regid_ol), unlist(doc_id_ol), unlist(date_ol), unlist(source_ol), unlist(snippet_ol),
                           unlist(keyword_ol), unlist(text_ol), unlist(para_id_ol)))

# name the columns
names(ig_para)[1] = "para_text_web"
names(ig_para)[2] = "regid"
names(ig_para)[3] = "doc_id"
names(ig_para)[4] = "date"
names(ig_para)[5] = "source"
names(ig_para)[6] = "snippet"
names(ig_para)[7] = "keyword"
names(ig_para)[8] = "text_web"
names(ig_para)[9] = "para_id"

#create a paragraph level index
ig_para$index = rownames(ig_para)

# export/import
write.csv(ig_para, "ig_para.csv")
ig_para = read.csv("ig_para.csv")