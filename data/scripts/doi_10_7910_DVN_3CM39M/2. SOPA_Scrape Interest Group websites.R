# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readxl, rvest, XML, dplyr, stringr, tidyr, R.utils, crayon, padr, pdftools, writexl, cowplot, quanteda, readtext, 
       bit64, igraph, ggraph, rtweet)

# The goal of this script is to scrape and download statements from the websites of interest groups interested in EU Copyright legislation.
# Those interest groups were identified in the `identify IGs and get IG info from lobbyfacts` script.

##################### Search for websites to scrape ##################### 
##### Define functions that access and use the Google Custom Search API #####

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
# the custom search api does only return results from the first ten pages (? ten results per page). 
# It is therefore necessary to limit the date range insofar that the results do fill less than ten pages.

search_IG_websites = function(search_term, date_range, site) {
  
  query = paste0(search_term, " site:", site)
  
  urls = vector(mode = "list", length = 0) # initiate an empty list
  
  # Loop over up to ten pages for all results in date range.
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
    select(Name, title, snippet, htmlSnippet, search.engine, keyword, SERP, source, pagemap)
  
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

# read in the dataframe cotaining the urls to interest group websites and other information on interest groups involved in copyright legislation
ig_info_full = read_xlsx("OpenSecrets_IGs_lobbying_on_Copyright.xlsx")
ig_info = subset(ig_info_full, is.na(ig_info_full$website) == F)


##### Query Search API #####
# Websites are searched for each year between 2010 and 2012 starting with SOPA and PIPA and followed by COICA
# 2010

# initiate empty lists to store results
ig_website_10 = vector(mode = "list", length = 0)
ig_id_10 = vector(mode = "list", length = 0)

# loop over all IG urls for site restricted search
for (i in 1:nrow(ig_info)) {
  ig_website_10[[i]] = search_IG_websites(search_term = "'Stop Online Piracy Act' OR 'Protect IP Act' OR 'Protect Intellectual Property Act'", 
                                          site = ig_info$website[[i]], 
                                          date_range = "20100101:20110101")
  ig_id_10[[i]] = ig_info$Name[[i]]
  print(paste0("page: ", i))
}

# create empty list
ig_web_id_10 = vector(mode = "list", length = 0)

# append interest group registration id to the search results
for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_10[[i]][[1]]) > 0){
    ig_website_10[[i]][[1]]$Name = ig_id_10[[i]]
    ig_web_id_10[[i]] = ig_website_10[[i]][[1]]
  }
}

# bind everything into a dataframe
ig_web_id_10 = bind_rows(ig_web_id_10)


# 2011

# initiate empty lists to store results
ig_website_11 = vector(mode = "list", length = 0)
ig_id_11 = vector(mode = "list", length = 0)

# loop over all IG urls for site restricted search
for (i in 1:nrow(ig_info)) {
  ig_website_11[[i]] = search_IG_websites(search_term = "'Stop Online Piracy Act' OR 'Protect IP Act' OR 'Protect Intellectual Property Act'", 
                                          site = ig_info$website[[i]], 
                                          date_range = "20110101:20120101")
  ig_id_11[[i]] = ig_info$Name[[i]]
  print(paste0("page: ", i))
}

# create empty list
ig_web_id_11 = vector(mode = "list", length = 0)

# append interest group registration id to the search results
for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_11[[i]][[1]]) > 0){
    ig_website_11[[i]][[1]]$Name = ig_id_11[[i]]
    ig_web_id_11[[i]] = ig_website_11[[i]][[1]]
  }
}

# bind everything into a dataframe
ig_web_id_11 = bind_rows(ig_web_id_11)


# 2012

# initiate empty lists to store results
ig_website_12 = vector(mode = "list", length = 0)
ig_id_12 = vector(mode = "list", length = 0)

# loop over all IG urls for site restricted search
for (i in 1:nrow(ig_info)) {
  ig_website_12[[i]] = search_IG_websites(search_term = "'Stop Online Piracy Act' OR 'Protect IP Act' OR 'Protect Intellectual Property Act'", 
                                          site = ig_info$website[[i]], 
                                          date_range = "20120101:20130101")
  ig_id_12[[i]] = ig_info$Name[[i]]
  print(paste0("page: ", i))
}

# create empty list
ig_web_id_12 = vector(mode = "list", length = 0)

# append interest group registration id to the search results
for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_12[[i]][[1]]) > 0){
    ig_website_12[[i]][[1]]$Name = ig_id_12[[i]]
    ig_web_id_12[[i]] = ig_website_12[[i]][[1]]
  }
}

# bind everything into a dataframe
ig_web_id_12 = bind_rows(ig_web_id_12)

# COICA
# 2010

# initiate empty lists to store results
ig_website_10_coica = vector(mode = "list", length = 0)
ig_id_10_coica = vector(mode = "list", length = 0)

# loop over all IG urls for site restricted search
for (i in 1:nrow(ig_info)) {
  ig_website_10_coica[[i]] = search_IG_websites(search_term = "'Combating Online Infringement and Counterfeits Act'", 
                                                site = ig_info$website[[i]], 
                                                date_range = "20100101:20110101")
  ig_id_10_coica[[i]] = ig_info$Name[[i]]
  print(paste0("page: ", i))
}

# create empty list
ig_web_id_10_coica = vector(mode = "list", length = 0)

# append interest group registration id to the search results
for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_10_coica[[i]][[1]]) > 0){
    ig_website_10_coica[[i]][[1]]$Name = ig_id_10_coica[[i]]
    ig_web_id_10_coica[[i]] = ig_website_10_coica[[i]][[1]]
  }
}

# bind everything into a dataframe
ig_web_id_10_coica = bind_rows(ig_web_id_10_coica)


# 2011

# initiate empty lists to store results
ig_website_11_coica = vector(mode = "list", length = 0)
ig_id_11_coica = vector(mode = "list", length = 0)

# loop over all IG urls for site restricted search
for (i in 1:nrow(ig_info)) {
  ig_website_11_coica[[i]] = search_IG_websites(search_term = "'Combating Online Infringement and Counterfeits Act'", 
                                          site = ig_info$website[[i]], 
                                          date_range = "20110101:20120101")
  ig_id_11_coica[[i]] = ig_info$Name[[i]]
  print(paste0("page: ", i))
}

# create empty list
ig_web_id_11_coica = vector(mode = "list", length = 0)

# append interest group registration id to the search results
for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_11_coica[[i]][[1]]) > 0){
    ig_website_11_coica[[i]][[1]]$Name = ig_id_11_coica[[i]]
    ig_web_id_11_coica[[i]] = ig_website_11_coica[[i]][[1]]
  }
}

# bind everything into a dataframe
ig_web_id_11_coica = bind_rows(ig_web_id_11_coica)


# 2012

# initiate empty lists to store results
ig_website_12_coica = vector(mode = "list", length = 0)
ig_id_12_coica = vector(mode = "list", length = 0)

# loop over all IG urls for site restricted search
for (i in 1:nrow(ig_info)) {
  ig_website_12_coica[[i]] = search_IG_websites(search_term = "'Combating Online Infringement and Counterfeits Act'", 
                                          site = ig_info$website[[i]], 
                                          date_range = "20120101:20130101")
  ig_id_12_coica[[i]] = ig_info$Name[[i]]
  print(paste0("page: ", i))
}

# create empty list
ig_web_id_12_coica = vector(mode = "list", length = 0)

# append interest group registration id to the search results
for (i in 1:nrow(ig_info)){
  if(nrow(ig_website_12_coica[[i]][[1]]) > 0){
    ig_website_12_coica[[i]][[1]]$Name = ig_id_12_coica[[i]]
    ig_web_id_12_coica[[i]] = ig_website_12_coica[[i]][[1]]
  }
}

# bind everything into a dataframe
ig_web_id_12_coica = bind_rows(ig_web_id_12_coica)


# Process websites

ig_web_id = rbind(ig_web_id_10, ig_web_id_11, ig_web_id_12, ig_web_id_10_coica, ig_web_id_11_coica, ig_web_id_12_coica)

ig_web_id = process_search(ig_web_id)

ig_web_id = unique(ig_web_id, by = "source")

write_xlsx(ig_web_id, "SOPA_ig_web_id.xlsx")

#load cleaned data
ig_web_id = read_xlsx("SOPA_ig_web_id.xlsx")

# drop hand cleaned websites and duplicate links
ig_web_id = subset(ig_web_id, drop != 1)

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

test = subset(test, date >= "2010-01-01" & date < "2013-01-01")

test = test %>% pad %>% fill_by_value(count)

test = test %>% 
  group_by(week = format(date, '%Y-%W')) %>% 
  summarise_if(is.numeric, sum)

ggplot(data = subset(test, is.na(test$week) == F), aes(x = week, y = count)) +
  geom_point()

##################### Scrape websites #####################
##### HTML websites #####
# Now that I have all the urls leading to the IG websites that contain the search terms it is time to scrape the relevant 
# passages from the websites. I do this in two steps according to the format of the website. 

# 1. For urls leading to a html website, I scrape the relevant paragraphs directly from the web.

# define a function to search for relevant keywords in paragraphs
search_html = function(strings){
  
  clean_string = vector(mode = "list") # initiate empty list
  
  # loop over each paragraph returned by the `scrape_html function` and search for keywords
  
  for(i in 1:length(strings)){
    if (grepl("Copyright|Counterfeit|Intellectual Property|Infringe|Piracy", strings[i], ignore.case = T) == T) {
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

##### Download files from urls leading to pdf or doc/x #####

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
                    paste0("Interest_Groups_Downloaded_Files/",ig_web_id_html$Name[i],"_index",i,".pdf"), # indicate storage path and create filename in this pattern: 'regid_index'
                    mode = "wb") # needed for windows machines to not fuck up file format
    } else if(ig_web_id_html$Format[i] == "DOC") { # the same for doc/x files
      download.file(ig_web_id_html$source[i], paste0("Interest_Groups_Downloaded_Files/",ig_web_id_html$Name[i],"_index",i,".doc"), mode = "wb")
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

##### Follow-up manually on errors #####
error_links = subset(ig_web_id_html, is.na(download_error) == F | is.na(errors) == F)
error_links = select(error_links, c(index, Name, source, download_error, errors))

# manually download files from non-corrupted urls that were not classified as leading to a pdf / doc/x file but do so nonetheless 
# (returned a format error in the html_scrape function) 

# manually copy paste the text from the remaining websites that returned an error into a txt file (separate file per website)

# before reading in the downloaded files some manual cleaning of formats is necessary: some docx are downloaded as doc; 
# some links are downloaded as doc but aren't a word document, those are copy-pasted into a txt file

##### Load downloaded & txt files #####

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

#create a character vector with the path of each files
files = list.files(path = "Interest_Groups_Downloaded_Files/", ignore.case = TRUE, full.names = TRUE)

# apply function to read in files
ig_web_id_text = lapply(files, no = 1, readtext_safe)

# bind list into dataframe
ig_web_id_text = data.frame(do.call(rbind, ig_web_id_text))

# look at the files with errors
readLines("errored_files.txt")

# processing
# extract regid and index number and add to dataframe
ig_web_id_text$index = gsub(".*index(.*)\\..*", "\\1", ig_web_id_text$doc_id)
ig_web_id_text$Name = gsub("(.*)_index.*", "\\1", ig_web_id_text$doc_id)

# define function to process downloaded files

process_files = function(files){
  
  #initiate empty lists
  result_list = vector(mode = "list", length = nrow(files))
  Name = vector(mode = "list", length = nrow(files))
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
    Name[[i]] = files$Name[i]
    index[[i]] = files$index[i]
  }
  
  # convert lists to a single dataframe
  result_list = data.frame(do.call(rbind, result_list))
  Name = data.frame(do.call(rbind, Name))
  index = data.frame(do.call(rbind, index))
  result_df = cbind(result_list, Name, index)
  
  # name the columns in the dataframe
  names(result_df)[1] <- "file_string"
  names(result_df)[2] <- "Name"
  names(result_df)[3] <- "index"
  
  return(result_df)
}

# apply the processing function
ig_web_id_text = process_files(ig_web_id_text)
ig_web_id_text$index = as.integer(ig_web_id_text$index)

ig_web_id = unique(full_join(ig_web_id_text, ig_web_id_html, by = c("index")))

names(ig_web_id)[5] = "Name"

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

##################### Data Processing ##################### 
# Since paragraphs are the unit of analysis for the quantitative text analysis, I create a new dataframe with paragraphs as rows.

# split text into list of paragraphs
ig_web_id$para_text_web = str_split(ig_web_id$text_web, "\n")

# initiate empty outer lists to store information from the original dataset
para_text_ol = vector(mode = "list")
Name_ol = vector(mode = "list")
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
  Name_il = vector(mode = "list")
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
    Name_il[j] = ig_web_id$Name[i]
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
  Name_ol[[i]] = Name_il
  doc_id_ol[[i]] = doc_id_il
  date_ol[[i]] = date_il
  source_ol[[i]] = source_il
  snippet_ol[[i]] = snippet_il
  keyword_ol[[i]] = keyword_il
  text_ol[[i]] = text_il
  para_id_ol[[i]] = para_id_il
}

# bind everything in a dataframe with each row being a paragraph
ig_para = data.frame(cbind(unlist(para_text_ol), unlist(Name_ol), unlist(doc_id_ol), unlist(date_ol), unlist(source_ol), unlist(snippet_ol),
                           unlist(keyword_ol), unlist(text_ol), unlist(para_id_ol)))

# name the columns
names(ig_para)[1] = "para_text_web"
names(ig_para)[2] = "Name"
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

##################### Data Analysis ##################### 
# Now, the data is prepared for the identification of frames in each paragraph #####

#load frames data
frames = read_xlsx("Frames_regex.xlsx", sheet = "all frames")

##### Define Functions to identify frames #####
frames_dta = function(data, frames){
  
  #convert date variable to date type
  data$date = as.Date(data$date, format = "%Y-%m-%d")
  
  data = subset(data, data$date >= as.Date("2010-01-01"))
  
  #add second index variable
  data$index1 = data$index
  
  #create a text corpus
  corp = corpus(data, text_field = "para_text_web", docid_field = "index")
  #tokenize & 4grams
  toks = tokens(corp, remove_punct = T)
  toks_4gram = tokens_ngrams(toks, n = 4)
  #construct dfm
  dfm = dfm(toks_4gram, tolower = T)
  #create dictionary
  dict = dictionary(list(
    censor_broad = c(na.omit(frames$censorship), na.omit(frames$overly_broad), na.omit(frames$effectiveness)),
    democracy = na.omit(frames$democracy),
    lobby = na.omit(frames$lobbying),
    econ_inno = c(na.omit(frames$innovation), na.omit(frames$economy), na.omit(frames$local_small)),
    user_rights = na.omit(frames$privacy),
    due_process = na.omit(frames$due_process),                       
    break_internet = na.omit(frames$break_internet_short),
    criminal = na.omit(frames$criminal),
    public_safety = na.omit(frames$public_safety),
    foreign = na.omit(frames$foreign)
  ))
  
  #create a sparse dfm including only the two categories of our dictionary as features 
  dfm.dict = dfm_lookup(dfm, #give our document feature matrix as input
                        verbose=T, #the command returns messages
                        dictionary = dict, #applies our dictionary to the initial dfm
                        case_insensitive = TRUE,
                        valuetype = "regex")
  
  dfm.dict = data.frame(dfm.dict, date = docvars(corp,"date"), id = docvars(corp, "index1")) #create a dataframe including information on date
  
  #convert date variable to date type
  dfm.dict$date <- as.Date(dfm.dict$date,
                           format = "%Y-%m-%d")
  
  #set paragraph to one when the frame is used in the paragraph
  # Create a variable indicating whether a tweet uses a frame
  dfm.dict$censor_broad = ifelse(dfm.dict$censor_broad > 0, 1, 0)
  dfm.dict$econ_inno = ifelse(dfm.dict$econ_inno > 0, 1, 0)
  dfm.dict$criminal = ifelse(dfm.dict$criminal > 0, 1, 0)
  dfm.dict$public_safety = ifelse(dfm.dict$public_safety > 0, 1, 0)
  dfm.dict$lobby = ifelse(dfm.dict$lobby > 0, 1, 0)
  dfm.dict$democracy = ifelse(dfm.dict$democracy > 0, 1, 0)
  dfm.dict$user_rights = ifelse(dfm.dict$user_rights > 0, 1, 0)
  dfm.dict$due_process = ifelse(dfm.dict$due_process > 0, 1, 0)
  dfm.dict$foreign = ifelse(dfm.dict$foreign > 0, 1, 0)
  dfm.dict$break_internet = ifelse(dfm.dict$break_internet > 0, 1, 0)
  
  #create a variable containing info on the phase
  dfm.dict$phase = ifelse(dfm.dict$date < as.Date("2011-11-14"), 0, 1)
  dfm.dict$phase = ifelse(dfm.dict$date >= as.Date("2012-01-16"), 2, dfm.dict$phase)
  dfm.dict$phase = ifelse(dfm.dict$date >= as.Date("2012-02-01"), 3, dfm.dict$phase)
  
  return(dfm.dict)
}

# Define a function to plot frames
tweet_plot = function(data, cnt_tit, sal_high = 10, multip = 2, unit = "hundred thousands"){
  
  #convert date variable to date type
  data$date = as.Date(data$date,
                      format = "%Y-%m-%d")
  
  dta_fra = data %>% drop_na(phase)
  
  #aggregate by phase
  dfm.dict.grouped = dta_fra %>%
    group_by(phase) %>% 
    summarise(censor_broad = sum(censor_broad),
              econ_inno = sum(econ_inno),
              criminal = sum(criminal),
              public_safety = sum(public_safety),
              lobby = sum(lobby),
              democracy = sum(democracy),
              user_rights = sum(user_rights),
              due_process = sum(due_process),
              break_internet = sum(break_internet),
              foreign = sum(foreign),
              no_paragraphs = length(id))
  
  #drop variables that wont be plotted
  dfm.dict.grouped = select(dfm.dict.grouped, -no_paragraphs)
  
  #convert from wide to long
  dfm.plot = dfm.dict.grouped %>%
    pivot_longer(-phase, names_to = "frame", values_to = "freq_twt")
  
  #plot
  plot_frames = ggplot(dfm.plot, aes(x = reorder(frame, freq_twt), ymin = 0, ymax = freq_twt, color = as.factor(phase))) +
    geom_linerange(aes(color = as.factor(phase)), position = position_dodge(width=0.66), size =1) +
    geom_point(aes(y=freq_twt, color = as.factor(phase)), position = position_dodge(width=0.66), size=4, alpha=0.6) +
    labs(title = "",
         subtitle = paste("Frames in", cnt_tit, "Tweets"),
         y = "Relative Frequency",
         x = NULL,
         color = "Phase") +scale_color_discrete(labels = c("Pre-ACD", "Post-ACD", "Post-IB", "Feb-Dec 12")) +
    scale_x_discrete(labels = c("due_process" = "Lack of due \nprocess", "break_internet" = "Break the \nInternet", "public_safety" = "Public Safety",
                                "foreign" = "Foreign Actors", "user_rights" = "User Rights", "democracy" = "Democracy", "lobby" = "Lobbyism", 
                                "censor_broad" = "Censorship & \nOverly Broad", "criminal" = "Criminality", "econ_inno" = "Economy &\nInnovation")) +
    coord_flip() +
    theme(legend.title = "Phase") +
    theme_classic()
  
  # aggregrate by week
  data_sal = subset(data, date >= as.Date("2010-01-01"))
  
  data_sal$date_week = format(data_sal$date, "%Y-%V")
  
  data_sal = data_sal %>% 
    group_by(date_week) %>%
    summarise(no_paragraphs = length(unique(id)))
  
  # fill missing weeks
  date_week = seq(as.Date("2010-01-01"), by = "week", length.out = 160)
  date_week = format(date_week, "%Y-%V")
  
  no_paragraphs = seq(0, by = 0, length.out = 160)
  
  empty_data = cbind(as.data.frame(date_week), as.data.frame(no_paragraphs))
  
  data_sal = rbind(data_sal, empty_data)
  
  data_sal = data_sal %>% 
    group_by(date_week) %>%
    summarise(no_paragraphs = sum(no_paragraphs))
  
  plot_sal = ggplot(data_sal, aes(x = date_week, y = no_paragraphs, group = 1)) + 
    geom_vline(xintercept = "2011-46", color = "grey") +
    geom_vline(xintercept = "2012-03", color = "grey") +
    geom_line() + 
    scale_x_discrete(breaks = c("2010-01", "2010-26","2011-01", "2011-26","2012-01", "2012-26")) +
    scale_y_continuous(breaks = c(1*multip, 2*multip, 3*multip, 4*multip, 5*multip, 6*multip, 7*multip, 8*multip, 9*multip, 10*multip),
                       limits = c(0, sal_high*multip),
                       labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) + 
    annotate("text", 
             x = c("2011-39", "2011-49"), 
             y = c(6*multip, 6*multip),
             label = c("American Censorship Day",
                       "Internet Blackout"),
             color = "grey",
             size = 3.5,
             angle = 90,
             fontface = "bold") + 
    labs(x = "Week",
         y = paste("Number of Paragraphs (in ", unit, ")"),
         subtitle = paste("Salience in", cnt_tit, "Interest Group Websites"),
         title = cnt_tit) +
    theme_classic()
  
  plot = plot_grid(plot_sal, plot_frames)
  
  return(plot)
}

memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=56000)

##### Identify frames (apply function) #####
dta_frames = frames_dta(data = ig_para, frames = frames)

# Some data processing and merging the original dataframes with the frames dataframe
# set format to date
ig_para$date = as.Date(ig_para$date,
                       format = "%Y-%m-%d")

# delete irrelevant variables
dta_frames = select(dta_frames, -doc_id)
ig_para = select(ig_para, -X)

# rename source to web_source
names(ig_para)[5] = "web_source"

# merge ig_para and dta_frames
ig_web_final = full_join(ig_para, dta_frames, by = c("index" = "id", "date" = "date"))

# merge
ig_web_final$Name = tolower(ig_web_final$Name)
ig_info$Name = tolower(ig_info$Name)

ig_web_final = full_join(ig_web_final, ig_info, by = "Name")

#export
write.csv(ig_web_final, "ig_web_final.csv")

##### Plot IG website frames #####

plot_dta = select(ig_web_final, c("censor_broad", "democracy", "lobby", "econ_inno", "user_rights", "due_process", "break_internet", "criminal",
                                  "public_safety", "foreign", "date", "index", "phase"))

names(plot_dta)[12] = "id"  

plot_all = tweet_plot(data = plot_dta, cnt_tit = "All")
plot_all
ggsave("tweets_all.jpg", plot_all, width = 15, height = 7.5)


##################### Interest Group Twitter ##################### 
# Get the user ids of interest groups
# In the `identify IGs and get IG info from lobbyfacts` script I collected information on Interest Groups involved in EU copyright policy,
# including their twitter handles. Using the twitter api I search their user ids based on these handles.

# authenticate with twitter API
twitter_token = create_token(app = 'Business Power in Digital Capitalism',
                             consumer_key = 'eSJE5I7VokkJFVezUosobKizX',
                             consumer_secret = 't8pRNsjQVeuLMXUP4NCved9fInWQpfE2caMvpVr8oBef0cVahp',
                             access_token = '1179756999220879360-OgPqVhvdCpDMqhtGVnK7Vdb0cQldLK',
                             access_secret = 'yILZBQ12dmtBk7vT30Bgqed5fWNkc2dzYSlgGhZ2brwxd'
)

# import data on interest groups
IG_info = read_xlsx("OpenSecrets_IGs_lobbying_on_Copyright.xlsx")

# select only usernames variable
IG_usernames = c(na.omit(IG_info$twitter_username1), na.omit(IG_info$twitter_username2), na.omit(IG_info$twitter_username3), na.omit(IG_info$twitter_username4))

# delete unneccessary whitespace
IG_usernames = gsub(" ", "", IG_usernames, fixed = TRUE)

# use the rtweet's lookup_users function to get the user ids and screennames
IG_users = lookup_users(IG_usernames)
IG_users = select(IG_users, c(user_id, screen_name))

# export IG twitter IDs
write.csv(IG_users, "Interest Group Twitter IDs.csv")

# Merge twitter ids and splitted usernames to ig_info dataframe
merge_tw_username = function(num) {
  twitter_username = paste0("twitter_username", num)
  tw_id = paste0("tw_id", num)
  
  IG_info[[tw_id]] = NA
  
  for (i in 1:nrow(IG_info)) {
    for (j in 1:nrow(IG_users)) {
      if (IG_info[[twitter_username]][[i]] %in% IG_users$screen_name[[j]]) {
        IG_info[[tw_id]][[i]] = IG_users$user_id[[j]]
      }
    }
  }
  return(IG_info)
}

IG_info = merge_tw_username(1)
IG_info = merge_tw_username(2)
IG_info = merge_tw_username(3)
IG_info = merge_tw_username(4)

write_xlsx(IG_info, "OpenSecrets_IGs_lobbying_on_Copyright.xlsx")

# load data on the frames in all tweets on EU copyright (the dataset was created in the `EU_Tweets` script)
tweets = read.csv("tweets_frames.csv")

# define date variable as date format
tweets$date = as.Date(tweets$datetime)

# load the list of Interest Groups Twitter ids
IG_ids = read.csv("Interest Group Twitter IDs.csv")

# keep only those tweets by interest group accounts
IG_tweets = subset(tweets, ((author_id %in% IG_ids$user_id) | (author_username %in% IG_ids$screen_name)))
IG_tweets$id = IG_tweets$index1


write.csv(IG_tweets, "Interest Groups Tweets.csv")



# plot it

plot_IGs = tweet_plot(data = IG_tweets, cnt_tit = "Interest Groups", sal_high = 10, multip = 100, unit = "hundreds")
plot_IGs

#ggsave("tweets_IGs.jpg", plot_meps, width = 15, height = 7.5)
