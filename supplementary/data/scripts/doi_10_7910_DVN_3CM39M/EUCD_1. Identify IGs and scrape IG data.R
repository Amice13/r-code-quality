library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(readtext)
library(R.utils)

# The goals of this script are (1) to identify all interest groups from the EU transparency register that lobbied on copyright and to 
# (2) retrieve information on their inside lobbying activities and their contact information. 

################ 1. Identify Interest Groups ################
# Since the transparency register does not hold historical information it cannot be used for that purpose. Lobbyfacts.eu, however, collects data from the
# register and stores them over time in so called data cards. Unfortunately, lobbyfacts search function does only search through the most recent data cards.
# Google, however, can also search through the historic data cards but for some reason does not return all groups that are returned by the lobbyfacts search.
# A third way to identify interest groups is extracting the transparency register ids of interest groups involved in the consultation
# process and scraping their info from lobbyfacts. To get the most complete picture I combine the results of all three searches.

##### Google Search API #####
# Lets start with the Google Custom Search API which is set to the following url https://lobbyfacts.eu/representative/*
google.key = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" #API key
google.cx = "7722ea80ac08947c7" #search engine id

Google.Search.API <- function(keyword, google.key, google.cx, country = "de", start_no = 1, daterange) # function to query and parse api
{
  url <- paste0("https://www.googleapis.com/customsearch/v1/siterestrict?"
                , "key=", google.key
                , "&q=", gsub(" ", "+", keyword)
                , "&gl=", country         # Country
                , "&hl=en"                # Language from Browser, english
                , "&cx=", google.cx
                , "&start=", as.character(start_no)
                , "&sort=date:r:", daterange
                #, "&sort=date:", order
                #, "&dateRestrict=", dates
                , "&fields=items(link)"
  )
  
  d2 <- url %>%
    httr::GET(ssl.verifypeer=TRUE) %>%
    httr::content(.) %>% .[["items"]] %>%
    data.table::rbindlist(.) %>%
    mutate(keyword, SERP = row_number(), search.engine = "Google API") %>%
    rename(source = link) %>%
    select(search.engine, keyword, SERP, source)
  
  pause <- round(runif(1, min = 1.1, max = 5), 1)
  if(nrow(d2) == 0)
  {cat("\nPausing", pause, "seconds. Failed for:", keyword)} else
  {cat("\nPausing", pause, "seconds. Successful for:", keyword)}
  
  Sys.sleep(pause)
  rm(keyword, country, pause, url, google.key, google.cx)
  return(d2)
}

# the custom search api does only return results from the first ten pages (a ten results per page). 
# It is therefore necessary to limit the date range insofar that the results do fill less than ten pages.

urls = vector(mode = "list", length = 0) # initiate an empty list

# Loop over up to ten pages for all results before January 01, 2014.
for (i in 0:9){
  
  if(i == 0){
    urls[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = ":20140101")
    
  } else {
    urls[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = ":20140101")
  }
}

urls = bind_rows(urls) #convert list of dataframes to single df

# Repeat for different data ranges
urls1 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls1[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20140101:20150630")

  } else {
    urls1[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20140101:20150630")
  }
}
urls1 = bind_rows(urls1)


urls2 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls2[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20150630:20151231")
    
  } else {
    urls2[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20150630:20151231")
  }
}
urls2 = bind_rows(urls2)


urls3 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls3[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20151231:20161231")
    
  } else {
    urls3[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20151231:20161231")
  }
}
urls3 = bind_rows(urls3)


urls4 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls4[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20161231:20171231")
    
  } else {
    urls4[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20161231:20171231")
  }
}
urls4 = bind_rows(urls4)


urls5 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls5[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20171231:20181231")
    
  } else {
    urls5[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20171231:20181231")
  }
}
urls5 = bind_rows(urls5)


urls6 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls6[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20181231:20191231")
    
  } else {
    urls6[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20181231:20191231")
  }
}
urls6 = bind_rows(urls6)


urls7 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls7[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20191231:20200630")
    
  } else {
    urls7[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20191231:20200630")
  }
}
urls7 = bind_rows(urls7)


urls8 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls8[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20200630:20201031")
    
  } else {
    urls8[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20200630:20201031")
  }
}
urls8 = bind_rows(urls8)


urls9 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls9[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20201031:20201231")
    
  } else {
    urls9[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20201031:20201231")
  }
}
urls9 = bind_rows(urls9)


urls10 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls10[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20201231:20210131")
    
  } else {
    urls10[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20201231:20210131")
  }
}
urls10 = bind_rows(urls10)


urls11 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls11[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20210131:20210228")
    
  } else {
    urls11[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20210131:20210228")
  }
}
urls11 = bind_rows(urls11)


urls12 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls12[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20210228:20210331")
    
  } else {
    urls12[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20210228:20210331")
  }
}
urls12 = bind_rows(urls12)


urls13 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls13[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20210331:20210430")
    
  } else {
    urls13[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20210331:20210430")
  }
}
urls13 = bind_rows(urls13)


urls14 = vector(mode = "list", length = 0)
for (i in 0:9){
  
  if(i == 0){
    urls14[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i, daterange = "20210430:20210820")
    
  } else {
    urls14[[i+1]] = Google.Search.API(keyword = "copyright", google.key, google.cx, country = "de", start_no = i*10+1, daterange = "20210430:20210820")
  }
}
urls14 = bind_rows(urls14)

# combine the urls to a single list and delete potential double entries
urls_all = rbind(urls, urls1, urls2, urls3, urls4, urls5, urls6, urls7, urls8, urls9, urls10, urls11, urls12, urls13, urls14)
urls_google_all = unique(urls_all$source)

##### Lobbyfacts search #####
# Get all the urls returned by the lobbyfacts search
# Define a function that scrapes the urls to all interest groups profiles that match a search
get_sub_urls = function(url) {
  
  page_copyright = read_html(url) #sefine the target url as input
  urls_copyright = page_copyright %>% html_nodes("a") %>% html_attr("href") #get the suburls of IGs profiles
  links_copyright = page_copyright %>% html_nodes("a") %>% html_text() #get the names of IGs
  
  data = data.frame(links = links_copyright, urls = urls_copyright, stringsAsFactors = F) #put into dataframe
  data = subset(data, grepl("representative", urls, fixed = T)) #drop all urls not redirecting to an IG profile
  data$urls = paste("https://lobbyfacts.eu", data$urls, sep = "") #convert suburl to functioning url (pasting site url)
  
  return(data)
}

# Loop over all result pages and apply get_sub_urls function
data = vector(mode = "list", length = 0)

for (i in 1:36){
  
  data[[i]] = get_sub_urls(paste("https://lobbyfacts.eu/search/node/copyright?page=", as.character(i-1), sep = ""))
  
}

# bind to single dataframe and delete double entries
urls_all = bind_rows(data)
urls_lf_all = unique(urls_all$url)

##### EU Consultations on Copyright #####
# Get the registration numbers from EU transparency register
# There were two consultations preceeding the Copyright directive. Extracting the EU transparency register registration numbers 
# from organizations that submitted non-anomynous answers in the consultation process is another way to identify the interest groups involved
# in the process. These numbers than can be used to search and scrape the lobbyfacts database.

## build a function to loop over files in directory and read them into R
read_files = function(path) { #input: the path of the sub folder you want to read in
  
  full_path = paste(getwd(), path, sep = "", collapse = NULL) #create full path
  
  setwd(full_path) #set wd to the path
  
  pdf_files = list.files(path = full_path, pattern = "_*.pdf$", recursive = F) #create list of relevant files
  odt_files = list.files(path = full_path, pattern = "_*.odt$", recursive = F)
  doc_files = list.files(path = full_path, pattern = "_*.doc$", recursive = F)
  docx_files = list.files(path = full_path, pattern = "_*.docx$", recursive = F)
  
  skiperrorread <- function (filename) {
    return(tryCatch(readtext(filename), error=function(e) NULL)) #define a function so that the readtext function skips over errors (some weird file names cannot be read in, as this is a tiny subsection of all files I just skip over them)
  }
  
  answers_pdf = lapply(pdf_files, skiperrorread) #loop over the list of file names and apply the skiperrorread function to read in answers
  answers_odt = lapply(odt_files, skiperrorread)
  answers_doc = lapply(doc_files, skiperrorread)
  answers_docx = lapply(docx_files, skiperrorread)
  
  answers_all = append(answers_pdf, answers_odt) #put all answers into a single list
  answers_all = append(answers_all, answers_doc)
  answers_all = append(answers_all, answers_docx)
  
  answers = vector(mode = "list", length = length(answers_all)) #initialize empty lists
  filenames = vector(mode = "list", length = length(answers_all))
  
  if (length(answers_all) > 0) { #necessary to skip file types that were not in the subfolder
    for (i in 1:length(answers_all)) {
      answers[[i]] = answers_all[[i]][["text"]] #extract the full text and put it into a list
      filenames[[i]] = answers_all[[i]][["doc_id"]] #extract the file name and put it into a list
    } 
  }
  
  out_dta = data.frame(text = unlist(answers), filenames = unlist(filenames)) #create a list of list as output
  
  return(out_dta)
}

#Apply the function to all subfolders and reset working directory (which was changed by the function)
answers_filenames_authorit = read_files(path = "/Consultations/2014_Q1_Review of the EU copyright rules/Submissions/Registered/authorities")
setwd("X:/Mitarbeiter/Michael Kemmerling/EU Copyright/Interessengruppe_Lobbyfacts")
answers_filenames_authors = read_files(path = "/Consultations/2014_Q1_Review of the EU copyright rules/Submissions/Registered/authors")
setwd("X:/Mitarbeiter/Michael Kemmerling/EU Copyright/Interessengruppe_Lobbyfacts")
answers_filenames_cmos = read_files(path = "/Consultations/2014_Q1_Review of the EU copyright rules/Submissions/Registered/cmos")
setwd("X:/Mitarbeiter/Michael Kemmerling/EU Copyright/Interessengruppe_Lobbyfacts")
answers_filenames_inst = read_files(path = "/Consultations/2014_Q1_Review of the EU copyright rules/Submissions/Registered/institutional-users")
setwd("X:/Mitarbeiter/Michael Kemmerling/EU Copyright/Interessengruppe_Lobbyfacts")
answers_filenames_other = read_files(path = "/Consultations/2014_Q1_Review of the EU copyright rules/Submissions/Registered/others")
setwd("X:/Mitarbeiter/Michael Kemmerling/EU Copyright/Interessengruppe_Lobbyfacts")
answers_filenames_pub = read_files(path = "/Consultations/2014_Q1_Review of the EU copyright rules/Submissions/Registered/publishers")
setwd("X:/Mitarbeiter/Michael Kemmerling/EU Copyright/Interessengruppe_Lobbyfacts")
answers_filenames_sp = read_files(path = "/Consultations/2014_Q1_Review of the EU copyright rules/Submissions/Registered/service-providers")
setwd("X:/Mitarbeiter/Michael Kemmerling/EU Copyright/Interessengruppe_Lobbyfacts")
answers_filenames_users = read_files(path = "/Consultations/2014_Q1_Review of the EU copyright rules/Submissions/Registered/users")
setwd("X:/Mitarbeiter/Michael Kemmerling/EU Copyright/Interessengruppe_Lobbyfacts")

answers_filenames = rbind(answers_filenames_authorit, answers_filenames_authors, answers_filenames_cmos, answers_filenames_inst, 
                          answers_filenames_other, answers_filenames_pub, answers_filenames_sp, answers_filenames_users)

#build a function to extract the registration number from the consultation answers. The number has the following pattern: 10 to 12 digits 
#followed by "-" and two digits. This can be used to extract them from the documents.
extract_regid = function(text){
  regid = sub(".*(\\d{12}-\\d{2}).*", "\\1", text)
  if(nchar(regid) > 15){
    regid = sub(".*(\\d{11}-\\d{2}).*", "\\1", text)
  }
  if(nchar(regid) > 15){
    regid = sub(".*(\\d{10}-\\d{2}).*", "\\1", text)
  }
  return(regid)
} 

# apply the function and loop over all answers
for (i in 1:length(answers_filenames$text)){
  answers_filenames$regid[i] = extract_regid(answers_filenames$text[i])
}

# get the lobbyfacts urls using the get_sub_urls function defined before 
data = vector(mode = "list", length = 0) #initiate empty list

for (i in 1:length(answers_filenames$text)){ #loop over all answers
  
  tryCatch(
    expr = {
      data[[i]] = get_sub_urls(paste("https://lobbyfacts.eu/search/node/", as.character(answers_filenames$regid[i]), sep = "")) #put the registration number into the lobbyfacts search
    },
    error = function(e){
      message('No Result!') #sometimes the search does not find any results for a regid, include a tryCatch function, so that the loop doesnt fail in this event
      print(e)
    }
  )
}

# bind to single dataframe and delete double entries
urls_all = bind_rows(data)
urls_consult_all = unique(urls_all$url)

#Combine urls from google search and lobbyfacts search and delete double entries
urls_all = unique(c(urls_lf_all, urls_google_all, urls_consult_all))

################ 2. Retrieve Inside Lobbying data and contact info ################
# Use the Urls to retrieve further information on the interest groups
get_ig_info = function(url) {
  page = read_html(url)
  
  text = page %>% html_nodes("div") %>% html_text("id")
  id = page %>% html_nodes("div") %>% html_attr("id")
  
  ig_dta = data.frame(text = text, id = id)
  
  ig_dta$text = gsub("[[:space:]]", " ", ig_dta$text)
  
  output = data.frame(name = ig_dta$text[ig_dta$id == "zone-title" & !is.na(ig_dta$id)],
                      website = grep("^ *Website", ig_dta$text, value = T),
                      regid = grep("^ *Registration on EU Transparency Register", ig_dta$text, value = T),
                      address = ig_dta$text[ig_dta$id == "offices_addr2" & !is.na(ig_dta$id)],
                      category = ig_dta$text[ig_dta$id == "category_div" & !is.na(ig_dta$id)],
                      subcategory = ig_dta$text[ig_dta$id == "subcategory_div" & !is.na(ig_dta$id)],
                      goal = grep("^ *Goals", ig_dta$text, value = T),
                      issues = grep("^ *Issues", ig_dta$text, value = T),
                      year = ig_dta$text[ig_dta$id == "expenses_dates" & !is.na(ig_dta$id)],
                      lobby_exp = ig_dta$text[ig_dta$id == "expenses_title" & !is.na(ig_dta$id)],
                      lobbyists_fte = ig_dta$text[ig_dta$id == "nr_lobbyists" & !is.na(ig_dta$id)],
                      ep_acc = ig_dta$text[ig_dta$id == "acc_div" & !is.na(ig_dta$id)],
                      ec_meetings = ig_dta$text[ig_dta$id == "explore-data-tab-content-meetings" & !is.na(ig_dta$id)],
                      ec_expertgr = grep("^ *Expert groups", ig_dta$text, value = T),
                      ep_intergr = grep("^ *Intergroups", ig_dta$text, value = T),
                      ep_indfor = grep("^ *Industry forums", ig_dta$text, value = T),
                      affil_memb = ig_dta$text[ig_dta$id == "explore-data-tab-content-networking" & !is.na(ig_dta$id)],
                      tot_budg = ig_dta$text[ig_dta$id == "total_budget" & !is.na(ig_dta$id)],
                      lobbyfacts_url = url,
                      stringsAsFactors = F)
  
  return(output)
}

ig_info_list = vector(mode = "list", length = 0)

for (i in 1:length(urls_consult_all)){
  ig_info_list[[i]] = get_ig_info(urls_consult_all[i])
  Sys.sleep(10)
}

ig_info_dta = bind_rows(ig_info_list)

# cleaning of the data
# remove unnecessary whitespace in names and registration info
ig_info_dta$name = str_squish(ig_info_dta$name)
ig_info_dta$regid = str_squish(ig_info_dta$regid)
ig_info_dta$website = str_squish(ig_info_dta$website)

# extract registration date
ig_info_dta$reg_date = sub(".*: (.*)\\)", "\\1", ig_info_dta$regid)
ig_info_dta$regid = sub(".*Register (.*) \\(.*", "\\1", ig_info_dta$regid)
ig_info_dta$website = sub(".*Website (.*)", "\\1", ig_info_dta$website)
#set to NA if a IG has no website
ig_info_dta$website = ifelse(ig_info_dta$website == "Website", "NA", ig_info_dta$website)

# delete those observations with different link but same regid (the same organization can have different links (one with a code and name another with just a code))
#ig_info_dta = ig_info_dta %>% distinct(regid, .keep_all = T)

write.csv(ig_info_dta, file = "EU Copyright_Interest Groups.csv")

##### Load Lobbying Expenditure and Lobbyists data as registered between 01012015 and 01012020 #####
get_lobby_table = function(url) {
  
  page = read_html(url) #sefine the target url as input
  table = page %>% html_table(header = T, trim = T) #get the table
  table_dta = table[[1]]
  urls = page %>% html_nodes("a") %>% html_attr("href") #get the suburls of IGs profiles
  urls_dta = data.frame(urls = urls, stringsAsFactors = F) #put into dataframe
  urls_dta = subset(urls_dta, grepl("representative", urls, fixed = T)) #drop all urls not redirecting to an IG profile

  data = cbind(table_dta, urls_dta)
  data$lobbyfacts_url = paste("https://lobbyfacts.eu", data$urls, sep = "") #convert suburl to functioning url (pasting site url)
  
  return(data)
}

## 2015
tables_list_15 = vector(mode = "list", length = 0)

for (i in 1:295){
  tables_list_15[[i]] = get_lobby_table(paste("https://lobbyfacts.eu/reports/lobby-costs/all/0/2/2/2/0/0/2015-01-01?page=", as.character(i-1), sep = ""))
  Sys.sleep(10)
}

tables_dta_15 = bind_rows(tables_list_15)
tables_dta_15$date = "01012015"

## 2016
tables_list_16 = vector(mode = "list", length = 0)

for (i in 1:360){
  tables_list_16[[i]] = get_lobby_table(paste("https://lobbyfacts.eu/reports/lobby-costs/all/0/2/2/2/0/0/2016-01-01?page=", as.character(i-1), sep = ""))
  Sys.sleep(10)
}

tables_dta_16 = bind_rows(tables_list_16)
tables_dta_16$date = "01012016"

## 2017
tables_list_17 = vector(mode = "list", length = 0)

for (i in 1:437){
  tables_list_17[[i]] = get_lobby_table(paste("https://lobbyfacts.eu/reports/lobby-costs/all/0/2/2/2/0/0/2017-01-01?page=", as.character(i-1), sep = ""))
  Sys.sleep(10)
}

tables_dta_17 = bind_rows(tables_list_17)
tables_dta_17$date = "01012017"

## 2018
tables_list_18 = vector(mode = "list", length = 0)

for (i in 1:465){
  tables_list_18[[i]] = get_lobby_table(paste("https://lobbyfacts.eu/reports/lobby-costs/all/0/2/2/2/0/0/2018-01-01?page=", as.character(i-1), sep = ""))
  Sys.sleep(10)
}

tables_dta_18 = bind_rows(tables_list_18)
tables_dta_18$date = "01012018"

## 2019
tables_list_19 = vector(mode = "list", length = 0)

for (i in 1:477){
  tables_list_19[[i]] = get_lobby_table(paste("https://lobbyfacts.eu/reports/lobby-costs/all/0/2/2/2/0/0/2019-01-01?page=", as.character(i-1), sep = ""))
  Sys.sleep(10)
}

tables_dta_19 = bind_rows(tables_list_19)
tables_dta_19$date = "01012019"

## 2020
tables_list_20 = vector(mode = "list", length = 0)

for (i in 1:478){
  tables_list_20[[i]] = get_lobby_table(paste("https://lobbyfacts.eu/reports/lobby-costs/all/0/2/2/2/0/0/2020-01-01?page=", as.character(i-1), sep = ""))
  Sys.sleep(10)
}

tables_dta_20 = bind_rows(tables_list_20)
tables_dta_20$date = "01012020"

# bind all dataframes together
tables_dta = rbind(tables_dta_15, tables_dta_16, tables_dta_17, tables_dta_18, tables_dta_19, tables_dta_20)

write.csv("Lobbying Tables_Full Sample_010115-010120.csv", tables_dta)

# Merge Info data with annual lobbying variables
ig_lobby_dta = left_join(ig_info_dta, tables_dta, by = "lobbyfacts_url") #Merge by lobbyfacts_url and keep all rows from the ig_info_dta dataframe

# Some observations have two different links in the ig_info_dta dataset and are matched only on one of them to the tables_dta dataset. These double observations that were not matched need to be dropped.
# Step 1: Identify the observations that are present in the ig_info_dta df but could not be matched to the tables_dta df
na_dta = subset(ig_lobby_dta, is.na(date)) # subset the dta to those observations
na_dta = na_dta %>% distinct(regid, .keep_all = T) #delete all multiple observations

# Step 2: Identify those observations that are present in ig_info_dta and could be matched to tables_dta
full_dta = subset(ig_lobby_dta, !is.na(date)) #subset to those observations
full_dta = full_dta %>% distinct(regid, date, .keep_all = T) #drop all observations with the same regid and the same date (duplicates)

# Step 3: Drop all observations from na_dta whose regid appears in full_dta and bind the two datasets together
na_dta = subset(na_dta, !(na_dta$regid %in% full_dta$regid))
ig_lobby_dta = rbind(full_dta, na_dta)

##### Clean the full dataset #####
#rename the variable names and drop irrelevant variables
ig_lobby_dta = select(ig_lobby_dta, -c(X.x, X.y, X., Organisation.name))

names(ig_lobby_dta)[names(ig_lobby_dta) == 'Head.office.in'] = 'ho_country'
names(ig_lobby_dta)[names(ig_lobby_dta) == 'lobby_exp'] = 'lobby_exp_static'
names(ig_lobby_dta)[names(ig_lobby_dta) == 'Lobbying.costs'] = 'lobby_exp'
names(ig_lobby_dta)[names(ig_lobby_dta) == 'ep_acc'] = 'ep_acc_static'
names(ig_lobby_dta)[names(ig_lobby_dta) == 'EP.passes'] = 'ep_acc'
names(ig_lobby_dta)[names(ig_lobby_dta) == 'lobbyists_fte'] = 'lobbyists_static'
names(ig_lobby_dta)[names(ig_lobby_dta) == 'Lobbyists..FTE.'] = 'lobbyists_fte'
names(ig_lobby_dta)[names(ig_lobby_dta) == 'Meetings.with.EC'] = 'ec_meet'
names(ig_lobby_dta)[names(ig_lobby_dta) == 'ec_meetings'] = 'ec_meet_static'

# clean lobby_exp variable
ig_lobby_dta$lobby_exp[ig_lobby_dta$lobby_exp == "no figure available"] = NA
ig_lobby_dta$lobby_exp = sub("< (.*) €" , "\\1", ig_lobby_dta$lobby_exp)
ig_lobby_dta$lobby_exp = sub("(.*) €" , "\\1", ig_lobby_dta$lobby_exp)
ig_lobby_dta$lobby_exp = gsub("," , "", ig_lobby_dta$lobby_exp)
ig_lobby_dta$lobby_exp_low = sub("(.*) - (.*)" , "\\1", ig_lobby_dta$lobby_exp)
ig_lobby_dta$lobby_exp_high = sub("(.*) - (.*)" , "\\2", ig_lobby_dta$lobby_exp)
ig_lobby_dta$lobby_exp_low = as.numeric(ig_lobby_dta$lobby_exp_low)
ig_lobby_dta$lobby_exp_high = as.numeric(ig_lobby_dta$lobby_exp_high)

# clean lobby_fte variable
ig_lobby_dta$lobbyists_fte[ig_lobby_dta$lobbyists_fte == "N/A"] = NA
ig_lobby_dta$lobbyists_fte = as.numeric(ig_lobby_dta$lobbyists_fte)

# Export data
write.csv(file="./EU Copyright_IGs_Tables.csv", ig_lobby_dta)

ig_lobby_dta = read.csv("EU Copyright_IGs_Tables.csv")

##### Get Twitter handles #####
# extract regid and website from the full data and drop annual observations
ig_websites = ig_lobby_dta %>% distinct(regid, website, .keep_all = F)

#define a function to extract the twitter links from the IGs webpages
twitter_handles = function(data) {
  page = read_html(data$website) #define the IGs websites as input
  urls_twitter = page %>% html_nodes("a") %>% html_attr("href") #get all the urls linked to on the IG website
  urls_twitter = subset(urls_twitter, grepl("twitter", urls_twitter, fixed =T)) # extract only those urls containing twitter
  # bind rsults into dataframe
  urls_data = data.frame(twitter = sub(".*.com/(.*)" , "\\1", urls_twitter), #extract everything in the twitter url following the .com/. For urls linking to a profile, thats the username
                         twitter_full = urls_twitter, #full twitter url
                         website = data$website,
                         regid = data$regid)
  
  return(urls_data)
}

twitter_links = vector(mode = "list", length = 0) #initiate empty list

#loop over all IG websites and apply the function; tryCatch to evade errors from broken urls; Timeout Exception for non-responding urls
for (i in 1:length(ig_websites$website)){
  print(i)
  tryCatch(
    expr = {
      twitter_links[[i]] = withTimeout({twitter_handles(ig_websites[i,])},
                                       timeout = 30)
    },
    TimeoutException = function(ex) cat("Timeout. Skipping.\n"),
    
    error = function(e){
      message('Broken Link')
      print(e)
    }
  )
}

#drop duplicate results and bind list of df into a single dataframe
twitter_dta = unique(bind_rows(twitter_links))

#aggregate by IG and put all links and handles into a list, if the website contained more than one link to twitter
twitter_dta = twitter_dta %>%
  group_by(regid, website) %>%
  summarise(twitter_short = paste(twitter, collapse = ", "),
            twitter_full = paste(twitter_full, collapse = ", "))

#Join the twitter links with the other information on the IGs
ig_lobby_dta = full_join(twitter_dta, ig_lobby_dta, by = c("regid", "website"))
# export data
write.csv(file = "./EU Copyright_IGs_Tables_Twitter_final.csv", ig_lobby_dta)

##### Classify Interest Groups by Type, subtype, side  #####
dta = read.csv(file = "./EU Copyright_IGs_Tables_Twitter_final.csv")

classify_dta = select(dta, c(regid, twitter_short, name, website))

classify_dta = unique(classify_dta)

#write_xlsx(classify_dta, "./EU Copyright_IGs_Klassifizierung.xlsx")

classify_dta = read_xlsx("./EU Copyright_IGs_Klassifizierung.xlsx")

dta = inner_join(dta, classify_dta, by=c("regid", "name", "website"))

plot = dta %>%
  group_by(Subtype, Side) %>%
  summarise(lobby_exp_low = sum(lobby_exp_low, na.rm = T),
            lobby_exp_high = sum(lobby_exp_high, na.rm = T),
            lobbyists_fte = sum(lobbyists_fte, na.rm = T),
            ec_meet = sum(ec_meet, na.rm = T),
            ep_acc = sum(ep_acc, na.rm = T))

plot1 = ggplot(subset(plot, (Side == "Pro" | Side == "Contra")), aes(x = Side, y = lobby_exp_low/1000000, fill = Subtype)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Million Euro") +
  ggtitle("Lobbying Expenditure (Lower Bound) by Side and Interest Group Type")

#plot2 = ggplot(subset(plot, (Side == "Pro" | Side == "Contra")), aes(x = Side, y = lobby_exp_high/1000000, fill = Subtype)) +
#  geom_bar(position = "stack", stat = "identity") +
#  ylab("Lobbying Expenditure (Higher Bound)\n in million Euro") +
#  ggtitle("Lobbying Expenditure by Side and Interest Group Type")

plot3 = ggplot(subset(plot, (Side == "Pro" | Side == "Contra")), aes(x = Side, y = lobbyists_fte, fill = Subtype)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Full-Time Equivalents") +
  ggtitle("Number of Lobbyists by Side and Interest Group Type")

plot4 = ggplot(subset(plot, (Side == "Pro" | Side == "Contra")), aes(x = Side, y = ec_meet, fill = Subtype)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Number of Meetings") +
  ggtitle("Meetings with European Commission by Side and Interest Group Type")

plot5 = ggplot(subset(plot, (Side == "Pro" | Side == "Contra")), aes(x = Side, y = ep_acc, fill = Subtype)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Number of Access Passes") +
  ggtitle("Access Passes to the European Parliament by Side and Interest Group Type")

plot_grid(plot1, plot3, plot4, plot5)
