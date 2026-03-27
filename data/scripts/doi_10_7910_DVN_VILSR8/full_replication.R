### FULL REPLICATION FOR PARLCYMRU: A DATASET OF LEGISLATIVE SPEECHES 
### FROM THE WELSH PARLIAMENT 2016-2021


#### REQUIRED PACKAGES
##########################################################################
require(rvest)
require(lubridate)
require(tidyverse)
require(readr)
require(data.table)
require(textclean)
require(XML)
require(lubridate)
require(pbapply)
require(tidywikidatar)
require(plyr)


#### STRINGS
#########################################################################
path <- "/Users/danbraby/Dropbox/parlCymru/raw_data/"
front <- "https://record.senedd.wales/XMLExport/?start="
mid  <- "&end="
wikidata_front <- "https://www.wikidata.org/wiki/Special:EntityPage/"
wiki_data_delete <- "#sitelinks-wikipedia"
base_url <- "https://en.wikipedia.org/"



#### RECORDS.SENEDD.WALES
#########################################################################


dates <- seq(from = as.Date("2016/5/5"), to = as.Date("2021/5/5"), "month")
links  <- paste0(front, dates, mid, dates + months(1))

results_html <- pblapply(links, read_html)

links_xml_recs <- pblapply(results_html, function(get_links) {
  get_links  %>% html_elements("a")  %>% html_attr("href")
}) %>% unlist() %>% str_subset(pattern = "Download?")

eng_lang <- links_xml_recs %>% str_subset(pattern = "EnglishTranscript") %>%
  paste0("https://record.senedd.wales", .)
welsh_lang <- links_xml_recs %>% str_subset(pattern = "WelshTranscript") %>%
  paste0("https://record.senedd.wales", .)


for (i in 1:length(eng_lang)) {
  download.file(url = eng_lang[i], destfile = paste0(path, "english/", i, ".xml"))
}

for (i in 1:length(welsh_lang)) {
  download.file(url = welsh_lang[i], destfile = paste0(path, "welsh/", i, ".xml"))
}

eng_files <- list.files(path = paste0(path, "english/"), full.names = TRUE)
welsh_files <- list.files(path = paste0(path, "welsh/"), full.names = TRUE)

eng_xml <- pblapply(eng_files, function(xml_formatter) {
  xml_formatter %>% xmlParse() %>% xmlToList()
})
welsh_xml <- pblapply(welsh_files, function(xml_formatter) {
  xml_formatter %>% xmlParse() %>% xmlToList()
})

eng_data <- NA
eng_data <- eng_data %>% as.list()
for(i in 1:length(eng_xml)) {
  eng_data[[i]] <- eng_xml[[i]][1:length(eng_xml[[i]]) - 1] %>% rbindlist(fill = TRUE)
} %>% bind_rows()

welsh_data <- NA
welsh_data <- welsh_data %>% as.list()
for(i in 1:length(welsh_xml)) {
  welsh_data[[i]] <- welsh_xml[[i]][1:length(welsh_xml[[i]]) - 1] %>% rbindlist(fill = TRUE)
} %>% bind_rows()

eng_data <- eng_data %>% bind_rows()
welsh_data <- welsh_data %>% bind_rows()


#### RECTANGULARIZE
#########################################################################


eng_data$Contribution <-  eng_data$Contribution_English %>% replace_html() %>% trimws()
welsh_data$Contribution <- welsh_data$Contribution_Welsh %>% replace_html() %>% trimws()

parlCymru <- eng_data
parlCymru$Contribution_Welsh <- welsh_data$Contribution

parlCymru$is_speech <- NA
parlCymru$is_speech[is.na(parlCymru$Member_name_English)] <- 0
parlCymru$is_speech[is.na(parlCymru$is_speech)] <- 1



parlCymru <- tibble(
  meeting_id = parlCymru$Meeting_ID,
  date = as.Date(parlCymru$MeetingDate),
  contribution_id = parlCymru$Contribution_ID,
  order_by_meeting = parlCymru$Contribution_Order_ID ,
  language = parlCymru$contribution_language,
  agenda_item = parlCymru$Agenda_item_english,
  agenda_item_id = parlCymru$Agenda_Item_ID,
  member_id = parlCymru$Member_Id,
  member_name = parlCymru$Member_name_English,
  role = parlCymru$Member_job_title_English,
  contribution_eng = parlCymru$Contribution,
  contribution_welsh = parlCymru$Contribution_Welsh,
  is_speech = parlCymru$is_speech
)





#### WIKIDATA
#######################################################################



pg  <- read_html("https://en.wikipedia.org/wiki/Members_of_the_5th_National_Assembly_for_Wales")

data <- pg %>% html_elements(".wikitable") %>% .[[5]] 

df <- data %>% html_table()

## Developing a get link function from wikipedia tables

get_link_table <- function(html_table, class){
  html_table %>% 
    html_nodes(xpath=paste0("//a[text()='", class, "']")) %>% 
    .[[1]] %>% 
    html_attr("href")
}

df$MS_link <- sapply(df$Name, function(x)get_link_table(data, x))

welsh_MS <- pblapply(paste0(base_url, df$MS_link), read_html)

welsh_wikidata_links <- lapply(welsh_MS, function(wikidata_grabber) {
  wikidata_grabber %>% html_element(".wb-langlinks-link a") %>% html_attr("href") %>%
    gsub(paste0(wikidata_front, "|", wiki_data_delete), "", .)
})

df$wikidataid <- unlist(welsh_wikidata_links)


welsh_ids <- tw_get_property(id = df$wikidataid,
                             p = "P4651",
                             language = "en")

df <- df[,2:7]

df <- left_join(df, welsh_ids, by = c("wikidataid" = "id"))

df <- df %>% select(-property, -MS_link, -`Senedd Leader`)

df <- df %>% rename(
  party = Party,
  member_name = Name,
  electoral_district = `Constituency or Region`,
  member_id = value
  
)





#### MISSING CASES
###################################################################################
pc <- parlCymru %>% distinct(member_id, .keep_all = TRUE)

pc$linked <- pc$member_id %in% df$member_id

pc <- pc %>% select(member_id, member_name, linked)


not_linked <- tibble(
  member_name = c("Nathan Gill", "Simon Thomas", "Steffan Lewis",
                  "Mohammed Asghar", "Carl Sargeant", "Alun Cairns"),
  member_id = c(433, 383, 289, 130, 178, 12),
  party = c("UKIP", "Plaid Cymru", "Plaid Cymru", "Conservative", "Labour",
            "Conservative"),
  MS_link = c("https://en.wikipedia.org/wiki/Nathan_Gill",
              "https://en.wikipedia.org/wiki/Simon_Thomas_(politician)",
              "https://en.wikipedia.org/wiki/Steffan_Lewis",
              "https://en.wikipedia.org/wiki/Mohammad_Asghar",
              "https://en.wikipedia.org/wiki/Carl_Sargeant",
              "https://en.wikipedia.org/wiki/Alun_Cairns"),
  electoral_district = c("North Wales", "Mid and West Wales",
                         "South East Wales", "South Wales East",
                         "Alyn and Deeside", "South Wales West")
)


not_linked_MS <- pblapply(not_linked$MS_link, read_html)


nl_wikidata_links <- lapply(not_linked_MS, function(wikidata_grabber) {
  wikidata_grabber %>% html_element(".wb-langlinks-link a") %>% html_attr("href") %>%
    gsub(paste0(wikidata_front, "|", wiki_data_delete), "", .)
})

not_linked$wikidataid <- unlist(nl_wikidata_links)

df$member_id <- as.numeric(df$member_id)

MS <- bind_rows(df, not_linked)


#### ADDITIONAL WIKIDATA
##################################################################################################


gender <- tw_get_property(id = MS$wikidataid,
                          p = "P21",
                          language = "en")

MS$gender <- gender$value

MS$gender <- ifelse(MS$gender == "Q6581097", "M", "F")

twitter <- tw_get_property(id = MS$wikidataid,
                           p = "P2002",
                           language = "en")

twitter <- twitter %>% select(id, value) %>% 
  rename(twitter = value,
         wikidataid = id)

MS <- left_join(MS, twitter, by = "wikidataid")


### FINAL SORTING
#####################################################################################################


parlCymru <- parlCymru %>% select(-member_name)

MS$member_id <- as.numeric(MS$member_id)
parlCymru$member_id <- as.numeric(parlCymru$member_id)

parlCymru <- left_join(parlCymru, MS, by = "member_id")


parlCymru$party <- gsub("\\s*\\([^\\)]+\\)","", parlCymru$party) %>% trimws()

parlCymru$electoral_district[parlCymru$electoral_district == "South East Wales"] <- "South Wales East"
parlCymru$party[parlCymru$party == "Conservative"] <- "Conservative Party"


districts <- read_html("https://en.wikipedia.org/wiki/Senedd_constituencies_and_electoral_regions#Electoral_regions")

districts_table <- districts %>% html_element(".wikitable")
districts_df <- districts_table %>% html_table()

districts_df <- districts_df %>% rename(
  constituency = `Constituency
(Welsh name)`,
  region = `Electoral Region`
)

parlCymru$ms_type <- ifelse(parlCymru$electoral_district %in% districts_df$region,
                            "Regional", "Constituency")

parlCymru$ms_type[is.na(parlCymru$wikidataid)] <- NA


districts_df$constituency <- word(districts_df$constituency, 1, sep = "\n")

parlCymru$region <- ifelse(parlCymru$electoral_district %in% districts_df$constituency, districts_df$region,
                           parlCymru$electoral_district)

parlCymru$constituency <- ifelse(parlCymru$electoral_district %in% districts_df$constituency, parlCymru$electoral_district,
                                 NA)

parlCymru$order_by_meeting <- as.numeric(parlCymru$order_by_meeting) + 1

parlCymru <- tibble(
  x = parlCymru$x,
  daily_order_no = parlCymru$daily_order_no,
  meeting_id = parlCymru$meeting_id,
  date = parlCymru$date,
  contribution_id = parlCymru$contribution_id,
  debate = parlCymru$agenda_item,
  debate_id = parlCymru$agenda_item_id,
  order_by_meeting = parlCymru$order_by_meeting,
  member_name = parlCymru$member_name,
  member_role = parlCymru$role,
  member_id = parlCymru$member_id,
  wikidataid = parlCymru$wikidataid,
  ms_type = parlCymru$ms_type,
  party = parlCymru$party,
  constituency = parlCymru$constituency,
  region = parlCymru$region,
  gender = parlCymru$gender,
  is_speech = parlCymru$is_speech,
  language_spoken = parlCymru$language,
  contribution_en = parlCymru$contribution_eng,
  contribution_cy = parlCymru$contribution_welsh
)

parlCymru <- parlCymru %>% distinct(contribution_id, .keep_all = TRUE)

parlCymru$debate <- gsub("[0-9].", "", parlCymru$debate) %>% trimws()
parlCymru$contribution_cy <- gsub("[0-9].", "", parlCymru$contribution_cy) %>% trimws()
parlCymru$contribution_en <- gsub("[0-9].", "", parlCymru$contribution_en) %>% trimws()

parlCymru$member_id[parlCymru$member_name == "Vaughan Gething"] <- 249

parlCymru <- filter(parlCymru, !is.na(contribution_en))

#### ORDER NUMBERING
#################################################################################################
parlCymru$x <- seq(nrow(parlCymru))

parlCymru <- split(parlCymru, parlCymru$date)

for (i in seq_along(parlCymru)) {
  parlCymru[[i]]$daily_order_no <- seq(nrow(parlCymru[[i]]))
}

parlCymru <- rbindlist(parlCymru) %>% arrange(x)

parlCymru <- parlCymru[,c(20, 21, 1:19)]

parlCymru$debate_id <- word(parlCymru$debate_id, 1, sep = "-")


write.csv(parlCymru, "data/parlCymru_5th_Senedd.csv")
write_rds(parlCymru, "data/parlCymru_5th_Senedd.rds")

members <- parlCymru %>% distinct(wikidataid, .keep_all = TRUE) %>% 
  select(member_name, member_id, member_role, wikidataid, party, 
         constituency, region, gender, ms_type) %>% drop_na(gender)


members <- left_join(members, select(MS, wikidataid, twitter), by = "wikidataid")

members <- members %>% distinct(wikidataid, .keep_all = TRUE)
write_csv(members, "data/parlCymru_members.csv")
write_rds(members, "data/parlCymry_members.rds")

parlCymru_5th_Senedd <- filter(parlCymru, is_speech == 1)


#### PARLSPEECH VERSIONS
###################################################################################################

parlSpeech_V2_en <- tibble(
  date = parlCymru_5th_Senedd$date,
  agenda = parlCymru_5th_Senedd$debate_id,
  speeechnumber = parlCymru_5th_Senedd$daily_order_no,
  party = parlCymru_5th_Senedd$party,
  chair =  is.na(parlCymru_5th_Senedd$member_id),
  text = parlCymru_5th_Senedd$contribution_en, 
  parliament = "Cy-Senedd", 
  iso3country = "GBR",
  
)

parlSpeech_V2_cy <- tibble(
  date = parlCymru_5th_Senedd$date,
  agenda = parlCymru_5th_Senedd$debate_id,
  speeechnumber = parlCymru_5th_Senedd$daily_order_no,
  party = parlCymru_5th_Senedd$party,
  chair =  is.na(parlCymru_5th_Senedd$member_id),
  text = parlCymru_5th_Senedd$contribution_cy, 
  parliament = "Cy-Senedd", 
  iso3country = "GBR",
  
)

write_rds(parlSpeech_V2_en, "/Users/danbraby/Dropbox/parlCymru/data/Corp_Senedd_en_V2.rds")
write_rds(parlSpeech_V2_cy, "/Users/danbraby/Dropbox/parlCymru/data/Corp_Senedd_cy_V2.rds")



