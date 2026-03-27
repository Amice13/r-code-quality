################################################
################################################
###                                          ###
### Connecting PDBD data to Third-Party Data ###
###                                          ###
################################################
################################################


# Prepared by [masked name] as supplementary material for the
# manuscript "Parliaments Day-by-Day: A New Open Source Database to Answer the Question Who Was in Which Parliament, Party and Party-group When"

# Goals:
# 1) Users learn how to generate analysable data frames on the level of their
#    unit of analysis.

# 2) Users learn how to connect PDBD data with third-party data.


###############
# Preparation #
###############

# Change the language and date formatting to English if it is not already
Sys.setenv(LANG = "EN")
Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct does not work
Sys.getlocale(category = "LC_ALL")

# Set working directory
setwd("......")
getwd()

# Note: Users of this script need to use the folder called "csv_files\primary_data_frames" 
#       one level below their working directory. The PDBD data files that are 
#       provided via Dataverse need to be stored there.
# 		https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FPYGBDO&version=DRAFT

############
# Packages #
############

# Install packages if necessary
if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
if("httr" %in% rownames(installed.packages()) == FALSE) {install.packages("httr")}
if("XML" %in% rownames(installed.packages()) == FALSE) {install.packages("XML")}
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
if("stringi" %in% rownames(installed.packages()) == FALSE) {install.packages("stringi")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}




# Load packages
library(sqldf)
library(rvest)
library(gtools)
library(httr)
library(XML)
library(stringr)
library(stringi)
library(dplyr)
library(data.table)
library(ggplot2)


##################
# Load PDBD data #
##################

# Load RESE
RESE = read.csv("./csv_files/primary_data_frames/RESE_2020-07-10_1004.csv", header = TRUE, sep = ";")
summary(RESE)
names(RESE)
head(RESE)

# Load POLI
POLI = read.csv("./csv_files/primary_data_frames/POLI_2020-07-10_1004.csv", header = TRUE, sep = ";")
summary(POLI)
names(POLI)
head(POLI)

# Load PARL
PARL = read.csv("./csv_files/primary_data_frames/PARL_2020-07-10_1004.csv", header = TRUE, sep = ";")
summary(PARL)
names(PARL)
head(PARL)


# Load MEME
MEME = read.csv("./csv_files/primary_data_frames/MEME_2020-07-10_1004.csv", header = TRUE, sep = ";")
summary(MEME)
names(MEME)
head(MEME)

##############################################
##############################################
###                                        ###
### 1 Example: Generating a New Data Frame ###
###                                        ###
##############################################
##############################################

# In this first part, we generate a small data frame that would, in essence, be analysable.
# This data frame combines external data from the Swiss Parliament with PDBD data.


#####################################################
# Webscraping Example Data: Legislative Instruments # 
#####################################################

### Legislative instruments records from the Swiss parliament
# See also: https://www.parlament.ch/centers/documents/de/kurzdokumentation-webservices-d.pdf

# Where are they stored? See for example http://ws-old.parlament.ch/affairs?pageNumber=507

# 1) Get a list of the ids of all intstruments
# a) Test:
url <- "http://ws-old.parlament.ch/affairs?pageNumber=678"
TEST <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div/div[2]/table') %>%
  html_table()
TEST <- TEST[[1]]
colnames(TEST)[1] <- "details"

# b) Do this in a loop
ALLINSTRU <- data.frame(matrix(NA, nrow = 0, ncol(TEST))) # Generate an empty data frame
colnames(ALLINSTRU) <- colnames(TEST) # Rename the columns


for (i in 734:776) # For all entries from 2016
{
  pb <- txtProgressBar(min = 734, max = 776, style = 3) # this adds a progress bar
  
  pagenumber <- as.character(i)
  url <- paste("http://ws-old.parlament.ch/affairs?pageNumber=",pagenumber,sep="")
  SOMEINSTRU <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/div/div[2]/table') %>%
    html_table()
  SOMEINSTRU <- SOMEINSTRU[[1]]
  colnames(SOMEINSTRU)[1] <- "details"
  tryCatch(ALLINSTRU <- smartbind(ALLINSTRU, SOMEINSTRU), error=function(e) ALLINSTRU) # if there is nothing to be added this creates an error. Therefore, in case of error, just keep df_total
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 2) Now use these ids to retrieve information on these legislative instruments 
# a) Test:
# See https://stackoverflow.com/questions/32661793/follow-up-how-to-download-xml-when-it-somehow-is-html

# Retrieve the file
x = GET("http://ws-old.parlament.ch/affairs/20173812?format=xml")

# Check that there were no errors.
stop_for_status(x)

# Get the XML content
TEST = content(x, encoding="UTF-8")

# Parse it
TEST = xmlParse(TEST)

# Extract information:
TESTEXTRACT <- data.frame(matrix(NA, nrow = 1, ncol=0)) # Generate an empty data frame

    # a) Submitter
    TESTEXTRACT$submitter <- unlist(TEST["string(/affair/author/councillor/name)"])
    # b) id_ch_parliament: The same id that is also stored in POLI
    TESTEXTRACT$id_ch_parliament <- unlist(TEST["string(/affair/author/councillor/id)"])
    # c) Date of submission
    TESTEXTRACT$submission_date <- unlist(TEST["string(/affair/deposit/date)"])
    # d) Title of the instrument
    TESTEXTRACT$instrument_title <- unlist(TEST["string(/affair/title)"])
    # e) Id of the legislative instrument
    TESTEXTRACT$instrument_id <- unlist(TEST["string(/affair/id)"])
    # f) Type of the legislative instrument
    TESTEXTRACT$instrument_type <- unlist(TEST["string(/affair/affairType/name)"])
    # g) Submission text
    TESTEXTRACT$submission_text <- unlist(TEST["string(/affair/texts/text[2]/value)"])
    # h) Policy areas
    TESTEXTRACT$policy_area <- unlist(TEST["string(/affair/additionalIndexing)"])
    #i ) Legislative period
    TESTEXTRACT$leg_period <- unlist(TEST["string(/affair/deposit/legislativePeriod)"])


# b) Again, do this in a loop for all legislative instruments whose ids are listed in ALLINSTRU
INSTRUMENTS <- data.frame(matrix(NA, nrow = 0, ncol(TESTEXTRACT))) # Generate an empty data frame
colnames(INSTRUMENTS) <- colnames(TESTEXTRACT) # Rename the columns


for (i in 1:nrow(ALLINSTRU)) # A loop for all rows of ALLINSTRU
{
  pb <- txtProgressBar(min = 1, max = nrow(ALLINSTRU), style = 3) # this adds a progress bar
  
  # Create the URL
  url <- paste("http://ws-old.parlament.ch/affairs/",ALLINSTRU$Id[i],"?format=xml", sep = "")
  
  # Retrieve the file
  x = GET(url)
  
  # Check that there were no errors.
  stop_for_status(x)

  # Get the XML content
  TEST = content(x, encoding="UTF-8")
  
  # Parse it
  TEST = xmlParse(TEST)
  
  # Extract information:
  INSTRUEXTRACT <- data.frame(matrix(NA, nrow = 1, ncol=0)) # Generate an empty data frame
  
  # a) Submitter
  INSTRUEXTRACT$submitter <- unlist(TEST["string(/affair/author/councillor/name)"])
  # b) id_ch_parliament: The same id that is also stored in POLI
  INSTRUEXTRACT$id_ch_parliament <- unlist(TEST["string(/affair/author/councillor/id)"])
  # c) Date of submission
  INSTRUEXTRACT$submission_date <- unlist(TEST["string(/affair/deposit/date)"])
  # d) Title of the instrument
  INSTRUEXTRACT$instrument_title <- unlist(TEST["string(/affair/title)"])
  # e) Id of the legislative instrument
  INSTRUEXTRACT$instrument_id <- unlist(TEST["string(/affair/id)"])
  # f) Type of the legislative instrument
  INSTRUEXTRACT$instrument_type <- unlist(TEST["string(/affair/affairType/name)"])
  # g) Submission text
  INSTRUEXTRACT$submission_text <- unlist(TEST["string(/affair/texts/text[2]/value)"])
  # h) Policy areas
  INSTRUEXTRACT$policy_area <- unlist(TEST["string(/affair/additionalIndexing)"])
  #i ) Legislative period
  INSTRUEXTRACT$leg_period <- unlist(TEST["string(/affair/deposit/legislativePeriod)"])

  tryCatch(INSTRUMENTS <- smartbind(INSTRUMENTS, INSTRUEXTRACT), error=function(e) ALLINSTRU) # if there is nothing to be added this creates an error. Therefore, in case of error, just keep df_total
  
  setTxtProgressBar(pb, i)
}
close(pb)

rm(x, TESTEXTRACT, SOMEINSTRU, pb, INSTRUEXTRACT, i, pagenumber, TEST, url)


# 3) Reduce the new INSTRUMENTS data frame to only entries referring to MPs.
# Note: Rows without information on MPs are blank in the id / name cell.
# These instruments are submitted by either party groups, committees, or refer to government communication.

# Drop rows if id_ch_parliament is missing.
# Key element here: grepl for subsetting data
INSTRUMENTS <- INSTRUMENTS[which(grepl("[0-9]",INSTRUMENTS$id_ch_parliament)==T),]


# 4) Fix dates. Make them readable for R.
INSTRUMENTS$submission_date <- as.Date(INSTRUMENTS$submission_date , format=c("%Y-%m-%d"))

# 5) Keep only instruments from 2016
INSTRUMENTS <- INSTRUMENTS[which(grepl("2016",as.character(INSTRUMENTS$submission_date), fixed = T)==T),]


####################################
# Connect INSTRUMENTS to PDBD Data #
####################################

# Add pers_id (PDBD-internal identifiers) from POLI to INSTRUMENTS
# Note: We use id_ch_parliament to connect the two data sources.
#       You could also take more variables at the same time from POLI at this stage using POLI.pers_id, POLI.gender
INSTRUMENTS <- sqldf("SELECT POLI.pers_id, INSTRUMENTS.*
                    FROM INSTRUMENTS LEFT JOIN POLI
                    ON
                    ( INSTRUMENTS.id_ch_parliament = POLI.id_ch_parliament )
                    ")


##########################################
# Generate Data on the Level of MP-Month #
##########################################

# Goal: We're now going to create a data frame on the level of MP-month.
#       This is a possible unit of analysis on which models could be run.
#       For this part, we assume that we're interested in the total number of submitted instruments.

# 1) Create a month variable
#    Note: We'll also need this information later on again in other data frames from which we add information to our main df
INSTRUMENTS$month <- str_extract(as.character(INSTRUMENTS$submission_date), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits


# 2) Obtain total sums
# a) Questions asked per month by MP
# Note: This is from R Tidyverse
INSTRUMENTS %>%
  group_by(pers_id, month) %>%
  mutate(total_instruments=n()) -> MPINSTRUMENTS


# b) Retain only three variables: pers_id, month, and total_instruments
MPINSTRUMENTS = subset(MPINSTRUMENTS, select = c(pers_id,month,total_instruments))

# 3) Reduce MPINSTRUMENTS to unique rows
MPINSTRUMENTS <- sqldf("SELECT DISTINCT MPINSTRUMENTS.*
                       FROM MPINSTRUMENTS
                    ")


# Alternative, faster solution for steps 2 and 3:
MPINSTRUMENTS <- sqldf("SELECT DISTINCT pers_id,month , COUNT(pers_id) as 'total_instruments' 
                       FROM INSTRUMENTS 
                       GROUP BY pers_id, month ")

###########################################
# Enrich the New MPINSTRUMENTS Data Frame #
###########################################

# There are different types of information that can be added from PDBD to MPINSTRUMENTS:

# 1) Time-invariant information: This is the easiest to extract from PDBD because no further
#                                data processing is required from the researcher.
#                                Example: gender

# 2) Time-variant stationary:    For these variables, the time point of the measurement matters.
#                                Example: party membership

# 3) Time-variant cumulative:    For duration variables, the time point of measurement also matter.
#                                To obtain the relevant information, it is necessary to calculate
#                                the new measures for a defined time point / period.
#                                Example: tenure


#################################
# Gender Added to MPINSTRUMENTS #
#################################

# Add gender from POLI to MPINSTRUMENTS
MPINSTRUMENTS <- sqldf("SELECT MPINSTRUMENTS.*, POLI.gender
                    FROM MPINSTRUMENTS LEFT JOIN POLI
                    ON
                    ( MPINSTRUMENTS.pers_id = POLI.pers_id )
                    ")


# Plot the data using ggplot
ggplot(MPINSTRUMENTS, aes(x=gender, y=total_instruments)) + 
  geom_boxplot() +
  scale_y_continuous("Number of instruments", position = "left", expand = c(0,0),
                     breaks=seq(0,20,5), limits = c(0, (max(MPINSTRUMENTS$total_instruments)+1))) +                     
  scale_x_discrete("Gender", position = "bottom",
                   labels=c('f'  = 'Women', 'm'  = 'Men')) +
  theme_minimal()


###########################################
# Party Membership Added to MPINSTRUMENTS #
###########################################

# For this variable, we will rely on the MEME data frame.

# 1) We reduce MEME to only relevant politicians.
# Note: Relevant are those MPs who are in our MPINSTRUMENTS sample.
# a) Generate a vector of all relevant MPs
relevantmps <- as.character(unique(MPINSTRUMENTS$pers_id))

# b) Keep only relevant MPs in MEME
# Note: [other author] calls this MEMEBU (MEME build up)
MEMECH <- MEME[MEME$pers_id %in% relevantmps, ]


# 2) Transform dates in the PDBD format to R dates
# For more information on date formats: https://www.r-bloggers.com/date-formats-in-r/
MEMECH$memep_startdate <- as.Date(MEMECH$memep_startdate, format=c("%d%b%Y")) # start dates
MEMECH$memep_enddate <- as.Date(MEMECH$memep_enddate, format=c("%d%b%Y")) # end dates

# 3) Remove rows that refer only to an election date.
# Note: These entries have only a start date in the Swiss case.
MEMECH <- MEMECH[which(!is.na(MEMECH$memep_enddate)),]


# 4) Transpose MEMECH to long format
# Note: 1) This is needed for aggregation purposes since MPs might switch parties during an observation month.
#       2) This also helps remove duplicate episodes, and detect overlapping episodes.

# Note: The function breaks if the start date is after the end date
MEMECHLONG <- setDT(MEMECH)[ , list(pers_id = pers_id, party_id = party_id,
            day = seq(as.Date(memep_startdate), as.Date(memep_enddate), by = "day")), by = 1:nrow(MEMECH)]

MEMECHLONG$nrow <- NULL # drop nrow

# 5) Get rid of duplicated rows
MEMECHLONG <- MEMECHLONG[!duplicated(MEMECHLONG),]

# 6)  Order the data first according to "pers_id" and then to "day"
MEMECHLONG <-  with(MEMECHLONG, MEMECHLONG[order(pers_id, day) , ])

# 7) Now, we prepare MEMECHLONG for aggregation on the MP-month level
# To that end, we generate a month variable like we did before.
MEMECHLONG$month <- str_extract(as.character(MEMECHLONG$day), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits

# 8) Obtain the mode for party membership for every MP-month in the MEMECHLONG
# a) Define a function that gets us the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode

# b) Get the party_id mode by groups (pers_id, month)
# Note: This is a decision we take. It's an alternative to taking the first day of the month etc.
MEMECHMONTH <- aggregate(MEMECHLONG$party_id, by = list(pers_id = MEMECHLONG$pers_id, month = MEMECHLONG$month), getmode)
colnames(MEMECHMONTH)[match("x",colnames(MEMECHMONTH))] <- "party_id" # rename variable x to party_id


# 9) Finally, add party_id to MPINSTRUMENTS
MPINSTRUMENTS <- sqldf("SELECT MPINSTRUMENTS.*, MEMECHMONTH.party_id
                    FROM MPINSTRUMENTS LEFT JOIN MEMECHMONTH
                       ON
                       ( MPINSTRUMENTS.pers_id = MEMECHMONTH.pers_id )
                       AND
                       ( MPINSTRUMENTS.month = MEMECHMONTH.month )
                       ")

# 10) Delete some data frames to save space
gc()
rm(MEMECH, MEMECHLONG, MEMECHMONTH)
gc()


# 11) Have a look at the use of instrument use by party
# a) Use national-level party_ids
MPINSTRUMENTS$party_id_national <- str_replace(MPINSTRUMENTS$party_id,"RE-[A-Z]{2}$","NT") #create a label for national parties (relevant for Switzerland)

# b) Plot the data with a bar plot
ggplot(MPINSTRUMENTS, aes(x=party_id_national, y=total_instruments)) + 
  geom_bar(stat="identity", fill="steelblue") +
  scale_y_continuous("Total instruments") +
  scale_x_discrete("Party", position = "bottom",
                   labels=c('CH_FDPLib_NT'  = 'FDP', 'CH_SVP|UDC_NT'  = 'SVP', 'CH_none_NT' = 'no affiliation',
                   'CH_SP|PS_NT' = 'SP', 'CH_CVP|PDC_NT' = 'CVP', 'CH_CsP|PCS_NT' = 'CSP',
                   'CH_GLP|PVL_NT' = 'GLP', 'CH_Grue_NT' = 'GP', 'CH_LPS_NT' = 'LDP', 'CH_LdT_NT' = 'Lega',
                   'CH_MCR_NT' = 'MCR', 'CH_EVP|PEP_NT' = 'EVP', 'CH_BastA_NT' ='BastA', 'CH_BDP_NT' = 'BDP')) +
  theme_minimal()

#################################
# Tenure Added to MPINSTRUMENTS #
#################################

# For this variable, we will rely on the RESE data frame.

# 1) We reduce RESE to only relevant politicians.
# Note: Relevant are those MPs who are in our MPINSTRUMENTS sample.
# a) Recall that we previously generated a vector of all relevant MPs
# relevantmps <- as.character(unique(MPINSTRUMENTS$pers_id))

# b) Keep only relevant MPs in MEME
RESECH <- RESE[RESE$pers_id %in% relevantmps, ]

# 2) We further reduce RESECH to only include information on parliamentary membership on the national level
RESECH <- RESECH[which(RESECH$pf_geolevel == "NT" & 
                         (RESECH$pf_instdomain == "LE" | RESECH$pf_instdomain == "LE-LH") &
                         RESECH$pf_orglevel == "T3" & 
                         is.na(RESECH$pf_policy_area) & 
                         RESECH$pf_position == "01" 
                        ),]

# 3) Make the dates readable for R
# a) Remove information on left ("[[lcen]]") and right censoring ("[[rcen]]")
RESECH$res_entry_start <- gsub("[[lcen]]", "", RESECH$res_entry_start, fixed = TRUE)
RESECH$res_entry_end <- gsub("[[rcen]]", "", RESECH$res_entry_end, fixed = TRUE)

# b) Transform dates to R format
RESECH$res_entry_start <- as.Date(RESECH$res_entry_start, format=c("%d%b%Y"))
RESECH$res_entry_end <- as.Date(RESECH$res_entry_end, format=c("%d%b%Y"))

# c) Remove rows that are (partly) NA for start and/or end date.
# Note: This is no problem because tenure information is complete on the day level.
# a) ... NAs on res_entry_start or res_entry_end
RESECH <- RESECH[which(!is.na(RESECH$res_entry_start)),]
RESECH <- RESECH[which(!is.na(RESECH$res_entry_end)),]

# 4) Let's extract what chamber we're looking at: National Council (NR) or Council of States (SR)
RESECH$chamber <- as.factor(str_extract(RESECH$parliament_id, "NR|SR|BT|TK|SE|DE"))


# 5) As with party membership data, we now transpose the tenure data to long format
# Note: 1) This is needed for aggregation purposes since MPs might switch parties during an observation month.
#       2) This also helps remove duplicate episodes, and detect overlapping episodes.

# Note: The function breaks if the start date is after the end date
RESECHLONG <- setDT(RESECH)[ , list(pers_id = pers_id, chamber = chamber,
                                    day = seq(as.Date(res_entry_start), as.Date(res_entry_end), by = "day")), by = 1:nrow(RESECH)]

RESECHLONG$nrow <- NULL # drop nrow

# 6) Get rid of duplicated rows
RESECHLONG <- RESECHLONG[!duplicated(RESECHLONG),]

# 7)  Order the data first according to "pers_id" and then to "day"
RESECHLONG <-  with(RESECHLONG, RESECHLONG[order(pers_id, day) , ])

# 8) Now, we calculate tenure on a daily level
# To that end, we simply count (number) rows
# See also: https://stackoverflow.com/questions/12925063/numbering-rows-within-groups-in-a-data-frame
RESECHLONG$daysinparl <- ave(as.character(RESECHLONG$pers_id), as.character(RESECHLONG$pers_id), FUN = seq_along)

# 9) Calculate tenure
RESECHLONG$tenure  <- as.numeric(RESECHLONG$daysinparl)/365.2422 #365.2422 = Average duration of year

# a) Remove all the numbers after the dot (no rounding)
RESECHLONG$tenure  <- floor(RESECHLONG$tenure)

# 10) Now, we prepare RESECHLONG for aggregation on the MP-month level
# To that end, we generate a month variable like we did before.
RESECHLONG$month <- str_extract(as.character(RESECHLONG$day), "[0-9]{4}-[0-9]{2}") # [0-9]{4}-[0-9]{2} matches 4 digits followed by a hyphen and 2 digits

# 11) Obtain the mode for tenure for every MP-month in the RESECHLONG
# a) Get tenure mode by groups (pers_id, month) 
RESECHMONTH <- aggregate(RESECHLONG$tenure, by = list(pers_id = RESECHLONG$pers_id, month = RESECHLONG$month), getmode)
colnames(RESECHMONTH)[match("x",colnames(RESECHMONTH))] <- "tenure" # rename variable x to tenure


# 12) Finally, add tenure to MPINSTRUMENTS
MPINSTRUMENTS <- sqldf("SELECT MPINSTRUMENTS.*, RESECHMONTH.tenure
                    FROM MPINSTRUMENTS LEFT JOIN RESECHMONTH
                       ON
                       ( MPINSTRUMENTS.pers_id = RESECHMONTH.pers_id )
                       AND
                       ( MPINSTRUMENTS.month = RESECHMONTH.month )
                       ")


# 13) Also, remember that we also stored information on the parliamentary chamber.
# We will now also add this information to MPINSTRUMENTS.
# Get the chamber mode by groups (pers_id, month) 
RESECHMONTH2 <- aggregate(RESECHLONG$chamber, by = list(pers_id = RESECHLONG$pers_id, month = RESECHLONG$month), getmode)
colnames(RESECHMONTH2)[match("x",colnames(RESECHMONTH2))] <- "chamber" # rename x to chamber


# 14) Add chamber to MPINSTRUMENTS as well
MPINSTRUMENTS <- sqldf("SELECT MPINSTRUMENTS.*, RESECHMONTH2.chamber
                    FROM MPINSTRUMENTS LEFT JOIN RESECHMONTH2
                       ON
                       ( MPINSTRUMENTS.pers_id = RESECHMONTH2.pers_id )
                       AND
                       ( MPINSTRUMENTS.month = RESECHMONTH2.month )
                       ")



# 15) Delete some data frames to save space
gc()
rm(RESECH, RESECHLONG, RESECHMONTH, RESECHMONTH2, relevantmps)
gc()

# 16) Use a scatter plot for the data on tenure and instruments
ggplot(MPINSTRUMENTS, aes(x=tenure, y=total_instruments)) + 
  geom_point()+
  geom_smooth(method=lm) +
  scale_y_continuous("Total instruments") +
  scale_x_continuous("Tenure") +
  theme_minimal()


# Important: This comes with a pipeline type of nature. Updates are very easily implemented if the data are changed.

###################
###################
###             ###
### 2 Exercises ###
###             ###
###################
###################

##########################################
# Exercise 1: Adding PDBD Data to Tweets #
##########################################

# PDBD data can also be combined with social media data. In this exercise, participants are going to 
# add information on parliamentarians that are active on Twitter.
# Have a look at the following data set of mock Tweets of Bundestag MPs sent between 2015 and 2016.


# This generates the random Tweets data frame:
set.seed(1234)

TWEETS <- data.frame(
    # A random sample of MPs' Twitter_ids repeated 50,000 times
    twitter_id <- sample(POLI[POLI$pers_id %in% 
                  unique(as.character(RESE[RESE$pers_id %in% as.character(unique(RESE[which(RESE$parliament_id == "DE_NT-BT_2013" & 
                  RESE$res_entry_start == "22oct2013" &  RESE$res_entry_end == "23oct2017"),]$pers_id)),]$pers_id))
                , ]$twitter_id[!is.na(
                  POLI[POLI$pers_id %in% 
                  unique(as.character(RESE[RESE$pers_id %in% as.character(unique(RESE[which(RESE$parliament_id == "DE_NT-BT_2013" & 
                  RESE$res_entry_start == "22oct2013" &  RESE$res_entry_end == "23oct2017"),]$pers_id)),]$pers_id))
                       , ]$twitter_id
                )], 50000, replace = TRUE),
    
    # A random sample tweet_timestamps
    tweet_timestamp <- sample(seq(as.POSIXct('2015/01/01'), as.POSIXct('2016/12/31'), by="1 sec"), 50000),
    
    # Random Tweet texts
    text <- stri_rand_lipsum(50000, start_lipsum = F)
    )
    
    names(TWEETS) <- c("twitter_id", "tweet_timestamp", "text")
    rm(text, tweet_timestamp, twitter_id)
    # Shorten the "Tweets"
    string_fun <- function(x) {
      ul = unlist(strsplit(x, split = "\\s+"))[1:10]
      paste(ul,collapse=" ")
    }
    TWEETS$text <- sapply(as.character(TWEETS$text), string_fun)


# Inspect the data
View(TWEETS)

# Task: Adapt the procedure from the legislative instruments example to the Tweets
# 1) Add information on pers_id to TWEETS. There is a column twitter_id in POLI as well.
# 2) Count how many Tweets every MP sent per year, and store this information in a new data frame.
#    You need to adapt the INSTRUMENTS example from month to years.
# 3) Add information on MPs' gender (in POLI), party_id (in MEME), and tenure (from RESE) to the new data frame.
#    Use the mode as your aggregation strategy for the year level.


################################################################
# Exercise 2: Adding PDBD Information to a Specific Time Point #
################################################################

# Sometimes, we're not interested in aggregating data on parliamentarians, but would like to use
# exact measures for specific points in time. This is for instance the case with survey data.
# In our example, we pretend the following 50 German MPs participated in a survey. The survey was fielded on 01 December 2017.

# Have a look at the list of participants:
SURVPARTICIP = 
  as.data.frame(c('DE_Achelwilm_Doris_1976','DE_Aggelidis_Grigorios_1965','DE_Akbulut_Goekay_1982','DE_Aken_Jan_1961','DE_Albani_Stephan_1968',
                  'DE_Albsteiger_Katrin_1983','DE_Alt_Renata_1965','DE_Altenkamp_Norbert_1972','DE_Altmaier_Peter_1958','DE_Amthor_Philipp_1992',
                  'DE_Amtsberg_Luise_1984','DE_Andreae_Kerstin_1968','DE_Annen_Niels_1973','DE_Arndt_Ingrid_1961','DE_Arnold_Rainer_1950',
                  'DE_Aschenberg_Christine_1959','DE_Auernhammer_Artur_1963','DE_Aumer_Peter_1976','DE_Badum_Lisa_1983','DE_Baehr_Bettina_1967',
                  'DE_Baehrens_Heike_1955','DE_Baer_Dorothee_1978','DE_Baerbock_Annalena_1980','DE_Bahr_Ulrike_1964','DE_Barchmann_Heinz_1950',
                  'DE_Bareiss_Thomas_1975','DE_Barley_Katarina_1968','DE_Barnett_Doris_1953','DE_Barrientos_Simone_1963','DE_Barthel_Klaus_1955',
                  'DE_Barthle_Norbert_1952','DE_Bartke_Matthias_1959','DE_Bartol_Soeren_1974','DE_Bartsch_Dietmar_1958','DE_Bas_Baerbel_1968',
                  'DE_Bauer_Nicole_1987','DE_Baumann_Bernd_1958','DE_Baumann_Guenter_1947','DE_Bause_Margarete_1959','DE_Bayaz_Danyal_1983',
                  'DE_Bayram_Canan_1966','DE_Beck_Marieluise_1952','DE_Beck_Volker_1960','DE_Beckmeyer_Uwe_1949','DE_Beeck_Jens_1969',
                  'DE_Beer_Nicola_1970','DE_Beermann_Maik_1981','DE_Behrens_Herbert_1954','DE_Behrens_Manfred_1956','DE_Bellmann_Veronika_1960'))
  
colnames(SURVPARTICIP)[1] <- "pers_id"


# Task: Add information on MPs for 01 December 2017.
# Build on the procedure from the example / the previous exercise, and adapt it to survey data.

# 1) Add information on MPs' gender (in POLI), party_id (in MEME), and tenure (from RESE) 
#    to SURVPARTICIP for 01 December 2017.
# 2) Also calculate MPs' age on 01 December 2017
#    You can use the following function for that:
#    It accounts for leap years.
#    Source: http://r.789695.n4.nabble.com/Calculate-difference-between-dates-in-years-td835196.html
      age_years <- function(first, second)
      {
        lt <- data.frame(first, second)
        age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
        first <- as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep=""))
        age[which(first > lt[,2])] <- age[which(first > lt[,2])] - 1
        age
      } 
      
    # Format: age_years(older_date, new_date)
      
      