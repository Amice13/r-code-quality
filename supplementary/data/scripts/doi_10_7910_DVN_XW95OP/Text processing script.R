## Mitchell Ransden
## Calibrating the Mudslinger Replication Code
## Statement Collection and Processing Script
## Last edited: 4-27-2023

## Relevant packages----
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(plyr)
library(corpus)
library(writexl)

## Twitter statement collection (requires own API access)----
# Access the Twitter API (as of January 2023; access requirements may have changed in the intervening time)
bearer_token <- "[your API bearer token]"
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

# Function to retrieve prespecified number of tweets from an account
# Adapted from https://rpubs.com/cschwarz/smappLab3
last_n_tweets <- function(bearer_token = "", user_id = "", 
                          end_time = "", n,
                          tweet_fields = c("attachments",
                                           "created_at",
                                           "entities",
                                           "in_reply_to_user_id",
                                           "public_metrics",
                                           "referenced_tweets",
                                           "source")){
  
  headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))
  
  # Convert User ID into Numerical ID
  
  sprintf('https://api.twitter.com/2/users/by?usernames=%s', user_id) %>% 
    httr::GET(url = .,
              httr::add_headers(.headers = headers),
              query = list()) %>% 
    httr::content(.,as="text") %>% 
    fromJSON(.,flatten = T) %>% 
    as.data.frame() -> tmp
  
  num_id <- tmp$data.id
  
  # For that user, grab most recent n tweets, in batches of 100
  
  if(n <= 100){
    requests <- n
  }else{
    requests <- rep(100,floor(n/100))
    if(n %% 100 != 0){
      requests <- c(requests, n %% 100)
    }
  }
  
  next_token <- NA
  
  all <- list()
  
  # Initialize, grab first results  
  paste0('https://api.twitter.com/2/users/',num_id,'/tweets') %>% 
    httr::GET(url = .,
              httr::add_headers(.headers = headers),
              query = list(`max_results` = requests[1],
                           tweet.fields = paste(tweet_fields,collapse=","),
                           `end_time` = paste0(end_time, 'T23:59:59.000Z'))) %>% 
    httr::content(.,as="text") %>% 
    fromJSON(.,flatten = T) %>% 
    as.data.frame() -> out
  
  all[[1]] <- out 
  
  
  # For more than 100, need to use pagination tokens.
  if(length(requests) >= 2){
    next_token[2] <- unique(as.character(all[[1]]$meta.next_token))
    
    for(i in 2:length(requests)){
      paste0('https://api.twitter.com/2/users/',num_id,'/tweets') %>% 
        httr::GET(url = .,
                  httr::add_headers(.headers = headers),
                  query = list(`max_results` = requests[i],
                               tweet.fields = paste(tweet_fields,collapse=","),
                               `end_time` = paste0(end_time, 'T23:59:59.000Z'), # added to provide "start point" for tweet retrieval
                               pagination_token = next_token[i])) %>% 
        httr::content(.,as="text") %>% 
        fromJSON(.,flatten = T) %>% 
        as.data.frame() -> out
      
      all[[i]] <- out
      
      next_token[i + 1] <- unique(as.character(all[[i]]$meta.next_token))
    }
  }
  
  do.call("rbind.fill",all)
  
}

# retrieving individual candidate accounts (combined in Excel to form master list)
# accounts with more than 3200 tweets are scraped using ScrapeHero and Octoparse
pingree <- last_n_tweets(bearer_token, "chelliepingree", "2020-11-04", 3200)
golden <- last_n_tweets(bearer_token, "golden4congress", "2018-11-06",365)
poliquin <- last_n_tweets(bearer_token, "BrucePoliquin", "2018-11-06", 1400)
adams <- last_n_tweets(bearer_token, "ericadamsfornyc", "2021-06-23", 1000)
garcia <- last_n_tweets(bearer_token, "KGforNYC", "2021-06-23", 850)
stringer <- last_n_tweets(bearer_token, "scottmstringer", "2021-06-23", 360)
ringelstein <- last_n_tweets(bearer_token, "RingZak", "2018-11-06", 170)
holbrook <- last_n_tweets(bearer_token, "MarkHolbrookME", "2018-11-06", 100)
collins1 <- last_n_tweets(bearer_token, "SenatorCollins", "2020-11-04", 130)
collins2 <- last_n_tweets(bearer_token, "SenSusanCollins", "2020-11-04", 410)
gideon <-  last_n_tweets(bearer_token, "SaraGideon", "2020-11-04", 600)
savage <- last_n_tweets(bearer_token, "NaturalGuard", "2020-11-04", 100)
frey1 <- last_n_tweets(bearer_token, "Jacob_Frey", "2021-11-03", 100)
frey2 <- last_n_tweets(bearer_token, "MayorFrey", "2021-11-03", 100)
nezhad <- last_n_tweets(bearer_token, "SheilaFTP", "2021-11-03", 275)
knuth <- last_n_tweets(bearer_token, "kateknuth", "2021-11-03", 400)
awed <- last_n_tweets(bearer_token, "ajawedmpls", "2021-11-03", 240)
craftsgen <- last_n_tweets(bearer_token, "DaleCraftsME", "2020-11-04", 110)
golden1 <- last_n_tweets(bearer_token, "golden4congress", "2020-11-04", 195)
golden2 <- last_n_tweets(bearer_token, "RepGolden", "2020-11-04", 95)
sliwa1 <-  last_n_tweets(bearer_token, "CurtisSliwa", "2021-06-23", 300)
sliwa2 <-  last_n_tweets(bearer_token, "SliwaforMayor", "2021-06-23", 65)
strimling <- last_n_tweets(bearer_token, "EthanForMayor", "2019-11-06", 65)
thibodeau <- last_n_tweets(bearer_token, "SpencerTfromME", "2019-11-06", 60)
golden3 <- last_n_tweets(bearer_token, "golden4congress", "2018-06-21", 200)
stclair <- last_n_tweets(bearer_token, "Lucasstclair", "2018-06-21", 200)
carter1 <- last_n_tweets(bearer_token, "MayorCarter", "2021-11-03", 200)
carter2 <- last_n_tweets(bearer_token, "melvincarter3", "2021-11-03", 300)
hosko <- last_n_tweets(bearer_token, "VoteBillHosko", "2021-11-03", 110)
johnson <- last_n_tweets(bearer_token, "alexisjohnsonnm", "2021-11-03", 110)
webber1 <- last_n_tweets(bearer_token, "MayorWebber", "2021-11-03", 110)
webber2 <- last_n_tweets(bearer_token, "alanforsantafe", "2021-11-03", 110)
brakey1 <- last_n_tweets(bearer_token, "SenatorBrakey", "2020-07-15", 100)
craftsGOP <- last_n_tweets(bearer_token, "DaleCraftsME", "2020-07-15", 100)
bennett <- last_n_tweets(bearer_token, "AdrienneMaine", "2020-07-15", 250)
dehn <- last_n_tweets(bearer_token, "raymonddehn", "2017-11-08", 500)
hodges1 <- last_n_tweets(bearer_token, "BetsyHodges", "2017-11-08", 300)
frey3 <- last_n_tweets(bearer_token, "Jacob_Frey", "2017-11-08", 100)
rahman <- last_n_tweets(bearer_token, "aswarformayor", "2017-11-08", 10)
weiss <- last_n_tweets(bearer_token, "AmyFarahWeiss", "2015-11-06", 100)
kolstad <- last_n_tweets(bearer_token, "PapaJohnKolstad", "2009-11-10", 100)
herrera <- last_n_tweets(bearer_token, "dennisherrera", "2011-11-10", 400)
baum <- last_n_tweets(bearer_token, "BaumforMayor", "2011-11-10", 150)
yee <- last_n_tweets(bearer_token, "LelandYee", "2011-11-10", 800)
pier <- last_n_tweets(bearer_token, "Michela_AliotoP", "2011-11-10", 30)
thao <- last_n_tweets(bearer_token, "DaiThao1", "2017-11-10", 100)
dickinson <- last_n_tweets(bearer_token, "DickinsonStPaul", "2017-11-10", 200)
carter3 <- last_n_tweets(bearer_token, "melvincarter3", "2017-11-10", 400)
alioto <- last_n_tweets(bearer_token, "AngelaForMayor", "2018-11-10", 300)
arreguin1 <- last_n_tweets(bearer_token, "JesseArreguin", "2020-11-03", 400)
arreguin2 <- last_n_tweets(bearer_token, "JAforBerkeley", "2020-11-03", 50)
hsiung <- last_n_tweets(bearer_token, "waynehhsiung", "2020-11-03", 100)
hill <- last_n_tweets(bearer_token, "AidanVote", "2020-11-03", 200)
arreguin3 <- last_n_tweets(bearer_token, "JesseArreguin", "2016-11-05", 50)
arreguin4 <- last_n_tweets(bearer_token, "JAforBerkeley", "2016-11-05", 50)
capitelli <- last_n_tweets(bearer_token, "berkcap", "2016-11-05", 100)
gould <- last_n_tweets(bearer_token, "bgould4berkeley", "2016-11-05", 100)
coleman <- last_n_tweets(bearer_token, "ChrisColemanMN", "2013-11-05", 100)

# Additional Candidates (no debate presence)
morales <- last_n_tweets(bearer_token, "Dianne4NYC", "2021-06-23", 500)
mcguire <- last_n_tweets(bearer_token, "RayJMcGuire", "2021-06-23", 900)
donovan <- last_n_tweets(bearer_token, "ShaunDonovanNYC", "2021-06-23", 510)
foldenauer <- last_n_tweets(bearer_token, "aaronfoldenauer", "2021-06-23", 300)
chang <- last_n_tweets(bearer_token, "achangnyc", "2021-06-23", 1050)
taylor <- last_n_tweets(bearer_token, "JajaTaylor2022", "2021-06-23", 700)
wright1 <- last_n_tweets(bearer_token, "IsaacWrightJr", "2021-06-23", 50)
wright2 <- last_n_tweets(bearer_token, "wrightfornyc", "2021-06-23", 50)
brennan <- last_n_tweets(bearer_token, "reelectbrennan", "2015-11-10", 100)
gers <- last_n_tweets(bearer_token, "CharlieGers", "2017-11-10", 50)
nik <- last_n_tweets(bearer_token, "RockerLANik", "2017-11-10", 50)
roth <- last_n_tweets(bearer_token, "TheITSystem", "2018-11-10", 50)
atkins <- last_n_tweets(bearer_token, "realnateatkins", "2021-11-10", 100)
perry <- last_n_tweets(bearer_token, "MayorPerry4Mpls", "2021-11-10", 100)

# Oakland 2022 Mayoral
thao2 <- last_n_tweets(bearer_token, "MayorShengThao", "2022-11-10", 1000)
taylor1 <- last_n_tweets(bearer_token, "lorenmtaylor", "2022-11-10", 1000)
fuente <- last_n_tweets(bearer_token, "IgnacioforMayor", "2022-11-10", 69)
victory <- last_n_tweets(bearer_token, "Victory4Oakland", "2022-11-10", 500)
reid <- last_n_tweets(bearer_token, "reidforoakland", "2022-11-10", 120)
hodge <- last_n_tweets(bearer_token, "hodgeforoakland", "2022-11-10", 1400)
scott <- last_n_tweets(bearer_token, "SenecaSpeaks21", "2022-11-10", 2200)
reimann <- last_n_tweets(bearer_token, "oklndsocialist", "2022-11-10", 100)
jordan1 <- last_n_tweets(bearer_token, "TyronJordan13", "2022-11-10", 1000)

# Santa Fe Mayoral 2018
webber3 <- last_n_tweets(bearer_token, "MayorWebber", "2018-11-07", 500)
maestas <- last_n_tweets(bearer_token, "Maestas4NM", "2018-11-07", 5)

#Oakland Mayoral 2018
price <- last_n_tweets(bearer_token, "PPriceCares", "2018-11-07", 100)
karamooz <- last_n_tweets(bearer_token, "SAiEDrk", "2018-11-07", 100)
houston <- last_n_tweets(bearer_token, "KHouston4Mayor", "2018-11-07", 100)
tatmon <- last_n_tweets(bearer_token, "therealmtat1", "2018-11-07", 100)

# Maine 2022 1st District 
pingree3 <- last_n_tweets(bearer_token, "PingreeForME", "2022-11-09", 100)
pingree4 <- last_n_tweets(bearer_token, "chelliepingree", "2022-11-09", 300)
thelander1 <- last_n_tweets(bearer_token, "EdForMaine", "2022-11-09", 500)

# Maine 2022 2nd District GOP Primary
poliquin5 <- last_n_tweets(bearer_token, "BrucePoliquin", "2022-06-14", 500)

# Maine 2022 2nd District GOP General
poliquin6 <- last_n_tweets(bearer_token, "BrucePoliquin", "2022-11-08", 500)
poliquin7 <- last_n_tweets(bearer_token, "RepPoliquin", "2022-11-08", 500)
bond2 <- last_n_tweets(bearer_token, "TiffanyBond", "2022-11-08", 1500)
golden4 <- last_n_tweets(bearer_token, "golden4congress", "2022-11-08", 300)
golden5 <- last_n_tweets(bearer_token, "RepGolden", "2022-11-08", 300)

## Ad data collection (SEE NOTE)----
# Please note that the user agreement to access WMP/CMAG advertising data prohibits
# providing access to the data, even for replication. The data I present here are 
# modified in order to comply with these requirements.
# To access to the full dataset, please visit https://mediaproject.wesleyan.edu/dataaccess/

# Read in modified WMP/CMAG data
house_ads_18  <- read_xlsx("Data/Ads/ME Second District Ads.xlsx")
senate_ads_18 <- read_xlsx("Data/Ads/ME 2018 Senate Ads.xlsx")
senate_ads_20 <- read_xlsx("Data/Ads/ME 2020 Senate Ads.xlsx")

# Create tables of advertisements by tone for each race
house_ads_18_table <- data.frame(prop.table(table(house_ads_18$sponsorcmag, house_ads_18$tonecmag),1)) %>%
  mutate(Var2 = case_when(Var2 == "NEGATIVE" ~ "Negative",
                          Var2 == "CONTRAST" ~ "Contrast",
                          Var2 == "POSITIVE" ~ "Positive"))

senate_ads_20_table <- data.frame(prop.table(table(senate_ads_20$sponsorcmag, senate_ads_20$tonecmag),1)) %>%
  mutate(Var2 = case_when(Var2 == "NEGATIVE" ~ "Negative",
                          Var2 == "CONTRAST" ~ "Contrast",
                          Var2 == "POSITIVE" ~ "Positive"))

# relevel variables
house_ads_18_table$Var2 <- factor(house_ads_18_table$Var2, levels = c("Negative","Contrast","Positive"))
senate_ads_20_table$Var2 <- factor(senate_ads_20_table$Var2, levels = c("Negative","Contrast","Positive"))

# Plot with ggplot and create figures
# Figure 3.4
house_ads_18_plot <- ggplot(data = house_ads_18_table, aes(x=Var1, y=Freq, group = Var2, fill =Var2 )) + 
  geom_bar(stat="identity",position = "dodge") + scale_fill_grey() + theme_bw() + 
  labs(x="Candidate", y="Proportion", color = "white") + labs(fill = " ") + 
  theme(text = element_text(size = 24)) + scale_x_discrete(labels=c("Golden", "Poliquin")) + 
  theme(legend.position = "top") + ylim(0,1)

# Figure 3.2
senate_ads_20_plot <- ggplot(data = ads_20_table, aes(x=Var1, y=Freq, group = Var2, fill =Var2 )) + 
  geom_bar(stat="identity",position = "dodge") + scale_fill_grey() + theme_bw() + 
  labs(x="Candidate", y="Proportion", color = "black") + labs(fill = " ") + 
  theme(text = element_text(size = 24)) + scale_x_discrete(labels=c("Collins", "Gideon", "Linn", "Savage")) + 
  theme(legend.position = "top")



# Nexis Uni plot (Figure 3.1)----
library(readxl)
nexis <- read_xlsx("Data/Nexis Uni RCV search by year.xlsx") %>%
  mutate(Year=as.character(Year))
nexis_plot <-  ggplot(data = nexis, aes(x=Year, y=`Total number of articles`)) + 
  geom_bar(stat="identity") + theme_bw() + scale_y_continuous(n.breaks=20) + 
  theme(text = element_text(size=16)) + geom_text(aes(label = `Total number of articles`), vjust = -0.2)

# end of file----