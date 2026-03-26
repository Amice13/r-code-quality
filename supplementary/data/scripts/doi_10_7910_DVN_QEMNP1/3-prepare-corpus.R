######################################################
# Policy Diffusion: The Issue-Definition Stage
# Fabrizio Gilardi, Charles R. Shipan & Bruno Wueest
# Prepare corpus
# 2019-11-18
######################################################

rm(list = ls())

library(tm)
library(parallel)
library(matrixStats)
library(XML)
options(stringsAsFactors = FALSE)
suppressMessages(library(stm))

setwd("/Users/fgilardi/Downloads/AJPS-final/")

# sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.1

# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] stm_1.3.4          XML_3.98-1.20      matrixStats_0.55.0 tm_0.7-6          
# [5] NLP_0.2-0         

# loaded via a namespace (and not attached):
# [1] compiler_3.6.1    Matrix_1.2-17     Rcpp_1.0.3        slam_0.1-46       xml2_1.2.2       
# [6] grid_3.6.1        data.table_1.12.6 lattice_0.20-38  

set.seed(0213)
lang <- "english"

# get a list of filenames for the files containing the names of the relevant paragraphs
filenames <- list.files("./relevant/", full.names = TRUE)

# merge all filelists into a data.frame
dataset <- data.frame()
for (file in filenames){
    temp_dataset <- read.table(file, header = FALSE, sep="\t", stringsAsFactors = F)
    dataset <- rbind(dataset, temp_dataset)
    rm(temp_dataset)
}

# for every file, add the newspaper name as well as the date as indicated in the filename
docs <- sapply(strsplit(dataset[,1], "/"), "[[", 5)

docs <- data.frame(year = sapply(strsplit(docs, "-"), "[[", 2), month = sapply(strsplit(docs, "-"), "[[", 3), path = dataset[,1], paper = sapply(strsplit(docs, "-"), "[[", 1), stringsAsFactors = FALSE)
docs$month <- paste(docs$year, docs$month, sep = "-")

head(docs)

# drop files from 2014 and 1995, since these years not available for all newspapers
docs <- subset(docs, year < 2014)
docs <- subset(docs, year > 1995)

# drop national newspapers from states where we already have another newspaper in the corpus
docs <- subset(docs, !paper %in% c("WashingtonPost", "NewYorkTimes", "USATODAY", "WallStreetJournal"))

# add the xml
for (i in 1:length(docs[,1])){
  docs$xml[i] <- readChar(docs$path[i], file.info(docs$path[i])$size)
}

# extract the text and collapse location n-grams
for (i in 1:length(docs[,1])){
  temp <- htmlParse(docs$xml[i], asText=TRUE)
  temp <- xpathSApply(temp, "//p", xmlValue)
  temp <- gsub("North Carolina", "NorthCarolina", temp) 
  temp <- gsub("North Dakota", "NorthDakota", temp) 
  temp <- gsub("New Hampshire", "NewHampshire", temp) 
  temp <- gsub("New Jersey", "NewJersey", temp) 
  temp <- gsub("New Mexico", "NewMexico", temp) 
  temp <- gsub("New York", "NewYork", temp) 
  temp <- gsub("Rhode Island", "RhodeIsland", temp) 
  temp <- gsub("South Carolina", "SouthCarolina", temp) 
  temp <- gsub("South Dakota", "SouthDakota", temp) 
  temp <- gsub("West Virginia", "WestVirginia", temp) 
  temp <- gsub("Los Angeles", "LosAngeles", temp) 
  temp <- gsub("San Francisco", "SanFrancisco", temp) 
  temp <- gsub("New Orleans", "NewOrleans", temp) 
  temp <- gsub("San Diego", "SanDiego", temp)
  temp <- gsub("Santa Fe", "SantaFe", temp)
  docs$text[i] <- paste(temp, collapse = "\n")
}

#preprocess texts
myCorpus <- VCorpus(VectorSource(docs$text))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- tm_map(myCorpus, stemDocument, lang)
for (i in 1:length(docs$text)) {
  docs$text2[i] <- gsub("[[:space:]]", " ", myCorpus[[i]])
}

#standardize newspaper names
docs$paper[docs$paper == "StPetersburgTimes"] <- "TampaBayTimes"
docs$paper[docs$paper == "TheBaltimoreSun2"] <- "BaltimoreSun"
docs$paper[docs$paper == "BismarckTribune2"] <- "BismarckTribune"
docs$paper[docs$paper == "TheBillingsGazette"] <- "BillingsGazette"
docs$paper[docs$paper == "TheClarionLedger"] <- "ClarionLedger"
docs$paper[docs$paper == "DailyNewsNewYork"] <- "DailyNews"
docs$paper[docs$paper == "DemokratGazette"] <- "DemocratGazette"
docs$paper[docs$paper == "DeseretNewsSaltLakeCity"] <- "DeseretNews"
docs$paper[docs$paper == "MilwaukeeJournalSentinel"] <- "JournalSentinel"
docs$paper[docs$paper == "PortlandPressHeraldMaine"] <- "PressHerald"
docs$paper[docs$paper == "TheBurlingtonFreePress"] <- "FreePress"
docs$paper[docs$paper == "TheOregonian"] <- "Oregonian"
docs$paper[docs$paper == "TheNewsJournal"] <- "NewsJournal"
docs$paper[docs$paper == "TheSeattleTimes"] <- "SeattleTimes"
docs$paper[docs$paper == "TheTimesPicayune"] <- "TimesPicayune"
docs$paper[docs$paper == "TheUnionLeader"] <- "UnionLeader"
docs$paper[docs$paper == "USATODAY"] <- "USAToday"
docs$paper[docs$paper == "WyomingTribuneEagle"] <- "TribuneEagle"
docs$paper[docs$paper == "TheTennessean"] <- "Tennessean"
docs$paper[docs$paper == "StLouisPostDispatchMissouri"] <- "StLouisPostDispatch"

docs_paper_names <- sort(unique(docs$paper))

# Read in sentiment data and merge with corpus
sentiment_scores_pro <- read.csv("./covariates/raw-data/sentiment.txt", header = FALSE, sep = "|", stringsAsFactors = FALSE) 

sentiment <- data.frame(path = sentiment_scores_pro[,1], sentiment = sentiment_scores_pro[,2])
docs <- merge(docs, sentiment, by = c("path"), all.x = TRUE)

# Add smoking ban covariates to corpus
covars <- read.csv("./covariates/covariates.csv", header = TRUE, stringsAsFactors = FALSE)

# covars_paper_names <- sort(unique(covars$newspaper))
# docs_paper_names %in% covars_paper_names

# Standardize newspaper names
covars$newspaper2 <- NA
covars$newspaper2[covars$newspaper == "Albuquerque Journal"] <- "AlbuquerqueJournal"
covars$newspaper2[covars$newspaper == "Anchorage Daily News"] <- ""
covars$newspaper2[covars$newspaper == "Argus Leader"] <- "ArgusLeader"
covars$newspaper2[covars$newspaper == "Arizona Republic"] <- "ArizonaRepublic"
covars$newspaper2[covars$newspaper == "Atlanta Journal-Constitution"] <- "AtlantaJournal"
covars$newspaper2[covars$newspaper == "Austin American-Statesman"] <- "AustinAmericanStatesman"
covars$newspaper2[covars$newspaper == "Baltimore Sun"] <- "BaltimoreSun"
covars$newspaper2[covars$newspaper == "Billings Gazette"] <- "BillingsGazette"
covars$newspaper2[covars$newspaper == "Birmingham News"] <- "BirminghamNews"
covars$newspaper2[covars$newspaper == "Bismarck Tribune"] <- "BismarckTribune"
covars$newspaper2[covars$newspaper == "Boston Globe"] <- "BostonGlobe"
covars$newspaper2[covars$newspaper == "Burlington Free Press"] <- "FreePress"
covars$newspaper2[covars$newspaper == "Charleston Gazette-Mail"] <- "CharlestonGazette"
covars$newspaper2[covars$newspaper == "Chicago Tribune"] <- "ChicagoTribune"
covars$newspaper2[covars$newspaper == "Clarion-Ledger"] <- "ClarionLedger"
covars$newspaper2[covars$newspaper == "Courier-Journal"] <- "CourierJournal"
covars$newspaper2[covars$newspaper == "Daily News"] <- "DailyNews"
covars$newspaper2[covars$newspaper == "Democrat-Gazette"] <- "DemocratGazette"
covars$newspaper2[covars$newspaper == "Denver Post"] <- "DenverPost"
covars$newspaper2[covars$newspaper == "Des Moines Register"] <- "DesMoinesRegister"
covars$newspaper2[covars$newspaper == "Deseret News"] <- "DeseretNews"
covars$newspaper2[covars$newspaper == "Detroit Free Press"] <- "DetroitFreePress"
covars$newspaper2[covars$newspaper == "Hartford Courant"] <- "HartfordCourant"
covars$newspaper2[covars$newspaper == "Honolulu Star-Advertiser"] <- "HonoluluAdvertiser"
covars$newspaper2[covars$newspaper == "Idaho Falls Post Register"] <- "IdahoFallsPostRegister"
covars$newspaper2[covars$newspaper == "Indianapolis Star"] <- "IndianapolisStar"
covars$newspaper2[covars$newspaper == "Journal Sentinel"] <- "JournalSentinel"
covars$newspaper2[covars$newspaper == "Las Vegas Review-Journal"] <- "LasVegasReviewJournal"
covars$newspaper2[covars$newspaper == "Los Angeles Times"] <- "LosAngelesTimes"
covars$newspaper2[covars$newspaper == "New York Times"] <- "NewYorkTimes"
covars$newspaper2[covars$newspaper == "Oklahoman"] <- "DailyOklahoman"
covars$newspaper2[covars$newspaper == "Omaha World-Herald"] <- "OmahaWorldHerald"
covars$newspaper2[covars$newspaper == "Oregonian"] <- "Oregonian"
covars$newspaper2[covars$newspaper == "Philadelphia Inquirer"] <- "PhiladelphiaInquirer"
covars$newspaper2[covars$newspaper == "Plain Dealer"] <- "DaytonDailyNews"
covars$newspaper2[covars$newspaper == "Portland Press Herald"] <- "PressHerald"
covars$newspaper2[covars$newspaper == "Providence Journal"] <- "ProvidenceJournal"
covars$newspaper2[covars$newspaper == "Record"] <- "Record"
covars$newspaper2[covars$newspaper == "Richmond Times-Dispatch"] <- "RichmondTimesDispatch"
covars$newspaper2[covars$newspaper == "Seattle Times"] <- "SeattleTimes"
covars$newspaper2[covars$newspaper == "St.Louis Post-Dispatch"] <- "StLouisPostDispatch"
covars$newspaper2[covars$newspaper == "Star Tribune"] <- "StarTribuneMinneapolis"
covars$newspaper2[covars$newspaper == "State"] <- "PostAndCourier"
covars$newspaper2[covars$newspaper == "Tampa Bay Times"] <- "TampaBayTimes"
covars$newspaper2[covars$newspaper == "Tennessean"] <- "Tennessean"
covars$newspaper2[covars$newspaper == "The News Journal"] <- "NewsJournal"
covars$newspaper2[covars$newspaper == "Times-Picayune"] <- "TimesPicayune"
covars$newspaper2[covars$newspaper == "Topeka Capital-Journal"] <- "TopekaCapitalJournal"
covars$newspaper2[covars$newspaper == "Union Leader"] <- "UnionLeader"
covars$newspaper2[covars$newspaper == "USA Today"] <- "USAToday"
covars$newspaper2[covars$newspaper == "Wall Street Journal"] <- "WallStreetJournal"
covars$newspaper2[covars$newspaper == "Washington Post"] <- "WashingtonPost"
covars$newspaper2[covars$newspaper == "Wilmington Star-News"] <- "WilmingtonStarNews"
covars$newspaper2[covars$newspaper == "Wyoming Tribune Eagle"] <- "TribuneEagle"

covars_paper_names_2 <- sort(unique(covars$newspaper2))
docs_paper_names %in% covars_paper_names_2


# Merge
docs_merged <- merge(docs, covars, by.x = c("paper", "month"), by.y = c("newspaper2", "month"), all.x = TRUE)

#generate a corpus for the stm and save it for later uses
corpus <- textProcessor(docs_merged[,c("text2")], docs_merged[,c("sentiment", "month", "paper", "smokers", "unifiedDemocrats", "unifiedRepublicans", "shareDemLow", "producer", "slant", "Policy_Enacted_This_Month", "Policy_Months_Before_After_Enacted", "Policy_Spatial_Lag_All", "Policy_Spatial_Lag_Neighbors", "Policy_Spatial_Lag_Network_Pcent", "text", "path")], lowercase = FALSE, removenumbers = FALSE, removepunctuation = FALSE, stem = FALSE, customstopwords = c("said", "say", "one", "can", "will"))


corpus <- prepDocuments(corpus$documents, corpus$vocab, corpus$meta, lower.thresh = 1)
corpus$meta$month <- as.numeric(as.factor(corpus$meta$month))

save(corpus, file = "./corpus/corpus_txt.RData")

