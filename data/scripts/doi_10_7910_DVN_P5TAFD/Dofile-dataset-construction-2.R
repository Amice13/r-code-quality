#############################################################################
### DO FILE FOR REPLICATION OF:                                           ###
### Winning Hearts and Minds in Civil Wars:                               ###
### Governance, Leadership Change, and Support for Violent Groups in Iraq ###
### (by Christoph Mikulaschek, Saurabh Pant, and Beza Tesfaye)            ###
############################################################################# 

# This do file makes it possible to replicate how we merged conflict data from the UCDP's GED and the START's GTD2 into our dataset. Run the STATA do file prior to running this do file. 

#######################################
### PREPARING DATASETS FOR ANALYSIS ###
#######################################

### Install relevant packages

install.packages('foreign')
install.packages('doBy')

### Load data

responses.all <- read.csv("responsesall.csv", header = TRUE)

### Exclude survey responses.all that were provided at an unreasonable speed (see fn. 4)

responses.all <- responses.all[responses.all$minutes > 12,]

### Merge in violence data from the UCDP's GED

library(foreign)
ucdp <- read.dta("UCDPGED.dta") # see Dofile.do on compilation of this dataset
days <- read.dta("days.dta") # this is an empty dataset with one observation per day per governorate (province): from May 1 to Sept. 30

ucdp$gov.fe <- ucdp$govfe # rename variable
ucdp$govfe <- NULL
days$gov.fe <- days$govfe # rename variable
days$govfe <- NULL

ucdp <- ucdp[order(ucdp$gov.fe, ucdp$mo, ucdp$day),]

ucdp <- merge(days, ucdp, by = c("mo", "day", "gov.fe"), all.x = TRUE, sort = FALSE) # merge UCDP data into empty data set

ucdp <- ucdp[order(ucdp$gov.fe, ucdp$mo, ucdp$day),]

ucdp$day.nr <- ifelse(ucdp$mo == 5, ucdp$day - 7,
                      ifelse(ucdp$mo == 6, ucdp$day - 7 + 31,
                             ifelse(ucdp$mo == 7, ucdp$day - 7 + 31 + 30,
                                    ifelse(ucdp$mo == 8, ucdp$day - 7 + 31 + 30 + 31,
                                           ucdp$day - 7 + 31 + 30 + 31 + 31)))) # code day.nr variable (see Codebook for details)

ucdp$deaths_a_adj[is.na(ucdp$deaths_a_adj) == TRUE] <- 0 # replace missing values (i.e., governorate-day observations with 0 casualties) with 0 values
ucdp$deaths_b_adj[is.na(ucdp$deaths_b_adj) == TRUE] <- 0
ucdp$deaths_civilians_adj[is.na(ucdp$deaths_civilians_adj) == TRUE] <- 0
ucdp$best_adj[is.na(ucdp$best_adj) == TRUE] <- 0

ucdp$deaths_a_adj_lag1 <- ucdp$deaths_b_adj_lag1 <- ucdp$deaths_civilians_adj_lag1 <- ucdp$best_adj_lag1 <- 0 # code lagged violence variables
for(i in 2:nrow(ucdp)){
   ucdp$deaths_a_adj_lag1[i] <- ifelse(ucdp$gov.fe[i] == ucdp$gov.fe[i-1] & ucdp$day.nr[i] == ucdp$day.nr[i-1] + 1, ucdp$deaths_a_adj[i-1], 0)
   ucdp$deaths_b_adj_lag1[i] <- ifelse(ucdp$gov.fe[i] == ucdp$gov.fe[i-1] & ucdp$day.nr[i] == ucdp$day.nr[i-1] + 1, ucdp$deaths_b_adj[i-1], 0)
   ucdp$deaths_civilians_adj_lag1[i] <- ifelse(ucdp$gov.fe[i] == ucdp$gov.fe[i-1] & ucdp$day.nr[i] == ucdp$day.nr[i-1] + 1, ucdp$deaths_civilians_adj[i-1], 0)
   ucdp$best_adj_lag1[i] <- ifelse(ucdp$gov.fe[i] == ucdp$gov.fe[i-1] & ucdp$day.nr[i] == ucdp$day.nr[i-1] + 1, ucdp$best_adj[i-1], 0)
}

ucdp$day.nr <- NULL

responses.all <- merge(responses.all, ucdp, by = c("mo", "day", "gov.fe"), all.x = TRUE, sort = FALSE) # merge violence data into main data set

responses.all$deaths_a_adj_lag1[is.na(responses.all$deaths_a_adj_lag1) == TRUE] <- 0 # replace missing values (i.e., survey responses with 0 casualties in spatial and temporal proximity) with 0 values
responses.all$deaths_b_adj_lag1[is.na(responses.all$deaths_b_adj_lag1) == TRUE] <- 0
responses.all$deaths_civilians_adj_lag1[is.na(responses.all$deaths_civilians_adj_lag1) == TRUE] <- 0
responses.all$best_adj_lag1[is.na(responses.all$best_adj_lag1) == TRUE] <- 0

responses.all$deaths.a.adj.lag1x <- responses.all$deaths_a_adj_lag1 + responses.all$deaths_a_adj # create violence measures for analysis (see Codebook for details)
responses.all$deaths.b.adj.lag1x <- responses.all$deaths_b_adj_lag1 + responses.all$deaths_b_adj
responses.all$deaths.civilians.adj.lag1x <- responses.all$deaths_civilians_adj_lag1 + responses.all$deaths_civilians_adj
responses.all$best.adj.lag1x <- responses.all$best_adj_lag1 + responses.all$best_adj

responses.all$deaths_a_adj <- responses.all$deaths_a_adj_lag1 <- responses.all$deaths_b_adj <- 
   responses.all$deaths_b_adj_lag1 <- responses.all$deaths_civilians_adj <- 
   responses.all$deaths_civilians_adj_lag1 <- responses.all$best_adj <- 
   responses.all$best_adj_lag1 <- NULL # delete variables that are not needed for analysis

### Merge in violence data from the START's GTD2

gtd2 <- read.dta("GTD2.dta") # see Dofile.do on compilation of this dataset
days <- read.dta("days.dta") # Empty dataset with one observation per day per governorate (province): from May 1 to Sept. 30

gtd2$gov.fe <- gtd2$govfe # rename variable
gtd2$govfe <- NULL
days$gov.fe <- days$govfe # rename variable
days$govfe <- NULL

gtd2 <- gtd2[order(gtd2$gov.fe, gtd2$mo, gtd2$day),]

gtd2 <- merge(days, gtd2, by = c("mo", "day", "gov.fe"), all.x = TRUE, sort = FALSE) # merge GTD data into empty data set

gtd2 <- gtd2[order(gtd2$gov.fe, gtd2$mo, gtd2$day),]

gtd2$day.nr <- ifelse(gtd2$mo == 5, gtd2$day - 7,
                      ifelse(gtd2$mo == 6, gtd2$day - 7 + 31,
                             ifelse(gtd2$mo == 7, gtd2$day - 7 + 31 + 30,
                                    ifelse(gtd2$mo == 8, gtd2$day - 7 + 31 + 30 + 31,
                                           gtd2$day - 7 + 31 + 30 + 31 + 31)))) # code day.nr variable (see Codebook for details)

gtd2 <- gtd2[gtd2$day.nr > 0,] # drop governorate-day observations before start of survey

gtd2$fatalities[is.na(gtd2$fatalities) == TRUE] <- 0 # replace missing values (i.e., governorate-day observations with 0 casualties) with 0 values
gtd2$injured[is.na(gtd2$injured) == TRUE] <- 0
gtd2$casualties[is.na(gtd2$casualties) == TRUE] <- 0

gtd2$fatalities_lag1 <- gtd2$casualties_lag1 <- 0 # code lagged violence variables
for(i in 2:nrow(gtd2)){
   gtd2$fatalities_lag1[i] <- ifelse(gtd2$gov.fe[i] == gtd2$gov.fe[i-1] & gtd2$day.nr[i] == gtd2$day.nr[i-1] + 1, gtd2$fatalities[i-1], 0)
   gtd2$casualties_lag1[i] <- ifelse(gtd2$gov.fe[i] == gtd2$gov.fe[i-1] & gtd2$day.nr[i] == gtd2$day.nr[i-1] + 1, gtd2$casualties[i-1], 0)
}

gtd2$fatalities_lag28 <- gtd2$casualties_lag28 <- 0 # code lagged violence variables
for(i in 1:nrow(gtd2)){
   day.nr.temp <- gtd2$day.nr[i]
   gov.fe.temp <- gtd2$gov.fe[i]
   temp <- gtd2[gtd2$day.nr < day.nr.temp & gtd2$day.nr > day.nr.temp - 29 & gtd2$gov.fe == gov.fe.temp,]
   gtd2$fatalities_lag28[i] <- sum(temp$fatalities)
   gtd2$casualties_lag28[i] <- sum(temp$casualties)
}

gtd2$day.nr <- NULL

responses.all <- merge(responses.all, gtd2, by = c("mo", "day", "gov.fe"), all.x = TRUE, sort = TRUE) # merge violence data into main data set

responses.all$fatalities[is.na(responses.all$fatalities) == TRUE] <- 0 # replace missing values (i.e., survey responses with 0 casualties in spatial and temporal proximity) with 0 values
responses.all$casualties[is.na(responses.all$casualties) == TRUE] <- 0

responses.all$fatalities_lag1[is.na(responses.all$fatalities_lag1) == TRUE] <- 0
responses.all$casualties_lag1[is.na(responses.all$casualties_lag1) == TRUE] <- 0

responses.all$fatalities_lag28[is.na(responses.all$fatalities_lag28) == TRUE] <- 0
responses.all$casualties_lag28[is.na(responses.all$casualties_lag28) == TRUE] <- 0

responses.all$fatalities.lag1x <- responses.all$fatalities_lag1 + responses.all$fatalities # create violence measures for analysis (see Codebook for details)
responses.all$fatalities.lag28x <- responses.all$fatalities_lag28 + responses.all$fatalities

responses.all$casualties.lag1x <- responses.all$casualties_lag1 + responses.all$casualties
responses.all$casualties.lag28x <- responses.all$casualties_lag28 + responses.all$casualties

### Create governorate-day dataset with daily number of fatalities and survey responses in each governorate

library(doBy)
responses.all$count <- 1
responses.govday <- summaryBy(count ~ mo + day + gov.fe, data = responses.all, header = TRUE, FUN=c(sum)) # transform main data set into governorate-day data set with a measure of the number of survey responses collected in each governorate on each day
responses.govday$responses <- responses.govday$count.sum # rename variable
responses.govday$count.sum <- NULL

gtd2 <- merge(gtd2, responses.govday, by = c("mo", "day", "gov.fe"), all.x = TRUE, sort = FALSE) # merge data set with number of daily responses gathered in each governorate into the data set with GTD2 violence
gtd2$responses[is.na(gtd2$responses)] <- 0 # replace missing values (i.e., governorate-day observations without a single survey response) with 0 values

gtd2$casualties.lag1x <- gtd2$casualties + gtd2$casualties_lag1 # create violence measures for analysis (see Codebook for details)

gtd2$gov.fe1 <- ifelse(gtd2$gov.fe == 1, 1, 0) # create governorate fixed effects variables (see Codebook for details)
gtd2$gov.fe2 <- ifelse(gtd2$gov.fe == 2, 1, 0)
gtd2$gov.fe3 <- ifelse(gtd2$gov.fe == 3, 1, 0)
gtd2$gov.fe4 <- ifelse(gtd2$gov.fe == 4, 1, 0)
gtd2$gov.fe5 <- ifelse(gtd2$gov.fe == 5, 1, 0)
gtd2$gov.fe6 <- ifelse(gtd2$gov.fe == 6, 1, 0)
gtd2$gov.fe7 <- ifelse(gtd2$gov.fe == 7, 1, 0)
gtd2$gov.fe8 <- ifelse(gtd2$gov.fe == 8, 1, 0)
gtd2$gov.fe9 <- ifelse(gtd2$gov.fe == 9, 1, 0)
gtd2$gov.fe10 <- ifelse(gtd2$gov.fe == 10, 1, 0)
gtd2$gov.fe11 <- ifelse(gtd2$gov.fe == 11, 1, 0)
gtd2$gov.fe12 <- ifelse(gtd2$gov.fe == 12, 1, 0)
gtd2$gov.fe13 <- ifelse(gtd2$gov.fe == 13, 1, 0)
gtd2$gov.fe14 <- ifelse(gtd2$gov.fe == 14, 1, 0)
gtd2$gov.fe15 <- ifelse(gtd2$gov.fe == 15, 1, 0)
gtd2$gov.fe16 <- ifelse(gtd2$gov.fe == 16, 1, 0)
gtd2$gov.fe17 <- ifelse(gtd2$gov.fe == 17, 1, 0)
gtd2$gov.fe18 <- ifelse(gtd2$gov.fe == 18, 1, 0)

gtd2 <- gtd2[gtd2$mo < 9 | (gtd2$mo == 9 & gtd2$day < 14),] # drop governorate-day observations after end of data collection
responses.perday <- gtd2 # rename data set

### Drop variables that are not needed for analysis

responses.all$fatalities.x <- responses.all$injured.x <- responses.all$casualties.x <-
   responses.all$casualties_lag1.x <- responses.all$fatalities_lag1.x <- responses.all$casualties_lag28.x <-
   responses.all$fatalities_lag28.x <- responses.all$count <- responses.all$fatalities.y <-
   responses.all$injured.y <- responses.all$casualties.y <- responses.all$casualties_lag1.y <-
   responses.all$fatalities_lag1.y <- responses.all$casualties_lag28.y <- responses.all$fatalities_lag28.y <- 
   responses.all$fatalities <- responses.all$injured <- responses.all$casualties <- 
   responses.all$casualties_lag1 <- responses.all$fatalities_lag28 <- responses.all$casualties_lag28 <- NULL

### Sort data set

responses.all <- responses.all[order(responses.all$gov.fe, responses.all$id.nr),]

### Create subsets for analysis

responses <- responses.all[responses.all$mo == 8,] # contains all responses gathered in August
responses14 <- responses.all[responses.all$day.nr > 99 - 15 & responses.all$day.nr < 99 + 15,] # contains all responses gathered within +/- 2 weeks from August 14
responses21 <- responses.all[responses.all$day.nr > 99 - 22 & responses.all$day.nr < 99 + 22,] # contains all responses gathered within +/- 3 weeks from August 14
responses.sep <- responses.all[(responses.all$mo == 8 | responses.all$mo == 9),] # contains all responses gathered in August or September
