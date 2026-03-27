# Clean the initial/raw protest data, creating partisanship identifiers and other protest magnitude metrics

# List of required packages
required_packages <- c("tidyr", "dplyr", "plyr")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
}

# Load all required libraries
lapply(required_packages, library, character.only = TRUE)

setwd('~/Dropbox/CPS/CPS_Materials') # set working directory for the project

# Due to special characters, the encoding had to be set to a non-default type
HUNdata <- read.csv(file="HungarianProtests.csv", header=TRUE)

# Make description of protest lower case
HUNdata$v003_lower <- stringr::str_to_lower(HUNdata$v003)

# Remove duplicates based on replicate description and date, besides the empty description ones (v003!='-2') from before 1995
duplicated<-  HUNdata[duplicated(HUNdata[, c("v003", "v004year", "v004month", "v004day")]), ]
duplicated<- subset(duplicated, v003!='-2')
HUNdata<- HUNdata %>% anti_join(duplicated)

# There is already a R function called "unique", and that may generate problems. Rename the unique ID column to uniqueID
colnames(HUNdata)[1] <- "uniqueID"

# Manually change the political association code for a case where a coder input the wrong association code
HUNdata$v019politicalparties01[HUNdata$uniqueID==101030] <- 3038

# Now we code partisan protest. See Appendix 4 for a list of these parties + partisan orgs. Here I'm 
# using the coding protocol developed in the org_checks excel sheet that lists all mentioned organizations,
# which are color coded by their partisan affiliation. 

# Define the set of values to look for far right parties leading/sponsoring/participating in protest.
values_to_check <- c(147, 274)

HUNdata$rad_part_szervez <- apply(HUNdata[, c('v019politicalparties01', 'v019politicalparties02', 'v019politicalparties03', 'v019politicalparties04',
                                              'v019politicalparties05', 'v01904name01', 'v01904name02', 'v01904name03', 'v01904name04',
                                              'v01904name05', 'v01911name01', 'v01911name02', 'v01911name03', 'v01911name04', 'v01911name05', 
                                              'v01912name01', 'v01912name02', 'v01912name03', 'v01912name04', 'v01912name05', 'v01913name01',
                                              'v01913name02', 'v01913name03', 'v01913name04', 'v01913name05', 'v01914name01', 'v01914name02',
                                              'v01914name03', 'v01914name04', 'v01914name05', 'v01915name01', 'v01915name02', 'v01915name03',
                                              'v01915name04', 'v01915name05', 'v01916name01', 'v01916name02', 'v01916name03', 'v01916name04',
                                              'v01916name05', 'v01917name01', 'v01917name02', 'v01917name03', 'v01917name04', 'v01917name05',
                                              'v01918name01', 'v01918name02', 'v01918name03', 'v01918name04', 'v01918name05', 'v01919name01',
                                              'v01919name02', 'v01919name03', 'v01919name04', 'v01919name05', 'v01919bname01', 'v01919bname02',
                                              'v01919bname03', 'v01919bname04', 'v01919bname05', 'v01920name01', 'v01920name02', 'v01920name03',
                                              'v01920name04', 'v01920name05', 'v01922name01', 'v01922name02', 'v01922name03', 'v01922name04',
                                              'v01922name05', 'v01923name01', 'v01923name02', 'v01923name03', 'v01923name04', 'v01923name05',
                                              'v01924name01', 'v01924name02', 'v01924name03', 'v01924name04', 'v01924name05', 'v01925name01',
                                              'v01925name02', 'v01925name03', 'v01925name04', 'v01925name05', 'v01926name01', 'v01926name02',
                                              'v01926name03', 'v01926name04', 'v01926name05', 'v01927name01', 'v01927name02', 'v01927name03',
                                              'v01927name04', 'v01927name05', 'v01928name01', 'v01928name02', 'v01928name03', 'v01928name04',
                                              'v01928name05', 'v01929name01', 'v01929name02', 'v01929name03', 'v01929name04', 'v01929name05',
                                              'v01930name01', 'v01930name02', 'v01930name03', 'v01930name04', 'v01930name05', 'v01931name01',
                                              'v01931name02', 'v01931name03', 'v01931name04', 'v01931name05', 'v01921name01', 'v01921name02',
                                              'v01921name03', 'v01921name04', 'v01921name05', 'v019apoliticalparties01',
                                              'v019apoliticalparties02', 'v019apoliticalparties03', 'v019apoliticalparties04',
                                              'v019apoliticalparties05', 'v019a04name01', 'v019a04name02', 'v019a04name03', 'v019a04name04',
                                              'v019a04name05', 'v019a11name01', 'v019a11name02', 'v019a11name03', 'v019a11name04', 'v019a11name05',
                                              'v019a12name01', 'v019a12name02', 'v019a12name03', 'v019a12name04', 'v019a12name05', 'v019a13name01',
                                              'v019a13name02', 'v019a13name03', 'v019a13name04', 'v019a13name05', 'v019a14name01', 'v019a14name02',
                                              'v019a14name03', 'v019a14name04', 'v019a14name05', 'v019a15name01', 'v019a15name02', 'v019a15name03',
                                              'v019a15name04', 'v019a15name05', 'v019a16name01', 'v019a16name02', 'v019a16name03', 'v019a16name04',
                                              'v019a16name05', 'v019a17name01', 'v019a17name02', 'v019a17name03', 'v019a17name04', 'v019a17name05',
                                              'v019a18name01', 'v019a18name02', 'v019a18name03', 'v019a18name04', 'v019a18name05', 'v019a19name01',
                                              'v019a19name02', 'v019a19name03', 'v019a19name04', 'v019a19name05', 'v019a19bname01',
                                              'v019a19bname02', 'v019a19bname03', 'v019a19bname04', 'v019a19bname05', 'v019a20name01',
                                              'v019a20name02', 'v019a20name03', 'v019a20name04', 'v019a20name05', 'v019a22name01', 'v019a22name02',
                                              'v019a22name03', 'v019a22name04', 'v019a22name05', 'v019a23name01', 'v019a23name02', 'v019a23name03',
                                              'v019a23name04', 'v019a23name05', 'v019a24name01', 'v019a24name02', 'v019a24name03', 'v019a24name04',
                                              'v019a24name05', 'v019a25name01', 'v019a25name02', 'v019a25name03', 'v019a25name04', 'v019a25name05',
                                              'v019a26name01', 'v019a26name02', 'v019a26name03', 'v019a26name04', 'v019a26name05', 'v019a27name01',
                                              'v019a27name02', 'v019a27name03', 'v019a27name04', 'v019a27name05', 'v019a28name01', 'v019a28name02',
                                              'v019a28name03', 'v019a28name04', 'v019a28name05', 'v019a29name01', 'v019a29name02', 'v019a29name03',
                                              'v019a29name04', 'v019a29name05', 'v019a30name01', 'v019a30name02', 'v019a30name03', 'v019a30name04',
                                              'v019a30name05', 'v019a31name01', 'v019a31name02', 'v019a31name03', 'v019a31name04', 'v019a31name05',
                                              'v019a21name01', 'v019a21name02', 'v019a21name03', 'v019a21name04', 'v019a21name05')], 1, function(x) {
                                                any(x %in% values_to_check)
                                              })                                                                                       

HUNdata$rad_part_szervez <- as.integer(HUNdata$rad_part_szervez)

# Define the set of values to look for far right parties publicly signalling support for protest.
HUNdata$rad_part_tamogat <- apply(HUNdata[, c('v07601name1', 'v07601name2', 'v07601name3', 'v07601name4', 'v07601name5',
                                              'v07601name6', 'v07601name7', 'v07601name8')], 1, function(x) {
                                                any(x %in% values_to_check)
                                              })                                                                                       

HUNdata$rad_part_tamogat <- as.integer(HUNdata$rad_part_tamogat)

# Define the set of values to look for left parties leading/sponsoring/participating in protest.
values_to_check <- c(281, 691, 1274, 367)

HUNdata$left_part_szervez <- apply(HUNdata[, c('v019politicalparties01', 'v019politicalparties02', 'v019politicalparties03', 'v019politicalparties04',
                                               'v019politicalparties05', 'v01904name01', 'v01904name02', 'v01904name03', 'v01904name04',
                                               'v01904name05', 'v01911name01', 'v01911name02', 'v01911name03', 'v01911name04', 'v01911name05', 
                                               'v01912name01', 'v01912name02', 'v01912name03', 'v01912name04', 'v01912name05', 'v01913name01',
                                               'v01913name02', 'v01913name03', 'v01913name04', 'v01913name05', 'v01914name01', 'v01914name02',
                                               'v01914name03', 'v01914name04', 'v01914name05', 'v01915name01', 'v01915name02', 'v01915name03',
                                               'v01915name04', 'v01915name05', 'v01916name01', 'v01916name02', 'v01916name03', 'v01916name04',
                                               'v01916name05', 'v01917name01', 'v01917name02', 'v01917name03', 'v01917name04', 'v01917name05',
                                               'v01918name01', 'v01918name02', 'v01918name03', 'v01918name04', 'v01918name05', 'v01919name01',
                                               'v01919name02', 'v01919name03', 'v01919name04', 'v01919name05', 'v01919bname01', 'v01919bname02',
                                               'v01919bname03', 'v01919bname04', 'v01919bname05', 'v01920name01', 'v01920name02', 'v01920name03',
                                               'v01920name04', 'v01920name05', 'v01922name01', 'v01922name02', 'v01922name03', 'v01922name04',
                                               'v01922name05', 'v01923name01', 'v01923name02', 'v01923name03', 'v01923name04', 'v01923name05',
                                               'v01924name01', 'v01924name02', 'v01924name03', 'v01924name04', 'v01924name05', 'v01925name01',
                                               'v01925name02', 'v01925name03', 'v01925name04', 'v01925name05', 'v01926name01', 'v01926name02',
                                               'v01926name03', 'v01926name04', 'v01926name05', 'v01927name01', 'v01927name02', 'v01927name03',
                                               'v01927name04', 'v01927name05', 'v01928name01', 'v01928name02', 'v01928name03', 'v01928name04',
                                               'v01928name05', 'v01929name01', 'v01929name02', 'v01929name03', 'v01929name04', 'v01929name05',
                                               'v01930name01', 'v01930name02', 'v01930name03', 'v01930name04', 'v01930name05', 'v01931name01',
                                               'v01931name02', 'v01931name03', 'v01931name04', 'v01931name05', 'v01921name01', 'v01921name02',
                                               'v01921name03', 'v01921name04', 'v01921name05', 'v019apoliticalparties01',
                                               'v019apoliticalparties02', 'v019apoliticalparties03', 'v019apoliticalparties04',
                                               'v019apoliticalparties05', 'v019a04name01', 'v019a04name02', 'v019a04name03', 'v019a04name04',
                                               'v019a04name05', 'v019a11name01', 'v019a11name02', 'v019a11name03', 'v019a11name04', 'v019a11name05',
                                               'v019a12name01', 'v019a12name02', 'v019a12name03', 'v019a12name04', 'v019a12name05', 'v019a13name01',
                                               'v019a13name02', 'v019a13name03', 'v019a13name04', 'v019a13name05', 'v019a14name01', 'v019a14name02',
                                               'v019a14name03', 'v019a14name04', 'v019a14name05', 'v019a15name01', 'v019a15name02', 'v019a15name03',
                                               'v019a15name04', 'v019a15name05', 'v019a16name01', 'v019a16name02', 'v019a16name03', 'v019a16name04',
                                               'v019a16name05', 'v019a17name01', 'v019a17name02', 'v019a17name03', 'v019a17name04', 'v019a17name05',
                                               'v019a18name01', 'v019a18name02', 'v019a18name03', 'v019a18name04', 'v019a18name05', 'v019a19name01',
                                               'v019a19name02', 'v019a19name03', 'v019a19name04', 'v019a19name05', 'v019a19bname01',
                                               'v019a19bname02', 'v019a19bname03', 'v019a19bname04', 'v019a19bname05', 'v019a20name01',
                                               'v019a20name02', 'v019a20name03', 'v019a20name04', 'v019a20name05', 'v019a22name01', 'v019a22name02',
                                               'v019a22name03', 'v019a22name04', 'v019a22name05', 'v019a23name01', 'v019a23name02', 'v019a23name03',
                                               'v019a23name04', 'v019a23name05', 'v019a24name01', 'v019a24name02', 'v019a24name03', 'v019a24name04',
                                               'v019a24name05', 'v019a25name01', 'v019a25name02', 'v019a25name03', 'v019a25name04', 'v019a25name05',
                                               'v019a26name01', 'v019a26name02', 'v019a26name03', 'v019a26name04', 'v019a26name05', 'v019a27name01',
                                               'v019a27name02', 'v019a27name03', 'v019a27name04', 'v019a27name05', 'v019a28name01', 'v019a28name02',
                                               'v019a28name03', 'v019a28name04', 'v019a28name05', 'v019a29name01', 'v019a29name02', 'v019a29name03',
                                               'v019a29name04', 'v019a29name05', 'v019a30name01', 'v019a30name02', 'v019a30name03', 'v019a30name04',
                                               'v019a30name05', 'v019a31name01', 'v019a31name02', 'v019a31name03', 'v019a31name04', 'v019a31name05',
                                               'v019a21name01', 'v019a21name02', 'v019a21name03', 'v019a21name04', 'v019a21name05')], 1, function(x) {
                                                 any(x %in% values_to_check)
                                               })                                                                                       

HUNdata$left_part_szervez <- as.integer(HUNdata$left_part_szervez)

# Define the set of values to look for left parties publicly signalling support for protest.
HUNdata$left_part_tamogat <- apply(HUNdata[, c('v07601name1', 'v07601name2', 'v07601name3', 'v07601name4', 'v07601name5',
                                               'v07601name6', 'v07601name7', 'v07601name8')], 1, function(x) {
                                                 any(x %in% values_to_check)
                                               })                                                                                       

HUNdata$left_part_tamogat <- as.integer(HUNdata$left_part_tamogat)

# Define the set of values to look for mainstream right parties leading/sponsoring/participating in protest.
values_to_check <- c(93, 94, 149, 260, 261, 754, 894, 1199, 2070)

HUNdata$right_part_szervez <- apply(HUNdata[, c('v019politicalparties01', 'v019politicalparties02', 'v019politicalparties03', 'v019politicalparties04',
                                                'v019politicalparties05', 'v01904name01', 'v01904name02', 'v01904name03', 'v01904name04',
                                                'v01904name05', 'v01911name01', 'v01911name02', 'v01911name03', 'v01911name04', 'v01911name05', 
                                                'v01912name01', 'v01912name02', 'v01912name03', 'v01912name04', 'v01912name05', 'v01913name01',
                                                'v01913name02', 'v01913name03', 'v01913name04', 'v01913name05', 'v01914name01', 'v01914name02',
                                                'v01914name03', 'v01914name04', 'v01914name05', 'v01915name01', 'v01915name02', 'v01915name03',
                                                'v01915name04', 'v01915name05', 'v01916name01', 'v01916name02', 'v01916name03', 'v01916name04',
                                                'v01916name05', 'v01917name01', 'v01917name02', 'v01917name03', 'v01917name04', 'v01917name05',
                                                'v01918name01', 'v01918name02', 'v01918name03', 'v01918name04', 'v01918name05', 'v01919name01',
                                                'v01919name02', 'v01919name03', 'v01919name04', 'v01919name05', 'v01919bname01', 'v01919bname02',
                                                'v01919bname03', 'v01919bname04', 'v01919bname05', 'v01920name01', 'v01920name02', 'v01920name03',
                                                'v01920name04', 'v01920name05', 'v01922name01', 'v01922name02', 'v01922name03', 'v01922name04',
                                                'v01922name05', 'v01923name01', 'v01923name02', 'v01923name03', 'v01923name04', 'v01923name05',
                                                'v01924name01', 'v01924name02', 'v01924name03', 'v01924name04', 'v01924name05', 'v01925name01',
                                                'v01925name02', 'v01925name03', 'v01925name04', 'v01925name05', 'v01926name01', 'v01926name02',
                                                'v01926name03', 'v01926name04', 'v01926name05', 'v01927name01', 'v01927name02', 'v01927name03',
                                                'v01927name04', 'v01927name05', 'v01928name01', 'v01928name02', 'v01928name03', 'v01928name04',
                                                'v01928name05', 'v01929name01', 'v01929name02', 'v01929name03', 'v01929name04', 'v01929name05',
                                                'v01930name01', 'v01930name02', 'v01930name03', 'v01930name04', 'v01930name05', 'v01931name01',
                                                'v01931name02', 'v01931name03', 'v01931name04', 'v01931name05', 'v01921name01', 'v01921name02',
                                                'v01921name03', 'v01921name04', 'v01921name05', 'v019apoliticalparties01',
                                                'v019apoliticalparties02', 'v019apoliticalparties03', 'v019apoliticalparties04',
                                                'v019apoliticalparties05', 'v019a04name01', 'v019a04name02', 'v019a04name03', 'v019a04name04',
                                                'v019a04name05', 'v019a11name01', 'v019a11name02', 'v019a11name03', 'v019a11name04', 'v019a11name05',
                                                'v019a12name01', 'v019a12name02', 'v019a12name03', 'v019a12name04', 'v019a12name05', 'v019a13name01',
                                                'v019a13name02', 'v019a13name03', 'v019a13name04', 'v019a13name05', 'v019a14name01', 'v019a14name02',
                                                'v019a14name03', 'v019a14name04', 'v019a14name05', 'v019a15name01', 'v019a15name02', 'v019a15name03',
                                                'v019a15name04', 'v019a15name05', 'v019a16name01', 'v019a16name02', 'v019a16name03', 'v019a16name04',
                                                'v019a16name05', 'v019a17name01', 'v019a17name02', 'v019a17name03', 'v019a17name04', 'v019a17name05',
                                                'v019a18name01', 'v019a18name02', 'v019a18name03', 'v019a18name04', 'v019a18name05', 'v019a19name01',
                                                'v019a19name02', 'v019a19name03', 'v019a19name04', 'v019a19name05', 'v019a19bname01',
                                                'v019a19bname02', 'v019a19bname03', 'v019a19bname04', 'v019a19bname05', 'v019a20name01',
                                                'v019a20name02', 'v019a20name03', 'v019a20name04', 'v019a20name05', 'v019a22name01', 'v019a22name02',
                                                'v019a22name03', 'v019a22name04', 'v019a22name05', 'v019a23name01', 'v019a23name02', 'v019a23name03',
                                                'v019a23name04', 'v019a23name05', 'v019a24name01', 'v019a24name02', 'v019a24name03', 'v019a24name04',
                                                'v019a24name05', 'v019a25name01', 'v019a25name02', 'v019a25name03', 'v019a25name04', 'v019a25name05',
                                                'v019a26name01', 'v019a26name02', 'v019a26name03', 'v019a26name04', 'v019a26name05', 'v019a27name01',
                                                'v019a27name02', 'v019a27name03', 'v019a27name04', 'v019a27name05', 'v019a28name01', 'v019a28name02',
                                                'v019a28name03', 'v019a28name04', 'v019a28name05', 'v019a29name01', 'v019a29name02', 'v019a29name03',
                                                'v019a29name04', 'v019a29name05', 'v019a30name01', 'v019a30name02', 'v019a30name03', 'v019a30name04',
                                                'v019a30name05', 'v019a31name01', 'v019a31name02', 'v019a31name03', 'v019a31name04', 'v019a31name05',
                                                'v019a21name01', 'v019a21name02', 'v019a21name03', 'v019a21name04', 'v019a21name05')], 1, function(x) {
                                                  any(x %in% values_to_check)
                                                })                                                                                       

HUNdata$right_part_szervez <- as.integer(HUNdata$right_part_szervez)

# Define the set of values to look for mainstream right parties publicly signalling support for protest.
HUNdata$right_part_tamogat <- apply(HUNdata[, c('v07601name1', 'v07601name2', 'v07601name3', 'v07601name4', 'v07601name5',
                                                'v07601name6', 'v07601name7', 'v07601name8')], 1, function(x) {
                                                  any(x %in% values_to_check)
                                                })                                                                                       

HUNdata$right_part_tamogat <- as.integer(HUNdata$right_part_tamogat)

# Define the set of values to look for radical left organizations leading/sponsoring/participating in protest.
values_to_check <- c(493, 627, 2109, 2345, 237, 284, 429, 478, 486)

HUNdata$radleftmoz <- apply(HUNdata[, c('v019politicalparties01', 'v019politicalparties02', 'v019politicalparties03', 'v019politicalparties04',
                                        'v019politicalparties05', 'v01904name01', 'v01904name02', 'v01904name03', 'v01904name04',
                                        'v01904name05', 'v01911name01', 'v01911name02', 'v01911name03', 'v01911name04', 'v01911name05', 
                                        'v01912name01', 'v01912name02', 'v01912name03', 'v01912name04', 'v01912name05', 'v01913name01',
                                        'v01913name02', 'v01913name03', 'v01913name04', 'v01913name05', 'v01914name01', 'v01914name02',
                                        'v01914name03', 'v01914name04', 'v01914name05', 'v01915name01', 'v01915name02', 'v01915name03',
                                        'v01915name04', 'v01915name05', 'v01916name01', 'v01916name02', 'v01916name03', 'v01916name04',
                                        'v01916name05', 'v01917name01', 'v01917name02', 'v01917name03', 'v01917name04', 'v01917name05',
                                        'v01918name01', 'v01918name02', 'v01918name03', 'v01918name04', 'v01918name05', 'v01919name01',
                                        'v01919name02', 'v01919name03', 'v01919name04', 'v01919name05', 'v01919bname01', 'v01919bname02',
                                        'v01919bname03', 'v01919bname04', 'v01919bname05', 'v01920name01', 'v01920name02', 'v01920name03',
                                        'v01920name04', 'v01920name05', 'v01922name01', 'v01922name02', 'v01922name03', 'v01922name04',
                                        'v01922name05', 'v01923name01', 'v01923name02', 'v01923name03', 'v01923name04', 'v01923name05',
                                        'v01924name01', 'v01924name02', 'v01924name03', 'v01924name04', 'v01924name05', 'v01925name01',
                                        'v01925name02', 'v01925name03', 'v01925name04', 'v01925name05', 'v01926name01', 'v01926name02',
                                        'v01926name03', 'v01926name04', 'v01926name05', 'v01927name01', 'v01927name02', 'v01927name03',
                                        'v01927name04', 'v01927name05', 'v01928name01', 'v01928name02', 'v01928name03', 'v01928name04',
                                        'v01928name05', 'v01929name01', 'v01929name02', 'v01929name03', 'v01929name04', 'v01929name05',
                                        'v01930name01', 'v01930name02', 'v01930name03', 'v01930name04', 'v01930name05', 'v01931name01',
                                        'v01931name02', 'v01931name03', 'v01931name04', 'v01931name05', 'v01921name01', 'v01921name02',
                                        'v01921name03', 'v01921name04', 'v01921name05', 'v019apoliticalparties01',
                                        'v019apoliticalparties02', 'v019apoliticalparties03', 'v019apoliticalparties04',
                                        'v019apoliticalparties05', 'v019a04name01', 'v019a04name02', 'v019a04name03', 'v019a04name04',
                                        'v019a04name05', 'v019a11name01', 'v019a11name02', 'v019a11name03', 'v019a11name04', 'v019a11name05',
                                        'v019a12name01', 'v019a12name02', 'v019a12name03', 'v019a12name04', 'v019a12name05', 'v019a13name01',
                                        'v019a13name02', 'v019a13name03', 'v019a13name04', 'v019a13name05', 'v019a14name01', 'v019a14name02',
                                        'v019a14name03', 'v019a14name04', 'v019a14name05', 'v019a15name01', 'v019a15name02', 'v019a15name03',
                                        'v019a15name04', 'v019a15name05', 'v019a16name01', 'v019a16name02', 'v019a16name03', 'v019a16name04',
                                        'v019a16name05', 'v019a17name01', 'v019a17name02', 'v019a17name03', 'v019a17name04', 'v019a17name05',
                                        'v019a18name01', 'v019a18name02', 'v019a18name03', 'v019a18name04', 'v019a18name05', 'v019a19name01',
                                        'v019a19name02', 'v019a19name03', 'v019a19name04', 'v019a19name05', 'v019a19bname01',
                                        'v019a19bname02', 'v019a19bname03', 'v019a19bname04', 'v019a19bname05', 'v019a20name01',
                                        'v019a20name02', 'v019a20name03', 'v019a20name04', 'v019a20name05', 'v019a22name01', 'v019a22name02',
                                        'v019a22name03', 'v019a22name04', 'v019a22name05', 'v019a23name01', 'v019a23name02', 'v019a23name03',
                                        'v019a23name04', 'v019a23name05', 'v019a24name01', 'v019a24name02', 'v019a24name03', 'v019a24name04',
                                        'v019a24name05', 'v019a25name01', 'v019a25name02', 'v019a25name03', 'v019a25name04', 'v019a25name05',
                                        'v019a26name01', 'v019a26name02', 'v019a26name03', 'v019a26name04', 'v019a26name05', 'v019a27name01',
                                        'v019a27name02', 'v019a27name03', 'v019a27name04', 'v019a27name05', 'v019a28name01', 'v019a28name02',
                                        'v019a28name03', 'v019a28name04', 'v019a28name05', 'v019a29name01', 'v019a29name02', 'v019a29name03',
                                        'v019a29name04', 'v019a29name05', 'v019a30name01', 'v019a30name02', 'v019a30name03', 'v019a30name04',
                                        'v019a30name05', 'v019a31name01', 'v019a31name02', 'v019a31name03', 'v019a31name04', 'v019a31name05',
                                        'v019a21name01', 'v019a21name02', 'v019a21name03', 'v019a21name04', 'v019a21name05')], 1, function(x) {
                                          any(x %in% values_to_check)
                                        })                                                                                       

HUNdata$radleftmoz <- as.integer(HUNdata$radleftmoz)

# Looks like there's only 44 events, many of them in the early 1990s, so we will not move forward with analysis
# of the radical left as a partisan category.
count(HUNdata$radleftmoz)

# Define the set of values to look for radical right organizations leading/sponsoring/participating in protest.
values_to_check <- c(4, 56, 135, 147, 152, 165, 179, 199, 219, 220, 221, 222, 246, 274, 276, 317, 361,
                     370, 390, 419, 422, 449, 459, 460, 487, 543, 557, 565, 648, 657, 709,718, 735, 756, 779,781, 897, 902,
                     927, 928, 969, 1314, 1351, 1370, 1371, 1376, 1397, 1409, 1422, 1670, 1846, 1950,
                     2040, 2240, 2284, 2287, 2289, 2290, 2304, 2415, 2421, 2500, 2668, 2684, 2692, 2706, 2724,
                     2725, 3003)

HUNdata$radrightmoz <- apply(HUNdata[, c('v019politicalparties01', 'v019politicalparties02', 'v019politicalparties03', 'v019politicalparties04',
                                         'v019politicalparties05', 'v01904name01', 'v01904name02', 'v01904name03', 'v01904name04',
                                         'v01904name05', 'v01911name01', 'v01911name02', 'v01911name03', 'v01911name04', 'v01911name05', 
                                         'v01912name01', 'v01912name02', 'v01912name03', 'v01912name04', 'v01912name05', 'v01913name01',
                                         'v01913name02', 'v01913name03', 'v01913name04', 'v01913name05', 'v01914name01', 'v01914name02',
                                         'v01914name03', 'v01914name04', 'v01914name05', 'v01915name01', 'v01915name02', 'v01915name03',
                                         'v01915name04', 'v01915name05', 'v01916name01', 'v01916name02', 'v01916name03', 'v01916name04',
                                         'v01916name05', 'v01917name01', 'v01917name02', 'v01917name03', 'v01917name04', 'v01917name05',
                                         'v01918name01', 'v01918name02', 'v01918name03', 'v01918name04', 'v01918name05', 'v01919name01',
                                         'v01919name02', 'v01919name03', 'v01919name04', 'v01919name05', 'v01919bname01', 'v01919bname02',
                                         'v01919bname03', 'v01919bname04', 'v01919bname05', 'v01920name01', 'v01920name02', 'v01920name03',
                                         'v01920name04', 'v01920name05', 'v01922name01', 'v01922name02', 'v01922name03', 'v01922name04',
                                         'v01922name05', 'v01923name01', 'v01923name02', 'v01923name03', 'v01923name04', 'v01923name05',
                                         'v01924name01', 'v01924name02', 'v01924name03', 'v01924name04', 'v01924name05', 'v01925name01',
                                         'v01925name02', 'v01925name03', 'v01925name04', 'v01925name05', 'v01926name01', 'v01926name02',
                                         'v01926name03', 'v01926name04', 'v01926name05', 'v01927name01', 'v01927name02', 'v01927name03',
                                         'v01927name04', 'v01927name05', 'v01928name01', 'v01928name02', 'v01928name03', 'v01928name04',
                                         'v01928name05', 'v01929name01', 'v01929name02', 'v01929name03', 'v01929name04', 'v01929name05',
                                         'v01930name01', 'v01930name02', 'v01930name03', 'v01930name04', 'v01930name05', 'v01931name01',
                                         'v01931name02', 'v01931name03', 'v01931name04', 'v01931name05', 'v01921name01', 'v01921name02',
                                         'v01921name03', 'v01921name04', 'v01921name05', 'v019apoliticalparties01',
                                         'v019apoliticalparties02', 'v019apoliticalparties03', 'v019apoliticalparties04',
                                         'v019apoliticalparties05', 'v019a04name01', 'v019a04name02', 'v019a04name03', 'v019a04name04',
                                         'v019a04name05', 'v019a11name01', 'v019a11name02', 'v019a11name03', 'v019a11name04', 'v019a11name05',
                                         'v019a12name01', 'v019a12name02', 'v019a12name03', 'v019a12name04', 'v019a12name05', 'v019a13name01',
                                         'v019a13name02', 'v019a13name03', 'v019a13name04', 'v019a13name05', 'v019a14name01', 'v019a14name02',
                                         'v019a14name03', 'v019a14name04', 'v019a14name05', 'v019a15name01', 'v019a15name02', 'v019a15name03',
                                         'v019a15name04', 'v019a15name05', 'v019a16name01', 'v019a16name02', 'v019a16name03', 'v019a16name04',
                                         'v019a16name05', 'v019a17name01', 'v019a17name02', 'v019a17name03', 'v019a17name04', 'v019a17name05',
                                         'v019a18name01', 'v019a18name02', 'v019a18name03', 'v019a18name04', 'v019a18name05', 'v019a19name01',
                                         'v019a19name02', 'v019a19name03', 'v019a19name04', 'v019a19name05', 'v019a19bname01',
                                         'v019a19bname02', 'v019a19bname03', 'v019a19bname04', 'v019a19bname05', 'v019a20name01',
                                         'v019a20name02', 'v019a20name03', 'v019a20name04', 'v019a20name05', 'v019a22name01', 'v019a22name02',
                                         'v019a22name03', 'v019a22name04', 'v019a22name05', 'v019a23name01', 'v019a23name02', 'v019a23name03',
                                         'v019a23name04', 'v019a23name05', 'v019a24name01', 'v019a24name02', 'v019a24name03', 'v019a24name04',
                                         'v019a24name05', 'v019a25name01', 'v019a25name02', 'v019a25name03', 'v019a25name04', 'v019a25name05',
                                         'v019a26name01', 'v019a26name02', 'v019a26name03', 'v019a26name04', 'v019a26name05', 'v019a27name01',
                                         'v019a27name02', 'v019a27name03', 'v019a27name04', 'v019a27name05', 'v019a28name01', 'v019a28name02',
                                         'v019a28name03', 'v019a28name04', 'v019a28name05', 'v019a29name01', 'v019a29name02', 'v019a29name03',
                                         'v019a29name04', 'v019a29name05', 'v019a30name01', 'v019a30name02', 'v019a30name03', 'v019a30name04',
                                         'v019a30name05', 'v019a31name01', 'v019a31name02', 'v019a31name03', 'v019a31name04', 'v019a31name05',
                                         'v019a21name01', 'v019a21name02', 'v019a21name03', 'v019a21name04', 'v019a21name05')], 1, function(x) {
                                           any(x %in% values_to_check)
                                         })                                                                                       

HUNdata$radrightmoz <- as.integer(HUNdata$radrightmoz)

# Define the set of values to look for left organizations leading/sponsoring/participating in protest.
values_to_check <- c(26, 28, 37, 71, 82, 89, 91, 123, 131, 132, 138, 139, 162,195,259,281, 283,360,367,375,380,
                     393, 394, 423, 466, 484, 485, 490, 518, 541, 562, 586, 587, 589, 590, 636, 637, 638,
                     640, 662, 670, 680, 683, 691, 725, 746, 747, 787, 825, 826, 827, 834, 847, 1034, 1128, 1195, 1206, 1239,
                     1300, 1319, 1388, 1505, 1528, 1561, 1605, 1662, 1716, 1725, 1733, 1866, 2038, 2136, 2227, 2270, 2309, 2330, 2405, 2428, 2453, 2480,
                     2484, 2490, 2507, 2510, 2577, 3007, 3038, 1038, 379)

HUNdata$leftmoz <- apply(HUNdata[, c('v019politicalparties01', 'v019politicalparties02', 'v019politicalparties03', 'v019politicalparties04',
                                     'v019politicalparties05', 'v01904name01', 'v01904name02', 'v01904name03', 'v01904name04',
                                     'v01904name05', 'v01911name01', 'v01911name02', 'v01911name03', 'v01911name04', 'v01911name05', 
                                     'v01912name01', 'v01912name02', 'v01912name03', 'v01912name04', 'v01912name05', 'v01913name01',
                                     'v01913name02', 'v01913name03', 'v01913name04', 'v01913name05', 'v01914name01', 'v01914name02',
                                     'v01914name03', 'v01914name04', 'v01914name05', 'v01915name01', 'v01915name02', 'v01915name03',
                                     'v01915name04', 'v01915name05', 'v01916name01', 'v01916name02', 'v01916name03', 'v01916name04',
                                     'v01916name05', 'v01917name01', 'v01917name02', 'v01917name03', 'v01917name04', 'v01917name05',
                                     'v01918name01', 'v01918name02', 'v01918name03', 'v01918name04', 'v01918name05', 'v01919name01',
                                     'v01919name02', 'v01919name03', 'v01919name04', 'v01919name05', 'v01919bname01', 'v01919bname02',
                                     'v01919bname03', 'v01919bname04', 'v01919bname05', 'v01920name01', 'v01920name02', 'v01920name03',
                                     'v01920name04', 'v01920name05', 'v01922name01', 'v01922name02', 'v01922name03', 'v01922name04',
                                     'v01922name05', 'v01923name01', 'v01923name02', 'v01923name03', 'v01923name04', 'v01923name05',
                                     'v01924name01', 'v01924name02', 'v01924name03', 'v01924name04', 'v01924name05', 'v01925name01',
                                     'v01925name02', 'v01925name03', 'v01925name04', 'v01925name05', 'v01926name01', 'v01926name02',
                                     'v01926name03', 'v01926name04', 'v01926name05', 'v01927name01', 'v01927name02', 'v01927name03',
                                     'v01927name04', 'v01927name05', 'v01928name01', 'v01928name02', 'v01928name03', 'v01928name04',
                                     'v01928name05', 'v01929name01', 'v01929name02', 'v01929name03', 'v01929name04', 'v01929name05',
                                     'v01930name01', 'v01930name02', 'v01930name03', 'v01930name04', 'v01930name05', 'v01931name01',
                                     'v01931name02', 'v01931name03', 'v01931name04', 'v01931name05', 'v01921name01', 'v01921name02',
                                     'v01921name03', 'v01921name04', 'v01921name05', 'v019apoliticalparties01',
                                     'v019apoliticalparties02', 'v019apoliticalparties03', 'v019apoliticalparties04',
                                     'v019apoliticalparties05', 'v019a04name01', 'v019a04name02', 'v019a04name03', 'v019a04name04',
                                     'v019a04name05', 'v019a11name01', 'v019a11name02', 'v019a11name03', 'v019a11name04', 'v019a11name05',
                                     'v019a12name01', 'v019a12name02', 'v019a12name03', 'v019a12name04', 'v019a12name05', 'v019a13name01',
                                     'v019a13name02', 'v019a13name03', 'v019a13name04', 'v019a13name05', 'v019a14name01', 'v019a14name02',
                                     'v019a14name03', 'v019a14name04', 'v019a14name05', 'v019a15name01', 'v019a15name02', 'v019a15name03',
                                     'v019a15name04', 'v019a15name05', 'v019a16name01', 'v019a16name02', 'v019a16name03', 'v019a16name04',
                                     'v019a16name05', 'v019a17name01', 'v019a17name02', 'v019a17name03', 'v019a17name04', 'v019a17name05',
                                     'v019a18name01', 'v019a18name02', 'v019a18name03', 'v019a18name04', 'v019a18name05', 'v019a19name01',
                                     'v019a19name02', 'v019a19name03', 'v019a19name04', 'v019a19name05', 'v019a19bname01',
                                     'v019a19bname02', 'v019a19bname03', 'v019a19bname04', 'v019a19bname05', 'v019a20name01',
                                     'v019a20name02', 'v019a20name03', 'v019a20name04', 'v019a20name05', 'v019a22name01', 'v019a22name02',
                                     'v019a22name03', 'v019a22name04', 'v019a22name05', 'v019a23name01', 'v019a23name02', 'v019a23name03',
                                     'v019a23name04', 'v019a23name05', 'v019a24name01', 'v019a24name02', 'v019a24name03', 'v019a24name04',
                                     'v019a24name05', 'v019a25name01', 'v019a25name02', 'v019a25name03', 'v019a25name04', 'v019a25name05',
                                     'v019a26name01', 'v019a26name02', 'v019a26name03', 'v019a26name04', 'v019a26name05', 'v019a27name01',
                                     'v019a27name02', 'v019a27name03', 'v019a27name04', 'v019a27name05', 'v019a28name01', 'v019a28name02',
                                     'v019a28name03', 'v019a28name04', 'v019a28name05', 'v019a29name01', 'v019a29name02', 'v019a29name03',
                                     'v019a29name04', 'v019a29name05', 'v019a30name01', 'v019a30name02', 'v019a30name03', 'v019a30name04',
                                     'v019a30name05', 'v019a31name01', 'v019a31name02', 'v019a31name03', 'v019a31name04', 'v019a31name05',
                                     'v019a21name01', 'v019a21name02', 'v019a21name03', 'v019a21name04', 'v019a21name05')], 1, function(x) {
                                       any(x %in% values_to_check)
                                     })                                                                                       

HUNdata$leftmoz <- as.integer(HUNdata$leftmoz)

# Define the set of values to look for mainstream right organizations leading/sponsoring/participating in protest.
values_to_check <- c(1,2,3,8,15,41,51,55,67,75,93,94,98,103,104,107,115,129,140,141,149,158,159,160,185,187,191,200,201,
                     206,226,235,247,250,260,261,275,277,289,320,332,333,340,347,377,388,392,395,431,444,445,448,452,458, 
                     496, 583,584, 615, 707, 708,719,722,745,851,900,926,943,971,973,985,1092,1163,1205,1241,1243,1258,
                     1277, 1292,1293,1298,1405,1492,1500,1533,1535,1597,1629,1643,1718,1719,1726,
                     1728,1759,1760,1780,1879,1934,2012,2013,2024,2043,2225, 2234,2236,2299,2300,2381,2397,2414,2416,2417,2418,2422, 2423,
                     2424,2448,2486,2495,2503,2531,2533,2538,2547,2565,2599,2600,2814)

HUNdata$rightmoz <- apply(HUNdata[, c('v019politicalparties01', 'v019politicalparties02', 'v019politicalparties03', 'v019politicalparties04',
                                      'v019politicalparties05', 'v01904name01', 'v01904name02', 'v01904name03', 'v01904name04',
                                      'v01904name05', 'v01911name01', 'v01911name02', 'v01911name03', 'v01911name04', 'v01911name05', 
                                      'v01912name01', 'v01912name02', 'v01912name03', 'v01912name04', 'v01912name05', 'v01913name01',
                                      'v01913name02', 'v01913name03', 'v01913name04', 'v01913name05', 'v01914name01', 'v01914name02',
                                      'v01914name03', 'v01914name04', 'v01914name05', 'v01915name01', 'v01915name02', 'v01915name03',
                                      'v01915name04', 'v01915name05', 'v01916name01', 'v01916name02', 'v01916name03', 'v01916name04',
                                      'v01916name05', 'v01917name01', 'v01917name02', 'v01917name03', 'v01917name04', 'v01917name05',
                                      'v01918name01', 'v01918name02', 'v01918name03', 'v01918name04', 'v01918name05', 'v01919name01',
                                      'v01919name02', 'v01919name03', 'v01919name04', 'v01919name05', 'v01919bname01', 'v01919bname02',
                                      'v01919bname03', 'v01919bname04', 'v01919bname05', 'v01920name01', 'v01920name02', 'v01920name03',
                                      'v01920name04', 'v01920name05', 'v01922name01', 'v01922name02', 'v01922name03', 'v01922name04',
                                      'v01922name05', 'v01923name01', 'v01923name02', 'v01923name03', 'v01923name04', 'v01923name05',
                                      'v01924name01', 'v01924name02', 'v01924name03', 'v01924name04', 'v01924name05', 'v01925name01',
                                      'v01925name02', 'v01925name03', 'v01925name04', 'v01925name05', 'v01926name01', 'v01926name02',
                                      'v01926name03', 'v01926name04', 'v01926name05', 'v01927name01', 'v01927name02', 'v01927name03',
                                      'v01927name04', 'v01927name05', 'v01928name01', 'v01928name02', 'v01928name03', 'v01928name04',
                                      'v01928name05', 'v01929name01', 'v01929name02', 'v01929name03', 'v01929name04', 'v01929name05',
                                      'v01930name01', 'v01930name02', 'v01930name03', 'v01930name04', 'v01930name05', 'v01931name01',
                                      'v01931name02', 'v01931name03', 'v01931name04', 'v01931name05', 'v01921name01', 'v01921name02',
                                      'v01921name03', 'v01921name04', 'v01921name05', 'v019apoliticalparties01',
                                      'v019apoliticalparties02', 'v019apoliticalparties03', 'v019apoliticalparties04',
                                      'v019apoliticalparties05', 'v019a04name01', 'v019a04name02', 'v019a04name03', 'v019a04name04',
                                      'v019a04name05', 'v019a11name01', 'v019a11name02', 'v019a11name03', 'v019a11name04', 'v019a11name05',
                                      'v019a12name01', 'v019a12name02', 'v019a12name03', 'v019a12name04', 'v019a12name05', 'v019a13name01',
                                      'v019a13name02', 'v019a13name03', 'v019a13name04', 'v019a13name05', 'v019a14name01', 'v019a14name02',
                                      'v019a14name03', 'v019a14name04', 'v019a14name05', 'v019a15name01', 'v019a15name02', 'v019a15name03',
                                      'v019a15name04', 'v019a15name05', 'v019a16name01', 'v019a16name02', 'v019a16name03', 'v019a16name04',
                                      'v019a16name05', 'v019a17name01', 'v019a17name02', 'v019a17name03', 'v019a17name04', 'v019a17name05',
                                      'v019a18name01', 'v019a18name02', 'v019a18name03', 'v019a18name04', 'v019a18name05', 'v019a19name01',
                                      'v019a19name02', 'v019a19name03', 'v019a19name04', 'v019a19name05', 'v019a19bname01',
                                      'v019a19bname02', 'v019a19bname03', 'v019a19bname04', 'v019a19bname05', 'v019a20name01',
                                      'v019a20name02', 'v019a20name03', 'v019a20name04', 'v019a20name05', 'v019a22name01', 'v019a22name02',
                                      'v019a22name03', 'v019a22name04', 'v019a22name05', 'v019a23name01', 'v019a23name02', 'v019a23name03',
                                      'v019a23name04', 'v019a23name05', 'v019a24name01', 'v019a24name02', 'v019a24name03', 'v019a24name04',
                                      'v019a24name05', 'v019a25name01', 'v019a25name02', 'v019a25name03', 'v019a25name04', 'v019a25name05',
                                      'v019a26name01', 'v019a26name02', 'v019a26name03', 'v019a26name04', 'v019a26name05', 'v019a27name01',
                                      'v019a27name02', 'v019a27name03', 'v019a27name04', 'v019a27name05', 'v019a28name01', 'v019a28name02',
                                      'v019a28name03', 'v019a28name04', 'v019a28name05', 'v019a29name01', 'v019a29name02', 'v019a29name03',
                                      'v019a29name04', 'v019a29name05', 'v019a30name01', 'v019a30name02', 'v019a30name03', 'v019a30name04',
                                      'v019a30name05', 'v019a31name01', 'v019a31name02', 'v019a31name03', 'v019a31name04', 'v019a31name05',
                                      'v019a21name01', 'v019a21name02', 'v019a21name03', 'v019a21name04', 'v019a21name05')], 1, function(x) {
                                        any(x %in% values_to_check)
                                      })                                                                                       

HUNdata$rightmoz <- as.integer(HUNdata$rightmoz)

# Recode rightist mobilization as 0 if a left or radical right party organized it
HUNdata <- HUNdata %>%
  mutate(rightmoz = case_when(
    left_part_szervez == 1 ~ 0,
    rad_part_szervez == 1 ~ 0,
    TRUE ~ rightmoz # Keep the original value if none of the conditions match
  ))

count(HUNdata$rightmoz)

# Recode leftist mobilization as 0 if a mainstream right or radical right party organized it
HUNdata <- HUNdata %>%
  mutate(leftmoz = case_when(
    right_part_szervez == 1 ~ 0,
    rad_part_szervez == 1 ~ 0,
    TRUE ~ leftmoz # Keep the original value if none of the conditions match
  ))

count(HUNdata$leftmoz)

# Recode radical right as 0 mobilization if a left or mainstream right party organized it
HUNdata <- HUNdata %>%
  mutate(radrightmoz = case_when(
    left_part_szervez == 1 ~ 0,
    right_part_szervez == 1 ~ 0,
    TRUE ~ radrightmoz # Keep the original value if none of the conditions match
  ))

count(HUNdata$radrightmoz)


# Aggregation to finalized partisan protest categories (rightist, leftist, radical rightist, as well as right_all)

HUNdata$leftist <- HUNdata$left_part_szervez==1 | HUNdata$left_part_tamogat==1 | HUNdata$leftmoz==1 
count(HUNdata$leftist)

HUNdata$rightist<- HUNdata$right_part_szervez==1 | HUNdata$right_part_tamogat==1 | HUNdata$rightmoz==1 
count(HUNdata$rightist)

HUNdata$radrightist <-HUNdata$rad_part_szervez==1 | HUNdata$rad_part_tamogat==1 | HUNdata$radrightmoz==1 
count(HUNdata$radrightist)

HUNdata$right_all<- HUNdata$right_part_szervez==1 | HUNdata$right_part_tamogat==1 | HUNdata$rightmoz==1  |HUNdata$rad_part_szervez==1 | HUNdata$rad_part_tamogat==1 | HUNdata$radrightmoz==1 
count(HUNdata$right_all)

# Use v024 variable (strategies and methods of protest) to aggregate up to "disruptive" vs "non-disruptive" protest
HUNdata$action_violent<-0
HUNdata$action_violent[HUNdata$v02401==1 | HUNdata$v02402==1 |
                         HUNdata$v02403==1 |HUNdata$v02404==1 |
                         HUNdata$v02405==1 |HUNdata$v02406==1] <-1
count(HUNdata$action_violent==1)

HUNdata$action_statement<-0
HUNdata$action_statement[HUNdata$v02416==1] <-1
count(HUNdata$action_statement==1)

HUNdata$action_legal<-0
HUNdata$action_legal[HUNdata$v02412==1 |HUNdata$v02417==1 ] <-1
count(HUNdata$action_legal==1)

HUNdata$action_blockade<-0
HUNdata$action_blockade[HUNdata$v02408==1 |HUNdata$v02410==1 ] <-1
count(HUNdata$action_blockade==1)

HUNdata$action_symbolic<-0
HUNdata$action_symbolic[HUNdata$v02418==1 |HUNdata$v02419==1 |
                          HUNdata$v02420==1 |HUNdata$v02422==1 |
                          HUNdata$v02424==1 |HUNdata$v02425==1 ] <-1
count(HUNdata$action_symbolic==1)

HUNdata$action_strike<-0
HUNdata$action_strike[HUNdata$v02407==1 |HUNdata$v02415==1 ] <-1
count(HUNdata$action_strike==1)

HUNdata$action_demonstration<-0
HUNdata$action_demonstration[HUNdata$v02409==1 |HUNdata$v02414==1 ] <-1
count(HUNdata$action_demonstration==1)

HUNdata$action_disruptive<-0
HUNdata$action_disruptive[HUNdata$action_blockade==1 |HUNdata$action_violent==1 |
                            HUNdata$action_strike==1 |HUNdata$action_demonstration==1 ] <-1
count(HUNdata$action_disruptive==1)

HUNdata$action_nondisruptive<-0
HUNdata$action_nondisruptive[HUNdata$action_legal==1 |HUNdata$action_symbolic==1 |
                               HUNdata$action_statement==1 ] <-1
count(HUNdata$action_nondisruptive==1)


## For protest magnitude measures, clean two variables (no. of participants and duration) that are numeric wherein 
## '99' stands for N/A 

count(HUNdata$v008)
count(HUNdata$v016)

HUNdata <- HUNdata %>%
  mutate(v008 = na_if(v008, 99)) %>%
  mutate(v016 = na_if(v016, 99)) %>%
  mutate(v008 = na_if(v008, 0)) %>%
  mutate(v016 = na_if(v016, 0)) 

count(HUNdata$v008)
count(HUNdata$v016)

# Now recode in terms of average number of days and average number of protesters.
HUNdata <- HUNdata %>%
  mutate(number_protestors = case_when(
    v016  == 1 ~ 10,
    v016  == 2 ~ 110,
    v016  == 3 ~ 350,
    v016  == 4 ~ 750,
    v016  == 5 ~ 1500,
    v016  == 6 ~ 6000,
    v016  == 7 ~ 30000,
    v016  == 8 ~ 50000
  ))


HUNdata <- HUNdata %>%
  mutate(protest_duration = case_when(
    v008  == 1 ~ 0.333,
    v008  == 2 ~ 0.666,
    v008  == 3 ~ 4.5,
    v008  == 4 ~ 19,
    v008  == 5 ~ 30,
    v008  == 6 ~ NA
  ))

count(HUNdata$protest_duration) 
count(HUNdata$number_protestors)


# Developing a protest salience measure #

#Following Gillion (2013), we conceptualize the scope of information in protest in terms of levels of salience.
#Salient political protest is defined as including any of the following: (1) protest activity
#that involves more than 100 individuals, (2) protest activity that lasts more than a day, (3)
#protest activity that is supported by a political organization, (4) protest activity that results
#in property damage, (5) protest activity that draws a police presence, (6) protest activity that
#leads to an arrest, (7) protest activity that involves individuals carrying weapons, (8) protest
#activity that leads to injury, or (9) protest activity that involves death. We transfer the nine
#definitions given above into binary variables and then sum across the binary variables to
#calculate a salience score for each instance of protest. 

#1) more than 100 individuals
HUNdata$size <- 0
HUNdata$size[HUNdata$number_protestors >10] <- 1
count(HUNdata$size)

#2) more than 1 day
HUNdata$duration <- 0
HUNdata$duration[HUNdata$protest_duration > 0.666] <- 1
count(HUNdata$duration)

#3) political org support
HUNdata$politicalorgsupport <- 0
HUNdata$politicalorgsupport[HUNdata$left_part_szervez==1 | HUNdata$left_part_tamogat==1  | HUNdata$right_part_szervez==1 | 
                              HUNdata$right_part_tamogat==1 | HUNdata$rad_part_szervez==1 | HUNdata$rad_part_tamogat==1 ] <- 1
count(HUNdata$politicalorgsupport)

#4) property damage
HUNdata$propertydamage <- 0
HUNdata$propertydamage[HUNdata$v02401 ==1] <- 1
count(HUNdata$propertydamage)

#5) draws police presence / intervention
HUNdata$police <- 0
HUNdata$police[HUNdata$v055 ==2 | HUNdata$v055==3] <- 1
count(HUNdata$police)

#6) leads to arrest
HUNdata$arrest <- 0
HUNdata$arrest[HUNdata$v05704==1] <- 1
count(HUNdata$arrest)

#7) individuals carrying weapons
HUNdata$weapons <- 0
HUNdata$weapons <- stringr::str_count(HUNdata$vdetaileddescription, stringr::str_to_lower("felfegyverkeztek"))
HUNdata$weapons <- ifelse(HUNdata$weapons>0, 1, 0)
count(HUNdata$weapons)

#8) leads to injury
HUNdata$injury <- 0
HUNdata$injury[HUNdata$v05703==1 |HUNdata$v02402 ==1 | HUNdata$v02403 ==1 |  HUNdata$v02405 ==1 |HUNdata$v02406 ==1] <- 1
count(HUNdata$injury)

#9) leads to death
HUNdata$deadly <- 0
HUNdata$deadly[HUNdata$v02403 ==1] <- 1
count(HUNdata$deadly)

# Aggregating 9 sub-indices to a protest salience index #
HUNdata$salience <- 0
HUNdata$salience <- HUNdata$size + HUNdata$duration + HUNdata$politicalorgsupport + HUNdata$propertydamage +
  HUNdata$police + HUNdata$arrest + HUNdata$arrest + HUNdata$injury + HUNdata$deadly
count(HUNdata$salience)

# The cleaned version saved below can be used in the analyses moving forward
write.csv(HUNdata,"HungarianProtests_cleaned.csv", row.names = FALSE)

