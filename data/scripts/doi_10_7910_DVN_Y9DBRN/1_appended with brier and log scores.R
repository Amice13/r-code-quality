setwd(dir = "XXX/Replication")

# Necessary Packages
library("readxl")
library("haven")
library("tidyverse")
library("gtools")
library("survey")
library("Hmisc")
library("gridExtra")
library("ggpubr")
library("reshape2")
library("scoring")
library("MLmetrics")

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#REAL LIFE DATA 

# Real US level mobility data
real_relative <- c(.337, .28, .184, .123, .075)
real_absolute <- .50 

# Real state level absolute mobility data set 
abs_chetty <- read_dta("data_for_importing/table2_state_absmob_by_cohort.dta")
abs_chetty <- abs_chetty[ !(abs_chetty$cohort %in% c(1940, 1950, 1960, 1970)),]
names(abs_chetty)[names(abs_chetty) == "state_fips"] <- "inputstate"

# Real CZ level relative mobility data
chetty_data <- read_excel("data_for_importing/online_data_tables.xls", sheet = "Online Data Table 6",skip = 12)[, c(1:1, 5:9)]
names(chetty_data)[names(chetty_data) == "CZ"] <- "cz"
chetty_data$cz <- as.numeric(chetty_data$cz)
chetty_data$`P(Child q1 |Par q1)` <- as.numeric(chetty_data$`P(Child q1 |Par q1)`) 
chetty_data$`P(Child q2 |Par q1)` <- as.numeric(chetty_data$`P(Child q2 |Par q1)`) 
chetty_data$`P(Child q3 |Par q1)` <- as.numeric(chetty_data$`P(Child q3 |Par q1)`) 
chetty_data$`P(Child q4 |Par q1)` <- as.numeric(chetty_data$`P(Child q4 |Par q1)`) 
chetty_data$`P(Child q5 |Par q1)` <- as.numeric(chetty_data$`P(Child q5 |Par q1)`)
chetty_data <- na.omit(chetty_data)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# BRIGHTLINE WATCH DATA
brightlinewatch <- read_dta("data_for_importing/DART0035_A_OUTPUT-ZIP-CORRECTED.dta")

#changing who number percentages to decimals
brightlinewatch$Q14grid_1_1 <- brightlinewatch$Q14grid_1_1 / 100 
brightlinewatch$Q14grid_2_1 <- brightlinewatch$Q14grid_2_1 / 100 
brightlinewatch$Q14grid_3_1 <- brightlinewatch$Q14grid_3_1 / 100 
brightlinewatch$Q14grid_4_1 <- brightlinewatch$Q14grid_4_1 / 100 
brightlinewatch$Q14grid_5_1 <- brightlinewatch$Q14grid_5_1 / 100 

brightlinewatch$Q19grid_1_1 <- brightlinewatch$Q19grid_1_1 / 100 
brightlinewatch$Q19grid_2_1 <- brightlinewatch$Q19grid_2_1 / 100 
brightlinewatch$Q19grid_3_1 <- brightlinewatch$Q19grid_3_1 / 100 
brightlinewatch$Q19grid_4_1 <- brightlinewatch$Q19grid_4_1 / 100 
brightlinewatch$Q19grid_5_1 <- brightlinewatch$Q19grid_5_1 / 100 

brightlinewatch$Q21 <- brightlinewatch$Q21 / 100 
brightlinewatch$Q31 <- brightlinewatch$Q31 / 100 

#----------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# Civic Pulse data
elite_data <- read.csv("data_for_importing/CivicPulse_GrossmannNyhan_Spring_2020.csv" )
names(elite_data)[names(elite_data) == "State_abb"] <- "state_name"

# US & CZ variables --> random variable assignment 
elite_data <- mutate(elite_data, Q14grid_1_1 = ifelse(GN_Scenario1 == "US", elite_data$GN_scenario1_1, elite_data$GN_scenario2_1)) 
elite_data <- mutate(elite_data, Q14grid_2_1 = ifelse(GN_Scenario1 == "US", elite_data$GN_scenario1_2, elite_data$GN_scenario2_2)) 
elite_data <- mutate(elite_data, Q14grid_3_1 = ifelse(GN_Scenario1 == "US", elite_data$GN_scenario1_3, elite_data$GN_scenario2_3)) 
elite_data <- mutate(elite_data, Q14grid_4_1 = ifelse(GN_Scenario1 == "US", elite_data$GN_scenario1_4, elite_data$GN_scenario2_4)) 
elite_data <- mutate(elite_data, Q14grid_5_1 = ifelse(GN_Scenario1 == "US", elite_data$GN_scenario1_5, elite_data$GN_scenario2_5)) 

elite_data <- mutate(elite_data, Q19grid_1_1 = ifelse(GN_Scenario1 == "Local Area", elite_data$GN_scenario1_1, elite_data$GN_scenario2_1)) 
elite_data <- mutate(elite_data, Q19grid_2_1 = ifelse(GN_Scenario1 == "Local Area", elite_data$GN_scenario1_2, elite_data$GN_scenario2_2)) 
elite_data <- mutate(elite_data, Q19grid_3_1 = ifelse(GN_Scenario1 == "Local Area", elite_data$GN_scenario1_3, elite_data$GN_scenario2_3)) 
elite_data <- mutate(elite_data, Q19grid_4_1 = ifelse(GN_Scenario1 == "Local Area", elite_data$GN_scenario1_4, elite_data$GN_scenario2_4)) 
elite_data <- mutate(elite_data, Q19grid_5_1 = ifelse(GN_Scenario1 == "Local Area", elite_data$GN_scenario1_5, elite_data$GN_scenario2_5)) 

elite_data <- subset( elite_data, select = -c(GN_Scenario1, GN_scenario1_1, GN_scenario1_2, GN_scenario1_3, GN_scenario1_4, GN_scenario1_5, GN_scenario2_1, GN_scenario2_2, GN_scenario2_3, GN_scenario2_4, GN_scenario2_5))

# Converting percentages into decimals
elite_data$Q14grid_1_1 <- elite_data$Q14grid_1_1 / 100
elite_data$Q14grid_2_1 <- elite_data$Q14grid_2_1 / 100
elite_data$Q14grid_3_1 <- elite_data$Q14grid_3_1 / 100
elite_data$Q14grid_4_1 <- elite_data$Q14grid_4_1 / 100
elite_data$Q14grid_5_1 <- elite_data$Q14grid_5_1 / 100

elite_data$Q19grid_1_1 <- elite_data$Q19grid_1_1 / 100
elite_data$Q19grid_2_1 <- elite_data$Q19grid_2_1 / 100
elite_data$Q19grid_3_1 <- elite_data$Q19grid_3_1 / 100
elite_data$Q19grid_4_1 <- elite_data$Q19grid_4_1 / 100
elite_data$Q19grid_5_1 <- elite_data$Q19grid_5_1 / 100

elite_data$Q21 <- elite_data$Q21 / 100
elite_data$Q31 <- elite_data$Q31 / 100 
#-------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#Miscellaneous Merge prep
# Zip codes --> CZ crosswalk data set
cz_zip_cw <- read_dta("data_for_importing/cw_zip99_cz.dta" )
names(cz_zip_cw)[names(cz_zip_cw) == "zip99nov"] <- "correctzip"
cz_zip_cw <- na.omit(cz_zip_cw)

#converting back to old zipcodes for zipcode changes listed in post office bulletin where new zip codes
#weren't found in the crosswalk
changed_zip <- read_excel("data_for_importing/zipcode research1.xlsx")
changed_zip <- subset(changed_zip, select = -c(State, Post_Office_Name, County, Station_Branch_Unit, Unit_Type, Effective_Date, Comments, Source))
zip_research <- changed_zip %>% group_by(ID) %>% 
  summarise(zipcodes = paste(zipcode, collapse = ",")) %>% separate(zipcodes, c("Before", "inputzip"))
zip_research <- subset(zip_research, select = -c(ID))
zip_research <- zip_research[!duplicated(zip_research$inputzip), ]
zip_research$inputzip <- as.numeric(zip_research$inputzip)
zip_research$Before <- as.numeric(zip_research$Before)

#update race for "Other" respondents
updated_race <- read.csv("data_for_importing/Race_data_supplement_R.csv")
updated_race <- subset(updated_race, select = -c(Race_6))
names(updated_race)[names(updated_race) == "X"] <- "flag"
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#MERGING
BLW_update <- left_join(brightlinewatch, zip_research, by = "inputzip" )
BLW_update$correctzip <- ifelse(is.na(BLW_update$Before), BLW_update$inputzip, BLW_update$Before)

# Public merging
BLW_abs <- left_join(BLW_update, abs_chetty, by = "inputstate")
BLW_abs_cz <- left_join(BLW_abs, cz_zip_cw, by = "correctzip")
BLW_abs_rel_cz <- left_join(BLW_abs_cz, chetty_data, by = "cz") 
abs_BLW_merge <- BLW_abs_rel_cz

# Elite merging
fixing_race_merge <- left_join(elite_data, updated_race, by = "CP_ID")
abs_civicpulse_merge <- right_join(abs_chetty, fixing_race_merge, by = "state_name")
#--------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
# APPENDING PREP

# Creating empty columns for unique variables in CP and BLW data sets
extra_civicpulse <- c("v1", "Complete", "GN_timer1_FirstClick",	"GN_timer1_LastClick",	
                      "GN_timer1_PageSubmit",	"GN_timer1_ClickCount", "GN_timer2_FirstClick",
                      "GN_timer2_LastClick",	"GN_timer2_PageSubmit",	"GN_timer2_ClickCount", 
                      "GN_timer3_FirstClick",	"GN_timer3_LastClick",	"GN_timer3_PageSubmit",	
                      "GN_timer3_ClickCount", "GN_timer4_FirstClick",	"GN_timer4_LastClick",	
                      "GN_timer4_PageSubmit",	"GN_timer4_ClickCount", "GN_timer5_FirstClick",	
                      "GN_timer5_LastClick",	"GN_timer5_PageSubmit",	"GN_timer5_ClickCount",	
                      "parenthetical_excluded","Pid", "PID_lean",	"PID_3",	"CZName",
                      "counties_in_cz", "Gov_exp",	"Electoral_compete",	"Ambition",	
                      "Professionalization", "Pid_lean",	"Race_other_flag", "flag",
                 	    "Level",	"College_prop_3",	"Population",	"Urban_prop_3",	
                      "Votes2016_per_gop_3", "Ideology", "Education", "State_code")
abs_BLW_merge[,extra_civicpulse] <- NA

extra_BLW <- c("consent", "statements_1", "statements_2", "statements_3", "statements_4", 
               "statements_5","statements_6", "statements_7", "statements_8", "statements_9",
               "statements_10","statements_11","statements_12","statements_13","statements_14",
               "statements_15","statements_16","statements_17","statements_18","statements_19",
               "statements_20","statements_21","statements_22","statements_23","statements_24",
               "statements_25","statements_26","statements_27","statements_28","rating_USA",
               "democracy_importance","pid7","polinterest","approve_trmp","agreement","senator_term",
               "prestimes","age", "age9", "birthyr", "educ", "educ7", "num_senators","prime_minister","houserep_term",
               "answer_lookup","take_serious","marstat","employ","faminc_new","votereg","newsint", "Before",
               "religpew","pew_churatd","pew_bornagain","pew_religimp","pew_prayer", "race",
               "ethnicity", "ideo7", "comments", "inputzip","pid3", "pid3_t", "race_other")  
abs_civicpulse_merge[,extra_BLW] <- NA

abs_civicpulse_merge$correctzip <- abs_civicpulse_merge$inputzip

# Matching variable names
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "CP_ID"] <- "caseid"
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "StartDate"] <- "starttime"
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "EndDate"] <- "endtime"
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "CZ"] <- "cz"
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "PChildq1Parq1"] <- "P(Child q1 |Par q1)"
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "PChildq2Parq1"] <- "P(Child q2 |Par q1)"
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "PChildq3Parq1"] <- "P(Child q3 |Par q1)"
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "PChildq4Parq1"] <- "P(Child q4 |Par q1)"
names(abs_civicpulse_merge)[names(abs_civicpulse_merge) == "PChildq5Parq1"] <- "P(Child q5 |Par q1)"

# Gender
abs_BLW_merge <- mutate(abs_BLW_merge, Sex = ifelse(gender == 1, "Male", 
                                                    ifelse(gender == 2, "Female", NA)))

abs_BLW_merge <- subset( abs_BLW_merge, select = -c(gender))

# Education
abs_BLW_merge <- mutate(abs_BLW_merge, Highest_Education = ifelse(educ7 == 1, "Less than high school",
                                                                  ifelse(educ7 == 2, "High school graduate",
                                                                         ifelse(educ7 == 3, "Some college",
                                                                                ifelse(educ7 %in% 4:5, "College graduate",
                                                                                       ifelse(educ7 %in% 6:7, "Graduate degree", NA))))))

abs_civicpulse_merge <- mutate(abs_civicpulse_merge, Highest_Education = ifelse(Education == "Less than high school", "Less than high school",
                                                                                ifelse(Education == "High school graduate", "High school graduate",
                                                                                       ifelse(Education == "Some college", "Some college",
                                                                                              ifelse(Education == "Technical/trade school", "Some college",
                                                                                                     ifelse(Education == "College graduate", "College graduate",
                                                                                                            ifelse(Education == "Some graduate school", "College graduate",
                                                                                                                   ifelse(Education == "Graduate degree", "Graduate degree", NA))))))))


#Birth cohort
abs_BLW_merge <- mutate(abs_BLW_merge, Birth = ifelse(birthyr %in% 0000:1910, "1910 or earlier",
                                                      ifelse(birthyr %in% 1911:1915, "1911-1915",
                                                             ifelse(birthyr %in% 1916:1920, "1916-1920",
                                                                    ifelse(birthyr %in% 1921:1925, "1921-1925",
                                                                           ifelse(birthyr %in% 1926:1930, "1926-1930",
                                                                                  ifelse(birthyr %in% 1931:1935, "1931-1935",
                                                                                         ifelse(birthyr %in% 1936:1940, "1936-1940",
                                                                                                ifelse(birthyr %in% 1941:1945, "1941-1945",
                                                                                                       ifelse(birthyr %in% 1946:1950, "1946-1950",
                                                                                                              ifelse(birthyr %in% 1951:1955, "1951-1955",
                                                                                                                     ifelse(birthyr %in% 1956:1960, "1956-1960",
                                                                                                                            ifelse(birthyr %in% 1961:1965, "1961-1965",
                                                                                                                                   ifelse(birthyr %in% 1966:1970, "1966-1970",
                                                                                                                                          ifelse(birthyr %in% 1971:1975, "1971-1975",
                                                                                                                                                 ifelse(birthyr %in% 1976:1980, "1976-1980",
                                                                                                                                                        ifelse(birthyr %in% 1981:1985, "1981-1985",
                                                                                                                                                               ifelse(birthyr %in% 1986:1990, "1986-1990",
                                                                                                                                                                      ifelse(birthyr %in% 1991:1995, "1991-1995",
                                                                                                                                                                             ifelse(birthyr %in% 1996:2000, "1996-2000",
                                                                                                                                                                                    ifelse(birthyr %in% 2001:2005, "2001-2005", 
                                                                                                                                                                                           ifelse(birthyr %in% 2006:2020, "2006 or later", NA))))))))))))))))))))))

# Party
abs_BLW_merge <- mutate(abs_BLW_merge, Pid_unified = ifelse(pid3 == 1, "Democrat",
                                                            ifelse(pid3 == 2, "Republican",
                                                                   ifelse(pid3 %in% 3:5, "Other", NA))))

abs_civicpulse_merge <- mutate(abs_civicpulse_merge, Pid_unified = ifelse(Pid == "Democrat", "Democrat",
                                                                          ifelse(Pid == "Republican", "Republican",
                                                                                 ifelse(Pid == "Independent", "Other", 
                                                                                        ifelse(Pid == "Other", "Other", NA)))))
# Ideology
abs_BLW_merge <- mutate(abs_BLW_merge, Ideo_unified = ifelse(ideo7 %in% 1:3, "Conservative",
                                                             ifelse(ideo7 %in% 5:7, "Liberal",
                                                                    ifelse(ideo7 == 4, "Neither", NA))))

abs_civicpulse_merge <- mutate(abs_civicpulse_merge, Ideo_unified = ifelse(Ideology == "Very conservative", "Conservative",
                                                                           ifelse(Ideology == "Somewhat conservative", "Conservative",
                                                                                  ifelse(Ideology == "Somewhat liberal", "Liberal",
                                                                                         ifelse(Ideology == "Very liberal","Liberal",
                                                                                                ifelse(Ideology == "Moderate, middle of the road", "Neither", 
                                                                                                       ifelse(Ideology == "Not sure", "Neither", NA)))))))
# Race
abs_BLW_merge <- mutate(abs_BLW_merge, NonHispanic_white= ifelse(race == 1, 1,
                                                                 ifelse(race %in% 2:8, 0, NA)))

abs_civicpulse_merge$NonHispanic_white[abs_civicpulse_merge$flag == 0] <- 0
abs_civicpulse_merge$NonHispanic_white[abs_civicpulse_merge$flag == 1] <- 1
abs_civicpulse_merge$NonHispanic_white[is.na(abs_civicpulse_merge$flag) & abs_civicpulse_merge$Race_other_flag == 1] <- NA

#Q15
abs_BLW_merge <- mutate(abs_BLW_merge, reasons_1 = ifelse(Q15grid_1 == 1, 5,
                                                      ifelse(Q15grid_1 == 2, 4,
                                                             ifelse(Q15grid_1 == 3, 3,
                                                                    ifelse(Q15grid_1 == 4, 2,
                                                                           ifelse(Q15grid_1 == 5, 1, NA))))))

abs_BLW_merge <- mutate(abs_BLW_merge, reasons_2 = ifelse(Q15grid_2 == 1, 5,
                                                      ifelse(Q15grid_2 == 2, 4,
                                                             ifelse(Q15grid_2 == 3, 3,
                                                                    ifelse(Q15grid_2 == 4, 2,
                                                                           ifelse(Q15grid_2 == 5, 1, NA))))))

abs_BLW_merge <- mutate(abs_BLW_merge, reasons_3 = ifelse(Q15grid_3 == 1, 5,
                                                      ifelse(Q15grid_3 == 2, 4,
                                                             ifelse(Q15grid_3 == 3, 3,
                                                                    ifelse(Q15grid_3 == 4, 2,
                                                                           ifelse(Q15grid_3 == 5, 1, NA))))))

abs_BLW_merge <- mutate(abs_BLW_merge, reasons_4 = ifelse(Q15grid_4 == 1, 5,
                                                      ifelse(Q15grid_4 == 2, 4,
                                                             ifelse(Q15grid_4 == 3, 3,
                                                                    ifelse(Q15grid_4 == 4, 2,
                                                                           ifelse(Q15grid_4 == 5, 1, NA))))))

abs_BLW_merge <- mutate(abs_BLW_merge, reasons_5 = ifelse(Q15grid_5 == 1, 5,
                                                      ifelse(Q15grid_5 == 2, 4,
                                                             ifelse(Q15grid_5 == 3, 3,
                                                                    ifelse(Q15grid_5 == 4, 2,
                                                                           ifelse(Q15grid_5 == 5, 1, NA))))))

abs_BLW_merge <- subset(abs_BLW_merge, select = -c(Q15grid_1, Q15grid_2, Q15grid_3, Q15grid_4, Q15grid_5))

abs_civicpulse_merge <- mutate(abs_civicpulse_merge, reasons_5 = ifelse(Q15_5 == "Essential", 5, 
                                                                        ifelse(Q15_5 == "Very important", 4, 
                                                                               ifelse(Q15_5 == "Fairly important", 3, 
                                                                                      ifelse(Q15_5 == "Not very important", 2, 
                                                                                             ifelse(Q15_5 == "Not important at all", 1, NA))))))
abs_civicpulse_merge <- mutate(abs_civicpulse_merge, reasons_4 = ifelse(Q15_4 == "Essential", 5, 
                                                                        ifelse(Q15_4 == "Very important", 4, 
                                                                               ifelse(Q15_4 == "Fairly important", 3, 
                                                                                      ifelse(Q15_4 == "Not very important", 2, 
                                                                                             ifelse(Q15_4 == "Not important at all", 1, NA))))))
abs_civicpulse_merge <- mutate(abs_civicpulse_merge, reasons_3 = ifelse(Q15_3 == "Essential", 5, 
                                                                        ifelse(Q15_3 == "Very important", 4, 
                                                                               ifelse(Q15_3 == "Fairly important", 3, 
                                                                                      ifelse(Q15_3 == "Not very important", 2, 
                                                                                             ifelse(Q15_3 == "Not important at all", 1, NA))))))
abs_civicpulse_merge <- mutate(abs_civicpulse_merge, reasons_2 = ifelse(Q15_2 == "Essential", 5, 
                                                                        ifelse(Q15_2 == "Very important", 4, 
                                                                               ifelse(Q15_2 == "Fairly important", 3, 
                                                                                      ifelse(Q15_2 == "Not very important", 2, 
                                                                                             ifelse(Q15_2 == "Not important at all", 1, NA))))))
abs_civicpulse_merge <- mutate(abs_civicpulse_merge, reasons_1 = ifelse(Q15_1 == "Essential", 5, 
                                                                        ifelse(Q15_1 == "Very important", 4, 
                                                                               ifelse(Q15_1 == "Fairly important", 3, 
                                                                                      ifelse(Q15_1 == "Not very important", 2, 
                                                                                             ifelse(Q15_1 == "Not important at all", 1, NA))))))
abs_civicpulse_merge <- subset(abs_civicpulse_merge, select = -c(Q15_1, Q15_2, Q15_3, Q15_4, Q15_5))

# Match start time & end time variable types
abs_BLW_merge$endtime <- as.character(abs_BLW_merge$endtime)
abs_BLW_merge$starttime <- as.character(abs_BLW_merge$starttime)
abs_civicpulse_merge$starttime <- as.character(abs_civicpulse_merge$starttime)
abs_civicpulse_merge$endtime <- as.character(abs_civicpulse_merge$endtime)

#----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#APPENDING
abs_BLW_merge$population <- "Public"
abs_civicpulse_merge$population <- "Elite"
appended <- rbind(abs_BLW_merge, abs_civicpulse_merge)

# Aligning CP & BLW responses with correct quintiles
names(appended)[names(appended) == "Q14grid_1_1"] <- "holding1" 
names(appended)[names(appended) == "Q14grid_2_1"] <- "holding2"
names(appended)[names(appended) == "Q14grid_4_1"] <- "Q14grid_2_1" 
names(appended)[names(appended) == "Q14grid_5_1"] <- "Q14grid_1_1" 
names(appended)[names(appended) == "holding1"] <- "Q14grid_5_1" 
names(appended)[names(appended) == "holding2"] <- "Q14grid_4_1"

names(appended)[names(appended) == "Q19grid_1_1"] <- "holding1" 
names(appended)[names(appended) == "Q19grid_2_1"] <- "holding2"
names(appended)[names(appended) == "Q19grid_4_1"] <- "Q19grid_2_1" 
names(appended)[names(appended) == "Q19grid_5_1"] <- "Q19grid_1_1" 
names(appended)[names(appended) == "holding1"] <- "Q19grid_5_1" 
names(appended)[names(appended) == "holding2"] <- "Q19grid_4_1"

#-----------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# SIGNED ERROR IN PERCEPTIONS

# (Signed) Relative mobility, commuting zone error
appended$cz_p1k1error <- appended$Q19grid_1_1 - appended$`P(Child q1 |Par q1)`
appended$cz_p1k2error <- appended$Q19grid_2_1 - appended$`P(Child q2 |Par q1)`
appended$cz_p1k3error <- appended$Q19grid_3_1 - appended$`P(Child q3 |Par q1)`
appended$cz_p1k4error <- appended$Q19grid_4_1 - appended$`P(Child q4 |Par q1)`
appended$cz_p1k5error <- appended$Q19grid_5_1 - appended$`P(Child q5 |Par q1)`

# (Signed) Relative mobility, national error
appended$us_p1k1error <- appended$Q14grid_1_1 - real_relative[1]
appended$us_p1k2error <- appended$Q14grid_2_1 - real_relative[2]
appended$us_p1k3error <- appended$Q14grid_3_1 - real_relative[3]
appended$us_p1k4error <- appended$Q14grid_4_1 - real_relative[4]
appended$us_p1k5error <- appended$Q14grid_5_1 - real_relative[5]

# (Signed) Absolute mobility, state error
appended$state_error <- appended$Q21 - appended$cohort_mean

# (Signed) Absolute mobility, national error
appended$us_error <- appended$Q31 - real_absolute

#------------------------------------------------------------------------------------------------------
# ABSOLUTE VALUE ERROR IN PERCEPTIONS 

# (Absolute) Relative mobility, commuting zone error
appended$abs_cz_p1k1error <- abs(appended$Q19grid_1_1 - appended$`P(Child q1 |Par q1)`)
appended$abs_cz_p1k2error <- abs(appended$Q19grid_2_1 - appended$`P(Child q2 |Par q1)`)
appended$abs_cz_p1k3error <- abs(appended$Q19grid_3_1 - appended$`P(Child q3 |Par q1)`)
appended$abs_cz_p1k4error <- abs(appended$Q19grid_4_1 - appended$`P(Child q4 |Par q1)`)
appended$abs_cz_p1k5error <- abs(appended$Q19grid_5_1 - appended$`P(Child q5 |Par q1)`)

# (Absolute) Relative mobility, national error
appended$abs_us_p1k1error <- abs(appended$Q14grid_1_1 - real_relative[1])
appended$abs_us_p1k2error <- abs(appended$Q14grid_2_1 - real_relative[2])
appended$abs_us_p1k3error <- abs(appended$Q14grid_3_1 - real_relative[3])
appended$abs_us_p1k4error <- abs(appended$Q14grid_4_1 - real_relative[4])
appended$abs_us_p1k5error <- abs(appended$Q14grid_5_1 - real_relative[5])

# (Absolute) Absolute mobility, state error
appended$abs_state_error <- abs(appended$Q21 - appended$cohort_mean)

# (Absolute) Absolute mobility, national error
appended$abs_us_error <- abs(appended$Q31 - real_absolute)
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# BRIER SCORES

appended$rel_cz_brierscore <- NA
appended$rel_us_brierscore <- NA
appended$abs_state_brierscore <- NA
appended$abs_us_brierscore <- NA

end <- nrow(appended)
#------------------------------------------------------------------------------------------------------
# Relative CZ Brier Scores

for (i in 1:end){
  # True probability matrix
  real_rel_cz_probs <- matrix(c(appended$`P(Child q1 |Par q1)`[i], appended$`P(Child q2 |Par q1)`[i], appended$`P(Child q3 |Par q1)`[i], appended$`P(Child q4 |Par q1)`[i], appended$`P(Child q5 |Par q1)`[i]))
  num_ones_1 <- real_rel_cz_probs[1] * 1000
  if (is.na(num_ones_1)) next
  num_ones_2 <- real_rel_cz_probs[2] * 1000 + num_ones_1
  num_ones_3 <- real_rel_cz_probs[3] * 1000 + num_ones_2
  num_ones_4 <- real_rel_cz_probs[4] * 1000 + num_ones_3
  num_ones_5 <- real_rel_cz_probs[5] * 1000 + num_ones_4
  index1 <- num_ones_1 + 1
  index2 <- num_ones_2 + 1
  index3 <- num_ones_3 + 1
  index4 <- num_ones_4 + 1
  
  base_matrix <- matrix(0, 1000, 5)
  prob_matrix <- base_matrix
  prob_matrix[1:num_ones_1,1] <- 1
  prob_matrix[index1:num_ones_2, 2] <- 1
  prob_matrix[index2:num_ones_3, 3] <- 1
  prob_matrix[index3:num_ones_4, 4] <- 1
  prob_matrix[index4:num_ones_5, 5] <- 1
  
# Perceptions matrix
  guesses <- matrix(0, 5, 1)
  guesses[1] <- appended$Q19grid_1_1[i]
  guesses[2] <- appended$Q19grid_2_1[i]
  guesses[3] <- appended$Q19grid_3_1[i]
  guesses[4] <- appended$Q19grid_4_1[i]
  guesses[5] <- appended$Q19grid_5_1[i]
  if (is.na(guesses[1])) next
  if (is.na(guesses[2])) next
  if (is.na(guesses[3])) next
  if (is.na(guesses[4])) next
  if (is.na(guesses[5])) next
  guesses <- t(guesses)
  guesses <- guesses[rep(1, times = 1000),] 
  
# Calculating Brier Scores!!
  difference <- guesses - prob_matrix
  difference <- difference^2
  difference <- rowSums(difference)
  score <- mean(difference)
  
  z = i
  appended$rel_cz_brierscore[z] <- score
}
#------------------------------------------------------------------------------------------------------
# Relative US Brier Scores

# True probability matrix
real_rel_us_probs <- matrix(c(real_relative[1], real_relative[2], real_relative[3], real_relative[4], real_relative[5]))
num_ones_1 <- real_rel_us_probs[1] * 1000
num_ones_2 <- real_rel_us_probs[2] * 1000 + num_ones_1
num_ones_3 <- real_rel_us_probs[3] * 1000 + num_ones_2
num_ones_4 <- real_rel_us_probs[4] * 1000 + num_ones_3
num_ones_5 <- real_rel_us_probs[5] * 1000 + num_ones_4
index1 <- num_ones_1 + 1
index2 <- num_ones_2 + 1
index3 <- num_ones_3 + 1
index4 <- num_ones_4 + 1

base_matrix <- matrix(0, 1000, 5)
prob_matrix <- base_matrix
prob_matrix[1:num_ones_1,1] <- 1
prob_matrix[index1:num_ones_2, 2] <- 1
prob_matrix[index2:num_ones_3, 3] <- 1
prob_matrix[index3:num_ones_4, 4] <- 1
prob_matrix[index4:num_ones_5, 5] <- 1

for (i in  1:end) {
# Perceptions matrix
  guesses <- matrix(0, 5, 1)
  guesses[1] <- appended$Q14grid_1_1[i]
  guesses[2] <- appended$Q14grid_2_1[i]
  guesses[3] <- appended$Q14grid_3_1[i]
  guesses[4] <- appended$Q14grid_4_1[i]
  guesses[5] <- appended$Q14grid_5_1[i]
  if (is.na(guesses[1])) next
  if (is.na(guesses[2])) next
  if (is.na(guesses[3])) next
  if (is.na(guesses[4])) next
  if (is.na(guesses[5])) next
  
  guesses <- t(guesses)
  guesses <- guesses[rep(1, times = 1000),] 
  
# Calculating Brier Scores!!
  difference <- guesses - prob_matrix
  difference <- difference^2
  difference <- rowSums(difference)
  score <- mean(difference)
  
  z = i
  appended$rel_us_brierscore[z] <- score
}
#--------------------------------------------------------------------------------------------------------
# Absolute State Brier Scores

for (i in 1:end){
# True probability matrix
  real_abs_state_probs <- matrix(c(appended$cohort_mean[i]))
  num_ones_1 <- real_abs_state_probs[1] * 1000
  if (is.na(num_ones_1)) next
  index1 <- num_ones_1 + 1
  
  base_matrix <- matrix(0, 1000, 2)
  prob_matrix <- base_matrix
  prob_matrix[1:num_ones_1,1] <- 1
  prob_matrix[index1:1000, 2] <- 1
  
# Perceptions matrix
  guesses <- matrix(0, 2, 1)
  guesses[1] <- appended$Q21[i]
  guesses[2] <- 1 - appended$Q21[i]
  if (is.na(guesses[1])) next
  if (is.na(guesses[2])) next
  guesses <- t(guesses)
  guesses <- guesses[rep(1, times = 1000),] 
  
# Calculating Brier Score!!
  difference <- guesses - prob_matrix
  difference <- difference^2
  difference <- rowSums(difference)
  score <- mean(difference)
  
  z = i
  appended$abs_state_brierscore[z] <- score
}
#--------------------------------------------------------------------------------------------------------
# Absolute US Brier Scores

# True probability matrix
real_abs_us_probs <- matrix(c(real_absolute))
num_ones_1 <- real_abs_us_probs[1] * 1000
index1 <- num_ones_1 + 1

base_matrix <- matrix(0, 1000, 2)
prob_matrix <- base_matrix
prob_matrix[1:num_ones_1,1] <- 1
prob_matrix[index1:1000, 2] <- 1

for (i in  1:end) {
# Perceptions matrix
  guesses <- matrix(0, 2, 1)
  guesses[1] <- appended$Q31[i]
  guesses[2] <- 1 - appended$Q31[i]
  if (is.na(guesses[1])) next
  if (is.na(guesses[2])) next
  guesses <- t(guesses)
  guesses <- guesses[rep(1, times = 1000),] 
  
# Calculating Brier Score!!
  difference <- guesses - prob_matrix
  difference <- difference^2
  difference <- rowSums(difference)
  score <- mean(difference)
  
  z = i
  appended$abs_us_brierscore[z] <- score
}
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# LOG SCORES
appended$rel_cz_logscore <- NA
appended$rel_us_logscore <- NA
appended$abs_state_logscore <- NA
appended$abs_us_logscore <- NA

end <- nrow(appended)
#------------------------------------------------------------------------------------------------------
# Relative CZ Log Scores

for (i in 1:end){
  # True probability matrix
  real_rel_cz_probs <- matrix(c(appended$`P(Child q1 |Par q1)`[i], appended$`P(Child q2 |Par q1)`[i], appended$`P(Child q3 |Par q1)`[i], appended$`P(Child q4 |Par q1)`[i], appended$`P(Child q5 |Par q1)`[i]))
  num_ones_1 <- real_rel_cz_probs[1] * 1000
  if (is.na(num_ones_1)) next
  num_ones_2 <- real_rel_cz_probs[2] * 1000 + num_ones_1
  num_ones_3 <- real_rel_cz_probs[3] * 1000 + num_ones_2
  num_ones_4 <- real_rel_cz_probs[4] * 1000 + num_ones_3
  num_ones_5 <- real_rel_cz_probs[5] * 1000 + num_ones_4
  index1 <- num_ones_1 + 1
  index2 <- num_ones_2 + 1
  index3 <- num_ones_3 + 1
  index4 <- num_ones_4 + 1
  
  base_matrix <- matrix(0, 1000, 5)
  prob_matrix <- base_matrix
  prob_matrix[1:num_ones_1,1] <- 1
  prob_matrix[index1:num_ones_2, 2] <- 1
  prob_matrix[index2:num_ones_3, 3] <- 1
  prob_matrix[index3:num_ones_4, 4] <- 1
  prob_matrix[index4:num_ones_5, 5] <- 1
  
  # Perceptions matrix
  guesses <- matrix(0, 5, 1)
  guesses[1] <- appended$Q19grid_1_1[i]
  guesses[2] <- appended$Q19grid_2_1[i]
  guesses[3] <- appended$Q19grid_3_1[i]
  guesses[4] <- appended$Q19grid_4_1[i]
  guesses[5] <- appended$Q19grid_5_1[i]
  if (is.na(guesses[1])) next
  if (is.na(guesses[2])) next
  if (is.na(guesses[3])) next
  if (is.na(guesses[4])) next
  if (is.na(guesses[5])) next
  
  # Min/Max
  if(guesses[1] == 0) {guesses[1] = 0.005}
  if(guesses[1] == 1) {guesses[1] = 0.995} 
  if(guesses[2] == 0) {guesses[2] = 0.005}
  if(guesses[2] == 1) {guesses[2] = 0.995}
  if(guesses[3] == 0) {guesses[3] = 0.005}
  if(guesses[3] == 1) {guesses[3] = 0.995} 
  if(guesses[4] == 0) {guesses[4] = 0.005}
  if(guesses[4] == 1) {guesses[4] = 0.995}
  if(guesses[5] == 0) {guesses[5] = 0.005}
  if(guesses[5] == 1) {guesses[5] = 0.995}
  guesses <- t(guesses)
  guesses <- guesses[rep(1, times = 1000),] 
  
  # Calculating Log Scores!!
  logresult <- log(guesses) * prob_matrix
  logresult <- rowSums(logresult)
  logresult <- -1 * logresult
  score <- mean(logresult)
  
  #how it can be done with one function
  #score <- mean(MultiLogLoss(y_pred = guesses, y_true = prob_matrix))
  
  z = i
  appended$rel_cz_logscore[z] <- score
}

#------------------------------------------------------------------------------------------------------
# Relative US Log Scores

# True probability matrix
real_rel_us_probs <- matrix(c(real_relative[1], real_relative[2], real_relative[3], real_relative[4], real_relative[5]))
num_ones_1 <- real_rel_us_probs[1] * 1000
num_ones_2 <- real_rel_us_probs[2] * 1000 + num_ones_1
num_ones_3 <- real_rel_us_probs[3] * 1000 + num_ones_2
num_ones_4 <- real_rel_us_probs[4] * 1000 + num_ones_3
num_ones_5 <- real_rel_us_probs[5] * 1000 + num_ones_4
index1 <- num_ones_1 + 1
index2 <- num_ones_2 + 1
index3 <- num_ones_3 + 1
index4 <- num_ones_4 + 1

base_matrix <- matrix(0, 1000, 5)
prob_matrix <- base_matrix
prob_matrix[1:num_ones_1,1] <- 1
prob_matrix[index1:num_ones_2, 2] <- 1
prob_matrix[index2:num_ones_3, 3] <- 1
prob_matrix[index3:num_ones_4, 4] <- 1
prob_matrix[index4:num_ones_5, 5] <- 1

for (i in  1:end) {
  # Perceptions matrix
  guesses <- matrix(0, 5, 1)
  guesses[1] <- appended$Q14grid_1_1[i]
  guesses[2] <- appended$Q14grid_2_1[i]
  guesses[3] <- appended$Q14grid_3_1[i]
  guesses[4] <- appended$Q14grid_4_1[i]
  guesses[5] <- appended$Q14grid_5_1[i]
  if (is.na(guesses[1])) next
  if (is.na(guesses[2])) next
  if (is.na(guesses[3])) next
  if (is.na(guesses[4])) next
  if (is.na(guesses[5])) next
  
  # Min/Max
  if(guesses[1] == 0) {guesses[1] = 0.005}
  if(guesses[1] == 1) {guesses[1] = 0.995} 
  if(guesses[2] == 0) {guesses[2] = 0.005}
  if(guesses[2] == 1) {guesses[2] = 0.995}
  if(guesses[3] == 0) {guesses[3] = 0.005}
  if(guesses[3] == 1) {guesses[3] = 0.995} 
  if(guesses[4] == 0) {guesses[4] = 0.005}
  if(guesses[4] == 1) {guesses[4] = 0.995}
  if(guesses[5] == 0) {guesses[5] = 0.005}
  if(guesses[5] == 1) {guesses[5] = 0.995}
  guesses <- t(guesses)
  guesses <- guesses[rep(1, times = 1000),] 
  
  # Calculating Log Scores!!
  logresult <- log(guesses) * prob_matrix
  logresult <- rowSums(logresult)
  logresult <- -1 * logresult
  score <- mean(logresult)
  
  z = i
  appended$rel_us_logscore[z] <- score
}
#--------------------------------------------------------------------------------------------------------
# Absolute State Log Scores

for (i in 1:end){
  # True probability matrix
  real_abs_state_probs <- matrix(c(appended$cohort_mean[i]))
  num_ones_1 <- real_abs_state_probs[1] * 1000
  if (is.na(num_ones_1)) next
  index1 <- num_ones_1 + 1
  
  base_matrix <- matrix(0, 1000, 2)
  prob_matrix <- base_matrix
  prob_matrix[1:num_ones_1,1] <- 1
  prob_matrix[index1:1000, 2] <- 1
  
  # Perceptions matrix
  guesses <- matrix(0, 2, 1)
  guesses[1] <- appended$Q21[i]
  guesses[2] <- 1 - appended$Q21[i]
  if (is.na(guesses[1])) next
  if (is.na(guesses[2])) next
  
  # Min/Max
  if(guesses[1] == 0) {guesses[1] = 0.005}
  if(guesses[1] == 1) {guesses[1] = 0.995} 
  if(guesses[2] == 0) {guesses[2] = 0.005}
  if(guesses[2] == 1) {guesses[2] = 0.995}
  guesses <- t(guesses)
  guesses <- guesses[rep(1, times = 1000),] 
  
  # Calculating Log Scores!!
  logresult <- log(guesses) * prob_matrix
  logresult <- rowSums(logresult)
  logresult <- -1 * logresult
  score <- mean(logresult)
  
  z = i
  appended$abs_state_logscore[z] <- score
}
#--------------------------------------------------------------------------------------------------------
# Absolute US Log Scores

# True probability matrix
real_abs_us_probs <- matrix(c(real_absolute))
num_ones_1 <- real_abs_us_probs[1] * 1000
index1 <- num_ones_1 + 1

base_matrix <- matrix(0, 1000, 2)
prob_matrix <- base_matrix
prob_matrix[1:num_ones_1,1] <- 1
prob_matrix[index1:1000, 2] <- 1

for (i in  1:end) {
  # Perceptions matrix
  guesses <- matrix(0, 2, 1)
  guesses[1] <- appended$Q31[i]
  guesses[2] <- 1 - appended$Q31[i]
  if (is.na(guesses[1])) next
  if (is.na(guesses[2])) next
  
  # Min/Max
  if(guesses[1] == 0) {guesses[1] = 0.005}
  if(guesses[1] == 1) {guesses[1] = 0.995} 
  if(guesses[2] == 0) {guesses[2] = 0.005}
  if(guesses[2] == 1) {guesses[2] = 0.995}
  guesses <- t(guesses)
  guesses <- guesses[rep(1, times = 1000),] 
  
  # Calculating Log Scores!!
  logresult <- log(guesses) * prob_matrix
  logresult <- rowSums(logresult)
  logresult <- -1 * logresult
  score <- mean(logresult) 

  z = i
  appended$abs_us_logscore[z] <- score
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#EXPORTING DATA

# Changing column names to allow stata file to be written
names(appended)[names(appended) == "P(Child q1 |Par q1)"] <- "pchild_q1_par_q1"
names(appended)[names(appended) == "P(Child q2 |Par q1)"] <- "pchild_q2_par_q1"
names(appended)[names(appended) == "P(Child q3 |Par q1)"] <- "pchild_q3_par_q1"
names(appended)[names(appended) == "P(Child q4 |Par q1)"] <- "pchild_q4_par_q1"
names(appended)[names(appended) == "P(Child q5 |Par q1)"] <- "pchild_q5_par_q1"

# Stata file with attached Brier and Log scores
write_dta(appended, "intermediate_files/1a_brier_log_output.dta")

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
#CHECKING ABSOLUTE VALUES
repubs <- subset(appended, Pid_unified = "Republican")
repub_pub <- subset(repubs, population = "Public")

mean(repub_pub$us_p1k5error, na.rm = TRUE)
mean(repub_pub$abs_us_p1k5error, na.rm = TRUE)
