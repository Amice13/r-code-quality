# Provides the numbers for the consort chart


# Please set working directory to be source file location

rm(list = ls())

library(readr)
library(anytime)
library(dplyr)


recode_desc_vid1 <- function(x) {
  if (!is.na(x)) {
    if ((x == "deportation") | (x == "trust")) {
      return("elephant1")
    } else if ((x == "econ_latino") | (x=="econ_black")) {
      return("elephant2")
    } else { #vanilla
      return("vanilla")
    }}
  else {
    return(NA)
  }
}



simulation = 0
set.seed(302)

# Data import 
# Please set working directory to be source file location

mainlaunch <- "./Data/COVID USA T1_May 27, 2020_07.20.csv"

df=read_csv(mainlaunch)[-c(1,2),]

df$StartTime <- anytime(df$StartDate) #parse
df$duration <- as.numeric(df$`Duration (in seconds)`)

#raw dataset of accepts 

raw <- df %>% filter((StartTime > anytime("2020-05-13 15:00:00 EDT")) & (DistributionChannel == "anonymous") & (Q2 == "Yes, I would like to take part in this study, and confirm that I LIVE IN THE U.S., and I am 18 or older") & is.na(test))


#### GENERATE COMPLETED BASELINE INDICATOR

#key_baseline_vars
raw$Q14_numeric = raw$Q14

for (i in 1:nrow(raw)){
  if (!is.na(raw$Q14_numeric[i])&&(raw$Q14_numeric[i] == "8 or more")){
    raw$Q14_numeric[i] = "8"
  }
}
raw$Q14_numeric = as.numeric(raw$Q14_numeric)

raw$standard_education_numeric = as.numeric(raw$standard_education)
raw$Q19_dummy = 1-(!is.na(raw$Q19))*(raw$Q19!="None")
raw$Q17_numeric = as.numeric((raw$Q17 == "Yes"))
raw$Q10_numeric = as.numeric((raw$Q10 == "Yes"))
raw$Q12_dummy = as.numeric((raw$Q12=="Very likely")+(raw$Q12=="Somewhat likely"))


raw$age_strat = as.numeric(raw$age_strat )

raw$strat_1 = as.numeric((raw$stratum=="1"))
raw$strat_2 = as.numeric((raw$stratum=="2"))
raw$strat_3 = as.numeric((raw$stratum=="3"))
raw$strat_4 = as.numeric((raw$stratum=="4"))
raw$strat_5 = as.numeric((raw$stratum=="5"))
raw$strat_6 = as.numeric((raw$stratum=="6"))
raw$strat_7 = as.numeric((raw$stratum=="7"))
raw$strat_8 = as.numeric((raw$stratum=="8"))

raw$female=1-as.numeric(raw$gender_strat) #as.numeric(raw$gender)-1
raw$age=as.numeric(raw$age)
raw$spanish=as.numeric(raw$Q3!="English/Ingles")
raw$unemployed=as.numeric(raw$Q8=="Unemployed and looking for work")
raw$food=as.numeric(raw$Q13=="Yes")

#Added by Tristan:

raw$eligible_Q10 = as.numeric((raw$Q8 == "Full-time employee" | raw$Q8 == "Part-time employee" | raw$Q8 == "Self-employed or small business owner")&(raw$Q9 == "I am still working in the same job." | raw$Q9 == "I am on sick leave or other leave from the same job" | raw$Q9 == "I am now working at a different job"))
raw$health_insurance= as.numeric((raw$Q17 == "Yes"))
raw$essential_worker = as.numeric((raw$Q10 == "Yes"))
raw$essential_worker[raw$eligible_Q10 != 1] = 0

#key_baseline_variables = c("health_insurance","essential_worker","Q12_dummy","Q19_dummy","standard_education_numeric","Q8","Q14_numeric","age","food","female","unemployed","race_resp","strat_1","strat_2","strat_3","strat_5","strat_6","strat_7")


key_baseline_variables = c("Q12_dummy","standard_education_numeric","Q14_numeric","age","food","female","race_resp","strat_1","strat_2","strat_3","strat_5","strat_6","strat_7")

#key_baseline_variables = c("Q12_dummy","Q19_dummy","standard_education_numeric","Q8","Q14_numeric","age","food","female","unemployed","race_resp","strat_1","strat_2","strat_3","strat_5","strat_6","strat_7")

raw$complete_key_baseline = as.numeric(complete.cases(raw[,key_baseline_variables]))

#number who randomized
raw <- raw %>% filter((!is.na(doctor_race) & !is.na(desc_vid1)) | pure_control == 1)

#######FLOW CHART CODE STARTS HERE

#filter out people with missing baseline data on key variables
raw <- raw %>% filter(complete_key_baseline == 1)



#attentions

raw$Q93 <- iconv(raw$Q93, 'UTF-8', 'ASCII')
v1 <- !is.na(raw$Q93) & tolower(raw$Q93) == "puce"
v2 <- !is.na(raw$Q295) & (tolower(raw$Q295) == "somewhat likely")

#pass at least one tests

raw$pass_attention_check <- (v1|v2)


#standard numeric conversion
raw$resp_race <- raw$race_resp #better naming convention
raw$resp_lang <- raw$Q3
raw$resp_gender <- ifelse(raw$gender == "2", "Female", "Male")
raw$age <- as.numeric(raw$age)


raw$resp_latino <- as.numeric(raw$resp_race == "latino")
raw$resp_black <- as.numeric(raw$resp_race == "black")


raw$race_concord <- as.numeric(raw$resp_race == raw$doctor_race)
raw$race_discord <- 1 - raw$race_concord

raw$elephant <- as.numeric(raw$desc_vid1 != "vanilla")
raw$non_elephant <- as.numeric(raw$desc_vid1 == "vanilla")

raw$elephant1 <- as.numeric(raw$desc_vid1 == "trust" | raw$desc_vid1 == "deportation")
raw$elephant2 <- as.numeric(raw$desc_vid1 == "econ_black" | raw$desc_vid1 == "econ_latino")

raw$birx <- as.numeric(raw$desc_vid2 == "birx")
raw$non_birx <- as.numeric(raw$desc_vid2 != "birx")

raw$debiasing <- as.numeric(raw$desc_vid3 == "stigma")

raw$desc_vid1_modified <- sapply(raw$desc_vid1, recode_desc_vid1)


raw$complete <- ifelse((raw$Finished == "True" & raw$duration <= 172800),1,0 )


raw$anytreat <- 1- as.numeric(raw$pure_control)

raw$complete_knowledge <- !is.na(raw$`Q47_Page Submit`)
raw$complete_links <- !is.na(raw$`Q94_Page Submit`)


raw_fail <- raw %>% filter((Q2 == "Yes, I would like to take part in this study, and confirm that I LIVE IN THE U.S., and I am 18 or older") & (Finished == "False"))


#number in treatment
raw %>% group_by(anytreat) %>% summarise(n())

pure_control <- raw %>% filter(anytreat == 0)
anytreat <- raw %>% filter(anytreat == 1)

#number in race concordant
anytreat %>% group_by(race_concord) %>% summarise(n())

#number assigned elephants and birx etc
anytreat %>% group_by(desc_vid1,desc_vid2) %>% summarise(n())

#number assigned debiasing
anytreat %>% group_by(debiasing) %>% summarise(n())

#number from treated who did not finish
anytreat %>% group_by(pass_attention_check, complete) %>% summarise(n())

#number control who did not finish

pure_control %>% group_by(pass_attention_check, complete) %>% summarise(n())


# This function is for flow chart update with knowledge question
get_new_flow_chart_numbers <- function(raw){
  #number in treatment
  message("number in treatment")
  raw %>% group_by(anytreat) %>% summarise(n()) %>% View("treatment")
  
  pure_control <- raw %>% filter(anytreat == 0)
  anytreat <- raw %>% filter(anytreat == 1)
  
  #number in race concordant
  message("number in race concordant")
  anytreat %>% group_by(race_concord) %>% summarise(n()) %>% View("race concordant")
  
  message("number birx or not")
  anytreat %>% group_by(race_concord, desc_vid1_modified, birx) %>% summarise(n()) %>% View("birx")
  
  message("number assigned perception or standard")
  anytreat %>% group_by(race_concord, desc_vid1_modified, birx, desc_vid3) %>% summarise(n()) %>% View("perception")
  
  message("number knowledge questions")
  anytreat %>% group_by(race_concord, desc_vid1_modified, birx, desc_vid3, complete_knowledge) %>% summarise(n()) %>% View("knowledge completion")
  
  message("number knowledge questions, control")
  pure_control %>% group_by(complete_knowledge) %>% summarise(n()) %>% View("knowledge completion_control")
  
  message("number link questions")
  anytreat %>% group_by(race_concord, desc_vid1_modified, birx, desc_vid3, complete_links) %>% summarise(n()) %>% View("links completion")
  
  message("number link questions, control")
  pure_control %>% group_by(complete_links) %>% summarise(n()) %>% View("links completion_control")
  
}

get_new_flow_chart_numbers(raw)



