# General pre-processing of the data

raw$Q14_numeric = raw$Q14

for (i in 1:nrow(raw)){
  if (!is.na(raw$Q14_numeric[i])&&(raw$Q14_numeric[i] == "8 or more")){
    raw$Q14_numeric[i] = "8"
  }
}
raw$hh_size= as.numeric(raw$Q14_numeric)
raw$Q14_numeric=NULL
raw$education_numeric = as.numeric(raw$standard_education)
raw$medical_condition = 1-(!is.na(raw$Q19))*(raw$Q19!="None")
raw$health_insurance= as.numeric((raw$Q17 == "Yes"))
raw$essential_worker = as.numeric((raw$Q10 == "Yes"))


raw$money_covid = as.numeric((raw$Q12=="Very likely")+(raw$Q12=="Somewhat likely"))

raw$student = as.numeric((raw$Q8 == "Student"))


recode_agree5_questions <- function(x) {
  if (!is.na(x)) {
    if (tolower(x) == "strongly disagree") {
      return(1)
    }
    else if (tolower(x) == "somewhat disagree") {
      return (2)
    }
    else if (tolower(x) == "neither agree nor disagree") {
      return(3)
    }
    else if (tolower(x) == "somewhat agree") {
      return(4)
    }
    else if (tolower(x) == "strongly agree") {
      return(5)
    }}
  else {
    return(NA)
  }
}

recode_age <- function(x) {
  if (!is.na(x)) {
    if ((x>=18) & (x < 25)) {
      return("18 - 24")
    }
    else if ((x>=25) & (x<45)) {
      return ("25 - 44")
    }
    else if ((x>=45) & (x<65)) {
      return("45 - 64")
    }
    else if (x>=65){
      return("65 - ")
    }
    else if (x<18) {
      return("- 18")
    }}
  else {
    return(NA)
  }
}



raw <- raw %>% filter((!is.na(doctor_race) & !is.na(desc_vid1)) | pure_control == 1)



#standard numeric conversion

to_numeric <- c("Q20_First Click", "Q20_Last Click", "Q20_Page Submit", "Q20_Click Count",
                "Q24_First Click", "Q24_Last Click", "Q24_Page Submit", "Q24_Click Count",
                "Q35_First Click", "Q35_Last Click", "Q35_Page Submit", "Q35_Click Count",
                "Q88_Page Submit",
                "Q62_1", "Q65_1", "Q68_1", "Q71_1", 
                "Q74_1", "Q77_1", "Q80_1", "Q83_1",
                "Q32_12", "Q33_12","Q34_12",
                "Q58_12", "Q59_12", "Q60_12")

for (q in to_numeric) {
  raw[,q] <- as.numeric(raw[,q])
}

extent_questions <- c("Q23_1", "Q23_2", "Q23_3", "Q23_4", 
                      "Q27_1", "Q27_2", "Q27_3", "Q27_4",
                      "Q38_1", "Q38_2", "Q38_3", "Q38_4")


for (q in extent_questions) {
  raw[,paste0(q,"_recoded")] <- sapply(raw[,q], recode_agree5_questions)
}

raw$resp_race <- raw$race_resp #better naming convention
raw$resp_lang <- raw$Q3


raw$resp_gender <- ifelse(raw$gender == "2", "Female", "Male")

raw$age <- as.numeric(raw$age)




raw$resp_latino <- as.numeric(raw$resp_race == "latino")
raw$resp_black <- as.numeric(raw$resp_race == "black")




raw$anytreat <- 1- as.numeric(raw$pure_control)

if (simulation == 1) { #make fake data
  raw$anytreat <- as.numeric(runif(nrow(raw)) > 0.11)
  
  raw$doctor_race <- ifelse(runif(nrow(raw)) > 0.5, raw$resp_race, "white")
  
  
  black <- raw %>% filter(resp_race == "black")
  latino <- raw %>% filter(resp_race == "latino")
  
  rblack <- runif(nrow(black))
  black$desc_vid1 <- ifelse(rblack < 0.5, "vanilla", ifelse(rblack < 0.75, "trust", "econ_black"))
  
  rlatino <- runif(nrow(latino))
  latino$desc_vid1 <- ifelse(rlatino < 0.5, "vanilla", ifelse(rlatino < 0.75, "deportation", "econ_latino"))
  
  raw <- smartbind(black,latino)
  
  raw$desc_vid2 <- ifelse((raw$desc_vid1 == "vanilla") & (runif(nrow(raw)) < 0.5), "birx", "mgh")
  
  raw$desc_vid3 <- ifelse(runif(nrow(raw)) < 0.5, "vanilla", "stigma")
}

raw$race_concord <- as.numeric(raw$resp_race == raw$doctor_race)
raw$race_discord <- 1 - raw$race_concord

raw$race_concord_black = raw$race_concord*raw$resp_black

raw$elephant <- as.numeric(raw$desc_vid1 != "vanilla")
raw$non_elephant <- as.numeric(raw$desc_vid1 == "vanilla")

raw$elephant1 <- as.numeric(raw$desc_vid1 == "trust" | raw$desc_vid1 == "deportation")
raw$elephant2 <- as.numeric(raw$desc_vid1 == "econ_black" | raw$desc_vid1 == "econ_latino")

raw$birx <- as.numeric(raw$desc_vid2 == "birx")
raw$non_birx <- as.numeric(raw$desc_vid2 != "birx")

raw$debiasing <- as.numeric(raw$desc_vid3 == "stigma")

raw$prior_underestimate <- as.numeric(raw$Q33_12 < 8) #pessimistic

raw$prior_overestimate <- as.numeric(raw$Q33_12 >= 8) #optimistic

raw$want_mask_vid <- as.numeric(raw$Q87 == "Yes")
raw$time_mask_vid <- ifelse(raw$want_mask_vid == 1, raw$`Q88_Page Submit`, 0)

raw$want_mask_vid_perc <- raw$want_mask_vid*100

raw$female=1-as.numeric(raw$gender_strat) #as.numeric(raw$gender)-1
raw$age=as.numeric(raw$age)
raw$spanish=as.numeric(raw$Q3!="English/Ingles")
raw$unemployed=as.numeric(raw$Q8=="Unemployed and looking for work")
raw$food=as.numeric(raw$Q13=="Yes")

baseline_vars_orig <- c("standard_education", 
                        "Q14", #how many ppl in household
                        "Q15", #how many bathrooms
                        "Q8", #employment status
                        "Q10", #essential worker status
                        "Q11", #wfh
                        "Q17", #health insurance y/n
                        "Q19", # health conditions 
                        "Q18", #where do you go for advice? HEALTHCARE USAGE PATTERN?
                        "Q13", #eat less than you thought you should?
                        "Q12") # how likely to run out of money?

raw <- dummy_cols(raw, select_columns = baseline_vars_orig, remove_first_dummy = TRUE, remove_selected_columns = TRUE) #omit one, lose the original baseline to make grep work 
names(raw) <- make.names(names(raw))

baseline_vars_dummies <- grep(paste0(baseline_vars_orig, "_", collapse = "|"), colnames(raw), value = TRUE)
baseline_vars_dummies <- grep("_TEXT", baseline_vars_dummies, value = TRUE, invert = TRUE)

for (var in baseline_vars_dummies) {
  raw[[var]][is.na(raw[[var]])] <- 0
}

#outcomes 

raw$vid1_useful <- raw$Q23_1_recoded
raw$vid1_trustworthy <- raw$Q23_2_recoded
raw$vid1_follow_recs <- raw$Q23_3_recoded
raw$vid1_will_share <- raw$Q23_4_recoded

raw$vid2_useful <- raw$Q27_1_recoded
raw$vid2_trustworthy <- raw$Q27_2_recoded
raw$vid2_follow_recs <- raw$Q27_3_recoded
raw$vid2_will_share <- raw$Q27_4_recoded

raw$vid3_useful <- raw$Q38_1_recoded
raw$vid3_trustworthy <- raw$Q38_2_recoded
raw$vid3_follow_recs <- raw$Q38_3_recoded
raw$vid3_will_share <- raw$Q38_4_recoded

raw$vid1_useful_std <- (raw$Q23_1_recoded - mean(raw$Q23_1_recoded, na.rm = TRUE))/sqrt(var(raw$Q23_1_recoded, na.rm = TRUE))
raw$vid1_trustworthy_std <- (raw$Q23_2_recoded - mean(raw$Q23_2_recoded, na.rm = TRUE))/sqrt(var(raw$Q23_2_recoded, na.rm = TRUE))
raw$vid1_follow_recs_std <- (raw$Q23_3_recoded - mean(raw$Q23_3_recoded, na.rm = TRUE))/sqrt(var(raw$Q23_3_recoded, na.rm = TRUE))
raw$vid1_will_share_std <- (raw$Q23_4_recoded - mean(raw$Q23_4_recoded, na.rm = TRUE))/sqrt(var(raw$Q23_4_recoded, na.rm = TRUE))

raw$vid2_useful_std <- (raw$Q27_1_recoded - mean(raw$Q27_1_recoded, na.rm = TRUE))/sqrt(var(raw$Q27_1_recoded, na.rm = TRUE))
raw$vid2_trustworthy_std <- (raw$Q27_2_recoded - mean(raw$Q27_2_recoded, na.rm = TRUE))/sqrt(var(raw$Q27_2_recoded, na.rm = TRUE))
raw$vid2_follow_recs_std <- (raw$Q27_3_recoded - mean(raw$Q27_3_recoded, na.rm = TRUE))/sqrt(var(raw$Q27_3_recoded, na.rm = TRUE))
raw$vid2_will_share_std <- (raw$Q27_4_recoded - mean(raw$Q27_4_recoded, na.rm = TRUE))/sqrt(var(raw$Q27_4_recoded, na.rm = TRUE))

raw$vid3_useful_std <- (raw$Q38_1_recoded - mean(raw$Q38_1_recoded, na.rm = TRUE))/sqrt(var(raw$Q38_1_recoded, na.rm = TRUE))
raw$vid3_trustworthy_std <- (raw$Q38_2_recoded - mean(raw$Q38_2_recoded, na.rm = TRUE))/sqrt(var(raw$Q38_2_recoded, na.rm = TRUE))
raw$vid3_follow_recs_std <- (raw$Q38_3_recoded - mean(raw$Q38_3_recoded, na.rm = TRUE))/sqrt(var(raw$Q38_3_recoded, na.rm = TRUE))
raw$vid3_will_share_std <- (raw$Q38_4_recoded - mean(raw$Q38_4_recoded, na.rm = TRUE))/sqrt(var(raw$Q38_4_recoded, na.rm = TRUE))


raw$vid1_net_rating <- (raw$vid1_useful_std + raw$vid1_trustworthy_std + raw$vid1_follow_recs_std + raw$vid1_will_share_std) / 4
raw$vid2_net_rating <- (raw$vid2_useful_std + raw$vid2_trustworthy_std + raw$vid2_follow_recs_std + raw$vid2_will_share_std) / 4
raw$vid3_net_rating <- (raw$vid3_useful_std + raw$vid3_trustworthy_std + raw$vid3_follow_recs_std + raw$vid3_will_share_std) / 4

raw$vid1_net_rating_nonorm <- (raw$Q23_1_recoded + raw$Q23_2_recoded + raw$Q23_3_recoded + raw$Q23_4_recoded) /4
raw$vid2_net_rating_nonorm <- (raw$Q27_1_recoded + raw$Q27_2_recoded + raw$Q27_3_recoded + raw$Q27_4_recoded) /4
raw$vid3_net_rating_nonorm <- (raw$Q38_1_recoded + raw$Q38_2_recoded + raw$Q38_3_recoded + raw$Q38_4_recoded) /4

raw$vid1_net_rating_perfect <- ((raw$Q23_1_recoded == 5) + (raw$Q23_2_recoded == 5) + (raw$Q23_3_recoded == 5) + (raw$Q23_4_recoded == 5)) /4
raw$vid2_net_rating_perfect <- ((raw$Q27_1_recoded == 5) + (raw$Q27_2_recoded == 5) + (raw$Q27_3_recoded == 5) + (raw$Q27_4_recoded == 5)) /4
raw$vid3_net_rating_perfect <- ((raw$Q38_1_recoded == 5) + (raw$Q38_2_recoded == 5) + (raw$Q38_3_recoded == 5) + (raw$Q38_4_recoded == 5)) /4




raw$vid13_avg_rating <- (raw$vid1_net_rating + raw$vid3_net_rating) / 2

raw$vid1_time <- raw$Q20_Page.Submit
raw$vid2_time <- raw$Q24_Page.Submit
raw$vid3_time <- raw$Q35_Page.Submit

#winsorize times:

raw$vid1_time[raw$vid1_time > 1200] = 1200
raw$vid2_time[raw$vid2_time > 1200] = 1200
raw$vid3_time[raw$vid3_time > 1200] = 1200


raw$watch_mask_yes <- as.numeric(raw$Q87 == "Yes")
raw$watch_mask_yes <- as.numeric(raw$Q87 == "Yes")

raw$watch_mask_length <- as.numeric(raw$`Q88_Page.Submit`)

raw$posterior_no_good <- as.numeric(raw$Q58_12)
raw$posterior_protect <- as.numeric(raw$Q59_12)
raw$posterior_was_sick <- as.numeric(raw$Q60_12)





#knowledge and practices outcome

raw$know_mask_practice <- as.numeric(grepl("Wear a mask", raw$Q43))
raw$know_mask_practice[is.na(raw$Q43)] = NA #keep missings

raw$know_3_practices <- as.numeric(raw$Q43 == "Stay six feet away from other people when outside,Wash your hands when you come home ,Wear a mask or facial covering when outside")
raw$know_asymptomatic <- as.numeric(raw$Q44 == "Yes") 
raw$know_3_symptoms <- as.numeric(raw$Q46 == "Fever,Cough,Difficulty breathing") #caveat about new loss of taste or smell
raw$know_symptoms <- as.numeric(raw$Q46 == "Fever,Cough,Difficulty breathing"
                                |raw$Q46 == "Fever,Cough,New loss of taste or smell"
                                |raw$Q46 == "Fever,Difficulty breathing,New loss of taste or smell"  
                                |raw$Q46 == "Cough,Difficulty breathing,New loss of taste or smell")
raw$know_taste_smell <- as.numeric(grepl("smell", raw$Q46))
raw$know_taste_smell[is.na(raw$Q46)] = NA #keep missings
raw$know_fever <- as.numeric(grepl("Fever", raw$Q46))
raw$know_fever[is.na(raw$Q46)] = NA #keep missings
raw$know_cough <- as.numeric(grepl("Cough", raw$Q46))
raw$know_cough[is.na(raw$Q46)] = NA #keep missings
raw$know_breathing <- as.numeric(grepl("breathing", raw$Q46))
raw$know_breathing[is.na(raw$Q46)] = NA #keep missings

raw$know_distance <- as.numeric(grepl("feet", raw$Q43))
raw$know_distance[is.na(raw$Q43)] = NA #keep missings
raw$know_hands <- as.numeric(grepl("hands", raw$Q43))
raw$know_hands[is.na(raw$Q43)] = NA #keep missings


raw$dont_know_3_practices <- 1 - raw$know_3_practices 
raw$dont_know_asymptomatic <- 1 - raw$know_asymptomatic
raw$dont_know_3_symptoms <- 1 - raw$know_3_symptoms
raw$dont_know_symptoms <- 1- raw$know_symptoms

raw$dont_know_3_practices_perc <- raw$dont_know_3_practices * 100
raw$dont_know_asymptomatic_perc <- raw$dont_know_asymptomatic * 100
raw$dont_know_3_symptoms_perc <- raw$dont_know_3_symptoms * 100
raw$dont_know_symptoms_perc <- raw$dont_know_symptoms * 100


raw$knowledge_gap_index <- (raw$dont_know_3_practices + raw$dont_know_asymptomatic + raw$dont_know_symptoms) /3
raw$any_knowledge_gap = as.numeric(raw$knowledge_gap_index>0)

raw$knowledge_gap_index_perc <- raw$knowledge_gap_index*100

raw$knowledge_count_variable <- raw$dont_know_asymptomatic + 3 - pmin(raw$know_breathing +raw$know_taste_smell+raw$know_fever+raw$know_cough,3) + 3 - raw$know_hands - raw$know_distance - raw$know_mask_practice



#donations outcomes

raw$pay_local_hospitals <- ifelse(!is.na(raw$Q62_1), 100 - raw$Q62_1, raw$Q65_1) #versus unemployed and small businesses
raw$pay_workers_extra <- ifelse(!is.na(raw$Q68_1), 100 - raw$Q68_1, raw$Q71_1) #versus protective equipment

raw$BET_fund <- ifelse(!is.na(raw$Q74_1), 1000 - raw$Q74_1, raw$Q77_1) #will be NA for latino respondents
raw$LNF_fund <- ifelse(!is.na(raw$Q80_1), 1000 - raw$Q80_1, raw$Q83_1) #will be NA for black respondents

raw$race_specific_fund <- ifelse(!is.na(raw$BET_fund), raw$BET_fund, raw$LNF_fund)
raw$masks_for_the_people <- 1000 - raw$race_specific_fund 

# black <- raw %>% filter(resp_race == "black")
# latino <- raw %>% filter(resp_race == "latino")
# 
# black$BET_fund <- ifelse(!is.na(black$Q74_1), black$Q74_1, 1000 - black$Q77_1)
# latino$LNF_fund <- ifelse(!is.na(latino$Q80_1), latino$Q80_1, 1000 - latino$Q83_1)

raw$want_mgh_app <- as.numeric(raw$Q86 == "Yes")
raw$want_disinfect <- as.numeric(raw$Q95_1 == "I would like this link")
raw$want_children_moving <- as.numeric(raw$Q95_2 == "I would like this link")
raw$want_working_out <- as.numeric(raw$Q95_3 == "I would like this link")
raw$want_telehealth <- as.numeric(raw$Q95_4 == "I would like this link")
raw$want_mental_health <- as.numeric(raw$Q95_5 == "I would like this link")
raw$want_state_hotline <- as.numeric(raw$Q95_6 == "I would like this link")
raw$want_testing_loc <- as.numeric(raw$Q95_7 == "I would like this link")
raw$want_mgh_resources <- as.numeric(raw$Q95_8 == "I would like this link")


answerQ86 = (raw$Q86 == "Yes")
for (i in 1:length(answerQ86)){
  if ((!is.na(raw$Q85_Page.Submit[i]))&is.na(answerQ86[i])){
    answerQ86[i]=FALSE
  }
  if ((!is.na(raw$Q85_Page.Submit[i]))&is.na(raw$want_mask_vid[i])){
  raw$want_mask_vid[i] = 0
    }
}

answerQ95 = c()
answerQ95[[1]] = as.numeric((raw$Q95_1 == "I would like this link"))
answerQ95[[2]] = as.numeric((raw$Q95_2 == "I would like this link") )
answerQ95[[3]]= as.numeric((raw$Q95_3 == "I would like this link") )
answerQ95[[4]] = as.numeric((raw$Q95_4 == "I would like this link") )
answerQ95[[5]] = as.numeric((raw$Q95_5 == "I would like this link") )
answerQ95[[6]] = as.numeric((raw$Q95_6 == "I would like this link") )
answerQ95[[7]] = as.numeric((raw$Q95_7 == "I would like this link") )
answerQ95[[8]]= as.numeric((raw$Q95_8 == "I would like this link") )

for (k in 1:8){
  for (i in 1:length(answerQ95[[k]])){
    if ((!is.na(raw$Q94_Page.Submit[i]))&(is.na(answerQ95[[k]][i]))){
      answerQ95[[k]][i]=0
    }
  }
}

# raw$num_links_interested <- (raw$Q86 == "Yes") +
#   (raw$Q95_1 == "I would like this link") +
#   (raw$Q95_2 == "I would like this link") +
#   (raw$Q95_3 == "I would like this link") +
#   (raw$Q95_4 == "I would like this link") +
#   (raw$Q95_5 == "I would like this link") +
#   (raw$Q95_6 == "I would like this link") +
#   (raw$Q95_7 == "I would like this link") +
#   (raw$Q95_8 == "I would like this link")


raw$num_links_interested = as.numeric(answerQ86)+answerQ95[[1]]+answerQ95[[2]]+answerQ95[[3]]+answerQ95[[4]]+answerQ95[[5]]+answerQ95[[6]]+answerQ95[[7]]+answerQ95[[8]]

raw$num_links_interested_scaled <- raw$num_links_interested/9
raw$masks_for_the_people_scaled <- raw$masks_for_the_people/1000

raw$num_links_interested_perc <- raw$num_links_interested_scaled *100

raw$intended_behavior <- raw$num_links_interested + raw$want_mask_vid

raw$want_any_link <- as.numeric(raw$intended_behavior > 0)

raw$intended_behavior_scaled <- (raw$intended_behavior/10)
raw$intended_behavior_perc <- (raw$intended_behavior/10)*100


raw$debiasing_prior_underestimate <- raw$debiasing * (1- raw$prior_overestimate)
raw$debiasing_prior_overestimate <- raw$debiasing * raw$prior_overestimate
raw$no_debiasing_prior_underestimate <- (1-raw$debiasing) * (1-raw$prior_overestimate)
raw$no_debiasing_prior_overestimate <- (1-raw$debiasing) * (raw$prior_overestimate)



raw_anytreat <- raw %>% filter(anytreat == 1)