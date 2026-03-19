# This code generates the summary table present in the main text. 
# Lines 207-256 enable to generate the balance tables with the cobalt package.

# Please set working directory to be source file location

rm(list = ls())


library(readr)
library(anytime)
library(cobalt)
library("WeightIt")
library(dplyr)


simulation = 0
set.seed(302)

#Data import:

mainlaunch <- "./Data/COVID USA T1_May 27, 2020_07.20.csv"

df=read_csv(mainlaunch)[-c(1,2),]

df$StartTime <- anytime(df$StartDate) #parse
df$duration <- as.numeric(df$`Duration (in seconds)`)

#raw dataset of accepts 
raw <- df %>% filter((StartTime > anytime("2020-05-13 15:00:00 EDT")) & (DistributionChannel == "anonymous") & (Q2 == "Yes, I would like to take part in this study, and confirm that I LIVE IN THE U.S., and I am 18 or older") & is.na(test))
raw <- as.data.frame(raw)
raw <- raw %>% filter((!is.na(doctor_race) & !is.na(desc_vid1)) | pure_control == 1)

# Variables preprocessing:

raw$anytreat <- 1- as.numeric(raw$pure_control)

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
raw$essential_worker = as.numeric((raw$Q10 == "Yes"))
raw$Q12_dummy = as.numeric((raw$Q12=="Very likely")+(raw$Q12=="Somewhat likely"))
raw$eligible_Q10 = as.numeric((raw$Q8 == "Full-time employee" | raw$Q8 == "Part-time employee" | raw$Q8 == "Self-employed or small business owner")&(raw$Q9 == "I am still working in the same job." | raw$Q9 == "I am on sick leave or other leave from the same job" | raw$Q9 == "I am now working at a different job"))

raw$essential_worker[raw$eligible_Q10 != 1] = 0

raw$age_strat = as.numeric(raw$age_strat )

raw$strat_1 = as.numeric((raw$stratum=="1"))
raw$strat_2 = as.numeric((raw$stratum=="2"))
raw$strat_3 = as.numeric((raw$stratum=="3"))
raw$strat_4 = as.numeric((raw$stratum=="4"))
raw$strat_5 = as.numeric((raw$stratum=="5"))
raw$strat_6 = as.numeric((raw$stratum=="6"))
raw$strat_7 = as.numeric((raw$stratum=="7"))
raw$strat_8 = as.numeric((raw$stratum=="8"))


raw$Q8_Student = as.numeric(raw$Q8=="Student")

raw$female=1-as.numeric(raw$gender_strat) #as.numeric(raw$gender)-1
raw$age=as.numeric(raw$age)
raw$spanish=as.numeric(raw$Q3!="English/Ingles")
raw$unemployed=as.numeric(raw$Q8=="Unemployed and looking for work")
raw$food=as.numeric(raw$Q13=="Yes")

raw$male = 1-raw$female
hispanic_types = c(1:16)
raw$hispanic_numeric = as.numeric(raw$hispanic)

# Extraction of key baseline variables:

baseline_variables = c("female","male","age","strat_1","strat_2","strat_3","strat_4","strat_5","strat_6","strat_7","strat_8","Q14_numeric","standard_education_numeric","Q12_dummy","food")

var_names = c("Female","Male","Age","Strata 1","Strata 2","Strata 3","Strata 4","Strata 5","Strata 6","Strata 7","Strata 8","Number of people in household","Education","Likely to run out of money because of covid","Trouble paying food"
              )


raw2=raw
raw2$complete_key_baseline = as.numeric(complete.cases(raw2[,baseline_variables]))



#filter out people with missing baseline data on key variables
raw2 <- raw2%>% filter(complete_key_baseline == 1)

black=raw2 %>% filter(race_resp=="black")
latino=raw2 %>% filter(race_resp=="latino")


panels = list(raw2,black,latino,raw2[raw2$anytreat==1,],black[black$anytreat==1,],latino[latino$anytreat==1,],raw2[raw2$anytreat==0,],black[black$anytreat==0,]
              , latino[latino$anytreat==0,])

continuous_variables = c("age","standard_education_numeric","Q14_numeric")

#Generation of the summary table:

lines = c()
i=1
for (panel in panels){
  lines = c(lines,nrow(panel))
  for (var in baseline_variables){
    if (var %in% continuous_variables){ # for continuous variable, we compute mean and standard deviation
      lines = c(lines,paste0(round(mean(panel[[var]]),2)," (",round(sd(panel[[var]]),1),")"))
    }else{ # for dummy variables, we compute number of observations and percentage 
      lines = c(lines,paste0(round(sum(panel[[var]]==1),2)," (",round(100*sum(panel[[var]]==1)/nrow(panel),1),")"))
    }
    
  }
  # For hispanic types
  if (i%%3!=0){
    lines = c(lines,rep("",length(hispanic_types)))
  }else{
    
    for (var in hispanic_types){
      lines = c(lines,paste0(round(sum(panel$hispanic_numeric==var),2)," (",round(100*sum(panel$hispanic_numeric==var)/nrow(panel),1),")"))

    }
  }
  i=i+1
}




tab = matrix(lines, ncol=9)

hispanic_labels = c("No , not of Hispanic, Latino, or Spanish origin",
"Yes, Mexican, Mexican American, Chicano",
"Yes, Cuban",
"Yes, another Hispanic, Latino, or Spanish origin *** Argentina",
"Yes, another Hispanic, Latino, or Spanish origin *** Colombia",
"Yes, another Hispanic, Latino, or Spanish origin *** Ecuador",
"Yes, another Hispanic, Latino, or Spanish origin *** El Salvadore",
"Yes, another Hispanic, Latino, or Spanish origin *** Guatemala",
"Yes, another Hispanic, Latino, or Spanish origin *** Nicaragua",
"Yes, another Hispanic, Latino, or Spanish origin *** Panama",
"Yes, another Hispanic, Latino, or Spanish origin *** Peru",
"Yes, another Hispanic, Latino, or Spanish origin *** Spain",
"Yes, another Hispanic, Latino, or Spanish origin *** Venezuela",
"Yes, another Hispanic, Latino, or Spanish origin *** Other Country",
"Prefer not to answer",
"Yes, Puerto Rican")

colnames(tab) <- c("All", "African-American",  "Latinx","All", "African-American",  "Latinx","All", "African-American",  "Latinx")
rownames(tab) = c("Nb observations",var_names,hispanic_labels)
tab
# save Summary Table
write.csv(tab, "./Output/summary_table.csv")

## We add other lines for those who answered Q8 and health questions:

additional_variables = c("essential_worker","unemployed","Q8_Student","Q19_dummy","Q17_numeric")

var_names = c("Essential worker","Unemployed","Student",
          "No medical condition reported","Health insurance")

raw2=raw
raw2$complete_key_baseline = as.numeric(complete.cases(raw[,c(baseline_variables,additional_variables)]))


#filter out people with missing baseline data on key variables
raw2 <- raw2%>% filter(complete_key_baseline == 1)

black=raw2 %>% filter(race_resp=="black")
latino=raw2 %>% filter(race_resp=="latino")


panels = list(raw2,black,latino,raw2[raw2$anytreat==1,],black[black$anytreat==1,],latino[latino$anytreat==1,],raw2[raw2$anytreat==0,],black[black$anytreat==0,]
              , latino[latino$anytreat==0,])

continuous_variables = c("age","standard_education_numeric","Q14_numeric")

lines = c()
i=1
for (panel in panels){
  lines = c(lines,nrow(panel))
  for (var in additional_variables){
    if (var %in% continuous_variables){# for continuous variable, we compute mean and standard error
      lines = c(lines,paste0(round(mean(panel[[var]]),2)," (",round(sd(panel[[var]]),1),")"))
    }else{# for dummy variables, we compute number of observations and percentage 
      
      lines = c(lines,paste0(round(sum(panel[[var]]==1),2)," (",round(100*sum(panel[[var]]==1)/nrow(panel),1),")"))
    }
    
    
  }
  
}

tab = matrix(lines, ncol=9)

colnames(tab) <- c("All", "African-American",  "Latinx","All", "African-American",  "Latinx","All", "African-American",  "Latinx")
rownames(tab) = c("Nb observations",var_names)

# save second part of the Summary Table
write.csv(tab, "./Output/summary_table_additional_variables.csv")

## Cobalt Balance Table

# We build the treatment variables:

raw$treated=1-as.numeric(raw$pure_control)
raw$control=1-raw$treated
raw$elephant1=as.numeric(raw$desc_vid1=="deportation" | raw$desc_vid1=="trust")
raw$elephant1[raw$pure_control==1]=NA
raw$elephant2=as.numeric(raw$desc_vid1=="econ_black" | raw$desc_vid1=="econ_latino")
raw$elephant2[raw$pure_control==1]=NA
raw$birx=as.numeric(raw$desc_vid2=="birx")
raw$birx[raw$pure_control==1]=NA
raw$stigma=as.numeric(raw$desc_vid3=="stigma")
raw$stigma[raw$pure_control==1]=NA
raw$concord=as.numeric(raw$doctor_race==raw$race_resp)
raw$concord[raw$pure_control==1]=NA

raw$resp_latino = as.numeric(raw$race_resp == "latino")


baseline_variables = c("female","age","strat_1","strat_2","strat_3","strat_5","strat_6","strat_7","Q14_numeric","standard_education_numeric","Q12_dummy","food",
"resp_latino")
raw2=raw

# We keep those who completed key baseline variables
raw2$complete_key_baseline = as.numeric(complete.cases(raw2[,baseline_variables]))



#filter out people with missing baseline data on key variables
raw2 <- raw2%>% filter(complete_key_baseline == 1)

### Choice of data set:

data = raw2 # panel: all
#data = raw2[!is.na(raw2$`Q47_Page Submit`),] # panel: completed knowledge questions
#data = raw2[!is.na(raw2$`Q94_Page Submit`),] # panel: completed links

### Choice of treatment:


treatment = "anytreat" # treated, concord, birx, elephant1, elephant2 or stigma

sum_variables = paste0(baseline_variables, collapse = "+")
formula = as.formula(paste0(treatment , "~",sum_variables))

W.out <- weightit(formula, data = data[!is.na(data[,treatment]),],
                  method = "ps", estimand = "ATT",focal=1)

bal.tab(W.out,int=TRUE,un = FALSE,m.threshold = .1,ks.threshold = 0.05)





