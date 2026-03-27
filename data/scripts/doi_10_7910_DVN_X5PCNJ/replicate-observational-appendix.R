load("observational_data.RData")

library(TAM); library(tidyverse);library(xtable); library(psych); library(GPArotation); library(stargazer); library(MASS)

##############################################
# Table A-2 (Sample Descriptive Information) # 
##############################################
# Unweighted
round(prop.table(table(survey$Q4005)),3) # gender
round(prop.table(table(survey$Q2146)),3) # education
round(prop.table(table(survey$QNEW)),3) # race
round(prop.table(table(survey$QM12)),3) # ideology
round(prop.table(table(survey$Q2157)),3) # income
round(prop.table(table(survey$Q1903)),3) # party

# Weighted
# Note: the survey asked respondents "What is your gender?"; response options were Male and Female.
# Because respondents were asked about gender, we use the term gender, though we recognize the response options refer to biological sex.
round(weighted_table(x=as.numeric(survey$Q4005), w=survey$Propwts, props=T),3) # Gender, 1 is male, 2 is female
round(weighted_table(x=as.numeric(survey$Q2146), w=survey$Propwts, props=T),3) # education, 1 is less than HS, 2 is some HS, 3 is HS, 4 is some college
# 5 is college, 6 is some grad, 7 is grad, 8 is associates, 9 is job-specific training program
round(weighted_table(x=as.numeric(survey$QNEW), w=survey$Propwts, props=T),3) # race, 1 is white, 2 is black, 3 is Hispanic, 4 is Asian, 5 is other
round(weighted_table(x=as.numeric(survey$QM12), w=survey$Propwts, props=T),3) # ideology, 1 is strong liberal, 5 is strong conservative
round(weighted_table(x=as.numeric(survey$Q2157), w=survey$Propwts, props=T),3) # income, 1 less than 15K, 2 15-25, 3 25-35, 4 35-50, 5 50-75...
round(weighted_table(x=as.numeric(survey$Q1903), w=survey$Propwts, props=T),3) # party, 1 is R, 2 is D, 3 is I, 4 is other

#############################################
# Table A-3 (Gender Descriptive Principles) # 
#############################################
gender.plain <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_1 %in% c("Very important", "Somewhat important"), w = Propwts)) #plain meaning to modern reader
gender.intent <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_2 %in% c("Very important", "Somewhat important"), w = Propwts)) #intent of framers
gender.adopted <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_3 %in% c("Very important", "Somewhat important"), w = Propwts)) #what most thought when adopted
gender.precedent <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_4 %in% c("Very important", "Somewhat important"), w = Propwts)) #stare decisis (previous SC decisions)
gender.strong <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_5 %in% c("Very important", "Somewhat important"), w = Propwts)) #strong reason, e.g. national security
gender.political <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_6 %in% c("Very important", "Somewhat important"), w = Propwts)) #political activity
gender.state <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_7 %in% c("Very important", "Somewhat important"), w = Propwts)) #state or federal
gender.consequences <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_8 %in% c("Very important", "Somewhat important"), w = Propwts)) #consequences
gender.countries <- survey %>% 
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_9 %in% c("Very important", "Somewhat important"), w = Propwts)) #other countries
gender.public <- survey %>%
  group_by(female) %>%
  summarize(weighted.mean(QPHILO3_10 %in% c("Very important", "Somewhat important"), w = Propwts)) #public opinion

ta3.plain.meaning <- c(gender.plain$`weighted.mean(...)`[c(2,1)])
ta3.original.intent <- c(gender.intent$`weighted.mean(...)`[c(2,1)])
ta3.opinion.when.adopted <- c(gender.adopted$`weighted.mean(...)`[c(2,1)])
ta3.precedent <- c(gender.precedent$`weighted.mean(...)`[c(2,1)])
ta3.average.traditional <- apply(rbind(ta3.plain.meaning, ta3.original.intent, ta3.opinion.when.adopted, ta3.precedent), 2, mean)

ta3.plain.meaning <- paste(as.character(round(ta3.plain.meaning*100,0)),"%",sep="")
ta3.original.intent <- paste(as.character(round(ta3.original.intent*100,0)),"%",sep="")
ta3.opinion.when.adopted <- paste(as.character(round(ta3.opinion.when.adopted*100,0)),"%",sep="")
ta3.precedent <- paste(as.character(round(ta3.precedent*100,0)),"%",sep="")
ta3.average.traditional <- paste(as.character(round(ta3.average.traditional*100,0)),"%",sep="") 

ta3.consequences <- c(gender.consequences$`weighted.mean(...)`[c(2,1)])
ta3.other.countries <- c(gender.countries$`weighted.mean(...)`[c(2,1)])
ta3.public.opinion <- c(gender.public$`weighted.mean(...)`[c(2,1)])
ta3.average.non.traditional <- apply(rbind(ta3.consequences, ta3.other.countries, ta3.public.opinion), 2, mean)

ta3.consequences <- paste(as.character(round(ta3.consequences*100,0)),"%",sep="")
ta3.other.countries <- paste(as.character(round(ta3.other.countries*100,0)),"%",sep="")
ta3.public.opinion <- paste(as.character(round(ta3.public.opinion*100,0)),"%",sep="")
ta3.average.non.traditional <- paste(as.character(round(ta3.average.non.traditional*100,0)),"%",sep="") 

ta3.strong.reason <- c(gender.strong$`weighted.mean(...)`[c(2,1)])
ta3.political.activity <- c(gender.political$`weighted.mean(...)`[c(2,1)])
ta3.state.federal <- c(gender.state$`weighted.mean(...)`[c(2,1)])

ta3.strong.reason <- paste(as.character(round(ta3.strong.reason*100,0)),"%",sep="")
ta3.political.activity <- paste(as.character(round(ta3.political.activity*100,0)),"%",sep="")
ta3.state.federal <- paste(as.character(round(ta3.state.federal*100,0)),"%",sep="")

ta3.dataframe <- data.frame(rbind(ta3.average.traditional, ta3.plain.meaning, ta3.original.intent, ta3.opinion.when.adopted, ta3.precedent,
                                 ta3.average.non.traditional, ta3.consequences, ta3.other.countries, ta3.public.opinion,
                                 ta3.strong.reason, ta3.political.activity, ta3.state.federal),
                           row.names = c("Traditional principles","Plain meaning","Original intent","Opinion when adopted","Precedent",
                                         "Non-traditional principles","Consequences","Other countries","Public opinion",
                                         "Strong reason","Political activity","State or federal"))
colnames(ta3.dataframe) <- c("Woman","Man")
xtable(ta3.dataframe)

###########################################
# Table A-4 (Race Descriptive Principles) # 
###########################################
race.plain <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_1 %in% c("Very important", "Somewhat important"), w = Propwts)) #plain meaning to modern reader
race.intent <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_2 %in% c("Very important", "Somewhat important"), w = Propwts)) #intent of framers
race.adopted <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_3 %in% c("Very important", "Somewhat important"), w = Propwts)) #what most thought when adopted
race.precedent <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_4 %in% c("Very important", "Somewhat important"), w = Propwts)) #stare decisis (previous SC decisions)
race.strong <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_5 %in% c("Very important", "Somewhat important"), w = Propwts)) #strong reason, e.g. national security
race.political <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_6 %in% c("Very important", "Somewhat important"), w = Propwts)) #political activity
race.state <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_7 %in% c("Very important", "Somewhat important"), w = Propwts)) #state or federal
race.consequences <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_8 %in% c("Very important", "Somewhat important"), w = Propwts)) #consequences
race.countries <- survey %>% 
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_9 %in% c("Very important", "Somewhat important"), w = Propwts)) #other countries
race.public <- survey %>%
  group_by(QNEW) %>%
  summarize(weighted.mean(QPHILO3_10 %in% c("Very important", "Somewhat important"), w = Propwts)) #public opinion

ta4.plain.meaning <- c(race.plain$`weighted.mean(...)`)
ta4.original.intent <- c(race.intent$`weighted.mean(...)`)
ta4.opinion.when.adopted <- c(race.adopted$`weighted.mean(...)`)
ta4.precedent <- c(race.precedent$`weighted.mean(...)`)
ta4.average.traditional <- apply(rbind(ta4.plain.meaning, ta4.original.intent, ta4.opinion.when.adopted, ta4.precedent), 2, mean)

ta4.plain.meaning <- paste(as.character(round(ta4.plain.meaning*100,0)),"%",sep="")
ta4.original.intent <- paste(as.character(round(ta4.original.intent*100,0)),"%",sep="")
ta4.opinion.when.adopted <- paste(as.character(round(ta4.opinion.when.adopted*100,0)),"%",sep="")
ta4.precedent <- paste(as.character(round(ta4.precedent*100,0)),"%",sep="")
ta4.average.traditional <- paste(as.character(round(ta4.average.traditional*100,0)),"%",sep="") 

ta4.consequences <- c(race.consequences$`weighted.mean(...)`)
ta4.other.countries <- c(race.countries$`weighted.mean(...)`)
ta4.public.opinion <- c(race.public$`weighted.mean(...)`)
ta4.average.non.traditional <- apply(rbind(ta4.consequences, ta4.other.countries, ta4.public.opinion), 2, mean)

ta4.consequences <- paste(as.character(round(ta4.consequences*100,0)),"%",sep="")
ta4.other.countries <- paste(as.character(round(ta4.other.countries*100,0)),"%",sep="")
ta4.public.opinion <- paste(as.character(round(ta4.public.opinion*100,0)),"%",sep="")
ta4.average.non.traditional <- paste(as.character(round(ta4.average.non.traditional*100,0)),"%",sep="") 

ta4.strong.reason <- c(race.strong$`weighted.mean(...)`)
ta4.political.activity <- c(race.political$`weighted.mean(...)`)
ta4.state.federal <- c(race.state$`weighted.mean(...)`)

ta4.strong.reason <- paste(as.character(round(ta4.strong.reason*100,0)),"%",sep="")
ta4.political.activity <- paste(as.character(round(ta4.political.activity*100,0)),"%",sep="")
ta4.state.federal <- paste(as.character(round(ta4.state.federal*100,0)),"%",sep="")

ta4.dataframe <- data.frame(rbind(ta4.average.traditional, ta4.plain.meaning, ta4.original.intent, ta4.opinion.when.adopted, ta4.precedent,
                                  ta4.average.non.traditional, ta4.consequences, ta4.other.countries, ta4.public.opinion,
                                  ta4.strong.reason, ta4.political.activity, ta4.state.federal),
                            row.names = c("Traditional principles","Plain meaning","Original intent","Opinion when adopted","Precedent",
                                          "Non-traditional principles","Consequences","Other countries","Public opinion",
                                          "Strong reason","Political activity","State or federal"))
colnames(ta4.dataframe) <- c("White","Black","Latino","Asian","Other")
xtable(ta4.dataframe)

###################################################
# Table A-5 (Partisanship Descriptive Principles) # 
###################################################
partisanship.plain <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_1 %in% c("Very important", "Somewhat important"), w = Propwts)) #plain meaning to modern reader
partisanship.intent <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_2 %in% c("Very important", "Somewhat important"), w = Propwts)) #intent of framers
partisanship.adopted <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_3 %in% c("Very important", "Somewhat important"), w = Propwts)) #what most thought when adopted
partisanship.precedent <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_4 %in% c("Very important", "Somewhat important"), w = Propwts)) #stare decisis (previous SC decisions)
partisanship.strong <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_5 %in% c("Very important", "Somewhat important"), w = Propwts)) #strong reason, e.g. national security
partisanship.political <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_6 %in% c("Very important", "Somewhat important"), w = Propwts)) #political activity
partisanship.state <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_7 %in% c("Very important", "Somewhat important"), w = Propwts)) #state or federal
partisanship.consequences <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_8 %in% c("Very important", "Somewhat important"), w = Propwts)) #consequences
partisanship.countries <- survey %>% 
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_9 %in% c("Very important", "Somewhat important"), w = Propwts)) #other countries
partisanship.public <- survey %>%
  group_by(Q1903) %>%
  summarize(weighted.mean(QPHILO3_10 %in% c("Very important", "Somewhat important"), w = Propwts)) #public opinion

ta5.plain.meaning <- c(partisanship.plain$`weighted.mean(...)`)
ta5.original.intent <- c(partisanship.intent$`weighted.mean(...)`)
ta5.opinion.when.adopted <- c(partisanship.adopted$`weighted.mean(...)`)
ta5.precedent <- c(partisanship.precedent$`weighted.mean(...)`)
ta5.average.traditional <- apply(rbind(ta5.plain.meaning, ta5.original.intent, ta5.opinion.when.adopted, ta5.precedent), 2, mean)

ta5.plain.meaning <- paste(as.character(round(ta5.plain.meaning*100,0)),"%",sep="")
ta5.original.intent <- paste(as.character(round(ta5.original.intent*100,0)),"%",sep="")
ta5.opinion.when.adopted <- paste(as.character(round(ta5.opinion.when.adopted*100,0)),"%",sep="")
ta5.precedent <- paste(as.character(round(ta5.precedent*100,0)),"%",sep="")
ta5.average.traditional <- paste(as.character(round(ta5.average.traditional*100,0)),"%",sep="") 

ta5.consequences <- c(partisanship.consequences$`weighted.mean(...)`)
ta5.other.countries <- c(partisanship.countries$`weighted.mean(...)`)
ta5.public.opinion <- c(partisanship.public$`weighted.mean(...)`)
ta5.average.non.traditional <- apply(rbind(ta5.consequences, ta5.other.countries, ta5.public.opinion), 2, mean)

ta5.consequences <- paste(as.character(round(ta5.consequences*100,0)),"%",sep="")
ta5.other.countries <- paste(as.character(round(ta5.other.countries*100,0)),"%",sep="")
ta5.public.opinion <- paste(as.character(round(ta5.public.opinion*100,0)),"%",sep="")
ta5.average.non.traditional <- paste(as.character(round(ta5.average.non.traditional*100,0)),"%",sep="") 

ta5.strong.reason <- c(partisanship.strong$`weighted.mean(...)`)
ta5.political.activity <- c(partisanship.political$`weighted.mean(...)`)
ta5.state.federal <- c(partisanship.state$`weighted.mean(...)`)

ta5.strong.reason <- paste(as.character(round(ta5.strong.reason*100,0)),"%",sep="")
ta5.political.activity <- paste(as.character(round(ta5.political.activity*100,0)),"%",sep="")
ta5.state.federal <- paste(as.character(round(ta5.state.federal*100,0)),"%",sep="")

ta5.dataframe <- data.frame(rbind(ta5.average.traditional, ta5.plain.meaning, ta5.original.intent, ta5.opinion.when.adopted, ta5.precedent,
                                  ta5.average.non.traditional, ta5.consequences, ta5.other.countries, ta5.public.opinion,
                                  ta5.strong.reason, ta5.political.activity, ta5.state.federal),
                            row.names = c("Traditional principles","Plain meaning","Original intent","Opinion when adopted","Precedent",
                                          "Non-traditional principles","Consequences","Other countries","Public opinion",
                                          "Strong reason","Political activity","State or federal"))
colnames(ta5.dataframe) <- c("Republican","Democratic","Independent","Other")
xtable(ta5.dataframe)

################################################
# Table A-6 (Education Descriptive Principles) # 
################################################
# Creating new dataset with re-coded education variable
survey_edu <- survey %>%
  mutate(education = case_when(Q2146 == "Job-specific training programs after high school" ~ "High school graduate or equivalent (e.g., GED)",
                               Q2146 == "Completed some graduate school, but no degree" ~ "College graduate (e.g., B.A., A.B., B.S.)",
                               Q2146 == "Completed some high school" ~ "Less than high school",
                               TRUE ~ as.character(Q2146)),
         education = factor(education,
                            levels = c("Less than high school",
                                       "High school graduate or equivalent (e.g., GED)",
                                       "Completed some college, but no degree",
                                       "Associate's degree",
                                       "College graduate (e.g., B.A., A.B., B.S.)",
                                       "Completed graduate school (e.g., M.S., M.D., Ph.D.)")))

education.plain <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_1 %in% c("Very important", "Somewhat important"), w = Propwts)) #plain meaning to modern reader
education.intent <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_2 %in% c("Very important", "Somewhat important"), w = Propwts)) #intent of framers
education.adopted <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_3 %in% c("Very important", "Somewhat important"), w = Propwts)) #what most thought when adopted
education.precedent <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_4 %in% c("Very important", "Somewhat important"), w = Propwts)) #stare decisis (previous SC decisions)
education.strong <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_5 %in% c("Very important", "Somewhat important"), w = Propwts)) #strong reason, e.g. national security
education.political <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_6 %in% c("Very important", "Somewhat important"), w = Propwts)) #political activity
education.state <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_7 %in% c("Very important", "Somewhat important"), w = Propwts)) #state or federal
education.consequences <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_8 %in% c("Very important", "Somewhat important"), w = Propwts)) #consequences
education.countries <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_9 %in% c("Very important", "Somewhat important"), w = Propwts)) #other countries
education.public <- survey_edu %>%
  group_by(education) %>%
  summarize(weighted.mean(QPHILO3_10 %in% c("Very important", "Somewhat important"), w = Propwts)) #public opinion

ta6.plain.meaning <- c(education.plain$`weighted.mean(...)`)
ta6.original.intent <- c(education.intent$`weighted.mean(...)`)
ta6.opinion.when.adopted <- c(education.adopted$`weighted.mean(...)`)
ta6.precedent <- c(education.precedent$`weighted.mean(...)`)
ta6.average.traditional <- apply(rbind(ta6.plain.meaning, ta6.original.intent, ta6.opinion.when.adopted, ta6.precedent), 2, mean)

ta6.plain.meaning <- paste(as.character(round(ta6.plain.meaning*100,0)),"%",sep="")
ta6.original.intent <- paste(as.character(round(ta6.original.intent*100,0)),"%",sep="")
ta6.opinion.when.adopted <- paste(as.character(round(ta6.opinion.when.adopted*100,0)),"%",sep="")
ta6.precedent <- paste(as.character(round(ta6.precedent*100,0)),"%",sep="")
ta6.average.traditional <- paste(as.character(round(ta6.average.traditional*100,0)),"%",sep="") 

ta6.consequences <- c(education.consequences$`weighted.mean(...)`)
ta6.other.countries <- c(education.countries$`weighted.mean(...)`)
ta6.public.opinion <- c(education.public$`weighted.mean(...)`)
ta6.average.non.traditional <- apply(rbind(ta6.consequences, ta6.other.countries, ta6.public.opinion), 2, mean)

ta6.consequences <- paste(as.character(round(ta6.consequences*100,0)),"%",sep="")
ta6.other.countries <- paste(as.character(round(ta6.other.countries*100,0)),"%",sep="")
ta6.public.opinion <- paste(as.character(round(ta6.public.opinion*100,0)),"%",sep="")
ta6.average.non.traditional <- paste(as.character(round(ta6.average.non.traditional*100,0)),"%",sep="") 

ta6.strong.reason <- c(education.strong$`weighted.mean(...)`)
ta6.political.activity <- c(education.political$`weighted.mean(...)`)
ta6.state.federal <- c(education.state$`weighted.mean(...)`)

ta6.strong.reason <- paste(as.character(round(ta6.strong.reason*100,0)),"%",sep="")
ta6.political.activity <- paste(as.character(round(ta6.political.activity*100,0)),"%",sep="")
ta6.state.federal <- paste(as.character(round(ta6.state.federal*100,0)),"%",sep="")

ta6.dataframe <- data.frame(rbind(ta6.average.traditional, ta6.plain.meaning, ta6.original.intent, ta6.opinion.when.adopted, ta6.precedent,
                                  ta6.average.non.traditional, ta6.consequences, ta6.other.countries, ta6.public.opinion,
                                  ta6.strong.reason, ta6.political.activity, ta6.state.federal),
                            row.names = c("Traditional principles","Plain meaning","Original intent","Opinion when adopted","Precedent",
                                          "Non-traditional principles","Consequences","Other countries","Public opinion",
                                          "Strong reason","Political activity","State or federal"))
colnames(ta6.dataframe) <- c("No high school","High school","Some college","College (2yr)","College (4yr)","Graduate degree")
xtable(ta6.dataframe)

#############################################
# Table A-7 (Income Descriptive Principles) # 
#############################################
# Creating new dataset with re-coded income variable
survey_income <- survey %>%
  mutate(income = case_when(Q2157 %in% c("Less than $15,000",
                                         "$15,000 to $24,999") ~ "Under $25,000",
                            Q2157 %in% c("$25,000 to $34,999",
                                         "$35,000 to $49,999") ~ "$25,000 to $49,999",
                            Q2157 %in% c("$150,000 to $199,999",
                                         "$200,000 to $249,999",
                                         "$250,000 or more") ~ "$150,000 or more",
                            TRUE ~ as.character(Q2157)),
         income = factor(income, levels = c("Under $25,000",
                                            "$25,000 to $49,999",
                                            "$50,000 to $74,999",
                                            "$75,000 to $99,999",
                                            "$100,000 to $124,999",
                                            "$125,000 to $149,999",
                                            "$150,000 or more",
                                            "Decline to answer")))


income.plain <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_1 %in% c("Very important", "Somewhat important"), w = Propwts)) #plain meaning to modern reader
income.intent <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_2 %in% c("Very important", "Somewhat important"), w = Propwts)) #intent of framers
income.adopted <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_3 %in% c("Very important", "Somewhat important"), w = Propwts)) #what most thought when adopted
income.precedent <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_4 %in% c("Very important", "Somewhat important"), w = Propwts)) #stare decisis (previous SC decisions)
income.strong <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_5 %in% c("Very important", "Somewhat important"), w = Propwts)) #strong reason, e.g. national security
income.political <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_6 %in% c("Very important", "Somewhat important"), w = Propwts)) #political activity
income.state <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_7 %in% c("Very important", "Somewhat important"), w = Propwts)) #state or federal
income.consequences <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_8 %in% c("Very important", "Somewhat important"), w = Propwts)) #consequences
income.countries <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_9 %in% c("Very important", "Somewhat important"), w = Propwts)) #other countries
income.public <- survey_income %>%
  group_by(income) %>%
  summarize(weighted.mean(QPHILO3_10 %in% c("Very important", "Somewhat important"), w = Propwts)) #public opinion

ta7.plain.meaning <- c(income.plain$`weighted.mean(...)`)
ta7.original.intent <- c(income.intent$`weighted.mean(...)`)
ta7.opinion.when.adopted <- c(income.adopted$`weighted.mean(...)`)
ta7.precedent <- c(income.precedent$`weighted.mean(...)`)
ta7.average.traditional <- apply(rbind(ta7.plain.meaning, ta7.original.intent, ta7.opinion.when.adopted, ta7.precedent), 2, mean)

ta7.plain.meaning <- paste(as.character(round(ta7.plain.meaning*100,0)),"%",sep="")
ta7.original.intent <- paste(as.character(round(ta7.original.intent*100,0)),"%",sep="")
ta7.opinion.when.adopted <- paste(as.character(round(ta7.opinion.when.adopted*100,0)),"%",sep="")
ta7.precedent <- paste(as.character(round(ta7.precedent*100,0)),"%",sep="")
ta7.average.traditional <- paste(as.character(round(ta7.average.traditional*100,0)),"%",sep="") 

ta7.consequences <- c(income.consequences$`weighted.mean(...)`)
ta7.other.countries <- c(income.countries$`weighted.mean(...)`)
ta7.public.opinion <- c(income.public$`weighted.mean(...)`)
ta7.average.non.traditional <- apply(rbind(ta7.consequences, ta7.other.countries, ta7.public.opinion), 2, mean)

ta7.consequences <- paste(as.character(round(ta7.consequences*100,0)),"%",sep="")
ta7.other.countries <- paste(as.character(round(ta7.other.countries*100,0)),"%",sep="")
ta7.public.opinion <- paste(as.character(round(ta7.public.opinion*100,0)),"%",sep="")
ta7.average.non.traditional <- paste(as.character(round(ta7.average.non.traditional*100,0)),"%",sep="") 

ta7.strong.reason <- c(income.strong$`weighted.mean(...)`)
ta7.political.activity <- c(income.political$`weighted.mean(...)`)
ta7.state.federal <- c(income.state$`weighted.mean(...)`)

ta7.strong.reason <- paste(as.character(round(ta7.strong.reason*100,0)),"%",sep="")
ta7.political.activity <- paste(as.character(round(ta7.political.activity*100,0)),"%",sep="")
ta7.state.federal <- paste(as.character(round(ta7.state.federal*100,0)),"%",sep="")

ta7.dataframe <- data.frame(rbind(ta7.average.traditional, ta7.plain.meaning, ta7.original.intent, ta7.opinion.when.adopted, ta7.precedent,
                                  ta7.average.non.traditional, ta7.consequences, ta7.other.countries, ta7.public.opinion,
                                  ta7.strong.reason, ta7.political.activity, ta7.state.federal),
                            row.names = c("Traditional principles","Plain meaning","Original intent","Opinion when adopted","Precedent",
                                          "Non-traditional principles","Consequences","Other countries","Public opinion",
                                          "Strong reason","Political activity","State or federal"))
colnames(ta7.dataframe) <- c("<$25K","$25-50K","$50-75K","$75-100K","$100-125K","$125-150K",">150K","Refused")
xtable(ta7.dataframe)

#############################################
# Table A-8 (Principal Components Analysis) # 
#############################################
# Factor analysis
# The principles questions
principlesQuestions <- subset(survey,select=c("QPHILO3_1","QPHILO3_2","QPHILO3_3","QPHILO3_4","QPHILO3_5","QPHILO3_6","QPHILO3_7","QPHILO3_8","QPHILO3_9","QPHILO3_10"))
# Function to make the questions numeric factors
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))
principlesQuestions <- factorsNumeric(principlesQuestions)
# Reversing the coding so very important is 4 and not at all important is 1
principlesQuestions <- principlesQuestions * -1 + 5

# Principal components analysis, two factors (components)
principlesFactors <- principal(principlesQuestions, nfactors = 2, rotate="varimax", missing=T)
# Question 1 is plain meaning, 2 is original intent, 3 is opinion when adopted, 4 is precedent, 5 is strong reason, 6 is political activity,
# 7 is state or federal, 8 is consequences, 9 is other countries, 10 is public opinion
principlesFactors
# Correlation of PCA with responses to traditionalism and non-traditionalism
cor(survey$trad, principlesFactors$scores[,1])
cor(survey$nontrad, principlesFactors$scores[,2])

# Exploratory factor analysis
efa <- fa(principlesQuestions, nfactors = 2, rotate = "oblimin", fm = "minres")
# Question 1 is plain meaning, 2 is original intent, 3 is opinion when adopted, 4 is precedent, 5 is strong reason, 6 is political activity,
# 7 is state or federal, 8 is consequences, 9 is other countries, 10 is public opinion
efa
# Correlation of factor analysis with responses to traditionalism and non-traditionalism
cor(survey$trad, efa$scores[, 1])
cor(survey$nontrad, efa$scores[, 2])

#################################################
# Figure A-3 (Scree Plot and Parallel Analysis) # 
#################################################
# Scree plot
pdf("figure-a-3a.pdf")
plot(principlesFactors$values,ylab="Eigenvalues",xlab="Component Number", main="Scree Plot", type="b")
dev.off()
# Parallel analysis
pdf("figure-a-3b.pdf")
fa.parallel(principlesQuestions, fa="pc", n.iter=100, main="Parallel Analysis",ylab="Eigenvalues")
dev.off()

################################################
# Table A-9 (Regressions, Combined Principles) # 
################################################
# Coding descriptive variables 
age <- ifelse(survey$Q4007 %in% c(1:29), 1, NA)
age <- ifelse(survey$Q4007 %in% c(30:45), 2, age)
age <- ifelse(survey$Q4007 %in% c(46:60), 3, age)
age <- ifelse(survey$Q4007 %in% c(61:75), 4, age)
age <- ifelse(survey$Q4007 %in% c(76:199), 5, age)
survey$age <- age

survey$white <- ifelse(survey$race_cat == 0, 1, 0)

edu_cat <- rep(NA, length(survey$Q2146))
edu_cat <- ifelse(survey$Q2146 %in% c("Less than high school","Completed some high school"),0,edu_cat)
edu_cat <- ifelse(survey$Q2146 %in% c("High school graduate or equivalent (e.g., GED)","Job-specific training programs after high school"),1,edu_cat)
edu_cat <- ifelse(survey$Q2146 %in% c("Completed some college, but no degree"),2,edu_cat)
edu_cat <- ifelse(survey$Q2146 %in% c("Associate's degree"),3,edu_cat)
edu_cat <- ifelse(survey$Q2146 %in% c("College graduate (e.g., B.A., A.B., B.S.)","Completed some graduate school, but no degree"),4,edu_cat)
edu_cat <- ifelse(survey$Q2146 %in% c("Completed graduate school (e.g., M.S., M.D., Ph.D.)"),5,edu_cat)

survey$edu_cat <- edu_cat

# Create index of first four principles and last three principles
trad <- survey %>%
  mutate(Support = (principles1 + principles2 + principles3 + principles4) / 4,
         Group = "Traditional Principles")

non_trad <- survey %>%
  mutate(Support = (principles8 + principles9 + principles10) / 3,
         Group = "Non-Traditional Principles")

# Regression results 
trad_knowledge_reg <- lm(I((Support-1)/3) ~ knowledge * ideo3 + female + edu_cat + white + age, trad, weights = Propwts)
non_trad_knowledge_reg <- lm(I((Support-1)/3) ~ knowledge * ideo3 + female + edu_cat + white + age, non_trad, weights = Propwts)

stargazer(trad_knowledge_reg, non_trad_knowledge_reg,
          digits = 2,
          style = "AJPS",
          star.cutoffs = 0.05,
          keep.stat = c("n", "adj.rsq"),
          label = "group_reg",
          title = "Politics, Judicial Knowledge and Support for Principles of Judging",
          column.labels = c("Traditional Principles",
                            "Non-Traditional Principles"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          covariate.labels = c("Knowledge",
                               "Liberal",
                               "Moderate",
                               "Female",
                               "Education (Cat.)",
                               "White",
                               "Age (Cat.)",
                               "Knowledge $\\times$ Liberal",
                               "Knowledge $\\times$ Moderate"))
  
###################################################
# Table A-10 (Regressions, Individual Principles) # 
###################################################
survey <- survey %>%
  mutate(principles1_01 = (principles1 - 1)/3,
         principles2_01 = (principles2 - 1)/3,
         principles3_01 = (principles3 - 1)/3,
         principles4_01 = (principles4 - 1)/3,
         principles5_01 = (principles5 - 1)/3,
         principles6_01 = (principles6 - 1)/3,
         principles7_01 = (principles7 - 1)/3,
         principles8_01 = (principles8 - 1)/3,
         principles9_01 = (principles9 - 1)/3,
         principles10_01 = (principles10 - 1)/3)

principles1_reg <- lm(principles1_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # plain meaning
principles2_reg <- lm(principles2_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # original intent
principles3_reg <- lm(principles3_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # opinion when adopted
principles4_reg <- lm(principles4_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # precedent

principles5_reg <- lm(principles5_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # strong reason/national security
principles6_reg <- lm(principles6_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # political activity
principles7_reg <- lm(principles7_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # state or federal

principles8_reg <- lm(principles8_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # consequences
principles9_reg <- lm(principles9_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # other countries
principles10_reg <- lm(principles10_01 ~ knowledge * ideo3 + female + edu_cat + white + age, survey, weights = Propwts) # public opinion

reg.list <- list(principles1_reg, principles2_reg,principles3_reg,principles4_reg,principles8_reg,
                 principles9_reg, principles10_reg,principles5_reg,principles6_reg,principles7_reg)

stargazer(reg.list,
          digits = 2,
          style = "AJPS",
          star.cutoffs = 0.05,
          keep.stat = c("n", "adj.rsq"),
          label = "indiv_reg2_all10",
          title = "Politics, Judicial Knowledge and Support for Principles of Judging: Individual Principles",
          column.labels = c("Plain M.","Original Int.","Opinion Adop.","Precedent","Consequences",
                            "Other Countries","Public Op.","Strong Reason","Political Act.","State/Fed."),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          covariate.labels = c("Knowledge",
                               "Liberal",
                               "Moderate",
                               "Female",
                               "Education (Cat.)",
                               "White",
                               "Age (Cat.)",
                               "Knowledge $\\times$ Liberal",
                               "Knowledge $\\times$ Moderate"))

####################################################################
# Table A-11 (Ordered Logistic Regressions, Individual Principles) # 
####################################################################
principles1_01_ordered <- as.ordered(survey$principles1_01)
principles2_01_ordered <- as.ordered(survey$principles2_01)
principles3_01_ordered <- as.ordered(survey$principles3_01)
principles4_01_ordered <- as.ordered(survey$principles4_01)
principles5_01_ordered <- as.ordered(survey$principles5_01)
principles6_01_ordered <- as.ordered(survey$principles6_01)
principles7_01_ordered <- as.ordered(survey$principles7_01)
principles8_01_ordered <- as.ordered(survey$principles8_01)
principles9_01_ordered <- as.ordered(survey$principles9_01)
principles10_01_ordered <- as.ordered(survey$principles10_01)

indiv1_ordered_logit <- polr(principles1_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # plain meaning
indiv2_ordered_logit <- polr(principles2_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # original intent
indiv3_ordered_logit <- polr(principles3_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # opinion when adopted
indiv4_ordered_logit <- polr(principles4_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # precedent

indiv5_ordered_logit <- polr(principles5_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # strong reason/national security
indiv6_ordered_logit <- polr(principles6_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # political activity
indiv7_ordered_logit <- polr(principles7_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # state or federal

indiv8_ordered_logit <- polr(principles8_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # consequences
indiv9_ordered_logit <- polr(principles9_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # other countries
indiv10_ordered_logit <- polr(principles10_01_ordered ~ knowledge * ideo3 + female + edu_cat + white + age, weights=Propwts, data=survey) # public opinion

ordered.reg.list <- list(indiv1_ordered_logit, indiv2_ordered_logit, indiv3_ordered_logit, indiv4_ordered_logit, indiv8_ordered_logit,
                      indiv9_ordered_logit, indiv10_ordered_logit, indiv5_ordered_logit, indiv6_ordered_logit, indiv7_ordered_logit)

stargazer(ordered.reg.list,
          digits = 2,
          style = "AJPS",
          star.cutoffs = 0.05,
          keep.stat = c("n", "adj.rsq"),
          label = "indiv_reg2_ordered_logit_all10",
          title = "Politics, Judicial Knowledge and Support for Principles of Judging: Ordered Logistic Regressions",
          dep.var.labels = c("Plain Meaning",
                             "Original Intent",
                             "Opinion when Adopted",
                             "Precedent",
                             "Consequences",
                             "Other Countries",
                             "Public Opinion",
                             "Strong Reason",
                             "Political Activity",
                             "State or Federal"),
          covariate.labels = c("Knowledge",
                               "Liberal",
                               "Moderate",
                               "Female",
                               "Education (Cat.)",
                               "White",
                               "Age (Cat.)",
                               "Knowledge $\\times$ Liberal",
                               "Knowledge $\\times$ Moderate"))

##################################################
# Figure A-4 (Results by Attention to the Court) # 
##################################################
pdf("figure-a-4.pdf")
bind_rows(trad, non_trad) %>%
  mutate(Support = (Support - 1) / 3,
         Group = factor(Group, levels = c("Traditional Principles", "Non-Traditional Principles")),
         ideo3 = factor(ideo3, levels = c("Conservative", "Moderate", "Liberal"))) %>%
  ggplot(aes(y=Support,x=attention,color=ideo3,linetype = ideo3, weight=Propwts)) +
  geom_smooth(method="lm") + xlab("Attention to the Court") + ylab("Average Support for Principles") +
  facet_wrap(~Group) +
  guides(color=guide_legend(title="Ideology"),
         linetype = guide_legend(title = "Ideology")) + theme_bw() +
  scale_color_manual(values=c("Red","Purple","Blue")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
