# This code merge all toloka datasets into one and merge them with main oTree dataset

library(pacman)
setwd('/Users/annaivanova/Documents/jbee_osf/data')
p_load(
  'haven',
  'tidyverse',
  'readxl',
  'modeest',
  'ggpubr',
  'kableExtra',
  'glue',
  'VGAM',
  'Publish',
  'sjlabelled'
)


processed_dictators_toloka <-
  read_csv('toloka_datasets/processed_toloka_df.csv')
main_df <-
  read_csv('raw/polar_2021-10-13.csv') %>% rename_with( ~ gsub('player\\.(.*)', '\\1', .))
merged_df <-
  left_join(main_df,
            processed_dictators_toloka,
            by = c("participant.label" = "assignment_id"))
df <- merged_df %>% filter(status == 'SUBMITTED')

# We have 799 dictators and 799 recipieints:
df %>% dim()

# some renaming
df <- df %>% rename(treatment = subsession.treatment)
# we reassign aligned var to count for two missing obs who quit earlier
df <- df %>% mutate(aligned = opinion_lgbt == partner_position)
# we also replace NA in reveal to something because they take NA value only for the forced_reveal treatment (where the information
# is observed)
df <- df %>% mutate(reveal = replace_na(reveal, -1))
# how many obs we have per treatment
df %>% group_by(treatment) %>% tally()



# some labelling
df %>% select(starts_with('ias')) %>% names()
df %>% group_by(ias_friend) %>% tally()
df %>% group_by(ias_coworker) %>% tally()
df %>% group_by(ias_stranger) %>% tally()
# Recoding Information Avoidance Scale (IAS): 'Don't want to know - 0, want to know  - 1
rus_want_friend <-
  "Даже если это меня расстроит, я хочу знать, что **мой друг** думает о приведенном выше высказывании."
rus_dwant_friend <-
  "Я бы предпочел не знать, что думает **мой друг** о приведенном выше высказывании."
rus_want_coworker <-
  "Даже если это меня расстроит, я хочу знать, что думает **мой коллега** по работе о приведенном выше высказывании."
rus_dwant_coworker <-
  "Я бы предпочел не знать, что думает **мой коллега** по работе о приведенном выше высказывании."
rus_want_stranger <-
  "Даже если это меня расстроит, я хочу знать, что думает о приведенном выше высказывании **малознакомый мне человек**"
rus_dwant_stranger <-
  "Я бы предпочел не знать, что **малознакомый мне человек** думает о приведенном выше высказывании."


ias_friend_recoder <-
  unlist(lst(!!rus_want_friend := 1,!!rus_dwant_friend := 0))
ias_coworker_recoder <-
  unlist(lst(!!rus_want_coworker := 1,!!rus_dwant_coworker := 0))
ias_stranger_recoder <-
  unlist(lst(!!rus_want_stranger := 1,!!rus_dwant_stranger := 0))
df <- df %>%
  mutate(
    ias_friend = recode(ias_friend,!!!ias_friend_recoder),
    ias_coworker = recode(ias_coworker,!!!ias_coworker_recoder),
    ias_stranger = recode(ias_stranger,!!!ias_stranger_recoder)
  )
# assigninlg labels to ias
ias_labels <- c('want to know' = 1, 'do not want to know' = 0)
df <- df %>% mutate(
  ias_friend = labelled(ias_friend, ias_labels),
  ias_coworker = labelled(ias_coworker, ias_labels),
  ias_stranger = labelled(ias_stranger, ias_labels)
)

# recoding Social Distance Index (SDI)
# Not at all comfortable = 0, not too comfortable = 1, somewhat comfortable = 2, extremely comfortable = 3.
# Not all all upset = 3, Not too upset = 2, Somewhat upset = 1, Extremely upset = 0
df %>% select(starts_with('sdi')) %>% names()
sdi_recoder <-
  c(
    'Полностью неприемлемо' = 0,
    'Испытываю некоторые затруднения' = 1,
    'Не вижу серьезных проблем' = 2,
    'Не испытываю малейших проблем' = 3
  )
sdi_family_recoder <-
  c(
    'Я бы сильно расстроился' = 0,
    'Я бы немного расстроился' = 1,
    'Я бы скорее не расстроился' = 2,
    'Я бы совершенно не расстроился' = 3
  )

df <- df %>%
  mutate(
    sdi_politics = recode(sdi_politics,!!!sdi_recoder),
    sdi_neighbors = recode(sdi_neighbors,!!!sdi_recoder),
    sdi_friends = recode(sdi_friends,!!!sdi_recoder),
    sdi_family = recode(sdi_family,!!!sdi_family_recoder)
  )

# SES
# Decode:age, education, gender, marital, employment, income
# AGE
age_recoder <-
  c(
    'Младше 18 лет' = 0,
    '18-24 года' = 1,
    "25-34 года" = 2,
    "35-44 года" = 3,
    "45-54 года" = 4,
    "55-64 года" = 5,
    '65 лет и старше' = 6
  )
df <- df %>%
  mutate(age = recode(age,!!!age_recoder))
#EDUCATION
education_recoder <- c(
  'Средняя школа' = 0,
  'Среднее профессиональное образование' = 1,
  "Незаконченное высшее образование" = 2,
  "Высшее образование" = 3,
  "Два и более диплома / Ученая степень" = 4
)
df <- df %>%
  mutate(education = recode(education,!!!education_recoder))

#GENDER
df %>% group_by(gender) %>% tally()
gender_recorder <- c('Женский' = 1, 'Мужской' = 0)
df <- df %>%
  mutate(gender = recode(gender,!!!gender_recorder))

# MARITAL STATUS
df %>% group_by(marital) %>% tally()
marital_recorder <- c(
  'Женаты/замужем' = 0,
  'Не женаты/не замужем' = 1,
  'В отношениях, но официально не состоите в браке' =
    2,
  'Разведены' = 3,
  'Живете отдельно от супруга/и' = 4,
  'Вдовец/Вдова' = 5
)
df <- df %>%
  mutate(marital = recode(marital,!!!marital_recorder))
# EMPLOYMENT
df %>% group_by(employment) %>% tally()
employment_recorder <- c(
  'Трудоустроен (полный рабочий день)' = 0,
  'Трудоустроен (частичная занятость)' = 1,
  'Не работаю и ищу работу' = 2,
  'Не работаю (по состоянию здоровья)' = 3,
  'Не работаю (на пенсии)' = 4,
  'Не работаю (другое)' = 5,
  'Самозанятый' = 6,
  'Предпочитаю не отвечать' = 999
)
df <- df %>%
  mutate(employment = recode(employment,!!!employment_recorder))

# income
df %>% group_by(income) %>% tally()
income_recorder <- c(
  'Не хватает денег даже на еду' = 0,
  'Хватает на еду, но не хватает на покупку одежды и обуви' = 1,
  'Хватает на одежду и обувь, но не хватает на покупку мелкой бытовой техники' = 2,
  'Хватает денег на небольшие покупки, но покупка дорогих вещей (компьютера, стиральной машины, холодильника) требует накоплений или кредита' = 3,
  'Хватает денег на покупки для дома, но на покупку машины, дачи, квартиры необходимо копить или брать кредит' = 4,
  'Можем позволить себе любые покупки без ограничений и кредитов' = 5
)
df <- df %>%
  mutate(income = recode(income,!!!income_recorder))%>% group_by(income) 
df<-df%>%ungroup()
# converting to factors with labelled+as_factor
# 
sdi_labels <-  c(
  'Not at all comfortable' = 0,
  'Not too comfortable' = 1,
  'Somewhat comfortable' = 2,
  'Extremely comfortable' = 3
)

sdi_family_labels <-  c(
  'Not all all upset' = 3,
  'Not too upset' = 2,
  'Somewhat upset' = 1,
  ' Extremely upset' = 0
)

df %>% mutate(
  ias = ias_friend+ias_coworker+ias_stranger,
  sdi = sdi_politics+sdi_neighbors+sdi_friends+sdi_family,
  risk = risk_general+risk_financial_matters+risk_free+risk_profession+risk_health+risk_strangers+risk_driving,
  scs=scs_habits+scs_why+scs_conversation+scs_listening+scs_quarrel,
  num_reveal=reveal,
  num_age=age,
  num_education=education,
  num_income=income,
  
  opinion_lgbt = labelled::to_factor(labelled(opinion_lgbt, c(
    "Pro" = 1, "Contra" = 0
  ))),
  gender = labelled::to_factor(labelled(gender, c(
    "Female" = 1, "Male" = 0
  ))),
  age = labelled::to_factor(labelled(
    age,
    c(
      "< 18 y.o." = 0,
      "18-24 y.o." = 1,
      "25-34 y.o." = 2,
      "35-44 y.o." = 3,
      "45-54 y.o." = 4,
      "55-64 y.o." = 5,
      "65 years or older" = 6
    )
  ), ordered = T),
  education = labelled::to_factor(labelled(
    education,
    c(
      'Secondary school' = 0,
      'Vocational school' = 1,
      "Undergraduate student" = 2,
      "BA/MA" = 3,
      "Ph.D" = 4
    )
  ), ordered = T),
  marital = labelled::to_factor(labelled(
    marital,
    c(
      'Married' = 0,
      'Single' = 1,
      'In a relationship' =    2,
      'Divorsed' = 3,
      'Separated' = 4,
      'Widow/er' = 5
    )
  ), ordered = F),
  employment = labelled::to_factor(labelled(
    employment,
    c(
      'Employed (full-time)' = 0,
      'Employed (part-time)' = 1,
      'Unemployed (looking for a job)' =    2,
      'Unemployed (health reasons)' = 3,
      'Unemployed (retired)' = 4,
      'Unemployed (other)' = 5,
      'Self-employed' = 6,
      'Do not want to answer' = 999
    )
  ), ordered = F),
  income = labelled::to_factor(labelled(
    income,
    c(
      'Not even enough money for food' = 0,
      'Enough for food, but not enough to buy clothes and shoes' = 1,
      'Enough money for clothes and shoes, but not enough to buy small appliances' = 2,
      'Enough money for small purchases, but buying expensive things (computer, washing machine, fridge) requires savings or credit' = 3,
      'I have enough money to buy things for the house, but to buy a car, a summer house or an apartment I need to save up or take a loan' = 4,
      'Can afford all purchases without restrictions or credit' = 5
    )
  ), ordered = T),
  ias_friend=labelled::to_factor(ias_friend),
  ias_coworker=labelled::to_factor(ias_coworker),
  ias_stranger=labelled::to_factor(ias_stranger),
  sdi_politics = labelled::to_factor(labelled(sdi_politics, sdi_labels), ordered=T),
  sdi_neighbors  = labelled::to_factor(labelled(sdi_neighbors, sdi_labels), ordered=T),
  sdi_friends  = labelled::to_factor(labelled(sdi_friends, sdi_labels), ordered=T),
  sdi_family  = labelled::to_factor(labelled(sdi_family, sdi_family_labels), ordered=T),
  reveal = labelled::to_factor(labelled(reveal, c('Forced_info'=-1, 'Not reveal'=0, 'Reveal'=1))),
  aligned= labelled::to_factor(labelled(as_numeric(aligned), c('Aligned'=TRUE, 'Conflicting'=FALSE))),
) ->df

filter_data<-read_tsv('prescreener/filter_final.tsv')%>%rename(worker_id=`ASSIGNMENT:worker_id`, first_opinion_lgbt=`OUTPUT:lgbt_opinion`)%>%
  select(worker_id, first_opinion_lgbt)

df<-df%>%left_join(filter_data)
# we calculate consistency variable to check whether their opinions in the first and second wave coincides
df<-df%>% mutate(consistent=case_when(
  opinion_lgbt == 'Pro' & first_opinion_lgbt %in% c('rather_agree', 'totally_agree')~ "Consistent",
   opinion_lgbt == 'Contra' & first_opinion_lgbt %in% c('rather_disagree', 'totally_disagree')~ "Consistent",
    TRUE ~ 'Inconsistent'
))

# we calculate the degree of strength of the opinion based on their first answer
df<-df%>% mutate(opinion_strength=case_when(
   first_opinion_lgbt %in% c('totally_disagree', 'totally_agree')~ "Strong",
   first_opinion_lgbt %in% c('rather_disagree','rather_agree')~ "Weak",
  
))
df%>%select(starts_with('scs'))%>%names()
df%>%left_join(filter_data)%>%group_by(opinion_lgbt,first_opinion_lgbt)%>%tally()%>%group_by(opinion_lgbt)%>%mutate(freq=n/sum(n))
df%>%saveRDS('processed_data.Rda')
