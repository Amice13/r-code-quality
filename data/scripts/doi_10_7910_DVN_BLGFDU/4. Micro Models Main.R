############## Microlevel Data Analysis
install.packages("readr")
#install.packages("dplyr")
install.packages("tidyr")
#install.packages("stringr")
#install.packages("lme4")
#install.packages("lfe")
#install.packages("stargazer")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lme4)
library(lfe)
library(stargazer)

#setwd('')
df <- read_csv('NewsVisits_Micro.csv') %>%
  filter(!person_id %in% c(745, 57))

########################### Create demographics data by merging lucid and other sample
# demographic data
demos <- readRDS("Respondent_Info.rds")

# keep relevant variables
demos <- demos %>% 
  select(person_id, AGE_1, ETHN, HISP, EDU, GENDER, ideology, FOLLOW)

# merge the data
df <- df %>% left_join(demos, by = "person_id")

# recode variables
df$white <- ifelse(df$ETHN == 1 & df$HISP == 2, 1, 0)
df$male = ifelse(df$GENDER == 1, 1, 0)
df$age <- floor(df$AGE_1/10)
df$edu <- ifelse(df$EDU %in% c(1, 5, 6), "no high school",
                       ifelse(df$EDU == 8, "high school",
                              ifelse(df$EDU %in% c(9, 10, 11), "associate/junior college",
                                     ifelse(df$EDU %in% c(12, 13), "undergraduate",
                                            ifelse(df$EDU %in% c(14, 15), "graduate", NA)))))


######### Data Preparation

for (option in c('in_party', 'out_party')) {
  
  colname = paste0('state_', option)
  
  # df for model which measures effect of state on following visit number
  df <- df %>% mutate(state = as.factor(.data[[colname]]))
  # split the own state into a partisan and a pos/neg variable
  df <- df %>% mutate(negative = as.integer(as.character(recode_factor(state,
                                                                       `neg_non` = 1,
                                                                       `neg_part` = 1,
                                                                       `pos_part` = 0,
                                                                       `pos_non` = 0))),
                      positive = as.integer(as.character(recode_factor(state,
                                                                       `neg_non` = 0,
                                                                       `neg_part` = 0,
                                                                       `pos_part` = 1,
                                                                       `pos_non` = 1))),
                      partisan = as.integer(as.character(recode_factor(state,
                                                                       `neg_non` = 0,
                                                                       `neg_part` = 1,
                                                                       `pos_part` = 1,
                                                                       `pos_non` = 0)))) }

######
df$conegative <- ifelse(df$state_in_party == "neg_non" |df$state_in_party == "neg_part",1,0)
df$outnegative <- ifelse(df$state_out_party == "neg_non" |df$state_out_party == "neg_part",1,0)

######


  
  # create variables recording row number and number of rows left
  df_final <- df %>%
    group_by(person_id, session_day) %>%
    arrange(person_id, session_day, created_utc) %>% 
    mutate(n_rows_left = n() - row_number(),
           n_row = row_number(),
           cum_neg_in = cumsum(conegative),
           cum_neg_out = cumsum(outnegative),
           prev_neg_in = lag(cum_neg_in, default = 0),
           prev_neg_out = lag(cum_neg_out, default = 0),
           total_neg_in = last(cum_neg_in),
           total_neg_out = last(cum_neg_out),
           subseq_neg_in = ((total_neg_in-cum_neg_in)/n_rows_left)*100,
           subseq_neg_out = ((total_neg_out-cum_neg_out)/n_rows_left)*100,
           cum_pos = cumsum(positive),
           total_pos = last(cum_pos),
           subseq_pos = (total_pos-cum_pos)/n_rows_left,
           cum_part = cumsum(partisan),
           total_part = last(cum_part),
           subseq_part = ((total_part-cum_part)/n_rows_left)*100,
           bin_part = ifelse(subseq_part>0,1,0),
           cum_pol = cumsum(pol_title),
           total_pol = last(cum_pol),
           subseq_pol = ((total_pol-cum_pol)/n_rows_left)*100)%>%
    dplyr::select(person_id, session_day, n_row, n_rows_left, negative,
           partisan, subseq_part, state, state_in_party, state_in_party, state_out_party, prev_neg_in, prev_neg_out, subseq_pos,
           cum_neg_in, cum_neg_out, affil_party, total_pos, total_part, subseq_pol,
           subseq_neg_in, subseq_neg_out, bin_part, edu, male, white, FOLLOW, ideology, age)

#####
df_final$personrow <- paste0(as.character(df_final$person_id),"-", as.character(df_final$n_row))
df_final$session_day <- as.factor(df_final$session_day)
df_final_non <- filter(df_final, state_in_party =='pos_non' | state_in_party == 'neg_non')

#####
################## main models: linear fixed effects regression 
######## tables shown in appendix E.2
######## coefficients plotted in script 5

### Visits remaining (in and out party)  
mod1 <- felm(n_rows_left ~ cum_neg_in | personrow + session_day, data = df_final) 
mod2 <- felm(n_rows_left ~ cum_neg_in | personrow + session_day, data = filter(df_final, affil_party == "dem")) 
mod3 <- felm(n_rows_left ~ cum_neg_in | personrow + session_day, data = filter(df_final, affil_party == "rep")) 

stargazer(mod1, mod2, mod3)
stargazer(mod1, mod2, mod3, type = "html", out= "Table22_News Visits Remaining Inparty.htm")

mod1 <- felm(n_rows_left ~ cum_neg_out | personrow + session_day, data = df_final) 
mod2 <- felm(n_rows_left ~ cum_neg_out | personrow + session_day, data = filter(df_final, affil_party == "dem")) 
mod3 <- felm(n_rows_left ~ cum_neg_out | personrow+ session_day, data = filter(df_final, affil_party == "rep")) 

stargazer(mod1, mod2, mod3)
stargazer(mod1, mod2, mod3, type = "html", out= "Table26_News Visits Remaining Outparty.htm")


### Subsequent partisan news (in and out party)  
mod1 <- felm(subseq_part ~ cum_neg_in | personrow + session_day, data = df_final_non) 
mod2 <- felm(subseq_part ~ cum_neg_in | personrow + session_day, data = filter(df_final_non, affil_party == "dem")) 
mod3 <- felm(subseq_part ~ cum_neg_in | personrow + session_day, data = filter(df_final_non, affil_party == "rep")) 

stargazer(mod1, mod2, mod3)
stargazer(mod1, mod2, mod3, type = "html", out= "Table23_Subsequent Partisan Visits Inparty.htm")

mod1 <- felm(subseq_part ~ cum_neg_out | personrow + session_day, data = df_final_non) 
mod2 <- felm(subseq_part ~ cum_neg_out | personrow + session_day, data = filter(df_final_non, affil_party == "dem")) 
mod3 <- felm(subseq_part ~ cum_neg_out | personrow + session_day, data = filter(df_final_non, affil_party == "rep")) 

stargazer(mod1, mod2, mod3)
stargazer(mod1, mod2, mod3, type = "html", out= "Table27_Subsequent Partisan Visits Outparty.htm")


### Subsequent hard news (in and out party)  
mod1 <- felm(subseq_pol ~ cum_neg_in | personrow+ session_day, data = df_final) 
mod2 <- felm(subseq_pol ~ cum_neg_in | personrow + session_day, data = filter(df_final, affil_party == "dem")) 
mod3 <- felm(subseq_pol ~ cum_neg_in | personrow + session_day, data = filter(df_final, affil_party == "rep")) 

stargazer(mod1, mod2, mod3)
stargazer(mod1, mod2, mod3, type = "html", out= "Table24_Subsequent Hard News Visits Inparty.htm")


mod1 <- felm(subseq_pol ~ cum_neg_out | personrow + session_day, data = df_final) 
mod2 <- felm(subseq_pol ~ cum_neg_out | personrow + session_day, data = filter(df_final, affil_party == "dem")) 
mod3 <- felm(subseq_pol ~ cum_neg_out | personrow+ session_day, data = filter(df_final, affil_party == "rep")) 

stargazer(mod1, mod2, mod3)
stargazer(mod1, mod2, mod3, type = "html", out= "Table28_Subsequent Hard News Visits Outparty.htm")


### Subsequent negative news (in and out party) 
mod1 <- felm(subseq_neg_in ~ cum_neg_in | personrow+ session_day, data = df_final) 
mod2 <- felm(subseq_neg_in ~ cum_neg_in | personrow + session_day, data = filter(df_final, affil_party == "dem")) 
mod3 <- felm(subseq_neg_in ~ cum_neg_in | personrow + session_day, data = filter(df_final, affil_party == "rep")) 

stargazer(mod1, mod2, mod3)
stargazer(mod1, mod2, mod3, type = "html", out= "Table25_Subsequent Negative News Visits Inparty.htm")


mod1 <- felm(subseq_neg_out ~ cum_neg_out | personrow + session_day, data = df_final) 
mod2 <- felm(subseq_neg_out ~ cum_neg_out | personrow + session_day, data = filter(df_final, affil_party == "dem")) 
mod3 <- felm(subseq_neg_out ~ cum_neg_out | personrow+ session_day, data = filter(df_final, affil_party == "rep")) 

stargazer(mod1, mod2, mod3)
stargazer(mod1, mod2, mod3, type = "html", out= "Table29_Subsequent Negative News Visits Outparty.htm")
  
  
##################### Random Effects (appendix)
##### coefficients plotted in appendix F.2 in script 5 (full tables removed from appendix due to space limitations, can be run to check results)

##Inparty, RE 1st set, RE plus controls 2nd

mod4 <- lmer(n_rows_left ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = df_final) 
mod5 <- lmer(n_rows_left ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod6 <- lmer(n_rows_left ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

mod7 <- lmer(n_rows_left ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = df_final) 
mod8 <- lmer(n_rows_left ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod9 <- lmer(n_rows_left ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

stargazer(mod4,mod5,mod6,mod7,mod8,mod9)


mod4 <- lmer(subseq_part ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = df_final_non) 
mod5 <- lmer(subseq_part ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = filter(df_final_non, affil_party == "dem")) 
mod6 <- lmer(subseq_part ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = filter(df_final_non, affil_party == "rep")) 

mod7 <- lmer(subseq_part ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = df_final_non) 
mod8 <- lmer(subseq_part ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final_non, affil_party == "dem")) 
mod9 <- lmer(subseq_part ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final_non, affil_party == "rep")) 

stargazer(mod4,mod5,mod6,mod7,mod8,mod9)


mod4 <- lmer(subseq_pol ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = df_final) 
mod5 <- lmer(subseq_pol ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod6 <- lmer(subseq_pol ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

mod7 <- lmer(subseq_pol ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = df_final) 
mod8 <- lmer(subseq_pol ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod9 <- lmer(subseq_pol ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

stargazer(mod4,mod5,mod6,mod7,mod8,mod9)


mod4 <- lmer(subseq_neg_in ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = df_final) 
mod5 <- lmer(subseq_neg_in ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod6 <- lmer(subseq_neg_in ~ cum_neg_in + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

mod7 <- lmer(subseq_neg_in ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = df_final) 
mod8 <- lmer(subseq_neg_in ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod9 <- lmer(subseq_neg_in ~ cum_neg_in + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

stargazer(mod4,mod5,mod6,mod7,mod8,mod9)


###

##Outparty, RE 1st set, RE plus controls 2nd
mod4 <- lmer(n_rows_left ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = df_final) 
mod5 <- lmer(n_rows_left ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod6 <- lmer(n_rows_left ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

mod7 <- lmer(n_rows_left ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = df_final) 
mod8 <- lmer(n_rows_left ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod9 <- lmer(n_rows_left ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

stargazer(mod4,mod5,mod6,mod7,mod8,mod9)


mod4 <- lmer(subseq_part ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = df_final_non) 
mod5 <- lmer(subseq_part ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = filter(df_final_non, affil_party == "dem")) 
mod6 <- lmer(subseq_part ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = filter(df_final_non, affil_party == "rep")) 

mod7 <- lmer(subseq_part ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = df_final_non) 
mod8 <- lmer(subseq_part ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final_non, affil_party == "dem")) 
mod9 <- lmer(subseq_part ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final_non, affil_party == "rep")) 

stargazer(mod4,mod5,mod6,mod7,mod8,mod9)


mod4 <- lmer(subseq_pol ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = df_final) 
mod5 <- lmer(subseq_pol ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod6 <- lmer(subseq_pol ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

mod7 <- lmer(subseq_pol ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = df_final) 
mod8 <- lmer(subseq_pol ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod9 <- lmer(subseq_pol ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

stargazer(mod4,mod5,mod6,mod7,mod8,mod9)


mod4 <- lmer(subseq_neg_out ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = df_final) 
mod5 <- lmer(subseq_neg_out ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod6 <- lmer(subseq_neg_out ~ cum_neg_out + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

mod7 <- lmer(subseq_neg_out ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = df_final) 
mod8 <- lmer(subseq_neg_out ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "dem")) 
mod9 <- lmer(subseq_neg_out ~ cum_neg_out + age + male + white + edu + ideology + FOLLOW + (1 | personrow) + (1 | session_day), data = filter(df_final, affil_party == "rep")) 

stargazer(mod4,mod5,mod6,mod7,mod8,mod9)

  