############################################################
# Lajevardi                                                #
# Journal: The Journal of Politics                         #
# R Replication Code                                       #
# Tested on R version 3.6                                  #
############################################################



rm(list=ls())
setwd("/Users/nazita/Dropbox/Media Matters/Media Matters Paper/Replication files/")
getwd()
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages<-c("quanteda","lubridate","topicmodels","xtable","here", "stargazer", "broom", "data.table","tidyverse","janitor","foreign","stm")
check.packages(packages)


##############
# Table B2  #
##############

# Muslim Correlations

df <- fread("FULL_CODED_FOR_REPLICATION.csv")

df %>%
  filter(race == "Muslim") -> df_Muslim

# sd_score_muslim_whole = sentiment 1 = HL whole score
# sd_handscore_muslim = score1 is nuances human seg
# sd_handscore_computer_muslim = score1_b is computer human seg
# sd_score_muslim = partial_score_HL is HL sent on seg 1
# sd_score_muslim_2 = partial_score_HL2 is HL sent on seg 2

df_Muslim %>%
  select(sentiment1,score1,score1_b,partial_score_HL,partial_score_HL2,year) %>%
  mutate(sd_score_muslim = (partial_score_HL-mean(partial_score_HL))/sd(partial_score_HL,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_2 = (partial_score_HL2-mean(partial_score_HL2))/sd(partial_score_HL2,na.rm = TRUE)) %>%
  mutate(sd_handscore_muslim = (score1-mean(score1))/sd(score1,na.rm = TRUE)) %>%
  mutate(sd_handscore_computer_muslim = (score1_b-mean(score1_b))/sd(score1_b,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_whole = (sentiment1-mean(sentiment1))/sd(sentiment1,na.rm = TRUE)) -> df

df %>%
  select(sd_score_muslim,sd_score_muslim_2,sd_handscore_muslim,sd_handscore_computer_muslim,sd_score_muslim_whole,year) %>%
  group_by(year) %>%
  summarise(sd_score_muslim = mean(sd_score_muslim,na.rm = TRUE),sd_score_muslim_2 = mean(sd_score_muslim_2,na.rm = TRUE), sd_score_muslim_whole = mean(sd_score_muslim_whole,na.rm = TRUE), sd_handscore_muslim =  mean(sd_handscore_muslim, na.rm = TRUE), sd_handscore_computer_muslim =  mean(sd_handscore_computer_muslim, na.rm = TRUE)) -> years

dplyr::summarize(years, 
                 "HCN & SSSS" = cor(sd_score_muslim,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & SSSS" = cor(sd_score_muslim,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"),
                 "Corr Repeated Words" = cor(sd_score_muslim,sd_score_muslim_2,method = "pearson", use = "complete.obs"),
                 "HCN & ESSS" = cor(sd_score_muslim_whole,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & ESSS" = cor(sd_score_muslim_whole,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"))



# Muslim American Correlation
df <- fread("FULL_CODED_FOR_REPLICATION.csv")

df %>%
  filter(race == "Muslim American") -> df_MuslimAm

df_MuslimAm %>%
  select(sentiment1,score1,score1_b,partial_score_HL,partial_score_HL2,year) %>%
  mutate(sd_score_muslim = (partial_score_HL-mean(partial_score_HL))/sd(partial_score_HL,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_2 = (partial_score_HL2-mean(partial_score_HL2))/sd(partial_score_HL2,na.rm = TRUE)) %>%
  mutate(sd_handscore_muslim = (score1-mean(score1))/sd(score1,na.rm = TRUE)) %>%
  mutate(sd_handscore_computer_muslim = (score1_b-mean(score1_b))/sd(score1_b,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_whole = (sentiment1-mean(sentiment1))/sd(sentiment1,na.rm = TRUE)) -> df

df %>%
  select(sd_score_muslim,sd_score_muslim_2,sd_handscore_muslim,sd_handscore_computer_muslim,sd_score_muslim_whole,year) %>%
  group_by(year) %>%
  summarise(sd_score_muslim = mean(sd_score_muslim,na.rm = TRUE),sd_score_muslim_2 = mean(sd_score_muslim_2,na.rm = TRUE), sd_score_muslim_whole = mean(sd_score_muslim_whole,na.rm = TRUE), sd_handscore_muslim =  mean(sd_handscore_muslim, na.rm = TRUE), sd_handscore_computer_muslim =  mean(sd_handscore_computer_muslim, na.rm = TRUE)) -> years

dplyr::summarize(years, 
                 "HCN & SSSS" = cor(sd_score_muslim,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & SSSS" = cor(sd_score_muslim,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"),
                 "Corr Repeated Words" = cor(sd_score_muslim,sd_score_muslim_2,method = "pearson", use = "complete.obs"),
                 "HCN & ESSS" = cor(sd_score_muslim_whole,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & ESSS" = cor(sd_score_muslim_whole,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"))


# Black Correlation

df <- fread("FULL_CODED_FOR_REPLICATION.csv")

df %>%
  filter(race == "Black") -> df_Black

df_Black %>%
  select(sentiment1,score1,score1_b,partial_score_HL,partial_score_HL2,year) %>%
  mutate(sd_score_muslim = (partial_score_HL-mean(partial_score_HL))/sd(partial_score_HL,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_2 = (partial_score_HL2-mean(partial_score_HL2))/sd(partial_score_HL2,na.rm = TRUE)) %>%
  mutate(sd_handscore_muslim = (score1-mean(score1))/sd(score1,na.rm = TRUE)) %>%
  mutate(sd_handscore_computer_muslim = (score1_b-mean(score1_b))/sd(score1_b,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_whole = (sentiment1-mean(sentiment1))/sd(sentiment1,na.rm = TRUE)) -> df

df %>%
  select(sd_score_muslim,sd_score_muslim_2,sd_handscore_muslim,sd_handscore_computer_muslim,sd_score_muslim_whole,year) %>%
  group_by(year) %>%
  summarise(sd_score_muslim = mean(sd_score_muslim,na.rm = TRUE),sd_score_muslim_2 = mean(sd_score_muslim_2,na.rm = TRUE), sd_score_muslim_whole = mean(sd_score_muslim_whole,na.rm = TRUE), sd_handscore_muslim =  mean(sd_handscore_muslim, na.rm = TRUE), sd_handscore_computer_muslim =  mean(sd_handscore_computer_muslim, na.rm = TRUE)) -> years

dplyr::summarize(years, 
                 "HCN & SSSS" = cor(sd_score_muslim,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & SSSS" = cor(sd_score_muslim,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"),
                 "Corr Repeated Words" = cor(sd_score_muslim,sd_score_muslim_2,method = "pearson", use = "complete.obs"),
                 "HCN & ESSS" = cor(sd_score_muslim_whole,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & ESSS" = cor(sd_score_muslim_whole,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"))


# Latino Correlation
df <- fread("FULL_CODED_FOR_REPLICATION.csv")

df %>%
  filter(race == "Latino") -> df_Latino

df_Latino %>%
  select(sentiment1,score1,score1_b,partial_score_HL,partial_score_HL2,year) %>%
  mutate(sd_score_muslim = (partial_score_HL-mean(partial_score_HL))/sd(partial_score_HL,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_2 = (partial_score_HL2-mean(partial_score_HL2))/sd(partial_score_HL2,na.rm = TRUE)) %>%
  mutate(sd_handscore_muslim = (score1-mean(score1))/sd(score1,na.rm = TRUE)) %>%
  mutate(sd_handscore_computer_muslim = (score1_b-mean(score1_b))/sd(score1_b,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_whole = (sentiment1-mean(sentiment1))/sd(sentiment1,na.rm = TRUE)) -> df

df %>%
  select(sd_score_muslim,sd_score_muslim_2,sd_handscore_muslim,sd_handscore_computer_muslim,sd_score_muslim_whole,year) %>%
  group_by(year) %>%
  summarise(sd_score_muslim = mean(sd_score_muslim,na.rm = TRUE),sd_score_muslim_2 = mean(sd_score_muslim_2,na.rm = TRUE), sd_score_muslim_whole = mean(sd_score_muslim_whole,na.rm = TRUE), sd_handscore_muslim =  mean(sd_handscore_muslim, na.rm = TRUE), sd_handscore_computer_muslim =  mean(sd_handscore_computer_muslim, na.rm = TRUE)) -> years

dplyr::summarize(years, 
                 "HCN & SSSS" = cor(sd_score_muslim,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & SSSS" = cor(sd_score_muslim,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"),
                 "Corr Repeated Words" = cor(sd_score_muslim,sd_score_muslim_2,method = "pearson", use = "complete.obs"),
                 "HCN & ESSS" = cor(sd_score_muslim_whole,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & ESSS" = cor(sd_score_muslim_whole,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"))

# Asian Correlation
df <- fread("FULL_CODED_FOR_REPLICATION.csv")

df %>%
  filter(race == "Asian American") -> df_AsianAm

df_AsianAm %>%
  select(sentiment1,score1,score1_b,partial_score_HL,partial_score_HL2,year) %>%
  mutate(sd_score_muslim = (partial_score_HL-mean(partial_score_HL))/sd(partial_score_HL,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_2 = (partial_score_HL2-mean(partial_score_HL2))/sd(partial_score_HL2,na.rm = TRUE)) %>%
  mutate(sd_handscore_muslim = (score1-mean(score1))/sd(score1,na.rm = TRUE)) %>%
  mutate(sd_handscore_computer_muslim = (score1_b-mean(score1_b))/sd(score1_b,na.rm = TRUE)) %>%
  mutate(sd_score_muslim_whole = (sentiment1-mean(sentiment1))/sd(sentiment1,na.rm = TRUE)) -> df

df %>%
  select(sd_score_muslim,sd_score_muslim_2,sd_handscore_muslim,sd_handscore_computer_muslim,sd_score_muslim_whole,year) %>%
  group_by(year) %>%
  summarise(sd_score_muslim = mean(sd_score_muslim,na.rm = TRUE),sd_score_muslim_2 = mean(sd_score_muslim_2,na.rm = TRUE), sd_score_muslim_whole = mean(sd_score_muslim_whole,na.rm = TRUE), sd_handscore_muslim =  mean(sd_handscore_muslim, na.rm = TRUE), sd_handscore_computer_muslim =  mean(sd_handscore_computer_muslim, na.rm = TRUE)) -> years

dplyr::summarize(years, 
                 "HCN & SSSS" = cor(sd_score_muslim,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & SSSS" = cor(sd_score_muslim,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"),
                 "Corr Repeated Words" = cor(sd_score_muslim,sd_score_muslim_2,method = "pearson", use = "complete.obs"),
                 "HCN & ESSS" = cor(sd_score_muslim_whole,sd_handscore_muslim,method = "pearson", use = "complete.obs"),
                 "HCD & ESSS" = cor(sd_score_muslim_whole,sd_handscore_computer_muslim,method = "pearson", use = "complete.obs"))



############################
# Table D2                 #
############################

# Muslim Corpus in CNN broadcasts ONLY:
load("Muslim_CNN_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D2_Muslim_CNN__table.tex")
gc()


############################
# Table D3                 #
############################
# Muslim-American Corpus in CNN broadcasts
load("MAM_CNN_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D3_MuslimAm_CNN_table.tex")
gc()

############################
# Table D4                 #
############################
# Muslim Corpus in All Three News Outlets

load("Muslim_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D4_Muslim_table.tex")
gc()


############################
# Table D5                 #
############################
# Muslim-American Corpus in All Three News Outlets

load("MAM_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D5_MAM_table.tex")
gc()

############################
# Table D6                 #
############################
# Black Corpus in All Three News Outlets

load("Black_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D6_Black_table.tex")
gc()

############################
# Table D7                 #
############################
# Latino Corpus in All Three News Outlets

load("Latino_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D7_Latino_table.tex")
gc()

############################
# Table D8                 #
############################
# Asian-American Corpus in All Three News Outlets

load("AsianAm_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D8_AsianAm_table.tex")
gc()

############################
# Table D9                 #
############################
# Alt-Black Corpus in All Three News Outlets


load("Alt_Black_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D9_AltBlack_table.tex")
gc()

############################
# Table D10                #
############################
# Alt-Latino Corpus in All Three News Outlets

load("Alt_Latino_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D10_AltLatino_table.tex")
gc()

############################
# Table D11                #
############################
# Alt Asian-American Corpus in All Three News Outlets

load("Alt_Asian_stm.out.Rdata")

labelTopics(stm.out)
test<-labelTopics(stm.out)
list<-as.data.frame(test$prob)
list$Probability<-colMeans(stm.out$theta)
list
print(xtable(list, type = "latex"), file = "D11_AltAsian_table.tex")
gc()



