############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code imports and cleans the data file    #
#       for Study 2                             #
#       from 2019 June DK YouGov survey.        #
#                                               #
# 2019.07.25.                                   #
# code by Alexander Bor                         #
############################################### #


# load packages

library(tidyverse)
library(rio)

source("C.code/functions.R")


# import data

rawdf1 <- rio::import("A.original.data/DK_2019.sav")


# rename variables ###########################################################
names(rawdf1) <- gsub("Q1_1",  "part_online1", names(rawdf1))
names(rawdf1) <- gsub("Q1_2",  "part_online2", names(rawdf1))
names(rawdf1) <- gsub("Q1_3",  "part_offline1", names(rawdf1))
names(rawdf1) <- gsub("Q1_4",  "part_offline2", names(rawdf1))
names(rawdf1) <- gsub("Q2_",  "tone_online", names(rawdf1))
names(rawdf1) <- gsub("Q3_",  "offended_online", names(rawdf1))
names(rawdf1) <- gsub("Q4_",  "conflict_online", names(rawdf1))
names(rawdf1) <- gsub("Q5_",  "talk_online", names(rawdf1))
names(rawdf1) <- gsub("Q6_",  "hostile_online", names(rawdf1))

names(rawdf1) <- gsub("Q8_1",  "ideology", names(rawdf1))
names(rawdf1) <- gsub("Q9_",  "cynicism", names(rawdf1))
names(rawdf1) <- gsub("Q10_1", "time_online", names(rawdf1))
names(rawdf1) <- gsub("Q10_2", "time_socialmedia", names(rawdf1))

names(rawdf1) <- gsub("q11_",  "tone_offline", names(rawdf1))
names(rawdf1) <- gsub("Q12_",  "offended_offline", names(rawdf1))
names(rawdf1) <- gsub("Q13_",  "conflict_offline", names(rawdf1))
names(rawdf1) <- gsub("Q14_",  "talk_offline", names(rawdf1))
names(rawdf1) <- gsub("Q15_",  "hostile_offline", names(rawdf1))

names(rawdf1) <- gsub("Q16_", "f2f", names(rawdf1))
names(rawdf1) <- gsub("Q17_", "cfc", names(rawdf1))
names(rawdf1) <- gsub("Q18_", "empathy", names(rawdf1))
names(rawdf1) <- gsub("Q19_", "aggression", names(rawdf1))
names(rawdf1) <- gsub("Q20_", "sdrt", names(rawdf1))
names(rawdf1) <- gsub("Q21_",  "ders", names(rawdf1))

names(rawdf1) <- gsub("RecordNo", "ID", names(rawdf1))

names(rawdf1) <- gsub("FT_next", "pid", names(rawdf1))



# Reverse reverse-coded items. #####################################################

df1 <- rawdf1 %>% 
        mutate(conflict_online6 = flip(conflict_online6),
               conflict_online7 = flip(conflict_online7),
               conflict_online8 = flip(conflict_online8),
               
               conflict_offline6 = flip(conflict_offline6),
               conflict_offline7 = flip(conflict_offline7),
               conflict_offline8 = flip(conflict_offline8),
              
               cynicism3 = flip(cynicism3), 
               cynicism4 = flip(cynicism4), 
               cynicism5 = flip(cynicism5), 
               cynicism6 = flip(cynicism6), 
               
               f2f3 = flip(f2f3),
               f2f4 = flip(f2f4),
               
               empathy2 = flip(empathy2),
               empathy8 = flip(empathy8),
               empathy9 = flip(empathy9), 
               
               aggression2 = flip(aggression2), 
               
               sdrt1 = flip(sdrt1), 
               sdrt7 = flip(sdrt7), 
               sdrt8 = flip(sdrt8), 
               sdrt12 = flip(sdrt12), 
               sdrt14 = flip(sdrt14),
               
               screenout_online = case_when(part_online1 >= 2 & part_online1 <= 7 ~ 0,
                                            part_online2 >= 2 & part_online2 <= 7 ~ 0,
                                            TRUE ~ 1),
               screenout_offline = case_when(part_offline1 >= 2 & part_offline1 <= 7 ~ 0,
                                             part_offline2 >= 2 & part_offline2 <= 7 ~ 0,
                                             TRUE ~ 1),
               screenout = ifelse(screenout_online == 1 | screenout_offline == 1, 1, 0)
               ) %>% 
        filter(screenout != 1)


# alpha reliability and aggregation  #####################################################

df1$part_online <- row.means(df1[, c("part_online1", "part_online2")])
df1$part_offline <- row.means(df1[, c("part_offline1", "part_offline2")])

# talking about politics offline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talk_offline <- paste0("talk_offline", 1:5)
df1$talk_offline <- row.means(df1[, talk_offline])

# talking about politics online ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talk_online <- paste0("talk_online", 1:5)
df1$talk_online <- row.means(df1[, talk_online])


# tone offline positive~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tone_offline_pos <- paste0("tone_offline", 1:3)

df1$tone_offline_pos <- row.means(df1[ , tone_offline_pos])

# tone offline negative~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tone_offline_neg <- paste0("tone_offline", 4:6)
df1$tone_offline_neg <- row.means(df1[, tone_offline_neg])

# tone online positive~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tone_online_pos <- paste0("tone_online", 1:3)
df1$tone_online_pos <- row.means((df1[, tone_online_pos]))

# tone online negative~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tone_online_neg <- paste0("tone_online", 4:6)
df1$tone_online_neg <- row.means(df1[ , tone_online_neg])



# offended online~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
offended_online <- paste0("offended_online", 1:3)

df1$offended_online <- row.means(df1[ , offended_online])

# offended offline~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

offended_offline <- paste0("offended_offline", 1:3)
df1$offended_offline <- row.means(df1[ , offended_offline])

# conflict online~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
conflict_all_online <- paste0("conflict_online", 1:8)

df1$conflict_all_online <- row.means(df1[, conflict_all_online])

# df1$conflict_sever_online <- row.means(df1[, c("conflict_online1", "conflict_online2",
#                                                "conflict_online3", "conflict_online4",
#                                                "conflict_online5")])
# 
# df1$conflict_resolv_online <- row.means(df1[, c("conflict_online6", "conflict_online7",
#                                                 "conflict_online8")])

# conflict offline~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
conflict_all_offline <-  paste0("conflict_offline", 1:8)
df1$conflict_all_offline <- row.means(df1[,conflict_all_offline ])

# df1$conflict_sever_offline <- row.means(df1[, c("conflict_offline1", "conflict_offline2",
#                                                 "conflict_offline3", "conflict_offline4",
#                                                 "conflict_offline5")])
# 
# df1$conflict_resolv_offline <- row.means(df1[, c("conflict_offline6", "conflict_offline7",
#                                                  "conflict_offline8")])


# hostility online~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hostile_online <- c("hostile_online1", "hostile_online2", "hostile_online3", 
                    "hostile_online4", "hostile_online5", "hostile_online6")
df1$hostile_online <- row.means(df1[ , hostile_online])

# # this variable allows to directly combine online and offline hostility
df1$hostile_online_short <- row.means(df1[ , c("hostile_online3",
                                        "hostile_online4", "hostile_online5")])


# hostility offline~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hostile_offline <-  c("hostile_offline1", "hostile_offline2",
                      "hostile_offline3", "hostile_offline4")
df1$hostile_offline <- row.means(df1[,hostile_offline])


# cynicism~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$cynicism <- row.means(df1[, c("cynicism1", "cynicism2", "cynicism3", "cynicism4",
                                 "cynicism5", "cynicism6", "cynicism7", "cynicism8")])

df1$cynicism.short <- row.means(df1[, c("cynicism3", "cynicism4",
                                        "cynicism5", "cynicism6")])


# preference for face-to-face (f2f) discussions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df1$f2f <- row.means(df1[ , c("f2f1", "f2f2", "f2f3", "f2f4", "f2f5", "f2f6")])


# craving for chaos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df1$cfc <- row.means((df1[, c("cfc1", "cfc2", "cfc3", "cfc4",
                             "cfc5", "cfc6", "cfc7" , "cfc8")]))


# cognitive empathy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df1$empathy <- row.means(df1[, c("empathy1", "empathy2", "empathy3", "empathy4",
                                "empathy5", "empathy6", "empathy7" , "empathy8", 
                                "empathy9")])

# trait aggression ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aggression <- paste0("aggression", 1:12)
df1$aggression <- row.means(df1[,aggression])


# status driven risk seeking~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdrt <- paste0("sdrt", 1:14)
df1$sdrt <- row.means(df1[, sdrt])

# Difficulties in emotion regulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ders <- paste0("ders", 1:12)
df1$ders <- row.means(df1[, ders])



# Party ID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$pid <- case_when(df1$pid == 1 ~ "Red_block",
                     df1$pid == 2 ~ "Red_block", 
                     df1$pid == 4 ~ "Red_block", 
                     df1$pid == 9 ~ "Red_block", 
                     df1$pid == 3 ~ "Blue_block", 
                     df1$pid == 5 ~ "Blue_block", 
                     df1$pid == 7 ~ "Blue_block", 
                     df1$pid == 8 ~ "Blue_block", 
                     df1$pid == 6 ~ "Other", 
                     df1$pid == 16 ~ "Other", 
                     df1$pid == 17 ~ "Other", 
                     df1$pid == 18 ~ "Other", 
                     df1$pid == 19 ~ "Other", 
                     df1$pid == 10 ~ "Other", 
                     TRUE ~ NA_character_)

# Age ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df1$age <- dplyr::recode(as.character(df1$profile_age2), 
                         `1` = 20L,  `2` = 25L, `3` = 30L, `4` = 35L, 
                         `5` = 40L,  `6` = 45L, `7` = 50L, `8` = 55L, 
                         `9` = 61L)

# create a list of variables with 1-7 where 8 = NA
vars18 <- c(offended_offline, offended_online, talk_offline, talk_online, 
            hostile_offline, hostile_online, tone_offline_neg, tone_offline_pos, 
            tone_online_neg, tone_online_pos, conflict_all_offline, 
            conflict_all_online, aggression, sdrt, ders)

# create a list of variables with 1-7 scale. 
vars17 <- c("part_offline", "part_online", "talk_offline", "talk_online", 
            "tone_offline_neg", "tone_online_neg", 
            "tone_offline_pos", "tone_online_pos",  
            "offended_offline", "offended_online", 
            #"conflict_sever_offline", "conflict_sever_online" , 
            #"conflict_resolv_offline", "conflict_resolv_online", 
            "conflict_all_online", "conflict_all_offline",
            "hostile_online", "hostile_offline",
            "cynicism", "f2f", "cfc", "empathy", "aggression", "sdrt", "ders")

df1[, vars17] <- df1[, vars17] %>% 
        mutate_all(funs(zero.one))

df1[, vars18] <- df1[, vars18] %>% 
        mutate_all(funs(zero.one(na_if(., 8))))

df1 <- df1 %>% mutate(
               female = gender -1, 
               highered = na_if(profile_education, 9),
               highered = ifelse(highered >= 5, 1, 0), 
               income = zero.one(na_if(household_income, 13), max = 12), 
               ideology = zero.one(ideology, min = 0, max = 10), 
               age_year = age,
               age = zero.one(age, min = 20, max = 61),
               Party_red = ifelse(pid == "Red_block", 1, 0), 
               Party_other = ifelse(pid == "Other", 1, 0))

summary(df1[, vars17])
summary(df1[, vars18])

# save file for supplementary analyses.
saveRDS(df1, file = "B.analysis.data/s2_dk_2019_full.rds")

# Save analysis data ########################################

depvars <- c("hostile_offline", hostile_offline, "hostile_online", hostile_online,
             "talk_offline", talk_offline, "talk_online", talk_online,
             tone_online_neg, tone_online_pos, "tone_online_neg", "tone_online_pos", 
             tone_offline_neg, tone_offline_pos,"tone_offline_neg", "tone_offline_pos", 
             #"conflict_resolv_offline", "conflict_resolv_online", 
             #"conflict_sever_offline", "conflict_sever_online",
             "conflict_all_online", "conflict_all_offline",
             conflict_all_online, conflict_all_offline, 
             offended_offline, offended_online,
             "part_online", "part_offline")

psychvars <- c("ders", "sdrt", "aggression", ders, sdrt, aggression)

controlvars <- c("female", "age", "age_year", "highered", "income", "pid", "Party_red", 
                 "Party_other")

df.dk <- df1 %>% 
        dplyr::select(depvars, psychvars, controlvars, pid) %>% 
        glimpse

apply(df.dk, 2, min, na.rm = T)
apply(df.dk, 2, max, na.rm = T)

saveRDS(df.dk, file = "B.analysis.data/s2_dk_2019.rds")
write.csv(df.dk, file = "B.analysis.data/s2_dk_2019.csv")

rm(list = ls())