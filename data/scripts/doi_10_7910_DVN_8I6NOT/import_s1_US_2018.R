############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code imports and cleans the data file    #
#       for Study 1                             #
#       from 2018 Nov US YouGov survey.         #
#                                               #
# 2018.08.27.                                   #
# code by Alexander Bor                         #
############################################### #


# load packages

library(rio)
library(psych)
library(tidyverse)
source("C.code/functions.R")


# import data

rawdf1 <- rio::import("A.original.data/US_2018.sav")


# rename variables ###########################################################
names(rawdf1) <- gsub("Q1_",  "part_offline", names(rawdf1))
names(rawdf1) <- gsub("Q2_",  "part_online", names(rawdf1))
names(rawdf1) <- gsub("Q3_",  "talk_offline", names(rawdf1))
names(rawdf1) <- gsub("Q4_",  "talk_online", names(rawdf1))
names(rawdf1) <- gsub("Q5_",  "express_offline", names(rawdf1))
names(rawdf1) <- gsub("Q6_",  "express_online", names(rawdf1))
names(rawdf1) <- gsub("Q7_",  "tone_offline", names(rawdf1))
names(rawdf1) <- gsub("Q8_",  "tone_online", names(rawdf1))
names(rawdf1) <- gsub("Q9a",  "timeonline", names(rawdf1))
names(rawdf1) <- gsub("Q10_", "hostile_online", names(rawdf1))
names(rawdf1) <- gsub("Q11_", "hostile_offline", names(rawdf1))
names(rawdf1) <- gsub("Q12_", "troll", names(rawdf1))
names(rawdf1) <- gsub("Q13_", "reason4hostility", names(rawdf1))
names(rawdf1) <- gsub("Q14_", "efficacy", names(rawdf1))
names(rawdf1) <- gsub("Q15_", "cynicism", names(rawdf1))
names(rawdf1) <- gsub("Q16",  "ideology", names(rawdf1))
names(rawdf1) <- gsub("Q17",  "pid3", names(rawdf1))
names(rawdf1) <- gsub("Q18",  "pid_rep", names(rawdf1))
names(rawdf1) <- gsub("Q19",  "pid_dem", names(rawdf1))
names(rawdf1) <- gsub("Q20",  "pid_ind", names(rawdf1))
names(rawdf1) <- gsub("Q21",  "know1", names(rawdf1))
names(rawdf1) <- gsub("Q22",  "know2", names(rawdf1))
names(rawdf1) <- gsub("Q23",  "know3", names(rawdf1))
names(rawdf1) <- gsub("Q24",  "know4", names(rawdf1))
names(rawdf1) <- gsub("Q25",  "know5", names(rawdf1))
names(rawdf1) <- gsub("Q26_", "hexaco", names(rawdf1))
names(rawdf1) <- gsub("Q27_", "ders", names(rawdf1))
names(rawdf1) <- gsub("Q28_", "aggression", names(rawdf1))
names(rawdf1) <- gsub("Q29_", "nfc", names(rawdf1))
names(rawdf1) <- gsub("Q30_", "loneliness", names(rawdf1))
names(rawdf1) <- gsub("Q31_", "empathy", names(rawdf1))
names(rawdf1) <- gsub("Q32_", "sdrt", names(rawdf1))
names(rawdf1) <- gsub("timeonline_1", "timeonline", names(rawdf1))
names(rawdf1) <- gsub("timeonline_2", "timeonsocialmedia", names(rawdf1))
names(rawdf1) <- gsub("RecordNo", "ID", names(rawdf1))


# Reverse reverse-coded items. #####################################################

df1 <- rawdf1 %>% 
        mutate(efficacy1 = flip(efficacy1),
               efficacy3 = flip(efficacy3),
               efficacy4 = flip(efficacy4),
               
               cynicism3 = flip(cynicism3), 
               cynicism4 = flip(cynicism4), 
               cynicism5 = flip(cynicism5), 
               cynicism6 = flip(cynicism6), 
               
               hexaco12 = flip(hexaco12), 
               hexaco18 = flip(hexaco18), 
               hexaco24 = flip(hexaco24), 
               hexaco11 = flip(hexaco11), 
               hexaco17 = flip(hexaco17), 
               hexaco4 = flip(hexaco4), 
               hexaco22 = flip(hexaco22), 
               hexaco3 = flip(hexaco3), 
               hexaco9 = flip(hexaco9), 
               hexaco8 = flip(hexaco8), 
               hexaco20 = flip(hexaco20), 
               hexaco7 = flip(hexaco7), 
               
               ders7  = flip(ders7),
               ders8  = flip(ders8), 
               
               aggression2 = flip(aggression2), 
               
               loneliness5 = flip(loneliness5), 
               
               empathy2 = flip(empathy2),
               empathy8 = flip(empathy8),
               empathy9 = flip(empathy9), 
               
               sdrt1 = flip(sdrt1), 
               sdrt7 = flip(sdrt7), 
               sdrt8 = flip(sdrt8), 
               sdrt12 = flip(sdrt12), 
               sdrt14 = flip(sdrt14)
               )


# scale aggregation  #####################################################



# offline participation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$part_offline <- rowMeans(df1[ , c("part_offline1", "part_offline2", "part_offline3", 
                                      "part_offline4","part_offline5", "part_offline6", 
                                      "part_offline7")])


# online participation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$part_online <- rowMeans(df1[ , c("part_online1", "part_online2", "part_online3", 
                                     "part_online4","part_online5", "part_online6")])

# talking about politics offline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talk_offline <- c("talk_offline1", "talk_offline2", 
                     "talk_offline3", "talk_offline4", 
                     "talk_offline5")

df1$talk_offline <- rowMeans(df1[, c(talk_offline)])

# talking about politics online ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talk_online <- c("talk_online1", "talk_online2", 
                    "talk_online3", "talk_online4", 
                    "talk_online5")

df1$talk_online <- rowMeans(df1[, c(talk_online)])


# expressive politics offline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$express_offline <- rowMeans(df1[, c("express_offline1", "express_offline2", 
                                   "express_offline3", "express_offline4")])


# expressive politics online~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$express_online <- rowMeans(df1[ , c("express_online1", "express_online2", 
                                  "express_online3", "express_online4")])

# tone offline positive~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tone_offline_pos <- paste0("tone_offline", 1:3)

df1$tone_offline_pos <- rowMeans(df1[ , tone_offline_pos])

# tone offline negative~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tone_offline_neg <- paste0("tone_offline", 4:6)
df1$tone_offline_neg <- rowMeans(df1[, tone_offline_neg])

# tone online positive~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tone_online_pos <- paste0("tone_online", 1:3)
df1$tone_online_pos <- rowMeans((df1[, tone_online_pos]))

# tone online negative~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tone_online_neg <- paste0("tone_online", 4:6)
df1$tone_online_neg <- rowMeans(df1[ , tone_online_neg])


# hostility online~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hostile_online <- paste0("hostile_online", 1:5)
df1$hostile_online <- rowMeans(df1[, hostile_online])

# this variable allows to directly combine online and offline hostility
df1$hostile_online_short <- rowMeans(df1[ , c("hostile_online3", 
                                        "hostile_online4", "hostile_online5")])


# hostility offline~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hostile_offline <- paste0("hostile_offline", 1:3)
df1$hostile_offline <- rowMeans(df1[, hostile_offline])

# trolling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$troll <- rowMeans(df1[ , c("troll1", "troll2", 
                               "troll3", "troll4")])

# efficacy~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$efficacy <- rowMeans(df1[ , c("efficacy1", "efficacy2", "efficacy3", "efficacy4")])

 
# cynicism~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cynicism_m <- c("cynicism1", "cynicism2", "cynicism3", "cynicism4")
cynicism_c <- c("cynicism5", "cynicism6", "cynicism7", "cynicism8")
# combined cynicism 
df1$cynicism <- rowMeans(df1[, c(cynicism_m, cynicism_c)])


# HEXACO! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df1$hexaco_h <- rowMeans(df1[, c("hexaco6", "hexaco12", "hexaco18", "hexaco24")])
df1$hexaco_e <- rowMeans(df1[, c("hexaco5", "hexaco11", "hexaco17", "hexaco23")])
df1$hexaco_x <- rowMeans(df1[, c("hexaco4", "hexaco10", "hexaco16", "hexaco22")])
df1$hexaco_a <- rowMeans(df1[, c("hexaco3", "hexaco9",  "hexaco15", "hexaco21")])
df1$hexaco_c <- rowMeans(df1[, c("hexaco2", "hexaco8", "hexaco14", "hexaco20")])
df1$hexaco_o <- rowMeans(df1[, c("hexaco1", "hexaco7", "hexaco13", "hexaco19")])


# Difficulties in emotion regulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ders <- paste0("ders", 1:12)
df1$ders <- rowMeans(df1[, ders])

# trait aggression ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aggression <- paste0("aggression", 1:12)
df1$aggression <- rowMeans(df1[, aggression])

# need for chaos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$nfc <- rowMeans((df1[, c("nfc1", "nfc2", "nfc3", "nfc4",
                             "nfc5", "nfc6", "nfc7" , "nfc8")]))

# loneliness~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$loneliness <- rowMeans((df1[, c("loneliness1", "loneliness2", "loneliness3", 
                                    "loneliness4","loneliness5", "loneliness6", 
                                    "loneliness7")]))

# cognitive empathy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$empathy <- rowMeans(df1[, c("empathy1", "empathy2", "empathy3", "empathy4",
                                "empathy5", "empathy6", "empathy7" , "empathy8", 
                                "empathy9")])

# status driven risk seeking~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdrt <- paste0("sdrt", 1:14)
df1$sdrt <- rowMeans(df1[, sdrt])

# Knowledge~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# we need to count correct answers 
df1$know <- rowSums(cbind(df1$know1 == 3, df1$know2 == 4, 
                          df1$know3 == 2, df1$know4 == 2, 
                          df1$know5 == 9))


# Party ID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df1$pid <- pid(xd = df1$pid_dem, xr = df1$pid_rep, xi = df1$pid_ind)

# Age

df1$age <- dplyr::recode(as.character(df1$age_grp2), 
                         `1` = 20L,  `2` = 25L, `3` = 30L, `4` = 35L, 
                         `5` = 40L,  `6` = 45L, `7` = 50L, `8` = 55L, 
                         `9` = 61L)

# create a list of variables with 1-7 scale. 
vars17 <- c("part_offline", "part_online", "express_offline", "express_online" , 
            "tone_offline_pos", "tone_offline_neg", "tone_online_pos", 
            "tone_online_neg" , "hostile_online", "hostile_offline", "troll", 
            "efficacy" , "cynicism", "hexaco_h", "hexaco_e" , 
            "hexaco_x", "hexaco_a", "hexaco_c", "hexaco_o" , "ders", 
            "aggression", "nfc", "empathy", "sdrt", "pid", "ideology", 
            talk_online, talk_offline, "talk_online", "talk_offline")

df1[, vars17] <- df1[, vars17] %>% 
        mutate_all(funs(zero.one))

df1 <- df1 %>% 
        mutate(know = know/5, 
               loneliness = zero.one(loneliness, max = 4), 
               female = Gender -1, 
               white = ifelse(race == 1, 1, 0), 
               highered = ifelse(educ >= 4, 1, 0), 
               income = zero.one(na_if(faminc_new, 97), max = 16), 
               age_year = age,
               age = zero.one(age, min = 20, max = 61))

saveRDS(df1, file = "B.analysis.data/s1_us_2018_full.rds")

depvars <- c("hostile_offline", hostile_offline, "hostile_online", hostile_online,
             "talk_offline", talk_offline, "talk_online", talk_online,
             tone_online_neg, tone_online_pos, "tone_online_neg", "tone_online_pos", 
             tone_offline_neg, tone_offline_pos,"tone_offline_neg", "tone_offline_pos")
psychvars <- c("aggression", "ders", "sdrt",aggression, ders, sdrt)
controlvars <- c("female", "age", "age_year", "highered", "income", "pid", "white")

# myscale <- function(x){
# as.numeric(scale(x))
# }

df.us <- df1 %>% 
        dplyr::select(depvars, psychvars, controlvars) %>% 
        glimpse

# apply(df.us, 2, range, na.rm =T)

# Save analysis data ########################################

saveRDS(df.us, file = "B.analysis.data/s1_us_2018.rds")
write.csv(df.us, file = "B.analysis.data/s1_us_2018.csv")

rm(list = ls())
