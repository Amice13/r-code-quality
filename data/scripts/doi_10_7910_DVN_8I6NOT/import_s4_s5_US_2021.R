############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code imports and cleans the data file    #
#       for Studies 4 - 5                       #
#       from 2021 March US YouGov survey.       #
#                                               #
# 2021.03.05.                                   #
# code by Alexander Bor                         #
############################################### #


# load packages
pacman::p_load("rio", "psych", "tidyverse", "here")
source("C.code/functions.R")


# import data

rawdf6 <- rio::import("A.original.data/US_2021.sav")


# rename variables ###########################################################
names(rawdf6) <- gsub("q1_1",  "part_offline", names(rawdf6))
names(rawdf6) <- gsub("q1_2",  "part_online", names(rawdf6))
names(rawdf6) <- gsub("q2_screen",  "offensive_", names(rawdf6))
names(rawdf6) <- gsub("q3_screen",  "inappropriate_", names(rawdf6))
names(rawdf6) <- gsub("q4_screen",  "rare_", names(rawdf6))
names(rawdf6) <- gsub("q5_",  "hostile_online_", names(rawdf6))
names(rawdf6) <- gsub("q6_",  "talk_online_", names(rawdf6))
names(rawdf6) <- gsub("q7",  "decaf", names(rawdf6))
names(rawdf6) <- gsub("q8_",  "sdrt", names(rawdf6))
names(rawdf6) <- gsub("q9_",  "aggression", names(rawdf6))
names(rawdf6) <- gsub("q10_",  "ders", names(rawdf6))

names(rawdf6) <- gsub("q11_",  "hostile_offline_", names(rawdf6))
names(rawdf6) <- gsub("q12_",  "talk_offline_", names(rawdf6))

names(rawdf6) <- gsub("q13",  "pid3", names(rawdf6))
names(rawdf6) <- gsub("q14a",  "pid_rep", names(rawdf6))
names(rawdf6) <- gsub("q14b",  "pid_dem", names(rawdf6))
names(rawdf6) <- gsub("q14c",  "pid_ind", names(rawdf6))

rawdf6$ID <- as.factor(1:nrow(rawdf6))


# Reverse reverse-coded items. #####################################################

df6 <- rawdf6 %>% 
        mutate(ders7  = flip(ders7),
               ders8  = flip(ders8), 
               
               aggression2 = flip(aggression2), 
               
               sdrt1 = flip(sdrt1), 
               sdrt7 = flip(sdrt7), 
               sdrt8 = flip(sdrt8), 
               sdrt12 = flip(sdrt12), 
               sdrt14 = flip(sdrt14), 
               
               attention = grepl("deca", decaf, ignore.case = T), 
               
               online_first = ifelse(substr(module_order_online_offline, 2, 2) == 0, "1", "0")
        ) %>% 
        dplyr::select(-caseid, -q15, -comments:-weight ) %>% 
        # dplyr::select(-identity:-Experiement_layout_4, -contains("q_chosen_context_message_all_"), qage) %>% 
        glimpse


# Scale aggregation  #####################################################
# talking about politics offline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talk_offline <- paste0("talk_offline_", 1:5)
df6$talk_offline <- row.means(df6[, talk_offline])

# talking about politics online ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talk_online <- paste0("talk_online_", 1:5)
df6$talk_online <- row.means(df6[, talk_online])

# being hostile offline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hostile_offline <- paste0("hostile_offline_", 1:8)
df6$hostile_offline <- row.means(df6[, hostile_offline])

# being hostile online ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hostile_online <- paste0("hostile_online_", 1:8)
df6$hostile_online <- row.means(df6[, hostile_online])


# Difficulties in emotion regulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ders <- paste0("ders", 1:12)
df6$ders <- rowMeans(df6[, ders])

# trait aggression ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aggression <- paste0("aggression", 1:12)
df6$aggression <- rowMeans(df6[, aggression])

# status driven risk seeking~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdrt <- paste0("sdrt", 1:14)
df6$sdrt <- rowMeans(df6[, sdrt])


# Party ID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df6$pid <- pid(xd = df6$pid_dem, xr = df6$pid_rep, xi = df6$pid_ind)


df6 <- df6 %>% 
        mutate(female = gender -1, 
               white = ifelse(race == 1, 1, 0), 
               highered = ifelse(educ >= 4, 1, 0), 
               age = dplyr::recode(as.character(profile_age2), 
                                        `1` = 20L,  `2` = 25L, `3` = 30L, `4` = 35L, 
                                        `5` = 40L,  `6` = 45L, `7` = 50L, `8` = 55L, 
                                        `9` = 60L, `10` = 65L, `11` = 70L, `12` = 75L, 
                                        `13` = 80L, `14` = 83L),
               income = zero.one(na_if(faminc_new, 97), max = 16)
               )


# create a list of variables with 1-7 scale. 
vars17 <- c("hostile_online", "hostile_offline", "ders", "part_online", 
            "part_offline", hostile_online, hostile_offline, sdrt, aggression, 
            "aggression", "sdrt", "pid", talk_online, talk_offline, 
            "talk_online", "talk_offline")

df6[, vars17] <- df6[, vars17] %>% 
        mutate_all(funs(zero.one(na_if(., 9))))

controlvars <- c("female", "age", "ID","highered", "income", "pid", "white", 
                 "online_first")


df.us21 <-df6 %>% 
        dplyr::select(vars17, controlvars) %>% 
        glimpse

# Save Change + Selection data 

saveRDS(df.us21, file = "B.analysis.data/s4_us_2021.rds")
write.csv(df.us21, file = "B.analysis.data/s4_us_2021.csv")


# Experimental data -------------------------------------------------------



on.off <- function(x){
        case_when(x <= 8 ~ "offline", 
                  x >= 9 ~ "online")
}

pub.priv <- function(x){
        case_when(x <= 4 | (x>= 9 & x<= 12) ~ "private", 
                  (x>= 5 & x<= 8) | x >= 13 ~ "public")
}

my.message <- function(x){
        case_when(x == 1 | x == 5 | x == 9 | x == 13 ~ "immigration", 
                  x == 2 | x == 6 | x == 10 | x == 14 ~ "covid",
                  x == 3 | x == 7 | x == 11 | x == 15 ~ "capitol",
                  x == 4 | x == 8 | x == 12 | x == 16 ~ "abortion")
}


dfe <- df6 %>% 
        dplyr::select(ID,contains("q_chosen_context"), contains("offens"), 
               contains("inapp"), contains("rare")) %>% 
        pivot_longer(cols = 2:17, names_to = "variables", values_to = "values") %>% 
        separate(variables, into= c("variables", "screen"), -1) %>% 
        pivot_wider(names_from  = variables, values_from = values) %>% 
        transmute(online = on.off(q_chosen_context_message_),
               public = pub.priv(q_chosen_context_message_),
               message = my.message(q_chosen_context_message_), 
               offensive = zero.one(offensive_),
               inappropriate = zero.one(inappropriate_),
               rare = zero.one(rare_), 
               ID = as.factor(ID), 
               screen = screen
               ) %>% 
        glimpse


# Save Perception data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

saveRDS(dfe, file = "B.analysis.data/s5_yougov.rds")
write.csv(dfe, file = "B.analysis.data/s5_yougov.csv")


rm(list = ls())
