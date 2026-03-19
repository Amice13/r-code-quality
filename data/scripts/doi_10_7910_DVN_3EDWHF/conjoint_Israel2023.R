#### パッケージの読み込み####
library(cjoint)
library(tidyverse)

#### データの読み込み####
conjoint_data <- read.qualtrics("IsraelSurvey2023Populismdata.csv",responses= NULL, covariates= c("Q8", "Q10_1"), respondentID="userID", ranks=c("Q21_1", "Q22_1", "Q23_1", "Q24_1", "Q25_1" , "Q26_1", "Q27_1", "Q21_2", "Q22_2", "Q23_2", "Q24_2", "Q25_2" , "Q26_2", "Q27_2"))

####ヘブライ語を英語に翻訳####
Israelpolicy <- conjoint_data %>% 
  select(!c(contains("rowpos")))

Israelpolicy <- Israelpolicy %>%
  mutate(
    Antijew = case_when(
      antijew == "תומכת בהענשתם"~"punish",
      antijew == "תמשיך במדיניות הקיימת"~"leave"
    )
  )

Israelpolicy <- Israelpolicy %>%
  mutate(
    Diplomacy = case_when(
      arabs == "תרחיב"~"expand",
      arabs == "לא תרחיב"~"not expand"
    )
  )

Israelpolicy <- Israelpolicy %>%
  mutate(
    Harediwork = case_when(
      haredi == "תומכת"~"support",
      haredi == "לא תומכת"~"not support"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Trade = case_when(
      imex == "תומכת"~"support",
      imex == "מתנגדת"~"oppose"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Inequality = case_when(
      inequality == "תשלם"~"subsiding",
      inequality == "לא תשלם"~"nonsubsiding"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Judicialreform = case_when(
      judicialreform == "תומכת"~"support",
      judicialreform == "מתנגדת"~"oppose"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Jewishmajority = case_when(
      jewmajority == "תחקוק"~"agree",
      jewmajority == "לא תחקוק"~"disagree"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    LGBT = case_when(
      lgbt == "תגביר"~"increase",
      lgbt == "תצמצם"~"decrease",
      lgbt == "תשאיר כמו שהוא"~"leave"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Settlement = case_when(
      settlement == "תגביר"~"increase",
      settlement == "תצמצם"~"decrease",
      settlement == "תשאיר כמו שהיא"~"leave"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    PA = case_when(
      pa == "תפתח מחדש"~"restart",
      pa == "לא תפתח"~"not restart"
    )
  )

#### 変数の水準を因子型に変更####
Israelpolicy$Antijew <- as.factor(Israelpolicy$Antijew)
Israelpolicy$Diplomacy <- as.factor(Israelpolicy$Diplomacy)
Israelpolicy$Harediwork <- as.factor(Israelpolicy$Harediwork)
Israelpolicy$Trade <- as.factor(Israelpolicy$Trade)
Israelpolicy$Inequality <- as.factor(Israelpolicy$Inequality)
Israelpolicy$Jewishmajority <- as.factor(Israelpolicy$Jewishmajority)
Israelpolicy$Judicialreform <- as.factor(Israelpolicy$Judicialreform)
Israelpolicy$LGBT <- as.factor(Israelpolicy$LGBT)
Israelpolicy$PA <- as.factor(Israelpolicy$PA)
Israelpolicy$Settlement <- as.factor(Israelpolicy$Settlement)

#### ラベルを付与####
attr_list <- list()             
attr_list[["Antijew"]] <- c("punish","leave")

attr_list[["Diplomacy"]] <- c("expand", "not expand")

attr_list[["Harediwork"]] <- c("support","not support")

attr_list[["Trade"]] <- c("support","oppose") 

attr_list[["Inequality"]] <- c("subsiding", "nonsubsiding")

attr_list[["Judicialreform"]] <- c("support","oppose")

attr_list[["Jewishmajority"]] <- c("agree","disagree")

attr_list[["LGBT"]] <- c("increase","leave","decrease")

attr_list[["PA"]] <- c("restart", "not restart") 

attr_list[["Settlement"]] <- c("increase", "leave", "decrease")


#### コンジョイント分析 ####
cjoint_design <- makeDesign(type = "constraints",
                            attribute.levels = attr_list)


cjoint_pool <- amce(selected ~ Antijew + Diplomacy + Harediwork + Trade + Inequality + Judicialreform + Jewishmajority + LGBT + PA + Settlement, 
                    data=Israelpolicy,
                    respondent.id= "respondentIndex",
                    cluster = TRUE,
                    na.ignore=TRUE)

#### コンジョイントのプロット ####
library(extrafont)

plot(cjoint_pool, point.size = 1.2, dodge.size = 1.2, text.size = 18
)

summary(cjoint_pool)

####################### Pro Bibi vs Anti Bibi #############################

antibibi <- filter(Israelpolicy, Q10_1 <= 49)

cjoint_antibibi <- amce(selected ~ Antijew + Diplomacy + Harediwork + Trade + Inequality + Judicialreform + Jewishmajority + LGBT + PA + Settlement, 
                        data=antibibi,
                        respondent.id= "respondentIndex",
                        cluster = TRUE,
                        na.ignore=TRUE)

plot(cjoint_antibibi, main="Anti-Netanyahu", point.size = 1.2, dodge.size = 1.2, text.size = 18)

summary(cjoint_antibibi)


probibi <- filter(Israelpolicy, Q10_1 >= 51)

cjoint_probibi <- amce(selected ~ Antijew + Diplomacy + Harediwork + Trade + Inequality + Judicialreform + Jewishmajority + LGBT + PA + Settlement,
                       data=probibi,
                       respondent.id= "respondentIndex",
                       cluster = TRUE,
                       na.ignore=TRUE)

plot(cjoint_probibi, main="Pro-Netayahu", point.size = 1.2, dodge.size = 1.2, text.size = 18)

summary(cjoint_probibi)


### save data ###
library(dplyr)
Israelpolicy<-select(Israelpolicy, Q8,Q10_1, selected, respondentIndex, Antijew, Diplomacy, Harediwork, Trade, Inequality, Judicialreform, Jewishmajority, LGBT, PA, Settlement)

write_csv(Israelpolicy, file="Israelpolicy.csv")