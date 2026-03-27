#### setting environment ####
require(cjoint)
require(estimatr)

attr.names <- c("gender", "age", "education", "occupation",
                "dynasty", "experience", "party")
attr.labels <- c("Gender", "Age", "Education", 
                 "Prior occupation", "Dynastic status", 
                 "Experience", "Party affiliation")
level.labels <- c("Female", "34", "The University of Tokyo", 
                  "Local politician", "No dynastic status", 
                  "Two terms", "LDP")

#### data ####
## 1st pilot survey
respondent.data.1st <- read.csv("data_Study2_pilot_1st.csv")

# number of respondents
nrow(respondent.data.1st)

# data of the manipulated mediator arm (MMA) for perceived anti-elitism
AE.MMA.data.1st <- read.qualtrics("data_Study2_pilot_1st_conjoint.csv", 
                                  ranks = c("Q1_AE_A_1", "Q1_AE_A_2", 
                                            "Q2_AE_A_1", "Q2_AE_A_2", 
                                            "Q3_AE_A_1", "Q3_AE_A_2", 
                                            "Q4_AE_A_1", "Q4_AE_A_2"), 
                                  new.format = TRUE)
# combine variables for the "special notes" attribute
AE.MMA.data.1st$notes <- unlist(respondent.data.1st[, paste0("note.", 1:8)])

# data of the natural mediator arm (NMA) for perceived anti-elitism
AE.NMA.data.1st <- read.qualtrics("data_Study2_pilot_1st_conjoint.csv", 
                                  ranks = c("Q1_AE_B_1", "Q1_AE_B_2", 
                                            "Q2_AE_B_1", "Q2_AE_B_2", 
                                            "Q3_AE_B_1", "Q3_AE_B_2", 
                                            "Q4_AE_B_1", "Q4_AE_B_2"), 
                                  new.format = TRUE)
# variables for the "special notes" attribute are missing for the NMA
AE.NMA.data.1st$notes <- "なし"

# combine the MMA and NMA data
AE.data.1st <- subset(rbind(AE.MMA.data.1st, AE.NMA.data.1st), ! is.na(selected))
colnames(AE.data.1st)[seq(5, 17, 2)] <- c("education", "dynasty", "experience", 
                                          "party", "gender", "occupation", "age")
AE.data.1st$survey <- "1st"

# data of the manipulated mediator arm (MMA) for perceived people-centrism
PC.MMA.data.1st <- read.qualtrics("data_Study2_pilot_1st_conjoint.csv", 
                                  ranks = c("Q1_PC_A_1", "Q1_PC_A_2", 
                                            "Q2_PC_A_1", "Q2_PC_A_2", 
                                            "Q3_PC_A_1", "Q3_PC_A_2", 
                                            "Q4_PC_A_1", "Q4_PC_A_2"), 
                                  new.format = TRUE)
# combine variables for the "special notes" attribute
PC.MMA.data.1st$notes <- unlist(respondent.data.1st[, paste0("note.", 1:8)])

# data of the natural mediator arm (NMA) for perceived people-centrism
PC.NMA.data.1st <- read.qualtrics("data_Study2_pilot_1st_conjoint.csv", 
                                  ranks = c("Q1_PC_B_1", "Q1_PC_B_2", 
                                            "Q2_PC_B_1", "Q2_PC_B_2", 
                                            "Q3_PC_B_1", "Q3_PC_B_2", 
                                            "Q4_PC_B_1", "Q4_PC_B_2"), 
                                  new.format = TRUE)
# variables for the "special notes" attribute are missing for the NMA
PC.NMA.data.1st$notes <- "なし"

# combine the MMA and NMA data
PC.data.1st <- subset(rbind(PC.MMA.data.1st, PC.NMA.data.1st), ! is.na(selected))
colnames(PC.data.1st)[seq(5, 17, 2)] <- c("education", "dynasty", "experience", 
                                          "party", "gender", "occupation", "age")
PC.data.1st$survey <- "1st"

## 2nd pilot survey
respondent.data.2nd <- read.csv("data_Study2_pilot_2nd.csv")

# number of respondents
nrow(respondent.data.2nd)

# data of the manipulated mediator arm (MMA) for perceived anti-elitism
AE.MMA.data.2nd <- read.qualtrics("data_Study2_pilot_2nd_conjoint.csv", 
                                  ranks = c("Q1_AE_A_1", "Q1_AE_A_2", 
                                            "Q2_AE_A_1", "Q2_AE_A_2", 
                                            "Q3_AE_A_1", "Q3_AE_A_2", 
                                            "Q4_AE_A_1", "Q4_AE_A_2"), 
                                  new.format = TRUE)
# combine variables for the "special notes" attribute
AE.MMA.data.2nd$notes <- unlist(respondent.data.2nd[, paste0("note.", 1:8)])

# data of the natural mediator arm (NMA) for perceived anti-elitism
AE.NMA.data.2nd <- read.qualtrics("data_Study2_pilot_2nd_conjoint.csv", 
                                  ranks = c("Q1_AE_B_1", "Q1_AE_B_2", 
                                            "Q2_AE_B_1", "Q2_AE_B_2", 
                                            "Q3_AE_B_1", "Q3_AE_B_2", 
                                            "Q4_AE_B_1", "Q4_AE_B_2"), 
                                  new.format = TRUE)
# variables for the "special notes" attribute are missing for the NMA
AE.NMA.data.2nd$notes <- "なし"

# combine the MMA and NMA data
AE.data.2nd <- subset(rbind(AE.MMA.data.2nd, AE.NMA.data.2nd), ! is.na(selected))
colnames(AE.data.2nd)[seq(5, 17, 2)] <- c("education", "dynasty", "experience", 
                                          "party", "gender", "occupation", "age")
AE.data.2nd$survey <- "2nd"
# distinguish respondents' ID from those in other surveys
AE.data.2nd$respondent <- AE.data.2nd$respondent + 1000

# data of the manipulated mediator arm (MMA) for perceived people-centrism
PC.MMA.data.2nd <- read.qualtrics("data_Study2_pilot_2nd_conjoint.csv", 
                                  ranks = c("Q1_PC_A_1", "Q1_PC_A_2", 
                                            "Q2_PC_A_1", "Q2_PC_A_2", 
                                            "Q3_PC_A_1", "Q3_PC_A_2", 
                                            "Q4_PC_A_1", "Q4_PC_A_2"), 
                                  new.format = TRUE)
# combine variables for the "special notes" attribute
PC.MMA.data.2nd$notes <- unlist(respondent.data.2nd[, paste0("note.", 1:8)])

# data of the natural mediator arm (NMA) for perceived people-centrism
PC.NMA.data.2nd <- read.qualtrics("data_Study2_pilot_2nd_conjoint.csv", 
                                  ranks = c("Q1_PC_B_1", "Q1_PC_B_2", 
                                            "Q2_PC_B_1", "Q2_PC_B_2", 
                                            "Q3_PC_B_1", "Q3_PC_B_2", 
                                            "Q4_PC_B_1", "Q4_PC_B_2"), 
                                  new.format = TRUE)
# variables for the "special notes" attribute are missing for the NMA
PC.NMA.data.2nd$notes <- "なし"

# combine the MMA and NMA data
PC.data.2nd <- subset(rbind(PC.MMA.data.2nd, PC.NMA.data.2nd), ! is.na(selected))
colnames(PC.data.2nd)[seq(5, 17, 2)] <- c("education", "dynasty", "experience", 
                                          "party", "gender", "occupation", "age")
PC.data.2nd$survey <- "2nd"
# distinguish respondents' ID from those in other surveys
PC.data.2nd$respondent <- PC.data.2nd$respondent + 1000

## 3rd pilot survey
respondent.data.3rd <- read.csv("data_Study2_pilot_3rd.csv")

# number of respondents
nrow(respondent.data.3rd)

# data of the manipulated mediator arm (MMA) for perceived anti-elitism
AE.MMA.data.3rd <- read.qualtrics("data_Study2_pilot_3rd_conjoint.csv", 
                                  ranks = c("Q1_AE_A_1", "Q1_AE_A_2", 
                                            "Q2_AE_A_1", "Q2_AE_A_2", 
                                            "Q3_AE_A_1", "Q3_AE_A_2", 
                                            "Q4_AE_A_1", "Q4_AE_A_2"), 
                                  new.format = TRUE)
# combine variables for the "special notes" attribute
AE.MMA.data.3rd$notes <- unlist(respondent.data.3rd[, paste0("note.", 1:8)])

# data of the natural mediator arm (NMA) for perceived anti-elitism
AE.NMA.data.3rd <- read.qualtrics("data_Study2_pilot_3rd_conjoint.csv", 
                                  ranks = c("Q1_AE_B_1", "Q1_AE_B_2", 
                                            "Q2_AE_B_1", "Q2_AE_B_2", 
                                            "Q3_AE_B_1", "Q3_AE_B_2", 
                                            "Q4_AE_B_1", "Q4_AE_B_2"), 
                                  new.format = TRUE)
# variables for the "special notes" attribute are missing for the NMA
AE.NMA.data.3rd$notes <- "なし"

# combine the MMA and NMA data
AE.data.3rd <- subset(rbind(AE.MMA.data.3rd, AE.NMA.data.3rd), ! is.na(selected))
colnames(AE.data.3rd)[seq(5, 17, 2)] <- c("education", "dynasty", "experience", 
                                          "party", "gender", "occupation", "age")
AE.data.3rd$survey <- "3rd"
# distinguish respondents' ID from those in other surveys
AE.data.3rd$respondent <- AE.data.3rd$respondent + 2000

# data of the manipulated mediator arm (MMA) for perceived people-centrism
PC.MMA.data.3rd <- read.qualtrics("data_Study2_pilot_3rd_conjoint.csv", 
                                  ranks = c("Q1_PC_A_1", "Q1_PC_A_2", 
                                            "Q2_PC_A_1", "Q2_PC_A_2", 
                                            "Q3_PC_A_1", "Q3_PC_A_2", 
                                            "Q4_PC_A_1", "Q4_PC_A_2"), 
                                  new.format = TRUE)
# combine variables for the "special notes" attribute
PC.MMA.data.3rd$notes <- unlist(respondent.data.3rd[, paste0("note.", 1:8)])

# data of the natural mediator arm (NMA) for perceived people-centrism
PC.NMA.data.3rd <- read.qualtrics("data_Study2_pilot_3rd_conjoint.csv", 
                                  ranks = c("Q1_PC_B_1", "Q1_PC_B_2", 
                                            "Q2_PC_B_1", "Q2_PC_B_2", 
                                            "Q3_PC_B_1", "Q3_PC_B_2", 
                                            "Q4_PC_B_1", "Q4_PC_B_2"), 
                                  new.format = TRUE)
# variables for the "special notes" attribute are missing for the NMA
PC.NMA.data.3rd$notes <- "なし"

# combine the MMA and NMA data
PC.data.3rd <- subset(rbind(PC.MMA.data.3rd, PC.NMA.data.3rd), ! is.na(selected))
colnames(PC.data.3rd)[seq(5, 17, 2)] <- c("education", "dynasty", "experience", 
                                          "party", "gender", "occupation", "age")
PC.data.3rd$survey <- "3rd"
# distinguish respondents' ID from those in other surveys
PC.data.3rd$respondent <- PC.data.3rd$respondent + 2000

## combine data of the three pilot surveys for perceived anti-elitism
AE.data <- rbind(AE.data.1st, AE.data.2nd, AE.data.3rd)
AE.data$notes <- factor(AE.data$notes, 
                        levels = c("なし", 
                                   "中立的な行政監視団体に所属し，政治家や公務員の汚職を告発する活動をしていた", 
                                   "政治家・官僚・財界の癒着関係を排除するための抜本的な政治・行政改革を提言する非営利団体で活動していた", 
                                   "政治家・官僚・財界の癒着関係を解消するため，政治資金や公文書の情報公開の徹底を提言する活動をしていた", 
                                   "身を切る改革の第一歩として議員報酬の削減を訴えており，当選したら自らの議員報酬を半額返納すると公言している", 
                                   "かつて務めていた企業で，企業と党のパイプ役になるために，ある政党のシンクタンクに出向していたことがある", 
                                   "経済界のリーダーと深いコネがあり，政治家のパーティーでしばしば政治家と財界人の仲介をしていた", 
                                   "かつて務めていた企業で，企業と党の橋渡し役になるために，ある政党の組織に出向していたことがある", 
                                   "先祖代々続く資産家の生まれで，土地や株式を多数保有しており，選挙に出る前から政界に顔が利く人物であった", 
                                   "普通の人々の目線を大切にするという信念をもち，街頭や商業施設に繰り出して人々の意見を開いている", 
                                   "インターネット上で一般の人々と頻繁にやりとりをしており，そこで得られた人々の要望を政策提言に取り入れている", 
                                   "知識をもつ政治家が政治的に無知な一般の人々を導く必要があると発言し，政治塾出身の仲間と勉強会をしている", 
                                   "近年の政治は一般有権者の感情的な意見に流されすぎだと考え，専門家と政治家を中心とした政策形成を訴えている", 
                                   "政治家は一般の人々に従うのではなく，むしろ知識をもつ政治家が政治的知識に乏しい人々を導くべきだと発言している"), 
                        labels = c("NMA", "AE+1", "AE+2", "AE+3", "AE+4", 
                                   "AE-1", "AE-2", "AE-3", "AE-4", "PC+1", 
                                   "PC+2", "PC-1", "PC-2", "PC-3"))
# change variables to factor class
AE.data$gender <- factor(AE.data$gender, 
                         levels = c("男性", "女性"), 
                         labels = c("Male", "Female"))
AE.data$age <- factor(AE.data$age, 
                      levels = c("70歳", "34歳"), 
                      labels = c("70", "34"))
AE.data$education <- factor(AE.data$education, 
                            levels = c("高校卒", "東京大学卒"), 
                            labels = c("High school", "The University of Tokyo"))
AE.data$occupation <- factor(AE.data$occupation, 
                             levels = c("タレント", "会社員", 
                                        "国会議員秘書", "地方政治家"), 
                             labels = c("Celebrity", "Employee", 
                                        "Secretary", "Local politician"))
AE.data$dynasty <- factor(AE.data$dynasty, 
                          levels = c("親が元大臣", "近親者に政治家はいない"), 
                          labels = c("Former minister", "No dynastic status"))
AE.data$experience <- factor(AE.data$experience, 
                             levels = c("経験なし", "衆議院議員2期"), 
                             labels = c("No experience", "Two terms"))
AE.data$party <- factor(AE.data$party, 
                        levels = c("立憲民主党", "自由民主党"), 
                        labels = c("CDP", "LDP"))
AE.data$survey <- factor(AE.data$survey)

## combine data of the three pilot surveys for perceived people-centrism
PC.data <- rbind(PC.data.1st, PC.data.2nd, PC.data.3rd)
PC.data$notes <- factor(PC.data$notes, 
                        levels = c("なし", 
                                   "中立的な行政監視団体に所属し，政治家や公務員の汚職を告発する活動をしていた", 
                                   "政治家・官僚・財界の癒着関係を排除するための抜本的な政治・行政改革を提言する非営利団体で活動していた", 
                                   "政治家・官僚・財界の癒着関係を解消するため，政治資金や公文書の情報公開の徹底を提言する活動をしていた", 
                                   "身を切る改革の第一歩として議員報酬の削減を訴えており，当選したら自らの議員報酬を半額返納すると公言している", 
                                   "かつて務めていた企業で，企業と党のパイプ役になるために，ある政党のシンクタンクに出向していたことがある", 
                                   "経済界のリーダーと深いコネがあり，政治家のパーティーでしばしば政治家と財界人の仲介をしていた", 
                                   "かつて務めていた企業で，企業と党の橋渡し役になるために，ある政党の組織に出向していたことがある", 
                                   "先祖代々続く資産家の生まれで，土地や株式を多数保有しており，選挙に出る前から政界に顔が利く人物であった", 
                                   "普通の人々の目線を大切にするという信念をもち，街頭や商業施設に繰り出して人々の意見を開いている", 
                                   "インターネット上で一般の人々と頻繁にやりとりをしており，そこで得られた人々の要望を政策提言に取り入れている", 
                                   "知識をもつ政治家が政治的に無知な一般の人々を導く必要があると発言し，政治塾出身の仲間と勉強会をしている", 
                                   "近年の政治は一般有権者の感情的な意見に流されすぎだと考え，専門家と政治家を中心とした政策形成を訴えている", 
                                   "政治家は一般の人々に従うのではなく，むしろ知識をもつ政治家が政治的知識に乏しい人々を導くべきだと発言している"), 
                        labels = c("NMA", "AE+1", "AE+2", "AE+3", "AE+4", 
                                   "AE-1", "AE-2", "AE-3", "AE-4", "PC+1", 
                                   "PC+2", "PC-1", "PC-2", "PC-3"))
# change variables to factor class
PC.data$gender <- factor(PC.data$gender, 
                         levels = c("男性", "女性"), 
                         labels = c("Male", "Female"))
PC.data$age <- factor(PC.data$age, 
                      levels = c("70歳", "34歳"), 
                      labels = c("70", "34"))
PC.data$education <- factor(PC.data$education, 
                            levels = c("高校卒", "東京大学卒"), 
                            labels = c("High school", "The University of Tokyo"))
PC.data$occupation <- factor(PC.data$occupation, 
                             levels = c("タレント", "会社員", 
                                        "国会議員秘書", "地方政治家"), 
                             labels = c("Celebrity", "Employee", 
                                        "Secretary", "Local politician"))
PC.data$dynasty <- factor(PC.data$dynasty, 
                          levels = c("親が元大臣", "近親者に政治家はいない"), 
                          labels = c("Former minister", "No dynastic status"))
PC.data$experience <- factor(PC.data$experience, 
                             levels = c("経験なし", "衆議院議員2期"), 
                             labels = c("No experience", "Two terms"))
PC.data$party <- factor(PC.data$party, 
                        levels = c("立憲民主党", "自由民主党"), 
                        labels = c("CDP", "LDP"))
PC.data$survey <- factor(PC.data$survey)

#### average marginal component effects (AMCEs) of special notes on perceived anti-elitism ####
## 1st pilot survey
AE.1st.result <- lm_robust(selected ~ notes + gender + age + education + 
                             occupation + dynasty + experience + party, 
                           data = AE.data, subset = survey == "1st", 
                           cluster = respondent)
# Table A.8 (1st survey)
round(summary(AE.1st.result)$coefficients[2:9, c(1, 2, 4)], 2)

## 2nd pilot survey
AE.2nd.result <- lm_robust(selected ~ notes + gender + age + education + 
                             occupation + dynasty + experience + party, 
                           data = AE.data, subset = survey == "2nd", 
                           cluster = respondent)
# Table A.8 (2nd survey)
round(summary(AE.2nd.result)$coefficients[2:9, c(1, 2, 4)], 2)

## 3rd pilot survey
AE.3rd.result <- lm_robust(selected ~ notes + gender + age + education + 
                             occupation + dynasty + experience + party, 
                           data = AE.data, subset = survey == "3rd", 
                           cluster = respondent)
# Table A.8 (3rd survey)
round(summary(AE.3rd.result)$coefficients[2:9, c(1, 2, 4)], 2)

## combine three surveys
AE.all.result <- lm_robust(selected ~ notes + gender + age + education + 
                             occupation + dynasty + experience + party, 
                           data = AE.data, cluster = respondent)
summary.AE.all.result <- summary(AE.all.result)$coefficients
# Table A.8 (combined)
round(summary.AE.all.result[2:14, c(1, 2, 4)], 2)

#### average marginal component effects of special notes on perceived people-centrism ####
## 1st pilot survey
PC.1st.result <- lm_robust(selected ~ notes + gender + age + education + 
                             occupation + dynasty + experience + party, 
                           data = PC.data, subset = survey == "1st", 
                           cluster = respondent)
# Table A.8 (1st survey)
round(summary(PC.1st.result)$coefficients[2:9, c(1, 2, 4)], 2)

## 2nd pilot survey
PC.2nd.result <- lm_robust(selected ~ notes + gender + age + education + 
                             occupation + dynasty + experience + party, 
                           data = PC.data, subset = survey == "2nd", 
                           cluster = respondent)
# Table A.8 (2nd survey)
round(summary(PC.2nd.result)$coefficients[2:9, c(1, 2, 4)], 2)

## 3rd pilot survey
PC.3rd.result <- lm_robust(selected ~ notes + gender + age + education + 
                             occupation + dynasty + experience + party, 
                           data = PC.data, subset = survey == "3rd", 
                           cluster = respondent)
# Table A.8 (3rd survey)
round(summary(PC.3rd.result)$coefficients[2:9, c(1, 2, 4)], 2)

## combine three surveys
PC.all.result <- lm_robust(selected ~ notes + gender + age + education + 
                             occupation + dynasty + experience + party, 
                           data = PC.data, cluster = respondent)
summary.PC.all.result <- summary(PC.all.result)$coefficients
# Table A.8 (combined)
round(summary.PC.all.result[2:14, c(1, 2, 4)], 2)

#### compare AMCEs of special notes with those of other attributes ####
## Figure A.3
cairo_pdf("Figure_A3.pdf", width = 6, height = 4, pointsize = 9)
par(mar = c(4, 0, 2.5, 0), lwd = 0.5, xpd = TRUE)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-1.5, 1.75), ylim = c(1, 19), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(seq(-0.5, 0.5, 0.25), 0, seq(-0.5, 0.5, 0.25), 19.5, 
         lwd = 0.4, col = "gray")
segments(seq(0.75, 1.75, 0.25), 0, seq(0.75, 1.75, 0.25), 19.5, 
         lwd = 0.4, col = "gray")
segments(-0.525, c(18:15, seq(13, 1, -2)), 
         0.525, c(18:15, seq(13, 1, -2)), lty = 3, col = "gray")
segments(0.725, c(18:15, seq(13, 1, -2)), 
         1.775, c(18:15, seq(13, 1, -2)), lty = 3, col = "gray")
text(-1.5, 19, labels = "Special notes", pos = 4, font = 2)
text(-0.59375, 18, labels = "AE+1", cex = 0.9, pos = 2)
points(summary.AE.all.result["notesAE+1", 1], 18, pch = 19)
segments(summary.AE.all.result["notesAE+1", 5], 18, 
         summary.AE.all.result["notesAE+1", 6], 18)
text(-0.59375, 17, labels = "AE-4", cex = 0.9, pos = 2)
points(summary.AE.all.result["notesAE-4", 1], 17, pch = 19)
segments(summary.AE.all.result["notesAE-4", 5], 17, 
         summary.AE.all.result["notesAE-4", 6], 17)
text(-0.59375, 16, labels = "PC+1", cex = 0.9, pos = 2)
points(summary.AE.all.result["notesPC+1", 1], 16, pch = 19)
segments(summary.AE.all.result["notesPC+1", 5], 16, 
         summary.AE.all.result["notesPC+1", 6], 16)
text(-0.59375, 15, labels = "PC-2", cex = 0.9, pos = 2)
points(summary.AE.all.result["notesPC-2", 1], 15, pch = 19)
segments(summary.AE.all.result["notesPC-2", 5], 15, 
         summary.AE.all.result["notesPC-2", 6], 15)
for (i in 1:7) {
  text(-1.5, 16 - 2 * i, labels = attr.labels[i], pos = 4, font = 2)
  text(-0.59375, 15 - 2 * i, labels = level.labels[i], cex = 0.9, pos = 2)
  points(summary.AE.all.result[paste0(attr.names[i], 
                                      level.labels[i]), 1], 
         15 - 2 * i, pch = 19)
  segments(summary.AE.all.result[paste0(attr.names[i], 
                                        level.labels[i]), 5], 
           15 - 2 * i, 
           summary.AE.all.result[paste0(attr.names[i], 
                                        level.labels[i]), 6], 
           15 - 2 * i)
}
# for perceived anti-elitism, AMCE = 0 is plotted at x = 1.25
points(summary.PC.all.result["notesAE+1", 1] + 1.25, 18, pch = 19)
segments(summary.PC.all.result["notesAE+1", 5] + 1.25, 18, 
         summary.PC.all.result["notesAE+1", 6] + 1.25, 18)
points(summary.PC.all.result["notesAE-4", 1] + 1.25, 17, pch = 19)
segments(summary.PC.all.result["notesAE-4", 5] + 1.25, 17, 
         summary.PC.all.result["notesAE-4", 6] + 1.25, 17)
points(summary.PC.all.result["notesPC+1", 1] + 1.25, 16, pch = 19)
segments(summary.PC.all.result["notesPC+1", 5] + 1.25, 16, 
         summary.PC.all.result["notesPC+1", 6] + 1.25, 16)
points(summary.PC.all.result["notesPC-2", 1] + 1.25, 15, pch = 19)
segments(summary.PC.all.result["notesPC-2", 5] + 1.25, 15, 
         summary.PC.all.result["notesPC-2", 6] + 1.25, 15)
for (i in 1:7) {
  points(summary.PC.all.result[paste0(attr.names[i], 
                                      level.labels[i]), 1] + 1.25, 
         15 - 2 * i, pch = 19)
  segments(summary.PC.all.result[paste0(attr.names[i], 
                                        level.labels[i]), 5] + 1.25, 
           15 - 2 * i, 
           summary.PC.all.result[paste0(attr.names[i], 
                                        level.labels[i]), 6] + 1.25, 
           15 - 2 * i)
}
axis(1, at = seq(-0.5, 0.5, 0.25), 
     labels = c("-0.50", "-0.25", "0.00", "0.25", "0.50"), lwd = 0.5)
axis(1, at = seq(0.75, 1.75, 0.25), 
     labels = c("-0.50", "-0.25", "0.00", "0.25", "0.50"), lwd = 0.5)
mtext("Anti-elitism", line = 1, at = 0, cex = 1.2, font = 2)
mtext("People-centrism", line = 1, at = 1.25, cex = 1.2, font = 2)
mtext("Average marginal component effect", side = 1, at = 0, line = 3)
mtext("Average marginal component effect", side = 1, at = 1.25, line = 3)
dev.off()