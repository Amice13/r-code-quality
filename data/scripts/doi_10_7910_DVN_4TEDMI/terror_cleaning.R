####################################################################
## author:    Steven M. Van Hauwaert & Robert A. Huber
## file name: terror_cleaning.R
## Context:   Causal Effects of Terror Attacks
## started:   2018-03-13
## Summary:   runs cleans the data
######################################################################

originalData <- read.dta("./original data/baro_drees_2015_queteletv2.dta")

originalDataLR <- read.dta("./original data/baro_drees_unif0017_queteletv2.dta")

id <- 1:nrow(originalData)

df <- data.frame(id)

rm(id)

df$year <- substr(originalData$interview, start = 1, stop = 4)

df$month <- substr(originalData$interview, start = 5, stop = 6)

df$day <- substr(originalData$interview, start = 7, stop = 8)

df$date <- paste0(df$day, "-", df$month, "-", df$year)

df$date <- as.Date(df$date, format = "%d-%m-%Y")

df$cutoff <- as.numeric(df$date - as.Date("14-11-2015", format = "%d-%m-%Y"))

df$postterror <- ifelse(df$cutoff < 0, "Pre-Terror", "Post-Terror")
df$postterror <- factor(df$postterror, levels = c("Pre-Terror", "Post-Terror"))
df$postterror<- relevel(df$postterror, ref = "Pre-Terror")
#Post Terror CutOff

df$genderF <- factor(originalData$sdsexe, levels = c(1,2), labels = c("Male", "Female"))

df$age <- originalData$sdage

df$inc <- ifelse(originalData$sdrevcl == 999999999, NA,
                 originalData$sdrevcl)

df$incgrp <- factor(originalData$sdrevtr, 
                    levels = c(1:8), 
                    labels = c("Less then 1000", "1000 to 1400", "1400 to 1900",
                               "1900 to 2400", "2400 to 3800", "3800 to 5300", 
                               "More than 5300", "No response"))

df$region <- factor(originalData$region)

df$lrscale <- ifelse(originalData$sdpol == 11, NA, as.numeric(originalData$sdpol))

df11 <- data.frame(ident = originalDataLR$ident,
                   lrscale11 = ifelse(originalDataLR$sdpol == 12, NA, as.numeric(originalDataLR$sdpol)))

originalData <- merge(originalData, df11, by = "ident")

df$lrscale11 <- originalData$lrscale11

df$extreme <- abs(df$lrscale-3.5)

df$extreme <- df$extreme - .5

df$extreme11 <- abs(df$lrscale11-6)

df$nationality <- factor(originalData$sdnatp,
                         levels = c(1:3),
                         labels = c("French", "Foreign", "No response"))

df$employment <- ifelse(originalData$sdsitua == 1 | originalData$sdsitua == 2, 1, 0)
df$employment <- factor(df$employment, 
                        levels = c(0:1),
                        labels = c("Not employed", "Employed"))

df$edu <- ifelse(originalData$sddipl > 8, NA, as.numeric(originalData$sddipl))

df$socCoh <- factor(originalData$cs1,
                    levels = c(1:5),
                    labels = c("Very strong", "Strong", "Weak", "Very weak", "Don't know"))

df$socCoh_num <- ifelse(df$socCoh == "Don't know", NA, as.numeric(df$socCoh))
df$socCoh_num <- abs(df$socCoh_num - 5)

df$socInt <- factor(originalData$cs5,
                    levels = c(1:5),
                    labels = c("Very strong", "Strong", "Weak", "Very weak", "Don't know"))

df$socInt_num <- ifelse(df$socInt == "Don't know", NA, as.numeric(df$socInt))
df$socInt_num <- abs(df$socInt_num - 5)

df$posImm <- factor(originalData$og8cd_1,
                    levels = c(1:5),
                    labels = c("Very positive", "Positive", "Negative", "Very negative", "Don't know"))

df$posImm_num <- ifelse(df$posImm == "Don't know", NA, as.numeric(df$posImm))
df$posImm_num <- abs(df$posImm_num - 5)

df$salImm <- factor(originalData$og4_2,
                    levels = c(1:5),
                    labels = c("Very strong", "Strong", "Weak", "Very weak", "Don't know"))

df$salImm_num <- ifelse(df$salImm == "Don't know", NA, as.numeric(df$salImm))
df$salImm_num <- abs(df$salImm_num - 5)

df$polTrust <- factor(originalData$sa13_4,
                      levels = c(1:5),
                      labels = c("Very high", "High", "Low", "Very low", "Don't know"))

df$polTrust_num <- ifelse(df$polTrust == "Don't know", NA, as.numeric(df$polTrust))
df$polTrust_num <- abs(df$polTrust_num - 5)

df$salAIDS <- factor(originalData$og5_3,
                    levels = c(1:5),
                    labels = c("Very strong", "Strong", "Weak", "Very weak", "Don't know"))

df$salAIDS_num <- ifelse(df$salAIDS == "Don't know", NA, as.numeric(df$salAIDS))
df$salAIDS_num <- abs(df$salAIDS_num - 5)

df$posHom <- factor(originalData$og8cd_3,
                     levels = c(1:5),
                     labels = c("Very strong", "Strong", "Weak", "Very weak", "Don't know"))

df$posHom_num <- ifelse(df$posHom == "Don't know", NA, as.numeric(df$posHom))
df$posHom_num <- abs(df$posHom_num - 5)

df$socPro <- factor(originalData$ps10,
                    levels = c(1:5),
                    labels = c("Very good", "Rather good", "Rather bad", "Very bad", "Don't know"))

df$socPro_num <- ifelse(df$socPro == "Don't know", NA, as.numeric(df$socPro))
df$socPro_num <- abs(df$socPro_num - 5)

df$healthcare <- factor(originalData$ps11,
                    levels = c(1:5),
                    labels = c("Strongly agree", "Rather agree", "Rather disagree", "Strongly disagree", "Don't know"))

df$healthcare_num <- ifelse(df$healthcare == "Don't know", NA, as.numeric(df$healthcare))
df$healthcare_num <- abs(df$healthcare_num - 5)

df$socSec1 <- factor(originalData$ps15_1,
                    levels = c(1:5),
                    labels = c("Strongly agree", "Rather agree", "Rather disagree", "Strongly disagree", "Don't know"))

df$socSec1_num <- ifelse(df$socSec1 == "Don't know", NA, as.numeric(df$socSec1))
df$socSec1_num <- abs(df$socSec1_num - 5)

df$socSec2 <- factor(originalData$ps15_3,
                     levels = c(1:5),
                     labels = c("Strongly agree", "Rather agree", "Rather disagree", "Strongly disagree", "Don't know"))

df$socSec2_num <- ifelse(df$socSec2 == "Don't know", NA, as.numeric(df$socSec2))
df$socSec2_num <- abs(df$socSec2_num - 5)

df$perceived_health <- factor(originalData$sa1,
                              levels = c(1:6),
                              labels = c("Very good", "Rather good", "Average", "Rather bad", "Very bad", "Don't know"))

df$perceived_health_num <- ifelse(df$perceived_health == "Don't know", NA, as.numeric(df$perceived_health))
df$perceived_health_num <- abs(df$perceived_health_num - 6)

df$weight <- originalData$poids

write.csv(df, file = paste0("./final data/data_clean_", Sys.Date(), ".csv", sep=""))
write.dta(df, file = paste0("./final data/data_clean_", Sys.Date(), ".dta", sep=""))

