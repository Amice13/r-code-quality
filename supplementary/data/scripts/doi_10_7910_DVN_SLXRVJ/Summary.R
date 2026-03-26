## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## A6, A7
##
## Tables reproduced in this file
## 1
##
### ### ### ###

## Packages

library(tidyverse)
library(stargazer)

## Read opinion report meta data

ref_reports <- read_rds('Opinion_Reports_V2.RDS')

#### Table 1, Columns 1 and 2 ####

n <- nrow(ref_reports)

## Get month, year and source stats

sum_mon <- table(ref_reports$month)
sum_mon <- cbind(sum_mon, round(sum_mon * 100 / n, 3))
sum_year <- table(ref_reports$year)
sum_year <- cbind(sum_year, round(sum_year * 100 / n, 3))
sum_firm <- table(ref_reports$source)
sum_firm <- cbind(sum_firm, round(sum_firm * 100 / n, 3))

## Disaggregated topics

topics_dagg <- c(ref_reports[, "topic1_th"], ref_reports[, "topic2_th"], 
                 ref_reports[, "topic3_th"])
topics_dagg <- topics_dagg[!is.na(topics_dagg)]
n_dagg <- length(topics_dagg)

topics_dagg <- table(topics_dagg)
topics_dagg <- cbind(topics_dagg, round(topics_dagg * 100/ n_dagg, 3))

## Aggregated Topics

topics_agg <- c(ref_reports[, "topic1_th_agg"], 
                ref_reports[, "topic2_th_agg"], 
                ref_reports[, "topic3_th_agg"])
topics_agg <- topics_agg[!is.na(topics_agg)]
n_agg <- length(topics_agg)

topics_agg <- table(topics_agg)
topics_agg <- cbind(topics_agg, 
                    round(topics_agg * 100 / n_agg, 3))

## Word count per survey

words <- cbind(mean(ref_reports$survey_words_stem), 
               sd(ref_reports$survey_words_stem))
words <- round(words, 3)

## Prepare for stargazer

out1 <- rbind(words, sum_firm, sum_year, sum_mon, topics_agg,
              topics_dagg)
out1 <- round(out1, 1)
out1 <- cbind(rownames(out1), out1)

## Rename rows 

out1[1, 1] <- "Words (post stemming)"
out1[2:8, 1] <- c("Allensbach", 'Dimap', "Emnid", 
                  "FG", "GMS", "Polis", "TNS")
out1[14:25, 1] <- c("Jan", "Feb", "Mar", "Apr", 
                    "May", "Jun", "Jul", "Aug",
                    "Sep", "Oct", "Nov", "Dec")
out1[26:32, 1] <- c("Culture", "Econ. policy", 
                    "Education", "Environmental policy",
                    "Foreign policy", "Interior", "Social policy")
out1[33:45, 1] <- c("Labor / Soc. policy", 
                    "Foreign policy", "Education / Research",
                    "Agriculture", "Families", "Finances", 
                    "Health", "Interior",
                    "Justice", "Culture", "Environmental policy", 
                    "Defense", "Econ. policy / Energy")

# Table output
# Columns 1 and 2 of Table 1

stargazer(out1, summary = F, rownames = F,
          digits = 1, font.size = "footnotesize")

## Remove some objects

rm(sum_firm, sum_year, sum_mon, topics_agg, 
   topics_dagg, words, n, n_agg, n_dagg, out1)

#### Table 1, Columns 3 and 4 ####

## Load meta data for tagged documents (pre-classified by gov't)

speech_df <- read_rds('Speech_Metadata_Tagged.RDS')

## Lead meta data for untagged documents (automatically classified by the authors)

speech_df_untagged <- read_rds('Speech_Metadata_Untagged.RDS')

## Drop everything that is not basepage, pressrelease, speech

speech_df <- speech_df[speech_df$type %in% c("basepage", 
                                             "pressrelease", "speech"), ]
speech_df_untagged <- speech_df_untagged[speech_df_untagged$doctype %in% 
                                           c("article", 
                                             "pressrelease", "speech"), ]  

## Rename article to basepage

speech_df_untagged$doctype[speech_df_untagged$doctype == 'article'] <- 'basepage'

## Summary stats for tagged documents

n <- nrow(speech_df)

## Year

sum_year <- table(speech_df$date_year)
sum_year <- cbind(sum_year, sum_year * 100 / n)

## Month

sum_mon <- table(speech_df$date_month)
sum_mon <- cbind(sum_mon, sum_mon * 100 / n)

## Length

words <- c(mean(speech_df$speech_words_stem), sd(speech_df$speech_words_stem))

## Doctype

sum_type <- table(speech_df$type)
sum_type <- cbind(sum_type, sum_type * 100 / n)

## Aggregate categories prior to summarizing

cat_dict <- list("soc_policy" = 
                   c("arbeit_und_soziales", "gesundheit", 
                     "familie_senioren_frauen_und_jugend"),
                 "econ_policy" = c("finanzen", 
                                   "verkehr_und_digitale_infrastruktur", 
                                   "wirtschaft_und_energie"),
                 "foreign_policy" = c("auswaertiges", 
                                      "verteidigung", 
                                      "wirtschaftliche_zusammenarbeit_und_entwicklung"),
                 "education" = "bildung_und_forschung",
                 "environmental" = c("ernaehrung_und_landwirtschaft", 
                                     "umwelt_naturschutz_bau_und_reaktorsicherheit"),
                 "culture" = "kulturstaatsminister",
                 "interior" = c("inneres", 
                                "justiz_und_verbraucherschutz"))

## Loop to aggregate the categories

speech_df$cat2 <- NA

## Actual loop

for (j in 1:length(cat_dict)) {
  speech_df$cat2[speech_df$cat %in% cat_dict[[j]]] <- names(cat_dict)[j]
}

## Category summarizing

sum_agg <- table(speech_df$cat2)
sum_agg <- cbind(sum_agg, sum_agg * 100 / n)

sum_dagg <- table(speech_df$cat)
sum_dagg <- cbind(sum_dagg, sum_dagg * 100 / n)

## Prep for output

out2 <- rbind(words, sum_type, sum_year, sum_mon, sum_agg,
              sum_dagg)
out2 <- round(out2, 1)
out2 <- cbind(rownames(out2), out2)

## Renaming

out2[1, 1] <- "Words (post stemming)"
out2[2:4, 1] <- c("Article", "Press release", "Speech")
out2[16:27, 1] <-  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                     "Sep", "Oct", "Nov", "Dec")
out2[28:34, 1] <- c("Culture", "Econ. policy", 
                    "Education", "Environmental policy",
                    "Foreign policy", "Interior", "Social policy")
out2[35:49, 1] <- c("Labor / Soc. policy", "Foreign policy", 
                    "Education / Research",
                    "Agriculture", "Families", "Finances", 
                    "Health", "Interior",
                    "Justice", "Culture", 
                    "Environmental policy", "Infrastrucutre",
                    "Defense", "Econ. policy / Energy", 
                    "Econ. development")

# To Table ()

stargazer(out2, summary = F, 
          rownames = F, 
          digits = 1, font.size = "footnotesize")

#### Table 1, Columns 5 and 6 ####

n_ut <- nrow(speech_df_untagged)

## Year year month speech_words_stem doctype pred_class

sum_year <- table(speech_df_untagged$year) 
sum_year <- cbind(sum_year, sum_year * 100 / n_ut)

## Month

sum_mon <- table(speech_df_untagged$month)
sum_mon <- cbind(sum_mon, sum_mon * 100 / n_ut)

## Length

words <- c(mean(speech_df_untagged$speech_words_stem), 
           sd(speech_df_untagged$speech_words_stem))

## Type

sum_type <- table(speech_df_untagged$doctype)
sum_type <- cbind(sum_type, sum_type * 100 / n_ut)

## Topic

sum_agg <- table(speech_df_untagged$pred_class)
sum_agg <- cbind(sum_agg, sum_agg * 100 / n_ut)

## Prep for Output

out3 <- rbind(sum_agg, sum_mon, sum_year,
              sum_type)
out3 <- round(out3, 1)
out3 <- cbind(rownames(out3), out3)

## Renaming

out3[29:31, 1] <- c("Article", "Press release", "Speech")
out3[8:19, 1] <-  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                     "Sep", "Oct", "Nov", "Dec")
out3[1:7, 1] <- c("Culture", "Econ. policy", 
                  "Education", "Environmental policy",
                    "Foreign policy", "Interior", "Social policy")

# To Table

stargazer(out3, summary = F, 
          rownames = F, 
          digits = 1, font.size = "footnotesize")

## Remove objects from memory

rm(sum_type, sum_year, sum_mon, sum_agg, sum_dagg, words, j ,n,
   cat_dict, out3, n_ut)

#### Figure A6 ####

## Get date of fielding and publication for all reports

surv_fielded <- ref_reports$end_date2
surv_published <- ref_reports$date

## Calculate time until publication

time_to_publ <- as.numeric(surv_published - surv_fielded)
time_to_publ <- ifelse(time_to_publ < 0, 1, time_to_publ)
time_to_publ <- data.frame(time = time_to_publ, source = ref_reports$source)

## Plot

p1 <- ggplot(time_to_publ, aes(x = time)) + 
  geom_density(fill = 'grey95') +
  geom_vline(xintercept = median(time_to_publ$time,
                                 na.rm = T),
             linetype = "dotted", color = "grey20") +
  xlab("Time between fielding and publication (days)") + 
  ylab("") + scale_x_continuous(breaks = seq(0, 140, 20)) +
  theme_bw()
p1

#### Figure A7 ####

## This justs splits the previous figure by source of opinion report

time_to_publ$source <- factor(time_to_publ$source)
levels(time_to_publ$source) <- c("Allensbach", 
                                 "Dimap", "Emnid", "FG",
                                 "GMS", "Polis", "TNS")

## Plot

p1 <- ggplot(time_to_publ, aes(x = time)) + 
  geom_density(fill = 'grey95') +
  xlab("Time between fielding and publication (days)") + 
  ylab("") +
  theme_bw() + facet_wrap(~ source) +
  scale_x_continuous(breaks = seq(0, 140, 20))
p1
