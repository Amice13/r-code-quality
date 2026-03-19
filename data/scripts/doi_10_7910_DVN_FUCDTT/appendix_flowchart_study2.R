
## This is code to take the full dataset and produce the numbers in the
## flowchart for study 2 in the appendix

library(tidyverse)
library(haven)
library(data.table)

## Can set your working directory here to the folder
setwd("~/Dropbox/GKMR/PapersPresentations/2020Submission/Additions_PostReplication/code")

## Read in the pre and post data and the one used in the replication
s02post <- read_csv("stud02_post.csv") %>% as.data.table()
s02pre <- read_csv("stud02_pre.csv") %>% as.data.table()

## Change names to allow distinguishing pre- from post- variables
setnames(s02pre, paste0("pre_", names(s02pre)))
setnames(s02pre, "pre_Voter ID", "Voter_ID") #but keep the merging var the same

## Remove the one duplicated respondent
s02pre <- s02pre[!(duplicated(Voter_ID))]

## Number invited to the wave 2 survey
nrow(s02pre)
## [1] 3623

## Combine
setkey(s02pre, "Voter_ID")
setkey(s02post, "Voter_ID")
s02comb <- s02pre[s02post]

## Create the treatment condition variable
## First for those in control
myvars <- str_subset(names(s02comb), "PositiveLegal")
myvars
## [1] "FL_238_DO_PositiveLegal" "FL_268_DO_PositiveLegal"
## [3] "FL_265_DO_PositiveLegal" "FL_262_DO_PositiveLegal"
s02comb[, condition := NA_integer_]
s02comb[rowSums(s02comb[, ..myvars], na.rm = TRUE) == 1, condition := 0]
## Now for those in treatment
myvars <- str_subset(names(s02comb), "PositiveIllegal")
## [1] "FL_238_DO_PositiveIllegal" "FL_268_DO_PositiveIllegal"
## [3] "FL_265_DO_PositiveIllegal" "FL_262_DO_PositiveIllegal"
s02comb[rowSums(s02comb[, ..myvars], na.rm = TRUE) == 1, condition := 1]

## Number that didn't respond is difference between the pre and combined surveys
nrow(s02pre)
## [1] 3623
nrow(s02comb)
## [1] 2632
nrow(s02pre) - nrow(s02comb)
## [1] 991

## There are also people in this dataset who finished/started taking the survey
## after it closed; eliminate those here
nrow(s02comb)
## [1] 2632
s02comb <- s02comb[as.POSIXct(EndDate) < as.POSIXct("2015-09-16 16:05:00 MDT")]
nrow(s02comb)
## [1] 2399
2632 - 2399   #233 removed for this reason

## How many in each condition now?
table(s02comb$condition, useNA = "ifany")
##    0    1 <NA>
## 1065 1062  272

## Remove the 272 respondents who were never assigned to a treatment condition
s02comb <- s02comb[!is.na(condition)]
nrow(s02comb) #new n-size is 2127

## Who did not finish the survey?
table(s02comb$condition, s02comb$Finished, useNA = "ifany")
##      0    1
## 0    8 1057
## 1    9 1053
## 8 in control and 9 in treatment that didn't finish

## Remove those who did not finish the survey
s02comb <- s02comb[Finished == 1]
nrow(s02comb) #new n-size is 2110

## What about non-whites/Latinos?
## In this raw dataset, this is asked in the question pre_Q58
table(s02comb$pre_Q58, useNA = "ifany")
## 1    2    3    4    5    6    7    8 <NA>
## 3   10    3   44 1977   14   14   40    5

## Everyone labeled as a 5 is white, so look at 5 vs other
table(s02comb$condition, as.integer(s02comb$pre_Q58 == 5), useNA = "ifany")
##     0   1 <NA>
## 0  74 978    5
## 1  54 999    0
## 74 in control and 54 in treatment that are non-white or Latino - for the NAs,
## keep those in (which in effect assumes that they are white)

## Remove those who are non-white or Latino
s02comb <- s02comb[!(pre_Q58 %in% c(1:4, 6:8))]
nrow(s02comb) #new n-size is 1982
table(s02comb$condition, useNA = "ifany")
##   0   1
## 983 999
## This gives us numbers that correspond to the final row of the flowchart
## in the appendix
