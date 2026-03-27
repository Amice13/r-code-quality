# 02_Balance_Check.R
# Purpose: The Role of Public Broadcasting in Media Bias
# Created: 2019-9-24 Taka-aki Asano
# Last Modified: 2021-10-14

# This file is encoded in UTF-8.

# package
require("dplyr")
require("tidyr")
require("gtsummary")


# make variables
## female
exp$Q1[exp$Q1 == 9] <- NA
## college education
exp$College <- ifelse(exp$Q3 == 4, 1, 0)
## LDP supporter
exp$LDP <- ifelse(exp$Q7 == 1, 1, 0)
## assigned TV
exp$TV <- dplyr::recode(exp$TV, `NHK` = "NHK", `テレビ朝日` = "EX", `日本テレビ` = "NTV")
## control group
exp$Control <- ifelse(exp$Group1 == 1 | exp$Group2 == 1 | exp$Group3 == 1, 
                      "Control", "Pro-government Bias")
exp$Control <- factor(exp$Control, levels = c("Control", "Pro-government Bias"))


# summary statistics
describe <- exp %>% 
  dplyr::select(Q1, Q2, College, Q5_1, Q6_1, Q17, Q11_3, Q11_1, Q11_2, Q11_5, TV, Control) %>% 
  na.omit() %>%
  rename(Female = Q1, 
         Age = Q2, 
         `College Education` = College, 
         `R's Ideology` = Q5_1, 
         `Political Interest` = Q6_1, 
         `Frequency of Viewing TV` = Q17, 
         `Credibility of EX` = Q11_3, 
         `Credibility of NHK` = Q11_1, 
         `Credibility of NTV` = Q11_2, 
         `Credibility of Abe Administration` = Q11_5) %>% 
  unite(col = Group, sep = " - ", TV, Control) %>% 
  tbl_summary(by = Group, missing = "no", 
              type = list(`Political Interest` ~ "continuous", 
                          `Frequency of Viewing TV` ~ "continuous", 
                          `Credibility of EX` ~ "continuous", 
                          `Credibility of NHK` ~ "continuous", 
                          `Credibility of NTV` ~ "continuous", 
                          `Credibility of Abe Administration` ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  as_tibble()
## Table A.3
describe
