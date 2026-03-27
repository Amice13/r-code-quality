# ReadMe ----------------------------------------------------------------------------
# Project - Covid19 WB info campaign
# Author  - Vasu Chaudhary - Louis-Mael Jean
# Purpose - attrition analysis for outcomes survey



# Libraries -----------------------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(estimatr)
library(car)

select = dplyr::select

# Set path -----------------------------------------------------------------------------------
path=DRIVE
setwd(path)

# INTERNAL NOTE -----------------------------------------------------------------------------------

# This is the code used to produce the data for attrition table A21
# The datasets used in the processing part contain PII hence are protected from veracrypt encrypted folders


# Data was mounted from encrypted veracrypt folder first (PII), ask relevant RA for password

# df_outcomes = read_csv("A:/1_Main/covid19_outcomes_v2_WIDE.csv") %>%
#   #select(contacts, status_1, pincode)
# 
# write_csv(df_outcomes, "../../../2_Data/df_outcomes_attrition.csv")
# 
# df_rand = read_csv("data/surveyorrandinput.csv") %>%
#   select(district, mobile, treatment)
# 
# df_outcomes = read_csv("data/df_outcomes_attrition.csv")
# 
# # Processing ----------------------------------------------------------------------------------
# 
# ## keeping unique call status for each uid (1 means completed) 
# df_outcomes = df_outcomes %>% 
#   filter(status_1 != -333) %>% 
#   group_by(contacts) %>%
#   filter(status_1 == min(status_1)) %>% 
#   distinct(status_1, .keep_all = T) %>% 
#   mutate(contacts = as.numeric(contacts))
# 
# df_status = df_outcomes %>% 
#   left_join(df_rand, by = c("contacts" = "mobile"))
# 
# df_status = df_status %>% 
#   mutate(Treatment = ifelse(treatment == 0, 0, 1)) %>% 
#   mutate(completed = ifelse(status_1 == 1, 1, 0)) %>%
#   ungroup() %>%
#   dplyr::select(-contacts)
# 
# write_csv(df_status, "data/df_outcomes_call_status.csv")

# END INTERNAL NOTE -----------------------------------------------------------------------------------




# loading data --------------------------------------------------------------------------------

df_status = read_csv("data/df_outcomes_call_status.csv")


# Outputs -------------------------------------------------------------------------------------

df_status %>% 
  group_by(treatment) %>%
  summarise(share_calls_completed = sum(completed)/n())


## regression 
reg = list()

reg[["Call Completed"]] = lm_robust(completed ~ factor(treatment), data = df_status)

aa = linearHypothesis(reg[[1]], c("factor(treatment)1 = 0",
                             "factor(treatment)2 = 0",
                             "factor(treatment)3 = 0",
                             "factor(treatment)4 = 0",
                             "factor(treatment)5 = 0",
                             "factor(treatment)6 = 0",
                             "factor(treatment)7 = 0",
                             "factor(treatment)8 = 0"),
                 test = "F")

row = tribble(~Var, ~main,
                  "pval F-test", aa$`Pr(>F)`[2])

attr(row, 'position') = 25

gm = gof_map
gm$clean[gm$raw == "nobs"] = "N"

modelsummary(reg,
             output = "output/Tables/TableA21_gp_attrition.tex",
             statistic = c('({std.error})', '[{p.value}]'),
             coef_map = c("factor(treatment)1" = "Hyg + Ext + NO",
                          "factor(treatment)2" = "SD + Ext + NO",
                          "factor(treatment)3" = "Hyg + Int + NO",
                          "factor(treatment)4" = "SD + Int + NO",
                          "factor(treatment)5" = "Hyg + Ext + Neut",
                          "factor(treatment)6" = "SD + Ext + Neut",
                          "factor(treatment)7" = "Hyg + Int + Neut",
                          "factor(treatment)8" = "SD + Int + Neut"),
             add_rows = row,
             gof_map = gm,
             gof_omit="[^Num.Obs.]")



#############################
########### END ############
#############################
