# ReadMe ----------------------------------------------------------------------------
# Project - Covid19 WB info campaign
# Author  - Vasu Chaudhary
# Purpose - Effects by content plot

# libraries -------------------------------------------------------------------------

library(tidyverse)
library(estimatr)
library(fixest)
library(broom)
library(modelsummary)
library(DescTools)
library(ggdist)
library(haven)
library(janitor)


# Set paths ----------------------------------------------------------------------

path_LM = "/Users/louis-maeljean/Dropbox (MIT)/West Bengal Information Campaign/AER_I/for_submission"
path = path_LM 

setwd(path)


# Loading Data ----------------------------------------------------------------------

df_main = read_dta("data/outcomes_reg_input.dta") ## 1989 obs


# Main computation ---------------------------------------------------------------------

df_main = df_main %>% 
  drop_na(jio_yn)

df_pins = read.csv("data/RANDOMIZEDTRT.csv")

gp = janitor::tabyl(df_main, treatment)
pncd = janitor::tabyl(df_pins, treatment)

pncd = pncd %>% 
  left_join(gp, by = "treatment") %>% 
  set_names(c("Type", "PIN code count", "PIN code share", "GP count", "GP share")) %>% 
  mutate(`GP count` = as.character(`GP count`),
         `PIN code count` = as.character(`PIN code count`, 0),
         `GP share` = round(`GP share`, 3),
         `PIN code share` = round(`PIN code share`, 3))

pncd$Type = c("Control", "Hyg + Ext + NO",
              "SD + Ext + NO",
              "Hyg + Int + NO",
              "SD+ Int + NO",
              "Hyg + Ext + Neut",
              "SD + Ext + Neut",
              "Hyg + Int + Neut",
              "SD + Int + Neut")

datasummary_df(pncd,
               output = "latex",
               fmt = 3) %>%
  save_kable("output/Tables/TableA22_GP_pincode_shares.tex")




############################
# END
############################

