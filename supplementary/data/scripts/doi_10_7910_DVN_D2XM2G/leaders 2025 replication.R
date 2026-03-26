# Boris Shor, "Are State Legislative Leaders Moderates?" (2025)
# Legislative Studies Quarterly
# This script will reproduce all tables and figures from the main text and supplementary appendix

source("Code/leaders 2025 replication funcs.R")

library(reshape2)
library(stringr)
library(ggplot2)
library(lme4)
library(stargazer)
library(dplyr)
library(fs)

yearz = 1995:2023; nyearz = length(yearz)

file_info("Objects/leaderz 1989-2023.Rdata") %>% select(size,modification_time)
load("Objects/leaderz 1989-2023.Rdata")

nrow(topleaderz)
length(unique(topleaderz$k.id))

# descriptive data
do_descriptive_plots()
gen_averages()
gen_tables()

# MC simulations

leader_sims()

# models

leader_models()
model_rolls()

# candidates (for appendix)

leader_selection()
