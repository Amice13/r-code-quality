# Preference Trial Experiment Analysis
# Thomas J. Leeper
# Northwestern University/Aarhus University/London School of Economics
# 2018-06-25

#setwd("C:/Users/Thomas/Dropbox/KnowledgeGaps")

library("car")
library("coin")
library("xtable")
library("eRm") # for Rasch model appendix
library("GK2011") # for Gaines/Kuklinski estimators (in appendix)
source("Analysis/functions.R")

# recoding
source("Analysis/experiment_cleaning.R")
.groupNames <- c("Hard-News","Soft-News","Entertainment-News",
                 "Hard-Entertainment","Soft-Entertainment","Entertainment-Entertainment",
                 "None-News","None-Entertainment")

# basic demographics
source("Analysis/experiment_demographics.R", echo = TRUE)

## relationship between choices and preferences
## & marginal effect of treatment across levels of self-reported preference differential
source("Analysis/experiment_choices.R")

## Main analysis: hard/soft conditions combined
source("Analysis/experiment_knowledge.R") # hard/soft combined

## Appendix: Item-specific results
source("Analysis/experiment_appendix_item.R")
## Appendix: Don't know responses to knowledge questions
## Appendix: IRT model of scaled outcome
source("Analysis/experiment_appendix_rasch.R")
## Appendix: Gaines--Kuklinski Estimators
source("Analysis/experiment_gaines_kuklinski.R")
## Appendix: randomization check by choiceset
source("Analysis/experiment_appendix_check.R")

## Appendix: preference stability
source("Analysis/experiment_stability.R")
## Appendix: preferences and habit measures
source("Analysis/experiment_habits.R")
## Appendix: factor analysis
source("Analysis/experiment_factoranalysis.R")

## Appendix: hard/soft conditions separated
source("Analysis/experiment_appendix_separated.R")
## Appendix: other outcomes (hard/soft separated; hard/soft combined)
source("Analysis/experiment_appendix_attitudes.R")
source("Analysis/experiment_appendix_certainty.R")

# END OF ANALYSIS #
