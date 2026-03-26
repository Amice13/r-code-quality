## Run below code to reproduce results for appendix and manuscript. 
## Please see each individual script for additional details. 

## Use devtools to install stdidx and autumn packages if not already installed:
## install.packages("devtools")
#devtools::install_github("graemeblair/stdidx")
#devtools::install_github("aaronrudkin/autumn")
suppressMessages({
  library(tidyverse) 
  library(lubridate)
  library(estimatr)
  library(coefplot)
  library(ggthemes)
  library(ggforce)
  library(haven)
  library(lemon)
  library(cowplot)
  library(grid)
  library(autumn)
  library(xtable)
  library(ri2)
  library(cobalt)
  library(gridExtra)
  library(Hmisc)
  library(corrplot)
  library(stdidx)
})

## Helper functions needed to run the replication code below. 
source("helpers.R")

## 1. Replication code for manuscript. Note that this script does not source
##    any other scripts and should execute quickly on most machines. However,
##    it is recommended to open this script in a separate window and iterate 
##    over each code chunk, otherwise the miscellaneous analyses reported in 
##    the text (see line 826) will not print to the console 
source("replicate_manuscript.R")

## 2. Replication code for Supporting Information (SI) appendix.
##    Note that replicate_si_appendix.R sources other scripts (enumerated below)
##    to produce a total of 75 figures and 4 tables (plus .CSV exports of the 
##    underlying estimates). Running this single script is a computationally 
##    intensive process. Rather than execute this directly in one call, it is 
##    instead recommended to open this script in a separate window and iterate 
##    over each code chunk. Depending on your machine the three scripts sourced 
##    in the chunk of code at line 207 (for Fig. S21-S68) may need to be 
##    executed piecemeal for the exported PDF figures to render properly. 
source("replicate_si_appendix.R")

## Description of scripts called by replicate_si_appendix.R:
## 1. si_corrplots.R produces Fig. S1-S2 
## 2. si_wts_lucid.R produces Fig. S3-S6
## 3. si_wts_norc.R produces Fig. S7-S9, Table S3
## 4. si_causal_interact_lucid.R produces Fig. S10-S12
## 5. si_conjoint_donate_norc.R produces Fig. S13-S14, Table S4 
## 6. si_conjoint_ordinal.R produces Fig. S15-S20
## 7. si_conjoint_hetfx_p_lucid.R produces Fig. S21-S38
## 8. si_conjoint_hetfx_a_lucid.R produces Fig. S39-S56
## 9. si_conjoint_hetfx_a_norc.R produces S57-S68
## 10. si_causal_forest.R runs the causal forest algorithm that produces the 
##     estimated treatment effects that are plotted in Fig. S69-S75. This
##     may be slow depending on your hardward (see line 222 in 
##     replicate_si_appendix.R for additional details)

## Description of datasets included in the replication archive: 

## 1. lucid_survey.rds: data from Lucid survey and conjoint experiments.
## 2. lucid_conjoint_person.rds: choices and ratings from Lucid conjoint on 
##    potential vaccine recipients in long format. 
## 3. lucid_conjoint_agreement.rds: choices and ratings from Lucid conjoint on 
##    potential agreements in long format. 
## 4. norc_survey.rds: data from NORC survey, persuasion experiment and conjoint
## 5. norc_conjoint.rds: choices and ratings from NORC conjoint on potential 
##    agreements in long format. 
## 6. census_targets.rds: census marginals used to create Table S2. 
## 7. grf_estimates.rds: causal forest estimated treatment effects produced by
##    si_causal_forest.R (see line 222 in replicate_si_appendix.R for additional 
##    details)






