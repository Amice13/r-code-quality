########################################################
## This R-file produces Table 1 in SI
########################################################

## Packages
rm(list = ls())

install.packages(c("tidyverse", "xtable", "haven", "stats"))
library(tidyverse);library(xtable);library(haven);library(stats)

##
set.seed(123)
## set working directory as root = ""


dataRaw = read_dta("10_Data_analysis_final.dta",
                   col_select = c('party_11', 'win_again_ofall', 'voteshare16_ofall', 'won_nomination_ofall', 
                                  'run_again_ofall', 'ncand16_ofall', 'eff_ncand16_ofall', 'id', 
                                  'total_score_2013_2014', 'PA3_11', 'master_mandate', 'coun_NRM', 'coun_IND', 
                                  'coun_age', 'coun_asst_motor', 'coun_terms', 'coun_speaker', 'log_pop_ea', 
                                  'literacy_share_ea', 'elf_ea', 'poverty_census_ea', 'agr_share_ea')) %>% 
  filter(party_11!="INDEPENDENT") %>% select(-party_11)

outcomes = data.frame(variables = c('Won again', 'Vote share', 'Nomination', 
                                    'Run again', 'Number of candidates', 'Effective number of candidates'),
                      N = c(sum(!(is.na(dataRaw$win_again_ofall))), sum(!(is.na(dataRaw$voteshare16_ofall))), 
                            sum(!(is.na(dataRaw$won_nomination_ofall))), sum(!(is.na(dataRaw$run_again_ofall))), 
                            sum(!(is.na(dataRaw$ncand16_ofall))), sum(!(is.na(dataRaw$eff_ncand16_ofall)))),
                      Mean = c(round(mean(dataRaw$win_again_ofall, na.rm = TRUE),2), round(mean(dataRaw$voteshare16_ofall, na.rm = TRUE),2),
                               round(mean(dataRaw$won_nomination_ofall, na.rm = TRUE),2), round(mean(dataRaw$run_again_ofall, na.rm = TRUE),2),
                               round(mean(dataRaw$ncand16_ofall, na.rm = TRUE),2), round(mean(dataRaw$eff_ncand16_ofall, na.rm = TRUE),2)),
                      SD = c(round(stats::sd(dataRaw$win_again_ofall, na.rm = TRUE),2), round(stats::sd(dataRaw$voteshare16_ofall, na.rm = TRUE),2),
                             round(stats::sd(dataRaw$won_nomination_ofall, na.rm = TRUE),2), round(stats::sd(dataRaw$run_again_ofall, na.rm = TRUE),2),
                             round(stats::sd(dataRaw$ncand16_ofall, na.rm = TRUE),2), round(stats::sd(dataRaw$eff_ncand16_ofall, na.rm = TRUE),2)),
                      Min = c(round(min(dataRaw$win_again_ofall, na.rm = TRUE),2), round(min(dataRaw$voteshare16_ofall, na.rm = TRUE),2),
                              round(min(dataRaw$won_nomination_ofall, na.rm = TRUE),2), round(min(dataRaw$run_again_ofall, na.rm = TRUE),2),
                              round(min(dataRaw$ncand16_ofall, na.rm = TRUE),2), round(min(dataRaw$eff_ncand16_ofall, na.rm = TRUE),2)),
                      Max = c(round(max(dataRaw$win_again_ofall, na.rm = TRUE),2), round(max(dataRaw$voteshare16_ofall, na.rm = TRUE),2),
                              round(max(dataRaw$won_nomination_ofall, na.rm = TRUE),2), round(max(dataRaw$run_again_ofall, na.rm = TRUE),2),
                              round(max(dataRaw$ncand16_ofall, na.rm = TRUE),2), round(max(dataRaw$eff_ncand16_ofall, na.rm = TRUE),2)),
                      stringsAsFactors = FALSE)
treatment = data.frame(variables = c("Treatment"), N = c(sum(!(is.na(dataRaw$id)))),
                       Mean = c(round(mean(dataRaw$id, na.rm = TRUE),2)), SD = c(round(stats::sd(dataRaw$id, na.rm = TRUE),2)),
                       Min = c(round(min(dataRaw$id, na.rm = TRUE),2)), Max = c(round(max(dataRaw$id, na.rm = TRUE),2)),
                       stringsAsFactors = FALSE)
moderators = data.frame(variables = c('Scorecard (2013-2014)', 'Party Advantage'),
                        N = c(sum(!(is.na(dataRaw$total_score_2013_2014))), sum(!(is.na(dataRaw$PA3_11)))),
                        Mean = c(round(mean(dataRaw$total_score_2013_2014, na.rm = TRUE),2), round(mean(dataRaw$PA3_11, na.rm = TRUE),2)),
                        SD = c(round(stats::sd(dataRaw$total_score_2013_2014, na.rm = TRUE),2), round(stats::sd(dataRaw$PA3_11, na.rm = TRUE),2)),
                        Min = c(round(min(dataRaw$total_score_2013_2014, na.rm = TRUE),2), round(min(dataRaw$PA3_11, na.rm = TRUE),2)),
                        Max = c(round(max(dataRaw$total_score_2013_2014, na.rm = TRUE),2), round(max(dataRaw$PA3_11, na.rm = TRUE),2)),
                        stringsAsFactors = FALSE)

councilors = data.frame(variables = c('Mandate', 'Councillor is NRM in 2011', 'Councillor age', 
                                      'Councillor asset motor', 'Number of terms served as LC5 councilor', 'Councillor is a Speaker'),
                        N = c(sum(!(is.na(dataRaw$master_mandate))), sum(!(is.na(dataRaw$coun_NRM))), 
                              sum(!(is.na(dataRaw$coun_age))), sum(!(is.na(dataRaw$coun_asst_motor))), 
                              sum(!(is.na(dataRaw$coun_terms))), sum(!(is.na(dataRaw$coun_speaker)))),
                        Mean = c(round(mean(dataRaw$master_mandate, na.rm = TRUE),2), round(mean(dataRaw$coun_NRM, na.rm = TRUE),2),
                                 round(mean(dataRaw$coun_age, na.rm = TRUE),2), round(mean(dataRaw$coun_asst_motor, na.rm = TRUE),2),
                                 round(mean(dataRaw$coun_terms, na.rm = TRUE),2), round(mean(dataRaw$coun_speaker, na.rm = TRUE),2)),
                        SD = c(round(stats::sd(dataRaw$master_mandate, na.rm = TRUE),2), round(stats::sd(dataRaw$coun_NRM, na.rm = TRUE),2),
                               round(stats::sd(dataRaw$coun_age, na.rm = TRUE),2), round(stats::sd(dataRaw$coun_asst_motor, na.rm = TRUE),2),
                               round(stats::sd(dataRaw$coun_terms, na.rm = TRUE),2), round(stats::sd(dataRaw$coun_speaker, na.rm = TRUE),2)),
                        Min = c(round(min(dataRaw$master_mandate, na.rm = TRUE),2), round(min(dataRaw$coun_NRM, na.rm = TRUE),2),
                                round(min(dataRaw$coun_age, na.rm = TRUE),2), round(min(dataRaw$coun_asst_motor, na.rm = TRUE),2),
                                round(min(dataRaw$coun_terms, na.rm = TRUE),2), round(min(dataRaw$coun_speaker, na.rm = TRUE),2)),
                        Max = c(round(max(dataRaw$master_mandate, na.rm = TRUE),2), round(max(dataRaw$coun_NRM, na.rm = TRUE),2),
                                round(max(dataRaw$coun_age, na.rm = TRUE),2), round(max(dataRaw$coun_asst_motor, na.rm = TRUE),2),
                                round(max(dataRaw$coun_terms, na.rm = TRUE),2), round(max(dataRaw$coun_speaker, na.rm = TRUE),2)),
                        stringsAsFactors = FALSE)

counstituency = data.frame(variables = c('Population of the electoral area (Log)', 'Share of literacy in the electoral area', 
                                         'Ethnic-linguistic fractionalization in the electoral area', 
                                         'Poverty index in the electoral area', 'Agricultural share of the electoral area'),
                           N = c(sum(!(is.na(dataRaw$log_pop_ea))), sum(!(is.na(dataRaw$literacy_share_ea))), 
                                 sum(!(is.na(dataRaw$elf_ea))), sum(!(is.na(dataRaw$poverty_census_ea))), 
                                 sum(!(is.na(dataRaw$agr_share_ea)))),
                           Mean = c(round(mean(dataRaw$log_pop_ea, na.rm = TRUE),2), round(mean(dataRaw$literacy_share_ea, na.rm = TRUE),2),
                                    round(mean(dataRaw$elf_ea, na.rm = TRUE),2), round(mean(dataRaw$poverty_census_ea, na.rm = TRUE),2),
                                    round(mean(dataRaw$agr_share_ea, na.rm = TRUE),2)),
                           SD = c(round(stats::sd(dataRaw$log_pop_ea, na.rm = TRUE),2), round(stats::sd(dataRaw$literacy_share_ea, na.rm = TRUE),2),
                                  round(stats::sd(dataRaw$elf_ea, na.rm = TRUE),2), round(stats::sd(dataRaw$poverty_census_ea, na.rm = TRUE),2),
                                  round(stats::sd(dataRaw$agr_share_ea, na.rm = TRUE),2)),
                           Min = c(round(min(dataRaw$log_pop_ea, na.rm = TRUE),2), round(min(dataRaw$literacy_share_ea, na.rm = TRUE),2),
                                   round(min(dataRaw$elf_ea, na.rm = TRUE),2), round(min(dataRaw$poverty_census_ea, na.rm = TRUE),2),
                                   round(min(dataRaw$agr_share_ea, na.rm = TRUE),2)),
                           Max = c(round(max(dataRaw$log_pop_ea, na.rm = TRUE),2), round(max(dataRaw$literacy_share_ea, na.rm = TRUE),2),
                                   round(max(dataRaw$elf_ea, na.rm = TRUE),2), round(max(dataRaw$poverty_census_ea, na.rm = TRUE),2),
                                   round(max(dataRaw$agr_share_ea, na.rm = TRUE),2)),
                           stringsAsFactors = FALSE)

dat <- rbind(
  c("\\textbf{Outcomes}", rep(NA, ncol(outcomes) - 1)),
  outcomes,
  c("\\textbf{Treatment}", rep(NA, ncol(treatment) - 1)),
  treatment,
  c("\\textbf{Moderators}", rep(NA, ncol(moderators) - 1)),
  moderators,
  c("\\textbf{Councilor covariates}", rep(NA, ncol(councilors) - 1)),
  councilors,
  c("\\textbf{Counstituency covariates}", rep(NA, ncol(counstituency) - 1)),
  counstituency)

colnames(dat) <- sprintf("\\multicolumn{1}{c}{%s}", colnames(dat))

print.xtable(
  xtable(dat,
         caption = "Descriptive stats",
         align = c("l", "l", rep(">{$}r<{$}", 5))),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  sanitize.colnames.function = identity, type="latex", file="Table-SI1.tex")

