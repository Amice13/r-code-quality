# ---------------------------------- TITLE -------------------------------------
# NAME: models_resubmission_appendix.R
# AUTHOR: Jacob Winter & Mark Manger
# DATE: October 2023
# ENCODING: utf-8
# ---------------------------------- NOTES -------------------------------------
# Creates all output tables and figures for the appendix
# ---------------------------------- SETUP -------------------------------------
dir = "~/wckm_wb_replication_package/"
setwd(dir)
sink("log_appendix.html")
library(tidyverse)
library(stargazer)
library(sandwich)
library(splines)

# load the script that creates the time splines with default values
source("btscs.R")

library(sampleSelection)
library(car)
options(scipen=999)

# ---------------------------------- DATA -------------------------------------
dir = "~/repos/wbenvironmentaltextsr/replication_package/"
setwd(dir)

# load the conditions data
p_merged <- read_csv("data/data_wb_conditions.csv")
# read the panel data for the first stage
panel <- read_csv("data/data_wb_panel.csv") %>%
  btscs(event="selected", tvar="year", csunit="iso3c") #Prep time splines

# ---------------------------------- MODELS -------------------------------------

set.seed(123)# keep this seed number so that the results of the first stage are the same as in the paper!
# first stage of the selection model

# Table 1: First Stage ####
panel_reserve <- panel |> 
  select(country_year, iso3c, selected, shift_share_replen, probability, replenishments_log, spell) |>
  drop_na()
myProbit_replen <- glm(selected ~ shift_share_replen +  probability + replenishments_log + bs(spell, knots=4)
                       ,family = binomial( link = "probit" ), data=panel_reserve)

panel_reserve$IMRreplen <- invMillsRatio(myProbit_replen)$IMR1 #Mills ratio
p_merged$IMRreplen <- panel_reserve$IMRreplen[match(p_merged$country_year,panel_reserve$country_year)] #Add mills ratio if exists

stargazer(myProbit_replen,
          type='html', star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          omit="Constant",
          covariate.labels= c("Probability x IDA replenishment",
                              "Probability", "IDA replenishment",
                              "Spline 1", "Spline 2", "Spline 3", "Spline 4" ),
          out="output/Appendix_table_1.doc")

#### Table 2: Alt Instruments ####

# Replenishment Full
panel_2 <- panel |> 
  select(country_year, iso3c, selected, shift_share_replen, probability, replenishments_log, 
         growth, inflation, log_gdp_pc, ieg_borr_filled, v2x_corr, v2x_polyarchy,
         spell) |>
  drop_na()

myProbit_replen_full <- glm(selected ~ shift_share_replen +  probability + replenishments_log + 
                              growth + inflation + log_gdp_pc + ieg_borr_filled + v2x_corr + v2x_polyarchy +
                              bs(spell, knots=4)
                            ,family = binomial( link = "probit" ), data=panel_2)

panel_2$IMRreplen_full <- invMillsRatio(myProbit_replen_full)$IMR1 #Mills ratio
p_merged$IMRreplen_full <- panel_2$IMRreplen_full[match(p_merged$country_year,panel_2$country_year)] #Add mills ratio if exists

#IBRD reserves
panel_3 <- panel |> 
  select(country_year, iso3c, selected, shift_share_reserves, probability, ibrd_reserves, 
         spell) |>
  drop_na()

myProbit_reserves <- glm(selected ~ shift_share_reserves +  probability + ibrd_reserves + 
                           bs(spell, knots=4)
                         ,family = binomial( link = "probit" ), data=panel_3)

panel_3$IMRreserves <- invMillsRatio(myProbit_reserves)$IMR1 #Mills ratio
p_merged$IMRreserves <- panel_3$IMRreserves[match(p_merged$country_year,panel_3$country_year)] #Add mills ratio if exists


# IBRD Reserves Full
panel_3 <- panel |> 
  select(country_year, iso3c, selected, shift_share_reserves, probability, ibrd_reserves, 
         growth, inflation, log_gdp_pc, ieg_borr_filled, v2x_corr, v2x_polyarchy,
         spell) |>
  drop_na()

myProbit_reserves_full <- glm(selected ~ shift_share_reserves +  probability + ibrd_reserves + 
                                growth + inflation + log_gdp_pc + ieg_borr_filled + v2x_corr + v2x_polyarchy +
                                bs(spell, knots=4)
                              ,family = binomial( link = "probit" ), data=panel_3)

panel_3$IMRreserves_full <- invMillsRatio(myProbit_reserves_full)$IMR1 #Mills ratio
p_merged$IMRreserves_full <- panel_3$IMRreserves_full[match(p_merged$country_year,panel_3$country_year)] #Add mills ratio if exists


# CPF Envelope 
panel_4 <- panel |> 
  select(country_year, iso3c, selected, envelope, 
         spell) |>
  drop_na()

my_probit_envelope <- glm(selected ~  envelope + 
                            bs(spell, knots=4)
                          ,family = binomial( link = "probit" ), data=panel_4)

panel_4$IMRenvelope <- invMillsRatio(my_probit_envelope)$IMR1 #Mills ratio
p_merged$IMRenvelope <- panel_4$IMRenvelope[match(p_merged$country_year,panel_4$country_year)] #Add mills ratio if exists


# CPF Envelope Full
panel_4 <- panel |> 
  select(country_year, iso3c, selected, envelope, 
         growth, inflation, log_gdp_pc, ieg_borr_filled, v2x_corr, v2x_polyarchy,
         spell) |>
  drop_na()

myProbit_envelope_full <- glm(selected ~  envelope + 
                                growth + inflation + log_gdp_pc + ieg_borr_filled + v2x_corr + v2x_polyarchy +
                                bs(spell, knots=4)
                              ,family = binomial( link = "probit" ), data=panel_4)

panel_4$IMRenvelope_full <- invMillsRatio(myProbit_envelope_full)$IMR1 #Mills ratio
p_merged$IMRenvelope_full <- panel_4$IMRenvelope_full[match(p_merged$country_year,panel_4$country_year)] #Add mills ratio if exists


models <- list(myProbit_replen, myProbit_replen_full, myProbit_reserves, myProbit_reserves_full, my_probit_envelope, myProbit_envelope_full)

stargazer(models,
          omit="Constant|growth|inflation|log_gdp_pc|ieg_borr_filled|v2x_corr|v2x_polyarchy|spell*",
          order = c("probability", "shift_share_replen", "replenishments_log", 
                    "shift_share_reserves", "ibrd_reserves", 
                    "envelope"),
          covariate.labels = c("Probability", "Shift Share Replenishment", "IDA Replenishments", 
                               "Shift Share Reserves", "IBRD Reserves", 
                               "CPF Envelope"),
          add.lines=list(
            c("Controls", "N","Y", "N", "Y", "N", "Y")),
          type='html',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F,
          notes.align="l",
          column.sep.width = "0pt",
          out="output/Appendix_table_2.doc"
)

#### Table 3: Alt outcomes ####
mod_IMR_replen_1 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                          urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                          cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                          growth + inflation + log_gdp_pc +
                          ieg_borr_filled + v2x_corr + v2x_polyarchy +
                          IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                        data=p_merged)

mod_IMR_replen_full_2 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                               urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                               cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                               growth + inflation + log_gdp_pc +
                               ieg_borr_filled + v2x_corr + v2x_polyarchy +
                               IMRreplen_full + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                             data=p_merged)

mod_IMR_reserves_3 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreserves + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

mod_IMR_reserves_full_4 <- lm(stringency ~ us_ideal_distance_important + unsc_member +
                                urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                                cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                                growth + inflation + log_gdp_pc +
                                ieg_borr_filled + v2x_corr + v2x_polyarchy +
                                IMRreserves_full + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                              data=p_merged)

mod_IMR_envelope_5 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRenvelope + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

mod_IMR_envelope_full_6 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                                 urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                                 cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                                 growth + inflation + log_gdp_pc +
                                 ieg_borr_filled + v2x_corr + v2x_polyarchy +
                                 IMRenvelope_full + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                               data=p_merged)
models_3 <- list(mod_IMR_replen_1, mod_IMR_replen_full_2, mod_IMR_reserves_3, mod_IMR_reserves_full_4, mod_IMR_envelope_5, mod_IMR_envelope_full_6)
errors_3 <- lapply(models_3, FUN = function(x)
  sqrt(diag(vcovBS(x, cluster = ~country_year, R=1000, type='wild'))
  )
)

varmap_3 <- c("UN Voting Distance (US)",
              "UNSC Membership",
              "Working Class Support",
              "Total Loan Size",
              "Final Fiscal Quarter",
              "IMF Program",
              "Number Loans 5Yrs",
              "Credit Rating",
              "Chinese lending",
              "IBRD",
              "Policy Lending",
              "Economic Growth",
              "Inflation",
              "ln GDP per capita",
              "IEG evaluations",
              "Corruption",
              "Democracy",
              "IMR Replenishment",
              "IMR Replenishment Full",
              "IMR Reserves",
              "IMR Reserves Full",
              "IMR Envelope",
              "IMR Envelope Full",
              "Post 2012",
              "Post 2005")

stargazer(models_3,
          se= errors_3,
          covariate.labels = varmap_3,
          omit=c("year", "iso3c", "topic_keyATM", "Constant"),
          omit.stat = c("adj.rsq", "ser"),
          add.lines=list(
            c("Country Effects", "Y","Y", "Y", "Y", "Y", "Y", "Y"),
            c("Year Effects", "N", "N","N", "N", "N", "N"),
            c("Sector Effects", "Y", "Y","Y", "Y", "Y", "Y")),
          type='html',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F,
          notes.align="l",
          column.sep.width = "0pt",
          out="output/Appendix_table_3.doc")

### Table 4: Alt Measures ####
# sum positive #
us_post0512_3_AT2_1 <-  lm(sum_positive ~ us_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

g5_post0512_AT2_2 <-  lm(sum_positive ~ g5_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

# sum upper quartile #
us_post0512_AT2_3 <-  lm(sum_upperq ~ us_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

g5_post0512_AT2_4 <-  lm(sum_upperq ~ g5_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

# sum negative #
us_post0512_AT2_5 <-  lm(sum_negative ~ us_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

g5_post0512_AT2_6 <-  lm(sum_negative ~ g5_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

# bottom quartile #
us_post0512_AT2_7 <-  lm(sum_lowerq ~ us_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)



g5_post0512_AT2_8 <-  lm(sum_lowerq ~ g5_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

# average stringency # 
us_post0512_AT2_9 <-  lm(stringency_average ~ us_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

g5_post0512_AT2_10 <-  lm(stringency_average ~ g5_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)


models_4 <- list(us_post0512_3_AT2_1, g5_post0512_AT2_2,
                 us_post0512_AT2_3, g5_post0512_AT2_4,
                 us_post0512_AT2_5, g5_post0512_AT2_6,
                 us_post0512_AT2_7, g5_post0512_AT2_8,
                 us_post0512_AT2_9, g5_post0512_AT2_10)

errors_4 <- lapply(models_4, FUN = function(x)
  sqrt(diag(vcovBS(x, cluster = ~country_year, R=1000, type='wild')))
)

varmap_4 <- c("UN Voting Distance, (US)",
             "UN Voting Distance, (G5)",
             "UNSC Membership",
             "Working Class Support",
             "Total Loan Size",
             "Final Fiscal Quarter",
             "IMF Program",
             "Number Loans 5Yrs",
             "Credit Rating",
             "Chinese lending",
             "IBRD",
             "Policy Lending",
             "Economic Growth",
             "Inflation",
             "ln GDP per capita",
             "IEG evaluations",
             "Corruption",
             "Democracy",
             "IMR",
             "Post 2012",
             "Post 2005")

stargazer(models_4,
          se = errors_4,
          covariate.labels = varmap_4,
          omit=c("year", "iso3c", "topic_keyATM", "Constant"),
          omit.stat = c("adj.rsq", "ser"),
          add.lines=list(
            c("Country Effects", "Y","Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
            c("Year Effects", "N", "N","N", "N", "N", "N", "N","N", "N", "N"),
            c("Sector Effects", "Y", "Y","Y", "Y", "Y", "Y", "Y", "Y", "Y","Y")),
          type='html',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F,
          notes.align="l",
          column.sep.width = "0pt",
          out="output/Appendix_table_4.doc"
)


#### Table 5 Alt specs ####

# condition count - replication of existing literature #
us_post0512_AT3_11 <-  lm(ALCID_number ~ us_ideal_distance_important + unsc_member +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            factor(iso3c),
                          data=p_merged)


g5_post0512_AT3_12 <-  lm(ALCID_number ~ g5_ideal_distance_important + unsc_member +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            factor(iso3c),
                          data=p_merged)

# Replace five-year project sum with total project sum, show this in the appendix 
us_post0512_AT3_13 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            project_count + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

g5_post0512_AT3_14 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            project_count + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

# WGI Corruption

us_post0512_AT3_15 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + wgi_corruption + v2x_polyarchy +
                            IMRreplen + post_05 + post_12 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)


g5_post0512_AT3_16 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + wgi_corruption + v2x_polyarchy +
                            IMRreplen + post_05 + post_12 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)


# post-2005 only

us_post0512_AT3_17 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged, subset= year >= 2005)

# Account balance
us_accnt_post0512_AT3_18 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                                  urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                                  cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                                  account_ln +
                                  growth + inflation + log_gdp_pc + 
                                  ieg_borr_filled + v2x_corr + v2x_polyarchy + 
                                  IMRreplen + post_05 + post_12 + factor(iso3c) + factor(topic_keyATM),
                                data=p_merged)

models_5 = list(us_post0512_AT3_11, g5_post0512_AT3_12, us_post0512_AT3_13, g5_post0512_AT3_14, us_post0512_AT3_15, 
                g5_post0512_AT3_16, us_post0512_AT3_17, us_accnt_post0512_AT3_18)

errors_5 =  lapply(models_5, FUN = function(x)
  sqrt(diag(vcovBS(x, cluster = ~country_year, R=1000, type='wild')))
)

varmap_5 <- c("UN Voting Distance (US)",
              "UN Voting Distance (G5)",
              "UNSC Membership",
              "Working Class Support",
              "Total Loan Size",
              "Final Fiscal Quarter",
              "IMF Program",
              
              "Num Loans All",
              "Number Loans 5Yrs",
              "Credit Rating",
              "Chinese lending",
              "IBRD",
              "Policy Lending",
              "Account Balance",
              "Economic Growth",
              "Inflation",
              "ln GDP per capita",
              "IEG evaluations",
              "Corruption",
              "Corruption, WGI",
              "Democracy",
              "IMR",
              "Post 2012",
              "Post 2005")

stargazer(models_5,
          se = errors_5,
          covariate.labels = varmap_5,
          omit=c("year", "iso3c", "topic_keyATM", "Constant"),
          omit.stat = c("adj.rsq", "ser"),
          add.lines=list(
            c("Country Effects", "Y","Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
            c("Year Effects", "N", "N","N", "N", "N", "N", "N","N", "N", "N"),
            c("Sector Effects", "Y", "Y","Y", "Y", "Y", "Y", "Y", "Y", "Y","Y")),
          type='html',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F,
          notes.align="l",
          column.sep.width = "0pt",
          out="output/Appendix_table_5.doc"
)
#### Table 6: alt principal ####

# all votes, not just "important" votes # 
us_post0512_AT4_19 <-  lm(stringency ~ us_ideal_distance + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

g5_post0512_AT4_20 <-  lm(stringency ~ g5_ideal + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

# lagged important votes

us_post0512_AT4_21 <-  lm(stringency ~ lag_usideal_i + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

g5_post0512_AT4_22 <-  lm(stringency ~ lag_g5_ideal_i + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)


# UNGA DeviatingFriends (Kilby 09)
us_post0512_AT4_23 <-  lm(stringency ~ unga_US_kilby09 + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

g7_post0512_AT4_24 <-  lm(stringency ~ unga_g7_kilby09 + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

# UNSC
us_post0512_AT4_25 <-  lm(stringency ~ unsc_align_drehervreeland + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)
# bilateral aid flows instead of UN voting distance

us_post0512_AT4_26 <-  lm(stringency ~ us_aid_flow + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

g5_post0512_AT4_27 <-  lm(stringency ~ g5_aid_flow + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

# troops #

troops_post0512_AT4_28 <-  lm(stringency ~ troops + unsc_member +
                                urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                                cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                                growth + inflation + log_gdp_pc +
                                ieg_borr_filled + v2x_corr + v2x_polyarchy +
                                IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                              data=p_merged)

# FDI

us_post0512_AT4_29 <-  lm(stringency ~ us_exposure + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)


g5_post0512_AT4_30 <-  lm(stringency ~ g5_exposure + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy +
                            IMRreplen + post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                          data=p_merged)

models_6 = list(us_post0512_AT4_19, g5_post0512_AT4_20, us_post0512_AT4_21, g5_post0512_AT4_22, us_post0512_AT4_23, 
                g7_post0512_AT4_24, us_post0512_AT4_25, us_post0512_AT4_26, g5_post0512_AT4_27, troops_post0512_AT4_28, 
                us_post0512_AT4_29, g5_post0512_AT4_30)

errors_6 = lapply(models_6, FUN = function(x)
  sqrt(diag(vcovBS(x, cluster = ~country_year, R=1000, type='wild')))
)

varmap_6 <- c("UN Distance, all Votes (US)",
              "UN Distance, all Votes (G5)",
              "Lagged UN Voting Distance (US)",
              "Lagged UN Voting Distance (G5)",
              "UNGA Deviating Friends (US)",
              "UNGA Deviating Friends (G7)",
              "UNSC Alignment",
              "Bilateral Aid (US)",
              "Bilateral Aid (G5)",
              "Troops",
              "FDI Exposure (US)",
              "FDI Exposure (G5)",
              "UNSC Membership",
              "Working Class Support",
              "Total Loan Size",
              "Final Fiscal Quarter",
              "IMF Program",
              "Number Loans 5Yrs",
              "Credit Rating",
              "Chinese lending",
              "IBRD",
              "Policy Lending",
              "Economic Growth",
              "Inflation",
              "ln GDP per capita",
              "IEG evaluations",
              "Corruption",
              "Democracy",
              "IMR",
              "Post 2012",
              "Post 2005")


stargazer(models_6,
          se = errors_6,
          covariate.labels = varmap_6,
          omit=c("year", "iso3c", "topic_keyATM", "Constant"),
          omit.stat = c("adj.rsq", "ser"),
          add.lines=list(
            c("Country Effects", "Y","Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y","Y","Y"),
            c("Year Effects", "N", "N","N", "N", "N", "N", "N","N", "N", "N","N","N"),
            c("Sector Effects", "Y", "Y","Y", "Y", "Y", "Y", "Y", "Y", "Y","Y", "Y", "Y")),
          type='html',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F,
          notes.align="l",
          column.sep.width = "0pt",
          out="output/Appendix_table_6.doc"
)

#### Table 7: Year FEs ####
us_i_yearfe <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                     urbandruralworking_d  + log_amt + fy_end_board + imf_program +
                     cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                     growth + inflation + log_gdp_pc +
                     ieg_borr_filled + v2x_corr + v2x_polyarchy +
                     factor(iso3c) + factor(topic_keyATM) + factor(year),
                   data=p_merged)

g5_i_yearfe <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                     urbandruralworking_d  + log_amt + fy_end_board + imf_program +
                     cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                     growth + inflation + log_gdp_pc +
                     ieg_borr_filled + v2x_corr + v2x_polyarchy +
                     factor(iso3c) + factor(topic_keyATM) + factor(year),
                   data=p_merged)

us_i_yearfe_t2 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                        urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                        cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                        growth + inflation + log_gdp_pc +
                        ieg_borr_filled + v2x_corr + v2x_polyarchy +
                        IMRreplen + 
                        factor(iso3c) + factor(topic_keyATM) + factor(year),
                      data=p_merged)

g5_i_yearfe_t2 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                        urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                        cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                        growth + inflation + log_gdp_pc +
                        ieg_borr_filled + v2x_corr + v2x_polyarchy +
                        IMRreplen + 
                        factor(iso3c) + factor(topic_keyATM) + factor(year),
                      data=p_merged)

models_7 <- list(us_i_yearfe, g5_i_yearfe, us_i_yearfe_t2, g5_i_yearfe_t2)

errors_7 <- lapply(models_7, FUN = function(x)
  sqrt(diag(vcovBS(x, cluster = ~country_year, R=1000, type='wild')))
)

varmap_7 <- c("UNGA Distance, (US, Important)",
             "UNGA Distance, (G5, Important)",
             "UNSC Membership",
             "Working Class Support",
             "Total Loan Size",
             "Final Fiscal Quarter",
             "IMF Program",
             "Number Loans 5Yrs",
             "Credit Rating",
             "Chinese lending",
             "IBRD",
             "Policy Lending",
             "Economic Growth",
             "Inflation",
             "ln GDP per capita",
             "IEG evaluations",
             "Corruption",
             "Democracy",
             "IMR")

stargazer(models_7,
          se= errors_7,
          covariate.labels = varmap_7,
          omit=c("year", "iso3c", "topic_keyATM", "Constant"),
          omit.stat = c("adj.rsq", "ser", 'f'),
          add.lines=list(
            c("Country and Sector Effects", "Y", "Y", "Y", "Y"),
            c("Year Effects", "Y", "Y", "Y", "Y")
            # c("Sector Effects", "N", "N", "N", "Y", "Y", "Y","Y", "Y", "Y", "Y", "Y")
          ),
          type='html',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          notes = "* p<0.05 ** p<0.01 *** p<0.001",
          notes.append = F,
          notes.align="r",
          column.sep.width = "0pt",
          out="output/Appendix_table_7.doc"
)


#### Figures in the Appendix ####
# Seed words - Figure 1 in the Appendix
# See separate script #

# Stringency at the project level by decile - Figure 2 in the Appendix
stringency_projectlevel <- read_csv("data/stringency_boxplot_data.csv")

stringency_boxplot <-  ggplot(stringency_projectlevel) +
  geom_boxplot(aes(x = stringency_dec, y = stringency_fit, fill= factor(stringency_dec)))  +
  labs(y = "Stringency of conditions",
       x = "Project stringency decile") +
  theme_minimal() +
  theme(legend.position = 'none')
stringency_boxplot
# write this plot to a pdf
ggsave("output/stringency_boxplot.pdf",
       width = 8, height = 6, units = "in", plot = stringency_boxplot)

# Stringency by sector - Figure 3 in the Appendix

stringency_sectorplot_data <- read_csv("data/stringency_sectorplot_data.csv")


# make a boxplot of stringency by the keyATM sector variable
stringency_by_sector <- ggplot(stringency_sectorplot_data) + 
  geom_boxplot(aes(x = topic_keyATM, y = stringency_sum, fill= factor(topic_keyATM))) +
  labs(y = "Stringency of conditions",
       x = "Sector") +
  # relabel the x axis
  scale_x_discrete(labels = c("X1_Financial_Reform" = "Financial\nReforms", 
                              "X2_Trade_and_Investment" = "Trade", 
                              "X3_Fiscal_Reform" = "Fiscal\nReform", 
                              "X4_Governance" = "Governance", 
                              "X5_Social_Spending" = "Social\nSpending", 
                              "X6_Borrower_Ownership" = "Borrower\nOwnership")) +
  theme_minimal() +
  theme(legend.position = 'none')

stringency_by_sector

ggsave("output/stringency_sectoral_boxplot.pdf",
       width = 8, height = 6, units = "in", plot = stringency_by_sector)

#### UN Variation Over Time ####
fig_unvariation <- p_merged |> 
  group_by(iso3c) |>
  mutate(ave_ideal = mean(us_ideal_distance_important, na.rm=T),
         ave_sd = sd(us_ideal_distance_important, na.rm=T),
         region = countrycode::countrycode(iso3c, "iso3c", "continent"),
         region = ifelse(region %in% c('Europe',"Oceania"),"Europe/Oceania", region)
  ) |> 
  ungroup() |>
  ggplot() +
  geom_line(aes(x=year, y=us_ideal_distance_important, 
                color=ave_ideal,
                group=iso3c
  ), alpha=.5) +
  scale_color_viridis_c() +
  theme_minimal() +
  theme(legend.position='none')  +
  facet_wrap(~region) +
  labs(x="Year",
       y = "US Ideal Distance, Important Votes",
       title="Variation in US Ideal Distance by Region")

ggsave(plot = fig_unvariation, "output/un_variation.pdf", width=6, height=6)

sink()
