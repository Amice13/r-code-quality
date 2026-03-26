#' ---
#' title: "Legislating Landlords: Private Interests, Issue Emphasis, and Policy Positions (LSQ)"
#' subtitle: "04b_analysis_salience.R"
#' author: "Authors: Stefan Mueller and Jihed Ncib"
#' date: "Note: Code compiled successfully on `r format(Sys.time(), '%d %B %Y')`"
#' ---

# script to merge and harmonise the text corpora and datasets on legislators
# and their interests

# If the code does not run, one or more packages may have been 
# updated, which may result in errors or conflicts. You can solve this issue
# by installing the package version listed above or by using the 
# groundhog package:
# after installing groundhog using install.packages("groundhog")
# change library(name_of_package) to
# groundhog::groundhog.library(name_of_package, date = "2024-05-06")
# Instead of adjusting the library() function for each package, 
# you can adjust them at all once using the
# the following syntax:
# groundhog.library("library('pkgA')
#                   library('pkgB')
#                   library('pkgC')", date = "2024-05-06")
# More details are available at: https://groundhogr.com/using/


# load packages and classify texts

library(tidyverse)       # CRAN v2.0.0 
library(broom)           # CRAN v1.0.5
library(forcats)         # CRAN v1.0.0
library(texreg)          # CRAN v1.38.6
library(xtable)          # CRAN v1.8-4
library(arrow)           # CRAN v14.0.0.2
library(fixest)          # CRAN v0.11.2
library(marginaleffects) # CRAN v0.18.0
library(cowplot)         # CRAN v1.1.1
library(scales)          # CRAN v1.2.1

source("function_theme_base.R")

# load full, clean, classified dataset

# read parquet file
dat_analysis <- arrow::read_parquet("data_dontshare/data_analysis_classified_housing.parquet")

nrow(dat_analysis)


dat_analysis |> 
    filter(!is.na(ownership_2)) |> 
    nrow()

# remove observations without information regarding ownership_2,
# between 2013 and 2022, 
dat_analysis <- dat_analysis |> 
    mutate(year = as.integer(year)) |> 
    filter(year %in% 2013:2022) |> 
    filter(!is.na(ownership_2))

nrow(dat_analysis)

table(dat_analysis$type)


length(unique(dat_analysis$mpname))


# check the housing DistilBERT prediction is in the dataset
table(dat_analysis$housing_bert)

# make housing_bert an integer

dat_analysis <- dat_analysis |> 
    mutate(housing_bert = dplyr::recode(housing_bert, "Housing" = 1, 
                                        "Non-housing" = 0)) |> 
    mutate(housing_bert = as.integer(housing_bert))

table(dat_analysis$housing_bert)


table(dat_analysis$housing_bert)

# table(dat_analysis$housing_bert,
#       dat_analysis$housing_dummy)


table(dat_analysis$propnum,
      dat_analysis$year)

dat_analysis <- dat_analysis |> 
    mutate(cycle = case_when(
        year %in% 2011:2015 ~ "2013-2016",
        year %in% 2016:2019 ~ "2016-2020",
        year %in% 2020:2025 ~ "2020-2022"
    ))


# calculate change became ownner

dat_analysis_change <- dat_analysis |> 
    dplyr::select(mpname, harmonised, year, ownership_2) |> 
    arrange(mpname, year) |> 
    unique() |> 
    group_by(mpname) |> 
    mutate(ownership_2_lag = lag(ownership_2)) |> 
    mutate(change_became_owner = ifelse(ownership_2 == "Homeowner or Landlord" & ownership_2_lag == "Not Homeowner or Landlord", 1, 0))


dat_analysis <- dat_analysis |> 
    dplyr::select(-change_became_owner) |> 
    left_join(dat_analysis_change)


dat_analysis <- dat_analysis |> 
    mutate(year = as.integer(year)) |> 
    filter(year %in% 2013:2022) |> 
    mutate(housing_committee = ifelse(is.na(housing_committee), 0, 1)) |>
    mutate(age = as.integer(year) - as.integer(birth),
           seniority_years = as.integer(seniority) / 365) 

table(dat_analysis$ownership_3,
      dat_analysis$ownership_2)

table(dat_analysis$ownership_2)

# table(dat_analysis$housing_dummy)
# 
# table(dat_analysis$housing_dummy,
#       dat_analysis$type)


table(dat_analysis$type)

table(dat_analysis$ownership_2,
      dat_analysis$year)

table(dat_analysis$year)

dat_analysis |> 
    filter(!is.na(ownership_2)) |> 
    group_by(harmonised, year) |> 
    summarise(mean_mp = mean(housing_bert)) |> 
    ungroup() |> 
    summarise(mean = mean(mean_mp),
              median = median(mean_mp),
              sd = sd(mean_mp, na.rm = TRUE))

# get summary stats for paper
dat_analysis |> 
    filter(!is.na(ownership_2)) |> 
    group_by(harmonised, year, type) |> 
    summarise(mean_mp = mean(housing_bert)) |> 
    group_by(type) |> 
    summarise(mean = mean(mean_mp),
              median = median(mean_mp),
              sd = sd(mean_mp, na.rm = TRUE))

dat_analysis |> 
    filter(!is.na(ownership_2)) |> 
    group_by(type) |> 
    filter(housing_bert == 1) |> 
    count()

# get summary stats for paper
dat_analysis |> 
    filter(!is.na(ownership_2)) |> 
    group_by(type) |> 
    summarise(mean = mean(housing_bert),
              sd = sd(housing_bert, na.rm = TRUE))


table(dat_analysis$housing_bert,
      dat_analysis$type)


# create age groups

dat_analysis <- dat_analysis |> 
    mutate(age_group = case_when(
        age %in% 18:34 ~ "18-34",
        age %in% 35:44 ~ "35-44",
        age %in% 45:54 ~ "45-54",
        age %in% 55:63 ~ "55-64", 
        age > 64 ~ "65+"
    ))


# calculate proportions after carrying over important
# contextual variables

dat_complete_prop <- dat_analysis |> 
    group_by(type, term, gov_opp,
             dublin_dummy, cycle,
             year, age, age_group, 
             seniority, birth,
             harmonised,
             housing_committee,
             party_recoded, party_broad,
             change_became_owner,
             ownership_2, ownership_3, 
             propnum,
             mean_price, median_price,
             perc_change_mean,
             perc_change_median,
             change_ownership) |> 
    summarise(prop_housing = mean(housing_bert),
              count_texts = n(),
              count_texts_housing = sum(housing_bert)) |> 
    #filter(count_texts >= 2) |> 
    ungroup() # keep only TDs with at least 5 tweets / 5 questions per year



dat_complete_prop_all <- dat_analysis |> 
    group_by(term, gov_opp,
             dublin_dummy, cycle, 
             year, age, age_group, 
             seniority, birth,
             harmonised,
             housing_committee,
             party_recoded, party_broad,
             change_became_owner,
             ownership_2, ownership_3, 
             propnum,
             mean_price, median_price,
             perc_change_mean, perc_change_median,
             change_ownership) |> 
    summarise(prop_housing = mean(housing_bert),
              count_texts = n(),
              count_texts_housing = sum(housing_bert)) |> 
    #filter(count_texts >= 2) |> 
    ungroup() # keep only TDs with at least 5 tweets / 5 questions per year

dat_summary_stats <- dat_analysis |> 
    group_by(harmonised, year) |> 
    summarise(prop_housing = mean(housing_bert),
              count_texts_housing = sum(housing_bert),
              count_texts = n()) |> 
   # filter(count_texts >= 2) |> 
    group_by(year) |> 
    summarise(Min = min(prop_housing) * 100,
              Max = max(prop_housing) * 100,
              Mean = mean(prop_housing) * 100, 
              Median = median(prop_housing) * 100,
              SD = sd(prop_housing) * 100,
              Docs = as.integer(sum(count_texts)),
              `Docs (Housing)` = as.integer(sum(count_texts_housing)),
              `N TDs` = length(unique(harmonised))) |> 
    rename(Year = year)



dat_summary_stats_all <- dat_analysis |> 
    group_by(harmonised, year) |> 
    summarise(prop_housing = mean(housing_bert),
              count_texts_housing = sum(housing_bert),
              count_texts = n()) |> 
   # filter(count_texts >= 2) |> 
    ungroup() |> 
    summarise(Min = min(prop_housing) * 100,
              Max = max(prop_housing) * 100,
              Mean = mean(prop_housing) * 100, 
              Median = median(prop_housing) * 100,
              SD = sd(prop_housing) * 100,
              Docs = as.integer(sum(count_texts)),
              `Docs (Housing)` = as.integer(sum(count_texts_housing)),
              `N TDs` = length(unique(harmonised)))


nrow(dat_analysis)


dat_summary_stats_all_type <- dat_analysis |> 
    group_by(type, harmonised, year) |> 
    summarise(prop_housing = mean(housing_bert),
              count_texts_housing = sum(housing_bert),
              count_texts = n()) |> 
   # filter(count_texts >= 2) |> 
    ungroup() |> 
    group_by(type) |> 
    summarise(Min = min(prop_housing) * 100,
              Max = max(prop_housing) * 100,
              Mean = mean(prop_housing) * 100, 
              Median = median(prop_housing) * 100,
              SD = sd(prop_housing) * 100,
              Docs = as.integer(sum(count_texts)),
              `Docs (Housing)` = as.integer(sum(count_texts_housing)),
              `N TDs` = length(unique(harmonised))) 


dat_summary_stats_type <- dat_complete_prop |> 
   #  filter(count_texts >= 2) |> 
    group_by(year, type) |> 
    summarise(Min = min(prop_housing) * 100,
              Max = max(prop_housing) * 100,
              Mean = mean(prop_housing) * 100, 
              Median = median(prop_housing) * 100,
              SD = sd(prop_housing) * 100,
              Docs = as.integer(sum(count_texts)),
              `Docs (Housing)` = as.integer(sum(count_texts_housing)),
              `N TDs` = length(unique(harmonised))) |> 
    rename(Type = type, Year = year)


# create table with summary statistics
dat_summary_stats$Year =factor(dat_summary_stats$Year)

print(xtable(dat_summary_stats,
             caption.placement = "top",
             digits = 1,
             label = "tab:summary",
             caption = "Summary statistics of issue emphasis on housing"),
      include.rownames = FALSE,
      format.args = list(big.mark = ",", decimal.mark = "."),
      type = "latex",
      digits = 1,
      caption.placement = "top",
      size = "footnotesize",
      file = "tab_a01.tex")

print(xtable(dat_summary_stats,
             caption.placement = "top",
             digits = 1,
             label = "tab:summary",
             caption = "Summary statistics of issue emphasis on housing"),
      include.rownames = FALSE,
      format.args = list(big.mark = ",", decimal.mark = "."),
      type = "html",
      digits = 1,
      caption.placement = "top",
      size = "footnotesize",
      file = "tab_a01.html")

dat_summary_stats_type$Year =factor(dat_summary_stats_type$Year)

print(xtable(dat_summary_stats_type,
             caption.placement = "top",
             digits = 1,
             label = "tab:summary_type",
             caption = "Summary statistics of issue emphasis on housing (separately for Tweets and parliamentary questions)"),
      include.rownames = FALSE,
      format.args = list(big.mark = ",", decimal.mark = "."),
      type = "latex",
      caption.placement = "top",
      size = "footnotesize",
      file = "tab_a02.tex")

print(xtable(dat_summary_stats_type,
             caption.placement = "top",
             digits = 1,
             label = "tab:summary_type",
             caption = "Summary statistics of issue emphasis on housing (separately for Tweets and parliamentary questions)"),
      include.rownames = FALSE,
      format.args = list(big.mark = ",", decimal.mark = "."),
      type = "html",
      caption.placement = "top",
      size = "footnotesize",
      file = "tab_a02.html")


# get overview across entire sample
dat_analysis |> 
    group_by(type) |> 
    summarise(prop_housing = mean(housing_bert),
              count_texts_housing = sum(housing_bert),
              count_texts = n())


dat_complete_prop$dublin_dummy <- factor(dat_complete_prop$dublin_dummy,
                                         levels = c("Other", "Dublin"))


dat_complete_prop_all$dublin_dummy <- factor(dat_complete_prop_all$dublin_dummy,
                                             levels = c("Other", "Dublin"))


dat_complete_prop$party_recoded <- relevel(factor(dat_complete_prop$party_recoded),
                                           ref = "Other/Independents")

dat_complete_prop_all$party_recoded <- relevel(factor(dat_complete_prop_all$party_recoded),
                                               ref = "Other/Independents")


dat_complete_prop <- dat_complete_prop |> 
    arrange(harmonised, year) |> 
    group_by(harmonised) |> 
    mutate(prop_housing_lag = lag(prop_housing)) |> 
    mutate(change_housing = prop_housing - lag(prop_housing)) |> 
    ungroup()

# run initial regression models

dat_complete_prop <- dat_complete_prop |>
    mutate(perc_housing = prop_housing * 100) |> 
    mutate(seniority_years = as.integer(seniority) / 365)


dat_complete_prop_all <- dat_complete_prop_all |> 
    mutate(perc_housing = prop_housing * 100) |> 
    mutate(seniority_years = as.integer(seniority) / 365)




dat_complete_prop_all <- dat_complete_prop_all |> 
    ungroup() |> 
    mutate(perc_housing_stand = (perc_housing - mean(perc_housing, na.rm = TRUE)) / sd(perc_housing, na.rm = TRUE)) |> 
    mutate(propnum_recoded = ifelse(propnum >= 4, "4+", propnum)) |> 
    mutate(propnum_recoded = as.factor(propnum_recoded)) |> 
    mutate(propnum_dummy = factor(ifelse(propnum > 0 , 1, 0))) |> 
    mutate(ownership_3 = relevel(factor(ownership_3), ref = "Not Homeowner or Landlord")) |> 
    mutate(ownership_2 = relevel(factor(ownership_2), ref = "Not Homeowner or Landlord")) |> 
    group_by(cycle) |> 
    mutate(perc_housing_stand_cycle = (perc_housing - mean(perc_housing, na.rm = TRUE)) / sd(perc_housing, na.rm = TRUE)) |> 
    ungroup()
    
# make sure the standard deviation of standardised variable is 1
sd(dat_complete_prop_all$perc_housing_stand)


dat_complete_prop <- dat_complete_prop |> 
    group_by(type) |> 
    mutate(perc_housing_stand = (perc_housing - mean(perc_housing, na.rm = TRUE)) / sd(perc_housing, na.rm = TRUE)) |> 
    mutate(propnum_recoded = ifelse(propnum >= 4, "4+", propnum)) |> 
    mutate(propnum_recoded = as.factor(propnum_recoded)) |> 
    mutate(propnum_dummy = factor(ifelse(propnum > 0 , 1, 0))) |> 
    mutate(ownership_3 = relevel(factor(ownership_3), ref = "Not Homeowner or Landlord")) |> 
    mutate(ownership_2 = relevel(factor(ownership_2), ref = "Not Homeowner or Landlord")) |> 
    group_by(cycle) |> 
    mutate(perc_housing_stand_cycle = (perc_housing - mean(perc_housing, na.rm = TRUE)) / sd(perc_housing, na.rm = TRUE)) |> 
    ungroup()

dat_complete_prop_all$propnum_factor <- as.factor(dat_complete_prop_all$propnum)
dat_complete_prop$propnum_factor <- as.factor(dat_complete_prop$propnum)


dat_complete_prop_twitter <- dat_complete_prop |> 
    filter(type == "Tweets") 

dat_complete_prop_questions <- dat_complete_prop |> 
    filter(type == "Parliamentary Questions") 




## run many models with all combinations

terms <- c("2011-2016", "2016-2020", "2020-2022")


variables <- c(#"propnum_dummy",
               "propnum_recoded",
               "ownership_2",
               "ownership_3")


dat_models_all <- data.frame()


for (i in variables) {
    
    cat("Running models using", i, "\n")
    
    lm_all <- fixest::feols(
        as.formula(paste0("perc_housing ~ ",
                   i, " + gov_opp +
            dublin_dummy + seniority_years | year")), 
        cluster = "harmonised", 
        data = dat_complete_prop_all)
    
    lm_all_tidy <- broom::tidy(lm_all) |> 
        mutate(model = "Combined",
               variable = i)
    
    lm_tweets <- update(lm_all,
                        data = dat_complete_prop_twitter) |> 
        broom::tidy() |> 
        mutate(model = "Twitter",
               variable = i)
    
    lm_questions <- update(lm_all,
                        data = dat_complete_prop_questions) |> 
        broom::tidy() |> 
        mutate(model = "Questions",
               variable = i)
    
    dat_models <- bind_rows(
        lm_questions, lm_tweets, lm_all_tidy
    )
    
    dat_models_all <- bind_rows(dat_models, dat_models_all)
    
}

# clean up data frame
dat_models_all |> 
    filter(str_detect(term, "propnum|ownership")) |>
    mutate(term = gsub(c("ownership_2|ownership_3|propnum_recoded"), "", term)) |>
    mutate(variable = dplyr::recode(
        variable, "ownership_2" = "2 Categories",
        "ownership_3" = "3 Categories",
        "propnum_dummy" = "Register:\nAt Least One Entry",
        "propnum_recoded" = "Register:\nNumber of Entries",
    )) |> 
    mutate(term = dplyr::recode(term, "Homeowner or Landlord" = "Landlord or Multiple Properties\n(ref.: Other)")) |> 
    mutate(term = dplyr::recode(term, "Homeowner Only" = "Multiple Properties")) |> 
    mutate(term = dplyr::recode(term, "Landlord" = "Landlord with Rental Income\n(ref.: Other)")) |> 
    mutate(term = dplyr::recode(term, "1" = "1 (ref.: 0)")) |> 
   # mutate(term = str_remove_all(term, "propnum_recoded")) |> 
    ggplot(aes(x = forcats::fct_rev(term), y = estimate,
               ymin = estimate - 1.96 * std.error, 
               ymax = estimate + 1.96 * std.error)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    # geom_label(aes(label = paste0(format(round(estimate, 1), nsmall = 1),
    #                               " [", 
    #                              format(round(estimate - 1.96 * std.error, 1), nsmall = 1),
    #                              ", ", 
    #                              format(round(estimate + 1.96 * std.error, 1), nsmall = 1), "]")), 
    #               nudge_x = 0.3, label.size = NA,
    #           colour = "grey50", size = 3) +
    geom_point(size = 3.5) +
    # geom_linerange(aes(ymin = estimate - 1.645 * std.error,
    #                    ymax = estimate + 1.645 * std.error),
    #                size = 1.3) +
    geom_linerange(aes(ymin = estimate - 1.96 * std.error,
                       ymax = estimate + 1.96 * std.error),
                   size = 0.8) +
    coord_flip() +
    #scale_y_continuous(limits = c(-5, 1)) +
    facet_grid(variable~model, space = "free", scales = "free_y") +
    labs(x = "Indicator of Private Interests", 
         y = "Coefficient on Housing Salience") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 13),
          strip.text.x = element_text(size = 13))
ggsave("fig_04.pdf",
       width = 9.25, height = 5)
ggsave("fig_04.eps",
       width = 9.25, height = 5)



# run for merged measure
lm_robust_all <- feols(
    perc_housing ~ 
    ownership_2 + dublin_dummy + gov_opp +
        seniority_years + perc_change_mean |
        year,
    cluster = "harmonised",
    data = dat_complete_prop_all
)


# twitter only
lm_robust_twitter <- update(
    lm_robust_all,
    data = dat_complete_prop_twitter
)


# questions only
lm_robust_questions <- update(
    lm_robust_all,
    data = dat_complete_prop_questions
)

screenreg(list(lm_robust_all,
            lm_robust_twitter,
            lm_robust_questions))


coef_names <- list(
    "ownership_2Homeowner or Landlord" = "Landlord (ref.: No Landlord)",
    "gov_oppOpposition" = "Opposition (ref.: Government)",
    "dublin_dummyDublin" = "Dublin Constituencies",
    "seniority_years" = "Seniority (Years)",
    "perc_change_mean" = "Change in Property Prices",
    "ownership_2Homeowner or Landlord:gov_oppOpposition" = "Landlord $\\times$ Opposition"
)



# predict count_texts

# run for merged measure
lm_count_all <- fenegbin(
    count_texts ~ 
        ownership_2 + gov_opp + dublin_dummy + 
        seniority_years + perc_change_mean |
        year,
    cluster = "harmonised",
    data = dat_complete_prop_all
)

lm_count_tweets <- update(
    lm_count_all, 
    data = dat_complete_prop_twitter)

lm_count_questions <- update(
    lm_count_all, 
    data = dat_complete_prop_questions)

library(modelsummary)    # CRAN v1.4.1

modelsummary(list("M1: Combined" = lm_count_all,
                  "M2: Tweets" = lm_count_tweets,
                  "M3: PQs" = lm_count_questions),
             fmt = 2,
             coef_rename = c('ownership_2Homeowner or Landlord' = 'Landlord (ref.: No Landlord)',
                             'gov_oppOpposition' = 'Opposition (ref.: Government)',
                             'dublin_dummyDublin' = 'Dublin Constituencies',
                             'seniority_years' = 'Seniority (Years)',
                             'perc_change_mean' = 'Change in Property Prices'),
             coef_omit = ".theta",
             output = "tab_a15.tex",
             gof_omit = c("Std.Errors|R2 Within|R2 Within Adj.|FE: year"),
             title = "Predicting the number of texts about housing using negative binomial regression models. All models include year fixed effects. Robust standard errors, clustered by politician, in parentheses. \\label{tab:modcount}",
             stars = c('*' = .05, '**' = .01, '***' = 0.001))


modelsummary(list("M1: Combined" = lm_count_all,
                  "M2: Tweets" = lm_count_tweets,
                  "M3: PQs" = lm_count_questions),
             fmt = 2,
             coef_rename = c('ownership_2Homeowner or Landlord' = 'Landlord (ref.: No Landlord)',
                             'gov_oppOpposition' = 'Opposition (ref.: Government)',
                             'dublin_dummyDublin' = 'Dublin Constituencies',
                             'seniority_years' = 'Seniority (Years)',
                             'perc_change_mean' = 'Change in Property Prices'),
             coef_omit = ".theta",
             output = "tab_a15.html",
             gof_omit = c("Std.Errors|R2 Within|R2 Within Adj.|FE: year"),
             title = "Predicting the number of texts about housing using negative binomial regression models. All models include year fixed effects. Robust standard errors, clustered by politician, in parentheses. \\label{tab:modcount}",
             stars = c('*' = .05, '**' = .01, '***' = 0.001))

p_count_combined <- plot_predictions(lm_count_all, 
                 condition = "ownership_2", draw = FALSE) |> 
    mutate(ownership_2 = dplyr::recode(ownership_2, "Homeowner or Landlord" = "Landlord",
                                       "Not Homeowner or Landlord" = "No Landlord")) |> 
    ggplot(aes(x = ownership_2, y = estimate, ymin = conf.low,
               ymax = conf.high)) +
    geom_linerange(linewidth = 0.8) +
    geom_point(size = 3) +
    labs(y = "Count of Texts About Housing Policy",
         x = NULL,
         title = "(a) Combined")
p_count_combined

p_count_tweets <- plot_predictions(lm_count_tweets, 
                                     condition = "ownership_2", draw = FALSE) |> 
    mutate(ownership_2 = dplyr::recode(ownership_2, "Homeowner or Landlord" = "Landlord",
                                       "Not Homeowner or Landlord" = "No Landlord")) |> 
    ggplot(aes(x = ownership_2, y = estimate, ymin = conf.low,
               ymax = conf.high)) +
    geom_linerange(linewidth = 0.8) +
    geom_point(size = 3) +
    labs(y = "Count of Texts About Housing Policy",
         x = NULL,
         title = "(b) Tweets")
p_count_tweets


p_count_questions <- plot_predictions(lm_count_questions, 
                                   condition = "ownership_2", draw = FALSE) |> 
    mutate(ownership_2 = dplyr::recode(ownership_2, "Homeowner or Landlord" = "Landlord",
                                       "Not Homeowner or Landlord" = "No Landlord")) |> 
    ggplot(aes(x = ownership_2, y = estimate, ymin = conf.low,
               ymax = conf.high)) +
    geom_linerange(linewidth = 0.8) +
    geom_point(size = 3) +
    labs(y = "Count of Texts About Housing Policy",
         x = NULL,
         title = "(c) Parliamentary\nQuestions")
p_count_questions


# Figure A10 ----
plot_grid(p_count_combined, 
          p_count_tweets,
          p_count_questions,
          nrow = 1, align = "vh")
ggsave("fig_a10.pdf",
       width = 9, height = 4.5)
ggsave("fig_a10.eps",
       width = 9, height = 4.5)


pred_change_q <- plot_predictions(lm_robust_questions,
                                    condition = "perc_change_mean") +
    geom_rug(data = dat_complete_prop_all, 
             aes(x = perc_change_mean),
             sides = "b",
             alpha = 0.4,
             colour = "grey50",
             inherit.aes = FALSE) +
    scale_y_continuous(limits = c(0, 12),
                       breaks = c(seq(0, 12, 2))) +
    xlab("Percentage Change\nof Average Property Price") +
    labs(y = "Predicted Value of Housing Salience",
         x = "Percentage Change\nof Average Property Price",
         title = "(c) Parliamentary Questions")
pred_change_q
    


pred_change_twitter <- plot_predictions(lm_robust_twitter,
                                  condition = "perc_change_mean") +
    geom_rug(data = dat_complete_prop_all, 
             aes(x = perc_change_mean),
             sides = "b",
             alpha = 0.4,
             colour = "grey50",
             inherit.aes = FALSE) +
    scale_y_continuous(limits = c(0, 12),
                       breaks = c(seq(0, 12, 2))) +
    xlab("Percentage Change\nof Average Property Price") +
    labs(y = "Predicted Value of Housing Salience",
         x = "Percentage Change\nof Average Property Price",
         title = "(b) Tweets")
pred_change_twitter


pred_change_all <- plot_predictions(lm_robust_all,
                                    condition = "perc_change_mean") +
    geom_rug(data = dat_complete_prop_all, 
             aes(x = perc_change_mean),
             sides = "b",
             alpha = 0.4,
             colour = "grey50",
             inherit.aes = FALSE) +
    scale_y_continuous(limits = c(0, 12),
                       breaks = c(seq(0, 12, 2))) +
    xlab("Percentage Change\nof Average Property Price") +
    labs(y = "Predicted Value of Housing Salience",
         x = "Percentage Change\nof Average Property Price",
         title = "(a) Combined")
pred_change_all


# Figure A06 ----
plot_grid(pred_change_all, pred_change_twitter, pred_change_q,
          nrow = 1)
ggsave("fig_a06.pdf",
       width = 10, height = 5)
ggsave("fig_a06.eps",
       width = 10, height = 5)


# repeat with interaction


# run for merged measure
lm_robust_all_int <- feols(
    perc_housing ~  
        ownership_2 * gov_opp + dublin_dummy +
        seniority_years + perc_change_mean | year,
    cluster = "harmonised",
    data = dat_complete_prop_all
)


# twitter only
lm_robust_twitter_int <- update(
    lm_robust_all_int,
    data = dat_complete_prop_twitter
)


# questions only
lm_robust_questions_int <- update(
    lm_robust_all_int,
    data = dat_complete_prop_questions
)



screenreg(list(lm_robust_all,
               lm_robust_twitter, 
               lm_robust_questions, 
               lm_robust_all_int,
               lm_robust_twitter_int,
               lm_robust_questions_int))

# save as table
texreg(list(lm_robust_all,
            lm_robust_twitter, 
            lm_robust_questions, 
            lm_robust_all_int,
            lm_robust_twitter_int,
            lm_robust_questions_int),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.coef.map = coef_names,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "footnotesize",
       caption.above = TRUE,
       label = "tab:reg_main_salience",
       caption = "Predicting issue emphasis on housing policy. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1: All", 
                              "M2: Tweets",
                              "M3: PQs", 
                              "M4: All",
                              "M5: Tweets",
                              "M6: PQs"),
       file = "tab_05.tex")

htmlreg(list(lm_robust_all,
            lm_robust_twitter, 
            lm_robust_questions, 
            lm_robust_all_int,
            lm_robust_twitter_int,
            lm_robust_questions_int),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.coef.map = coef_names,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "footnotesize",
       caption.above = TRUE,
       label = "tab:reg_main_salience",
       caption = "Predicting issue emphasis on housing policy. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1: All", 
                              "M2: Tweets",
                              "M3: PQs", 
                              "M4: All",
                              "M5: Tweets",
                              "M6: PQs"),
       file = "tab_05.html")


# get confidence intervals, reported in paper
screenreg(list(lm_robust_all,
               lm_robust_twitter, 
               lm_robust_questions),
          ci.force = TRUE)



# first difference analysis for these models

# function for first-difference analysis
first_diff <- function(x) {
    # run first difference analysis here
    
    #https://iqss.github.io/clarify/reference/sim_setx.html
    
    sim_model <- clarify::sim(x, n = 1000)

    
    fd_opp_raw <- sim_setx(
        sim_model,
        x = data.frame(ownership_2 = "Not Homeowner or Landlord",
                       gov_opp = "Opposition"),
        x1 = data.frame(ownership_2 = "Homeowner or Landlord",
                        gov_opp = "Opposition"),
        verbose = FALSE)
    
    print(fd_opp_raw)
    
    fd_opp <- fd_opp_raw |>
        as.data.frame() |>
        summarise(estimate = mean(FD),
                  conf.low = quantile(FD, 0.025),
                  conf.high = quantile(FD, 0.975)) |>
        mutate(model = "Opposition:\nLandlord vs. No Landlord")
    
    fd_gov_raw <- sim_setx(
        sim_model,
        x = list(ownership_2 = "Not Homeowner or Landlord",
                 gov_opp = "Government"),
        x1 = list(ownership_2 = "Homeowner or Landlord",
                  gov_opp = "Government"),
        verbose = TRUE)
    
    print(fd_gov_raw)
    
    fd_gov <- fd_gov_raw |>
        as.data.frame() |>
        summarise(estimate = mean(FD),
                  conf.low = quantile(FD, 0.025),
                  conf.high = quantile(FD, 0.975)) |>
        mutate(model = "Government:\nLandlord vs. No Landlord")
    
    fd_both <- bind_rows(fd_opp, fd_gov)
    
    return(fd_both)
}


dat_complete_prop_all$year <- factor(dat_complete_prop_all$year)

# run for merged measure
lm_robust_all_int_fd <- feols(
    perc_housing ~  
        ownership_2 * gov_opp + dublin_dummy +
        seniority_years + perc_change_mean + year, 
    cluster = "harmonised",
    data = dat_complete_prop_all
)

dat_complete_prop_twitter$year <- factor(dat_complete_prop_twitter$year)

# twitter only
lm_robust_twitter_int_fd <- update(
    lm_robust_all_int_fd,
    data = dat_complete_prop_twitter
)


dat_complete_prop_questions$year <- factor(dat_complete_prop_questions$year)

# questions only
lm_robust_questions_int_fd <- update(
    lm_robust_all_int_fd,
    data = dat_complete_prop_questions
)

fd_sal_combined <- first_diff(x = lm_robust_all_int_fd) |> 
    mutate(dv = "Combined")

fd_sal_tweets <- first_diff(x = lm_robust_twitter_int_fd) |> 
    mutate(dv = "Twitter")

fd_sal_questions <- first_diff(x = lm_robust_questions_int_fd) |> 
    mutate(dv = "Parliamentary Questions")


# combine into single data frame
dat_fd_fullsample <- bind_rows(fd_sal_combined,
                               fd_sal_tweets,
                               fd_sal_questions) |> 
    mutate(year = "Full Sample")



# for each cycle
dat_fd_cycles <- data.frame()

# get cycles

cycles <- unique(dat_complete_prop_questions$cycle)

cycles

for (i in cycles) {
    
    dat_cycle_all <- filter(dat_complete_prop_all, cycle == i) |> 
        mutate(year = factor(year))
    dat_cycle_twitter <- filter(dat_complete_prop_twitter, cycle == i) |> 
        mutate(year = factor(year))
    
    dat_cycle_questions <- filter(dat_complete_prop_questions, cycle == i) |> 
        mutate(year = factor(year))
    
    
    lm_robust_all_int_cycle <- feols(
        perc_housing ~  
            ownership_2 * gov_opp + dublin_dummy +
            seniority_years + perc_change_mean +
            year,
        cluster = "harmonised",
        data = dat_cycle_all
    )
    
    # twitter only
    lm_robust_twitter_int_cycle <- update(
        lm_robust_all_int_cycle,
        data = dat_cycle_twitter
    )
    
    
    # questions only
    lm_robust_questions_int_cycle <- update(
        lm_robust_all_int_cycle,
        data = dat_cycle_questions
    )
    
    fd_all <- first_diff(x = lm_robust_all_int_cycle) |> 
        mutate(dv = "Combined")
    
    fd_twitter <- first_diff(x = lm_robust_twitter_int_cycle) |> 
        mutate(dv = "Twitter")
    
    fd_questions <- first_diff(x = lm_robust_questions_int_cycle) |> 
        mutate(dv = "Parliamentary Questions")
    
    fd_combined <- bind_rows(fd_questions,
                             fd_twitter,
                             fd_all) |> 
        mutate(year = i)
    
    dat_fd_cycles <- bind_rows(dat_fd_cycles, fd_combined)
    
}


# add information on sample
dat_fd_cycles <- dat_fd_cycles |> 
    mutate(sample = "Cycle-Specific Sample")


dat_fd_all_sal <- bind_rows(dat_fd_cycles,
                        dat_fd_fullsample) |> 
    mutate(type_dummy = ifelse(year == "Full Sample", TRUE, FALSE)) |> 
    mutate(year = factor(year, levels = c("Full Sample",
                                          "2013-2016",
                                          "2016-2020",
                                          "2020-2022"))) |> 
    mutate(dv = str_replace_all(dv, "Parliamentary Questions", "Parliamentary\nQuestions")) |> 
    mutate(dv = factor(dv, levels = c("Combined", 
                                      "Twitter",
                                      "Parliamentary\nQuestions"))) 



# Figure 06 ----
ggplot(dat_fd_all_sal,
       aes(x = estimate, y = fct_rev(factor(year)),
           xmin = conf.low,
           xmax = conf.high,
           colour = type_dummy,
           shape = type_dummy)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "red") +
    geom_linerange(linewidth = 1) +
    scale_x_continuous(limits = c(-7, 7)) +
    geom_point(size = 4) +
    scale_y_discrete(labels = label_wrap(30)) +
    ggplot2::annotate("text", x = -0.5, y = 0.7,
             size = 3.5,
             hjust = 1, colour = "black",
             label = "Landlords lower") +
    ggplot2::annotate("text", x = 0.5, y = 0.7,
             size = 3.5,
             hjust = 0, colour = "black",
             label = "Landlords higher") +
    ggplot2::annotate("segment",
             x = -0.5, xend = -5.5,
             y = 0.5, yend = 0.5, colour = "grey60",
             size = 0.5,
             arrow = arrow(angle = 25,
                           length = unit(0.2, "cm"))) +
    ggplot2::annotate("segment",
             x = 0.5, xend = 5.5,
             y = 0.5, yend = 0.5, colour = "grey60",
             size = 0.5,
             arrow = arrow(angle = 25,
                           length = unit(0.2, "cm"))) +
    facet_grid(dv~model) +
    scale_color_manual(values = c("grey30", "black")) +
    scale_shape_manual(values = c(16, 15)) +
    theme(legend.position = "none") +
    ylab(NULL) +
    xlab('First Difference in Percentage Points: Landlord-No Landlord')
ggsave("fig_06.pdf",
       width = 9, height = 7)
ggsave("fig_06.eps",
       width = 9, height = 7)



# run split sample: gov/opp

lm_robust_all_gov <- feols(
    perc_housing ~ ownership_2 +
        dublin_dummy + 
        perc_change_mean + seniority_years | year,
    cluster = "harmonised",
    data = filter(dat_complete_prop_all, gov_opp == "Government")
)

lm_robust_all_opp <- update(lm_robust_all_gov,
    data = filter(dat_complete_prop_all, gov_opp == "Opposition")
)

lm_robust_tweets_gov <- update(lm_robust_all_gov,
                               data = filter(dat_complete_prop_twitter, 
                                             gov_opp == "Government")
)
lm_robust_tweets_opp <- update(lm_robust_all_gov,
                            data = filter(dat_complete_prop_twitter, 
                                          gov_opp == "Opposition")
)


lm_robust_questions_gov <- update(lm_robust_all_gov,
                               data = filter(dat_complete_prop_questions, 
                                             gov_opp == "Government")
)
lm_robust_questions_opp <- update(lm_robust_all_gov,
                               data = filter(dat_complete_prop_questions, 
                                             gov_opp == "Opposition")
)


# save as table
texreg(list(lm_robust_all_gov,
            lm_robust_all_opp, 
            lm_robust_tweets_gov, 
            lm_robust_tweets_opp,
            lm_robust_questions_gov,
            lm_robust_questions_opp),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.coef.map = coef_names,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "scriptsize",
       caption.above = TRUE,
       label = "tab:reg_main_salience_splitsample",
       caption = "Predicting issue emphasis on housing policy. Models 1, 3, and 5 are limited to government TDs; Models 2, 4, and 6 are limited to opposition TDs. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1: All (G)", 
                              "M2: All (O)",
                              "M3: Tw. (G)",
                              "M4: Tw. (O)",
                              "M5: PQs (G)",
                              "M6: PQs (O)"),
       file = "tab_a09.tex")

htmlreg(list(lm_robust_all_gov,
            lm_robust_all_opp, 
            lm_robust_tweets_gov, 
            lm_robust_tweets_opp,
            lm_robust_questions_gov,
            lm_robust_questions_opp),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.coef.map = coef_names,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "scriptsize",
       caption.above = TRUE,
       label = "tab:reg_main_salience_splitsample",
       caption = "Predicting issue emphasis on housing policy. Models 1, 3, and 5 are limited to government TDs; Models 2, 4, and 6 are limited to opposition TDs. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1: All (G)", 
                              "M2: All (O)",
                              "M3: Tw. (G)",
                              "M4: Tw. (O)",
                              "M5: PQs (G)",
                              "M6: PQs (O)"),
       file = "tab_a09.html")




# robustness: run models with age continuous,
# age categorical, and number of times elected as proxies
# for personal interests

lm_robust_all_age <- feols(
    perc_housing ~ gov_opp +
      dublin_dummy +  
        perc_change_mean +
       age | year,
    cluster = "harmonised",
    data = dat_complete_prop_all
)


lm_robust_all_age_group <- feols(
    perc_housing ~ gov_opp +
        age_group + dublin_dummy + 
        perc_change_mean 
         | year,
    cluster = "harmonised",
    data = dat_complete_prop_all
)


screenreg(list(lm_robust_all_age,
            lm_robust_all_age_group))

texreg(list(lm_robust_all_age,
            lm_robust_all_age_group),
       include.var = FALSE,
       custom.coef.names = c(
           "Opposition (ref.: Government)",
           "Dublin Constituencies",
           "Change in Property Prices",
           "Age (continuous)",
           "Age Group: 35-44 (ref.: 18-34)",
           "Age Group: 45-54",
           "Age Group: 55-64",
           "Age Group: 65+"),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "footnotesize",
       caption.above = TRUE,
       label = "tab:reg_main_age",
       caption = "Predicting issue emphasis on housing policy. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1", "M2"),
       file = "tab_a13.tex")

htmlreg(list(lm_robust_all_age,
            lm_robust_all_age_group),
       include.var = FALSE,
       custom.coef.names = c(
           "Opposition (ref.: Government)",
           "Dublin Constituencies",
           "Change in Property Prices",
           "Age (continuous)",
           "Age Group: 35-44 (ref.: 18-34)",
           "Age Group: 45-54",
           "Age Group: 55-64",
           "Age Group: 65+"),
       include.var = TRUE,
       include.adjrs = TRUE,
       include.proj.stats = FALSE,
       include.pseudors = FALSE,
       custom.gof.names = c(
           "Num. obs",
           "Fixed Effects: Year",
           "R$^2$", "Adj. R$^2$"
       ),
       fontsize = "footnotesize",
       caption.above = TRUE,
       label = "tab:reg_main_age",
       caption = "Predicting issue emphasis on housing policy. Robust standard errors, clustered by politician, in parentheses.",
       custom.model.names = c("M1", "M2"),
       file = "tab_a13.html")


pred_age <- plot_predictions(lm_robust_all_age, 
                             condition = "age",
                             draw = FALSE)

pred_age_group <- plot_predictions(lm_robust_all_age_group,
                                   condition = "age_group",
                                   draw = FALSE)


diff_age <- max(dat_complete_prop_all$age, na.rm = TRUE) - min(dat_complete_prop_all$age, na.rm = TRUE)

dat_age_unique <- dat_complete_prop_all |> 
    dplyr::select(age, harmonised, year) |> 
    unique()

p_age <- ggplot(pred_age, aes(x = age,
                              y = estimate,
                  ymin = conf.low,
                  ymax = conf.high)) +
    geom_ribbon(fill = "grey90") +
    geom_line() +
    geom_histogram(data = dat_age_unique,
                   aes(x = age,
                       stat(count) / 70), 
                   bins = diff_age,
                   colour = "grey30",
                   fill = "grey80",
                   inherit.aes = FALSE,
                   alpha = 0.8) +
    scale_y_continuous(limits = c(0, 10),
                       breaks = c(seq(0, 10, 2))) +   
    scale_x_continuous(breaks = c(seq(25, 85, 10))) +
    labs(x = "Age", y = "Predicted Salience of Housing",
         title = "(a) Age: Continuous Measure")

p_age


p_age_group <- ggplot(pred_age_group, 
                      aes(x = age_group, y = estimate,
                        ymin = conf.low,
                        ymax = conf.high)) +
    geom_point(size = 3) +
    geom_linerange(linewidth = 0.8) +
    scale_y_continuous(limits = c(0, 10),
                       breaks = c(seq(0, 10, 2))) +
    labs(x = "Age Group", y = "Predicted Salience of Housing",
         title = "(b) Age: Categorical Measure")
p_age_group

# Figure A08 ----
plot_grid(p_age, p_age_group, nrow = 1)
ggsave("fig_a08.pdf",
       width = 9, height = 5)
ggsave("fig_a08.eps",
       width = 9, height = 5)



# function to plot effect of factor variable
plot_effect_factor <- function(x) {
    ggplot(x, aes(x = term, y = estimate,
                  ymin = conf.low, ymax = conf.high)) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
        geom_point(size = 3.5) +
        geom_linerange(aes(ymin = estimate - 1.645 * std.error,
                           ymax = estimate + 1.645 * std.error),
                       size = 1.3) +
        geom_linerange(aes(ymin = estimate - 1.96 * std.error,
                           ymax = estimate + 1.96 * std.error),
                       size = 0.8) +    
        scale_y_continuous(breaks = c(seq(-5, 5, 1))) +
        coord_flip() +
        labs(x = NULL, y = "Coefficient on Housing Salience")
    
}


dat_housing_boot_mp_all <- dat_analysis |> 
    group_by(harmonised, year) |> 
    mutate(prop_housing = mean(housing_bert),
           count_texts = n(),
           count_texts_housing = sum(housing_bert)) |> 
    filter(count_texts >= 2) |> 
    mutate(age = as.integer(year) - as.integer(birth),
           seniority_years = as.integer(seniority) / 365) |> 
    group_by(year, party_recoded, candidate, housing_committee,
             dublin_dummy, gov_opp,# age, seniority_years, 
             perc_change_mean, age, seniority_years,
             harmonised, change_became_owner) |> 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$housing_bert, B = 50)))) |> 
    mutate(prop_housing_mean = 100 * Mean,
           prop_housing_95_lower = 100 * Lower,
           prop_housing_95_upper = 100 * Upper) |> 
    ungroup() |> 
    arrange(harmonised, year) |> 
    mutate(prop_housing_mean_lag = lag(prop_housing_mean),
           prop_housing_95_lower_lag = lag(prop_housing_95_lower),
           prop_housing_95_upper_lag = lag(prop_housing_95_upper)) |> 
    group_by(harmonised) |> 
    mutate(after_minus_before = prop_housing_mean - prop_housing_mean_lag)


dat_housing_boot_mp_type <- dat_analysis |> 
    group_by(harmonised, year) |> 
    mutate(prop_housing = mean(housing_bert),
           count_texts = n(),
           count_texts_housing = sum(housing_bert)) |> 
    filter(count_texts >= 2) |> 
    mutate(age = as.integer(year) - as.integer(birth),
           seniority_years = as.integer(seniority) / 365) |> 
    group_by(year, type, party_recoded, candidate, housing_committee,
             dublin_dummy, gov_opp,# age, seniority_years, 
             perc_change_mean, age, seniority_years,
             harmonised, change_became_owner) |> 
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$housing_bert, B = 50)))) |> 
    mutate(prop_housing_mean = 100 * Mean,
           prop_housing_95_lower = 100 * Lower,
           prop_housing_95_upper = 100 * Upper) |> 
    ungroup() |> 
    arrange(harmonised, year) |> 
    mutate(prop_housing_mean_lag = lag(prop_housing_mean),
           prop_housing_95_lower_lag = lag(prop_housing_95_lower),
           prop_housing_95_upper_lag = lag(prop_housing_95_upper)) |> 
    group_by(harmonised) |> 
    mutate(after_minus_before = prop_housing_mean - prop_housing_mean_lag)


lm_change_all <- feols(after_minus_before ~ 
                               change_became_owner | year,
                           cluster = "harmonised",
                           data = dat_housing_boot_mp_all)



lm_change_twitter <- update(lm_change_all,
                            data = filter(dat_housing_boot_mp_type, type == "Tweets"))


lm_change_questions <- update(lm_change_all,
                              data = filter(dat_housing_boot_mp_type, type == "Parliamentary Questions"))


dat_housing_boot_mp <- filter(dat_housing_boot_mp_all, 
                              change_became_owner == 1)


# select first time they became owner

dat_housing_boot_mp <- dat_housing_boot_mp |> 
    arrange(harmonised, year) |>
    group_by(harmonised) |> 
    slice(1) 
    
nrow(dat_housing_boot_mp)


dat_housing_boot_mp <- dat_housing_boot_mp |> 
    mutate(name_clean = str_to_title(candidate)) |> 
    mutate(name_clean = str_replace_all(name_clean, "O'r", "O'R")) |> 
    mutate(name_clean = str_replace_all(name_clean, "O'd", "O'D")) |> 
    mutate(name_clean = str_replace_all(name_clean, "O's", "O'S")) |> 
    mutate(name_clean = str_replace_all(name_clean, "Mcgrath", "McGrath")) |> 
    mutate(name_clean = paste0(name_clean, " (", year, ")"))


# get average and 95% CI of average

dat_avg <- dat_housing_boot_mp |> 
    ungroup() |> 
    summarise(
        mean = mean(after_minus_before, na.rm = TRUE), # Calculate mean
        sd = sd(after_minus_before, na.rm = TRUE), # Calculate standard deviation
        n = n(), # Count observations
        se = sd / sqrt(n) # Calculate Standard Error
    ) %>%
    mutate(
        ci_lower = mean - (1.96 * se), # Calculate lower bound of the 95% CI
        ci_upper = mean + (1.96 * se) # Calculate upper bound of the 95% CI
    ) |> 
    mutate(label = paste0("Mean: ", 
                          sprintf("%.2f", round(mean, 2)), 
                          "\n[95% CI: ", sprintf("%.2f", round(ci_lower, 2)), ", ",
                          sprintf("%.2f", round(ci_upper, 2)), "]"))



mean(dat_housing_boot_mp$after_minus_before)

# Figure A12 ----
ggplot(dat_housing_boot_mp, 
       aes(x = after_minus_before,
           colour = gov_opp, shape = gov_opp,
           y = fct_rev(name_clean))) +
    geom_vline(xintercept = mean(dat_housing_boot_mp$after_minus_before),
               colour = "red") +
    geom_segment(aes(x = 0, y =  fct_rev(name_clean), 
                     xend = after_minus_before,
                     yend =  fct_rev(name_clean))) +
    geom_vline(data = dat_avg,
               aes(xintercept = ci_lower),
               colour = "red", linetype = "dashed") +
    geom_vline(data = dat_avg,
               aes(xintercept = ci_upper),
               colour = "red", linetype = "dashed") +
    geom_text(data = dat_avg, aes(label = label,
                                  x = ci_upper + 6, y = 30),
              colour = "red", size = 5,
              inherit.aes = FALSE, hjust = 0.5) +
    geom_segment(data = dat_avg,
                 aes(x = ci_lower - 4, xend = ci_lower - 0.5,
                     y = 34, yend = 34),
                 arrow = arrow(angle = 25, length = unit(0.3, "cm")),
                 colour = "red",
                 inherit.aes = FALSE) +
    geom_text(data = dat_avg, aes(label = "Lower 95% CI",
                                  x = ci_lower - 0.5, hjust = 1,
                                  y = 34.8),
              inherit.aes = FALSE, size = 4, colour = "red") + 
    geom_segment(data = dat_avg,
                 aes(x = ci_upper + 4, xend = ci_upper + 0.5,
                     y = 34, yend = 34),
                 arrow = arrow(angle = 25, length = unit(0.3, "cm")),
                 colour = "red", 
                 inherit.aes = FALSE) +
    geom_text(data = dat_avg, aes(label = "Upper 95% CI",
                                  x = ci_upper + 0.5, hjust = 0,
                                  y = 34.8),
              inherit.aes = FALSE, size = 4, colour = "red") + 
    geom_point(size = 3) +
    scale_x_continuous(breaks = c(seq(-15, 20, 5)),
                       limits = c(-10, 20)) +
    scale_shape_manual(values = c(16, 15)) +
    scale_colour_manual(values = c("black", "grey50")) +
    labs(x = "Percentage Point Difference in Emphasis on Housing\nAfter Becoming Landlord",
         y  = NULL) +
    ggplot2::annotate("text", x = 3 , y = 2,
                      hjust = 0, colour = "grey20",
                      size = 4,
                      label = "Increase ") +
    ggplot2::annotate("text", x = -3 , y = 2,
                      hjust = 1, colour = "grey20",
                      size = 4,
                      label = "Decrease") +
    ggplot2::annotate("segment",
                      x = 2, xend = 8,
                      y = 1.5, yend = 1.5, colour = "grey20",
                      size = 0.5,
                      arrow = arrow(angle = 25,
                                    length = unit(0.3, "cm"))) +
    annotate("segment",
             x = -2, xend = -8,
             y = 1.5, yend = 1.5, colour = "grey20",
             size = 0.5,
             arrow = arrow(angle = 25, 
                           length = unit(0.3, "cm"))) +
    theme(legend.title = element_blank())
ggsave("fig_a12.pdf",
       width = 9, height = 9)
ggsave("fig_a12.eps",
       width = 9, height = 9)

