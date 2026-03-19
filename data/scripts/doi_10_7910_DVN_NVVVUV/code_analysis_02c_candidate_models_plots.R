############################################################################
############################################################################
#### 
#### Constantine Boussalis, Travis G. Coan, Mirya R. Holman, Stefan Müller
#### Gender, Candidate Emotional Expression,
#### and Voter Reactions During Televised Debates 
####
#### American Political Science Review 
####
#### code_analysis_02c_candidate_models_plots.R
#### Note: this code reproduces the coefficient plots for the candidate-level
#### models, based on the output from 
#### code_analysis_02a_candidate_models_merkel_debates.do and 
#### code_analysis_02b_candidate_models_minor_debate.do.
####
#### For details on all datasets, R scripts, and instructions 
#### please consult the file "0000_replication_instructions.pdf" in the 
#### Dataverse of this project.
############################################################################
############################################################################

# load required packages

library(dplyr)   # CRAN v1.0.5
library(ggplot2) # CRAN v3.3.3
library(cowplot) # CRAN v1.1.1
library(tidyr)   # CRAN v1.1.3
library(stringr) # CRAN v1.4.0
library(forcats) # CRAN v0.5.1
library(readr)   # CRAN v1.4.0

# set working directory 

setwd("")

# load custom ggplot2 theme
source("code_function_theme_base.R")


## Figure 03 ----

# load coefficients Merkel debates

dat_coef_raw_2005 <- read_csv("tab_a_cand_main_2005.csv", skip = 2) 
dat_coef_raw_2009 <- read_csv("tab_a_cand_main_2009.csv", skip = 2) 
dat_coef_raw_2013 <- read_csv("tab_a_cand_main_2013.csv", skip = 2) 
dat_coef_raw_2017 <- read_csv("tab_a_cand_main_2017.csv", skip = 2) 

# keep Merkel coefficients and SEs
dat_coef_raw_2005 <- dat_coef_raw_2005[2:3, ] %>% 
    mutate(type = c("beta", "se"), 
           year = "2005")
dat_coef_raw_2009 <- dat_coef_raw_2009[2:3, ] %>% 
    mutate(type = c("beta", "se"), 
           year = "2009")
dat_coef_raw_2013 <- dat_coef_raw_2013[2:3, ] %>% 
    mutate(type = c("beta", "se"), 
           year = "2013")
dat_coef_raw_2017 <- dat_coef_raw_2017[2:3, ] %>% 
    mutate(type = c("beta", "se"), 
           year = "2017")

# bind coefficients
dat_coef_merkel <- bind_rows(
    dat_coef_raw_2005,
    dat_coef_raw_2009,
    dat_coef_raw_2013,
    dat_coef_raw_2017
)

# clean coefficient names and values
dat_coef <- dat_coef_merkel %>% 
    select(-c("=\"\"")) %>% 
    gather(model, value, -c(type, year, type)) %>% 
    mutate(value = gsub('\\(|\\)|=|"', "", value)) %>% 
    mutate(value = as.numeric(value)) %>% 
    mutate(model_clean = case_when(
        str_detect(model, "anger") ~ "M 2: Anger",
        str_detect(model, "happ") ~ "M 1: Happiness",
        str_detect(model, "emo_noneutral") ~ "M 3: Non-neutral Emotions",
        str_detect(model, "high_zff1") ~ "M 5: Pitch (+1 SD)",
        str_detect(model, "high_zff2") ~ "M 6: Pitch (+1.5 SD)",
        str_detect(model, "sentiment") ~ "M 4: Sentiment"
    )) %>% 
    select(model_clean, model, type, value, year)



# get 90 and 95 percent confidence intervals
dat_coef_clean <- dat_coef %>% 
    spread(type, value) %>% 
    mutate(ci_low90 = beta - 1.645 * se,
           ci_high90 = beta + 1.645 * se) %>% 
    mutate(ci_low95 = beta - 1.96 * se,
           ci_high95 = beta + 1.96 * se) 


# function to plot coefficients from candidate models
ggcandidate <- function(x, xlab = "") {
    
    ggplot(x, aes(x = beta, y = factor(year))) +
        geom_vline(xintercept = 0,
                   colour = "red", size = 1.05) +
        geom_errorbarh(aes(xmin = ci_low95,
                           xmax = ci_high95),
                       size = 0.5, height = 0) +
        geom_errorbarh(aes(xmin = ci_low90,
                           xmax = ci_high90),
                       size = 1.3, height = 0)  +  
        geom_point(size = 3) +
        facet_wrap(~model_clean, 
                   nrow = 3, scales = "free") +
        scale_y_discrete(limits = rev) +
        labs(x = xlab, y = NULL) +
        theme(axis.text.x = element_text(size = 11),
              #axis.title = element_text(size = 14),
              legend.position = "none")
}

# remove unnecessary characters from model names
dat_coef_clean$model <- str_remove_all(dat_coef_clean$model, '"')
dat_coef_clean$model <- str_remove_all(dat_coef_clean$model, '=')

table(dat_coef_clean$model)



p_happiness <- ggcandidate(filter(dat_coef_clean, model == "emo_happiness")) +
   scale_x_continuous(limits = c(-0.1, 0.1))
p_happiness

p_anger <- ggcandidate(filter(dat_coef_clean, model == "emo_anger")) +
    scale_x_continuous(limits = c(-0.04, 0.0))
p_anger

p_nonneutral <- ggcandidate(filter(dat_coef_clean, model == "emo_noneutral")) + 
    scale_x_continuous(limits = c(-0.1, 0.1))
p_nonneutral


p_zff1 <- ggcandidate(filter(dat_coef_clean, model == "high_zff1"),
                      xlab = "Coefficient of Merkel") +
    scale_x_continuous(limits = c(-2, 1.6))
p_zff1

p_zff15 <- ggcandidate(filter(dat_coef_clean, model == "high_zff2")) + 
    scale_x_continuous(limits = c(-2, 1.6))
p_zff15

p_sentiment <- ggcandidate(filter(dat_coef_clean, model == "sentiment_log_lexicoder")) +
    scale_x_continuous(limits = c(-0.1, 0.6))
p_sentiment


                 # create and store Figure 03 
cowplot::plot_grid(
    p_happiness,
    p_anger,
    p_nonneutral,
    p_sentiment,
    p_zff1,
    p_zff15,
    ncol = 3
)
ggsave("fig_03.pdf",
       width = 10.3, height = 4)


## Figure 04 ----

# repeat for minor debates

dat_coef_raw_minor <- read_csv("tab_a_cand_minor.csv", skip = 2)

# keep female coefficients and SEs
dat_coef_raw_minor <- dat_coef_raw_minor[2:3, ] %>% 
    mutate(type = c("beta", "se"))


# clean coefficient names and values
dat_coef_minor <- dat_coef_raw_minor %>% 
    select(-c("=\"\"")) %>% 
    gather(model, value, -c(type, type)) %>% 
    mutate(value = gsub('\\(|\\)|=|"', "", value)) %>% 
    mutate(value = as.numeric(value)) %>% 
    mutate(model_clean = case_when(
        str_detect(model, "anger") ~ "M 2: Anger",
        str_detect(model, "happ") ~ "M 1: Happiness",
        str_detect(model, "emo_noneutral") ~ "M 3: Non-neutral Emotions",
        str_detect(model, "high_zff1") ~ "M 5: Pitch (+1 SD)",
        str_detect(model, "high_zff2") ~ "M 6: Pitch (+1.5 SD)",
        str_detect(model, "sentiment") ~ "M 4: Sentiment"
    )) %>% 
    select(model_clean, model, type, value)


# get 90 percent and 95 percent confidence intervals
dat_coef_minor_clean <- dat_coef_minor %>% 
    spread(type, value) %>% 
    mutate(ci_low90 = beta - 1.645 * se,
           ci_high90 = beta + 1.645 * se) %>% 
    mutate(ci_low95 = beta - 1.96 * se,
           ci_high95 = beta + 1.96 * se) 


dat_coef_minor_clean$model <- str_remove_all(dat_coef_minor_clean$model, '"')
dat_coef_minor_clean$model <- str_remove_all(dat_coef_minor_clean$model, '=')

dat_coef_minor_clean$year <- "2017 (minor parties)"

# function to plot coefficients from debate between minor parties
ggcandidate_minor <- function(x, xlab = "") {
    
    ggplot(x, aes(x = beta, y = factor(year))) +
        geom_vline(xintercept = 0,
                   colour = "red", size = 1.05) +
        geom_errorbarh(aes(xmin = ci_low95,
                           xmax = ci_high95),
                       size = 0.5, height = 0) +
        geom_errorbarh(aes(xmin = ci_low90,
                           xmax = ci_high90),
                       size = 1.3, height = 0)  +  
        geom_point(size = 3) +
        facet_wrap(~model_clean, 
                   nrow = 3, scales = "free") +
        scale_y_discrete(limits = rev) +
        labs(x = xlab, y = NULL) +
        theme(axis.text.x = element_text(size = 11),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              #axis.title = element_text(size = 14),
              legend.position = "none")
}


table(dat_coef_minor_clean$model)

p_anger_minor <- ggcandidate_minor(filter(dat_coef_minor_clean, model == "emo_anger")) +
    scale_x_continuous(limits = c(-0.04, 0.0))
p_anger_minor

p_happiness_minor <- ggcandidate_minor(filter(dat_coef_minor_clean, model == "emo_happiness"))  +
    scale_x_continuous(limits = c(-0.1, 0.1))
p_happiness_minor


p_nonneutral_minor <- ggcandidate_minor(filter(dat_coef_minor_clean, model == "emo_noneutral")) +
    scale_x_continuous(limits = c(-0.1, 0.1))
p_nonneutral_minor


p_zff1_minor <- ggcandidate_minor(filter(dat_coef_minor_clean, model == "high_zff1"),
                      xlab = "Coefficient of Female Candidates")  +
    scale_x_continuous(limits = c(-2, 2))
p_zff1_minor

p_zff15_minor <- ggcandidate_minor(filter(dat_coef_minor_clean, model == "high_zff2")) +
    scale_x_continuous(limits = c(-2, 2))
p_zff15_minor

p_sentiment_minor <- ggcandidate_minor(filter(dat_coef_minor_clean, model == "sentiment_log_lexicoder")) +
    scale_x_continuous(limits = c(-0.5, 0.5))

p_sentiment_minor


# create and save Figure 04
cowplot::plot_grid(
    p_happiness_minor,
    p_anger_minor,
    p_nonneutral_minor,
    p_sentiment_minor,
    p_zff1_minor,
    p_zff15_minor,
    ncol = 3
)
ggsave("fig_04.pdf",
       width = 10.3, height = 3)
