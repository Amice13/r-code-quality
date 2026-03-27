############################################################################
############################################################################
#### 
#### Constantine Boussalis, Travis G. Coan, Mirya R. Holman, Stefan Müller
#### Gender, Candidate Emotional Expression,
#### and Voter Reactions During Televised Debates 
####
#### American Political Science Review 
####
#### code_analysis_03g_voter_models_plots.R
#### Note: this code reproduces the plots based on the cumulative effects of the 
#### voter-level models. 
#### 
#### For details on all datasets, R scripts, and instructions 
#### please consult the file "0000_replication_instructions.pdf" in the 
#### Dataverse of this project.
############################################################################
############################################################################


# load packages
library(dplyr)   # CRAN v1.0.5
library(ggplot2) # CRAN v3.3.3
library(cowplot) # CRAN v1.1.1
library(tidyr)   # CRAN v1.1.3
library(stringr) # CRAN v1.4.0
library(forcats) # CRAN v0.5.1
library(xtable)  # CRAN v1.8-4

# load custom ggplot2 theme
source("code_function_theme_base.R")

# set working directory
setwd("")

# function for plots of voter-level results for major debates
ggvoterresults <- function(x,
                           rev_order = FALSE,
                           title) {
    
    data <- x %>% 
        mutate(type = ifelse(term == "sentiment", "Verbal",
                             ifelse(term == "frequency", "Vocal",
                                    "Facial Display"))) %>% 
        mutate(term = str_to_title(term)) %>% 
        mutate(term = ifelse(term == "Frequency", "Avg. Fundamental\nFreq. (Hz)",
                             ifelse(term == "Sentiment", "Log Sentiment", term)))
    
    
    ggplot(data, aes(x = estimate, y = term, colour = factor(model),
                     shape = factor(model))) +
        geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "red") +
        geom_point(position = position_dodge(width = 0.6),
                   size = 3.5) +
        geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                           xmax = estimate + 1.96 * std.error),
                       size = 0.5, height = 0,
                       position = position_dodge(width = 0.6)) +
        geom_errorbarh(aes(xmin = estimate - 1.645 * std.error,
                           xmax = estimate + 1.645 * std.error),
                       size = 1.3, height = 0,
                       position = position_dodge(width = 0.6))  +
        scale_colour_manual(values = color_values,
                            guide = guide_legend(reverse=TRUE)) +
        scale_shape_manual(guide = guide_legend(reverse=TRUE),
                           values = c(15, 1, 17, 19)) +
        facet_grid(type~., scales = "free_y", 
                   space = "free") +
        labs(x = "Estimate", y = NULL) +
        ggtitle(title) +
        theme(legend.position = "right", legend.title = element_blank(),
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              strip.text.x = element_text(size = 14),
              strip.text.y = element_text(face = "italic",
                                          angle = 0, size = 12, hjust = 0),
              plot.title = element_text(face = "bold", 
                                        hjust = 0.5,
                                        size = 16,
                                        margin=margin(10,0,15,0))) 
    
    
}



# function to plot candidate-specific results (one model for Merkel, one model for opponents)

ggvoterresults_candidates <- function(x,
                                      rev_order = FALSE,
                                      title) {
    
    data <- x %>% 
        mutate(type = ifelse(term == "sentiment", "Verbal",
                             ifelse(term == "frequency", "Vocal",
                                    "Facial Display"))) %>% 
        mutate(term = str_to_title(term)) %>% 
        mutate(term = ifelse(term == "Frequency", "Avg. Fundamental\nFreq. (Hz)",
                             ifelse(term == "Sentiment", "Log Sentiment", term)))
    
    
    ggplot(data, aes(x = estimate, y = term, colour = factor(year),
                     shape = factor(year))) +
        geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "red") +
        geom_point(position = position_dodge(width = 0.7),
                   size = 3.5) +
        geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                           xmax = estimate + 1.96 * std.error),
                       size = 0.5, height = 0,
                       position = position_dodge(width = 0.7)) +
        geom_errorbarh(aes(xmin = estimate - 1.645 * std.error,
                           xmax = estimate + 1.645 * std.error),
                       size = 1.3, height = 0,
                       position = position_dodge(width = 0.7))  +
        scale_colour_manual(values = color_values,
                            guide = guide_legend(reverse=TRUE)) +
        scale_shape_manual(guide = guide_legend(reverse=TRUE),
                           values =  c(15, 1, 17, 19)) +
        facet_grid(type~., scales = "free_y", 
                   space = "free") +
        labs(x = "Estimate", y = NULL) +
        ggtitle(title) +
        theme(legend.position = "right", legend.title = element_blank(),
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              strip.text.x = element_text(size = 14),
              strip.text.y = element_text(face = "italic",
                                          hjust = 0,
                                          size = 12, angle = 0),
              plot.title = element_text(face = "bold", 
                                        hjust = 0.5,
                                        size = 16,
                                        margin=margin(10,0,15,0))) 
    
    
}




# set colours for plot
color_values <- c("grey60",             
                  "grey50",
                  "grey25",
                  "black")


## Figure 05 ----

# prepare data for top-panel of Figure 05


# load spreadsheet with cumulative effects for model
# that considers all emotions
dat_general_raw <- read.csv("voter01_results_table_general.csv", 
                            stringsAsFactors = FALSE) %>% 
    mutate(term = str_squish(term)) 
    

## Table A07 ----

# save coefficients as table for appendix

dat_general_table <- dat_general_raw %>% 
    mutate(term = str_to_title(str_squish(term))) %>% 
    select(-t) %>% 
    rename(Debate = model, Emotion = term,
           Coefficient = estimate,
           `Std error` = std.error,
           `p-value` = p,
           `Lower 95% CI` = lb,
           `Upper 95% CI` = ub) %>% 
    filter(!str_detect(Debate, "_rauh"))
    

# store Table A07 
print(xtable(dat_general_table, type = "latex",
             caption.above = TRUE,
             digits = 4,
             label = "tab:cumulative_happiness_anger",
             caption = "Cumulative effects across 4 lags for anger, happiness, sentiment, and avg. fundamental frequency."),
      size = "scriptsize",
      include.rownames = FALSE,
      file = "tab_a07.tex")


# extract coefficients of interest
dat_general_all <- dat_general_raw %>% 
    filter(!term %in% c("contempt", "disgust", "sadness",
                        "surprise", "fear")) # exclude non-validated emotions


# only use Lexicoder Sentiment Dictionary for main paper (we use both dictionaries in the appendix)
dat_general_all <- dat_general_all %>% 
  mutate(sentiment_model = ifelse(str_detect(model, "rauh"),
                                  "Rauh", "Lexicoder")) %>% 
  mutate(model = str_replace_all(model, "_rauh", "")) %>% 
    mutate(term = str_squish(term)) # make sure the terms do not contain extra whitespaces


dat_general_all_lexicoder <- filter(dat_general_all, sentiment_model == "Lexicoder") 

# first panel of Figure 05
p_fig_05_top <- ggvoterresults(dat_general_all_lexicoder,
                                              title = "(a) Voter Reactions to Specific Emotions from Merkel vs Opponent") +
  scale_x_continuous(limits = c(-0.4, 0.2), breaks = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2)) +
    theme(legend.position = "none")
p_fig_05_top


## lower panel of Figure 5

# non-neutral emotions
dat_general_nonneutral <- read.csv("voter01_results_table_nonneutral.csv", 
                                   stringsAsFactors = FALSE)


## Table A08 ----
                 # create dataframe with coefficients for appendix
dat_general_nonneutral_table <- dat_general_nonneutral %>% 
    mutate(term = str_to_title(str_squish(term))) %>% 
    select(-t) %>% 
    rename(Debate = model, Emotion = term,
           Coefficient = estimate,
           `Std error` = std.error,
           `p-value` = p,
           `Lower 95% CI` = lb,
           `Upper 95% CI` = ub) %>% 
    filter(!str_detect(Debate, "_rauh"))


                 # create Table A08
print(xtable(dat_general_nonneutral_table, type = "latex",
             caption.above = TRUE,
             digits = 4,
             label = "tab:cumulative_nonneutral",
             caption = "Cumulative effects across 4 lags for non-neutral emotions, sentiment, and avg. fundamental frequency."),
      size = "scriptsize",
      include.rownames = FALSE,
      file = "tab_a08.tex")


                 # create indicator which dictionary was used
# and only keep Lexicoder Sentiment dictionary for this plot 
dat_general_nonneutral <- dat_general_nonneutral %>% 
  mutate(sentiment_model = ifelse(str_detect(model, "rauh"),
                                  "Rauh", "Lexicoder")) %>% 
  mutate(term = str_squish(term)) %>% 
    filter(sentiment_model == "Lexicoder") %>% 
  mutate(term = ifelse(term == "noneutral", "Non-neutral\nEmotion", term))

    
p_fig_05_bottom <- ggvoterresults(dat_general_nonneutral,
                                  title = "(b) Voter Reactions to Levels of Emotion from Merkel vs Opponent") +
    scale_x_continuous(limits = c(-0.4, 0.2), breaks = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2)) +
    theme(legend.position = "bottom")

p_fig_05_bottom


# combine both plots and save as Figure 05
cowplot::plot_grid(p_fig_05_top, 
                   p_fig_05_bottom,
                   rel_heights = c(0.5, 0.55),
                   ncol = 1, align = 'v')
ggsave("fig_05.pdf", 
       width = 10, height = 8)


## Figure 06 ----


# plot results from separate models for Merkel and opponent


# load spreadsheet with cumulative effects for model
# that considers all emotions
dat_candidate_specific_all <- read.csv("voter02_results_table_general.csv", 
                                       stringsAsFactors = FALSE)


# make sure the terms do not contain extra whitspaces
dat_candidate_specific_all$term <- str_squish(dat_candidate_specific_all$term)


## Table A09 ----

# prepare table with coefficients for appendix
dat_candidate_specific_happiness_anger <-  dat_candidate_specific_all %>% 
    filter(!term %in% c("contempt", "disgust", "sadness",
                        "surprise", "fear")) %>% 
    separate(model, into = c("candidate", "year")) %>% 
    mutate(candidate = ifelse(candidate == "Opponent", "Male Opponent", candidate))



dat_general_specific_happiness_anger_table <- dat_candidate_specific_happiness_anger %>% 
    mutate(term = str_to_title(str_squish(term))) %>% 
    select(-t) %>% 
    rename(Candidate = candidate, Year = year, Emotion = term,
           Coefficient = estimate,
           `Std error` = std.error,
           `p-value` = p,
           `Lower 95% CI` = lb,
           `Upper 95% CI` = ub)


                 # create Table A09
print(xtable(dat_general_specific_happiness_anger_table, type = "latex",
             caption.above = TRUE,
             digits = 4,
             label = "tab:cumulative_happiness_anger_candidates",
             caption = "Cumulative effects across 4 lags for anger, happiness, sentiment, and avg. fundamental frequency. Separate models for each candidate in the four debates."),
      size = "scriptsize",
      include.rownames = FALSE,
      file = "tab_a09.tex")


# relevel order of candidates
dat_candidate_specific_happiness_anger$candidate <- factor(dat_candidate_specific_happiness_anger$candidate,
                                                           levels = c("Merkel", "Male Opponent"))

                 # create plot for happiness and anger (upper panel of Figure 06)
p_fig_06_top <- ggvoterresults_candidates(dat_candidate_specific_happiness_anger,
                                                        title = "(a) Voter Reactions to Specific Emotions from Merkel and Her Opponents") +
    facet_grid(type~candidate, scales = "free_y", 
               space = "free") +
    scale_x_continuous(limits = c(-0.2, 0.35), 
                       breaks = c(seq(-0.2, 0.3, 0.1))) +
    theme(legend.position = "none")
p_fig_06_top


# repeat analsyis for non-neutral emotions
dat_candidate_specific_nonneutral <- read.csv("voter02_results_table_nonneutral.csv", 
                                              stringsAsFactors = FALSE)

# make sure the terms do not contain extra whitespaces
dat_candidate_specific_nonneutral$term <- str_squish(dat_candidate_specific_nonneutral$term)

dat_candidate_specific_nonneutral_clean <- dat_candidate_specific_nonneutral %>%
    mutate(term = ifelse(term == "noneutral", "Non-neutral\nEmotion", term)) %>% 
    separate(model, into = c("candidate", "year")) %>% 
    mutate(candidate = ifelse(candidate == "Opponent", "Male Opponent", candidate))


## Table A10 ----
# save coefficients as table for appendix
dat_general_specific_nonneutral_table <- dat_candidate_specific_nonneutral_clean %>% 
    mutate(term = str_to_title(str_squish(term))) %>% 
    select(-t) %>% 
    rename(Candidate = candidate, Year = year, Emotion = term,
           Coefficient = estimate,
           `Std error` = std.error,
           `p-value` = p,
           `Lower 95% CI` = lb,
           `Upper 95% CI` = ub)


                 # create Table 
print(xtable(dat_general_specific_nonneutral_table, type = "latex",
             caption.above = TRUE,
             digits = 4,
             label = "tab:cumulative_nonneutral_candidates",
             caption = "Cumulative effects across 4 lags lags for non-neutral facial emotions, sentiment, and avg. fundamental frequency. 
             Separate models for each candidate in the four debates."),
      size = "scriptsize",
      include.rownames = FALSE,
      file = "tab_a10.tex")




dat_candidate_specific_nonneutral_clean$candidate <- factor(dat_candidate_specific_nonneutral_clean$candidate,
                                                            levels = c("Merkel", "Male Opponent"))

p_fig_06_bottom <- ggvoterresults_candidates(dat_candidate_specific_nonneutral_clean,
                                                   title = "(b) Voter Reactions to General Emotions from Merkel and Her Opponents") +
    facet_grid(type~candidate, scales = "free_y", 
               space = "free") +
    scale_x_continuous(limits = c(-0.2, 0.35), 
                       breaks = c(seq(-0.2, 0.3, 0.1))) +
   theme(legend.position = "bottom")
p_fig_06_bottom

cowplot::plot_grid(p_fig_06_top, p_fig_06_bottom, 
                   rel_heights = c(0.5, 0.55),
                   ncol = 1, align = 'v') 
ggsave("fig_06.pdf", 
       width = 10, height = 9.5)


## Figure 7 ----

# repeat analysis from above, but use coefficient for debate between minor parties


dat_minor_female_all <- read.csv("voter01_results_table_general_2017minor_female.csv")

                 # create indicator which dictionary was used
dat_minor_female_all <- dat_minor_female_all %>% 
  mutate(sentiment_model = ifelse(str_detect(model, "rauh"),
                                  "Rauh", "Lexicoder")) %>% 
    mutate(term = str_squish(term)) %>% 
    filter(!term %in% c("contempt", "disgust", "sadness",
                        "surprise", "fear"))


# focus on Lexicoder models
dat_minor_female_all_lexicoder <- filter(dat_minor_female_all,
                                         sentiment_model == "Lexicoder") 
    


## Table A12 ----

# store coefficients for table in appendix
dat_minor_table <- dat_minor_female_all_lexicoder %>% 
    mutate(term = str_to_title(str_squish(term))) %>% 
    select(-c(model, sentiment_model, t)) %>% 
    mutate(Debate = "2017 (minor parties)") %>% 
    rename(Emotion = term,
           Coefficient = estimate,
           `Std error` = std.error,
           `p-value` = p,
           `Lower 95% CI` = lb,
           `Upper 95% CI` = ub) %>% 
    filter(!str_detect(Debate, "_rauh")) %>% 
    select(Debate, everything())


                 # create Table A12
print(xtable(dat_minor_table, type = "latex",
             caption.above = TRUE,
             digits = 4,
             label = "tab:cumulative_happiness_anger_minor",
             caption = "Cumulative effects across 4 lags for anger, happiness, sentiment, and avg. fundamental frequency"),
      size = "scriptsize",
      include.rownames = FALSE,
      file = "tab_a12.tex")


                 # create and store Figure 7
ggvoterresults(dat_minor_female_all_lexicoder,
                               title = "Voter Reactions to Specific Emotions by Female Candidates vs Male Candidates") +
    scale_shape_manual(values = c(16)) +
    scale_colour_manual(values = "black") +
    scale_x_continuous(limits = c(-0.5, 0.2), 
                       breaks = c(seq(-0.5, 0.2, 0.1))) +
    theme(legend.position = "none", strip.text.y = element_text(angle = 0, hjust = 0)) 
ggsave("fig_07.pdf",
       width = 10, height = 3)



# Figure A16 ----

# non-neutral emotions only, minor debate

dat_minor_female_nonneutral <- read.csv("voter01_results_table_nonneutral_2017minor_female.csv")

# make sure the terms do not contain extra whitespaces
dat_minor_female_nonneutral$term <- str_squish(dat_minor_female_nonneutral$term)

# rename noneutral
dat_minor_female_nonneutral <- dat_minor_female_nonneutral %>%
  mutate(term = ifelse(term == "noneutral", "Non-neutral\nEmotion", term))


# only keep models using the Lexicoder sentiment dictionary 
dat_minor_female_nonneutral <- dat_minor_female_nonneutral %>% 
  mutate(sentiment_model = ifelse(str_detect(model, "rauh"),
                                  "Rauh", "Lexicoder")) %>% 
    filter(sentiment_model == "Lexicoder")


                 # create and save Figure A16
ggvoterresults(dat_minor_female_nonneutral,
                                        title = "Voter Reactions to Emotions by Female Candidates vs Male Candidates") +
  scale_shape_manual(values = c(16)) +
  scale_colour_manual(values = "black") +
  theme(legend.position = "none", strip.text.y = element_text(angle = 0, hjust = 0)) +
    scale_x_continuous(limits = c(-0.5, 0.2), 
                       breaks = c(seq(-0.5, 0.2, 0.1))) 
ggsave("fig_a16.pdf",
       width = 10, height = 3)




# 
# p_minor_female_all_both <- ggvoterresults_minor(dat_minor_female_all,
#                                                 title = "Voter Reactions to Specific Emotions by Female Candidates vs Male Candidates") +
#   facet_grid(type~sentiment_model, scales = "free_y", 
#              space = "free") +
#   scale_y_discrete(limits = rev)  +
#   scale_shape_manual(values = c(16)) +
#   scale_colour_manual(values = "black") +
#   theme(legend.position = "none")
# p_minor_female_all_both
# ggsave(p_minor_female_all_both,
#        file = "analysis/plots_r_and_r/voters_minor_all_emotions_two_dictionaries.pf", 
#        width = 10, height = 6)



## Figure A15 ---- 

# plot estimates using different dictionaries

                 # create and store Figure A15
ggvoterresults(dat_general_all, title = "Voter Reactions to Merkel's Emotions vs Opponent") +
    facet_grid(type~sentiment_model, scales = "free_y", 
               space = "free") +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2)) +
    theme(legend.position = "bottom")
ggsave("fig_a15.pdf", 
       width = 10, height = 6.5)


## Figure A14 ---


## repeat for male and female respondents


# load spreadsheet with cumulative effects for model
# that considers all emotions

dat_general_all_gender <- read.csv("voter03_results_table_general.csv", 
                                   stringsAsFactors = FALSE)


# make sure the terms do not contain extra whitespaces
dat_general_all_gender$term <- str_squish(dat_general_all_gender$term)

dat_general_all_gender <- dat_general_all_gender %>%  
    filter(!term %in% c("contempt", "disgust", "sadness",
                        "surprise", "fear")) %>% 
    separate(model, into = c("gender", "model")) %>% 
    mutate(gender = paste0(gender, " Respondents"))

                 # create data frame for main paper
dat_gender_happiness_anger <-  dat_general_all_gender %>% 
    filter(!term %in% c("contempt", "disgust", "sadness",
                        "surprise", "fear"))


                 # create and store Figure A14
ggvoterresults(dat_gender_happiness_anger,
               title = "Voter Gender and Reactions to Specific Emotions from Merkel vs Opponent") +
    facet_grid(type~gender, scales = "free_y", 
               space = "free") +
    theme(legend.position = "bottom")
ggsave("fig_a14.pdf", 
       width = 10, height = 6.5)
