#***********************************************************************#
#                                                                       #
#                      Replication files for:                           #
#                                                                       #
#        "The Autocracy Bias: Evaluating Democratic Citizens'           #
#    Perception of Human Rights Violations in Policy Proposals Abroad"  #
#                 Barceló, Joan and Greg Chih-Hsin Sheen                #
#               International Studies Quarterly (ISQ, 2024)             # 
#                                                                       #
#***********************************************************************#


#################
#               #
# Load packages #
#               #
#################

library(rstudioapi)
library(readstata13)
library(Amelia)
library(randomizr)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(qualtRics)
library(Hmisc)
library(MASS)
library(car)
library(ggpubr)
library(foreign)
library(tidyverse)
library(reporttools)
library(stargazer)
library(forcats)
library(tidyr)
library(knitr)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#################
#               #
# Load datasets #
#               #
#################

# Survey experiments in Spain
survey1 <- read.dta13("survey1.dta", convert.factors=T) # wave 1
survey2 <- read.dta13("survey2.dta", convert.factors=T) # wave 2
survey3 <- read.dta13("survey3.dta", convert.factors=T) # wave 3

# Survey experiments across 6 democracies

survey_multicountry <- read.dta13("survey_multicountry.dta", convert.factors=T) # multicountry survey

#################
#               #
#   Figure 1    #
#               #
#################

# Outcome 1

dignitiy_survey1_means <- survey1 %>% 
  group_by(double_standards_treatment) %>% 
  dplyr::summarise(
    mean_dignity_survey1 = mean(humandignity, na.rm = T),
    sd_dignity_survey1   = sd(humandignity, na.rm = T),
    n_dignity_survey1   = n(),
    se_dignity_survey1 = sd_dignity_survey1/sqrt(n())
  )  %>% 
  drop_na() %>% 
  mutate(
    double_standards_treatment = case_when(
      double_standards_treatment == 0 ~ "China",
      double_standards_treatment == 1 ~ "Italy",
      double_standards_treatment == 2 ~ "Spain"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy",
                  "Spain")
  )

dignitiy_survey1_means <- cbind(dignitiy_survey1_means, "Dignity")
dignitiy_survey1_means <- cbind(dignitiy_survey1_means, "Survey 1 (N = 3854)")

colnames(dignitiy_survey1_means) <- c("Country Prime", "mean", "sd",
                                      "n", "se", "Outcome", "survey")

dignity_survey1_means <- dignitiy_survey1_means


dignity_survey2_means <- survey2 %>% 
  group_by(country) %>% 
  dplyr::summarise(
    mean_dignity_survey2 = mean(outcome2, na.rm = T),
    sd_dignity_survey2   = sd(outcome2, na.rm = T),
    n_dignity_survey2   = n(),
    se_dignity_survey2 = sd_dignity_survey2/sqrt(n())
  )  %>% 
  drop_na() %>% 
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

dignity_survey2_means <- cbind(dignity_survey2_means, "Dignity")
dignity_survey2_means <- cbind(dignity_survey2_means, "Survey 2 (N = 1938)")

colnames(dignity_survey2_means) <- c("Country Prime", "mean", "sd",
                                      "n", "se", "Outcome", "survey")

dignity_survey2_means <- dignity_survey2_means

survey1_humandignity <- survey1[, c('double_standards_treatment', 'humandignity', 'gender', 'age_num', 'region', 'employment', 'covid_employment')]
colnames(survey1_humandignity) <- c('country', 'dignity_outcome', 'gender', 'age', 'region', 'employment', 'covid_employment')
survey1_humandignity <- survey1_humandignity[complete.cases(survey1_humandignity),]

survey2_humandignity <- survey2[, c('country', 'outcome2', 'gender', 'age', 'region', 'employment', 'covid_employment')]
colnames(survey2_humandignity) <- c('country', 'dignity_outcome', 'gender', 'age', 'region', 'employment', 'covid_employment')
survey2_humandignity <- survey2_humandignity[complete.cases(survey2_humandignity),]

pooled_survey_humandignity <- rbind(survey1_humandignity, survey2_humandignity)

pooled_survey_humandignity$gender <- ifelse(pooled_survey_humandignity$gender %in% c("Mujer", 1), 1, 0)

dignitiy_poolsurvey_means <- pooled_survey_humandignity %>% 
  group_by(country) %>% 
  dplyr::summarise(
    mean_dignity_poolsurvey = mean(dignity_outcome, na.rm = T),
    sd_dignity_poolsurvey   = sd(dignity_outcome, na.rm = T),
    n_dignity_poolsurvey   = n(),
    se_dignity_poolsurvey = sd_dignity_poolsurvey/sqrt(n())
  )  %>% 
  drop_na() %>%
  filter(!country == 2) %>%
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

dignitiy_poolsurvey_means <- cbind(dignitiy_poolsurvey_means, "Dignity")
dignitiy_poolsurvey_means <- cbind(dignitiy_poolsurvey_means, "Pooled (N = 4476)")

colnames(dignitiy_poolsurvey_means) <- c("Country Prime", "mean", "sd",
                                         "n", "se", "Outcome", "survey")

dignity_poolsurvey_means <- dignitiy_poolsurvey_means

dignity_surveys <- rbind(dignity_survey1_means, dignity_survey2_means, dignity_poolsurvey_means)

# Outcome 2

abuse_survey1_means <- survey1 %>% 
  group_by(double_standards_treatment) %>% 
  dplyr::summarise(
    mean_abuse_survey1 = mean(humanrightsabuse, na.rm = T),
    sd_abuse_survey1   = sd(humanrightsabuse, na.rm = T),
    n_abuse_survey1   = n(),
    se_abuse_survey1 = sd_abuse_survey1/sqrt(n())
  )  %>% 
  drop_na() %>% 
  mutate(
    double_standards_treatment = case_when(
      double_standards_treatment == '0' ~ "China",
      double_standards_treatment == '1' ~ "Italy",
      double_standards_treatment == '2' ~ "Spain"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy",
                  "Spain")
  )

abuse_survey1_means <- cbind(abuse_survey1_means, "Abuse")
abuse_survey1_means <- cbind(abuse_survey1_means, "Survey 1 (N = 3854)")

colnames(abuse_survey1_means) <- c("Country Prime", "mean", "sd",
                                   "n", "se", "Outcome", "survey")

abuse_survey2_means <- survey2 %>% 
  group_by(country) %>% 
  dplyr::summarise(
    mean_abuse_survey2 = mean(outcome1, na.rm = T),
    sd_abuse_survey2   = sd(outcome1, na.rm = T),
    n_abuse_survey2   = n(),
    se_abuse_survey2 = sd_abuse_survey2/sqrt(n())
  )  %>% 
  drop_na() %>% 
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

abuse_survey2_means <- cbind(abuse_survey2_means, "Abuse")
abuse_survey2_means <- cbind(abuse_survey2_means, "Survey 2 (N = 1938)")

colnames(abuse_survey2_means) <- c("Country Prime", "mean", "sd",
                                   "n", "se", "Outcome", "survey")

survey1_abuse <- survey1[, c('double_standards_treatment', 'humanrightsabuse', 'gender', 'age_num', 'region', 'employment', 'covid_employment')]
colnames(survey1_abuse) <- c('country', 'abuse_outcome', 'gender', 'age', 'region', 'employment', 'covid_employment')
survey1_abuse <- survey1_abuse[complete.cases(survey1_abuse),]

survey2_abuse <- survey2[, c('country', 'outcome1', 'gender', 'age', 'region', 'employment', 'covid_employment')]
colnames(survey2_abuse) <- c('country', 'abuse_outcome', 'gender', 'age', 'region', 'employment', 'covid_employment')
survey2_abuse <- survey2_abuse[complete.cases(survey2_abuse),]

pooled_survey_abuse <- rbind(survey1_abuse, survey2_abuse)

pooled_survey_abuse$gender <- ifelse(pooled_survey_abuse$gender %in% c("Mujer", 1), 1, 0)

abuse_poolsurvey_means <- pooled_survey_abuse[, c('country', 'abuse_outcome')] %>% 
  group_by(country) %>% 
  dplyr::summarise(
    mean_abuse_poolsurvey = mean(abuse_outcome, na.rm = T),
    sd_abuse_poolsurvey   = sd(abuse_outcome, na.rm = T),
    n_abuse_poolsurvey   = n(),
    se_abuse_poolsurvey = sd_abuse_poolsurvey/sqrt(n_abuse_poolsurvey)
  )  %>% 
  drop_na() %>%
  filter(!country == 2) %>%
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

abuse_poolsurvey_means <- cbind(abuse_poolsurvey_means, "Abuse")
abuse_poolsurvey_means <- cbind(abuse_poolsurvey_means, "Pooled (N = 4476)")

colnames(abuse_poolsurvey_means) <- c("Country Prime", "mean", "sd",
                                      "n", "se", "Outcome", "survey")

abuse_surveys <- rbind(abuse_poolsurvey_means, abuse_survey1_means, abuse_survey2_means)

# Outcome 3

authoritarian_survey1_means <- survey1 %>% 
  group_by(double_standards_treatment) %>% 
  dplyr::summarise(
    mean_authoritarian_survey1 = mean(authoritarianpolicy, na.rm = T),
    sd_authoritarian_survey1   = sd(authoritarianpolicy, na.rm = T),
    n_authoritarian_survey1   = n(),
    se_authoritarian_survey1 = sd_authoritarian_survey1/sqrt(n())
  )  %>% 
  drop_na() %>% 
  mutate(
    double_standards_treatment = case_when(
      double_standards_treatment == 0 ~ "China",
      double_standards_treatment == 1 ~ "Italy",
      double_standards_treatment == 2 ~ "Spain"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy",
                  "Spain")
  )

authoritarian_survey1_means <- cbind(authoritarian_survey1_means, "Authoritarian")
authoritarian_survey1_means <- cbind(authoritarian_survey1_means, "Survey 1 (N = 3854)")

colnames(authoritarian_survey1_means) <- c("Country Prime", "mean", "sd",
                                           "n", "se", "Outcome", "survey")


authoritarian_survey2_means <- survey2 %>% 
  group_by(country) %>% 
  dplyr::summarise(
    mean_authoritarian_survey2 = mean(outcome3, na.rm = T),
    sd_authoritarian_survey2   = sd(outcome3, na.rm = T),
    n_authoritarian_survey2   = n(),
    se_authoritarian_survey2 = sd_authoritarian_survey2/sqrt(n())
  )  %>% 
  drop_na() %>% 
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

p_value_one <- tibble(
  x = c("China", "China", "Italy", "Italy"),
  y = c(1.75, 1.80, 1.80, 1.75)
)

plot_authoritarian_survey2 <- authoritarian_survey2_means %>%
  ggplot(aes(country, mean_authoritarian_survey2)) +
  geom_errorbar(aes(ymin = mean_authoritarian_survey2 - se_authoritarian_survey2, ymax = mean_authoritarian_survey2 + se_authoritarian_survey2), width = 0.3) +
  geom_col(aes(fill = country), color = "black", width = 0.85, alpha = 0.5) +
  geom_line(data = p_value_one, 
            aes(x = x, y = y, group = 1)) +
  ggplot2::annotate("text", x = 1.5, y = 1.85, 
                    label = "p < 0.01",
                    size = 4, color = "#22292F") +
  scale_fill_manual(values = c('gray10', 'gray40')) +
  scale_y_continuous(limits = c(0, 1.9), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Country Prime",
    y = "",
    title = ""
  ) +
  ggplot2::theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35, unit = "pt")),
    plot.margin = ggplot2::margin(1, 1, 1, 1, unit = "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15, unit = "pt")),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15, unit = "pt")),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5, unit = "pt")),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5, unit = "pt")),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15, unit = "pt")),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()


authoritarian_survey2_means <- cbind(authoritarian_survey2_means, "Authoritarian")
authoritarian_survey2_means <- cbind(authoritarian_survey2_means, "Survey 2 (N = 1938)")

colnames(authoritarian_survey2_means) <- c("Country Prime", "mean", "sd",
                                           "n", "se", "Outcome", "survey")

survey1_authoritarian <- survey1[, c('double_standards_treatment', 'authoritarianpolicy', 'gender', 'age_num', 'region', 'employment', 'covid_employment')]
colnames(survey1_authoritarian) <- c('country', 'authoritarian_outcome', 'gender', 'age', 'region', 'employment', 'covid_employment')
survey1_authoritarian <- survey1_authoritarian[complete.cases(survey1_authoritarian),]

survey2_authoritarian <- survey2[, c('country', 'outcome3', 'gender', 'age', 'region', 'employment', 'covid_employment')]
colnames(survey2_authoritarian) <- c('country', 'authoritarian_outcome', 'gender', 'age', 'region', 'employment', 'covid_employment')
survey2_authoritarian <- survey2_authoritarian[complete.cases(survey2_authoritarian),]

pooled_survey_authoritarian <- rbind(survey1_authoritarian, survey2_authoritarian)

pooled_survey_authoritarian$gender <- ifelse(pooled_survey_authoritarian$gender %in% c("Mujer", 1), 1, 0)

authoritarian_poolsurvey_means <- pooled_survey_authoritarian %>% 
  group_by(country) %>% 
  dplyr::summarise(
    mean_authoritarian_poolsurvey = mean(authoritarian_outcome, na.rm = T),
    sd_authoritarian_poolsurvey   = sd(authoritarian_outcome, na.rm = T),
    n_authoritarian_poolsurvey   = n(),
    se_authoritarian_poolsurvey = sd_authoritarian_poolsurvey/sqrt(n())
  )  %>% 
  drop_na() %>%
  filter(!country == 2) %>%
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

authoritarian_poolsurvey_means <- cbind(authoritarian_poolsurvey_means, "Authoritarian")
authoritarian_poolsurvey_means <- cbind(authoritarian_poolsurvey_means, "Pooled (N = 4476)")

colnames(authoritarian_poolsurvey_means) <- c("Country Prime", "mean", "sd",
                                              "n", "se", "Outcome", "survey")

authoritarian_surveys <- rbind(authoritarian_poolsurvey_means, authoritarian_survey1_means, authoritarian_survey2_means)


# Outcome 4

privacy_survey2_means <- survey2 %>% 
  group_by(country) %>% 
  dplyr::summarise(
    mean_privacy_survey2 = mean(outcome4, na.rm = T),
    sd_privacy_survey2   = sd(outcome4, na.rm = T),
    n_privacy_survey2   = n(),
    se_privacy_survey2 = sd_privacy_survey2/sqrt(n())
  )  %>% 
  drop_na() %>% 
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

privacy_survey2_means <- cbind(privacy_survey2_means, "Privacy")

privacy_pooled_means <- privacy_survey2_means

privacy_survey2_means <- cbind(privacy_survey2_means, "Survey 2 (N = 1938)")
privacy_pooled_means <- cbind(privacy_pooled_means, "Pooled (N = 4476)")

colnames(privacy_survey2_means) <- c("Country Prime", "mean", "sd",
                                     "n", "se", "Outcome", "survey")
colnames(privacy_pooled_means) <- c("Country Prime", "mean", "sd",
                                    "n", "se", "Outcome", "survey")

privacy_surveys <- rbind(privacy_survey2_means)

#All outcomes together

all_surveys <- rbind(dignity_surveys, abuse_surveys, authoritarian_surveys, privacy_surveys)

ann_text <- data.frame("text", color = "#22292F",
                       Outcome = factor(3,levels = c("Pooled (N = 4476)", "Survey 1 (N = 3854)","Survey 2 (N = 1938)")))

t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

#Survey 1: abuse: China vs. Italy or Spain
survey1_abuse_pvalue <- t.test(survey1[survey1$double_standards_treatment == 0,]$humanrightsabuse, 
                               survey1[survey1$double_standards_treatment > 0,]$humanrightsabuse
)$p.value

#Survey 2: abuse: China vs. Italy
survey2_abuse_pvalue <- t.test(survey2[survey2$country == 0,]$outcome1, 
                               survey2[survey2$country == 1,]$outcome1)$p.value

#Pooled Survey: abuse: China vs. Italy
pooled_abuse_pvalue <- t.test(pooled_survey_abuse[pooled_survey_abuse$country == 0,]$abuse_outcome, 
                              pooled_survey_abuse[pooled_survey_abuse$country == 1,]$abuse_outcome)$p.value


#Survey 1: authoritarian: China vs. Italy or Spain
survey1_authoritarian_pvalue <- t.test(survey1[survey1$double_standards_treatment == 0,]$authoritarianpolicy, 
                                       survey1[survey1$double_standards_treatment > 0,]$authoritarianpolicy
)$p.value

#Survey 2: authoritarian: China vs. Italy
survey2_authoritarian_pvalue <- t.test(survey2[survey2$country == 0,]$outcome3, 
                                       survey2[survey2$country == 1,]$outcome3)$p.value

#Pooled Survey: authoritarian: China vs. Italy
pooled_authoritarian_pvalue <- t.test(pooled_survey_authoritarian[pooled_survey_authoritarian$country == 0,]$authoritarian_outcome, 
                                      pooled_survey_authoritarian[pooled_survey_authoritarian$country == 1,]$authoritarian_outcome)$p.value

#Survey 1: human dignity: China vs. Italy or Spain
survey1_dignity_pvalue <- t.test(survey1[survey1$double_standards_treatment == 0,]$humandignity, 
                                 survey1[survey1$double_standards_treatment > 0,]$humandignity
)$p.value

#Survey 2: human dignity: China vs. Italy
survey2_dignity_pvalue <-  t.test(survey2[survey2$country == 0,]$outcome2, 
                                  survey2[survey2$country == 1,]$outcome2)$p.value

#Pooled Survey: human dignity: China vs. Italy
pooled_dignity_pvalue <- t.test(pooled_survey_humandignity[pooled_survey_humandignity$country == 0,]$dignity_outcome, 
                                pooled_survey_humandignity[pooled_survey_humandignity$country == 1,]$dignity_outcome)$p.value

#Survey 2: privacy: China vs. Italy
survey2_authoritarian_pvalue <- t.test(survey2[survey2$country == 0,]$outcome4, 
                                       survey2[survey2$country == 1,]$outcome4)$p.value

#Pooled Survey: privacy: China vs. Italy
survey2_authoritarian_pvalue <- t.test(survey2[survey2$country == 0,]$outcome4, 
                                       survey2[survey2$country == 1,]$outcome4)$p.value


data.segm1<-data.frame(x = 4.25, y = 2.30, xend = 4.25, yend = 2.35,
                       survey=c("Pooled (N = 4476)","Survey 2 (N = 1938)"))

data.segm2<-data.frame(x = 3.75, y = 2.35, xend = 4.25, yend = 2.35,
                       survey=c("Pooled (N = 4476)","Survey 2 (N = 1938)"))

data.segm3<-data.frame(x = 3.75, y = 2.30, xend = 3.75, yend = 2.35,
                       survey=c("Pooled (N = 4476)","Survey 2 (N = 1938)"))

all_surveys_plot <- all_surveys %>% # data
  mutate(Outcome = factor(Outcome, levels = c("Abuse", "Dignity", "Authoritarian", "Privacy"))) %>%
  ggplot() +
  geom_bar(aes(x=Outcome, y=mean, 
               fill=`Country Prime`), 
           stat = "identity", 
           position="dodge") +
  facet_wrap(~ survey, scales = "free_x") +
  scale_fill_manual(values = c('gray10', 'gray50', 'gray55')) +
  scale_y_continuous(limits = c(0, 2.6), expand = c(0, 0)) +
  
  geom_segment(aes(x = 0.75, y = 2.35, xend = 1.25, yend = 2.35)) +
  geom_segment(aes(x = 0.75, y = 2.30, xend = 0.75, yend = 2.35)) +
  geom_segment(aes(x = 1.25, y = 2.30, xend = 1.25, yend = 2.35)) +
  
  geom_segment(aes(x = 1.75, y = 2.35, xend = 2.25, yend = 2.35)) +
  geom_segment(aes(x = 1.75, y = 2.30, xend = 1.75, yend = 2.35)) +
  geom_segment(aes(x = 2.25, y = 2.30, xend = 2.25, yend = 2.35)) +
  
  geom_segment(aes(x = 2.75, y = 2.35, xend = 3.25, yend = 2.35)) +
  geom_segment(aes(x = 2.75, y = 2.30, xend = 2.75, yend = 2.35)) +
  geom_segment(aes(x = 3.25, y = 2.30, xend = 3.25, yend = 2.35)) +
  
  ggplot2::geom_text(data = ann_text, x = 1, y = 2.41, size = 2.5,#abuse
                     label = c("p < .01", "p < .01", "p < 0.01")) +
  ggplot2::geom_text(data = ann_text, x = 2, y = 2.41, size = 2.5,#dignity
                     label = c("p < .01", "p < .01", "p = .06")) +
  ggplot2::geom_text(data = ann_text, x = 3, y = 2.41, size = 2.5,#authoritarian
                     label = c("p < .01", "p < .01", "p < 0.01")) +
  ggplot2::geom_text(data = ann_text, x = 4, y = 2.41, size = 2.5,#privacy
                     label = c("p < .01", "", "p < .01")) +
  theme_minimal() +
  labs(
    x = "Outcome of interest",
    y = "",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

all_surveys_plot

png(filename="plot_all_surveys.png", 
    units="in", 
    width=13, 
    height=7, 
    pointsize=15, 
    res=1000)
all_surveys_plot
dev.off()


#################
#               #
#   Figure 2    #
#               #
#################

# Outcome 1

abuse_longpolicy_means <- survey3 %>% 
  group_by(treatments) %>% 
  dplyr::summarise(
    mean_abuse_long = mean(outcome1, na.rm = T),
    sd_abuse_long   = sd(outcome1, na.rm = T),
    n_abuse_long   = n(),
    se_abuse_long = sd_abuse_long/sqrt(n())
  )  %>% 
  drop_na()

p_value_one <- tibble(
  x = c(1, 1, 1.99, 1.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome1, 
             survey3[survey3$treatments == "no country - authoritarian",]$outcome1, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

p_value_onehalf <- tibble(
  x = c(2.01, 2.01, 2.99, 2.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "no country - authoritarian",]$outcome1, 
             survey3[survey3$treatments == "no country - democratic",]$outcome1, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

p_value_twohalf <- tibble(
  x = c(3.01, 3.01, 3.99, 3.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "no country - democratic",]$outcome1, 
             survey3[survey3$treatments == "no country - no label",]$outcome1, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

plot_abuse_longpolicy <- abuse_longpolicy_means[c(1:4),] %>% 
  ggplot(aes(treatments, mean_abuse_long)) +
  geom_errorbar(aes(ymin = mean_abuse_long-se_abuse_long, ymax = mean_abuse_long+se_abuse_long), width = 0.3) +
  geom_col(aes(fill = treatments), color = "black", width = 0.85, alpha = 0.75) +
  geom_line(data = p_value_one, 
            aes(x = x, y = y, group = 1)) +
  geom_line(data = p_value_onehalf, 
            aes(x = x, y = y, group = 1)) +
  geom_line(data = p_value_twohalf, 
            aes(x = x, y = y, group = 1)) +
  ggplot2::annotate("text", x = 1.5, y = 3.55, 
                    label = "p = .03",
                    size = 5, color = "#22292F")  +
  ggplot2::annotate("text", x = 2.5, y = 3.55, 
                    label = "p < .01",
                    size = 5, color = "#22292F")  +
  ggplot2::annotate("text", x = 3.5, y = 3.55, 
                    label = "p = 0.01",
                    size = 5, color = "#22292F")  +
  scale_x_discrete(limits = c('China - authoritarian', 'no country - authoritarian', 'no country - democratic', 'no country - no label'),
                   labels = c(
                     "China - authoritarian" = "China \n + \n Authoritarian",
                     "no country - authoritarian" = "Authoritarian",
                     "no country - democratic" = "Democratic",
                     "no country - no label" = "A country"
                   )) +
  scale_fill_manual(values = c('gray70', 'gray30', 'gray50', 'gray5')) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Treatments",
    y = "Stay-at-home Orders Are \n An Abuse of Human Rights",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_abuse_longpolicy

png(filename="plot_abuse_longpolicy.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_abuse_longpolicy
dev.off()

# Outcome 2

dignity_longpolicy_means <- survey3 %>% 
  group_by(treatments) %>% 
  dplyr::summarise(
    mean_dignity_long = mean(outcome2, na.rm = T),
    sd_dignity_long   = sd(outcome2, na.rm = T),
    n_dignity_long   = n(),
    se_dignity_long = sd_dignity_long/sqrt(n())
  )  %>% 
  drop_na()

p_value_one <- tibble(
  x = c(1, 1, 1.99, 1.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "China - no label",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

p_value_onehalf <- tibble(
  x = c(2.01, 2.01, 2.99, 2.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "no country - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "China - no label",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

p_value_twohalf <- tibble(
  x = c(3.01, 3.01, 3.99, 3.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "no country - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "no country - no label",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

plot_dignity_longpolicy <- dignity_longpolicy_means[c(1:4),] %>% 
  ggplot(aes(treatments, mean_dignity_long)) +
  geom_errorbar(aes(ymin = mean_dignity_long-se_dignity_long, ymax = mean_dignity_long+se_dignity_long), width = 0.3) +
  geom_col(aes(fill = treatments), color = "black", width = 0.85, alpha = 0.75) +
  geom_line(data = p_value_one, 
            aes(x = x, y = y, group = 1)) +
  geom_line(data = p_value_onehalf, 
            aes(x = x, y = y, group = 1)) +
  geom_line(data = p_value_twohalf, 
            aes(x = x, y = y, group = 1)) +
  ggplot2::annotate("text", x = 1.5, y = 3.55, 
                    label = "p = .01",
                    size = 5, color = "#22292F")  +
  ggplot2::annotate("text", x = 2.5, y = 3.55, 
                    label = "p < .01",
                    size = 5, color = "#22292F")  +
  ggplot2::annotate("text", x = 3.5, y = 3.55, 
                    label = "n.s.",
                    size = 5, color = "#22292F")  +
  scale_x_discrete(limits = c('China - authoritarian', 'no country - authoritarian', 'no country - democratic', 'no country - no label'),
                   labels = c(
                     "China - authoritarian" = "China \n + \n Authoritarian",
                     "no country - authoritarian" = "Authoritarian",
                     "no country - democratic" = "Democratic",
                     "no country - no label" = "A country"
                   )) +
  scale_fill_manual(values = c('gray70', 'gray30', 'gray50', 'gray5')) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Treatments",
    y = "Stay-at-home Orders Do Not \n Respect Human Dignity",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_dignity_longpolicy

png(filename="plot_dignity_longpolicy.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_dignity_longpolicy
dev.off()

# Outcome 3 
authoritarian_longpolicy_means <- survey3 %>% 
  group_by(treatments) %>% 
  dplyr::summarise(
    mean_authoritarian_long = mean(outcome3, na.rm = T),
    sd_authoritarian_long   = sd(outcome3, na.rm = T),
    n_authoritarian_long   = n(),
    se_authoritarian_long = sd_authoritarian_long/sqrt(n())
  )  %>% 
  drop_na()

p_value_one <- tibble(
  x = c(1, 1, 1.99, 1.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome3, 
             survey3[survey3$treatments == "no country - authoritarian",]$outcome3, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

p_value_onehalf <- tibble(
  x = c(2.01, 2.01, 2.99, 2.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "no country - authoritarian",]$outcome3, 
             survey3[survey3$treatments == "no country - democratic",]$outcome3, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

p_value_twohalf <- tibble(
  x = c(3.01, 3.01, 3.99, 3.99),
  y = c(3.4, 3.45, 3.45, 3.40)
)

round(t.test(survey3[survey3$treatments == "no country - democratic",]$outcome3, 
             survey3[survey3$treatments == "no country - no label",]$outcome3, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

plot_authoritarian_longpolicy <- authoritarian_longpolicy_means[c(1:4),] %>% 
  ggplot(aes(treatments, mean_authoritarian_long)) +
  geom_errorbar(aes(ymin = mean_authoritarian_long-se_authoritarian_long, ymax = mean_authoritarian_long+se_authoritarian_long), width = 0.3) +
  geom_col(aes(fill = treatments), color = "black", width = 0.85, alpha = 0.75) +
  geom_line(data = p_value_one, 
            aes(x = x, y = y, group = 1)) +
  geom_line(data = p_value_onehalf, 
            aes(x = x, y = y, group = 1)) +
  geom_line(data = p_value_twohalf, 
            aes(x = x, y = y, group = 1)) +
  ggplot2::annotate("text", x = 1.5, y = 3.55, 
                    label = "p < .01",
                    size = 5, color = "#22292F")  +
  ggplot2::annotate("text", x = 2.5, y = 3.55, 
                    label = "p < .01",
                    size = 5, color = "#22292F")  +
  ggplot2::annotate("text", x = 3.5, y = 3.55, 
                    label = "n.s.",
                    size = 5, color = "#22292F")  +
  scale_x_discrete(limits = c('China - authoritarian', 'no country - authoritarian', 'no country - democratic', 'no country - no label'),
                   labels = c(
                     "China - authoritarian" = "China \n + \n Authoritarian",
                     "no country - authoritarian" = "Authoritarian",
                     "no country - democratic" = "Democratic",
                     "no country - no label" = "A country"
                   )) +
  scale_fill_manual(values = c('gray70', 'gray30', 'gray50', 'gray5')) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Treatments",
    y = "Stay-at-home Orders Are \n Authoritarian Policies",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_authoritarian_longpolicy

png(filename="plot_authoritarian_longpolicy.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_authoritarian_longpolicy
dev.off()

png(filename="allplot_longpolicy_horizontal.png", 
    units="in", 
    width=15, 
    height=8, 
    pointsize=15, 
    res=1000)
ggarrange(plot_dignity_longpolicy, plot_abuse_longpolicy, plot_authoritarian_longpolicy, 
          labels = c(),
          ncol = 3, nrow = 1)
dev.off()


#################
#               #
#   Figure 3    #
#               #
#################

mechanism_survey2_means_out1 <- survey2 %>% 
  group_by(country, condition) %>% 
  dplyr::summarise(
    mean_mechanism_survey2 = mean(outcome1, na.rm = T),
    sd_mechanism_survey2   = sd(outcome1, na.rm = T),
    n_mechanism_survey2   = n(),
    se_mechanism_survey2 = sd_mechanism_survey2/sqrt(n())
  )  %>% 
  drop_na()  %>%
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

plot_survey2_out1 <- mechanism_survey2_means_out1 %>% 
  ggplot(aes(condition, mean_mechanism_survey2)) +
  geom_errorbar(aes(ymin = mean_mechanism_survey2-se_mechanism_survey2, ymax = mean_mechanism_survey2+se_mechanism_survey2), width = 0.3) +
  geom_col(aes(fill = country), color = "black", width = 0.85, alpha = 0.5) +
  facet_wrap(~country, nrow=1) +
  scale_fill_manual(values = c('gray10', 'gray70')) +
  scale_y_continuous(limits = c(0, 2.2), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Country and mechanism",
    y = "Stay-at-Home Orders Do Not \n Respect Human Dignity",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_survey2_out1 <- plot_survey2_out1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                               axis.title.x = element_text(size = 18, face = 'bold', vjust = 1),
                                               axis.title.y = element_text(size = 18, face = 'bold', vjust = 1))

png(filename="plot_mechanisms_out1.png", 
    units="in", 
    width=13, 
    height=7, 
    pointsize=15, 
    res=1000)
plot_survey2_out1
dev.off()

mechanism_survey2_means_out2 <- survey2 %>% 
  filter(condition != "NA") %>%
  group_by(country, condition) %>% 
  dplyr::summarise(
    mean_mechanism_survey2 = mean(outcome2, na.rm = T),
    sd_mechanism_survey2   = sd(outcome2, na.rm = T),
    n_mechanism_survey2   = n(),
    se_mechanism_survey2 = sd_mechanism_survey2/sqrt(n())
  )  %>% 
  drop_na()  %>%
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

plot_survey2_out2 <- mechanism_survey2_means_out2 %>% 
  ggplot(aes(condition, mean_mechanism_survey2)) +
  geom_errorbar(aes(ymin = mean_mechanism_survey2-se_mechanism_survey2, ymax = mean_mechanism_survey2+se_mechanism_survey2), width = 0.3) +
  geom_col(aes(fill = country), color = "black", width = 0.85, alpha = 0.5) +
  facet_wrap(~country, nrow=1) +
  scale_fill_manual(values = c('gray10', 'gray70')) +
  scale_y_continuous(limits = c(0, 1.8), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Country and mechanism",
    y = "Stay-at-Home Orders Are \n An Abuse of Human Rights",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_survey2_out2 <- plot_survey2_out2 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                               axis.title.x = element_text(size = 18, face = 'bold', vjust = 1),
                                               axis.title.y = element_text(size = 18, face = 'bold', vjust = 1))

png(filename="plot_mechanisms_out2.png", 
    units="in", 
    width=13, 
    height=7, 
    pointsize=15, 
    res=1000)
plot_survey2_out2
dev.off()

mechanism_survey2_means_out3 <- survey2 %>% 
  filter(condition != "NA") %>%
  group_by(country, condition) %>% 
  dplyr::summarise(
    mean_mechanism_survey2 = mean(outcome3, na.rm = T),
    sd_mechanism_survey2   = sd(outcome3, na.rm = T),
    n_mechanism_survey2   = n(),
    se_mechanism_survey2 = sd_mechanism_survey2/sqrt(n())
  )  %>% 
  drop_na()  %>%
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )

plot_survey2_out3 <- mechanism_survey2_means_out3 %>% 
  ggplot(aes(condition, mean_mechanism_survey2)) +
  geom_errorbar(aes(ymin = mean_mechanism_survey2-se_mechanism_survey2, ymax = mean_mechanism_survey2+se_mechanism_survey2), width = 0.3) +
  geom_col(aes(fill = country), color = "black", width = 0.85, alpha = 0.5) +
  facet_wrap(~country, nrow=1) +
  scale_fill_manual(values = c('gray10', 'gray70')) +
  scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Country and mechanism",
    y = "Stay-at-Home Orders Are \n Authoritarian Policies",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_survey2_out3 <- plot_survey2_out3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                               axis.title.x = element_text(size = 18, face = 'bold', vjust = 1),
                                               axis.title.y = element_text(size = 18, face = 'bold', vjust = 1))

png(filename="plot_mechanisms_out3.png", 
    units="in", 
    width=13, 
    height=7, 
    pointsize=15, 
    res=1000)
plot_survey2_out3
dev.off()

mechanism_survey2_means_out4 <- survey2 %>% 
  filter(condition != "NA") %>%
  group_by(country, condition) %>% 
  dplyr::summarise(
    mean_mechanism_survey2 = mean(outcome4, na.rm = T),
    sd_mechanism_survey2   = sd(outcome4, na.rm = T),
    n_mechanism_survey2   = n(),
    se_mechanism_survey2 = sd_mechanism_survey2/sqrt(n())
  )  %>% 
  drop_na()  %>%
  mutate(
    country = case_when(
      country == 0 ~ "China",
      country == 1 ~ "Italy"
    ) %>% 
      as.factor %>% 
      fct_relevel("China", "Italy")
  )


plot_survey2_out4 <- mechanism_survey2_means_out4 %>% 
  ggplot(aes(condition, mean_mechanism_survey2)) +
  geom_errorbar(aes(ymin = mean_mechanism_survey2-se_mechanism_survey2, ymax = mean_mechanism_survey2+se_mechanism_survey2), width = 0.3) +
  geom_col(aes(fill = country), color = "black", width = 0.85, alpha = 0.5) +
  facet_wrap(~country, nrow=1) +
  scale_fill_manual(values = c('gray10', 'gray70')) +
  scale_y_continuous(limits = c(0, 2.6), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Country and mechanism",
    y = "Phone Tracking Policies \n Violate Human Rights",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_survey2_out4 <- plot_survey2_out4 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                               axis.title.x = element_text(size = 18, face = 'bold', vjust = 1),
                                               axis.title.y = element_text(size = 18, face = 'bold', vjust = 1))

png(filename="plot_mechanisms_out4.png", 
    units="in", 
    width=13, 
    height=7, 
    pointsize=15, 
    res=1000)
plot_survey2_out4
dev.off()


plot_survey2_out1_all <- plot_survey2_out1 + theme(
  axis.title.x = element_blank())

plot_survey2_out2_all <- plot_survey2_out2 + theme(
  axis.title.x = element_blank())

plot_survey2_out3_all <- plot_survey2_out3 + theme(
  axis.title.x = element_blank())

plot_survey2_out4_all <- plot_survey2_out4 + theme(
  axis.title.x = element_blank())


png(filename="all_mechanisms.png", 
    units="in", 
    width=9, 
    height=12, 
    pointsize=15, 
    res=1000)
ggarrange(plot_survey2_out2_all, plot_survey2_out1_all, plot_survey2_out3_all, plot_survey2_out4_all, 
          labels = c(" A)", "B)", 
                     "C)", "D)"),
          ncol = 2, nrow = 2)
dev.off()


#################
#               #
#   Figure 4    #
#               #
#################

(experiment_results <- survey_multicountry %>% 
   dplyr::group_by(country, double_china) %>% 
   dplyr::summarise(
     mean_out1= mean(outcome1, na.rm = T),
     sd_out1 = sd(outcome1, na.rm = T),
     n_out1 = sum(!is.na(outcome1)),
     mean_out2= mean(outcome2, na.rm = T),
     sd_out2 = sd(outcome2, na.rm = T),
     n_out2 = sum(!is.na(outcome2)),
     mean_out3= mean(outcome3, na.rm = T),
     sd_out3 = sd(outcome3, na.rm = T),
     n_out3 = sum(!is.na(outcome3)),
     mean_out4= mean(outcome4, na.rm = T),
     sd_out4 = sd(outcome4, na.rm = T),
     n_out4 = sum(!is.na(outcome4)),
   ) 
)

(experiment_results_all <- survey_multicountry %>% 
    dplyr::group_by(double_china) %>% 
    dplyr::summarise(
      mean_out1= mean(outcome1, na.rm = T),
      sd_out1 = sd(outcome1, na.rm = T),
      mean_out2= mean(outcome2, na.rm = T),
      sd_out2 = sd(outcome2, na.rm = T),
      mean_out3= mean(outcome3, na.rm = T),
      sd_out3 = sd(outcome3, na.rm = T),
      mean_out4= mean(outcome4, na.rm = T),
      sd_out4 = sd(outcome4, na.rm = T)
    ) 
)

experiment_results_with_global <- rbind(experiment_results,
                                        data.frame(country = " Six Countries", experiment_results_all))


experiment_results_with_global$country <- relevel(as.factor(experiment_results_with_global$country), " Six Countries")
experiment_results_with_global$double_china <- ifelse(experiment_results_with_global$double_china == 1,
                                                      'China', 'Italy')

experiment_results_with_global[experiment_results_with_global$country == " Six Countries" & 
                                 experiment_results_with_global$double_china == 'Italy',]$n_out1 <- sum(experiment_results_with_global[experiment_results_with_global$double_china == 'Italy',]$n_out1, na.rm = T)

experiment_results_with_global[experiment_results_with_global$country == " Six Countries" & 
                                 experiment_results_with_global$double_china == 'China',]$n_out1 <- sum(experiment_results_with_global[experiment_results_with_global$double_china == 'China',]$n_out1, na.rm = T)

experiment_results_with_global[experiment_results_with_global$country == " Six Countries" & 
                                 experiment_results_with_global$double_china == 'Italy',]$n_out2 <- sum(experiment_results_with_global[experiment_results_with_global$double_china == 'Italy',]$n_out2, na.rm = T)

experiment_results_with_global[experiment_results_with_global$country == " Six Countries" & 
                                 experiment_results_with_global$double_china == 'China',]$n_out2 <- sum(experiment_results_with_global[experiment_results_with_global$double_china == 'China',]$n_out2, na.rm = T)

experiment_results_with_global[experiment_results_with_global$country == " Six Countries" & 
                                 experiment_results_with_global$double_china == 'Italy',]$n_out3 <- sum(experiment_results_with_global[experiment_results_with_global$double_china == 'Italy',]$n_out3, na.rm = T)

experiment_results_with_global[experiment_results_with_global$country == " Six Countries" & 
                                 experiment_results_with_global$double_china == 'China',]$n_out3 <- sum(experiment_results_with_global[experiment_results_with_global$double_china == 'China',]$n_out3, na.rm = T)

experiment_results_with_global[experiment_results_with_global$country == " Six Countries" & 
                                 experiment_results_with_global$double_china == 'Italy',]$n_out4 <- sum(experiment_results_with_global[experiment_results_with_global$double_china == 'Italy',]$n_out4, na.rm = T)

experiment_results_with_global[experiment_results_with_global$country == " Six Countries" & 
                                 experiment_results_with_global$double_china == 'China',]$n_out4 <- sum(experiment_results_with_global[experiment_results_with_global$double_china == 'China',]$n_out4, na.rm = T)

country_names <- list(
  " Six Countries"=" Six Countries \n (N ≈ 6,350)",
  'Brazil'="Brazil \n (N ≈ 1,000)",
  'France'="France \n (N ≈ 1,000)",
  'Germany'="Germany \n (N ≈ 1,000)",
  'Mexico'="Mexico \n (N ≈ 1,150)",
  'United Kingdom'="United Kingdom \n (N ≈ 1,000)",
  'United States'="United States \n (N ≈ 1,200)"
)

country_labeller <- function(variable,value){
  return(country_names[value])
}

#Obtaining p-values

#Global estimates (pooling all countries) - outcome 1; p < 0.01
t.test(survey_multicountry[survey_multicountry$double_china == 0,]$outcome1, 
       survey_multicountry[survey_multicountry$double_china == 1,]$outcome1, 
       alternative = "two.sided", var.equal = FALSE)
#Global estimates (pooling all countries) - outcome 2; p < 0.01
t.test(survey_multicountry[survey_multicountry$double_china == 0,]$outcome2, 
       survey_multicountry[survey_multicountry$double_china == 1,]$outcome2, 
       alternative = "two.sided", var.equal = FALSE)
#Global estimates (pooling all countries) - outcome 3; p < 0.01
t.test(survey_multicountry[survey_multicountry$double_china == 0,]$outcome3, 
       survey_multicountry[survey_multicountry$double_china == 1,]$outcome3, 
       alternative = "two.sided", var.equal = FALSE)
#Global estimates (pooling all countries) - outcome 4; p < 0.01
t.test(survey_multicountry[survey_multicountry$double_china == 0,]$outcome4, 
       survey_multicountry[survey_multicountry$double_china == 1,]$outcome4, 
       alternative = "two.sided", var.equal = FALSE)

#Brazil estimate - outcome 1; p < 0.01
t.test(survey_multicountry[survey_multicountry$country == "Brazil" & survey_multicountry$double_china == 0,]$outcome1, 
       survey_multicountry[survey_multicountry$country == "Brazil" & survey_multicountry$double_china == 1,]$outcome1, 
       alternative = "two.sided", var.equal = FALSE)
#Brazil estimate - outcome 2; p = 0.04
t.test(survey_multicountry[survey_multicountry$country == "Brazil" & survey_multicountry$double_china == 0,]$outcome2, 
       survey_multicountry[survey_multicountry$country == "Brazil" & survey_multicountry$double_china == 1,]$outcome2, 
       alternative = "two.sided", var.equal = FALSE)
#Brazil estimate - outcome 3; p = 0.03
t.test(survey_multicountry[survey_multicountry$country == "Brazil" & survey_multicountry$double_china == 0,]$outcome3, 
       survey_multicountry[survey_multicountry$country == "Brazil" & survey_multicountry$double_china == 1,]$outcome3, 
       alternative = "two.sided", var.equal = FALSE)
#Brazil estimate - outcome 4; p < 0.01
t.test(survey_multicountry[survey_multicountry$country == "Brazil" & survey_multicountry$double_china == 0,]$outcome4, 
       survey_multicountry[survey_multicountry$country == "Brazil" & survey_multicountry$double_china == 1,]$outcome4, 
       alternative = "two.sided", var.equal = FALSE)

#France estimate - outcome 1; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "France" & survey_multicountry$double_china == 0,]$outcome1, 
       survey_multicountry[survey_multicountry$country == "France" & survey_multicountry$double_china == 1,]$outcome1, 
       alternative = "two.sided", var.equal = FALSE)
#France estimate - outcome 2; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "France" & survey_multicountry$double_china == 0,]$outcome2, 
       survey_multicountry[survey_multicountry$country == "France" & survey_multicountry$double_china == 1,]$outcome2, 
       alternative = "two.sided", var.equal = FALSE)
#France estimate - outcome 3; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "France" & survey_multicountry$double_china == 0,]$outcome3, 
       survey_multicountry[survey_multicountry$country == "France" & survey_multicountry$double_china == 1,]$outcome3, 
       alternative = "two.sided", var.equal = FALSE)
#France estimate - outcome 4; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "France" & survey_multicountry$double_china == 0,]$outcome4, 
       survey_multicountry[survey_multicountry$country == "France" & survey_multicountry$double_china == 1,]$outcome4, 
       alternative = "two.sided", var.equal = FALSE)

#Germany estimate - outcome 1; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "Germany" & survey_multicountry$double_china == 0,]$outcome1, 
       survey_multicountry[survey_multicountry$country == "Germany" & survey_multicountry$double_china == 1,]$outcome1, 
       alternative = "two.sided", var.equal = FALSE)
#Germany estimate - outcome 2; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "Germany" & survey_multicountry$double_china == 0,]$outcome2, 
       survey_multicountry[survey_multicountry$country == "Germany" & survey_multicountry$double_china == 1,]$outcome2, 
       alternative = "two.sided", var.equal = FALSE)
#Germany estimate - outcome 3; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "Germany" & survey_multicountry$double_china == 0,]$outcome3, 
       survey_multicountry[survey_multicountry$country == "Germany" & survey_multicountry$double_china == 1,]$outcome3, 
       alternative = "two.sided", var.equal = FALSE)
#Germany estimate - outcome 4; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "Germany" & survey_multicountry$double_china == 0,]$outcome4, 
       survey_multicountry[survey_multicountry$country == "Germany" & survey_multicountry$double_china == 1,]$outcome4, 
       alternative = "two.sided", var.equal = FALSE)

#Mexico estimate - outcome 1; p = 0.008
t.test(survey_multicountry[survey_multicountry$country == "Mexico" & survey_multicountry$double_china == 0,]$outcome1, 
       survey_multicountry[survey_multicountry$country == "Mexico" & survey_multicountry$double_china == 1,]$outcome1, 
       alternative = "two.sided", var.equal = FALSE)
#Mexico estimate - outcome 2; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "Mexico" & survey_multicountry$double_china == 0,]$outcome2, 
       survey_multicountry[survey_multicountry$country == "Mexico" & survey_multicountry$double_china == 1,]$outcome2, 
       alternative = "two.sided", var.equal = FALSE)
#Mexico estimate - outcome 3; p < 0.01
t.test(survey_multicountry[survey_multicountry$country == "Mexico" & survey_multicountry$double_china == 0,]$outcome3, 
       survey_multicountry[survey_multicountry$country == "Mexico" & survey_multicountry$double_china == 1,]$outcome3, 
       alternative = "two.sided", var.equal = FALSE)
#Mexico estimate - outcome 4; p = 0.09
t.test(survey_multicountry[survey_multicountry$country == "Mexico" & survey_multicountry$double_china == 0,]$outcome4, 
       survey_multicountry[survey_multicountry$country == "Mexico" & survey_multicountry$double_china == 1,]$outcome4, 
       alternative = "two.sided", var.equal = FALSE)

#United States estimate - outcome 1; p = 0.08
t.test(survey_multicountry[survey_multicountry$country == "United States" & survey_multicountry$double_china == 0,]$outcome1, 
       survey_multicountry[survey_multicountry$country == "United States" & survey_multicountry$double_china == 1,]$outcome1, 
       alternative = "two.sided", var.equal = FALSE)
#United States estimate - outcome 2; p = 0.03
t.test(survey_multicountry[survey_multicountry$country == "United States" & survey_multicountry$double_china == 0,]$outcome2, 
       survey_multicountry[survey_multicountry$country == "United States" & survey_multicountry$double_china == 1,]$outcome2, 
       alternative = "two.sided", var.equal = FALSE)
#United States estimate - outcome 3; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "United States" & survey_multicountry$double_china == 0,]$outcome3, 
       survey_multicountry[survey_multicountry$country == "United States" & survey_multicountry$double_china == 1,]$outcome3, 
       alternative = "two.sided", var.equal = FALSE)
#United States estimate - outcome 4; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "United States" & survey_multicountry$double_china == 0,]$outcome4, 
       survey_multicountry[survey_multicountry$country == "United States" & survey_multicountry$double_china == 1,]$outcome4, 
       alternative = "two.sided", var.equal = FALSE)

#United Kingdom estimate - outcome 1; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "United Kingdom" & survey_multicountry$double_china == 0,]$outcome1, 
       survey_multicountry[survey_multicountry$country == "United Kingdom" & survey_multicountry$double_china == 1,]$outcome1, 
       alternative = "two.sided", var.equal = FALSE)
#United Kingdom estimate - outcome 2; p = 0.04
t.test(survey_multicountry[survey_multicountry$country == "United Kingdom" & survey_multicountry$double_china == 0,]$outcome2, 
       survey_multicountry[survey_multicountry$country == "United Kingdom" & survey_multicountry$double_china == 1,]$outcome2, 
       alternative = "two.sided", var.equal = FALSE)
#United Kingdom estimate - outcome 3; p = 0.03
t.test(survey_multicountry[survey_multicountry$country == "United Kingdom" & survey_multicountry$double_china == 0,]$outcome3, 
       survey_multicountry[survey_multicountry$country == "United Kingdom" & survey_multicountry$double_china == 1,]$outcome3, 
       alternative = "two.sided", var.equal = FALSE)
#United Kingdom estimate - outcome 4; p < 0.001
t.test(survey_multicountry[survey_multicountry$country == "United Kingdom" & survey_multicountry$double_china == 0,]$outcome4, 
       survey_multicountry[survey_multicountry$country == "United Kingdom" & survey_multicountry$double_china == 1,]$outcome4, 
       alternative = "two.sided", var.equal = FALSE)


dat_text_out1 <- data.frame(
  label = c("p < 0.01", "p < 0.01", "p < 0.01", "p < 0.01", "p < 0.01", "p < 0.01", "p = 0.08"),
  country   = c(" Six Countries", 'Brazil', 'France', 'Germany', 'Mexico', 'United Kingdom', 'United States')
)

dat_text_out2 <- data.frame(
  label = c("p < 0.01", "p = 0.04", "p < 0.01", "p < 0.01", "p < 0.01", "p = 0.03", "p = 0.04"),
  country   = c(" Six Countries", 'Brazil', 'France', 'Germany', 'Mexico', 'United States', 'United Kingdom')
)

dat_text_out3 <- data.frame(
  label = c("p < 0.01", "p = 0.03", "p < 0.01", "p < 0.01", "p < 0.01", "p < 0.01", "p = 0.03"),
  country   = c(" Six Countries", 'Brazil', 'France', 'Germany', 'Mexico', 'United States', 'United Kingdom')
)

dat_text_out4 <- data.frame(
  label = c("p < 0.01", "p < 0.01", "p < 0.01", "p < 0.01", "p = 0.09", "p < 0.01", "p < 0.01"),
  country   = c(" Six Countries", 'Brazil', 'France', 'Germany', 'Mexico', 'United States', 'United Kingdom')
)

experiment_results_with_global$double_china <- as.factor(experiment_results_with_global$double_china)
experiment_results_with_global$double_china <- relevel(experiment_results_with_global$double_china, "Italy")


plot_abuse_global <- experiment_results_with_global %>% 
  ggplot(aes(double_china, mean_out1)) +
  geom_col(aes(fill = double_china), color = "black", width = 0.90) + 
  facet_grid(. ~ country, labeller = country_labeller) +
  scale_fill_grey(start = 0.3) +
  scale_y_continuous(limits = c(0, 2.8), expand = c(0, 0)) +
  guides(fill = FALSE) +
  geom_line(data = p_value_one, 
            aes(x = x, y = y, group = 1)) +
  theme_minimal() +
  labs(
    x = "Country Prime",
    y = "Stay-at-Home Orders Are Always \n An Abuse of Human Rights",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 5,
                              face = "bold",
                              margin = ggplot2::margin(b = 0)),
    plot.margin = unit(rep(0, 4), "cm"),
    axis.text = element_text(size = 11, color = "#22292F"),
    axis.title = element_text(size = 13, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 5)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 0)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 0)),
    plot.caption = element_text(size = 15, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

plot_abuse_global <- plot_abuse_global +
  geom_text(
    size    = 3,
    data    = dat_text_out1,
    mapping = aes(x = 1.5, y = 2.5, label = label)
  ) +
  theme(
    strip.text = element_text(
      size = 12, color = "black", face = "bold"
    ))

plot_abuse_global

png(filename="plot_abuse_global.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_abuse_global
dev.off()

p_value_one <- tibble(
  x = c("China", "China", "Italy", "Italy"),
  y = c(2.40, 2.35, 2.35, 2.40)
)

plot_dignity_global <- experiment_results_with_global %>% 
  ggplot(aes(double_china, mean_out2)) +
  geom_col(aes(fill = double_china), color = "black", width = 0.90) + 
  facet_grid(. ~ country, labeller = country_labeller) +
  scale_fill_grey(start = 0.3) +
  scale_y_continuous(limits = c(0, 2.8), expand = c(0, 0)) +
  guides(fill = FALSE) +
  geom_line(data = p_value_one, 
            aes(x = x, y = y, group = 1)) +
  theme_minimal() +
  labs(
    x = "Country Prime",
    y = "Stay-at-Home Orders Are Always \n Disrespectful of Human Dignity",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 0,
                              face = "bold",
                              margin = ggplot2::margin(b = 0)),
    plot.margin = unit(rep(0, 4), "cm"),
    axis.text = element_text(size = 11, color = "#22292F"),
    axis.title = element_text(size = 13, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 5)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 0)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 0)),
    plot.caption = element_text(size = 15, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

plot_dignity_global <- plot_dignity_global +
  geom_text(
    size    = 3,
    data    = dat_text_out2,
    mapping = aes(x = 1.5, y = 2.5, label = label)
  ) +
  theme(
    strip.text = element_text(
      size = 12, color = "black", face = "bold"
    ))

plot_dignity_global

png(filename="plot_dignity_global.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_dignity_global
dev.off()

p_value_two <- tibble(
  x = c("China", "China", "Italy", "Italy"),
  y = c(2.55, 2.50, 2.50, 2.55)
)

plot_authoritarian_global <- experiment_results_with_global %>% 
  ggplot(aes(double_china, mean_out3)) +
  geom_col(aes(fill = double_china), color = "black", width = 0.90) + 
  facet_grid(. ~ country, labeller = country_labeller) +
  scale_fill_grey(start = 0.3) +
  scale_y_continuous(limits = c(0, 2.8), expand = c(0, 0)) +
  guides(fill = FALSE) +
  geom_line(data = p_value_two, 
            aes(x = x, y = y, group = 1)) +
  theme_minimal() +
  labs(
    x = "Country Prime",
    y = "Stay-at-Home Orders Are Always \n Authoritarian Policies",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 5,
                              face = "bold",
                              margin = ggplot2::margin(b = 0)),
    plot.margin = unit(rep(0, 4), "cm"),
    axis.text = element_text(size = 11, color = "#22292F"),
    axis.title = element_text(size = 13, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 5)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 0)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 0)),
    plot.caption = element_text(size = 15, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

plot_authoritarian_global <- plot_authoritarian_global +
  geom_text(
    size    = 3,
    data    = dat_text_out3,
    mapping = aes(x = 1.5, y = 2.65, label = label)
  ) +
  theme(
    strip.text = element_text(
      size = 12, color = "black", face = "bold"
    ))

plot_authoritarian_global

png(filename="plot_authoritarian_global.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_authoritarian_global
dev.off()

p_value_three <- tibble(
  x = c("China", "China", "Italy", "Italy"),
  y = c(2.71, 2.66, 2.66, 2.71)
)

plot_privacy_global <- experiment_results_with_global %>% 
  ggplot(aes(double_china, mean_out4)) +
  geom_col(aes(fill = double_china), color = "black", width = 0.90) + 
  facet_grid(. ~ country, labeller = country_labeller) +
  scale_fill_grey(start = 0.3) +
  scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
  guides(fill = FALSE) +
  geom_line(data = p_value_three, 
            aes(x = x, y = y, group = 1)) +
  theme_minimal() +
  labs(
    x = "Country Prime",
    y = "Phone Tracking Policies Always \n Violate Human Rights",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 5,
                              face = "bold",
                              margin = ggplot2::margin(b = 0)),
    plot.margin = unit(rep(0, 4), "cm"),
    axis.text = element_text(size = 11, color = "#22292F"),
    axis.title = element_text(size = 13, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 5)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 0)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 0)),
    plot.caption = element_text(size = 15, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

plot_privacy_global <- plot_privacy_global +
  geom_text(
    size    = 3,
    data    = dat_text_out4,
    mapping = aes(x = 1.5, y = 2.80, label = label)
  ) +
  theme(
    strip.text = element_text(
      size = 12, color = "black", face = "bold"
    ))

plot_privacy_global

png(filename="plot_privacy_global.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_privacy_global
dev.off()


png(filename="allplots_global.png", 
    units="in", 
    width=10.5, 
    height=16, 
    pointsize=15, 
    res=1000)
ggarrange(plot_abuse_global, plot_dignity_global, plot_authoritarian_global, plot_privacy_global, 
          labels = c(" A)", "B)", 
                     "C)", "D)"),
          ncol = 1, nrow = 4)
dev.off()


#################
#               #
#   Figure 5    #
#               #
#################

p_value_one <- tibble(
  x = c("China - authoritarian", "China - authoritarian", "North Korea - authoritarian", "North Korea - authoritarian"),
  y = c(3.3, 3.4, 3.4, 3.3)
)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome1, 
             survey3[survey3$treatments == "North Korea - authoritarian",]$outcome1, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

p_value_two <- tibble(
  x = c("China - authoritarian", "China - authoritarian", "Kazakhstan - authoritarian", "Kazakhstan - authoritarian"),
  y = c(3.6, 3.7, 3.7, 3.6)
)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome1, 
             survey3[survey3$treatments == "Kazakhstan - authoritarian",]$outcome1, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)


p_value_three <- tibble(
  x = c("China - authoritarian", "China - authoritarian", "Belarus - authoritarian", "Belarus - authoritarian"),
  y = c(3.9, 4, 4, 3.9)
)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome1, 
             survey3[survey3$treatments == "Belarus - authoritarian",]$outcome1, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)


p_value_four <- tibble(
  x = c("Turkey - authoritarian", "Turkey - authoritarian", "no country - democratic", "no country - democratic"),
  y = c(3.6, 3.7, 3.7, 3.6)
)

round(t.test(survey3[survey3$treatments == "Turkey - authoritarian",]$outcome1, 
             survey3[survey3$treatments == "no country - democratic",]$outcome1, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

p_value_five <- tibble(
  x = c("Venezuela - authoritarian", "Venezuela - authoritarian", "no country - democratic", "no country - democratic"),
  y = c(3.3, 3.4, 3.4, 3.3)
)

round(t.test(survey3[survey3$treatments == "Venezuela - authoritarian",]$outcome1, 
             survey3[survey3$treatments == "no country - democratic",]$outcome1, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

plot_abuse_longpolicy_vs_allothers <- abuse_longpolicy_means[c(3:4, 6:16),] %>% 
  ggplot(aes(treatments, mean_abuse_long)) +
  geom_errorbar(aes(ymin = mean_abuse_long-se_abuse_long, ymax = mean_abuse_long+se_abuse_long), width = 0.3) +
  geom_col(aes(fill = treatments), color = "black", width = 0.85, alpha = 0.75) +
  scale_x_discrete(limits = c("China - authoritarian",
                              "North Korea - authoritarian",
                              "Sudan - authoritarian",
                              "Saudi Arabia - authoritarian",
                              "Singapore - authoritarian",
                              "Bangladesh - authoritarian",
                              "Kazakhstan - authoritarian",
                              "Belarus - authoritarian",
                              "Turkey - authoritarian",
                              "Venezuela - authoritarian",
                              "Iran - authoritarian",
                              "Morocco - authoritarian",
                              "no country - democratic"
  ),
  labels = c(
    "China - authoritarian" = "CN",
    "Saudi Arabia - authoritarian" = "SA",
    "Iran - authoritarian" = "IR",
    "Belarus - authoritarian" = "BYS",
    "Turkey - authoritarian" = "TRK",
    "Morocco - authoritarian" = "MOR",
    "Sudan - authoritarian" = "SUD",
    "Venezuela - authoritarian" = "VEN",
    "Kazakhstan - authoritarian" = "KAZ",
    "Bangladesh - authoritarian" = "BAN",
    "North Korea - authoritarian" = "PRK",
    "Singapore - authoritarian" = "SIN",
    "no country - democratic" = "DEM"
  )) +
  scale_fill_manual(values = c('gray100', #DEM
                               'gray0', #China
                               'gray0', #Saudi
                               'gray100', #Iran
                               'gray50', #Belarus
                               'gray50', #Turkey
                               'gray100', #Morocco
                               'gray0',#Sudan 
                               'gray100',#Venezuela
                               'gray0',
                               'gray0',
                               'gray0',
                               'gray0')) +
  scale_y_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "",
    y = "Stay-at-home Orders Are \n An Abuse of Human Rights",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_abuse_longpolicy_vs_allothers

png(filename="plot_abuse_longpolicy_vs_allothers.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_abuse_longpolicy_vs_allothers
dev.off()

# Outcome 2: Dignity

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "North Korea - authoritarian",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "Turkey - authoritarian",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

round(t.test(survey3[survey3$treatments == "Morocco - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "no country - democratic",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

round(t.test(survey3[survey3$treatments == "Belarus - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "no country - democratic",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "Belarus - authoritarian",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

plot_dignity_longpolicy_vs_allothers <- dignity_longpolicy_means[c(3:4, 6:16),] %>% 
  ggplot(aes(treatments, mean_dignity_long)) +
  geom_errorbar(aes(ymin = mean_dignity_long-se_dignity_long, ymax = mean_dignity_long+se_dignity_long), width = 0.3) +
  geom_col(aes(fill = treatments), color = "black", width = 0.85, alpha = 0.75) +
  scale_x_discrete(limits = c("China - authoritarian",
                              "North Korea - authoritarian",
                              "Saudi Arabia - authoritarian",
                              "Kazakhstan - authoritarian",
                              "Sudan - authoritarian",
                              "Venezuela - authoritarian",
                              "Singapore - authoritarian",
                              "Bangladesh - authoritarian",
                              "Turkey - authoritarian",
                              "Iran - authoritarian",
                              "Belarus - authoritarian",
                              "Morocco - authoritarian",
                              "no country - democratic"
  ),
  labels = c(
    "China - authoritarian" = "CN",
    "Saudi Arabia - authoritarian" = "SA",
    "Iran - authoritarian" = "IR",
    "Belarus - authoritarian" = "BYS",
    "Turkey - authoritarian" = "TRK",
    "Morocco - authoritarian" = "MOR",
    "Sudan - authoritarian" = "SUD",
    "Venezuela - authoritarian" = "VEN",
    "Kazakhstan - authoritarian" = "KAZ",
    "Bangladesh - authoritarian" = "BAN",
    "North Korea - authoritarian" = "PRK",
    "Singapore - authoritarian" = "SIN",
    "no country - democratic" = "DEM"
  )) +
  scale_fill_manual(values = c('gray95', #DEM
                               'gray0', #China
                               'gray0', #Saudi
                               'gray0', #Iran
                               'gray50', #Belarus
                               'gray0', #Turkey
                               'gray95', #Morocco
                               'gray0',#Sudan 
                               'gray0',#Venezuela
                               'gray0',
                               'gray0',
                               'gray0',
                               'gray0')) +
  scale_y_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "",
    y = "Stay-at-home Orders Do Not \n Respect Human Dignity",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_dignity_longpolicy_vs_allothers

png(filename="plot_plot_dignity_longpolicy_vs_allothers.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_dignity_longpolicy_vs_allothers
dev.off()

# Outcome 3: Authoritarian

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome3, 
             survey3[survey3$treatments == "North Korea - authoritarian",]$outcome3, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome2, 
             survey3[survey3$treatments == "Singapore - authoritarian",]$outcome2, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

round(t.test(survey3[survey3$treatments == "China - authoritarian",]$outcome3, 
             survey3[survey3$treatments == "Turkey - authoritarian",]$outcome3, 
             alternative = "two.sided", var.equal = FALSE)$p.value,3)

round(t.test(survey3[survey3$treatments == "Belarus - authoritarian",]$outcome3, 
             survey3[survey3$treatments == "no country - democratic",]$outcome3, 
             alternative = "two.sided", var.equal = FALSE)$p.value,2)

plot_authoritarian_longpolicy_vs_allothers <- authoritarian_longpolicy_means[c(3:4, 6:16),] %>% 
  ggplot(aes(treatments, mean_authoritarian_long)) +
  geom_errorbar(aes(ymin = mean_authoritarian_long-se_authoritarian_long, ymax = mean_authoritarian_long+se_authoritarian_long), width = 0.3) +
  geom_col(aes(fill = treatments), color = "black", width = 0.85, alpha = 0.75) +
  scale_x_discrete(limits = c("China - authoritarian",
                              "North Korea - authoritarian",
                              "Kazakhstan - authoritarian",
                              "Saudi Arabia - authoritarian",
                              "Bangladesh - authoritarian",
                              "Singapore - authoritarian",
                              "Venezuela - authoritarian",
                              "Turkey - authoritarian",
                              "Sudan - authoritarian",
                              "Morocco - authoritarian",
                              "Iran - authoritarian",
                              "Belarus - authoritarian",
                              "no country - democratic"
  ),
  labels = c(
    "China - authoritarian" = "CN",
    "Saudi Arabia - authoritarian" = "SA",
    "Iran - authoritarian" = "IR",
    "Belarus - authoritarian" = "BYS",
    "Turkey - authoritarian" = "TRK",
    "Morocco - authoritarian" = "MOR",
    "Sudan - authoritarian" = "SUD",
    "Venezuela - authoritarian" = "VEN",
    "Kazakhstan - authoritarian" = "KAZ",
    "Bangladesh - authoritarian" = "BAN",
    "North Korea - authoritarian" = "PRK",
    "Singapore - authoritarian" = "SIN",
    "no country - democratic" = "DEM"
  )) +
  scale_fill_manual(values = c('gray95', #DEM
                               'gray0', #China
                               'gray0', #Saudi
                               'gray50', #Iran
                               'gray50', #Belarus
                               'gray50', #Turkey
                               'gray50', #Morocco
                               'gray50',#Sudan 
                               'gray0',#Venezuela
                               'gray0',
                               'gray0',
                               'gray0',
                               'gray0')) +  
  scale_y_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Country Treatments",
    y = "Stay-at-home Orders Are \n Authoritarian Policies",
    title = ""
  ) +
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = ggplot2::margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 1),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = ggplot2::margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) + theme_pubr()

plot_authoritarian_longpolicy_vs_allothers

png(filename="plot_authoritarian_longpolicy_vs_allothers.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_authoritarian_longpolicy_vs_allothers
dev.off()

png(filename="allplot_china_vs_allothers.png", 
    units="in", 
    width=22, 
    height=15, 
    pointsize=10, 
    res=1000)
ggarrange(plot_abuse_longpolicy_vs_allothers, 
          plot_dignity_longpolicy_vs_allothers, 
          plot_authoritarian_longpolicy_vs_allothers, 
          labels = c("", "", 
                     ""),
          common.legend = TRUE, legend = "bottom",
          ncol = 1, nrow = 3,
          widths=c(1,2))
dev.off()



#################
#               #
#  Appendix A   #
#               #
#################

# Load population statistics
population_stats <- read_csv("population_stats.csv")

# Survey 1
round(prop.table(table(survey1$gender)), 3)
round(prop.table(table(survey1$age_cat)), 3) #rounded to ensure a sum of 1

# Survey 2
round(prop.table(table(survey2$gender)), 3)
round(prop.table(table(survey2$age_cat)), 3) #rounded to ensure a sum of 1

# Survey 3

round(prop.table(table(survey3$gender)), 3)
round(prop.table(table(survey3$age_cat)), 3) #rounded to ensure a sum of 1

# Multicountry survey

gender_proportions <- survey_multicountry %>%
  dplyr::group_by(country, gender) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(proportion = count / sum(count)) %>%
  dplyr::mutate(proportion = round(proportion, 3)) %>%
  dplyr::select(country, gender, proportion)

age_proportions <- survey_multicountry %>%
  dplyr::group_by(country, age_cat) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(proportion = count / sum(count)) %>%
  dplyr::mutate(proportion = round(proportion, 3)) %>%
  dplyr::select(country, age_cat, proportion)

age_proportions[age_proportions$country == "Brazil",]
age_proportions[age_proportions$country == "France",]
age_proportions[age_proportions$country == "Germany",]
age_proportions[age_proportions$country == "Mexico",]
age_proportions[age_proportions$country == "United Kingdom",]
age_proportions[age_proportions$country == "United States",]

#Population statistics
population_stats

#################
#               #
#  Appendix B   #
#               #
#################

#################
#  Table B.1    #
#################

# Column 1

summary(lm(humanrightsabuse ~ italy_treatment + spain_treatment, data = survey1))

# Column 2

summary(lm(humanrightsabuse ~ italy_treatment + spain_treatment + gender + age_num  + as.factor(employment) + as.factor(covid_employment) + region, data = survey1))

# Column 3; outcome 1 = abuse; country = Italy

summary(lm(outcome1 ~ country, data = survey2))

# Column 4; outcome 1 = abuse; country = Italy

summary(lm(outcome1 ~ country + gender + age + as.factor(employment) + as.factor(covid_employment) + region, data = survey2))

# Column 5

summary(lm(abuse_outcome ~ as.factor(country), data = pooled_survey_abuse))

# Column 6

summary(lm(abuse_outcome ~ as.factor(country) + gender + age + as.factor(employment) + as.factor(covid_employment) + region, data = pooled_survey_abuse))


#################
#  Table B.2    #
#################

# Column 1

summary(lm(humandignity ~ italy_treatment + spain_treatment, data = survey1))

# Column 2

summary(lm(humandignity ~ italy_treatment + spain_treatment + gender + age_num  + as.factor(employment) + as.factor(covid_employment) + region, data = survey1))

# Column 3; outcome 2 = dignity; country = Italy

summary(lm(outcome2 ~ country, data = survey2))

# Column 4; outcome 2 = dignity; country = Italy

summary(lm(outcome2 ~ country + gender + age + as.factor(employment) + as.factor(covid_employment) + region, data = survey2))

# Column 5

summary(lm(dignity_outcome ~ as.factor(country), data = pooled_survey_humandignity))

# Column 6

summary(lm(dignity_outcome ~ as.factor(country) + gender + age + as.factor(employment) + as.factor(covid_employment) + region, data = pooled_survey_humandignity))


#################
#  Table B.3    #
#################

# Column 1

summary(lm(authoritarianpolicy ~ italy_treatment + spain_treatment, data = survey1))

# Column 2

summary(lm(authoritarianpolicy ~ italy_treatment + spain_treatment + gender + age_num  + as.factor(employment) + as.factor(covid_employment) + region, data = survey1))

# Column 3; outcome 2 = dignity; country = Italy

summary(lm(outcome3 ~ country, data = survey2))

# Column 4; outcome 2 = dignity; country = Italy

summary(lm(outcome3 ~ country + gender + age + as.factor(employment) + as.factor(covid_employment) + region, data = survey2))

# Column 5

summary(lm(authoritarian_outcome ~ as.factor(country), data = pooled_survey_authoritarian))

# Column 6

summary(lm(authoritarian_outcome ~ as.factor(country) + gender + age + as.factor(employment) + as.factor(covid_employment) + region, data = pooled_survey_authoritarian))


#################
#  Table B.4    #
#################

# Column 1

summary(lm(outcome4 ~ country, data = survey2))

# Column 2

summary(lm(outcome4 ~ country + gender + age  + as.factor(employment) + as.factor(covid_employment) + region, data = survey2))


#################
#               #
#  Appendix C   #
#               #
#################

# Column 1

summary(regime.mechanism_abuse <- lm(outcome1 ~ china_regime + china_region + china_immi + china_power + china_unlabel +
                                       italy_regime + italy_region + italy_immi + italy_power, 
                                     data = survey2))

# Column 2

summary(regime.mechanism_abuse_controls <- lm(outcome1 ~ china_regime + china_region + china_immi + china_power + china_unlabel +
                                                italy_regime + italy_region + italy_immi + italy_power + 
                                                gender + age + as.factor(region) + as.factor(covid_employment) + as.factor(employment), 
                                              data = survey2))

# Column 3

summary(regime.mechanism_dignity <- lm(outcome2 ~ china_regime + china_region + china_immi + china_power + china_unlabel +
                                         italy_regime + italy_region + italy_immi + italy_power, 
                                       data = survey2))

# Column 4

summary(regime.mechanism_dignity_controls <- lm(outcome2 ~ china_regime + china_region + china_immi + china_power + china_unlabel +
                                                  italy_regime + italy_region + italy_immi + italy_power + 
                                                  gender + age + as.factor(region) + as.factor(covid_employment) + as.factor(employment), 
                                                data = survey2))

# Column 5

summary(regime.mechanism_autho <- lm(outcome3 ~ china_regime + china_region + china_immi + china_power + china_unlabel +
                                       italy_regime + italy_region + italy_immi + italy_power, 
                                     data = survey2))

# Column 6

summary(regime.mechanism_autho_controls <- lm(outcome3 ~ china_regime + china_region + china_immi + china_power + china_unlabel +
                                                italy_regime + italy_region + italy_immi + italy_power + 
                                                gender + age + as.factor(region) + as.factor(covid_employment) + as.factor(employment), 
                                              data = survey2))

# Column 7

summary(regime.mechanism_privacy <- lm(outcome4 ~ china_regime + china_region + china_immi + china_power + china_unlabel +
                                         italy_regime + italy_region + italy_immi + italy_power, 
                                       data = survey2))

# Column 8

summary(regime.mechanism_privacy_controls <- lm(outcome4 ~ china_regime + china_region + china_immi + china_power + china_unlabel +
                                                  italy_regime + italy_region + italy_immi + italy_power + 
                                                  gender + age + as.factor(region) + as.factor(covid_employment) + as.factor(employment), 
                                                data = survey2))


#################
#               #
#  Appendix D   #
#               #
#################

#################
#  Figure D.1   #
#################

fit_models_no_controls <- function(data, outcome, media_trust) {
  model <- lm(as.formula(paste(outcome, "~ china_treatment")), data = data[data$media_trust == media_trust, ])
  return(tidy(model, conf.int = TRUE))
}

outcomes <- c("humanrightsabuse", "humandignity", "authoritarianpolicy")
media_trust_levels <- 0:2
models <- list()

for (outcome in outcomes) {
  for (level in media_trust_levels) {
    models[[paste(outcome, "trust", level, sep = "_")]] <- fit_models_no_controls(survey1, outcome, level)
  }
}

results <- bind_rows(models, .id = "model") %>%
  mutate(Media_Trust = as.numeric(gsub(".*trust_(\\d+)", "\\1", model)),
         Outcome = gsub("_trust_.*", "", model)) %>%
  mutate(Outcome = factor(Outcome, levels = c("humanrightsabuse", "humandignity", "authoritarianpolicy")))

plot_media_trust <- ggplot(results %>% filter(term == "china_treatment"), aes(x = factor(Media_Trust, labels = c("0", "1", ">2")), y = estimate, color = Outcome)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(x = "Level of Media Trust", y = "Estimated Effect of China Over Italy or Spain", title = "",
       subtitle = "") +
  theme_minimal() +
  scale_color_manual(values = c("grey10", "grey50", "grey80"),
                     labels = c("Abuse", "Dignity", "Authoritarian"))  # Updated labels

png(filename="plot_media_trust.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_media_trust
dev.off()

#################
#  Figure D.2   #
#################

fit_models_no_controls <- function(data, outcome, pol_interest) {
  model <- lm(as.formula(paste(outcome, "~ china_treatment")), data = data[data$polinterest == pol_interest, ])
  return(tidy(model, conf.int = TRUE))
}

outcomes <- c("humanrightsabuse", "humandignity", "authoritarianpolicy")
pol_interest_levels <- 0:3
models <- list()

for (outcome in outcomes) {
  for (level in pol_interest_levels) {
    models[[paste(outcome, "interest", level, sep = "_")]] <- fit_models_no_controls(survey1, outcome, level)
  }
}

results <- bind_rows(models, .id = "model") %>%
  mutate(Political_Interest = as.numeric(gsub(".*interest_(\\d+)", "\\1", model)),
         Outcome = gsub("_interest_.*", "", model)) %>%
  mutate(Outcome = factor(Outcome, levels = c("humanrightsabuse", "humandignity", "authoritarianpolicy")))

plot_interest_politics <- ggplot(results %>% filter(term == "china_treatment"), aes(x = factor(Political_Interest), y = estimate, color = Outcome)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(x = "Level of Political Interest", y = "Estimated Effect of China Over Italy or Spain", title = "",
       subtitle = "") +
  theme_minimal() +
  scale_color_manual(values = c("grey10", "grey50", "grey80"),
                     labels = c("Abuse", "Dignity", "Authoritarian"))  # Updated labels

png(filename="plot_interest_politics.png", 
    units="in", 
    width=9, 
    height=6, 
    pointsize=15, 
    res=1000)
plot_interest_politics
dev.off()


#################
#               #
#  Appendix E   #
#               #
#################

table_data <- data.frame(
  Treatment = c(
    "No country - No label", "No country - Authoritarian", "No country - Democratic",
    "China - Authoritarian", "China - No label", "Saudi Arabia - Authoritarian",
    "Iran - Authoritarian", "Belarus - Authoritarian", "Turkey - Authoritarian",
    "Morocco - Authoritarian", "Sudan - Authoritarian", "Venezuela - Authoritarian",
    "Kazakhstan - Authoritarian", "Bangladesh - Authoritarian",
    "North Korea - Authoritarian", "Singapore - Authoritarian"
  ),
  Count = c(641, 636, 638, 623, 639, 181, 175, 176, 177, 173, 176, 174, 174, 174, 173, 172)
)

#################
#               #
#  Appendix F   #
#               #
#################

treatment_list_nocountries <- c("no country - authoritarian", "no country - democratic",
                                "China - authoritarian", "China - no label", "no country - no label")

# Column 1

summary(lm(outcome1 ~ treatments, survey3[which(survey3$treatments %in% treatment_list_nocountries),]))

# Column 2

summary(lm(outcome1 ~ treatments + gender + age + region, survey3[which(survey3$treatments %in% treatment_list_nocountries),])) 

# Column 3

summary(lm(outcome2 ~ treatments, survey3[which(survey3$treatments %in% treatment_list_nocountries),]))

# Column 4

summary(lm(outcome2 ~ treatments + gender + age + region, survey3[which(survey3$treatments %in% treatment_list_nocountries),]))

# Column 5

summary(lm(outcome3 ~ treatments, survey3[which(survey3$treatments %in% treatment_list_nocountries),]))

# Column 6

summary(lm(outcome3 ~ treatments + gender + age + region, survey3[which(survey3$treatments %in% treatment_list_nocountries),]))


#################
#               #
#  Appendix G   #
#               #
#################

calculate_percentage <- function(column, country) {
  corrected_column <- ifelse(column == "Marruecos", "Morocco", column)
  sum(corrected_column == country, na.rm = TRUE) / length(column) * 100
}

questions <- paste0("info_questions_", 1:14)
countries <- c("China", "Italia", "Estados Unidos", "Arabia SaudÃ­", "India", "Singapur", "Morocco")

results <- data.frame(
  Question = character(),
  China = numeric(),
  Italy = numeric(),
  US = numeric(),
  Saudi_Arabia = numeric(),
  India = numeric(),
  Singapore = numeric(),
  Morocco = numeric()
)

for (i in 1:length(questions)) {
  question_data <- sapply(1:7, function(j) {
    country_name <- countries[j]
    variable_name <- if (j == 7) {  # Adjust for Morocco
      paste0(questions[i], "_16")
    } else {
      paste0(questions[i], "_", j)
    }
    calculate_percentage(survey3[[variable_name]], country_name)
  })
  
  # Add row to results data frame
  results <- rbind(results, data.frame(
    Question = questions[i],
    China = question_data[1],
    Italy = question_data[2],
    US = question_data[3],
    Saudi_Arabia = question_data[4],
    India = question_data[5],
    Singapore = question_data[6],
    Morocco = question_data[7]
  ))
}


results <- results %>%
  mutate(across(-Question, ~ round(as.numeric(.), 2)))

results <- results %>%
  mutate(across(-Question, ~ sprintf("%.2f%%", .)))


results$Question <- c(
  "Authoritarian political regime",
  "Democratic political regime",
  "Large country",
  "Small country",
  "European country",
  "American country",
  "African country",
  "East Asian country",
  "Middle Eastern country",
  "A large immigrant community in Spain",
  "A small immigrant community in Spain",
  "A consolidated economic and political power",
  "An emerging economic and political power",
  "Critical with Spain's human rights violations"
)

results %>%
  kable("html", col.names = c("", "China", "Italy", "U.S.", "Saudi Arabia", "India", "Singapore", "Morocco")) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" " = 1, "Respondents' knowledge about several countries' attributes (N = 5,041)" = 7)) %>%
  footnote(general = "N = 5,041 respondents.")


#################
#               #
#  Appendix H   #
#               #
#################

survey3$info_questions_china_dictatorial <- ifelse(survey3$info_questions_1_1 == "China", 1, 0)

# Column 1

summary(lm(info_questions_china_dictatorial ~ treatments_with_any, survey3[which(survey3$treatments_with_any == "no country - no label" | survey3$treatments_with_any == "China - no label"), ]))

# Column 2

summary(lm(info_questions_china_dictatorial ~ treatments_with_any + gender + age + region, survey3[which(survey3$treatments_with_any == "no country - no label" | survey3$treatments_with_any == "China - no label"), ]))

# Column 3

summary(lm(info_questions_china_dictatorial ~ treatments_with_any, survey3[which(survey3$treatments_with_any == "no country - no label" | survey3$treatments_with_any == "China - authoritarian"), ]))

# Column 4

summary(lm(info_questions_china_dictatorial ~ treatments_with_any + gender + age + region, survey3[which(survey3$treatments_with_any == "no country - no label" | survey3$treatments_with_any == "China - authoritarian"), ]))


