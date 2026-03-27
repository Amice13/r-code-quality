# load necessary data
# required packages

library(dplyr)
library(tidyverse)
library(tidyr)
library(jtools)
library(stringr)
library(dplyr)
library(gplots)
library(FSA)
library(dunn.test)
library(dplyr)
library(rstatix)
library(dunn.test)

# ------------------------------------
# descriptive statistics
# ------------------------------------

data <- transform (data,
                   risk_1 = as.numeric(as.character(data$risk_1)),
                   risk_2 = as.numeric(as.character(data$risk_2)),
                   risk_3 = as.numeric(as.character(data$risk_3)),
                   risk_4 = as.numeric(as.character(data$risk_4)),
                   risk_5 = as.numeric(as.character(data$risk_5)),
                   risk_6 = as.numeric(as.character(data$risk_6)),
                   
                   competence_1 = as.numeric(as.character(data$competence_1)),
                   competence_2 = as.numeric(as.character(data$competence_2)),
                   competence_3 = as.numeric(as.character(data$competence_3)),
                   
                   experience = as.numeric(as.character(data$experience)),
                   
                   responsibility_1 = as.numeric(as.character(data$responsibility_1)),
                   responsibility_2 = as.numeric(as.character(data$responsibility_2)),
                   responsibility_3 = as.numeric(as.character(data$responsibility_3)),
                   responsibility_4 = as.numeric(as.character(data$responsibility_4)),
                   responsibility_5 = as.numeric(as.character(data$responsibility_5)),
                   responsibility_6 = as.numeric(as.character(data$responsibility_6))
)

data <- data %>% 
  mutate (data,
          age_groups =
            # Create categories
            case_when(
              age <= 25 ~ "16-25",
              age >= 26 & age <= 35 ~ "26-35",
              age >= 36 & age <= 45 ~ "36-45",
              age >= 46 & age <= 55 ~ "46-55",
              age >= 56 & age <= 65 ~ "56-65",
              age >= 66 & age <= 75 ~ "66-75",
              age >= 76 & age <= 90 ~ "76-90"
            ))

table(data$age_groups)
mean(data$age)
sd(data$age)

data <- data %>% 
  mutate (data,
          education_groups =
            # Create categories
            case_when(
              education <= 5 ~ "Bachelor's",
              education <= 6 ~ "Master's",
              education <= 7 ~ "PhD"
            ))

table(data$education_groups)


data <- 
  mutate (data,
          gender_groups =
            case_when (data$gender == 1 ~ "woman",
                       data$gender == 2 ~ "man",
                       data$gender == 3 ~ "non-binary",
                       data$gender == 4 ~ "self-describe")
  )

table(data$gender_groups)

data <-
  mutate (data,
          expert_groups =
            case_when (data$job_1 == 1 ~ "fact-checker",
                       data$job_1 == 2 ~ "journalist",
                       data$job_1 == 3 ~ "academic"))
table(data$expert_groups)

summary(data$experience)
sd(data$experience)

summary(data$job_misinfo_freq_1)
sd(data$job_misinfo_freq_1)

data %>%
  filter(expert_groups %in% c("academic", "fact-checker", "journalist")) %>%
  group_by(expert_groups) %>%
  summarize(mean_misinfo = mean(job_misinfo_freq_1, na.rm = TRUE))

data %>%
  filter(expert_groups %in% c("academic", "fact-checker", "journalist")) %>%
  group_by(expert_groups) %>%
  summarize(mean_misinfo = sd(job_misinfo_freq_1, na.rm = TRUE))

data %>%
  filter(expert_groups %in% c("academic", "fact-checker", "journalist")) %>%
  group_by(expert_groups) %>%
  summarize(mean_experience = mean(experience, na.rm = TRUE))


# ------------------------------------
# main data analysis
# ------------------------------------

## DV: self-perceived competence
data <- data %>%
  mutate(competence = rowMeans(select(., competence_1, competence_2, competence_3), na.rm = TRUE))

summary(data$competence)
sd(data$competence)

CA_competence <- data %>%
  summarize (
    competence_1, competence_2, competence_3, na.rm = TRUE)

cronbach.alpha(CA_competence, na.rm = TRUE)

# self-perceived competence conservative test: Kruskal-Wallis
kruskal.test(competence ~ expert_groups, data = data)
dunn.test(data$competence, data$expert_groups, method = "bonferroni")
summary(data$competence)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(competence, na.rm = TRUE),
    sd_value = sd(competence, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

# self-perceived competence conservative test: Welch's ANOVA and Games–Howell post-hoc test (for Welch’s ANOVA)
welch_result <- oneway.test(competence ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(competence ~ expert_groups)
print(games_howell_result)



# ------------------------------------
## DV: risk perceptions of disinformation
# moral panic
kruskal.test(risk_1 ~ expert_groups, data = data)
dunn.test(data$risk_1, data$expert_groups, method = "bonferroni")
summary(data$risk_1)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(risk_1, na.rm = TRUE),
    sd_value = sd(risk_1, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(risk_1 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(risk_1 ~ expert_groups)
print(games_howell_result)


# long-standing issue
kruskal.test(risk_2 ~ expert_groups, data = data)
dunn.test(data$risk_2, data$expert_groups, method = "bonferroni")
summary(data$risk_2)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(risk_2, na.rm = TRUE),
    sd_value = sd(risk_2, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(risk_2 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(risk_2 ~ expert_groups)
print(games_howell_result)


#seeing is no longer believing
kruskal.test(risk_3 ~ expert_groups, data = data)
dunn.test(data$risk_3, data$expert_groups, method = "bonferroni")
summary(data$risk_3)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(risk_3, na.rm = TRUE),
    sd_value = sd(risk_3, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(risk_3 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(risk_3 ~ expert_groups)
print(games_howell_result)


# undermine democratic debate
kruskal.test(risk_4 ~ expert_groups, data = data)
dunn.test(data$risk_4, data$expert_groups, method = "bonferroni")
summary(data$risk_4)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(risk_4, na.rm = TRUE),
    sd_value = sd(risk_4, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)


welch_result <- oneway.test(risk_4 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(risk_4 ~ expert_groups)
print(games_howell_result)


# confusion real/fake
kruskal.test(risk_5 ~ expert_groups, data = data)
dunn.test(data$risk_5, data$expert_groups, method = "bonferroni")
summary(data$risk_5)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(risk_5, na.rm = TRUE),
    sd_value = sd(risk_5, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)


welch_result <- oneway.test(risk_5 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(risk_5 ~ expert_groups)
print(games_howell_result)


# disrupt pol. decision making
kruskal.test(risk_6 ~ expert_groups, data = data)
dunn.test(data$risk_6, data$expert_groups, method = "bonferroni")
summary(data$risk_6)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(risk_6, na.rm = TRUE),
    sd_value = sd(risk_6, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(risk_6 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(risk_6 ~ expert_groups)
print(games_howell_result)



# ------------------------------------
## DV: attribution of responsibility to mitigate disinformation
# VLOPs
kruskal.test(responsibility_1 ~ expert_groups, data = data)
dunn.test(data$responsibility_1, data$expert_groups, method = "bonferroni")
summary(data$responsibility_1)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(responsibility_1, na.rm = TRUE),
    sd_value = sd(responsibility_1, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(responsibility_1 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(responsibility_1 ~ expert_groups)
print(games_howell_result)


# journalists
kruskal.test(responsibility_2 ~ expert_groups, data = data)
dunn.test(data$responsibility_2, data$expert_groups, method = "bonferroni")
summary(data$responsibility_2)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(responsibility_2, na.rm = TRUE),
    sd_value = sd(responsibility_2, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(responsibility_2 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(responsibility_2 ~ expert_groups)
print(games_howell_result)


# fact-checkers
kruskal.test(responsibility_3 ~ expert_groups, data = data)
dunn.test(data$responsibility_3, data$expert_groups, method = "bonferroni")
summary(data$responsibility_3)

plotmeans(formula = data$responsibility_3~data$expert_groups,
          xlab = "group", 
          ylab = "fact-checkers",         
          n.label = FALSE          
)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(responsibility_3, na.rm = TRUE),
    sd_value = sd(responsibility_3, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(responsibility_3 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(responsibility_3 ~ expert_groups)
print(games_howell_result)


# government/political actors
kruskal.test(responsibility_4 ~ expert_groups, data = data)
dunn.test(data$responsibility_4, data$expert_groups, method = "bonferroni")
summary(data$responsibility_4)

plotmeans(formula = data$responsibility_4~data$expert_groups,
          xlab = "group", 
          ylab = "government/political actors",         
          n.label = FALSE          
)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(responsibility_4, na.rm = TRUE),
    sd_value = sd(responsibility_4, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(responsibility_4 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(responsibility_4 ~ expert_groups)
print(games_howell_result)


# experts/academics
kruskal.test(responsibility_5 ~ expert_groups, data = data)
dunn.test(data$responsibility_5, data$expert_groups, method = "bonferroni")
summary(data$responsibility_5)

plotmeans(formula = data$responsibility_5~data$expert_groups,
          xlab = "group", 
          ylab = "experts/academics",         
          n.label = FALSE          
)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(responsibility_5, na.rm = TRUE),
    sd_value = sd(responsibility_5, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(responsibility_5 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(responsibility_5 ~ expert_groups)
print(games_howell_result)


# news users 
kruskal.test(responsibility_6 ~ expert_groups, data = data)
dunn.test(data$responsibility_6, data$expert_groups, method = "bonferroni")
summary(data$responsibility_6)

plotmeans(formula = data$responsibility_6~data$expert_groups,
          xlab = "group", 
          ylab = "news users",         
          n.label = FALSE          
)

summary_data <- data %>%
  group_by(expert_groups) %>%
  summarise(
    mean_value = mean(responsibility_6, na.rm = TRUE),
    sd_value = sd(responsibility_6, na.rm = TRUE),
    n = n(),
    se = sd_value / sqrt(n)
  )
print(summary_data)

welch_result <- oneway.test(responsibility_6 ~ expert_groups, data = data, var.equal = FALSE)
print(welch_result)

games_howell_result <- data %>%
  games_howell_test(responsibility_6 ~ expert_groups)
print(games_howell_result)





