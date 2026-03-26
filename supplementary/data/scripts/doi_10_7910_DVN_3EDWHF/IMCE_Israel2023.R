#### IMCE for conjoint Israel 2023 by Shingo Hamanaka 20260302 ####

#### Loading packages ####
library(cjoint)
library(tidyverse)

#### Loading data ####
conjoint_data <- read.qualtrics("IsraelSurvey2023Populismdata_new.csv",responses= NULL, covariates= c("religiosity", "Likud","Bibi","ideology","pop_score"),
                                respondentID="userID",
                                ranks=c("Q21_1", "Q22_1", "Q23_1", "Q24_1", "Q25_1" , "Q26_1", "Q27_1", "Q21_2", "Q22_2", "Q23_2", "Q24_2", "Q25_2" , "Q26_2", "Q27_2"))

#### Translation ####
Israelpolicy <- conjoint_data %>% 
  select(!c(contains("rowpos")))

Israelpolicy <- Israelpolicy %>%
  mutate(
    Antijew = case_when(
      antijew == "תומכת בהענשתם"~"punish",
      antijew == "תמשיך במדיניות הקיימת"~"leave"
    )
  )

Israelpolicy <- Israelpolicy %>%
  mutate(
    Diplomacy = case_when(
      arabs == "תרחיב"~"expand",
      arabs == "לא תרחיב"~"not expand"
    )
  )

Israelpolicy <- Israelpolicy %>%
  mutate(
    Harediwork = case_when(
      haredi == "תומכת"~"support",
      haredi == "לא תומכת"~"not support"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Trade = case_when(
      imex == "תומכת"~"support",
      imex == "מתנגדת"~"oppose"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Inequality = case_when(
      inequality == "תשלם"~"subsiding",
      inequality == "לא תשלם"~"nonsubsiding"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Judicialreform = case_when(
      judicialreform == "תומכת"~"support",
      judicialreform == "מתנגדת"~"oppose"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Jewishmajority = case_when(
      jewmajority == "תחקוק"~"agree",
      jewmajority == "לא תחקוק"~"disagree"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    LGBT = case_when(
      lgbt == "תגביר"~"increase",
      lgbt == "תצמצם"~"decrease",
      lgbt == "תשאיר כמו שהוא"~"leave"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Settlement = case_when(
      settlement == "תגביר"~"increase",
      settlement == "תצמצם"~"decrease",
      settlement == "תשאיר כמו שהיא"~"leave"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    PA = case_when(
      pa == "תפתח מחדש"~"restart",
      pa == "לא תפתח"~"not restart"
    )
  )

#### Convert the variables to factors ####
Israelpolicy$Antijew <- as.factor(Israelpolicy$Antijew)
Israelpolicy$Diplomacy <- as.factor(Israelpolicy$Diplomacy)
Israelpolicy$Harediwork <- as.factor(Israelpolicy$Harediwork)
Israelpolicy$Trade <- as.factor(Israelpolicy$Trade)
Israelpolicy$Inequality <- as.factor(Israelpolicy$Inequality)
Israelpolicy$Jewishmajority <- as.factor(Israelpolicy$Jewishmajority)
Israelpolicy$Judicialreform <- as.factor(Israelpolicy$Judicialreform)
Israelpolicy$LGBT <- as.factor(Israelpolicy$LGBT)
Israelpolicy$PA <- as.factor(Israelpolicy$PA)
Israelpolicy$Settlement <- as.factor(Israelpolicy$Settlement)

#### Putting labels ####
attr_list <- list()             
attr_list[["Antijew"]] <- c("punish","leave")

attr_list[["Diplomacy"]] <- c("expand", "not expand")

attr_list[["Harediwork"]] <- c("support","not support")

attr_list[["Trade"]] <- c("support","oppose") 

attr_list[["Inequality"]] <- c("subsiding", "nonsubsiding")

attr_list[["Judicialreform"]] <- c("support","oppose")

attr_list[["Jewishmajority"]] <- c("agree","disagree")

attr_list[["LGBT"]] <- c("increase","leave","decrease")

attr_list[["PA"]] <- c("restart", "not restart") 

attr_list[["Settlement"]] <- c("increase", "leave", "decrease")

#### cjmodel/BART ####
library(cjbart)
library(dplyr)
library(ggplot2)

## 1) # Handle missing values (Note: `na.omit` is aggressive; it is safer to drop rows only for the required variables)
attribs <- c("Antijew","Diplomacy","Harediwork","Trade","Inequality",
             "Judicialreform","Jewishmajority","LGBT","PA","Settlement")

Israelpolicy_bart <- Israelpolicy %>%
  # Restrict to the columns required for BART/IMCE, then remove missing values (recommended)
  drop_na(selected, respondentIndex, task, all_of(attribs))

## 2) Convert all variables to factors (safe even if already done)
Israelpolicy_bart <- Israelpolicy_bart %>%
  mutate(across(all_of(attribs), as.factor))

## 3) cjmodel/BART
set.seed(100)

cj_model <- cjbart(
  data = Israelpolicy_bart,
  Y = "selected",
  id = "respondentIndex",
  round = "task",
  use_round = TRUE,
  cores = 1
)

## 4) IMCE (set the reference level according to the "baseline" in attr_list)
##    In your attr_list, the three-level attributes are assumed to have "leave" as the reference level.
het_effects <- IMCE(
  data = Israelpolicy_bart,
  model = cj_model,
  attribs = attribs,
  ref_levels = c(
    "leave",        # Antijew: punish vs leave
    "not expand",   # Diplomacy: expand vs not expand
    "not support",  # Harediwork: support vs not support
    "oppose",       # Trade: support vs oppose
    "nonsubsiding", # Inequality: subsiding vs nonsubsiding
    "oppose",       # Judicialreform: support vs oppose
    "disagree",     # Jewishmajority: agree vs disagree
    "leave",        # LGBT: increase / leave / decrease
    "not restart",  # PA: restart vs not restart
    "leave"         # Settlement: increase / leave / decrease
  ),
  cores = 1
)

summary(het_effects)

## 5-1) Heterogeneity plot by the covariate religiosity
plot(het_effects, covar = "religiosity", se = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "grey90", high = "grey10")

## 5-2) Heterogeneity plot by the covariate Ideology 
plot(het_effects, covar = "ideology", se = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "grey90", high = "grey10")

## 5-3) Heterogeneity plot by Likud 
plot(het_effects, covar = "Likud", se = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "grey90", high = "grey10")

## 5-4) Heterogeneity plot by Bibi
plot(het_effects, covar = "Bibi", se = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "grey90", high = "grey10")

## 5-5) Heterogeneity plot by populism 
plot(het_effects, covar = "pop_score", se = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "grey90", high = "grey10")


## 6) VIMP（identify important splitting variables）
vimp_estimates <- het_vimp(imces = het_effects, cores = 1)

plot(vimp_estimates)
plot(vimp_estimates, covars = c("religiosity", "Likud", "Bibi", "ideology", "pop_score"))

summary(vimp_estimates)

p <- plot(
  vimp_estimates,
  covars = c("religiosity", "Likud", "Bibi", "ideology", "pop_score")
) +
  theme(
    strip.text.y.right = element_text(
      angle = 0,   # Landscape
      hjust = 0.5  # Centered (default; can be omitted)
    ),
    plot.margin = margin(5.5, 80, 5.5, 5.5)
  )

print(p)
