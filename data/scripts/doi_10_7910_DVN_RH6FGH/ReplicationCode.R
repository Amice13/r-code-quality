#### "Gender and the Expression of International Political Knowledge"
#### Replication materials prepared by Irene Entringer & Emily Jackson
#### 13 January 2025


# Packages ----------------------------------------------------------------

library(tidyverse)
library(stargazer)
library(kableExtra)
library(marginaleffects)
library(cluster)
library(miceadds)
library(ggeffects)
library(estimatr)
library(sandwich)
library(lmtest)
library(clubSandwich)
library(prediction)
library(gridExtra)
library(patchwork)
library(modelsummary)
library(broom.helpers)

# Reading in Data ---------------------------------------------------------

# Add your working directory here

# Load files
Public_idk=read.csv("ReplicationFile/Replication_IDontKnow_Public.csv",na.strings=c("", "NA"), header = TRUE)
Scholar_idk=read.csv("ReplicationFile/Replication_IDontKnow_Scholar.csv",na.strings=c("", "NA"), header = TRUE)
Public_extreme=read.csv("ReplicationFile/Replication_Extreme_Public.csv",na.strings=c("", "NA"), header = TRUE)
Scholar_extreme=read.csv("ReplicationFile/Replication_Extreme_Scholar.csv",na.strings=c("", "NA"), header = TRUE)
Pub_extreme_numerical=read.csv("ReplicationFile/Replication_Extreme_Numerical_Public.csv",na.strings=c("", "NA"), header = TRUE)
Scholar_extreme_numerical=read.csv("ReplicationFile/Replication_Extreme_Numerical_Scholar.csv",na.strings=c("", "NA"), header = TRUE)
Scholar_confidence_string=read.csv("ReplicationFile/Replication_Confidence_Scholar.csv",na.strings=c("", "NA"), header = TRUE)
Pub_confidence=read.csv("ReplicationFile/Replication_Confidence_Numerical_Public.csv",na.strings=c("", "NA"), header = TRUE)
Scholar_confidence=read.csv("ReplicationFile/Replication_Confidence_Numerical_Scholar.csv",na.strings=c("", "NA"), header = TRUE)

# you'll have to order some categorical variables manually in order to run the models with the same reference categories we used.
race_opts <- c("White", "Non-white","White and non-white")
category_opts <- c("Analytical", "Normative", "Predictive")
pretty_latex_tables = TRUE


plot_theme_withlegend = theme(text = element_text(size=12, family="Times New Roman"),  
                              axis.text = element_text(color="black", size=12),
                              axis.line = element_line(colour = "black"),
                              strip.text.y = element_text(angle = 0), 
                              axis.title.y = element_text(angle=0, hjust=.5, vjust=.5, size=12),
                              axis.title = element_text(lineheight=1.1, size=12), 
                              axis.title.x = element_text(size = 12),
                              legend.position = 'bottom',
                              legend.title = element_blank(),
                              legend.text = element_text(size = 12),
                              legend.key.size = unit(1.5, "lines"),
                              panel.grid.minor = element_blank(),
                              plot.caption = element_text(hjust = 0, face = "italic", size=12, family="Times New Roman"), 
                              plot.title = element_text(hjust = 0.5)
)



# 1. Don't Know - Public Only  -----------------------------------------------------

# order variables to set references for regression covariates
Public_idk <- Public_idk %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow", "Broad")),
         category = factor(category, levels=category_opts),
         Race = factor(Race, levels=race_opts))

# Public only regression models: 
idk_public_models <- list(
  "IDK Public OLS" = lm.cluster( idk ~ gender, cluster="id" , data=Public_idk),
  "IDK Public Pro" = glm.cluster( idk ~ gender , cluster="id", family = binomial(link = "probit")  , data=Public_idk),
  "IDK Public Pro Controls" = glm.cluster( idk ~ gender + category + age + Education + Race, cluster="id", family = binomial(link = "probit"), data=Public_idk),
  "IDK Public Pro Cont Int" = glm.cluster( idk ~ gender*Education + gender*age + category + Race, cluster="id" , family = binomial(link = "probit"), data=Public_idk)
)

#To create the table, need to extract the models:
# Extract the actual model objects based on their class
nested_idk_public_models <- lapply(idk_public_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_idk_public_models) <- names(idk_public_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list and create table
clustered_idk_public_models <- lapply(nested_idk_public_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Public_idk$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Public_idk$id))
  }
})

# TABLE A1: Regression table, Probability of selecting "IDK," Public sample
modelsummary(clustered_idk_public_models, stars = TRUE) 

# 2. Don't Know - Scholars Only ----------------------------------------------

# order variables to set references for regression covariates
Scholar_idk <- Scholar_idk %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow", "Broad")),
         category = factor(category, levels=category_opts),
         Race = factor(Race, levels=race_opts))

# Scholars only regression models: 
idk_scholar_models <- list(
  "IDK Scholar OLS" = lm.cluster(idk ~ gender,  data = Scholar_idk, cluster="id") ,
  "IDK Scholar Probit" = glm.cluster(idk ~ gender, cluster="id", family = binomial(link = "probit"), data = Scholar_idk),
  "IDK Scholar Probit Con out Expt" = glm.cluster(idk ~ gender + category + scope + age + Education + Race + Rank, cluster="id",family = binomial(link = "probit"), data =Scholar_idk),
  "IDK Scholar Probit Controls" = glm.cluster(idk ~ gender + category + scope + age + Education + Race + Rank + IssueAreaMatch + IssueRegionMatch, cluster="id", family = binomial(link = "probit"), data = Scholar_idk),
  "IDK Scholar Probit Cont Int" = glm.cluster(idk ~ gender * IssueAreaMatch + gender * IssueRegionMatch + gender * age + category + scope + Education + Race + Rank,  cluster="id", family = binomial(link = "probit"), data = Scholar_idk)
)

#To create the table, need to extract the models:
# Extract the actual model objects based on their class
nested_idk_scholar_models <- lapply(idk_scholar_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_idk_scholar_models) <- names(idk_scholar_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_idk_scholar_models <- lapply(nested_idk_scholar_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Scholar_idk$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Scholar_idk$id))
  }
})

# TABLE A2: Regression table, Probability of selecting "IDK" in scholar sample
modelsummary(clustered_idk_scholar_models, stars = TRUE) 


# TABLE 2 and TABLE A3: Marginal effects using probit models with controls, public and scholars
idk_public_models_ME_all <-tidy_marginal_contrasts(idk_public_models$"IDK Public Pro Controls"$glm_res, vcov = vcovCR(idk_public_models$"IDK Public Pro Controls"$glm_res, cluster = Public_idk$id, type = "CR1"))  %>% select(variable, term, estimate, p.value) %>% mutate(Stars = case_when(
  p.value < 0.01 ~ "3 stars",
  p.value < 0.05 ~ "2 stars",
  p.value < 0.1 ~ "1 star",
  TRUE ~ "No stars" 
)) # TABLE 2 

idk_public_models_ME_int <-tidy_marginal_contrasts(idk_public_models$"IDK Public Pro Cont Int"$glm_res, vcov = vcovCR(idk_public_models$"IDK Public Pro Cont Int"$glm_res, cluster = Public_idk$id, type = "CR1"))  %>% select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  )) # TABLE 2

idk_scholar_models_ME <-tidy_marginal_contrasts(idk_scholar_models$"IDK Scholar Probit Con out Expt"$glm_res, vcov = vcovCR(idk_scholar_models$"IDK Scholar Probit Con out Expt"$glm_res, cluster = Scholar_idk$id, type = "CR1"))  %>% select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  )) # TABLE A3

idk_scholar_models_ME_all <-tidy_marginal_contrasts(idk_scholar_models$"IDK Scholar Probit Controls"$glm_res, vcov = vcovCR(idk_scholar_models$"IDK Scholar Probit Controls"$glm_res, cluster = Scholar_idk$id, type = "CR1"))  %>% select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  )) # TABLE 2 and TABLE A3


idk_scholar_models_ME_interactions <-tidy_marginal_contrasts(idk_scholar_models$"IDK Scholar Probit Cont Int"$glm_res, vcov = vcovCR(idk_scholar_models$"IDK Scholar Probit Cont Int"$glm_res, cluster = Scholar_idk$id, type = "CR1")) %>%
  select(variable, term, estimate, p.value) %>% mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  )) # TABLE 2 and TABLE A3


# 3. Don't Know - Combined Samples -------------------------------------------

# Combining samples 
Public_idk$sample <- "Public"
Scholar_idk_combined  <- Scholar_idk %>%
  select(-IssueAreaMatch,-IssueRegionMatch, -Rank) %>% #no equivalent in public sample
  mutate(sample='Scholar')  %>%
  mutate(Education = str_replace_all(Education, "PhD", "Medical (MD), law (JD),  or other doctorate degree (PhD)")) #reformat to match public sample

idk_combined <- rbind(Public_idk, Scholar_idk_combined)  %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow", "Broad")),
         category = factor(category, levels=category_opts),
         Race = factor(Race, levels=race_opts))

# Regression models with combined sample: OLS, Probit, Probit with controls, Probit interacting gender*sample with controls
idk_combined_models <- list(
  "IDK Combined OLS" = lm.cluster(idk ~ gender, cluster="id" , data=idk_combined),
  "IDK Comb Probit" = glm.cluster(idk ~ gender, cluster="id", family = binomial(link = "probit"), data=idk_combined),
  "IDK Comb Probit Cont" = glm.cluster(idk ~ gender + scope  + category + age + sample + Education + Race, cluster="id" ,family = binomial(link = "probit"), data=idk_combined),
  "IDK Comb Prob Cont Int" = glm.cluster(idk ~ gender*sample + scope + category + age + Education + Race, cluster="id" , family = binomial(link = "probit")  , data=idk_combined)
)

# Extract the actual model objects based on their class
nested_idk_combined_models <- lapply(idk_combined_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_idk_combined_models) <- names(idk_combined_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_idk_combined_models <- lapply(nested_idk_combined_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, idk_combined$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, idk_combined$id))
  }
})

# TABLE A4: Regression table, Probability of selecting "IDK" in combined sample
modelsummary(clustered_idk_combined_models, stars = TRUE) 

# TABLE 3: Marginal effects for "don't know," combined samples
idk_combined_models_ME_nointeraction <-tidy_marginal_contrasts(idk_combined_models$"IDK Comb Probit Cont"$glm_res, vcov = vcovCR(idk_combined_models$"IDK Comb Probit Cont"$glm_res, cluster = idk_combined$id, type = "CR1")) %>% select(variable, term, estimate, p.value)  %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
idk_combined_models_ME_all <-tidy_marginal_contrasts(idk_combined_models$"IDK Comb Prob Cont Int"$glm_res, vcov = vcovCR(idk_combined_models$"IDK Comb Prob Cont Int"$glm_res, cluster = idk_combined$id, type = "CR1")) %>% select(variable, term, estimate, p.value)  %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))

idk_combined_models_ME_nointeraction %>% 
  kable(.,caption="I don't know. Combined. Without interactions", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

idk_combined_models_ME_all %>% 
  kable(.,caption="I don't know. Combined. With interactions", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

# 4. Extreme Responses (Ordinal Non-Numeric) - Public Only -------------------------------

# order variables to set references for regression covariates
Public_extreme <- Public_extreme %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow", "Broad")),
         category = factor(category, levels=c("Analytical", "Normative")), #No question is Predictive
         Race = factor(Race, levels=race_opts),
         qtype = factor(qtype, levels=c("5-pt", "5-pt IDK")))

# Public only regression models:
Extreme_public_models <- list(
  "Extreme Public OLS"=lm.cluster(extreme ~ gender, cluster="id", data=Public_extreme),
  "Extreme Pub Probit"=glm.cluster(extreme ~ gender, cluster="id", family = binomial(link = "probit")  , data=Public_extreme),
  "Ext Pub Pro Con" = glm.cluster(extreme ~ gender + category + age + Education + Race + qtype, cluster="id", family = binomial(link = "probit")  , data=Public_extreme),
  "Ext Pub Pro Con Int" = glm.cluster(extreme ~ gender*Education + gender*age + category  + Race + qtype, cluster="id", family = binomial(link = "probit")  , data=Public_extreme)
)

#To create the table, need to extract the models:

# Extract the actual model objects based on their class
nested_extreme_public_models <- lapply(Extreme_public_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_extreme_public_models) <- names(Extreme_public_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_extreme_public_models <- lapply(nested_extreme_public_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Public_extreme$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Public_extreme$id))
  }
})

# TABLE A5: Regression table, Probability of selecting extreme result, ordinal, Public
modelsummary(clustered_extreme_public_models, stars = TRUE) 

# 5. Extreme Responses (Ordinal Non-Numeric) - Scholars Only -----------------------------

# order variables to set references for regression covariates
Scholar_extreme <- Scholar_extreme %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow", "Broad")),
         category = factor(category, levels=category_opts),
         Race = factor(Race, levels=race_opts),
         qtype = factor(qtype, levels=c("5-pt", "4-pt", "5-pt IDK", "4-pt IDK")))

# Scholars only regression models:
Extreme_scholar_models <- list(
  "Extreme Scholar OLS"=lm.cluster(extreme ~ gender ,cluster="id", data=Scholar_extreme),
  "Extreme Scholar Probit"=glm.cluster(extreme ~ gender , cluster="id",family = binomial(link = "probit")  , data=Scholar_extreme),
  "Extreme Scholar Probit Controls" = glm.cluster(extreme ~ gender + scope + category + age + Education + Race + Rank  +IssueAreaMatch + IssueRegionMatch  + qtype,cluster="id", family = binomial(link = "probit")  , data=Scholar_extreme),
  "Ext Scho Pro Cont Int" = glm.cluster(extreme ~ gender*IssueAreaMatch + gender*IssueRegionMatch+ gender*age + scope + category + Education + Race + Rank  + qtype, cluster="id",family = binomial(link = "probit"), data=Scholar_extreme))

#To create the table, need to extract the models:

# Extract the actual model objects based on their class
nested_extreme_scholar_models <- lapply(Extreme_scholar_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_extreme_scholar_models) <- names(Extreme_scholar_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_extreme_scholar_models <- lapply(nested_extreme_scholar_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Scholar_extreme$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Scholar_extreme$id))
  }
})

# TABLE A6: Regression table, Probability of selecting extreme answer, ordinal, scholars
modelsummary(clustered_extreme_scholar_models, stars = TRUE) 

# TABLE 4: Marginal effects, Extreme response, Scholars and Public
Extreme_scholar_models_ME_all <-tidy_marginal_contrasts(Extreme_scholar_models$"Extreme Scholar Probit Controls"$glm_res, vcov = vcovCR(Extreme_scholar_models$"Extreme Scholar Probit Controls"$glm_res, cluster = Scholar_extreme$id, type = "CR1")) %>% select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
Extreme_scholar_models_ME_interactions <-tidy_marginal_contrasts(Extreme_scholar_models$"Ext Scho Pro Cont Int"$glm_res, vcov = vcovCR(Extreme_scholar_models$"Extreme Scholar Probit Controls"$glm_res, cluster = Scholar_extreme$id, type = "CR1")) %>% select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
Extreme_public_models_ME_all <-tidy_marginal_contrasts(Extreme_public_models$"Ext Pub Pro Con"$glm_res, vcov = vcovCR(Extreme_public_models$"Ext Pub Pro Con"$glm_res, cluster = Public_extreme$id, type = "CR1"))  %>% select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
Extreme_public_models_ME_Int <-tidy_marginal_contrasts(Extreme_public_models$"Ext Pub Pro Con Int"$glm_res, vcov = vcovCR(Extreme_public_models$"Ext Pub Pro Con"$glm_res, cluster = Public_extreme$id, type = "CR1")) %>% select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))

Extreme_scholar_models_ME_all %>% 
  kable(.,caption="Extreme. Scholar. Without interactions.", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

Extreme_scholar_models_ME_interactions %>% 
  kable(.,caption="Extreme. Scholar. With interactions.", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

Extreme_public_models_ME_all %>% 
  kable(.,caption="Extreme Responses Public. Without interactions.", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

Extreme_public_models_ME_Int %>% 
  kable(.,caption="Extreme Responses Public with Interaction", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

# 6. Extreme Responses (Ordinal Non-Numeric) - Combined Samples --------------------------

#Formatting samples
Public_extreme$sample <- "Public"
Scholar_extreme_combined  <- Scholar_extreme %>%
  select(-IssueAreaMatch,-IssueRegionMatch, -Rank) %>% #no equivalent in public sample
  mutate(sample="Scholar")  %>%
  mutate(Education = str_replace_all(Education, "PhD", "Medical (MD), law (JD),  or other doctorate degree (PhD)")) #reformat to match public sample

# Combine samples
extreme_combined <- rbind(Public_extreme, Scholar_extreme_combined) %>%
  mutate(sample = factor(sample, levels=c("Public", "Scholar"))) 

# Regression models
Extreme_combined_models <- list(
  "Extreme Comb OLS"= lm.cluster(extreme ~ gender, cluster="id", data=extreme_combined),
  "Ext Comb Probit"= glm.cluster(extreme ~ gender, cluster="id",  family = binomial(link = "probit")  , data=extreme_combined),
  "Ext Comb Prob Cont" = glm.cluster(extreme ~ gender + scope + category + age + Education + Race + sample + qtype, cluster="id",  family = binomial(link = "probit")  , data=extreme_combined),
  "Ext Comb Prob Cont Int" = glm.cluster(extreme ~ gender*sample + scope + category + age + Education + Race + qtype, cluster="id", family = binomial(link = "probit")  , data=extreme_combined)
)

# Extract the actual model objects based on their class
nested_extreme_combined_models <- lapply(Extreme_combined_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_extreme_combined_models) <- names(Extreme_combined_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_extreme_combined_models <- lapply(nested_extreme_combined_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, extreme_combined$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, extreme_combined$id))
  }
})

# TABLE A7 - Regression table, probability of selecting extreme response, ordinal, combined sample
modelsummary(clustered_extreme_combined_models, stars = TRUE) 

# 7. Extreme Responses (Numeric) - Public Only --------------------------

# order variables to set references for regression covariates
Pub_extreme_numerical <- Pub_extreme_numerical %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow")), # all questions are narrow
         category = factor(category, levels=c("Analytical")), # no questions are "Normative" nor "Predictive"
         Race = factor(Race, levels=race_opts))

#Public only regression models:
extreme_numerical_public_models <- list(
  "Extreme Num Public OLS" = lm.cluster(extreme ~ gender, cluster="id",  data=Pub_extreme_numerical),
  "Extreme Num Pub Probit" = glm.cluster(extreme ~ gender, cluster="id", family = binomial(link = "probit")  , data=Pub_extreme_numerical),
  "Extreme Num Pub Cont Probit" = glm.cluster(extreme ~ gender + age + Education + Race,  cluster="id", family = binomial(link = "probit")  , data=Pub_extreme_numerical),
  "Extreme Num Pub Cont Int Probit" = glm.cluster(extreme ~ gender*Education + gender*age + Race,  cluster="id",  family = binomial(link = "probit")  , data=Pub_extreme_numerical)
)
# Extract the actual model objects based on their class
nested_extreme_numerical_public_models <- lapply(extreme_numerical_public_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_extreme_numerical_public_models) <- names(extreme_numerical_public_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_extreme_numerical_public_models <- lapply(nested_extreme_numerical_public_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Pub_extreme_numerical$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Pub_extreme_numerical$id))
  }
})

#TABLE A8: Regression table, probability of selecting an extreme response, numeric, public
modelsummary(clustered_extreme_numerical_public_models, stars = TRUE) 

# 8. Extreme Responses (Numeric) - Scholars Only  --------------------------

# order variables to set references for regression covariates
Scholar_extreme_numerical <- Scholar_extreme_numerical %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow")), # all questions are narrow
         category = factor(category, levels=c("Analytical", "Predictive")), # no question is Normative
         Race = factor(Race, levels=race_opts))

#Scholars only regression models:
extreme_numerical_scholar_models <- list(
  "Extreme Num Scholar OLS" = lm.cluster(extreme ~ gender , cluster="id", data=Scholar_extreme_numerical),
  "Extreme Num Scholar Probit" = glm.cluster(extreme ~ gender, cluster="id", family = binomial(link = "probit")  , data=Scholar_extreme_numerical),
  "Extreme Num Scholar Controls Probit" = glm.cluster(extreme ~ gender + category + age + Education + Race + Rank  +IssueAreaMatch + IssueRegionMatch,cluster="id", family = binomial(link = "probit")  , data=Scholar_extreme_numerical),
  "Extreme Num Sch Cont Pro Int" = glm.cluster(extreme ~ gender*IssueAreaMatch + gender*IssueRegionMatch + gender*age + category  + Education + Race + Rank , cluster="id", family = binomial(link = "probit")  , data=Scholar_extreme_numerical)
)

# Extract the actual model objects based on their class
nested_extreme_num_scholar_models <- lapply(extreme_numerical_scholar_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_extreme_num_scholar_models) <- names(extreme_numerical_scholar_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_extrme_num_scholar_models <- lapply(nested_extreme_num_scholar_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Scholar_extreme_numerical$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Scholar_extreme_numerical$id))
  }
})

# TABLE A9 - Regression table, probability of selecting extreme response, numerical, scholar sample
modelsummary(clustered_extrme_num_scholar_models, stars = TRUE) 

# TABLE 5 - Marginal effects, probability of selecting an extreme response, numeric, both scholar and public

Extreme_numerical_public_models_ME_all <-tidy_marginal_contrasts(extreme_numerical_public_models$"Extreme Num Pub Cont Probit"$glm_res, vcov = vcovCR(extreme_numerical_public_models$"Extreme Num Pub Cont Probit"$glm_res, cluster = Pub_extreme_numerical$id, type = "CR1")) %>%
  select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
Extreme_numerical_public_models_ME_Int <-tidy_marginal_contrasts(extreme_numerical_public_models$"Extreme Num Pub Cont Int Probit"$glm_res, vcov = vcovCR(extreme_numerical_public_models$"Extreme Num Pub Cont Int Probit"$glm_res, cluster = Pub_extreme_numerical$id, type = "CR1")) %>%
  select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
Extreme_numerical_scholar_models_ME_all <-tidy_marginal_contrasts(extreme_numerical_scholar_models$"Extreme Num Scholar Controls Probit"$glm_res, vcov = vcovCR(extreme_numerical_scholar_models$"Extreme Num Scholar Controls Probit"$glm_res, cluster = Scholar_extreme_numerical$id, type = "CR1")) %>%
  select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
Extreme_numerical_scholar_models_ME_withint <-tidy_marginal_contrasts(extreme_numerical_scholar_models$"Extreme Num Sch Cont Pro Int"$glm_res, vcov = vcovCR(extreme_numerical_scholar_models$"Extreme Num Sch Cont Pro Int"$glm_res, cluster = Scholar_extreme_numerical$id, type = "CR1")) %>%
  select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
Extreme_numerical_public_models_ME_all %>% 
  kable(.,caption="Extreme Numerical Responses Public", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

Extreme_numerical_public_models_ME_Int %>% 
  kable(.,caption="Extreme Numerical Responses Public, with Interaction", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

Extreme_numerical_scholar_models_ME_all %>% 
  kable(.,caption="Extreme Numerical Responses Scholar", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

Extreme_numerical_scholar_models_ME_withint  %>% 
  kable(.,caption="Extreme Numerical Responses Scholar. With interactions", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

# 9. Extreme Responses (Numeric) - Combined Samples --------------------------

#Formatting samples
Pub_extreme_numerical$sample <- "Public"
Scholar_extreme_numerical_combined  <- Scholar_extreme_numerical %>%
  select(-IssueAreaMatch,-IssueRegionMatch, -Rank) %>% #no equivalent in public sample
  mutate(sample="Scholar")  %>%
  mutate(Education = str_replace_all(Education, "PhD", "Medical (MD), law (JD),  or other doctorate degree (PhD)")) #reformat to match public sample

#Combine samples
extreme_numerical_combined <- rbind(Pub_extreme_numerical, Scholar_extreme_numerical_combined)

#Reorder to set reference category for regressions
extreme_numerical_combined <- extreme_numerical_combined %>%
  mutate(sample = factor(sample, levels=c("Public", "Scholar")))

#Combined sample regression models:
extreme_numerical_combined_models <- list(
  "Extreme Num Combined OLS" = lm.cluster(extreme ~ gender, cluster="id", data=extreme_numerical_combined),
  "Probit" = glm.cluster(extreme ~ gender, cluster="id", family = binomial(link = "probit")  , data=extreme_numerical_combined),
  "Probit Controls" = glm.cluster(extreme ~ gender + category + age + Education + Race + sample, cluster="id", family = binomial(link = "probit"), data=extreme_numerical_combined),
  "Probit Cont Int" =glm.cluster(extreme ~ gender*sample + category + age + Education + Race, family = binomial(link = "probit"), cluster="id", data=extreme_numerical_combined)
)

# Extract the actual model objects based on their class
nested_extreme_num_combined_models <- lapply(extreme_numerical_combined_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_extreme_num_combined_models) <- names(extreme_numerical_combined_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_extreme_num_comb_models <- lapply(nested_extreme_num_combined_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, extreme_numerical_combined$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, extreme_numerical_combined$id))
  }
})

#TABLE A10: Regression table, probability of selecting an extreme response, numerical, combined sample
modelsummary(clustered_extreme_num_comb_models, stars = TRUE) 

# TABLE 6 - Marginal effects, probability of selecting an extreme response,BOTH ordinal and numeric, combined sample
extreme_combined_models_ME_all_wout_int <-tidy_marginal_contrasts(Extreme_combined_models$"Ext Comb Prob Cont"$glm_res, vcov = vcovCR(Extreme_combined_models$"Ext Comb Prob Cont"$glm_res, cluster = extreme_combined$id, type = "CR1"))  %>% 
  select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
extreme_combined_models_ME_all <-tidy_marginal_contrasts(Extreme_combined_models$"Ext Comb Prob Cont Int"$glm_res, vcov = vcovCR(Extreme_combined_models$"Ext Comb Prob Cont Int"$glm_res, cluster = extreme_combined$id, type = "CR1"))  %>% 
  select(variable, term, estimate, p.value) %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
extreme_numerical_combined_models_ME_all_wout_int <-tidy_marginal_contrasts(extreme_numerical_combined_models$"Probit Controls"$glm_res, vcov = vcovCR(extreme_numerical_combined_models$"Probit Controls"$glm_res, cluster = extreme_numerical_combined$id, type = "CR1"))  %>%
  select(variable, term, estimate, p.value)  %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))
extreme_numerical_combined_models_ME_all <-tidy_marginal_contrasts(extreme_numerical_combined_models$"Probit Cont Int"$glm_res, vcov = vcovCR(extreme_numerical_combined_models$"Probit Cont Int"$glm_res, cluster = extreme_numerical_combined$id, type = "CR1"))  %>% select(variable, term, estimate, p.value)  %>%
  mutate(Stars = case_when(
    p.value < 0.01 ~ "3 stars",
    p.value < 0.05 ~ "2 stars",
    p.value < 0.1 ~ "1 star",
    TRUE ~ "No stars" 
  ))

extreme_combined_models_ME_all_wout_int %>% 
  kable(.,caption="Extreme Ordinal. Combined. Without interactions", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

extreme_combined_models_ME_all %>% 
  kable(.,caption="Extreme Ordinal. Combined. With interactions", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

extreme_numerical_combined_models_ME_all_wout_int %>% 
  kable(.,caption="Extreme Numerical. Combined. Without interactions", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value", "Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))

extreme_numerical_combined_models_ME_all %>% 
  kable(.,caption="Extreme Numerical. Combined. With interactions", 
        booktabs=pretty_latex_tables, 
        linesep = "",
        col.names = c("Variable","Term","Estimates","P.value","Stars"), 
        digits = 3,
        align=c("l", "r", "r", "r", "r")
  ) %>% kable_styling(latex_options = c("hold_position", "scale_down"))


# 10. Confidence - Non-Numerical (String) Responses - Scholars (No Public Equivalent) --------------------------

# order variables to set references for regression covariates
Scholar_confidence_string <- Scholar_confidence_string %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow", "Broad")),
         category = factor(category, levels=c("Analytical")), # No "Normative", "Predictive"questions, all analytical or NA
         qtype = factor(qtype, levels=c("4-pt", "4-pt IDK")), # No "5-pt" or "5-pt IDK" questions
         Race = factor(Race, levels=race_opts))

#Regressions
confidence_scholar_models <- list(
  "Confidence Scholar OLS" = lm.cluster(confidence ~ gender,  cluster="id", data=Scholar_confidence_string),
  "Confidence Scholar OLS Controls" = lm.cluster(confidence ~ gender + age + Education + Race + Rank  +IssueAreaMatch + IssueRegionMatch, cluster="id", data=Scholar_confidence_string),
  "Confidence Scholar OLS Controls Int" = lm.cluster(confidence ~ gender*IssueAreaMatch + gender*IssueRegionMatch + gender*age + Education + Race + Rank, cluster="id", data=Scholar_confidence_string))

# Extract the actual model objects based on their class
nested_confidence_scholar_models <- lapply(confidence_scholar_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_confidence_scholar_models) <- names(confidence_scholar_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_confidence_scholar_models <- lapply(nested_confidence_scholar_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Scholar_confidence_string$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Scholar_confidence_string$id))
  }
})

#TABLE 7: Regression table: Confidence levels, ordinal, scholar
#TABLE A12: confidence levels, ordinal, scholar, OLS
modelsummary(clustered_confidence_scholar_models, stars = TRUE) 

# 10. Confidence - Numerical Responses - Scholars --------------------------

# Scholar Data - reorder covariates to set reference categories
Scholar_confidence <- Scholar_confidence %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow", "Broad")),
         Race = factor(Race, levels=race_opts))

# Regressions for scholar confidence in numeric responses
confidence_numerical_scholar_models <- list(
  "Confidence Num Scholar OLS" = lm.cluster(confidence ~ gender,cluster="id", data=Scholar_confidence),
  "Confidence Num Scholar OLS Controls" = lm.cluster(confidence ~ gender + scope + age + Education + Race + Rank  +IssueAreaMatch + IssueRegionMatch, cluster="id",data=Scholar_confidence),
  "Confidence Num Scholar OLS Controls Int" = lm.cluster(confidence ~ gender*IssueAreaMatch + gender*IssueRegionMatch + gender*age + scope + Education + Race + Rank, cluster="id",data=Scholar_confidence)
)

# Extract the actual model objects based on their class
nested_confidence_num_scholar_models <- lapply(confidence_numerical_scholar_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_confidence_num_scholar_models) <- names(confidence_numerical_scholar_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_confidence_num_scholar_models <- lapply(nested_confidence_num_scholar_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Scholar_confidence$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Scholar_confidence$id))
  }
})

#TABLE 7 - Regression table: confidence levels, numerical, scholar
#TABLE A12 - confidence levels, numerical, scholar, OLS
modelsummary(clustered_confidence_num_scholar_models, stars = TRUE)

# 11. Confidence - Numerical Responses - Public --------------------------

# Public Data - reorder covariates to set reference categories
Pub_confidence <- Pub_confidence  %>%
  mutate(gender = factor(gender, levels=c("Male", "Female")),
         scope = factor(scope, levels=c("Narrow", "Broad")),
         Race = factor(Race, levels=race_opts))

# Regressions
confidence_numerical_public_models <- list(
  "Confidence Num Public OLS" = lm.cluster(confidence ~ gender, cluster="id", data=Pub_confidence),
  "Confidence Num Public OLS Controls" = lm.cluster(confidence ~ gender + scope + age + Education + Race, cluster="id", data=Pub_confidence),
  "Confidence Num Public OLS Int" = lm.cluster(confidence ~ gender*Education + gender*age  + scope + Race, cluster="id", data=Pub_confidence)
)

# Extract the actual model objects based on their class
nested_confidence_num_public_models <- lapply(confidence_numerical_public_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_confidence_num_public_models) <- names(confidence_numerical_public_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_confidence_num_public_models <- lapply(nested_confidence_num_public_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, Pub_confidence$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, Pub_confidence$id))
  }
}) 

#TABLE 7 - Regression table: confidence levels, numerical, public
#TABLE A12 - confidence levels, numerical, public, OLS
modelsummary(clustered_confidence_num_public_models, stars = TRUE)

# 12. Confidence - Numerical Responses - Combined --------------------------

#Formatting samples

Pub_confidence$sample  <- "Public"
Scholar_confidence_combined  <- Scholar_confidence %>%
  select(-IssueAreaMatch,-IssueRegionMatch, -Rank) %>% #no equivalent in public sample
  mutate(sample='Scholar') %>%
  mutate(Education = str_replace_all(Education, "PhD", "Medical (MD), law (JD),  or other doctorate degree (PhD)")) #reformat to match public sample

#Combine samples
confidence_combined <- rbind(Pub_confidence, Scholar_confidence_combined)

#Reorder to set reference category for regressions
confidence_combined <- confidence_combined %>%
  mutate(sample = factor(sample, levels=c("Public", "Scholar" )))

#Combined sample regression models:
confidence_numerical_combined_models <- list(
  "Confidence Num OLS" = lm.cluster(confidence ~ gender, cluster="id",  data=confidence_combined),
  "Confidence Num OLS Controls" = lm.cluster(confidence ~ gender +  scope  + age + sample + Education + Race, cluster="id", data=confidence_combined),
  "Confidence Num OLS Cont Int" = lm.cluster(confidence ~ gender*sample + scope  + age + Education + Race, cluster="id", data=confidence_combined)
)

# Extract the actual model objects based on their class
nested_confidence_num_combined_models <- lapply(confidence_numerical_combined_models, function(x) {
  if ("lm_res" %in% names(x)) {
    return(x$lm_res)
  } else if ("glm_res" %in% names(x)) {
    return(x$glm_res)
  }
})
# Create a named list of models
names(nested_confidence_num_combined_models) <- names(confidence_numerical_combined_models)
# Function to compute clustered standard errors
clustered_se <- function(model, cluster) {
  vcovCL <- vcovCL(model, cluster = cluster)
  coeftest(model, vcov = vcovCL)
}
# Apply the function to each model in the list
clustered_confidence_num_combined_models <- lapply(nested_confidence_num_combined_models, function(model) {
  if ("lm" %in% class(model)) {
    return(clustered_se(model, confidence_combined$id))
  } else if ("glm" %in% class(model)) {
    return(clustered_se(model, confidence_combined$id))
  }
})

#TABLE A11: numerical confidence levels, combined samples
modelsummary(clustered_confidence_num_combined_models,stars=TRUE)

# 13. FIGURES --------------------------

# FIGURE 1: Predicted probabilities of responding "IDK" by gender and education, public sample

# prep data for visualization
Public_idk_graph <- Public_idk
Public_idk_graph$Education <- recode(Public_idk_graph$Education,
                                     "Some schooling, but did not graduate from High School" = "Some HS",
                                     "High School graduate or equivalent (GED)" = "HS Graduate",
                                     "Some college, but did not complete a Bachelor's degree" = "Some College",
                                     "Bachelor's degree (BA/BS)" = "BA/BS", 
                                     "Master's degree (MA/MS/MBA, etc)" = "Masters",
                                     "Medical (MD), law (JD),  or other doctorate degree (PhD)" = "Doctorate")
Public_idk_graph$Education <- factor(Public_idk_graph$Education, levels = c("Some HS",
                                                                            "HS Graduate",
                                                                            "Some College",
                                                                            "BA/BS", 
                                                                            "Masters",
                                                                            "Doctorate"))
Public_idk_graph$gender <- factor(Public_idk_graph$gender, levels = c("Male","Female"))
# predicted probabilities
idk_public_models_graph <-  glm( idk ~ gender + category + age + Education + Race, family = binomial(link = "probit"), data=Public_idk_graph)
idk_public_pro_controls_ggpred <- ggpredict(idk_public_models_graph, terms = c("Education","gender"))
# plot
idk_public_graph  <- ggplot(idk_public_pro_controls_ggpred) + 
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high, linetype = group), 
                width = 0.2, color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  geom_text(aes(x = x, y = predicted, label = round(predicted, 2)),
            position = position_dodge(width = 0.4), hjust = -0.15, size = 3) +
  scale_linetype_manual(values = c("solid", "dashed")) + 
  ylab("Predicted Probability") +
  xlab("Education") +
  ggtitle("Probability of Answering Don't Know, Public Sample") +
  plot_theme_withlegend +
  theme(axis.title.y=element_text(angle=90, vjust=0.5, size=12)) 
#labs(fill = "", caption = "*The model from which these estimates are drawn also includes age, category, and race\nvariables, clustered at the respondent level.")

# FIGURE 2: Predicted probabilities of answering "don't know," Scholar & public

#public
#predicted probabilities
reg_idk_public_birthyear <- glm.cluster(idk ~ gender + Final_birth_year + Education + category + Race, cluster="id" , family = binomial(link = "probit"), data=Public_idk)
reg_idk_public_birthyear <- reg_idk_public_birthyear$glm_res
idk_public_birthyear_ggpred <- ggpredict(reg_idk_public_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot
idk_public_graph_birthyear <- ggplot(idk_public_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.5, show.legend = FALSE) +  
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Public Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5,size=12)) +
  scale_x_continuous(breaks = seq(min(idk_public_birthyear_ggpred$x), 
                                  max(idk_public_birthyear_ggpred$x), 
                                  by = 10))  +
  scale_fill_manual(values = c("gray75", "gray25")) +
  ylim(0, 0.6) 
# labs(fill = "", caption="*The model from which these estimates are drawn also\nincludes education, category, and race variables.") 


#scholar
#predicted probabilities
reg_idk_scholar_birthyear <- glm.cluster(idk ~ gender + Final_birth_year + category + scope  + Education + Race + Rank + IssueAreaMatch + IssueRegionMatch, cluster="id" , family = binomial(link = "probit"), data=Scholar_idk)
reg_idk_scholar_birthyear <- reg_idk_scholar_birthyear$glm_res
idk_scholar_birthyear_ggpred <- ggpredict(reg_idk_scholar_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot
idk_scholar_graph_birthyear <- ggplot(idk_scholar_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.5, show.legend = FALSE) +  
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Scholar Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5,size=12)) +
  scale_x_continuous(breaks = seq(min(idk_scholar_birthyear_ggpred$x), 
                                  max(idk_scholar_birthyear_ggpred$x), 
                                  by = 10))  +
  scale_fill_manual(values = c("grey75", "grey25")) +
  ylim(0, 0.6) 
# labs(fill = "", caption="*The model from which these estimates are drawn also includes\neducation, rank, category, scope, area of expertise, region of\nexpertise, and race variables.") 

#combine plots
combined_idk_plot <- (idk_scholar_graph_birthyear + idk_public_graph_birthyear) +
  plot_layout(ncol = 2, guides = "collect") & 
  plot_annotation(title = "Probability of Answering Don't Know") & # caption = "Models are clustered at the respondent level.") & 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times New Roman"), # Larger title size
    plot.caption = element_text(hjust = 0.5, size = 14, face = "italic", family = "Times New Roman"), 
    legend.text = element_text(size = 14, family = "Times New Roman"), 
    axis.text = element_text(size = 14, family = "Times New Roman"), 
    axis.title = element_text(size = 14, family = "Times New Roman") 
  )
# FIGURE 3: Predicted Probability, Extreme Response

#ORDINAL - SCHOLAR
#predicted probabilities
reg_extreme_scholar_birthyear <- glm.cluster(extreme ~ gender + Final_birth_year + scope + category  + Education + Race + Rank + IssueAreaMatch + IssueRegionMatch  + qtype, cluster="id", family = binomial(link = "probit"), data=Scholar_extreme)
reg_extreme_scholar_birthyear <- reg_extreme_scholar_birthyear$glm_res
extreme_scholar_birthyear_ggpred <- ggpredict(reg_extreme_scholar_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot
extreme_scholar_graph_birthyear <-  ggplot(extreme_scholar_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.5, show.legend = FALSE) +  
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Ordinal Extreme Answer, Scholar Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(extreme_scholar_birthyear_ggpred$x), 
                                  max(extreme_scholar_birthyear_ggpred$x), 
                                  by = 10)) +
  ylim(0, 0.6) +
  scale_fill_manual(values = c("gray75", "gray25")) 
# labs(fill = "", caption="*The model from which these estimates are drawn also includes\neducation, rank, category, scope, area of expertise, region of\nexpertise, question type, and race variables.") 



#ORDINAL - PUBLIC
#predicted probabilities
reg_extreme_public_birthyear <- glm.cluster(extreme ~ gender + category + Final_birth_year + Education + Race + qtype, cluster="id", family = binomial(link = "probit"), data=Public_extreme)
reg_extreme_public_birthyear <- reg_extreme_public_birthyear$glm_res
extreme_public_birthyear_ggpred <- ggpredict(reg_extreme_public_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot
extreme_public_graph_birthyear <- ggplot(extreme_public_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.5, show.legend = FALSE) +  
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Ordinal Extreme Answer, Public Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(extreme_public_birthyear_ggpred$x), 
                                  max(extreme_public_birthyear_ggpred$x), 
                                  by = 10))  +
  ylim(0, 0.6) +
  scale_fill_manual(values = c("gray75", "gray25")) 
# labs(fill = "", caption="*The model from which these estimates are drawn also\nincludes education, category, question type, and race\nvariables.") 


#NUMERIC - SCHOLAR
reg_extreme_num_scholar_birthyear <- glm.cluster(extreme ~ gender + category + Final_birth_year + Education + Race + Rank  +IssueAreaMatch + IssueRegionMatch, cluster="id" , family = binomial(link = "probit"), data=Scholar_extreme_numerical)
reg_extreme_num_scholar_birthyear <- reg_extreme_num_scholar_birthyear$glm_res
extreme_num_scholar_birthyear_ggpred <- ggpredict(reg_extreme_num_scholar_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot
extreme_num_scholar_birthyear <- ggplot(extreme_num_scholar_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.5, show.legend = FALSE) +  
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Numerical Extreme Answer, Scholar Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(extreme_num_scholar_birthyear_ggpred$x), 
                                  max(extreme_num_scholar_birthyear_ggpred$x), 
                                  by = 10))  +
  ylim(0, 0.6) +
  scale_fill_manual(values = c("gray75", "gray25")) 
# labs(fill = "", caption="*The model from which these estimates are drawn also includes\neducation, rank, category, area of expertise, region of\nexpertise, and race variables.") 


#NUMERIC - PUBLIC
#predicted probabilities
reg_extreme_num_public_birthyear <- glm.cluster(extreme ~ gender + Final_birth_year + Education + Race, cluster="id" , family = binomial(link = "probit"), data=Pub_extreme_numerical)
reg_extreme_num_public_birthyear <- reg_extreme_num_public_birthyear$glm_res
extreme_num_public_birthyear_ggpred <- ggpredict(reg_extreme_num_public_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot

extreme_num_public_birthyear <- ggplot(extreme_num_public_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.5, show.legend = FALSE) + 
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Numerical Extreme Answer, Public Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(extreme_num_public_birthyear_ggpred$x), 
                                  max(extreme_num_public_birthyear_ggpred$x), 
                                  by = 10)) +
  ylim(0, 0.6)  +
  scale_fill_manual(values = c("gray75", "gray25"))
# labs(fill = "", caption="*The model from which these estimates are drawn also\nincludes education, and race variables.") 

#combine plots
combined_extreme_graphs <- (extreme_scholar_graph_birthyear + extreme_public_graph_birthyear + 
                              extreme_num_scholar_birthyear + extreme_num_public_birthyear) +
  plot_layout(ncol = 2, guides = "collect") & 
  plot_annotation(title = "Probability of Selecting an Extreme Answer") & # caption = "Models are clustered at the respondent level.") & 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times New Roman"), # Larger title size
    plot.caption = element_text(hjust = 0.5, size = 14, face = "italic", family = "Times New Roman"), 
    legend.text = element_text(size = 14, family = "Times New Roman"), 
    axis.text = element_text(size = 14, family = "Times New Roman"), 
    axis.title = element_text(size = 14, family = "Times New Roman") 
  )

# FIGURE 4: Predicted Probability, Confidence Levels

#ORDINAL - SCHOLARS
#predicted probabilities
reg_confidence_scholar_birthyear <- lm.cluster(confidence ~ gender + Final_birth_year + Education + Race + Rank  +IssueAreaMatch + IssueRegionMatch, cluster="id" , data=Scholar_confidence_string)
reg_confidence_scholar_birthyear <- reg_confidence_scholar_birthyear$lm_res
confidence_scholar_birthyear_ggpred <- ggpredict(reg_confidence_scholar_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot

confidence_scholar_birthyear <- ggplot(confidence_scholar_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.5, show.legend = FALSE) +  
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Ordinal Confidence Levels, Scholar Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(confidence_scholar_birthyear_ggpred$x), 
                                  max(confidence_scholar_birthyear_ggpred$x), 
                                  by = 10))  +
  ylim(2.4, 8) +
  scale_fill_manual(values = c("grey75", "grey25")) 
# labs(fill = "", caption="*The model from which these estimates are drawn also includes\neducation, rank, area of expertise, region of expertise,\nand race variables.") 

#NUMERIC - SCHOLARS
#predicted probabilities
reg_confidence_num_scholar_birthyear <- lm.cluster(confidence ~ gender + scope + Final_birth_year + Education + Race + Rank + IssueAreaMatch + IssueRegionMatch, cluster="id",data=Scholar_confidence)
reg_confidence_num_scholar_birthyear <- reg_confidence_num_scholar_birthyear$lm_res
confidence_num_scholar_birthyear_ggpred <- ggpredict(reg_confidence_num_scholar_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot
confidence_num_scholar_birthyear <- ggplot(confidence_num_scholar_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.5, show.legend = FALSE) +  
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Numerical Confidence Levels, Scholar Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(confidence_num_scholar_birthyear_ggpred$x), 
                                  max(confidence_num_scholar_birthyear_ggpred$x), 
                                  by = 10)) +
  ylim(2.4, 8) +
  scale_fill_manual(values = c("grey75", "grey25")) 
# labs(fill = "", caption="*The model from which these estimates are drawn also includes\neducation, rank, scope, area of expertise, region of expertise,\nand race variables.") 

#NUMERIC - PUBLIC
#predicted probabilities
reg_confidence_num_public_birthyear <- lm.cluster(confidence ~ gender + scope + Final_birth_year + Education + Race, cluster="id" , data=Pub_confidence)
reg_confidence_num_public_birthyear <- reg_confidence_num_public_birthyear$lm_res
confidence_num_public_birthyear_ggpred <- ggpredict(reg_confidence_num_public_birthyear, terms = c("Final_birth_year [all]", "gender"))
#plot
confidence_num_public_birthyear <- ggplot(confidence_num_public_birthyear_ggpred) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.5, show.legend = FALSE) +  
  geom_line(aes(x = x, y = predicted, linetype = group, group = group), color = "black") +
  geom_point(aes(x = x, y = predicted, shape = group), size = 1.5, color = "black") +
  ylab("Predicted Probability") +
  xlab("Birth Year") +
  ggtitle("Numerical Confidence Levels, Public Sample") +
  plot_theme_withlegend +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = seq(min(confidence_num_public_birthyear_ggpred$x), 
                                  max(confidence_num_public_birthyear_ggpred$x), 
                                  by = 10)) +
  ylim(2.4, 8) +
  scale_fill_manual(values = c("grey75", "grey25")) 
# labs(fill = "", caption="*The model from which these estimates are drawn also\nincludes education, scope, and race variables.") 

# combine plots
combined_confidence_graphs <- (confidence_num_scholar_birthyear + confidence_num_public_birthyear +
                                 confidence_scholar_birthyear ) +
  plot_layout(ncol = 2, guides = "collect") & 
  plot_annotation(title = "Predicted Probability, Confidence Levels") & #caption = "Models are clustered at the respondent level.") & 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times New Roman"), # Larger title size
    plot.caption = element_text(hjust = 0.5, size = 14, face = "italic", family = "Times New Roman"), 
    legend.text = element_text(size = 14, family = "Times New Roman"), 
    axis.text = element_text(size = 14, family = "Times New Roman"), 
    axis.title = element_text(size = 14, family = "Times New Roman") 
  )