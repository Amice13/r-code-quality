#-------------------------------------------------------------------------------#
#
#                 Re-analyze the Sero-EPi in Children
#                 Author: Hillary Kibet Kiprono.
#-------------------------------------------------------------------------------#

# House keeping
rm(list = ls())


#setwd("") 
getwd()

#load Libraries
library(haven)      # Read .dta to R.
library(tidyverse)  # Data Manipulation
library(gtsummary)  # Data summaries
library(ggplot2)    # Graphs in R.

# Load Stata dataset to R.
#mydir <- setwd(r"(")

SEROEPI<- haven::as_factor(read_dta(paste0(mydir,"/analysis_set8_mv4fold.dta")))

# Analysis.
# Table 1:0 : Descriptives
table1::table1(~ age_months + age_cat6 + sex + 
                 weight + height + muac + hb +
                 p_diarrhea + m_diarrhea + m_fibrile + p_febrile| location, SEROEPI)

# Table 2.0
# Proportion of sero-types.
Prevalence <- SEROEPI %>% 
  select(location,age_cat6,170:194)

# Prevalence of IgG antibodies against Shigella.
# Explore whether the effect of age is heterogeneous (or constant) across the different levels of site.
# This will be done using traditional chi-square test of heterogeneity.
# Later explore this interactions using binary logistic regression for each antibody antigen.

# Hypothesis: Ho: The effect of age_category is homogeneous across different sites ;
#             HA: the effect of age_category varies by site.


# Use logistic regression models

# Sonnei
# Null model : without interaction.
m0 <- glm(status_sonnei ~ age_cat6 + location , family = binomial(), Prevalence)

# Full model with interactions.
m1 <- glm(status_sonnei ~ age_cat6 + location + age_cat6*location, family = binomial(), Prevalence)

# Compare the models
anova(m0,m1, test = "LRT") # Just above the marginal boarder. P = 0.08047

# Flexneri 6
# Null model : without interaction.
m1 <- glm(status_flexneri_6 ~ age_cat6 + location , family = binomial(), Prevalence)

# Full model with interactions.
m2 <- glm(status_flexneri_6 ~ age_cat6 + location + age_cat6*location, family = binomial(), Prevalence)

# Compare the models
anova(m1,m2, test = "LRT") # Sufficient evidence to suggest interaction. Reject the null hypothesis.p < 0.001


# Flexneri 3a
# Null model : without interaction.
m3 <- glm(status_flexneri_3a ~ age_cat6 + location , family = binomial(), Prevalence)

# Full model with interactions.
m4 <- glm(status_flexneri_3a ~ age_cat6 + location + age_cat6*location, family = binomial(), Prevalence)

# Compare the models
anova(m3,m4, test = "LRT") # Marginal border line. p = 0.06721

# Flexneri 2a
# Null model : without interaction.
m5 <- glm(status_flexneri_2a ~ age_cat6 + location , family = binomial(), Prevalence)

# Full model with interactions.
m6 <- glm(status_flexneri_2a ~ age_cat6 + location + age_cat6*location, family = binomial(), Prevalence)

# Compare the models
anova(m5,m6, test = "LRT") # Interaction not significant. p = 0.4834

# Flexneri 1b
# Null model : without interaction.
m7 <- glm(status_flexneri_1b ~ age_cat6 + location , family = binomial(), Prevalence)

# Full model with interactions.
m8 <- glm(status_flexneri_1b ~ age_cat6 + location + age_cat6*location, family = binomial(), Prevalence)

# Compare the models
anova(m7,m8, test = "LRT") # No significant interactions; p = 0.185

#-------------------------------------------------------------------------------#