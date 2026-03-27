#load packages
library(tidyverse)
library(stargazer)
library(lfe)
library(broom)
library(jtools)
library(broom.mixed)
library(forcats)

here::here()

#Three respondents only answered the first conjoint question

#repeat handles
#95eacdd0 other repeat
#d0459e57

#load data
df_rc <- read_rds("data/clean_conjoint_values.rds") %>%   glimpse()

#base model
mylpm1 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)|0|0|RecipientEmail, 
               data=df_rc)
summary(mylpm1)

#has twitter
rc1 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)|0|0|RecipientEmail, 
               data=df_rc, subset=has_twitter==1)
summary(rc1)

#has fec amount over 50000
rc2 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)|0|0|RecipientEmail, 
               data=df_rc, subset=has_fec_fifty==1)
summary(rc2)

#has fec amount in upper quartile
rc3 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)|0|0|RecipientEmail, 
               data=df_rc, subset=fec_upperq==1)
summary(rc3)

#won primary
rc4 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)|0|0|RecipientEmail, 
               data=df_rc, subset=primary_winner==1)
summary(rc4)



#Create table
robust <- stargazer(mylpm1, rc1, rc2, rc4,
                      title="Robustness Checks Base Model",
                      dep.var.labels=c("Dependent Variable: Choice"),
                      column.labels = c("All", "Has Twitter", "FEC gt/eq 50000", "Primary Winner"),
                      covariate.labels=c("Gender: Female","Age: 22", "Age: 47", "Race: African-American", "Race: Hispanic", "Dedication: Consistent",
                                         "Dedication: Inconsistent", "Reliability: Consistent", "Reliability: Inconsistent", "Ideology: Strong Conservative/Progressive"),
                      align=TRUE, 
                      font.size="small", 
                      style="apsr",
                      digits=2,
                      no.space=TRUE,
                      notes="Three campaigns responded twice. One email address in our contact list was associated with two similar campaigns; one campaign was included.",
                      out ="output/tables/robust_table.tex")



#interaction model 
mylpm2 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol  + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*CandidateIdeology_scaled|0|0|RecipientEmail, 
               data=df_rc)
summary(mylpm2)

#has twitter
rc_a <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol  + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*CandidateIdeology_scaled|0|0|RecipientEmail, 
               data=df_rc, subset=has_twitter==1)
summary(rc_a)

#has fec amount over 50000
rc_b <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol  + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*CandidateIdeology_scaled|0|0|RecipientEmail, 
               data=df_rc, subset=has_fec_fifty==1)
summary(rc_b)

#has fec in upper quartile
rc_c <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol  + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*CandidateIdeology_scaled|0|0|RecipientEmail, 
               data=df_rc, subset=primary_winner==1)
summary(rc_c)





#Create table
conjoint2 <- stargazer(mylpm2, rc_a, rc_b, rc_c,
                      title="Robustness Checks Candidate Ideology",
                      dep.var.labels=c("Dependent Variable: Choice"),
                      column.labels = c("All", "Has Twitter", "FEC gt/eq 50000", "Primary Winner"),
                      covariate.labels=c("Gender: Female","Age: 22", "Age: 47", "Race: African-American", "Race: Hispanic", "Dedication: Consistent",
                                         "Dedication: Inconsistent", "Reliability: Consistent", "Reliability: Inconsistent", "Ideology: Strong Conservative/Progressive", 
                     "Candidate Ideology", "Strong Conservative/Progressive $\\times$ Candidate Ideology"),
                      align=TRUE, 
                      font.size="small", 
                      style="apsr",
                      digits=2,
                      no.space=TRUE,
                   #  notes="Omitted dummy variables include: Gender: Male, Age: 72, Race: White, Dedication: No Information, Reliability: No Information.",
                      out ="output/tables/robust2_table.tex")

