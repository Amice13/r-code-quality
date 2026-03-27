#load packages
library(tidyverse)
library(stargazer)
library(lfe)
library(broom)
library(jtools)
library(broom.mixed)
library(forcats)

here::here()

#Treating ideology as a straight up dummy variable (id_strong_vol), gives the same result as treating the id_vol_scaled as a factor. Treating id_vol_scaled as continuous leads to a different result.
#Three respondents only answered the first conjoint question

#load data
df_conjoint <- read_rds("data/clean_conjoint_values.rds") %>% 
  glimpse()

# 1) Just conjoint attributes, no controls
# Uses scaled volunteer ideology attribute, treated as a factor

# Fit a linear model with multiple group fixed effects, The formula specification is a response variable followed by a four part formula. The first part 
# consists of ordinary covariates, the second part consists of factors to be projected out. The third part is an IV-specification. The fourth part is a 
# cluster specification for the standard errors

mylpm1 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)|0|0|RecipientEmail, 
               data=df_conjoint)
summary(mylpm1)

mylpm1_rep <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol + 
                     ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)|0|0|RecipientEmail, 
                   data=subset(df_conjoint, republican_candidate=="1")) #select republicans
summary(mylpm1_rep)

mylpm1_dem <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol + 
                     ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)|0|0|RecipientEmail, 
                   data=subset(df_conjoint, republican_candidate=="0")) #select democrats
summary(mylpm1_dem)


### Create plots
coeflabels <- c("Gender",
                "   (Baseline = Male)",
                "   Female", 
                "Age",
                "   (Baseline = 72 years old)",
                "   22 years old", "   47 years old",
                "Race", 
                "   (Baseline = White)",
                "   African-American", "   Hispanic/Latino",
                "Dedication", 
                "   (Baseline = No Information)  ",
                "   Very dedicated and consistent", "   Inconsistent in their effort level", 
                "Reliability", 
                "   (Baseline = No Information)",
                "   Consistent and always follows campaign instructions", "   Inconsistent in following campaign instructions", 
                "Ideology",
                "   (Baseline = Moderate) ",
                "   Strong Conservative/Strong Progressive")


# create data frame from felm output
mylpm1_plot = data.frame(Model = "All",
                         value = c(NA, NA, subset(reshape::melt(mylpm1$coefficients))$value[2],
                                   NA, NA, subset(reshape::melt(mylpm1$coefficients))$value[3:4],
                                   NA, NA, subset(reshape::melt(mylpm1$coefficients))$value[5:6],
                                   NA, NA, subset(reshape::melt(mylpm1$coefficients))$value[7:8],
                                   NA, NA, subset(reshape::melt(mylpm1$coefficients))$value[9:10],
                                   NA, NA, subset(reshape::melt(mylpm1$coefficients))$value[11]),
                         se = c(NA, NA, subset(reshape::melt(mylpm1$cse))$value[2],
                                NA, NA, subset(reshape::melt(mylpm1$cse))$value[3:4],
                                NA, NA, subset(reshape::melt(mylpm1$cse))$value[5:6],
                                NA, NA, subset(reshape::melt(mylpm1$cse))$value[7:8],
                                NA, NA, subset(reshape::melt(mylpm1$cse))$value[9:10],
                                NA, NA, subset(reshape::melt(mylpm1$cse))$value[11]),
                         coef = coeflabels,
                         Type = "Base Model")

mylpm1_rep_plot = data.frame(Model = "Republicans",
                             value = c(NA, NA, subset(reshape::melt(mylpm1_rep$coefficients))$value[2],
                                       NA, NA, subset(reshape::melt(mylpm1_rep$coefficients))$value[3:4],
                                       NA, NA, subset(reshape::melt(mylpm1_rep$coefficients))$value[5:6],
                                       NA, NA, subset(reshape::melt(mylpm1_rep$coefficients))$value[7:8],
                                       NA, NA, subset(reshape::melt(mylpm1_rep$coefficients))$value[9:10],
                                       NA, NA, subset(reshape::melt(mylpm1_rep$coefficients))$value[11]),
                             se = c(NA, NA, subset(reshape::melt(mylpm1_rep$cse))$value[2],
                                    NA, NA, subset(reshape::melt(mylpm1_rep$cse))$value[3:4],
                                    NA, NA, subset(reshape::melt(mylpm1_rep$cse))$value[5:6],
                                    NA, NA, subset(reshape::melt(mylpm1_rep$cse))$value[7:8],
                                    NA, NA, subset(reshape::melt(mylpm1_rep$cse))$value[9:10],
                                    NA, NA, subset(reshape::melt(mylpm1_rep$cse))$value[11]),
                             coef = coeflabels,
                             Type = "Base Model")

mylpm1_dem_plot = data.frame(Model = "Democrats",
                             value = c(NA, NA, subset(reshape::melt(mylpm1_dem$coefficients))$value[2],
                                       NA, NA, subset(reshape::melt(mylpm1_dem$coefficients))$value[3:4],
                                       NA, NA, subset(reshape::melt(mylpm1_dem$coefficients))$value[5:6],
                                       NA, NA, subset(reshape::melt(mylpm1_dem$coefficients))$value[7:8],
                                       NA, NA, subset(reshape::melt(mylpm1_dem$coefficients))$value[9:10],
                                       NA, NA, subset(reshape::melt(mylpm1_dem$coefficients))$value[11]),
                             se = c(NA, NA, subset(reshape::melt(mylpm1_dem$cse))$value[2],
                                    NA, NA, subset(reshape::melt(mylpm1_dem$cse))$value[3:4],
                                    NA, NA, subset(reshape::melt(mylpm1_dem$cse))$value[5:6],
                                    NA, NA, subset(reshape::melt(mylpm1_dem$cse))$value[7:8],
                                    NA, NA, subset(reshape::melt(mylpm1_dem$cse))$value[9:10],
                                    NA, NA, subset(reshape::melt(mylpm1_dem$cse))$value[11]),
                             coef = coeflabels,
                             Type = "Base Model")

# bind data frames together for plotting
big.lpm1.plot.frame <- rbind(mylpm1_plot, mylpm1_rep_plot, mylpm1_dem_plot)
big.lpm1.plot.frame$coef <- factor(big.lpm1.plot.frame$coef, levels = coeflabels)

#plot coefficients
big.amce.plot1 <- ggplot(big.lpm1.plot.frame, aes(x = forcats::fct_rev(coef), y = value)) + 
  geom_point() + 
  coord_flip() + 
  theme_bw() + 
  facet_grid(Type ~ Model) + 
  geom_errorbar(aes(ymin = value - qnorm(0.975)*se, ymax = value + qnorm(0.975)*se), width = 0, size = 1)  + 
  #geom_errorbar(aes(ymin = value - qnorm(0.95)*se, ymax = value + qnorm(0.95)*se), width = 0, size = 1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  theme(axis.text.y = element_text(hjust = 0),
        axis.text.x = element_text(hjust = 1, angle = 45)) + 
  ylab('Expected Change in Probability of Choosing Volunteer with 95% Confidence Intervals') + 
  xlab('')
big.amce.plot1

# save last plot as png to the figs subfolder of the doc subfolder
ggsave("output/figures/amce_lpm1.png", height = 5, width = 10)

#########################################################################################

# 2) Conjoint attributes, volunteer ideology attribute (moderate=1, liberal/conservative=2, very liberal/conservative=3) interacted with campaign ideology (same modified scale)
## Uses factor volunteer ideology attribute and scaled CandidateIdeology variables, treated as continuous variable

mylpm2 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol  + 
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*CandidateIdeology_scaled|0|0|RecipientEmail, 
               data=df_conjoint)
summary(mylpm2)

mylpm2_rep <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol  + 
                     ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*CandidateIdeology_scaled|0|0|RecipientEmail, 
                   data=subset(df_conjoint, republican_candidate=="1"))
summary(mylpm2_rep)

mylpm2_dem <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol  + 
                     ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*CandidateIdeology_scaled|0|0|RecipientEmail, 
                   data=subset(df_conjoint, republican_candidate=="0"))
summary(mylpm2_dem)

# Create plot
coeflabels_lpm2 <- c("Gender",
                     "   (Baseline = Male)",
                     "   Female", 
                     "Age",
                     "   (Baseline = 72 years old)",
                     "   22 years old", "   47 years old",
                     "Race", 
                     "   (Baseline = White)",
                     "   African-American", "   Hispanic/Latino",
                     "Dedication", 
                     "   (Baseline = No Information)  ",
                     "   Very dedicated and consistent", "   Inconsistent in their effort level", 
                     "Reliability", 
                     "   (Baseline = No Information)",
                     "   Consistent and always follows campaign instructions", "   Inconsistent in following campaign instructions", 
                     "Ideology",
                     "   (Baseline = Moderate) ",
                     "   Strong Conservative/Strong Progressive",
                     "Candidate Ideology",
                     "Strong Conservative/Progressive X Candidate Ideology")

# create data frame from felm output
mylpm2_plot = data.frame(Model = "All",
                         value = c(NA, NA, subset(reshape::melt(mylpm2$coefficients))$value[2],
                                   NA, NA, subset(reshape::melt(mylpm2$coefficients))$value[3:4],
                                   NA, NA, subset(reshape::melt(mylpm2$coefficients))$value[5:6],
                                   NA, NA, subset(reshape::melt(mylpm2$coefficients))$value[7:8],
                                   NA, NA, subset(reshape::melt(mylpm2$coefficients))$value[9:10],
                                   NA, NA, subset(reshape::melt(mylpm2$coefficients))$value[11:13]),
                         se = c(NA, NA, subset(reshape::melt(mylpm2$cse))$value[2],
                                NA, NA, subset(reshape::melt(mylpm2$cse))$value[3:4],
                                NA, NA, subset(reshape::melt(mylpm2$cse))$value[5:6],
                                NA, NA, subset(reshape::melt(mylpm2$cse))$value[7:8],
                                NA, NA, subset(reshape::melt(mylpm2$cse))$value[9:10],
                                NA, NA, subset(reshape::melt(mylpm2$cse))$value[11:13]),
                         coef = coeflabels_lpm2,
                         Type = "Candidate Ideology Model")

mylpm2_dem_plot = data.frame(Model = "Democrats",
                             value = c(NA, NA, subset(reshape::melt(mylpm2_dem$coefficients))$value[2],
                                       NA, NA, subset(reshape::melt(mylpm2_dem$coefficients))$value[3:4],
                                       NA, NA, subset(reshape::melt(mylpm2_dem$coefficients))$value[5:6],
                                       NA, NA, subset(reshape::melt(mylpm2_dem$coefficients))$value[7:8],
                                       NA, NA, subset(reshape::melt(mylpm2_dem$coefficients))$value[9:10],
                                       NA, NA, subset(reshape::melt(mylpm2_dem$coefficients))$value[11:13]),
                             se = c(NA, NA, subset(reshape::melt(mylpm2_dem$cse))$value[2],
                                    NA, NA, subset(reshape::melt(mylpm2_dem$cse))$value[3:4],
                                    NA, NA, subset(reshape::melt(mylpm2_dem$cse))$value[5:6],
                                    NA, NA, subset(reshape::melt(mylpm2_dem$cse))$value[7:8],
                                    NA, NA, subset(reshape::melt(mylpm2_dem$cse))$value[9:10],
                                    NA, NA, subset(reshape::melt(mylpm2_dem$cse))$value[11:13]),
                             coef = coeflabels_lpm2,
                             Type = "Candidate Ideology Model")

mylpm2_rep_plot = data.frame(Model = "Republicans",
                             value = c(NA, NA, subset(reshape::melt(mylpm2_rep$coefficients))$value[2],
                                       NA, NA, subset(reshape::melt(mylpm2_rep$coefficients))$value[3:4],
                                       NA, NA, subset(reshape::melt(mylpm2_rep$coefficients))$value[5:6],
                                       NA, NA, subset(reshape::melt(mylpm2_rep$coefficients))$value[7:8],
                                       NA, NA, subset(reshape::melt(mylpm2_rep$coefficients))$value[9:10],
                                       NA, NA, subset(reshape::melt(mylpm2_rep$coefficients))$value[11:13]),
                             se = c(NA, NA, subset(reshape::melt(mylpm2_rep$cse))$value[2],
                                    NA, NA, subset(reshape::melt(mylpm2_rep$cse))$value[3:4],
                                    NA, NA, subset(reshape::melt(mylpm2_rep$cse))$value[5:6],
                                    NA, NA, subset(reshape::melt(mylpm2_rep$cse))$value[7:8],
                                    NA, NA, subset(reshape::melt(mylpm2_rep$cse))$value[9:10],
                                    NA, NA, subset(reshape::melt(mylpm2_rep$cse))$value[11:13]),
                             coef = coeflabels_lpm2,
                             Type = "Candidate Ideology Model")

# bind data frames together for plotting
big.lpm2.plot.frame <- rbind(mylpm2_plot, mylpm2_rep_plot, mylpm2_dem_plot)
big.lpm2.plot.frame$coef <- factor(big.lpm2.plot.frame$coef, levels = coeflabels_lpm2)

#plot coefficients
big.amce.plot2 <- ggplot(big.lpm2.plot.frame, aes(x = forcats::fct_rev(coef), y = value)) + 
  geom_point() + 
  coord_flip() + 
  theme_bw() + 
  facet_grid(Type ~ Model) + 
  geom_errorbar(aes(ymin = value - qnorm(0.975)*se, ymax = value + qnorm(0.975)*se), width = 0, size = 1)  + 
  #geom_errorbar(aes(ymin = value - qnorm(0.95)*se, ymax = value + qnorm(0.95)*se), width = 0, size = 1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  theme(axis.text.y = element_text(hjust = 0),
        axis.text.x = element_text(hjust = 1, angle = 45)) + 
  ylab('Expected Change in Probability of Choosing Volunteer with 95% Confidence Intervals') + 
  xlab('')
big.amce.plot2

# save last plot as png to the figs subfolder of the doc subfolder
ggsave("output/figures/amce_lpm2.png", height = 5, width = 10)

####################################################################################################

# 3) Same as #2 but with personal ideology instead of campaign ideology
## Uses factor volunteer ideology attribute and scaled CandidateIdeology variables, treated as continuous variable

mylpm3 <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol +  
                 ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*PersonalIdeology_scaled|0|0|RecipientEmail, 
               data=df_conjoint)
summary(mylpm3)

mylpm3_rep <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol +  
                     ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*PersonalIdeology_scaled|0|0|RecipientEmail, 
                   data=subset(df_conjoint, republican_candidate=="1"))
summary(mylpm3_rep)

mylpm3_dem <- felm(choice ~ female_vol + age_twentytwo_vol + age_fortyseven_vol + race_africanamerican_vol + race_hispanic_vol +  
                     ded_cons_vol + ded_incon_vol + comp_con_vol + comp_incon_vol + as.factor(id_vol_scaled)*PersonalIdeology_scaled|0|0|RecipientEmail, 
                   data=subset(df_conjoint, republican_candidate=="0"))
summary(mylpm3_dem)


# Create plot
coeflabels_lpm3 <- c("Gender",
                     "   (Baseline = Male)",
                     "   Female", 
                     "Age",
                     "   (Baseline = 72 years old)",
                     "   22 years old", "   47 years old",
                     "Race", 
                     "   (Baseline = White)",
                     "   African-American", "   Hispanic/Latino",
                     "Dedication", 
                     "   (Baseline = No Information)  ",
                     "   Very dedicated and consistent", "   Inconsistent in their effort level", 
                     "Reliability", 
                     "   (Baseline = No Information)",
                     "   Consistent and always follows campaign instructions", "   Inconsistent in following campaign instructions", 
                     "Ideology",
                     "   (Baseline = Moderate) ",
                     "   Strong Conservative/Strong Progressive",
                     "Personal Ideology",
                     "Strong Conservative/Progressive X Personal Ideology")

# create data frame from felm output
mylpm3_plot = data.frame(Model = "All",
                         value = c(NA, NA, subset(reshape::melt(mylpm3$coefficients))$value[2],
                                   NA, NA, subset(reshape::melt(mylpm3$coefficients))$value[3:4],
                                   NA, NA, subset(reshape::melt(mylpm3$coefficients))$value[5:6],
                                   NA, NA, subset(reshape::melt(mylpm3$coefficients))$value[7:8],
                                   NA, NA, subset(reshape::melt(mylpm3$coefficients))$value[9:10],
                                   NA, NA, subset(reshape::melt(mylpm3$coefficients))$value[11:13]),
                         se = c(NA, NA, subset(reshape::melt(mylpm3$cse))$value[2],
                                NA, NA, subset(reshape::melt(mylpm3$cse))$value[3:4],
                                NA, NA, subset(reshape::melt(mylpm3$cse))$value[5:6],
                                NA, NA, subset(reshape::melt(mylpm3$cse))$value[7:8],
                                NA, NA, subset(reshape::melt(mylpm3$cse))$value[9:10],
                                NA, NA, subset(reshape::melt(mylpm3$cse))$value[11:13]),
                         coef = coeflabels_lpm3,
                         Type = "Personal Ideology Model")

mylpm3_dem_plot = data.frame(Model = "Democrats",
                             value = c(NA, NA, subset(reshape::melt(mylpm3_dem$coefficients))$value[2],
                                       NA, NA, subset(reshape::melt(mylpm3_dem$coefficients))$value[3:4],
                                       NA, NA, subset(reshape::melt(mylpm3_dem$coefficients))$value[5:6],
                                       NA, NA, subset(reshape::melt(mylpm3_dem$coefficients))$value[7:8],
                                       NA, NA, subset(reshape::melt(mylpm3_dem$coefficients))$value[9:10],
                                       NA, NA, subset(reshape::melt(mylpm3_dem$coefficients))$value[11:13]),
                             se = c(NA, NA, subset(reshape::melt(mylpm3_dem$cse))$value[2],
                                    NA, NA, subset(reshape::melt(mylpm3_dem$cse))$value[3:4],
                                    NA, NA, subset(reshape::melt(mylpm3_dem$cse))$value[5:6],
                                    NA, NA, subset(reshape::melt(mylpm3_dem$cse))$value[7:8],
                                    NA, NA, subset(reshape::melt(mylpm3_dem$cse))$value[9:10],
                                    NA, NA, subset(reshape::melt(mylpm3_dem$cse))$value[11:13]),
                             coef = coeflabels_lpm3,
                             Type = "Personal Ideology Model")

mylpm3_rep_plot = data.frame(Model = "Republicans",
                             value = c(NA, NA, subset(reshape::melt(mylpm3_rep$coefficients))$value[2],
                                       NA, NA, subset(reshape::melt(mylpm3_rep$coefficients))$value[3:4],
                                       NA, NA, subset(reshape::melt(mylpm3_rep$coefficients))$value[5:6],
                                       NA, NA, subset(reshape::melt(mylpm3_rep$coefficients))$value[7:8],
                                       NA, NA, subset(reshape::melt(mylpm3_rep$coefficients))$value[9:10],
                                       NA, NA, subset(reshape::melt(mylpm3_rep$coefficients))$value[11:13]),
                             se = c(NA, NA, subset(reshape::melt(mylpm3_rep$cse))$value[2],
                                    NA, NA, subset(reshape::melt(mylpm3_rep$cse))$value[3:4],
                                    NA, NA, subset(reshape::melt(mylpm3_rep$cse))$value[5:6],
                                    NA, NA, subset(reshape::melt(mylpm3_rep$cse))$value[7:8],
                                    NA, NA, subset(reshape::melt(mylpm3_rep$cse))$value[9:10],
                                    NA, NA, subset(reshape::melt(mylpm3_rep$cse))$value[11:13]),
                             coef = coeflabels_lpm3,
                             Type = "Personal Ideology Model")

# bind data frames together for plotting
big.lpm3.plot.frame <- rbind(mylpm3_plot, mylpm3_rep_plot, mylpm3_dem_plot)
big.lpm3.plot.frame$coef <- factor(big.lpm3.plot.frame$coef, levels = coeflabels_lpm3)

#plot coefficients
big.amce.plot3 <- ggplot(big.lpm3.plot.frame, aes(x = forcats::fct_rev(coef), y = value)) + 
  geom_point() + 
  coord_flip() + 
  theme_bw() + 
  facet_grid(Type ~ Model) + 
  geom_errorbar(aes(ymin = value - qnorm(0.975)*se, ymax = value + qnorm(0.975)*se), width = 0, size = 1)  + 
  #geom_errorbar(aes(ymin = value - qnorm(0.95)*se, ymax = value + qnorm(0.95)*se), width = 0, size = 1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  theme(axis.text.y = element_text(hjust = 0),
        axis.text.x = element_text(hjust = 1, angle = 45)) + 
  ylab('Expected Change in Probability of Choosing Volunteer with 95% Confidence Intervals') + 
  xlab('')
big.amce.plot3

# save last plot as png to the figs subfolder of the doc subfolder
ggsave("output/figures/amce_lpm3.png", height = 5, width = 10)



#####################################################################

# Create Latex Tables

conjoint <- stargazer(mylpm1, mylpm1_dem, mylpm1_rep,
                      title="Conjoint Results: Base Model",
                      dep.var.labels=c("Dependent Variable: Choice"),
                      column.labels = c("All", "Democrats", "Republicans"),
                      covariate.labels=c("Gender: Female","Age: 22", "Age: 47", "Race: African-American", "Race: Hispanic", "Dedication: Consistent",
                                         "Dedication: Inconsistent", "Reliability: Consistent", "Reliability: Inconsistent", "Ideology: Strong Conservative/Progressive"),
                      align=TRUE, 
                      font.size="small", 
                      style="apsr",
                      digits=2,
                      no.space=TRUE,
                     # notes="Omitted dummy variables: Gender: Male, Age: 72, Race: White, Dedication: No Information, Reliability: No Information.",
                      out ="doc/tables/conjoint_table.tex")

conjoint2 <- stargazer(mylpm2, mylpm2_dem, mylpm2_rep,
                      title="Conjoint Results: Candidate Ideology",
                      dep.var.labels=c("Dependent Variable: Choice"),
                      column.labels = c("All", "Democrats", "Republicans"),
                      covariate.labels=c("Gender: Female","Age: 22", "Age: 47", "Race: African-American", "Race: Hispanic", "Dedication: Consistent",
                                         "Dedication: Inconsistent", "Reliability: Consistent", "Reliability: Inconsistent", "Ideology: Strong Conservative/Progressive", 
                     "Candidate Ideology", "Strong Conservative/Progressive $\\times$ Candidate Ideology"),
                      align=TRUE, 
                      font.size="small", 
                      style="apsr",
                      digits=2,
                      no.space=TRUE,
                   #  notes="Omitted dummy variables include: Gender: Male, Age: 72, Race: White, Dedication: No Information, Reliability: No Information.",
                      out ="output/tables/conjoint2_table.tex")

conjoint3 <- stargazer(mylpm3, mylpm3_dem, mylpm3_rep,
                      title="Conjoint Results: Personal Ideology",
                      dep.var.labels=c("Dependent Variable: Choice"),
                      column.labels = c("All", "Democrats", "Republicans"),
                      covariate.labels=c("Gender: Female","Age: 22", "Age: 47", "Race: African-American", "Race: Hispanic", "Dedication: Consistent",
                                         "Dedication: Inconsistent", "Reliability: Consistent", "Reliability: Inconsistent", "Ideology: Strong Conservative/Progressive", 
                                         "Personal Ideology", "Strong Conservative/Progressive $\\times$ Personal Ideology"),
                      align=TRUE, 
                      font.size="small", 
                      style="apsr",
                      digits=2,
                      no.space=TRUE,
                  #   notes="Omitted dummy variables include: Gender: Male, Age: 72, Race: White, Dedication: No Information, Reliability: No Information.",
                      out ="output/tables/conjoint3_table.tex")

# Notes
# https://stackoverflow.com/questions/44027482/cluster-robust-standard-errors-in-stargazer