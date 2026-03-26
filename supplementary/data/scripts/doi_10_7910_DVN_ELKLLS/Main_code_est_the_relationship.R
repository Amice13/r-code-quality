###### Replication Code #######
# Replication code for 
# “Estimating the Relationship Between Water Utility Workforce Quality
# and Water Utility Performance” 
# Last updated 8/17/2025 by Robin Rose Saywitz 
# 
# Note 1 on data availability: This code begins at the point of merging the data 
# from the American  Community Survey and Safe Drinking Water Information System 
# in one data set  with the workforce data available in the data set from the 
# Texas Commission on Environmental Quality. At this point all data is 
# aggregated at the water utility-level. 
# 
# Data in earlier steps in the data cleaning process contains information that 
# Texas no longer considers publicly available according to SB 510, written 
# by the 88th Texas State Legislature. Interested parties may contact the author
# regarding this data by emailing her at robin.saywitz@slu.edu. Depending on the 
# request, it may be possible to reconfigure and provide data from earlier steps
# without the data now no longer considered public. 
# 
# Note 2 on tables and figures: Below is a list of figures and tables in the 
# manuscript. Figures created in Power Point are noted. Tables modified or 
# created in Excel are also noted. When a table in the manuscript is a 
# combination of a table created in R and outside calculations, this is noted
# as well. The final tables in the manuscript were reformatted by the publisher
# so the tables produced by this file will not be identical in formatting 
# to those in the manuscript. 


##### Installing and loading required packages ####

#install.packages("readxl")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("MASS")
#install.packages("gridExtra")
#install.packages("RecordLinkage")
#install.packages("stringdist")
#install.packages("fuzzyjoin")
#install.packages("ggpubr")
#install.packages("stargazer")
#install.packages("sjPlot")
#install.packages("sjmisc")
#install.packages("GGally")
#install.packages("margins")
#install.packages("marginaleffects")
#install.packages("modelsummary")
#install.packages("broom")
#install.packages("jtools")
#install.packages("corrplot")

library("readxl")
library("plyr")
library("dplyr")
library("tidyverse")
library("MASS")
library("gridExtra")
library("RecordLinkage")
library("stringdist")
library("fuzzyjoin")
library("ggpubr")
library("stargazer")
library("sjPlot")
library("sjmisc")
library("GGally")
library("margins")
library("marginaleffects")
library("modelsummary")
library("broom")
library("jtools")
library("corrplot")

#### Loading the two data files ####

ACS_SDWIS_data <- read_csv("ACS_SDWIS_data.csv")
TCEQ_data <- read_csv("TCEQ_data.csv")

#### Matching water utilities by name across the two data files ####

# Direct matching procedure  
utility_operator_complete_direct <- inner_join(ACS_SDWIS_data, TCEQ_data, by = "pwsname_direct")
utility_operator_complete_anti <- anti_join(ACS_SDWIS_data, TCEQ_data, by = "pwsname_direct")


# Fuzzy matching procedure
# Step 1: Creating the match table
match_table <- expand.grid(pwsname_a = ACS_SDWIS_data$pwsname_probabalistic, pwsname_b = TCEQ_data$pwsname_probabalistic) %>%
  mutate(distance = stringdist(pwsname_a, pwsname_b, method = "jw")) 

# Step 2: Best match in B for each A
a_to_b_best <- match_table %>%
  group_by(pwsname_a) %>%
  slice_min(order_by = distance, n = 1, with_ties = FALSE) %>%
  ungroup()

# Step 3: Best match in A for each B
b_to_a_best <- match_table %>%
  group_by(pwsname_b) %>%
  slice_min(order_by = distance, n = 1, with_ties = FALSE) %>%
  ungroup()

reciprocal_best <- inner_join(a_to_b_best, b_to_a_best, 
                              by = c("pwsname_a", "pwsname_b"))

ACS_SDWIS_data$pwsname_a <- ACS_SDWIS_data$pwsname_probabalistic
TCEQ_data$pwsname_b <- TCEQ_data$pwsname_probabalistic

utility_operator_complete_fuzzy <- ACS_SDWIS_data %>%
  inner_join(reciprocal_best, by = "pwsname_a") %>%
  inner_join(TCEQ_data, by = "pwsname_b")

##### Tables #### 

##### Table 1. ####
# Summary statistics for all datasets including the original 
# SDWIS data, the matched datasets, and the unmatched utilities

# This table was created in Excel using the values calculated through
# running the summary command on the four data sets below
summary(ACS_SDWIS_data) # all ACS and SDWIS data
summary(utility_operator_complete_direct) # Utilities that matched using deterministic matching 
summary(utility_operator_complete_fuzzy) # Utilities that matched using fuzzy matching
summary(utility_operator_complete_anti) # Utilities from the ACS and SDWIS dataset that did not match deterministically
# with the workforce data available in the TCEQ dataset. 


##### Table 2. #####
# Negative binomial predicting health and monitoring and reporting
# violations from 2020 to 2023

# First running the two negative binomial regression models using the 
# deterministic matched data set

h_20_23_direct <- glm.nb(health_20_23 ~ workforce_score +
                           pwsid_has_BA + pwsid_medincome  + 
                           pwsid_black + pwsid_asian+ pwsid_hispanic +
                           pop_density_utility + very_large_utility + large_utility + medium_utility + 
                           ground_water + surface_water + owner_private + owner_municipal_district,
                         data = utility_operator_complete_direct)

summary(h_20_23_direct)


mr_20_23_direct <- glm.nb(mr_20_23 ~ workforce_score +
                            pwsid_has_BA + pwsid_medincome  + 
                            pwsid_black + pwsid_asian+ pwsid_hispanic +
                            pop_density_utility + very_large_utility + large_utility + medium_utility + 
                            ground_water + surface_water + owner_private + owner_municipal_district,
                          data = utility_operator_complete_direct)

summary(mr_20_23_direct)

# Producing the average marginal effects included in the reformatted table
margins(h_20_23_direct)
margins(mr_20_23_direct)

# Regression output 
stargazer(h_20_23_direct, mr_20_23_direct, dep.var.labels=c("Health - 2020-2023","Monitoring and Reporting - 2020-2023"), 
          covariate.labels=c("Workforce score", "Percent has BA", "Median income",
                             "Percent Black residents", "Percent Asian residents", "Percent Hispanic residents", 
                             "Population density", "Very large utility", "Large utility", "Medium utility", 
                             "Ground water", "Surface water", "Private utility", "Special district utility"),
          style = "all",
          notes = "Underlying data was created with the direct matching procedure discussed in the body of the paper.",
          type="html",  out="body_table_2.htm")
# The final table 2 is build on the stargazer output above, rounded to 
# two decimal points, and with two additional columns containing the 
# average marginal effects

# These lines of code produce the values discussed in the paragraph below the table
avg_comparisons(h_20_23_direct, newdata = "mean",  variables = list(workforce_score = c(8, 24)))
avg_comparisons(mr_20_23_direct, newdata = "mean",  variables = list(workforce_score = c(8, 24)))


#### Table B1. ####
#Summary of original data set, matched data sets, 
# and unmatched utilities

# This table was created in Excel using the values calculated through
# running the summary command on the four data sets below
summary(ACS_SDWIS_data)
summary(utility_operator_complete_direct)
summary(utility_operator_complete_fuzzy)
summary(utility_operator_complete_anti)

# Table C1. Negative binomial predicting health violations from 
# 2017 to 2020, 2020 alone, and 2020 to 2023


h_20_23_fuzzy <- glm.nb(health_20_23 ~ workforce_score +
                             pwsid_has_BA + pwsid_medincome  + 
                             pwsid_black + pwsid_asian+ pwsid_hispanic +
                             pop_density_utility + very_large_utility + large_utility + medium_utility + 
                             ground_water + surface_water + owner_private + owner_municipal_district,
                           data = utility_operator_complete_fuzzy)

summary(h_20_23_fuzzy)


h_17_20_fuzzy<- glm.nb(health_17_20 ~ workforce_score +
                             pwsid_has_BA + pwsid_medincome  + 
                             pwsid_black + pwsid_asian+ pwsid_hispanic +
                             pop_density_utility + very_large_utility + large_utility + medium_utility + 
                             ground_water + surface_water + owner_private + owner_municipal_district,
                           data = utility_operator_complete_fuzzy)

summary(h_17_20_fuzzy)


h_20_fuzzy <- glm.nb(health_2020 ~ workforce_score +
                          pwsid_has_BA + pwsid_medincome  + 
                          pwsid_black + pwsid_asian+ pwsid_hispanic +
                          pop_density_utility + very_large_utility + large_utility + medium_utility + 
                          ground_water + surface_water + owner_private + owner_municipal_district, 
                        data = utility_operator_complete_fuzzy)

summary(h_20_fuzzy)


# Regression output with fuzzy matching
stargazer(h_20_fuzzy, h_17_20_fuzzy, h_20_23_fuzzy, dep.var.labels=c("Health - 2020","health - 2017-2020","health - 2020-2023" ), 
          covariate.labels=c("Workforce score", "Percent has BA", "Median income",
                             "Percent Black residents", "Percent Asian residents", "Percent Hispanic residents", 
                             "Population density", "Very large utility", "Large utility", "Medium utility", 
                             "Ground water", "Surface water", "Private utility", "Special district utility"),
          style = "all",
          notes = "Underlying data was created with the fuzzy matching procedure discussed in the appendix of the paper.",
          type="html",  out="appendix_table_C1.htm")


# Table D1. Negative binomial predicting health and monitoring 
# and reporting violations in 2020 
h_20_direct <- glm.nb(health_2020 ~ workforce_score + 
                        pwsid_has_BA + pwsid_medincome  + 
                        pwsid_black + pwsid_asian+ pwsid_hispanic +
                        pop_density_utility + very_large_utility + large_utility + medium_utility + 
                        ground_water + surface_water + owner_private + owner_municipal_district,
                      data = utility_operator_complete_direct)

summary(h_20_direct)



mr_20_direct <- glm.nb(mr_2020 ~ workforce_score +
                         pwsid_has_BA + pwsid_medincome  + 
                         pwsid_black + pwsid_asian+ pwsid_hispanic +
                         pop_density_utility + very_large_utility + large_utility + medium_utility + 
                         ground_water + surface_water + owner_private + owner_municipal_district,
                       data = utility_operator_complete_direct)

summary(mr_20_direct)


# Regression output for models into the future 
stargazer(h_20_direct, mr_20_direct, dep.var.labels=c("Health SDWA violations - 2020","Monitoring and reporting SDWA violations - 2020"), 
          covariate.labels=c("Workforce score", "Percent has BA", "Median income",
                             "Percent Black residents", "Percent Asian residents", "Percent Hispanic residents", 
                             "Population density", "Very large utility", "Large utility", "Medium utility", 
                             "Ground water", "Surface water", "Private utility", "Special district utility"),
          style = "all",
          notes = "Underlying data was created with the direct matching procedure discussed in the body of the paper.",
          type="html",  out="appendix_table_D1.htm")

##### Table E1.####
# Average health and monitoring and reporting SDWA violations 
# from 2016 to 2023 

# The values in this table come from the summary of the direct match 
summary(utility_operator_complete_direct)


##### Table E2. ####
# Negative binomial predicting health and monitoring and 
# reporting violations from 2017 to 2020

h_17_20_direct <- glm.nb(health_17_20 ~ workforce_score +
                              pwsid_has_BA + pwsid_medincome  + 
                              pwsid_black + pwsid_asian+ pwsid_hispanic +
                              pop_density_utility + very_large_utility + large_utility + medium_utility + 
                              ground_water + surface_water + owner_private + owner_municipal_district,
                            data = utility_operator_complete_direct)

summary(h_17_20_direct)

mr_17_20_direct <- glm.nb(mr_17_20 ~ workforce_score +
                               pwsid_has_BA + pwsid_medincome  + 
                               pwsid_black + pwsid_asian+ pwsid_hispanic +
                               pop_density_utility + very_large_utility + large_utility + medium_utility + 
                               ground_water + surface_water + owner_private + owner_municipal_district,
                             data = utility_operator_complete_direct)

summary(mr_17_20_direct)

# Regression output with direct matching and years 2017-2020
stargazer(h_17_20_direct, mr_17_20_direct, dep.var.labels=c("Health - 2017-2020","Monitoring and Reporting - 2017-2020"), 
          covariate.labels=c("Workforce score", "Percent has BA", "Median income",
                             "Percent Black residents", "Percent Asian residents", "Percent Hispanic residents", 
                             "Population density", "Very large utility", "Large utility", "Medium utility", 
                             "Ground water", "Surface water", "Private utility", "Special district utility"),
          style = "all",
          notes = "Underlying data was created with the direct matching procedure discussed in the body of the paper.",
          type="html",  out="appendix_table_E2.htm")


##### Table F1. ##### 
# Negative binomial predicting health and monitoring and reporting 
# violations from 2020 to 2023 using population instead of utility class 
# as a measure of utility size 

h_20_23_direct_pop <- glm.nb(health_20_23 ~ workforce_score +
                               pwsid_has_BA + pwsid_medincome  + 
                               pwsid_black + pwsid_asian+ pwsid_hispanic +
                               pop_density_utility + `Population Served` +
                               ground_water + surface_water + owner_private + owner_municipal_district,
                             data = utility_operator_complete_direct)

summary(h_20_23_direct_pop)


mr_20_23_direct_pop <- glm.nb(mr_20_23 ~ workforce_score +
                                pwsid_has_BA + pwsid_medincome  + 
                                pwsid_black + pwsid_asian+ pwsid_hispanic +
                                pop_density_utility + `Population Served` +
                                ground_water + surface_water + owner_private + owner_municipal_district,
                              data = utility_operator_complete_direct)

summary(mr_20_23_direct_pop)

# Regression output for models into the future 
stargazer(h_20_23_direct_pop, mr_20_23_direct_pop, dep.var.labels=c("Health - 2020-2023","Monitoring and Reporting - 2020-2023"), 
          covariate.labels=c("Workforce score", "Percent has BA", "Median income",
                             "Percent Black residents", "Percent Asian residents", "Percent Hispanic residents", 
                             "Population density", "Population served", 
                             "Ground water", "Surface water", "Private utility", "Special district utility"),
          style = "all",
          notes = "Underlying data was created with the direct matching procedure discussed in the body of the paper.",
          type="html",  out="appendix_table_F1.htm")


#### Table G1. ####
# Logistic regression model trends in health and monitoring and 
# reporting violations from 2020 to 2023

h_increase <- glm(health_increase ~ workforce_score +
                    pwsid_has_BA + pwsid_medincome  + 
                    pwsid_black + pwsid_asian+ pwsid_hispanic +
                    pop_density_utility + very_large_utility + large_utility + medium_utility + 
                    ground_water + surface_water + owner_private + owner_municipal_district, family = binomial(link = "logit"),
                  data = utility_operator_complete_direct)

summary(h_increase)


mr_increase <- glm(mr_increase ~ workforce_score +
                     pwsid_has_BA + pwsid_medincome  + 
                     pwsid_black + pwsid_asian+ pwsid_hispanic +
                     pop_density_utility + very_large_utility + large_utility + medium_utility + 
                     ground_water + surface_water + owner_private + owner_municipal_district, family = binomial(link = "logit"),
                   data = utility_operator_complete_direct)

summary(mr_increase)

# Regression output for models into the future 
stargazer(h_increase, mr_increase, dep.var.labels=c("Increasing trend of health SDWA violations from 2020-2023","Increasing trend of monitoring and reporting standards from 2020-2023"), 
          covariate.labels=c("Workforce score", "Percent has BA", "Median income",
                             "Percent Black residents", "Percent Asian residents", "Percent Hispanic residents", 
                             "Population density", "Very large utility", "Large utility", "Medium utility", 
                             "Ground water", "Surface water", "Private utility", "Special district utility"),
          style = "all",
          notes = "Underlying data was created with the direct matching procedure discussed in the body of the paper.",
          type="html",  out="appendix_table_G1.htm")


##### Table H1. ####
# Negative binomial regression model predicting health and 
# monitoring and reporting violations from 2020 to 2023 with surface water and
# workforce interaction 

h_20_23_direct_interact_surface <- glm.nb(health_20_23 ~ workforce_score +
                                            pwsid_has_BA + pwsid_medincome  + 
                                            pwsid_black + pwsid_asian+ pwsid_hispanic +
                                            pop_density_utility + very_large_utility + large_utility + medium_utility + 
                                            ground_water + surface_water + owner_private + owner_municipal_district +
                                            workforce_score*surface_water ,
                                          data = utility_operator_complete_direct)

summary(h_20_23_direct_interact_surface)

mr_20_23_direct_interact_surface <- glm.nb(mr_20_23 ~ workforce_score +
                                             pwsid_has_BA + pwsid_medincome  + 
                                             pwsid_black + pwsid_asian+ pwsid_hispanic +
                                             pop_density_utility + very_large_utility + large_utility + medium_utility + 
                                             ground_water + surface_water + owner_private + owner_municipal_district + 
                                             workforce_score*surface_water,
                                           data = utility_operator_complete_direct)

summary(mr_20_23_direct_interact_surface)

stargazer(h_20_23_direct_interact_surface, mr_20_23_direct_interact_surface, 
          dep.var.labels=c("Health",
                           "Monitoring and Reporting"), 
          covariate.labels=c("Workforce score", "Percent has BA", "Median income",
                             "Percent Black residents", "Percent Asian residents", "Percent Hispanic residents", 
                             "Population density", "Very large utility", "Large utility", "Medium utility", 
                             "Ground water", "Surface water", "Private utility", "Special district utility"),
          style = "all",
          notes = "Underlying data was created with the direct matching procedure discussed in the body of the paper.",
          type="html",  out="appendix_table_H1.htm")

#### Table H2. ####
# Negative binomial regression model predicting health and 
# monitoring and reporting violations from 2020 to 2023 with median income and
# workforce interaction 

h_20_23_direct_interact_income <- glm.nb(health_20_23 ~ workforce_score +
                                           pwsid_has_BA + pwsid_medincome  + 
                                           pwsid_black + pwsid_asian+ pwsid_hispanic +
                                           pop_density_utility + very_large_utility + large_utility + medium_utility + 
                                           ground_water + surface_water + owner_private + owner_municipal_district +
                                           workforce_score*pwsid_medincome ,
                                         data = utility_operator_complete_direct)

summary(h_20_23_direct_interact_income)

mr_20_23_direct_interact_income <- glm.nb(mr_20_23 ~ workforce_score +
                                            pwsid_has_BA + pwsid_medincome  + 
                                            pwsid_black + pwsid_asian+ pwsid_hispanic +
                                            pop_density_utility + very_large_utility + large_utility + medium_utility + 
                                            ground_water + surface_water + owner_private + owner_municipal_district + 
                                            workforce_score*pwsid_medincome,
                                          data = utility_operator_complete_direct)

summary(utility_operator_complete_direct)


stargazer(h_20_23_direct_interact_income, mr_20_23_direct_interact_income, 
          dep.var.labels=c("Health",
                           "Monitoring and Reporting"), 
          covariate.labels=c("Workforce score", "Percent has BA", "Median income",
                             "Percent Black residents", "Percent Asian residents", "Percent Hispanic residents", 
                             "Population density", "Very large utility", "Large utility", "Medium utility", 
                             "Ground water", "Surface water", "Private utility", "Special district utility"),
          style = "all",
          notes = "Underlying data was created with the direct matching procedure discussed in the body of the paper.",
          type="html",  out="appendix_table_H2.htm")


#### Figures #### 

# Figure 1. Texas surface water operator licensing progression 

# Figure 2. SDWIS, ATC, and TCEQ matching procedure overview 

# Figure 3. Predicted SDWA violations from the interaction models 

health_interaction_surface <- plot_model(h_20_23_direct_interact_surface, type = "pred", 
                                         terms = c("workforce_score", "surface_water"),
                                         title = "Predicted Violations by Workforce Score and Water Source",
                                         axis.title = c("Workforce Score", "Predicted SDWA health violations"))

ggsave("health_interaction_surface.png", plot = health_interaction_surface, width = 8, height = 6, dpi = 800)

mr_interaction_surface <- plot_model(mr_20_23_direct_interact_surface, type = "pred", 
                                     terms = c("workforce_score", "surface_water"),
                                     title = "Predicted Violations by Workforce Score and Water Source",
                                     axis.title = c("Workforce Score", "Predicted SDWA monitoring and reporting violations"))

ggsave("mr_interaction_surface.png", plot = mr_interaction_surface, width = 8, height = 6, dpi = 800)

health_interaction_income <- plot_model(h_20_23_direct_interact_income, type = "pred", 
                                        terms = c("workforce_score", "pwsid_medincome"),
                                        title = "Predicted Violations by Workforce Score and Median Income",
                                        axis.title = c("Workforce Score", "Predicted SDWA health violations"))+
  labs(color = "Median income in 1000s")  # Change the legend title

ggsave("health_interaction_income.png", plot = health_interaction_income, width = 8, height = 6, dpi = 800)

mr_interaction_income <- plot_model(mr_20_23_direct_interact_income, type = "pred", 
                                    terms = c("workforce_score", "pwsid_medincome"),
                                    
                                    title = "Predicted Violations by Workforce Score and Median Income",
                                    axis.title = c("Workforce Score", "Predicted SDWA monitoring and reporting violations")) +
  labs(color = "Median income in 1000s")  # Change the legend title

ggsave("mr_interaction_income.png", plot = mr_interaction_income, width = 8, height = 6, dpi = 800)


plots_interact <- grid.arrange(health_interaction_income, mr_interaction_income, health_interaction_surface, mr_interaction_surface, ncol = 2 )

ggsave("plots_interact_figure_3.png", plot = plots_interact, width = 12.5, height = 12.5, dpi = 1200)


# Figure J1. Correlation Matrix for covariates in Table 2
correlation_plot <- subset(utility_operator_complete_direct, select = c(mr_20_23, health_20_23, workforce_score,
                                                                        pwsid_has_BA, pwsid_medincome, `Population Served`,
                                                                        pwsid_black, pwsid_asian, pwsid_hispanic,
                                                                        pop_density_utility, very_large_utility, large_utility, medium_utility,  
                                                                        ground_water, surface_water, owner_private, owner_municipal_district))


M = cor(na.omit(correlation_plot))
corrplot(M, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10))
