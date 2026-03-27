# Code to create the tables and figures from 
# "You Have One Job: Political Institutions and Strategic Investments in Human Capital"
# Produced in December, 2025 by Dr. Robin Rose Saywitz 

library("reader")
library("data.table")
library("readxl")
library("plyr")
library("dplyr")
library("tidyverse")
library("tidyr")
library("stringr")
library("conflicted")
library("stargazer")
library("MASS")
library("jtools")
library("ggplot2")
library("ggstance")
library("vtable")

#### Descriptive Statistics ####
# The code below creates Table 1 Descriptive statistics of all matched utilities.

descriptive_stats <- read_csv("ARPA data files/descriptive_stats.csv")
combined_complete <- read_csv("ARPA data files/combined_complete.csv")

gpg_sum_stat<- subset(descriptive_stats, speciald ==0)
sd_sum_stat<- subset(descriptive_stats, speciald ==1)

labs <- data.frame(speciald  = 'Special District',
                   per_class_a  = '% Class A Operators', #
                   per_class_b  = '% Class B Operators', #
                   per_class_c  = '% Class C Operators', #
                   per_class_d  = '% Class D Operators', #
                   populationservedcount = 'Population Served', #
                   medianinc = 'Mean Median Income', #
                   bach = '% Holding BA Degree', #
                   urban = '% Population in Urban Area', #
                   groundw = 'Water source - Ground Water', #
                   purchased = 'Water source - Purchased', #
                   class_a_utility = 'Class A Utility', #
                   class_b_utility = 'Class B Utility') #

st(gpg_sum_stat,digits = 4,labels=labs)
st(sd_sum_stat,digits = 4,labels=labs)

t.test(per_class_a ~ speciald, data = combined_complete)
t.test( per_class_b~ speciald, data = combined_complete)
t.test( per_class_c~ speciald, data = combined_complete)
t.test( per_class_d~ speciald, data = combined_complete)
t.test( populationservedcount~ speciald, data = combined_complete)
t.test( medianinc~ speciald, data = combined_complete)
t.test( bach~ speciald, data = combined_complete)
t.test( urban~ speciald, data = combined_complete)
t.test( groundw~ speciald, data = combined_complete)
t.test( purchased~ speciald, data = combined_complete)
t.test( class_a_utility~ speciald, data = combined_complete)
t.test( class_b_utility~ speciald, data = combined_complete)

#### Main Analysis ####
combined_complete_s <- read_csv("ARPA data files/combined_complete_s.csv")

# First the models where the DV is the % of each class of operator
OLS_per_class_a <- lm(per_class_a ~ speciald + medianinc + bach + populationservedcount  + urban  + groundw + purchased + class_a_utility + class_b_utility  ,data = combined_complete_s)
OLS_per_class_b <- lm(per_class_b ~ speciald + medianinc + bach + populationservedcount  + urban  + groundw + purchased + class_a_utility + class_b_utility  ,data = combined_complete_s)
OLS_per_class_c <- lm(per_class_c ~ speciald + medianinc + bach + populationservedcount  + urban  + groundw + purchased + class_a_utility + class_b_utility  ,data = combined_complete_s)
OLS_per_class_d <- lm(per_class_d ~ speciald + medianinc + bach + populationservedcount  + urban  + groundw + purchased + class_a_utility + class_b_utility  ,data = combined_complete_s)

# Table 2 Results for models using % Class Operators as the dependent variable.

stargazer(OLS_per_class_a,OLS_per_class_b,OLS_per_class_c,OLS_per_class_d, type="html",
          style = "ajps",
          add.lines=list(c("AIC", round(AIC(OLS_per_class_a),1), round(AIC(OLS_per_class_b),1),
                           round(AIC(OLS_per_class_c),1),round(AIC(OLS_per_class_d),1))), digits = 2,
          dep.var.labels=c("% Class A Operators", "% Class B Operators","% Class C Operators","% Class D Operators"),
          covariate.labels = c("Special District", "Median Income",
                               "% Bachelor's Degree", "Population served","% Population in Urban Area", "Ground Water", 
                               "Purchased water","Class A Utility", "Class B Utility"),  
          notes = "Note: standard error in parentheses",
          out = "per_regs.html", align=TRUE)

# Figure 1 Effects plot for the models using percent Class A, B, C, and D 
# operators as the dependent variable.
plot_summs(OLS_per_class_a,OLS_per_class_b,OLS_per_class_c,OLS_per_class_d, 
           coefs = c("Special District" = "speciald", "Median Income" = "medianinc",
                     "% Bachelor’s Degree" = "bach", "Population served" = "populationservedcount", 
                     "% Population in Urban Area" = "urban",
                     "Ground Water" = "groundw", "Purchased water" = "purchased",
                     "Class A Utility" = "class_a_utility", "Class B Utility" = "class_b_utility"),
           scale = FALSE, plot.distributions = FALSE, 
           model.names = c("% Class A Operators", "% Class B Operators","% Class C Operators","% Class D Operators"), 
           ci_level = .95, colors = "Qual2",robust = TRUE)

#### Appendix A ####
# Figure A1 Count of operators in each class in the data set from TCEQ (top) 
# and in the matched sample (bottom)

# operators by class in the data for the state of Texas
ops_by_class_tx <- read_csv("ARPA data files/ops_by_class_tx.csv")
ops_by_class_sample <- read_csv("ARPA data files/ops_by_class_sample.csv")

ggplot(data=ops_by_class_tx, aes(x=ABBR_TXT, y=count_class)) +
  geom_bar(stat="identity",color="black", fill="grey") +
  labs(title="Count of Water System Operators by Licensing Level in Texas",x="Water System Operator Licensing Classes", y = "Count of Water System Operators")

ggplot(data=ops_by_class_sample, aes(x=ABBR_TXT, y=count_class)) +
  geom_bar(stat="identity",color="black", fill="grey") +
  labs(title="Count of Water System Operators by Licensing Level in Sample",x="Water System Operator Licensing Classes", y = "Count of Water System Operators")


#### Appendix B ####

figure_A2 <- read_csv("ARPA data files/figure_A2.csv")

# Figure B1: Histogram of the distribution of the number of water utilities
# that operators in the data set from TCEQ are associated with. 
# Base data for this figure is not available because it includes 
# names of water utility operators and all possible identifiers have 
# been removed


ggplot(figure_A2, aes(x=count_utilities)) + 
  geom_histogram(color="black", fill="grey") +
  theme(text = element_text(size = 20, family = "Times New Roman"))   +
  labs(x="Number of Utilities Associated with a Water Systems Operator", y = "Count of Utilities") 


# Table B1 Results for models using % Class Operators as the dependent variable when outliers are removed. 
combined_complete_outlier <- read_csv("ARPA data files/combined_complete_outlier.csv")

a_robust <- lm(per_class_a ~ speciald + medianinc + bach + populationservedcount  + urban  + groundw + purchased + class_a_utility + class_b_utility  ,data = combined_complete_outlier)
b_robust <- lm(per_class_b ~ speciald + medianinc + bach + populationservedcount  + urban  + groundw + purchased + class_a_utility + class_b_utility  ,data = combined_complete_outlier)
c_robust <- lm(per_class_c ~ speciald + medianinc + bach + populationservedcount  + urban  + groundw + purchased + class_a_utility + class_b_utility  ,data = combined_complete_outlier)
d_robust <- lm(per_class_d ~ speciald + medianinc + bach + populationservedcount  + urban  + groundw + purchased + class_a_utility + class_b_utility  ,data = combined_complete_outlier)

stargazer(a_robust,b_robust,c_robust,d_robust, type="html",
          add.lines=list(c("AIC", round(AIC(a_robust),1), round(AIC(b_robust),1),
                           round(AIC(c_robust),1),round(AIC(d_robust),1))), digits = 2,
          dep.var.labels=c("% Class A Operators", "% Class B Operators","% Class C Operators","% Class D Operators"),
          covariate.labels = c("Special District", "Median Income",
                               "% Bachelors Degree", "Population served","% Population in Urban Area", "Ground Water", 
                               "Purchased water","Class A Utility", "Class B Utility"),  
          notes = "Note: standard error in parentheses",
          out = "per_regs_robust.html", align=TRUE)



# Table Table D1 Proportion of matched water utilities by utility type across utility classes
# Cannot be reproduced because the data used to create this table is, under Texas state law, 
# can no longer be shared. You can submit questions about the creation of the table and related 
# data to the corresponding author and depending on the needs of the request completely anonymized
# data may be able to be produced. 

