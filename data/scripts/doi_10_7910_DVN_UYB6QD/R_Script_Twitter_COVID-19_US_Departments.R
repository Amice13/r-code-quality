#-------------------------------------------------------------------------------
# Name:         R_Script_Twitter_COVID-19_US_Departments.R
# Purpose:      Explores the relationships tied to vairance in the change of 
#               us federal executive departments Twitter followers using regression analysis
#
# Author:       Joel Chavez
#
# Created:      06/02/2020
# Updated:      06/16/2020 - JC
#-------------------------------------------------------------------------------

# ---*IMPORTANT*---> 
# Alter the code on line 21 CHANGE DIRECTORY TO "R_CVS_Twitter_COVID-19_US_Departments.csv" 
# FILES obtained from https://dataverse.harvard.edu/dataverse/chavez

# Loading Libaries, if not already installed, install.packages("MASS") ----

library(MASS)

# Reads in CSV data aka "Excel Data" *******REPLACE with actual file path after downloading .CVS data----
twitter_executivedepartments <- read.csv(file = 'C:/Users/chave/Downloads/R_CVS_Twitter_COVID-19_US_Departments.csv', stringsAsFactors = FALSE)

   # Model 1----     
         model1 <- lm(Followers_percentchange ~ Tweets_percentchange, 
                              data=twitter_executivedepartments)
   # Model 2----
         model2 <- lm(Followers_percentchange ~ Tweets_percentchange+Months_on_Twitter, 
                       data=twitter_executivedepartments)
   # Model 3----
         model3 <- lm(Followers_percentchange ~ Tweets_percentchange+Months_on_Twitter+COVID19_services, 
                       data=twitter_executivedepartments)       
#Displays results
summary(model1)   
summary(model2)
summary(model3)
        