################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendix 9 Part I- Creation of Ideological Indices
################################################################################

################################ SET UP #########################################

rm(list=ls()) #clear global environ


# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()

#packages
library(lavaan)
library(tidyverse)
library(semPlot)

#read in data for each country
arg <- readRDS("arg_clean.rds") #read in argentina data
arg$country <- "argentina" #country variable


brazil <- readRDS("brazil_clean.rds") #read in brazil data
brazil$country <- "brazil" #country variable

#combine
data <- rbind(arg, brazil)

# create victim or close victim variable
data$close_vic <- ifelse(data$vic == 1 | data$vic_fam == 1, 1, 0)


###################### SEM FOR IDEO LATENT VARIABLES ###########################

#function for complete cases in selected variables
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
}

#filter data so only includes complete cases across the following variables
data <- completeFun(data, c("ideo_05b", "ideo_05c", "ideo_05d",
                          "ideo_05e", "ideo_01b", "ideo_01c",
                          "ideo_01d"))

data2 <- data %>% 
  select(id, ideo_05b, ideo_05c, ideo_05d,
         ideo_05e, ideo_01b, ideo_01c,
         ideo_01d) %>%
  unique()



#########  One-factor model ##########
vars_1 <- 'ideo =~ ideo_01b + ideo_01c + ideo_01d + ideo_05b + ideo_05c +
ideo_05d + ideo_05e' 

vars_fit1 <- sem(vars_1, data = data2, check.gradient = FALSE)
#summary(vars_fit, standardized = TRUE)

# Fit measures - Table A9.4
fitMeasures(vars_fit1, c("cfi", "rmsea", "srmr", "chisq", "pvalue"))

#plot - Figure A9.2
#jpeg("semplot1.jpeg", res=600, width=2500, height=1500)
semPaths(vars_fit1, "std",
         title = FALSE, curvePivot = TRUE, rotation = 2,
         edge.color = "black",
         edge.label.cex = 1,
         label.font = 2)
#dev.off()

#########  Two-factor model ##########
vars_2 <- 'economy =~ ideo_01b + ideo_01c + ideo_01d
         social =~ ideo_05b + ideo_05c + ideo_05d + ideo_05e' 


vars_fit2 <- sem(vars_2, data = data2, check.gradient = FALSE)
#summary(vars_fit, standardized = TRUE)

# Fit measures - Table A9.3
fitMeasures(vars_fit2, c("cfi", "rmsea", "srmr", "chisq", "pvalue"))

#plot - Figure A9.1
#jpeg("semplot2.jpeg", res=600, width=2500, height=1500)
semPaths(vars_fit2, "std",
         title = FALSE, curvePivot = TRUE, rotation = 2,
         edge.color = "black",
         edge.label.cex = 1,
         label.font = 2)
#dev.off()


#predicted values
vars_predicted1 <- as.data.frame(lavPredict(vars_fit1))
vars_predicted2 <- as.data.frame(lavPredict(vars_fit2))

#bind these predicted variables to each respondent
df1 <- cbind(data2, vars_predicted1) %>%
  select(id, ideo)

df2 <- cbind(data2, vars_predicted2) %>%
  select(id, social, economy)

merge1 <- merge(df1, df2, by = "id")

final_data <- merge(data, merge1, by = "id")
head(final_data)

obs <- final_data %>%
  select(id) %>%
  unique()


# save new data file to be used in main analysis
#saveRDS(final_data, "laterzo_cps_2023c.RDS")


