### Replication Code for Fluctuations of Immigration Salience/ Journal of Public Policy #################
#Authors: Barbara Gomez-Aguinaga, Jason L. Morin, and Gabriel R. Sanchez 
# Data Used: Collaborative Multi-Racial Post Election Survey (2008, 2012, 2016)
# Data Website: https://cmpsurvey.org
# Created on 2022_09_01




### Load Libararies ###########################################################
library(tidyverse)
library(survey)
library(stargazer)
library(margins)
library(ggplot2)
library(ggpubr)


### Read in DataSets ##########################################################

setwd("Insert Path ")
cmps_2008 <- read_csv("cmps_2008_replication.csv")

cmps_2012 <- read_csv("cmps_2012_replication.csv")

cmps_2016 <- read_csv("cmps_2016_replication.csv")


### TABLE 1: Distribution of Immigration Salience and Top Three Issue ##########

cmps_2008_wt <-svydesign(id=~cmps_id,weights=~wt_lat_nat, data=cmps_2008)
svymean(~economy_important + immigration_important + education_important, design=cmps_2008_wt, na.rm = TRUE)

cmps_2012_wt <-svydesign(id=~caseid,weights=~weight2, data=cmps_2012)
svymean(~immigration_important +economy_important +education_important, design=cmps_2012_wt, na.rm = TRUE)

cmps_2016_wt <-svydesign(id=~respid,weights=~weight, data=cmps_2016)
svymean(~health_important +economy_important + immigration_important, design=cmps_2016_wt, na.rm = TRUE)

### TABLE 2 ####################################################################
# MODELS 1 and 2 USING CMPS 2008 Data 

summary(model_8_1 <- glm(immigration_important ~linked_fate + per_discrimination + spanish_media + 
                           foreign  + catholic + lat_percent_change, 
                         data=cmps_2008, family="binomial", weights=wt_lat_nat)) 

summary(model_8_2 <- glm(immigration_important ~linked_fate + per_discrimination + spanish_media + 
                           foreign  + catholic + lat_percent_change + border_state +partisan_dummy + ideology_strength + 
                           pol_interest + female + age_cat4 + collegeB + mexican + rican + cuban, 
                         data=cmps_2008, family="binomial", weights=wt_lat_nat)) 

#Models 3 and 4 Using CMPS 2012 Data

summary(model_12_1 <- glm(immigration_important  ~ linked_fate  +  per_discrimination + spanish_media +
                            foreign + catholic + lat_percent_change, 
                          data=cmps_2012, family="binomial", weights=weight2))

summary(model_12_2 <- glm(immigration_important  ~ linked_fate  +  per_discrimination + spanish_media +
                            foreign + catholic + lat_percent_change + border_state + partisan_dummy + ideology_strength + pol_interest +
                            female + age_cat4 + collegeB + mexican + rican + cuban,data=cmps_2012, family="binomial", weights=weight2))

# Models 5 and 6 Using CMPS 2016 Data

summary(model_16_1 <- glm(immigration_important ~linked_fate  + per_discrimination  + spanish_media  + 
                            foreign + catholic + lat_percent_change, 
                          data=cmps_2016, family="binomial", weights = weight)) 

summary(model_16_2 <- glm(immigration_important ~linked_fate + per_discrimination  + spanish_media  + 
                            foreign + catholic + lat_percent_change  + border_state + partisan_dummy + ideology_strength + pol_interest +
                            female + age_cat4 + collegeB + mexican +  rican + cuban, 
                          data=cmps_2016, family="binomial", weights = weight)) 


#Create Table 2

table_2_labels <- c ("Linked Fate", "Perceived Discrimination","Spanish Media",
                    "Foreign Born", "Catholic", "Percent Change Latino Pop.", 
                    "U.S.-Mexico Border", "Partisan", "Ideological Strength",
                    "Political Interest", "Female", "Age",  "College Education", 
                    "Mexican", "Puerto Rican", "Cuban" )


stargazer( model_8_1, model_8_2, model_12_1, model_12_2, model_16_1, model_16_2,
           model.numbers=TRUE,
           out.header=FALSE,
           covariate.labels = table_2_labels,
           dep.var.labels = c("Immigration Salience"),
           font.size = "footnotesize",
           column.sep.width = "2pt",
           omit.stat = c("ser","f"),
           title = "Logit Results. Perceptions of Immigration Salience among Latino Likely Voters"
)

### Figure 1 ##################################################################

#Create Marginal Effects
summary(margins(model_8_2))
x <- margins(model_8_2)
x <- summary(x)

#REORDER MARGINS ACCORDING TO SUBSTANTIVE EFFECT
x$factor <- factor(x$factor, levels = x$factor[order(x$AME)])
summary(x$factor)

#Create Plot 
x_plot <- ggplot(data=x) + geom_point(aes(factor, AME) ) + 
  geom_errorbar(aes(x=factor, ymin=lower, ymax=upper,width=0)) + 
  geom_hline(yintercept =0) + 
  labs(y="", x="") +
  scale_y_continuous(limits=c(-.25, .25)) + 
  scale_x_discrete(labels = c("Catholic", "Cuban",  "Age", "Puerto Rican","Ideological Strength",
                              "Political Interest",  "Mexican", "Linked Fate", "% Change Latino Pop.", "Partisan", "U.S.-Mexico Border", "Female", "College Education",
                              "Per. Discrimination", "Spanish Media" , "Foreign Born")) + rotate()

print(x_plot)


### Figure 2 ##################################################################
#Create Marginal Effects
summary(margins(model_12_2))
y <- margins(model_12_2)
y <- summary(y)

#REORDER MARGINS ACCORDING TO SUBSTANTIVE EFFECT
y$factor <- factor(y$factor, levels = y$factor[order(y$AME)])
summary(y$factor)

#Create Plot 
y_plot <- ggplot(data=y) + geom_point(aes(factor, AME) ) + 
  geom_errorbar(aes(x=factor, ymin=lower, ymax=upper,width=0)) + 
  geom_hline(yintercept =0) + 
  labs(y="", x="") +
  scale_y_continuous(limits=c(-.3, .3)) + 
  scale_x_discrete(labels =c("Puerto Rican", "U.S.-Mexico Border", "Female", "Cuban", 
                             "Ideological Strength", "Catholic",  "% Change Latino Pop.", "Partisan", "Political Interest", "Linked Fate", "College Education", 
                             "Mexican", "Age",  "Spanish Media", "Foreign Born", "Per. Discrimination")) + rotate()

print(y_plot)

### Figure 3 ##################################################################
#Create Marginal Effects
summary(margins(model_16_2))
z <- margins(model_16_2)
z <-summary(z)


#REORDER MARGINS ACCORDING TO SUBSTANTIVE EFFECT
z$factor <- factor(z$factor, levels = z$factor[order(z$AME)])
summary(z$factor)

#Create Plot
z_plot <- ggplot(data=z) + geom_point(aes(factor, AME) ) + 
  geom_errorbar(aes(x=factor, ymin=lower, ymax=upper,width=0)) + 
  geom_hline(yintercept =0) +
  xlab("")+
  ylab("") +
  scale_y_continuous(limits=c(-.25, .25)) + 
  scale_x_discrete(labels = c("Puerto Rican", "College Education", "Partisan", "Female",
                              "Cuban", "U.S.-Mexico Border", "Per. Discrimination", "% Change Latino Pop.",
                              "Catholic", "Political Interest", "Linked Fate", "Age", "Ideological Strength", "Mexican", 
                              "Foreign Born", "Spanish Media")) +  rotate()

print(z_plot)


###Appendix A: Summary Statistics ############################################
#2008 CMPS
cmps_2008_wt <-svydesign(id=~cmps_id,weights=~wt_lat_nat, data=cmps_2008)

svymean(~linked_fate + per_discrimination + spanish_media + foreign  + catholic + lat_percent_change + border_state +partisan_dummy + ideology_strength + 
          pol_interest + female + age_cat4 + collegeB + mexican + rican + cuban, design=cmps_2008_wt, na.rm = TRUE)

var_2008 <-svyvar(~linked_fate + per_discrimination + spanish_media + foreign  + catholic + lat_percent_change + border_state +partisan_dummy + ideology_strength + 
                    pol_interest + female + age_cat4 + collegeB + mexican + rican + cuban, design=cmps_2008_wt, na.rm = TRUE)

sqrt(var_2008)


#2012 CMPS 

cmps_2012_wt <-svydesign(id=~caseid ,weights=~weight2, data=cmps_2012)

svymean(~linked_fate + per_discrimination + spanish_media + foreign  + catholic + lat_percent_change + border_state +partisan_dummy + ideology_strength + 
          pol_interest + female + age_cat4 + collegeB + mexican + rican + cuban, design=cmps_2012_wt, na.rm = TRUE)

var_2012 <-svyvar(~linked_fate + per_discrimination + spanish_media + foreign  + catholic + lat_percent_change + border_state +partisan_dummy + ideology_strength + 
                    pol_interest + female + age_cat4 + collegeB + mexican + rican + cuban, design=cmps_2012_wt, na.rm = TRUE)

sqrt(var_2012)


#2016 CMPS 

cmps_2016_wt <-svydesign(id=~respid ,weights=~weight, data=cmps_2016)

svymean(~linked_fate + per_discrimination + spanish_media + foreign  + catholic + lat_percent_change + border_state +partisan_dummy + ideology_strength + 
          pol_interest + female + age_cat4 + collegeB + mexican + rican + cuban, design=cmps_2016_wt, na.rm = TRUE)

var_2016 <-svyvar(~linked_fate + per_discrimination + spanish_media + foreign  + catholic + lat_percent_change + border_state +partisan_dummy + ideology_strength + 
                    pol_interest + female + age_cat4 + collegeB + mexican + rican + cuban, design=cmps_2016_wt, na.rm =TRUE)

sqrt(var_2016)










