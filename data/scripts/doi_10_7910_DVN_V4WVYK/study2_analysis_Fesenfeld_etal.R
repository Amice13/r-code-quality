## ------------------------------------------------------------------------------------------------

## Article Title: Tasting and re-labeling meat substitute products can affect consumers’ product evaluations and dietary preferences

## R Script - Results of the field experimental Study 2 (focusing on the effects of individuals' tasting experience)

## Authors: Lukas Paul Fesenfeld1,2, Nadja Zeiske3, Maiken Maier1*, Maria Rachelle Gallmann1, Ellen Van der Werff 3, Linda Steg3

## Current affiliations: 1 University of Bern/Oeschger Centre for Climate Change Research, Switzerland; 2 ETH Zurich, Switzerland; 3 University of Groningen, Netherlands
## *Corresponding author: Maiken Maier, maiken.maier@unibe.ch, University of Bern, Fabrikstrasse 8, 3012 Bern, Switzerland

## ------------------------------------------------------------------------------------------------

# load data ---------------------------------------------------------------

data2 <- read.csv("study2data.csv", header = TRUE, stringsAsFactors = TRUE)
str(data2) #to check data classes
data2$gender <- as.factor(as.integer(data2$gender))
data2 <- data2[-c(17:18)] #remove canteen 1 and product_eval
data2 <- data2[-c(25:27)]
variable.names(data2)

# load libraries ----------------------------------------------------------

library("data.table")
library("rlang")
library("ggplot2")
library(plyr)
library(dplyr)
library(tidyverse) 
library(ggpubr)

library("PerformanceAnalytics")
library("gplots")

library(mediation)

# single and non-parametric analyses ---------------------------------------------------------
#to check if same significant result since mostly non-normal distribution

ggdensity(data2$taste, fill = "gray") #skew right
ggqqplot(data2$taste)
shapiro.test(data2$taste) #response variable non-normal distribution
shapiro.test(residuals(object = aov_taste)) #residuals non-normal distribution
kruskal.test(taste~condition, data = data2)
summary(aov(taste~condition, data = data2))# significant ***

shapiro.test(data2$appetizing) #non-normal distribution
kruskal.test(appetizing~condition, data = data2) # significant *
summary(aov(appetizing~condition, data = data2)) # significant *

shapiro.test(data2$texture) #non-normal distribution
kruskal.test(texture~condition, data = data2) # significant ***
summary(aov(texture~condition, data = data2)) # significant ***

shapiro.test(data2$attitude) #non-normal distribution
kruskal.test(attitude~condition, data = data2) # significant *
summary(aov(attitude~condition, data = data2)) # significant *

shapiro.test(data2$eval_rating) #non-normal distribution
kruskal.test(eval_rating~condition, data = data2) # significant **
summary(aov(eval_rating~condition, data = data2)) # significant **

shapiro.test(data2$canteen2) #non-normal distribution
kruskal.test(canteen2~condition, data = data2) 
summary(aov(canteen2~condition, data = data2)) 

shapiro.test(data2$healthy) #non-normal distribution
kruskal.test(healthy~condition, data = data2) # significant *
summary(aov(healthy~condition, data = data2)) # significant *

shapiro.test(data2$animal) #non-normal distribution
kruskal.test(animal~condition, data = data2) 
summary(aov(animal~condition, data = data2)) 

shapiro.test(data2$environment) #non-normal distribution
kruskal.test(environment~condition, data = data2)
summary(aov(environment~condition, data = data2))

shapiro.test(data2$climate) #non-normal distribution
kruskal.test(climate~condition, data = data2)
summary(aov(climate~condition, data = data2))

shapiro.test(data2$consume_like) #non-normal distribution
kruskal.test(consume_like~condition, data = data2) 
summary(aov(consume_like~condition, data = data2)) 

shapiro.test(data2$reduce_like) #non-normal distribution
kruskal.test(reduce_like~condition, data = data2) 
summary(aov(reduce_like~condition, data = data2))

shapiro.test(data2$consume_will) #non-normal distribution
kruskal.test(consume_will~condition, data = data2) 
summary(aov(consume_will~condition, data = data2)) #almost

shapiro.test(data2$reduce_will) #non-normal distribution
kruskal.test(reduce_will~condition, data = data2) 
summary(aov(reduce_will~condition, data = data2))

shapiro.test(data2$replace1_will) #non-normal distribution
kruskal.test(replace1_will~condition, data = data2) 
summary(aov(replace1_will~condition, data = data2))

shapiro.test(data2$replace2_will) #non-normal distribution
kruskal.test(replace2_will~condition, data = data2) 
summary(aov(replace2_will~condition, data = data2))

shapiro.test(data2$replace3_will) #non-normal distribution
kruskal.test(replace3_will~condition, data = data2) 
summary(aov(replace3_will~condition, data = data2))

## aov results are valid as non-parametric tests give results to a similar significance level


# Summary statistics ------------------------------------------------------
library(pastecs)
library("xlsx")

descstat_data2_tasting <- stat.desc(data2[1:71,13:28])
descstat_data2_control <- stat.desc(data2[72:142,13:28])

write.xlsx(descstat_data2_control, file = "veggie_descstat.xlsx", sheetName = "descstat_data2_control", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(descstat_data2_tasting, file = "veggie_descstat.xlsx", sheetName = "descstat_data2_tasting", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

# MANOVAS ----------------------------------------------------------------

##TASTE##
maov_taste <- aov(taste ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
             grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_taste) 
#condition ***
#portrait_ALT **
#conshabit_GRAMS **
#meateaterID *

##APPETIZING##
maov_appetizing <- aov(appetizing ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                     grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_appetizing) 
#condition *
#conshabit_GRAMS **

##TEXTURE##
maov_texture <- aov(texture ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                         grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_texture)
#condition ***
#portrait_BIO *
#grocery_SELF *
#conshabit_GRAMS ***
#meateaterID *

##ATTITUDE##
maov_attitude <- aov(attitude ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                      grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_attitude)
#condition **
#portrait_BIO *
#conshabit_GRAMS ***
#meateaterID **

##EVAL RATING##
maov_rating <- aov(eval_rating ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                       grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_rating)
#condition **
#conshabit_GRAMS ***
#meateatersID*

##CANTEEN##
maov_canteen1 <- aov(canteen1 ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                     grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_canteen1)

maov_canteen2 <- aov(canteen2 ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                       grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_canteen2)
#conshabit_GRAMS **
#meateaterID ***
#meathhealthy *

##HEALTHY##
maov_healthy <- aov(healthy ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                       grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_healthy)
#condition **
#grocery_SELF *
#conshabit_GRAMS **

##ANIMAL-FRIENDLY##
maov_animal <- aov(animal ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                      grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_animal)

##ENVIRONMENT-FRIENDLY##
maov_environment <- aov(environment ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                     grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_environment)

##CLIMATE-FRIENDLY##
maov_climate <- aov(climate ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                          grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_climate)

##CONSUME##
maov_consume_will <- aov(consume_will ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                          grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_consume_will)
#conshabit ***
#meateaterID **

maov_consume_like <- aov(consume_like ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                           grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_consume_like)
#conshabit *
#meateaterID **

##REDUCE##
maov_reduce_will <- aov(reduce_will ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                      grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_reduce_will)
#portrait_BIO **
#portrait_HED *
#grocery_SELF *
#grocery_OTHER ***
#conshabit ***
#meateaterID ***
#meathealthy *

maov_reduce_like <- aov(reduce_like ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                          grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_reduce_like)
#portrait_BIO **
#portrait_EGO *
#portrait_HED *
#grocery_SELF *
#grocery_OTHER ***
#conshabit ***
#meateaterID ***
#meathealthy *

##REPLACE##
maov_replace1_will <- aov(replace1_will ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                          grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_replace1_will)
#gender *
#portrait_ALT *
#meateaterID *

maov_replace2_will <- aov(replace2_will ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                            grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_replace2_will)
#conshabit *
#meateaterID *

maov_replace3_will <- aov(replace3_will ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                            grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_replace3_will)
#gender ***
#meateaterID **

maov_replace1_like <- aov(replace1_like ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                          grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_replace1_like)

maov_replace2_like <- aov(replace2_like ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                            grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_replace2_like)
#grocery_OTHER **
#conshabit ***
#meateaterID ***
#meathealthy *
summary(maov_replace2_like <- aov(replace2_like ~ condition, data = data2))

maov_replace3_like <- aov(replace3_like ~ condition + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                            grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
summary(maov_replace3_like)
#gender **
#portrait_BIO *



# Two-way manova trial ----------------------------------------------------
#with significant covariates from one-way manova
#to find out which covariates interact with each other

##TASTE##
maov2_taste <- aov(taste ~ condition * portrait_ALT * conshabit_GRAMS * meateaterID, data = data2)
summary(maov2_taste)
#portrait_ALT:meateaterID

##APPETIZING##
maov2_appetizing <- aov(appetizing ~ condition * conshabit_GRAMS, data = data2)
summary(maov2_appetizing)
#condition:conshabit

##TEXTURE##
maov2_texture <- aov(texture ~ condition * portrait_BIO * grocery_SELF * conshabit_GRAMS * meateaterID, data = data2)
summary(maov2_texture)
#condition:conshabit

##ATTITUDE##
maov2_attitude <- aov(attitude ~ condition * portrait_BIO * conshabit_GRAMS * meateaterID, data = data2)
summary(maov2_attitude)

##EVAL RATING##
maov2_rating <- aov(eval_rating ~ condition * conshabit_GRAMS * meateaterID, data = data2)
summary(maov2_rating)
#condition:conshabit

##HEALTHY##
maov2_healthy <- aov(healthy ~ condition * grocery_SELF * conshabit_GRAMS, data = data2)
summary(maov2_healthy)


##check correlation among covariates##
test2 <- data2[,2:12]
test2$product <- as.numeric(as.factor(test2$product))
test2$gender <- as.numeric(as.factor(test2$gender))
chart.Correlation(test2, pch= 19)
#some covariates show correlation


# Mediation analysis ----------------------------------------------------------------
##TASTE##
med.fit1 <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit1 <- glm(consume_like ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out1 <- mediate(med.fit1, out.fit1, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out1)

med.fit2 <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit2 <- glm(consume_will ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out2 <- mediate(med.fit2, out.fit2, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out2)

med.fit3 <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit3 <- glm(reduce_like ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out3 <- mediate(med.fit3, out.fit3, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out3)

med.fit4 <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit4 <- glm(reduce_will ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out4 <- mediate(med.fit4, out.fit4, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out4)

med.fit4a <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit4a <- glm(canteen2 ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out4a <- mediate(med.fit4a, out.fit4a, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out4a)

##APPETIZING##
med.fit5 <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit5 <- glm(consume_like ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out5 <- mediate(med.fit5, out.fit5, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out5)

med.fit6 <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit6 <- glm(consume_will ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out6 <- mediate(med.fit6, out.fit6, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out6)

med.fit7 <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit7 <- glm(reduce_like ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out7 <- mediate(med.fit7, out.fit7, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out7)

med.fit8 <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit8 <- glm(reduce_will ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out8 <- mediate(med.fit8, out.fit8, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out8)

med.fit8a <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit8a <- glm(canteen2 ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out8a <- mediate(med.fit8a, out.fit8a, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out8a)

##TEXTURE##
med.fit9 <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit9 <- glm(consume_like ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out9 <- mediate(med.fit9, out.fit9, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out9)

med.fit10 <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit10 <- glm(consume_will ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out10 <- mediate(med.fit10, out.fit10, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out10)

med.fit11 <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit11 <- glm(reduce_like ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out11 <- mediate(med.fit11, out.fit11, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out11)

med.fit12 <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit12 <- glm(reduce_will ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out12 <- mediate(med.fit12, out.fit12, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out12)

med.fit12a <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit12a <- glm(canteen2 ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out12a <- mediate(med.fit12a, out.fit12a, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out12a)

##ATTITUDE##
med.fit13 <- lm(attitude ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit13 <- glm(consume_like ~ attitude + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED +grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out13 <- mediate(med.fit13, out.fit13, treat = "condition", mediator = "attitude",robustSE = TRUE, sims = 1000)
summary(med.out13)

med.fit14 <- lm(attitude ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit14 <- glm(consume_will ~ attitude + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out14 <- mediate(med.fit14, out.fit14, treat = "condition", mediator = "attitude",robustSE = TRUE, sims = 1000)
summary(med.out14)

med.fit15 <- lm(attitude ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit15 <- glm(reduce_like ~ attitude + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out15 <- mediate(med.fit15, out.fit15, treat = "condition", mediator = "attitude",robustSE = TRUE, sims = 1000)
summary(med.out15)

med.fit16 <- lm(attitude ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit16 <- glm(reduce_will ~ attitude + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out16 <- mediate(med.fit16, out.fit16, treat = "condition", mediator = "attitude",robustSE = TRUE, sims = 1000)
summary(med.out16)

med.fit16a <- lm(attitude ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit16a <- glm(canteen2 ~ attitude + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out16a <- mediate(med.fit16a, out.fit16a, treat = "condition", mediator = "attitude",robustSE = TRUE, sims = 1000)
summary(med.out16a)

##ENVIRONMENT##
med.fit17 <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit17 <- glm(consume_like ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out17 <- mediate(med.fit17, out.fit17, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out17)

med.fit18 <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit18 <- glm(consume_will ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out18 <- mediate(med.fit18, out.fit18, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out18)

med.fit19 <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit19 <- glm(reduce_like ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out19 <- mediate(med.fit19, out.fit19, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out19)

med.fit20 <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit20 <- glm(reduce_will ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out20 <- mediate(med.fit20, out.fit20, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out20)

med.fit20a <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit20a <- glm(canteen2 ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out20a <- mediate(med.fit20a, out.fit20a, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out20a)

##CLIMATE##
med.fit21 <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit21 <- glm(consume_like ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out21 <- mediate(med.fit21, out.fit21, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out21)

med.fit22 <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit22 <- glm(consume_will ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out22 <- mediate(med.fit22, out.fit22, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out22)

med.fit23 <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit23 <- glm(reduce_like ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out23 <- mediate(med.fit23, out.fit23, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out23)

med.fit24 <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit24 <- glm(reduce_will ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out24 <- mediate(med.fit24, out.fit24, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out24)

med.fit24a <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit24a <- glm(canteen2 ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out24a <- mediate(med.fit24a, out.fit24a, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out24a)

##HEALTHY##
med.fit25 <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit25 <- glm(consume_like ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out25 <- mediate(med.fit25, out.fit25, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out25)

med.fit26 <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit26 <- glm(consume_will ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out26 <- mediate(med.fit26, out.fit26, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out26)

med.fit27 <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit27 <- glm(reduce_like ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out27 <- mediate(med.fit27, out.fit27, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out27)

med.fit28 <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit28 <- glm(reduce_will ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out28 <- mediate(med.fit28, out.fit28, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out28)

med.fit28a <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit28a <- glm(canteen2 ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out28a <- mediate(med.fit28a, out.fit28a, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out28a)

##ANIMAL##
med.fit29 <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit29 <- glm(consume_like ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out29 <- mediate(med.fit29, out.fit29, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out29)

med.fit30 <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit30 <- glm(consume_will ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out30 <- mediate(med.fit30, out.fit30, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out30)

med.fit31 <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit31 <- glm(reduce_like ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out31 <- mediate(med.fit31, out.fit31, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out31)

med.fit32 <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit32 <- glm(reduce_will ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out32 <- mediate(med.fit32, out.fit32, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out32)

med.fit32a <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
out.fit32a <- glm(canteen2 ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data2)
med.out32a <- mediate(med.fit32a, out.fit32a, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out32a)

# Plotting ----------------------------------------------------------------

##plotmeans##

#change panel titles
head(data2)
names(data2) [13:28] <- c("Taste","Appetizing","Texture", "Valence", "canteen share", "consume meat substitutes_w",
                          "Consume meat substitutes", "reduce meat consumption_w", "Reduce meat consumption", "Replace with synthetic meat",
                          "Replace with plant-based meat", "Replace with insect-based meat","Healthiness", "Animal-friendliness", "Environmental-friendliness", "Climate-friendliness") 

#plot of product eval variables
ggerrorplot(data2, x = "condition", y = c("Taste", "Appetizing", "Texture", "Valence"), combine = TRUE, title = "Evaluation of plant-based meat substitutes",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(4,5.8), xlab = FALSE, ylab = "ave. rating"
) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 5.6)
#plot of likelihood
ggerrorplot(data2, x = "condition", y = c("Consume meat substitutes", "Reduce meat consumption"), combine = TRUE, title = "Likelihood to change food consumption behavior",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(3.8,5.3), xlab = FALSE, ylab = "ave. rating"
) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 5.1)
#plot of attributes
ggerrorplot(data2, x = "condition", y = c("Healthiness", "Animal-friendliness", "Environmental-friendliness", "Climate-friendliness"), combine = TRUE, title = "Attributes associated with plant-based meat substitutes",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(4,6.5), xlab = FALSE, ylab = "ave. rating"
) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 5.7)
#plot of canteen question
ggerrorplot(data2, x = "condition", y = "canteen share", title = "Desired minimum share of vegetarian \nproducts in canteen",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(40,55),xlab = FALSE, ylab = "%"
) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 53)
#plot of willingness (maybe)
ggerrorplot(data2, x = "condition", y = c("Replace with synthetic meat", "Replace with plant-based meat", "Replace with insect-based meat"), combine = TRUE, title = "Willingness to replace meat",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(2.6,6), xlab = FALSE, ylab = "ave. rating"
) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.3, label.y = 5.8)

names(data2) [18:21] <- c("Consume meat substitutes","Consume meat substitutes_L", "Reduce meat consumption", "Reduce meat consumption_L") 
#plot of willingness to replace (maybe)
ggerrorplot(data2, x = "condition", y = c("Consume meat substitutes", "Reduce meat consumption"), combine = TRUE, title = "Willingness to change food consumption behavior",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(4.3,5.8), xlab = FALSE, ylab = "ave. rating"
) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 5.6)

# Export tables -----------------------------------------------------------

#simple text file of output
capture.output(summary(maov_taste),file="maov_taste.txt")
capture.output(summary(maov_appetizing),file="maov_appetizing.txt")
capture.output(summary(maov_texture),file="maov_texture.txt")
capture.output(summary(maov_attitude),file="maov_valence.txt")
capture.output(summary(maov_canteen2),file="canteen.txt")
capture.output(summary(maov_consume_will),file="consume_will.txt")
capture.output(summary(maov_consume_like),file="consume_like.txt")
capture.output(summary(maov_reduce_will),file="reduce_will.txt")
capture.output(summary(maov_reduce_like),file="reduce_like.txt")
capture.output(summary(maov_replace1_will),file="replace_synth.txt")
capture.output(summary(maov_replace2_will),file="replace_plant.txt")
capture.output(summary(maov_replace3_will),file="replace_insect.txt")
capture.output(summary(maov_healthy),file="healthy.txt")
capture.output(summary(maov_animal),file="animal.txt")
capture.output(summary(maov_environment),file="environment.txt")
capture.output(summary(maov_climate),file="climate.txt")

# Descriptive analysis -----------------------------------------------------------
summary(data2)
summary(data2$gender)


# Appendix creation -------------------------------------------------------

rn <- c("condition","gender","product", "portrait_BIO","portrait_ALT","portrait_EGO",
        "portrait_HED","grocery_SELF", "grocery_OTHER", "conshabit_GRAMS", "meateaterID", "meathealthy", "Residuals")

a <- summary(maov_taste)[[1]][["Pr(>F)"]]
b <- summary(maov_appetizing)[[1]][["Pr(>F)"]]
c <- summary(maov_texture)[[1]][["Pr(>F)"]]
d <- summary(maov_attitude)[[1]][["Pr(>F)"]]
eval <- cbind(rn,a,b,c,d)

e <- summary(maov_healthy)[[1]][["Pr(>F)"]]
f <- summary(maov_animal)[[1]][["Pr(>F)"]]
g <- summary(maov_environment)[[1]][["Pr(>F)"]]
h <- summary(maov_climate)[[1]][["Pr(>F)"]]
attribs <- cbind(rn,e,f,g,h)

i <- summary(maov_replace1_will)[[1]][["Pr(>F)"]]
j <- summary(maov_replace2_will)[[1]][["Pr(>F)"]]
k <- summary(maov_replace3_will)[[1]][["Pr(>F)"]]
willrep <- cbind(rn,i,j,k)

l <- summary(maov_consume_will)[[1]][["Pr(>F)"]]
m <- summary(maov_consume_like)[[1]][["Pr(>F)"]]
n <- summary(maov_reduce_will)[[1]][["Pr(>F)"]]
o <- summary(maov_reduce_like)[[1]][["Pr(>F)"]]
willnlike <- cbind(rn,l,m,n,o)

p <- summary(maov_canteen2)[[1]][["Pr(>F)"]]
canteen <- cbind(rn, p)

write.xlsx(eval, file = "maovtable.xlsx", sheetName = "eval_study2", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(attribs, file = "maovtable.xlsx", sheetName = "attribs_study22", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(willrep, file = "maovtable.xlsx", sheetName = "willrep_study2", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(willnlike, file = "maovtable.xlsx", sheetName = "willnlike_study2", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(canteen, file = "maovtable.xlsx", sheetName = "canteen_study2", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

