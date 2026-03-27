## ------------------------------------------------------------------------------------------------

## Article Title: Tasting and re-labeling meat substitute products can affect consumers’ product evaluations and dietary preferences

## R Script - Results of the field experimental Study 1 (focusing on the effects of vegetarian labels)

## Authors: Lukas Paul Fesenfeld1,2, Nadja Zeiske3, Maiken Maier1*, Maria Rachelle Gallmann1, Ellen Van der Werff 3, Linda Steg3

## Current affiliations: 1 University of Bern/Oeschger Centre for Climate Change Research, Switzerland; 2 ETH Zurich, Switzerland; 3 University of Groningen, Netherlands
## *Corresponding author: Maiken Maier, maiken.maier@unibe.ch, University of Bern, Fabrikstrasse 8, 3012 Bern, Switzerland

## ------------------------------------------------------------------------------------------------

# load data ---------------------------------------------------------------

data1 <- read.csv("study1data.csv", header = TRUE, stringsAsFactors = TRUE)
str(data1) #to check data classes
data1$portrait_BIO <- as.numeric(as.factor(data1$portrait_BIO))
data1$portrait_ALT <- as.numeric(as.factor(data1$portrait_ALT))
data1$portrait_EGO <- as.numeric(as.factor(data1$portrait_EGO))
data1$portrait_HED <- as.numeric(as.factor(data1$portrait_HED))
data1$grocery_SELF <- as.numeric(as.factor(data1$grocery_SELF))
data1$grocery_OTHER <- as.numeric(as.factor(data1$grocery_OTHER))
data1$conshabit_GRAMS <- as.integer(as.factor(data1$conshabit_GRAMS))
data1$meateaterID <- as.numeric(as.factor(data1$meateaterID))
data1$meathealthy <- as.integer(as.factor(data1$meathealthy))
str(data1)
variable.names(data1)

# load libraries ----------------------------------------------------------

library("data.table")
library("rlang")
library(plyr)
library(dplyr)
library(tidyverse)
library(ggpubr)

library("PerformanceAnalytics")

library("gplots")
library("ggplot2")

library(mediation)

# single and non-parametric analyses ---------------------------------------------------------
#to check if same significant result since mostly non-normal distribution

ggdensity(data1$taste, fill = "gray") #skew right
ggqqplot(data1$taste)

shapiro.test(data1$taste) #response variable non-normal distribution
shapiro.test(residuals(object = aov_taste)) #residuals non-normal distribution
kruskal.test(taste~condition, data = data1)
summary(aov(taste~condition, data = data1))

shapiro.test(data1$appetizing) #non-normal distribution
kruskal.test(appetizing~condition, data = data1)
summary(aov(appetizing~condition, data = data1))

shapiro.test(data1$texture) #non-normal distribution
kruskal.test(texture~condition, data = data1) 
summary(aov(texture~condition, data = data1))

shapiro.test(data1$eval_valence) #non-normal distribution
kruskal.test(eval_valence~condition, data = data1)
summary(aov(eval_valence~condition, data = data1))

shapiro.test(data1$eval_rating) #non-normal distribution
kruskal.test(eval_rating~condition, data = data1) 
summary(aov(eval_rating~condition, data = data1)) 

shapiro.test(data1$canteen) #non-normal distribution
kruskal.test(canteen~condition, data = data1) 
summary(aov(canteen~condition, data = data1))

shapiro.test(data1$healthy) #non-normal distribution
kruskal.test(healthy~condition, data = data1) # significant **
summary(aov(healthy~condition, data = data1)) # significant **

shapiro.test(data1$animal) #non-normal distribution
kruskal.test(animal~condition, data = data1) # significant ***
summary(aov(animal~condition, data = data1)) # significant ***

shapiro.test(data1$environment) #non-normal distribution
kruskal.test(environment~condition, data = data1) # significant ***
summary(aov(environment~condition, data = data1)) # significant ***

shapiro.test(data1$climate) #non-normal distribution
kruskal.test(climate~condition, data = data1) # significant ***
summary(aov(climate~condition, data = data1)) # significant ***

shapiro.test(data1$consume_like) #non-normal distribution
kruskal.test(consume_like~condition, data = data1) 
summary(aov(consume_like~condition, data = data1)) 

shapiro.test(data1$reduce_like) #non-normal distribution
kruskal.test(reduce_like~condition, data = data1) 
summary(aov(reduce_like~condition, data = data1))

shapiro.test(data1$consume_will) #non-normal distribution
kruskal.test(consume_will~condition, data = data1) 
summary(aov(consume_will~condition, data = data1)) #quite different but still not sig

shapiro.test(data1$reduce_will) #non-normal distribution
kruskal.test(reduce_will~condition, data = data1) 
summary(aov(reduce_will~condition, data = data1))

shapiro.test(data1$replace1) #non-normal distribution
kruskal.test(replace1~condition, data = data1) 
summary(aov(replace1~condition, data = data1))

shapiro.test(data1$replace2) #non-normal distribution
kruskal.test(replace2~condition, data = data1) 
summary(aov(replace2~condition, data = data1))

shapiro.test(data1$replace3) #non-normal distribution
kruskal.test(replace3~condition, data = data1) 
summary(aov(replace3~condition, data = data1))

## aov results are valid as non-parametric tests give results to a similar significance level

# Summary statistics ------------------------------------------------------
install.packages("pastecs")
library(pastecs)

descstat_data1_sublabel <- stat.desc(data1[1:41,14:30])
descstat_data1_meatlabel <- stat.desc(data1[42:83,14:30])

library("xlsx")
write.xlsx(descstat_data1_meatlabel, file = "veggie_descstat.xlsx", sheetName = "descstat_data1_meatlabel", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(descstat_data1_sublabel, file = "veggie_descstat.xlsx", sheetName = "descstat_data1_sublabel", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

# MANOVA analysis ----------------------------------------------------------------

##TASTE##
maov_taste <- aov(taste ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                    grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_taste)

##APPETIZING##
maov_appetizing <- aov(appetizing ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                    grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_appetizing)  
#product *
#portrait_ALT **

##TEXTURE##
maov_texture <- aov(texture ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                    grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_texture) 

##EVAL VALENCE##
maov_valence <- aov(eval_valence ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                      grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_valence) 
#portrait_ALT *

##EVAL RATING##
maov_rating <- aov(eval_rating ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                      grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_rating) 

##CANTEEN##
maov_canteen <- aov(canteen ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                     grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_canteen) 
#portrait_BIO **
#conshabit_GRAMS **

##CONSUME##
maov_consume_will <- aov(consume_will ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                     grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_consume_will) 

maov_consume_like <- aov(consume_like ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                           grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_consume_like) 
#product *

##REDUCE##
maov_reduce_will <- aov(reduce_will ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                           grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_reduce_will) 
#gender **
#portrait_BIO **
#conshabit_GRAMS *
#meateaterID ***

maov_reduce_like <- aov(consume_like ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                           grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_reduce_like) 
#product *

##REPLACEG##
maov_replace1 <- aov(replace1 ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                     grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_replace1) 

maov_replace2 <- aov(replace1 ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                       grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_replace2) 

maov_replace3 <- aov(replace1 ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                       grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_replace3) 

##HEALTHY##
maov_healthy <- aov(healthy ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                       grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_healthy) 
#condition **
#language *
#gender *
#grocery_SELF *

##ANIMAL-FRIENDLY##
maov_animal <- aov(animal ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                      grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_animal) 
#condition ***

##ENVIRONMENT-FRIENDLY##
maov_environment <- aov(environment ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                     grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_environment)
#condition ***
#language *

##CLIMATE-FRIENDLY##
maov_climate <- aov(climate ~ condition + language + gender + product + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + 
                     grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
summary(maov_climate)
#condition ***



# Two-way manova trial ----------------------------------------------------
#with significant covariates from one-way manova
#to find out which covariates interact with each other

##HEALTHY##
maov2_healthy <- aov(healthy ~ condition * language * gender * grocery_SELF, data = data1)
summary(maov2_healthy)


##ENVIRONMENT##
maov2_environment <- aov(environment ~ condition * language, data = data1)
summary(maov2_environment)
#condition:language ***


##check correlation among covariates##
test1 <- data1[,2:13]
test1$product <- as.numeric(as.factor(test1$product))
test1$gender <- as.numeric(as.factor(test1$gender))
test1$language <- as.numeric(as.factor(test1$language))
chart.Correlation(test1, pch= 19)
#some covariates show correlation


# Mediation analysis ----------------------------------------------------------------
##TASTE##
med.fit1 <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit1 <- glm(consume_like ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out1 <- mediate(med.fit1, out.fit1, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out1)

med.fit2 <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit2 <- glm(consume_will ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out2 <- mediate(med.fit2, out.fit2, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out2)

med.fit3 <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit3 <- glm(reduce_like ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out3 <- mediate(med.fit3, out.fit3, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out3)

med.fit4 <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit4 <- glm(reduce_will ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out4 <- mediate(med.fit4, out.fit4, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out4)

med.fit4a <- lm(taste ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit4a <- glm(canteen ~ taste + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out4a <- mediate(med.fit4a, out.fit4a, treat = "condition", mediator = "taste",robustSE = TRUE, sims = 1000)
summary(med.out4a)

##APPETIZING##
med.fit5 <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit5 <- glm(consume_like ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out5 <- mediate(med.fit5, out.fit5, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out5)

med.fit6 <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit6 <- glm(consume_will ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out6 <- mediate(med.fit6, out.fit6, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out6)

med.fit7 <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit7 <- glm(reduce_like ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out7 <- mediate(med.fit7, out.fit7, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out7)

med.fit8 <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit8 <- glm(reduce_will ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out8 <- mediate(med.fit8, out.fit8, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out8)

med.fit8a <- lm(appetizing ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit8a <- glm(canteen ~ appetizing + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out8a <- mediate(med.fit8a, out.fit8a, treat = "condition", mediator = "appetizing",robustSE = TRUE, sims = 1000)
summary(med.out8a)

##TEXTURE##
med.fit9 <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit9 <- glm(consume_like ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out9 <- mediate(med.fit9, out.fit9, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out9)

med.fit10 <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit10 <- glm(consume_will ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out10 <- mediate(med.fit10, out.fit10, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out10)

med.fit11 <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit11 <- glm(reduce_like ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out11 <- mediate(med.fit11, out.fit11, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out11)

med.fit12 <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit12 <- glm(reduce_will ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out12 <- mediate(med.fit12, out.fit12, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out12)

med.fit12a <- lm(texture ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit12a <- glm(canteen ~ texture + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out12a <- mediate(med.fit12a, out.fit12a, treat = "condition", mediator = "texture",robustSE = TRUE, sims = 1000)
summary(med.out12a)

##VALENCE##
med.fit13 <- lm(eval_valence ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit13 <- glm(consume_like ~ eval_valence + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED +grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out13 <- mediate(med.fit13, out.fit13, treat = "condition", mediator = "eval_valence",robustSE = TRUE, sims = 1000)
summary(med.out13)

med.fit14 <- lm(eval_valence ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit14 <- glm(consume_will ~ eval_valence + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out14 <- mediate(med.fit14, out.fit14, treat = "condition", mediator = "eval_valence",robustSE = TRUE, sims = 1000)
summary(med.out14)

med.fit15 <- lm(eval_valence ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit15 <- glm(reduce_like ~ eval_valence + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out15 <- mediate(med.fit15, out.fit15, treat = "condition", mediator = "eval_valence",robustSE = TRUE, sims = 1000)
summary(med.out15)

med.fit16 <- lm(eval_valence ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit16 <- glm(reduce_will ~ eval_valence + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out16 <- mediate(med.fit16, out.fit16, treat = "condition", mediator = "eval_valence",robustSE = TRUE, sims = 1000)
summary(med.out16)

med.fit16a <- lm(eval_valence ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit16a <- glm(canteen ~ eval_valence + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out16a <- mediate(med.fit16a, out.fit16a, treat = "condition", mediator = "eval_valence",robustSE = TRUE, sims = 1000)
summary(med.out16a)

##ENVIRONMENT##
med.fit17 <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit17 <- glm(consume_like ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out17 <- mediate(med.fit17, out.fit17, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out17)

med.fit18 <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit18 <- glm(consume_will ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out18 <- mediate(med.fit18, out.fit18, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out18)

med.fit19 <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit19 <- glm(reduce_like ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out19 <- mediate(med.fit19, out.fit19, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out19)

med.fit20 <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit20 <- glm(reduce_will ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out20 <- mediate(med.fit20, out.fit20, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out20)

med.fit20a <- lm(environment ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit20a <- glm(canteen ~ environment + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out20a <- mediate(med.fit20a, out.fit20a, treat = "condition", mediator = "environment",robustSE = TRUE, sims = 1000)
summary(med.out20a)

##CLIMATE##
med.fit21 <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit21 <- glm(consume_like ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out21 <- mediate(med.fit21, out.fit21, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out21)

med.fit22 <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit22 <- glm(consume_will ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out22 <- mediate(med.fit22, out.fit22, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out22)

med.fit23 <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit23 <- glm(reduce_like ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out23 <- mediate(med.fit23, out.fit23, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out23)

med.fit24 <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit24 <- glm(reduce_will ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out24 <- mediate(med.fit24, out.fit24, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out24)

med.fit24a <- lm(climate ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit24a <- glm(canteen ~ climate + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out24a <- mediate(med.fit24a, out.fit24a, treat = "condition", mediator = "climate",robustSE = TRUE, sims = 1000)
summary(med.out24a)

##HEALTHY##
med.fit25 <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit25 <- glm(consume_like ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out25 <- mediate(med.fit25, out.fit25, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out25)

med.fit26 <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit26 <- glm(consume_will ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out26 <- mediate(med.fit26, out.fit26, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out26)

med.fit27 <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit27 <- glm(reduce_like ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out27 <- mediate(med.fit27, out.fit27, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out27)

med.fit28 <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit28 <- glm(reduce_will ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out28 <- mediate(med.fit28, out.fit28, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out28)

med.fit28a <- lm(healthy ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit28a <- glm(canteen ~ healthy + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out28a <- mediate(med.fit28a, out.fit28a, treat = "condition", mediator = "healthy",robustSE = TRUE, sims = 1000)
summary(med.out28a)

##ANIMAL##
med.fit29 <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit29 <- glm(consume_like ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out29 <- mediate(med.fit29, out.fit29, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out29)

med.fit30 <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit30 <- glm(consume_will ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out30 <- mediate(med.fit30, out.fit30, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out30)

med.fit31 <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit31 <- glm(reduce_like ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out31 <- mediate(med.fit31, out.fit31, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out31)

med.fit32 <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit32 <- glm(reduce_will ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out32 <- mediate(med.fit32, out.fit32, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out32)

med.fit32a <- lm(animal ~ condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
out.fit32a <- glm(canteen ~ animal + condition + gender + portrait_BIO + portrait_ALT + portrait_EGO + portrait_HED + grocery_SELF + grocery_OTHER + conshabit_GRAMS + meateaterID + meathealthy, data = data1)
med.out32a <- mediate(med.fit32a, out.fit32a, treat = "condition", mediator = "animal",robustSE = TRUE, sims = 1000)
summary(med.out32a)


# plotting ----------------------------------------------------------------

#change panel titles
head(data1)
names(data1) [14:30] <- c("Taste","Appetizing","Texture", "Valence", "Evaluation", "canteen share", "to consume meat substitutes",
                          "Consume meat substitutes", "to reduce meat consumption", "Reduce meat consumption", "Replace with synthetic meat",
                          "Replace with plant-based meat", "Replace with insect-based meat","Healthiness", "Animal-friendliness", "Environmental-friendliness", "Climate-friendliness") 

#plot of product eval variables
ggerrorplot(data1, x = "condition", y = c("Taste", "Appetizing", "Texture", "Valence"), combine = TRUE, title = "Evaluation of the product tasted",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(4.5,6), xlab = FALSE, ylab = "ave. rating"
            ) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 5.8)
#plot of likelihood
ggerrorplot(data1, x = "condition", y = c("Consume meat substitutes", "Reduce meat consumption"), combine = TRUE, title = "Likelihood to change food consumption behavior",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(4,5.5), xlab = FALSE, ylab = "ave. rating"
            ) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 5.3)
#plot of attributes
ggerrorplot(data1, x = "condition", y = c("Healthiness", "Animal-friendliness", "Environmental-friendliness", "Climate-friendliness"), combine = TRUE, title = "Attributes associated with the product",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(2,6), xlab = FALSE, ylab = "ave. rating"
            ) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 5.7)
#plot of canteen question
ggerrorplot(data1, x = "condition", y = "canteen share", title = "Desired minimum share of vegetarian \nproducts in canteen",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(40,55),xlab = FALSE, ylab = "%"
            ) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.4, label.y = 53)
#plot of willingness (maybe)
ggerrorplot(data1, x = "condition", y = c("Replace with synthetic meat", "Replace with plant-based meat", "Replace with insect-based meat"), combine = TRUE, title = "Willingness to replace meat",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(2.6,5.8), xlab = FALSE, ylab = "ave. rating"
            ) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.3, label.y = 5.6)

names(data1) [20:23] <- c("Consume meat substitutes","Consume meat substitutes_L", "Reduce meat consumption", "Reduce meat consumption_L") 
#plot of willingness to replace (maybe)
ggerrorplot(data1, x = "condition", y = c("Consume meat substitutes", "Reduce meat consumption"), combine = TRUE, title = "Willingness to change food consumption behavior",
            add = "mean", desc_stat = "mean_se", error.plot ="errorbar", ylim = c(4.5,6), xlab = FALSE, ylab = "ave. rating"
            ) + stat_compare_means(label = "p.format", method = "anova", label.x = 1.3, label.y = 5.8)


# Export tables -----------------------------------------------------------

#simple text file of output
capture.output(summary(maov_taste),file="maov_taste.txt")
capture.output(summary(maov_appetizing),file="maov_appetizing.txt")
capture.output(summary(maov_texture),file="maov_texture.txt")
capture.output(summary(maov_valence),file="maov_valence.txt")
capture.output(summary(maov_canteen),file="canteen.txt")
capture.output(summary(maov_consume_will),file="consume_will.txt")
capture.output(summary(maov_consume_like),file="consume_like.txt")
capture.output(summary(maov_reduce_will),file="reduce_will.txt")
capture.output(summary(maov_reduce_like),file="reduce_like.txt")
capture.output(summary(maov_replace1),file="replace_synth.txt")
capture.output(summary(maov_replace2),file="replace_plant.txt")
capture.output(summary(maov_replace3),file="replace_insect.txt")
capture.output(summary(maov_healthy),file="healthy.txt")
capture.output(summary(maov_animal),file="animal.txt")
capture.output(summary(maov_environment),file="environment.txt")
capture.output(summary(maov_climate),file="climate.txt")


# Descriptive analysis -----------------------------------------------------------
summary(data1)
summary(data1$gender)


# appendix creation -------------------------------------------------------

rn <- c("condition","language","gender","product", "portrait_BIO","portrait_ALT","portrait_EGO",
        "portrait_HED","grocery_SELF", "grocery_OTHER", "conshabit_GRAMS", "meateaterID", "meathealthy", "Residuals")

a <- summary(maov_taste)[[1]][["Pr(>F)"]]
b <- summary(maov_appetizing)[[1]][["Pr(>F)"]]
c <- summary(maov_texture)[[1]][["Pr(>F)"]]
d <- summary(maov_valence)[[1]][["Pr(>F)"]]
eval <- cbind(rn,a,b,c,d)

e <- summary(maov_healthy)[[1]][["Pr(>F)"]]
f <- summary(maov_animal)[[1]][["Pr(>F)"]]
g <- summary(maov_environment)[[1]][["Pr(>F)"]]
h <- summary(maov_climate)[[1]][["Pr(>F)"]]
attribs <- cbind(rn,e,f,g,h)

i <- summary(maov_replace1)[[1]][["Pr(>F)"]]
j <- summary(maov_replace2)[[1]][["Pr(>F)"]]
k <- summary(maov_replace3)[[1]][["Pr(>F)"]]
willrep <- cbind(rn,i,j,k)

l <- summary(maov_consume_will)[[1]][["Pr(>F)"]]
m <- summary(maov_consume_like)[[1]][["Pr(>F)"]]
n <- summary(maov_reduce_will)[[1]][["Pr(>F)"]]
o <- summary(maov_reduce_like)[[1]][["Pr(>F)"]]
willnlike <- cbind(rn,l,m,n,o)

p <- summary(maov_canteen)[[1]][["Pr(>F)"]]
canteen <- cbind(rn, p)

write.xlsx(eval, file = "maovtable.xlsx", sheetName = "eval", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(attribs, file = "maovtable.xlsx", sheetName = "attribs", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(willrep, file = "maovtable.xlsx", sheetName = "willrep", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(willnlike, file = "maovtable.xlsx", sheetName = "willnlike", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
write.xlsx(canteen, file = "maovtable.xlsx", sheetName = "canteen", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
