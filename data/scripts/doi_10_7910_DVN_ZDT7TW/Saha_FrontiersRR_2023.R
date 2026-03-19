rm(list=ls())
set.seed(1234)


library(sjmisc)
library(dplyr)
library(lavaan)
library(ggplot2)
library(stargazer)
library(sjPlot)
library(lubridate)
library(psych)
library(effects)
library(tidyverse)
library(forcats)
library(interactions)
library(mediation)
library(rosetta)
library(cregg)
library(stargazer)
library(FindIt)
library(effectsize)
options(es.use_symbols = TRUE) 
theme_set(theme_sjplot())

####################################
## Vignette Experiment (Study One)##
####################################

load("modeldata.RData")
head(modeldata) #looks good, cleaned dataset
nrow(modeldata) #2116

modeldata <- subset(modeldata, fmac==1)
nrow(modeldata) #1872 (attention and manipulation check passed)

levels(modeldata$Treatment) <- c("Control", "Meat (Environment)", "Animal Rights", "Transportation (Environment)")


modeldata$Treatment1 <- modeldata$Treatment
modeldata$Treatment1[(modeldata$Treatment == "Animal Rights") | (modeldata$Treatment == "Transportation (Environment)")] = NA
levels(modeldata$Treatment)
levels(modeldata$Treatment1)
table(modeldata$Treatment)
table(modeldata$Treatment1)
modeldata$Treatment1 <- droplevels(modeldata$Treatment1)

modeldata$Treatment2 <- modeldata$Treatment
modeldata$Treatment2[(modeldata$Treatment == "Meat (Environment)") | (modeldata$Treatment == "Transportation (Environment)")] = NA
levels(modeldata$Treatment)
levels(modeldata$Treatment2)
table(modeldata$Treatment)
table(modeldata$Treatment2)
modeldata$Treatment2 <- droplevels(modeldata$Treatment2)

modeldata$Treatment3 <- modeldata$Treatment
modeldata$Treatment3[(modeldata$Treatment == "Meat (Environment)") | (modeldata$Treatment == "Animal Rights")] = NA
levels(modeldata$Treatment)
levels(modeldata$Treatment3)
table(modeldata$Treatment)
table(modeldata$Treatment3)
modeldata$Treatment3 <- droplevels(modeldata$Treatment3)

modeldata$Treatment4 <- modeldata$Treatment
modeldata$Treatment4[(modeldata$Treatment == "Animal Rights") | (modeldata$Treatment == "Control")] = NA
levels(modeldata$Treatment)
levels(modeldata$Treatment4)
table(modeldata$Treatment)
table(modeldata$Treatment4)
modeldata$Treatment4 <- droplevels(modeldata$Treatment4)


## presenting average treatment effects and conditional average treatment effects

res1 <- lm(Support ~ Treatment, weight=WEIGHT, data=modeldata)
summary(res1)

cohens_d(Support ~ Treatment1, weight=WEIGHT, data=modeldata)
## All respondents, control v meat (environment)
## 0.36      | [0.23, 0.49]
## d = 0.2, 0.5 and 0.8 correspond to small, medium and large effects

eff <- allEffects(res1)
eff_df <- as.data.frame(eff[["Treatment"]])

ggplot(eff_df, aes(x = Treatment, y = fit)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    scale_y_continuous(limits = c(2, 6)) +
    labs(x = "Treatment", y = "Predicted Support (1-7)") +
    ggtitle("Average Treatment Effects (ALL RESPONDENTS)") +
    coord_flip() +
    theme_classic()

stargazer(res1, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))

# comparing to transportation

res1b <- lm(Support ~ relevel(Treatment, ref="Transportation (Environment)"), weight=WEIGHT, data=modeldata)
summary(res1b)

cohens_d(Support ~ Treatment4, weight=WEIGHT, data=modeldata)
##Cohen's d |         95% CI
##0.39     | [0.52, 0.26]

stargazer(res1b, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))


## Rural v. Urban 


modeldata_Urban <- subset(modeldata, Rural=="Urban")
nrow(modeldata_Urban)
modeldata_Urban$Rural <- droplevels(modeldata_Urban$Rural)
levels(modeldata_Urban$Rural)

res2 <- lm(Support ~ Treatment1, weight=WEIGHT, data=modeldata_Urban)
summary(res2)

cohens_d(Support ~ Treatment1, weight=WEIGHT, data=modeldata_Urban)
## 0.32      | [0.18, 0.46]

modeldata_Rural <- subset(modeldata, Rural=="Rural")
nrow(modeldata_Rural)
modeldata_Rural$Rural <- droplevels(modeldata_Rural$Rural)
levels(modeldata_Rural$Rural)

res3 <- lm(Support ~ Treatment1, weight=WEIGHT, data=modeldata_Rural)
summary(res3)

cohens_d(Support ~ Treatment1, weight=WEIGHT, data=modeldata_Rural)
## 0.58      | [0.24, 0.92]

stargazer(res2, res3, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))

res4 <- lm(Support ~ Treatment1*Rural, weight=WEIGHT, data=modeldata)
summary(res4)
## differences between urban and rural is significant. 
## Treatment1Meat (Environment):RuralRural -0.92694    0.30734  -3.016 0.002632 ** 

stargazer(res4, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))


## to see interaction effect clearly, use: plot_model(res2, type = "pred", terms = c("Treatment1", "Rural"), axis.title = "")


## Partisanship CATES


modeldata_Dems <- subset(modeldata, Party=="Democrat")
nrow(modeldata_Dems)
modeldata_Dems$Party <- droplevels(modeldata_Dems$Party)
levels(modeldata_Dems$Party)

res5 <- lm(Support ~ Treatment1, weight=WEIGHT, data=modeldata_Dems)
summary(res5)



modeldata_Reps <- subset(modeldata, Party=="Republican")
nrow(modeldata_Reps)
modeldata_Reps$Party <- droplevels(modeldata_Reps$Party)
levels(modeldata_Reps$Party)

res6 <- lm(Support ~ Treatment1, weight=WEIGHT, data=modeldata_Reps)
summary(res6)

cohens_d(Support ~ Treatment1, weight=WEIGHT, data=modeldata_Reps)
## 0.67      | [0.45, 0.89]





res7 <- lm(Support ~ Treatment2, weight=WEIGHT, data=modeldata_Dems)
summary(res7)

res8 <- lm(Support ~ Treatment2, weight=WEIGHT, data=modeldata_Reps)
summary(res8)




res9 <- lm(Support ~ Treatment3, weight=WEIGHT, data=modeldata_Dems)
summary(res9)
cohens_d(Support ~ Treatment3, weight=WEIGHT, data=modeldata_Dems)
## 0.25     | [0.43, 0.06]


res10 <- lm(Support ~ Treatment3, weight=WEIGHT, data=modeldata_Reps)
summary(res10)
cohens_d(Support ~ Treatment3, weight=WEIGHT, data=modeldata_Reps)
## 0.22      | [0.01, 0.42]



stargazer(res5, res6, res7, res8, res9, res10, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))


res11 <- lm(Support ~ Treatment1*Party, weight=WEIGHT, data=modeldata)
summary(res11)
## Treatment1Meat (Environment):PartyRepublican  -0.87651    0.24213  -3.620 0.000311 ***

res12 <- lm(Support ~ Treatment3*Party, weight=WEIGHT, data=modeldata)
summary(res12)
## Treatment3Transportation (Environment):PartyRepublican  -0.77376    0.22516  -3.436 0.000615 ***

respartyall <- lm(Support ~ Treatment*Party, weight=WEIGHT, data=modeldata)
summary(respartyall)

stargazer(respartyall, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))



## Race CATES

modeldata_Black <- subset(modeldata, RACE6=="Black")
nrow(modeldata_Black)
modeldata_Black$RACE6 <- droplevels(modeldata_Black$RACE6)
levels(modeldata_Black$RACE6)

res13 <- lm(Support ~ Treatment2, weight=WEIGHT, data=modeldata_Black)
summary(res13)


modeldata_White <- subset(modeldata, RACE6=="White")
nrow(modeldata_White)
modeldata_White$RACE6 <- droplevels(modeldata_White$RACE6)
levels(modeldata_White$RACE6)

res14 <- lm(Support ~ Treatment2, weight=WEIGHT, data=modeldata_White)
summary(res14)

stargazer(res13, res14, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))



modeldata_Hispanic <- subset(modeldata, RACE6=="Hispanic")
nrow(modeldata_Hispanic)
modeldata_Hispanic$RACE6 <- droplevels(modeldata_Hispanic$RACE6)
levels(modeldata_Hispanic$RACE6)

res13H <- lm(Support ~ Treatment2, weight=WEIGHT, data=modeldata_Hispanic)
summary(res13H)


stargazer(res13H, res14, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))



res15 <- lm(Support ~ Treatment2*RACE6, weight=WEIGHT, data=modeldata)
summary(res15)
## no significant interaction terms for any racial groups. 

stargazer(res15, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))


## OTHER DVs

modeldata$Treatment4 <- modeldata$Treatment
modeldata$Treatment4[(modeldata$Treatment == "Transportation (Environment)")] = NA
levels(modeldata$Treatment)
levels(modeldata$Treatment4)
table(modeldata$Treatment)
table(modeldata$Treatment4)
modeldata$Treatment4 <- droplevels(modeldata$Treatment4)

res16 <- lm(morality ~ Treatment4, weight=WEIGHT, data=modeldata)
summary(res16)

cohens_d(morality ~ Treatment2, weight=WEIGHT, data=modeldata)
## 0.12      | [-0.01, 0.25]
## d = 0.2, 0.5 and 0.8 correspond to small, medium and large effects

res16a <- lm(morality ~ Treatment2, weight=WEIGHT, data=modeldata_Dems)
summary(res16a)

res16b <- lm(morality ~ Treatment2, weight=WEIGHT, data=modeldata_Reps)
summary(res16b)
# largely driven by Republicans

res16c <- lm(morality ~ Treatment4*Party, weight=WEIGHT, data=modeldata)
summary(res16c)
# Treatment4Animal Rights:PartyRepublican        -0.8087     0.3592  -2.251  0.02452 *  




res17 <- lm(power ~ Treatment4, weight=WEIGHT, data=modeldata)
summary(res17)

res17a <- lm(power ~ Treatment1, weight=WEIGHT, data=modeldata_Dems)
summary(res17a)
res17b <- lm(power ~ Treatment1, weight=WEIGHT, data=modeldata_Reps)
summary(res17b)
res17c <- lm(power ~ Treatment2, weight=WEIGHT, data=modeldata_Dems)
summary(res17c)
res17d <- lm(power ~ Treatment2, weight=WEIGHT, data=modeldata_Reps)
summary(res17d)
# Nothing by party either


# reverse score

modeldata$likeability <- 10 - modeldata$likeability

res18 <- lm(likeability ~ Treatment4, weight=WEIGHT, data=modeldata)
summary(res18)

cohens_d(likeability ~ Treatment1, weight=WEIGHT, data=modeldata)
## 0.38     | [0.51, 0.25]
## d = 0.2, 0.5 and 0.8 correspond to small, medium and large effects

res18a <- lm(likeability ~ Treatment2, weight=WEIGHT, data=modeldata_Dems)
summary(res18a)
res18b <- lm(likeability ~ Treatment2, weight=WEIGHT, data=modeldata_Reps)
summary(res18b)
## nothing for ar by party

res18c <- lm(likeability ~ Treatment1, weight=WEIGHT, data=modeldata_Dems)
summary(res18c)
res18d <- lm(likeability ~ Treatment1, weight=WEIGHT, data=modeldata_Reps)
summary(res18d)
## stronger for Republicans

res18e <- lm(likeability ~ Treatment4*Party, weight=WEIGHT, data=modeldata)
summary(res18e)
# Treatment4Meat (Environment):PartyRepublican  -0.70378    0.31774  -2.215   0.0269 *  



stargazer(res16, res17, res18, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))

stargazer(res16c, res18e, align=TRUE, no.space=TRUE, omit.stat=c("LL", "ser", "f"))


#######################
## Conjoint (Study 2)##
#######################

final <- read.csv("clean_dat.csv", na.strings=c("","NA"))
dim(final)
head(final) # 8570

final$Gender <- factor(final$Gender, levels=unique(c("Male", "Female")))
final$Diet <- factor(final$Diet, levels=unique(c("None", "Vegetarian", "Vegan (no animal products including dairy and eggs)")))
final$Animals <- factor(final$Animals, levels=unique(c("Does not support", "Moderate supporter", "Strong supporter")))
final$Race <- factor(final$Race, levels=unique(c("White", "Black", "Latino/Latina")))
final$Pets <- factor(final$Pets, levels=unique(c("No pets", "Owns dogs", "Owns cats", "Owns rescued farm animals")))
final$Marital <- factor(final$Marital, levels=unique(c("Single", "Married", "Divorced")))
final$Age <- factor(final$Age, levels=unique(c("35","45","55","65")))
final$Experience <- factor(final$Experience, levels=unique(c("None", "Mayor", "Representative in congress", "Senator")))

library(plyr)

final$Experience <- revalue(final$Experience, c("None"="No Experience", "Mayor"="Mayor", "Representative in congress"="Representative in congress", "Senator"="Senator"))


final$Resp_Gender <- final$Gender_ssi
final$Gender_ssi <- NULL


final$Resp_Party_Weak[final$Q25=="Democrat" | final$Q27=="Democrats"] <- "Democrat"
final$Resp_Party_Weak[final$Q25=="Republican" | final$Q27=="Republicans"] <- "Republican"
final$Resp_Party_Weak <- factor(final$Resp_Party_Weak, levels=unique(c("Democrat", "Republican")))
final$Resp_Party_Weak <- relevel(final$Resp_Party_Weak, ref = "Republican")
table(final$Resp_Party_Weak)



###Baseline

amces <- cj(na.omit(final), chosen ~ Diet + Animals + Race + Pets + Marital + Age + Experience + Gender, id = ~respondent)
plot(amces, xlab="Change in Pr(Candidate Winning)")


###Marginal means

mms <- cj(na.omit(final), chosen ~ Diet + Animals + Race + Pets + Marital + Age + Experience + Gender, id = ~respondent, estimate = "mm", by = ~Resp_Party_Weak)
diff_mms <- cj(na.omit(final), chosen ~ Diet + Animals + Race + Pets + Marital + Age + Experience + Gender, id = ~respondent, estimate = "mm_diff", by = ~Resp_Party_Weak)
plot(rbind(mms, diff_mms), xlab = "Effect on Pr(Candidate Selected)") + ggplot2::facet_wrap(~BY, ncol = 3L)

## to see within the party

dems <- subset(final, Resp_Party_Weak=="Democrat")
nrow(dems)

reps <- subset(final, Resp_Party_Weak=="Republican")
nrow(reps)

amces <- cj(na.omit(dems), chosen ~ Diet + Animals + Race + Pets + Marital + Age + Experience + Gender, id = ~respondent)
amces <- cj(na.omit(reps), chosen ~ Diet + Animals + Race + Pets + Marital + Age + Experience + Gender, id = ~respondent)


#######################
### gender and race ###
#######################

final$x <- ifelse((final$Race=="Black" & final$Gender=="Female"), 1, 0)

# 1,414 black woman candidates

final$x <- ifelse((final$Race=="Latino/Latina" & final$Gender=="Female"), 2, final$x)

# 1,454 Latina candidates

final$x <- ifelse((final$Race=="White" & final$Gender=="Female"), 3, final$x)

# 1,388 white women candidates

final$x <- ifelse((final$Race=="Black" & final$Gender=="Male"), 4, final$x)

#1,454 Black men

final$x <- ifelse((final$Race=="Latino/Latina" & final$Gender=="Male"), 5, final$x)

#1,418 latino candidates

final$x <- ifelse((final$Race=="White" & final$Gender=="Male"), 6, final$x)

#1,442 white men candidates

final$Intersections <- as.factor(final$x)
table(final$Intersections)

levels(final$Intersections) <- c("Black Woman Candidate", "Latina Candidate", "White Woman Candidate", "Black Man Candidate", "Latino Candidate", "White Man Candidate")

final$Intersections <- relevel(final$Intersections, ref = "White Man Candidate")


#####################
### AMIE analysis ###
#####################

final_dem <- subset(final, Resp_Party_Weak=="Democrat")
final_rep <- subset(final, Resp_Party_Weak=="Republican")

### Dems first

#set.seed(1234)
F4<- FindIt(model.treat= chosen ~ Intersections+Diet+Animals+Pets,
             nway=4,
                        data = final_dem,
             type="continuous",
             treat.type="multiple",
             search.lambdas=TRUE)

## Returns coefficient estimates.
summary(F4)

## Returns predicted values for unique treatment combinations.
pred4 <- predict(F4,unique=TRUE)

## Table 3: Top 10 combinations of most ambitious profiles
x <- head(pred4$data, n=10)

## Bottom 10 (not shown)
tail(pred4$data, n=10)

plot(pred4)

write.csv(x, 'dems.csv')


### Reps

#set.seed(1234)
F4<- FindIt(model.treat= chosen ~ Intersections+Diet+Animals+Pets,
             nway=4,
                        data = final_rep,
             type="continuous",
             treat.type="multiple",
             search.lambdas=TRUE)

## Returns coefficient estimates.
summary(F4)

## Returns predicted values for unique treatment combinations.
pred4 <- predict(F4,unique=TRUE)

## Table 3: Top 10 combinations of most ambitious profiles
x <- head(pred4$data, n=10)

## Bottom 10 (not shown)
tail(pred4$data, n=10)

plot(pred4)

write.csv(x, 'reps.csv')



## MM analysis below

final$IntersectionsxAnimals <- with(final, interaction(Intersections, Animals), drop = TRUE)
final$IntersectionsxDiet <- with(final, interaction(Intersections, Diet), drop = TRUE)
final$IntersectionsxPets <- with(final, interaction(Intersections, Pets), drop = TRUE)


final_dem <- subset(final, Resp_Party_Weak=="Democrat")
final_rep <- subset(final, Resp_Party_Weak=="Republican")


## Animal Rights 

mms <- cj(na.omit(final), chosen ~  IntersectionsxAnimals + Diet + Pets + Marital + Age + Experience, id = ~respondent, estimate = "mm", by = ~Resp_Party_Weak)

diff_mms <- cj(na.omit(final), chosen ~  IntersectionsxAnimals + Diet + Pets + Marital + Age + Experience, id = ~respondent, estimate = "mm_diff", by = ~Resp_Party_Weak)

plot(rbind(mms, diff_mms), xlab = "Effect on Pr(Candidate Selected)") + ggplot2::facet_wrap(~BY, ncol = 3L)



final_dem$IntersectionsxAnimals <- relevel(final_dem$IntersectionsxAnimals, ref = "White Woman Candidate.Strong supporter")

amces_rep <- cj(na.omit(final_rep), chosen ~ IntersectionsxAnimals + Diet + Pets + Marital + Age + Experience, id = ~respondent)
plot(amces_rep, xlab="Change in Pr(Candidate Winning)")

amces_dem <- cj(na.omit(final_dem), chosen ~ IntersectionsxAnimals + Diet + Pets + Marital + Age + Experience, id = ~respondent)
plot(amces_dem, xlab="Change in Pr(Candidate Winning)")

write.csv(amces_dem, "amcesdem.csv", row.names=FALSE)

write.csv(amces_rep, "amcesrep.csv", row.names=FALSE)



## Diet

mms <- cj(na.omit(final), chosen ~  Animals + IntersectionsxDiet + Pets + Marital + Age + Experience, id = ~respondent, estimate = "mm", by = ~Resp_Party_Weak)

diff_mms <- cj(na.omit(final), chosen ~  Animals + IntersectionsxDiet + Pets + Marital + Age + Experience, id = ~respondent, estimate = "mm_diff", by = ~Resp_Party_Weak)

plot(rbind(mms, diff_mms), xlab = "Effect on Pr(Candidate Selected)") + ggplot2::facet_wrap(~BY, ncol = 3L)

amces <- cj(na.omit(final_rep), chosen ~ IntersectionsxDiet + Animals + Pets + Marital + Age + Experience, id = ~respondent)
plot(amces, xlab="Change in Pr(Candidate Winning)")

amces <- cj(na.omit(final_dem), chosen ~ IntersectionsxDiet + Animals + Pets + Marital + Age + Experience, id = ~respondent)
plot(amces, xlab="Change in Pr(Candidate Winning)")


## Pets

mms <- cj(na.omit(final), chosen ~  Animals + Diet + IntersectionsxPets + Marital + Age + Experience, id = ~respondent, estimate = "mm", by = ~Resp_Party_Weak)

diff_mms <- cj(na.omit(final), chosen ~  Animals + Diet + IntersectionsxPets + Marital + Age + Experience, id = ~respondent, estimate = "mm_diff", by = ~Resp_Party_Weak)

plot(rbind(mms, diff_mms), xlab = "Effect on Pr(Candidate Selected)") + ggplot2::facet_wrap(~BY, ncol = 3L)


final_dem$IntersectionsxPets <- relevel(final_dem$IntersectionsxPets, ref = "White Woman Candidate.Owns rescued farm animals")

amces_rep2 <- cj(na.omit(final_rep), chosen ~ IntersectionsxPets + Diet + Animals + Marital + Age + Experience, id = ~respondent)
plot(amces_rep2, xlab="Change in Pr(Candidate Winning)")

amces_dem2 <- cj(na.omit(final_dem), chosen ~ IntersectionsxPets + Diet + Animals + Marital + Age + Experience, id = ~respondent)
plot(amces_dem2, xlab="Change in Pr(Candidate Winning)")

write.csv(amces_dem2, "amcesdem2.csv", row.names=FALSE)

write.csv(amces_rep2, "amcesrep2.csv", row.names=FALSE)


