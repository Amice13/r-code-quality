# LGBT CANDIDATES - 2015 UK - APSR
# GABRIELE MAGNI & ANDREW REYNOLDS


#######


library(lme4)
library(arm)
library(stargazer)
library(interplot)
library(car)
library(plyr)



UKdata <-  read.csv("C:/Users/Gabriele/OneDrive/USB-Sept16/Papers/z-Paper-Andy/Data-Latest/0-new/uk-election-2015-lgb-sep12-eliminatedsmallparties-white-final-visibility-nnn-newdata-SSMconstituency-revision-final.csv")


ls(UKdata)
attach(UKdata)


# Create vbl only for major parties
table(Party.id)
n = nrow(UKdata)
party.id.4 = rep(NA, n)
party.id.4[Party.id=="Conservative"]="Conservative"
party.id.4[Party.id=="Labour"]="Labour"
party.id.4[Party.id=="Liberal Democrat"]="Liberal Democrat"
party.id.4[Party.id=="UKIP"]="UKIP"
party.id.4[Party.id=="Green Party"]="Green Party"
party.id.4[Party.id=="Scottish National Party"]="Scottish National Party"
party.id.4[Party.id=="Plaid Cymru"]="Plaid Cymru"
table(party.id.4)

UKdata$party.id.4 <- party.id.4

table(white.const)
nonwhite.const <- 100-white.const
table(nonwhite.const)

scotland <- as.numeric(Region == "Scotland")
table(scotland)

#Write as Stata dataset
library(foreign)
write.dta(UKdata, "c:/Users/Gabriele/Desktop/Stata_analysis/UKdata.candidates.dta")


#Compare districts with and without LGBT candidates
ddply(UKdata, .(LGB), summarize,  Muslim=mean(Muslim))
ddply(UKdata, .(LGB), summarize,  Depriv=mean(Deprivation))
ddply(UKdata, .(LGB), summarize,  Social.grade=mean(Social.grade))
ddply(UKdata, .(LGB), summarize,  Urban=mean(Urban))
ddply(UKdata, .(LGB), summarize,  Ukborn=mean(Ukborn))
ddply(UKdata, .(LGB), summarize,  SSM.const=mean(SSM.const))
ddply(UKdata, .(LGB), summarize,  Marginal=mean(Marginal))
ddply(UKdata, .(LGB), summarize,  White=mean(white.const))




##############
##############



####
## Subset Conservative ##
subset.trust <- subset(UKdata, party.id.4 == "Conservative")

subset.trust <- subset(UKdata, party.id.4 == "Scottish National Party")
ddply(subset.trust, .(LGB), summarize,  White=mean(white.const))

table(subset.trust$white.const)


UKdata <- subset(UKdata, Region != "Scotland")
UKdata <- subset(UKdata, Region != "Wales")
UKdata <- subset(UKdata, Region == "Scotland")

attach(UKdata)

table(Region)

library(plyr)
ddply(subset.trust, .(LGB), summarize,  vote.2010=mean(X2010.Vote.perc))
ddply(subset.trust, .(LGB), summarize,  Muslim=mean(Muslim))
ddply(subset.trust, .(LGB), summarize,  Depriv=mean(Deprivation))
ddply(subset.trust, .(LGB), summarize,  Social.grade=mean(Social.grade))
ddply(subset.trust, .(LGB), summarize,  Urban=mean(Urban))
ddply(subset.trust, .(LGB), summarize,  Ukborn=mean(Ukborn))
ddply(subset.trust, .(LGB), summarize,  SSM.const=mean(SSM.const))
ddply(subset.trust, .(LGB), summarize,  Marginal=mean(Marginal))




## Paper analysis ##  

## Main paper - APSR Revisions ##

table(Deprivation)
Depriv.real <- 100-Deprivation
table(Depriv.real)

mod.1 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  
                Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + 
              (1| Region/Constituency))
summary(mod.1)

mod.2 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME + Educ + Party.Spend + 
                Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + 
                (1| Region/Constituency))
summary(mod.2)

stargazer(mod.1, mod.2, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


## APPENDIX ##

# With interaction terms 

interact.1 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  
                         Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                         party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*SSM.const +
                         (1| Region/Constituency))
summary(interact.1)

interact.2 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*Urban +
                     (1| Region/Constituency))
summary(interact.2)

interact.3 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*Muslim +
                     (1| Region/Constituency))
summary(interact.3)

interact.4 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*white.const +
                     (1| Region/Constituency))
summary(interact.4)

interact.5 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*Depriv.real +
                     (1| Region/Constituency))
summary(interact.5)

stargazer(interact.1, interact.2, interact.3, interact.4, interact.5, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


# Adding education and party spending

interact.b1 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME + Educ + Party.Spend +  
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*SSM.const +
                     (1| Region/Constituency))
summary(interact.b1)

interact.b2 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  Educ + Party.Spend +
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*Urban +
                     (1| Region/Constituency))
summary(interact.b2)

interact.b3 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  Educ + Party.Spend +
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*Muslim +
                     (1| Region/Constituency))
summary(interact.b3)

interact.b4 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  Educ + Party.Spend +
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*white.const +
                     (1| Region/Constituency))
summary(interact.b4)

interact.b5 <- lmer(Votes.perc ~ Incumbent + LGB + Female + BME +  Educ + Party.Spend +
                     Depriv.real + Muslim + Urban + Ukborn + white.const + SSM.const + 
                     party.id.4 + X2010.Vote.perc + region.partyvote.15.10 + + LGB*Depriv.real +
                     (1| Region/Constituency))
summary(interact.b5)

stargazer(interact.b1, interact.b2, interact.b3, interact.b4, interact.b5, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")

stargazer(interact.1, interact.2, interact.3, interact.4,
          interact.b1, interact.b2, interact.b3, interact.b4, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


#Oxbridge instead of education
mod.intercept.sp.oxb <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + Female + BME + Oxbridge + Party.Spend + party.id.4 + Deprivation + 
                               Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + LGB + 
                               (1| Region/Constituency))
summary(mod.intercept.sp.oxb)


#Fixed Effect for constituency
fixed.dummy <- lm(Votes.perc ~ X2010.Vote.perc + Incumbent +  
           LGB + Female + BME + party.id.4 + factor(Constituency) -1) 
summary(fixed.dummy)



#varying slope for (party.id.4 + LGB + Female + BME) by region
mult.slope<-lmer(Votes.perc ~ LGB + Female + BME + party.id.4 + X2010.Vote.perc + Incumbent + 
           Deprivation + Muslim + Urban + Ukborn + Social.grade + marriage.equal +
           region.partyvote.15.10 + (1 + party.id.4 + LGB + Female + BME| Region))
summary(mult.slope)
round(coef(mult.slope)$Region, digits = 3)
round(se.ranef(mult.slope)$Region, digits = 3)
stargazer(mult.slope, type="html", digits = 2,
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="mod.htm")



## Visibility
mod.intercept.vis <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + Female + BME + party.id.4 + Deprivation + 
                        Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + Visibility + 
                        (1| Region/Constituency))
summary(mod.intercept.vis)

mod.intercept.vis.2 <- lmer(Votes.perc ~ Incumbent + LGB + Visibility + Female + BME + 
                              party.id.4 + X2010.Vote.perc + Deprivation + 
                            Muslim + Urban + Ukborn + Social.grade +  
                              region.partyvote.15.10 +  marriage.equal +
                            (1| Region/Constituency))
summary(mod.intercept.vis.2)


mod.intercept.vis.0 <- lmer(Votes.perc ~ Incumbent + LGB + Visibility + Female + BME + 
                              party.id.4 + X2010.Vote.perc + Deprivation + 
                              Muslim + Urban + Ukborn + Social.grade +  
                              region.partyvote.15.10 +  marriage.equal +
                              (1| Region/Constituency))
summary(mod.intercept.vis.0)


library(stargazer)
stargazer(mod.intercept, type="html",
          out="mod.htm")


#2
#APPENDIX (Oxbridge instead of education)
interact.urban.sp.oxb <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + Female + BME + 
                                Party.Spend + Oxbridge + party.id.4 + Deprivation +
                                Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + LGB + LGB*Urban +
                                (1| Region/Constituency))
summary(interact.urban.sp.oxb)


interact.urban.vis.2 <- lmer(Votes.perc ~ Incumbent + LGB + Visibility + Female + BME + 
                               party.id.4 + X2010.Vote.perc + Deprivation + 
                             Muslim + Urban + Ukborn + Social.grade +  
                               region.partyvote.15.10 + marriage.equal + LGB*Urban +
                             (1| Region/Constituency))
summary(interact.urban.vis.2)

interact.urban.vis.0 <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Visibility + Female + BME + party.id.4 + Deprivation + 
                             Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + Visibility*Urban +
                             (1| Region/Constituency))
summary(interact.urban.vis.0)

## 2b - LGB*Muslim 

#APPENDIX (Oxbridge)
interact.muslim.sp.oxb <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + Female + BME +
                                 Party.Spend + Oxbridge + party.id.4 + Deprivation +
                               Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + LGB + LGB*Muslim +
                                 (1| Region/Constituency))
summary(interact.muslim.sp.oxb)


interact.muslim.vis <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + Visibility + Female + BME + party.id.4 + Deprivation + 
                          Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + Visibility*Muslim +
                          (1| Region/Constituency))
summary(interact.muslim.vis)

interplot(m = interact.muslim.vis, var1 = "Visibility", var2 = "Muslim") +
  geom_hline(yintercept = 0, linetype = "dashed")

interact.muslim.vis.2 <- lmer(Votes.perc ~ Incumbent + LGB + Visibility + Female + BME + 
                                party.id.4 + X2010.Vote.perc + Deprivation + 
                              Muslim + Urban + Ukborn + Social.grade +  
                                region.partyvote.15.10 + marriage.equal + LGB*Muslim +
                              (1| Region/Constituency))
summary(interact.muslim.vis.2)

interact.muslim.vis.0 <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Visibility + Female + BME + party.id.4 + Deprivation + 
                              Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + Visibility*Muslim +
                              (1| Region/Constituency))
summary(interact.muslim.vis.0)

## 2c - LGB*Deprivation 

#APPENDIX (Oxbridge)
interact.depriv.sp.oxb <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + Female + BME + 
                                 Party.Spend + Oxbridge + party.id.4 + Deprivation + 
                                 Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + LGB + LGB*Deprivation +
                                 (1| Region/Constituency)) 
summary(interact.depriv.sp.oxb)


interact.depriv.vis <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + Visibility + Female + BME + party.id.4 + Deprivation + 
                          Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + Visibility*Deprivation +
                          (1| Region/Constituency)) 
summary(interact.depriv.vis) 

interact.depriv.vis.2 <- lmer(Votes.perc ~ Incumbent + LGB + Visibility + Female + BME + 
                                party.id.4 + X2010.Vote.perc + Deprivation + 
                              Muslim + Urban + Ukborn + Social.grade +  
                                region.partyvote.15.10 + marriage.equal + LGB*Deprivation +
                              (1| Region/Constituency)) 
summary(interact.depriv.vis.2)

interact.depriv.vis.0 <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Visibility + Female + BME + party.id.4 + Deprivation + 
                              Muslim + Urban + Ukborn + Social.grade + marriage.equal + region.partyvote.15.10 + Visibility*Deprivation +
                              (1| Region/Constituency)) 
summary(interact.depriv.vis.0)





########
##EXPORT

#Export baseline models
stargazer(mod.intercept, interact.urban, interact.muslim, interact.depriv, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")

#Export baseline models with SSM constituency
stargazer(mod.intercept.SSMconst, interact.urban.SSMconst, interact.muslim.SSMconst, interact.depriv.SSMconst, interact.SSM.SSMconst, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


#Export models with party spending and education
stargazer(mod.intercept.sp.edu, interact.urban.sp.edu, interact.muslim.sp.edu, interact.depriv.sp.edu, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")

stargazer(mod.intercept.sp.oxb, interact.urban.sp.oxb, interact.muslim.sp.oxb, interact.depriv.sp.oxb, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


#Export models with party spending and education with SSM constituency
stargazer(mod.intercept.sp.edu.ssm, interact.urban.sp.edu.ssm, interact.muslim.sp.edu.ssm, interact.depriv.sp.edu.ssm, interact.SSM.sp.edu.ssm, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


#Export models with visibility
library(stargazer)
stargazer(mod.intercept.vis, interact.urban.vis, interact.muslim.vis, interact.depriv.vis, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")

stargazer(mod.intercept.vis.2, interact.urban.vis.2, interact.muslim.vis.2, interact.depriv.vis.2, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")

stargazer(mod.intercept.vis.0, interact.urban.vis.0, interact.muslim.vis.0, interact.depriv.vis.0, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")



# Clustering by parties 

# Varying intercept and slope by party and grouping also by constituency clustered into regions 
party.model.1 <- lmer(Votes.perc ~ LGB + Female + BME + X2010.Vote.perc + Incumbent + region.partyvote.15.10 + 
                      Deprivation + Muslim + Urban +  Ukborn + Social.grade + marriage.equal + 
                      (1 + LGB + Female + BME|party.id.4) + (1| Region/Constituency)) 
summary(party.model.1) 
round(coef(party.model.1)$party.id.4, digits = 3) 
round(se.ranef(party.model.1)$party.id.4, digits = 3) #party coefficient standard error

coef.party <- round(coef(party.model.1)$party.id.4, digits = 3) 



# Plot slopes by party

# LGB
plot(1:7, coef(party.model.1)$party.id.4[ , 2], pch = 19, xlab = "Parties", ylab = "Estimated Coefficient for LGBT", ylim = c(-2, 2), axes = FALSE)
axis(1, at = 1:7, labels = c("Conserv","Green","Labour","Libdem","PC","SNP","UKIP"))
axis(2, las = 2) 
segments(1:7, coef(party.model.1)$party.id.4[ , 2] - 1.96*se.ranef(party.model.1)$party.id.4[ , 2], 1:7, coef(party.model.1)$party.id.4[ , 2] + 1.96*se.ranef(party.model.1)$party.id.4[ , 2]) 
abline(h=0)
box()
title(main = "Estimated Coefficient of Sexual Orientation (LGBT) \non Candidate Vote Share by Party")

# Female
plot(1:7, coef(party.model.1)$party.id.4[ , 3], pch = 19, xlab = "Parties", ylab = "Estimated Coefficient for Female", ylim = c(-1, 1.5), axes = FALSE)
axis(1, at = 1:7, labels = c("Conserv","Green","Labour","Libdem","PC","SNP","UKIP"))
axis(2, las = 2) 
segments(1:7, coef(party.model.1)$party.id.4[ , 3] - 1.96*se.ranef(party.model.1)$party.id.4[ , 3], 1:7, coef(party.model.1)$party.id.4[ , 3] + 1.96*se.ranef(party.model.1)$party.id.4[ , 3]) 
abline(h=0)
box()
title(main = "Estimated Coefficient of Gender (Female) \non Candidate Vote Share by Party")

# BME
plot(1:7, coef(party.model.1)$party.id.4[ , 4], pch = 19, xlab = "Parties", ylab = "Estimated Coefficient for BME", ylim = c(-2.5, 2.5), axes = FALSE)
axis(1, at = 1:7, labels = c("Conserv","Green","Labour","Libdem","PC","SNP","UKIP"))
axis(2, las = 2) 
segments(1:7, coef(party.model.1)$party.id.4[ , 4] - 1.96*se.ranef(party.model.1)$party.id.4[ , 4], 1:7, coef(party.model.1)$party.id.4[ , 4] + 1.96*se.ranef(party.model.1)$party.id.4[ , 4]) 
abline(h=0)
box()
title(main = "Estimated Coefficient of Ethnic Identity (BME) \non Candidate Vote Share by Party")




#Clustering by parties with spending and education

party.model.1.sp <- lmer(Votes.perc ~ LGB + Female + BME + Educ + Party.Spend +
                           X2010.Vote.perc + Incumbent + region.partyvote.15.10 + 
                           Deprivation + Muslim + Urban +  Ukborn + Social.grade + marriage.equal + 
                           (1 + LGB + Female + BME|party.id.4) + (1| Region/Constituency)) 
summary(party.model.1.sp)
round(coef(party.model.1.sp)$party.id.4, digits = 3) 
round(se.ranef(party.model.1.sp)$party.id.4, digits = 3) #party coefficient standard error
coef.party <- round(coef(party.model.1.sp)$party.id.4, digits = 3) 

# LGB
plot(1:7, coef(party.model.1.sp)$party.id.4[ , 2], pch = 19, xlab = "Parties", ylab = "Estimated Coefficient for LGBT", ylim = c(-2, 2.7), axes = FALSE)
axis(1, at = 1:7, labels = c("Conserv","Green","Labour","Libdem","PC","SNP","UKIP"))
axis(2, las = 2) 
segments(1:7, coef(party.model.1.sp)$party.id.4[ , 2] - 1.96*se.ranef(party.model.1.sp)$party.id.4[ , 2], 1:7, coef(party.model.1.sp)$party.id.4[ , 2] + 1.96*se.ranef(party.model.1.sp)$party.id.4[ , 2]) 
abline(h=0)
box()
title(main = "Estimated Coefficient of Sexual Orientation (LGBT) \non Candidate Vote Share by Party")


#Clustering by parties with spending and oxbridge

party.model.1.sp.oxb <- lmer(Votes.perc ~ LGB + Female + BME + Oxbridge + Party.Spend +
                           X2010.Vote.perc + Incumbent + region.partyvote.15.10 + 
                           Deprivation + Muslim + Urban +  Ukborn + Social.grade + marriage.equal + 
                           (1 + LGB + Female + BME|party.id.4) + (1| Region/Constituency)) 
summary(party.model.1.sp.oxb)
round(coef(party.model.1.sp.oxb)$party.id.4, digits = 3) 
round(se.ranef(party.model.1.sp.oxb)$party.id.4, digits = 3) #party coefficient standard error
coef.party <- round(coef(party.model.1.sp.oxb)$party.id.4, digits = 3) 

# LGB
plot(1:7, coef(party.model.1.sp.oxb)$party.id.4[ , 2], pch = 19, xlab = "Parties", ylab = "Estimated Coefficient for LGBT", ylim = c(-2, 2.7), axes = FALSE)
axis(1, at = 1:7, labels = c("Conserv","Green","Labour","Libdem","PC","SNP","UKIP"))
axis(2, las = 2) 
segments(1:7, coef(party.model.1.sp.oxb)$party.id.4[ , 2] - 1.96*se.ranef(party.model.1.sp.oxb)$party.id.4[ , 2], 1:7, coef(party.model.1.sp.oxb)$party.id.4[ , 2] + 1.96*se.ranef(party.model.1.sp.oxb)$party.id.4[ , 2]) 
abline(h=0)
box()
title(main = "Estimated Coefficient of Sexual Orientation (LGBT) \non Candidate Vote Share by Party")


#Clustering by parties with SSM.constituency
party.model.1.SSMconst <- lmer(Votes.perc ~ LGB + Female + BME + X2010.Vote.perc + Incumbent + region.partyvote.15.10 + 
                        Deprivation + Muslim + Urban +  Ukborn + Social.grade + SSM.const + 
                        (1 + LGB + Female + BME|party.id.4) + (1| Region/Constituency)) 
summary(party.model.1.SSMconst) 
round(coef(party.model.1.SSMconst)$party.id.4, digits = 3) 
round(se.ranef(party.model.1.SSMconst)$party.id.4, digits = 3) 
      

# Party models with visibility

party.model.1.vis <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + region.partyvote.15.10 + 
                            Deprivation + Muslim + Urban +  Ukborn + Social.grade + marriage.equal + LGB + Visibility + Female + BME +
                            (1 + LGB + Visibility + Female + BME|party.id.4) + (1| Region/Constituency)) 
#only visibility, no LGB vbl
party.model.1.vis <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + region.partyvote.15.10 + 
                            Deprivation + Muslim + Urban +  Ukborn + Social.grade + marriage.equal + Visibility + Female + BME +
                            (1 + Visibility + Female + BME|party.id.4) + (1| Region/Constituency)) 
summary(party.model.1.vis) 
round(coef(party.model.1.vis)$party.id.4, digits = 3) 
round(se.ranef(party.model.1.vis)$party.id.4, digits = 3) #party coefficient standard error
coef.party <- round(coef(party.model.1.vis)$party.id.4, digits = 3)

# Plot LGB
plot(1:7, coef(party.model.1.vis)$party.id.4[ , 11], pch = 19, xlab = "Parties", ylab = "Estimated Coefficient for LGBT", ylim = c(-2, 2), axes = FALSE)
axis(1, at = 1:7, labels = c("Conserv","Green","Labour","Libdem","PC","SNP","UKIP"))
axis(2, las = 2) 
segments(1:7, coef(party.model.1.vis)$party.id.4[ , 11] - 1.96*se.ranef(party.model.1.vis)$party.id.4[ , 2], 1:7, coef(party.model.1.vis)$party.id.4[ , 11] + 1.96*se.ranef(party.model.1.vis)$party.id.4[ , 2]) 
abline(h=0)
box()
title(main = "Estimated Coefficient of Sexual Orientation (LGBT) \non Candidate Vote Share by Party")

# Plot Visibility (in model WITH LGB)
plot(1:7, coef(party.model.1.vis)$party.id.4[ , 12], pch = 19, xlab = "Parties", ylab = "Estimated Coefficient for Visibility", ylim = c(-4, 4), axes = FALSE)
axis(1, at = 1:7, labels = c("Conserv","Green","Labour","Libdem","PC","SNP","UKIP"))
axis(2, las = 2) 
segments(1:7, coef(party.model.1.vis)$party.id.4[ , 12] - 1.96*se.ranef(party.model.1.vis)$party.id.4[ , 3], 1:7, coef(party.model.1.vis)$party.id.4[ , 12] + 1.96*se.ranef(party.model.1.vis)$party.id.4[ , 3]) 
abline(h=0)
box()
title(main = "Estimated Coefficient of Visibility of LGBT Candidates \non Candidate Vote Share by Party")

# Plot Visibility (in model WITHOUT LGB)
plot(1:7, coef(party.model.1.vis)$party.id.4[ , 11], pch = 19, xlab = "Parties", ylab = "Estimated Coefficient for Visibility", ylim = c(-4, 4), axes = FALSE)
axis(1, at = 1:7, labels = c("Conserv","Green","Labour","Libdem","PC","SNP","UKIP"))
axis(2, las = 2) 
segments(1:7, coef(party.model.1.vis)$party.id.4[ , 11] - 1.96*se.ranef(party.model.1.vis)$party.id.4[ , 2], 1:7, coef(party.model.1.vis)$party.id.4[ , 11] + 1.96*se.ranef(party.model.1.vis)$party.id.4[ , 2]) 
abline(h=0)
box()
title(main = "Estimated Coefficient of Visibility of LGBT Candidates \non Candidate Vote Share by Party")




#####################
## COLLINEARITY TESTS
#####################


#Correlation
cor(Deprivation, Social.grade, use = "complete.obs")
cor(Deprivation, Urban, use = "complete.obs")
cor(Urban, Social.grade, use = "complete.obs")
cor(Urban, Muslim, use = "complete.obs")
cor(Muslim, Social.grade, use = "complete.obs")
cor(Muslim, Deprivation, use = "complete.obs")



cor(LGB, Social.grade, use = "complete.obs")
cor(LGB, Deprivation, use = "complete.obs")
cor(LGB, Muslim, use = "complete.obs")
cor(LGB, Ukborn, use = "complete.obs")
cor(LGB, SSM.const, use = "complete.obs")
cor(LGB, Female, use = "complete.obs")
cor(LGB, Urban, use = "complete.obs")
cor(LGB, Incumbent, use = "complete.obs")
cor(LGB, X2010.Vote.perc, use = "complete.obs")
cor(LGB, BME, use = "complete.obs")
cor(LGB, region.partyvote.15.10, use = "complete.obs")




#test collinearity


#Run models with interaction LGB*Deprivation [i.e. models] without social grade

interact.depriv.SSMconst.nosocgrade <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + SSM.const + region.partyvote.15.10 + LGB*Deprivation +
                                   (1| Region/Constituency)) 
summary(interact.depriv.SSMconst.nosocgrade) 

interact.depriv.sp.edu.ssm.nosocgrade <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                     Party.Spend + Educ + party.id.4 + Deprivation + 
                                     Muslim + Urban + Ukborn + SSM.const + region.partyvote.15.10 + LGB*Deprivation +
                                     (1| Region/Constituency)) 
summary(interact.depriv.sp.edu.ssm.nosocgrade) 

stargazer(interact.depriv.SSMconst.nosocgrade, interact.depriv.sp.edu.ssm.nosocgrade, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")

#urban interaction without Muslim (models 2)

interact.urban.ssm.nosocgrade <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                         Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Urban +
                         (1| Region/Constituency))
summary(interact.urban.ssm.nosocgrade)

interact.urban.sp.edu.ssm.nosocgrade <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                    Party.Spend + Educ + party.id.4 + Deprivation +
                                    Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Urban +
                                    (1| Region/Constituency))
summary(interact.urban.sp.edu.ssm.nosocgrade)

#Muslim interaction without urban and deprivation (models 3)

interact.muslim.SSMconst.nosocgrade <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + 
                                   Muslim + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Muslim +
                                   (1| Region/Constituency))
summary(interact.muslim.SSMconst.nosocgrade)

interact.muslim.sp.edu.ssm.nosocgrade <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME +
                                     Party.Spend + Educ + party.id.4 +
                                     Muslim + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Muslim +
                                     (1| Region/Constituency))
summary(interact.muslim.sp.edu.ssm.nosocgrade)



############################################
## SUBSET ANALYSIS FOR SCOTLAND AND WALES ##
############################################

#All models for revisions (with SSM.const)

#SCOTLAND

subset.scotland <- subset(UKdata, UKdata$Region == "Scotland")
attach(subset.scotland)

table(Party.id)
n = nrow(subset.scotland)
party.id.4 = rep(NA, n)
party.id.4[Party.id=="Conservative"]="Conservative"
party.id.4[Party.id=="Labour"]="Labour"
party.id.4[Party.id=="Liberal Democrat"]="Liberal Democrat"
party.id.4[Party.id=="UKIP"]="UKIP"
party.id.4[Party.id=="Green Party"]="Green Party"
party.id.4[Party.id=="Scottish National Party"]="Scottish National Party"
party.id.4[Party.id=="Plaid Cymru"]="Plaid Cymru"
table(party.id.4)

#Models for table 4
mod.intercept.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                 Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + 
                                 (1| Constituency))
summary(mod.intercept.SSMconst)

interact.urban.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                  Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Urban +
                                  (1| Constituency))
summary(interact.urban.SSMconst)

interact.muslim.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Muslim +
                                   (1| Constituency))
summary(interact.muslim.SSMconst)

interact.depriv.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Deprivation +
                                   (1| Constituency)) 
summary(interact.depriv.SSMconst) 

interact.SSM.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*SSM.const +
                                (1| Constituency)) 
summary(interact.SSM.SSMconst)

stargazer(mod.intercept.SSMconst, interact.urban.SSMconst, interact.muslim.SSMconst, interact.depriv.SSMconst, interact.SSM.SSMconst, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


#Models for table 5

mod.intercept.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + Educ + Party.Spend + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + 
                                   (1| Constituency))
summary(mod.intercept.sp.edu.ssm)

interact.urban.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                    Party.Spend + Educ + party.id.4 + Deprivation +
                                    Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Urban +
                                    (1| Constituency))
summary(interact.urban.sp.edu.ssm)

interact.muslim.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME +
                                     Party.Spend + Educ + party.id.4 + Deprivation +
                                     Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Muslim +
                                     (1| Constituency))
summary(interact.muslim.sp.edu.ssm)

interact.depriv.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                     Party.Spend + Educ + party.id.4 + Deprivation + 
                                     Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Deprivation +
                                     (1| Constituency)) 
summary(interact.depriv.sp.edu.ssm) 

interact.SSM.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                  Party.Spend + Educ + party.id.4 + Deprivation + 
                                  Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*SSM.const +
                                  (1| Constituency)) 
summary(interact.SSM.sp.edu.ssm) 

stargazer(mod.intercept.sp.edu.ssm, interact.urban.sp.edu.ssm, interact.muslim.sp.edu.ssm, interact.depriv.sp.edu.ssm, interact.SSM.sp.edu.ssm, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


# WALES

subset.wales <- subset(UKdata, UKdata$Region == "Wales")
attach(subset.wales)

table(Party.id)
n = nrow(subset.wales)
party.id.4 = rep(NA, n)
party.id.4[Party.id=="Conservative"]="Conservative"
party.id.4[Party.id=="Labour"]="Labour"
party.id.4[Party.id=="Liberal Democrat"]="Liberal Democrat"
party.id.4[Party.id=="UKIP"]="UKIP"
party.id.4[Party.id=="Green Party"]="Green Party"
party.id.4[Party.id=="Scottish National Party"]="Scottish National Party"
party.id.4[Party.id=="Plaid Cymru"]="Plaid Cymru"
table(party.id.4)

#Models for table 4

mod.intercept.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                 Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + 
                                 (1| Constituency))
summary(mod.intercept.SSMconst)

interact.urban.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                  Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Urban +
                                  (1| Constituency))
summary(interact.urban.SSMconst)

interact.muslim.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Muslim +
                                   (1| Constituency))
summary(interact.muslim.SSMconst)

interact.depriv.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Deprivation +
                                   (1| Constituency)) 
summary(interact.depriv.SSMconst) 

interact.SSM.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*SSM.const +
                                (1| Constituency)) 
summary(interact.SSM.SSMconst)

stargazer(mod.intercept.SSMconst, interact.urban.SSMconst, interact.muslim.SSMconst, interact.depriv.SSMconst, interact.SSM.SSMconst, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


#Models for table 5

mod.intercept.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + Educ + Party.Spend + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + 
                                   (1| Constituency))
summary(mod.intercept.sp.edu.ssm)

interact.urban.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                    Party.Spend + Educ + party.id.4 + Deprivation +
                                    Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Urban +
                                    (1| Constituency))
summary(interact.urban.sp.edu.ssm)

interact.muslim.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME +
                                     Party.Spend + Educ + party.id.4 + Deprivation +
                                     Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Muslim +
                                     (1| Constituency))
summary(interact.muslim.sp.edu.ssm)

interact.depriv.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                     Party.Spend + Educ + party.id.4 + Deprivation + 
                                     Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*Deprivation +
                                     (1| Constituency)) 
summary(interact.depriv.sp.edu.ssm) 

interact.SSM.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                  Party.Spend + Educ + party.id.4 + Deprivation + 
                                  Muslim + Urban + Ukborn + Social.grade + SSM.const + region.partyvote.15.10 + LGB*SSM.const +
                                  (1| Constituency)) 
summary(interact.SSM.sp.edu.ssm) 

stargazer(mod.intercept.sp.edu.ssm, interact.urban.sp.edu.ssm, interact.muslim.sp.edu.ssm, interact.depriv.sp.edu.ssm, interact.SSM.sp.edu.ssm, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")



################################################################
### Additional analysis with constituency racial composition ###
################################################################


#Models for table 4

#Replicate models in table 4 with control for share of blacks and Asians
mod.intercept.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                 Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                 region.partyvote.15.10 + black.const + asian.const +
                                 (1| Region/Constituency))
summary(mod.intercept.SSMconst)

interact.urban.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                  Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                  region.partyvote.15.10 + LGB*Urban + black.const + asian.const +
                                  (1| Region/Constituency))
summary(interact.urban.SSMconst)

interact.muslim.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                   region.partyvote.15.10 + LGB*Muslim + black.const + asian.const +
                                   (1| Region/Constituency))
summary(interact.muslim.SSMconst)

interact.depriv.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                   region.partyvote.15.10 + LGB*Deprivation + black.const + asian.const +
                                   (1| Region/Constituency)) 
summary(interact.depriv.SSMconst) 

interact.SSM.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                region.partyvote.15.10 + LGB*SSM.const + black.const + asian.const +
                                (1| Region/Constituency)) 
summary(interact.SSM.SSMconst)

stargazer(mod.intercept.SSMconst, interact.urban.SSMconst, interact.muslim.SSMconst, interact.depriv.SSMconst, interact.SSM.SSMconst, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")

#Add to model 1 in table 4 interaction between LGB and nonwhite, black, Asian
interact.nonwhiteconst.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                          Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                          region.partyvote.15.10 + LGB*nonwhite.const + nonwhite.const +
                                          (1| Region/Constituency)) 
summary(interact.nonwhiteconst.SSMconst)
interplot(m = interact.nonwhiteconst.SSMconst, var1 = "LGB", var2 = "nonwhite.const") +
  geom_hline(yintercept = 0, linetype = "dashed")

interact.blackconst.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                       Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                       region.partyvote.15.10 + LGB*black.const + black.const + asian.const +
                                       (1| Region/Constituency)) 
summary(interact.blackconst.SSMconst)
interplot(m = interact.blackconst.SSMconst, var1 = "LGB", var2 = "black.const") +
  geom_hline(yintercept = 0, linetype = "dashed")

interact.asianconst.SSMconst <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + party.id.4 + Deprivation + 
                                       Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                       region.partyvote.15.10 + LGB*asian.const + black.const + asian.const +
                                       (1| Region/Constituency)) 
summary(interact.asianconst.SSMconst)
interplot(m = interact.asianconst.SSMconst, var1 = "LGB", var2 = "asian.const") +
  geom_hline(yintercept = 0, linetype = "dashed")

stargazer(interact.nonwhiteconst.SSMconst, interact.blackconst.SSMconst, interact.asianconst.SSMconst, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


#Models for table 5

mod.intercept.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + Educ + Party.Spend + party.id.4 + Deprivation + 
                                   Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                   region.partyvote.15.10 + black.const + asian.const +
                                   (1| Region/Constituency))
summary(mod.intercept.sp.edu.ssm)

interact.urban.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                    Party.Spend + Educ + party.id.4 + Deprivation +
                                    Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                    region.partyvote.15.10 + LGB*Urban + black.const + asian.const +
                                    (1| Region/Constituency))
summary(interact.urban.sp.edu.ssm)

interact.muslim.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME +
                                     Party.Spend + Educ + party.id.4 + Deprivation +
                                     Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                     region.partyvote.15.10 + LGB*Muslim + black.const + asian.const +
                                     (1| Region/Constituency))
summary(interact.muslim.sp.edu.ssm)

interact.depriv.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                     Party.Spend + Educ + party.id.4 + Deprivation + 
                                     Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                     region.partyvote.15.10 + LGB*Deprivation + black.const + asian.const +
                                     (1| Region/Constituency)) 
summary(interact.depriv.sp.edu.ssm) 

interact.SSM.sp.edu.ssm <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                  Party.Spend + Educ + party.id.4 + Deprivation + 
                                  Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                  region.partyvote.15.10 + LGB*SSM.const + black.const + asian.const +
                                  (1| Region/Constituency)) 
summary(interact.SSM.sp.edu.ssm) 

stargazer(mod.intercept.sp.edu.ssm, interact.urban.sp.edu.ssm, interact.muslim.sp.edu.ssm, interact.depriv.sp.edu.ssm, interact.SSM.sp.edu.ssm, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")


#Add to model 1 in table 5 interaction between LGB and nonwhite, black, Asian
interact.nonwhiteconst.SSMconst.edu <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                          Party.Spend + Educ + party.id.4 + Deprivation + 
                                          Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                          region.partyvote.15.10 + LGB*nonwhite.const + nonwhite.const +
                                          (1| Region/Constituency)) 
summary(interact.nonwhiteconst.SSMconst.edu)
interplot(m = interact.nonwhiteconst.SSMconst.edu, var1 = "LGB", var2 = "nonwhite.const") +
  geom_hline(yintercept = 0, linetype = "dashed")

interplot(m = interact.nonwhiteconst.SSMconst.edu, var1 = "LGB", var2 = "nonwhite.const", plot = F)

interact.blackconst.SSMconst.edu <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                       Party.Spend + Educ + party.id.4 + Deprivation + 
                                       Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                       region.partyvote.15.10 + LGB*black.const + black.const + asian.const +
                                       (1| Region/Constituency)) 
summary(interact.blackconst.SSMconst.edu)
interplot(m = interact.blackconst.SSMconst.edu, var1 = "LGB", var2 = "black.const") +
  geom_hline(yintercept = 0, linetype = "dashed")

interact.asianconst.SSMconst.edu <- lmer(Votes.perc ~ X2010.Vote.perc + Incumbent + LGB + Female + BME + 
                                       Party.Spend + Educ + party.id.4 + Deprivation + 
                                       Muslim + Urban + Ukborn + Social.grade + SSM.const + 
                                       region.partyvote.15.10 + LGB*asian.const + black.const + asian.const +
                                       (1| Region/Constituency)) 
summary(interact.asianconst.SSMconst.edu)
interplot(m = interact.asianconst.SSMconst.edu, var1 = "LGB", var2 = "asian.const") +
  geom_hline(yintercept = 0, linetype = "dashed")

interplot(m = interact.asianconst.SSMconst.edu, var1 = "LGB", var2 = "asian.const", plot = F)


stargazer(interact.nonwhiteconst.SSMconst.edu, interact.blackconst.SSMconst.edu, interact.asianconst.SSMconst.edu, type="html", 
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="modpval.htm")
