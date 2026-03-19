### Living in Fear: Political Violence and Authoritarian Attitudes Among Politicians in South Africa ###

# Authors: Philip Martin & Sarah Lockwood

# R version 4.3.2 (2023-10-31 ucrt) #

## Load packages
library(lmtest)
library(sandwich)
library(xtable)
library(stringr)
library(mediation)
library(car)


## Load Data
round2022 <- read.csv("round2022.csv")
round2022_munmerged <- read.csv("round2022_munmerged.csv")


## Tables and Figures for Main Text ###

## Table 1: Covariate Balance

round2022_treated <- subset(round2022, round2022$treated==1)
round2022_control <- subset(round2022, round2022$treated==0)

#Individual covariates
balancetable <- matrix(nrow=8, ncol=4, NA)
t = t.test(round2022_control$female, round2022_treated$female)
balancetable[1,1] <- t$estimate[1]
balancetable[1,2] <- t$estimate[2]
balancetable[1,3] <- t$estimate[2] - t$estimate[1]
balancetable[1,4] <- t$p.value
t = t.test(round2022_control$black, round2022_treated$black)
balancetable[2,1] <- t$estimate[1]
balancetable[2,2] <- t$estimate[2]
balancetable[2,3] <- t$estimate[2] - t$estimate[1]
balancetable[2,4] <- t$p.value
t = t.test(round2022_control$ANC, round2022_treated$ANC)
balancetable[3,1] <- t$estimate[1]
balancetable[3,2] <- t$estimate[2]
balancetable[3,3] <- t$estimate[2] - t$estimate[1]
balancetable[3,4] <- t$p.value
t = t.test(round2022_control$universityeducated, round2022_treated$universityeducated)
balancetable[4,1] <- t$estimate[1]
balancetable[4,2] <- t$estimate[2]
balancetable[4,3] <- t$estimate[2] - t$estimate[1]
balancetable[4,4] <- t$p.value
t = t.test(round2022_control$informalsettlement, round2022_treated$informalsettlement)
balancetable[5,1] <- t$estimate[1]
balancetable[5,2] <- t$estimate[2]
balancetable[5,3] <- t$estimate[2] - t$estimate[1]
balancetable[5,4] <- t$p.value
t = t.test(round2022_control$lengthoftenure, round2022_treated$lengthoftenure)
balancetable[6,1] <- t$estimate[1]
balancetable[6,2] <- t$estimate[2]
balancetable[6,3] <- t$estimate[2] - t$estimate[1]
balancetable[6,4] <- t$p.value
t = t.test(round2022_control$threatorattackfiveyears, round2022_treated$threatorattackfiveyears)
balancetable[7,1] <- t$estimate[1]
balancetable[7,2] <- t$estimate[2]
balancetable[7,3] <- t$estimate[2] - t$estimate[1]
balancetable[7,4] <- t$p.value
t = t.test(round2022_control$too_many_foreigners_dummy, round2022_treated$too_many_foreigners_dummy)
balancetable[8,1] <- t$estimate[1]
balancetable[8,2] <- t$estimate[2]
balancetable[8,3] <- t$estimate[2] - t$estimate[1]
balancetable[8,4] <- t$p.value
colnames(balancetable) <- c("Mean Control", "Mean Treated", "Treated-Control", "p-value")
rownames(balancetable) <- c("Female", "Black", "ANC", "University Educated", "Lived in Informal Settlement", "Years in Politics", "Previously Threatened or Attacked", "Thinks we have too many foreigners")

#Municipal covariates
round2022_munmerged_treated <- subset(round2022_munmerged, round2022_munmerged$treated==1)
round2022_munmerged_control <- subset(round2022_munmerged, round2022_munmerged$treated==0)

balancetable2 <- matrix(nrow=9, ncol=4, NA)
t = t.test(round2022_munmerged_control$employed_perc2011, round2022_munmerged_treated$employed_perc2011)
balancetable2[1,1] <- t$estimate[1]
balancetable2[1,2] <- t$estimate[2]
balancetable2[1,3] <- t$estimate[2] - t$estimate[1]
balancetable2[1,4] <- t$p.value
t = t.test(round2022_munmerged_control$log_pop2016, round2022_munmerged_treated$log_pop2016)
balancetable2[2,1] <- t$estimate[1]
balancetable2[2,2] <- t$estimate[2]
balancetable2[2,3] <- t$estimate[2] - t$estimate[1]
balancetable2[2,4] <- t$p.value
t = t.test(round2022_munmerged_control$elec_perc2016, round2022_munmerged_treated$elec_perc2016)
balancetable2[3,1] <- t$estimate[1]
balancetable2[3,2] <- t$estimate[2]
balancetable2[3,3] <- t$estimate[2] - t$estimate[1]
balancetable2[3,4] <- t$p.value
t = t.test(round2022_munmerged_control$all_flush_perc2016, round2022_munmerged_treated$all_flush_perc2016)
balancetable2[4,1] <- t$estimate[1]
balancetable2[4,2] <- t$estimate[2]
balancetable2[4,3] <- t$estimate[2] - t$estimate[1]
balancetable2[4,4] <- t$p.value
t = t.test(round2022_munmerged_control$refuse_perc2016, round2022_munmerged_treated$refuse_perc2016)
balancetable2[5,1] <- t$estimate[1]
balancetable2[5,2] <- t$estimate[2]
balancetable2[5,3] <- t$estimate[2] - t$estimate[1]
balancetable2[5,4] <- t$p.value
t = t.test(round2022_munmerged_control$home_water_perc2016, round2022_munmerged_treated$home_water_perc2016)
balancetable2[6,1] <- t$estimate[1]
balancetable2[6,2] <- t$estimate[2]
balancetable2[6,3] <- t$estimate[2] - t$estimate[1]
balancetable2[6,4] <- t$p.value
t = t.test(round2022_munmerged_control$ANCcontrolled, round2022_munmerged_treated$ANCcontrolled)
balancetable2[7,1] <- t$estimate[1]
balancetable2[7,2] <- t$estimate[2]
balancetable2[7,3] <- t$estimate[2] - t$estimate[1]
balancetable2[7,4] <- t$p.value
t = t.test(round2022_munmerged_control$win_margin, round2022_munmerged_treated$win_margin)
balancetable2[8,1] <- t$estimate[1]
balancetable2[8,2] <- t$estimate[2]
balancetable2[8,3] <- t$estimate[2] - t$estimate[1]
balancetable2[8,4] <- t$p.value
t = t.test(round2022_munmerged_control$enumerated, round2022_munmerged_treated$enumerated)
balancetable2[9,1] <- t$estimate[1]
balancetable2[9,2] <- t$estimate[2]
balancetable2[9,3] <- t$estimate[2] - t$estimate[1]
balancetable2[9,4] <- t$p.value
colnames(balancetable2) <- c("Mean Control", "Mean Treated", "Treated-Control", "p-value")
rownames(balancetable2) <- c("Employment % (municipal)", "Log population (municipal)", "Electricity access % (municipal)", "Flush toilet % (municipal)", "Refuse removal % (municipal)", "Piped water % (municipal)", "ANC Majority (municipal)", "Majority size (municipal)", "Survey enumerated via phone/Zoom")

xtable(balancetable)
xtable(balancetable2)

## Figure 1
round2022$authoritarianism_scale_dummies <- round2022$ordervscitizensay_dummy + round2022$punishunrest_dummy + round2022$againstprotests_dummy + round2022$listentoleaders_dummy + round2022$efficacyvsinput_dummy + round2022$againstforeigners_dummy
par(mar=c(6,6,2,2))
hist(round2022$authoritarianism_scale_dummies, main="", xlab = "Authoritarianism Index", cex.lab=2, col = "darkgrey")

## Figure 2
round2022_ANC <- subset(round2022, round2022$ANC==1)
round2022_nonANC <- subset(round2022, round2022$ANC==0)

plot(density(na.omit(round2022_nonANC$authoritarianism_scale_dummies)), col = "black",
     main = "", xlab = "Authoritarianism Index")
lines(density(na.omit(round2022_ANC$authoritarianism_scale_dummies)), add=T, col = "black", lty=2)
legend("topright", lty=c(1,2), c("Opposition Party", "Ruling Party"))


## Table 2

#Main Specification
baseline_allcovars <- lm(authoritarianism_scale_dummies ~ treated + ANC + female + black
                         + universityeducated + lengthoftenure + informalsettlement + threatorattackfiveyears
                         + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                           refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_munmerged)
res <- summary(baseline_allcovars)
2*pt(-abs(coef(res)[, 3]), baseline_allcovars$df)[2] #these are the two-tailed test p values
pt(coef(res)[, 3], baseline_allcovars$df, lower = FALSE)[2] #these are the one-tailed test p values
res.HC2 <- coeftest(baseline_allcovars, vcov. = vcovHC(baseline_allcovars, type = "HC2")) #HC2 standard errors

#Extra Controls
round2022_munmerged$startmonth <- str_extract(round2022_munmerged$StartDate, "[0-9]+")
round2022_munmerged$communityprotest1 <- ifelse(round2022_munmerged$communityprotest==1, 1, 0)
round2022_munmerged$broker1 <- ifelse(round2022_munmerged$broker==1, 1, 0)
round2022_munmerged$inperson <- ifelse(round2022_munmerged$DistributionChannel=="gl", 1, 0)

allcontrols <- lm(authoritarianism_scale_dummies ~ treated + ANC + female + black
                  + universityeducated + lengthoftenure + informalsettlement + threatorattackfiveyears
                  + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016  
                  + refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin 
                  + I(startmonth) + communityprotest1 + broker1 + inperson, data = round2022_munmerged)
res_allcontrols <- summary(allcontrols)
2*pt(-abs(coef(res_allcontrols)[, 3]), allcontrols$df)[2] #these are the two-tailed test p values
pt(coef(res_allcontrols)[, 3], allcontrols$df, lower = FALSE)[2] #these are the one-tailed test p values
res.HC2 <- coeftest(allcontrols, vcov. = vcovHC(allcontrols, type = "HC2")) #HC2 standard errors

#No Controls
used_data <- model.frame(baseline_allcovars) 
nocontrols <- lm(authoritarianism_scale_dummies ~ treated, data = used_data)


## Figure 3
round2022_authoritarians <- subset(round2022_munmerged, round2022_munmerged$pretreatmentauthoritarian==2)
round2022_nonauthoritarians <- subset(round2022_munmerged, round2022_munmerged$pretreatmentauthoritarian<2)
round2022_black <- subset(round2022_munmerged, round2022_munmerged$black==1)
round2022_nonblack <- subset(round2022_munmerged, round2022_munmerged$black==0)
round2022_ANC <- subset(round2022_munmerged, round2022_munmerged$ANC==1)
round2022_nonANC <- subset(round2022_munmerged, round2022_munmerged$ANC==0)
round2022_DA <- subset(round2022_munmerged, round2022_munmerged$DA==1)
round2022_otherparty <- subset(round2022, round2022$ANC==0 & round2022$DA==0)
round2022_EFF <- subset(round2022, round2022$party=="Economic Freedom Fighters (EFF)")
round2022_ANCcontrolled <- subset(round2022_munmerged, round2022_munmerged$ANCcontrolled==1)
round2022_nonANCcontrolled <- subset(round2022_munmerged, round2022_munmerged$ANCcontrolled==0)
hetmod1 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
              + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_authoritarians) 
hetmod2 <- lm(authoritarianism_scale_dummies ~ treated + female + universityeducated + lengthoftenure
              + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_nonauthoritarians)
hetmod3 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
              + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_black) 
hetmod4 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
              + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_nonblack) 
hetmod5 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
              + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_ANC) 
hetmod6 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
              + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_nonANC) 
hetmod7 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
              + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_DA) 
hetmod8 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
              , data = round2022_EFF) 
hetmod9 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
              + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                refuse_perc2016 + home_water_perc2016 + win_margin, data = round2022_ANCcontrolled) 
hetmod10 <- lm(authoritarianism_scale_dummies ~ treated + female + black + universityeducated + lengthoftenure
               + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                 refuse_perc2016 + home_water_perc2016 + win_margin, data = round2022_nonANCcontrolled) 
plot(0, 0, type = "n", ylim=c(-0.1, 1.25), xlim=c(-0.75, 0.75),
     yaxt="n", ylab="", xlab = "Estimated Treatment Effect", cex.lab=1.25)
abline(v=0, lty=2)
points(x=coef(hetmod1)[2], y=1, pch=15)
arrows(confint(hetmod1)[2,1], 1, confint(hetmod1)[2,2], 1, lty = 1, lwd = 1, col = "black",
       angle = 90, length = 0.05, code = 3)
points(x=coef(hetmod2)[2], y=0.9, pch=17)
arrows(confint(hetmod2)[2,1], 0.9, confint(hetmod2)[2,2], 0.9, lty = 1, lwd = 1, col = "black",
       angle = 90, length = 0.05, code = 3)
points(x=coef(hetmod3)[2], y=.6, pch=15, col="blue")
arrows(confint(hetmod3)[2,1], .6, confint(hetmod3)[2,2], .6, lty = 1, lwd = 1, col = "blue",
       angle = 90, length = 0.05, code = 3)
points(x=coef(hetmod4)[2], y=0.5, pch=17, col="blue")
arrows(confint(hetmod4)[2,1], 0.5, confint(hetmod4)[2,2], 0.5, lty = 1, lwd = 1, col = "blue",
       angle = 90, length = 0.05, code = 3)
points(x=coef(hetmod5)[2], y=.2, pch=15, col="red")
arrows(confint(hetmod5)[2,1], .2, confint(hetmod5)[2,2], .2, lty = 1, lwd = 1, col = "red",
       angle = 90, length = 0.05, code = 3)
points(x=coef(hetmod6)[2], y=0.1, pch=17, col="red")
arrows(confint(hetmod6)[2,1], 0.1, confint(hetmod6)[2,2], 0.1, lty = 1, lwd = 1, col = "red",
       angle = 90, length = 0.05, code = 3)
legend("topleft", c("Authoritarians", "Non-Authoritarians",
                    "Blacks", "Non-Blacks",
                    "ANC", "Non-ANC"), 
       pch=c(15,17,15,17,15,17), col=c("black", "black", "blue", "blue", "red", "red"),
       ncol=1, cex=1.2, bty='n')
box(lty = 1, col = 'black')

### Appendix Tables 

## Table A1 - Post-stratification by party ID
round2022_munmerged$incomplete <- 0
round2022_munmerged$incomplete[which(is.na(round2022_munmerged$authoritarianism_scale))] <- 1
round2022_munmerged_complete <- subset(round2022_munmerged, round2022_munmerged$incomplete==0)
weightsANC <- ifelse(round2022_munmerged_complete$ANC==1, 1.53, 0.79)

# Column 1 - Main Specification
weighted_ANC <- lm(authoritarianism_scale_dummies ~ treated + ANC + female + black
                   + universityeducated + lengthoftenure + informalsettlement + threatorattackfiveyears
                   + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                     refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, weights = weightsANC, data = round2022_munmerged_complete)
res_ANCweights <- summary(weighted_ANC)
2*pt(-abs(coef(res_ANCweights)[, 3]), weighted_ANC$df)[2] #these are the two-tailed test p values
pt(coef(res_ANCweights)[, 3], weighted_ANC$df, lower = FALSE)[2] #these are the one-tailed test p values

# Column 2 - Extra controls
weighted_ANC_extracontrols <- lm(authoritarianism_scale_dummies ~ treated + ANC + female + black
                                 + universityeducated + lengthoftenure + informalsettlement + threatorattackfiveyears
                                 + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                                   refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin
                                 + I(startmonth) + communityprotest1 + broker1 + inperson, weights = weightsANC, data = round2022_munmerged_complete)
res_ANCweights_extracontrols <- summary(weighted_ANC_extracontrols)
2*pt(-abs(coef(res_ANCweights_extracontrols)[, 3]), weighted_ANC_extracontrols$df)[2] #these are the two-tailed test p values
pt(coef(res_ANCweights_extracontrols)[, 3], weighted_ANC_extracontrols$df, lower = FALSE)[2] #these are the one-tailed test p values


## Table A2 - Mediation Analysis

# Mediator = Fear
fit.totaleffect <- lm(authoritarianism_scale_dummies ~ treated, data = round2022_munmerged_complete)
fit.mediator <- lm(fearviolenceagainstyou ~ treated + black + ANC + female + threatorattackfiveyears + informalsettlement + universityeducated +lengthoftenure
                   + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                     refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_munmerged_complete)
fit.dv <- lm(authoritarianism_scale_dummies ~ treated + fearviolenceagainstyou + black + ANC + female + threatorattackfiveyears + informalsettlement + universityeducated + lengthoftenure
             + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
               refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_munmerged_complete)
results_mediation1 = mediate(fit.mediator, fit.dv, treat='treated', mediator='fearviolenceagainstyou', 
                             boot = T)
mediation1 <- summary(results_mediation1)

# Mediator = Ethnic Identification
fit.totaleffect <- lm(authoritarianism_scale_dummies ~ treated, data = round2022_munmerged_complete)
fit.mediator <- lm(ethnicidentification ~ treated + black + ANC + female + threatorattackfiveyears + informalsettlement + universityeducated +lengthoftenure
                   + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                     refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_munmerged_complete)
fit.dv <- lm(authoritarianism_scale_dummies ~ treated + ethnicidentification + black + ANC + female + threatorattackfiveyears + informalsettlement + universityeducated + lengthoftenure
             + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
               refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_munmerged_complete)
results_mediation2 = mediate(fit.mediator, fit.dv, treat='treated', mediator='ethnicidentification', 
                             boot = T)
summary(results_mediation2)


### Robustness Checks

# Results without anti-immigrant question (item 6)
round2022_munmerged$authoritarianism_scale_dummies_v2 <- round2022_munmerged$ordervscitizensay_dummy + round2022_munmerged$punishunrest_dummy + round2022_munmerged$againstprotests_dummy + round2022_munmerged$listentoleaders_dummy + round2022_munmerged$efficacyvsinput_dummy
baseline_allcovars_v2 <- lm(authoritarianism_scale_dummies_v2 ~ treated + ANC + female + black
                            + universityeducated + lengthoftenure + informalsettlement + threatorattackfiveyears
                            + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                              refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_munmerged)
res_v2 <- summary(baseline_allcovars_v2)
2*pt(-abs(coef(res_v2)[, 3]), baseline_allcovars_v2$df)[2] #these are the two-tailed test p values
pt(coef(res_v2)[, 3], baseline_allcovars_v2$df, lower = FALSE)[2] #these are the one-tailed test p values

# Results without unrest/insurrection question (item 2)
round2022_munmerged$authoritarianism_scale_dummies_v3 <- round2022_munmerged$ordervscitizensay_dummy + round2022_munmerged$againstprotests_dummy + round2022_munmerged$listentoleaders_dummy + round2022_munmerged$efficacyvsinput_dummy + round2022_munmerged$againstforeigners_dummy
baseline_allcovars_v3 <- lm(authoritarianism_scale_dummies_v3 ~ treated + ANC + female + black
                            + universityeducated + lengthoftenure + informalsettlement + threatorattackfiveyears
                            + employed_perc2011 + log_pop2016 + elec_perc2016 + all_flush_perc2016 + 
                              refuse_perc2016 + home_water_perc2016 + ANCcontrolled + win_margin, data = round2022_munmerged)
res_v3 <- summary(baseline_allcovars_v3)
2*pt(-abs(coef(res_v3)[, 3]), baseline_allcovars_v3$df)[2] #these are the two-tailed test p values
pt(coef(res_v3)[, 3], baseline_allcovars_v3$df, lower = FALSE)[2] #these are the one-tailed test p values

# Linear hypothesis tests for treatment heterogeneity
model <- lm(authoritarianism_scale_dummies ~ treated*ANCcontrolled, data = round2022_munmerged)
summary(model)
hypothesis_test <- linearHypothesis(model, "treated:ANCcontrolled = 0")
print(hypothesis_test)
