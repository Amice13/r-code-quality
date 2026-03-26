#### Defining Variables ####
## Loading packages
library(foreign)
library(psych)

## Loading data
data <- read.spss("CCES16_DKU_OUTPUT_Feb2017_unmatched.sav") # need to have wd set
randomization <- as.numeric(data$DKU309_310rand) - 1 # 1 is lopsided race

## poll legitimacy measures
accurate <- as.numeric(data$DKU311)
accurate[accurate == 6] <- NA
trustworthy <- as.numeric(data$DKU312)
trustworthy[trustworthy == 6] <- NA
informative <- as.numeric(data$DKU313)
informative[informative == 6] <- NA

## candidate enthusiasm prior to manipulation
clinton_enthus <- 6 - as.numeric(data$DKU306) # reverse coding 5pt item
trump_enthus <- 6 - as.numeric(data$DKU307)
johnson_enthus <- 6 - as.numeric(data$DKU308)

## Enthusiasm for 'most enthused' candidate (post-manipulation)
cand_enthus_post <- 6 - as.numeric(data$DKU316)
cand_enthus_post[cand_enthus_post == 0] <- NA

## manipulation check
pass_manip_check <- 
  ifelse((as.numeric(data$DKU309_310rand) == 2 & as.numeric(data$DKU314) == 4) | 
      (as.numeric(data$DKU309_310rand) == 1 & as.numeric(data$DKU314) == 2) |
      (as.numeric(data$DKU309_310rand) == 1 & as.numeric(data$DKU314) == 5), 1,0)
sum(pass_manip_check)/length(pass_manip_check)

## Demographics
pid7 <- as.numeric(data$pid7)
pid7[pid7 == 8 | pid7 == 9 | pid7 == 10] <- NA

ideo5 <- as.numeric(data$ideo5)
ideo5[ideo5 == 6] <- 3 # 'Not sure' as moderate
ideo5[ideo5 == 7] <- NA

educ <- as.numeric(data$educ) - 1

female <- as.numeric(data$gender) - 1

black <- ifelse(as.numeric(data$race) == 2,1,0)
hispanic <- ifelse(as.numeric(data$race) == 3,1,0)

age <- 2016 - as.numeric(as.character(data$birthyr))

pol_int <- 5 - as.numeric(data$newsint)
pol_int[pol_int == 0 | pol_int == -1] <- NA

income <- as.numeric(data$faminc)
income[income == 18] <- NA

# Forming Dataset with vars of interest
cces2016 <- data.frame(randomization, accurate, trustworthy, 
  informative, clinton_enthus, trump_enthus, johnson_enthus, pass_manip_check,
  cand_enthus_post, pid7, ideo5, educ, female, black,
  hispanic, age, pol_int, income)

#### Balance Checks - Table A9 ####
treatment <- subset(cces2016, cces2016$randomization == 1)
control <- subset(cces2016, cces2016$randomization == 0)

trt_demograph_means <- apply(as.matrix(treatment[ ,10:18]), 2, mean, na.rm = T)
ctrl_demograph_means <- apply(as.matrix(control[ ,10:18]), 2, mean, na.rm = T)

round(cbind(trt_demograph_means, ctrl_demograph_means),2)

# comparison of means
for(i in 10:18) {
  print(t.test(treatment[i], control[i])[3])
}
# significant difference for Political Interest (0.03)


#### Creating Candidate Preference Variable ####
summary(data$CC16_364b) # Pres vote - those who already voted (pre)
summary(data$CC16_364c) # Pres pref - vote intention

cces2016$pref_trump <- ifelse(as.numeric(data$CC16_364b) == 1 |
    as.numeric(data$CC16_364c) == 1, 1,0)
cces2016$pref_clinton <- ifelse(as.numeric(data$CC16_364b) == 2 |
    as.numeric(data$CC16_364c) == 2, 1,0)
cces2016$pref_johnson <- ifelse(as.numeric(data$CC16_364b) == 3 |
    as.numeric(data$CC16_364c) == 3, 1,0)


#### Creating Poll Legitimacy Variable ####
# poll legitimacy taken as average over three measured dimensions
cces2016$poll_legitimacy <- rep(0, length(cces2016$accurate))
for (i in 1:length(cces2016$accurate)) { #written with conditionals to deal with NAs
  divisor <- 0 # keeps track of denominator when calculating average
  
  if (!is.na(cces2016$accurate[i])) {
    cces2016$poll_legitimacy[i] <- cces2016$poll_legitimacy[i] + cces2016$accurate[i]
    divisor <- divisor + 1
  }
  if (!is.na(cces2016$trustworthy[i])) {
    cces2016$poll_legitimacy[i] <- cces2016$poll_legitimacy[i] + cces2016$trustworthy[i]
    divisor <- divisor + 1
  }
  if(!is.na(cces2016$informative[i])) {
    cces2016$poll_legitimacy[i] <- cces2016$poll_legitimacy[i] + cces2016$informative[i]
    divisor <- divisor + 1
  }
  
  cces2016$poll_legitimacy[i] <- cces2016$poll_legitimacy[i] / divisor #average of rated dimensions
}

hist(cces2016$poll_legitimacy, xlab = "Poll Legitimacy",
  main = "Poll Legitimacy - CCES")

# normalize variable
cces2016$poll_legitimacy_norm <- (cces2016$poll_legitimacy - min(cces2016$poll_legitimacy)) / 
  (max(cces2016$poll_legitimacy) - min(cces2016$poll_legitimacy))

# Reliability of scale
poll_df <- data.frame(cces2016$accurate, cces2016$trustworthy, cces2016$informative)
psych::alpha(poll_df)

#### Creating Change in Enthusiasm Pre & Post ####
# Measuring enthusiasm change
cces2016$enthus_change <- rep(NA, length(cces2016$clinton_enthus))

# positive value indicates increased enthusiasm post-manipulation
for (i in 1:length(cces2016$clinton_enthus)) { 
  cces2016$enthus_change[i] <- cces2016$cand_enthus_post[i] - 
    max(cces2016$clinton_enthus[i],cces2016$trump_enthus[i],cces2016$johnson_enthus[i])
}

hist(cces2016$enthus_change, xlab = "Change in Enthusiasm",
  main = "Enthusiasm Change Pre to Post Manipulation - CCES")


## Split dataset on two conditions
treatment <- subset(cces2016, cces2016$randomization == 1)
control <- subset(cces2016, cces2016$randomization == 0)

#### Table A10 
round(matrix(c(
  
  # Control Clinton Support
  length(which((control$accurate[control$pref_clinton == 1] >= 4))) /
    length(control$accurate[control$pref_clinton == 1]),
  length(which((control$trustworthy[control$pref_clinton == 1] >= 4))) /
    length(control$trustworthy[control$pref_clinton == 1]),
  length(which((control$informative[control$pref_clinton == 1] >= 4))) /
    length(control$informative[control$pref_clinton == 1]),
  length(which(control$pref_clinton == 1)),
  
  # Control Trump Support
  length(which((control$accurate[control$pref_trump == 1] >= 4))) /
    length(control$accurate[control$pref_trump == 1]),
  length(which((control$trustworthy[control$pref_trump == 1] >= 4))) /
    length(control$trustworthy[control$pref_trump == 1]),
  length(which((control$informative[control$pref_trump == 1] >= 4))) /
    length(control$informative[control$pref_trump == 1]),
  length(which(control$pref_trump == 1)),
  
  # Treatment Clinton Support
  length(which((treatment$accurate[treatment$pref_clinton == 1] >= 4))) /
    length(treatment$accurate[treatment$pref_clinton == 1]),
  length(which((treatment$trustworthy[treatment$pref_clinton == 1] >= 4))) /
    length(treatment$trustworthy[treatment$pref_clinton == 1]),
  length(which((treatment$informative[treatment$pref_clinton == 1] >= 4))) /
    length(treatment$informative[treatment$pref_clinton == 1]),
  length(which(treatment$pref_clinton == 1)),
  
  # Treatment Trump Support
  length(which((treatment$accurate[treatment$pref_trump == 1] >= 4))) /
    length(treatment$accurate[treatment$pref_trump == 1]),
  length(which((treatment$trustworthy[treatment$pref_trump == 1] >= 4))) /
    length(treatment$trustworthy[treatment$pref_trump == 1]),
  length(which((treatment$informative[treatment$pref_trump == 1] >= 4))) /
    length(treatment$informative[treatment$pref_trump == 1]),
  length(which(treatment$pref_trump == 1))
  
), ncol = 4, nrow = 4, byrow = F),2)


# Control condition, entire sample
round(matrix(c(
  
  length(which((control$accurate >= 4))) /
    length(control$accurate),
  length(which((control$trustworthy >= 4))) /
    length(control$trustworthy),
  length(which((control$informative >= 4))) /
    length(control$informative)
  
), ncol = 1, nrow = 3, byrow = F),2)

#### Models - Attitude Change ####
# w/out controls -- Table A16
m2.1 <- lm(enthus_change ~ pref_trump + pref_clinton, data = treatment)
m2.2 <- lm(enthus_change ~ pref_trump + pref_clinton, data = control)

summary(m2.1)
summary(m2.2)

# w/controls -- Table A12
m3.1 <- lm(enthus_change ~ pref_trump + pref_clinton + pol_int + pid7 + ideo5 +
    educ + female + black + hispanic, data = treatment)
m3.2 <- lm(enthus_change ~ pref_trump + pref_clinton + pol_int + pid7 + ideo5 +
    educ + female + black + hispanic, data = control)

summary(m3.1)
summary(m3.2)

# test with dropped pid & ideo -- Table A15
m3.1_npi <- lm(enthus_change ~ pref_trump + pref_clinton + pol_int + 
    educ + female + black + hispanic, data = treatment)
m3.2_npi <- lm(enthus_change ~ pref_trump + pref_clinton + pol_int + 
    educ + female + black + hispanic, data = control)

summary(m3.1_npi)
summary(m3.2_npi)

# for only those who passed the manipulation check
treat_pass <- subset(treatment, treatment$pass_manip_check == 1)
ctrl_pass <- subset(control, control$pass_manip_check == 1)

summary(lm(enthus_change ~ pref_trump + pref_clinton, data = treat_pass))
summary(lm(enthus_change ~ pref_trump + pref_clinton, data = ctrl_pass))
summary(lm(enthus_change ~ pref_trump + pref_clinton + pol_int + pid7 + 
    educ + black + hispanic, data = treat_pass))
summary(lm(enthus_change ~ pref_trump + pref_clinton + pol_int + pid7 + 
    educ + black + hispanic, data = ctrl_pass))


#### Figure 5 ####
plot(1:4, c(coef(m3.1)[2], coef(m3.1)[3],
  coef(m3.2)[2], coef(m3.2)[3]), axes = F,
  ylab = "β Coefficient", xlab = "", cex = 1.3,
  pch = c(rep(c(16,15),2)), ylim = c(-.25,1), xlim = c(.5,4.5))

segments(1, confint(m3.1)[2,1], 1, confint(m3.1)[2,2])
segments(3, confint(m3.2)[2,1], 3, confint(m3.2)[2,2])
segments(2, confint(m3.1)[3,1], 2, confint(m3.1)[3,2])
segments(4, confint(m3.2)[3,1], 4, confint(m3.2)[3,2])

axis(1, at = c(1.3,3.6), c("Lopsided Condition", "Close Condition"), tick = F)
axis(2, at=c(-.25, 0,.25,.5,.75,1))

abline(h = 0, lty = 2)
abline(v=2.5)

legend('topright', c("Prior: Trump", "Prior: Clinton"),
  pch = c(16,15))

## desc stat for enthus change - note that 0.5 represents no change -- part of A10
mean((cces2016$enthus_change - min(cces2016$enthus_change, na.rm = T)) / 
    (max(cces2016$enthus_change, na.rm = T) - min(cces2016$enthus_change, na.rm = T)), na.rm = T)
mean((treatment$enthus_change - min(treatment$enthus_change, na.rm = T)) / 
    (max(treatment$enthus_change, na.rm = T) - min(treatment$enthus_change, na.rm = T)), na.rm = T)
mean((control$enthus_change - min(control$enthus_change, na.rm = T)) / 
    (max(control$enthus_change, na.rm = T) - min(control$enthus_change, na.rm = T)), na.rm = T)

## Exploring Floor / Ceiling effects of attitude polarization
length(which(cces2016$clinton_enthus == 5)) # 259 / 1448
length(which(cces2016$trump_enthus == 5)) # 224 / 1448
(224+259) / 1448 # a third of our data already at ceiling from pre measure

length(which(cces2016$clinton_enthus == 5 & cces2016$cand_enthus_post == 5)) # 220
length(which(cces2016$trump_enthus == 5 & cces2016$cand_enthus_post == 5)) # 198
# only 65 of those cases moved towards less enthusiasm

# since expectation is directional towards positive, lets drop all pre 5's since they are biasing estimates to see what happens
new_data <- subset(cces2016, cces2016$clinton_enthus < 5 & cces2016$trump_enthus < 5 & cces2016$johnson_enthus < 5)
# drops 507 responses (over 1/3 data)

summary(lm(enthus_change ~ pref_trump + pref_clinton, data = new_data))
length(which(new_data$pref_trump == 0 & new_data$pref_clinton == 0)) # n = 315 for base case

new_data_t <- subset(new_data, randomization == 1)
new_data_c <- subset(new_data, randomization == 0)

summary(lm(enthus_change ~ pref_trump + pref_clinton + pol_int + pid7 + ideo5 +
    educ + female + black + hispanic, data = new_data_t))
summary(lm(enthus_change ~ pref_trump + pref_clinton + pol_int + pid7 + ideo5 +
    educ + female + black + hispanic, data = new_data_t))


#### Models - Poll Perception ####
# subset out those with no prior opinion/preference for Trump or Clinton
treatment <- subset(treatment, treatment$pref_clinton == 1 | treatment$pref_trump == 1)
control <- subset(control, control$pref_clinton == 1 | control$pref_trump == 1)

# w/out controls -- Table A14
m0.1_nbl <- lm(poll_legitimacy ~  pref_trump, data = treatment)
m0.2_nbl <- lm(poll_legitimacy ~  pref_trump, data = control)

summary(m0.1_nbl)
summary(m0.2_nbl)

# w/controls -- Table A11
m1.1_nbl <- lm(poll_legitimacy ~ pref_trump + pol_int + pid7 + ideo5 +
    educ + female + black + hispanic, data = treatment)
m1.2_nbl <- lm(poll_legitimacy ~ pref_trump + pol_int + pid7 + ideo5 +
    educ + female + black + hispanic, data = control)

summary(m1.1_nbl)
summary(m1.2_nbl)

#### Figure 4 ####
scalar <- 2.6 # adjust everything for better interpretation of coefficients

par(las = 1)
plot(rev(scalar*coef(m1.2_nbl)[2:9]), 1:8, axes = F,
  ylab = "", xlab = "Coefficient Value", cex = 1.3, pch = 16,
  ylim = c(.5,8.5), xlim = c(-4,13))

points(rev(scalar*coef(m1.1_nbl)[2:9] + 9), 1:8, pch = 16, cex = 1.3)

segments(rev(scalar*confint(m1.2_nbl)[2:9,1]), 1:8, rev(scalar*confint(m1.2_nbl)[2:9,2]), 1:8)
segments(rev(scalar*confint(m1.1_nbl)[2:9,1] + 9), 1:8, rev(scalar*confint(m1.1_nbl)[2:9,2] + 9), 1:8)

axis(1, at=c(-3,-2,-1,0,1,2,3,6,7,8,9,10,11,12),
  c("-3","-2","-1","0","1","2","3","-3","-2","-1","0","1","2","3"))

axis(2, at = seq(from = 1, to = 8, by = 1), rev(c("Prior: Trump", "Pol. Interest",
  "Party ID", "Ideology", "Education", "Female", "Black", "Hispanic")), tick = F,
  pos = -2.75)


abline(v = 0, lty = 2)
abline(v = 9, lty = 2)
abline(v = 4.5)

mtext("Close Condition", side = 3, at = 0)
mtext("Lopsided Condition", side = 3, at = 9)


## drop ideo and pid from analysis -- Table A13
m1.1_npi <- lm(poll_legitimacy ~ pref_trump + pol_int +
    educ + female + black + hispanic, data = treatment)
m1.2_npi <- lm(poll_legitimacy ~ pref_trump + pol_int + 
    educ + female + black + hispanic, data = control)

summary(m1.1_npi)
summary(m1.2_npi)

## check robustness by making pid the new moderator -- Table A17
m1.1_party <- lm(poll_legitimacy ~ pid7 + pol_int +
    educ + female + black + hispanic, data = treatment)
m1.2_party <- lm(poll_legitimacy ~ pid7 + pol_int + 
    educ + female + black + hispanic, data = control)

summary(m1.1_party)
summary(m1.2_party)

## descriptive stats for key IDVs/DVs (rescale 0-1)
# note that these numbers are with respect to Johnson & non-supporters omitted
desc_stats <- matrix(c(
  mean((cces2016$accurate[cces2016$pref_clinton == 1 | cces2016$pref_trump == 1] - min(cces2016$accurate, na.rm = T)) /
      (max(cces2016$accurate, na.rm = T) - min(cces2016$accurate, na.rm = T)), na.rm = T),
  mean((treatment$accurate - min(treatment$accurate, na.rm = T)) /
      (max(treatment$accurate, na.rm = T) - min(treatment$accurate, na.rm = T)), na.rm = T),
  mean((control$accurate - min(control$accurate, na.rm = T)) /
      (max(control$accurate, na.rm = T) - min(control$accurate, na.rm = T)), na.rm = T),
  
  mean((cces2016$trustworthy[cces2016$pref_clinton == 1 | cces2016$pref_trump == 1] - min(cces2016$trustworthy, na.rm = T)) /
      (max(cces2016$trustworthy, na.rm = T) - min(cces2016$trustworthy, na.rm = T)), na.rm = T),
  mean((treatment$trustworthy - min(treatment$trustworthy, na.rm = T)) /
      (max(treatment$trustworthy, na.rm = T) - min(treatment$trustworthy, na.rm = T)), na.rm = T),
  mean((control$trustworthy - min(control$trustworthy, na.rm = T)) /
      (max(control$trustworthy, na.rm = T) - min(control$trustworthy, na.rm = T)), na.rm = T),
  
  mean((cces2016$informative[cces2016$pref_clinton == 1 | cces2016$pref_trump == 1] - min(cces2016$informative, na.rm = T)) /
      (max(cces2016$informative, na.rm = T) - min(cces2016$informative, na.rm = T)), na.rm = T),
  mean((treatment$informative - min(treatment$informative, na.rm = T)) /
      (max(treatment$informative, na.rm = T) - min(treatment$informative, na.rm = T)), na.rm = T),
  mean((control$informative - min(control$informative, na.rm = T)) /
      (max(control$informative, na.rm = T) - min(control$informative, na.rm = T)), na.rm = T),
  
  mean(cces2016$pref_clinton [cces2016$pref_clinton == 1 | cces2016$pref_trump == 1], na.rm = T),
  mean(treatment$pref_clinton, na.rm = T),
  mean(control$pref_clinton, na.rm = T),
  
  mean(cces2016$pref_trump [cces2016$pref_clinton == 1 | cces2016$pref_trump == 1], na.rm = T),
  mean(treatment$pref_trump, na.rm = T),
  mean(control$pref_trump, na.rm = T)
), ncol = 5, byrow = F)

desc_stats <- round(desc_stats, 2)
row.names(desc_stats) <- c("full","treat","control")
colnames(desc_stats) <- c("accurate", "trust", "inform","pref clinton", "pref trump")

# scale combined
mean((cces2016$poll_legitimacy[cces2016$pref_clinton == 1 | cces2016$pref_trump == 1] - min(cces2016$poll_legitimacy, na.rm = T)) /
    (max(cces2016$poll_legitimacy, na.rm = T) - min(cces2016$poll_legitimacy, na.rm = T)), na.rm = T)
mean((treatment$poll_legitimacy - min(treatment$poll_legitimacy, na.rm = T)) /
    (max(treatment$poll_legitimacy, na.rm = T) - min(treatment$poll_legitimacy, na.rm = T)), na.rm = T)
mean((control$poll_legitimacy - min(control$poll_legitimacy, na.rm = T)) /
    (max(control$poll_legitimacy, na.rm = T) - min(control$poll_legitimacy, na.rm = T)), na.rm = T)
