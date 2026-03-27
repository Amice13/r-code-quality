#######Evaluations of Violence at the Polls: Civilian Victimization and Support for Perpetrators after War#######
#######Gabriella Levy#######
#######Journal of Politics#######

#######Data Preparation#######

#packages
library(cjoint) #will also need library(cregg) but override AMCE function in cjoint so attach and detach each use of cregg
library(stats19) 
library(dplyr)
library(Amelia)
library(plm)
library(scales)
library(foreign)
library(lmtest)
library(ggplot2)

#import
df <- read.csv("GLevy_JOP_Data.csv")

#establish baselines for conjoint AMCE analysis
baselines <- list()
baselines$Age <- "25"
baselines$Gender <- "Man"
baselines$"Education Level" <- "Primary School"
baselines$Recruitment <- "Ideology"
baselines$"Reputation According to the Troops" <- "Good Example"
baselines$"Military Success" <- "Won almost all battles"
baselines$Crime <- "Killing"
baselines$"Target of the Crime" <- "Enemy Informants"
baselines$"Type of Involvement in the Crime" <- "Decided to commit on his/her own"
baselines$"Attitude Toward the Crime" <- "Reluctant"

#establish factor order so conjoint levels in correct order
df$Age <- factor(df$Age, levels=c("25", "47", "68"))
df$Gender <- factor(df$Gender, levels=c("Man", "Woman"))
df$Education.Level <- factor(df$Education.Level, levels= c("Primary School", "Secondary School", "Associate Degree", "University"))
df$Recruitment <- factor(df$Recruitment, levels= c("Ideology", "A Job", "Forced Recruitment"))
df$Reputation.According.to.the.Troops <- factor(df$Reputation.According.to.the.Troops, levels= c("Good Example", "Kind", "Power-hungry", "Unjust"))
df$Military.Success <- factor(df$Military.Success, levels=c("Won almost all battles", "Won half of battles", "Won few battles"))
df$Crime <- factor(df$Crime, levels=c("Killing", "Sexual Violence"))
df$Target.of.the.Crime <- factor(df$Target.of.the.Crime, levels=c("Enemy Informants", "People living in an area that supported the enemy ", "People living in an area controlled by the enemy"))
df$Type.of.Involvement.in.the.Crime <- factor(df$Type.of.Involvement.in.the.Crime, levels=c("Decided to commit on his/her own", "Did not prevent someone else from committing", "Followed an order to commit", "Ordered someone else to commit"))
df$Attitude.Toward.the.Crime <- factor(df$Attitude.Toward.the.Crime, levels=c("Reluctant", "Enthusiastic"))

##make non-conjoint variables numeric
df$ResAge <- as.numeric(as.character(df$ResAge))
df$ResGender <- as.numeric(as.character(df$ResGender))
df$Edu <- as.numeric(as.character(df$Edu))
df$Ideology <- as.numeric(as.character(df$Ideology))
df$Income <- as.numeric(as.character(df$Income))
df$Ideology <- as.numeric(as.character(df$Ideology))
df$Urban <- as.numeric(as.character(df$Urban))
df$CivilianAttitude <- as.numeric(as.character(df$CivilianAttitude))
df$ExposureVictimizatio <- as.numeric(as.character(df$ExposureVictimizatio))
df$Duque <- ifelse(df$WhoVote2018==1, 1, 0)
df[df$Género==3] <- NA #option of prefer not to say

#take out . from conjoint column names with multiple words
colnames(df)[colnames(df)=="Education.Level"] <- "Education Level"
colnames(df)[colnames(df)=="Reputation.According.to.the.Troops"] <- "Reputation According to the Troops"
colnames(df)[colnames(df)=="Military.Success"] <- "Military Success"
colnames(df)[colnames(df)=="Target.of.the.Crime"] <- "Target of the Crime"
colnames(df)[colnames(df)=="Type.of.Involvement.in.the.Crime"] <- "Type of Involvement in the Crime"
colnames(df)[colnames(df)=="Attitude.Toward.the.Crime"] <- "Attitude Toward the Crime"

#remove conjoint non-responses
FC <- df[!(is.na(df$selected_FC)),] #removes FC NA (1488 didn't answer i.e. 11.7 percent)
Rating <- df[!(is.na(df$selected_Rating)),] #removes Rating NA (832 NAs i.e. 6.5%)

####Main Text Figure 2 + Appendices 1-3: Conjoint Results####

#Figure 2 (Main) + Table A.1: FC Results
FC_Main <- amce(selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FC, cluster=TRUE, respondent.id="Response.ID", baselines=baselines)
summary(FC_Main) 

fg2 <- plot(FC_Main, xlab="Change in Pr(Prefer Candidate)", xlim=c(-.2,.2), ci=.95, colors="black", group.order= c("Age", "Gender", "Education Level","Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime"), text.size=13)
ggsave(file="fg2.eps", width=11, height=12.5, dpi=300)

#Table A.1 + Figure A.1: Rating Results
Ratings_Main <- amce(selected_Rating_rescaled ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= Rating, cluster=TRUE, respondent.id="Response.ID", baselines=baselines)
summary(Ratings_Main)

pdf("Ratings_Main.pdf", width=11, height=12.5) 
plot(Ratings_Main, xlab="Change in Rating (0 `very unlikely` to 1 `very likely`)", xlim=c(-.1, .1), ci=.95, colors="black", group.order= c("Age", "Gender", "Education Level","Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime"), text.size=13)
dev.off()

#Figures A.2 and A.3: FC and Rating Regressions with Altered Baselines

baselines3 <- list()
baselines3$"Military Success" <- "Won almost all battles"
baselines3$"Education Level" <- "Primary School"
baselines3$"Target of the Crime" <- "People living in an area controlled by the enemy"
baselines3$Recruitment <- "A Job"
baselines3$"Type of Involvement in the Crime" <- "Did not prevent someone else from committing"
baselines3$"Attitude Toward the Crime" <- "Reluctant"

FC_supplemental <- amce(selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FC, cluster=TRUE, respondent.id="Response.ID", baselines=baselines3)
summary(FC_supplemental)

pdf("FC_Supplemental.pdf", width=11, height=12.5) 
plot(FC_supplemental, xlab="Change in Pr(Prefer the Candidate)", xlim=c(-.2,.2), ci=.95, group.order= c("Age", "Gender", "Education Level","Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime"), colors="black", text.size=13)
dev.off()

#bonferroni correction for target: because three hypotheses in PAP, combine p-values from main and altered baseline regressions
p.adjust(c(0.004498009284807844704312529415801691357046365737915039062500000000000, 0.005069750867685829040509837284389504930004477500915527343750000000000, 0.892392845266234302492591723421355709433555603027343750000000000000000 ), method="bonferroni", n=3)

Rating_supplemental <- amce(selected_Rating_rescaled ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= Rating, cluster=TRUE, respondent.id="Response.ID", baselines=baselines3)
summary(Rating_supplemental)

pdf("Rating_Supplemental.pdf", width=11, height=12.5) 
plot(Rating_supplemental, xlab="Change in Rating (0 `very unlikely` to 1 `very likely`)", xlim=c(-.1, .1), ci=.95, group.order= c("Age", "Gender", "Education Level","Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime"), colors="black", text.size=13)
dev.off()

#Figure A.4 and A.5: Marginal Means (instead of AMCEs)
library(cregg)

MM_Main_FC <- cj(FC, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="mm", h0 = 0.5, level_order=c("descending"))
MM_Main_FC

pdf("MM_Main_FC.pdf", width=11, height=12.5) 
plot(MM_Main_FC, xlab="Marginal Mean, Forced Choice", vline=.5, ci=.95) + scale_color_manual(values = rep("black", 10)) + 
  theme_grey() + theme(legend.position="none") + theme(axis.text.y = element_text(size = 13))
dev.off()

MM_Main_Rating <- cj(Rating, selected_Rating_rescaled ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="mm", h0 = 0.375, level_order=c("descending"))
MM_Main_Rating

pdf("MM_Main_Rating.pdf", width=11, height=12.5) 
plot(MM_Main_Rating, xlab="Marginal Mean, Rating (0-1)", ci=.95, vline=.375) + scale_color_manual(values = rep("black", 10)) + 
  theme_grey() + theme(legend.position="none") + theme(axis.text.y = element_text(size = 13))
dev.off()

####Appendix 5: Diagnostic Checks, Done on Forced Choice####

#Figure A.6, carryover effects
FC$task <- as.numeric(FC$task)
facet.levels1 <- list()
facet.levels1[["task"]] <- c(1,2,3,4) 
FC$task <- as.factor(as.numeric(FC$task))
FC_Carryover <- cj(selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FC, id=~Response.ID, by=~task, estimate="mm", h0 = 0.5, level_order=c("descending"))
FC_Carryover 

pdf("Carryover effects.pdf", width=12.5, height=11)
plot(FC_Carryover, xlab="MM", vline=.5, group="task") + theme_grey() + theme(legend.position="bottom") + theme(axis.text.y = element_text(size = 13))
dev.off()

carryover_ftest <- cj_anova(FC, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ task)
carryover_ftest #not significant

#Figure A.7, Profile Order

FC$profile <- as.numeric(FC$profile)
facet.levels2 <- list()
facet.levels2[["profile"]] <- c(1,2)
FC$profile <- as.factor(as.numeric(FC$profile))
FC_Profile <- cj(selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FC, id=~Response.ID, by=~profile, h0 = 0.5, estimate="mm", level_order=c("descending"))
FC_Profile

pdf("Profile Order.pdf", width=11, height=12.5)
plot(FC_Profile, xlab="MM", group="profile", vline=.5) + theme_grey() + theme(legend.position="bottom") + theme(axis.text.y = element_text(size = 13))
dev.off()

profile_ftest <- cj_anova(FC, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ profile)
profile_ftest #not significant

detach(package:cregg)

#Table A.2, Randomization Check
FCa <- FC[!(is.na(FC$ResGender)),] #removes those who left their gender blank
FC_Randomization_Gender <- amce(ResGender ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FCa, cluster=TRUE, respondent.id="Response.ID", baselines=baselines)
summary(FC_Randomization_Gender)

FC$Edubinary <- ifelse(FC$Edu <=7, 0, 1)
FCc <- FC[!(is.na(FC$Edubinary)),]
FC_Randomization_Edu <- amce(Edubinary ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FCc, cluster=TRUE, respondent.id="Response.ID", baselines=baselines)
summary(FC_Randomization_Edu)

FC$Incomebinary <- ifelse(FC$Income <=10, 0, 1)
FCb <- FC[!(is.na(FC$Incomebinary)),]
FC_Randomization_Income <- amce(Incomebinary ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FCb, cluster=TRUE, respondent.id="Response.ID", baselines=baselines)
summary(FC_Randomization_Income)

FC$Urbanbinary <- ifelse(FC$Urban <=4, 0, 1)
FCd <- FC[!(is.na(FC$Urbanbinary)),]
FC_Randomization_Urban <- amce(Urbanbinary ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FCd, cluster=TRUE, respondent.id="Response.ID", baselines=baselines)
summary(FC_Randomization_Urban)

#Figure A.8, Atypical Profiles (excluding female rapists)

FCe <- FC[!(FC$Gender=="Woman" & FC$Crime=="Sexual Violence"),]
FCe$Response.IDtask <- paste(FCe$Response.ID, FCe$task)
FCe$Response.IDtask <- gsub("[[:space:]]", "", FCe$Response.IDtask)
FCf <- subset(FCe,duplicated(Response.IDtask) | duplicated(Response.IDtask, fromLast=TRUE))

FC_Atypical <- amce(selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, data= FCf, cluster=TRUE, respondent.id="Response.ID", baselines=baselines)
summary(FC_Atypical)

pdf("Atypical Profiles.pdf", width=11, height=12.5)
plot(FC_Atypical, color="black",  xlim= c(-.2, .2) ,xlab="Change in Pr(Prefer the Candidate)", ci=.95, group.order=c("Age", "Gender", "Education Level","Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime"), text.size=13)
dev.off()

####Appendix 6: Non-Response Bias and Imputed Results####

#Table A.3 -- All Survey Respondents Column
options(digits=5)

hist(df$ResAge)
summary(df$ResAge) #avg birth year 1983

hist(df$Edu)
summary(df$Edu) #mean 6.5

hist(df$Income)
summary(df$Income) #average 9.8

hist(df$Urban)
summary(df$Urban) #mean 3.6

hist(df$Ideology)
summary(df$Ideology) #5.8

df$Vote2018 <- as.numeric(as.character(df$Vote2018))
summary(df$Vote2018) #82%

df$ExposureVictimizatio <- as.numeric(as.character(df$ExposureVictimizatio))
summary(df$ExposureVictimizatio) #.34

#Appendix Table 3, FC Non-Response

df$FC_na <- ifelse(is.na(df$selected_FC), 1, 0)
summary(df$FC_na)
FCNA <- df[which(df$FC_na==1),] #exclusively people that didn't answer the FC

summary(FCNA$ResAge)#mean 1981

summary(FCNA$Edu) #Mean 5.95

summary(FCNA$Income) #mean 8.6

summary(FCNA$Urban) #mean 3.5

summary(FCNA$Ideology) #mean 6

summary(FCNA$Vote2018) #.71

summary(FCNA$ExposureVictimizatio) #.31

t.test(FCNA$ResAge, df$ResAge) #significant
t.test(FCNA$Edu, df$Edu) #significant
t.test(FCNA$Income, df$Income) #significant
t.test(FCNA$Urban, df$Urban) #significant
t.test(FCNA$Ideology, df$Ideology) #significant
t.test(FCNA$Vote2018, df$Vote2018) #significant
t.test(FCNA$ExposureVictimizatio, df$ExposureVictimizatio) #not significant

p.adjust(c(1.8e-08, 2e-16, 9.9e-13, 3.7e-06, 0.005, 2.8e-12, .083), method = "bonferroni", n = 7) #not significant (Victimization)

#Appendix Table 3, Ratings Non Response

df$Rating_na <- ifelse(is.na(df$selected_Rating), 1, 0)
summary(df$Rating_na)
ratingNA <- df[which(df$Rating_na==1),] #exclusively people that didn't answer the ratings (6.5 percent)

summary(ratingNA$ResAge) #mean 1981

summary(ratingNA$Edu) #Mean 5.89

summary(ratingNA$Income) #mean 8.4

summary(ratingNA$Urban) #3.4

summary(ratingNA$Ideology) #mean 6.1

summary(ratingNA$Vote2018) #67

summary(ratingNA$ExposureVictimizatio) #mean .31

t.test(ratingNA$ResAge, df$ResAge) #significant
t.test(ratingNA$Edu, df$Edu) #significant
t.test(ratingNA$Income, df$Income) #significant
t.test(ratingNA$Urban, df$Urban) #significant
t.test(ratingNA$Ideology, df$Ideology) #significant
t.test(ratingNA$Vote2018, df$Vote2018) #significant
t.test(ratingNA$ExposureVictimizatio, df$ExposureVictimizatio) #not significant

p.adjust(c(3e-04, 2e-16, 2.2e-09, 5.1e-05, .012,1.2e-06, .22), method = "bonferroni", n = 7) #exposure victimization not significant

#Appendix Table 3, Rating of 1

Rating1 <- Rating[which(Rating$selected_Rating==1),]

summary(Rating1$ResAge) #mean 1981

summary(Rating1$Edu) #mean 6.8

summary(Rating1$Income)#mean 10.3

summary(Rating1$Urban) #mean 3.7

summary(Rating1$Ideology) #mean 5.8

summary(Rating1$Vote2018) #.84

summary(Rating1$ExposureVictimizatio) #.33

t.test(Rating1$ResAge, df$ResAge) #significant
t.test(Rating1$Edu, df$Edu) #significant
t.test(Rating1$Income, df$Income) #significant
t.test(Rating1$Urban, df$Urban) #significant
t.test(Rating1$Ideology, df$Ideology) #not significant
t.test(Rating1$Vote2018, df$Vote2018) #not significant
t.test(Rating1$ExposureVictimizatio, df$ExposureVictimizatio) #not significant

p.adjust(c(4.6e-15, 2e-16, 1.1e-05, .00024, .15, .03, .29), method = "bonferroni", n = 7) #ideology, vote, exposure victimization not significant

#Table A.4: Multiple Imputation w Amelia Package, Forced Choice
set.seed(123)
vars_small <- c("Response.ID", "ResAge", "ResGender", "Income", "Urban", "Ideology", "CivilianAttitude", "ExposureVictimizatio","selected_FC","Age", "Gender", "Education Level", "Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime")
df_small <- df[vars_small]
to_impute <- df_small %>% filter_all(any_vars(complete.cases(.)))

imputed_fc = amelia(to_impute, m=5, idvars=c("Response.ID", "Age", "Gender", "Education Level", "Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime")) #this is the actual imputation (5 times)
overimpute(imputed_fc, var = "selected_FC")
summary(imputed_fc)

imp_fc_1 <- imputed_fc$imputations[[1]] #Cleaning of each imputed dataset
imp_fc_1 <- imp_fc_1[!is.na(imp_fc_1$selected),] #had to remove rows that couldn't be imputed because respondents answered no questions
imp_fc_2 <- imputed_fc$imputations[[2]]
imp_fc_2 <- imp_fc_2[!is.na(imp_fc_2$selected),]
imp_fc_3 <- imputed_fc$imputations[[3]]
imp_fc_3 <- imp_fc_3[!is.na(imp_fc_3$selected),]
imp_fc_4 <- imputed_fc$imputations[[4]]
imp_fc_4 <- imp_fc_4[!is.na(imp_fc_4$selected),]
imp_fc_5 <- imputed_fc$imputations[[5]]
imp_fc_5 <- imp_fc_5[!is.na(imp_fc_5$selected),]

library(cregg)
imp_fc_1_cj <- cj(imp_fc_1, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending")) #each of the five regressions, one on each imputed dataset
imp_fc_2_cj <- cj(imp_fc_2, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))
imp_fc_3_cj <- cj(imp_fc_3, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))
imp_fc_4_cj <- cj(imp_fc_4, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))
imp_fc_5_cj <- cj(imp_fc_5, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))

mean1 <- mean(imp_fc_1$selected_FC) #calculate mean of each imputed variable to manually calculate coefficients and standard errors from Rubin 1987 (amce outputs don't work with existing packages)
mean1 
mean2 <- mean(imp_fc_2$selected_FC)
mean2 
mean3 <- mean(imp_fc_3$selected_FC)
mean3 
mean4 <- mean(imp_fc_4$selected_FC)
mean4 
mean5 <- mean(imp_fc_5$selected_FC)
mean5 
grandm <- mean(mean1, mean2, mean3, mean4, mean5)
grandm 

coefs <- (imp_fc_1_cj[5] + imp_fc_2_cj[5] + imp_fc_3_cj[5] + imp_fc_4_cj[5] + imp_fc_5_cj[5])/5 #calculation of pooled coefficients
coefs2 <- cbind(imp_fc_1_cj[4], coefs)

within_variance <- ((imp_fc_1_cj[6])^2) + ((imp_fc_2_cj[6])^2) + ((imp_fc_3_cj[6])^2) + ((imp_fc_4_cj[6])^2) + ((imp_fc_5_cj[6])^2) #calculation of pooled standard errors
within_variance_2 <- within_variance/5
between_variance <- ((mean1-grandm)^2) + ((mean2-grandm)^2) + ((mean3-grandm)^2) + ((mean4-grandm)^2) + ((mean5-grandm)^2)
between_variance_2 <- between_variance/4
grandvariance <- within_variance_2 + (1.125*between_variance_2)
grandse <- sqrt(grandvariance)
coefs3 <- cbind(coefs2, grandse)
coefs3

ci95_high <- coefs3[2] + 1.96*(coefs3[3]) #calculation of various confidence intervals
ci95_low <- coefs3[2] - 1.96*(coefs3[3])
ci99_high <- coefs3[2] + 2.576*(coefs3[3])
ci99_low <-  coefs3[2] - 2.576*(coefs3[3])
ci999_high <- coefs3[2] + 3.291*(coefs3[3])
ci999_low <- coefs3[2] - 3.291*(coefs3[3])

coefs4 <- cbind(coefs3, ci95_low, ci95_high, ci99_low, ci99_high, ci999_low, ci999_high) #this is information put into Table A.4 for forced choice
options(digits=2)
colnames(coefs4)<- c("level", "estimate", "std. error", "ci95_low", "ci95_high", "ci99_low", "ci99_high", "ci999_low", "ci999_high")
coefs4

#Table A.4 -- Multiple Imputation w Amelia Package, Rating (same process as above)
set.seed(123)
vars_small2 <- c("Response.ID", "ResAge", "ResGender", "Income", "Urban", "Ideology", "CivilianAttitude", "ExposureVictimizatio","selected_Rating_rescaled","Age", "Gender", "Education Level", "Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime")
df_small_Rating <- df[vars_small2]
to_impute_rating <- df_small_Rating %>% filter_all(any_vars(complete.cases(.)))
imputed_rating = amelia(to_impute_rating, m=5, idvars=c("Response.ID", "Age", "Gender", "Education Level", "Recruitment", "Reputation According to the Troops", "Military Success", "Crime", "Target of the Crime", "Type of Involvement in the Crime", "Attitude Toward the Crime"))
overimpute(imputed_rating, var = "selected_Rating_rescaled")

imp_rating_1 <- imputed_rating$imputations[[1]]
imp_rating_1 <- imp_rating_1[!is.na(imp_rating_1$selected_Rating_rescaled),]
imp_rating_2 <- imputed_rating$imputations[[2]]
imp_rating_2 <- imp_rating_2[!is.na(imp_rating_2$selected_Rating_rescaled),]
imp_rating_3 <- imputed_rating$imputations[[3]]
imp_rating_3 <- imp_rating_3[!is.na(imp_rating_3$selected_Rating_rescaled),]
imp_rating_4 <- imputed_rating$imputations[[4]]
imp_rating_4 <- imp_rating_4[!is.na(imp_rating_4$selected_Rating_rescaled),]
imp_rating_5 <- imputed_rating$imputations[[5]]
imp_rating_5 <- imp_rating_5[!is.na(imp_rating_5$selected_Rating_rescaled),]

imp_rating_1_cj <- cj(imp_rating_1, selected_Rating_rescaled ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))
imp_rating_2_cj <- cj(imp_rating_2, selected_Rating_rescaled ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))
imp_rating_3_cj <- cj(imp_rating_3, selected_Rating_rescaled ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))
imp_rating_4_cj <- cj(imp_rating_4, selected_Rating_rescaled ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))
imp_rating_5_cj <- cj(imp_rating_5, selected_Rating_rescaled ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="amce", level_order=c("ascending"))

mean_rating1 <- mean(imp_rating_1$selected_Rating_rescaled)
mean_rating1 
mean_rating2 <- mean(imp_rating_2$selected_Rating_rescaled)
mean_rating2 
mean_rating3 <- mean(imp_rating_3$selected_Rating_rescaled)
mean_rating3 
mean_rating4 <- mean(imp_rating_4$selected_Rating_rescaled)
mean_rating4
mean_rating5 <- mean(imp_rating_5$selected_Rating_rescaled)
mean_rating5 
grandm_rating <- mean(mean_rating1, mean_rating2, mean_rating3, mean_rating4, mean_rating5)
grandm 

coefs_rating <- (imp_rating_1_cj[5] + imp_rating_2_cj[5] + imp_rating_3_cj[5] + imp_rating_4_cj[5] + imp_rating_5_cj[5])/5
coefs_rating_2 <- cbind(imp_rating_1_cj[4], coefs_rating)

within_variance_rating <- ((imp_rating_1_cj[6])^2) + ((imp_rating_2_cj[6])^2) + ((imp_rating_3_cj[6])^2) + ((imp_rating_4_cj[6])^2) + ((imp_rating_5_cj[6])^2)
within_variance_rating_2 <- within_variance/5
between_variance_rating <- ((mean_rating1-grandm_rating)^2) + ((mean_rating2-grandm_rating)^2) + ((mean_rating3-grandm_rating)^2) + ((mean_rating4-grandm_rating)^2) + ((mean_rating5-grandm_rating)^2)
between_variance_rating_2 <- between_variance_rating/4
grandvariance_rating <- within_variance_rating_2 + (1.125*between_variance_rating_2)
grandse_rating <- sqrt(grandvariance_rating)

coefs3_rating <- cbind(coefs_rating_2, grandse_rating)
coefs3_rating

ci95_high_rating <- coefs3_rating[2] + 1.96*(coefs3_rating[3])
ci95_low_rating <- coefs3_rating[2] - 1.96*(coefs3_rating[3])
ci99_high_rating <- coefs3_rating[2] + 2.576*(coefs3_rating[3])
ci99_low_rating <-  coefs3_rating[2] - 2.576*(coefs3_rating[3])
ci999_high_rating <- coefs3_rating[2] + 3.291*(coefs3_rating[3])
ci999_low_rating <- coefs3_rating[2] - 3.291*(coefs3_rating[3])

coefs4_rating <- cbind(coefs3_rating, ci95_low_rating, ci95_high_rating, ci99_low_rating, ci99_high_rating, ci999_low_rating, ci999_high_rating)
coefs4_rating
colnames(coefs4_rating)<- c("level", "estimate", "std. error", "ci95_low", "ci95_high", "ci99_low", "ci99_high", "ci999_low", "ci999_high")
coefs4_rating

####Appendix 8: HTEs i.e. Subgroup Analysis####

#Table A6; heterogeneity in ratings responses
Rating$Income_rescaled <- rescale(Rating$Income, to=c(0,3)) #quartiles
Ratings_OLS <- lm(selected_Rating_rescaled ~  ResGender + ResAge + Income_rescaled + Urban + Ideology + CivilianAttitude + ExposureVictimizatio, data=Rating)
summary(Ratings_OLS)
Ratings_OLS_clustered <- coeftest(Ratings_OLS, vcov=vcovHC(Ratings_OLS, type="HC", cluster="Response.ID")) 
Ratings_OLS_clustered

Ratings_OLS_2 <- lm(selected_Rating_rescaled ~  ResGender + ResAge + Income_rescaled + Urban + Duque + CivilianAttitude + ExposureVictimizatio, data=Rating)
summary(Ratings_OLS_2)
Ratings_OLS_2_clustered <- coeftest(Ratings_OLS_2, vcov=vcovHC(Ratings_OLS_2, type="HC", cluster="Response.ID")) 
Ratings_OLS_2_clustered

#Create Ideology Variables
library(cregg)
FC$Conservative <- ifelse(FC$Ideology > 5, 1, 0)
FC$Duque <- ifelse(FC$WhoVote2018==1, 1, 0)
FC$VConservative <- ifelse(FC$Ideology>=8,1,0)
FC$Leftist <- ifelse(FC$Ideology<=3,1,0)

#Table A.7 & Table A.8

conservative_mm_ftest <- cj_anova(subset(FC, !is.na(Conservative)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ Conservative)
conservative_mm_ftest #not significant

vconservative_mm_ftest <- cj_anova(subset(FC, !is.na(VConservative)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ VConservative)
vconservative_mm_ftest #not significant

Leftist_mm_ftest <- cj_anova(subset(FC, !is.na(Leftist)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ Leftist)
Leftist_mm_ftest #not significant

Duque_mm_ftest <- cj_anova(subset(FC, !is.na(Duque)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ Duque)
Duque_mm_ftest #significant

Duque_mm_ftest_small <- cj_anova(subset(FC, !is.na(Duque)), selected_FC ~ Gender + `Military Success` + `Target of the Crime`, id= ~ Response.ID, by= ~ Duque)
Duque_mm_ftest_small #only security competence variables, not significant --> not in Table A.7 but requested by reviewer

Duque_mmdiffs_test <- mm_diffs(FC, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ Duque)
Duque_mmdiffs_test #This is Table A.8

Duque_mmdiffs_test_small <- mm_diffs(subset(FC, !is.na(Duque)), selected_FC ~ Gender + `Military Success` + `Target of the Crime`, id= ~ Response.ID, by= ~ Duque)
Duque_mmdiffs_test_small #identical results to Duque_mmdiffs_test

Victimized_mm_ftest <- cj_anova(subset(FC, !is.na(ExposureVictimizatio)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ ExposureVictimizatio)
Victimized_mm_ftest #not significant

gender_mm_ftest <- cj_anova(subset(FC, !is.na(Gender)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ Gender)
gender_mm_ftest #not significant

FC$urbanbinary <- ifelse(FC$Urban==1,0,1)
urban_mm_ftest <- cj_anova(subset(FC, !is.na(urbanbinary)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ urbanbinary)
urban_mm_ftest #not significant

FC$Income <- as.numeric(as.character(FC$Income))
summary(FC$Income) #mean 10
FC$incomebinary <- ifelse(FC$Income > 10, 1, 0)
income_mm_ftest <- cj_anova(subset(FC, !is.na(incomebinary)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ incomebinary)
income_mm_ftest

summary(FC$CivilianAttitude) 
FC$CivilianAttitude <- as.numeric(as.character(FC$CivilianAttitude))
FC$civilianattitudebinary <- ifelse(FC$CivilianAttitude >= 4, 0, 1) #Disagree with IHL
civilianattitude_mm_ftest <- cj_anova(subset(FC, !is.na(civilianattitudebinary)), selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, by= ~ civilianattitudebinary)
civilianattitude_mm_ftest #not significant

#Figures A.9 and A.10
FC$Conservative <- as.factor(as.numeric(FC$Conservative))
Conservative_mm <- cj(FC, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="mm", by= ~ Conservative, h0=0.5, level_order=c("descending"))
Conservative_mm

pdf("Conservative_mm.pdf", width=11, height=12.5)
plot(Conservative_mm, group="Conservative", vline=.5, main="Forced Choice Responses By Ideology (Conservative = right of center)") + theme_grey() + theme(legend.position="bottom") + theme(axis.text.y = element_text(size = 13))
dev.off()

FC$Duque <- as.factor(as.numeric(FC$Duque))
Duque_mm <- cj(FC, selected_FC ~ Age + Gender + `Education Level` + Recruitment + `Reputation According to the Troops` + `Military Success` + Crime + `Target of the Crime` + `Type of Involvement in the Crime` + `Attitude Toward the Crime`, id= ~ Response.ID, estimate="mm", by= ~ Duque,  h0=0.5, level_order=c("descending"))
Duque_mm

pdf("Duque_mm.pdf", width=11, height=12.5)
plot(Duque_mm, group="Duque", vline=.5, xlab="Marginal Mean") + theme_grey() + theme(legend.position="bottom") + theme(axis.text.y = element_text(size = 13))
dev.off()
  
detach("package:cregg", unload=TRUE)

####Data Description (main text p. 24): Respondent Conjoint Consistency####

df$noNA <- ifelse(is.na(df$selected_FC) | (is.na(df$selected_Rating)), 1, 0)
Consistency <- df[df$noNA==0,]
Consistency <- subset(Consistency, select = c(Response.ID, task, profile, selected_FC, selected_Rating)) #selecting relevant variables
Consistency$identify <- paste(Consistency$Response.ID, Consistency$task, sep="-")
Consistency_profile1 <- Consistency[Consistency$profile==1,]
colnames(Consistency_profile1)[colnames(Consistency_profile1)=="selected_FC"] <- "selected_FC_profile1"
colnames(Consistency_profile1)[colnames(Consistency_profile1)=="selected_Rating"] <- "selected_Rating_profile1"
Consistency_profile2 <- Consistency[Consistency$profile==2,]
colnames(Consistency_profile2)[colnames(Consistency_profile2)=="selected_FC"] <- "selected_FC_profile2"
colnames(Consistency_profile2)[colnames(Consistency_profile2)=="selected_Rating"] <- "selected_Rating_profile2"
Consistency_profile2 <- subset(Consistency_profile2, select = c(identify, selected_Rating_profile2))
Consistency_profile1 <- subset(Consistency_profile1, select = c(identify, selected_FC_profile1, selected_Rating_profile1))
Consistency2 <- merge(Consistency_profile1, Consistency_profile2, by="identify")

Consistency2$consistent <- ifelse(Consistency2$selected_FC_profile1 == 1 & Consistency2$selected_Rating_profile1 >= Consistency2$selected_Rating_profile2 | Consistency2$selected_FC_profile1 == 0 & Consistency2$selected_Rating_profile1 <= Consistency2$selected_Rating_profile2, 1,0)
summary(Consistency2$consistent) #94 percent consistency

#####Data Description (main text p. 22, 24): Respondent Political Attitudes & Behavior####
options(digits=5)
summary(df$Vote2018) #mean=.82 i.e. turnout 82 percent
#turnout in this election was 53 percent (Registraduría, https://elecciones1.registraduria.gov.co/pre_pres_2018/resultados/html/resultados.html)

df$WhoVote2018 <- as.factor(df$WhoVote2018)
summary(df$WhoVote2018) #divide by 8 because each respondent has 8 rows: 
#378 for Duque (33%), 280 for Petro (25%), 351 for Fajardo (31%), 28 for Vargas Lleras, 40 for de la Calle, 43 for other
#nationally, in the first round, Duque won 39 percent, Petro 25 percent (Registraduría)

summary(df$ExposureVictimizatio) #mean is .34, meaning about a third of respondents have been exposed to victimization

summary(df$Ideology) #mean is 5.76
#LAPOP indicates that national average in 2017 was 5.8 on the same scale (https://www.vanderbilt.edu/lapop/colombia/Colombia_2018_Democracia_e_Instituciones_W_11.07.19.pdf)

#are conservative voters unwilling to vote (lowest possible rating) i.e. do they think candidates are FARC?
Rating$conservative <- ifelse(Rating$Ideology > 5, 1, 0)
conservative <- Rating[which(Rating$conservative==1),]
summary(Rating$selected_Rating) #mean=2.5
summary(conservative$selected_Rating) #mean=2.52
t.test(conservative$selected_Rating, Rating$selected_Rating) #p value is .5, not significant
Rating$Duque <- ifelse(Rating$WhoVote2018 ==1, 1, 0)
DuqueVote <- Rating[which(Rating$Duque==1),]
summary(DuqueVote$selected_Rating) #2.52
t.test(DuqueVote$selected_Rating, Rating$selected_Rating) #p value is .51, not significant

