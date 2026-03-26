# Libraries
library(readstata13)
library(reporttools)
library(weights)
library(texreg)
library(plyr)
library(csodata)

# Read in the data
load("Data_prepostcombined.Rdata")

##################
#### Create data objects
#####################

# Create an object with only high-attention respondents 
combined_sub <- subset(combined, attention_check.x=="2" & attention_check.y=="2")

# Shape the data into long format
combined_long <- reshape(combined, varying = list(c("status_quo_1.x", "status_quo_1.y"),
                                                  c("status_quo_2.x", "status_quo_2.y"),
                                                  c("directional_1.x","directional_1.y"),
                                                  c("directional_2.x","directional_2.y"),
                                                  c("self_1.x","self_1.y"),c("self_2.x","self_2.y"),
                                                  c("status_quo_mean.x","status_quo_mean.y"),
                                                  c("directional_mean.x","directional_mean.y"),
                                                  c("self_mean.x","self_mean.y"),c("thermo.x","thermo.y")),
                         v.name=c("status_quo_1","status_quo_2","directional_1","directional_2",
                                  "self_1","self_2","status_quo_mean","directional_mean","self_mean","thermo","date"), 
                         timevar="post", direction="long", idvar="PID",times=c(0,1))

combined_long_sub <- subset(combined_long, attention_check.x=="2" & attention_check.y=="2")

##################
##### OA3: Comparison of census and first wave
#####################

# Read in data on gender and age
sexagedata <- read.csv("EY007.20201202T101230.csv")

# Delete All ages observations
sexagedata <- sexagedata[-c(1:3),]

# Keep those 18 or above
sexagedata <- subset(sexagedata, Age.Last.Birthday!="Under 1 year")
sexagedata <- subset(sexagedata, Age.Last.Birthday!="All ages")
sexagedata$age <- as.numeric(gsub("\\D", "", sexagedata$Age.Last.Birthday))
sexagedata <- subset(sexagedata, age >= 18)

# Delete Population at or under/over this age
sexagedata <- subset(sexagedata, At.Each.Year.of.Age=="Population")

# Delete Both sexes
sexagedata <- subset(sexagedata, Sex!="Both sexes")

# Calculate gender groups
sexagedata <- subset(sexagedata, CensusYear==2016)
men <- sum(subset(sexagedata,Sex=="Male")[,c("VALUE")])
women <- sum(subset(sexagedata,Sex=="Female")[,c("VALUE")])
total <- men + women
gender_census_vec <- c(women,men)

# Sex of respondent
gender_vec <- rep(NA,2)
names(gender_vec) <- c("female","male")
gender_vec[1] <- length(which(pre$male==0))
gender_vec[2] <- length(which(pre$male==1))

tab <- rbind(gender_census_vec, gender_vec)
chisq.test(tab)

# Calculate age groups
# Our dataset has seven age groups.
age_census_vec <- rep(NA,7)
names(age_census_vec) <- c("18-24","25-34","35-44","45-54","55-64","65-74","75+")
age_census_vec[1] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=18 & sexagedata$age<=24)])
age_census_vec[2] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=25 & sexagedata$age<=34)])
age_census_vec[3] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=35 & sexagedata$age<=44)])
age_census_vec[4] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=45 & sexagedata$age<=54)])
age_census_vec[5] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=55 & sexagedata$age<=64)])
age_census_vec[6] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=65 & sexagedata$age<=74)])
age_census_vec[7] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=75)])

# Compare age
pre$age_seven_category <- as.numeric(mapvalues(pre$validation_age,
                                             c("18-24","25-34","35-44","45-54","55-64","65-74","75-84"),
                                             c(1,2,3,4,5,6,7)))
age_vec <- table(pre$age_seven_category)

tab <- rbind(age_census_vec, age_vec)
chisq.test(tab)

# Read in data on education
educdata <- read.csv("EZ055.20201202T101232.csv")

# Subset to 18 or above in age
educdata$age <- as.numeric(gsub("\\D", "", educdata$Single.Year.of.Age))
educdata <- subset(educdata, age>=18)

# Delete Both sexes
educdata <- subset(educdata, Sex!="Both sexes")

# Delete non-relevant educations
educdata <- subset(educdata, CensusYear==2016)
educdata <- subset(educdata, Highest.Level.of.Education.Completed!="Total education ceased and not ceased")
educdata <- subset(educdata, Highest.Level.of.Education.Completed!="Not stated")

# Calculate proportion of education groups
educdata$educ_six_category <- as.numeric(as.character(mapvalues(educdata$Highest.Level.of.Education.Completed,
                                                                c("No formal education"    ,
                                                                  "Primary"                 ,
                                                                  "Lower secondary"          ,
                                                                  "Upper secondary"           ,
                                                                  "Technical/vocational"           ,
                                                                  "Advanced certificate/completed apprenticeship"      ,
                                                                  "Higher certificate"                             ,
                                                                  "Ordinary bachelor degree/professional qualification or both",
                                                                  "Honours bachelor degree/professional qualification or both" ,
                                                                  "Postgraduate diploma or degree"                        ,
                                                                  "Doctorate (Ph.D.)"                   ,
                                                                  "Economic status - total at school, university, etc."   ,
                                                                  "Economic status - other" ),
                                                                c(1,2,3,4,5,5,5,6,6,6,6,NA,NA))))
educdata <- subset(educdata,!is.na(educ_six_category))
educ_census_vec <- c(by(educdata$"VALUE",educdata$"educ",sum))

pre$educ_six_category <- pre$education+1
pre$educ_six_category[pre$educ_six_category==7] <- 6

tab <- rbind(educ_census_vec, table(pre$educ_six_category))
chisq.test(tab)

####################
###### Table OA3.1: Descriptive statistics 
#######################

tableContinuous(subset(combined,!is.na(status_quo_1.y))[,c("age","male","education","hh_income","religiosity","married")],stats=c("n","min","median","mean","max","s"))

############################
####### OA4: Compare sociodemographics of census and subsample
################################

# Age
combined_sub$age_seven_category <- as.numeric(mapvalues(combined_sub$validation_age, 
                                                      c("18-24","25-34","35-44","45-54","55-64","65-74","75-84"),
                                                      c(1,2,3,4,5,6,7)))
age_vec <- table(combined_sub$age_seven_category)

tab <- rbind(age_census_vec, age_vec)
chisq.test(tab)

# Sex
gender_vec <- rep(NA,2) 
names(gender_vec) <- c("female","male")
gender_vec[1] <- length(which(combined_sub$male==0))
gender_vec[2] <- length(which(combined_sub$male==1))

tab <- rbind(gender_census_vec, gender_vec)
chisq.test(tab)

# highest level of education completed
tab <- rbind(educ_census_vec, c("1"=0,table(combined_sub$educ_six_category)))
chisq.test(tab)

############################
####### OA4: Compare sociodemographics of full sample and subsample
################################

# Year of birth
chisq.test(rbind(table(combined$age),table(combined_sub$age))) 

# Sex of respondent
chisq.test(rbind(table(combined$male),table(combined_sub$male))) 

# highest level of education completed
temp <- rep(NA,7)
names(temp) <- names(table(combined$education))
temp[1] <- 0
temp[2:7] <- table(combined_sub$education)
chisq.test(rbind(table(combined$education),temp))

#############################
##### OA4: Check whether failure to pay attention is systematically affected by covariates 
#############################

# Create a variable indicating whether the respondent correctly answered the attention check question
temp <- subset(combined,!is.na(attention_check.y))
temp$attention_success <- NA
temp$attention_success[which(temp$attention_check.x=="2"&temp$attention_check.y=="2")] <- 1
temp$attention_success[-which(temp$attention_check.x=="2"&temp$attention_check.y=="2")] <- 0

# Check whether failure to pay attention is systematically affected by covariates
mod <- glm(attention_success ~ age + male + education + hh_income + religiosity + married + status_quo_mean.x + directional_mean.x + self_mean.x + thermo.x, data=temp,family="binomial") 
summary(mod)

####################
###### Table OA4:1 Descriptive statistics
#######################

tableContinuous(subset(combined_sub,!is.na(status_quo_1.y))[,c("age","male","education","hh_income","religiosity","married")],stats=c("n","min","median","mean","max","s"))

############################
####### OA5: Full main results
################################

set.seed(1120)

ttest_sq_w <- wtd.t.test(combined$status_quo_mean.y-combined$status_quo_mean.x, weight=combined$weights*combined$weights_pst_census,bootse=F)
ttest_sq_w$additional[1]-1.96*ttest_sq_w$additional[4]
ttest_sq_w$additional[1]+1.96*ttest_sq_w$additional[4]
ttest_dir_w <- wtd.t.test(combined$directional_mean.y-combined$directional_mean.x, weight=combined$weights*combined$weights_pst_census,bootse=F)
ttest_dir_w$additional[1]-1.96*ttest_dir_w$additional[4]
ttest_dir_w$additional[1]+1.96*ttest_dir_w$additional[4]

lm_sq_w <- lm(status_quo_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_census)
lm_dir_w <- lm(directional_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_census)

ttest_sq_w_sub <- wtd.t.test(combined_sub$status_quo_mean.y-combined_sub$status_quo_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_census,bootse=F)
ttest_sq_w_sub$additional[1]-1.96*ttest_sq_w_sub$additional[4]
ttest_sq_w_sub$additional[1]+1.96*ttest_sq_w_sub$additional[4]
ttest_dir_w_sub <- wtd.t.test(combined_sub$directional_mean.y-combined_sub$directional_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_census,bootse=F)
ttest_dir_w_sub$additional[1]-1.96*ttest_dir_w_sub$additional[4]
ttest_dir_w_sub$additional[1]+1.96*ttest_dir_w_sub$additional[4]

lm_sq_w_sub <- lm(status_quo_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_census)
lm_dir_w_sub <- lm(directional_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_census)

texreg(list(lm_sq_w,lm_sq_w_sub,lm_dir_w,lm_dir_w_sub),stars=0.05)

ttest_self_w <- wtd.t.test(combined$self_mean.y-combined$self_mean.x, weight=combined$weights*combined$weights_pst_census,bootse=F)
ttest_self_w$additional[1]-1.96*ttest_self_w$additional[4]
ttest_self_w$additional[1]+1.96*ttest_self_w$additional[4]
ttest_thermo_w <- wtd.t.test(combined$thermo.y-combined$thermo.x, weight=combined$weights*combined$weights_pst_census,bootse=F)
ttest_thermo_w$additional[1]-1.96*ttest_thermo_w$additional[4]
ttest_thermo_w$additional[1]+1.96*ttest_thermo_w$additional[4]

lm_self_w <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_census)
lm_thermo_w <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_census)

ttest_self_w_sub <- wtd.t.test(combined_sub$self_mean.y-combined_sub$self_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_census,bootse=F)
ttest_self_w_sub$additional[1]-1.96*ttest_self_w_sub$additional[4]
ttest_self_w_sub$additional[1]+1.96*ttest_self_w_sub$additional[4]
ttest_thermo_w_sub <- wtd.t.test(combined_sub$thermo.y-combined_sub$thermo.x, weight=combined_sub$weights*combined_sub$weights_pst_census,bootse=F)
ttest_thermo_w_sub$additional[1]-1.96*ttest_thermo_w_sub$additional[4]
ttest_thermo_w_sub$additional[1]+1.96*ttest_thermo_w_sub$additional[4]

lm_self_w_sub <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_census)
lm_thermo_w_sub <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_census)

texreg(list(lm_self_w,lm_self_w_sub,lm_thermo_w,lm_thermo_w_sub),stars=0.05)

############################
####### OA6: Robustness using INES-based weights
################################

ttest_sq_w <- wtd.t.test(combined$status_quo_mean.y-combined$status_quo_mean.x, weight=combined$weights*combined$weights_pst_ines,bootse=F)
ttest_dir_w <- wtd.t.test(combined$directional_mean.y-combined$directional_mean.x, weight=combined$weights*combined$weights_pst_ines,bootse=F)

lm_sq_w <- lm(status_quo_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_ines)
lm_dir_w <- lm(directional_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_ines)

ttest_sq_w_sub <- wtd.t.test(combined_sub$status_quo_mean.y-combined_sub$status_quo_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_ines,bootse=F)
ttest_dir_w_sub <- wtd.t.test(combined_sub$directional_mean.y-combined_sub$directional_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_ines,bootse=F)

lm_sq_w_sub <- lm(status_quo_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_ines)
lm_dir_w_sub <- lm(directional_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_ines)

pdf("plot_norm_perception_pstines.pdf",width=8,height=6)
par(mfrow=c(2,2),mai=c(0.6,0.6806,0.3,0.3486))

diff_sq_w <- ttest_sq_w$additional[1]
ttest_se_sq_w <- ttest_sq_w$additional[4]

coef_sq_w <- summary(lm_sq_w)$coef[2,1]
se_sq_w <- summary(lm_sq_w)$coef[2,2]

diff_sq_w_sub <- ttest_sq_w_sub$additional[1]
ttest_se_sq_w_sub <- ttest_sq_w_sub$additional[4]

coef_sq_w_sub <- summary(lm_sq_w_sub)$coef[2,1]
se_sq_w_sub <- summary(lm_sq_w_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(0,2),xlim=c(0,0.6), ylab="Effect of referendum", main="Status quo perception (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_sq_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_sq_w-1.96*ttest_se_sq_w,diff_sq_w+1.96*ttest_se_sq_w),lwd=2)
points(0.45,coef_sq_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_sq_w-1.96*se_sq_w,coef_sq_w+1.96*se_sq_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(0,2),xlim=c(0,0.6), ylab="Effect of referendum", main="Status quo perception (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_sq_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_sq_w_sub-1.96*ttest_se_sq_w_sub,diff_sq_w_sub+1.96*ttest_se_sq_w_sub),lwd=2)
points(0.45,coef_sq_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_sq_w_sub-1.96*se_sq_w_sub,coef_sq_w_sub+1.96*se_sq_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

diff_dir_w <- ttest_dir_w$additional[1]
ttest_se_dir_w <- ttest_dir_w$additional[4]

coef_dir_w <- summary(lm_dir_w)$coef[2,1]
se_dir_w <- summary(lm_dir_w)$coef[2,2]

diff_dir_w_sub <- ttest_dir_w_sub$additional[1]
ttest_se_dir_w_sub <- ttest_dir_w_sub$additional[4]

coef_dir_w_sub <- summary(lm_dir_w_sub)$coef[2,1]
se_dir_w_sub <- summary(lm_dir_w_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(0,2),xlim=c(0,0.6), ylab="Effect of referendum", main="Directional perception (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_dir_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_dir_w-1.96*ttest_se_dir_w,diff_dir_w+1.96*ttest_se_dir_w),lwd=2)
points(0.45,coef_dir_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_dir_w-1.96*se_dir_w,coef_dir_w+1.96*se_dir_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(0,2),xlim=c(0,0.6), ylab="Effect of referendum", main="Directional perception (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_dir_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_dir_w_sub-1.96*ttest_se_dir_w_sub,diff_dir_w_sub+1.96*ttest_se_dir_w_sub),lwd=2)
points(0.45,coef_dir_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_dir_w_sub-1.96*se_dir_w_sub,coef_dir_w_sub+1.96*se_dir_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

dev.off()

ttest_self_w <- wtd.t.test(combined$self_mean.y-combined$self_mean.x, weight=combined$weights*combined$weights_pst_ines,bootse=F)
ttest_thermo_w <- wtd.t.test(combined$thermo.y-combined$thermo.x, weight=combined$weights*combined$weights_pst_ines,bootse=F)

lm_self_w <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_ines)
lm_thermo_w <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=combined_long,weights=weights * weights_pst_ines)

ttest_self_w_sub <- wtd.t.test(combined_sub$self_mean.y-combined_sub$self_mean.x, weight=combined_sub$weights*combined_sub$weights_pst_ines,bootse=F)
ttest_thermo_w_sub <- wtd.t.test(combined_sub$thermo.y-combined_sub$thermo.x, weight=combined_sub$weights*combined_sub$weights_pst_ines,bootse=F)

lm_self_w_sub <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_ines)
lm_thermo_w_sub <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=combined_long_sub,weights=weights * weights_pst_ines)

pdf("plot_personal_opinion_pstines.pdf",width=8,height=6)
par(mfrow=c(2,2),mai=c(0.6,0.6806,0.3,0.3486))

diff_self_w <- ttest_self_w$additional[1]
ttest_se_self_w <- ttest_self_w$additional[4]

coef_self_w <- summary(lm_self_w)$coef[2,1]
se_self_w <- summary(lm_self_w)$coef[2,2]

diff_self_w_sub <- ttest_self_w_sub$additional[1]
ttest_se_self_w_sub <- ttest_self_w_sub$additional[4]

coef_self_w_sub <- summary(lm_self_w_sub)$coef[2,1]
se_self_w_sub <- summary(lm_self_w_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-1,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w-1.96*ttest_se_self_w,diff_self_w+1.96*ttest_se_self_w),lwd=2)
points(0.45,coef_self_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w-1.96*se_self_w,coef_self_w+1.96*se_self_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-1,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w_sub-1.96*ttest_se_self_w_sub,diff_self_w_sub+1.96*ttest_se_self_w_sub),lwd=2)
points(0.45,coef_self_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w_sub-1.96*se_self_w_sub,coef_self_w_sub+1.96*se_self_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

diff_thermo_w <- ttest_thermo_w$additional[1]
ttest_se_thermo_w <- ttest_thermo_w$additional[4]

coef_thermo_w <- summary(lm_thermo_w)$coef[2,1]
se_thermo_w <- summary(lm_thermo_w)$coef[2,2]

diff_thermo_w_sub <- ttest_thermo_w_sub$additional[1]
ttest_se_thermo_w_sub <- ttest_thermo_w_sub$additional[4]

coef_thermo_w_sub <- summary(lm_thermo_w_sub)$coef[2,1]
se_thermo_w_sub <- summary(lm_thermo_w_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-6,7),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w-1.96*ttest_se_thermo_w,diff_thermo_w+1.96*ttest_se_thermo_w),lwd=2)
points(0.45,coef_thermo_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w-1.96*se_thermo_w,coef_thermo_w+1.96*se_thermo_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-6,7),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w_sub-1.96*ttest_se_thermo_w_sub,diff_thermo_w_sub+1.96*ttest_se_thermo_w_sub),lwd=2)
points(0.45,coef_thermo_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w_sub-1.96*se_thermo_w_sub,coef_thermo_w_sub+1.96*se_thermo_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

dev.off()

##################
###### OA6: Comparison of distributions
#######################

ks.test(combined$status_quo_mean.x, combined$status_quo_mean.y)
ks.test(combined$directional_mean.x, combined$directional_mean.y) 
ks.test(combined$self_mean.x, combined$self_mean.y) 
ks.test(combined$thermo.x, combined$thermo.y) 

ks.test(combined_sub$status_quo_mean.x, combined_sub$status_quo_mean.y) 
ks.test(combined_sub$directional_mean.x, combined_sub$directional_mean.y) 
ks.test(combined_sub$self_mean.x, combined_sub$self_mean.y)
ks.test(combined_sub$thermo.x, combined_sub$thermo.y) 

##################
###### OA6: Personal opinion analysis of those who wanted to vote against repealing abortion ban
#######################

against_repeal <- subset(combined, ban==1)

ttest_self_w_anti <- wtd.t.test(against_repeal$self_mean.y-against_repeal$self_mean.x, weight=against_repeal$weights*against_repeal$weights_pst_census,bootse=F) # sig diff
ttest_thermo_w_anti <- wtd.t.test(against_repeal$thermo.y-against_repeal$thermo.x, weight=against_repeal$weights*against_repeal$weights_pst_census,bootse=F) # no diff

lm_self_w_anti <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=subset(combined_long, ban==1),weights=weights*weights_pst_census) # no diff
lm_thermo_w_anti <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=subset(combined_long, ban==1),weights=weights*weights_pst_census) # no diff

against_repeal_sub <- subset(against_repeal, attention_check.x=="2" & attention_check.y=="2")

ttest_self_w_anti_sub <- wtd.t.test(against_repeal_sub$self_mean.y-against_repeal_sub$self_mean.x, weight=against_repeal_sub$weights*against_repeal_sub$weights_pst_census,bootse=F) # no diff
ttest_thermo_w_anti_sub <- wtd.t.test(against_repeal_sub$thermo.y-against_repeal_sub$thermo.x, weight=against_repeal_sub$weights*against_repeal_sub$weights_pst_census,bootse=F) # no diff

lm_self_w_anti_sub <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=subset(combined_long_sub, ban==1),weights=weights*weights_pst_census) # no diff
lm_thermo_w_anti_sub <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=subset(combined_long_sub, ban==1),weights=weights*weights_pst_census) # no diff

pdf("plot_against_repeal_pstcensus.pdf",width=8,height=6)
par(mfrow=c(2,2),mai=c(0.6,0.6806,0.3,0.3486))

diff_self_w <- ttest_self_w_anti$additional[1]
ttest_se_self_w <- ttest_self_w_anti$additional[4]

coef_self_w <- summary(lm_self_w_anti)$coef[2,1]
se_self_w <- summary(lm_self_w_anti)$coef[2,2]

diff_self_w_sub <- ttest_self_w_anti_sub$additional[1]
ttest_se_self_w_sub <- ttest_self_w_anti_sub$additional[4]

coef_self_w_sub <- summary(lm_self_w_anti_sub)$coef[2,1]
se_self_w_sub <- summary(lm_self_w_anti_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-1.5,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w-1.96*ttest_se_self_w,diff_self_w+1.96*ttest_se_self_w),lwd=2)
points(0.45,coef_self_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w-1.96*se_self_w,coef_self_w+1.96*se_self_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-1.5,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w_sub-1.96*ttest_se_self_w_sub,diff_self_w_sub+1.96*ttest_se_self_w_sub),lwd=2)
points(0.45,coef_self_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w_sub-1.96*se_self_w_sub,coef_self_w_sub+1.96*se_self_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

diff_thermo_w <- ttest_thermo_w_anti$additional[1]
ttest_se_thermo_w <- ttest_thermo_w_anti$additional[4]

coef_thermo_w <- summary(lm_thermo_w_anti)$coef[2,1]
se_thermo_w <- summary(lm_thermo_w_anti)$coef[2,2]

diff_thermo_w_sub <- ttest_thermo_w_anti_sub$additional[1]
ttest_se_thermo_w_sub <- ttest_thermo_w_anti_sub$additional[4]

coef_thermo_w_sub <- summary(lm_thermo_w_anti_sub)$coef[2,1]
se_thermo_w_sub <- summary(lm_thermo_w_anti_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-15,10),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w-1.96*ttest_se_thermo_w,diff_thermo_w+1.96*ttest_se_thermo_w),lwd=2)
points(0.45,coef_thermo_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w-1.96*se_thermo_w,coef_thermo_w+1.96*se_thermo_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-15,10),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w_sub-1.96*ttest_se_thermo_w_sub,diff_thermo_w_sub+1.96*ttest_se_thermo_w_sub),lwd=2)
points(0.45,coef_thermo_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w_sub-1.96*se_thermo_w_sub,coef_thermo_w_sub+1.96*se_thermo_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

dev.off()

##################
###### OA6: Personal opinion analysis of those changed norm perception
#######################

changed_perception <- combined[which((combined$status_quo_mean.y - combined$status_quo_mean.x) > 0),]

ttest_self_w_changed <- wtd.t.test(changed_perception$self_mean.y-changed_perception$self_mean.x, weight=changed_perception$weights*changed_perception$weights_pst_census,bootse=F) # sig diff
ttest_thermo_w_changed <-wtd.t.test(changed_perception$thermo.y-changed_perception$thermo.x, weight=changed_perception$weights*changed_perception$weights_pst_census,bootse=F) # sig diff

# Shape the data into long format
temp_long <- reshape(changed_perception, varying = list(c("status_quo_1.x", "status_quo_1.y"),c("status_quo_2.x", "status_quo_2.y"),c("directional_1.x","directional_1.y"),c("directional_2.x","directional_2.y"),c("self_1.x","self_1.y"),c("self_2.x","self_2.y"),c("status_quo_mean.x","status_quo_mean.y"),c("directional_mean.x","directional_mean.y"),c("self_mean.x","self_mean.y"),c("thermo.x","thermo.y")),v.name=c("status_quo_1","status_quo_2","directional_1","directional_2","self_1","self_2","status_quo_mean","directional_mean","self_mean","thermo","date"), timevar="post", direction="long", idvar="PID",times=c(0,1))

lm_self_w_changed <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=temp_long, weights=weights*weights_pst_census) # sig pos
lm_thermo_w_changed <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=temp_long, weights=weights*weights_pst_census) # not sig

changed_perception_sub <- subset(changed_perception, attention_check.x=="2" & attention_check.y=="2")

ttest_self_w_changed_sub <- wtd.t.test(changed_perception_sub$self_mean.y-changed_perception_sub$self_mean.x, weight=changed_perception_sub$weights*changed_perception_sub$weights_pst_census,bootse=F) # not sig
ttest_thermo_w_changed_sub <- wtd.t.test(changed_perception_sub$thermo.y-changed_perception_sub$thermo.x, weight=changed_perception_sub$weights*changed_perception_sub$weights_pst_census,bootse=F) # not sig

temp_long_sub <- subset(temp_long, attention_check.x=="2" & attention_check.y=="2")

lm_self_w_changed_sub <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=temp_long_sub, weights=weights*weights_pst_census) # not pos
lm_thermo_w_changed_sub <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=temp_long_sub, weights=weights*weights_pst_census) # not sig

pdf("plot_norm_perception_changed_pstcensus.pdf",width=8,height=6)
par(mfrow=c(2,2),mai=c(0.6,0.6806,0.3,0.3486))

diff_self_w <- ttest_self_w_changed$additional[1]
ttest_se_self_w <- ttest_self_w_changed$additional[4]

coef_self_w <- summary(lm_self_w_changed)$coef[2,1]
se_self_w <- summary(lm_self_w_changed)$coef[2,2]

diff_self_w_sub <- ttest_self_w_changed_sub$additional[1]
ttest_se_self_w_sub <- ttest_self_w_changed_sub$additional[4]

coef_self_w_sub <- summary(lm_self_w_changed_sub)$coef[2,1]
se_self_w_sub <- summary(lm_self_w_changed_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-0.5,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w-1.96*ttest_se_self_w,diff_self_w+1.96*ttest_se_self_w),lwd=2)
points(0.45,coef_self_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w-1.96*se_self_w,coef_self_w+1.96*se_self_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-0.5,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w_sub-1.96*ttest_se_self_w_sub,diff_self_w_sub+1.96*ttest_se_self_w_sub),lwd=2)
points(0.45,coef_self_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w_sub-1.96*se_self_w_sub,coef_self_w_sub+1.96*se_self_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

diff_thermo_w <- ttest_thermo_w_changed$additional[1]
ttest_se_thermo_w <- ttest_thermo_w_changed$additional[4]

coef_thermo_w <- summary(lm_thermo_w_changed)$coef[2,1]
se_thermo_w <- summary(lm_thermo_w_changed)$coef[2,2]

diff_thermo_w_sub <- ttest_thermo_w_changed_sub$additional[1]
ttest_se_thermo_w_sub <- ttest_thermo_w_changed_sub$additional[4]

coef_thermo_w_sub <- summary(lm_thermo_w_changed_sub)$coef[2,1]
se_thermo_w_sub <- summary(lm_thermo_w_changed_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-10,8),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w-1.96*ttest_se_thermo_w,diff_thermo_w+1.96*ttest_se_thermo_w),lwd=2)
points(0.45,coef_thermo_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w-1.96*se_thermo_w,coef_thermo_w+1.96*se_thermo_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-10,8),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w_sub-1.96*ttest_se_thermo_w_sub,diff_thermo_w_sub+1.96*ttest_se_thermo_w_sub),lwd=2)
points(0.45,coef_thermo_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w_sub-1.96*se_thermo_w_sub,coef_thermo_w_sub+1.96*se_thermo_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

dev.off()

##################
###### OA6: Is there effect for immediate respondents?
#######################

earliest <- combined[which(combined$date.y==4),]
prop.table(table(combined$date.y))

ttest_self_w_earliest <- wtd.t.test(earliest$self_mean.y-earliest$self_mean.x, weight=earliest$weights*earliest$weights_pst_census,bootse=F) # sig diff
ttest_thermo_w_earliest <-wtd.t.test(earliest$thermo.y-earliest$thermo.x, weight=earliest$weights*earliest$weights_pst_census,bootse=F) # not sig diff

# Shape the data into long format
temp_long <- reshape(earliest, varying = list(c("status_quo_1.x", "status_quo_1.y"),
                                              c("status_quo_2.x", "status_quo_2.y"),
                                              c("directional_1.x","directional_1.y"),
                                              c("directional_2.x","directional_2.y"),
                                              c("self_1.x","self_1.y"),
                                              c("self_2.x","self_2.y"),
                                              c("status_quo_mean.x","status_quo_mean.y"),
                                              c("directional_mean.x","directional_mean.y"),
                                              c("self_mean.x","self_mean.y"),
                                              c("thermo.x","thermo.y")),
                     v.name=c("status_quo_1","status_quo_2","directional_1","directional_2",
                              "self_1","self_2","status_quo_mean","directional_mean",
                              "self_mean","thermo"), timevar="post", direction="long", idvar="PID",times=c(0,1))

lm_self_w_earliest <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=temp_long, weights=weights*weights_pst_census) # not sig
lm_thermo_w_earliest <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=temp_long, weights=weights*weights_pst_census) # not sig

earliest_sub <- subset(earliest, attention_check.x=="2" & attention_check.y=="2")

ttest_self_w_earliest_sub <- wtd.t.test(earliest_sub$self_mean.y-earliest_sub$self_mean.x, weight=earliest_sub$weights*earliest_sub$weights_pst_census,bootse=F) # not sig
ttest_thermo_w_earliest_sub <- wtd.t.test(earliest_sub$thermo.y-earliest_sub$thermo.x, weight=earliest_sub$weights*earliest_sub$weights_pst_census,bootse=F) # not sig

temp_long_sub <- subset(temp_long, attention_check.x=="2" & attention_check.y=="2")

lm_self_w_earliest_sub <- lm(self_mean~post+hh_income + religiosity + age + male + education + married, data=temp_long_sub, weights=weights*weights_pst_census) # not pos
lm_thermo_w_earliest_sub <- lm(thermo~post+hh_income + religiosity + age + male + education + married, data=temp_long_sub, weights=weights*weights_pst_census) # not sig

pdf("plot_earliest_pstcensus.pdf",width=8,height=6)
par(mfrow=c(2,2),mai=c(0.6,0.6806,0.3,0.3486))

diff_self_w <- ttest_self_w_earliest$additional[1]
ttest_se_self_w <- ttest_self_w_earliest$additional[4]

coef_self_w <- summary(lm_self_w_earliest)$coef[2,1]
se_self_w <- summary(lm_self_w_earliest)$coef[2,2]

diff_self_w_sub <- ttest_self_w_earliest_sub$additional[1]
ttest_se_self_w_sub <- ttest_self_w_earliest_sub$additional[4]

coef_self_w_sub <- summary(lm_self_w_earliest_sub)$coef[2,1]
se_self_w_sub <- summary(lm_self_w_earliest_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-1.2,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w-1.96*ttest_se_self_w,diff_self_w+1.96*ttest_se_self_w),lwd=2)
points(0.45,coef_self_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w-1.96*se_self_w,coef_self_w+1.96*se_self_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-1.2,1),xlim=c(0,0.6), ylab="Effect of referendum", main="Personal position (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_self_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_self_w_sub-1.96*ttest_se_self_w_sub,diff_self_w_sub+1.96*ttest_se_self_w_sub),lwd=2)
points(0.45,coef_self_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_self_w_sub-1.96*se_self_w_sub,coef_self_w_sub+1.96*se_self_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

diff_thermo_w <- ttest_thermo_w_earliest$additional[1]
ttest_se_thermo_w <- ttest_thermo_w_earliest$additional[4]

coef_thermo_w <- summary(lm_thermo_w_earliest)$coef[2,1]
se_thermo_w <- summary(lm_thermo_w_earliest)$coef[2,2]

diff_thermo_w_sub <- ttest_thermo_w_earliest_sub$additional[1]
ttest_se_thermo_w_sub <- ttest_thermo_w_earliest_sub$additional[4]

coef_thermo_w_sub <- summary(lm_thermo_w_earliest_sub)$coef[2,1]
se_thermo_w_sub <- summary(lm_thermo_w_earliest_sub)$coef[2,2]

plot(x=c(),y=c(),ylim=c(-12,8),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (full sample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w-1.96*ttest_se_thermo_w,diff_thermo_w+1.96*ttest_se_thermo_w),lwd=2)
points(0.45,coef_thermo_w,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w-1.96*se_thermo_w,coef_thermo_w+1.96*se_thermo_w),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

plot(x=c(),y=c(),ylim=c(-12,8),xlim=c(0,0.6), ylab="Effect of referendum", main="Thermometer (subsample)", xaxt="n", xlab="",cex.lab=1.2)
points(0.15,diff_thermo_w_sub,pch=16)
lines(x=c(0.15,0.15),y=c(diff_thermo_w_sub-1.96*ttest_se_thermo_w_sub,diff_thermo_w_sub+1.96*ttest_se_thermo_w_sub),lwd=2)
points(0.45,coef_thermo_w_sub,pch=16)
lines(x=c(0.45,0.45),y=c(coef_thermo_w_sub-1.96*se_thermo_w_sub,coef_thermo_w_sub+1.96*se_thermo_w_sub),lwd=2)
abline(h=0,lty="dashed")
mtext("T-test", side=1, at=0.15, line=1,cex=0.9)
mtext("Multiple regression", side=1, at=0.45, line=1,cex=0.9)

dev.off()

