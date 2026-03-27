library(AER)
library(data.table)
library(multcomp)
library(nnet)
library(psych)
library(texreg)
set.seed(1989)

# setwd("")

study1 <- fread("study1.csv", header = TRUE, stringsAsFactors = FALSE)
study2 <- fread("study2.csv", header = TRUE, stringsAsFactors = FALSE)
study3 <- fread("study3.csv", header = TRUE, stringsAsFactors = FALSE)

################################################################################

# UNDERLYING MODELS FOR FIGURE 1

# STUDY 1: SCOTUS NOMINATION

# SPECIFIC SUPPORT FOR NOMINEE
study1_approval_bin <- lm(nom_approval_bin ~ scandal_treatment, data = study1)
summary(study1_approval_bin)
# ARE DIFFERENCES AMONG TREATMENTS DISTINGUISHABLE?
summary(glht(study1_approval_bin, 
             "scandal_treatmentethics - scandal_treatmentsexual = 0"))
summary(glht(study1_approval_bin, 
             "scandal_treatmenttaxes - scandal_treatmentsexual = 0"))
summary(glht(study1_approval_bin, 
             "scandal_treatmentethics - scandal_treatmenttaxes = 0"))

# DIFFUSE SUPPORT
study1_inst_support <- lm(inst_support ~ scandal_treatment, data = study1)
summary(study1_inst_support)
# ARE DIFFERENCES AMONG TREATMENTS DISTINGUISHABLE?
summary(glht(study1_inst_support, 
             "scandal_treatmentethics - scandal_treatmentsexual = 0"))
summary(glht(study1_inst_support, 
             "scandal_treatmenttaxes - scandal_treatmentsexual = 0"))
summary(glht(study1_inst_support, 
             "scandal_treatmentethics - scandal_treatmenttaxes = 0"))

# SAVING POINT ESTIMATES/CIS FOR PLOTTING
study1_coef_df <- cbind(c(rep("specific", 3), rep("diffuse", 3)), 
                        rep(c("ethics", "sexual", "taxes"),2), 
                           c(study1_approval_bin$coefficients[2:4], 
                             study1_inst_support$coefficients[2:4]), 
                           c(confint(study1_approval_bin)[2:4,1], 
                             confint(study1_inst_support)[2:4,1]), 
                           c(confint(study1_approval_bin)[2:4,2], 
                             confint(study1_inst_support)[2:4,2]))

# STUDY 2: LOWER COURT

# SPECIFIC SUPPORT
study2_approval_bin <- lm(judge_approval_bin ~ scandal_treatment, data = study2)
summary(study2_approval_bin)
# ARE DIFFERENCES AMONG TREATMENTS DISTINGUISHABLE?
summary(glht(study2_approval_bin, 
             "scandal_treatmentEthics - scandal_treatmentHarassment = 0"))
summary(glht(study2_approval_bin, 
             "scandal_treatmentTaxFraud - scandal_treatmentHarassment = 0"))
summary(glht(study2_approval_bin, 
             "scandal_treatmentEthics - scandal_treatmentTaxFraud = 0"))

# DIFFUSE SUPPORT
study2_inst_support <- lm(inst_support~ scandal_treatment, data = study2)
summary(study2_inst_support)
# ARE DIFFERENCES AMONG TREATMENTS DISTINGUISHABLE?
summary(glht(study2_inst_support, 
             "scandal_treatmentEthics - scandal_treatmentHarassment = 0"))
summary(glht(study2_inst_support, 
             "scandal_treatmentTaxFraud - scandal_treatmentHarassment = 0"))
summary(glht(study2_inst_support, 
             "scandal_treatmentEthics - scandal_treatmentTaxFraud = 0"))

# SAVING POINT ESTIMATES/CIS FOR PLOTTING
study2_coef_df <- cbind(c(rep("specific", 3), rep("diffuse", 3)), 
                          rep(c("ethics", "sexual", "taxes"),2), 
                             c(study2_approval_bin$coefficients[2:4], 
                               study2_inst_support$coefficients[2:4]), 
                             c(confint(study2_approval_bin)[2:4,1], 
                               confint(study2_inst_support)[2:4,1]), 
                             c(confint(study2_approval_bin)[2:4,2], 
                               confint(study2_inst_support)[2:4,2]))

# STUDY 3: SCOTUS OPINION

# SPECIFIC SUPPORT
study3_approval_bin <- lm(opinion_approval_bin ~ justice_treatment, 
                          data = study3)
summary(study3_approval_bin)

# DIFFUSE SUPPORT
study3_inst_support <- lm(inst_support~ justice_treatment, 
                                        data = study3)
summary(study3_inst_support)

# SAVING POINT ESTIMATES/CIS FOR PLOTTING
study3_coef_df <- cbind(c("specific", "diffuse"), rep(c("kavanaugh"),2), 
                         c(study3_approval_bin$coefficients[2], 
                           study3_inst_support$coefficients[2]), 
                         c(confint(study3_approval_bin)[2,1], 
                           confint(study3_inst_support)[2,1]), 
                         c(confint(study3_approval_bin)[2,2], 
                           confint(study3_inst_support)[2,2]))

# CREATING FIGURE 1

pdf(file = "fig1.pdf", 
    family = "Times", height = 9, width=13)
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,11,2.75,.5))
plot(x=c(as.numeric(study3_coef_df[1,3]),
         as.numeric(study2_coef_df[3,3]),
         as.numeric(study2_coef_df[2,3]),
         as.numeric(study2_coef_df[1,3]),
         as.numeric(study1_coef_df[3,3]),
         as.numeric(study1_coef_df[2,3]),
         as.numeric(study1_coef_df[1,3]),
         as.numeric(study3_coef_df[2,3])+0.6,
         as.numeric(study2_coef_df[6,3])+0.6,
         as.numeric(study2_coef_df[5,3])+0.6,
         as.numeric(study2_coef_df[4,3])+0.6,
         as.numeric(study1_coef_df[6,3])+0.6,
         as.numeric(study1_coef_df[5,3])+0.6,
         as.numeric(study1_coef_df[4,3])+0.6), 
     y=rep(c(1,3,4,5,7,8,9),2),
     xlim=c(-.65,0.85),
     xaxt="n",
     ylim=c(0.5, 9.5),
     pch=rep(c(18,19,15,17,19,15,17),2),
     tck=-.02,
     cex.axis=0.9,
     cex=1.5,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE,
     panel.first = c(abline(v=0,lwd=2, col="gray70",lty=2),
                     abline(v=0.6,lwd=2, col="gray70",lty=2)))
segments(x0=c(as.numeric(study3_coef_df[1,4]),
              as.numeric(study2_coef_df[3,4]),
              as.numeric(study2_coef_df[2,4]),
              as.numeric(study2_coef_df[1,4]),
              as.numeric(study1_coef_df[3,4]),
              as.numeric(study1_coef_df[2,4]),
              as.numeric(study1_coef_df[1,4]),
              as.numeric(study3_coef_df[2,4])+0.6,
              as.numeric(study2_coef_df[6,4])+0.6,
              as.numeric(study2_coef_df[5,4])+0.6,
              as.numeric(study2_coef_df[4,4])+0.6,
              as.numeric(study1_coef_df[6,4])+0.6,
              as.numeric(study1_coef_df[5,4])+0.6,
              as.numeric(study1_coef_df[4,4])+0.6),
         x1=c(as.numeric(study3_coef_df[1,5]),
              as.numeric(study2_coef_df[3,5]),
              as.numeric(study2_coef_df[2,5]),
              as.numeric(study2_coef_df[1,5]),
              as.numeric(study1_coef_df[3,5]),
              as.numeric(study1_coef_df[2,5]),
              as.numeric(study1_coef_df[1,5]),
              as.numeric(study3_coef_df[2,5])+0.6,
              as.numeric(study2_coef_df[6,5])+0.6,
              as.numeric(study2_coef_df[5,5])+0.6,
              as.numeric(study2_coef_df[4,5])+0.6,
              as.numeric(study1_coef_df[6,5])+0.6,
              as.numeric(study1_coef_df[5,5])+0.6,
              as.numeric(study1_coef_df[4,5])+0.6),
         y0=rep(c(1,3,4,5,7,8,9),2))
axis(1,at=c(-.6, -.4, -.2, 0, .2), 
     labels = c("-60%", "-40%", "-20%", "0%", "20%"), cex.axis = 1.5)
axis(1,at=c(.4, .6, .8), 
     labels = c("-0.20", "0.00", "0.20"), cex.axis = 1.5)
par(mgp=c(0,8,0))
axis(2,at=c(1,4,8),
     las=2,
     labels=c("Supreme Court\nOpinion", "Lower\nCourt", "Supreme Court\nNomination"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
par(mgp=c(0,11,0))
mtext("Difference from Control", side = 1, at=-0.20, line = 3, cex = 2)
mtext("Difference from Control", side = 1, at=0.60, line = 3, cex = 2)
mtext("Specific Support", side = 3, at=-0.20, line=0.5, cex = 2.5)
mtext("Diffuse Support", side = 3, at=0.60, line=0.5, cex = 2.5)
text(c(as.numeric(study3_coef_df[1,3]),
       as.numeric(study2_coef_df[3,3]),
       as.numeric(study2_coef_df[2,3]),
       as.numeric(study2_coef_df[1,3]),
       as.numeric(study1_coef_df[3,3]),
       as.numeric(study1_coef_df[2,3]),
       as.numeric(study1_coef_df[1,3]),
       as.numeric(study3_coef_df[2,3])+0.6,
       as.numeric(study2_coef_df[6,3])+0.6,
       as.numeric(study2_coef_df[5,3])+0.6,
       as.numeric(study2_coef_df[4,3])+0.6,
       as.numeric(study1_coef_df[6,3])+0.6,
       as.numeric(study1_coef_df[5,3])+0.6,
       as.numeric(study1_coef_df[4,3])+0.6), 
     c(1,3,4,5,7,8,9) + 0.35,
     c(paste0(sprintf("%.0f",round(c(as.numeric(study3_coef_df[1,3]),
                                     as.numeric(study2_coef_df[3,3]),
                                     as.numeric(study2_coef_df[2,3]),
                                     as.numeric(study2_coef_df[1,3]),
                                     as.numeric(study1_coef_df[3,3]),
                                     as.numeric(study1_coef_df[2,3]),
                                     as.numeric(study1_coef_df[1,3]))*100, 0)),"%"),
       sprintf("%.2f",round(c(as.numeric(study3_coef_df[2,3]),
                              as.numeric(study2_coef_df[6,3]),
                              as.numeric(study2_coef_df[5,3]),
                              as.numeric(study2_coef_df[4,3]),
                              as.numeric(study1_coef_df[6,3]),
                              as.numeric(study1_coef_df[5,3]),
                              as.numeric(study1_coef_df[4,3])), 2))), cex=1.75)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x=0, y=1, legend=c("Ethics Scandal",
                          "Sexual Scandal",
                          "Financial Scandal",
                          "Kavanaugh"), lty=1, lwd=1, 
       pch = c(17,15,19,18), cex = 1.25)
dev.off()

################################################################################

# TABLE SI.1: SAMPLE DESCRIPTIVE STATISTICS

# NOTE: HOW THESE DEMOGRAPHICS ARE MEASURED BY THE RESEARCHERS WITH THE MTURK
# SAMPLE AND BY LUCID FOR THE LUCID SAMPLES DIFFER SLIGHTLY IN THEIR QUESTION
# WORDING.  WHERE NEEDED, WE RECODED THE ORIGINAL MEASURES TO THAT WE CAN
# PRESENT DEMOGRAPHIC INFO ACROSS SAMPLES UNIFORMLY.  PLEASE CONTACT THE
# RESEARCHERS FOR THE ORIGINAL CODINGS OF THE DEMOGRAPHIC INFO

# AGE
table(study1$age, useNA = "always")
round(prop.table(table(study1$age, useNA = "always")),3)*100
table(study2$age, useNA = "always")
round(prop.table(table(study2$age, useNA = "always")),3)*100
table(study3$age, useNA = "always")
round(prop.table(table(study3$age, useNA = "always")),3)*100

# GENDER
table(study1$female, useNA = "always")
round(prop.table(table(study1$female, useNA = "always")),3)*100
table(study2$female, useNA = "always")
round(prop.table(table(study2$female, useNA = "always")),3)*100
table(study3$female, useNA = "always")
round(prop.table(table(study3$female, useNA = "always")),3)*100

# RACE/ETHNICITY

# RESPONDENTS WHO DID NOT RECORD 1'S FOR ANY OF THE CATEGORIES (INCLUDING OTHER)
# ARE CODED AS MISSING; BECAUSE THIS AFFECTS THE DENOMINATOR, PERCENTAGES MUST
# BE CALCULATED MANUALLY
table(study1$white_nonhispanic, useNA = "always")
round(table(study1$white_nonhispanic, useNA = "always")/dim(study1)[1],3)*100
table(study1$white_hispanic, useNA = "always")
round(table(study1$white_hispanic, useNA = "always")/dim(study1)[1],3)*100
table(study1$black_nonhispanic, useNA = "always")
round(table(study1$black_nonhispanic, useNA = "always")/dim(study1)[1],3)*100
table(study1$black_hispanic, useNA = "always")
round(table(study1$black_hispanic, useNA = "always")/dim(study1)[1],3)*100
table(study1$asian, useNA = "always")
round(table(study1$asian, useNA = "always")/dim(study1)[1],3)*100
table(study1$other_race_eth, useNA = "always")
round(table(study1$other_race_eth, useNA = "always")/dim(study1)[1],3)*100
table(study1$white_nonhispanic==0 &
        study1$white_hispanic==0 &
        study1$black_nonhispanic==0 &
        study1$black_hispanic==0 &
        study1$asian==0 &
        study1$other_race_eth==0, useNA = "always")
round(table(study1$white_nonhispanic==0 &
              study1$white_hispanic==0 &
              study1$black_nonhispanic==0 &
              study1$black_hispanic==0 &
              study1$asian==0 &
              study1$other_race_eth==0, useNA = "always")/dim(study1)[1],3)*100

table(study2$white_nonhispanic, useNA = "always")
round(table(study2$white_nonhispanic, useNA = "always")/dim(study2)[1],3)*100
table(study2$white_hispanic, useNA = "always")
round(table(study2$white_hispanic, useNA = "always")/dim(study2)[1],3)*100
table(study2$black_nonhispanic, useNA = "always")
round(table(study2$black_nonhispanic, useNA = "always")/dim(study2)[1],3)*100
table(study2$black_hispanic, useNA = "always")
round(table(study2$black_hispanic, useNA = "always")/dim(study2)[1],3)*100
table(study2$asian, useNA = "always")
round(table(study2$asian, useNA = "always")/dim(study2)[1],3)*100
table(study2$other_race_eth, useNA = "always")
round(table(study2$other_race_eth, useNA = "always")/dim(study2)[1],3)*100
table(study2$white_nonhispanic==0 &
        study2$white_hispanic==0 &
        study2$black_nonhispanic==0 &
        study2$black_hispanic==0 &
        study2$asian==0 &
        study2$other_race_eth==0, useNA = "always")
round(table(study2$white_nonhispanic==0 &
              study2$white_hispanic==0 &
              study2$black_nonhispanic==0 &
              study2$black_hispanic==0 &
              study2$asian==0 &
              study2$other_race_eth==0, useNA = "always")/dim(study2)[1],3)*100

table(study3$white_nonhispanic, useNA = "always")
round(table(study3$white_nonhispanic, useNA = "always")/dim(study3)[1],3)*100
table(study3$white_hispanic, useNA = "always")
round(table(study3$white_hispanic, useNA = "always")/dim(study3)[1],3)*100
table(study3$black_nonhispanic, useNA = "always")
round(table(study3$black_nonhispanic, useNA = "always")/dim(study3)[1],3)*100
table(study3$black_hispanic, useNA = "always")
round(table(study3$black_hispanic, useNA = "always")/dim(study3)[1],3)*100
table(study3$asian, useNA = "always")
round(table(study3$asian, useNA = "always")/dim(study3)[1],3)*100
table(study3$other_race_eth, useNA = "always")
round(table(study3$other_race_eth, useNA = "always")/dim(study3)[1],3)*100
table(study3$white_nonhispanic==0 &
        study3$white_hispanic==0 &
        study3$black_nonhispanic==0 &
        study3$black_hispanic==0 &
        study3$asian==0 &
        study3$other_race_eth==0, useNA = "always")
round(table(study3$white_nonhispanic==0 &
              study3$white_hispanic==0 &
              study3$black_nonhispanic==0 &
              study3$black_hispanic==0 &
              study3$asian==0 &
              study3$other_race_eth==0, useNA = "always")/dim(study3)[1],3)*100

# EDUCATION
table(study1$educ, useNA = "always")
round(prop.table(table(study1$educ, useNA = "always")),3)*100
table(study2$educ, useNA = "always")
round(prop.table(table(study2$educ, useNA = "always")),3)*100
table(study3$educ, useNA = "always")
round(prop.table(table(study3$educ, useNA = "always")),3)*100

# INCOME
table(study1$income, useNA = "always")
round(prop.table(table(study1$income, useNA = "always")),3)*100
table(study2$income, useNA = "always")
round(prop.table(table(study2$income, useNA = "always")),3)*100
table(study3$income, useNA = "always")
round(prop.table(table(study3$income, useNA = "always")),3)*100

# PARTY IDENTIFICATION
table(study1$pid, useNA = "always")
round(prop.table(table(study1$pid, useNA = "always")),3)*100
table(study2$pid, useNA = "always")
round(prop.table(table(study2$pid, useNA = "always")),3)*100
table(study3$pid, useNA = "always")
round(prop.table(table(study3$pid, useNA = "always")),3)*100

# IDEOLOGY
table(study1$ideology, useNA = "always")
round(prop.table(table(study1$ideology, useNA = "always")),3)*100
table(study2$ideology, useNA = "always")
round(prop.table(table(study2$ideology, useNA = "always")),3)*100
table(study3$ideology, useNA = "always")
round(prop.table(table(study3$ideology, useNA = "always")),3)*100

################################################################################

# SI SECTION C

# IN-TEXT DISCUSSION OF DIFFUSE SUPPORT INDEX, PAGE SI.17

# INTERNAL CONSISTENCY OF INDEX FOR EACH STUDY

alpha(cbind(study1$inst_support_Q1, study1$inst_support_Q2,
            study1$inst_support_Q3, study1$inst_support_Q4,
            study1$inst_support_Q5, study1$inst_support_Q6))

alpha(cbind(study2$inst_support_Q1, study2$inst_support_Q2,
            study2$inst_support_Q3, study2$inst_support_Q4,
            study2$inst_support_Q5, study2$inst_support_Q6))

alpha(cbind(study3$inst_support_Q1, study3$inst_support_Q2,
            study3$inst_support_Q3, study3$inst_support_Q4,
            study3$inst_support_Q5, study3$inst_support_Q6))

# IN-TEXT DISCUSSION OF ROBUSTNESS OF RESULTS TO ACCOUNTING FOR INATTENTIVENESS,
# PAGE SI.18 FOOTNOTE 39
#
# (MAIN RESULTS INCLUDE ALL RESPONDENTS IRRESPECTIVE OF ATTENTION CHECK PASSAGE,
# ROBUSTNESS CHECKS USE TREATMENT ASSIGNMENT AS AN INSTRUMENT AND ATTENTIVENESS
# AS THE TREATMENT FOR AN ESTIMATE OF THE COMPLIER AVERAGE CAUSAL EFFECT).

# WE CALCULATE THE CACEs WHEN RESPONDENTS WHO PASSED AT LEAST ONE ATTENTION 
# CHECK ARE CONSIDERED TREATED AND WHEN ONLY RESPONDENTS WHO PASSED BOTH 
# ATTENTION CHECKS ARE CONSIDERED TREATED; THESE CACEs REPRESENT THE LOWER AND 
# UPPER BOUNDS OF THE TRUE CACE (GERBER AND GREEN 2012, PGS. 164-165).

# CACEs CALCULATED USING INSTRUMENTAL VARIABLES APPROACH/TWO-STAGE LEAST SQUARES
# WITH ivreg, WHICH CAN ONLY ACCOMMODATE ONE TREATMENT AT A TIME.  THUS, WE 
# CONDUCT SEPARATE ANALYSES FOR EACH TREATMENT COMPARED TO THE CONTROL FOR EACH
# OUTCOME MEASURE IN EACH OF THE THREE STUDIES

# STUDY 1--SPECIFIC SUPPORT

study1$ethics <- ifelse(study1$scandal_treatment=="ethics", 1, 0)
study1$attentive_1ethics <- ifelse(study1$attention_score>=1 & 
                                     study1$scandal_treatment=="ethics", 1, 0)
study1$attentive_2ethics <- ifelse(study1$attention_score==2 & 
                                     study1$scandal_treatment=="ethics", 1, 0)

study1$sexual <- ifelse(study1$scandal_treatment=="sexual", 1, 0)
study1$attentive_1sexual <- ifelse(study1$attention_score>=1 & 
                                     study1$scandal_treatment=="sexual", 1, 0)
study1$attentive_2sexual <- ifelse(study1$attention_score==2 & 
                                   study1$scandal_treatment=="sexual", 1, 0)

study1$taxes <- ifelse(study1$scandal_treatment=="taxes", 1, 0)
study1$attentive_1taxes <- ifelse(study1$attention_score>=1 & 
                                    study1$scandal_treatment=="taxes", 1, 0)
study1$attentive_2taxes <- ifelse(study1$attention_score==2 & 
                                    study1$scandal_treatment=="taxes", 1, 0)

summary(ivreg(nom_approval_bin ~ attentive_1ethics | ethics, 
              data = study1[which(study1$scandal_treatment=="ethics" |
                                    study1$scandal_treatment=="control")]))
summary(ivreg(nom_approval_bin ~ attentive_2ethics | ethics, 
              data = study1[which(study1$scandal_treatment=="ethics" |
                              study1$scandal_treatment=="control")]))

summary(ivreg(nom_approval_bin ~ attentive_1sexual | sexual, 
              data = study1[which(study1$scandal_treatment=="sexual" |
                                    study1$scandal_treatment=="control")]))
summary(ivreg(nom_approval_bin ~ attentive_2sexual | sexual, 
              data = study1[which(study1$scandal_treatment=="sexual" |
                                    study1$scandal_treatment=="control")]))

summary(ivreg(nom_approval_bin ~ attentive_1taxes | taxes, 
              data = study1[which(study1$scandal_treatment=="taxes" |
                                    study1$scandal_treatment=="control")]))
summary(ivreg(nom_approval_bin ~ attentive_2taxes | taxes, 
              data = study1[which(study1$scandal_treatment=="taxes" |
                                    study1$scandal_treatment=="control")]))

# STUDY 1--DIFFUSE SUPPORT

summary(ivreg(inst_support ~ attentive_1ethics | ethics, 
              data = study1[which(study1$scandal_treatment=="ethics" |
                                    study1$scandal_treatment=="control")]))
summary(ivreg(inst_support ~ attentive_2ethics | ethics, 
              data = study1[which(study1$scandal_treatment=="ethics" |
                                    study1$scandal_treatment=="control")]))

summary(ivreg(inst_support ~ attentive_1sexual | sexual, 
              data = study1[which(study1$scandal_treatment=="sexual" |
                                    study1$scandal_treatment=="control")]))
summary(ivreg(inst_support ~ attentive_2sexual | sexual, 
              data = study1[which(study1$scandal_treatment=="sexual" |
                                    study1$scandal_treatment=="control")]))

summary(ivreg(inst_support ~ attentive_1taxes | taxes, 
              data = study1[which(study1$scandal_treatment=="taxes" |
                                    study1$scandal_treatment=="control")]))
summary(ivreg(inst_support ~ attentive_2taxes | taxes, 
              data = study1[which(study1$scandal_treatment=="taxes" |
                                    study1$scandal_treatment=="control")]))

# STUDY 2--SPECIFIC SUPPORT

study2$Ethics <- ifelse(study2$scandal_treatment=="Ethics", 1, 0)
study2$attentive_1Ethics <- ifelse(study2$attention_score>=1 & 
                                     study2$scandal_treatment=="Ethics", 1, 0)
study2$attentive_2Ethics <- ifelse(study2$attention_score==2 & 
                                    study2$scandal_treatment=="Ethics", 1, 0)

study2$Harassment <- ifelse(study2$scandal_treatment=="Harassment", 1, 0)
study2$attentive_1Harassment <- ifelse(study2$attention_score>=1 & 
                                         study2$scandal_treatment=="Harassment", 1, 0)
study2$attentive_2Harassment <- ifelse(study2$attention_score==2 & 
                                    study2$scandal_treatment=="Harassment", 1, 0)

study2$TaxFraud <- ifelse(study2$scandal_treatment=="TaxFraud", 1, 0)
study2$attentive_1TaxFraud <- ifelse(study2$attention_score>=1 & 
                                       study2$scandal_treatment=="TaxFraud", 1, 0)
study2$attentive_2TaxFraud <- ifelse(study2$attention_score==2 & 
                                   study2$scandal_treatment=="TaxFraud", 1, 0)

summary(ivreg(judge_approval_bin ~ attentive_1Ethics | Ethics, 
              data = study2[which(study2$scandal_treatment=="Ethics" |
                                    study2$scandal_treatment=="Cancer")]))
summary(ivreg(judge_approval_bin ~ attentive_2Ethics | Ethics, 
              data = study2[which(study2$scandal_treatment=="Ethics" |
                                    study2$scandal_treatment=="Cancer")]))

summary(ivreg(judge_approval_bin ~ attentive_1Harassment | Harassment, 
              data = study2[which(study2$scandal_treatment=="Harassment" |
                                    study2$scandal_treatment=="Cancer")]))
summary(ivreg(judge_approval_bin ~ attentive_2Harassment | Harassment, 
              data = study2[which(study2$scandal_treatment=="Harassment" |
                                    study2$scandal_treatment=="Cancer")]))

summary(ivreg(judge_approval_bin ~ attentive_1TaxFraud | TaxFraud, 
              data = study2[which(study2$scandal_treatment=="TaxFraud" |
                                    study2$scandal_treatment=="Cancer")]))
summary(ivreg(judge_approval_bin ~ attentive_2TaxFraud | TaxFraud, 
              data = study2[which(study2$scandal_treatment=="TaxFraud" |
                                    study2$scandal_treatment=="Cancer")]))

# STUDY 2--DIFFUSE SUPPORT

summary(ivreg(inst_support ~ attentive_1Ethics | Ethics, 
              data = study2[which(study2$scandal_treatment=="Ethics" |
                                    study2$scandal_treatment=="Cancer")]))
summary(ivreg(inst_support ~ attentive_2Ethics | Ethics, 
              data = study2[which(study2$scandal_treatment=="Ethics" |
                                    study2$scandal_treatment=="Cancer")]))

summary(ivreg(inst_support ~ attentive_1Harassment | Harassment, 
              data = study2[which(study2$scandal_treatment=="Harassment" |
                                    study2$scandal_treatment=="Cancer")]))
summary(ivreg(inst_support ~ attentive_2Harassment | Harassment, 
              data = study2[which(study2$scandal_treatment=="Harassment" |
                                    study2$scandal_treatment=="Cancer")]))

summary(ivreg(inst_support ~ attentive_1TaxFraud | TaxFraud, 
              data = study2[which(study2$scandal_treatment=="TaxFraud" |
                                    study2$scandal_treatment=="Cancer")]))
summary(ivreg(inst_support ~ attentive_2TaxFraud | TaxFraud, 
              data = study2[which(study2$scandal_treatment=="TaxFraud" |
                                    study2$scandal_treatment=="Cancer")]))

# STUDY 3--SPECIFIC SUPPORT

study3$Kavanaugh <- ifelse(study3$justice_treatment=="Kavanaugh", 1, 0)
study3$attentive_1Kavanaugh <- ifelse(study3$attention_score>=1 & 
                                        study3$justice_treatment=="Kavanaugh", 1, 0)
study3$attentive_2Kavanaugh <- ifelse(study3$attention_score==2 & 
                                    study3$justice_treatment=="Kavanaugh", 1, 0)

summary(ivreg(opinion_approval_bin ~ attentive_1Kavanaugh | Kavanaugh, 
              data = study3))
summary(ivreg(opinion_approval_bin ~ attentive_2Kavanaugh | Kavanaugh, 
              data = study3))

# STUDY 3--DIFFUSE SUPPORT
summary(ivreg(inst_support ~ attentive_1Kavanaugh | Kavanaugh, 
              data = study3))
summary(ivreg(inst_support ~ attentive_2Kavanaugh | Kavanaugh, 
              data = study3))

################################################################################

# TABLE SI.2: TREATMENT EFFECTS ON BINARY OUTCOMES (UNDERLYING MODELS FOR FIGURE
# 1)

texreg(l=list(study1_approval_bin, study2_approval_bin, study3_approval_bin,
              study1_inst_support, study2_inst_support, study3_inst_support),
       stars = c(0.05),
       custom.model.names = c("SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion",
                              "SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion"),
       include.rsquared = FALSE, include.adjrs = FALSE, include.rmse= FALSE,
       custom.coef.map = list("(Intercept)" = "Intercept", 
                              "scandal_treatmentethics" = "Ethics Scandal", 
                              "scandal_treatmentsexual" = "Sexual Scandal", 
                              "scandal_treatmenttaxes" = "Financial Scandal",
                              "scandal_treatmentEthics" = "Ethics Scandal", 
                              "scandal_treatmentHarassment" = "Sexual Scandal", 
                              "scandal_treatmentTaxFraud" = "Financial Scandal",
                              "justice_treatmentKavanaugh" = "Kavanaugh"),
       caption.above = TRUE, caption = "Effect of Judicial Scandal on Specific and Diffuse Support (OLS, 0-1 Scales)",
       custom.note="$^{*}$ denotes statistical significance at the $p<0.05$ level.  Models estimated using ordinary
       least squares regression (OLS).  Models include
       all respondents, irrespective of attention check passage.  Specific support outcomes are measured
       with binary indicators where responses indicating that respondents somewhat or strongly approve are coded as 1
       and all other_race_eth non-missing responses coded as 0.  Diffuse support is coded following \\citet[][]{gibson2003measuring} 
       where we dichotomize respondents' answers to indicate support for the judiciary or lack thereof, 
       sum the binary indicators, and rescale the final measure to range between 0 and 1.",
       label="table:mainresults")

################################################################################

# TABLE SI.3: TREATMENT EFFECTS ON BINARY OUTCOMES W/COVARIATES

# STUDY 1

study1_approval_bin_covars <- lm(nom_approval_bin ~ scandal_treatment + female + 
                                   age + educ + white_hispanic + black_nonhispanic + 
                                   black_hispanic + asian + other_race_eth + 
                                   income + pid + ideology + judic_knowledge, 
                                 data = study1)
summary(study1_approval_bin_covars)

study1_inst_support_covars <- lm(inst_support ~ scandal_treatment + female + 
                                   age + educ + white_hispanic + black_nonhispanic + 
                                   black_hispanic + asian + other_race_eth + 
                                   income + pid + ideology + judic_knowledge, 
                                 data = study1)
summary(study1_inst_support_covars)

# STUDY 2

study2_approval_bin_covars <- lm(judge_approval_bin ~ scandal_treatment + female + 
                                   age + educ + white_hispanic + black_nonhispanic + 
                                   black_hispanic + asian + other_race_eth + 
                                   income + pid + ideology + judic_knowledge, 
                                 data = study2)
summary(study2_approval_bin_covars)

study2_inst_support_covars <- lm(inst_support ~ scandal_treatment + female + 
                                   age + educ + white_hispanic + black_nonhispanic + 
                                   black_hispanic + asian + other_race_eth + 
                                   income + pid + ideology + judic_knowledge, 
                                 data = study2)
summary(study2_inst_support_covars)

# STUDY 3

study3_approval_bin_covars <- lm(opinion_approval_bin ~ justice_treatment + female + 
                                   age + educ + white_hispanic + black_nonhispanic + 
                                   black_hispanic + asian + other_race_eth + 
                                   income + pid + ideology + judic_knowledge, 
                                 data = study3)
summary(study3_approval_bin_covars)

study3_inst_support_covars <- lm(inst_support ~ justice_treatment + female + 
                                   age + educ + white_hispanic + black_nonhispanic + 
                                   black_hispanic + asian + other_race_eth + 
                                   income + pid + ideology + judic_knowledge, 
                                 data = study3)
summary(study3_inst_support_covars)

texreg(l=list(study1_approval_bin_covars, study2_approval_bin_covars,
              study3_approval_bin_covars, study1_inst_support_covars,
              study2_inst_support_covars, study3_inst_support_covars),
       stars = c(0.05),
       custom.model.names = c("SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion",
                              "SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion"),
       include.rsquared = FALSE, include.adjrs = FALSE, include.rmse= FALSE,
       custom.coef.map = list("(Intercept)" = "Intercept", 
                              "scandal_treatmentethics" = "Ethics Scandal", 
                              "scandal_treatmentsexual" = "Sexual Scandal", 
                              "scandal_treatmenttaxes" = "Financial Scandal",
                              "scandal_treatmentEthics" = "Ethics Scandal", 
                              "scandal_treatmentHarassment" = "Sexual Scandal", 
                              "scandal_treatmentTaxFraud" = "Financial Scandal",
                              "justice_treatmentKavanaugh" = "Kavanaugh"),
       caption.above = TRUE, caption = "Effect of Judicial Scandal on Specific and Diffuse Support (OLS w/Covariates, 
       0-1 Scales)",
       custom.note="$^{*}$ denotes statistical significance at the $p<0.05$ level.  Models estimated using ordinary
       least squares regression (OLS) and include the following pretreatment covariates: gender, education, ethnicity/race,
       income, party identification, ideology, and judicial knowledge.  Models include
       all respondents, irrespective of attention check passage.  Specific support outcomes are measured
       with binary indicators where responses indicating that respondents somewhat or strongly approve are coded as 1
       and all other non-missing responses coded as 0.  Diffuse support is coded following \\citet[][]{gibson2003measuring} 
       where we dichotomize respondents' answers to indicate support for the judiciary or lack thereof, 
       sum the binary indicators, and rescale the final measure to range between 0 and 1.",
       label="table:mainresults_covars")

################################################################################

# TABLE SI.4: TREATMENT EFFECTS ON ORDINAL SPECIFIC SUPPORT OUTCOMES (OLS)

study1_approval <- lm(nom_approval ~ scandal_treatment, data = study1)
summary(study1_approval)

study2_approval <- lm(judge_approval ~ scandal_treatment, 
                                  data = study2)
summary(study2_approval)

study3_approval <- lm(opinion_approval ~ justice_treatment, #+ treatment_margin,
                                    data = study3)
summary(study3_approval)

texreg(l=list(study1_approval, study2_approval, study3_approval),
       stars = c(0.05),
       custom.model.names = c("SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion"),
       include.rsquared = FALSE, include.adjrs = FALSE, include.rmse= FALSE,
       custom.coef.map = list("(Intercept)" = "Intercept", 
                              "scandal_treatmentethics" = "Ethics Scandal", 
                              "scandal_treatmentsexual" = "Sexual Scandal", 
                              "scandal_treatmenttaxes" = "Financial Scandal",
                              "scandal_treatmentEthics" = "Ethics Scandal", 
                              "scandal_treatmentHarassment" = "Sexual Scandal", 
                              "scandal_treatmentTaxFraud" = "Financial Scandal",
                              "justice_treatmentKavanaugh" = "Kavanaugh"),
       caption.above = TRUE, caption = "Effect of Judicial Scandal on Specific Support (OLS, Ordinal Scales)",
       custom.note="$^{*}$ denotes statistical significance at the $p<0.05$ level.  Models estimated using
       ordinary least squares regression (OLS).  Models include
       all respondents, irrespective of attention check passage.  Specific support outcomes are measured
       with ordinal indicators where responses on a 1-4 scale with respondents who strongly approve coded as 4
       and respondents who strongly disapprove coded as 1.  Respondents who indicated ``don't know'' are coded as
       NAs and excluded from the analysis.",
       label="table:specific_ols_ordinal")

################################################################################

# TABLE SI.5: TREATMENT EFFECTS ON ORDINAL SPECIFIC SUPPORT OUTCOMES FOR STUDY 2
# (MNL)

study2_approval_DK <- multinom(judge_approval_DK ~ scandal_treatment, data = study2)
summary(study2_approval_DK)

texreg(l=list(study2_approval_DK),
       stars=c(0.05),
       custom.coef.map = list("(Intercept)"="Intercept",
                              "scandal_treatmentEthics" = "Ethics Scandal", 
                              "scandal_treatmentHarassment" = "Sexual Scandal", 
                              "scandal_treatmentTaxFraud" = "Financial Scandal"),
       label = "table:specific_lower_mnl",
       caption.above = TRUE, 
       caption = "Effect of Judicial Scandal on Specific Support of Lower Court 
                  Judge (Multinomial Logistic Regression)",
       custom.note = "$^{*}$ denotes statistical significance at the $p<0.05$ 
                      level.  Models estimated using multinomial logistic 
                      regression to account for ``Don't know'' responses in the
                      two surveys fielded on Lucid.  Models include all 
                      respondents, irrespective of attention check passage.  
                      Our outcome variable  has five factor values\textemdash 
                      don't know (the baseline choice), strongly disapprove, 
                      somewhat disapprove, somewhat approve, and strongly 
                      approve.",
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE, 
       include.thresholds=TRUE, beside = TRUE)

################################################################################

# TABLE SI.6: TREATMENT EFFECTS ON ORDINAL SPECIFIC SUPPORT OUTCOMES FOR STUDY 3
# (MNL)

study3_approval_DK <- multinom(opinion_approval_DK ~ justice_treatment, 
                                      data = study3)
summary(study3_approval_DK)

texreg(l=list(study3_approval_DK),
       stars=c(0.05),
       custom.coef.map = list("(Intercept)"="Intercept",
                              "justice_treatmentKavanaugh"="Kavanaugh"),
       label = "table:specific_opinion_mnl",
       caption.above = TRUE, 
       caption = "Effect of Judicial Scandal on Specific Support of Supreme Court Opinion (Multinomial Logistic Regression)",
       custom.note = "$^{*}$ denotes statistical significance at the $p<0.05$ level.  Models estimated using
       multinomial logistic regression to account for ``Don't know'' responses in the two surveys fielded
       on Lucid.  Models include all respondents, irrespective of attention check passage.  Our outcome variable 
       has five factor values\textemdash don't know (the baseline choice),
       strongly disapprove, somewhat disapprove, somewhat approve, and strongly approve.",
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE, 
       include.thresholds=TRUE, beside = TRUE)

################################################################################

# TABLE SI.7: TREATMENT EFFECTS ON BINARY OUTCOMES W/ALL RANDOMIZED FACTORS

# STUDY 1

study1_approval_bin_int <- lm(nom_approval_bin ~ scandal_treatment*pres_treatment, 
                                data = study1)
summary(study1_approval_bin_int)

study1_inst_support_int <- lm(inst_support ~ scandal_treatment*pres_treatment, 
                                data = study1)
summary(study1_inst_support_int)

# STUDY 2

study2_approval_bin_int <- lm(judge_approval_bin~ scandal_treatment*pres_treatment,
                                  data = study2)
summary(study2_approval_bin_int)

study2_inst_support_int <- lm(inst_support~ scandal_treatment*pres_treatment,
                                  data = study2)
summary(study2_inst_support_int)

# STUDY 3

study3_approval_bin_int <- lm(opinion_approval_bin ~ justice_treatment*margin_treatment, 
                              data = study3)
summary(study3_approval_bin_int)

study3_inst_support_int <- lm(inst_support~ justice_treatment*margin_treatment, 
                              data = study3)
summary(study3_inst_support_int)

texreg(l=list(study1_approval_bin_int, study2_approval_bin_int, 
              study3_approval_bin_int, study1_inst_support_int,
              study2_inst_support_int, study3_inst_support_int),
       stars = c(0.05),
       custom.model.names = c("SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion",
                              "SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion"),
       include.rsquared = FALSE, include.adjrs = FALSE, include.rmse= FALSE,
       custom.coef.map = list("(Intercept)" = "Intercept", 
                              "scandal_treatmentethics" = "Ethics Scandal", 
                              "scandal_treatmentsexual" = "Sexual Scandal", 
                              "scandal_treatmenttaxes" = "Financial Scandal",
                              "scandal_treatmentEthics" = "Ethics Scandal", 
                              "scandal_treatmentHarassment" = "Sexual Scandal", 
                              "scandal_treatmentTaxFraud" = "Financial Scandal",
                              "pres_treatmentObama" = "Obama",
                              "pres_treatmentObama" = "Obama",
                              "scandal_treatmentethics:pres_treatmentObama" = "Ethics Scandal:Obama", 
                              "scandal_treatmentsexual:pres_treatmentObama" = "Sexual Scandal:Obama", 
                              "scandal_treatmenttaxes:pres_treatmentObama" = "Financial Scandal:Obama",
                              "scandal_treatmentEthics:pres_treatmentObama" = "Ethics Scandal:Obama", 
                              "scandal_treatmentHarassment:pres_treatmentObama" = "Sexual Scandal:Obama", 
                              "scandal_treatmentTaxFraud:pres_treatmentObama" = "Financial Scandal:Obama",
                              "justice_treatmentKavanaugh" = "Kavanaugh",
                              "margin_treatmentUnanimous" = "Unanimous",
                              "justice_treatmentKavanaugh:margin_treatmentUnanimous" = "Kavanaugh:Unanimous"),
       caption.above = TRUE, caption = "Effect of Judicial Scandal on Specific and Diffuse Support (OLS, 0-1 Scales, 
       Including All Randomized Factors)",
       custom.note="$^{*}$ denotes statistical significance at the $p<0.05$ level.  Models estimated using ordinary
       least squares regression (OLS).  Models include
       all respondents, irrespective of attention check passage.  Specific support outcomes are measured
       with binary indicators where responses indicating that respondents somewhat or strongly approve are coded as 1
       and all other non-missing responses coded as 0.  Diffuse support is coded following \\citet[][]{gibson2003measuring} 
       where we dichotomize respondents' answers to indicate support for the judiciary or lack thereof, 
       sum the binary indicators, and rescale the final measure to range between 0 and 1.  All models interact our
       scandal treatments with the other factors we randomized in the vignettes \\textemdash the appointing president in 
       our nomination and lower court experiments (George W. Bush or Barack Obama) and the margin by which the Supreme Court
       decided \\textit{Ramos v. Louisiana} (9-0 or 6-3).",
       label="table:all_factors")

################################################################################

# FIGURE SI.1 & TABLE SI.8: COPARTISANSHIP-CONDITIONAL TREATMENT EFFECTS ON 
# BINARY OUTCOMES

# STUDY 1

study1_approval_bin_copart <- lm(nom_approval_bin ~ scandal_treatment*copartisan_treat, 
                                 data = study1)
summary(study1_approval_bin_copart)

study1_inst_support_copart <- lm(inst_support ~ scandal_treatment*copartisan_treat, 
                                 data = study1)
summary(study1_inst_support_copart)

study1_specific_copart_ests <- confint(glht(study1_approval_bin_copart, 
                                     linfct = c("`scandal_treatmentethics` = 0",
                                                "`scandal_treatmentsexual` = 0",
                                                "`scandal_treatmenttaxes` = 0",
                                                "`scandal_treatmentethics` + `scandal_treatmentethics:copartisan_treat` = 0",
                                                "`scandal_treatmentsexual` + `scandal_treatmentsexual:copartisan_treat` = 0",
                                                "`scandal_treatmenttaxes` + `scandal_treatmenttaxes:copartisan_treat` = 0"
                                                )),
                                calpha = univariate_calpha())$confint

study1_diffuse_copart_ests <- confint(glht(study1_inst_support_copart, 
                                            linfct = c("`scandal_treatmentethics` = 0",
                                                       "`scandal_treatmentsexual` = 0",
                                                       "`scandal_treatmenttaxes` = 0",
                                                       "`scandal_treatmentethics` + `scandal_treatmentethics:copartisan_treat` = 0",
                                                       "`scandal_treatmentsexual` + `scandal_treatmentsexual:copartisan_treat` = 0",
                                                       "`scandal_treatmenttaxes` + `scandal_treatmenttaxes:copartisan_treat` = 0"
                                            )),
                                       calpha = univariate_calpha())$confint

# STUDY 2

study2_approval_bin_copart <- lm(judge_approval_bin ~ scandal_treatment*copartisan_treat, 
                                 data = study2)
summary(study2_approval_bin_copart)

study2_inst_support_copart <- lm(inst_support ~ scandal_treatment*copartisan_treat, 
                                 data = study2)
summary(study2_inst_support_copart)

study2_specific_copart_ests <- confint(glht(study2_approval_bin_copart, 
                                            linfct = c("`scandal_treatmentEthics` = 0",
                                                       "`scandal_treatmentHarassment` = 0",
                                                       "`scandal_treatmentTaxFraud` = 0",
                                                       "`scandal_treatmentEthics` + `scandal_treatmentEthics:copartisan_treat` = 0",
                                                       "`scandal_treatmentHarassment` + `scandal_treatmentHarassment:copartisan_treat` = 0",
                                                       "`scandal_treatmentTaxFraud` + `scandal_treatmentTaxFraud:copartisan_treat` = 0"
                                            )),
                                       calpha = univariate_calpha())$confint

study2_diffuse_copart_ests <- confint(glht(study2_inst_support_copart, 
                                           linfct = c("`scandal_treatmentEthics` = 0",
                                                      "`scandal_treatmentHarassment` = 0",
                                                      "`scandal_treatmentTaxFraud` = 0",
                                                      "`scandal_treatmentEthics` + `scandal_treatmentEthics:copartisan_treat` = 0",
                                                      "`scandal_treatmentHarassment` + `scandal_treatmentHarassment:copartisan_treat` = 0",
                                                      "`scandal_treatmentTaxFraud` + `scandal_treatmentTaxFraud:copartisan_treat` = 0"
                                           )),
                                      calpha = univariate_calpha())$confint

# STUDY 3

study3_approval_bin_copart <- lm(opinion_approval_bin ~ justice_treatment*copartisan_treat, 
                                 data = study3)
summary(study3_approval_bin_copart)

study3_inst_support_copart <- lm(inst_support ~ justice_treatment*copartisan_treat, 
                                 data = study3)
summary(study3_inst_support_copart)

study3_specific_copart_ests <- confint(glht(study3_approval_bin_copart, 
                                            linfct = c("`justice_treatmentKavanaugh` = 0",
                                                       "`justice_treatmentKavanaugh` + `justice_treatmentKavanaugh:copartisan_treat` = 0"
                                            )),
                                       calpha = univariate_calpha())$confint

study3_diffuse_copart_ests <- confint(glht(study3_inst_support_copart, 
                                           linfct = c("`justice_treatmentKavanaugh` = 0",
                                                      "`justice_treatmentKavanaugh` + `justice_treatmentKavanaugh:copartisan_treat` = 0"
                                           )),
                                      calpha = univariate_calpha())$confint

# FIGURE SI.1

pdf(file = "figsi1.pdf", 
    family = "Times", height = 13, width=13)
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,11,2.75,.5))
plot(x=c(as.numeric(study3_specific_copart_ests[2,1]),
         as.numeric(study3_specific_copart_ests[1,1]),
         as.numeric(study2_specific_copart_ests[6,1]),
         as.numeric(study2_specific_copart_ests[3,1]),
         as.numeric(study2_specific_copart_ests[5,1]),
         as.numeric(study2_specific_copart_ests[2,1]),
         as.numeric(study2_specific_copart_ests[4,1]),
         as.numeric(study2_specific_copart_ests[1,1]),
         as.numeric(study1_specific_copart_ests[6,1]),
         as.numeric(study1_specific_copart_ests[3,1]),
         as.numeric(study1_specific_copart_ests[5,1]),
         as.numeric(study1_specific_copart_ests[2,1]),
         as.numeric(study1_specific_copart_ests[4,1]),
         as.numeric(study1_specific_copart_ests[1,1]),
         as.numeric(study3_diffuse_copart_ests[2,1])+0.6,
         as.numeric(study3_diffuse_copart_ests[1,1])+0.6,
         as.numeric(study2_diffuse_copart_ests[6,1])+0.6,
         as.numeric(study2_diffuse_copart_ests[3,1])+0.6,
         as.numeric(study2_diffuse_copart_ests[5,1])+0.6,
         as.numeric(study2_diffuse_copart_ests[2,1])+0.6,
         as.numeric(study2_diffuse_copart_ests[4,1])+0.6,
         as.numeric(study2_diffuse_copart_ests[1,1])+0.6,
         as.numeric(study1_diffuse_copart_ests[6,1])+0.6,
         as.numeric(study1_diffuse_copart_ests[3,1])+0.6,
         as.numeric(study1_diffuse_copart_ests[5,1])+0.6,
         as.numeric(study1_diffuse_copart_ests[2,1])+0.6,
         as.numeric(study1_diffuse_copart_ests[4,1])+0.6,
         as.numeric(study1_diffuse_copart_ests[1,1])+0.6), 
     y=rep(c(1,2,5,6,7,8,9,10,13,14,15,16,17,18),2),
     xlim=c(-.65,0.85),
     xaxt="n",
     ylim=c(0.5, 18.5),
     pch=rep(c(18,18,19,19,15,15,17,17,19,19,15,15,17,17),2),
     tck=-.02,
     cex.axis=0.9,
     cex=1.5,
     ylab="",
     yaxt="n",
     xlab="",
     col = c("black","gray70"),
     axes = FALSE,
     panel.first = c(abline(v=0,lwd=2, col="gray70",lty=2),
                     abline(v=0.6,lwd=2, col="gray70",lty=2)))
segments(x0=c(as.numeric(study3_specific_copart_ests[2,2]),
              as.numeric(study3_specific_copart_ests[1,2]),
              as.numeric(study2_specific_copart_ests[6,2]),
              as.numeric(study2_specific_copart_ests[3,2]),
              as.numeric(study2_specific_copart_ests[5,2]),
              as.numeric(study2_specific_copart_ests[2,2]),
              as.numeric(study2_specific_copart_ests[4,2]),
              as.numeric(study2_specific_copart_ests[1,2]),
              as.numeric(study1_specific_copart_ests[6,2]),
              as.numeric(study1_specific_copart_ests[3,2]),
              as.numeric(study1_specific_copart_ests[5,2]),
              as.numeric(study1_specific_copart_ests[2,2]),
              as.numeric(study1_specific_copart_ests[4,2]),
              as.numeric(study1_specific_copart_ests[1,2]),
              as.numeric(study3_diffuse_copart_ests[2,2])+0.6,
              as.numeric(study3_diffuse_copart_ests[1,2])+0.6,
              as.numeric(study2_diffuse_copart_ests[6,2])+0.6,
              as.numeric(study2_diffuse_copart_ests[3,2])+0.6,
              as.numeric(study2_diffuse_copart_ests[5,2])+0.6,
              as.numeric(study2_diffuse_copart_ests[2,2])+0.6,
              as.numeric(study2_diffuse_copart_ests[4,2])+0.6,
              as.numeric(study2_diffuse_copart_ests[1,2])+0.6,
              as.numeric(study1_diffuse_copart_ests[6,2])+0.6,
              as.numeric(study1_diffuse_copart_ests[3,2])+0.6,
              as.numeric(study1_diffuse_copart_ests[5,2])+0.6,
              as.numeric(study1_diffuse_copart_ests[2,2])+0.6,
              as.numeric(study1_diffuse_copart_ests[4,2])+0.6,
              as.numeric(study1_diffuse_copart_ests[1,2])+0.6),
         x1=c(as.numeric(study3_specific_copart_ests[2,3]),
              as.numeric(study3_specific_copart_ests[1,3]),
              as.numeric(study2_specific_copart_ests[6,3]),
              as.numeric(study2_specific_copart_ests[3,3]),
              as.numeric(study2_specific_copart_ests[5,3]),
              as.numeric(study2_specific_copart_ests[2,3]),
              as.numeric(study2_specific_copart_ests[4,3]),
              as.numeric(study2_specific_copart_ests[1,3]),
              as.numeric(study1_specific_copart_ests[6,3]),
              as.numeric(study1_specific_copart_ests[3,3]),
              as.numeric(study1_specific_copart_ests[5,3]),
              as.numeric(study1_specific_copart_ests[2,3]),
              as.numeric(study1_specific_copart_ests[4,3]),
              as.numeric(study1_specific_copart_ests[1,3]),
              as.numeric(study3_diffuse_copart_ests[2,3])+0.6,
              as.numeric(study3_diffuse_copart_ests[1,3])+0.6,
              as.numeric(study2_diffuse_copart_ests[6,3])+0.6,
              as.numeric(study2_diffuse_copart_ests[3,3])+0.6,
              as.numeric(study2_diffuse_copart_ests[5,3])+0.6,
              as.numeric(study2_diffuse_copart_ests[2,3])+0.6,
              as.numeric(study2_diffuse_copart_ests[4,3])+0.6,
              as.numeric(study2_diffuse_copart_ests[1,3])+0.6,
              as.numeric(study1_diffuse_copart_ests[6,3])+0.6,
              as.numeric(study1_diffuse_copart_ests[3,3])+0.6,
              as.numeric(study1_diffuse_copart_ests[5,3])+0.6,
              as.numeric(study1_diffuse_copart_ests[2,3])+0.6,
              as.numeric(study1_diffuse_copart_ests[4,3])+0.6,
              as.numeric(study1_diffuse_copart_ests[1,3])+0.6),
         y0=rep(c(1,2,5,6,7,8,9,10,13,14,15,16,17,18),2),
         col = c("black","gray70"))
axis(1,at=c(-.6, -.4, -.2, 0, .2), 
     labels = c("-60%", "-40%", "-20%", "0%", "20%"), cex.axis = 1.5)
axis(1,at=c(.4, .6, .8), 
     labels = c("-0.20", "0.00", "0.20"), cex.axis = 1.5)
par(mgp=c(0,8,0))
axis(2,at=c(1.5,7.5,15.5),
     las=2,
     labels=c("Supreme Court\nOpinion", "Lower\nCourt", "Supreme Court\nNomination"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
par(mgp=c(0,11,0))
mtext("Difference from Control", side = 1, at=-0.20, line = 3, cex = 2)
mtext("Difference from Control", side = 1, at=0.60, line = 3, cex = 2)
mtext("Specific Support", side = 3, at=-0.20, line=0.5, cex = 2.5)
mtext("Diffuse Support", side = 3, at=0.60, line=0.5, cex = 2.5)
text(c(as.numeric(study3_specific_copart_ests[2,1]),
       as.numeric(study3_specific_copart_ests[1,1]),
       as.numeric(study2_specific_copart_ests[6,1]),
       as.numeric(study2_specific_copart_ests[3,1]),
       as.numeric(study2_specific_copart_ests[5,1]),
       as.numeric(study2_specific_copart_ests[2,1]),
       as.numeric(study2_specific_copart_ests[4,1]),
       as.numeric(study2_specific_copart_ests[1,1]),
       as.numeric(study1_specific_copart_ests[6,1]),
       as.numeric(study1_specific_copart_ests[3,1]),
       as.numeric(study1_specific_copart_ests[5,1]),
       as.numeric(study1_specific_copart_ests[2,1]),
       as.numeric(study1_specific_copart_ests[4,1]),
       as.numeric(study1_specific_copart_ests[1,1]),
       as.numeric(study3_diffuse_copart_ests[2,1])+0.6,
       as.numeric(study3_diffuse_copart_ests[1,1])+0.6,
       as.numeric(study2_diffuse_copart_ests[6,1])+0.6,
       as.numeric(study2_diffuse_copart_ests[3,1])+0.6,
       as.numeric(study2_diffuse_copart_ests[5,1])+0.6,
       as.numeric(study2_diffuse_copart_ests[2,1])+0.6,
       as.numeric(study2_diffuse_copart_ests[4,1])+0.6,
       as.numeric(study2_diffuse_copart_ests[1,1])+0.6,
       as.numeric(study1_diffuse_copart_ests[6,1])+0.6,
       as.numeric(study1_diffuse_copart_ests[3,1])+0.6,
       as.numeric(study1_diffuse_copart_ests[5,1])+0.6,
       as.numeric(study1_diffuse_copart_ests[2,1])+0.6,
       as.numeric(study1_diffuse_copart_ests[4,1])+0.6,
       as.numeric(study1_diffuse_copart_ests[1,1])+0.6), 
     rep(c(1,2,5,6,7,8,9,10,13,14,15,16,17,18),2) + 0.55,
     c(paste0(sprintf("%.0f",round(c(as.numeric(study3_specific_copart_ests[2,1]),
                                     as.numeric(study3_specific_copart_ests[1,1]),
                                     as.numeric(study2_specific_copart_ests[6,1]),
                                     as.numeric(study2_specific_copart_ests[3,1]),
                                     as.numeric(study2_specific_copart_ests[5,1]),
                                     as.numeric(study2_specific_copart_ests[2,1]),
                                     as.numeric(study2_specific_copart_ests[4,1]),
                                     as.numeric(study2_specific_copart_ests[1,1]),
                                     as.numeric(study1_specific_copart_ests[6,1]),
                                     as.numeric(study1_specific_copart_ests[3,1]),
                                     as.numeric(study1_specific_copart_ests[5,1]),
                                     as.numeric(study1_specific_copart_ests[2,1]),
                                     as.numeric(study1_specific_copart_ests[4,1]),
                                     as.numeric(study1_specific_copart_ests[1,1]))*100, 0)),"%"),
       sprintf("%.2f",round(c(as.numeric(study3_diffuse_copart_ests[2,1]),
                              as.numeric(study3_diffuse_copart_ests[1,1]),
                              as.numeric(study2_diffuse_copart_ests[6,1]),
                              as.numeric(study2_diffuse_copart_ests[3,1]),
                              as.numeric(study2_diffuse_copart_ests[5,1]),
                              as.numeric(study2_diffuse_copart_ests[2,1]),
                              as.numeric(study2_diffuse_copart_ests[4,1]),
                              as.numeric(study2_diffuse_copart_ests[1,1]),
                              as.numeric(study1_diffuse_copart_ests[6,1]),
                              as.numeric(study1_diffuse_copart_ests[3,1]),
                              as.numeric(study1_diffuse_copart_ests[5,1]),
                              as.numeric(study1_diffuse_copart_ests[2,1]),
                              as.numeric(study1_diffuse_copart_ests[4,1]),
                              as.numeric(study1_diffuse_copart_ests[1,1])), 2))), cex=1.75)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x=0, y=1, legend=c("Ethics Scandal",
                          "Sexual Scandal",
                          "Financial Scandal",
                          "Kavanaugh",
                          "Not Copartisan",
                          "Copartisan"), 
       pch = c(17,15,19,18,NA,NA),
       lty = 1, lwd = 1,
       col = c(rep("black",4),"gray70", "black"), cex = 1.25)
dev.off()

# TABLE SI.8

texreg(l=list(study1_approval_bin_copart,  study2_approval_bin_copart, 
              study3_approval_bin_copart, study1_inst_support_copart, 
              study2_inst_support_copart, study3_inst_support_copart),
       stars = c(0.05),
       custom.model.names = c("SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion",
                              "SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion"),
       include.rsquared = FALSE, include.adjrs = FALSE, include.rmse= FALSE,
       custom.coef.map = list("(Intercept)" = "Intercept", 
                              "scandal_treatmentethics" = "Ethics Scandal", 
                              "scandal_treatmentsexual" = "Sexual Scandal", 
                              "scandal_treatmenttaxes" = "Financial Scandal",
                              "scandal_treatmentEthics" = "Ethics Scandal", 
                              "scandal_treatmentHarassment" = "Sexual Scandal", 
                              "scandal_treatmentTaxFraud" = "Financial Scandal",
                              "justice_treatmentKavanaugh" = "Kavanaugh",
                              "copartisan_treat" = "Copartisan",
                              "scandal_treatmentethics:copartisan_treat" = "Ethics Scandal:Copartisan", 
                              "scandal_treatmentsexual:copartisan_treat" = "Sexual Scandal:Copartisan", 
                              "scandal_treatmenttaxes:copartisan_treat" = "Financial Scandal:Copartisan",
                              "scandal_treatmentEthics:copartisan_treat" = "Ethics Scandal:Copartisan", 
                              "scandal_treatmentHarassment:copartisan_treat" = "Sexual Scandal:Copartisan", 
                              "scandal_treatmentTaxFraud:copartisan_treat" = "Financial Scandal:Copartisan",
                              "justice_treatmentKavanaugh:copartisan_treat" = "Kavanaugh:Copartisan"),
       caption.above = TRUE, caption = "Effect of Judicial Scandal on Specific and Diffuse Support (OLS, 0-1 Scales, 
       Copartisanship-Conditional)",
       custom.note="$^{*}$ denotes statistical significance at the $p<0.05$ level.  Models estimated using ordinary
       least squares regression (OLS).  Models include
       all respondents, irrespective of attention check passage.  Specific support outcomes are measured
       with binary indicators where responses indicating that respondents somewhat or strongly approve are coded as 1
       and all other non-missing responses coded as 0.  Diffuse support is coded following \\citet[][]{gibson2003measuring} 
       where we dichotomize respondents' answers to indicate support for the judiciary or lack thereof, 
       sum the binary indicators, and rescale the final measure to range between 0 and 1.  All models interact our
       scandal treatments with a binary indicator for whether the respondent shares the partisan affiliation of the featured
       judge.  For the nomination and lower court experiments, respondents are coded as copartisans if they are Democrats
       and the president who appointed the judge was Barack Obama or if they are Republicans and the president who appointed
       the judge is George W. Bush.  For the opinion experiment, respondents are coded as copartisans if they are Republicans
       (since both justices who could be featured were appointed by a Republican president).",
       label="table:copart")

################################################################################

# TABLE SI.9: PARTISANSHIP-CONDITIONAL TREATMENT EFFECTS ON BINARY OUTCOMES

study1$pid <- relevel(as.factor(study1$pid), ref = "Republican")
study2$pid  <- relevel(as.factor(study2$pid), ref = "Republican")
study3$pid  <- relevel(as.factor(study3$pid), ref = "Republican")

study1_approval_bin_pid <- lm(nom_approval_bin ~ scandal_treatment*pid, data = study1)
summary(study1_approval_bin_pid)

study1_inst_support_pid <- lm(inst_support ~ scandal_treatment*pid, data = study1)
summary(study1_inst_support_pid)

study2_approval_bin_pid <- lm(judge_approval_bin ~ scandal_treatment*pid, data = study2)
summary(study2_approval_bin_pid)

study2_inst_support_pid <- lm(inst_support ~ scandal_treatment*pid, data = study2)
summary(study2_inst_support_pid)

study3_approval_bin_pid <- lm(opinion_approval_bin ~ justice_treatment*pid, data = study3)
summary(study3_approval_bin_pid)

study3_inst_support_pid <- lm(inst_support ~ justice_treatment*pid, data = study3)
summary(study3_inst_support_pid)

texreg(l=list(study1_approval_bin_pid,  study2_approval_bin_pid, 
              study3_approval_bin_pid, study1_inst_support_pid, 
              study2_inst_support_pid, study3_inst_support_pid),
       stars = c(0.05),
       custom.model.names = c("SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion",
                              "SCOTUS Nomination",
                              "Lower Court",
                              "SCOTUS Opinion"),
       include.rsquared = FALSE, include.adjrs = FALSE, include.rmse= FALSE,
       custom.coef.map = list("(Intercept)" = "Intercept", 
                              "scandal_treatmentethics" = "Ethics Scandal", 
                              "scandal_treatmentsexual" = "Sexual Scandal", 
                              "scandal_treatmenttaxes" = "Financial Scandal",
                              "scandal_treatmentEthics" = "Ethics Scandal", 
                              "scandal_treatmentHarassment" = "Sexual Scandal", 
                              "scandal_treatmentTaxFraud" = "Financial Scandal",
                              "justice_treatmentKavanaugh" = "Kavanaugh",
                              "pidDemocrat" = "Democrat",
                              "pidD" = "Democrat",
                              "pidIndependent" = "Independent",
                              "pidI" = "Independent",
                              "pidOther" = "Other",
                              "pidO" = "Other",
                              "scandal_treatmentethics:pidDemocrat" = "Ethics Scandal:Democrat", 
                              "scandal_treatmentsexual:pidDemocrat" = "Sexual Scandal:Democrat", 
                              "scandal_treatmenttaxes:pidDemocrat" = "Financial Scandal:Democrat",
                              "scandal_treatmentEthics:pidDemocrat" = "Ethics Scandal:Democrat", 
                              "scandal_treatmentHarassment:pidDemocrat" = "Sexual Scandal:Democrat", 
                              "scandal_treatmentTaxFraud:pidDemocrat" = "Financial Scandal:Democrat",
                              "justice_treatmentKavanaugh:pidDemocrat" = "Kavanaugh:Democrat",
                              "scandal_treatmentethics:pidIndependent" = "Ethics Scandal:Independent", 
                              "scandal_treatmentsexual:pidIndependent" = "Sexual Scandal:Independent", 
                              "scandal_treatmenttaxes:pidIndependent" = "Financial Scandal:Independent",
                              "scandal_treatmentEthics:pidIndependent" = "Ethics Scandal:Independent", 
                              "scandal_treatmentHarassment:pidIndependent" = "Sexual Scandal:Independent", 
                              "scandal_treatmentTaxFraud:pidIndependent" = "Financial Scandal:Independent",
                              "justice_treatmentKavanaugh:pidIndependent" = "Kavanaugh:Independent",
                              "scandal_treatmentethics:pidOther" = "Ethics Scandal:Other", 
                              "scandal_treatmentsexual:pidOther" = "Sexual Scandal:Other", 
                              "scandal_treatmenttaxes:pidOther" = "Financial Scandal:Other",
                              "scandal_treatmentEthics:pidOther" = "Ethics Scandal:Other", 
                              "scandal_treatmentHarassment:pidOther" = "Sexual Scandal:Other", 
                              "scandal_treatmentTaxFraud:pidOther" = "Financial Scandal:Other",
                              "justice_treatmentKavanaugh:pidOther" = "Kavanaugh:Other"),
       caption.above = TRUE, caption = "Effect of Judicial Scandal on Specific and Diffuse Support (OLS, 0-1 Scales, 
       Partisanship-Conditional)",
       custom.note="$^{*}$ denotes statistical significance at the $p<0.05$ level.  Models estimated using ordinary
       least squares regression (OLS).  Models include
       all respondents, irrespective of attention check passage.  Specific support outcomes are measured
       with binary indicators where responses indicating that respondents somewhat or strongly approve are coded as 1
       and all other non-missing responses coded as 0.  Diffuse support is coded following \\citet[][]{gibson2003measuring} 
       where we dichotomize respondents' answers to indicate support for the judiciary or lack thereof, 
       sum the binary indicators, and rescale the final measure to range between 0 and 1.  All models interact our
       scandal treatments with binary indicators for whether the respondent identifies as a Democrat, Independent, or
       an unspecified party (Other), with identification as a Republican as the reference category.",
       label="table:partisanship")

################################################################################

# TABLE SI.10: KNOWLEDGE-CONDITIONAL TREATMENT EFFECTS IN STUDY 3

study3_approval_bin_know <- lm(opinion_approval_bin ~ justice_treatment*judic_knowledge, 
                                               data = study3)
summary(study3_approval_bin_know)

study3_inst_support_know <- lm(inst_support ~ justice_treatment*judic_knowledge, 
                                                data = study3)
summary(study3_inst_support_know)

texreg(l=list(study3_approval_bin_know, 
              study3_inst_support_know),
  stars = c(0.05),
  custom.model.names = c("Specific Support", "Diffuse Support"),
  include.rsquared = FALSE, include.adjrs = FALSE, include.rmse= FALSE,
  custom.coef.map = list("(Intercept)" = "Intercept",
                         "justice_treatmentKavanaugh" = "Kavanaugh",
                         "judic_knowledge" = "Judicial Knowledge",
                         "justice_treatmentKavanaugh:judic_knowledge" = "Kavanaugh:Judicial Knowledge"),
  caption.above = TRUE, caption = "Effect of Kavanaugh Authorship on Specific 
       and Diffuse Support (OLS, 0-1 Scales, 
       Knowledge-Conditional)",
  custom.note="$^{*}$ denotes statistical significance at the $p<0.05$ level.  
       Models estimated using ordinary least squares regression (OLS).  Models 
       include all respondents, irrespective of attention check passage.  
       Specific support outcome is measured with a binary indicator where 
       responses indicating that respondents somewhat or strongly approve are 
       coded as 1 and all other non-missing responses coded as 0.  Diffuse 
       support is coded following \\citet[][]{gibson2003measuring} where we 
       dichotomize respondents' answers to indicate support for the judiciary or
       lack thereof, sum the binary indicators, and rescale the final measure to
       range between 0 and 1.  Judicial knowledge is measured by scaling 
       respondents' answers to the standard three-question battery used by the 
       American National Election Studies \\citet[see][]{gibsoncaldeira2009book}.",
  label="table:know_cond")
