# library(AER)
# library(car)
library(data.table)
library(fixest)
library(mediation)
library(multcomp)
library(psych)
# library(stringr)
library(texreg)

set.seed(1989) # <3 TS

# set working directory to where replication files are stored locally
setwd("")

# read in data files--exp_data_lucid.csv has data for both Lucid-based experiments,
# since these were conducted with a split sample from a larger survey; exp_data_cloud.csv
# has data only for the FAA experiment

exp_data_lucid <- fread("exp_data_lucid.csv", header = TRUE, stringsAsFactors = FALSE)

exp_data_cloud <- fread("exp_data_cloud.csv", header = TRUE, stringsAsFactors = FALSE)
# SETTING THE REFERENCE LEVEL FOR THE TREATMENT INDICATOR
exp_data_cloud$FAA_treatment <- relevel(as.factor(exp_data_cloud$FAA_treatment), ref = "Control")

################################################################################
################################################################################
################################################################################

# ANALYSIS FOR FIGURE 1 AND TABLES SI.2-SI.4

# THE FOLLOWING FOUR REGRESSION MODELS ESTIMATE THE EFFECT OF TREATMENT ON THE
# OUTCOMES OF INTEREST FOR THE EXECUTIVE BRANCH EXPERIMENT

WSJ_pres_approval_model <- lm(WSJ_pres_approval_bin ~ WSJ_condition, data = exp_data_lucid)
summary(WSJ_pres_approval_model)

WSJ_pres_exec_branch_model <- lm(WSJ_pres_exec_branch_bin ~ WSJ_condition, data = exp_data_lucid)
summary(WSJ_pres_exec_branch_model)

WSJ_exec_branch_competence_model <- lm(WSJ_competence ~ WSJ_condition, data = exp_data_lucid)
summary(WSJ_exec_branch_competence_model)

WSJ_exec_branch_legitimacy_model <- lm(WSJ_legitimacy ~ WSJ_condition, data = exp_data_lucid)
summary(WSJ_exec_branch_legitimacy_model)

# THE FOLLOWING FOUR REGRESSION MODELS ESTIMATE THE EFFECT OF TREATMENT ON THE
# OUTCOMES OF INTEREST FOR THE FDA EXPERIMENT

FDA_pres_approval_model <- lm(FDA_pres_approval_bin ~ FDA_condition, data = exp_data_lucid)
summary(FDA_pres_approval_model)

FDA_pres_FDA_model <- lm(FDA_pres_FDA_bin ~ FDA_condition, data = exp_data_lucid)
summary(FDA_pres_FDA_model)

FDA_commissioner_approval_model <- lm(FDA_commissioner_approval_bin ~ FDA_condition, data = exp_data_lucid)
summary(FDA_commissioner_approval_model)

FDA_drug_approval_model <- lm(FDA_drug_approval_bin ~ FDA_condition, data = exp_data_lucid)
summary(FDA_drug_approval_model)

FDA_FDA_competence_model <- lm(FDA_competence ~ FDA_condition, data = exp_data_lucid)
summary(FDA_FDA_competence_model)

FDA_FDA_legitimacy_model <- lm(FDA_legitimacy ~ FDA_condition, data = exp_data_lucid)
summary(FDA_FDA_legitimacy_model)

# THE FOLLOWING FOUR REGRESSION MODELS ESTIMATE THE EFFECT OF TREATMENT ON THE
# OUTCOMES OF INTEREST FOR THE FDA EXPERIMENT

FAA_pres_approval_model <- lm(FAA_pres_approval_bin ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_pres_approval_model)

FAA_pres_FAA_model <- lm(FAA_pres_FAA_bin ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_pres_FAA_model)

FAA_administrator_approval_model <- lm(FAA_administrator_approval_bin ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_administrator_approval_model)

FAA_malfunction_model <- lm(FAA_malfunction_bin ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_malfunction_model)

FAA_FAA_competence_model <- lm(FAA_competence ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_FAA_competence_model)

FAA_FAA_legitimacy_model <- lm(FAA_legitimacy ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_FAA_legitimacy_model)

# TABLE SI.2
texreg(l = list(WSJ_pres_approval_model, WSJ_pres_exec_branch_model, WSJ_exec_branch_competence_model,
                WSJ_exec_branch_legitimacy_model),
       file = "WSJ_table.tex",
       stars = 0.05/8,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation"),
       custom.model.names = c("Pres. Approval", "Pres. Exec. Handling", 
                              "Exec. Branch Competence", "Exec. Branch Legitimacy"),
       caption = "Vacancies in the Executive Branch",
       caption.above = TRUE,
       label = "table:WSJ_exp",
       custom.note = "$^{*}p<0.05$.  Omitted category is the control condition, where respondents
       are told only about how many nominees Biden has, not that most have not been confirmed.  Third
       and fourth models are scales based on two questions about appraisals of the executive branch's
       competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# TABLE SI.3
texreg(l = list(FDA_pres_approval_model, FDA_pres_FDA_model, 
                FDA_commissioner_approval_model, FDA_drug_approval_model,
                FDA_FDA_competence_model, FDA_FDA_legitimacy_model),
       file = "FDA_table.tex",
       stars = 0.05/12,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation"),
       custom.model.names = c("Pres. Approval", "Pres. FDA Handling",
                              "FDA Comm. Approval", "FDA Decision Approval",
                              "FDA Competence", "FDA Legitimacy"),
       caption = "Vacancies in the FDA",
       caption.above = TRUE,
       label = "table:FDA_exp",
       custom.note = "$^{*}p<0.05$.  Omitted category is the control condition,
       where respondents are not told that the FDA does not have a confirmed commissioner.  Third
       and fourth models are scales based on two questions about appraisals of the executive branch's
       competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# TABLE SI.4
texreg(l = list(FAA_pres_approval_model, FAA_pres_FAA_model, 
                FAA_administrator_approval_model, FAA_malfunction_model,
                FAA_FAA_competence_model, FAA_FAA_legitimacy_model),
       file = "FAA_table.tex",
       stars = 0.05/12,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation"),
       custom.model.names = c("Pres. Approval", "Pres. FAA Handling",
                              "FAA Admin. Approval", "FAA Handling Approval",
                              "FAA Competence", "FAA Legitimacy"),
       caption = "Vacancies in the FAA",
       caption.above = TRUE,
       label = "table:FAA_exp",
       custom.note = "$^{*}p<0.05$.  Omitted category is the control condition,
       where respondents are not told that the FAA does not have a confirmed administrator  Third
       and fourth models are scales based on two questions about appraisals of the FAA's
       competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# FIGURE 1

pdf(file = "unconditional_figure_all.pdf", family = "Times", height = 11, width = 14)
layout(matrix(c(1,2,3,4,4,4), nrow=2, byrow=TRUE), 
       widths = c(1/3, 1/3, 1/3), heights = c(0.80, 0.20))
par(mar=c(5.1,10,4,.7))
plot(NULL, ylim=c(0.5, 11.5), xlim=c(-0.40,0.40), axes=FALSE,  bg = "black",
     tck=-.02, cex.axis=0.9, cex=1.5, cex.main = 3,
     xlab="", ylab="", yaxt="n", xaxt="n", main="Executive Branch")
points(c(coef(WSJ_pres_approval_model)[2:3],
         coef(WSJ_pres_exec_branch_model)[2:3],
         coef(WSJ_exec_branch_competence_model)[2:3],
         coef(WSJ_exec_branch_legitimacy_model)[2:3]),
       c(11, 10, 8, 7, 5, 4, 2, 1),
       pch=rep(c(19,17), 4), cex=1.25,
       panel.first = c(abline(v=0,lwd=2, col="gray90",lty=2)))
segments(x0=c(confint(WSJ_pres_approval_model, level = 0.99375)[2:3,1],
              confint(WSJ_pres_exec_branch_model, level = 0.99375)[2:3,1],
              confint(WSJ_exec_branch_competence_model, level = 0.99375)[2:3,1],
              confint(WSJ_exec_branch_legitimacy_model, level = 0.99375)[2:3,1]),
         x1=c(confint(WSJ_pres_approval_model, level = 0.99375)[2:3,2],
              confint(WSJ_pres_exec_branch_model, level = 0.99375)[2:3,2],
              confint(WSJ_exec_branch_competence_model, level = 0.99375)[2:3,2],
              confint(WSJ_exec_branch_legitimacy_model, level = 0.99375)[2:3,2]),
         y0=c(11, 10, 8, 7, 5, 4, 2, 1), cex=1.25)
axis(1,at=c(-0.40, -0.20, 0.00, 0.20, 0.40),
     labels=sprintf("%.2f",c(-0.40, -0.20, 0.00, 0.20, 0.40)),cex.axis = 1.75, lwd=2)
axis(2,at=c(10.5, 7.5, 4.5, 1.5),
     las=2,
     labels=c("Presidential\nApproval", "President's\nHandling of\nExec. Branch", 
              "Exec. Branch\nCompetence", "Exec. Branch\nLegitimacy"),
     tck=0,
     lwd =0,
     line = 0,
     cex.axis=2)
mtext("Difference from Control", side=1, line = 3, cex =2)
#mtext("Clinton", side = 3, line=1, cex = 3.5)
text(c(coef(WSJ_pres_approval_model)[2:3],
       coef(WSJ_pres_exec_branch_model)[2:3],
       coef(WSJ_exec_branch_competence_model)[2:3],
       coef(WSJ_exec_branch_legitimacy_model)[2:3]),
     c(11, 10, 8, 7, 5, 4, 2, 1)+0.35,
     paste0(sprintf("%.2f",round(c(coef(WSJ_pres_approval_model)[2:3],
                                   coef(WSJ_pres_exec_branch_model)[2:3],
                                   coef(WSJ_exec_branch_competence_model)[2:3],
                                   coef(WSJ_exec_branch_legitimacy_model)[2:3]), 2))), cex=1.75)

par(mar=c(5.1,10,4,.7))
plot(NULL, ylim=c(0.5, 17.5), xlim=c(-0.40,0.40), axes=FALSE,  bg = "black",
     tck=-.02, cex.axis=0.9, cex=1.5, cex.main=3,
     xlab="", ylab="", yaxt="n", xaxt="n", main="FDA")
points(c(coef(FDA_pres_approval_model)[2:3],
         coef(FDA_pres_FDA_model)[2:3],
         coef(FDA_commissioner_approval_model)[2:3],
         coef(FDA_drug_approval_model)[2:3],
         coef(FDA_FDA_competence_model)[2:3],
         coef(FDA_FDA_legitimacy_model)[2:3]),
       c(17, 16, 14, 13, 11, 10, 8, 7, 5, 4, 2, 1),
       pch=rep(c(19,17), 4), cex=1.25,
       panel.first = c(abline(v=0,lwd=2, col="gray90",lty=2)))
segments(x0=c(confint(FDA_pres_approval_model, level = 1-(0.05/12))[2:3,1],
              confint(FDA_pres_FDA_model, level = 1-(0.05/12))[2:3,1],
              confint(FDA_commissioner_approval_model, level = 1-(0.05/12))[2:3,1],
              confint(FDA_drug_approval_model, level = 1-(0.05/12))[2:3,1],
              confint(FDA_FDA_competence_model, level = 1-(0.05/12))[2:3,1],
              confint(FDA_FDA_legitimacy_model, level = 1-(0.05/12))[2:3,1]),
         x1=c(confint(FDA_pres_approval_model, level = 1-(0.05/12))[2:3,2],
              confint(FDA_pres_FDA_model, level = 1-(0.05/12))[2:3,2],
              confint(FDA_commissioner_approval_model, level = 1-(0.05/12))[2:3,2],
              confint(FDA_drug_approval_model, level = 1-(0.05/12))[2:3,2],
              confint(FDA_FDA_competence_model, level = 1-(0.05/12))[2:3,2],
              confint(FDA_FDA_legitimacy_model, level = 1-(0.05/12))[2:3,2]),
         y0=c(17, 16, 14, 13, 11, 10, 8, 7, 5, 4, 2, 1), cex=1.25)
axis(1,at=c(-0.40, -0.20, 0.00, 0.20, 0.40),
     labels=sprintf("%.2f",c(-0.40, -0.20, 0.00, 0.20, 0.40)),cex.axis = 1.75, lwd=2)
axis(2,at=c(16.5, 13.5, 10.5, 7.5, 4.5, 1.5),
     las=2,
     labels=c("Presidential\nApproval", "President's\nHandling of\nFDA", 
              "FDA Commissioner\nApproval", "FDA Decision\nApproval",
              "FDA\nCompetence", "FDA\nLegitimacy"),
     tck=0,
     lwd =0,
     line = 0,
     cex.axis=2)
mtext("Difference from Control", side=1, line = 3, cex =2)
#mtext("Clinton", side = 3, line=1, cex = 3.5)
text(c(coef(FDA_pres_approval_model)[2:3],
       coef(FDA_pres_FDA_model)[2:3],
       coef(FDA_commissioner_approval_model)[2:3],
       coef(FDA_drug_approval_model)[2:3],
       coef(FDA_FDA_competence_model)[2:3],
       coef(FDA_FDA_legitimacy_model)[2:3]),
     c(17, 16, 14, 13, 11, 10, 8, 7, 5, 4, 2, 1)+0.50,
     paste0(sprintf("%.2f",round(c(coef(FDA_pres_approval_model)[2:3],
                                   coef(FDA_pres_FDA_model)[2:3],
                                   coef(FDA_commissioner_approval_model)[2:3],
                                   coef(FDA_drug_approval_model)[2:3],
                                   coef(FDA_FDA_competence_model)[2:3],
                                   coef(FDA_FDA_legitimacy_model)[2:3]), 2))), cex=1.75)
par(mar=c(5.1,10,4,.7))
plot(NULL, ylim=c(0.5, 17.5), xlim=c(-0.40,0.40), axes=FALSE,  bg = "black",
     tck=-.02, cex.axis=0.9, cex=1.5, cex.main=3,
     xlab="", ylab="", yaxt="n", xaxt="n", main="FAA")
points(c(coef(FAA_pres_approval_model)[2:3],
         coef(FAA_pres_FAA_model)[2:3],
         coef(FAA_administrator_approval_model)[2:3],
         coef(FAA_malfunction_model)[2:3],
         coef(FAA_FAA_competence_model)[2:3],
         coef(FAA_FAA_legitimacy_model)[2:3]),
       c(17, 16, 14, 13, 11, 10, 8, 7, 5, 4, 2, 1),
       pch=rep(c(19,17), 4), cex=1.25,
       panel.first = c(abline(v=0,lwd=2, col="gray90",lty=2)))
segments(x0=c(confint(FAA_pres_approval_model, level = 1-(0.05/12))[2:3,1],
              confint(FAA_pres_FAA_model, level = 1-(0.05/12))[2:3,1],
              confint(FAA_administrator_approval_model, level = 1-(0.05/12))[2:3,1],
              confint(FAA_malfunction_model, level = 1-(0.05/12))[2:3,1],
              confint(FAA_FAA_competence_model, level = 1-(0.05/12))[2:3,1],
              confint(FAA_FAA_legitimacy_model, level = 1-(0.05/12))[2:3,1]),
         x1=c(confint(FAA_pres_approval_model, level = 1-(0.05/12))[2:3,2],
              confint(FAA_pres_FAA_model, level = 1-(0.05/12))[2:3,2],
              confint(FAA_administrator_approval_model, level = 1-(0.05/12))[2:3,2],
              confint(FAA_malfunction_model, level = 1-(0.05/12))[2:3,2],
              confint(FAA_FAA_competence_model, level = 1-(0.05/12))[2:3,2],
              confint(FAA_FAA_legitimacy_model, level = 1-(0.05/12))[2:3,2]),
         y0=c(17, 16, 14, 13, 11, 10, 8, 7, 5, 4, 2, 1), cex=1.25)
axis(1,at=c(-0.40, -0.20, 0.00, 0.20, 0.40),
     labels=sprintf("%.2f",c(-0.40, -0.20, 0.00, 0.20, 0.40)),cex.axis = 1.75, lwd=2)
axis(2,at=c(16.5, 13.5, 10.5, 7.5, 4.5, 1.5),
     las=2,
     labels=c("Presidential\nApproval", "President's\nHandling of\nFAA", 
              "FAA Administrator\nApproval", "FAA Malfunction\nHandling",
              "FAA\nCompetence", "FAA\nLegitimacy"),
     tck=0,
     lwd =0,
     line = 0,
     cex.axis=2)
mtext("Difference from Control", side=1, line = 3, cex =2)
text(c(coef(FAA_pres_approval_model)[2:3],
       coef(FAA_pres_FAA_model)[2:3],
       coef(FAA_administrator_approval_model)[2:3],
       coef(FAA_malfunction_model)[2:3],
       coef(FAA_FAA_competence_model)[2:3],
       coef(FAA_FAA_legitimacy_model)[2:3]),
     c(17, 16, 14, 13, 11, 10, 8, 7, 5, 4, 2, 1)+0.50,
     paste0(sprintf("%.2f",round(c(coef(FAA_pres_approval_model)[2:3],
                                   coef(FAA_pres_FAA_model)[2:3],
                                   coef(FAA_administrator_approval_model)[2:3],
                                   coef(FAA_malfunction_model)[2:3],
                                   coef(FAA_FAA_competence_model)[2:3],
                                   coef(FAA_FAA_legitimacy_model)[2:3]), 2))), cex=1.75)

par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend("center",legend=c("Acting Official(s)",
                         "Acting Official(s) with Context"), 
       col="white", text.col = "white",
       title="Treatment Conditions", cex=2.50,
       ncol = 1, title.col = "black")
legend("center",legend=c("Acting Official(s)",
                         "Acting Official(s) with Context"),
       pch=c(19,17), col=c("black"),
       title=" ", cex=2,
       ncol = 1, bty = "n")
dev.off()

################################################################################

# ANALYSIS FOR FIGURE 2 AND TABLES SI.5-SI.7

# THE FOLLOWING FOUR REGRESSION MODELS ESTIMATE THE EFFECT OF TREATMENT INTERACTED 
# WITH COPARTISANSHIP ON THE OUTCOMES OF INTEREST FOR THE EXECUTIVE BRANCH EXPERIMENT

WSJ_pres_approval_int_model <- lm(WSJ_pres_approval_bin ~ WSJ_condition*copart, data = exp_data_lucid)
summary(WSJ_pres_approval_int_model)

WSJ_pres_exec_branch_int_model <- lm(WSJ_pres_exec_branch_bin ~ WSJ_condition*copart, data = exp_data_lucid)
summary(WSJ_pres_exec_branch_int_model)

WSJ_exec_branch_competence_int_model <- lm(WSJ_competence ~ WSJ_condition*copart, data = exp_data_lucid)
summary(WSJ_exec_branch_competence_int_model)

WSJ_exec_branch_legitimacy_int_model <- lm(WSJ_legitimacy ~ WSJ_condition*copart, data = exp_data_lucid)
summary(WSJ_exec_branch_legitimacy_int_model)

# THE FOLLOWING FOUR REGRESSION MODELS ESTIMATE THE EFFECT OF TREATMENT INTERACTED 
# WITH COPARTISANSHIP ON THE OUTCOMES OF INTEREST FOR THE FDA EXPERIMENT

FDA_pres_approval_int_model <- lm(FDA_pres_approval_bin ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_pres_approval_int_model)

FDA_pres_FDA_int_model <- lm(FDA_pres_FDA_bin ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_pres_FDA_int_model)

FDA_commissioner_approval_int_model <- lm(FDA_commissioner_approval_bin ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_commissioner_approval_int_model)

FDA_drug_approval_int_model <- lm(FDA_drug_approval_bin ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_drug_approval_int_model)

FDA_FDA_competence_int_model <- lm(FDA_competence ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_FDA_competence_int_model)

FDA_FDA_legitimacy_int_model <- lm(FDA_legitimacy ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_FDA_legitimacy_int_model)

# THE FOLLOWING FOUR REGRESSION MODELS ESTIMATE THE EFFECT OF TREATMENT INTERACTED 
# WITH COPARTISANSHIP ON THE OUTCOMES OF INTEREST FOR THE FAA EXPERIMENT

FAA_pres_approval_int_model <- lm(FAA_pres_approval_bin ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_pres_approval_int_model)

FAA_pres_FAA_int_model <- lm(FAA_pres_FAA_bin ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_pres_FAA_int_model)

FAA_administrator_approval_int_model <- lm(FAA_administrator_approval_bin ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_administrator_approval_int_model)

FAA_malfunction_int_model <- lm(FAA_malfunction_bin ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_malfunction_int_model)

FAA_FAA_competence_int_model <- lm(FAA_competence ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_FAA_competence_model)

FAA_FAA_legitimacy_int_model <- lm(FAA_legitimacy ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_FAA_legitimacy_model)

# TABLE SI.5
texreg(l = list(WSJ_pres_approval_int_model, WSJ_pres_exec_branch_int_model, WSJ_exec_branch_competence_int_model,
                WSJ_exec_branch_legitimacy_int_model),
       file = "WSJ_int_table.tex",
       stars = 0.05/16,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation",
                             "Pres. Copart.", "Acting:Pres. Copart", 
                             "Acting w/Explanation:Pres. Copart"),
       custom.model.names = c("Pres. Approval", "Pres. Exec. Handling",
                              "Exec. Branch Competence", "Exec. Branch Legitimacy"),
       caption = "Vacancies in the Executive Branch (Partisanship-Conditional)",
       caption.above = TRUE,
       label = "table:WSJ_exp",
       custom.note = "$^{*}p<0.05$.  Omitted category for experimental condition is the control condition, 
       where respondents are told only about how many nominees Biden has, not that most have not been confirmed.
       Omitted category for copartisanship is non-presidential copartisan, which is any respondent who does not
       identify as a Democrat.  Third and fourth models are scales based on two questions about appraisals 
       of the executive branch's competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# TABLE SI.6
texreg(l = list(FDA_pres_approval_int_model, FDA_pres_FDA_int_model, 
                FDA_commissioner_approval_int_model, FDA_drug_approval_int_model,
                FDA_FDA_competence_int_model, FDA_FDA_legitimacy_int_model),
       file = "FDA_int_table.tex",
       stars = 0.05/24,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation",
                             "Pres. Copart.", "Acting:Pres. Copart", 
                             "Acting w/Explanation:Pres. Copart"),
       custom.model.names = c("Pres. Approval", "Pres. FDA Handling",
                              "FDA Comm. Approval", "FDA Decision Approval",
                              "FDA Competence", "FDA Legitimacy"),
       caption = "Vacancies in the FDA (Partisanship-Conditional)",
       caption.above = TRUE,
       label = "table:FDA_exp",
       custom.note = "$^{*}p<0.05$.  Omitted category for experimental condition is the control condition, 
       where respondents are not told that the FDA does not have a confirmed commissioner. 
       Omitted category for copartisanship is non-presidential copartisan, which is any respondent who does not
       identify as a Democrat.  Fifth and sixth models are scales based on two questions about appraisals 
       of the FDA's competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# TABLE SI.7
texreg(l = list(FAA_pres_approval_int_model, FAA_pres_FAA_int_model, 
                FAA_administrator_approval_int_model, FAA_malfunction_int_model,
                FAA_FAA_competence_int_model, FAA_FAA_legitimacy_int_model),
       file = "FAA_int_table.tex",
       stars = 0.05/24,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation",
                             "Pres. Copart.", "Acting:Pres. Copart", 
                             "Acting w/Explanation:Pres. Copart"),
       custom.model.names = c("Pres. Approval", "Pres. FAA Handling",
                              "FAA Admin. Approval", "FAA Handling Approval",
                              "FAA Competence", "FAA Legitimacy"),
       caption = "Vacancies in the FAA (Partisanship-Conditional)",
       caption.above = TRUE,
       label = "table:FAA_exp",
       custom.note = "$^{*}p<0.05$.  Omitted category for experimental condition is the control condition, 
       where respondents are not told that the FAA does not have a confirmed commissioner. 
       Omitted category for copartisanship is non-presidential copartisan, which is any respondent who does not
       identify as a Democrat.  Fifth and sixth models are scales based on two questions about appraisals 
       of the FAA's competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# USING GLHT FUNCTION TO CALCULATE TREATMENT EFFECT ESTIMATES FOR EACH OUTCOME
# AGAINST THE CONTROL GROUP, HOLDING COPARTISANSHIP FIXED

WSJ_pres_approval_acting_noncopart <- glht(WSJ_pres_approval_int_model, linfct = "WSJ_conditionTreatment = 0")
WSJ_pres_approval_acting_context_noncopart <- glht(WSJ_pres_approval_int_model, linfct = "WSJ_conditionTreatment_Explain = 0")
WSJ_pres_approval_acting_copart <- glht(WSJ_pres_approval_int_model, linfct = "WSJ_conditionTreatment + WSJ_conditionTreatment:copart = 0")
WSJ_pres_approval_acting_context_copart <- glht(WSJ_pres_approval_int_model, linfct = "WSJ_conditionTreatment_Explain + WSJ_conditionTreatment_Explain:copart= 0")
WSJ_pres_exec_branch_acting_noncopart <- glht(WSJ_pres_exec_branch_int_model, linfct = "WSJ_conditionTreatment = 0")
WSJ_pres_exec_branch_acting_context_noncopart <- glht(WSJ_pres_exec_branch_int_model, linfct = "WSJ_conditionTreatment_Explain = 0")
WSJ_pres_exec_branch_acting_copart <- glht(WSJ_pres_exec_branch_int_model, linfct = "WSJ_conditionTreatment + WSJ_conditionTreatment:copart = 0")
WSJ_pres_exec_branch_acting_context_copart <- glht(WSJ_pres_exec_branch_int_model, linfct = "WSJ_conditionTreatment_Explain + WSJ_conditionTreatment_Explain:copart= 0")
WSJ_exec_branch_competence_acting_noncopart <- glht(WSJ_exec_branch_competence_int_model, linfct = "WSJ_conditionTreatment = 0")
WSJ_exec_branch_competence_acting_context_noncopart <- glht(WSJ_exec_branch_competence_int_model, linfct = "WSJ_conditionTreatment_Explain = 0")
WSJ_exec_branch_competence_acting_copart <- glht(WSJ_exec_branch_competence_int_model, linfct = "WSJ_conditionTreatment + WSJ_conditionTreatment:copart = 0")
WSJ_exec_branch_competence_acting_context_copart <- glht(WSJ_exec_branch_competence_int_model, linfct = "WSJ_conditionTreatment_Explain + WSJ_conditionTreatment_Explain:copart= 0")
WSJ_exec_branch_legitimacy_acting_noncopart <- glht(WSJ_exec_branch_legitimacy_int_model, linfct = "WSJ_conditionTreatment = 0")
WSJ_exec_branch_legitimacy_acting_context_noncopart <- glht(WSJ_exec_branch_legitimacy_int_model, linfct = "WSJ_conditionTreatment_Explain = 0")
WSJ_exec_branch_legitimacy_acting_copart <- glht(WSJ_exec_branch_legitimacy_int_model, linfct = "WSJ_conditionTreatment + WSJ_conditionTreatment:copart = 0")
WSJ_exec_branch_legitimacy_acting_context_copart <- glht(WSJ_exec_branch_legitimacy_int_model, linfct = "WSJ_conditionTreatment_Explain + WSJ_conditionTreatment_Explain:copart= 0")

FDA_pres_approval_acting_noncopart <- glht(FDA_pres_approval_int_model, linfct = "FDA_conditionTreatment = 0")
FDA_pres_approval_acting_context_noncopart <- glht(FDA_pres_approval_int_model, linfct = "FDA_conditionTreatment_Explain = 0")
FDA_pres_approval_acting_copart <- glht(FDA_pres_approval_int_model, linfct = "FDA_conditionTreatment + FDA_conditionTreatment:copart = 0")
FDA_pres_approval_acting_context_copart <- glht(FDA_pres_approval_int_model, linfct = "FDA_conditionTreatment_Explain + FDA_conditionTreatment_Explain:copart= 0")
FDA_pres_FDA_acting_noncopart <- glht(FDA_pres_FDA_int_model, linfct = "FDA_conditionTreatment = 0")
FDA_pres_FDA_acting_context_noncopart <- glht(FDA_pres_FDA_int_model, linfct = "FDA_conditionTreatment_Explain = 0")
FDA_pres_FDA_acting_copart <- glht(FDA_pres_FDA_int_model, linfct = "FDA_conditionTreatment + FDA_conditionTreatment:copart = 0")
FDA_pres_FDA_acting_context_copart <- glht(FDA_pres_FDA_int_model, linfct = "FDA_conditionTreatment_Explain + FDA_conditionTreatment_Explain:copart= 0")
FDA_commissioner_approval_acting_noncopart <- glht(FDA_commissioner_approval_int_model, linfct = "FDA_conditionTreatment = 0")
FDA_commissioner_approval_acting_context_noncopart <- glht(FDA_commissioner_approval_int_model, linfct = "FDA_conditionTreatment_Explain = 0")
FDA_commissioner_approval_acting_copart <- glht(FDA_commissioner_approval_int_model, linfct = "FDA_conditionTreatment + FDA_conditionTreatment:copart = 0")
FDA_commissioner_approval_acting_context_copart <- glht(FDA_commissioner_approval_int_model, linfct = "FDA_conditionTreatment_Explain + FDA_conditionTreatment_Explain:copart= 0")
FDA_drug_approval_acting_noncopart <- glht(FDA_drug_approval_int_model, linfct = "FDA_conditionTreatment = 0")
FDA_drug_approval_acting_context_noncopart <- glht(FDA_drug_approval_int_model, linfct = "FDA_conditionTreatment_Explain = 0")
FDA_drug_approval_acting_copart <- glht(FDA_drug_approval_int_model, linfct = "FDA_conditionTreatment + FDA_conditionTreatment:copart = 0")
FDA_drug_approval_acting_context_copart <- glht(FDA_drug_approval_int_model, linfct = "FDA_conditionTreatment_Explain + FDA_conditionTreatment_Explain:copart= 0")
FDA_FDA_competence_acting_noncopart <- glht(FDA_FDA_competence_int_model, linfct = "FDA_conditionTreatment = 0")
FDA_FDA_competence_acting_context_noncopart <- glht(FDA_FDA_competence_int_model, linfct = "FDA_conditionTreatment_Explain = 0")
FDA_FDA_competence_acting_copart <- glht(FDA_FDA_competence_int_model, linfct = "FDA_conditionTreatment + FDA_conditionTreatment:copart = 0")
FDA_FDA_competence_acting_context_copart <- glht(FDA_FDA_competence_int_model, linfct = "FDA_conditionTreatment_Explain + FDA_conditionTreatment_Explain:copart= 0")
FDA_FDA_legitimacy_acting_noncopart <- glht(FDA_FDA_legitimacy_int_model, linfct = "FDA_conditionTreatment = 0")
FDA_FDA_legitimacy_acting_context_noncopart <- glht(FDA_FDA_legitimacy_int_model, linfct = "FDA_conditionTreatment_Explain = 0")
FDA_FDA_legitimacy_acting_copart <- glht(FDA_FDA_legitimacy_int_model, linfct = "FDA_conditionTreatment + FDA_conditionTreatment:copart = 0")
FDA_FDA_legitimacy_acting_context_copart <- glht(FDA_FDA_legitimacy_int_model, linfct = "FDA_conditionTreatment_Explain + FDA_conditionTreatment_Explain:copart= 0")

FAA_pres_approval_acting_noncopart <- glht(FAA_pres_approval_int_model, linfct = "FAA_treatmentActing = 0")
FAA_pres_approval_acting_context_noncopart <- glht(FAA_pres_approval_int_model, linfct = "`FAA_treatmentActing with Context` = 0")
FAA_pres_approval_acting_copart <- glht(FAA_pres_approval_int_model, linfct = "FAA_treatmentActing + FAA_treatmentActing:copart = 0")
FAA_pres_approval_acting_context_copart <- glht(FAA_pres_approval_int_model, linfct = "`FAA_treatmentActing with Context` + `FAA_treatmentActing with Context:copart`= 0")
FAA_pres_FAA_acting_noncopart <- glht(FAA_pres_FAA_int_model, linfct = "FAA_treatmentActing = 0")
FAA_pres_FAA_acting_context_noncopart <- glht(FAA_pres_FAA_int_model, linfct = "`FAA_treatmentActing with Context` = 0")
FAA_pres_FAA_acting_copart <- glht(FAA_pres_FAA_int_model, linfct = "FAA_treatmentActing + FAA_treatmentActing:copart = 0")
FAA_pres_FAA_acting_context_copart <- glht(FAA_pres_FAA_int_model, linfct = "`FAA_treatmentActing with Context` + `FAA_treatmentActing with Context:copart`= 0")
FAA_administrator_approval_acting_noncopart <- glht(FAA_administrator_approval_int_model, linfct = "FAA_treatmentActing = 0")
FAA_administrator_approval_acting_context_noncopart <- glht(FAA_administrator_approval_int_model, linfct = "`FAA_treatmentActing with Context` = 0")
FAA_administrator_approval_acting_copart <- glht(FAA_administrator_approval_int_model, linfct = "FAA_treatmentActing + FAA_treatmentActing:copart = 0")
FAA_administrator_approval_acting_context_copart <- glht(FAA_administrator_approval_int_model, linfct = "`FAA_treatmentActing with Context` + `FAA_treatmentActing with Context:copart`= 0")
FAA_malfunction_acting_noncopart <- glht(FAA_malfunction_int_model, linfct = "FAA_treatmentActing = 0")
FAA_malfunction_acting_context_noncopart <- glht(FAA_malfunction_int_model, linfct = "`FAA_treatmentActing with Context` = 0")
FAA_malfunction_acting_copart <- glht(FAA_malfunction_int_model, linfct = "FAA_treatmentActing + FAA_treatmentActing:copart = 0")
FAA_malfunction_acting_context_copart <- glht(FAA_malfunction_int_model, linfct = "`FAA_treatmentActing with Context` + `FAA_treatmentActing with Context:copart`= 0")
FAA_FAA_competence_acting_noncopart <- glht(FAA_FAA_competence_int_model, linfct = "FAA_treatmentActing = 0")
FAA_FAA_competence_acting_context_noncopart <- glht(FAA_FAA_competence_int_model, linfct = "`FAA_treatmentActing with Context` = 0")
FAA_FAA_competence_acting_copart <- glht(FAA_FAA_competence_int_model, linfct = "FAA_treatmentActing + FAA_treatmentActing:copart = 0")
FAA_FAA_competence_acting_context_copart <- glht(FAA_FAA_competence_int_model, linfct = "`FAA_treatmentActing with Context` + `FAA_treatmentActing with Context:copart`= 0")
FAA_FAA_legitimacy_acting_noncopart <- glht(FAA_FAA_legitimacy_int_model, linfct = "FAA_treatmentActing = 0")
FAA_FAA_legitimacy_acting_context_noncopart <- glht(FAA_FAA_legitimacy_int_model, linfct = "`FAA_treatmentActing with Context` = 0")
FAA_FAA_legitimacy_acting_copart <- glht(FAA_FAA_legitimacy_int_model, linfct = "FAA_treatmentActing + FAA_treatmentActing:copart = 0")
FAA_FAA_legitimacy_acting_context_copart <- glht(FAA_FAA_legitimacy_int_model, linfct = "`FAA_treatmentActing with Context` + `FAA_treatmentActing with Context:copart`= 0")

# FIGURE 2
pdf(file = "part_conditional_figure_all.pdf", family = "Times", height = 16, width = 14)
layout(matrix(c(1,2,3,4,4,4), nrow=2, byrow=TRUE), 
       widths = c(1/3, 1/3, 1/3), heights = c(0.80, 0.20))
par(mar=c(5.1,10,4,.7))
plot(NULL, ylim=c(0.5, 19.5), xlim=c(-0.50,0.50), axes=FALSE,  bg = "black",
     tck=-.02, cex.axis=0.9, cex=1.5, cex.main = 3,
     xlab="", ylab="", yaxt="n", xaxt="n", main="Executive Branch")
points(c(coef(WSJ_pres_approval_acting_noncopart),
         coef(WSJ_pres_approval_acting_context_noncopart),
         coef(WSJ_pres_approval_acting_copart),
         coef(WSJ_pres_approval_acting_context_copart),
         coef(WSJ_pres_exec_branch_acting_noncopart),
         coef(WSJ_pres_exec_branch_acting_context_noncopart),
         coef(WSJ_pres_exec_branch_acting_copart),
         coef(WSJ_pres_exec_branch_acting_context_copart),
         coef(WSJ_exec_branch_competence_acting_noncopart),
         coef(WSJ_exec_branch_competence_acting_context_noncopart),
         coef(WSJ_exec_branch_competence_acting_copart),
         coef(WSJ_exec_branch_competence_acting_context_copart),
         coef(WSJ_exec_branch_legitimacy_acting_noncopart),
         coef(WSJ_exec_branch_legitimacy_acting_context_noncopart),
         coef(WSJ_exec_branch_legitimacy_acting_copart),
         coef(WSJ_exec_branch_legitimacy_acting_context_copart)),
       c(19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1),
       pch=rep(c(19, 17, 19, 17), 4), 
       col = c("gray80", "gray80", "black", "black"),
       cex=1.25,
       panel.first = c(abline(v=0,lwd=2, col="gray90",lty=2)))
segments(x0=c(confint(WSJ_pres_approval_acting_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_pres_approval_acting_context_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_pres_approval_acting_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_pres_approval_acting_context_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_pres_exec_branch_acting_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_pres_exec_branch_acting_context_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_pres_exec_branch_acting_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_pres_exec_branch_acting_context_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_exec_branch_competence_acting_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_exec_branch_competence_acting_context_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_exec_branch_competence_acting_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_exec_branch_competence_acting_context_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_exec_branch_legitimacy_acting_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_exec_branch_legitimacy_acting_context_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_exec_branch_legitimacy_acting_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2],
              confint(WSJ_exec_branch_legitimacy_acting_context_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[2]),
         x1=c(confint(WSJ_pres_approval_acting_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_pres_approval_acting_context_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_pres_approval_acting_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_pres_approval_acting_context_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_pres_exec_branch_acting_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_pres_exec_branch_acting_context_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_pres_exec_branch_acting_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_pres_exec_branch_acting_context_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_exec_branch_competence_acting_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_exec_branch_competence_acting_context_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_exec_branch_competence_acting_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_exec_branch_competence_acting_context_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_exec_branch_legitimacy_acting_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_exec_branch_legitimacy_acting_context_noncopart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_exec_branch_legitimacy_acting_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3],
              confint(WSJ_exec_branch_legitimacy_acting_context_copart, calpha = qnorm(0.003125, lower.tail = FALSE))$confint[3]),
         y0=c(19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1), 
         col = c("gray80", "gray80", "black", "black"), cex=1.25)
axis(1,at=c(-0.50, -0.25, 0.00, 0.25, 0.50),
     labels=sprintf("%.2f",c(-0.50, -0.25, 0.00, 0.25, 0.50)),cex.axis = 1.75, lwd=2)
axis(2,at=c(17.5, 12.5, 7.5, 2.5),
     las=2,
     labels=c("Presidential\nApproval", "President's\nHandling of\nExec. Branch", 
              "Exec. Branch\nCompetence", "Exec. Branch\nLegitimacy"),
     tck=0,
     lwd =0,
     line = 0,
     cex.axis=2)
mtext("Difference from Control", side=1, line = 3, cex =2)
text(c(coef(WSJ_pres_approval_acting_noncopart),
       coef(WSJ_pres_approval_acting_context_noncopart),
       coef(WSJ_pres_approval_acting_copart),
       coef(WSJ_pres_approval_acting_context_copart),
       coef(WSJ_pres_exec_branch_acting_noncopart),
       coef(WSJ_pres_exec_branch_acting_context_noncopart),
       coef(WSJ_pres_exec_branch_acting_copart),
       coef(WSJ_pres_exec_branch_acting_context_copart),
       coef(WSJ_exec_branch_competence_acting_noncopart),
       coef(WSJ_exec_branch_competence_acting_context_noncopart),
       coef(WSJ_exec_branch_competence_acting_copart),
       coef(WSJ_exec_branch_competence_acting_context_copart),
       coef(WSJ_exec_branch_legitimacy_acting_noncopart),
       coef(WSJ_exec_branch_legitimacy_acting_context_noncopart),
       coef(WSJ_exec_branch_legitimacy_acting_copart),
       coef(WSJ_exec_branch_legitimacy_acting_context_copart)),
     c(19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1)+0.35,
     paste0(sprintf("%.2f",round(c(coef(WSJ_pres_approval_acting_noncopart),
                                   coef(WSJ_pres_approval_acting_context_noncopart),
                                   coef(WSJ_pres_approval_acting_copart),
                                   coef(WSJ_pres_approval_acting_context_copart),
                                   coef(WSJ_pres_exec_branch_acting_noncopart),
                                   coef(WSJ_pres_exec_branch_acting_context_noncopart),
                                   coef(WSJ_pres_exec_branch_acting_copart),
                                   coef(WSJ_pres_exec_branch_acting_context_copart),
                                   coef(WSJ_exec_branch_competence_acting_noncopart),
                                   coef(WSJ_exec_branch_competence_acting_context_noncopart),
                                   coef(WSJ_exec_branch_competence_acting_copart),
                                   coef(WSJ_exec_branch_competence_acting_context_copart),
                                   coef(WSJ_exec_branch_legitimacy_acting_noncopart),
                                   coef(WSJ_exec_branch_legitimacy_acting_context_noncopart),
                                   coef(WSJ_exec_branch_legitimacy_acting_copart),
                                   coef(WSJ_exec_branch_legitimacy_acting_context_copart)), 2))), cex=1.75)

par(mar=c(5.1,10,4,.7))
plot(NULL, ylim=c(0.5, 29.5), xlim=c(-0.50,0.50), axes=FALSE,  bg = "black",
     tck=-.02, cex.axis=0.9, cex=1.5, cex.main = 3,
     xlab="", ylab="", yaxt="n", xaxt="n", main="FDA")
points(c(coef(FDA_pres_approval_acting_noncopart),
         coef(FDA_pres_approval_acting_context_noncopart),
         coef(FDA_pres_approval_acting_copart),
         coef(FDA_pres_approval_acting_context_copart),
         coef(FDA_pres_FDA_acting_noncopart),
         coef(FDA_pres_FDA_acting_context_noncopart),
         coef(FDA_pres_FDA_acting_copart),
         coef(FDA_pres_FDA_acting_context_copart),
         coef(FDA_commissioner_approval_acting_noncopart),
         coef(FDA_commissioner_approval_acting_context_noncopart),
         coef(FDA_commissioner_approval_acting_copart),
         coef(FDA_commissioner_approval_acting_context_copart),
         coef(FDA_drug_approval_acting_noncopart),
         coef(FDA_drug_approval_acting_context_noncopart),
         coef(FDA_drug_approval_acting_copart),
         coef(FDA_drug_approval_acting_context_copart),
         coef(FDA_FDA_competence_acting_noncopart),
         coef(FDA_FDA_competence_acting_context_noncopart),
         coef(FDA_FDA_competence_acting_copart),
         coef(FDA_FDA_competence_acting_context_copart),
         coef(FDA_FDA_legitimacy_acting_noncopart),
         coef(FDA_FDA_legitimacy_acting_context_noncopart),
         coef(FDA_FDA_legitimacy_acting_copart),
         coef(FDA_FDA_legitimacy_acting_context_copart)),
       c(29, 28, 27, 26, 24, 23, 22, 21, 19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1),
       pch=rep(c(19, 17, 19, 17), 4), 
       col = c("gray80", "gray80", "black", "black"),
       cex=1.25,
       panel.first = c(abline(v=0,lwd=2, col="gray90",lty=2)))
segments(x0=c(confint(FDA_pres_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_pres_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_pres_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_pres_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_pres_FDA_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_pres_FDA_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_pres_FDA_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_pres_FDA_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_commissioner_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_commissioner_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_commissioner_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_commissioner_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_drug_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_drug_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_drug_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_drug_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_FDA_competence_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_FDA_competence_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_FDA_competence_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_FDA_competence_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_FDA_legitimacy_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_FDA_legitimacy_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_FDA_legitimacy_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FDA_FDA_legitimacy_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2]),
         x1=c(confint(FDA_pres_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_pres_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_pres_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_pres_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_pres_FDA_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_pres_FDA_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_pres_FDA_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_pres_FDA_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_commissioner_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_commissioner_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_commissioner_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_commissioner_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_drug_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_drug_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_drug_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_drug_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_FDA_competence_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_FDA_competence_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_FDA_competence_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_FDA_competence_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_FDA_legitimacy_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_FDA_legitimacy_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_FDA_legitimacy_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FDA_FDA_legitimacy_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3]),
         y0=c(29, 28, 27, 26, 24, 23, 22, 21, 19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1), 
         col = c("gray80", "gray80", "black", "black"), cex=1.25)
axis(1,at=c(-0.50, -0.25, 0.00, 0.25, 0.50),
     labels=sprintf("%.2f",c(-0.50, -0.25, 0.00, 0.25, 0.50)),cex.axis = 1.75, lwd=2)
axis(2,at=c(27.5, 22.5, 17.5, 12.5, 7.5, 2.5),
     las=2,
     labels=c("Presidential\nApproval", "President's\nHandling of\nFDA", 
              "FDA Commissioner\nApproval", "FDA Decision\nApproval",
              "FDA\nCompetence", "FDA\nLegitimacy"),
     tck=0,
     lwd =0,
     line = 0,
     cex.axis=2)
mtext("Difference from Control", side=1, line = 3, cex =2)
#mtext("Clinton", side = 3, line=1, cex = 3.5)
text(c(coef(FDA_pres_approval_acting_noncopart),
       coef(FDA_pres_approval_acting_context_noncopart),
       coef(FDA_pres_approval_acting_copart),
       coef(FDA_pres_approval_acting_context_copart),
       coef(FDA_pres_FDA_acting_noncopart),
       coef(FDA_pres_FDA_acting_context_noncopart),
       coef(FDA_pres_FDA_acting_copart),
       coef(FDA_pres_FDA_acting_context_copart),
       coef(FDA_commissioner_approval_acting_noncopart),
       coef(FDA_commissioner_approval_acting_context_noncopart),
       coef(FDA_commissioner_approval_acting_copart),
       coef(FDA_commissioner_approval_acting_context_copart),
       coef(FDA_drug_approval_acting_noncopart),
       coef(FDA_drug_approval_acting_context_noncopart),
       coef(FDA_drug_approval_acting_copart),
       coef(FDA_drug_approval_acting_context_copart),
       coef(FDA_FDA_competence_acting_noncopart),
       coef(FDA_FDA_competence_acting_context_noncopart),
       coef(FDA_FDA_competence_acting_copart),
       coef(FDA_FDA_competence_acting_context_copart),
       coef(FDA_FDA_legitimacy_acting_noncopart),
       coef(FDA_FDA_legitimacy_acting_context_noncopart),
       coef(FDA_FDA_legitimacy_acting_copart),
       coef(FDA_FDA_legitimacy_acting_context_copart)),
     c(29, 28, 27, 26, 24, 23, 22, 21, 19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1)+0.55,
     paste0(sprintf("%.2f",round(c(coef(FDA_pres_approval_acting_noncopart),
                                   coef(FDA_pres_approval_acting_context_noncopart),
                                   coef(FDA_pres_approval_acting_copart),
                                   coef(FDA_pres_approval_acting_context_copart),
                                   coef(FDA_pres_FDA_acting_noncopart),
                                   coef(FDA_pres_FDA_acting_context_noncopart),
                                   coef(FDA_pres_FDA_acting_copart),
                                   coef(FDA_pres_FDA_acting_context_copart),
                                   coef(FDA_commissioner_approval_acting_noncopart),
                                   coef(FDA_commissioner_approval_acting_context_noncopart),
                                   coef(FDA_commissioner_approval_acting_copart),
                                   coef(FDA_commissioner_approval_acting_context_copart),
                                   coef(FDA_drug_approval_acting_noncopart),
                                   coef(FDA_drug_approval_acting_context_noncopart),
                                   coef(FDA_drug_approval_acting_copart),
                                   coef(FDA_drug_approval_acting_context_copart),
                                   coef(FDA_FDA_competence_acting_noncopart),
                                   coef(FDA_FDA_competence_acting_context_noncopart),
                                   coef(FDA_FDA_competence_acting_copart),
                                   coef(FDA_FDA_competence_acting_context_copart),
                                   coef(FDA_FDA_legitimacy_acting_noncopart),
                                   coef(FDA_FDA_legitimacy_acting_context_noncopart),
                                   coef(FDA_FDA_legitimacy_acting_copart),
                                   coef(FDA_FDA_legitimacy_acting_context_copart)), 2))), cex=1.75)
par(mar=c(5.1,10,4,.7))
plot(NULL, ylim=c(0.5, 29.5), xlim=c(-0.50,0.50), axes=FALSE,  bg = "black",
     tck=-.02, cex.axis=0.9, cex=1.5, cex.main = 3,
     xlab="", ylab="", yaxt="n", xaxt="n", main="FAA")
points(c(coef(FAA_pres_approval_acting_noncopart),
         coef(FAA_pres_approval_acting_context_noncopart),
         coef(FAA_pres_approval_acting_copart),
         coef(FAA_pres_approval_acting_context_copart),
         coef(FAA_pres_FAA_acting_noncopart),
         coef(FAA_pres_FAA_acting_context_noncopart),
         coef(FAA_pres_FAA_acting_copart),
         coef(FAA_pres_FAA_acting_context_copart),
         coef(FAA_administrator_approval_acting_noncopart),
         coef(FAA_administrator_approval_acting_context_noncopart),
         coef(FAA_administrator_approval_acting_copart),
         coef(FAA_administrator_approval_acting_context_copart),
         coef(FAA_malfunction_acting_noncopart),
         coef(FAA_malfunction_acting_context_noncopart),
         coef(FAA_malfunction_acting_copart),
         coef(FAA_malfunction_acting_context_copart),
         coef(FAA_FAA_competence_acting_noncopart),
         coef(FAA_FAA_competence_acting_context_noncopart),
         coef(FAA_FAA_competence_acting_copart),
         coef(FAA_FAA_competence_acting_context_copart),
         coef(FAA_FAA_legitimacy_acting_noncopart),
         coef(FAA_FAA_legitimacy_acting_context_noncopart),
         coef(FAA_FAA_legitimacy_acting_copart),
         coef(FAA_FAA_legitimacy_acting_context_copart)),
       c(29, 28, 27, 26, 24, 23, 22, 21, 19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1),
       pch=rep(c(19, 17, 19, 17), 4), 
       col = c("gray80", "gray80", "black", "black"),
       cex=1.25,
       panel.first = c(abline(v=0,lwd=2, col="gray90",lty=2)))
segments(x0=c(confint(FAA_pres_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_pres_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_pres_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_pres_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_pres_FAA_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_pres_FAA_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_pres_FAA_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_pres_FAA_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_administrator_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_administrator_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_administrator_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_administrator_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_malfunction_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_malfunction_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_malfunction_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_malfunction_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_FAA_competence_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_FAA_competence_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_FAA_competence_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_FAA_competence_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_FAA_legitimacy_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_FAA_legitimacy_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_FAA_legitimacy_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2],
              confint(FAA_FAA_legitimacy_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[2]),
         x1=c(confint(FAA_pres_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_pres_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_pres_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_pres_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_pres_FAA_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_pres_FAA_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_pres_FAA_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_pres_FAA_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_administrator_approval_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_administrator_approval_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_administrator_approval_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_administrator_approval_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_malfunction_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_malfunction_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_malfunction_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_malfunction_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_FAA_competence_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_FAA_competence_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_FAA_competence_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_FAA_competence_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_FAA_legitimacy_acting_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_FAA_legitimacy_acting_context_noncopart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_FAA_legitimacy_acting_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3],
              confint(FAA_FAA_legitimacy_acting_context_copart, calpha = qnorm(1-(0.05/24), lower.tail = FALSE))$confint[3]),
         y0=c(29, 28, 27, 26, 24, 23, 22, 21, 19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1), 
         col = c("gray80", "gray80", "black", "black"), cex=1.25)
axis(1,at=c(-0.50, -0.25, 0.00, 0.25, 0.50),
     labels=sprintf("%.2f",c(-0.50, -0.25, 0.00, 0.25, 0.50)),cex.axis = 1.75, lwd=2)
axis(2,at=c(27.5, 22.5, 17.5, 12.5, 7.5, 2.5),
     las=2,
     labels=c("Presidential\nApproval", "President's\nHandling of\nFAA", 
              "FAA Administrator\nApproval", "FAA Malfunction\nHandling",
              "FAA\nCompetence", "FAA\nLegitimacy"),
     tck=0,
     lwd =0,
     line = 0,
     cex.axis=2)
mtext("Difference from Control", side=1, line = 3, cex =2)
text(c(coef(FAA_pres_approval_acting_noncopart),
       coef(FAA_pres_approval_acting_context_noncopart),
       coef(FAA_pres_approval_acting_copart),
       coef(FAA_pres_approval_acting_context_copart),
       coef(FAA_pres_FAA_acting_noncopart),
       coef(FAA_pres_FAA_acting_context_noncopart),
       coef(FAA_pres_FAA_acting_copart),
       coef(FAA_pres_FAA_acting_context_copart),
       coef(FAA_administrator_approval_acting_noncopart),
       coef(FAA_administrator_approval_acting_context_noncopart),
       coef(FAA_administrator_approval_acting_copart),
       coef(FAA_administrator_approval_acting_context_copart),
       coef(FAA_malfunction_acting_noncopart),
       coef(FAA_malfunction_acting_context_noncopart),
       coef(FAA_malfunction_acting_copart),
       coef(FAA_malfunction_acting_context_copart),
       coef(FAA_FAA_competence_acting_noncopart),
       coef(FAA_FAA_competence_acting_context_noncopart),
       coef(FAA_FAA_competence_acting_copart),
       coef(FAA_FAA_competence_acting_context_copart),
       coef(FAA_FAA_legitimacy_acting_noncopart),
       coef(FAA_FAA_legitimacy_acting_context_noncopart),
       coef(FAA_FAA_legitimacy_acting_copart),
       coef(FAA_FAA_legitimacy_acting_context_copart)),
     c(29, 28, 27, 26, 24, 23, 22, 21, 19, 18, 17, 16, 14, 13, 12, 11, 9, 8, 7, 6, 4, 3, 2, 1)+0.55,
     paste0(sprintf("%.2f",round(c(coef(FAA_pres_approval_acting_noncopart),
                                   coef(FAA_pres_approval_acting_context_noncopart),
                                   coef(FAA_pres_approval_acting_copart),
                                   coef(FAA_pres_approval_acting_context_copart),
                                   coef(FAA_pres_FAA_acting_noncopart),
                                   coef(FAA_pres_FAA_acting_context_noncopart),
                                   coef(FAA_pres_FAA_acting_copart),
                                   coef(FAA_pres_FAA_acting_context_copart),
                                   coef(FAA_administrator_approval_acting_noncopart),
                                   coef(FAA_administrator_approval_acting_context_noncopart),
                                   coef(FAA_administrator_approval_acting_copart),
                                   coef(FAA_administrator_approval_acting_context_copart),
                                   coef(FAA_malfunction_acting_noncopart),
                                   coef(FAA_malfunction_acting_context_noncopart),
                                   coef(FAA_malfunction_acting_copart),
                                   coef(FAA_malfunction_acting_context_copart),
                                   coef(FAA_FAA_competence_acting_noncopart),
                                   coef(FAA_FAA_competence_acting_context_noncopart),
                                   coef(FAA_FAA_competence_acting_copart),
                                   coef(FAA_FAA_competence_acting_context_copart),
                                   coef(FAA_FAA_legitimacy_acting_noncopart),
                                   coef(FAA_FAA_legitimacy_acting_context_noncopart),
                                   coef(FAA_FAA_legitimacy_acting_copart),
                                   coef(FAA_FAA_legitimacy_acting_context_copart)), 2))), cex=1.75)

par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend("center",legend=c("Acting Official(s), Noncopartisans",
                         "Acting Official(s) with Context, Noncopartisans",
                         "Acting Official(s), Copartisans",
                         "Acting Official(s) with Context, Copartisans"), 
       col="white", text.col = "white",
       title="Treatment Conditions", cex=2.50,
       ncol = 1, title.col = "black")
legend("center",legend=c("Acting Official(s), Noncopartisans",
                         "Acting Official(s) with Context, Noncopartisans",
                         "Acting Official(s), Copartisans",
                         "Acting Official(s) with Context, Copartisans"),
       pch=c(19,17,19,17), 
       lty = c(1,1,1,1),
       col=c("gray80","gray80","black","black"),
       title=" ", cex=2,
       ncol = 1, bty = "n")
dev.off()

################################################################################
################################################################################
################################################################################

# SUPPLEMENTAL INFORMATION

# PAGE SI.3--HOW MAY RESPONDENTS IN EACH STUDY?

# 1548 in EXECUTIVE BRANCH EXPERIMENT
dim(exp_data_lucid[!is.na(exp_data_lucid$WSJ_pres_approval_bin) |
                   !is.na(exp_data_lucid$WSJ_pres_exec_branch_bin) |
                   !is.na(exp_data_lucid$WSJ_competence) |
                   !is.na(exp_data_lucid$WSJ_legitimacy)])

# 1560 IN FDA EXPERIMENT
dim(exp_data_lucid[!is.na(exp_data_lucid$FDA_pres_approval_bin) |
                   !is.na(exp_data_lucid$FDA_pres_FDA_bin) |
                   !is.na(exp_data_lucid$FDA_commissioner_approval_bin) |
                   !is.na(exp_data_lucid$FDA_drug_approval_bin) |
                   !is.na(exp_data_lucid$FDA_competence) |
                   !is.na(exp_data_lucid$FDA_legitimacy)])

# 1170 IN FAA EXPERIMENT
dim(exp_data_cloud[!is.na(exp_data_cloud$FAA_pres_approval_bin) |
                   !is.na(exp_data_cloud$FAA_pres_FAA_bin) |
                   !is.na(exp_data_cloud$FAA_administrator_approval_bin) |
                   !is.na(exp_data_cloud$FAA_malfunction_bin) |
                   !is.na(exp_data_cloud$FAA_competence) |
                   !is.na(exp_data_cloud$FAA_legitimacy)])

################################################################################

# TABLE SI.1--RESPONDENT DEMOGRAPHICS

# EXECUTIVE BRANCH STUDY

# SUBSETTING TO THOSE WHO PROVIDED AN ANSWER TO AT LEAST ONE POST-TREATMENT OUTCOME
exec_branch_resps <- exp_data_lucid[!is.na(exp_data_lucid$WSJ_pres_approval_bin) |
                                      !is.na(exp_data_lucid$WSJ_pres_exec_branch_bin) |
                                      !is.na(exp_data_lucid$WSJ_competence) |
                                      !is.na(exp_data_lucid$WSJ_legitimacy)]
# GENDER
table(exec_branch_resps$female)
round(prop.table(table(exec_branch_resps$female)),3)*100

# AGE
table(exec_branch_resps$age_bin)
round(prop.table(table(exec_branch_resps$age_bin)),3)*100

# PARTY ID
table(exec_branch_resps$pid3)
round(prop.table(table(exec_branch_resps$pid3)),3)*100

# IDEOLOGY
table(exec_branch_resps$ideology, useNA = "always")
round(prop.table(table(exec_branch_resps$ideology, useNA = "always")),3)*100

# EDUCATION
table(exec_branch_resps$educ, useNA = "always")
round(prop.table(table(exec_branch_resps$educ, useNA = "always")),3)*100

# INCOME
table(exec_branch_resps$income, useNA = "always")
round(prop.table(table(exec_branch_resps$income, useNA = "always")),3)*100

# RACE/ETHNICITY: PLEASE NOTE THAT LUCID CODES DIFFERENT COMBINATIONS OF RACE/
# ETHNICITY AS SEPARATE VARIABLES, SO THOSE WHO DID NOT RECORD 1'S FOR ANY OF 
# THE CATEGORIES (INCLUDING OTHER) ARE CODED AS MISSING; BECAUSE THIS AFFECTS
# THE DENOMINATOR, PERCENTAGES MUST BE CALCULATED MANUALLY
table(exec_branch_resps$white_nh, useNA = "always")
round(table(exec_branch_resps$white_nh, useNA = "always")/dim(exec_branch_resps)[1],3)*100
table(exec_branch_resps$white_h, useNA = "always")
round(table(exec_branch_resps$white_h, useNA = "always")/dim(exec_branch_resps)[1],3)*100
table(exec_branch_resps$black_nh, useNA = "always")
round(table(exec_branch_resps$black_nh, useNA = "always")/dim(exec_branch_resps)[1],3)*100
table(exec_branch_resps$black_h, useNA = "always")
round(table(exec_branch_resps$black_h, useNA = "always")/dim(exec_branch_resps)[1],3)*100
table(exec_branch_resps$asian, useNA = "always")
round(table(exec_branch_resps$asian, useNA = "always")/dim(exec_branch_resps)[1],3)*100
table(exec_branch_resps$other, useNA = "always")
round(table(exec_branch_resps$other, useNA = "always")/dim(exec_branch_resps)[1],3)*100
table(exec_branch_resps$white_nh==0 &
        exec_branch_resps$white_h==0 &
        exec_branch_resps$black_nh==0 &
        exec_branch_resps$black_h==0 &
        exec_branch_resps$asian==0 &
        exec_branch_resps$other==0, useNA = "always")
round(table(exec_branch_resps$white_nh==0 &
              exec_branch_resps$white_h==0 &
              exec_branch_resps$black_nh==0 &
              exec_branch_resps$black_h==0 &
              exec_branch_resps$asian==0 &
              exec_branch_resps$other==0, useNA = "always")/dim(exec_branch_resps)[1],3)*100

# FDA STUDY

fda_resps <- exp_data_lucid[which(!is.na(exp_data_lucid$FDA_pres_approval_bin)|
                                   !is.na(exp_data_lucid$FDA_pres_FDA_bin)|
                                   !is.na(exp_data_lucid$FDA_commissioner_approval_bin)|
                                   !is.na(exp_data_lucid$FDA_drug_approval_bin)|
                                   !is.na(exp_data_lucid$FDA_legitimacy)|
                                   !is.na(exp_data_lucid$FDA_competence)),]

table(fda_resps$female)
round(prop.table(table(fda_resps$female)),3)*100

table(fda_resps$age_bin)
round(prop.table(table(fda_resps$age_bin)),3)*100

table(fda_resps$pid3)
round(prop.table(table(fda_resps$pid3)),3)*100

table(fda_resps$ideology, useNA = "always")
round(prop.table(table(fda_resps$ideology, useNA = "always")),3)*100

table(fda_resps$educ, useNA = "always")
round(prop.table(table(fda_resps$educ, useNA = "always")),3)*100

table(fda_resps$income, useNA = "always")
round(prop.table(table(fda_resps$income, useNA = "always")),3)*100

# RESPONDENTS WHO DID NOT RECORD 1'S FOR ANY OF THE CATEGORIES (INCLUDING OTHER)
# ARE CODED AS MISSING; BECAUSE THIS AFFECTS THE DENOMINATOR, PERCENTAGES MUST
# BE CALCULATED MANUALLY
table(fda_resps$white_nh, useNA = "always")
round(table(fda_resps$white_nh, useNA = "always")/dim(fda_resps)[1],3)*100
table(fda_resps$white_h, useNA = "always")
round(table(fda_resps$white_h, useNA = "always")/dim(fda_resps)[1],3)*100
table(fda_resps$black_nh, useNA = "always")
round(table(fda_resps$black_nh, useNA = "always")/dim(fda_resps)[1],3)*100
table(fda_resps$black_h, useNA = "always")
round(table(fda_resps$black_h, useNA = "always")/dim(fda_resps)[1],3)*100
table(fda_resps$asian, useNA = "always")
round(table(fda_resps$asian, useNA = "always")/dim(fda_resps)[1],3)*100
table(fda_resps$other, useNA = "always")
round(table(fda_resps$other, useNA = "always")/dim(fda_resps)[1],3)*100
table(fda_resps$white_nh==0 &
        fda_resps$white_h==0 &
        fda_resps$black_nh==0 &
        fda_resps$black_h==0 &
        fda_resps$asian==0 &
        fda_resps$other==0, useNA = "always")
round(table(fda_resps$white_nh==0 &
              fda_resps$white_h==0 &
              fda_resps$black_nh==0 &
              fda_resps$black_h==0 &
              fda_resps$asian==0 &
              fda_resps$other==0, useNA = "always")/dim(fda_resps)[1],3)*100

# FAA STUDY

# AGE
table(exp_data_cloud$age, useNA = "always")
round(prop.table(table(exp_data_cloud$age, useNA = "always")),3)*100

# GENDER
table(exp_data_cloud$gender, useNA = "always")
round(prop.table(table(exp_data_cloud$gender, useNA = "always")),3)*100

# RACE/ETHNICITY
table(exp_data_cloud$race_ethnicity, useNA = "always")
round(prop.table(table(exp_data_cloud$race_ethnicity, useNA = "always")),3)*100

# EDUCATION
table(exp_data_cloud$education, useNA = "always")
round(prop.table(table(exp_data_cloud$education, useNA = "always")),3)*100

# INCOME
table(exp_data_cloud$income, useNA = "always")
round(prop.table(table(exp_data_cloud$income, useNA = "always")),3)*100

# PARTY ID
table(exp_data_cloud$pid, useNA = "always")
round(prop.table(table(exp_data_cloud$pid, useNA = "always")),3)*100

# IDEOLOGY
table(exp_data_cloud$ideology, useNA = "always")
round(prop.table(table(exp_data_cloud$ideology, useNA = "always")),3)*100

################################################################################

# PAGE SI.19, FOOTNOTE 18--ATTENTION CHECKS

# EXECUTIVE BRANCH STUDY

table(exp_data_lucid$attentionscore[which(!is.na(exp_data_lucid$WSJ_pres_approval_bin)|
                                      !is.na(exp_data_lucid$WSJ_pres_exec_branch_bin)|
                                      !is.na(exp_data_lucid$WSJ_legitimacy)|
                                      !is.na(exp_data_lucid$WSJ_competence))])
round(prop.table(table(exp_data_lucid$attentionscore[which(!is.na(exp_data_lucid$WSJ_pres_approval_bin)|
                                                       !is.na(exp_data_lucid$WSJ_pres_exec_branch_bin)|
                                                       !is.na(exp_data_lucid$WSJ_legitimacy)|
                                                       !is.na(exp_data_lucid$WSJ_competence))])),3)*100

# FDA STUDY

table(exp_data_lucid$attentionscore[which(!is.na(exp_data_lucid$FDA_pres_approval_bin)|
                                      !is.na(exp_data_lucid$FDA_pres_FDA_bin)|
                                      !is.na(exp_data_lucid$FDA_commissioner_approval_bin)|
                                      !is.na(exp_data_lucid$FDA_drug_approval_bin)|
                                      !is.na(exp_data_lucid$FDA_legitimacy)|
                                      !is.na(exp_data_lucid$FDA_competence))])
round(prop.table(table(exp_data_lucid$attentionscore[which(!is.na(exp_data_lucid$FDA_pres_approval_bin)|
                                                       !is.na(exp_data_lucid$FDA_pres_FDA_bin)|
                                                       !is.na(exp_data_lucid$FDA_commissioner_approval_bin)|
                                                       !is.na(exp_data_lucid$FDA_drug_approval_bin)|
                                                       !is.na(exp_data_lucid$FDA_legitimacy)|
                                                       !is.na(exp_data_lucid$FDA_competence))])),3)*100

# FAA STUDY

table(exp_data_cloud$attentiveness[which(!is.na(exp_data_cloud$FAA_pres_approval_bin)|
                                         !is.na(exp_data_cloud$FAA_pres_FAA_bin)|
                                         !is.na(exp_data_cloud$FAA_administrator_approval_bin)|
                                         !is.na(exp_data_cloud$FAA_malfunction_bin)|
                                         !is.na(exp_data_cloud$FAA_legitimacy)|
                                         !is.na(exp_data_cloud$FAA_competence))])
round(prop.table(table(exp_data_cloud$attentiveness[which(!is.na(exp_data_cloud$FAA_pres_approval_bin)|
                                                          !is.na(exp_data_cloud$FAA_pres_FAA_bin)|
                                                          !is.na(exp_data_cloud$FAA_administrator_approval_bin)|
                                                          !is.na(exp_data_cloud$FAA_malfunction_bin)|
                                                          !is.na(exp_data_cloud$FAA_legitimacy)|
                                                          !is.na(exp_data_cloud$FAA_competence))])),3)*100

################################################################################

# PAGE SI.19, FOOTNOTE 19--CACEs FOR MAIN ANALYSES (TABLES SI.2-SI.7)

# In-text at the beginning of Section SI.B, we note that "The substantive 
# interpretation of our findings is consistent across both experiments when we use
# information about attention check passage to calculate complier average treatment
# effects."  We provide the code here to estimate complier average treatment effects
# for the analyses that underlie our main results.

# We calculate the CACEs when respondents who passed at least one attention check
# are considered treated and when only respondents who passed both attention 
# checks are treated; these CACEs represent the lower and upper bounds of the
# true CACE (Gerber and Green 2012, pgs. 164-165).

# CACEs calculated using instrumental variables approach/two-stage least squares
# with feols

# Please note that the general substantive results from our main analysis persist.

# EXECUTIVE BRANCH

# RECODING TREATMENT AS A BINARY INDICATOR AND INCORPORATING ATTENTIVENESS INTO
# CODING OF TREATMENT
exp_data_lucid$WSJ_treatment_acting <- ifelse(exp_data_lucid$WSJ_condition=="Control", 0, NA)
exp_data_lucid$WSJ_treatment_acting <- ifelse(exp_data_lucid$WSJ_condition=="Treatment", 1, exp_data_lucid$WSJ_treatment_acting)
exp_data_lucid$WSJ_treatment_acting_explain <- ifelse(exp_data_lucid$WSJ_condition=="Control", 0, NA)
exp_data_lucid$WSJ_treatment_acting_explain <- ifelse(exp_data_lucid$WSJ_condition=="Treatment_Explain", 1, exp_data_lucid$WSJ_treatment_acting_explain)
exp_data_lucid$WSJ_treatment_acting_att1 <- ifelse(exp_data_lucid$WSJ_condition=="Treatment" & exp_data_lucid$attentionscore>=1, 1, 0)
exp_data_lucid$WSJ_treatment_acting_att2 <- ifelse(exp_data_lucid$WSJ_condition=="Treatment" & exp_data_lucid$attentionscore>=2, 1, 0)
exp_data_lucid$WSJ_treatment_acting_explain_att1 <- ifelse(exp_data_lucid$WSJ_condition=="Treatment_Explain" & exp_data_lucid$attentionscore>=1, 1, 0)
exp_data_lucid$WSJ_treatment_acting_explain_att2 <- ifelse(exp_data_lucid$WSJ_condition=="Treatment_Explain" & exp_data_lucid$attentionscore>=2, 1, 0)

# CALCULATING THE LOWER AND UPPER BOUNDS OF THE TRUE CACEs FOR EACH OUTCOME
WSJ_pres_approval_model_iv_WSJ_treatment_acting_att1 <- feols(WSJ_pres_approval_bin ~ 1 | WSJ_treatment_acting_att1 ~ WSJ_treatment_acting, 
                                                              cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_pres_approval_model_iv_WSJ_treatment_acting_att1)
WSJ_pres_approval_model_iv_WSJ_treatment_acting_att2 <- feols(WSJ_pres_approval_bin ~ 1 | WSJ_treatment_acting_att2 ~ WSJ_treatment_acting, 
                                                              cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_pres_approval_model_iv_WSJ_treatment_acting_att2)

WSJ_pres_approval_model_iv_WSJ_treatment_acting_explain_att1 <- feols(WSJ_pres_approval_bin ~ 1 | WSJ_treatment_acting_explain_att1 ~ WSJ_treatment_acting_explain, 
                                                                      cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_pres_approval_model_iv_WSJ_treatment_acting_explain_att1)
WSJ_pres_approval_model_iv_WSJ_treatment_acting_explain_att2 <- feols(WSJ_pres_approval_bin ~ 1 | WSJ_treatment_acting_explain_att2 ~ WSJ_treatment_acting_explain, 
                                                                      cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_pres_approval_model_iv_WSJ_treatment_acting_explain_att2)

WSJ_pres_exec_branch_model_iv_WSJ_treatment_acting_att1 <- feols(WSJ_pres_exec_branch_bin ~ 1 | WSJ_treatment_acting_att1 ~ WSJ_treatment_acting, 
                                                                 cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_pres_exec_branch_model_iv_WSJ_treatment_acting_att1)
WSJ_pres_exec_branch_model_iv_WSJ_treatment_acting_att2 <- feols(WSJ_pres_exec_branch_bin ~ 1 | WSJ_treatment_acting_att2 ~ WSJ_treatment_acting, 
                                                                 cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_pres_exec_branch_model_iv_WSJ_treatment_acting_att2)

WSJ_pres_exec_branch_model_iv_WSJ_treatment_acting_explain_att1 <- feols(WSJ_pres_exec_branch_bin ~ 1 | WSJ_treatment_acting_explain_att1 ~ WSJ_treatment_acting_explain, 
                                                                         cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_pres_exec_branch_model_iv_WSJ_treatment_acting_explain_att1)
WSJ_pres_exec_branch_model_iv_WSJ_treatment_acting_explain_att2 <- feols(WSJ_pres_exec_branch_bin ~ 1 | WSJ_treatment_acting_explain_att2 ~ WSJ_treatment_acting_explain, 
                                                                         cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_pres_exec_branch_model_iv_WSJ_treatment_acting_explain_att2)

WSJ_competence_model_iv_WSJ_treatment_acting_att1 <- feols(WSJ_competence ~ 1 | WSJ_treatment_acting_att1 ~ WSJ_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_competence_model_iv_WSJ_treatment_acting_att1)
WSJ_competence_model_iv_WSJ_treatment_acting_att2 <- feols(WSJ_competence ~ 1 | WSJ_treatment_acting_att2 ~ WSJ_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_competence_model_iv_WSJ_treatment_acting_att2)

WSJ_competence_model_iv_WSJ_treatment_acting_explain_att1 <- feols(WSJ_competence ~ 1 | WSJ_treatment_acting_explain_att1 ~ WSJ_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_competence_model_iv_WSJ_treatment_acting_explain_att1)
WSJ_competence_model_iv_WSJ_treatment_acting_explain_att2 <- feols(WSJ_competence ~ 1 | WSJ_treatment_acting_explain_att2 ~ WSJ_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_competence_model_iv_WSJ_treatment_acting_explain_att2)

WSJ_legitimacy_model_iv_WSJ_treatment_acting_att1 <- feols(WSJ_legitimacy ~ 1 | WSJ_treatment_acting_att1 ~ WSJ_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_legitimacy_model_iv_WSJ_treatment_acting_att1)
WSJ_legitimacy_model_iv_WSJ_treatment_acting_att2 <- feols(WSJ_legitimacy ~ 1 | WSJ_treatment_acting_att2 ~ WSJ_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_legitimacy_model_iv_WSJ_treatment_acting_att2)

WSJ_legitimacy_model_iv_WSJ_treatment_acting_explain_att1 <- feols(WSJ_legitimacy ~ 1 | WSJ_treatment_acting_explain_att1 ~ WSJ_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_legitimacy_model_iv_WSJ_treatment_acting_explain_att1)
WSJ_legitimacy_model_iv_WSJ_treatment_acting_explain_att2 <- feols(WSJ_legitimacy ~ 1 | WSJ_treatment_acting_explain_att2 ~ WSJ_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_lucid)
summary(WSJ_legitimacy_model_iv_WSJ_treatment_acting_explain_att2)

# FDA STUDY

# RECODING TREATMENT AS A BINARY INDICATOR AND INCORPORATING ATTENTIVENESS INTO
# CODING OF TREATMENT
exp_data_lucid$FDA_treatment_acting <- ifelse(exp_data_lucid$FDA_condition=="Control", 0, NA)
exp_data_lucid$FDA_treatment_acting <- ifelse(exp_data_lucid$FDA_condition=="Treatment", 1, exp_data_lucid$FDA_treatment_acting)
exp_data_lucid$FDA_treatment_acting_explain <- ifelse(exp_data_lucid$FDA_condition=="Control", 0, NA)
exp_data_lucid$FDA_treatment_acting_explain <- ifelse(exp_data_lucid$FDA_condition=="Treatment_Explain", 1, exp_data_lucid$FDA_treatment_acting_explain)
exp_data_lucid$FDA_treatment_acting_att1 <- ifelse(exp_data_lucid$FDA_condition=="Treatment" & exp_data_lucid$attentionscore>=1, 1, 0)
exp_data_lucid$FDA_treatment_acting_att2 <- ifelse(exp_data_lucid$FDA_condition=="Treatment" & exp_data_lucid$attentionscore>=2, 1, 0)
exp_data_lucid$FDA_treatment_acting_explain_att1 <- ifelse(exp_data_lucid$FDA_condition=="Treatment_Explain" & exp_data_lucid$attentionscore>=1, 1, 0)
exp_data_lucid$FDA_treatment_acting_explain_att2 <- ifelse(exp_data_lucid$FDA_condition=="Treatment_Explain" & exp_data_lucid$attentionscore>=2, 1, 0)

# CALCULATING THE LOWER AND UPPER BOUNDS OF THE TRUE CACEs FOR EACH OUTCOME
FDA_pres_approval_model_iv_FDA_treatment_acting_att1 <- feols(FDA_pres_approval_bin ~ 1 | FDA_treatment_acting_att1 ~ FDA_treatment_acting, 
                                                              cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_approval_model_iv_FDA_treatment_acting_att1)
FDA_pres_approval_model_iv_FDA_treatment_acting_att2 <- feols(FDA_pres_approval_bin ~ 1 | FDA_treatment_acting_att2 ~ FDA_treatment_acting, 
                                                              cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_approval_model_iv_FDA_treatment_acting_att2)

FDA_pres_approval_model_iv_FDA_treatment_acting_explain_att1 <- feols(FDA_pres_approval_bin ~ 1 | FDA_treatment_acting_explain_att1 ~ FDA_treatment_acting_explain, 
                                                                      cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_approval_model_iv_FDA_treatment_acting_explain_att1)
FDA_pres_approval_model_iv_FDA_treatment_acting_explain_att2 <- feols(FDA_pres_approval_bin ~ 1 | FDA_treatment_acting_explain_att2 ~ FDA_treatment_acting_explain, 
                                                                      cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_approval_model_iv_FDA_treatment_acting_explain_att2)

FDA_pres_FDA_model_iv_FDA_treatment_acting_att1 <- feols(FDA_pres_FDA_bin ~ 1 | FDA_treatment_acting_att1 ~ FDA_treatment_acting, 
                                                         cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_FDA_model_iv_FDA_treatment_acting_att1)
FDA_pres_FDA_model_iv_FDA_treatment_acting_att2 <- feols(FDA_pres_FDA_bin ~ 1 | FDA_treatment_acting_att2 ~ FDA_treatment_acting, 
                                                         cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_FDA_model_iv_FDA_treatment_acting_att2)

FDA_pres_FDA_model_iv_FDA_treatment_acting_explain_att1 <- feols(FDA_pres_FDA_bin ~ 1 | FDA_treatment_acting_explain_att1 ~ FDA_treatment_acting_explain, 
                                                                 cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_FDA_model_iv_FDA_treatment_acting_explain_att1)
FDA_pres_FDA_model_iv_FDA_treatment_acting_explain_att2 <- feols(FDA_pres_FDA_bin ~ 1 | FDA_treatment_acting_explain_att2 ~ FDA_treatment_acting_explain, 
                                                                 cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_FDA_model_iv_FDA_treatment_acting_explain_att2)

FDA_pres_FDA_commissioner_model_iv_FDA_treatment_acting_att1 <- feols(FDA_commissioner_approval_bin ~ 1 | FDA_treatment_acting_att1 ~ FDA_treatment_acting, 
                                                                      cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_FDA_commissioner_model_iv_FDA_treatment_acting_att1)
FDA_pres_FDA_commissioner_model_iv_FDA_treatment_acting_att2 <- feols(FDA_commissioner_approval_bin ~ 1 | FDA_treatment_acting_att2 ~ FDA_treatment_acting, 
                                                                      cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_FDA_commissioner_model_iv_FDA_treatment_acting_att2)

FDA_pres_FDA_commissioner_model_iv_FDA_treatment_acting_explain_att1 <- feols(FDA_commissioner_approval_bin ~ 1 | FDA_treatment_acting_explain_att1 ~ FDA_treatment_acting_explain, 
                                                                              cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_FDA_commissioner_model_iv_FDA_treatment_acting_explain_att1)
FDA_pres_FDA_commissioner_model_iv_FDA_treatment_acting_explain_att2 <- feols(FDA_commissioner_approval_bin ~ 1 | FDA_treatment_acting_explain_att2 ~ FDA_treatment_acting_explain, 
                                                                              cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_pres_FDA_commissioner_model_iv_FDA_treatment_acting_explain_att2)

FDA_drug_approval_model_iv_FDA_treatment_acting_att1 <- feols(FDA_drug_approval_bin ~ 1 | FDA_treatment_acting_att1 ~ FDA_treatment_acting, 
                                                              cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_drug_approval_model_iv_FDA_treatment_acting_att1)
FDA_drug_approval_model_iv_FDA_treatment_acting_att2 <- feols(FDA_drug_approval_bin ~ 1 | FDA_treatment_acting_att2 ~ FDA_treatment_acting, 
                                                              cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_drug_approval_model_iv_FDA_treatment_acting_att2)

FDA_drug_approval_model_iv_FDA_treatment_acting_explain_att1 <- feols(FDA_drug_approval_bin ~ 1 | FDA_treatment_acting_explain_att1 ~ FDA_treatment_acting_explain, 
                                                                      cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_drug_approval_model_iv_FDA_treatment_acting_explain_att1)
FDA_drug_approval_model_iv_FDA_treatment_acting_explain_att2 <- feols(FDA_drug_approval_bin ~ 1 | FDA_treatment_acting_explain_att2 ~ FDA_treatment_acting_explain, 
                                                                      cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_drug_approval_model_iv_FDA_treatment_acting_explain_att2)

FDA_competence_model_iv_FDA_treatment_acting_att1 <- feols(FDA_competence ~ 1 | FDA_treatment_acting_att1 ~ FDA_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_competence_model_iv_FDA_treatment_acting_att1)
FDA_competence_model_iv_FDA_treatment_acting_att2 <- feols(FDA_competence ~ 1 | FDA_treatment_acting_att2 ~ FDA_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_competence_model_iv_FDA_treatment_acting_att2)

FDA_competence_model_iv_FDA_treatment_acting_explain_att1 <- feols(FDA_competence ~ 1 | FDA_treatment_acting_explain_att1 ~ FDA_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_competence_model_iv_FDA_treatment_acting_explain_att1)
FDA_competence_model_iv_FDA_treatment_acting_explain_att2 <- feols(FDA_competence ~ 1 | FDA_treatment_acting_explain_att2 ~ FDA_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_competence_model_iv_FDA_treatment_acting_explain_att2)

FDA_legitimacy_model_iv_FDA_treatment_acting_att1 <- feols(FDA_legitimacy ~ 1 | FDA_treatment_acting_att1 ~ FDA_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_legitimacy_model_iv_FDA_treatment_acting_att1)
FDA_legitimacy_model_iv_FDA_treatment_acting_att2 <- feols(FDA_legitimacy ~ 1 | FDA_treatment_acting_att2 ~ FDA_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_legitimacy_model_iv_FDA_treatment_acting_att2)

FDA_legitimacy_model_iv_FDA_treatment_acting_explain_att1 <- feols(FDA_legitimacy ~ 1 | FDA_treatment_acting_explain_att1 ~ FDA_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_legitimacy_model_iv_FDA_treatment_acting_explain_att1)
FDA_legitimacy_model_iv_FDA_treatment_acting_explain_att2 <- feols(FDA_legitimacy ~ 1 | FDA_treatment_acting_explain_att2 ~ FDA_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_lucid)
summary(FDA_legitimacy_model_iv_FDA_treatment_acting_explain_att2)

# FAA

# RECODING TREATMENT AS A BINARY INDICATOR AND INCORPORATING ATTENTIVENESS INTO
# CODING OF TREATMENT
exp_data_cloud$FAA_treatment_acting <- ifelse(exp_data_cloud$FAA_treatment=="Control", 0, NA)
exp_data_cloud$FAA_treatment_acting <- ifelse(exp_data_cloud$FAA_treatment=="Acting", 1, exp_data_cloud$FAA_treatment_acting)
exp_data_cloud$FAA_treatment_acting_explain <- ifelse(exp_data_cloud$FAA_treatment=="Control", 0, NA)
exp_data_cloud$FAA_treatment_acting_explain <- ifelse(exp_data_cloud$FAA_treatment=="Acting with Context", 1, exp_data_cloud$FAA_treatment_acting_explain)
exp_data_cloud$FAA_treatment_acting_att1 <- ifelse(exp_data_cloud$FAA_treatment=="Acting" & exp_data_cloud$attentiveness>=1, 1, 0)
exp_data_cloud$FAA_treatment_acting_att2 <- ifelse(exp_data_cloud$FAA_treatment=="Acting" & exp_data_cloud$attentiveness>=2, 1, 0)
exp_data_cloud$FAA_treatment_acting_explain_att1 <- ifelse(exp_data_cloud$FAA_treatment=="Acting with Context" & exp_data_cloud$attentiveness>=1, 1, 0)
exp_data_cloud$FAA_treatment_acting_explain_att2 <- ifelse(exp_data_cloud$FAA_treatment=="Acting with Context" & exp_data_cloud$attentiveness>=2, 1, 0)

# CALCULATING THE LOWER AND UPPER BOUNDS OF THE TRUE CACEs FOR EACH OUTCOME
FAA_pres_approval_model_iv_FAA_treatment_acting_att1 <- feols(FAA_pres_approval_bin ~ 1 | FAA_treatment_acting_att1 ~ FAA_treatment_acting, 
                                                              cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_approval_model_iv_FAA_treatment_acting_att1)
FAA_pres_approval_model_iv_FAA_treatment_acting_att2 <- feols(FAA_pres_approval_bin ~ 1 | FAA_treatment_acting_att2 ~ FAA_treatment_acting, 
                                                              cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_approval_model_iv_FAA_treatment_acting_att2)

FAA_pres_approval_model_iv_FAA_treatment_acting_explain_att1 <- feols(FAA_pres_approval_bin ~ 1 | FAA_treatment_acting_explain_att1 ~ FAA_treatment_acting_explain, 
                                                                      cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_approval_model_iv_FAA_treatment_acting_explain_att1)
FAA_pres_approval_model_iv_FAA_treatment_acting_explain_att2 <- feols(FAA_pres_approval_bin ~ 1 | FAA_treatment_acting_explain_att2 ~ FAA_treatment_acting_explain, 
                                                                      cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_approval_model_iv_FAA_treatment_acting_explain_att2)

FAA_pres_FAA_model_iv_FAA_treatment_acting_att1 <- feols(FAA_pres_FAA_bin ~ 1 | FAA_treatment_acting_att1 ~ FAA_treatment_acting, 
                                                         cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_FAA_model_iv_FAA_treatment_acting_att1)
FAA_pres_FAA_model_iv_FAA_treatment_acting_att2 <- feols(FAA_pres_FAA_bin ~ 1 | FAA_treatment_acting_att2 ~ FAA_treatment_acting, 
                                                         cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_FAA_model_iv_FAA_treatment_acting_att2)

FAA_pres_FAA_model_iv_FAA_treatment_acting_explain_att1 <- feols(FAA_pres_FAA_bin ~ 1 | FAA_treatment_acting_explain_att1 ~ FAA_treatment_acting_explain, 
                                                                 cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_FAA_model_iv_FAA_treatment_acting_explain_att1)
FAA_pres_FAA_model_iv_FAA_treatment_acting_explain_att2 <- feols(FAA_pres_FAA_bin ~ 1 | FAA_treatment_acting_explain_att2 ~ FAA_treatment_acting_explain, 
                                                                 cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_FAA_model_iv_FAA_treatment_acting_explain_att2)

FAA_pres_FAA_administrator_model_iv_FAA_treatment_acting_att1 <- feols(FAA_administrator_approval_bin ~ 1 | FAA_treatment_acting_att1 ~ FAA_treatment_acting, 
                                                                       cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_FAA_administrator_model_iv_FAA_treatment_acting_att1)
FAA_pres_FAA_administrator_model_iv_FAA_treatment_acting_att2 <- feols(FAA_administrator_approval_bin ~ 1 | FAA_treatment_acting_att2 ~ FAA_treatment_acting, 
                                                                       cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_FAA_administrator_model_iv_FAA_treatment_acting_att2)

FAA_pres_FAA_administrator_model_iv_FAA_treatment_acting_explain_att1 <- feols(FAA_administrator_approval_bin ~ 1 | FAA_treatment_acting_explain_att1 ~ FAA_treatment_acting_explain, 
                                                                               cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_FAA_administrator_model_iv_FAA_treatment_acting_explain_att1)
FAA_pres_FAA_administrator_model_iv_FAA_treatment_acting_explain_att2 <- feols(FAA_administrator_approval_bin ~ 1 | FAA_treatment_acting_explain_att2 ~ FAA_treatment_acting_explain, 
                                                                               cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_pres_FAA_administrator_model_iv_FAA_treatment_acting_explain_att2)

FAA_malfunction_model_iv_FAA_treatment_acting_att1 <- feols(FAA_malfunction_bin ~ 1 | FAA_treatment_acting_att1 ~ FAA_treatment_acting, 
                                                            cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_malfunction_model_iv_FAA_treatment_acting_att1)
FAA_malfunction_model_iv_FAA_treatment_acting_att2 <- feols(FAA_malfunction_bin ~ 1 | FAA_treatment_acting_att2 ~ FAA_treatment_acting, 
                                                            cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_malfunction_model_iv_FAA_treatment_acting_att2)

FAA_malfunction_model_iv_FAA_treatment_acting_explain_att1 <- feols(FAA_malfunction_bin ~ 1 | FAA_treatment_acting_explain_att1 ~ FAA_treatment_acting_explain, 
                                                                    cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_malfunction_model_iv_FAA_treatment_acting_explain_att1)
FAA_malfunction_model_iv_FAA_treatment_acting_explain_att2 <- feols(FAA_malfunction_bin ~ 1 | FAA_treatment_acting_explain_att2 ~ FAA_treatment_acting_explain, 
                                                                    cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_malfunction_model_iv_FAA_treatment_acting_explain_att2)

FAA_competence_model_iv_FAA_treatment_acting_att1 <- feols(FAA_competence ~ 1 | FAA_treatment_acting_att1 ~ FAA_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_competence_model_iv_FAA_treatment_acting_att1)
FAA_competence_model_iv_FAA_treatment_acting_att2 <- feols(FAA_competence ~ 1 | FAA_treatment_acting_att2 ~ FAA_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_competence_model_iv_FAA_treatment_acting_att2)

FAA_competence_model_iv_FAA_treatment_acting_explain_att1 <- feols(FAA_competence ~ 1 | FAA_treatment_acting_explain_att1 ~ FAA_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_competence_model_iv_FAA_treatment_acting_explain_att1)
FAA_competence_model_iv_FAA_treatment_acting_explain_att2 <- feols(FAA_competence ~ 1 | FAA_treatment_acting_explain_att2 ~ FAA_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_competence_model_iv_FAA_treatment_acting_explain_att2)

FAA_legitimacy_model_iv_FAA_treatment_acting_att1 <- feols(FAA_legitimacy ~ 1 | FAA_treatment_acting_att1 ~ FAA_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_legitimacy_model_iv_FAA_treatment_acting_att1)
FAA_legitimacy_model_iv_FAA_treatment_acting_att2 <- feols(FAA_legitimacy ~ 1 | FAA_treatment_acting_att2 ~ FAA_treatment_acting, 
                                                           cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_legitimacy_model_iv_FAA_treatment_acting_att2)

FAA_legitimacy_model_iv_FAA_treatment_acting_explain_att1 <- feols(FAA_legitimacy ~ 1 | FAA_treatment_acting_explain_att1 ~ FAA_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_legitimacy_model_iv_FAA_treatment_acting_explain_att1)
FAA_legitimacy_model_iv_FAA_treatment_acting_explain_att2 <- feols(FAA_legitimacy ~ 1 | FAA_treatment_acting_explain_att2 ~ FAA_treatment_acting_explain, 
                                                                   cluster = "ResponseId", data = exp_data_cloud)
summary(FAA_legitimacy_model_iv_FAA_treatment_acting_explain_att2)

################################################################################

# PAGE SI.20--INTERNAL CONSISTENCY OF COMPETENCE AND LEGITIMACY SCALES

# EXECUTIVE BRANCH STUDY
# LEGITIMACY
alpha(cbind(exp_data_lucid$WSJ_exec_branch_right, exp_data_lucid$WSJ_exec_branch_no_favor))
# COMPETENCE
alpha(cbind(exp_data_lucid$WSJ_exec_branch_effective, exp_data_lucid$WSJ_exec_branch_qualified))

# FDA STUDY
# LEGITIMACY
alpha(cbind(exp_data_lucid$FDA_FDA_right, exp_data_lucid$FDA_FDA_no_favor))
# COMPETENCE
alpha(cbind(exp_data_lucid$FDA_FDA_effective, exp_data_lucid$FDA_FDA_qualified))

# FAA STUDY
# LEGITIMACY
alpha(cbind(exp_data_cloud$FAA_FAA_right, exp_data_cloud$FAA_FAA_no_favor))
# COMPETENCE
alpha(cbind(exp_data_cloud$FAA_FAA_efficient, exp_data_cloud$FAA_FAA_qualified))

################################################################################

# TABLES SI.8-SI.10--UNCONDITIONAL EFFECTS, FOUR-POINT OUTCOMES

# EXECUTIVE BRANCH STUDY

WSJ_pres_approval_model_ord <- lm(WSJ_pres_approval ~ WSJ_condition, data = exp_data_lucid)
summary(WSJ_pres_approval_model_ord)

WSJ_pres_exec_branch_model_ord <- lm(WSJ_pres_exec_branch ~ WSJ_condition, data = exp_data_lucid)
summary(WSJ_pres_exec_branch_model_ord)

texreg(l = list(WSJ_pres_approval_model_ord, WSJ_pres_exec_branch_model_ord),
       file = "WSJ_table_ord.tex",
       stars = 0.05/8,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation"),
       custom.model.names = c("Pres. Approval", "Pres. Exec. Handling"),
       caption = "Vacancies in the Executive Branch",
       caption.above = TRUE,
       label = "table:WSJ_exp_ord",
       custom.note = "$^{*}p<0.05$.  Omitted category is the control condition, where respondents
       are told only about how many nominees Biden has, not that most have not been confirmed.  The first and second models 
       use as outcome measures the original four-point approval questions.  Third
       and fourth models are scales based on two questions about appraisals of the executive branch's
       competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# FDA STUDY

FDA_pres_approval_model_ord <- lm(FDA_pres_approval ~ FDA_condition, data = exp_data_lucid)
summary(FDA_pres_approval_model_ord)

FDA_pres_FDA_model_ord <- lm(FDA_pres_FDA ~ FDA_condition, data = exp_data_lucid)
summary(FDA_pres_FDA_model_ord)

FDA_commissioner_approval_model_ord <- lm(FDA_commissioner_approval ~ FDA_condition, data = exp_data_lucid)
summary(FDA_commissioner_approval_model_ord)

FDA_drug_approval_model_ord <- lm(FDA_drug_approval ~ FDA_condition, data = exp_data_lucid)
summary(FDA_drug_approval_model_ord)

texreg(l = list(FDA_pres_approval_model_ord, FDA_pres_FDA_model_ord, 
                FDA_commissioner_approval_model_ord, FDA_drug_approval_model_ord),
       file = "FDA_table_ord.tex",
       stars = 0.05/12,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation"),
       custom.model.names = c("Pres. Approval", "Pres. FDA Handling",
                              "FDA Comm. Approval", "FDA Decision Approval"),
       caption = "Vacancies in the FDA",
       caption.above = TRUE,
       label = "table:FDA_exp",
       custom.note = "$^{*}p<0.05$.  Omitted category is the control condition,
       where respondents are not told that the FDA does not have a confirmed commissioner.  The first and second models 
       use as outcome measures the original four-point approval questions.  Third
       and fourth models are scales based on two questions about appraisals of the FDA's
       competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# FAA STUDY

FAA_pres_approval_model_ord <- lm(FAA_pres_approval ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_pres_approval_model_ord)

FAA_pres_FAA_model_ord <- lm(FAA_pres_FAA ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_pres_FAA_model_ord)

FAA_administrator_approval_model_ord <- lm(FAA_administrator_approval ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_administrator_approval_model_ord)

FAA_malfunction_model_ord <- lm(FAA_malfunction ~ FAA_treatment, data = exp_data_cloud)
summary(FAA_malfunction_model_ord)

texreg(l = list(FAA_pres_approval_model_ord, FAA_pres_FAA_model_ord, 
                FAA_administrator_approval_model_ord, FAA_malfunction_model_ord),
       file = "FAA_table_ord.tex",
       stars = 0.05/12,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation"),
       custom.model.names = c("Pres. Approval", "Pres. FAA Handling",
                              "FAA Admin. Approval", "FAA Handling Approval"),
       caption = "Vacancies in the FAA",
       caption.above = TRUE,
       label = "table:FAA_exp",
       custom.note = "$^{*}p<0.05$.  Omitted category is the control condition,
       where respondents are not told that the FAA does not have a confirmed administrator  Third
       and fourth models are scales based on two questions about appraisals of the FAA's
       competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

################################################################################

# TABLES SI.11-SI.13--PARTISANSHIP-CONDITION EFFECTS, FOUR-POINT OUTCOMES

# EXECUTIVE BRANCH STUDY

WSJ_pres_approval_int_model_ord <- lm(WSJ_pres_approval ~ WSJ_condition*copart, data = exp_data_lucid)
summary(WSJ_pres_approval_int_model_ord)

WSJ_pres_exec_branch_int_model_ord <- lm(WSJ_pres_exec_branch ~ WSJ_condition*copart, data = exp_data_lucid)
summary(WSJ_pres_exec_branch_int_model_ord)

texreg(l = list(WSJ_pres_approval_int_model_ord, WSJ_pres_exec_branch_int_model_ord),
       file = "WSJ_int_table_ord.tex",
       stars = 0.05/16,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation",
                             "Pres. Copart.", "Acting:Pres. Copart", 
                             "Acting w/Explanation:Pres. Copart"),
       custom.model.names = c("Pres. Approval", "Pres. Exec. Handling"),
       caption = "Executive Branch Experiment, Copartisanship-Conditional (Four-Point Outcome Scales))",
       caption.above = TRUE,
       label = "table:WSJ_exp_copart_ord",
       custom.note = "$^{*}p<0.05$.  Omitted category for experimental condition is the control condition, 
       where respondents are told only about how many nominees Biden has, not that most have not been confirmed.
       Omitted category for copartisanship is non-presidential copartisan, which is any respondent who does not
       identify as a Democrat.  The first and second models 
       use as outcome measures the original four-point approval questions.    
       Third and fourth models are scales based on two questions about appraisals 
       of the executive branch's competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# FDA STUDY

FDA_pres_approval_int_model_ord <- lm(FDA_pres_approval ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_pres_approval_int_model_ord)

FDA_pres_FDA_int_model_ord <- lm(FDA_pres_FDA ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_pres_FDA_int_model_ord)

FDA_commissioner_approval_int_model_ord <- lm(FDA_commissioner_approval ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_commissioner_approval_int_model_ord)

FDA_drug_approval_int_model_ord <- lm(FDA_drug_approval ~ FDA_condition*copart, data = exp_data_lucid)
summary(FDA_drug_approval_int_model_ord)

texreg(l = list(FDA_pres_approval_int_model_ord, FDA_pres_FDA_int_model_ord, 
                FDA_commissioner_approval_int_model_ord, FDA_drug_approval_int_model_ord),
       file = "FDA_int_table_ord.tex",
       stars = 0.05/16,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation",
                             "Pres. Copart.", "Acting:Pres. Copart", 
                             "Acting w/Explanation:Pres. Copart"),
       custom.model.names = c("Pres. Approval", "Pres. FDA Handling",
                              "FDA Comm. Approval", "FDA Decision Approval"),
       caption = "FDA Experiment, Copartisanship-Conditional (Four-Point Outcome Scales)",
       caption.above = TRUE,
       label = "table:FDA_exp_copart_ord",
       custom.note = "$^{*}p<0.05$.  Omitted category for experimental condition is the control condition, 
       where respondents are not told that the FDA does not have a confirmed commissioner. 
       Omitted category for copartisanship is non-presidential copartisan, which is any respondent who does not
       identify as a Democrat.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

# FAA STUDY

FAA_pres_approval_int_model_ord <- lm(FAA_pres_approval ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_pres_approval_int_model_ord)

FAA_pres_FAA_int_model_ord <- lm(FAA_pres_FAA ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_pres_FAA_int_model_ord)

FAA_administrator_approval_int_model_ord <- lm(FAA_administrator_approval ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_administrator_approval_int_model_ord)

FAA_malfunction_int_model_ord <- lm(FAA_malfunction ~ FAA_treatment*copart, data = exp_data_cloud)
summary(FAA_malfunction_int_model_ord)

texreg(l = list(FAA_pres_approval_int_model_ord, FAA_pres_FAA_int_model_ord, 
                FAA_administrator_approval_int_model_ord, FAA_malfunction_int_model_ord),
       file = "FAA_int_table_ord.tex",
       stars = 0.05/24,
       custom.coef.names = c("Intercept", "Acting", "Acting w/Explanation",
                             "Pres. Copart.", "Acting:Pres. Copart", 
                             "Acting w/Explanation:Pres. Copart"),
       custom.model.names = c("Pres. Approval", "Pres. FAA Handling",
                              "FAA Admin. Approval", "FAA Handling Approval"),
       caption = "FAA Experiment, Copartisanship-Conditional (Four-Point Outcome Scales)",
       caption.above = TRUE,
       label = "table:FAA_exp_copart_ord",
       custom.note = "$^{*}p<0.05$.  Omitted category is the control condition,
       where respondents are not told that the FAA does not have a confirmed administrator  Third
       and fourth models are scales based on two questions about appraisals of the FAA's
       competence and legitimacy, respectively.",
       include.rsquared = FALSE,
       include.adjrs = FALSE)

################################################################################

# TABLES SI.14-SI.33--CAUSAL MEDIATION ANALYSES

# EXECUTIVE BRANCH--MEDIATED BY COMPETENCE

# UNCONDITIONAL ANALYSIS
# TABLE SI.14

med.fit <- lm(WSJ_competence ~ WSJ_condition, data = exp_data_lucid)
summary(med.fit)

out.fit <- lm(WSJ_pres_approval_bin ~ WSJ_condition + WSJ_competence, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition", 
                              mediator="WSJ_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$WSJ_competence) &
                     !is.na(exp_data_lucid$WSJ_pres_approval_bin) & 
                     exp_data_lucid$WSJ_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition", 
                              mediator="WSJ_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$WSJ_competence) &
                     !is.na(exp_data_lucid$WSJ_pres_approval_bin) & 
                     exp_data_lucid$WSJ_condition!="Treatment")])

# TABLE SI.15

out.fit <- lm(WSJ_pres_exec_branch_bin ~ WSJ_condition + WSJ_competence, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition", 
                              mediator="WSJ_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$WSJ_competence) &
                     !is.na(exp_data_lucid$WSJ_pres_exec_branch_bin) & 
                     exp_data_lucid$WSJ_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition", 
                              mediator="WSJ_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$WSJ_competence) &
                     !is.na(exp_data_lucid$WSJ_pres_exec_branch_bin) & 
                     exp_data_lucid$WSJ_condition!="Treatment")])

# COPARTISANSHIP-CONDITIONAL ANALYSIS--NOT INCLUDED IN SI
# AMONG COPARTISANS

med.fit <- lm(WSJ_competence ~ WSJ_condition, data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(med.fit)

out.fit <- lm(WSJ_pres_approval_bin ~ WSJ_condition + WSJ_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(WSJ_pres_exec_branch_bin ~ WSJ_condition + WSJ_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# AMONG NON-COPARTISANS

med.fit <- lm(WSJ_competence ~ WSJ_condition, data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(med.fit)

out.fit <- lm(WSJ_pres_approval_bin ~ WSJ_condition + WSJ_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(WSJ_pres_exec_branch_bin ~ WSJ_condition + WSJ_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# EXECUTIVE BRANCH--MEDIATED BY LEGITIMACY

# UNCONDITIONAL ANALYSIS
# TABLE SI.16

med.fit <- lm(WSJ_legitimacy ~ WSJ_condition, data = exp_data_lucid)
summary(med.fit)

out.fit <- lm(WSJ_pres_approval_bin ~ WSJ_condition + WSJ_legitimacy, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition", 
                              mediator="WSJ_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$WSJ_legitimacy) &
                     !is.na(exp_data_lucid$WSJ_pres_approval_bin) & 
                     exp_data_lucid$WSJ_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition", 
                              mediator="WSJ_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$WSJ_legitimacy) &
                     !is.na(exp_data_lucid$WSJ_pres_approval_bin) & 
                     exp_data_lucid$WSJ_condition!="Treatment")])

# TABLE SI.17

out.fit <- lm(WSJ_pres_exec_branch_bin ~ WSJ_condition + WSJ_legitimacy, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition", 
                              mediator="WSJ_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$WSJ_legitimacy) &
                     !is.na(exp_data_lucid$WSJ_pres_exec_branch_bin) & 
                     exp_data_lucid$WSJ_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition", 
                              mediator="WSJ_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$WSJ_legitimacy) &
                     !is.na(exp_data_lucid$WSJ_pres_exec_branch_bin) & 
                     exp_data_lucid$WSJ_condition!="Treatment")])

# COPARTISANSHIP-CONDITIONAL ANALYSIS--NOT INCLUDED IN SI
# AMONG COPARTISANS

med.fit <- lm(WSJ_legitimacy ~ WSJ_condition, data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(med.fit)

out.fit <- lm(WSJ_pres_approval_bin ~ WSJ_condition + WSJ_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(WSJ_pres_exec_branch_bin ~ WSJ_condition + WSJ_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# AMONG NON-COPARTISANS 

med.fit <- lm(WSJ_legitimacy ~ WSJ_condition, data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(med.fit)

out.fit <- lm(WSJ_pres_approval_bin ~ WSJ_condition + WSJ_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(WSJ_pres_exec_branch_bin ~ WSJ_condition + WSJ_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="WSJ_condition",
                              mediator="WSJ_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

################################################################################

# FDA, MEDIATED BY COMPETENCE

# UNCONDITIONAL ANALYSIS
# TABLE SI.18

med.fit <- lm(FDA_competence ~ FDA_condition, data = exp_data_lucid)
summary(med.fit)

out.fit <- lm(FDA_pres_approval_bin ~ FDA_condition + FDA_competence, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_competence) &
                     !is.na(exp_data_lucid$FDA_pres_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_competence) &
                     !is.na(exp_data_lucid$FDA_pres_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment")])

# TABLE SI.19
out.fit <- lm(FDA_pres_FDA_bin ~ FDA_condition + FDA_competence, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_competence) &
                     !is.na(exp_data_lucid$FDA_pres_FDA_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_competence) &
                     !is.na(exp_data_lucid$FDA_pres_FDA_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment")])

# TABLE SI.20
out.fit <- lm(FDA_commissioner_approval_bin ~ FDA_condition + FDA_competence, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_competence) &
                     !is.na(exp_data_lucid$FDA_commissioner_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_competence) &
                     !is.na(exp_data_lucid$FDA_commissioner_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment")])

# TABLE SI.21
out.fit <- lm(FDA_drug_approval_bin ~ FDA_condition + FDA_competence, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_competence) &
                     !is.na(exp_data_lucid$FDA_drug_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_competence) &
                     !is.na(exp_data_lucid$FDA_drug_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment")])

# COPARTISANSHIP-CONDITIONAL ANALYSIS--NOT INCLUDED IN SI
# AMONG COPARTISANS

med.fit <- lm(FDA_competence ~ FDA_condition, data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(med.fit)

out.fit <- lm(FDA_pres_approval_bin ~ FDA_condition + FDA_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_pres_FDA_bin ~ FDA_condition + FDA_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_commissioner_approval_bin ~ FDA_condition + FDA_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_drug_approval_bin ~ FDA_condition + FDA_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# AMONG NON-COPARTISANS

med.fit <- lm(FDA_competence ~ FDA_condition, data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(med.fit)

out.fit <- lm(FDA_pres_approval_bin ~ FDA_condition + FDA_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_pres_FDA_bin ~ FDA_condition + FDA_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_commissioner_approval_bin ~ FDA_condition + FDA_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_drug_approval_bin ~ FDA_condition + FDA_competence,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_competence",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

################################################################################

# FDA, MEDIATED BY LEGITIMACY

# UNCONDITIONAL ANALYSIS
# TABLE SI.22

med.fit <- lm(FDA_legitimacy ~ FDA_condition, data = exp_data_lucid)
summary(med.fit)

out.fit <- lm(FDA_pres_approval_bin ~ FDA_condition + FDA_legitimacy, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_legitimacy) &
                     !is.na(exp_data_lucid$FDA_pres_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_legitimacy) &
                     !is.na(exp_data_lucid$FDA_pres_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment")])
# TABLE SI.23

out.fit <- lm(FDA_pres_FDA_bin ~ FDA_condition + FDA_legitimacy, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_legitimacy) &
                     !is.na(exp_data_lucid$FDA_pres_FDA_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_legitimacy) &
                     !is.na(exp_data_lucid$FDA_pres_FDA_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment")])

# TABLE SI.24
out.fit <- lm(FDA_commissioner_approval_bin ~ FDA_condition + FDA_legitimacy, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_legitimacy) &
                     !is.na(exp_data_lucid$FDA_commissioner_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_legitimacy) &
                     !is.na(exp_data_lucid$FDA_commissioner_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment")])

# TABLE SI.25
out.fit <- lm(FDA_drug_approval_bin ~ FDA_condition + FDA_legitimacy, 
              data = exp_data_lucid)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_legitimacy) &
                     !is.na(exp_data_lucid$FDA_drug_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment_Explain")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition", 
                              mediator="FDA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_lucid[which(!is.na(exp_data_lucid$FDA_legitimacy) &
                     !is.na(exp_data_lucid$FDA_drug_approval_bin) & 
                     exp_data_lucid$FDA_condition!="Treatment")])

# COPARTISANSHIP-CONDITIONAL ANALYSIS--NOT INCLUDED IN SI
# AMONG COPARTISANS

med.fit <- lm(FDA_legitimacy ~ FDA_condition, data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(med.fit)

out.fit <- lm(FDA_pres_approval_bin ~ FDA_condition + FDA_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_pres_FDA_bin ~ FDA_condition + FDA_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_commissioner_approval_bin ~ FDA_condition + FDA_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_drug_approval_bin ~ FDA_condition + FDA_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

# AMONG NON-COPARTISANS

med.fit <- lm(FDA_legitimacy ~ FDA_condition, data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(med.fit)

out.fit <- lm(FDA_pres_approval_bin ~ FDA_condition + FDA_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_pres_FDA_bin ~ FDA_condition + FDA_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_commissioner_approval_bin ~ FDA_condition + FDA_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FDA_drug_approval_bin ~ FDA_condition + FDA_legitimacy,
              data = exp_data_lucid[which(exp_data_lucid$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FDA_condition",
                              mediator="FDA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Treatment_Explain", boot=TRUE, dropobs = TRUE)

summary(med.out)


################################################################################

# FAA, MEDIATED BY COMPETENCE

# UNCONDITIONAL ANALYSIS
# TABLE SI.26

med.fit <- lm(FAA_competence ~ FAA_treatment, data = exp_data_cloud)
summary(med.fit)

out.fit <- lm(FAA_pres_approval_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_competence) &
                         !is.na(exp_data_cloud$FAA_pres_approval_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting with Context")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_competence) &
                         !is.na(exp_data_cloud$FAA_pres_approval_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting")])

# TABLE SI.27
out.fit <- lm(FAA_pres_FAA_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_competence) &
                         !is.na(exp_data_cloud$FAA_pres_FAA_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting with Context")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_competence) &
                         !is.na(exp_data_cloud$FAA_pres_FAA_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting")])

# TABLE SI.28
out.fit <- lm(FAA_administrator_approval_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_competence) &
                         !is.na(exp_data_cloud$FAA_administrator_approval_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting with Context")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_competence) &
                         !is.na(exp_data_cloud$FAA_administrator_approval_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting")])

# TABLE SI.29
out.fit <- lm(FAA_malfunction_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_competence) &
                         !is.na(exp_data_cloud$FAA_malfunction_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting with Context")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_competence) &
                         !is.na(exp_data_cloud$FAA_malfunction_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting")])

# COPARTISANSHIP-CONDITIONAL ANALYSIS--NOT INCLUDED IN SI
# AMONG COPARTISANS

med.fit <- lm(FAA_competence ~ FAA_treatment, data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(med.fit)

out.fit <- lm(FAA_pres_approval_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_pres_FAA_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_administrator_approval_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_malfunction_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# AMONG NON-COPARTISANS

med.fit <- lm(FAA_competence ~ FAA_treatment, data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(med.fit)

out.fit <- lm(FAA_pres_approval_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_pres_FAA_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_administrator_approval_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_malfunction_bin ~ FAA_treatment + FAA_competence, 
              data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_competence",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

################################################################################

# FAA, MEDIATED BY LEGITIMACY

# UNCONDITIONAL ANALYSIS

# TABLE SI.30
med.fit <- lm(FAA_legitimacy ~ FAA_treatment, data = exp_data_cloud)
summary(med.fit)

out.fit <- lm(FAA_pres_approval_bin ~ FAA_treatment + FAA_legitimacy, 
              data = exp_data_cloud)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_legitimacy) &
                         !is.na(exp_data_cloud$FAA_pres_approval_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting with Context")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_legitimacy) &
                         !is.na(exp_data_cloud$FAA_pres_approval_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting")])

# TABLE SI.31
out.fit <- lm(FAA_pres_FAA_bin ~ FAA_treatment + FAA_legitimacy, 
              data = exp_data_cloud)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_legitimacy) &
                         !is.na(exp_data_cloud$FAA_pres_FAA_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting with Context")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_legitimacy) &
                         !is.na(exp_data_cloud$FAA_pres_FAA_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting")])

# TABLE SI.32
out.fit <- lm(FAA_administrator_approval_bin ~ FAA_treatment + FAA_legitimacy, 
              data = exp_data_cloud)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_legitimacy) &
                         !is.na(exp_data_cloud$FAA_administrator_approval_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting with Context")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_legitimacy) &
                         !is.na(exp_data_cloud$FAA_administrator_approval_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting")])

# TABLE SI.33
out.fit <- lm(FAA_malfunction_bin ~ FAA_treatment + FAA_legitimacy, 
              data = exp_data_cloud)
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_legitimacy) &
                         !is.na(exp_data_cloud$FAA_malfunction_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting with Context")])

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment", 
                              mediator="FAA_legitimacy",  robustSE = FALSE, 
                              sims=1000,  control.value = "Control", 
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

# HOW MANY OBSERVATIONS IN ABOVE ANALYSIS?
dim(exp_data_cloud[which(!is.na(exp_data_cloud$FAA_legitimacy) &
                         !is.na(exp_data_cloud$FAA_malfunction_bin) & 
                         exp_data_cloud$FAA_treatment!="Acting")])

# COPARTISANSHIP-CONDITIONAL ANALYSIS--NOT INCLUDED IN SI

med.fit <- lm(FAA_legitimacy ~ FAA_treatment, data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(med.fit)

out.fit <- lm(FAA_pres_approval_bin ~ FAA_treatment + FAA_legitimacy,
              data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_pres_FAA_bin ~ FAA_treatment + FAA_legitimacy,
              data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_administrator_approval_bin ~ FAA_treatment + FAA_legitimacy,
              data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_malfunction_bin ~ FAA_treatment + FAA_legitimacy,
              data = exp_data_cloud[which(exp_data_cloud$copart==1),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

non-copartisans

med.fit <- lm(FAA_legitimacy ~ FAA_treatment, data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(med.fit)

out.fit <- lm(FAA_pres_approval_bin ~ FAA_treatment + FAA_legitimacy,
              data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_pres_FAA_bin ~ FAA_treatment + FAA_legitimacy,
              data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_administrator_approval_bin ~ FAA_treatment + FAA_legitimacy,
              data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)

out.fit <- lm(FAA_malfunction_bin ~ FAA_treatment + FAA_legitimacy,
              data = exp_data_cloud[which(exp_data_cloud$copart==0),])
summary(out.fit)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting", boot=TRUE, dropobs = TRUE)

summary(med.out)

med.out <- mediation::mediate(med.fit, out.fit, treat="FAA_treatment",
                              mediator="FAA_legitimacy",  robustSE = FALSE,
                              sims=1000,  control.value = "Control",
                              treat.value = "Acting with Context", boot=TRUE, dropobs = TRUE)

summary(med.out)