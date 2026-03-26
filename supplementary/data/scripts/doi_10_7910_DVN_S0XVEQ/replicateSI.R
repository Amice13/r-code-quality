library(data.table)
library(mediation)
library(multcomp)
library(nnet)
library(readstata13)
library(texreg)
set.seed(1989)

setwd() # set to location where files are stored

TAPSdata <- fread("TAPSdata.csv", header = TRUE, stringsAsFactors = FALSE)
MTurk_stylized_data <- fread("MTurk_stylized_data.csv", header = TRUE, stringsAsFactors = FALSE)
MTurk_stylized_data_ordering <- fread("MTurk_stylized_data_ordering.csv", header = TRUE, stringsAsFactors = FALSE)
MTurk_factorial_data <- fread("MTurk_factorial_data.csv", header = TRUE, stringsAsFactors = FALSE)
MTurk_flint_data <- fread("MTurk_flint_data.csv", header = TRUE, stringsAsFactors = FALSE)

# set baseline levels of factor variables

TAPSdata$Floodresponse <- relevel(as.factor(TAPSdata$Floodresponse), ref = "control")
MTurk_stylized_data$BCresponse <- relevel(as.factor(MTurk_stylized_data$BCresponse), ref = "control")
MTurk_stylized_data$BSresponse <- relevel(as.factor(MTurk_stylized_data$BSresponse), ref = "control")
MTurk_stylized_data$HWresponse <- relevel(as.factor(MTurk_stylized_data$HWresponse), ref = "control")
MTurk_flint_data$treatment <- relevel(as.factor(MTurk_flint_data$treatment), ref = "control")

TAPSdata$handling_4pt <- factor(TAPSdata$handling_4pt, levels = c("Strongly disagree",
                                                                  "Disagree",
                                                                  "Agree",
                                                                  "Strongly agree"))
MTurk_stylized_data$BCgovapprove_ord <- factor(MTurk_stylized_data$BCgovapprove_ord,
                                               levels = c("Strongly disapprove",
                                                          "Disapprove",
                                                          "Approve",
                                                          "Strongly approve"))
MTurk_stylized_data$BSgovapprove_ord <- factor(MTurk_stylized_data$BSgovapprove_ord,
                                               levels = c("Strongly disapprove",
                                                          "Disapprove",
                                                          "Approve",
                                                          "Strongly approve"))
MTurk_stylized_data$HWgovapprove_ord <- factor(MTurk_stylized_data$HWgovapprove_ord,
                                               levels = c("Strongly disapprove",
                                                          "Disapprove",
                                                          "Approve",
                                                          "Strongly approve"))

TAPSdata$vote_4pt <- factor(TAPSdata$vote_4pt, levels = c("Very unlikely",
                                                          "Unlikely",
                                                          "Likely",
                                                          "Very likely"))
MTurk_stylized_data$BCgovvote_ord <- factor(MTurk_stylized_data$BCgovvote_ord,
                                            levels = c("Very unlikely",
                                                       "Somewhat unlikely",
                                                       "Somewhat likely",
                                                       "Very likely"))
MTurk_stylized_data$BSgovvote_ord <- factor(MTurk_stylized_data$BSgovvote_ord,
                                            levels = c("Very unlikely",
                                                       "Somewhat unlikely",
                                                       "Somewhat likely",
                                                       "Very likely"))
MTurk_stylized_data$HWgovvote_ord <- factor(MTurk_stylized_data$HWgovvote_ord,
                                            levels = c("Very unlikely",
                                                       "Somewhat unlikely",
                                                       "Somewhat likely",
                                                       "Very likely"))

TAPSdata$handling_5pt <- factor(TAPSdata$handling_5pt, levels = c("DK",
                                                                  "Strongly disagree",
                                                                  "Disagree",
                                                                  "Agree",
                                                                  "Strongly agree"))
TAPSdata$vote_5pt <- factor(TAPSdata$vote_5pt, levels = c("DK",
                                                          "Very unlikely",
                                                          "Unlikely",
                                                          "Likely",
                                                          "Very likely"))

MTurk_stylized_data_ordering$first_treatment <- factor(MTurk_stylized_data_ordering$first_treatment,
                                                       levels = c("control", "blameclaim",
                                                                  "blamedeflect"))
MTurk_stylized_data_ordering$second_treatment <- factor(MTurk_stylized_data_ordering$second_treatment,
                                                        levels = c("control", "blameclaim",
                                                                   "blamedeflect"))
MTurk_stylized_data_ordering$third_treatment <- factor(MTurk_stylized_data_ordering$third_treatment,
                                                       levels = c("control", "blameclaim",
                                                                  "blamedeflect"))

MTurk_flint_data$treatment <- factor(MTurk_flint_data$treatment, levels = c("control",
                                                                            "self",
                                                                            "appointee",
                                                                            "expert"))
MTurk_flint_data$handling_ord <- factor(MTurk_flint_data$handling_ord, levels = c("Very negative",
                                                                                  "Somewhat negative",
                                                                                  "Somewhat positive",
                                                                                  "Very positive"))
MTurk_flint_data$office_ord <- factor(MTurk_flint_data$office_ord, levels = c("Resign from office",
                                                                              "Remain in office",
                                                                              "Not sure"))

###############################################################################
### SUPPLEMENTAL INFORMATION FIGURES AND TABLES
###############################################################################

# Figure SI.1, distribution of DVs for four governmental crises

pdf(file = , # enter your preferred location to save the figure here
    family = "Times", height = 13, width = 12)
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,13,13), nrow=5, byrow=TRUE), 
       heights = c(22.5, 22.5, 22.5, 22.5, 10), widths = c(0.16, 0.42, 0.42))
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
text(x = 0, y = 0, labels = "Flood", cex = 2)
par(mar=c(5.1,4.1,4.1,2.1))
barplot(table(TAPSdata$Floodresponse, TAPSdata$handling_5pt), 
        main="Approval", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,350),
        col = c("gray80", "gray50", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Don't\nknow", "Strongly\ndisapprove", "Somewhat\ndisapprove", "Somewhat\napprove", "Strongly\napprove"),
      side=1, at=c(2.5,6.5,10.5,14.5,18.5), line=2.5, cex=0.75)
text(c(1.5, 2.5, 3.5, 5.5, 6.5, 7.5, 9.5, 10.5, 11.5, 13.5, 14.5, 15.5, 17.5, 18.5, 19.5),
     table(TAPSdata$Floodresponse, TAPSdata$handling_5pt) + 15, 
     table(TAPSdata$Floodresponse, TAPSdata$handling_5pt), cex = 1.25)
barplot(table(TAPSdata$Floodresponse, TAPSdata$vote_5pt), 
        main="Vote Choice", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,350),
        col = c("gray80", "gray50", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Don't\nknow", "Very\nunlikely", "Somewhat\nunlikely", "Somewhat\nlikely", "Very\nlikely"),
      side=1, at=c(2.5,6.5,10.5,14.5,18.5), line=2.5, cex=0.75)
text(c(1.5, 2.5, 3.5, 5.5, 6.5, 7.5, 9.5, 10.5, 11.5, 13.5, 14.5, 15.5, 17.5, 18.5, 19.5),
     table(TAPSdata$Floodresponse, TAPSdata$vote_5pt) + 15, 
     table(TAPSdata$Floodresponse, TAPSdata$vote_5pt), cex = 1.25)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
text(x = 0, y = 0, labels = "Bridge\nCollapse", cex = 2)
par(mar=c(5.1,4.1,4.1,2.1))
barplot(table(MTurk_stylized_data$BCresponse, MTurk_stylized_data$BCgovapprove_4pt), 
        main="Approval", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,200),
        col = c("gray80", "gray50", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Strongly\ndisapprove", "Somewhat\ndisapprove", "Somewhat\napprove", "Strongly\napprove"),
      side=1, at=c(2.5,6.5,10.5, 14.5), line=2.5, cex=0.75)
text(c(1.5, 2.5, 3.5, 5.5, 6.5, 7.5, 9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     table(MTurk_stylized_data$BCresponse, MTurk_stylized_data$BCgovapprove_4pt) + 10, 
     table(MTurk_stylized_data$BCresponse, MTurk_stylized_data$BCgovapprove_4pt), cex = 1.25)
barplot(table(MTurk_stylized_data$BCresponse, MTurk_stylized_data$BCgovvote_4pt), 
        main="Vote Choice", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,200),
        col = c("gray80", "gray50", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Very\nunlikely", "Somewhat\nunlikely", "Somewhat\nlikely", "Very\nlikely"),
      side=1, at=c(2.5,6.5,10.5, 14.5), line=2.5, cex=0.75)
text(c(1.5, 2.5, 3.5, 5.5, 6.5, 7.5, 9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     table(MTurk_stylized_data$BCresponse, MTurk_stylized_data$BCgovvote_4pt) + 10, 
     table(MTurk_stylized_data$BCresponse, MTurk_stylized_data$BCgovvote_4pt), cex = 1.25)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
text(x = 0, y = 0, labels = "Budget\nShortfall", cex = 2)
par(mar=c(5.1,4.1,4.1,2.1))
barplot(table(MTurk_stylized_data$BSresponse, MTurk_stylized_data$BSgovapprove_4pt), 
        main="Approval", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,200),
        col = c("gray80", "gray50", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Strongly\ndisapprove", "Somewhat\ndisapprove", "Somewhat\napprove", "Strongly\napprove"),
      side=1, at=c(2.5,6.5,10.5, 14.5), line=2.5, cex=0.75)
text(c(1.5, 2.5, 3.5, 5.5, 6.5, 7.5, 9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     table(MTurk_stylized_data$BSresponse, MTurk_stylized_data$BSgovapprove_4pt) + 10, 
     table(MTurk_stylized_data$BSresponse, MTurk_stylized_data$BSgovapprove_4pt), cex = 1.25)
barplot(table(MTurk_stylized_data$BSresponse, MTurk_stylized_data$BSgovvote_4pt), 
        main="Vote Choice", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,200),
        col = c("gray80", "gray50", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Very\nunlikely", "Somewhat\nunlikely", "Somewhat\nlikely", "Very\nlikely"),
      side=1, at=c(2.5,6.5,10.5, 14.5), line=2.5, cex=0.75)
text(c(1.5, 2.5, 3.5, 5.5, 6.5, 7.5, 9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     table(MTurk_stylized_data$BSresponse, MTurk_stylized_data$BSgovvote_4pt) + 10, 
     table(MTurk_stylized_data$BSresponse, MTurk_stylized_data$BSgovvote_4pt), cex = 1.25)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
text(x = 0, y = 0, labels = "Heat\nWave", cex = 2)
par(mar=c(5.1,4.1,4.1,2.1))
barplot(table(MTurk_stylized_data$HWresponse, MTurk_stylized_data$HWgovapprove_4pt), 
        main="Approval", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,200),
        col = c("gray80", "gray50", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Strongly\ndisapprove", "Somewhat\ndisapprove", "Somewhat\napprove", "Strongly\napprove"),
      side=1, at=c(2.5,6.5,10.5, 14.5), line=2.5, cex=0.75)
text(c(1.5, 2.5, 3.5, 5.5, 6.5, 7.5, 9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     table(MTurk_stylized_data$HWresponse, MTurk_stylized_data$HWgovapprove_4pt) + 10, 
     table(MTurk_stylized_data$HWresponse, MTurk_stylized_data$HWgovapprove_4pt), cex = 1.25)
barplot(table(MTurk_stylized_data$HWresponse, MTurk_stylized_data$HWgovvote_4pt), 
        main="Vote Choice", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,200),
        col = c("gray80", "gray50", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Very\nunlikely", "Somewhat\nunlikely", "Somewhat\nlikely", "Very\nlikely"),
      side=1, at=c(2.5,6.5,10.5, 14.5), line=2.5, cex=0.75)
text(c(1.5, 2.5, 3.5, 5.5, 6.5, 7.5, 9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     table(MTurk_stylized_data$HWresponse, MTurk_stylized_data$HWgovvote_4pt) + 10, 
     table(MTurk_stylized_data$HWresponse, MTurk_stylized_data$HWgovvote_4pt), cex = 1.25)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Control", "Blame Claim",
                            "Blame Deflect"),
       pch = 22,
       bty = "n",
       pt.bg = c("gray80", "gray50", "gray20"),
       cex = 1.5, pt.cex = 2.5)
dev.off()

###############################################################################
# Table SI.1--Four Governmental Crises Handling Models (Binary OLS)

# main paper models
# approval

flood_handling_bi_model <- lm(handling_bin ~ Floodresponse,
                              data = TAPSdata, weights = jan2018wt1)
summary(flood_handling_bi_model)

BC_handling_bi_model <- lm(BCgovapprove_bin ~ BCresponse, data=MTurk_stylized_data)
summary(BC_handling_bi_model)

BS_handling_bi_model <- lm(BSgovapprove_bin ~ BSresponse, data=MTurk_stylized_data)
summary(BS_handling_bi_model)

HW_handling_bi_model <- lm(HWgovapprove_bin ~ HWresponse, data=MTurk_stylized_data)
summary(HW_handling_bi_model)

texreg(l=list(flood_handling_bi_model,
              BC_handling_bi_model,
              BS_handling_bi_model,
              HW_handling_bi_model),
       stars=c(0.05), 
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect"),
       label = "table:mainmodelshandling",
       caption.above = TRUE, caption = "Main Study Models\\textemdash Handling (OLS with Binary Outcome)",
       custom.note = "%stars.  This table presents the the linear regression models
       we used to construct the plots for the results of our flood, bridge collapse, budget
       shortfall, and heat wave studies displayed in the main analysis
       of the paper.  Our outcome variable, approval of the executive's handling of the governmental
       crisis, is coded as a dichotomous variable (1 if strongly approve or approve, 0 otherwise).
       The control condition is the baseline condition.  Flood model includes survey weights; results
       remain substantively unchanged when weights are not included.",
       include.rmse=FALSE,
       include.adjrs=FALSE)

###############################################################################
# Table SI.2--Four Governmental Crises Vote Choice Models (Binary OLS)

flood_vote_next_bi_model <- lm(vote_bin ~ Floodresponse,
                               data = TAPSdata, weights = jan2018wt1)
summary(flood_vote_next_bi_model)

BC_vote_next_bi_model <- lm(BCgovvote_bin ~ BCresponse, data=MTurk_stylized_data)
summary(BC_vote_next_bi_model)

BS_vote_next_bi_model <- lm(BSgovvote_bin ~ BSresponse, data=MTurk_stylized_data)
summary(BS_vote_next_bi_model)

HW_vote_next_bi_model <- lm(HWgovvote_bin ~ HWresponse, data=MTurk_stylized_data)
summary(HW_vote_next_bi_model)

texreg(l=list(flood_vote_next_bi_model,
              BC_vote_next_bi_model,
              BS_vote_next_bi_model,
              HW_vote_next_bi_model),
       stars=c(0.05), 
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect"),
       label = "table:mainmodelsvote",
       caption.above = TRUE, caption = "Main Study Models\\textemdash Vote (OLS with Binary Outcome)",
       custom.note = "%stars.  This table presents the the linear regression models
       we used to construct the plots for the results of our flood, bridge collapse, budget
       shortfall, and heat wave studies displayed in the main analysis
       of the paper.  Our outcome variable, likelihood of voting for the executive in the next election,
       is coded as a dichotomous variable (1 if very likely or likely, 0 otherwise).
       The control condition is the baseline condition.  Flood model includes survey weights; results
       remain substantively unchanged when weights are not included.",
       include.rmse=FALSE,
       include.adjrs=FALSE)

###############################################################################
# Table SI.3--Four Governmental Crises Approval Models (Ordinal OLS)

flood_handling_model <- lm(handling_org ~ Floodresponse,
                           data = TAPSdata, weights = jan2018wt1)
summary(flood_handling_model)

BC_handling_model <- lm(BCgovapprove_4pt ~ BCresponse, data=MTurk_stylized_data)
summary(BC_handling_model)

BS_handling_model <- lm(BSgovapprove_4pt ~ BSresponse, data=MTurk_stylized_data)
summary(BS_handling_model)

HW_handling_model <- lm(HWgovapprove_4pt ~ HWresponse, data=MTurk_stylized_data)
summary(HW_handling_model)

texreg(l=list(flood_handling_model,
              BC_handling_model,
              BS_handling_model,
              HW_handling_model),
       stars=c(0.05), 
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect"),
       label = "table:mainmodelshandlingols4",
       caption.above = TRUE, caption = "Main Study Models\\textemdash Handling (OLS with Ordinal Outcome)",
       custom.note = "%stars.  This table presents the the linear regression models
       we used to construct the plots for the results of our flood, bridge collapse, budget
       shortfall, and heat wave studies displayed in the main analysis
       of the paper.  Our outcome variable, approval of the 
       executive's handling of the governmental crisis, is coded as a four-point an ordinal variable.  
       Because ``don\'t know\" outcome
       responses do not fit into an ordinal framework, these responses in the flood study
       are recoded as NAs for
       these models only.  The control condition is the baseline condition.   
       Flood model includes survey weights; results
       remain substantively unchanged when weights are not included.",
       include.rmse=FALSE,
       include.adjrs=FALSE)

###############################################################################
# Table SI.4--Four Governmental Crises Vote Choice Models (Ordinal OLS)

flood_vote_next_model <- lm(vote_org ~ Floodresponse,
                            data = TAPSdata, weights = jan2018wt1)
summary(flood_vote_next_model)

BC_vote_next_model <- lm(BCgovvote_4pt ~ BCresponse, data=MTurk_stylized_data)
summary(BC_vote_next_model)

BS_vote_next_model <- lm(BSgovvote_4pt ~ BSresponse, data=MTurk_stylized_data)
summary(BS_vote_next_model)

HW_vote_next_model <- lm(HWgovvote_4pt ~ HWresponse, data=MTurk_stylized_data)
summary(HW_vote_next_model)

texreg(l=list(flood_vote_next_model,
              BC_vote_next_model,
              BS_vote_next_model,
              HW_vote_next_model),
       stars=c(0.05), 
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect"),
       label = "table:mainmodelsvoteols4",
       caption.above = TRUE, caption = "Main Study Models\\textemdash Vote (OLS with Ordinal Outcome)",
       custom.note = "%stars.  This table presents the the linear regression models
       we used to construct the plots for the results of our flood, bridge collapse, budget
       shortfall, and heat wave studies displayed in the main analysis
       of the paper.  Our outcome variable, respondents' likelihood of voting for the executive in
       the next election, is coded as a four-point an ordinal variable.  
       Because ``don\'t know\" outcome
       responses do not fit into an ordinal framework, these responses in the flood study
       are recoded as NAs for
       these models only.  The control condition is the baseline condition.   
       Flood model includes survey weights; results
       remain substantively unchanged when weights are not included.",
       include.rmse=FALSE,
       include.adjrs=FALSE)

###############################################################################
# Table SI.5--Four Governmental Crises Approval Models (Binary Logistic)

flood_handling_bi_model <- glm(handling_bin ~ Floodresponse, family = binomial(link="logit"),
                               data = TAPSdata, weights = jan2018wt1)
summary(flood_handling_bi_model)

BC_handling_bi_model <- glm(BCgovapprove_bin ~ BCresponse, family = binomial(link="logit"),
                            data=MTurk_stylized_data)
summary(BC_handling_bi_model)

BS_handling_bi_model <- glm(BSgovapprove_bin ~ BSresponse, family = binomial(link="logit"),
                            data=MTurk_stylized_data)
summary(BS_handling_bi_model)

HW_handling_bi_model <- glm(HWgovapprove_bin ~ HWresponse, family = binomial(link="logit"),
                            data=MTurk_stylized_data)
summary(HW_handling_bi_model)


texreg(l=list(flood_handling_bi_model,
              BC_handling_bi_model,
              BS_handling_bi_model,
              HW_handling_bi_model),
       stars=c(0.05), 
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect"),
       label = "table:mainmodelshandlinglogistic",
       caption.above = TRUE, caption = "Main Study Models\\textemdash Handling (Logistic Regression)",
       custom.note = "%stars.  This table presents the the logistic regression models
       that are analogous to the linear regression models we used to estimate the results
       presented in the main analysis of the paper.  Our outcome variable, approval of the executive's 
       handling of the governmental
       crisis, is coded as a dichotomous variable (1 if strongly approve or approve, 0 otherwise).
       The control condition is the baseline condition.  Flood model includes survey weights; results
       remain substantively unchanged when weights are not included.", 
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE)

###############################################################################
# Table SI.6--Four Governmental Crises Vote Choice Models (Binary Logistic)

flood_vote_next_bi_model <- glm(vote_bin ~ Floodresponse, family = binomial(link="logit"),
                                data = TAPSdata, weights = jan2018wt1)
summary(flood_vote_next_bi_model)

BC_vote_next_bi_model <- glm(BCgovvote_bin ~ BCresponse, family = binomial(link="logit"),
                             data=MTurk_stylized_data)
summary(BC_vote_next_bi_model)

BS_vote_next_bi_model <- glm(BSgovvote_bin ~ BSresponse, family = binomial(link="logit"),
                             data=MTurk_stylized_data)
summary(BS_vote_next_bi_model)

HW_vote_next_bi_model <- glm(HWgovvote_bin ~ HWresponse, family = binomial(link="logit"),
                             data=MTurk_stylized_data)
summary(HW_vote_next_bi_model)


texreg(l=list(flood_vote_next_bi_model,
              BC_vote_next_bi_model,
              BS_vote_next_bi_model,
              HW_vote_next_bi_model),
       stars=c(0.05), 
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect"),
       label = "table:mainmodelsvotelogistic",
       caption.above = TRUE, caption = "Main Study Models\\textemdash Vote (Logistic Regression)",
       custom.note = "%stars.  This table presents the the logistic regression models
       that are analogous to the linear regression models we used to estimate the results
       presented in the main analysis of the paper.   Our outcome variable, likelihood of voting for the executive in the next election,
       is coded as a dichotomous variable (1 if very likely or likely, 0 otherwise).
       The control condition is the baseline condition.  Flood model includes survey weights; results
       remain substantively unchanged when weights are not included.", 
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE)

###############################################################################
# Table SI.7--Four Governmental Crises Approval Models (Ordinal Logistic)

flood_handling_4point <- polr(handling_4pt ~ Floodresponse,
                              data = TAPSdata, weights = jan2018wt1)

BC_handling_4point <- polr(BCgovapprove_ord ~ BCresponse,
                           data = MTurk_stylized_data)

BS_handling_4point <- polr(BSgovapprove_ord ~ BSresponse,
                           data = MTurk_stylized_data)

HW_handling_4point <- polr(HWgovapprove_ord ~ HWresponse,
                           data = MTurk_stylized_data)

texreg(l=list(flood_handling_4point,
              BC_handling_4point,
              BS_handling_4point,
              HW_handling_4point),
       stars=c(0.05),        
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect",
                              "Strongly disagree|Disagree"="Strongly disapprove|Disapprove",
                              "Disagree|Agree"="Disapprove|Approve",
                              "Agree|Strongly agree"="Approve|Strongly approve",
                              "Strongly disapprove|Disapprove"="Strongly disapprove|Disapprove",
                              "Disapprove|Approve"="Disapprove|Approve",
                              "Approve|Strongly approve"="Approve|Strongly approve"),
       label = "table:mainmodelshandlingpolr",
       caption.above = TRUE, 
       caption = "Main Study Models\\textemdash Handling (Ordinal Logistic Regression)",
       custom.note = "%stars.  This table presents the the ordinal logistic regression models
       that are analogous to the linear regression models we used to estimate the results
       presented in the main analysis of the paper.  Our outcome variable, approval of the 
       executive's handling of the governmental crisis, is coded as a four-point an ordinal variable.  
       Because ``don\'t know\" outcome
       responses do not fit into an ordinal framework, these responses in the flood study
       are recoded as NAs for
       these models only.  The control condition is the baseline condition.   
       Flood model includes survey weights; results
       remain substantively unchanged when weights are not included.", 
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE, 
       include.thresholds=TRUE)
# note that the number of observations for the Flood model do not equal that
# reported in the paper; this is because polr returns effective sample size
# rather than raw sample size

###############################################################################
# Table SI.8--Four Governmental Crises Vote Choice Models (Ordinal Logistic)

flood_vote_next_4point <- polr(vote_4pt ~ Floodresponse,
                               data = TAPSdata, weights = jan2018wt1)

BC_vote_next_4point <- polr(BCgovvote_ord ~ BCresponse,
                            data = MTurk_stylized_data)

BS_vote_next_4point <- polr(BSgovvote_ord ~ BSresponse,
                            data = MTurk_stylized_data)

HW_vote_next_4point <- polr(HWgovvote_ord ~ HWresponse,
                            data = MTurk_stylized_data)

texreg(l=list(flood_vote_next_4point,
              BC_vote_next_4point,
              BS_vote_next_4point,
              HW_vote_next_4point),
       stars=c(0.05),        
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect",
                              "Very unlikely|Unlikely"="Very unlikely|Somewhat unlikely",
                              "Unlikely|Likely"="Somewhat unlikely|Somewhat likely",
                              "Likely|Very likely"="Somewhat likely|Very likely",
                              "Very unlikely|Somewhat unlikely"="Very unlikely|Somewhat unlikely",
                              "Somewhat unlikely|Somewhat likely"="Somewhat unlikely|Somewhat likely",
                              "Somewhat likely|Very likely"="Somewhat likely|Very likely"),
       label = "table:mainmodelsvotepolr",
       caption.above = TRUE, 
       caption = "Main Study Models\\textemdash Vote (Ordinal Logistic Regression)",
       custom.note = "%stars.  This table presents the the ordinal logistic regression models
       that are analogous to the linear regression models we used to estimate the results
       presented in the main analysis of the paper.   Our outcome variable, likelihood of voting for the executive in the next election,
       is coded as a four-point ordinal variable.  
       Because ``don\'t know\" outcome
       responses do not fit into an ordinal framework, these responses in the flood study
       are recoded as NAs for
       these models only.  The control condition is the baseline condition.   
       Flood model includes survey weights; results
       remain substantively unchanged when weights are not included.", 
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE, 
       include.thresholds=TRUE)
# note that the number of observations for the Flood model do not equal that
# reported in the paper; this is because polr returns effective sample size
# rather than raw sample size

###############################################################################
# Table SI.9--Flood Study Approval Model (MNL)

flood_handling_5pt <- multinom(handling_5pt ~ Floodresponse,
                                  data = TAPSdata, weights = jan2018wt1, Hess = TRUE)

texreg(l=list(flood_handling_5pt),
       stars=c(0.05), #custom.model.names = c("Handling", "Vote")
       custom.coef.names = c("Intercept", "Blame Claim", "Blame Deflect"),
       label = "table:floodhandlingmnl",
       caption.above = TRUE, 
       caption = "Flood Study Models\\textemdash Handling (Multinomial Logistic Regression)",
       custom.note = "%stars.  This table presents the a multinomial logistic regression model
       for approval of the mayor's handling of the flood.  Our outcome variable
       is coded to account for responses of strongly disagree, disagree,
       agree, strongly agree, and don\'t know (which is the baseline response choice). 
       The control condition is the baseline condition.  Model includes
       survey weights; results remain substantively unchanged when weights are not included.",
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE, 
       include.thresholds=TRUE, beside = TRUE)

###############################################################################
# Table SI.10--Flood Study Vote Choice Model (MNL)

flood_vote_5pt <- multinom(vote_5pt ~ Floodresponse,
                                   data = TAPSdata, weights = jan2018wt1, Hess = TRUE)
texreg(l=list(flood_vote_5pt),
       stars=c(0.05), #custom.model.names = c("Handling", "Vote")
       custom.coef.names = c("Intercept", "Blame Claim", "Blame Deflect"),
       label = "table:floodvotemnl",
       caption.above = TRUE, 
       caption = "Flood Study Models\\textemdash Vote (Multinomial Logistic Regression)",
       custom.note = "%stars.  This table presents the the multinomial logistic regression models
       for likelihood of voting for the mayor in the next election.  Our outcome variable is coded 
       to account for responses of very unlikely, unlikely, likely, very likely, and
       don\'t know (which is the baseline response choice). 
       The control condition is the baseline condition.  Models include
       survey weights; results remain substantively unchanged when weights are not included.",
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE, 
       include.thresholds=TRUE, beside = TRUE)

###############################################################################
# Table SI.11--MTurk Sample Ordering Effects--Handling (Binary OLS)

only_first_approve_model <- lm(first_handling ~ first_treatment, data = MTurk_stylized_data_ordering)
summary(only_first_approve_model)

only_second_approve_model <- lm(second_handling ~ second_treatment, data = MTurk_stylized_data_ordering)
summary(only_second_approve_model)

only_third_approve_model <- lm(third_handling ~ third_treatment, data = MTurk_stylized_data_ordering)
summary(only_third_approve_model)

texreg(l=list(only_first_approve_model,
  only_second_approve_model,
  only_third_approve_model),
  stars=c(0.05), 
  custom.model.names = c("First Module", "Second Module", "Third Module"),
  custom.coef.map = list("(Intercept)"="Intercept", 
                         "first_treatmentblameclaim"="Blame Claim", 
                         "first_treatmentblamedeflect"="Blame Deflect",
                         "second_treatmentblameclaim"="Blame Claim", 
                         "second_treatmentblamedeflect"="Blame Deflect",
                         "third_treatmentblameclaim"="Blame Claim", 
                         "third_treatmentblamedeflect"="Blame Deflect"),
  label = "table:orderingeffectshandling",
  caption.above = TRUE, caption = "mTurk Sample Ordering Effects\\textemdash Handling (OLS with Binary Outcome)",
  custom.note = "%stars.  This table presents linear regression models
       we use to investigate whether the results from our studies which use our mTurk sample are
       artifacts of ordering effects.  Our outcome variable, approval of the executive's handling of 
       the governmental crisis, is coded as a dichotomous variable (1 if strongly approve or approve, 0 
       otherwise). The control condition is the baseline condition.  To investigate ordering effects, we estimate the treatment
       effects for blame claiming and blame deflecting among respondents in each study 
       (i.e., the first, second, and third studies they completed in the survey); if ordering 
       effects are problematic, we would expect to see instability in the treatment effects given the 
       temporal ordering of the studies.  However, we see that the treatment effects for blame claiming and blame deflecting are
       substantively similar across temporal ordering of studies and in comparison to those presented in the main paper, which
       pool across each respondents' ordering for each study.  This suggests that our results
       are not an artifact of ordering effects.",
  include.rmse=FALSE,
  include.adjrs=FALSE)

###############################################################################
# Table SI.12--MTurk Sample Ordering Effects--Vote Choice (Binary OLS)

only_first_vote_model <- lm(first_vote ~ first_treatment, data = MTurk_stylized_data_ordering)
summary(only_first_vote_model)

only_second_vote_model <- lm(second_vote ~ second_treatment, data = MTurk_stylized_data_ordering)
summary(only_second_vote_model)

only_third_vote_model <- lm(third_vote ~ third_treatment, data = MTurk_stylized_data_ordering)
summary(only_third_vote_model)

texreg(l=list(only_first_vote_model,
  only_second_vote_model,
  only_third_vote_model),
  stars=c(0.05), 
  custom.model.names = c("First Module", "Second Module", "Third Module"),
  custom.coef.map = list("(Intercept)"="Intercept", 
                         "first_treatmentblameclaim"="Blame Claim", 
                         "first_treatmentblamedeflect"="Blame Deflect",
                         "second_treatmentblameclaim"="Blame Claim", 
                         "second_treatmentblamedeflect"="Blame Deflect",
                         "third_treatmentblameclaim"="Blame Claim", 
                         "third_treatmentblamedeflect"="Blame Deflect"),
  label = "table:orderingeffectshandling",
  caption.above = TRUE, caption = "mTurk Sample Ordering Effects\\textemdash Vote (OLS with Binary Outcome)",
  custom.note = "%stars.  This table presents linear regression models
  we use to investigate whether the results from our studies which use our mTurk sample are
  artifacts of ordering effects.  Our outcome variable, likelihood of voting for the executive in the next election,
  is coded as a dichotomous variable (1 if very likely or likely, 0 otherwise). The control condition 
  is the baseline condition.  To investigate ordering effects, we estimate the treatment
       effects for blame claiming and blame deflecting among respondents in each study 
  (i.e., the first, second, and third studies they completed in the survey); if ordering 
  effects are problematic, we would expect to see instability in the treatment effects given the 
  temporal ordering of the studies.  However, we see that the treatment effects for blame claiming and blame deflecting are
  substantively similar across temporal ordering of studies and in comparison to those presented in the main paper, which
  pool across each respondents' ordering for each study.  This suggests that our results
  are not an artifact of ordering effects.",
  include.rmse=FALSE,
  include.adjrs=FALSE)

###############################################################################
# Table SI.13--Factorial Experiments Handling (Binary OLS)

BCmarginal_handling_model <- lm(BCgovapprovebin ~ BCresponse_blameclaim + BCresponse_blamedeflect +
                                  BCseverity_mod + BCseverity_high + BCcopart + BCnoncopart,
                                data=MTurk_factorial_data)
summary(BCmarginal_handling_model)

BCtwoway_handling_model <- lm(BCgovapprovebin ~ BCresponse_blameclaim + BCresponse_blamedeflect +
                                BCseverity_mod + BCseverity_high + BCcopart + BCnoncopart +
                                BCresponse_blameclaim:BCseverity_mod + BCresponse_blameclaim:BCseverity_high +
                                BCresponse_blamedeflect:BCseverity_mod + BCresponse_blamedeflect:BCseverity_high +
                                BCresponse_blameclaim:BCcopart + BCresponse_blamedeflect:BCcopart +
                                BCresponse_blameclaim:BCnoncopart + BCresponse_blamedeflect:BCnoncopart,
                              data=MTurk_factorial_data)
summary(BCtwoway_handling_model)

BSmarginal_handling_model <- lm(BSgovapprovebin ~ BSresponse_blameclaim + BSresponse_blamedeflect +
                                  BSseverity_mod + BSseverity_high + BScopart + BSnoncopart,
                                data=MTurk_factorial_data)
summary(BSmarginal_handling_model)

BStwoway_handling_model <- lm(BSgovapprovebin ~ BSresponse_blameclaim + BSresponse_blamedeflect +
                                BSseverity_mod + BSseverity_high + BScopart + BSnoncopart +
                                BSresponse_blameclaim:BSseverity_mod + BSresponse_blameclaim:BSseverity_high +
                                BSresponse_blamedeflect:BSseverity_mod + BSresponse_blamedeflect:BSseverity_high +
                                BSresponse_blameclaim:BScopart + BSresponse_blamedeflect:BScopart +
                                BSresponse_blameclaim:BSnoncopart + BSresponse_blamedeflect:BSnoncopart,
                              data=MTurk_factorial_data)
summary(BStwoway_handling_model)

HWmarginal_handling_model <- lm(HWgovapprovebin ~ HWresponse_blameclaim + HWresponse_blamedeflect +
                                  HWseverity_mod + HWseverity_high + HWcopart + HWnoncopart,
                                data=MTurk_factorial_data)
summary(HWmarginal_handling_model)

HWtwoway_handling_model <- lm(HWgovapprovebin ~ HWresponse_blameclaim + HWresponse_blamedeflect +
                                HWseverity_mod + HWseverity_high + HWcopart + HWnoncopart +
                                HWresponse_blameclaim:HWseverity_mod + HWresponse_blameclaim:HWseverity_high +
                                HWresponse_blamedeflect:HWseverity_mod + HWresponse_blamedeflect:HWseverity_high +
                                HWresponse_blameclaim:HWcopart + HWresponse_blamedeflect:HWcopart +
                                HWresponse_blameclaim:HWnoncopart + HWresponse_blamedeflect:HWnoncopart,
                              data=MTurk_factorial_data)
summary(HWtwoway_handling_model)

texreg(l=list(BCmarginal_handling_model, BCtwoway_handling_model, 
              BSmarginal_handling_model, BStwoway_handling_model,
              HWmarginal_handling_model, HWtwoway_handling_model),
       custom.model.names = c("Bridge Collapse", "Bridge Collapse", "Budget Shortfall",
                              "Budget Shortfall", "Heat Wave", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "BCresponse_blameclaim"="Blame Claim",
                              "BCresponse_blamedeflect"="Blame Deflect",
                              "BCseverity_mod"="Moderate Severity",
                              "BCseverity_high"="High Severity",
                              "BCresponse_blameclaim:BCseverity_mod"="Blame Claim x Moderate Severity",
                              "BCresponse_blameclaim:BCseverity_high"="Blame Claim x High Severity",
                              "BCresponse_blamedeflect:BCseverity_mod"="Blame Deflect x Moderate Severity",
                              "BCresponse_blamedeflect:BCseverity_high"="Blame Deflect x High Severity",
                              "BCcopart"="Copartisan",
                              "BCnoncopart"="Noncopartisan",
                              "BCresponse_blameclaim:BCcopart"="Blame Claim x Copartisan",
                              "BCresponse_blameclaim:BCnoncopart"="Blame Claim x Noncopartisan",
                              "BCresponse_blamedeflect:BCcopart"="Blame Deflect x Copartisan",
                              "BCresponse_blamedeflect:BCnoncopart"="Blame Deflect x Noncopartisan",
                              "BSresponse_blameclaim"="Blame Claim",
                              "BSresponse_blamedeflect"="Blame Deflect",
                              "BSseverity_mod"="Moderate Severity",
                              "BSseverity_high"="High Severity",
                              "BSresponse_blameclaim:BSseverity_mod"="Blame Claim x Moderate Severity",
                              "BSresponse_blameclaim:BSseverity_high"="Blame Claim x High Severity",
                              "BSresponse_blamedeflect:BSseverity_mod"="Blame Deflect x Moderate Severity",
                              "BSresponse_blamedeflect:BSseverity_high"="Blame Deflect x High Severity",
                              "BScopart"="Copartisan",
                              "BSnoncopart"="Noncopartisan",
                              "BSresponse_blameclaim:BScopart"="Blame Claim x Copartisan",
                              "BSresponse_blameclaim:BSnoncopart"="Blame Claim x Noncopartisan",
                              "BSresponse_blamedeflect:BScopart"="Blame Deflect x Copartisan",
                              "BSresponse_blamedeflect:BSnoncopart"="Blame Deflect x Noncopartisan",
                              "HWresponse_blameclaim"="Blame Claim",
                              "HWresponse_blamedeflect"="Blame Deflect",
                              "HWseverity_mod"="Moderate Severity",
                              "HWseverity_high"="High Severity",
                              "HWresponse_blameclaim:HWseverity_mod"="Blame Claim x Moderate Severity",
                              "HWresponse_blameclaim:HWseverity_high"="Blame Claim x High Severity",
                              "HWresponse_blamedeflect:HWseverity_mod"="Blame Deflect x Moderate Severity",
                              "HWresponse_blamedeflect:HWseverity_high"="Blame Deflect x High Severity",
                              "HWcopart"="Copartisan",
                              "HWnoncopart"="Noncopartisan",
                              "HWresponse_blameclaim:HWcopart"="Blame Claim x Copartisan",
                              "HWresponse_blameclaim:HWnoncopart"="Blame Claim x Noncopartisan",
                              "HWresponse_blamedeflect:HWcopart"="Blame Deflect x Copartisan",
                              "HWresponse_blamedeflect:HWnoncopart"="Blame Deflect x Noncopartisan"),
       sideways = TRUE,        
       label = "table:conjointmodelsapproval",
       caption.above = TRUE, 
       caption = "Conjoint Experiments Models\\textemdash Handling (OLS with Binary Outcome)",
       custom.note = "%stars.  This table presents linear regression analyses of our three
       conjoint experiments.  For each experimental context (bridge collapse, budget
       shortfall, and heat wave) we regress dichotomous indicators of respondents'
       approval of the executive's handling of the governmental crisis on dichotomous indicators for
       the levels of each attribute (executive response, severity, and the correspondence between
       the party identifications of the respondent and the executive) presented to the respondents.",
       include.rmse=FALSE, include.adjrs=FALSE)

###############################################################################
# Table SI.14--Factorial Experiments Vote Choice (Binary OLS)

BCmarginal_vote_next_model <- lm(BCgovvotebin ~ BCresponse_blameclaim + BCresponse_blamedeflect +
                                   BCseverity_mod + BCseverity_high + BCcopart + BCnoncopart,
                                 data=MTurk_factorial_data)
summary(BCmarginal_vote_next_model)

BCtwoway_vote_next_model <- lm(BCgovvotebin ~ BCresponse_blameclaim + BCresponse_blamedeflect +
                                 BCseverity_mod + BCseverity_high + BCcopart + BCnoncopart +
                                 BCresponse_blameclaim:BCseverity_mod + BCresponse_blameclaim:BCseverity_high +
                                 BCresponse_blamedeflect:BCseverity_mod + BCresponse_blamedeflect:BCseverity_high +
                                 BCresponse_blameclaim:BCcopart + BCresponse_blamedeflect:BCcopart +
                                 BCresponse_blameclaim:BCnoncopart + BCresponse_blamedeflect:BCnoncopart,
                               data=MTurk_factorial_data)
summary(BCtwoway_vote_next_model)

BSmarginal_vote_next_model <- lm(BSgovvotebin ~ BSresponse_blameclaim + BSresponse_blamedeflect +
                                   BSseverity_mod + BSseverity_high + BScopart + BSnoncopart,
                                 data=MTurk_factorial_data)
summary(BSmarginal_vote_next_model)

BStwoway_vote_next_model <- lm(BSgovvotebin ~ BSresponse_blameclaim + BSresponse_blamedeflect +
                                 BSseverity_mod + BSseverity_high + BScopart + BSnoncopart +
                                 BSresponse_blameclaim:BSseverity_mod + BSresponse_blameclaim:BSseverity_high +
                                 BSresponse_blamedeflect:BSseverity_mod + BSresponse_blamedeflect:BSseverity_high +
                                 BSresponse_blameclaim:BScopart + BSresponse_blamedeflect:BScopart +
                                 BSresponse_blameclaim:BSnoncopart + BSresponse_blamedeflect:BSnoncopart,
                               data=MTurk_factorial_data)
summary(BStwoway_vote_next_model)

HWmarginal_vote_next_model <- lm(HWgovvotebin ~ HWresponse_blameclaim + HWresponse_blamedeflect +
                                   HWseverity_mod + HWseverity_high + HWcopart + HWnoncopart,
                                 data=MTurk_factorial_data)
summary(HWmarginal_vote_next_model)

HWtwoway_vote_next_model <- lm(HWgovvotebin ~ HWresponse_blameclaim + HWresponse_blamedeflect +
                                 HWseverity_mod + HWseverity_high + HWcopart + HWnoncopart +
                                 HWresponse_blameclaim:HWseverity_mod + HWresponse_blameclaim:HWseverity_high +
                                 HWresponse_blamedeflect:HWseverity_mod + HWresponse_blamedeflect:HWseverity_high +
                                 HWresponse_blameclaim:HWcopart + HWresponse_blamedeflect:HWcopart +
                                 HWresponse_blameclaim:HWnoncopart + HWresponse_blamedeflect:HWnoncopart,
                               data=MTurk_factorial_data)
summary(HWtwoway_vote_next_model)

texreg(l=list(BCmarginal_vote_next_model, BCtwoway_vote_next_model, 
              BSmarginal_vote_next_model, BStwoway_vote_next_model,
              HWmarginal_vote_next_model, HWtwoway_vote_next_model),
       custom.model.names = c("Bridge Collapse", "Bridge Collapse", "Budget Shortfall",
                              "Budget Shortfall", "Heat Wave", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "BCresponse_blameclaim"="Blame Claim",
                              "BCresponse_blamedeflect"="Blame Deflect",
                              "BCseverity_mod"="Moderate Severity",
                              "BCseverity_high"="High Severity",
                              "BCresponse_blameclaim:BCseverity_mod"="Blame Claim x Moderate Severity",
                              "BCresponse_blameclaim:BCseverity_high"="Blame Claim x High Severity",
                              "BCresponse_blamedeflect:BCseverity_mod"="Blame Deflect x Moderate Severity",
                              "BCresponse_blamedeflect:BCseverity_high"="Blame Deflect x High Severity",
                              "BCcopart"="Copartisan",
                              "BCnoncopart"="Noncopartisan",
                              "BCresponse_blameclaim:BCcopart"="Blame Claim x Copartisan",
                              "BCresponse_blameclaim:BCnoncopart"="Blame Claim x Noncopartisan",
                              "BCresponse_blamedeflect:BCcopart"="Blame Deflect x Copartisan",
                              "BCresponse_blamedeflect:BCnoncopart"="Blame Deflect x Noncopartisan",
                              "BSresponse_blameclaim"="Blame Claim",
                              "BSresponse_blamedeflect"="Blame Deflect",
                              "BSseverity_mod"="Moderate Severity",
                              "BSseverity_high"="High Severity",
                              "BSresponse_blameclaim:BSseverity_mod"="Blame Claim x Moderate Severity",
                              "BSresponse_blameclaim:BSseverity_high"="Blame Claim x High Severity",
                              "BSresponse_blamedeflect:BSseverity_mod"="Blame Deflect x Moderate Severity",
                              "BSresponse_blamedeflect:BSseverity_high"="Blame Deflect x High Severity",
                              "BScopart"="Copartisan",
                              "BSnoncopart"="Noncopartisan",
                              "BSresponse_blameclaim:BScopart"="Blame Claim x Copartisan",
                              "BSresponse_blameclaim:BSnoncopart"="Blame Claim x Noncopartisan",
                              "BSresponse_blamedeflect:BScopart"="Blame Deflect x Copartisan",
                              "BSresponse_blamedeflect:BSnoncopart"="Blame Deflect x Noncopartisan",
                              "HWresponse_blameclaim"="Blame Claim",
                              "HWresponse_blamedeflect"="Blame Deflect",
                              "HWseverity_mod"="Moderate Severity",
                              "HWseverity_high"="High Severity",
                              "HWresponse_blameclaim:HWseverity_mod"="Blame Claim x Moderate Severity",
                              "HWresponse_blameclaim:HWseverity_high"="Blame Claim x High Severity",
                              "HWresponse_blamedeflect:HWseverity_mod"="Blame Deflect x Moderate Severity",
                              "HWresponse_blamedeflect:HWseverity_high"="Blame Deflect x High Severity",
                              "HWcopart"="Copartisan",
                              "HWnoncopart"="Noncopartisan",
                              "HWresponse_blameclaim:HWcopart"="Blame Claim x Copartisan",
                              "HWresponse_blameclaim:HWnoncopart"="Blame Claim x Noncopartisan",
                              "HWresponse_blamedeflect:HWcopart"="Blame Deflect x Copartisan",
                              "HWresponse_blamedeflect:HWnoncopart"="Blame Deflect x Noncopartisan"),
       sideways = TRUE,        
       label = "table:conjointmodelsvote",
       caption.above = TRUE, 
       caption = "Conjoint Experiments Models\\textemdash Vote (OLS with Binary Outcome)",
       custom.note = "%stars.  This table presents linear regression analyses of our three
       conjoint experiments.  For each experimental context (bridge collapse, budget
       shortfall, and heat wave) we regress dichotomous indicators of respondents'
       likelihood of voting for the executive in the next election on dichotomous indicators for
       the levels of each attribute (executive response, severity, and the correspondence between
       the party identifications of the respondent and the executive) presented to the respondents.",
       include.rmse=FALSE, include.adjrs=FALSE)

###############################################################################
# Figure SI.2--Factorial Experiments Handling (Response and Severity)

# bridge collapse

# control/low severirty vs. blameclaim/low severity
BCcontrol_blameclaim_low <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blameclaim = 0"))
summary(BCcontrol_blameclaim_low)
# control/low severirty vs. blamedeflect/low severity
BCcontrol_blamedeflect_low <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blamedeflect = 0"))
summary(BCcontrol_blamedeflect_low)
# control/mod severirty vs. blameclaim/mod severity
BCcontrol_blameclaim_mod <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blameclaim + 
                                                        BCresponse_blameclaim:BCseverity_mod = 0"))
summary(BCcontrol_blameclaim_mod)
# control/mod severirty vs. blamedeflect/mod severity
BCcontrol_blamedeflect_mod <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blamedeflect +
                                                          BCresponse_blamedeflect:BCseverity_mod = 0"))
summary(BCcontrol_blamedeflect_mod)
# control/high severirty vs. blameclaim/high severity
BCcontrol_blameclaim_high <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blameclaim +
                                                         BCresponse_blameclaim:BCseverity_high = 0"))
summary(BCcontrol_blameclaim_high)
# control/high severirty vs. blamedeflect/high severity
BCcontrol_blamedeflect_high <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blamedeflect +
                                                           BCresponse_blamedeflect:BCseverity_high = 0"))
summary(BCcontrol_blamedeflect_high)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(BCcontrol_blameclaim_low),
       coef(BCcontrol_blameclaim_mod),
       coef(BCcontrol_blameclaim_high),
       coef(BCcontrol_blamedeflect_low),
       coef(BCcontrol_blamedeflect_mod),
       coef(BCcontrol_blamedeflect_high)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(BCcontrol_blameclaim_low)$confint[2],
         x1=confint(BCcontrol_blameclaim_low)$confint[3],
         y0=7)
segments(x0=confint(BCcontrol_blameclaim_mod)$confint[2],
         x1=confint(BCcontrol_blameclaim_mod)$confint[3],
         y0=6)
segments(x0=confint(BCcontrol_blameclaim_high)$confint[2],
         x1=confint(BCcontrol_blameclaim_high)$confint[3],
         y0=5)
segments(x0=confint(BCcontrol_blamedeflect_low)$confint[2],
         x1=confint(BCcontrol_blamedeflect_low)$confint[3],
         y0=3)
segments(x0=confint(BCcontrol_blamedeflect_mod)$confint[2],
         x1=confint(BCcontrol_blamedeflect_mod)$confint[3],
         y0=2)
segments(x0=confint(BCcontrol_blamedeflect_high)$confint[2],
         x1=confint(BCcontrol_blamedeflect_high)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Bridge Collapse", side = 3, line=0.5, cex = 2)
text(rev(c(coef(BCcontrol_blameclaim_low),
           coef(BCcontrol_blameclaim_mod),
           coef(BCcontrol_blameclaim_high),
           coef(BCcontrol_blamedeflect_low),
           coef(BCcontrol_blamedeflect_mod),
           coef(BCcontrol_blamedeflect_high))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(BCcontrol_blameclaim_low),
                                                          coef(BCcontrol_blameclaim_mod),
                                                          coef(BCcontrol_blameclaim_high),
                                                          coef(BCcontrol_blamedeflect_low),
                                                          coef(BCcontrol_blamedeflect_mod),
                                                          coef(BCcontrol_blamedeflect_high)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Low Severity", "Moderate Severity",
                            "High Severity"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

# budget shortfall

# control/low severirty vs. blameclaim/low severity
BScontrol_blameclaim_low <- glht(BStwoway_handling_model, linfct=c("BSresponse_blameclaim = 0"))
summary(BScontrol_blameclaim_low)
# control/low severirty vs. blamedeflect/low severity
BScontrol_blamedeflect_low <- glht(BStwoway_handling_model, linfct=c("BSresponse_blamedeflect = 0"))
summary(BScontrol_blamedeflect_low)
# control/mod severirty vs. blameclaim/mod severity
BScontrol_blameclaim_mod <- glht(BStwoway_handling_model, linfct=c("BSresponse_blameclaim + 
                                                                   BSresponse_blameclaim:BSseverity_mod = 0"))
summary(BScontrol_blameclaim_mod)
# control/mod severirty vs. blamedeflect/mod severity
BScontrol_blamedeflect_mod <- glht(BStwoway_handling_model, linfct=c("BSresponse_blamedeflect +
                                                                     BSresponse_blamedeflect:BSseverity_mod = 0"))
summary(BScontrol_blamedeflect_mod)
# control/high severirty vs. blameclaim/high severity
BScontrol_blameclaim_high <- glht(BStwoway_handling_model, linfct=c("BSresponse_blameclaim +
                                                                    BSresponse_blameclaim:BSseverity_high = 0"))
summary(BScontrol_blameclaim_high)
# control/high severirty vs. blamedeflect/high severity
BScontrol_blamedeflect_high <- glht(BStwoway_handling_model, linfct=c("BSresponse_blamedeflect +
                                                                      BSresponse_blamedeflect:BSseverity_high = 0"))
summary(BScontrol_blamedeflect_high)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(BScontrol_blameclaim_low),
       coef(BScontrol_blameclaim_mod),
       coef(BScontrol_blameclaim_high),
       coef(BScontrol_blamedeflect_low),
       coef(BScontrol_blamedeflect_mod),
       coef(BScontrol_blamedeflect_high)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(BScontrol_blameclaim_low)$confint[2],
         x1=confint(BScontrol_blameclaim_low)$confint[3],
         y0=7)
segments(x0=confint(BScontrol_blameclaim_mod)$confint[2],
         x1=confint(BScontrol_blameclaim_mod)$confint[3],
         y0=6)
segments(x0=confint(BScontrol_blameclaim_high)$confint[2],
         x1=confint(BScontrol_blameclaim_high)$confint[3],
         y0=5)
segments(x0=confint(BScontrol_blamedeflect_low)$confint[2],
         x1=confint(BScontrol_blamedeflect_low)$confint[3],
         y0=3)
segments(x0=confint(BScontrol_blamedeflect_mod)$confint[2],
         x1=confint(BScontrol_blamedeflect_mod)$confint[3],
         y0=2)
segments(x0=confint(BScontrol_blamedeflect_high)$confint[2],
         x1=confint(BScontrol_blamedeflect_high)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Budget Shortfall", side = 3, line=0.5, cex = 2)
text(rev(c(coef(BScontrol_blameclaim_low),
           coef(BScontrol_blameclaim_mod),
           coef(BScontrol_blameclaim_high),
           coef(BScontrol_blamedeflect_low),
           coef(BScontrol_blamedeflect_mod),
           coef(BScontrol_blamedeflect_high))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(BScontrol_blameclaim_low),
                                                          coef(BScontrol_blameclaim_mod),
                                                          coef(BScontrol_blameclaim_high),
                                                          coef(BScontrol_blamedeflect_low),
                                                          coef(BScontrol_blamedeflect_mod),
                                                          coef(BScontrol_blamedeflect_high)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Low Severity", "Moderate Severity",
                            "High Severity"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

# heat wave

# control/low severirty vs. blameclaim/low severity
HWcontrol_blameclaim_low <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blameclaim = 0"))
summary(HWcontrol_blameclaim_low)
# control/low severirty vs. blamedeflect/low severity
HWcontrol_blamedeflect_low <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blamedeflect = 0"))
summary(HWcontrol_blamedeflect_low)
# control/mod severirty vs. blameclaim/mod severity
HWcontrol_blameclaim_mod <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blameclaim + 
                                                                   HWresponse_blameclaim:HWseverity_mod = 0"))
summary(HWcontrol_blameclaim_mod)
# control/mod severirty vs. blamedeflect/mod severity
HWcontrol_blamedeflect_mod <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blamedeflect +
                                                                     HWresponse_blamedeflect:HWseverity_mod = 0"))
summary(HWcontrol_blamedeflect_mod)
# control/high severirty vs. blameclaim/high severity
HWcontrol_blameclaim_high <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blameclaim +
                                                                    HWresponse_blameclaim:HWseverity_high = 0"))
summary(HWcontrol_blameclaim_high)
# control/high severirty vs. blamedeflect/high severity
HWcontrol_blamedeflect_high <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blamedeflect +
                                                                      HWresponse_blamedeflect:HWseverity_high = 0"))
summary(HWcontrol_blamedeflect_high)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(HWcontrol_blameclaim_low),
       coef(HWcontrol_blameclaim_mod),
       coef(HWcontrol_blameclaim_high),
       coef(HWcontrol_blamedeflect_low),
       coef(HWcontrol_blamedeflect_mod),
       coef(HWcontrol_blamedeflect_high)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(HWcontrol_blameclaim_low)$confint[2],
         x1=confint(HWcontrol_blameclaim_low)$confint[3],
         y0=7)
segments(x0=confint(HWcontrol_blameclaim_mod)$confint[2],
         x1=confint(HWcontrol_blameclaim_mod)$confint[3],
         y0=6)
segments(x0=confint(HWcontrol_blameclaim_high)$confint[2],
         x1=confint(HWcontrol_blameclaim_high)$confint[3],
         y0=5)
segments(x0=confint(HWcontrol_blamedeflect_low)$confint[2],
         x1=confint(HWcontrol_blamedeflect_low)$confint[3],
         y0=3)
segments(x0=confint(HWcontrol_blamedeflect_mod)$confint[2],
         x1=confint(HWcontrol_blamedeflect_mod)$confint[3],
         y0=2)
segments(x0=confint(HWcontrol_blamedeflect_high)$confint[2],
         x1=confint(HWcontrol_blamedeflect_high)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Heat Wave", side = 3, line=0.5, cex = 2)
text(rev(c(coef(HWcontrol_blameclaim_low),
           coef(HWcontrol_blameclaim_mod),
           coef(HWcontrol_blameclaim_high),
           coef(HWcontrol_blamedeflect_low),
           coef(HWcontrol_blamedeflect_mod),
           coef(HWcontrol_blamedeflect_high))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(HWcontrol_blameclaim_low),
                                                          coef(HWcontrol_blameclaim_mod),
                                                          coef(HWcontrol_blameclaim_high),
                                                          coef(HWcontrol_blamedeflect_low),
                                                          coef(HWcontrol_blamedeflect_mod),
                                                          coef(HWcontrol_blamedeflect_high)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Low Severity", "Moderate Severity",
                            "High Severity"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()


###############################################################################
# Figure SI.3--Factorial Experiments Handling (Partisanship and Severity)

# bridge collapse

# control/no match vs. blameclaim/no match
BCcontrol_blameclaim_nomatch <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blameclaim = 0"))
summary(BCcontrol_blameclaim_nomatch)
# control/no match vs. blamedeflect/no match
BCcontrol_blamedeflect_nomatch <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blamedeflect = 0"))
summary(BCcontrol_blamedeflect_nomatch)
# control/copart vs. blameclaim/copart
BCcontrol_blameclaim_copart <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blameclaim + 
                                                           BCresponse_blameclaim:BCcopart = 0"))
summary(BCcontrol_blameclaim_copart)
# control/copart vs. blamedeflect/copart
BCcontrol_blamedeflect_copart <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blamedeflect +
                                                             BCresponse_blamedeflect:BCcopart = 0"))
summary(BCcontrol_blamedeflect_copart)
# control/noncopart vs. blameclaim/high noncopart
BCcontrol_blameclaim_noncopart <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blameclaim +
                                                              BCresponse_blameclaim:BCnoncopart = 0"))
summary(BCcontrol_blameclaim_noncopart)
# control/high severirty vs. blamedeflect/high severity
BCcontrol_blamedeflect_noncopart <- glht(BCtwoway_handling_model, linfct=c("BCresponse_blamedeflect +
                                                                BCresponse_blamedeflect:BCnoncopart = 0"))
summary(BCcontrol_blamedeflect_noncopart)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(BCcontrol_blameclaim_nomatch),
       coef(BCcontrol_blameclaim_copart),
       coef(BCcontrol_blameclaim_noncopart),
       coef(BCcontrol_blamedeflect_nomatch),
       coef(BCcontrol_blamedeflect_copart),
       coef(BCcontrol_blamedeflect_noncopart)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(BCcontrol_blameclaim_nomatch)$confint[2],
         x1=confint(BCcontrol_blameclaim_nomatch)$confint[3],
         y0=7)
segments(x0=confint(BCcontrol_blameclaim_copart)$confint[2],
         x1=confint(BCcontrol_blameclaim_copart)$confint[3],
         y0=6)
segments(x0=confint(BCcontrol_blameclaim_noncopart)$confint[2],
         x1=confint(BCcontrol_blameclaim_noncopart)$confint[3],
         y0=5)
segments(x0=confint(BCcontrol_blamedeflect_nomatch)$confint[2],
         x1=confint(BCcontrol_blamedeflect_nomatch)$confint[3],
         y0=3)
segments(x0=confint(BCcontrol_blamedeflect_copart)$confint[2],
         x1=confint(BCcontrol_blamedeflect_copart)$confint[3],
         y0=2)
segments(x0=confint(BCcontrol_blamedeflect_noncopart)$confint[2],
         x1=confint(BCcontrol_blamedeflect_noncopart)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Bridge Collapse", side = 3, line=0.5, cex = 2)
text(rev(c(coef(BCcontrol_blameclaim_nomatch),
           coef(BCcontrol_blameclaim_copart),
           coef(BCcontrol_blameclaim_noncopart),
           coef(BCcontrol_blamedeflect_nomatch),
           coef(BCcontrol_blamedeflect_copart),
           coef(BCcontrol_blamedeflect_noncopart))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(BCcontrol_blameclaim_nomatch),
                                                          coef(BCcontrol_blameclaim_copart),
                                                          coef(BCcontrol_blameclaim_noncopart),
                                                          coef(BCcontrol_blamedeflect_nomatch),
                                                          coef(BCcontrol_blamedeflect_copart),
                                                          coef(BCcontrol_blamedeflect_noncopart)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("No Match", "Copartisan",
                            "Noncopartisan"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

# budget shortfall

# control/no match vs. blameclaim/no match
BScontrol_blameclaim_nomatch <- glht(BStwoway_handling_model, linfct=c("BSresponse_blameclaim = 0"))
summary(BScontrol_blameclaim_nomatch)
# control/no match vs. blamedeflect/no match
BScontrol_blamedeflect_nomatch <- glht(BStwoway_handling_model, linfct=c("BSresponse_blamedeflect = 0"))
summary(BScontrol_blamedeflect_nomatch)
# control/copart vs. blameclaim/copart
BScontrol_blameclaim_copart <- glht(BStwoway_handling_model, linfct=c("BSresponse_blameclaim + 
                                                                      BSresponse_blameclaim:BScopart = 0"))
summary(BScontrol_blameclaim_copart)
# control/copart vs. blamedeflect/copart
BScontrol_blamedeflect_copart <- glht(BStwoway_handling_model, linfct=c("BSresponse_blamedeflect +
                                                                        BSresponse_blamedeflect:BScopart = 0"))
summary(BScontrol_blamedeflect_copart)
# control/noncopart vs. blameclaim/high noncopart
BScontrol_blameclaim_noncopart <- glht(BStwoway_handling_model, linfct=c("BSresponse_blameclaim +
                                                                         BSresponse_blameclaim:BSnoncopart = 0"))
summary(BScontrol_blameclaim_noncopart)
# control/high severirty vs. blamedeflect/high severity
BScontrol_blamedeflect_noncopart <- glht(BStwoway_handling_model, linfct=c("BSresponse_blamedeflect +
                                                                           BSresponse_blamedeflect:BSnoncopart = 0"))
summary(BScontrol_blamedeflect_noncopart)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(BScontrol_blameclaim_nomatch),
       coef(BScontrol_blameclaim_copart),
       coef(BScontrol_blameclaim_noncopart),
       coef(BScontrol_blamedeflect_nomatch),
       coef(BScontrol_blamedeflect_copart),
       coef(BScontrol_blamedeflect_noncopart)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(BScontrol_blameclaim_nomatch)$confint[2],
         x1=confint(BScontrol_blameclaim_nomatch)$confint[3],
         y0=7)
segments(x0=confint(BScontrol_blameclaim_copart)$confint[2],
         x1=confint(BScontrol_blameclaim_copart)$confint[3],
         y0=6)
segments(x0=confint(BScontrol_blameclaim_noncopart)$confint[2],
         x1=confint(BScontrol_blameclaim_noncopart)$confint[3],
         y0=5)
segments(x0=confint(BScontrol_blamedeflect_nomatch)$confint[2],
         x1=confint(BScontrol_blamedeflect_nomatch)$confint[3],
         y0=3)
segments(x0=confint(BScontrol_blamedeflect_copart)$confint[2],
         x1=confint(BScontrol_blamedeflect_copart)$confint[3],
         y0=2)
segments(x0=confint(BScontrol_blamedeflect_noncopart)$confint[2],
         x1=confint(BScontrol_blamedeflect_noncopart)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Budget Shortfall", side = 3, line=0.5, cex = 2)
text(rev(c(coef(BScontrol_blameclaim_nomatch),
           coef(BScontrol_blameclaim_copart),
           coef(BScontrol_blameclaim_noncopart),
           coef(BScontrol_blamedeflect_nomatch),
           coef(BScontrol_blamedeflect_copart),
           coef(BScontrol_blamedeflect_noncopart))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(BScontrol_blameclaim_nomatch),
                                                          coef(BScontrol_blameclaim_copart),
                                                          coef(BScontrol_blameclaim_noncopart),
                                                          coef(BScontrol_blamedeflect_nomatch),
                                                          coef(BScontrol_blamedeflect_copart),
                                                          coef(BScontrol_blamedeflect_noncopart)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("No Match", "Copartisan",
                            "Noncopartisan"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

# heat wave

# control/no match vs. blameclaim/no match
HWcontrol_blameclaim_nomatch <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blameclaim = 0"))
summary(HWcontrol_blameclaim_nomatch)
# control/no match vs. blamedeflect/no match
HWcontrol_blamedeflect_nomatch <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blamedeflect = 0"))
summary(HWcontrol_blamedeflect_nomatch)
# control/copart vs. blameclaim/copart
HWcontrol_blameclaim_copart <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blameclaim + 
                                                                      HWresponse_blameclaim:HWcopart = 0"))
summary(HWcontrol_blameclaim_copart)
# control/copart vs. blamedeflect/copart
HWcontrol_blamedeflect_copart <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blamedeflect +
                                                                        HWresponse_blamedeflect:HWcopart = 0"))
summary(HWcontrol_blamedeflect_copart)
# control/noncopart vs. blameclaim/high noncopart
HWcontrol_blameclaim_noncopart <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blameclaim +
                                                                         HWresponse_blameclaim:HWnoncopart = 0"))
summary(HWcontrol_blameclaim_noncopart)
# control/high severirty vs. blamedeflect/high severity
HWcontrol_blamedeflect_noncopart <- glht(HWtwoway_handling_model, linfct=c("HWresponse_blamedeflect +
                                                                           HWresponse_blamedeflect:HWnoncopart = 0"))
summary(HWcontrol_blamedeflect_noncopart)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(HWcontrol_blameclaim_nomatch),
       coef(HWcontrol_blameclaim_copart),
       coef(HWcontrol_blameclaim_noncopart),
       coef(HWcontrol_blamedeflect_nomatch),
       coef(HWcontrol_blamedeflect_copart),
       coef(HWcontrol_blamedeflect_noncopart)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(HWcontrol_blameclaim_nomatch)$confint[2],
         x1=confint(HWcontrol_blameclaim_nomatch)$confint[3],
         y0=7)
segments(x0=confint(HWcontrol_blameclaim_copart)$confint[2],
         x1=confint(HWcontrol_blameclaim_copart)$confint[3],
         y0=6)
segments(x0=confint(HWcontrol_blameclaim_noncopart)$confint[2],
         x1=confint(HWcontrol_blameclaim_noncopart)$confint[3],
         y0=5)
segments(x0=confint(HWcontrol_blamedeflect_nomatch)$confint[2],
         x1=confint(HWcontrol_blamedeflect_nomatch)$confint[3],
         y0=3)
segments(x0=confint(HWcontrol_blamedeflect_copart)$confint[2],
         x1=confint(HWcontrol_blamedeflect_copart)$confint[3],
         y0=2)
segments(x0=confint(HWcontrol_blamedeflect_noncopart)$confint[2],
         x1=confint(HWcontrol_blamedeflect_noncopart)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Heat Wave", side = 3, line=0.5, cex = 2)
text(rev(c(coef(HWcontrol_blameclaim_nomatch),
           coef(HWcontrol_blameclaim_copart),
           coef(HWcontrol_blameclaim_noncopart),
           coef(HWcontrol_blamedeflect_nomatch),
           coef(HWcontrol_blamedeflect_copart),
           coef(HWcontrol_blamedeflect_noncopart))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(HWcontrol_blameclaim_nomatch),
                                                          coef(HWcontrol_blameclaim_copart),
                                                          coef(HWcontrol_blameclaim_noncopart),
                                                          coef(HWcontrol_blamedeflect_nomatch),
                                                          coef(HWcontrol_blamedeflect_copart),
                                                          coef(HWcontrol_blamedeflect_noncopart)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("No Match", "Copartisan",
                            "Noncopartisan"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

###############################################################################
# Figure SI.4--Factorial Experiments Vote Choice (Response and Severity)

# bridge collapse

# control/low severirty vs. blameclaim/low severity
BCcontrol_blameclaim_low <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blameclaim = 0"))
summary(BCcontrol_blameclaim_low)
# control/low severirty vs. blamedeflect/low severity
BCcontrol_blamedeflect_low <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blamedeflect = 0"))
summary(BCcontrol_blamedeflect_low)
# control/mod severirty vs. blameclaim/mod severity
BCcontrol_blameclaim_mod <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blameclaim + 
                                                                   BCresponse_blameclaim:BCseverity_mod = 0"))
summary(BCcontrol_blameclaim_mod)
# control/mod severirty vs. blamedeflect/mod severity
BCcontrol_blamedeflect_mod <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blamedeflect +
                                                                     BCresponse_blamedeflect:BCseverity_mod = 0"))
summary(BCcontrol_blamedeflect_mod)
# control/high severirty vs. blameclaim/high severity
BCcontrol_blameclaim_high <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blameclaim +
                                                                    BCresponse_blameclaim:BCseverity_high = 0"))
summary(BCcontrol_blameclaim_high)
# control/high severirty vs. blamedeflect/high severity
BCcontrol_blamedeflect_high <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blamedeflect +
                                                                      BCresponse_blamedeflect:BCseverity_high = 0"))
summary(BCcontrol_blamedeflect_high)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(BCcontrol_blameclaim_low),
       coef(BCcontrol_blameclaim_mod),
       coef(BCcontrol_blameclaim_high),
       coef(BCcontrol_blamedeflect_low),
       coef(BCcontrol_blamedeflect_mod),
       coef(BCcontrol_blamedeflect_high)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(BCcontrol_blameclaim_low)$confint[2],
         x1=confint(BCcontrol_blameclaim_low)$confint[3],
         y0=7)
segments(x0=confint(BCcontrol_blameclaim_mod)$confint[2],
         x1=confint(BCcontrol_blameclaim_mod)$confint[3],
         y0=6)
segments(x0=confint(BCcontrol_blameclaim_high)$confint[2],
         x1=confint(BCcontrol_blameclaim_high)$confint[3],
         y0=5)
segments(x0=confint(BCcontrol_blamedeflect_low)$confint[2],
         x1=confint(BCcontrol_blamedeflect_low)$confint[3],
         y0=3)
segments(x0=confint(BCcontrol_blamedeflect_mod)$confint[2],
         x1=confint(BCcontrol_blamedeflect_mod)$confint[3],
         y0=2)
segments(x0=confint(BCcontrol_blamedeflect_high)$confint[2],
         x1=confint(BCcontrol_blamedeflect_high)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Bridge Collapse", side = 3, line=0.5, cex = 2)
text(rev(c(coef(BCcontrol_blameclaim_low),
           coef(BCcontrol_blameclaim_mod),
           coef(BCcontrol_blameclaim_high),
           coef(BCcontrol_blamedeflect_low),
           coef(BCcontrol_blamedeflect_mod),
           coef(BCcontrol_blamedeflect_high))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(BCcontrol_blameclaim_low),
                                                          coef(BCcontrol_blameclaim_mod),
                                                          coef(BCcontrol_blameclaim_high),
                                                          coef(BCcontrol_blamedeflect_low),
                                                          coef(BCcontrol_blamedeflect_mod),
                                                          coef(BCcontrol_blamedeflect_high)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Low Severity", "Moderate Severity",
                            "High Severity"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

# budget shortfall

# control/low severirty vs. blameclaim/low severity
BScontrol_blameclaim_low <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blameclaim = 0"))
summary(BScontrol_blameclaim_low)
# control/low severirty vs. blamedeflect/low severity
BScontrol_blamedeflect_low <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blamedeflect = 0"))
summary(BScontrol_blamedeflect_low)
# control/mod severirty vs. blameclaim/mod severity
BScontrol_blameclaim_mod <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blameclaim + 
                                                                   BSresponse_blameclaim:BSseverity_mod = 0"))
summary(BScontrol_blameclaim_mod)
# control/mod severirty vs. blamedeflect/mod severity
BScontrol_blamedeflect_mod <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blamedeflect +
                                                                     BSresponse_blamedeflect:BSseverity_mod = 0"))
summary(BScontrol_blamedeflect_mod)
# control/high severirty vs. blameclaim/high severity
BScontrol_blameclaim_high <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blameclaim +
                                                                    BSresponse_blameclaim:BSseverity_high = 0"))
summary(BScontrol_blameclaim_high)
# control/high severirty vs. blamedeflect/high severity
BScontrol_blamedeflect_high <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blamedeflect +
                                                                      BSresponse_blamedeflect:BSseverity_high = 0"))
summary(BScontrol_blamedeflect_high)

pdf(file = ,
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(BScontrol_blameclaim_low),
       coef(BScontrol_blameclaim_mod),
       coef(BScontrol_blameclaim_high),
       coef(BScontrol_blamedeflect_low),
       coef(BScontrol_blamedeflect_mod),
       coef(BScontrol_blamedeflect_high)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(BScontrol_blameclaim_low)$confint[2],
         x1=confint(BScontrol_blameclaim_low)$confint[3],
         y0=7)
segments(x0=confint(BScontrol_blameclaim_mod)$confint[2],
         x1=confint(BScontrol_blameclaim_mod)$confint[3],
         y0=6)
segments(x0=confint(BScontrol_blameclaim_high)$confint[2],
         x1=confint(BScontrol_blameclaim_high)$confint[3],
         y0=5)
segments(x0=confint(BScontrol_blamedeflect_low)$confint[2],
         x1=confint(BScontrol_blamedeflect_low)$confint[3],
         y0=3)
segments(x0=confint(BScontrol_blamedeflect_mod)$confint[2],
         x1=confint(BScontrol_blamedeflect_mod)$confint[3],
         y0=2)
segments(x0=confint(BScontrol_blamedeflect_high)$confint[2],
         x1=confint(BScontrol_blamedeflect_high)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Budget Shortfall", side = 3, line=0.5, cex = 2)
text(rev(c(coef(BScontrol_blameclaim_low),
           coef(BScontrol_blameclaim_mod),
           coef(BScontrol_blameclaim_high),
           coef(BScontrol_blamedeflect_low),
           coef(BScontrol_blamedeflect_mod),
           coef(BScontrol_blamedeflect_high))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(BScontrol_blameclaim_low),
                                                          coef(BScontrol_blameclaim_mod),
                                                          coef(BScontrol_blameclaim_high),
                                                          coef(BScontrol_blamedeflect_low),
                                                          coef(BScontrol_blamedeflect_mod),
                                                          coef(BScontrol_blamedeflect_high)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Low Severity", "Moderate Severity",
                            "High Severity"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

# heat wave

# control/low severirty vs. blameclaim/low severity
HWcontrol_blameclaim_low <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blameclaim = 0"))
summary(HWcontrol_blameclaim_low)
# control/low severirty vs. blamedeflect/low severity
HWcontrol_blamedeflect_low <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blamedeflect = 0"))
summary(HWcontrol_blamedeflect_low)
# control/mod severirty vs. blameclaim/mod severity
HWcontrol_blameclaim_mod <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blameclaim + 
                                                                   HWresponse_blameclaim:HWseverity_mod = 0"))
summary(HWcontrol_blameclaim_mod)
# control/mod severirty vs. blamedeflect/mod severity
HWcontrol_blamedeflect_mod <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blamedeflect +
                                                                     HWresponse_blamedeflect:HWseverity_mod = 0"))
summary(HWcontrol_blamedeflect_mod)
# control/high severirty vs. blameclaim/high severity
HWcontrol_blameclaim_high <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blameclaim +
                                                                    HWresponse_blameclaim:HWseverity_high = 0"))
summary(HWcontrol_blameclaim_high)
# control/high severirty vs. blamedeflect/high severity
HWcontrol_blamedeflect_high <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blamedeflect +
                                                                      HWresponse_blamedeflect:HWseverity_high = 0"))
summary(HWcontrol_blamedeflect_high)

pdf(file = ,
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(HWcontrol_blameclaim_low),
       coef(HWcontrol_blameclaim_mod),
       coef(HWcontrol_blameclaim_high),
       coef(HWcontrol_blamedeflect_low),
       coef(HWcontrol_blamedeflect_mod),
       coef(HWcontrol_blamedeflect_high)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(HWcontrol_blameclaim_low)$confint[2],
         x1=confint(HWcontrol_blameclaim_low)$confint[3],
         y0=7)
segments(x0=confint(HWcontrol_blameclaim_mod)$confint[2],
         x1=confint(HWcontrol_blameclaim_mod)$confint[3],
         y0=6)
segments(x0=confint(HWcontrol_blameclaim_high)$confint[2],
         x1=confint(HWcontrol_blameclaim_high)$confint[3],
         y0=5)
segments(x0=confint(HWcontrol_blamedeflect_low)$confint[2],
         x1=confint(HWcontrol_blamedeflect_low)$confint[3],
         y0=3)
segments(x0=confint(HWcontrol_blamedeflect_mod)$confint[2],
         x1=confint(HWcontrol_blamedeflect_mod)$confint[3],
         y0=2)
segments(x0=confint(HWcontrol_blamedeflect_high)$confint[2],
         x1=confint(HWcontrol_blamedeflect_high)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Heat Wave", side = 3, line=0.5, cex = 2)
text(rev(c(coef(HWcontrol_blameclaim_low),
           coef(HWcontrol_blameclaim_mod),
           coef(HWcontrol_blameclaim_high),
           coef(HWcontrol_blamedeflect_low),
           coef(HWcontrol_blamedeflect_mod),
           coef(HWcontrol_blamedeflect_high))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(HWcontrol_blameclaim_low),
                                                          coef(HWcontrol_blameclaim_mod),
                                                          coef(HWcontrol_blameclaim_high),
                                                          coef(HWcontrol_blamedeflect_low),
                                                          coef(HWcontrol_blamedeflect_mod),
                                                          coef(HWcontrol_blamedeflect_high)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Low Severity", "Moderate Severity",
                            "High Severity"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

###############################################################################
# Figure SI.5--Factorial Experiments Vote Choice (Partisanship and Severity)

# bridge collapse

# control/no match vs. blameclaim/no match
BCcontrol_blameclaim_nomatch <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blameclaim = 0"))
summary(BCcontrol_blameclaim_nomatch)
# control/no match vs. blamedeflect/no match
BCcontrol_blamedeflect_nomatch <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blamedeflect = 0"))
summary(BCcontrol_blamedeflect_nomatch)
# control/copart vs. blameclaim/copart
BCcontrol_blameclaim_copart <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blameclaim + 
                                                                      BCresponse_blameclaim:BCcopart = 0"))
summary(BCcontrol_blameclaim_copart)
# control/copart vs. blamedeflect/copart
BCcontrol_blamedeflect_copart <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blamedeflect +
                                                                        BCresponse_blamedeflect:BCcopart = 0"))
summary(BCcontrol_blamedeflect_copart)
# control/noncopart vs. blameclaim/high noncopart
BCcontrol_blameclaim_noncopart <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blameclaim +
                                                                         BCresponse_blameclaim:BCnoncopart = 0"))
summary(BCcontrol_blameclaim_noncopart)
# control/high severirty vs. blamedeflect/high severity
BCcontrol_blamedeflect_noncopart <- glht(BCtwoway_vote_next_model, linfct=c("BCresponse_blamedeflect +
                                                                           BCresponse_blamedeflect:BCnoncopart = 0"))
summary(BCcontrol_blamedeflect_noncopart)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(BCcontrol_blameclaim_nomatch),
       coef(BCcontrol_blameclaim_copart),
       coef(BCcontrol_blameclaim_noncopart),
       coef(BCcontrol_blamedeflect_nomatch),
       coef(BCcontrol_blamedeflect_copart),
       coef(BCcontrol_blamedeflect_noncopart)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(BCcontrol_blameclaim_nomatch)$confint[2],
         x1=confint(BCcontrol_blameclaim_nomatch)$confint[3],
         y0=7)
segments(x0=confint(BCcontrol_blameclaim_copart)$confint[2],
         x1=confint(BCcontrol_blameclaim_copart)$confint[3],
         y0=6)
segments(x0=confint(BCcontrol_blameclaim_noncopart)$confint[2],
         x1=confint(BCcontrol_blameclaim_noncopart)$confint[3],
         y0=5)
segments(x0=confint(BCcontrol_blamedeflect_nomatch)$confint[2],
         x1=confint(BCcontrol_blamedeflect_nomatch)$confint[3],
         y0=3)
segments(x0=confint(BCcontrol_blamedeflect_copart)$confint[2],
         x1=confint(BCcontrol_blamedeflect_copart)$confint[3],
         y0=2)
segments(x0=confint(BCcontrol_blamedeflect_noncopart)$confint[2],
         x1=confint(BCcontrol_blamedeflect_noncopart)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Bridge Collapse", side = 3, line=0.5, cex = 2)
text(rev(c(coef(BCcontrol_blameclaim_nomatch),
           coef(BCcontrol_blameclaim_copart),
           coef(BCcontrol_blameclaim_noncopart),
           coef(BCcontrol_blamedeflect_nomatch),
           coef(BCcontrol_blamedeflect_copart),
           coef(BCcontrol_blamedeflect_noncopart))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(BCcontrol_blameclaim_nomatch),
                                                          coef(BCcontrol_blameclaim_copart),
                                                          coef(BCcontrol_blameclaim_noncopart),
                                                          coef(BCcontrol_blamedeflect_nomatch),
                                                          coef(BCcontrol_blamedeflect_copart),
                                                          coef(BCcontrol_blamedeflect_noncopart)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("No Match", "Copartisan",
                            "Noncopartisan"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

# budget shortfall

# control/no match vs. blameclaim/no match
BScontrol_blameclaim_nomatch <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blameclaim = 0"))
summary(BScontrol_blameclaim_nomatch)
# control/no match vs. blamedeflect/no match
BScontrol_blamedeflect_nomatch <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blamedeflect = 0"))
summary(BScontrol_blamedeflect_nomatch)
# control/copart vs. blameclaim/copart
BScontrol_blameclaim_copart <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blameclaim + 
                                                                      BSresponse_blameclaim:BScopart = 0"))
summary(BScontrol_blameclaim_copart)
# control/copart vs. blamedeflect/copart
BScontrol_blamedeflect_copart <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blamedeflect +
                                                                        BSresponse_blamedeflect:BScopart = 0"))
summary(BScontrol_blamedeflect_copart)
# control/noncopart vs. blameclaim/high noncopart
BScontrol_blameclaim_noncopart <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blameclaim +
                                                                         BSresponse_blameclaim:BSnoncopart = 0"))
summary(BScontrol_blameclaim_noncopart)
# control/high severirty vs. blamedeflect/high severity
BScontrol_blamedeflect_noncopart <- glht(BStwoway_vote_next_model, linfct=c("BSresponse_blamedeflect +
                                                                           BSresponse_blamedeflect:BSnoncopart = 0"))
summary(BScontrol_blamedeflect_noncopart)

pdf(file = ,
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(BScontrol_blameclaim_nomatch),
       coef(BScontrol_blameclaim_copart),
       coef(BScontrol_blameclaim_noncopart),
       coef(BScontrol_blamedeflect_nomatch),
       coef(BScontrol_blamedeflect_copart),
       coef(BScontrol_blamedeflect_noncopart)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(BScontrol_blameclaim_nomatch)$confint[2],
         x1=confint(BScontrol_blameclaim_nomatch)$confint[3],
         y0=7)
segments(x0=confint(BScontrol_blameclaim_copart)$confint[2],
         x1=confint(BScontrol_blameclaim_copart)$confint[3],
         y0=6)
segments(x0=confint(BScontrol_blameclaim_noncopart)$confint[2],
         x1=confint(BScontrol_blameclaim_noncopart)$confint[3],
         y0=5)
segments(x0=confint(BScontrol_blamedeflect_nomatch)$confint[2],
         x1=confint(BScontrol_blamedeflect_nomatch)$confint[3],
         y0=3)
segments(x0=confint(BScontrol_blamedeflect_copart)$confint[2],
         x1=confint(BScontrol_blamedeflect_copart)$confint[3],
         y0=2)
segments(x0=confint(BScontrol_blamedeflect_noncopart)$confint[2],
         x1=confint(BScontrol_blamedeflect_noncopart)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Budget Shortfall", side = 3, line=0.5, cex = 2)
text(rev(c(coef(BScontrol_blameclaim_nomatch),
           coef(BScontrol_blameclaim_copart),
           coef(BScontrol_blameclaim_noncopart),
           coef(BScontrol_blamedeflect_nomatch),
           coef(BScontrol_blamedeflect_copart),
           coef(BScontrol_blamedeflect_noncopart))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(BScontrol_blameclaim_nomatch),
                                                          coef(BScontrol_blameclaim_copart),
                                                          coef(BScontrol_blameclaim_noncopart),
                                                          coef(BScontrol_blamedeflect_nomatch),
                                                          coef(BScontrol_blamedeflect_copart),
                                                          coef(BScontrol_blamedeflect_noncopart)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("No Match", "Copartisan",
                            "Noncopartisan"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

# heat wave

# control/no match vs. blameclaim/no match
HWcontrol_blameclaim_nomatch <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blameclaim = 0"))
summary(HWcontrol_blameclaim_nomatch)
# control/no match vs. blamedeflect/no match
HWcontrol_blamedeflect_nomatch <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blamedeflect = 0"))
summary(HWcontrol_blamedeflect_nomatch)
# control/copart vs. blameclaim/copart
HWcontrol_blameclaim_copart <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blameclaim + 
                                                                      HWresponse_blameclaim:HWcopart = 0"))
summary(HWcontrol_blameclaim_copart)
# control/copart vs. blamedeflect/copart
HWcontrol_blamedeflect_copart <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blamedeflect +
                                                                        HWresponse_blamedeflect:HWcopart = 0"))
summary(HWcontrol_blamedeflect_copart)
# control/noncopart vs. blameclaim/high noncopart
HWcontrol_blameclaim_noncopart <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blameclaim +
                                                                         HWresponse_blameclaim:HWnoncopart = 0"))
summary(HWcontrol_blameclaim_noncopart)
# control/high severirty vs. blamedeflect/high severity
HWcontrol_blamedeflect_noncopart <- glht(HWtwoway_vote_next_model, linfct=c("HWresponse_blamedeflect +
                                                                           HWresponse_blamedeflect:HWnoncopart = 0"))
summary(HWcontrol_blamedeflect_noncopart)

pdf(file = , 
    family = "Times")
layout(matrix(c(1,2), nrow=2, byrow=TRUE), heights = c(0.85,0.15))
par(mar=c(5.1,6.25,3,.25))
plot(c(coef(HWcontrol_blameclaim_nomatch),
       coef(HWcontrol_blameclaim_copart),
       coef(HWcontrol_blameclaim_noncopart),
       coef(HWcontrol_blamedeflect_nomatch),
       coef(HWcontrol_blamedeflect_copart),
       coef(HWcontrol_blamedeflect_noncopart)),
     c(7,6,5,3,2,1),
     xlim=c(-0.55,0.70),
     xaxt="n",
     ylim=c(0, 8),
     pch=c(15,17,19,15,17,19),
     tck=-.02,
     cex.axis=0.9,
     cex=2,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
segments(x0=confint(HWcontrol_blameclaim_nomatch)$confint[2],
         x1=confint(HWcontrol_blameclaim_nomatch)$confint[3],
         y0=7)
segments(x0=confint(HWcontrol_blameclaim_copart)$confint[2],
         x1=confint(HWcontrol_blameclaim_copart)$confint[3],
         y0=6)
segments(x0=confint(HWcontrol_blameclaim_noncopart)$confint[2],
         x1=confint(HWcontrol_blameclaim_noncopart)$confint[3],
         y0=5)
segments(x0=confint(HWcontrol_blamedeflect_nomatch)$confint[2],
         x1=confint(HWcontrol_blamedeflect_nomatch)$confint[3],
         y0=3)
segments(x0=confint(HWcontrol_blamedeflect_copart)$confint[2],
         x1=confint(HWcontrol_blamedeflect_copart)$confint[3],
         y0=2)
segments(x0=confint(HWcontrol_blamedeflect_noncopart)$confint[2],
         x1=confint(HWcontrol_blamedeflect_noncopart)$confint[3],
         y0=1)
axis(1,at=seq(-0.5,0.7,0.1),
     labels=c(NA, "-40%", NA, "-20%", NA, "0%",
              NA, "20%", NA, "40%", NA, "60%", NA),
     cex.axis = 1.75)
axis(1,at=-.2,
     labels="-20%",
     cex.axis = 1.75)
par(mgp=c(0,-1.5,0), xpd=NA)
axis(2,at=c(6,2),
     las=2,
     labels=c("Blame\n Claim", "Blame\n Deflect"),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75)
mtext("Difference from Control", side = 1, line = 3, cex = 2)
mtext("Heat Wave", side = 3, line=0.5, cex = 2)
text(rev(c(coef(HWcontrol_blameclaim_nomatch),
           coef(HWcontrol_blameclaim_copart),
           coef(HWcontrol_blameclaim_noncopart),
           coef(HWcontrol_blamedeflect_nomatch),
           coef(HWcontrol_blamedeflect_copart),
           coef(HWcontrol_blamedeflect_noncopart))),
     c(1,2,3,5,6,7)+0.5,paste0(sprintf("%.0f",round(rev(c(coef(HWcontrol_blameclaim_nomatch),
                                                          coef(HWcontrol_blameclaim_copart),
                                                          coef(HWcontrol_blameclaim_noncopart),
                                                          coef(HWcontrol_blamedeflect_nomatch),
                                                          coef(HWcontrol_blamedeflect_copart),
                                                          coef(HWcontrol_blamedeflect_noncopart)))*100, 0)),"%"), cex=1.5)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("No Match", "Copartisan",
                            "Noncopartisan"), lty=1, lwd=1, 
       pch = c(15, 17, 19), cex = 1.25)
dev.off()

###############################################################################
# Figure SI.6 Flint Study Outcome Distributions

pdf(file = , 
    family = "Times", height = 5, width = 10)
layout(matrix(c(1,2,3,3), nrow=2, byrow=TRUE), 
       heights = c(0.8,0.2))
par(mar=c(5.1,4.1,4.1,2.1))
barplot(table(MTurk_flint_data$treatment, MTurk_flint_data$handling_ord), 
        main="Approval", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,200),
        col = c("gray80", "gray60", "gray40", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Very\nnegative", "Somewhat\nnegative", "Somewhat\npositive", "Very\npositive"),
      side=1, at=c(3,8,13,18), line=2, cex=0.8)
text(c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5),
     table(MTurk_flint_data$treatment, MTurk_flint_data$handling_ord) + 10, 
     table(MTurk_flint_data$treatment, MTurk_flint_data$handling_ord), cex = 1.25)
barplot(table(MTurk_flint_data$treatment, MTurk_flint_data$office_ord), 
        main="Remain in Office", font.main = 1, cex.main = 2,
        xlab = NULL,
        ylab = "Num. Respondents",
        ylim = c(0,200),
        col = c("gray80", "gray60", "gray40", "gray20"),
        beside = TRUE, cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5,
        axisnames = FALSE)
mtext(text = c("Resign from\noffice", "Remain in\noffice", "Not sure"),
      side=1, at=c(3,8,13), line=2, cex=0.8)
text(c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5),
     table(MTurk_flint_data$treatment, MTurk_flint_data$office_ord) + 10, 
     table(MTurk_flint_data$treatment, MTurk_flint_data$office_ord), cex = 1.25)
par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend(x="center", legend=c("Control", "Blame Claim",
                            "Blame Appointee", "Blame Expert"),
       pch = 22,
       bty = "n",
       pt.bg = c("gray80", "gray60", "gray40", "gray20"),
       cex = 1, pt.cex = 2)
dev.off()

###############################################################################
# Table SI.15 Flint Study Models (Binary OLS)

flint_handling_bi_model <- lm(handling_bin ~ treatment, data = MTurk_flint_data)
summary(flint_handling_bi_model)

flint_stay_model <- lm(stay ~ treatment, data = MTurk_flint_data)
summary(flint_stay_model)

texreg(l=list(flint_handling_bi_model,
              flint_stay_model),
       stars=c(0.05), 
       custom.model.names = c("Approval", "Remain"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "treatmentself"="Blame Claim",
                              "treatmentappointee"="Blame Appointee",
                              "treatmentexpert"="Blame Expert"),
       label = "table:flintmainmodels",
       caption.above = TRUE, caption = "Flint Study Models\\textemdash Approval and Vote (OLS with Binary Outcome)",
       custom.note = "%stars.  This table presents the linear regression models
       we used to construct the plots for the results of our Flint study displayed in the main analysis
       of the paper.  Our outcome variables, approval of Governor Rick Snyder's handling of the Flint
       water crisis and whether the respondent thinks the governor should remain in office 
       (as opposed to resign), are coded as
       dichotomous variables, and our covariates are dichotomous indicators of the respondents'
       treatment conditions.
       The control condition is the baseline condition.",
       include.rmse=FALSE,
       include.adjrs=FALSE)

###############################################################################
# Table SI.16 Flint Study Models (Logistic Regression)

flint_handling_bi_model <- glm(handling_bin ~ treatment, family=binomial(link="logit"),
                               data = MTurk_flint_data)
summary(flint_handling_bi_model)

flint_stay_model <- glm(stay ~ treatment, family=binomial(link="logit"),
                        data = MTurk_flint_data)
summary(flint_stay_model)

texreg(l=list(flint_handling_bi_model,
              flint_stay_model),
       stars=c(0.05), 
       custom.model.names = c("Approval", "Remain"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "treatmentself"="Blame Claim",
                              "treatmentappointee"="Blame Appointee",
                              "treatmentexpert"="Blame Expert"),
       label = "table:flintlogitmodels",
       caption.above = TRUE, caption = "Flint Study Models\\textemdash Approval and Vote (Logistic Regression)",
       custom.note = "%stars.  This table presents the logistic regression models
       that are analogous to the linear regression models we used to estimate the results
       presented in the main analysis of the paper.  Our outcome variables, 
       approval of Governor Rick Snyder's handling of the Flint
       water crisis and whether the respondent thinks the governor should remain in office
       (as opposed to resign), are coded as
       dichotomous variables, and our covariates are dichotomous indicators of the respondents'
       treatment conditions. The control condition is the baseline condition.", 
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE)

###############################################################################
# Table SI.17 Flint Study Handling (Ordinal OLS)

flint_handling_model <- lm(handling_4pt ~ treatment, data = MTurk_flint_data)
summary(flint_handling_model)

texreg(l=list(flint_handling_model),
       stars=c(0.05), 
       custom.model.names = c("Approval"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "treatmentself"="Blame Claim",
                              "treatmentappointee"="Blame Appointee",
                              "treatmentexpert"="Blame Expert"),
       label = "table:flint4pointapprovalmodel",
       caption.above = TRUE, caption = "Flint Study Models\\textemdash Approval (OLS with Ordinal Outcome)",
       custom.note = "%stars.  This table presents a linear regression model that uses a four point
       scale of respondents' approval of Governor Rick Snyder as the outcome variable rather than the
       dichotomous measure of approval used in the main analysis presented in the paper.
       Our covariates are dichotomous indicators of the respondents'
       treatment conditions.
       The control condition is the baseline condition.",
       include.rmse=FALSE,
       include.adjrs=FALSE)

###############################################################################
# Table SI.18 Flint Study Handling (Ordinal Logistic Regression)

flint_handling_polr_model <- polr(handling_ord ~ treatment, data = MTurk_flint_data, Hess = TRUE)
summary(flint_handling_polr_model)

texreg(l=list(flint_handling_polr_model),
       stars=c(0.05), 
       custom.model.names = c("Approval"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "treatmentself"="Blame Claim",
                              "treatmentappointee"="Blame Appointee",
                              "treatmentexpert"="Blame Expert",
                              "Very negative|Somewhat negative"="Very negative|Somewhat negative",
                              "Somewhat negative|Somewhat positive"="Somewhat negative|Somewhat positive",
                              "Somewhat positive|Very positive"="Somewhat positive|Very positive"),
       label = "table:flintapprovalpolrmodel",
       caption.above = TRUE, caption = "Flint Study Models\\textemdash Approval (Ordinal Logistic Regression)",
       custom.note = "%stars.  This table presents an ordinal logistic regression model of approval
       for Governor Snyder's handling of the Flint water crisis
       that is analogous to the linear regression model we used to estimate the results
       presented in the main analysis of the paper.   
       Our covariates are dichotomous indicators of the respondents'
       treatment conditions.  The control condition is the baseline condition.", 
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE, 
       include.thresholds=TRUE)

###############################################################################
# Table SI.19 Flint Study Vote (Multinomial Logistic Regression)

MTurk_flint_data$office_factor <- relevel(MTurk_flint_data$office_ord, ref="Resign from office")

flint_resign_model <- multinom(office_factor ~ treatment, data = MTurk_flint_data, Hess = TRUE)
summary(flint_resign_model)

texreg(l=list(flint_resign_model),
       stars=c(0.05), 
       custom.model.names = c("Remain in office", "Not sure"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "treatmentself"="Blame Claim",
                              "treatmentappointee"="Blame Appointee",
                              "treatmentexpert"="Blame Expert"),
       label = "table:flintvotemnlmodel",
       caption.above = TRUE, caption = "Flint Study Models\\textemdash Vote (Multinomial Logistic Regression)",
       custom.note = "%stars.  This table presents a multinomial logistic regression model of whether
       respondents think that Governor Snyder should remain in office (as opposed to resign)
       that is analogous to the linear regression model we used to estimate the results
       presented in the main analysis of the paper.  Our outcome variable is trichotomous, with
       respondents indicating that the governor should resign from office (the baseline outcome),
       remain in office, or that they are not sure what the governor should do.
       Our covariates are dichotomous indicators of the respondents'
       treatment conditions.  The control condition is the baseline condition.", 
       include.aic=FALSE, include.bic=FALSE, include.deviance=FALSE, 
       include.thresholds=TRUE, beside = TRUE)

###############################################################################
# Tables SI.20, 21, and 22 (Leadership Valance Mediation Analysis Components)

med.fit.flood <- lm(traits_scale ~ Floodresponse, data = TAPSdata,
                    weights = jan2018wt1)
out.fit.handling.flood <- lm(handling_bin ~ Floodresponse + traits_scale, 
                             data = TAPSdata, weights = jan2018wt1)
out.fit.vote_next.flood <- lm(vote_bin ~ Floodresponse + traits_scale, 
                              data = TAPSdata, weights = jan2018wt1)
med.fit.BC <- lm(BCtraits ~ BCresponse, data = MTurk_stylized_data)
out.fit.handling.BC <- lm(BCgovapprove_bin ~ BCresponse + BCtraits, 
                          data = MTurk_stylized_data)
out.fit.vote_next.BC <- lm(BCgovvote_bin ~ BCresponse + BCtraits, 
                           data = MTurk_stylized_data)
med.fit.BS <- lm(BStraits ~ BSresponse, data = MTurk_stylized_data)
out.fit.handling.BS <- lm(BSgovapprove_bin ~ BSresponse + BStraits, 
                          data = MTurk_stylized_data)
out.fit.vote_next.BS <- lm(BSgovvote_bin ~ BSresponse + BStraits, 
                           data = MTurk_stylized_data)
med.fit.HW <- lm(HWtraits ~ HWresponse, data = MTurk_stylized_data)
out.fit.handling.HW <- lm(HWgovapprove_bin ~ HWresponse + HWtraits, 
                          data = MTurk_stylized_data)
out.fit.vote_next.HW <- lm(HWgovvote_bin ~ HWresponse + HWtraits, 
                           data = MTurk_stylized_data)

# Table SI.20 (Effect of Treatments on Traits)

texreg(l=list(med.fit.flood, med.fit.BC, med.fit.BS, med.fit.HW), stars = c(0.05),
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect"),
       label = "table:traitsols",
       caption.above = TRUE, 
       caption = "Causal Mediation Regressions (Traits on Treatment)",
       custom.note = "%stars.  This table presents linear regression models of respondents' perceptions
       of the executives' traits regressed on their treatment conditions, which were
       then used in the causal mediation analysis presented in the paper.  The model
       in the first column uses an additive scale of respondents' perceptions of the 
       executive's character traits (scaled from 0 to 5, where a value of 1 is added to the 
       scale for every trait the respondent evaluates as describing the executive at 
       least moderately well; Cronbach's $\\alpha$ for this scale
       is 0.93) as the dependent variable, and dichotomous indicators of treatment assignment 
       as the  covariates (with the control condition as the baseline condition). The 
       models in the other columns use an average of the respondents' perceptions of
       the executive's character traits (each trait is scaled from 1 to 5, as is the
       averaged scale; Cronbach's $\\alpha$ exceeds 0.90 for each of these scales) as the
       dependent variable, and dichotomous indicators of treatment assignment as
       the covariates (with the control condition as the baseline condition). The model
       in the first column includes survey weights; results remain substantively 
       unchanged when weights are not included.",
       include.rmse=FALSE, include.adjrs=FALSE)

# Table SI.21 (Effect of Treatments and Traits on Handling)

texreg(l=list(out.fit.handling.flood, out.fit.handling.BC, out.fit.handling.BS, out.fit.handling.HW)
       , stars = c(0.05),
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "traits_scale"="Character Valence",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BCtraits"="Character Valence",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "BStraits"="Character Valence",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect",
                              "HWtraits"="Character Valence"),
       label = "table:approvalmediateols",
       caption.above = TRUE, 
       caption = "Causal Mediation Regressions (Approval on Treatment and Traits)",
       custom.note = "%stars.  This table presents linear regression models of respondents' 
       approval of the executive's handling of the government crisis regressed on
       their treatment conditions, which were
       then used in the causal mediation analysis presented in the paper.  All models use a dichomotous
       indicator of approval, coded as 1 if the respondent strongly approves or approves of the 
       executive's handling of the governmental crisis, and coded as 0 otherwise.  Covariates include
       dichotomous indicators of treatment assignment and scales of respondents' perceptions of the
       executive's traits; see the notes in
       Table \\ref{table:traitsols} for details on the coding of character valence.
       The model in the first column includes survey weights; results remain substantively 
       unchanged when weights are not included.",
       include.rmse=FALSE, include.adjrs=FALSE)

# Table SI.22 (Effect of Treatments and Traits on Vote Choice)

texreg(l=list(out.fit.vote_next.flood, out.fit.vote_next.BC, out.fit.vote_next.BS, out.fit.vote_next.HW)
       , stars = c(0.05),
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "traits_scale"="Character Valence",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BCtraits"="Character Valence",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "BStraits"="Character Valence",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect",
                              "HWtraits"="Character Valence"),
       label = "table:votemediateols",
       caption.above = TRUE, 
       caption = "Causal Mediation Regressions (Vote on Treatment and Traits)",
       custom.note = "%stars.  This table presents linear regression models of respondents' 
       likelihood of voting for the executive in the next election regressed on
       their treatment conditions, which were
       then used in the causal mediation analysis presented in the paper.  All models use a dichomotous
       indicator of likelihood of voting for the executive in the next election, coded as 1 if the 
       respondent is very likely or somewhat likely to vote for the executive, and coded as 0 
       otherwise.  Covariates include
       dichotomous indicators of treatment assignment and scales of respondents' perceptions of the
       executive's traits; see the notes in
       Table \\ref{table:traitsols} for details on the coding of character valence.
       The model in the first column includes survey weights; results remain substantively 
       unchanged when weights are not included.",
       include.rmse=FALSE, include.adjrs=FALSE)

###############################################################################
# Table SI.23 Causal Mediation Analysis (Leadership Valence and Approval)

# flood

med.out.handling.flood <- mediate(med.fit.flood, out.fit.handling.flood, treat="Floodresponse", mediator="traits_scale", 
                                              robustSE = FALSE, sims=1000,  control.value = "control", 
                                              treat.value = "blame claim", boot=TRUE, dropobs = TRUE)

summary(med.out.handling.flood)

avg_ACME_handling_flood <- med.out.handling.flood$d.avg
avg_ACME_ci_handling_flood <- med.out.handling.flood$d.avg.ci
avg_ACME_p_handling_flood <- med.out.handling.flood$d.avg.p

avg_ADE_handling_flood <- med.out.handling.flood$z.avg
avg_ADE_ci_handling_flood <- med.out.handling.flood$z.avg.ci
avg_ADE_p_handling_flood <- med.out.handling.flood$z.avg.p

totaleff_handling_flood <- med.out.handling.flood$tau.coef
totaleff_ci_handling_flood <- med.out.handling.flood$tau.ci
totaleff_p_handling_flood <- med.out.handling.flood$tau.p

avg_propmed_handling_flood <- med.out.handling.flood$n.avg
avg_propmed_ci_handling_flood <- med.out.handling.flood$n.avg.ci
avg_propmed_p_handling_flood <- med.out.handling.flood$n.avg.p

mediate_handling_flood <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                     "point_est"=c(sprintf("%.2f",round(med.out.handling.flood$d.avg, 2)), 
                                                   sprintf("%.2f",round(med.out.handling.flood$z.avg, 2)),
                                                   sprintf("%.2f",round(med.out.handling.flood$tau.coef, 2)), 
                                                   sprintf("%.2f",round(med.out.handling.flood$n.avg, 2)),
                                                   sprintf("%.0f",round(med.out.handling.flood$nobs, 0))),
                                     "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.handling.flood$d.avg.ci[1],2)),", ",
                                                         sprintf("%.2f",round(med.out.handling.flood$d.avg.ci[2],2)),"]"),
                                                  paste0("[",sprintf("%.2f",round(med.out.handling.flood$z.avg.ci[1],2)),", ",
                                                         sprintf("%.2f",round(med.out.handling.flood$z.avg.ci[2],2)),"]"),
                                                  paste0("[",sprintf("%.2f",round(med.out.handling.flood$tau.ci[1],2)),", ",
                                                         sprintf("%.2f",round(med.out.handling.flood$tau.ci[2],2)),"]"),
                                                  paste0("[",sprintf("%.2f",round(med.out.handling.flood$n.avg.ci[1],2)),", ",
                                                         sprintf("%.2f",round(med.out.handling.flood$n.avg.ci[2],2)),"]"), ""))

# bridge collapse

med.out.handling.BC <- mediate(med.fit.BC, out.fit.handling.BC, treat="BCresponse", mediator="BCtraits", 
                               robustSE = FALSE, sims=1000,  control.value = "control", 
                               treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out.handling.BC)

avg_ACME_handling_BC <- med.out.handling.BC$d.avg
avg_ACME_ci_handling_BC <- med.out.handling.BC$d.avg.ci
avg_ACME_p_handling_BC <- med.out.handling.BC$d.avg.p

avg_ADE_handling_BC <- med.out.handling.BC$z.avg
avg_ADE_ci_handling_BC <- med.out.handling.BC$z.avg.ci
avg_ADE_p_handling_BC <- med.out.handling.BC$z.avg.p

totaleff_handling_BC <- med.out.handling.BC$tau.coef
totaleff_ci_handling_BC <- med.out.handling.BC$tau.ci
totaleff_p_handling_BC <- med.out.handling.BC$tau.p

avg_propmed_handling_BC <- med.out.handling.BC$n.avg
avg_propmed_ci_handling_BC <- med.out.handling.BC$n.avg.ci
avg_propmed_p_handling_BC <- med.out.handling.BC$n.avg.p

mediate_handling_BC <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                  "point_est"=c(sprintf("%.2f",round(med.out.handling.BC$d.avg, 2)), 
                                                sprintf("%.2f",round(med.out.handling.BC$z.avg, 2)),
                                                sprintf("%.2f",round(med.out.handling.BC$tau.coef, 2)), 
                                                sprintf("%.2f",round(med.out.handling.BC$n.avg, 2)),
                                                sprintf("%.0f",round(med.out.handling.BC$nobs, 0))),
                                  "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.handling.BC$d.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BC$d.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BC$z.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BC$z.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BC$tau.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BC$tau.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BC$n.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BC$n.avg.ci[2],2)),"]"), ""))


# budget shortfall

med.out.handling.BS <- mediate(med.fit.BS, out.fit.handling.BS, treat="BSresponse", mediator="BStraits", 
                               robustSE = FALSE, sims=1000,  control.value = "control", 
                               treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out.handling.BS)

avg_ACME_handling_BS <- med.out.handling.BS$d.avg
avg_ACME_ci_handling_BS <- med.out.handling.BS$d.avg.ci
avg_ACME_p_handling_BS <- med.out.handling.BS$d.avg.p

avg_ADE_handling_BS <- med.out.handling.BS$z.avg
avg_ADE_ci_handling_BS <- med.out.handling.BS$z.avg.ci
avg_ADE_p_handling_BS <- med.out.handling.BS$z.avg.p

totaleff_handling_BS <- med.out.handling.BS$tau.coef
totaleff_ci_handling_BS <- med.out.handling.BS$tau.ci
totaleff_p_handling_BS <- med.out.handling.BS$tau.p

avg_propmed_handling_BS <- med.out.handling.BS$n.avg
avg_propmed_ci_handling_BS <- med.out.handling.BS$n.avg.ci
avg_propmed_p_handling_BS <- med.out.handling.BS$n.avg.p

mediate_handling_BS <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                  "point_est"=c(sprintf("%.2f",round(med.out.handling.BS$d.avg, 2)), 
                                                sprintf("%.2f",round(med.out.handling.BS$z.avg, 2)),
                                                sprintf("%.2f",round(med.out.handling.BS$tau.coef, 2)), 
                                                sprintf("%.2f",round(med.out.handling.BS$n.avg, 2)),
                                                sprintf("%.0f",round(med.out.handling.BS$nobs, 0))),
                                  "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.handling.BS$d.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BS$d.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BS$z.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BS$z.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BS$tau.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BS$tau.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BS$n.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BS$n.avg.ci[2],2)),"]"), ""))


# heat wave
med.out.handling.HW <- mediate(med.fit.HW, out.fit.handling.HW, treat="HWresponse", mediator="HWtraits", 
                               robustSE = FALSE, sims=1000,  control.value = "control", 
                               treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out.handling.HW)

avg_ACME_handling_HW <- med.out.handling.HW$d.avg
avg_ACME_ci_handling_HW <- med.out.handling.HW$d.avg.ci
avg_ACME_p_handling_HW <- med.out.handling.HW$d.avg.p

avg_ADE_handling_HW <- med.out.handling.HW$z.avg
avg_ADE_ci_handling_HW <- med.out.handling.HW$z.avg.ci
avg_ADE_p_handling_HW <- med.out.handling.HW$z.avg.p

totaleff_handling_HW <- med.out.handling.HW$tau.coef
totaleff_ci_handling_HW <- med.out.handling.HW$tau.ci
totaleff_p_handling_HW <- med.out.handling.HW$tau.p

avg_propmed_handling_HW <- med.out.handling.HW$n.avg
avg_propmed_ci_handling_HW <- med.out.handling.HW$n.avg.ci
avg_propmed_p_handling_HW <- med.out.handling.HW$n.avg.p

mediate_handling_HW <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                  "point_est"=c(sprintf("%.2f",round(med.out.handling.HW$d.avg, 2)), 
                                                sprintf("%.2f",round(med.out.handling.HW$z.avg, 2)),
                                                sprintf("%.2f",round(med.out.handling.HW$tau.coef, 2)), 
                                                sprintf("%.2f",round(med.out.handling.HW$n.avg, 2)),
                                                sprintf("%.0f",round(med.out.handling.HW$nobs, 0))),
                                  "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.handling.HW$d.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.HW$d.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.HW$z.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.HW$z.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.HW$tau.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.HW$tau.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.HW$n.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.HW$n.avg.ci[2],2)),"]"), ""))


###############################################################################
# Table SI.24 Causal Mediation Analysis (Leadership Valence and Vote Choice)

# flood
med.out.vote_next.flood <- mediate(med.fit.flood, out.fit.vote_next.flood, treat="Floodresponse", mediator="traits_scale", 
                                   robustSE = FALSE, sims=1000,  control.value = "control", 
                                   treat.value = "blame claim", boot=TRUE, dropobs = TRUE)

summary(med.out.vote_next.flood)

avg_ACME_vote_next_flood <- med.out.vote_next.flood$d.avg
avg_ACME_ci_vote_next_flood <- med.out.vote_next.flood$d.avg.ci
avg_ACME_p_vote_next_flood <- med.out.vote_next.flood$d.avg.p

avg_ADE_vote_next_flood <- med.out.vote_next.flood$z.avg
avg_ADE_ci_vote_next_flood <- med.out.vote_next.flood$z.avg.ci
avg_ADE_p_vote_next_flood <- med.out.vote_next.flood$z.avg.p

totaleff_vote_next_flood <- med.out.vote_next.flood$tau.coef
totaleff_ci_vote_next_flood <- med.out.vote_next.flood$tau.ci
totaleff_p_vote_next_flood <- med.out.vote_next.flood$tau.p

avg_propmed_vote_next_flood <- med.out.vote_next.flood$n.avg
avg_propmed_ci_vote_next_flood <- med.out.vote_next.flood$n.avg.ci
avg_propmed_p_vote_next_flood <- med.out.vote_next.flood$n.avg.p

mediate_vote_next_flood <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                      "point_est"=c(sprintf("%.2f",round(med.out.vote_next.flood$d.avg, 2)), 
                                                    sprintf("%.2f",round(med.out.vote_next.flood$z.avg, 2)),
                                                    sprintf("%.2f",round(med.out.vote_next.flood$tau.coef, 2)), 
                                                    sprintf("%.2f",round(med.out.vote_next.flood$n.avg, 2)),
                                                    sprintf("%.0f",round(med.out.vote_next.flood$nobs, 0))),
                                      "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.vote_next.flood$d.avg.ci[1],2)),", ",
                                                          sprintf("%.2f",round(med.out.vote_next.flood$d.avg.ci[2],2)),"]"),
                                                   paste0("[",sprintf("%.2f",round(med.out.vote_next.flood$z.avg.ci[1],2)),", ",
                                                          sprintf("%.2f",round(med.out.vote_next.flood$z.avg.ci[2],2)),"]"),
                                                   paste0("[",sprintf("%.2f",round(med.out.vote_next.flood$tau.ci[1],2)),", ",
                                                          sprintf("%.2f",round(med.out.vote_next.flood$tau.ci[2],2)),"]"),
                                                   paste0("[",sprintf("%.2f",round(med.out.vote_next.flood$n.avg.ci[1],2)),", ",
                                                          sprintf("%.2f",round(med.out.vote_next.flood$n.avg.ci[2],2)),"]"), ""))

# bridge collapse
med.out.vote_next.BC <- mediate(med.fit.BC, out.fit.vote_next.BC, treat="BCresponse", mediator="BCtraits", 
                                robustSE = FALSE, sims=1000,  control.value = "control", 
                                treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out.vote_next.BC)

avg_ACME_vote_next_BC <- med.out.vote_next.BC$d.avg
avg_ACME_ci_vote_next_BC <- med.out.vote_next.BC$d.avg.ci
avg_ACME_p_vote_next_BC <- med.out.vote_next.BC$d.avg.p

avg_ADE_vote_next_BC <- med.out.vote_next.BC$z.avg
avg_ADE_ci_vote_next_BC <- med.out.vote_next.BC$z.avg.ci
avg_ADE_p_vote_next_BC <- med.out.vote_next.BC$z.avg.p

totaleff_vote_next_BC <- med.out.vote_next.BC$tau.coef
totaleff_ci_vote_next_BC <- med.out.vote_next.BC$tau.ci
totaleff_p_vote_next_BC <- med.out.vote_next.BC$tau.p

avg_propmed_vote_next_BC <- med.out.vote_next.BC$n.avg
avg_propmed_ci_vote_next_BC <- med.out.vote_next.BC$n.avg.ci
avg_propmed_p_vote_next_BC <- med.out.vote_next.BC$n.avg.p

mediate_vote_next_BC <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                   "point_est"=c(sprintf("%.2f",round(med.out.vote_next.BC$d.avg, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.BC$z.avg, 2)),
                                                 sprintf("%.2f",round(med.out.vote_next.BC$tau.coef, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.BC$n.avg, 2)),
                                                 sprintf("%.0f",round(med.out.vote_next.BC$nobs, 0))),
                                   "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.vote_next.BC$d.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BC$d.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BC$z.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BC$z.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BC$tau.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BC$tau.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BC$n.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BC$n.avg.ci[2],2)),"]"), ""))

# budget shortfall
med.out.vote_next.BS <- mediate(med.fit.BS, out.fit.vote_next.BS, treat="BSresponse", mediator="BStraits", 
                                robustSE = FALSE, sims=1000,  control.value = "control", 
                                treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out.vote_next.BS)

avg_ACME_vote_next_BS <- med.out.vote_next.BS$d.avg
avg_ACME_ci_vote_next_BS <- med.out.vote_next.BS$d.avg.ci
avg_ACME_p_vote_next_BS <- med.out.vote_next.BS$d.avg.p

avg_ADE_vote_next_BS <- med.out.vote_next.BS$z.avg
avg_ADE_ci_vote_next_BS <- med.out.vote_next.BS$z.avg.ci
avg_ADE_p_vote_next_BS <- med.out.vote_next.BS$z.avg.p

totaleff_vote_next_BS <- med.out.vote_next.BS$tau.coef
totaleff_ci_vote_next_BS <- med.out.vote_next.BS$tau.ci
totaleff_p_vote_next_BS <- med.out.vote_next.BS$tau.p

avg_propmed_vote_next_BS <- med.out.vote_next.BS$n.avg
avg_propmed_ci_vote_next_BS <- med.out.vote_next.BS$n.avg.ci
avg_propmed_p_vote_next_BS <- med.out.vote_next.BS$n.avg.p

mediate_vote_next_BS <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                   "point_est"=c(sprintf("%.2f",round(med.out.vote_next.BS$d.avg, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.BS$z.avg, 2)),
                                                 sprintf("%.2f",round(med.out.vote_next.BS$tau.coef, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.BS$n.avg, 2)),
                                                 sprintf("%.0f",round(med.out.vote_next.BS$nobs, 0))),
                                   "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.vote_next.BS$d.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BS$d.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BS$z.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BS$z.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BS$tau.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BS$tau.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BS$n.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BS$n.avg.ci[2],2)),"]"), ""))


# heat wave
med.out.vote_next.HW <- mediate(med.fit.HW, out.fit.vote_next.HW, treat="HWresponse", mediator="HWtraits", 
                                robustSE = FALSE, sims=1000,  control.value = "control", 
                                treat.value = "blameclaim", boot=TRUE, dropobs = TRUE)

summary(med.out.vote_next.HW)

avg_ACME_vote_next_HW <- med.out.vote_next.HW$d.avg
avg_ACME_ci_vote_next_HW <- med.out.vote_next.HW$d.avg.ci
avg_ACME_p_vote_next_HW <- med.out.vote_next.HW$d.avg.p

avg_ADE_vote_next_HW <- med.out.vote_next.HW$z.avg
avg_ADE_ci_vote_next_HW <- med.out.vote_next.HW$z.avg.ci
avg_ADE_p_vote_next_HW <- med.out.vote_next.HW$z.avg.p

totaleff_vote_next_HW <- med.out.vote_next.HW$tau.coef
totaleff_ci_vote_next_HW <- med.out.vote_next.HW$tau.ci
totaleff_p_vote_next_HW <- med.out.vote_next.HW$tau.p

avg_propmed_vote_next_HW <- med.out.vote_next.HW$n.avg
avg_propmed_ci_vote_next_HW <- med.out.vote_next.HW$n.avg.ci
avg_propmed_p_vote_next_HW <- med.out.vote_next.HW$n.avg.p

mediate_vote_next_HW <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                   "point_est"=c(sprintf("%.2f",round(med.out.vote_next.HW$d.avg, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.HW$z.avg, 2)),
                                                 sprintf("%.2f",round(med.out.vote_next.HW$tau.coef, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.HW$n.avg, 2)),
                                                 sprintf("%.0f",round(med.out.vote_next.HW$nobs, 0))),
                                   "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.vote_next.HW$d.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.HW$d.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.HW$z.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.HW$z.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.HW$tau.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.HW$tau.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.HW$n.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.HW$n.avg.ci[2],2)),"]"), ""))

###############################################################################
# Figure SI.7 Sensitivity Analyses for Flood Experiment Outcomes

sens.out.handling.flood <- medsens(med.out.handling.flood, rho.by=0.1, effect.type = "indirect", sims=1000)
summary(sens.out.handling.flood)

pdf(file = , 
    family = "Times")
plot(sens.out.handling.flood, sens.par="rho", main=expression(paste("Flood Approval ACME (", 
                                                                    rho, ")")))
dev.off()

sens.out.vote_next.flood <- medsens(med.out.vote_next.flood, rho.by=0.1, effect.type = "indirect", sims=1000)
summary(sens.out.vote_next.flood)

pdf(file = , 
    family = "Times")
plot(sens.out.vote_next.flood, sens.par="rho", main=expression(paste("Flood Vote Choice ACME (", 
                                                                     rho, ")")))
dev.off()

###############################################################################
# Figure SI.8 Sensitivity Analyses for Bridge Collapse Outcomes

sens.out.handling.BC <- medsens(med.out.handling.BC, rho.by=0.1, effect.type = "indirect", sims=1000)
summary(sens.out.handling.BC)

pdf(file = , 
    family = "Times")
plot(sens.out.handling.BC, sens.par="rho", main=expression(paste("Bridge Collapse Approval ACME (", 
                                                                 rho, ")")))
dev.off()

sens.out.vote_next.BC <- medsens(med.out.vote_next.BC, rho.by=0.1, effect.type = "indirect", sims=1000)
summary(sens.out.vote_next.BC)

pdf(file = , 
    family = "Times")
plot(sens.out.vote_next.BC, sens.par="rho", main=expression(paste("Bridge Collapse Vote Choice ACME (", 
                                                                  rho, ")")))
dev.off()

###############################################################################
# Figure SI.9 Sensitivity Analyses for Budget Shortfall Outcomes

sens.out.handling.BS <- medsens(med.out.handling.BS, rho.by=0.1, effect.type = "indirect", sims=1000)
summary(sens.out.handling.BS)

pdf(file = , 
    family = "Times")
plot(sens.out.handling.BS, sens.par="rho", main=expression(paste("Budget Shortfall Approval ACME (", 
                                                                 rho, ")")))
dev.off()

sens.out.vote_next.BS <- medsens(med.out.vote_next.BS, rho.by=0.1, effect.type = "indirect", sims=1000)
summary(sens.out.vote_next.BS)

pdf(file = , 
    family = "Times")
plot(sens.out.vote_next.BS, sens.par="rho", main=expression(paste("Budget Shortfall Vote Choice ACME (", 
                                                                  rho, ")")))
dev.off()

###############################################################################
# Figure SI.10 Sensitivity Analyses for Heat Wave Experiment Outcomes

sens.out.handling.HW <- medsens(med.out.handling.HW, rho.by=0.1, effect.type = "indirect", sims=1000)
summary(sens.out.handling.HW)

pdf(file = , 
    family = "Times")
plot(sens.out.handling.HW, sens.par="rho", main=expression(paste("Heat Wave Approval ACME (", 
                                                                 rho, ")")))
dev.off()

sens.out.vote_next.HW <- medsens(med.out.vote_next.HW, rho.by=0.1, effect.type = "indirect", sims=1000)
summary(sens.out.vote_next.HW)

pdf(file = , 
    family = "Times")
plot(sens.out.vote_next.HW, sens.par="rho", main=expression(paste("Heat Wave Vote Choice ACME (", 
                                                                  rho, ")")))
dev.off()

###############################################################################
# Tables SI.25, 26, and 27 (Blameworthiness Mediation Analysis Components)

med.fit.flood <- lm(blame_mayor ~ Floodresponse, data = TAPSdata,
                    weights = jan2018wt1)
out.fit.handling.flood <- lm(handling_bin ~ Floodresponse + blame_mayor, 
                             data = TAPSdata, weights = jan2018wt1)
out.fit.vote_next.flood <- lm(vote_bin ~ Floodresponse + blame_mayor, 
                              data = TAPSdata, weights = jan2018wt1)
med.fit.BC <- lm(BCblame_governor ~ BCresponse, data = MTurk_stylized_data)
out.fit.handling.BC <- lm(BCgovapprove_bin ~ BCresponse + BCblame_governor, 
                          data = MTurk_stylized_data)
out.fit.vote_next.BC <- lm(BCgovvote_bin ~ BCresponse + BCblame_governor, 
                           data = MTurk_stylized_data)
med.fit.BS <- lm(BSblame_governor ~ BSresponse, data = MTurk_stylized_data)
out.fit.handling.BS <- lm(BSgovapprove_bin ~ BSresponse + BSblame_governor, 
                          data = MTurk_stylized_data)
out.fit.vote_next.BS <- lm(BSgovvote_bin ~ BSresponse + BSblame_governor, 
                           data = MTurk_stylized_data)
med.fit.HW <- lm(HWblame_mayor ~ HWresponse, data = MTurk_stylized_data)
out.fit.handling.HW <- lm(HWgovapprove_bin ~ HWresponse + HWblame_mayor, 
                          data = MTurk_stylized_data)
out.fit.vote_next.HW <- lm(HWgovvote_bin ~ HWresponse + HWblame_mayor, 
                           data = MTurk_stylized_data)

# Table SI.25 (Effect of Treatments on Blameworthiness)

texreg(l=list(med.fit.flood, med.fit.BC, med.fit.BS, med.fit.HW), stars = c(0.05),
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect"),
       label = "table:blamesols",
       caption.above = TRUE, 
       caption = "Causal Mediation Regressions (Blameworthiness on Treatment)",
       custom.note = "%stars.  This table presents linear regression models of respondents' perceptions
       of the executives' blameworthiness regressed on their treatment conditions, which were
       then used in the causal mediation analysis presented in the paper.
       Each model uses the blame points respondents assigned to the executive (from 0 to 100) as the
       dependent variable, and dichotomous indicators of treatment assignment 
       as the covariates (with the control condition as the baseline condition). The model
       in the first column includes survey weights; results remain substantively 
       unchanged when weights are not included.",
       include.rmse=FALSE, include.adjrs=FALSE)

# Table SI.26 (Effect of Treatments and Blameworthiness on Handling)

texreg(l=list(out.fit.handling.flood, out.fit.handling.BC, out.fit.handling.BS, out.fit.handling.HW)
       , stars = c(0.05),
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "blame_mayor"="Blameworthiness",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BCblame_governor"="Blameworthiness",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "BSblame_governor"="Blameworthiness",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect",
                              "HWblame_mayor"="Blameworthiness"),
       label = "table:approvalmediateblameols",
       caption.above = TRUE, 
       caption = "Causal Mediation Regressions (Approval on Treatment and Blameworthiness)",
       custom.note = "%stars.  This table presents linear regression models of respondents' 
       approval of the executive's handling of the government crisis regressed on
       their treatment conditions, which were
       then used in the causal mediation analysis presented in the paper.  All models use a dichomotous
       indicator of approval, coded as 1 if the respondent strongly approves or approves of the 
       executive's handling of the governmental crisis, and coded as 0 otherwise.  Covariates include
       dichotomous indicators of treatment assignment and a continuous measure of blame points
       respondents assigned to the executive.
       The model in the first column includes survey weights; results remain substantively 
       unchanged when weights are not included.",
       include.rmse=FALSE, include.adjrs=FALSE)

# Table SI.26 (Effect of Treatments and Blameworthiness on Vote Choice)

texreg(l=list(out.fit.vote_next.flood, out.fit.vote_next.BC, out.fit.vote_next.BS, out.fit.vote_next.HW)
       , stars = c(0.05),
       custom.model.names = c("Flood", "Bridge Collapse", "Budget Shortfall", "Heat Wave"),
       custom.coef.map = list("(Intercept)"="Intercept", 
                              "Floodresponseblame claim"="Blame Claim", 
                              "Floodresponseblame deflect"="Blame Deflect",
                              "blame_mayor"="Blameworthiness",
                              "BCresponseblameclaim"="Blame Claim", 
                              "BCresponseblamedeflect"="Blame Deflect",
                              "BCblame_governor"="Blameworthiness",
                              "BSresponseblameclaim"="Blame Claim", 
                              "BSresponseblamedeflect"="Blame Deflect",
                              "BSblame_governor"="Blameworthiness",
                              "HWresponseblameclaim"="Blame Claim", 
                              "HWresponseblamedeflect"="Blame Deflect",
                              "HWblame_mayor"="Blameworthiness"),
       label = "table:votemediateblameols",
       caption.above = TRUE, 
       caption = "Causal Mediation Regressions (Vote on Treatment and Blameworthiness)",
       custom.note = "%stars.  This table presents linear regression models of respondents' 
       likelihood of voting for the executive in the next election regressed on
       their treatment conditions, which were
       then used in the causal mediation analysis presented in the paper.  All models use a dichomotous
       indicator of likelihood of voting for the executive in the next election, coded as 1 if the 
       respondent is very likely or somewhat likely to vote for the executive, and coded as 0 
       otherwise.  Covariates include
       dichotomous indicators of treatment assignment and a continuous measure of blame points
       respondents assigned to the executive.
       The model in the first column includes survey weights; results remain substantively 
       unchanged when weights are not included.",
       include.rmse=FALSE, include.adjrs=FALSE)

###############################################################################
# Table SI.28 Causal Mediation Analysis (Blameworthiness and Handling)

med.out.handling.flood <- mediate(med.fit.flood, out.fit.handling.flood, treat="Floodresponse", mediator="blame_mayor", 
                                  robustSE = FALSE, sims=1000,  control.value = "control", 
                                  treat.value = "blame deflect", boot=TRUE, dropobs = TRUE)

summary(med.out.handling.flood)

avg_ACME_handling_flood <- med.out.handling.flood$d.avg
avg_ACME_ci_handling_flood <- med.out.handling.flood$d.avg.ci
avg_ACME_p_handling_flood <- med.out.handling.flood$d.avg.p

avg_ADE_handling_flood <- med.out.handling.flood$z.avg
avg_ADE_ci_handling_flood <- med.out.handling.flood$z.avg.ci
avg_ADE_p_handling_flood <- med.out.handling.flood$z.avg.p

totaleff_handling_flood <- med.out.handling.flood$tau.coef
totaleff_ci_handling_flood <- med.out.handling.flood$tau.ci
totaleff_p_handling_flood <- med.out.handling.flood$tau.p

avg_propmed_handling_flood <- med.out.handling.flood$n.avg
avg_propmed_ci_handling_flood <- med.out.handling.flood$n.avg.ci
avg_propmed_p_handling_flood <- med.out.handling.flood$n.avg.p

mediate_handling_flood <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                     "point_est"=c(sprintf("%.2f",round(med.out.handling.flood$d.avg, 2)), 
                                                   sprintf("%.2f",round(med.out.handling.flood$z.avg, 2)),
                                                   sprintf("%.2f",round(med.out.handling.flood$tau.coef, 2)), 
                                                   sprintf("%.2f",round(med.out.handling.flood$n.avg, 2)),
                                                   sprintf("%.0f",round(med.out.handling.flood$nobs, 0))),
                                     "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.handling.flood$d.avg.ci[1],2)),", ",
                                                         sprintf("%.2f",round(med.out.handling.flood$d.avg.ci[2],2)),"]"),
                                                  paste0("[",sprintf("%.2f",round(med.out.handling.flood$z.avg.ci[1],2)),", ",
                                                         sprintf("%.2f",round(med.out.handling.flood$z.avg.ci[2],2)),"]"),
                                                  paste0("[",sprintf("%.2f",round(med.out.handling.flood$tau.ci[1],2)),", ",
                                                         sprintf("%.2f",round(med.out.handling.flood$tau.ci[2],2)),"]"),
                                                  paste0("[",sprintf("%.2f",round(med.out.handling.flood$n.avg.ci[1],2)),", ",
                                                         sprintf("%.2f",round(med.out.handling.flood$n.avg.ci[2],2)),"]"), ""))

med.out.handling.BC <- mediate(med.fit.BC, out.fit.handling.BC, treat="BCresponse", mediator="BCblame_governor", 
                               robustSE = FALSE, sims=1000,  control.value = "control", 
                               treat.value = "blamedeflect", boot=TRUE, dropobs = TRUE)

summary(med.out.handling.BC)

avg_ACME_handling_BC <- med.out.handling.BC$d.avg
avg_ACME_ci_handling_BC <- med.out.handling.BC$d.avg.ci
avg_ACME_p_handling_BC <- med.out.handling.BC$d.avg.p

avg_ADE_handling_BC <- med.out.handling.BC$z.avg
avg_ADE_ci_handling_BC <- med.out.handling.BC$z.avg.ci
avg_ADE_p_handling_BC <- med.out.handling.BC$z.avg.p

totaleff_handling_BC <- med.out.handling.BC$tau.coef
totaleff_ci_handling_BC <- med.out.handling.BC$tau.ci
totaleff_p_handling_BC <- med.out.handling.BC$tau.p

avg_propmed_handling_BC <- med.out.handling.BC$n.avg
avg_propmed_ci_handling_BC <- med.out.handling.BC$n.avg.ci
avg_propmed_p_handling_BC <- med.out.handling.BC$n.avg.p

mediate_handling_BC <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                  "point_est"=c(sprintf("%.2f",round(med.out.handling.BC$d.avg, 2)), 
                                                sprintf("%.2f",round(med.out.handling.BC$z.avg, 2)),
                                                sprintf("%.2f",round(med.out.handling.BC$tau.coef, 2)), 
                                                sprintf("%.2f",round(med.out.handling.BC$n.avg, 2)),
                                                sprintf("%.0f",round(med.out.handling.BC$nobs, 0))),
                                  "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.handling.BC$d.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BC$d.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BC$z.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BC$z.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BC$tau.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BC$tau.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BC$n.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BC$n.avg.ci[2],2)),"]"), ""))

med.out.handling.BS <- mediate(med.fit.BS, out.fit.handling.BS, treat="BSresponse", mediator="BSblame_governor", 
                               robustSE = FALSE, sims=1000,  control.value = "control", 
                               treat.value = "blamedeflect", boot=TRUE, dropobs = TRUE)

summary(med.out.handling.BS)

avg_ACME_handling_BS <- med.out.handling.BS$d.avg
avg_ACME_ci_handling_BS <- med.out.handling.BS$d.avg.ci
avg_ACME_p_handling_BS <- med.out.handling.BS$d.avg.p

avg_ADE_handling_BS <- med.out.handling.BS$z.avg
avg_ADE_ci_handling_BS <- med.out.handling.BS$z.avg.ci
avg_ADE_p_handling_BS <- med.out.handling.BS$z.avg.p

totaleff_handling_BS <- med.out.handling.BS$tau.coef
totaleff_ci_handling_BS <- med.out.handling.BS$tau.ci
totaleff_p_handling_BS <- med.out.handling.BS$tau.p

avg_propmed_handling_BS <- med.out.handling.BS$n.avg
avg_propmed_ci_handling_BS <- med.out.handling.BS$n.avg.ci
avg_propmed_p_handling_BS <- med.out.handling.BS$n.avg.p

mediate_handling_BS <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                  "point_est"=c(sprintf("%.2f",round(med.out.handling.BS$d.avg, 2)), 
                                                sprintf("%.2f",round(med.out.handling.BS$z.avg, 2)),
                                                sprintf("%.2f",round(med.out.handling.BS$tau.coef, 2)), 
                                                sprintf("%.2f",round(med.out.handling.BS$n.avg, 2)),
                                                sprintf("%.0f",round(med.out.handling.BS$nobs, 0))),
                                  "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.handling.BS$d.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BS$d.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BS$z.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BS$z.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BS$tau.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BS$tau.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.BS$n.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.BS$n.avg.ci[2],2)),"]"), ""))

med.out.handling.HW <- mediate(med.fit.HW, out.fit.handling.HW, treat="HWresponse", mediator="HWblame_mayor", 
                               robustSE = FALSE, sims=1000,  control.value = "control", 
                               treat.value = "blamedeflect", boot=TRUE, dropobs = TRUE)

summary(med.out.handling.HW)

avg_ACME_handling_HW <- med.out.handling.HW$d.avg
avg_ACME_ci_handling_HW <- med.out.handling.HW$d.avg.ci
avg_ACME_p_handling_HW <- med.out.handling.HW$d.avg.p

avg_ADE_handling_HW <- med.out.handling.HW$z.avg
avg_ADE_ci_handling_HW <- med.out.handling.HW$z.avg.ci
avg_ADE_p_handling_HW <- med.out.handling.HW$z.avg.p

totaleff_handling_HW <- med.out.handling.HW$tau.coef
totaleff_ci_handling_HW <- med.out.handling.HW$tau.ci
totaleff_p_handling_HW <- med.out.handling.HW$tau.p

avg_propmed_handling_HW <- med.out.handling.HW$n.avg
avg_propmed_ci_handling_HW <- med.out.handling.HW$n.avg.ci
avg_propmed_p_handling_HW <- med.out.handling.HW$n.avg.p

mediate_handling_HW <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                  "point_est"=c(sprintf("%.2f",round(med.out.handling.HW$d.avg, 2)), 
                                                sprintf("%.2f",round(med.out.handling.HW$z.avg, 2)),
                                                sprintf("%.2f",round(med.out.handling.HW$tau.coef, 2)), 
                                                sprintf("%.2f",round(med.out.handling.HW$n.avg, 2)),
                                                sprintf("%.0f",round(med.out.handling.HW$nobs, 0))),
                                  "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.handling.HW$d.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.HW$d.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.HW$z.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.HW$z.avg.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.HW$tau.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.HW$tau.ci[2],2)),"]"),
                                               paste0("[",sprintf("%.2f",round(med.out.handling.HW$n.avg.ci[1],2)),", ",
                                                      sprintf("%.2f",round(med.out.handling.HW$n.avg.ci[2],2)),"]"), ""))

###############################################################################
# Table SI.28 Causal Mediation Analysis (Blameworthiness and Vote Choice)

med.out.vote_next.flood <- mediate(med.fit.flood, out.fit.vote_next.flood, treat="Floodresponse", mediator="blame_mayor", 
                                   robustSE = FALSE, sims=1000,  control.value = "control", 
                                   treat.value = "blame deflect", boot=TRUE, dropobs = TRUE)

summary(med.out.vote_next.flood)

avg_ACME_vote_next_flood <- med.out.vote_next.flood$d.avg
avg_ACME_ci_vote_next_flood <- med.out.vote_next.flood$d.avg.ci
avg_ACME_p_vote_next_flood <- med.out.vote_next.flood$d.avg.p

avg_ADE_vote_next_flood <- med.out.vote_next.flood$z.avg
avg_ADE_ci_vote_next_flood <- med.out.vote_next.flood$z.avg.ci
avg_ADE_p_vote_next_flood <- med.out.vote_next.flood$z.avg.p

totaleff_vote_next_flood <- med.out.vote_next.flood$tau.coef
totaleff_ci_vote_next_flood <- med.out.vote_next.flood$tau.ci
totaleff_p_vote_next_flood <- med.out.vote_next.flood$tau.p

avg_propmed_vote_next_flood <- med.out.vote_next.flood$n.avg
avg_propmed_ci_vote_next_flood <- med.out.vote_next.flood$n.avg.ci
avg_propmed_p_vote_next_flood <- med.out.vote_next.flood$n.avg.p

mediate_vote_next_flood <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                      "point_est"=c(sprintf("%.2f",round(med.out.vote_next.flood$d.avg, 2)), 
                                                    sprintf("%.2f",round(med.out.vote_next.flood$z.avg, 2)),
                                                    sprintf("%.2f",round(med.out.vote_next.flood$tau.coef, 2)), 
                                                    sprintf("%.2f",round(med.out.vote_next.flood$n.avg, 2)),
                                                    sprintf("%.0f",round(med.out.vote_next.flood$nobs, 0))),
                                      "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.vote_next.flood$d.avg.ci[1],2)),", ",
                                                          sprintf("%.2f",round(med.out.vote_next.flood$d.avg.ci[2],2)),"]"),
                                                   paste0("[",sprintf("%.2f",round(med.out.vote_next.flood$z.avg.ci[1],2)),", ",
                                                          sprintf("%.2f",round(med.out.vote_next.flood$z.avg.ci[2],2)),"]"),
                                                   paste0("[",sprintf("%.2f",round(med.out.vote_next.flood$tau.ci[1],2)),", ",
                                                          sprintf("%.2f",round(med.out.vote_next.flood$tau.ci[2],2)),"]"),
                                                   paste0("[",sprintf("%.2f",round(med.out.vote_next.flood$n.avg.ci[1],2)),", ",
                                                          sprintf("%.2f",round(med.out.vote_next.flood$n.avg.ci[2],2)),"]"), ""))

med.out.vote_next.BC <- mediate(med.fit.BC, out.fit.vote_next.BC, treat="BCresponse", mediator="BCblame_governor", 
                                robustSE = FALSE, sims=1000,  control.value = "control", 
                                treat.value = "blamedeflect", boot=TRUE, dropobs = TRUE)

summary(med.out.vote_next.BC)

avg_ACME_vote_next_BC <- med.out.vote_next.BC$d.avg
avg_ACME_ci_vote_next_BC <- med.out.vote_next.BC$d.avg.ci
avg_ACME_p_vote_next_BC <- med.out.vote_next.BC$d.avg.p

avg_ADE_vote_next_BC <- med.out.vote_next.BC$z.avg
avg_ADE_ci_vote_next_BC <- med.out.vote_next.BC$z.avg.ci
avg_ADE_p_vote_next_BC <- med.out.vote_next.BC$z.avg.p

totaleff_vote_next_BC <- med.out.vote_next.BC$tau.coef
totaleff_ci_vote_next_BC <- med.out.vote_next.BC$tau.ci
totaleff_p_vote_next_BC <- med.out.vote_next.BC$tau.p

avg_propmed_vote_next_BC <- med.out.vote_next.BC$n.avg
avg_propmed_ci_vote_next_BC <- med.out.vote_next.BC$n.avg.ci
avg_propmed_p_vote_next_BC <- med.out.vote_next.BC$n.avg.p

mediate_vote_next_BC <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                   "point_est"=c(sprintf("%.2f",round(med.out.vote_next.BC$d.avg, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.BC$z.avg, 2)),
                                                 sprintf("%.2f",round(med.out.vote_next.BC$tau.coef, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.BC$n.avg, 2)),
                                                 sprintf("%.0f",round(med.out.vote_next.BC$nobs, 0))),
                                   "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.vote_next.BC$d.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BC$d.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BC$z.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BC$z.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BC$tau.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BC$tau.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BC$n.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BC$n.avg.ci[2],2)),"]"), ""))

med.out.vote_next.BS <- mediate(med.fit.BS, out.fit.vote_next.BS, treat="BSresponse", mediator="BSblame_governor", 
                                robustSE = FALSE, sims=1000,  control.value = "control", 
                                treat.value = "blamedeflect", boot=TRUE, dropobs = TRUE)

summary(med.out.vote_next.BS)

avg_ACME_vote_next_BS <- med.out.vote_next.BS$d.avg
avg_ACME_ci_vote_next_BS <- med.out.vote_next.BS$d.avg.ci
avg_ACME_p_vote_next_BS <- med.out.vote_next.BS$d.avg.p

avg_ADE_vote_next_BS <- med.out.vote_next.BS$z.avg
avg_ADE_ci_vote_next_BS <- med.out.vote_next.BS$z.avg.ci
avg_ADE_p_vote_next_BS <- med.out.vote_next.BS$z.avg.p

totaleff_vote_next_BS <- med.out.vote_next.BS$tau.coef
totaleff_ci_vote_next_BS <- med.out.vote_next.BS$tau.ci
totaleff_p_vote_next_BS <- med.out.vote_next.BS$tau.p

avg_propmed_vote_next_BS <- med.out.vote_next.BS$n.avg
avg_propmed_ci_vote_next_BS <- med.out.vote_next.BS$n.avg.ci
avg_propmed_p_vote_next_BS <- med.out.vote_next.BS$n.avg.p

mediate_vote_next_BS <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                   "point_est"=c(sprintf("%.2f",round(med.out.vote_next.BS$d.avg, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.BS$z.avg, 2)),
                                                 sprintf("%.2f",round(med.out.vote_next.BS$tau.coef, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.BS$n.avg, 2)),
                                                 sprintf("%.0f",round(med.out.vote_next.BS$nobs, 0))),
                                   "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.vote_next.BS$d.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BS$d.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BS$z.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BS$z.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BS$tau.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BS$tau.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.BS$n.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.BS$n.avg.ci[2],2)),"]"), ""))

med.out.vote_next.HW <- mediate(med.fit.HW, out.fit.vote_next.HW, treat="HWresponse", mediator="HWblame_mayor", 
                                robustSE = FALSE, sims=1000,  control.value = "control", 
                                treat.value = "blamedeflect", boot=TRUE, dropobs = TRUE)

summary(med.out.vote_next.HW)

avg_ACME_vote_next_HW <- med.out.vote_next.HW$d.avg
avg_ACME_ci_vote_next_HW <- med.out.vote_next.HW$d.avg.ci
avg_ACME_p_vote_next_HW <- med.out.vote_next.HW$d.avg.p

avg_ADE_vote_next_HW <- med.out.vote_next.HW$z.avg
avg_ADE_ci_vote_next_HW <- med.out.vote_next.HW$z.avg.ci
avg_ADE_p_vote_next_HW <- med.out.vote_next.HW$z.avg.p

totaleff_vote_next_HW <- med.out.vote_next.HW$tau.coef
totaleff_ci_vote_next_HW <- med.out.vote_next.HW$tau.ci
totaleff_p_vote_next_HW <- med.out.vote_next.HW$tau.p

avg_propmed_vote_next_HW <- med.out.vote_next.HW$n.avg
avg_propmed_ci_vote_next_HW <- med.out.vote_next.HW$n.avg.ci
avg_propmed_p_vote_next_HW <- med.out.vote_next.HW$n.avg.p

mediate_vote_next_HW <- data.frame("quantity"=c("ACME","ADE","Total Effect", "Prop. Mediated", "Num. obs."),
                                   "point_est"=c(sprintf("%.2f",round(med.out.vote_next.HW$d.avg, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.HW$z.avg, 2)),
                                                 sprintf("%.2f",round(med.out.vote_next.HW$tau.coef, 2)), 
                                                 sprintf("%.2f",round(med.out.vote_next.HW$n.avg, 2)),
                                                 sprintf("%.0f",round(med.out.vote_next.HW$nobs, 0))),
                                   "conf_int"=c(paste0("[",sprintf("%.2f",round(med.out.vote_next.HW$d.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.HW$d.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.HW$z.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.HW$z.avg.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.HW$tau.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.HW$tau.ci[2],2)),"]"),
                                                paste0("[",sprintf("%.2f",round(med.out.vote_next.HW$n.avg.ci[1],2)),", ",
                                                       sprintf("%.2f",round(med.out.vote_next.HW$n.avg.ci[2],2)),"]"), ""))

################################################################################
# Figure SI.11 Causal Mediation Analysis (Effect of Blameworthiness)

pdf(file = , 
    family = "Times", height = 9.5, width=13)
par(mar=c(5.1,10.25,2.75,.5))
plot(x=c(totaleff_handling_HW, avg_ADE_handling_HW, avg_ACME_handling_HW,
         totaleff_handling_BS, avg_ADE_handling_BS, avg_ACME_handling_BS,
         totaleff_handling_BC, avg_ADE_handling_BC, avg_ACME_handling_BC,
         totaleff_handling_flood, avg_ADE_handling_flood, avg_ACME_handling_flood,
         totaleff_vote_next_HW+0.6, avg_ADE_vote_next_HW+0.6, avg_ACME_vote_next_HW+0.6,
         totaleff_vote_next_BS+0.6, avg_ADE_vote_next_BS+0.6, avg_ACME_vote_next_BS+0.6,
         totaleff_vote_next_BC+0.6, avg_ADE_vote_next_BC+0.6, avg_ACME_vote_next_BC+0.6,
         totaleff_vote_next_flood+0.6, avg_ADE_vote_next_flood+0.6, avg_ACME_vote_next_flood+0.6), 
     y=rep(c(seq(1,5, by=2), seq(11,15, by=2), seq(21,25, by=2), seq(31,35, by=2)),2),
     xlim=c(-.35,0.75),
     xaxt="n",
     ylim=c(0.5,38.5),
     pch=19,
     tck=-.02,
     cex.axis=0.9,
     cex=1.5,
     ylab="",
     yaxt="n",
     xlab="",
     axes = FALSE)
abline(v=0,lwd=2, col="gray",lty=2)
abline(v=0.6,lwd=2, col="gray",lty=2)
segments(x0=c(totaleff_ci_handling_HW[1], avg_ADE_ci_handling_HW[1], avg_ACME_ci_handling_HW[1],
              totaleff_ci_handling_BS[1], avg_ADE_ci_handling_BS[1], avg_ACME_ci_handling_BS[1],
              totaleff_ci_handling_BC[1], avg_ADE_ci_handling_BC[1], avg_ACME_ci_handling_BC[1],
              totaleff_ci_handling_flood[1], avg_ADE_ci_handling_flood[1], avg_ACME_ci_handling_flood[1],
              totaleff_ci_vote_next_HW[1]+0.6, avg_ADE_ci_vote_next_HW[1]+0.6, avg_ACME_ci_vote_next_HW[1]+0.6,
              totaleff_ci_vote_next_BS[1]+0.6, avg_ADE_ci_vote_next_BS[1]+0.6, avg_ACME_ci_vote_next_BS[1]+0.6,
              totaleff_ci_vote_next_BC[1]+0.6, avg_ADE_ci_vote_next_BC[1]+0.6, avg_ACME_ci_vote_next_BC[1]+0.6,
              totaleff_ci_vote_next_flood[1]+0.6, avg_ADE_ci_vote_next_flood[1]+0.6, avg_ACME_ci_vote_next_flood[1]+0.6),
         x1=c(totaleff_ci_handling_HW[2], avg_ADE_ci_handling_HW[2], avg_ACME_ci_handling_HW[2],
              totaleff_ci_handling_BS[2], avg_ADE_ci_handling_BS[2], avg_ACME_ci_handling_BS[2],
              totaleff_ci_handling_BC[2], avg_ADE_ci_handling_BC[2], avg_ACME_ci_handling_BC[2],
              totaleff_ci_handling_flood[2], avg_ADE_ci_handling_flood[2], avg_ACME_ci_handling_flood[2],
              totaleff_ci_vote_next_HW[2]+0.6, avg_ADE_ci_vote_next_HW[2]+0.6, avg_ACME_ci_vote_next_HW[2]+0.6,
              totaleff_ci_vote_next_BS[2]+0.6, avg_ADE_ci_vote_next_BS[2]+0.6, avg_ACME_ci_vote_next_BS[2]+0.6,
              totaleff_ci_vote_next_BC[2]+0.6, avg_ADE_ci_vote_next_BC[2]+0.6, avg_ACME_ci_vote_next_BC[2]+0.6,
              totaleff_ci_vote_next_flood[2]+0.6, avg_ADE_ci_vote_next_flood[2]+0.6, avg_ACME_ci_vote_next_flood[2]+0.6),
         y0=rep(c(seq(1,5, by=2), seq(11,15, by=2), seq(21,25, by=2), seq(31,35, by=2)),2))
axis(1,at=c(-.3, -.1, .1), 
     labels = c("-30%", "-10%", "10%"), cex.axis = 1.75)
axis(1,at=c(.3, .5, .7), 
     labels = c("-30%", "-10%", "10%"), cex.axis = 1.75)
axis(1,at=c(-.2, 0), 
     labels = c("-20%", "0%"), cex.axis = 1.75)
axis(1,at=c(.4, .6), 
     labels = c("-20%", "0%"), cex.axis = 1.75)
par(mgp=c(0,6,0))
axis(2,at=c(seq(1,5, by=2), seq(11,15, by=2), seq(21,25, by=2), seq(31,35, by=2)),
     las=2,
     labels=rep(c("Total Effect", "ADE", "ACME"),4),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
par(mgp=c(0,10,0))
axis(2,at=c(7.25, 17.25, 27.25, 37.25),
     las=2,
     labels=c(expression(~bold(~underline("Heat Wave"))), 
              expression(~bold(~underline("Budget Shortfall"))), 
              expression(~bold(~underline("Bridge Collapse"))), 
              expression(~bold(~underline("Flood")))),
     tck=0,
     lwd = 0,
     line = 0,
     cex.axis=1.75, hadj=0)
mtext("Difference from Control", side = 1, at=-0.10, line = 3, cex = 2)
mtext("Difference from Control", side = 1, at=0.50, line = 3, cex = 2)
mtext("Approval", side = 3, cex = 2.5, at=-0.10, line=0.5)
mtext("Vote Choice", side = 3, cex = 2.5, at=0.50, line=0.5)
text(c(totaleff_handling_HW, avg_ADE_handling_HW, avg_ACME_handling_HW,
       totaleff_handling_BS, avg_ADE_handling_BS, avg_ACME_handling_BS,
       totaleff_handling_BC, avg_ADE_handling_BC, avg_ACME_handling_BC,
       totaleff_handling_flood, avg_ADE_handling_flood, avg_ACME_handling_flood,
       totaleff_vote_next_HW+0.6, avg_ADE_vote_next_HW+0.6, avg_ACME_vote_next_HW+0.6,
       totaleff_vote_next_BS+0.6, avg_ADE_vote_next_BS+0.6, avg_ACME_vote_next_BS+0.6,
       totaleff_vote_next_BC+0.6, avg_ADE_vote_next_BC+0.6, avg_ACME_vote_next_BC+0.6,
       totaleff_vote_next_flood+0.6, avg_ADE_vote_next_flood+0.6, avg_ACME_vote_next_flood+0.6), 
     c(seq(1,5, by=2), seq(11,15, by=2), seq(21,25, by=2), seq(31,35, by=2))+0.9,
     paste0(sprintf("%.0f",round(c(totaleff_handling_HW, avg_ADE_handling_HW, avg_ACME_handling_HW,
                                   totaleff_handling_BS, avg_ADE_handling_BS, avg_ACME_handling_BS,
                                   totaleff_handling_BC, avg_ADE_handling_BC, avg_ACME_handling_BC,
                                   totaleff_handling_flood, avg_ADE_handling_flood, avg_ACME_handling_flood,
                                   totaleff_vote_next_HW, avg_ADE_vote_next_HW, avg_ACME_vote_next_HW,
                                   totaleff_vote_next_BS, avg_ADE_vote_next_BS, avg_ACME_vote_next_BS,
                                   totaleff_vote_next_BC, avg_ADE_vote_next_BC, avg_ACME_vote_next_BC,
                                   totaleff_vote_next_flood, avg_ADE_vote_next_flood, avg_ACME_vote_next_flood)*100, 0)),"%"), cex=1.75)
dev.off()

################################################################################
# Table SI.30 Respondent Descriptive Characteristics

# age 
round(prop.table(table(TAPSdata$age, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_stylized_data$age, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_factorial_data$age, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_flint_data$age, useNA = "always"))*100, 1)

# gender
round(prop.table(table(TAPSdata$female, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_stylized_data$female, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_factorial_data$female, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_flint_data$female, useNA = "always"))*100, 1)

# race/ethnicity
round(prop.table(table(TAPSdata$white, useNA = "always"))*100, 1)
round(prop.table(table(TAPSdata$black, useNA = "always"))*100, 1)
round(prop.table(table(TAPSdata$asian, useNA = "always"))*100, 1)
round(prop.table(table(TAPSdata$hispanic, useNA = "always"))*100, 1)

round(prop.table(table(MTurk_stylized_data$race, useNA = "always"))*100, 1)

round(prop.table(table(MTurk_factorial_data$race, useNA = "always"))*100, 1)

round(prop.table(table(MTurk_flint_data$race, useNA = "always"))*100, 1)

# education

round(prop.table(table(TAPSdata$educ, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_stylized_data$educ, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_factorial_data$educ, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_flint_data$educ, useNA = "always"))*100, 1)

# income
round(prop.table(table(TAPSdata$income, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_stylized_data$income, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_factorial_data$income, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_flint_data$income, useNA = "always"))*100, 1)

# PID
round(prop.table(table(TAPSdata$pid, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_stylized_data$pid, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_factorial_data$pid, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_flint_data$pid, useNA = "always"))*100, 1)

# ideology (values collapsed to four categories used in Table SI.30)

round(prop.table(table(TAPSdata$ideo, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_stylized_data$ideo, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_factorial_data$ideo, useNA = "always"))*100, 1)
round(prop.table(table(MTurk_flint_data$ideo, useNA = "always"))*100, 1)
