##---------------------------
## AUTHORS:  D. Alex Hughes, Micah GR
## PURPOSE:  Analysis of voting experiment with 1st generation immigrants
## DATA IN:  'analysisData.Rdata'
## DATA OUT: 
## DATE BEGAN: Rocktober 2014
## DATE PUBLISHED: 2018
#----------------------------

rm(list = ls())

##
## Load Packages
##

library(stargazer)
library(data.table) 
library(mlogit)
library(magrittr)

##
## check wd:
##

setwd("~/Dropbox/1-Projects/Alex-Micah/San Diego Experiment/Current Version/phenotypic_preference/")
getwd()

## 
## 1. Make Balance Table
## 

dVote <- fread('./data/processed/dVote.csv')

source('./code-analysis/balanceTable.R')
balance_table(DT=dVote)


## 
## Estimate balance model:
##  Null result fails to reject null hypothesis that there
##  is no imbalance on observables
##

balance_data <- mlogit.data(
    data=na.omit(dVote[ , .(conditions, age, sex, yearsinus, educ)]),
    shape='wide',
    varrying=2:5,
    choice='conditions'
)

balance_null <- mlogit(formula = conditions ~ 1 | 1 , data = balance_data)
balance_alt  <- mlogit(
    formula = conditions ~ 1  | 1 + factor(sex) + age + yearsinus + educ,
    method = "bfgs",
    data = balance_data
)
lrtest(balance_null, balance_alt)
                            
##
## 2. Models and Tests 
##

rm(list = ls())
source('./code-analysis/sem.R')
dVote <- fread('./data/processed/dVote.csv')

## Some individuals did not complete the entire As a test of differential
## attrition, we check that missingness on these variables is not predicted
## by the treatment conditions. Indeed, it is not.

attrition_1 = dVote[ , lm(is.na(age)  ~ factor(conditions))]
attrition_2 = dVote[ , lm(is.na(sex)  ~ factor(conditions))]
attrition_3 = dVote[ , lm(is.na(educ) ~ factor(conditions))]

stargazer(attrition_1, attrition_2, attrition_3, type = "text")

## There is a systematic relationship between age and the likelihood of
## proceeding far enough in the survey to receive the randomization and
## subsequent treatment. Older individuals are more likely to have
## dropped out of the study prior to randomization than younger individuals.
##
## This does not affect our causal identification among the set of individuals
## who make it to randomization.

attrition_4 = dVote[ , lm(is.na(conditions) ~ age + factor(sex) + factor(educ))]

stargazer(attrition_4, type = "text")

## Is there a difference between any of the conditions when
## compared against the control condition?

## In the following tests:
## - conditions == 1: Control condition -- no images 
## - conditions == 2: white looking candidate
## - conditions == 3: mestizo looking candidate 
## - conditions == 4: indigenous looking candidate 

## There is only evidence for this at the indigenous vs. control
dVote[conditions %in% c(1,2), t.test(votecanmiss ~ conditions)$p.val]
dVote[conditions %in% c(1,3), t.test(votecanmiss ~ conditions)$p.val]
dVote[conditions %in% c(1,4), t.test(votecanmiss ~ conditions)$p.val]

##
## Estimate logit models 
##

## We estimate relatively familiar binary choice models. The outcome
##  variable, `votecanmiss` is scored as a 1 if the subject voted for the
##  central (e.g. experimental) candidate. If the subject voted for either
##  of the other two candidates, `votecanmiss == 0`.

## These models are reported in Table 1 of the published paper. 


m1 <- dVote[ , glm(votecanmiss ~ factor(conditions),
                   family='binomial') ]
m2 <- dVote[ , glm(votecanmiss ~ factor(conditions) + age + I(sex==1) + educ,
                   family='binomial') ]
m3 <- dVote[ , glm(votecanmiss ~ factor(conditions) + age + I(sex==1) + educ + yearsinus,
                   family='binomial') ]
m4 <- dVote[ , glm(votecanmiss ~ factor(conditions) + age + I(sex==1) + educ + yearsinus +
                       factor(conditions) * yearsinus, family='binomial') ]

## Print models to screen
stargazer(m1, m2, m3, m4, type = "text",
          covariate.labels = c('White candidate', 'Mestizo candidate',
              'Indigenous candidate', 'Age', 'Female?', 'Education',
              'Years in U.S.', 'White cand*Years in U.S.',
              'Mestizo cand*Years in U.S.', 'Indigenous cand*Years in U.S.')
)

##
## Table 1
##

stargazer(m1, m2, m3, m4,
          type = "latex",
          out = "./reports/tables-figures/coreResults.tex",
          float = TRUE,
          float.env = "table",
          table.placement = "t",
          covariate.labels = c('White candidate', 'Mestizo candidate',
              'Indigenous candidate', 'Age', 'Female?', 'Education',
              'Years in U.S.', 'White cand*Years in U.S.',
              'Mestizo cand*Years in U.S.', 'Indigenous cand*Years in U.S.'),
          omit.stat=c("ser","f"),
          column.sep.width = '1pt',
          align = TRUE,
          label = 'coreEstimates',
          title = "Logit regression of experimental treatment. The dependent variable is subjects' registered preference for the target candidate. The base category for the experimental treatment conditions is the control condition.",
          dep.var.labels = 'Vote for target candidate')

##
## 2. Figures for printing
##

##
## (Figure 1 and 2 are examples of treatment stimulus.)
## 

##
## Figure 3
## 

## Mean Data Cuts
non_model = dVote %>%
    .[ , .(m = mean(votecanmiss, na.rm = TRUE),
           se = sem(votecanmiss)),
      keyby = .(conditions)]
non_model


non_model = non_model[-1]
non_model[ , ':='(
    uprSE = m + se,
    lwrSE = m - se,
    upr95 = m + 1.96 * se,
    lwr95 = m - 1.96 * se)
    ]
non_model



pdf(file = "./reports/tables-figures/mainEffectPlot.pdf")
source("./src/visualization/pubPlot.R")
non_model[ , barplot(
    height = m,
    names.arg = c('Control', 'White', 'Mestizo', 'Indigenous'),
    xlab = 'Treatment Condition',
    col = 'grey', border = NA,
    ylim = c(0, .6),
    main = 'Rate of choosing target candidate',
    yaxt='n')
    ]
axis(2, at = seq(from = 0, to = 0.6, by = 0.1),
     labels = c("0", "10", "20", "30", "40", "50", "60%") )
xLoc <- c(0.7, 1.9, 3.1, 4.3)
for(i in 1:4) {
    lines(x = c(xLoc[i], xLoc[i]),
          y = c(non_model$upr95[i], non_model$lwr95[i]),
          col = "black", lwd = 2
          )
    lines(x = c(xLoc[i], xLoc[i]),
          y = c(non_model$uprSE[i], non_model$lwrSE[i]),
          col = "black", lwd = 4
          )
}
dev.off()

##
##  Figure 5
## 

pred1 <- data.table(conditions = c(rep(2, 100), rep(3, 100), rep(4, 100)),
                    condition_name = c(rep('White', 100),
                                       rep('Mestizo', 100),
                                       rep('Indigenous', 100)), 
                    yearsinus = rep(seq(min(dVote$yearsinus, na.rm = TRUE),
                                        max(dVote$yearsinus, na.rm = TRUE),
                                        length.out = 100), 3),
                    age = median(dVote$age, na.rm = TRUE),
                    sex = median(dVote$sex, na.rm = TRUE),
                    educ = median(dVote$educ, na.rm = TRUE)
                    )

predictions <- predict(m4, newdata = pred1, type = "response", se.fit = TRUE)
pred1[ , fit := predictions$fit]
pred1[ , se.fit := predictions$se.fit]

## x-spot

xSpot <- c(mean(dVote$yearsinus, na.rm = T),
           predict(m4, data.frame(conditions = 1,
                                  yearsinus = mean(dVote$yearsinus, na.rm = TRUE),
                                  age = median(dVote$age, na.rm = TRUE),
                                  sex = median(dVote$sex, na.rm = TRUE),
                                  educ = median(dVote$educ, na.rm = TRUE)),
                   type = "response") )

pdf(file = "./reports/tables-figures/predictedProbs.pdf", width = 10, height = 3.3)
source("./src/visualization/predictedProbPlots.R")
source("./src/visualization/pubPlot.R")
par(mfrow = c(1,3))
par(mar = c(2.1, 9, 4.1, 0), mgp = c(5,1,0))
prediction_plots(DT=pred1, condition = "White", ylab = TRUE)
prediction_plots(DT=pred1, condition = "Mestizo", ylab = FALSE)
prediction_plots(DT=pred1, condition = "Indigenous", ylab = FALSE)
dev.off()


##
## 3. Ideology results
##

dIdeology <- fread('./data/processed/dIdeology.csv')
dPartyID  <- fread('./data/processed/dPartyID.csv')

## What does the racial signal do to individuals perceptions of
## candidates' ideologies? This is another down-stream effect that
## we can measure as an outcome of treatment.

## First: What does the distribution of ideologies look like? 
dIdeology[ , table(gonzideol)]       # 998 and 999 are missing values;
                                     # 8 is I do not think in these terms       
dIdeology[ , table(miss.ideol)]      # miss.ideol includes NAs for them

## attributed ideology as a response variable
## Not much evidence that education matters, so not reported in tables. 
mIdeology1 <- dIdeology[ , lm(miss.ideol ~ factor(conditions))]
mIdeology2 <- dIdeology[ , lm(miss.ideol ~ factor(conditions)
                              + age + sex + educ + yearsinus)]
mIdeology3 <- dIdeology[ , lm(miss.ideol ~ factor(conditions) * educ)]
stargazer(mIdeology1, mIdeology2, type = "text",
          covariate.labels = c(
              'White Cue', 'Mestizo Cue', 'Indigenous Cue',
              'Age', 'Sex', 'Education', 'Years in US'))
          
##
## In a barplot
##

barIdeology <- dIdeology[!(is.na(miss.ideol)|is.na(conditions)),
                         .(m  = mean(miss.ideol, na.rm = T),
                           se = sem(miss.ideol)),
                         by = conditions]
setkey(barIdeology, conditions)
barIdeology[ , ':='(uprSE = m + se,
                    lwrSE = m - se,
                    lwr90 = m - qnorm(.95) * se,
                    upr90 = m + qnorm(.95) * se,
                    lwr95 = m - qnorm(.975) * se,
                    upr95 = m + qnorm(.975) * se),
            by = conditions]

##
## Figure 4
##

pdf(file = "./reports/tables-figures/gonzideolPrediction.pdf")
source("./src/visualization/pubPlot.R")
bars <- barplot(barIdeology[,m],
                ylim = c(0, max(barIdeology) + .5),
                main = "Respondent estimate of \n candidate ideology",
                names.arg = c("Control", "White", "Mestizo", "Indigenous"),
                xlab = "Treatment Condition",
                col = "grey", border = NA,
                )
arrows(x0 = bars, y0 = barIdeology$lwrSE,
       x1 = bars, y1 = barIdeology$uprSE,
       length = 0, lwd = 4, col = "black")
arrows(x0 = bars, y0 = barIdeology$lwr95,
       x1 = bars, y1 = barIdeology$upr95,
       length = 0, lwd = 2, col = "black")
mtext(text = expression("" %<-%"More Liberal     More Conservative"%->%""),
      side = 2, line = 3, las = 3,
      cex = 1.5, font=2, col = "grey30")
dev.off()

## 
## Robustness Checks: Ideology 
## 
## check correlation between skin color and
## years in US
##
## the concern is that it might be a possible compositional shift
## in who is coming to the US, and that changing preferneces that
## we're arguing are a result of one feature, instead are just a
## result of something else.
##
## 1. are people just getting more extreme in their political
##    viewpoints ?
## 2. are people just coming from different regions that have
##    particular phenotypic characteristics?
##
## let's look!
##

## changing viewpoints
dIdeology[ , cor(miss.ideol, yearsinus, use = "c")]
dIdeology[ , cor.test(miss.ideol, yearsinus, use = "c")]

## This does not seem to be the case. 

##
## 3. Other Features of Candidate:
##    Party of candidate
##    

## matches with presumed ideology of candidate
dPartyID[ , table(gonzparty)]
dPartyID[ , table(gonzideol)]
## plot
pri.d <- dPartyID[gonzideol <= 7 & cand.id == "pri", density(gonzideol)]
pan.d <- dPartyID[gonzideol <= 7 & cand.id == "pan", density(gonzideol)]
prd.d <- dPartyID[gonzideol <= 7 & cand.id == "prd", density(gonzideol)]

## this plot demonstrates that r.s understood the task
pdf(file = "../tables-figures/IdeoDensities.pdf")
source("./src/visualization/pubPlot.R")
plot(pri.d, main = "Respondents' assessments of \n candidate ideology",
     ylim = c(0, 0.3), xlab = 'Left-to-right ideology scale', ylab = '')
lines(pan.d)
lines(prd.d)
polygon(pri.d, col = rgb(0, 1, 0, alpha = 0.5))
polygon(pan.d, col = rgb(0, 0, 1, alpha = 0.5))
polygon(prd.d, col = rgb(1, 0, 0, alpha = 0.5))
legend("topright",
       legend = c('PRI', 'PAN', 'PRD'),
       fill = c('green', 'blue', 'red'),
       bty = "n")
dev.off()

##
## Appendix
##

pdf(file = './reports/tables-figures/ideologyTimeUS.pdf', height = 5, width = 10)
par(mfrow = c(1,2))
source('./src/visualization/pubPlot.R')
dVote[ideology < 8, .(ideology_bin = cut(ideology, breaks = c(0,3,5,7)),
                      yearsinus = yearsinus)] %>%
    boxplot(yearsinus ~ ideology_bin, data = .,
            main = "Years in US does not \nchange with Ideology",
            ylab = "Years in US",
            xlab = "Ideology Bin")
dVote[ideology < 8, plot(y=ideology, x=yearsinus,
                         main = "Ideology Over Time",
                         xlab = "Years in US",
                         ylab = "Ideology")]
dev.off()

cat('end of file. \n')
