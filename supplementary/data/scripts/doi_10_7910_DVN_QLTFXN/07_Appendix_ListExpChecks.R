## This file conducts the list experiment diagnostics 
## of the List Experiment Design that appear in Appendix Section 9
## This command file produces tables A29-A35 and Figures A2 & A4
## It also exports Figures A2 & A4
## Note: Sensitivity Analysis (Figure A3) is in coding file "08_Appendix_Sensitivity.R" due to long run time.

# this file is designed to run after the following files
# uncomment to run previous coding files
# source("00_Packages.R")
# source("01_DataVariables.R")
# source("02_MainAnalyses_Estimation.R")
# source("03_MainAnalyses_Mechanisms.R")
# source("04_MainAnalyses_Outcomes.R")
# source("05_Appendix_Descriptives.R")
# source("06_Appendix_SpatialCRSVData.R")


#############################
#### Appendix Section 9 List Experiment and Design Assumptions
#############################

########################################
##### Appendix Section 9.2.1, Table A29
########################################
# Verifying the first assumption of list experiments

# SUBSET DATA 
## CRSV
DLL1 <- subset(D.bal, treat_list_CRSV == 1)
DLL0 <- subset(D.bal, treat_list_CRSV == 0)

#### t-test function
ttestoutput <- function(dattreat, datcontrol, vars) {
  ttest <- t.test(dattreat[vars], datcontrol[vars])
  tvalue <- ttest$statistic
  pvalue <- ttest$p.value
  meanoft <- ttest$estimate[1]
  meanofc <- ttest$estimate[2]
  meandif <- meanoft - meanofc
  out <- c(meanoft, meanofc, meandif, pvalue)
  return(out)
}

#### create list of variables for descriptive balance statistics

vars <- c("female", "isHHhead", "age", "edu_level", "occup_farmer", "married",
          "hh_size", "occup_sal", "assets_sum", "exchange_prev", "activity_prev",
          "dist_village", "geography3", "dist_militia", "survey_before", 
          "population", "rape_prev2", "rape_prev7", "rape_prev15")


# empty frame
balanceDF1 <- data.frame("Variable Name" = character(),  "CRSV=1" = integer(), "CRSV=0" = integer(),
                         "Difference" = integer(), "p-value" = integer(), stringsAsFactors = FALSE)
# fill table with loop
for (nVar in 1:length(vars)) {
  balanceDF1[nVar,1] <- sprintf("%s",vars[nVar])
  balanceDF1[nVar,2:5] <- round(ttestoutput(DLL1, DLL0, sprintf("%s",vars[nVar])),4)
}

# name variables
balanceDF1$Variable.Name <- c("Female", "Head of Household", "Age", "Education", "Farmer", "Married",
                              "Household Size", "Monthly Income", "Assets", "Previous Social Exchange", "Previous Social Activity",
                              "Dist. to Village", "Dist. from Mines", "Dist. from Armed Groups", "Participated in Survey Before", 
                              "Village Population", "Rape in Village", "Rape in Village(7yr)", "Rape in Village (15yr)")

colnames(balanceDF1) <- c("Variable Name", "List Treatment = 1", "List Treatment = 0", "Difference", "p-value")

observ1 <- c("Observations", nrow(DLL1), nrow(DLL0), NA, NA)

balanceDF1 <- rbind(balanceDF1, observ1)

# produce table A29
stargazer(balanceDF1, summary=FALSE, rownames = FALSE, type = "text", title = "Balance Table for List Experiment Randomization", header=FALSE, font.size = "footnotesize")


########################################
##### Appendix Section 9.2.1, Table A30
########################################
# code tables of proportions (table 2 from Blair and Imai)

DIST <- data.frame(table(D.vis$treat_list_CRSV, D.vis$list1_CRSV))
names(DIST)<- c("LETreatment", "ResponseValue", "Frequency")
DIST1 <- data.frame(cbind(DIST[DIST$LETreatment==0,c("ResponseValue","Frequency")], DIST[DIST$LETreatment==1,"Frequency"]))
names(DIST1)[2:3] <- c("FrequencyControl", "FrequencyTreatment")

temptab <- table(D.vis$treat_list_CRSV, D.vis$list1_CRSV)
PROP <- data.frame(prop.table(temptab, margin=1))
names(PROP)<- c("LETreatment", "ResponseValue", "Proportion")
PROP1 <- data.frame(cbind(PROP[PROP$LETreatment==0,c("ResponseValue","Proportion")], PROP[PROP$LETreatment==1,"Proportion"]))
names(PROP1)[2:3] <- c("ProportionControl", "ProportionTreatment")


DistTab <- data.frame(DIST1$FrequencyControl, PROP1$ProportionControl, DIST1$FrequencyTreatment, PROP1$ProportionTreatment)
row.names(DistTab) <- DIST1$ResponseValue
names(DistTab) <- c("Frequency_Control", "Proportion_Control", "Frequency_Treatment", "Proportion_Treatment")

# produce table
kable(DistTab, row.names = TRUE, format="html", digits=3, caption="Distribution of LE Responses") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size=10) 


########################################
##### Appendix Section 9.2.2, Table A31
########################################
#  runs design effects and reformulate the 
# design effects table for readability (table 6 from Blair and Imai)

DESEFF <- ict.test(y = D.vis$list1_CRSV, treat = D.vis$treat_list_CRSV, J = 3, gms= TRUE, pi.table=TRUE)
DESEFF$p
DESEFFtab <- data.frame(DESEFF$pi.table)
#estimated respondent types for the list experiment
DESEFFtab2 <- cbind(DESEFFtab$est.[5:8], DESEFFtab$s.e.[5:8], DESEFFtab$est.[1:4], DESEFFtab$s.e.[1:4])
DESEFFtab3 <- data.frame(rbind(DESEFFtab2, c(sum(DESEFFtab$est.[5:8]), NA, sum(DESEFFtab$est.[1:4]), NA )))
row.names(DESEFFtab3) <- c("ControlItem=0","ControlItem=1","ControlItem=2","ControlItem=3", "Total")
names(DESEFFtab3) <- c("PropSensitiveEst=0", "s.e.","PropSensitiveEst=1", "s.e.")

# produce table
kable(DESEFFtab3, format="html", row.names = TRUE, booktabs = TRUE, digits=3, caption = "Estimated respondent types for the list experiment") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size=10) 


#alternative (gms=FALSE) as reported in appendix text Section 9.2.2 
ict.test(y = D.vis$list1_CRSV, treat = D.vis$treat_list_CRSV, J = 3, gms= FALSE, pi.table=TRUE) 

########################################
##### Appendix Section 9.3.2, Figure A2
########################################

### Parallel Lines Test Calculations/coding

D$treat_list_CRSV <- 
  factor(D$treat_list_CRSV, levels=c(1,0),
         labels=c("List Experiment Treatment =1", 
                  "List Experiment Treatment =0"))

table(D$murder_yes, useNA = 'ifany')
table(D$leavehome_yes, useNA = 'ifany')
table(D$burn_yes, useNA = 'ifany')
table(D$list1_CRSV, useNA = 'ifany')

# code variable adding up all three approximations of the non-sensitive item
D$sum.directs <- D$murder_yes+D$leavehome_yes+D$burn_yes
crosstab <- table(D$sum.directs, D$list1_CRSV)
crosstab

#look at cross tab in list experiment control group only
table(D$list1_CRSV[D$treat_list_CRSV==0], D$sum.directs[D$treat_list_CRSV==0])

#produce Figure A2
FigureA2 <- ggplot(data=D, mapping=aes(sum.directs, list1_CRSV, color=as.factor(treat_list_CRSV), fill=as.factor(treat_list_CRSV))) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_minimal() +
  labs(x='Direct Non-Sensitive Responses', y='List Experiment Responses', caption = "by LE Treatment and Control") + theme(legend.title=element_blank())


jpeg("FigureA2.jpeg", units="in", width=5, height=4, res=300)
FigureA2
dev.off()

########################################
##### Appendix Section 9.3.3, Table A33
########################################

### Ceiling and Floor ML models no covariates

empty.ml <- ictreg(list1_CRSV ~ 1, treat = "treat_list_CRSV",
                   J = 3, data = D.vis, method = "ml", fit.start="nls")

empty.ceiling.ml <- ictreg(list1_CRSV ~ 1, treat = "treat_list_CRSV",
                           J = 3, data = D.vis, method = "ml", fit.start="nls",
                           ceiling = TRUE, ceiling.fit = "bayesglm", 
                           ceiling.formula = ~ 1)

empty.floor.ml <- ictreg(list1_CRSV ~ 1, treat = "treat_list_CRSV",
                         J = 3, data = D.vis, method = "ml", fit.start="nls",
                         floor = TRUE, floor.fit = "bayesglm", 
                         floor.formula = ~ 1)


summary(empty.ml,boundary.proportions = T)
summary(empty.ceiling.ml,boundary.proportions = T)
summary(empty.floor.ml,boundary.proportions = T)

### Put table together
ceiling.table1 <- rbind(
  cbind(
    empty.ml$par.treat,
    empty.ml$se.treat),
  
  cbind(
    empty.ceiling.ml$par.treat,
    empty.ceiling.ml$se.treat),
  
  cbind(
    empty.floor.ml$par.treat,
    empty.floor.ml$se.treat)
)

ceiling.table1 <- data.frame(round(ceiling.table1, 3))

names(ceiling.table1) <- c("Estimate", "Standard Error")
row.names(ceiling.table1) <- c("ML", "ML w Ceiling", "ML w Floor")

# produce appendix table
kable(ceiling.table1, row.names = TRUE, format="html", caption="Empty Models with and without Ceiling or Floor Effects") %>%
  kable_styling(latex_options = c("striped","hold_position", "repeat_header"), font_size=10)

## Calculation of estimated 3.6 percent of liars as reported in Appendix Section 9.3.3
## Is found in a separate command file 08_Appendix_Sensitivity.R.

########################################
##### Appendix Section 9.3.4, Figure A3
########################################

## See command file 08_Appendix_Sensitivity.R.
## Note that the data for sensitivity analyses draw from data frame D (data frame that is not subsetted) 
## Appendix Table A33 uses data frame D.vis (dataset subsetted for a single outcome),
## We estimate and report the sensitivity analyses using analyses data frame D
## Since this gives a slightly larger estimate of floor effects
## which would bias against our findings.

########################################
##### Appendix Section 9.4, Table A34
########################################

## Additional Statistical Tests of Monotonicity, No Liars and No Design Effects 
## From Aronow et al. (2015)

# Re-estimate our estimate of CRSV using their provided functions for robustness. The estimates are the same as our previously calculated direct and list experiment estimates.

# V = List response
# Z = Treatment Assignment
# Y = Direct Question Response

### Table 3, Columns 1-2
direct_est <- function(Y){
  est <- mean(Y)
  return(est)
}

direct_var_est <- function(Y){
  var_est <- var(Y)/length(Y)
  return(var_est)
}

direct_est(Y=D.vis$rape_yes)
direct_var_est(Y=D.vis$rape_yes)

###Table 3, columns 3-4
conv_est <- function(V, Z) {
  est <- mean(V[Z==1]) - mean(V[Z==0])
  return(est)
}

conv_var_est<- function(V, Z){
  var_est <- (var(V[Z==1])/sum(Z) + var(V[Z==0])/sum(1-Z))
  return(var_est)
}

conv_est(V=D.vis$list1_CRSV, Z=D.vis$treat_list_CRSV)
conv_var_est(V=D.vis$list1_CRSV, Z=D.vis$treat_list_CRSV)

###Table 3, columns 5-6
combined_est <- function(V,Z,Y) {
  est <- mean(Y) + mean(1-Y)*(mean(V[Z==1 & Y == 0]) - mean(V[Z==0 & Y == 0]))
  return(est)
}

combined_var_est <- function(V,Z,Y){
  mu_hat <- combined_est(V = V, Z = Z, Y = Y)
  Y_bar <- direct_est(Y = Y)
  var_Z_1 <- var(V[Z==1 & Y==0])
  var_Z_0 <- var(V[Z==0 & Y==0])
  gamma_hat <- mean(Z)
  var_est <- ((((1-mu_hat)^2)/(1-Y_bar))*Y_bar + 
                (1-Y_bar)*(var_Z_1/gamma_hat + var_Z_0/(1-gamma_hat)))/length(V)
  return(var_est)
}

combined_est(V=D.vis$list1_CRSV, Z=D.vis$treat_list_CRSV, Y=D.vis$rape_yes)
combined_var_est(V=D.vis$list1_CRSV, Z=D.vis$treat_list_CRSV, Y=D.vis$rape_yes)

improve_var <- function(V, Z, Y) {
  return((1 - combined_var_est(V=V,Z = Z,Y = Y) /conv_var_est(V = V, Z = Z))*100)
}

improve_var(V=D.vis$list1_CRSV, Z=D.vis$treat_list_CRSV, Y=D.vis$rape_yes)

##output for Table 3 columns all together
whole_shebang <- function(V,Z,Y){
  return(c(direct_est(Y = Y), direct_var_est(Y = Y)^0.5,
           conv_est(V = V, Z = Z), conv_var_est(V = V, Z = Z)^0.5,
           combined_est(V = V, Z = Z,Y = Y), combined_var_est(V = V, Z = Z,Y = Y)^0.5,
           improve_var(V = V, Z = Z,Y = Y)))
}

whole_shebang(V=D.vis$list1_CRSV, Z=D.vis$treat_list_CRSV, Y=D.vis$rape_yes)


## Placebo Tests, Joint Null hypothesis is Monotonicity, No Liars and No Design Effects
## Reject null hypothesis if statistically significant

placeboI_test <- function(V,Z,Y){
  treated_admitters <- V[Y==1 & Z == 1]
  control_admitters <- V[Y==1 & Z == 0]
  est <- mean(treated_admitters) - mean(control_admitters)
  var <- (var(treated_admitters))/length(treated_admitters) + (var(control_admitters))/length(control_admitters)
  se <- sqrt(var)
  p_value <- 2*pnorm(- abs(est -1)/ se )
  n <- length(treated_admitters ) + length(control_admitters)
  return(c(est=est, se=se, p_value=p_value, n=n))
}

P1 <- round(placeboI_test(V=D.vis$list1_CRSV, Z=D.vis$treat_list_CRSV, Y=D.vis$rape_yes),4)

names(P1) <- c("Estimate", "Standard Error", "p-value", "n")
P1 <- round(P1,3)

## Checks whether treatment assignment affects direct question responses (not sure of our order if necessary)
## No significant effects means not affected by treatment
placeboII_test <- function(Z,Y){
  delta_hat <- sum(Y*Z)/sum(Z) - sum(Y*(1-Z))/sum(1-Z)
  var_Z_1 <- var(Y[Z==1])
  var_Z_0 <- var(Y[Z==0])
  var_delta_hat <- var_Z_1/sum(Z) + var_Z_0/sum(1-Z) 
  se_delta_hat <- sqrt(var_delta_hat)
  p_value <- 2*pnorm(- abs(delta_hat)/se_delta_hat)
  n = length(Y)
  return(c(est=delta_hat, se=se_delta_hat, p_value=p_value, n=n))
}


P2 <- placeboII_test(Z=D.vis$treat_list_CRSV, Y=D.vis$rape_yes)
names(P2) <- c("Estimate", "Standard Error", "p-value", "n")
P2<- round(P2,3)

#produce as single table (A34)
P3 <- data.frame(t(rbind(P1, P2)))
names(P3) <- c("Test I", "Test II")
kable(P3,caption = "Placebo Tests", format="html") %>%
  kable_styling(latex_options = c("striped","hold_position", "repeat_header"), font_size=10)


########################################
##### Appendix Section 9.6, Figure A4
########################################

## Estimate CRSV using different methods
## Linear, NLS and Maximum likelihood
## Creates comparison table used for Appendix plot
## Conducts Hausman test of difference between ML and NLS

estimate.LM <- ictreg(list1_CRSV ~ 1, data = D.vis,
                      treat = "treat_list_CRSV", J=3, method = "lm")

estimate.NLS <- ictreg(list1_CRSV ~ 1, data = D.vis,
                       treat = "treat_list_CRSV", J=3, method = "nls")

estimate.ML <- ictreg(list1_CRSV ~ 1, data = D.vis,
                      treat = "treat_list_CRSV", J=3, method = "ml")


## Also estimate with Territoire Fixed Effects

territoire.fac <- as.factor(D.vis$territoire)  ## turn into factor variable

estimate.LM.COV <- ictreg(list1_CRSV ~ vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + exchange_prev + territoire.fac, data = D.vis,
                          treat = "treat_list_CRSV", J=3, method = "lm")


estimate.NLS.COV <- ictreg(list1_CRSV ~ vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + exchange_prev, data = D.vis,
                           treat = "treat_list_CRSV", J=3, method = "nls")


estimate.ML.COV <- ictreg(list1_CRSV ~ vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + exchange_prev + territoire.fac, data = D.vis,
                          treat = "treat_list_CRSV", J=3, method = "ml")



EST.LM <- data.frame(Estimate = c("sensitive.(Intercept)"),
                     Coefficient = c(predict(estimate.LM, se.fit=TRUE, avg = TRUE)$fit),
                     SE = c(predict(estimate.LM, se.fit=TRUE, avg = TRUE)$se),
                     modelName = "Linear")

EST.NLS <- data.frame(Estimate = c("sensitive.(Intercept)"),
                      Coefficient = c(predict(estimate.NLS, se.fit=TRUE, avg = TRUE)$fit),
                      SE = c(predict(estimate.NLS, se.fit=TRUE, avg = TRUE)$se),
                      modelName = "NLS")

EST.ML <- data.frame(Estimate = c("sensitive.(Intercept)"),
                     Coefficient = c(predict(estimate.ML, se.fit=TRUE, avg = TRUE)$fit),
                     SE = c(predict(estimate.ML, se.fit=TRUE, avg = TRUE)$se),
                     modelName = "ML")


EST.LM.COV <- data.frame(Estimate = c("sensitive.(Intercept)"),
                         Coefficient = c(predict(estimate.LM.COV, se.fit=TRUE, avg = TRUE)$fit),
                         SE = c(predict(estimate.LM.COV, se.fit=TRUE, avg = TRUE)$se),
                         modelName = "Linear COV")

EST.NLS.COV <- data.frame(Estimate = c("sensitive.(Intercept)"),
                          Coefficient = c(predict(estimate.NLS.COV, se.fit=TRUE, avg = TRUE)$fit),
                          SE = c(predict(estimate.NLS.COV, se.fit=TRUE, avg = TRUE)$se),
                          modelName = "NLS COV")

EST.ML.COV <- data.frame(Estimate = c("sensitive.(Intercept)"),
                         Coefficient = c(predict(estimate.ML.COV, se.fit=TRUE, avg = TRUE)$fit),
                         SE = c(predict(estimate.ML.COV, se.fit=TRUE, avg = TRUE)$se),
                         modelName = "ML COV")


EST.MOD2.COMPAREALL <- data.frame(rbind(EST.LM, EST.NLS, EST.ML, EST.LM.COV, EST.NLS.COV, EST.ML.COV))

# Table A35
kable(EST.MOD2.COMPAREALL[,c(4,2,3)], row.names=TRUE, format = "html", caption = "Difference in Means Estimates", digits=4)%>% 
  kable_styling(latex_options = c("hold_position"))

# Figure A4
FigureA4 <- ggplot(EST.MOD2.COMPAREALL, aes(colour = modelName, ymax=.5)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = Estimate, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = Estimate, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  geom_text(aes(x=Estimate,y=Coefficient, label=as.character(round(Coefficient,2))), colour="gray20", alpha=1.5, size=3, nudge_x = .25, nudge_y = 0, check_overlap = T) + 
  theme(legend.position="top")+
  coord_flip() + theme_bw() + theme(legend.position = "top")+
  scale_color_manual(values=c('pink','red','light blue', 'dark blue', 'light green', 'dark green')) + 
  theme(legend.title=element_blank(), axis.title.y = element_blank(), axis.text.y=element_blank()) 

jpeg("FigureA4.jpeg", units="in", width=5, height=3, res=300)
FigureA4
dev.off()

# Hausman Test of difference between ML and NLS (as reported in Section 9.6 appendix text)
# estimate model that approximates the model used in outcome analyses
estimate.ML.COV.NOTERR <- ictreg(list1_CRSV ~ vio_witness1 + murder_yes + female + age + edu_level + hh_size + assets_sum + exchange_prev , data = D.vis,
                                 treat = "treat_list_CRSV", J=3, method = "ml")

# compares ml and nls both with no territory fixed effects
hausman.tab <- ict.hausman.test(estimate.NLS.COV, estimate.ML.COV.NOTERR, psd = FALSE, abs = TRUE) 
hausman.tab # as reported in Appendix text section 9.6
