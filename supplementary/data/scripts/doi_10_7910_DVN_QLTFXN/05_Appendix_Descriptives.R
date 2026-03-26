## This file calculates descriptive statistics and balance statistics
## that appear in Appendix Section 4 (Descriptive Statistics) 
## and Appendix Section 5 (Balance on Determinants of Exposure) 
## This file produces Appendix Tables A1-A6


# this file is designed to run after the following files
# uncomment to run previous coding files
# source("00_Packages.R")
# source("01_DataVariables.R")
# source("02_MainAnalyses_Estimation.R")
# source("03_MainAnalyses_Mechanisms.R")
# source("04_MainAnalyses_Outcomes.R")

###########################################
###### Appendix 4: Descriptive Statistics
###########################################

###########################################
###### Appendix Table A1
###########################################

table(D$rape_yes, useNA = "ifany")
prop.table(table(D$rape_yes, useNA = "ifany"))

se.direct <- sqrt(mean(D$rape_yes, na.rm = TRUE) * (1 - mean(D$rape_yes, na.rm=TRUE)) / nrow(D))
direct.lower.p <- mean(D$rape_yes, na.rm = TRUE) - se.direct*1.96
direct.upper.p <- mean(D$rape_yes, na.rm = TRUE) + se.direct*1.96
cbind(direct.lower.p, mean(D$rape_yes, na.rm = TRUE), direct.upper.p) # mean and confidence interval

# List Experiment calculation for Appendix Table A1
table(D$list1_CRSV[D$treat_list_CRSV == 1], useNA = "ifany")
table(D$list1_CRSV[D$treat_list_CRSV == 0], useNA = "ifany")

table(is.na(D$list1_CRSV[D$treat_list_CRSV == 1])) #no missing values
table(is.na(D$list1_CRSV[D$treat_list_CRSV == 0])) #no missing values

se.list <- sqrt(var(D$list1_CRSV[D$treat_list_CRSV == 1], na.rm = T)/nrow(D[D$treat_list_CRSV == 1,]) + var(D$list1_CRSV[D$treat_list_CRSV == 0], na.rm = T)/(nrow(D[D$treat_list_CRSV == 0,])))
list.est <- mean(D$list1_CRSV[D$treat_list_CRSV == 1], na.rm = T) - mean(D$list1_CRSV[D$treat_list_CRSV == 0], na.rm = T)
list.lower.p <- list.est - se.list*1.96
list.upper.p <- list.est + se.list*1.96
cbind(list.lower.p, list.est, list.upper.p)

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

estimates <- data.frame(rbind(cbind(list.lower.p, list.est, list.upper.p), 
                              cbind(direct.lower.p, mean(D$rape_yes, na.rm = TRUE), direct.upper.p)))
row.names(estimates) <- c("list", "direct")
names(estimates) <- c("lower", "estimate", "upper")
estimates$measure <- c("list", "direct")


## produce Table A1 
kable(estimates[1:3], row.names=TRUE, format = "html", caption = "Difference in Means Estimates")%>% 
  kable_styling(latex_options = c("hold_position"))

###########################################
###### Appendix tables A2, A3, and A4
###########################################

# turn relevant variables into factor variables and label variables

D$female <- 
  factor(D$female, levels=c(1,0),
         labels=c("Female", 
                  "Male"))

label(D$age) <- "Age"
label(D$edu_level) <- "Education Level"
label(D$hh_size) <- "Household Size"
label(D$assets_sum) <- "Assets"
label(D$vio_witness1) <- "Witnessing CRSV"

label(D$rape_yes) <- "CRSV Exposure (Direct)"
label(D$ingroup_visit_mean) <- "Freq. Personal Exchanges"
label(D$org_leader_d) <- "Leader of Organization"
label(D$org_member_r1) <- "Memberships in Organizations"
label(D$event_com_mean) <- "Freq. Engagment in Events"
label(D$donate_amount) <- "Public Goods Contribution"

label(D$stigma_anticip_m) <- "Anticipated Stigma"
label(D$stigma_internal_r1) <- "Internal Stigma, Self-Blame"
label(D$ptg_all) <- "Post-Traumatic Growth"
label(D$leavehome_yes) <- "Displacement"
label(D$hhhum_assist_d) <- "Receipt of Humanitarian Aid"
label(D$murder_yes) <- "Homicide"
label(D$exchange_prev) <- "Previous Social Exchange"



# Appendix Table A2

desctable1 <- table1(~rape_yes  + age + edu_level + hh_size + vio_witness1 + assets_sum + exchange_prev + murder_yes | female, data = D, 
                     caption = "Descriptive Statistics for Independent and Control Variables by Respondent Gender")

t1kable(desctable1, format = "html")%>% 
  kable_styling(latex_options = c("hold_position"))


# Appendix Table A3

desctable3 <- table1(~stigma_anticip_m + stigma_internal_r1 + ptg_all 
                     + leavehome_yes | female, data = D, 
                     caption = "Descriptive Statistics of Mechanisms by Respondent Gender")

t1kable(desctable3, format = "html")%>% 
  kable_styling(latex_options = c("hold_position"))


# Appendix Table A4

desctable2 <- table1(~ingroup_visit_mean + org_leader_d + org_member_r1 
                     + event_com_mean + donate_amount | female, data = D, 
                     caption = "Descriptive Statistics for Outcome Variables by Respondent Gender")

t1kable(desctable2, format = "html")%>% 
  kable_styling(latex_options = c("hold_position"))

###########################################
###### Appendix 5: Balance on Determinants of Exposure
###########################################

#############################
#### Create Appendix Table A5
#############################

######## balance on determinants of exposure

# SUBSET DATA 
## direct measure, rape_yes
Ddirect1 <- subset(D.bal, rape_yes == 1)
Ddirect0 <- subset(D.bal, rape_yes == 0)

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




# Assessing balance on key potential determinants of CRSV
# Using the list experiment measure
# (Education and Assets). No relationship supports the
# idea that households were not targeted based on
# pre-exposure social activity.

#empty frame
balanceDF2 <- data.frame("Variable Name" = character(),  "CRSV=1" = integer(), "CRSV=0" = integer(),
                         "Difference" = integer(), "p-value" = integer(), stringsAsFactors = FALSE) 

#fill table with loop
for (nVar in 1:length(vars)) {
  balanceDF2[nVar,1] <- sprintf("%s",vars[nVar])
  balanceDF2[nVar,2:5] <- round(ttestoutput(Ddirect1, Ddirect0, sprintf("%s",vars[nVar])),4)
}

# name variables
balanceDF2$Variable.Name <- c("Female", "Head of Household", "Age", "Education", "Farmer", "Married",
                              "Household Size", "Monthly Income", "Assets", "Previous Social Exchange", "Previous Social Activity",
                              "Dist. to Village", "Dist. from Mines", "Dist. from Armed Groups", "Participated in Survey Before", 
                              "Village Population", "Rape in Village", "Rape in Village(7yr)", "Rape in Village (15yr)")

colnames(balanceDF2) <- c("Variable Name", "CRSV=1", "CRSV=0", "Difference", "p-value")

observ2 <- c("Observations", nrow(Ddirect1), nrow(Ddirect0), NA, NA)

balanceDF2 <- rbind(balanceDF2, observ2)

# produce table
stargazer(balanceDF2, summary=FALSE, rownames = FALSE, type = "text", title = "Balance Table for Direct Measure of CRSV", header=FALSE)

#############################
#### Create Appendix Table A6
#############################

# Assessing balance on key potential determinants of CRSV
# Using the list experiment measure
# (Education and Assets). No relationship supports the
# idea that households were not targeted based on
# pre-exposure social activity.

balance.list <- ictreg(formula = list1_CRSV ~ edu_level + 
                         assets_sum, data = D, treat = "treat_list_CRSV", J = 3, constrained = TRUE, method='ml' )

balance.list <- data.frame(balance.list$par.treat, balance.list$se.treat)
balance.list <- round(balance.list, 3)
row.names(balance.list) <- c("Intercept", "Edu", "Assets")
names(balance.list) <- c("Estimate", "Std. Error")

#produce table
kable(balance.list, format="html", caption = "Correlates with List Experiment Measure of CRSV", row.names = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))
