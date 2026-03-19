### Replication code
### Article: "How patronage delivers: Political appointments, bureaucratic accountability, and service delivery in Brazil"
### Author: Guillermo Toral (www.guillermotoral.com)
### Date: June 2022
### This file analyzes the bureaucrat survey data, generating plots and tables
### R version, platform, and package versions reported at the end of the file

# Prepare the environment -------------------------------------------------

### This section of the code prepares the environment 

# Clean the environment
rm(list = ls())

# Install required packages if not previously installed
package_list <- c("tidyverse", "here", "texreg", "xtable", "sandwich", "lmtest") 
packages_to_install <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(packages_to_install)>0){
  install.packages(packages_to_install)
}

# Load required packages
library(tidyverse)
library(here)
library(texreg)
library(xtable)
library(sandwich)
library(lmtest)

# Set Working Directory to wherever this file is located.
setwd(here())
# The directory where the code folder is located must also have a "plots" and a "tables" subdirectory

# Load dataset ------------------------------------------------------------

b <- read_csv("../../datasets/analysis/survey_bureaucrats/survey_bureaucrats_analysis.csv")

# Generate Figure 6-left (meetings by selection mode) --------

# Exclude respondents who report more mixed appointment systems
bb <- subset(b, b$appointed==1 | b$elected==1 | b$civil_service==1)

# Run regressions where the DV corresponds to meetings with different actors
m1 <- lm(log(meetings_with_mayor+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m2 <- lm(log(meetings_with_secretary+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m3 <- lm(log(meetings_with_technicians+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m4 <- lm(log(meetings_with_citycouncilor+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m5 <- lm(log(meetings_with_professionals+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m6 <- lm(log(meetings_with_clients+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)

# Define vector with the names of stakeholders, for the plot
stakeholders <- rev(c("The mayor", "The secretary", "Technicians", "City councilors", "Professionals", "Clients"))

# Define the critical value
critical_value <- qt(.975, df=m1$df)

# Create a table with regression coefficients, HC1 standard errors, and confidence intervals for appointed managers
beta <- rev(c(coeftest(m1,vcov=vcovHC(m1,type="HC1"))["appointed",1], coeftest(m2,vcov=vcovHC(m2,type="HC1"))["appointed",1],coeftest(m3,vcov=vcovHC(m3,type="HC1"))["appointed",1],coeftest(m4,vcov=vcovHC(m4,type="HC1"))["appointed",1],coeftest(m5,vcov=vcovHC(m5,type="HC1"))["appointed",1],coeftest(m6,vcov=vcovHC(m6,type="HC1"))["appointed",1]))
se <- rev(c(coeftest(m1,vcov=vcovHC(m1,type="HC1"))["appointed",2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))["appointed",2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))["appointed",2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))["appointed",2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))["appointed",2],coeftest(m6,vcov=vcovHC(m6,type="HC1"))["appointed",2]))
results_appointed <- as_tibble(cbind(stakeholders, beta, se))
results_appointed <- results_appointed %>%
  mutate(beta = as.numeric(beta), se = as.numeric(se),
         cilow = beta-critical_value*se, ciup=beta+critical_value*se)

# Create a table with regression coefficients, HC1 standard errors, and confidence intervals for elected managers
beta <- rev(c(coeftest(m1,vcov=vcovHC(m1,type="HC1"))["elected",1], coeftest(m2,vcov=vcovHC(m2,type="HC1"))["elected",1],coeftest(m3,vcov=vcovHC(m3,type="HC1"))["elected",1],coeftest(m4,vcov=vcovHC(m4,type="HC1"))["elected",1],coeftest(m5,vcov=vcovHC(m5,type="HC1"))["elected",1],coeftest(m6,vcov=vcovHC(m6,type="HC1"))["elected",1]))
se <- rev(c(coeftest(m1,vcov=vcovHC(m1,type="HC1"))["elected",2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))["elected",2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))["elected",2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))["elected",2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))["elected",2],coeftest(m6,vcov=vcovHC(m6,type="HC1"))["elected",2]))
results_elected <- as_tibble(cbind(stakeholders, beta, se))
results_elected <- results_elected %>%
  mutate(beta = as.numeric(beta), se = as.numeric(se),
         cilow = beta-critical_value*se, ciup=beta+critical_value*se)

# Generate the plot
pdf("../../plots/correlates_meetings.pdf", width=6.5, height=5.5)
par(mar=c(5, 12, 2, 2))
plot(NULL, ylim = c(0.5,6.5), xlim = c(-0.3, 1.65), yaxt = "n",
     xlab = "Coefficients and 95% confidence intervals", ylab = "")
     # main="DV: Logged number of meetings with..."
grid()
abline(v=0, col="gray", lty=1,lwd=1.5)
axis(2, at=c(1:6),labels=results_appointed$stakeholders, las=2, cex=.8)
segments(y0=c(1.1:6.1),x0 = results_appointed$cilow, x1 = results_appointed$ciup,
         col = "black", lwd = 2)
points(y = c(1.1:6.1), x = results_appointed$beta, pch = 16, col="black", cex=1)
segments(y0=c(1:6),x0 = results_elected$cilow, x1 = results_elected$ciup,
         col = "red", lwd = 2)
points(y = c(1:6), x = results_elected$beta, pch = 17, col="red", cex=1)
points(y = c(0.9:5.9), x = rep(0,6), pch = 15, col="blue", cex=1)
legend("bottomright", c("Appointed", "Elected", "Civil service \n (baseline)"), xpd=T, horiz=F, lty=1, pch=c(16,17,15), col=c("black", "red", "blue"), bty="n",cex=1)
dev.off()

# Generate Figure 6-right (attitudes by selection mode) --------

# Exclude respondents who report more mixed appointment systems
bb <- subset(b, b$appointed==1 | b$elected==1 | b$civil_service==1)

# Run regressions where the DV corresponds to different survey items on attitudes about the mayor and the secretary
m1 <- lm(trust_mayor ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)
m2 <- lm(proximity_to_mayor ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m3 <- lm(alignment_mayor_professionals ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m4 <- lm(mayor_concerned ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m5 <- lm(trust_secretary ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)
m6 <- lm(proximity_to_secretary ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)

# Define vector with outcome names
attitudes <- rev(c("'I trust the mayor'", "'I feel close to the mayor'", "'The mayor and professionals \n have the same priorities'", "'The mayor is concerned \n with improving services'", "'I trust the secretary'", "'I feel close to the secretary'"))

# Define critical value
critical_value <- qt(.975, df=m1$df)

# Create a table with regression coefficients, HC1 standard errors, and confidence intervals for appointed managers
beta <- rev(c(coeftest(m1,vcov=vcovHC(m1,type="HC1"))["appointed",1], coeftest(m2,vcov=vcovHC(m2,type="HC1"))["appointed",1],coeftest(m3,vcov=vcovHC(m3,type="HC1"))["appointed",1],coeftest(m4,vcov=vcovHC(m4,type="HC1"))["appointed",1],coeftest(m5,vcov=vcovHC(m5,type="HC1"))["appointed",1],coeftest(m6,vcov=vcovHC(m6,type="HC1"))["appointed",1]))
se <- rev(c(coeftest(m1,vcov=vcovHC(m1,type="HC1"))["appointed",2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))["appointed",2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))["appointed",2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))["appointed",2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))["appointed",2],coeftest(m6,vcov=vcovHC(m6,type="HC1"))["appointed",2]))
results_appointed <- as_tibble(cbind(attitudes, beta, se))
results_appointed <- results_appointed %>%
  mutate(beta = as.numeric(beta), se = as.numeric(se),
         cilow = beta-critical_value*se, ciup=beta+critical_value*se)

# Create a table with regression coefficients, HC1 standard errors, and confidence intervals for elected managers
beta <- rev(c(coeftest(m1,vcov=vcovHC(m1,type="HC1"))["elected",1], coeftest(m2,vcov=vcovHC(m2,type="HC1"))["elected",1],coeftest(m3,vcov=vcovHC(m3,type="HC1"))["elected",1],coeftest(m4,vcov=vcovHC(m4,type="HC1"))["elected",1],coeftest(m5,vcov=vcovHC(m5,type="HC1"))["elected",1],coeftest(m6,vcov=vcovHC(m6,type="HC1"))["elected",1]))
se <- rev(c(coeftest(m1,vcov=vcovHC(m1,type="HC1"))["elected",2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))["elected",2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))["elected",2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))["elected",2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))["elected",2],coeftest(m6,vcov=vcovHC(m6,type="HC1"))["elected",2]))
results_elected <- as_tibble(cbind(attitudes, beta, se))
results_elected <- results_elected %>%
  mutate(beta = as.numeric(beta), se = as.numeric(se),
         cilow = beta-critical_value*se, ciup=beta+critical_value*se)

# Generate and export plot
pdf("../../plots/correlates_attitudes.pdf", width=6.5, height=5.5)
par(mar=c(5, 12, 2, 2))
plot(NULL, ylim = c(0.5,6.5), xlim = c(-0.3, 1.65), yaxt = "n",
     xlab = "Coefficients and 95% confidence intervals", ylab = "")
    # main="DV: Agreement with...")
grid()
abline(v=0, col="gray", lty=1,lwd=1.5)
axis(2, at=c(1:6),labels=results_appointed$attitudes, las=2, cex=.8)
segments(y0=c(1.1:6.1),x0 = results_appointed$cilow, x1 = results_appointed$ciup,
         col = "black", lwd = 2)
points(y = c(1.1:6.1), x = results_appointed$beta, pch = 16, col="black", cex=1)
segments(y0=c(1:6),x0 = results_elected$cilow, x1 = results_elected$ciup,
         col = "red", lwd = 2)
points(y = c(1:6), x = results_elected$beta, pch = 17, col="red", cex=1)
points(y = c(0.9:5.9), x = rep(0,6), pch = 15, col="blue", cex=1)
dev.off()

# SUPLEMENTARY ANALYSES (reported in Online Appendix) ---------------------
# Generate Table 20 (descriptive statistics) ------------------------------

# Create vector with list of variables
var <- c("age", "female", "education_highschool_orless", "education_college", "appointed", "elected", "civil_service", "experience_manager", "experience_professional", "no_other_jobs", "union_member", "party_member", "worked_for_campaign")

# Create empty vectors to store mean and standard deviation for each variable
average <- c()
stddev <- c()
data <- b # Do it first for the whole sample
for(i in var){ # For each variable, store the mean and the SD
  average <- c(average, mean(data[,i][[1]],na.rm=T))
  stddev <- c(stddev, sd(data[,i][[1]],na.rm=T))
}
all <- cbind(average, stddev) # Create a table with the mean and SD of each covariate

# Same, for respondents in the education sector
average <- c()
stddev <- c()
data <- subset(b,b$sector=="education")
for(i in var){
  average <- c(average, mean(data[,i][[1]],na.rm=T))
  stddev <- c(stddev, sd(data[,i][[1]],na.rm=T))
}
education <- cbind(average, stddev)

# Same, for respondents in the healthcare sector
average <- c()
stddev <- c()
data <- subset(b,b$sector=="healthcare")
for(i in var){
  average <- c(average, mean(data[,i][[1]],na.rm=T))
  stddev <- c(stddev, sd(data[,i][[1]],na.rm=T))
}
healthcare <- cbind(average, stddev)

# Same, for respondents in the social assistance sector
average <- c()
stddev <- c()
data <- subset(b,b$sector=="social assistance")
for(i in var){
  average <- c(average, mean(data[,i][[1]],na.rm=T))
  stddev <- c(stddev, sd(data[,i][[1]],na.rm=T))
}
socialassistance <- cbind(average, stddev)

# Bind all descriptive statistics in a single table, giving names to rows and columns
descriptive <- cbind(all, education, healthcare, socialassistance)
rownames(descriptive) <- c("Age", "Female", "High school degree or less", "College degree", "Politically appointed", "Elected","Civil service", "Experience as a manger", "Experience as a professional", "Exclusive dedication", "Union member", "Party member", "Worked for a campaign")
colnames(descriptive) <- rep(c("Mean", "SD"),4)

# Export the table
print(file="../../tables/bur_descriptives.tex",
      only.contents=TRUE,
      type="latex",
      xtable(descriptive, align=c("lcccccccc"), digits=3),caption.placement="top", 
      rowname=c("Age", "Female", "High school degree or less", "College degree", "Politically appointed", "Elected", "Selected", "Civil service", "Experience as a manger", "Experience as a professional", "Hours worked per week", "Exclusive dedication", "Union member", "Party member", "Worked for a campaign"), 
      comment=F,floating=F,booktabs=F)

# Generate Table 21 (correlates of appointment mode) ------------------------

# Run regressions, where each DV corresponds to a selection mode
m1 <- lm(appointed ~ party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + education_lessthan_college + as.factor(sector), data=b)
m2 <- lm(elected ~ party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + education_lessthan_college + as.factor(sector), data=b)
m3 <- lm(civil_service ~ party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + education_lessthan_college + as.factor(sector), data=b)

# Export results, using HC1 standard errors
texreg(list(m1,m2,m3),
       file="../../tables/bur_appointments.tex", table=T, float.pos="htp", scalebox=.75,
       digits=3,
       override.se = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,2]),
       override.pvalues = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,4], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,4],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4]),
       booktabs = T, use.packages=F,
       custom.model.names = c("Appointed", "Elected", "Civil service"),
       custom.coef.map = list("party_member" = "Party member", "worked_for_campaign" = "Has worked for an electoral campaign","union_member" = "Union member", "experience_manager" = "Experience as manager", "experience_professional" = "Experience as professional", "lives_in_municipality" = "Lives in the municipality", "no_other_jobs" = "Has no other jobs", "female" = "Female", "age" = "Age", "education_morethan_collegeTRUE" = "Has more than a college degree", "education_lessthan_college" = "Has less than a college degree", "as.factor(sector)healthcare" = "Healthcare sector (vs education)", "as.factor(sector)social assistance" = "Social assistance (vs education)","(Intercept)" = "Constant"),
       custom.gof.names = c("R-squared", "Observations"),
       reorder.gof = c(2,1),
       include.adjrs=FALSE,
       stars = c(0.05, 0.01, 0.001),
       custom.note = paste("\\item %stars. HC1 standard errors in brackets."),
       caption="Correlates of street-level managers' appointment mode",
       caption.above = TRUE,
       label="tab:bur_appointments",
       single.row = TRUE,
       custom.coef.names=c("Constant", NA))

# Generate Table 22 (meetings by appointment mode) --------

# Exclude respondents who report more mixed appointment systems, to facilitate comparisons
bb <- subset(b, b$appointed==1 | b$elected==1 | b$civil_service==1)

# Run regressions where the DV corresopnds to meetings with different actors
m1 <- lm(log(meetings_with_mayor+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m2 <- lm(log(meetings_with_secretary+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m3 <- lm(log(meetings_with_technicians+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m4 <- lm(log(meetings_with_citycouncilor+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m5 <- lm(log(meetings_with_professionals+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m6 <- lm(log(meetings_with_clients+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)

# Export results, with HC1 standard errors
texreg(list(m1,m2,m3,m4,m5,m6),
       file="../../tables/bur_meetings.tex", table=F,tabular=F, float.pos="htp", scalebox=.75,
       digits=3,
       override.se = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,2],coeftest(m6,vcov=vcovHC(m6,type="HC1"))[,2]),
       override.pvalues = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,4], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,4],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,4],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,4],coeftest(m6,vcov=vcovHC(m6,type="HC1"))[,4]),
       booktabs = F, use.packages=F,
       custom.coef.map = list("appointed" = "Appointed", "elected" = "Elected"),
       custom.gof.names = c("R-squared", "Observations"),
       custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
       reorder.gof = c(2,1),
       include.adjrs=FALSE,
       stars = c(0.05, 0.01, 0.001),
       custom.note = " ",
       single.row = F)

# Generate Table 23 (meetings, appointed vs elected) ---------

# Exclude respondents who report mixed appointment systems or being in the civil service
bb <- subset(b, b$appointed==1 | b$elected==1)

# Run regressions where each DV is the respondent's number of meetings with stakeholders
m1 <- lm(log(meetings_with_mayor+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m2 <- lm(log(meetings_with_secretary+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m3 <- lm(log(meetings_with_technicians+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m4 <- lm(log(meetings_with_citycouncilor+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m5 <- lm(log(meetings_with_professionals+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m6 <- lm(log(meetings_with_clients+1) ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)

# Export results, with HC1 standard errors
texreg(list(m1,m2,m3,m4,m5,m6),
       file="../../tables/bur_meetings_ap_vs_el.tex", table=F,tabular=F, float.pos="htp", scalebox=.75,
       digits=3,
       override.se = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,2],coeftest(m6,vcov=vcovHC(m6,type="HC1"))[,2]),
       override.pvalues = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,4], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,4],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,4],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,4],coeftest(m6,vcov=vcovHC(m6,type="HC1"))[,4]),
       booktabs = F, use.packages=F,
       custom.coef.map = list("appointed" = "Appointed"),
       custom.gof.names = c("R-squared", "Observations"),
       custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
       reorder.gof = c(2,1),
       include.adjrs=FALSE,
       stars = c(0.05, 0.01, 0.001),
       custom.note = " ",
       single.row = F)

# Generate Table 24 (attitudes by appointment mode) --------

# Exclude respondents who report more mixed appointment systems
bb <- subset(b, b$appointed==1 | b$elected==1 | b$civil_service==1)

# Run regressions, where each DV is respondents' answers to questions about the mayor and the secretary
m1 <- lm(trust_mayor ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)
m2 <- lm(proximity_to_mayor ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m3 <- lm(alignment_mayor_professionals ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m4 <- lm(mayor_concerned ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m5 <- lm(trust_secretary ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)
m6 <- lm(proximity_to_secretary ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)

# Export table, using HC1 standard errors
texreg(list(m1,m2,m3,m4,m5,m6),
       file="../../tables/bur_attitudes.tex", table=F,tabular=F, float.pos="htp", scalebox=.75,
       digits=3,
       override.se = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,2],coeftest(m6,vcov=vcovHC(m6,type="HC1"))[,2]),
       override.pvalues = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,4], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,4],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,4],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,4],coeftest(m6,vcov=vcovHC(m6,type="HC1"))[,4]),
       booktabs = F, use.packages=F,
       custom.coef.map = list("appointed" = "Appointed", "elected" = "Elected"),
       custom.gof.names = c("R-squared", "Observations"),
       custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
       reorder.gof = c(2,1),
       include.adjrs=FALSE,
       stars = c(0.05, 0.01, 0.001),
       custom.note = " ",
       single.row = F)

# Generate Table 25 (attitudes, appointed vs elected) --------

# Exclude respondents who report more mixed appointment systems, and those in the civil service
bb <- subset(b, b$appointed==1 | b$elected==1)

# Run regressions, where each DV is respondents' answers to questions about the mayor and the secretary
m1 <- lm(trust_mayor ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)
m2 <- lm(proximity_to_mayor ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m3 <- lm(alignment_mayor_professionals ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m4 <- lm(mayor_concerned ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)
m5 <- lm(trust_secretary ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college + as.factor(sector), data=bb)
m6 <- lm(proximity_to_secretary ~ appointed + elected + party_member + worked_for_campaign + union_member + experience_manager + experience_professional + lives_in_municipality + no_other_jobs + female + age + education_morethan_college  + as.factor(sector), data=bb)

# Export table, using HC1 standard errors
texreg(list(m1,m2,m3,m4,m5,m6),
       file="../../tables/bur_attitudes_ap_vs_el.tex", table=F,tabular=F, float.pos="htp", scalebox=.75,
       digits=3,
       override.se = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,2], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,2],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,2],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,2],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,2],coeftest(m6,vcov=vcovHC(m6,type="HC1"))[,2]),
       override.pvalues = list(coeftest(m1,vcov=vcovHC(m1,type="HC1"))[,4], coeftest(m2,vcov=vcovHC(m2,type="HC1"))[,4],coeftest(m3,vcov=vcovHC(m3,type="HC1"))[,4],coeftest(m4,vcov=vcovHC(m4,type="HC1"))[,4],coeftest(m5,vcov=vcovHC(m5,type="HC1"))[,4],coeftest(m6,vcov=vcovHC(m6,type="HC1"))[,4]),
       booktabs = F, use.packages=F,
       custom.coef.map = list("appointed" = "Appointed", "elected" = "Elected"),
       custom.gof.names = c("R-squared", "Observations"),
       custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
       reorder.gof = c(2,1),
       include.adjrs=FALSE,
       stars = c(0.05, 0.01, 0.001),
       custom.note = " ",
       single.row = F)

# Extract statistics mentioned in the text: Beliefs about political influence on temp hiring --------

### These data are mentioned in the main body of the article, when the politician conjoint experiment is introduced

# 58% believe that politics influences "a lot" temporary hiring
mean(b$political_links_influence_temp_hiring==4,na.rm=T)

# 16% believe that politics influences only "a little" or "not at all"
mean(b$political_links_influence_temp_hiring<3,na.rm=T)


# Extract statistics mentioned in the text: Electoral performance of elected directors ----------------------

### These data are mentioned in Appendix D3

# Elected directors report a median vote share of 90%
summary(b[which(b$sector=="education"),"percent_votes_election"][[1]])

# Over 70% of elected directors report having run unopposed
summary(b[which(b$sector=="education"),"only_one_candidate_election"][[1]])

# NOTES -- R version, platform, and loaded packages -------------------------

### sessionInfo(package = NULL)

# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] xtable_1.8-4    cjoint_2.1.0    survey_4.0      survival_3.1-11 Matrix_1.2-18   lmtest_0.9-37  
# [7] zoo_1.8-7       sandwich_2.5-1  texreg_1.37.1   codebook_0.9.2  readxl_1.3.1    here_1.0.1     
# [13] forcats_0.5.0   stringr_1.4.0   dplyr_1.0.0     purrr_0.3.3     readr_1.4.0     tidyr_1.0.2    
# [19] tibble_3.0.0    ggplot2_3.3.5   tidyverse_1.3.0
# 
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-145      fs_1.4.1          lubridate_1.7.4   webshot_0.5.2     httr_1.4.1        rprojroot_2.0.2  
# [7] repr_1.1.0        tools_3.6.3       backports_1.1.5   utf8_1.1.4        R6_2.4.1          DT_0.13          
# [13] DBI_1.1.0         colorspace_1.4-1  withr_2.1.2       tidyselect_1.1.0  compiler_3.6.3    cli_2.3.0        
# [19] rvest_0.3.5       xml2_1.3.0        desc_1.2.0        labeling_0.3      scales_1.1.0      digest_0.6.25    
# [25] rmarkdown_2.6     base64enc_0.1-3   pkgconfig_2.0.3   htmltools_0.4.0   labelled_2.5.0    fastmap_1.0.1    
# [31] dbplyr_1.4.2      highr_0.8         htmlwidgets_1.5.1 rlang_1.0.0       rstudioapi_0.11   shiny_1.4.0.2    
# [37] farver_2.0.3      generics_0.0.2    jsonlite_1.6.1    crosstalk_1.1.0.1 magrittr_1.5      Rcpp_1.0.7       
# [43] munsell_0.5.0     fansi_0.4.1       lifecycle_0.2.0   stringi_1.4.6     yaml_2.2.1        rmdpartials_0.5.8
# [49] plyr_1.8.6        parallel_3.6.3    listenv_0.8.0     promises_1.1.0    crayon_1.3.4      lattice_0.20-40  
# [55] haven_2.3.1       splines_3.6.3     hms_0.5.3         knitr_1.28        pillar_1.4.3      pkgload_1.0.2    
# [61] codetools_0.2-16  reprex_0.3.0      glue_1.3.2        evaluate_0.14     mitools_2.4       modelr_0.1.6     
# [67] vctrs_0.3.1       httpuv_1.5.2      testthat_2.3.2    cellranger_1.1.0  gtable_0.3.0      future_1.17.0    
# [73] assertthat_0.2.1  xfun_0.20         mime_0.9          skimr_2.1.3       broom_0.5.5       later_1.0.0      
# [79] rsconnect_0.8.16  globals_0.12.5    ellipsis_0.3.0   