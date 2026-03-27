################################################################################################
####################### INSTRUCTIONS ###########################################################
################################################################################################


######      1. Copy this do-file and database file into the same directory (folder)
######      2. Change the working directory to the folder above (see line 9)

setwd("")   ## SHOULD CHANGE TO YOUR WORKING DIRECTORY HERE (e.g. "C:/data/")

# 
# ## Code Summary
# 
# This code (sequentially) provides all descriptive statistics, the main text regressions, as 
# well as unreported results of the paper A Tutorial on the Use of Differences-in-Differences in Management, Finance, 
# and Accounting, by Schiozer, Mourad and Martins (RAC, 2021). We thank Trung Ly for pointing out one typo in the 
# original script
#
# This code was tested in RStudio Version 1.2.5042 and R4.0.0
# 


# This code also uses the following non-default packages: haven, tidyverse, stargazer, lfe, sandwich, coefplot, dplyr.
# For more information on each package, please consult R´s help in the command window (e.g. ?haven for "haven" support)

# Installing non-default packages should be done accordingly, prior to the script
# Example: install.packages("haven") installs the haven package (this procedure needs to be done only once)


# Loading of (already installed) third-party packages
library(haven)
library(tidyverse)
library(stargazer)
library(lfe)
library(sandwich)
library(coefplot)
library(dplyr)

## read Stata's DTA database (should be in the working directory)
RAC <- read_dta("RAC.dta")  


#***********************************************************************************************************
#*******************  MAIN TEXT ANALYSIS *******************************************************************
#***********************************************************************************************************


######
######   Bank Descriptive Statistics - Table 1
######

## Statistics by period (pre-crisis / crisis) and group (protected / unprotected)
## Output in the folowing order: "Pre-crisis Untreated", "Pre-crisis Treated", "Crisis Untreated", "Crisis Treated"
vars <- c("w_ln_zScore", "liquidAssetsToStFunding", "assets")
RAC %>% 
	filter(year >= 2005, year <= 2010) %>%
	split(.[,c("treated", "crisis")]) %>%
	walk(~ cat(stargazer(as.data.frame(.[,vars]), type = "html", summary.stat = c("mean", "median", "sd", "n"), digits = 2), file = "Table1.html", append = TRUE))


## Median comparison tests between groups (Wilcoxon rank-sum tests) for each variable
## Note: refer to Appendix for (unreported) standard t-tests
## Note2: please note that R's and Stata output for this test differ (marginally)
##        Refer to http://www.senresearch.org/stata-and-r-ranksum-test-p-values-differ.html

RAC_Before <- RAC %>% filter(year >= 2005, year <= 2010, crisis == 0) %>% select(all_of(vars), treated)
for(v in vars) {
    wt <- wilcox.test(as.formula(paste(v, " ~ treated")), data = RAC_Before)
	print(paste(qnorm(wt$p.value / 2), wt$p.value))
}

RAC_During <- RAC %>% filter(year >= 2005, year <= 2010, crisis == 1) %>% select(all_of(vars), treated)
for(v in vars) {
    wt <- wilcox.test(as.formula(paste(v, " ~ treated")), data = RAC_During)
	print(paste(qnorm(wt$p.value / 2), wt$p.value))
}

rm(wt, v)



######
######   Parallel Trendind Checking - Figure 2
######


temp <- aggregate(list(w_ln_zScore = RAC$w_ln_zScore), list(year = factor(RAC$year), treated = factor(RAC$treated)), mean) 
temp <- temp %>% 
  filter(as.numeric(as.character(year))>=2005 & as.numeric(as.character(year))<=2010)


ggplot(temp, aes(x=year, y=w_ln_zScore, shape = as.factor(treated),color=as.factor(treated)))+  
  geom_point(size=5) +
  labs(x = "Year",y="Ln(zScore)") +
  scale_color_manual(
                    name = NULL,
                    labels = c("Unprotected", "Protected"), 
                    values = c("red", "blue")
                    ) +
  scale_shape_manual(
                    name = NULL,
                    labels = c("Unprotected", "Protected"), 
                    values = c(19,15)
                    ) +
  geom_rect(aes(xmin = 4, xmax = Inf, ymin = -Inf, ymax = Inf),linetype = 0, 
          fill = "gray70", alpha = 0.03) +
  geom_rect(aes(xmin = 1, xmax = 3, ymin = -Inf, ymax = Inf),linetype = 0, 
            fill = "gray10", alpha = 0.03,border="gray50") + theme(panel.background = element_rect(fill = 'gray93', colour = 'gray80'))


ggsave("Figure_2.png") ## save Figure 1 as PNG file

## Remove temporary (aggregate) database
rm(temp)


######
######   DID Estimator (2005 - 2010) - Table 2
######

## Column 1: 	No Fixed Effects or Controls
m1 <- felm(w_ln_zScore ~ crisis*treated | 0 | 0 | indexNumber, data = RAC, subset = (RAC$year <= 2010))

## Column 2: 	No Fixed Effect, with Bank Covariates
m2 <- felm(w_ln_zScore ~ crisis*treated + ln_assets_pre2007 + liquidAssetsToStFunding_pre2007 | 0 | 0 | indexNumber, data = RAC, subset = (RAC$year <= 2010))

## Column 3: 	No Fixed Effect, with Bank and Country Covariates
m3 <- felm(w_ln_zScore ~ crisis*treated + ln_assets_pre2007 + liquidAssetsToStFunding_pre2007 + HHI_pre2007 + gdpPerCapita_pre2007 + LgdpGrowth_pre2007 + L2gdpGrowth_pre2007 + creditToPrivate_pre2007 | 0 | 0 | indexNumber, data = RAC, subset = (RAC$year <= 2010))

## Column 4: 	With Country FE and Bank Covariates
m4 <- felm(w_ln_zScore ~ crisis*treated + ln_assets_pre2007 + liquidAssetsToStFunding_pre2007 | countryIndex | 0 | indexNumber, data = RAC, subset = (RAC$year <= 2010))

## Column 5: 	With Bank FE (exclude banks with exactly one observation, i.e. "N_indexNumber" > 1 )
RAC_BankFE <- RAC %>% filter(year <= 2010) %>% group_by(indexNumber) %>% filter(n() > 1)
m5 <- lm(w_ln_zScore ~ crisis + crisis:treated + factor(indexNumber), data = RAC_BankFE)
m5_cluster_se <- sqrt(diag(vcovCL(m5, cluster = RAC_BankFE$indexNumber)))

## Column 6: 	With Bank FE and yearly dummies
m6 <- lm(w_ln_zScore ~ crisis:treated + factor(year) + factor(indexNumber), data = RAC_BankFE)
m6_cluster_se <- sqrt(diag(vcovCL(m6, cluster = RAC_BankFE$indexNumber)))

## Export table to file
stargazer(m1, m2, m3, m4, m5, m6, type = "html", out = "Table2.html", omit = "factor*",
		  se = list(NULL, NULL, NULL, NULL, m5_cluster_se, m6_cluster_se),
		  order = c("crisis$", "^treated", "crisis:treated", "ln_assets_pre2007",
		   "liquidAssetsToStFunding_pre2007", "HHI_pre2007", "gdpPerCapita_pre2007",
		   "LgdpGrowth_pre2007", "L2gdpGrowth_pre2007", "creditToPrivate_pre2007"))

## Clear models results
rm(m1, m2, m3, m4, m5, m6, m5_cluster_se, m6_cluster_se)



######
######   DID Estimator (reversal: 2008 - 2013) - Table 3
######

## Column 1: 	No Fixed Effects or Controls
m1 <- felm(w_ln_zScore ~ crisis_reversal*treated | 0 | 0 | indexNumber, data = RAC, subset = (RAC$year >= 2008))

## Column 2: 	No Fixed Effect, with Bank Covariates
m2 <- felm(w_ln_zScore ~ crisis_reversal*treated + ln_assets_pre2010 + liquidAssetsToStFunding_pre2010 | 0 | 0 | indexNumber, data = RAC, subset = (RAC$year >= 2008))

## Column 3: 	No Fixed Effect, with Bank and Country Covariates
m3 <- felm(w_ln_zScore ~ crisis_reversal*treated + ln_assets_pre2010 + liquidAssetsToStFunding_pre2010 + HHI_pre2010 + gdpPerCapita_pre2010 + LgdpGrowth_pre2010 + L2gdpGrowth_pre2010 + creditToPrivate_pre2010 | 0 | 0 | indexNumber, data = RAC, subset = (RAC$year >= 2008))

## Column 4: 	With Country FE and Bank Covariates
m4 <- felm(w_ln_zScore ~ crisis_reversal*treated + ln_assets_pre2010 + liquidAssetsToStFunding_pre2010 | countryIndex | 0 | indexNumber, data = RAC, subset = (RAC$year >= 2008))

## Column 5: 	With Bank FE (exclude banks with exactly one observation, i.e. "N_indexNumber" > 1
RAC_BankFE <- RAC %>% filter(year >= 2008) %>% group_by(indexNumber) %>% filter(n() > 1)
m5 <- lm(w_ln_zScore ~ crisis_reversal + crisis_reversal:treated + factor(indexNumber), data = RAC_BankFE)
m5_cluster_se <- sqrt(diag(vcovCL(m5, cluster = RAC_BankFE$indexNumber)))

## Column 6: 	With Bank FE and yearly dummies (exclude banks with exactly one observation)
m6 <- lm(w_ln_zScore ~ crisis_reversal:treated + factor(year) + factor(indexNumber), data = RAC_BankFE)
m6_cluster_se <- sqrt(diag(vcovCL(m6, cluster = RAC_BankFE$indexNumber)))

## Export table to file
stargazer(m1, m2, m3, m4, m5, m6, type = "html", out = "Table3.html", omit = "factor*",
		  se = list(NULL, NULL, NULL, NULL, m5_cluster_se, m6_cluster_se),
		  order = c("crisis$", "^treated", "crisis:treated", "ln_assets_pre2007",
		   "liquidAssetsToStFunding_pre2007", "HHI_pre2007", "gdpPerCapita_pre2007",
		   "LgdpGrowth_pre2007", "L2gdpGrowth_pre2007", "creditToPrivate_pre2007"))

## Clear models results
rm(m1, m2, m3, m4, m5, m6, m5_cluster_se, m6_cluster_se)




######
######   Treatment effect over time - Figure 3
######
## Estimate regression and store coefficients
Figure_3 <- felm(w_ln_zScore ~ crisis + treated + factor(year):treated | 0 | 0 | indexNumber, data = RAC, subset = (RAC$year <= 2010))

## Draw graph

coefplot(Figure_3, horizontal = T, ylab = "Year", xlab = "Ln(Z_Score)", 
         coefficients = dimnames(Figure_3$coefficients)[[1]][4:8],
         newNames = c("treated:factor(year)2006"="2006", "treated:factor(year)2007"="2007", "treated:factor(year)2008"="2008", "treated:factor(year)2009"="2009", "treated:factor(year)2010"="2010"))

ggsave("Figure_3.png") ## save Figure 3 as PNG file

## Clear model result
rm(Figure_3)


# ***********************************************************************************************************
# ************************************* APPENDIX: UNREPORTED RESULTS ****************************************
# ***********************************************************************************************************
  
####### Mean comparison tests between groups (t-test)

RAC_Before <- RAC %>% filter(year >= 2005, year <= 2010, crisis == 0) %>% select(all_of(vars), treated)
for(v in vars) {
  wt <- t.test(as.formula(paste(v, " ~ treated")), data = RAC_Before)
  print(paste(qnorm(wt$p.value / 2), wt$p.value))
}

RAC_During <- RAC %>% filter(year >= 2005, year <= 2010, crisis == 1) %>% select(all_of(vars), treated)
for(v in vars) {
  wt <- t.test(as.formula(paste(v, " ~ treated")), data = RAC_During)
  print(paste(qnorm(wt$p.value / 2), wt$p.value))
}

rm(wt, v)


######
###### 	Equation 20 estimation (i.e. DiD expressed in differences)
######

## Excludes years 2011 onwards and Keeps banks that appear both before and after the shock
RAC_balanced <- RAC %>% filter(year >= 2005, year <= 2010, Banks_2005_2007 == 1, Banks_2008_2010 == 1) 

## Caclulate the mean of ln(Z_Score) by pre/post crisis and group (protected/unprotected)
RAC_eq20temp <- aggregate(w_ln_zScore ~ treated + crisis + indexNumber, data=RAC_balanced, FUN=mean)

RAC_eq20 <- RAC_eq20temp %>% group_by(indexNumber) %>% mutate(delta_zScore = c(NA,diff(w_ln_zScore))) %>% filter(!is.na(delta_zScore)) 
rm(RAC_eq20temp)
m20 <- felm(delta_zScore ~ treated | 0 | 0 | indexNumber, data = RAC_eq20)

## Export table to file
stargazer(m20, type = "html", out = "Table_Equation20_Unreported.html", omit = "Constant")

## Clear models results
rm(m20)




####### DiD Estimator (2005-2010) with only banks that appear before and after the crisis

## Column 1: 	No Fixed Effects or Controls
m1 <- felm(w_ln_zScore ~ crisis*treated | 0 | 0 | indexNumber, data = RAC_balanced)

## Column 2: 	No Fixed Effect, with Bank Covariates
m2 <- felm(w_ln_zScore ~ crisis*treated + ln_assets_pre2007 + liquidAssetsToStFunding_pre2007 | 0 | 0 | indexNumber, data = RAC_balanced)

## Column 3: 	No Fixed Effect, with Bank and Country Covariates
m3 <- felm(w_ln_zScore ~ crisis*treated + ln_assets_pre2007 + liquidAssetsToStFunding_pre2007 + HHI_pre2007 + gdpPerCapita_pre2007 + LgdpGrowth_pre2007 + L2gdpGrowth_pre2007 + creditToPrivate_pre2007 | 0 | 0 | indexNumber, data = RAC_balanced)

## Column 4: 	With Country FE and Bank Covariates
m4 <- felm(w_ln_zScore ~ crisis*treated + ln_assets_pre2007 + liquidAssetsToStFunding_pre2007 | countryIndex | 0 | indexNumber, data = RAC_balanced)

## Column 5: 	With Bank FE (exclude banks with exactly one observation, i.e. "N_indexNumber" > 1 )
RAC_balanced_BankFE <- RAC_balanced %>% group_by(indexNumber) %>% filter(n() > 1)
m5 <- lm(w_ln_zScore ~ crisis + crisis:treated + factor(indexNumber), data = RAC_balanced_BankFE)
m5_cluster_se <- sqrt(diag(vcovCL(m5, cluster = RAC_balanced_BankFE$indexNumber)))

## Column 6: 	With Bank FE and yearly dummies
m6 <- lm(w_ln_zScore ~ crisis:treated + factor(year) + factor(indexNumber), data = RAC_balanced_BankFE)
m6_cluster_se <- sqrt(diag(vcovCL(m6, cluster = RAC_balanced_BankFE$indexNumber)))

## Export table to file
stargazer(m1, m2, m3, m4, m5, m6, type = "html", out = "Table_2_BanksBeforeAndAfter_Unreported.html", omit = "factor*",
          se = list(NULL, NULL, NULL, NULL, m5_cluster_se, m6_cluster_se),
          order = c("crisis$", "^treated", "crisis:treated", "ln_assets_pre2007",
                    "liquidAssetsToStFunding_pre2007", "HHI_pre2007", "gdpPerCapita_pre2007",
                    "LgdpGrowth_pre2007", "L2gdpGrowth_pre2007", "creditToPrivate_pre2007"))


## Clear models results
rm(m1, m2, m3, m4, m5, m6, m5_cluster_se, m6_cluster_se)




####### Treatment effect over time - 2007 as the reference year

RAC2 <- RAC %>% filter(year <= 2010)
RAC2$year <- factor(RAC2$year, levels = c("2007", "2005", "2006", "2008", "2009", "2010"))
Figure_3_2007 <- felm(w_ln_zScore ~ crisis + treated + year:treated | 0 | 0 | indexNumber, data = RAC2)


## Draw graph

coefplot(Figure_3_2007, horizontal = T, ylab = "Year", xlab = "Ln(Z_Score)", 
         coefficients = dimnames(Figure_3_2007$coefficients)[[1]][4:8],
         newNames = c("treated:year2005"="2005", "treated:year2006"="2006", "treated:year2008"="2008", "treated:year2009"="2009", "treated:year2010"="2010"))

ggsave("Figure_3_2007_as_reference_Unreported.png") ## save Figure 3 as PNG file

## Clear temporary database and models results
rm(RAC2)
rm(Figure_3_2007)


## END