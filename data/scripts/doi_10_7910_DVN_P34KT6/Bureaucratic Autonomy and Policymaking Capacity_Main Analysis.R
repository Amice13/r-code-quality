###############################################################################
##### MEASURING THE POLICYMAKING CAPACITY OF THE UNITED STATES GOVERNMENT #####
##### VERSION: MAY 13, 2022                                               #####
###############################################################################

##### Clearing Environment
rm(list=ls())

######################
##### 1.0 Set Up #####
######################

##### 1.1 INSTRUCTION: Set working directory to location of all data files
setwd("F:\\Research\\Measuring Workforce Capacity\\_REPLICATION")
sink(file = "analysis_log.txt", type = c("output"),split = FALSE)


##### 1.2 INSTRUCTION: Install any missing packages 
library(tidyverse)
library(plm)
library(stargazer)
library(lmtest)
library(ggrepel)
library(ggpubr)
library(sjPlot)
library(grid)
library(gridExtra)
library(sandwich)
library(survival)

####################
##### 2.0 Data #####
####################

##### 2.1 Main Data

scores <- read.csv("analysis_data.csv")

inflation_data <- data.frame(INFLATION_RATE_2020 = c(1.588, 1.553, 1.503, 1.462, 1.439, 1.407,
                                      1.370, 1.325, 1.285, 1.248, 1.202, 1.206,
                                      1.187, 1.151, 1.127, 1.111, 1.093, 1.092, 
                                      1.078, 1.056, 1.031, 1.012, 1, 0.95),
                             DATE = c(1998, 1999, 2000, 2001, 2002,
                                          2003, 2004, 2005, 2006, 2007,
                                          2008, 2009, 2010, 2011, 2012,
                                          2013, 2014, 2015, 2016, 2017,
                                          2018, 2019, 2020, 2021))

scores <- left_join(scores, inflation_data) %>%
          mutate(POLICYMAKERS_SALARY = POLICYMAKERS_SALARY*INFLATION_RATE_2020)


##### 2.2 Rulemaking Data

rulemaking <- read.csv("rulemaking_data.csv") %>%
              filter(is.na(ideo_distance) == F) %>%
              filter(is.na(POLICYMAKERS_LOG) == F) %>%
              filter(is.na(CAPACITY_SCORE) == F) %>%
              filter(is.na(PoliticalReview_Est) == F) %>%
              mutate(CAPACITY_SCORE_SCALE = scale(CAPACITY_SCORE)[,1],
                     POLICYMAKERS_SCALE = scale(POLICYMAKERS_LOG)[,1],
                     ALIGNED_PRES = ifelse(ideo_distance < 2, 1, 0),
                     PARENT_AGENCY = ifelse(is.na(PARENT_AGENCY), AGENCY, PARENT_AGENCY),
                     ideo_rating = scale(ideo_rating)[,1],
                     DecisionMaker_Est = scale(DecisionMaker_Est)[,1],
                     WORKLOAD = scale(log1p(WORKLOAD))[,1])

######################################
##### 3.0 Descriptive Statistics #####
######################################

##### 3.1 Number of Agencies
print("Number of Agencies")
length(unique(scores$ABBR))

##### 3.2 Number of Capacity Scores
print("Number of Capacity Scores")
nrow(scores)

##### 3.3 Year Range
print("Earliest Year")
min(scores$DATE)
print("Latest Year")
max(scores$DATE)

##### 3.4 Policymaking Employees - College
print("Mean: Policymaking Employees - College")
mean(scores$POLICYMAKERS_COLLEGE, na.rm = T)
print("SD: Policymaking Employees - College")
sd(scores$POLICYMAKERS_COLLEGE, na.rm = T)

##### 3.5 Policymaking Employees - Experience
print("Mean: Policymaking Employees - Length of Service")
mean(scores$POLICYMAKERS_LOS, na.rm = T)
print("SD: Policymaking Employees - Length of Service")
sd(scores$POLICYMAKERS_LOS, na.rm = T)

##### 3.6 Policymaking Employees - Salary
print("Mean: Policymaking Employees - Salary")
mean(scores$POLICYMAKERS_SALARY, na.rm = T)
print("SD: Policymaking Employees - Salary")
sd(scores$POLICYMAKERS_SALARY, na.rm = T)

##### 3.6 Policymaking Employees - Proportion Policymakers
print("Mean: Proportion Policymakers")
mean(scores$POLICYMAKERS_PROP, na.rm = T)
print("SD: Proportion Policymakers")
sd(scores$POLICYMAKERS_PROP, na.rm = T)

print("Correlation: Proportion of Policymakers and FEVS Managers Collaboration Question")
cor(scores$POLICYMAKERS_PROP, scores$MANAGER_PROMOTE_COLLAB, "pairwise.complete.obs")
print("Correlation: Proportion of Policymakers and FEVS Cooperation Question")
cor(scores$POLICYMAKERS_PROP, scores$COOPERATE, "pairwise.complete.obs")

##### 3.7 Policymaking Employees - Logged
print("Mean: Policymaking Employees - Logged Number")
mean(scores$POLICYMAKERS_LOG, na.rm = T)
print("SD: Policymaking Employees - Logged Number")
sd(scores$POLICYMAKERS_LOG, na.rm = T)

##### 3.8 Capacity Scores
print("Min: Capacity Score")
min(scores$CAPACITY_SCORE)
print("Max: Capacity Score")
max(scores$CAPACITY_SCORE)
print("Mean: Capacity Score")
mean(scores$CAPACITY_SCORE)
print("SD: Capacity Score")
sd(scores$CAPACITY_SCORE)

########################
##### 4.0 FIGURE 1 #####
########################

#### 4.1 Dividing Sample into Large Agencies
dist_bigs <- scores %>%
  filter(ABBR == "DOED" | 
           ABBR == "DOD " | 
           ABBR == "TRS" | 
           ABBR == "DVA" |
           ABBR == "INT" |
           ABBR == "STAT" |
           ABBR == "DOE" |
           ABBR == "DOT" |
           ABBR == "HUD" |
           ABBR == "DHS" |
           ABBR == "HHS" |
           ABBR == "DOJ" |
           ABBR == "DOL" |
           ABBR == "USDA" |
           ABBR == "DOC" |
           ABBR == "EPA" |
           ABBR == "OMB" |
           ABBR == "FCC" |
           ABBR == "EEOC" |
           ABBR == "SBA" |
           ABBR == "SEC" |
           ABBR == "FTC" |
           ABBR == "NASA" |
           ABBR == "NLRB" |
           ABBR == "SSA " |
           ABBR == "NRC" |
           ABBR == "OPM" |
           ABBR == "NARA" |
           ABBR == "CFPB" |
           ABBR == "FDIC" |
           ABBR == "GSA" |
           ABBR == "USAID" |
           ABBR == "NCUA" |
           ABBR == "BBG") %>%
  group_by(ABBR) %>%
  summarize(AVG_CAPACITY = mean(CAPACITY_SCORE, na.rm = T),
            SD_CAPACITY = sd(CAPACITY_SCORE, na.rm = T),
            AVG_POLICYMAKER = mean(POLICYMAKERS_LOG, na.rm = T),
            SD_POLICYMAKER = sd(POLICYMAKERS_LOG, na.rm = T)) %>%
  mutate(CAPACITY_LB = AVG_CAPACITY-SD_CAPACITY,
         CAPACITY_UB = AVG_CAPACITY+SD_CAPACITY,
         POLICYMAKER_LB = AVG_POLICYMAKER-SD_POLICYMAKER,
         POLICYMAKER_UB = AVG_POLICYMAKER+SD_POLICYMAKER,
         ABBR = as.character(ABBR)) 

dist_bigs$ABBR[dist_bigs$ABBR == "DOED"] <- "Educ"
dist_bigs$ABBR[dist_bigs$ABBR == "DVA"] <- "VA"
dist_bigs$ABBR[dist_bigs$ABBR == "TRS"] <- "Treas."
dist_bigs$ABBR[dist_bigs$ABBR == "INT"] <- "Interior"
dist_bigs$ABBR[dist_bigs$ABBR == "STAT"] <- "State"
dist_bigs$ABBR[dist_bigs$ABBR == "DOE"] <- "Energy"
dist_bigs$ABBR[dist_bigs$ABBR == "DOT"] <- "Transp."
dist_bigs$ABBR[dist_bigs$ABBR == "DOL"] <- "Labor"
dist_bigs$ABBR[dist_bigs$ABBR == "DOC"] <- "Commerce"



dist_bigs <- dist_bigs %>%
  mutate(ABBR = fct_reorder(ABBR, AVG_CAPACITY))

##### 4.2 Making Graph of Capacity Score for Large Agencies

avg_cap <- ggplot(dist_bigs, aes(x = reorder(ABBR, AVG_CAPACITY), y = AVG_CAPACITY)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = ABBR, 
                 y = AVG_CAPACITY)) + 
  geom_linerange(aes(x = ABBR, 
                     ymin = CAPACITY_LB,
                     ymax = CAPACITY_UB),
                 lwd = 1) +
  labs(y="Mean Capacity Score", x="Agency")+
  scale_y_continuous(limits = c(-2.1,2.1))+
  coord_flip()+
  theme_light()

##### 4.3 Making Graph of Logged Policymaker for Large Agencies

dist_bigs <- dist_bigs %>%
  mutate(ABBR = fct_reorder(ABBR, AVG_POLICYMAKER))
avg_emp <- ggplot(dist_bigs, aes(x = reorder(ABBR, AVG_POLICYMAKER), y = AVG_POLICYMAKER)) +
  geom_hline(yintercept = mean(dist_bigs$AVG_POLICYMAKER, na.rm = T), colour = gray(1/2), lty = 2) +
  geom_point(aes(x = ABBR, 
                 y = AVG_POLICYMAKER)) + 
  geom_linerange(aes(x = ABBR, 
                     ymin = POLICYMAKER_LB,
                     ymax = POLICYMAKER_UB),
                 lwd = 1) +
  labs(y="Mean Employees (Logged)", x="Agency")+
  coord_flip()+
  theme_light()

##### 4.4 Correlation between Capacity and Workforce Size

cor(dist_bigs$AVG_CAPACITY, dist_bigs$AVG_POLICYMAKER)

##### 4.5 Combining Plots

combplots <- arrangeGrob(avg_cap, avg_emp , ncol = 2, nrow = 1)
ggsave("_FIGURES//FIG1_avgcapemp.pdf", plot=combplots, device = "pdf", width = 6, height = 8, units = "in")

###################################
##### 5.0 RULEMAKING ANALYSIS #####
###################################

##### 5.1 Descriptive Statistics

#### 5.1.1 Duration of Rulemaking
print("Mean: Duration of Rulemaking - Significiant Rules")
mean(rulemaking$DURATION[rulemaking$ALL_SIG == 1])
print("SD: Duration of Rulemaking - Significant Rules")
sd(rulemaking$DURATION[rulemaking$ALL_SIG == 1])

#### 5.1.2 Proportion of Aligned Agencies
print("Proportion of Aligned Agencies")
mean(rulemaking$ALIGNED_PRES[rulemaking$ALL_SIG == 1])


##### 5.2 Model 1 - Bivariate - Capacity Score
m1 <- coxph(Surv(DURATION, EVENT, type="right") ~ CAPACITY_SCORE_SCALE, 
            data = rulemaking[rulemaking$ALL_SIG == 1,],
            cluster = rulemaking$PARENT_AGENCY[rulemaking$ALL_SIG == 1],
            robust = T)

m1_robustse <- as.data.frame(summary(m1)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')



##### 5.3 Model 2 - Full Model - Capacity Score
m2 <- coxph(Surv(DURATION, EVENT,  type="right") ~ CAPACITY_SCORE_SCALE+ALIGNED_PRES+ideo_rating+DecisionMaker_Est+WORKLOAD+strata(ADMIN), 
            data = rulemaking[rulemaking$ALL_SIG == 1,],
            cluster = rulemaking$PARENT_AGENCY[rulemaking$ALL_SIG == 1],
            robust = T)

m2_robustse <- as.data.frame(summary(m2)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')


##### 5.4 Model 3 - Full Model - Capacity Score
m3 <- coxph(Surv(DURATION_FT_NPRM, EVENT_FT,  type="right") ~ CAPACITY_SCORE_SCALE+ALIGNED_PRES+ideo_rating+DecisionMaker_Est+WORKLOAD+strata(ADMIN), 
            data = rulemaking[rulemaking$ALL_SIG == 1,],
            cluster = rulemaking$PARENT_AGENCY[rulemaking$ALL_SIG == 1],
            robust = T)

m3_robustse <- as.data.frame(summary(m3)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')


##### 5.5 Table

star_order_1 <- paste("^",c("CAPACITY_SCORE_SCALE", "ALIGNED_PRES", "ideo_rating", "DecisionMaker_Est","WORKLOAD"), "$", sep = "")
star_labels_1 <- c("Capacity", "Ideologically Aligned", "Agency Ideology", "Independence", "Workload (Logged)")

stargazer(m1,m2,m3,
          title = "Estimated Days to Final Rule: First Term of the Bush, Obama, and Trump Administrations",
          label = "tab:duration",
          style = "default",
          dep.var.caption = "",
          dep.var.labels = "Hazard Rate of Rulemaking",
          model.numbers = T,
          column.separate = c(2,1),
          covariate.labels =star_labels_1,
          order = star_order_1,
          model.names = F,
          star.cutoffs = c(0.05,0.01,0.001),
          digits = 2,
          digits.extra = 2,
          se=c(m1_robustse, m2_robustse, m3_robustse),
          add.lines = list(c("\\hline Presidency Strata", "No", "Yes", "Yes"),
                           c("First-Term Censor", "No", "No", "Yes"),
                           c("Estimator", "Cox", "Cox", "Cox")),
          notes = "\\parbox[t]{3.75in}{Standard errors clustered at the agency level. All continuous variables normalized prior to estimation.}",
          keep.stat = c("n"),
          out = "_TABLES//TAB1_RulemakingDuration_MainText.tex")

##### 5.6 Hazard Ratios
print("Hazard Ratio: +1SD Capacity")
exp(0.11)
print("Hazard Ratio: -1SD Capacity")
exp(-0.11)
1-exp(-0.11)

################################################
##### 6.0 STRUCTURAL INDEPENDENCE ANALYSIS #####
################################################

##### 6.1 Creating Aggregated Data and Descriptive Statistics
agency_avgs <- scores %>%
  group_by(ABBR, FEDSCOPE_ID) %>%
  summarize(AVG_CAPACITY = mean(CAPACITY_SCORE, na.rm = T),
            SD_CAPACITY = sd(CAPACITY_SCORE, na.rm = T))

##### 6.1.1 Decision-Maker Independence
print("Mean: Decision-Maker Independence")
mean(scores$DecisionMaker_Est, na.rm = T)
print("SD: Decision-Maker Independence")
sd(scores$DecisionMaker_Est, na.rm = T)

##### 6.1.2 Political Review Independence
print("Mean: Political Review Independence")
mean(scores$PoliticalReview_Est, na.rm = T)
print("SD: Political Review Independence")
sd(scores$PoliticalReview_Est, na.rm = T)

##### 6.1.3 Agency Ideology
print("Mean: Agency Ideology")
mean(scores$ideo_rating, na.rm = T)
print("SD: Agency Ideology")
sd(scores$ideo_rating, na.rm = T)

##### 6.1.2 Agency Age
print("Mean: Agency Age")
mean(scores$YRS_SINCE_CREATION, na.rm = T)
print("SD: Agency Age")
sd(scores$YRS_SINCE_CREATION, na.rm = T)

##### 6.2 Decision-Maker Independence Models
dec_form1 <- formula(CAPACITY_SCORE~DecisionMaker_Est)
indep_model1 <- lm(dec_form1, data = scores)
indep_model1_clust <- coeftest(indep_model1, vcovCL(indep_model1, cluster = scores$FEDSCOPE_ID, type = "HC1")) #Clustering Standard errors
indep_model1_se <- indep_model1_clust[,2] #Extracting Standard Errors

dec_form2 <- formula(CAPACITY_SCORE~DecisionMaker_Est+as.factor(DATE)+YRS_SINCE_CREATION+ideo_rating+CAP_MAIN)
indep_model2 <- lm(dec_form2, data = scores)
indep_model2_clust <- coeftest(indep_model2, vcovCL(indep_model2, cluster = scores$FEDSCOPE_ID, type = "HC1")) #Clustering Standard errors
indep_model2_se <- indep_model2_clust[,2] #Extracting Standard Errors


##### 6.3 Left-Hand Plot of Figure 2
decisionmaker_plot <- plot_model(indep_model2,
                                 type = "pred",
                                 title = "",
                                 se = indep_model2_se,
                                 terms = c("DecisionMaker_Est","DATE[2021]"))+
                      scale_y_continuous(name = "Policymaking Capacity", breaks = c(-2.5, 2.5), labels = c("Low", "High"), limits = c(-2.5,2.5))+
                      scale_x_continuous(name = "Decision-Maker Independence", breaks = c(-1,2), labels = c("Low", "High"), limits = c(-1,2.5))+
                      geom_point(data = scores, aes(x=DecisionMaker_Est, y=CAPACITY_SCORE), alpha = 0.005)+
                      geom_point(data = scores[scores$DATE ==2021 & (scores$ABBR == "SEC" | 
                                                      scores$ABBR == "FTC" |
                                                      scores$ABBR == "DHS" |
                                                      scores$ABBR == "DOJ" |
                                                      scores$ABBR == "USDA" |
                                                      scores$ABBR == "EPA" |
                                                      scores$ABBR == "EDU" |
                                                      scores$ABBR == "FCC" |
                                                      scores$ABBR == "DOC" |
                                                      scores$ABBR == "USDA" |
                                                      scores$ABBR == "FEC" |
                                                      scores$ABBR == "DHS" |
                                                      scores$ABBR == "DOD " |
                                                      scores$ABBR == "DOED" |
                                                      scores$ABBR == "DOE" |
                                                      scores$ABBR == "DOT" |
                                                      scores$ABBR == "TRS" |
                                                      scores$ABBR == "HUD" |
                                                      scores$ABBR == "DVA" | 
                                                      scores$ABBR == "STAT" |
                                                      scores$ABBR == "INT" |
                                                      scores$ABBR == "HHS" |
                                                      scores$ABBR == "DOL" |
                                                      scores$ABBR == "OMB" |
                                                      scores$ABBR == "CPSC" | 
                                                      scores$ABBR == "SBA" |
                                                      scores$ABBR == "FERC" |
                                                      scores$ABBR == "NRC" |
                                                      scores$ABBR == "NASA" |
                                                      scores$ABBR == "USITC" |
                                                      scores$ABBR == "FDIC" |
                                                      scores$ABBR == "USAID" |
                                                      scores$ABBR == "CFPB" |
                                                      scores$ABBR == "EEOC"),],
                                 aes(x=DecisionMaker_Est, y=CAPACITY_SCORE), alpha = 1)+
                      geom_text_repel(data = scores[scores$DATE == 2021 & (scores$ABBR == "SEC" | 
                                                           scores$ABBR == "FTC" |
                                                           scores$ABBR == "DHS" |
                                                           scores$ABBR == "DOJ" |
                                                           scores$ABBR == "USDA" |
                                                           scores$ABBR == "EPA" |
                                                           scores$ABBR == "EDU" |
                                                           scores$ABBR == "FCC" |
                                                           scores$ABBR == "DOC" |
                                                           scores$ABBR == "USDA" |
                                                           scores$ABBR == "DHS" |
                                                           scores$ABBR == "DOD " |
                                                           scores$ABBR == "DOED" |
                                                           scores$ABBR == "DOE" |
                                                           scores$ABBR == "DOT" |
                                                           scores$ABBR == "TRS" |
                                                           scores$ABBR == "HUD" |
                                                           scores$ABBR == "DVA" | 
                                                           scores$ABBR == "STAT" |
                                                           scores$ABBR == "INT" |
                                                           scores$ABBR == "HHS" |
                                                           scores$ABBR == "DOL" |
                                                           scores$ABBR == "OMB" |
                                                           scores$ABBR == "FEC" |
                                                           scores$ABBR == "CPSC" | 
                                                           scores$ABBR == "SBA" |
                                                           scores$ABBR == "FERC" |
                                                          scores$ABBR == "NRC" |
                                                           scores$ABBR == "NASA" |
                                                           scores$ABBR == "FDIC" |
                                                           scores$ABBR == "USAID" |
                                                           scores$ABBR == "USITC" |
                                                           scores$ABBR == "CFPB" |
                                                           scores$ABBR == "EEOC"),], 
                                      aes(x=DecisionMaker_Est, y= CAPACITY_SCORE, label = ABBR))+
                      theme_light()+
                      theme(axis.text=element_text(size=16),
                            axis.title=element_text(size=16))


ggsave("_FIGURES//FIG2a_decisionmaker.pdf", plot=decisionmaker_plot, device = "pdf", width = 6, height = 4, scale = 1.5, units = "in")


##### 6.4 Political-Review Independence Models

pol_form1 <- formula(CAPACITY_SCORE~PoliticalReview_Est)
indep_model3 <- lm(pol_form1, data = scores)
indep_model3_clust <- coeftest(indep_model3, vcovCL(indep_model3, cluster = scores$FEDSCOPE_ID, type = "HC1")) #Clustering Standard errors
indep_model3_se <- indep_model3_clust[,2] #Extracting Standard Errors

pol_form2 <- formula(CAPACITY_SCORE~PoliticalReview_Est+as.factor(DATE)+YRS_SINCE_CREATION+ideo_rating+CAP_MAIN)
indep_model4 <- lm(pol_form2, data = scores)
indep_model4_clust <- coeftest(indep_model4, vcovCL(indep_model4, cluster = scores$FEDSCOPE_ID, type = "HC1")) #Clustering Standard errors
indep_model4_se <- indep_model4_clust[,2] #Extracting Standard Errors


policydec_plot <- plot_model(indep_model4,
                                 type = "pred",
                                 title = "",
                                 se = indep_model4_se,
                                 axis.lim = list(c(-1,4.5),c(-2.5,2.5)),
                                 terms = c("PoliticalReview_Est", "DATE [2021]"))+
  scale_y_continuous(name = "Policymaking Capacity", breaks = c(-2.5, 2.5), labels = c("Low", "High"), limits = c(-2.5,2.5))+
  scale_x_continuous(name = "Political-Review Independence", breaks = c(-1,4.25), labels = c("Low", "High"), limits = c(-1,4.25))+
  geom_point(data = scores, aes(x=PoliticalReview_Est, y=CAPACITY_SCORE), alpha = 0.005)+
  geom_point(data = scores[scores$DATE ==2021 & (scores$ABBR == "SEC" | 
                                                   scores$ABBR == "FTC" |
                                                   scores$ABBR == "DHS" |
                                                   scores$ABBR == "DOJ" |
                                                   scores$ABBR == "USDA" |
                                                   scores$ABBR == "EPA" |
                                                   scores$ABBR == "EDU" |
                                                   scores$ABBR == "FCC" |
                                                   scores$ABBR == "DOC" |
                                                   scores$ABBR == "USDA" |
                                                   scores$ABBR == "FEC" |
                                                   scores$ABBR == "DHS" |
                                                   scores$ABBR == "DOD " |
                                                   scores$ABBR == "DOED" |
                                                   scores$ABBR == "DOE" |
                                                   scores$ABBR == "DOT" |
                                                   scores$ABBR == "TRS" |
                                                   scores$ABBR == "HUD" |
                                                   scores$ABBR == "DVA" | 
                                                   scores$ABBR == "STAT" |
                                                   scores$ABBR == "INT" |
                                                   scores$ABBR == "HHS" |
                                                   scores$ABBR == "DOL" |
                                                   scores$ABBR == "OMB" |
                                                   scores$ABBR == "CPSC" | 
                                                   scores$ABBR == "SBA" |
                                                   scores$ABBR == "FERC" |
                                                   scores$ABBR == "NRC" |
                                                   scores$ABBR == "NASA" |
                                                   scores$ABBR == "USITC" |
                                                   scores$ABBR == "FDIC" |
                                                   scores$ABBR == "USAID" |
                                                   scores$ABBR == "CFPB" |
                                                   scores$ABBR == "EEOC"),],
             aes(x=PoliticalReview_Est, y=CAPACITY_SCORE), alpha = 1)+
  geom_text_repel(data = scores[scores$DATE == 2021 & (scores$ABBR == "SEC" | 
                                                         scores$ABBR == "FTC" |
                                                         scores$ABBR == "DHS" |
                                                         scores$ABBR == "DOJ" |
                                                         scores$ABBR == "USDA" |
                                                         scores$ABBR == "EPA" |
                                                         scores$ABBR == "EDU" |
                                                         scores$ABBR == "FCC" |
                                                         scores$ABBR == "DOC" |
                                                         scores$ABBR == "USDA" |
                                                         scores$ABBR == "DHS" |
                                                         scores$ABBR == "DOD " |
                                                         scores$ABBR == "DOED" |
                                                         scores$ABBR == "DOE" |
                                                         scores$ABBR == "DOT" |
                                                         scores$ABBR == "TRS" |
                                                         scores$ABBR == "HUD" |
                                                         scores$ABBR == "DVA" | 
                                                         scores$ABBR == "STAT" |
                                                         scores$ABBR == "INT" |
                                                         scores$ABBR == "HHS" |
                                                         scores$ABBR == "DOL" |
                                                         scores$ABBR == "OMB" |
                                                         scores$ABBR == "FEC" |
                                                         scores$ABBR == "CPSC" | 
                                                         scores$ABBR == "SBA" |
                                                         scores$ABBR == "FERC" |
                                                         scores$ABBR == "NRC" |
                                                         scores$ABBR == "NASA" |
                                                         scores$ABBR == "FDIC" |
                                                         scores$ABBR == "USAID" |
                                                         scores$ABBR == "USITC" |
                                                         scores$ABBR == "CFPB" |
                                                         scores$ABBR == "EEOC"),], 
                  aes(x=PoliticalReview_Est, y= CAPACITY_SCORE, label = ABBR))+
  theme_light()+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16))

ggsave("_FIGURES//FIG2b_policymaker.pdf", plot=policydec_plot, device = "pdf", width = 6, height = 4, scale = 1.5, units = "in")



combplots <- arrangeGrob(decisionmaker_plot, policydec_plot , ncol = 2, nrow = 1)
ggsave("_FIGURES//FIG2_independence.pdf", plot=combplots, device = "pdf", width = 6, height = 4, scale = 1.5, units = "in")

(coef(indep_model2)[2]*sd(scores$DecisionMaker_Est,na.rm=T))/sd(scores$CAPACITY_SCORE,na.rm=T)
(coef(indep_model4)[2]*sd(scores$PoliticalReview_Est,na.rm=T))/sd(scores$CAPACITY_SCORE,na.rm=T)

##### 6.5 Creating Table

star_order <- paste("^",c("DecisionMaker_Est", "PoliticalReview_Est", "ideo_rating", "YRS_SINCE_CREATION", "Constant"), "$", sep = "")
star_labels <- c("Decision-Maker Independence", "Political-Review Independence", "Agency Ideology", "Age (Hundreds of Years)", "Constant")


stargazer(indep_model1 , indep_model2, indep_model3, indep_model4,
          title = "Model Estimates of Effect of Independence on Capacity",
          style = "default",
          label = "tab:independence",
          dep.var.labels = "Policymaking Capacity",
          model.numbers = T,
          dep.var.caption = "",
          covariate.labels = star_labels,
          keep = star_order,
          order = star_order,
          model.names = F,
          single.row = F,
          star.cutoffs = c(0.05,0.01,0.001),
          digits = 2,
          digits.extra = 0,
          add.lines = list(c("\\hline Agency Fixed Effects", "No", "Yes", "No", "Yes"),
                           c("Topic Fixed Effects", "No", "Yes", "No", "Yes"),
                           c("Estimator", "OLS", "OLS", "OLS", "OLS")),
          se=list(indep_model1_se, indep_model2_se, indep_model3_se, indep_model4_se),
          notes = "Standard errors clustered at agency level.",
          omit.stat = c("logrank", "wald", "F", "ser", "chi2", "adj.rsq"),
          out = "_TABLES//TAB2_independence.tex")

##### 6.6 Cleaning the environment

rm(indep_model1, indep_model2, indep_model3, indep_model4,
   indep_model1_clust, indep_model2_clust, indep_model3_clust, indep_model4_clust,
   indep_model1_se, indep_model2_se, indep_model3_se, indep_model4_se)

##################################
##### 7.0 OA1 MODEL ANALYSIS #####
##################################

##### 7.1 Variable Averages

vars_tab <- data.frame(Indicator = c("Logged Number of Policymaking Employees",
                                      "Proportion of Employees Involved in Policymaking",
                                      "Mean Salary of Policymaking Employees (Thousands of 2021 Dollars)",
                                      "Mean Length of Service of Policymaking Employees (Years)",
                                      "Proportion of Policymaking Employees with a College Education"),
                        Mean = c(round(mean(scores$POLICYMAKERS_LOG[is.na(scores$CAPACITY_SCORE) == F]), 2),
                                 round(mean(scores$POLICYMAKERS_PROP[is.na(scores$CAPACITY_SCORE) == F]), 2),
                                 round(mean(scores$POLICYMAKERS_SALARY[is.na(scores$CAPACITY_SCORE) == F])/1000, 2),
                                 round(mean(scores$POLICYMAKERS_LOS[is.na(scores$CAPACITY_SCORE) == F]), 2),
                                 round(mean(scores$POLICYMAKERS_COLLEGE[is.na(scores$CAPACITY_SCORE) == F], na.rm = T), 2)),
                        SD   = c(round(sd(scores$POLICYMAKERS_LOG[is.na(scores$CAPACITY_SCORE) == F]), 2),
                                 round(sd(scores$POLICYMAKERS_PROP[is.na(scores$CAPACITY_SCORE) == F]), 2),
                                 round(sd(scores$POLICYMAKERS_SALARY[is.na(scores$CAPACITY_SCORE) == F])/1000, 2),
                                 round(sd(scores$POLICYMAKERS_LOS[is.na(scores$CAPACITY_SCORE) == F]), 2),
                                 round(sd(scores$POLICYMAKERS_COLLEGE[is.na(scores$CAPACITY_SCORE) == F],na.rm = T), 2)))

stargazer(vars_tab,
          title = "Variables of Workforce Capacity",
          style = "default",
          label = "tab:inds",
          rownames = FALSE,
          summary = FALSE,
          digits = 2,
          notes = "\\textit{Note:} All variables are normalized before estimation. Averages calculated before normalization.",
          out = "_TABLES/TABOA1_vars.tex")



rm(vars_tab)

##### 7.2 Estimates of Model Parameters
#### Interested in Beta2 and Gamma1

model_params <- read.csv("model_parameters.csv")

model_params <- data.frame(Indicator = c("Logged Number of Policymaking Employees",
                                         "Proportion of Employees Involved in Policymaking",
                                         "Mean Salary of Policymaking Employees (Thousands of 2021 Dollars)",
                                         "Mean Length of Service of Policymaking Employees (Years)",
                                         "Proportion of Policymaking Employees with a College Education"),
                           Beta = c(model_params$summary.mean[model_params$X == "beta2[1]"],
                                    model_params$summary.mean[model_params$X == "beta2[2]"],
                                    model_params$summary.mean[model_params$X == "beta2[3]"],
                                    model_params$summary.mean[model_params$X == "beta2[4]"],
                                    model_params$summary.mean[model_params$X == "beta2[5]"]),
                           Gamma = c(model_params$summary.mean[model_params$X == "gamma[1]"],
                                     model_params$summary.mean[model_params$X == "gamma[2]"],
                                     model_params$summary.mean[model_params$X == "gamma[3]"],
                                     model_params$summary.mean[model_params$X == "gamma[4]"],
                                     model_params$summary.mean[model_params$X == "gamma[5]"]))

model_params$Beta <- round(model_params$Beta, 2)
model_params$Gamma <- round(model_params$Gamma, 3)

stargazer(model_params,
          title = "Estimates of Model Parameters",
          style = "default",
          label = "tab:params",
          rownames = FALSE,
          summary = FALSE,
          digits = 2,
          out = "_TABLES/TABOA2_modelparams.tex")


##### 7.3 Correlation Matrix of Scores

#### 7.3.1 Creating the Correlation Matrix

cor_table <- scores %>%
            select(CAPACITY_SCORE, starts_with("SCORE")) %>% 
            select(-SCORE_M017) %>%
            cor(use = "pairwise.complete.obs") 

cor_table <- round(cor_table, digits = 2)
colnames(cor_table) <- c(1:nrow(cor_table))
rownames(cor_table) <- c(1:nrow(cor_table))

#### 7.3.2 Table

stargazer(cor_table,
          title = "Correlations of Model Specifications",
          style = "default",
          label = "tab:correlations",
          summary = FALSE,
          digits = 2,
          out = "_TABLES/TABOA4_speccors.tex")

#### 7.3.3 Smallest Correlaton
print("Minimum Correlation Between Models")
min(cor_table)

rm(cor_table)

#### 7.3.4 Education Correlation
print("Model Correlation: Education - Model 2")
cor(scores$CAPACITY_SCORE, scores$SCORE_M002, "pairwise.complete.obs")

#### 7.3.5 Size of Workforce
print("Model Correlation: No Proportion - Model 3")
cor(scores$CAPACITY_SCORE, scores$SCORE_M003, "pairwise.complete.obs")
print("Model Correlation: No Total- Model 5")
cor(scores$CAPACITY_SCORE, scores$SCORE_M005, "pairwise.complete.obs")
print("Model Correlation: No Proportion and No Total - Model 7")
cor(scores$CAPACITY_SCORE, scores$SCORE_M007, "pairwise.complete.obs")

##### 7.4 OEWS Analysis

#### 7.4.1 Correlation
print("Model Correlation: OEWS Salary - Model 17")
cor(scores$CAPACITY_SCORE, scores$SCORE_M017, "pairwise.complete.obs")

#### 7.4.2 Figure OA1

dist_bigs <- scores %>%
  filter(ABBR == "DOED" | 
           ABBR == "DOD " | 
           ABBR == "TRS" | 
           ABBR == "DVA" |
           ABBR == "INT" |
           ABBR == "STAT" |
           ABBR == "DOE" |
           ABBR == "DOT" |
           ABBR == "HUD" |
           ABBR == "DHS" |
           ABBR == "HHS" |
           ABBR == "DOJ" |
           ABBR == "DOL" |
           ABBR == "USDA" |
           ABBR == "DOC" |
           ABBR == "EPA" |
           ABBR == "OMB" |
           ABBR == "FCC" |
           ABBR == "EEOC" |
           ABBR == "SBA" |
           ABBR == "SEC" |
           ABBR == "FTC" |
           ABBR == "NASA" |
           ABBR == "NLRB" |
           ABBR == "SSA " |
           ABBR == "NRC" |
           ABBR == "OPM" |
           ABBR == "NARA" |
           ABBR == "CFPB" |
           ABBR == "FDIC" |
           ABBR == "GSA" |
           ABBR == "USAID" |
           ABBR == "NCUA" |
           ABBR == "BBG") %>%
  group_by(ABBR) %>%
  summarize(AVG_CAPACITY001 = mean(CAPACITY_SCORE, na.rm = T),
            SD_CAPACITY001 = sd(CAPACITY_SCORE, na.rm = T),
            AVG_CAPACITY017 = mean(SCORE_M017, na.rm = T),
            SD_CAPACITY017 = sd(SCORE_M017, na.rm = T)) %>%
  mutate(CAPACITY001_LB = AVG_CAPACITY001-SD_CAPACITY001,
         CAPACITY001_UB = AVG_CAPACITY001+SD_CAPACITY001,
         CAPACITY017_LB = AVG_CAPACITY017-SD_CAPACITY017,
         CAPACITY017_UB = AVG_CAPACITY017+SD_CAPACITY017,
         ABBR = as.character(ABBR)) 

dist_bigs$ABBR[dist_bigs$ABBR == "DOED"] <- "Educ"
dist_bigs$ABBR[dist_bigs$ABBR == "DVA"] <- "VA"
dist_bigs$ABBR[dist_bigs$ABBR == "TRS"] <- "Treas."
dist_bigs$ABBR[dist_bigs$ABBR == "INT"] <- "Interior"
dist_bigs$ABBR[dist_bigs$ABBR == "STAT"] <- "State"
dist_bigs$ABBR[dist_bigs$ABBR == "DOE"] <- "Energy"
dist_bigs$ABBR[dist_bigs$ABBR == "DOT"] <- "Transp."
dist_bigs$ABBR[dist_bigs$ABBR == "DOL"] <- "Labor"
dist_bigs$ABBR[dist_bigs$ABBR == "DOC"] <- "Commerce"



dist_bigs <- dist_bigs %>%
  mutate(ABBR = fct_reorder(ABBR, AVG_CAPACITY001))

avg_cap001 <- ggplot(dist_bigs, aes(x = reorder(ABBR, AVG_CAPACITY001), y = AVG_CAPACITY001)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = ABBR, 
                 y = AVG_CAPACITY001)) + 
  geom_linerange(aes(x = ABBR, 
                     ymin = CAPACITY001_LB,
                     ymax = CAPACITY001_UB),
                 lwd = 1) +
  labs(y="Mean Capacity (Raw Salary)", x="Agency")+
  scale_y_continuous(limits = c(-2.1,2.1))+
  coord_flip()+
  theme_light()

dist_bigs <- dist_bigs %>%
  mutate(ABBR = fct_reorder(ABBR, AVG_CAPACITY017))

avg_cap017 <- ggplot(dist_bigs, aes(x = reorder(ABBR, AVG_CAPACITY017), y = AVG_CAPACITY017)) +
  geom_hline(yintercept = mean(dist_bigs$AVG_CAPACITY017, na.rm = T), colour = gray(1/2), lty = 2) +
  geom_point(aes(x = ABBR, 
                 y = AVG_CAPACITY017)) + 
  geom_linerange(aes(x = ABBR, 
                     ymin = CAPACITY017_LB,
                     ymax = CAPACITY017_UB),
                 lwd = 1) +
  labs(y="Mean Capacity (Differential)", x="Agency")+
  coord_flip()+
  theme_light()


combplots <- arrangeGrob(avg_cap001, avg_cap017 , ncol = 2, nrow = 1)
ggsave("_FIGURES/FIGOA1_OEWS.pdf", plot=combplots, device = "pdf", width = 6, height = 8, units = "in")


#### 7.5 Correlation with Model 15
print("Model Correlation: Just Washington DC- Model 15")
cor(scores$CAPACITY_SCORE, scores$SCORE_M015, "pairwise.complete.obs")

#### 7.6 Correlation with Model 9
print("Model Correlation: No Salary - Model 9")
cor(scores$CAPACITY_SCORE, scores$SCORE_M009, "pairwise.complete.obs")

#### 7.6 Correlation with Model 11
print("Model Correlation: Total Experience - Model 11")
cor(scores$CAPACITY_SCORE, scores$SCORE_M011, "pairwise.complete.obs")

#### 7.6 Correlation with Model 13
print("Model Correlation: No Researchers - Model 13")
cor(scores$CAPACITY_SCORE, scores$SCORE_M013, "pairwise.complete.obs")


########################
##### 8.0 OA3 FEVS #####
########################

##### 8.1 Descriptive Statistics
#### 8.1.1 Sufficient Knowledge
print("Mean: Sufficient Knowledge")
mean(scores$SUFFICIENT_KNOWLEDGE, na.rm = T)
print("SD: Sufficient Knowledge")
sd(scores$SUFFICIENT_KNOWLEDGE, na.rm = T)

#### 8.1.2 Share Knowledge Knowledge
print("Mean: Share Knowledge")
mean(scores$SHARE_KNOWLEDGE, na.rm = T)
print("SD: Share Knowledge")
sd(scores$SHARE_KNOWLEDGE, na.rm = T)

#### 8.1.3 Managers Promote Collaboration
print("Mean: Managers Promote Collaboration")
mean(scores$MANAGER_PROMOTE_COLLAB, na.rm = T)
print("SD: Managers Promote Collaboration")
sd(scores$MANAGER_PROMOTE_COLLAB, na.rm = T)

#### 8.1.4 Sufficient Knowledge
print("Mean: Cooperate")
mean(scores$COOPERATE, na.rm = T)
print("SD: Cooperate")
sd(scores$COOPERATE, na.rm = T)

#### 8.1.5 Recruit
print("Mean: Recruit")
mean(scores$RECRUIT, na.rm = T)
print("SD: Recruit")
sd(scores$RECRUIT, na.rm = T)

#### 8.1.6 Quality
print("Mean: Quality")
mean(scores$QUALITY_WORKUNIT, na.rm = T)
print("SD: Quality")
sd(scores$QUALITY_WORKUNIT, na.rm = T)

##### 8.2 Creating Correlation Matrix

#### 8.2.1 Creating an empty matrix
FEVS_cors <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(FEVS_cors) <- c("Sufficient Knowledge", "Share Knowledge", "Collaboration", "Cooperate", "Recruit", "Quality Work", "Average")


#### 8.2.2 Adding Corelations to matrix
fevs_row <- c(cor(scores$CAPACITY_SCORE, scores$SUFFICIENT_KNOWLEDGE, use = "pairwise.complete.obs"),
              cor(scores$CAPACITY_SCORE, scores$SHARE_KNOWLEDGE, use = "pairwise.complete.obs"),
              cor(scores$CAPACITY_SCORE, scores$MANAGER_PROMOTE_COLLAB, use = "pairwise.complete.obs"),
              cor(scores$CAPACITY_SCORE, scores$COOPERATE, use = "pairwise.complete.obs"),
              cor(scores$CAPACITY_SCORE, scores$RECRUIT, use = "pairwise.complete.obs"),
              cor(scores$CAPACITY_SCORE, scores$QUALITY_WORKUNIT, use = "pairwise.complete.obs"))
fevs_row <- c(fevs_row, mean(fevs_row))
FEVS_cors <- rbind(FEVS_cors, fevs_row)
colnames(FEVS_cors) <- c("Sufficient Knowledge", "Share Knowledge", "Collaboration", "Cooperate", "Recruit", "Quality Work", "Avg.")

for(i in 13:27){
  fevs_row <- c(cor(scores[,i], scores$SUFFICIENT_KNOWLEDGE, use = "pairwise.complete.obs"),
                cor(scores[,i], scores$SHARE_KNOWLEDGE, use = "pairwise.complete.obs"), 
                cor(scores[,i], scores$MANAGER_PROMOTE_COLLAB, use = "pairwise.complete.obs"),
                cor(scores[,i], scores$COOPERATE, use = "pairwise.complete.obs"),
                cor(scores[,i], scores$RECRUIT, use = "pairwise.complete.obs"),
                cor(scores[,i], scores$QUALITY_WORKUNIT, use = "pairwise.complete.obs"))
  fevs_row <- c(fevs_row, mean(fevs_row))
  FEVS_cors <- rbind(FEVS_cors, fevs_row)
}

FEVS_cors <- round(FEVS_cors, 2)
rownames(FEVS_cors) <- c(1:16)

stargazer(FEVS_cors,
          title = "Correlations Between Scores and FEVS Questions",
          style = "default",
          label = "tab:FEVScorrs",
          summary = FALSE,
          digits = 2,
          out = "_TABLES//TABOA9_fevscors.tex")

##### 8.3 Producing Graphs for Primary Specification
ggplot(scores, aes(x=CAPACITY_SCORE, y=SUFFICIENT_KNOWLEDGE))+
      geom_point(color = "gray50", alpha = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  scale_y_continuous(name = "Sufficient Knowledge", lim = c(0,1), breaks = c(0,1))+
  scale_x_continuous(name = "Capacity", lim = c(-4, 4), breaks = c(-2,2))+
  theme_light()

ggsave("_FIGURES//FIGOA2_fevs_suff_know.pdf",  device = "pdf", width = 2, height = 2, scale = 1.5, units = "in")

ggplot(scores, aes(x=CAPACITY_SCORE, y=SHARE_KNOWLEDGE))+
  geom_point(color = "gray50", alpha = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  scale_y_continuous(name = "Share Knowledge", lim = c(0,1), breaks = c(0,1))+
  scale_x_continuous(name = "Capacity", lim = c(-4, 4), breaks = c(-2,2))+
  theme_light()

ggsave("_FIGURES//FIGOA2_fevs_share_know.pdf",  device = "pdf", width = 2, height = 2, scale = 1.5, units = "in")

ggplot(scores, aes(x=CAPACITY_SCORE, y=MANAGER_PROMOTE_COLLAB))+
  geom_point(color = "gray50", alpha = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  scale_y_continuous(name = "Share Knowledge", lim = c(0,1), breaks = c(0,1))+
  scale_x_continuous(name = "Capacity", lim = c(-4, 4), breaks = c(-2,2))+
  theme_light()

ggsave("_FIGURES//FIGOA2_fevs_manager_collab.pdf",  device = "pdf", width = 2, height = 2, scale = 1.5, units = "in")

ggplot(scores, aes(x=CAPACITY_SCORE, y=COOPERATE))+
  geom_point(color = "gray50", alpha = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  scale_y_continuous(name = "Cooperate", lim = c(0,1), breaks = c(0,1))+
  scale_x_continuous(name = "Capacity", lim = c(-4, 4), breaks = c(-2,2))+
  theme_light()

ggsave("_FIGURES//FIGOA2_fevs_cooperate.pdf",  device = "pdf", width = 2, height = 2, scale = 1.5, units = "in")

ggplot(scores, aes(x=CAPACITY_SCORE, y=RECRUIT))+
  geom_point(color = "gray50", alpha = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  scale_y_continuous(name = "Recruit", lim = c(0,1), breaks = c(0,1))+
  scale_x_continuous(name = "Capacity", lim = c(-4, 4), breaks = c(-2,2))+
  theme_light()

ggsave("_FIGURES//FIGOA2_fevs_recruit.pdf",  device = "pdf", width = 2, height = 2, scale = 1.5, units = "in")

ggplot(scores, aes(x=CAPACITY_SCORE, y=QUALITY_WORKUNIT))+
  geom_point(color = "gray50", alpha = 0.1)+
  geom_smooth(method = "lm", color = "black")+
  scale_y_continuous(name = "Quality Work", lim = c(0,1), breaks = c(0,1))+
  scale_x_continuous(name = "Capacity", lim = c(-4, 4), breaks = c(-2,2))+
  theme_light()

ggsave("_FIGURES//FIGOA2_fevs_quality.pdf",  device = "pdf", width = 2, height = 2, scale = 1.5, units = "in")

fevs_m1 <- plm(CAPACITY_SCORE ~ SUFFICIENT_KNOWLEDGE, data=scores, effect = "twoways", model = "within", index = c("FEDSCOPE_ID","DATE"))
fevs_m2 <- plm(CAPACITY_SCORE ~ MANAGER_PROMOTE_COLLAB, data=scores, effect = "twoways", model = "within", index = c("FEDSCOPE_ID","DATE"))
fevs_m3 <- plm(CAPACITY_SCORE ~ COOPERATE, data=scores, effect = "twoways", model = "within", index = c("FEDSCOPE_ID","DATE"))
fevs_m4 <- plm(CAPACITY_SCORE ~ SHARE_KNOWLEDGE, data=scores, effect = "twoways", model = "within", index = c("FEDSCOPE_ID","DATE"))
fevs_m5 <- plm(CAPACITY_SCORE ~ RECRUIT, data=scores, effect = "twoways", model = "within", index = c("FEDSCOPE_ID","DATE"))
fevs_m6 <- plm(CAPACITY_SCORE ~ QUALITY_WORKUNIT, data=scores, effect = "twoways", model = "within", index = c("FEDSCOPE_ID","DATE"))

################################################
##### 9.0 OA4 Rulemaking Robustness Checks #####
################################################

##### 9.1 Model 1 - Bivariate - Capacity Score - All Rules
m4 <- coxph(Surv(DURATION, EVENT,  type="right") ~ CAPACITY_SCORE_SCALE, 
            data = rulemaking,
            cluster = rulemaking$PARENT_AGENCY,
            robust = T)

m4_robustse <- as.data.frame(summary(m4)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')

##### 9.2 Model 2 - Full Model - Capacity Score - All Rules
m5 <- coxph(Surv(DURATION, EVENT,  type="right") ~ CAPACITY_SCORE_SCALE+ALIGNED_PRES+ideo_rating+DecisionMaker_Est+WORKLOAD+strata(ADMIN), 
            data = rulemaking,
            cluster = rulemaking$PARENT_AGENCY,
            robust = T)

m5_robustse <- as.data.frame(summary(m5)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')

##### 9.3 Model 3 - Full Model - Capacity Score - All Rules
m6 <- coxph(Surv(DURATION_FT_NPRM, EVENT_FT,  type="right") ~ CAPACITY_SCORE_SCALE+ALIGNED_PRES+ideo_rating+DecisionMaker_Est+WORKLOAD+strata(ADMIN), 
            data = rulemaking,
            cluster = rulemaking$PARENT_AGENCY,
            robust = T)

m6_robustse <- as.data.frame(summary(m6)[[7]]) %>% dplyr::select('robust se') %>% rename(rse = 'robust se')

##### 9.4 Table
stargazer(m4,m5,m6,
          title = "Estimated Days to Final Rule, All Rulemakings: First Term of the Bush, Obama, and Trump Administrations",
          style = "APSR",
          label = "tab:duration_allrules",
          dep.var.labels = "Hazard Rate of Rulemaking",
          model.numbers = T,
          column.separate = c(2,1),
          covariate.labels =star_labels_1,
          order = star_order_1,
          model.names = F,
          star.cutoffs = c(0.05,0.01,0.001),
          digits = 2,
          digits.extra = 2,
          se=c(m4_robustse, m5_robustse, m6_robustse),
          add.lines = list(c("\\hline Presidency Strata", "No", "Yes", "Yes"),
                           c("First-Term Censor", "No", "No", "Yes"),
                           c("Estimator", "Cox", "Cox", "Cox")),
          notes = "\\parbox[t]{3.75in}{Standard errors clustered at the agency level. All continuous variables normalized prior to estimation.}",
          keep.stat = c("n"),
          out = "_TABLES/TABOA10_RulemakingDuration_AllRules.tex")


###################################################
##### 10.0 OA5 Independence Robustness Checks #####
###################################################

##### 10.1 Creating Indicators for Agency Structure

scores <- scores %>%
          mutate(EXEC_AGY = ifelse(str_detect(AGENCY, "Department") == T, 1, 0),
                 IND_COMMN = ifelse(ABBR == "CFPB" |
                                    ABBR == "CFTC" |
                                    ABBR == "CPSC" |
                                    ABBR == "DNFSB" |
                                    ABBR == "EEOC" |
                                    ABBR == "FCA" |
                                    ABBR == "FCC" |
                                    ABBR == "FDIC" |
                                    ABBR == "FEC" |
                                    ABBR == "FLRA" |
                                    ABBR == "FMC" |
                                    ABBR == "FMSHRC" |
                                    ABBR == "FTC" |
                                    ABBR == "MSPB" |
                                    ABBR == "NCUA" |
                                    ABBR == "NLRB" |
                                    ABBR == "NTSB" |
                                    ABBR == "OSHRC" |
                                    ABBR == "SEC" |
                                    ABBR == "STB" |
                                    ABBR == "USITC" |
                                    ABBR == "CSHIB" |
                                    ABBR == "EAC" |
                                    ABBR == "FRTIB" |
                                    ABBR == "IAF" |
                                    ABBR == "NMB" | 
                                    ABBR == "PCLOB"|
                                    ABBR == "RRB", 1, 0)) 

##### 10.2 Estimating Models for Executive Agencies

indep_model1 <- lm(dec_form2, data = scores[scores$EXEC_AGY == 1,])
indep_model1_clust <- coeftest(indep_model1, vcovCL(indep_model1, cluster = scores$FEDSCOPE_ID[scores$EXEC_AGY == 1], type = "HC1")) #Clustering Standard errors
indep_model1_se <- indep_model1_clust[,2] #Extracting Standard Errors

indep_model2 <- lm(pol_form2, data = scores[scores$EXEC_AGY == 1,])
indep_model2_clust <- coeftest(indep_model2, vcovCL(indep_model2, cluster = scores$FEDSCOPE_ID[scores$EXEC_AGY == 1], type = "HC1")) #Clustering Standard errors
indep_model2_se <- indep_model2_clust[,2] #Extracting Standard Errors

print("Mean: Politicial Review - Department Agencies")
mean(scores$PoliticalReview_Est[scores$EXEC_AGY == 1], na.rm = T)
print("SD: Politicial Review - Department Agencies")
sd(scores$PoliticalReview_Est[scores$EXEC_AGY == 1], na.rm = T)
print("Mean: Politicial Review - Independent Agencies")
mean(scores$PoliticalReview_Est[scores$EXEC_AGY == 0], na.rm = T)
print("Mean: Politicial Review - Independent Agencies")
sd(scores$PoliticalReview_Est[scores$EXEC_AGY == 0], na.rm = T)

##### 10.3 Estimating Models for Independent Agencies

indep_model3 <- lm(dec_form2, data = scores[scores$EXEC_AGY == 0,])
indep_model3_clust <- coeftest(indep_model3, vcovCL(indep_model3, cluster = scores$FEDSCOPE_ID[scores$EXEC_AGY == 0], type = "HC1")) #Clustering Standard errors
indep_model3_se <- indep_model3_clust[,2] #Extracting Standard Errors

indep_model4 <- lm(pol_form2, data = scores[scores$EXEC_AGY == 0,])
indep_model4_clust <- coeftest(indep_model4, vcovCL(indep_model4, cluster = scores$FEDSCOPE_ID[scores$EXEC_AGY == 0], type = "HC1")) #Clustering Standard errors
indep_model4_se <- indep_model4_clust[,2] #Extracting Standard Errors

star_order <- paste("^",c("DecisionMaker_Est", "PoliticalReview_Est", "ideo_rating", "YRS_SINCE_CREATION", "Constant"), "$", sep = "")
star_labels <- c("Decision-Maker Independence", "Political-Review Independence", "Agency Ideology", "Age (Hundreds of Years)", "Constant")


##### 10.4 Table

stargazer(indep_model1 , indep_model2, indep_model3, indep_model4,
          title = "Model Estimates of Effect of Independence on Capacity, Agency Subsets",
          style = "default",
          label = "tab:independence_subsets",
          dep.var.labels = "Policymaking Capacity",
          model.numbers = T,
          dep.var.caption = "",
          covariate.labels = star_labels,
          keep = star_order,
          order = star_order,
          model.names = F,
          single.row = F,
          star.cutoffs = c(0.05,0.01,0.001),
          digits = 2,
          digits.extra = 0,
          add.lines = list(c("\\hline Subset", "Department", "Department", "Independent", "Independent"),
                          c("Agency Fixed Effects", "No", "Yes", "No", "Yes"),
                           c("Topic Fixed Effects", "No", "Yes", "No", "Yes"),
                           c("Estimator", "OLS", "OLS", "OLS", "OLS")),
          se=list(indep_model1_se, indep_model2_se, indep_model3_se, indep_model4_se),
          notes = "Standard errors clustered at agency level.",
          omit.stat = c("logrank", "wald", "F", "ser", "chi2", "adj.rsq"),
          out = "_TABLES/TABOA11_independence_subsets.tex")


##### 10.5 Estimating Effect Size with Independent Commission Indicator

indicator_form <- formula(CAPACITY_SCORE~IND_COMMN)
indep_model5 <- lm(indicator_form, data = scores)
indep_model5_clust <- coeftest(indep_model5, vcovCL(indep_model5, cluster = scores$FEDSCOPE_ID, type = "HC1")) #Clustering Standard errors
indep_model5_se <- indep_model5_clust[,2] #Extracting Standard Errors


indep_model6 <- lm(CAPACITY_SCORE~IND_COMMN+ideo_rating+as.factor(DATE)+YRS_SINCE_CREATION+as.factor(CAP_MAIN), data = scores)
indep_model6_clust <- coeftest(indep_model6, vcovCL(indep_model6, cluster = scores$FEDSCOPE_ID, type = "HC1")) #Clustering Standard errors
indep_model6_se <- indep_model6_clust[,2] #Extracting Standard Errors



star_order <- paste("^",c("IND_COMMN", "ideo_rating", "YRS_SINCE_CREATION", "Constant"), "$", sep = "")
star_labels <- c("Independent Commission", "Agency Ideology", "Age (Hundreds of Years)", "Constant")

##### 10.6 Table

stargazer(indep_model5, indep_model6,
          title = "Model Estimates of Effect of Independence Commissions on Capacity",
          style = "default",
          label = "tab:indep_commn",
          dep.var.labels = "Policymaking Capacity",
          model.numbers = T,
          dep.var.caption = "",
          covariate.labels = star_labels,
          keep = star_order,
          order = star_order,
          model.names = F,
          single.row = F,
          star.cutoffs = c(0.05,0.01,0.001),
          digits = 2,
          digits.extra = 0,
          se=list(indep_model5_se, indep_model6_se),
          add.lines = list(c("Topic Fixed Effects", "No", "Yes"),
                           c("Estimator", "OLS", "OLS")),
          notes = "Standard errors clustered at agency level.",
          omit.stat = c("logrank", "wald", "F", "ser", "chi2", "adj.rsq"),
          out = "_TABLES/TABOA12_indep_commn.tex")



###################################
##### 8.0 OA5 Politicization  #####
###################################

##### 4.1 Descriptive Statistics

#### 4.1.1 Politicization 
print("Mean: Politicization")
mean(scores$POLITICIZATION, na.rm =T)
print("SD: Politicization")
sd(scores$POLITICIZATION, na.rm =T)

#### 4.1.3 Correlation of Supervisors and Capacity Score
print("Correlation: Supervisors and Capacity Score")
cor(scores$SUPERVISORS, scores$CAPACITY_SCORE, "pairwise.complete.obs")

##### 4.2 Main Politicization Analysis

#### 4.2.1 Model 1
model1 <- plm(CAPACITY_SCORE ~ POLITICIZATION+YRS_SINCE_CREATION+as.factor(DATE), data=scores, effect = "individual", model = "within", index = "FEDSCOPE_ID")
model1_adj <- coeftest(model1, plm::vcovHC(model1, type = "HC1", cluster = "group")) #Clustering Standard errors
model1_se <- model1_adj[,2] #Extracting Standard Errors

#### 4.2.2 Model 2
model2 <- plm(CAPACITY_SCORE ~ POLITICIZATION*OPPOSED_AGY+YRS_SINCE_CREATION+as.factor(DATE), data=scores, effect = "individual", model = "within", index = "FEDSCOPE_ID")
model2_adj <- coeftest(model2, plm::vcovHC(model2, type = "HC1", cluster = "group")) #Clustering Standard errors
model2_se <- model2_adj[,2] #Extracting Standard Errors

##### 4.2.3 Table

star_order <- paste("^",c("POLITICIZATION", "OPPOSED_AGY", "POLITICIZATION:OPPOSED_AGY", "YRS_SINCE_CREATION"), "$", sep = "")
star_labels <- c("Politicization", "Opposed", "Politicization $\\times$ Opposed", "Age (Hundreds of Years)")

stargazer(model1, model2,
          title = "Model Estimates of Effect of Politicization on Capacity",
          style = "default",
          label = "tab:politicization",
          dep.var.labels = "Policymaking Capacity",
          model.numbers = T,
          dep.var.caption = "",
          covariate.labels = star_labels,
          keep = star_order,
          order = star_order,
          model.names = F,
          single.row = F,
          star.cutoffs = c(0.05,0.01,0.001),
          digits = 2,
          digits.extra = 0,
          se=list(model1_se, model2_se),
          notes = "Standard errors clustered at agency level.",
          omit.stat = c("logrank", "wald", "F", "ser", "chi2", "adj.rsq"),
          out = "_TABLES/TABOA13_politicization.tex")


#### 4.2.4 Effect sizes
sd(scores$POLITICIZATION,na.rm = T)*coef(model2)[1]/sd(scores$CAPACITY_SCORE,na.rm =T)
sd(scores$POLITICIZATION,na.rm = T)*coef(model2)[1]-coef(model2)[27]/sd(scores$CAPACITY_SCORE,na.rm =T)

##### 4.3 Two Way Fixed Effects


model1 <- plm(CAPACITY_SCORE ~ POLITICIZATION+YRS_SINCE_CREATION, data=scores, effect = "twoways", model = "within", index = c("FEDSCOPE_ID","DATE"))
model1_adj <- coeftest(model1, plm::vcovHC(model1, type = "HC1", cluster = "group")) #Clustering Standard errors
model1_se <- model1_adj[,2] #Extracting Standard Errors


stargazer(model1, 
          title = "Model Estimates of Effect of Politicization on Capacity, Two-Way Fixed Effects",
          style = "default",
          label = "tab:politicization_twfe",
          dep.var.labels = "Policymaking Capacity",
          model.numbers = T,
          dep.var.caption = "",
          covariate.labels = star_labels,
          keep = star_order,
          order = star_order,
          model.names = F,
          single.row = F,
          star.cutoffs = c(0.05,0.01,0.001),
          digits = 2,
          digits.extra = 0,
          se=list(model1_se),
          notes = "Standard errors clustered at agency level.",
          omit.stat = c("logrank", "wald", "F", "ser", "chi2", "adj.rsq"),
          out = "_TABLES/TABOA14_politicization_twfe.tex")


sink()
