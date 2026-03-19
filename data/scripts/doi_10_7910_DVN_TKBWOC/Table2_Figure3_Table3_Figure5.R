######################## Code Summary ##################
#Replication for The Impact of Vote-By-Mail Policy on Turnout and Vote Share in the 2020 Election
#Election Law Journal 
#Amlani & Collitt (2021)
#R Version: 4.1.1
#December 8 2021

#This R script runs the turnout and vote share models, creates the regression table (Table 2 and Table 3) and makes the coefficient plots (Figures 3 & Figure 5)

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen =99)
set.seed(1993)

######################### Functions ###################
coeftest_Model_DF<- function(model){
  require(plyr)
  Estimate_DF <- setNames(data.frame(model[,1]), c("Coeff"));Estimate_DF$Label <-  rownames(Estimate_DF); rownames(Estimate_DF) <- NULL
  SE_DF <- setNames(data.frame(model[,2]), c("SE")); SE_DF$Label <-  rownames(SE_DF); rownames(SE_DF) <- NULL
  t_DF <- setNames(data.frame(model[,3]), c("t.value")); t_DF$Label <-  rownames(t_DF); rownames(t_DF) <- NULL
  p_DF <- setNames(data.frame(model[,4]), c("P.Value")); p_DF$Label <-  rownames(p_DF); rownames(p_DF) <- NULL
  
  df <- plyr::join_all(list(Estimate_DF, SE_DF, t_DF, p_DF), by = c("Label"))
  
  df2 <- df[, c(2,1,3,4,5)]
  
  #Calualte Confidence Intervals 
  df2$lower <- df2$Coeff - (qt(0.975,df=3000) * df2$SE)
  df2$upper <- df2$Coeff + (qt(0.975,df=3000) * df2$SE)
  
  #Signifigance 
  df2$P.Value <- as.numeric(df2$P.Value)
  df2$Sig.05<- ifelse(df2$P.Value < .05, 1, 0)
  df2$Sig.10<- ifelse(df2$P.Value < .1, 1, 0)
  
  return(df2)
  
}


######################### Library #####################
library(sandwich)
library(lmtest)
library(stargazer)
library(ggplot2)
library(broom)
library(multiwayvcov) # clustered standard errors
library(plyr)
######################## Upload Data ##################

#Set Working Directory
#This neat snippet sets your working directory to wherever the script is located!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load(file = "Replication Data - Amlani & Collitt 2021.rda"); VBM_Master.1 <- VBM_Master.Pure

####################### Examine Data ####################
head(VBM_Master.1)
colnames(VBM_Master.1)

####################### Data Management #####################
#********************* Subset Cases Less Than 100 Percent VAP *********************
VBM_Master.2 <- subset(VBM_Master.1, Percent_Turnout_VAP <= 100)

########################## Main Models  ##########################

#************************ Difference-in-Difference Models: Turnout ************************
#Base
B_DID.Base <- lm(Percent_Turnout_VAP ~ Conditions_Detail*as.factor(year), data = VBM_Master.2); summary(B_DID.Base)

#Control Model
B_DID.1 <- lm(Percent_Turnout_VAP ~ Conditions_Detail*as.factor(year) + 
                prcntBlack +  democratic_percent  + medianIncome + medianAge + prcntHS + daily_change_deaths_1week_100K + Automatic_Voter_Registration_Change + battleground, 
              data = VBM_Master.2); summary(B_DID.1)
#County FE
B_DID.2 <- lm(Percent_Turnout_VAP ~ Conditions_Detail*as.factor(year) + 
                prcntBlack +  democratic_percent  + medianIncome + medianAge + prcntHS + daily_change_deaths_1week_100K + Automatic_Voter_Registration_Change + battleground +
                as.factor(county_name), data = VBM_Master.2)

#County FE and State Clustered SE
B_DID.3 <- coeftest(B_DID.2, vcovCL, cluster = VBM_Master.2$state.name)

#************************ Difference-in-Difference Models: Vote Share************************
Base.VS <- lm(republican_percent ~ Conditions_Detail*as.factor(year), data = VBM_Master.2); summary(Base.VS)

#Control
B_DID.VS.1 <- lm(republican_percent ~ Conditions_Detail*as.factor(year) +
                   prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + pop_sq_mi + Unemployment_Rate + daily_change_deaths_1week_100K  + Automatic_Voter_Registration_Change + battleground, 
                 data = VBM_Master.2); summary(B_DID.VS.1)

#County FE
B_DID.VS.2 <- lm(republican_percent ~ Conditions_Detail*as.factor(year) + 
                   prcntHS + prcntBA + prcntBlack + prcntMulti + prcntHisp + prcntForeignBorn + medianIncome + pop_sq_mi +Unemployment_Rate +  daily_change_deaths_1week_100K + Automatic_Voter_Registration_Change  + battleground + 
                   as.factor(county_name), data = VBM_Master.2)

#County FE and State SE
B_DID.VS.3 <- coeftest(B_DID.VS.2, vcovCL, cluster = VBM_Master.2$state.name)


###################### Tables###################### 
#******************** Table 2: Turnout **********************
#Table 2 - OLS Regression Results for the Difference-in-Difference Turnout Analysis

B_DID.3_DF <- tidy(B_DID.3)

stargazer(list(B_DID.Base, B_DID.1, B_DID.2, B_DID.2), type = "text",omit=c("state.name", "county_name"),
          title    = "Table 2 - OLS Regression Results for the Difference-in-Difference Turnout Analysis",
          column.labels   = c("Base", "Control", "County FE", "County FE and State Clustered SE"),
          se = list(NULL, NULL, NULL, B_DID.3_DF$std.error),
          p = list(NULL, NULL, NULL, B_DID.3_DF$p.value),
          model.names = FALSE,
          column.separate = c(1, 1),
          covariate.labels = c(
            "Condition: Excuse-Needed to No Excuse-Needed",
            "Condition: Excuse-Needed to Applications Sent", 
            "Condition: No Excuse-Needed to Applications Sent",
            "Condition: No Excuse-Needed to Ballots Sent",
            "Year: 2020",
            "% Black",
            "Democratic Vote Share", 
            "Median Income", 
            "Median Age", 
            "% High School Degree",
            "Weekly Trend in COVID-19 Deaths (per 100k)",
            "Automatic Voter Registration Change: Change",
            "Battleground State",
            "Condition: Excuse-Needed to No Excuse-Needed x Year: 2020",
            "Condition: Excuse-Needed to Applications Sent x Year: 2020", 
            "Condition: No Excuse-Needed to Applications Sent x Year: 2020",
            "Condition: No Excuse-Needed to Ballots Sent x Year: 2020",
            "Constant"),
          dep.var.caption = c("Dependent Variable:"),
          dep.var.labels   = c("VAP Turnout (%)"), #Write the DV Here
          omit.stat = c("f","rsq"))


#******************** Table 3: Vote Share **********************
#Table 3 - OLS Regression Results for the Difference-in-Difference Vote Share Analysis

B_DID.VS.3_DF <- tidy(B_DID.VS.3)

stargazer(list(Base.VS, B_DID.VS.1, B_DID.VS.2, B_DID.VS.2), type = "text",omit=c("state.name", "county_name"),
          title    = "Table 3 - OLS Regression Results for the Difference-in-Difference Vote Share Analysis",
          column.labels   = c("Base","Control", "County FE", "County FE and State Clustered SE"),
          se = list(NULL, NULL, NULL, B_DID.VS.3_DF$std.error),
          p = list(NULL, NULL, NULL, B_DID.VS.3_DF$p.value),
          model.names = FALSE,
          column.separate = c(1, 1),
          covariate.labels = c(
            "Condition: Excuse-Needed to No Excuse-Needed",
            "Condition: Excuse-Needed to Applications Sent", 
            "Condition: No Excuse-Needed to Applications Sent",
            "Condition: No Excuse-Needed to Ballots Sent",
            "Year: 2020",
            "% High School Degree",
            "% Bachelor's Degree",
            "% Black",
            "% Multiracial",
            "% Hispanic",
            "% Foreign-Born",
            "Median Income", 
            "Population Density",
            "Oct. Unemployment Rate",
            "Weekly Trend in COVID-19 Deaths (per 100k)",
            "Automatic Voter Registration Change: Change",
            "Battleground State",
            "Condition: Excuse-Needed to No Excuse-Needed x Year: 2020",
            "Condition: Excuse-Needed to Applications Sent x Year: 2020", 
            "Condition: No Excuse-Needed to Applications Sent x Year: 2020",
            "Condition: No Excuse-Needed to Ballots Sent x Year: 2020",
            "Constant"),
          dep.var.caption = c("Dependent Variable:"),
          dep.var.labels   = c("Two-Party Republican Vote Share"), #Write the DV Here
          omit.stat = c("f","rsq"))


###################### Figures ###################### 

#******************** Figure 3: Turnout **********************
#Figure 3: Difference-in-Difference Estimates of the Effects of the Vote-By-Mail

#County FE and State CE
library(plyr)
Results_Master.1 <- coeftest_Model_DF(B_DID.3)

#Subset Key Term
Results_Master.2 <- subset(Results_Master.1, Results_Master.1$Label %in% paste("Conditions_Detail", unique(VBM_Master.2$Conditions_Detail),":as.factor(year)2020",sep = ""))

#Recode Labels
#Remove factor label
Results_Master.2$Label[Results_Master.2$Label == "Conditions_DetailWith Excuse To No Excuse:as.factor(year)2020"] <- "Excuse-Needed to No Excuse-Needed"
Results_Master.2$Label[Results_Master.2$Label == "Conditions_DetailWith Excuse To Applications:as.factor(year)2020"] <- "Excuse-Needed to Applications Sent"
Results_Master.2$Label[Results_Master.2$Label == "Conditions_DetailNo Excuse to Applications:as.factor(year)2020"  ] <- "No Excuse-Needed to Applications Sent"
Results_Master.2$Label[Results_Master.2$Label == "Conditions_DetailNo Excuse to VBM Election:as.factor(year)2020" ] <- "No Excuse-Needed to Ballots Sent"

#Reorder Levels
unique(Results_Master.2$Label)
Results_Master.2$Label <- factor(Results_Master.2$Label, levels = c("No Excuse-Needed to Ballots Sent","Excuse-Needed to Applications Sent", 
                                                                    "No Excuse-Needed to Applications Sent", "Excuse-Needed to No Excuse-Needed"))

#Plot
library(ggplot2)
Plot_Final <- ggplot(Results_Master.2, aes(x = Label, y = Coeff, ymin = lower, ymax = upper, color = as.factor(Sig.05))) +
  geom_pointrange() +
  coord_flip ()+
  scale_colour_manual(values = c("black", "black")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  guides(color="none") +
  theme_bw() +
  labs(subtitle = "Figure 3: Difference-in-Difference Estimates of the Effects of the Vote-By-Mail \nConditions on Turnout",
       y = "Percent Change in Turnout",
       caption = stringr::str_wrap("Note: Point estimates are dots with lines indicating 95 percent confidence intervals. The baseline/reference category is no change in VBM laws. Model controls for Democratic vote share, the weekly trend in COVID-19 deaths (per 100,000), the percentage of African Americans, median household income, median age, and percentage of individuals with only a high school diploma, battleground states, change in automatic voter registration, county fixed effects with standard errors clustered at the state level.", 100)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y=element_blank(),
        plot.caption= element_text(hjust = 0)); Plot_Final

# tiff(file = "AmlaniCollittFig3.tiff", units="in", width=8, height=6,  res = 300)
# Plot_Final
# dev.off()


#********************* Figure 5: Vote Share ***********************
#Figure 5: Difference-in-Difference Estimates of the Effect of the Vote-By-Mail

#County FE and State CE
library(plyr)
Results_Master.1 <- coeftest_Model_DF(B_DID.VS.3)

#Subset Key Term
Results_Master.2 <- subset(Results_Master.1, Results_Master.1$Label %in% paste("Conditions_Detail", unique(VBM_Master.2$Conditions_Detail),":as.factor(year)2020",sep = ""))

#Recode Labels
#Remove factor label
Results_Master.2$Label[Results_Master.2$Label == "Conditions_DetailWith Excuse To No Excuse:as.factor(year)2020"] <- "Excuse-Needed to No Excuse-Needed"
Results_Master.2$Label[Results_Master.2$Label == "Conditions_DetailWith Excuse To Applications:as.factor(year)2020"] <- "Excuse-Needed to Applications Sent"
Results_Master.2$Label[Results_Master.2$Label == "Conditions_DetailNo Excuse to Applications:as.factor(year)2020"  ] <- "No Excuse-Needed to Applications Sent"
Results_Master.2$Label[Results_Master.2$Label == "Conditions_DetailNo Excuse to VBM Election:as.factor(year)2020" ] <- "No Excuse-Needed to Ballots Sent"

#Reorder Levels
unique(Results_Master.2$Label)
Results_Master.2$Label <- factor(Results_Master.2$Label, levels = c("No Excuse-Needed to Ballots Sent","Excuse-Needed to Applications Sent", 
                                                                    "No Excuse-Needed to Applications Sent", "Excuse-Needed to No Excuse-Needed"))

#Plot
library(ggplot2)
Plot_Final <- ggplot(Results_Master.2, aes(x = Label, y = Coeff, ymin = lower, ymax = upper, color = as.factor(Sig.05))) +
  geom_pointrange() +
  coord_flip ()+
  scale_colour_manual(values = c("black", "black")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  guides(color="none") +
  theme_bw() +
  labs(subtitle = "Figure 5: Difference-in-Difference Estimates of the Effect of the Vote-By-Mail \nConditions on Two-Party Republican Vote Share ",
       y = "Percent Change in Republican Vote Share",
       caption = stringr::str_wrap("Note: Point estimates are dots with lines indicating 95 percent confidence intervals. The baseline/reference category is no change in VBM laws. Model controls for population density, unemployment one month before Election Day,the weekly trend in COVID-19 deaths (per 100,000), percent of the county with a high school degree and bachelor's degrees, battleground states, change in automatic voter registration, the percent of the county that is Black, multiracial, Latino, and foreign-born, county fixed effects with standard errors clustered at the state level.",100)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y=element_blank(),
        plot.caption= element_text(hjust = 0)); Plot_Final
# 
# tiff(file = "AmlaniCollittFig5.tiff", units="in", width=8, height=6,  res = 300)
# Plot_Final
# dev.off()
