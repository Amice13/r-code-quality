# This code generates the knowledge score histogram in control and any intervention groups

# Please set working directory to be source file location

rm(list = ls())

library(ggplot2)
library('dplyr')
library('fastDummies')
library('readr')
library('anytime')
library('stats')
library('hdm')
library('lubridate') 
library("MASS")
library("ebal")
library('car')

# Import of useful functions
source('shortlist_functions.R')

simulation = 0
set.seed(302)

# Import of the data set
mainlaunch <- "./Data/COVID USA T1_May 27, 2020_07.20.csv"


df=read_csv(mainlaunch)[-c(1,2),]

df$StartTime <- anytime(df$StartDate) #parse
df$duration <- as.numeric(df$`Duration (in seconds)`)

raw <- df %>% filter((StartTime > anytime("2020-05-13 15:00:00 EDT")) & (DistributionChannel == "anonymous") & (Q2 == "Yes, I would like to take part in this study, and confirm that I LIVE IN THE U.S., and I am 18 or older") & is.na(test))

raw <- as.data.frame(raw)


# Pre-processing
source('basic_cleaning.R')

# Computation of weights
source('hainmueller_weights.R')


raw=raw[!is.na(raw$weights_knowledge),] # we select the observations with complete knowledge responses

controlgroup = raw[raw$anytreat==0,]
treatmentgroup = raw[raw$anytreat==1,]

hist_values = c()
lowerbound = c()
upperbound = c()

temp1 = controlgroup[!is.na(controlgroup$knowledge_count_variable),]
temp2 = treatmentgroup[!is.na(treatmentgroup$knowledge_count_variable),]


ks.test(temp1$knowledge_count_variable,temp2$knowledge_count_variable)

#Computation of the numbers for the histogram for knowledge score =0,1,2
for (i in 0:2){
  temp1$outcome = as.numeric(temp1$knowledge_count_variable==i)
  formula = as.formula(paste("outcome","  ~  ","1"))
  reg =  lm(formula = formula, data = temp1)
  coef_model = summary(reg)$coefficients
  se = coef_model[1,"Std. Error"]
  coef = coef_model[1,"Estimate"]
  
  lowerbound = c(lowerbound, round(coef - 1.96*se,3))
  upperbound = c(upperbound, round(coef + 1.96*se,3))
  
  temp2$outcome = as.numeric(temp2$knowledge_count_variable==i)
  formula = as.formula(paste("outcome","  ~  ","1"))
  reg =  lm(formula = formula, data = temp2)
  coef_model = summary(reg)$coefficients
  se = coef_model[1,"Std. Error"]
  coef = coef_model[1,"Estimate"]
  
  lowerbound = c(lowerbound, round(coef - 1.96*se,3))
  upperbound = c(upperbound, round(coef + 1.96*se,3))
  
 

  
  hist_values = c(hist_values,round(sum(temp1$knowledge_count_variable==i,na.rm=TRUE)/sum(!is.na(temp1$knowledge_count_variable)),3))
  hist_values = c(hist_values,round(sum(temp2$knowledge_count_variable==i,na.rm=TRUE)/sum(!is.na(temp2$knowledge_count_variable)),3))
}

#Computation of the numbers for the histogram for knowledge score > 2
temp1$outcome = as.numeric(temp1$knowledge_count_variable>2)
formula = as.formula(paste("outcome","  ~  ","1"))
reg =  lm(formula = formula, data = temp1)
coef_model = summary(reg)$coefficients
se = coef_model[1,"Std. Error"]
coef = coef_model[1,"Estimate"]

lowerbound = c(lowerbound, round(coef - 1.96*se,3))
upperbound = c(upperbound, round(coef + 1.96*se,3))

temp2$outcome = as.numeric(temp2$knowledge_count_variable>2)
formula = as.formula(paste("outcome","  ~  ","1"))
reg =  lm(formula = formula, data = temp2)
coef_model = summary(reg)$coefficients
se = coef_model[1,"Std. Error"]
coef = coef_model[1,"Estimate"]

lowerbound = c(lowerbound, round(coef - 1.96*se,3))
upperbound = c(upperbound, round(coef + 1.96*se,3))

hist_values = c(hist_values,round(sum(temp1$knowledge_count_variable>2,na.rm=TRUE)/sum(!is.na(temp1$knowledge_count_variable)),3))
hist_values = c(hist_values,round(sum(temp2$knowledge_count_variable>2,na.rm=TRUE)/sum(!is.na(temp2$knowledge_count_variable)),3))


# Creation of the dataframe for histogram

df1 <- data.frame(val = c(1,2,3,4,5,6,7,8), 
                   Share = hist_values,
                   lowerbound = lowerbound,
                   upperbound = upperbound,
                   Share_string = format(round(hist_values, digits=3), nsmall = 3),
                   lowerbound_string = format(round(lowerbound, digits=3), nsmall = 3),
                   upperbound_string = format(round(upperbound, digits=3), nsmall = 3),
                   Intervention = c("Control","Any_Intervention",
                                    "Control","Any_Intervention",
                                    "Control","Any_Intervention",
                                    "Control","Any_Intervention"
                                    ),
                   Value = c(0,0,1,1,2,2,3,3))

df1$ci <- paste("(",df1$lowerbound_string,", ",df1$upperbound_string,")", sep="")

df1 <- df1 %>%
  arrange(val) %>%
  mutate(Outcome = factor(Value, levels = c(0,1,2,3))) %>%
  mutate(Intervention = factor(Intervention, levels = c("Control","Any_Intervention")))

levels(df1$Outcome)[levels(df1$Outcome) =="3"] <- "3 or more"

## WITH INTERVENTIONS AS FACETS
# MEAN ODDS
# clean base plot 
plot_base_clean <- ggplot(data = df1, mapping = aes(x = Outcome, y = Share, ymin = lowerbound, ymax = upperbound,fill = Intervention)) + 
  scale_x_discrete("Knowledge gap score") +
  # apply basic black and white theme - this theme removes the background colour by default
theme_bw() + 
  # remove gridlines. Panel.grid.major is for vertical lines, Panel.grid.minor is for horizontal lines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # remove borders
        panel.border = element_blank(),
        # removing borders also removes x and y axes. Add them back
        axis.line = element_line(),
        axis.title = element_text()
        )

Share_histogram <- plot_base_clean + 
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  theme(# remove background colour from facet labels
    #strip.background  = element_blank(),
    # remove border from facet label
    panel.border = element_blank()) +
  geom_errorbar(position = position_dodge(width=0.9), width=.1) +
  geom_text(aes(label = Share_string), size = 3, vjust = -4,position = position_dodge(width=0.9)) +
  geom_text(aes(label = ci), size = 3, position = position_dodge(width=0.9), vjust = -2.5) 

last_plot()
# Saves the result
ggsave("./Output/histogram_knowledge.png", width = 10, height = 9)
