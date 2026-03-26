######################## Code Summary ##################
#Replication for The Impact of Vote-By-Mail Policy on Turnout and Vote Share in the 2020 Election
#Election Law Journal 
#Amlani & Collitt (2021)
#R Version: 4.1.1
#December 8 2021

#This R script creates the turnout and vote share boxplots in Figure 2 and Figure 4

######################### Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen =99)
set.seed(1993)

######################### Functions ###################
fun_mean <- function(x){
  return(data.frame(y=mean(x,na.rm=T),label=round(mean(x,na.rm=T), 2)   ))}

######################### Library #####################
library(ggplot2)
######################## Upload Data ##################

#Set Working Directory
#This neat snippet sets your working directory to wherever the script is located!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data 
load("Replication Data - Amlani & Collitt 2021.rda"); VBM_Master.1 <- VBM_Master.Pure 

####################### Data Management #####################
#********************* Update to Ballots Sent *********************
VBM_Master.1$Conditions_Detail <- as.character(VBM_Master.1$Conditions_Detail)
VBM_Master.1$Conditions_Detail[VBM_Master.1$Conditions_Detail == "No Excuse to VBM Election"] <- "No Excuse to Ballots Sent"

unique(VBM_Master.1$Conditions_Detail)

#********************* Fix Levels *********************
VBM_Master.1$Conditions_Detail <- factor(VBM_Master.1$Conditions_Detail, 
                                         levels = c("Control",
                                                    "With Excuse To No Excuse",  "With Excuse To Applications",
                                                    "No Excuse to Applications", "No Excuse to Ballots Sent"))


sum(is.na(VBM_Master.1$Conditions_Detail))

#********************** 2020 Only Data *********************
VBM_Master_2020.1<- subset(VBM_Master.1, year == 2020)

#********************** Fix Levels **************************
VBM_Master_2020.1$Mail_In_Vote <- factor(VBM_Master_2020.1$Mail_In_Vote, levels = c("need an excuse", "request a mail-in ballot", "Mail-in ballot applications automatically sent", "Mail-in ballots automatically sent"))
levels(VBM_Master_2020.1$Mail_In_Vote)
table(VBM_Master_2020.1$Mail_In_Vote)

#********************** Check For Error Correction **************************
cor(VBM_Master_2020.1$daily_change_cases_1week, VBM_Master_2020.1$mean_covid_cases_1w, use = "complete")

#********************** VBM Liberalization **************************
VBM_Master_2020.1$VBM_Liberalization <- NA
VBM_Master_2020.1$VBM_Liberalization[VBM_Master_2020.1$Conditions == "Control"] <- 0
VBM_Master_2020.1$VBM_Liberalization[VBM_Master_2020.1$Conditions == "Treatment"] <- 1

table(VBM_Master_2020.1$VBM_Liberalization, VBM_Master_2020.1$Conditions)

####################### Change in Boxplots #####################

C1 <- ggplot(data = subset(VBM_Master_2020.1, Percent_Turnout_VAP <= 100), aes(x = Conditions_Detail, y = Change_Percent_Turnout_VAP)) +
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Treatment Conditions", 
       y = "Change in VAP Turnout", 
       #  title = "Change in Voting Age Population Turnout Between 2016 and 2020",
       subtitle = "Figure 2: Change in Voting Age Population Turnout Between 2016 and 2020",
       caption = "Note: Outliers Omited From Boxplot")+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  stat_summary(fun=mean, colour="black", geom="point",
               shape=18, size=3, show.legend=FALSE) +
  # stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, size=4) +
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position =  "none",
        plot.caption= element_text(hjust = 0));C1


C2 <- ggplot(data = subset(VBM_Master_2020.1, Percent_Turnout_VAP <= 100), aes(x = Conditions_Detail, y = Rep_Change_Vote_Share)) +
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Treatment Conditions", y = "Change in Republican Vote Share", 
       # title = "Figure 4: Change in Republican Vote Share between 2016 and 2020",
       subtitle = "Figure 4: Change in Republican Vote Share Between 2016 and 2020",
       caption = "Note: Outliers Omited From Boxplot")+
  theme_minimal()+
  ylim(-10, 10) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  stat_summary(fun=mean, colour="black", geom="point",
               shape=18, size=3, show.legend=FALSE) +
  # stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, size=4) +
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position =  "none",
        plot.caption= element_text(hjust = 0));C2

####################### Save #####################
#Figure 2: Change in Voting Age Population Turnout Between 2016 and 2020

# tiff(file = "AmlaniCollittFig2.tiff", units="in", width=10, height=6,  res = 300)
C1
# dev.off()


#Figure 4: Change in Republican Vote Share Between 2016 and 2020
# tiff(file = "AmlaniCollittFig4.tiff", units="in", width=10, height=6,  res = 300)
C2
# dev.off()
