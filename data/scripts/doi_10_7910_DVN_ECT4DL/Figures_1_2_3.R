######################## Code Summary ##################
#Replication for Polarization In COVID-19 Vaccine Discussion Networks
#American Politics Research
#Amlani, Butters, and Kiesel (2022)
#R Version: 4.1.1
#December 14 2022

#This R script makes figures 1, 2, and 3
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################
library(ggplot2)
library(margins)
library(survey)

######################## Upload Data ##################

#Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load(file = "Replication Data - Amlani, Butters, and Kiesel 2022.rda"); SN.2 <- SN.1

##################### Examine Data #########################
head(SN.2)

####################Data Management############################
SN.2$vaccine_question_update[SN.2$vaccine_question == "Yes"] <- "Vaccinated"
SN.2$vaccine_question_update[SN.2$vaccine_question == "No"] <- "Unvaccinated"

##################### Make Survey Object ###################
library(survey)
SN.3 <- svydesign(ids=SN.2$ResponseId, data=SN.2, weights=SN.2$weights) 

##################### Figures #########################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##################### Figure 1 #########################
#Stats
Plot_DF.1 <- data.frame(svytable(~SN_Vaccine_Master, design=SN.3))
Plot_DF.1$Proportion <- (Plot_DF.1$Freq/ sum(Plot_DF.1$Freq))
Plot_DF.1$Percent <- (Plot_DF.1$Freq/ sum(Plot_DF.1$Freq))*100
Plot_DF.1$SE <- (sqrt(Plot_DF.1$Proportion*(1-Plot_DF.1$Proportion )/sum(Plot_DF.1$Freq)))

sample.n <- sum(Plot_DF.1$Freq)
alpha <-  0.05
degrees.freedom = sample.n - 1
t.score <-  qt(p=alpha/2, df=degrees.freedom,lower.tail=F)

Plot_DF.1$margin.error <- t.score * Plot_DF.1$SE
Plot_DF.1$lower <- Plot_DF.1$Proportion - Plot_DF.1$margin.error
Plot_DF.1$upper <- Plot_DF.1$Proportion + Plot_DF.1$margin.error

#Plot
P1 <- ggplot(Plot_DF.1, aes(x = SN_Vaccine_Master,  y =Proportion, fill = SN_Vaccine_Master)) +
  geom_bar(stat="identity") +
  labs(x = "Number of Vaccinated Discussants in Respondents' Social Network",
       #y = "Percent",
       title = "Distribution of Vaccination in Social Networks",
       caption = "Note: Numbers represent estimated percentage with standard errors in parentheses.") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits= c(0, .50)) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.y=element_blank()) +
  geom_text(aes(label=paste(round(Percent,0), "%", "\n(",round(SE*100, 2), ")", sep = "")), position=position_dodge(width=0.9), vjust=-0.25, lineheight = 0.9); P1

#Save Plot
ggsave(P1, 
       file = "Figure 1.png",
       width=7, height=6,  dpi = 300) 


##################### Figure 2 #########################
SN.3$variables$SN_Vaccine_Master_Character[SN.3$variables$SN_Vaccine_Master == 0] <- "0 vaccinated discussants"
SN.3$variables$SN_Vaccine_Master_Character[SN.3$variables$SN_Vaccine_Master == 1] <- "1 vaccinated discussant"
SN.3$variables$SN_Vaccine_Master_Character[SN.3$variables$SN_Vaccine_Master == 2] <- "2 vaccinated discussants"
SN.3$variables$SN_Vaccine_Master_Character[SN.3$variables$SN_Vaccine_Master == 3] <- "3 vaccinated discussants"
SN.3$variables$SN_Vaccine_Master_Character <- factor(SN.3$variables$SN_Vaccine_Master_Character, levels = c(
  "0 vaccinated discussants",
  "1 vaccinated discussant",
  "2 vaccinated discussants",
  "3 vaccinated discussants"))

#Put Measure on 5pt scale
SN.3$variables$SN_Attitude_Master_Equal_5pt[SN.3$variables$SN_Attitude_Master >= 0 & SN.3$variables$SN_Attitude_Master <= 2] <- "High Resistance" #Note Inclusive of Zero Here
SN.3$variables$SN_Attitude_Master_Equal_5pt[SN.3$variables$SN_Attitude_Master > 2 & SN.3$variables$SN_Attitude_Master <= 4] <- "Resistance"
SN.3$variables$SN_Attitude_Master_Equal_5pt[SN.3$variables$SN_Attitude_Master > 4 & SN.3$variables$SN_Attitude_Master <= 6] <- "Hesitancy"
SN.3$variables$SN_Attitude_Master_Equal_5pt[SN.3$variables$SN_Attitude_Master > 6 & SN.3$variables$SN_Attitude_Master <= 8] <- "Support"
SN.3$variables$SN_Attitude_Master_Equal_5pt[SN.3$variables$SN_Attitude_Master > 8 & SN.3$variables$SN_Attitude_Master <= 10] <- "High Support"

#Calculate Key Statistics
Plot_DF.2 <- setNames(data.frame(svytable(~SN_Attitude_Master_Equal_5pt + SN_Vaccine_Master_Character, design=SN.3)), c("SN", "V", "Freq"))

Plot_DF.2_Total <- setNames(aggregate(Freq ~ V, data = Plot_DF.2, sum), c( "V", "Total"))

Plot_DF.2.1 <- merge(Plot_DF.2, Plot_DF.2_Total, by = c("V"))

Plot_DF.2.1$Proportion <- (Plot_DF.2.1$Freq / Plot_DF.2.1$Total)

Plot_DF.2.1$Percent <- (Plot_DF.2.1$Freq / Plot_DF.2.1$Total)*100

#Calculate SE and CI
Plot_DF.2.2 <- NULL
for(i in unique(Plot_DF.2.1$V)){
  DF.Loop.1<- subset(Plot_DF.2.1, V == i)
  DF.Loop.1$SE <- (sqrt(DF.Loop.1$Proportion*(1-DF.Loop.1$Proportion )/sum(DF.Loop.1$Freq)))
  
  sample.n <- sum(DF.Loop.1$Freq)
  alpha <-  0.05
  degrees.freedom = sample.n - 1
  t.score <-  qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  
  DF.Loop.1$margin.error <- t.score * DF.Loop.1$SE
  DF.Loop.1$lower <- DF.Loop.1$Proportion - DF.Loop.1$margin.error
  DF.Loop.1$upper <- DF.Loop.1$Proportion + DF.Loop.1$margin.error
  
  Plot_DF.2.2 <- rbind(Plot_DF.2.2, DF.Loop.1)
}

#Fix Levels
Plot_DF.2.2$SN <- factor(Plot_DF.2.2$SN, levels = c("High Resistance", "Resistance", "Hesitancy", "Support", "High Support"))

#Plot
P2 <- ggplot(Plot_DF.2.2, aes(x = SN,  y =Proportion, fill = SN)) +
  facet_wrap(tools::toTitleCase(as.character(Plot_DF.2.2$V)) ~ .) +
  geom_bar(stat="identity") +
  labs(x = "Attitudes about COVID-19 vaccines in respondents' social network",
       # y = "Percent",
       title = "Distribution of Attitudes About COVID-19 Vaccines",
       subtitle = "Conditional on the Number of Vaccinated Discussants",
       caption = "Note: We measure COVID-19 attitudes in respondents' social network on an 11pt scale.\nWe bins attitudes every 2 units and bins are inclusive at the high bound.\nNumbers represent estimated percentages with standard errors in parentheses within each group.") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits= c(0, .90)) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.y=element_blank()) +
  geom_text(aes(label=paste(round(Percent,0), "%", "\n(",round(SE*100, 2), ")", sep = "")), position=position_dodge(width=0.9), vjust=-0.25, lineheight = 0.9); P2


#Save Plot
ggsave(P2,
       file = "Figure 2.png",
       width=9, height=9,  dpi = 300)



##################### Figure 3 #########################
Plot_DF.3 <- setNames(data.frame(svytable(~SN_Vaccine_Master + vaccine_question_update, design=SN.3)), c("SN", "V", "Freq"))
Plot_DF.3_Total <- setNames(aggregate(Freq ~ V, data = Plot_DF.3, sum), c( "V", "Total"))

Plot_DF.3.1 <- merge(Plot_DF.3, Plot_DF.3_Total, by = c("V"))

Plot_DF.3.1$Percent <- (Plot_DF.3.1$Freq / Plot_DF.3.1$Total)*100

Plot_DF.3.1$Proportion <- (Plot_DF.3.1$Freq / Plot_DF.3.1$Total)

Plot_DF.3.2 <- NULL
for(i in unique(Plot_DF.3.1$V)){
  DF.Loop.1<- subset(Plot_DF.3.1, V == i)
  DF.Loop.1$SE <- (sqrt(DF.Loop.1$Proportion*(1-DF.Loop.1$Proportion )/sum(DF.Loop.1$Freq)))
  
  sample.n <- sum(DF.Loop.1$Freq)
  alpha <-  0.05
  degrees.freedom = sample.n - 1
  t.score <-  qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  
  DF.Loop.1$margin.error <- t.score * DF.Loop.1$SE
  DF.Loop.1$lower <- DF.Loop.1$Proportion - DF.Loop.1$margin.error
  DF.Loop.1$upper <- DF.Loop.1$Proportion + DF.Loop.1$margin.error
  
  Plot_DF.3.2 <- rbind(Plot_DF.3.2, DF.Loop.1)
}


P3 <- ggplot(Plot_DF.3.2, aes(x = SN,  y =Proportion, fill = SN)) +
  facet_wrap(V ~ .) +
  geom_bar(stat="identity") +
  labs(x = "Number of Vaccinated Discussants in Respondents' Social Network",
       # y = "Percent",
       title = "Distribution of Vaccination in Social Networks",
       subtitle = "Conditional on Vaccination Status",
       caption = "Note: Numbers represent estimated percentages with standard errors in parentheses within each group.") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits= c(0, .70)) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.y=element_blank()) +
  geom_text(aes(label=paste(round(Percent,0), "%", "\n(",round(SE*100, 2), ")", sep = "")), position=position_dodge(width=0.9), vjust=-0.25, lineheight = 0.9); P3

#Save Plot
ggsave(P3,
       file = "Figure 3.png",
       width=7, height=6,  dpi = 300)



