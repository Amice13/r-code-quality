######################## Code Summary ##################
#Replication for Polarization In COVID-19 Vaccine Discussion Networks
#American Politics Research
#Amlani, Butters, and Kiesel (2022)
#R Version: 4.1.1
#December 14 2022

#This R script makes figure 6
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################
library(stargazer)
library(ggplot2)
library(marginaleffects)
library(sjPlot)
library(dplyr)
library(survey)

######################## Upload Data ##################

#Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load(file = "Replication Data - Amlani, Butters, and Kiesel 2022.rda"); SN.2 <- SN.1

##################### Examine Data #########################
head(SN.2)

##################### Encouragement Distribution ##############################
#Create Dataset
En.DF.1 <- reshape2::melt(with(SN.2, data.frame(ResponseId, SN_Encourage_Them_1_numeric, 
                                                SN_Encourage_Them_2_numeric, 
                                                SN_Encourage_Them_3_numeric, weights)), id.vars = c("ResponseId", "weights"))


#Recode
En.DF.1$Person[En.DF.1$variable %in% unique(grep("1", En.DF.1$variable, value = TRUE))] <- "Person 1"
En.DF.1$Person[En.DF.1$variable %in% unique(grep("2", En.DF.1$variable, value = TRUE))] <- "Person 2"
En.DF.1$Person[En.DF.1$variable %in% unique(grep("3", En.DF.1$variable, value = TRUE))] <- "Person 3"

En.DF.1$Direction[En.DF.1$variable %in% unique(grep("Encourage", En.DF.1$variable, value = TRUE))] <- "Encourage"

#Reshape to Dyadic Network
En.DF.2 <- reshape2::dcast(En.DF.1, ResponseId + weights + Person ~ Direction)

#Recode
En.DF.2$Indifference[En.DF.2$Encourage == 0] <- 1

#Code NA's
En.DF.2$MakeNA <- 0
En.DF.2$MakeNA[is.na(En.DF.2$Encourage) == T] <- 1
En.DF.2$MakeNA[is.na(En.DF.2$Vaccine) == T] <- 1

#*********************** Aggregate ************************
En.DF.3 <- En.DF.2 %>%
  group_by(ResponseId) %>%
  summarise(Encourage_Agg = sum(Encourage, na.rm = T),
            Ind_Agg = sum(Indifference, na.rm = T),
            MakeNA_Agg = sum(MakeNA, na.rm = T)) 

table(En.DF.3$Encourage_Agg)
table(En.DF.3$Ind_Agg)
table(En.DF.3$MakeNA_Agg)

En.DF.3$Encourage <- En.DF.3$Encourage_Agg

#Code NA's
En.DF.3$Encourage[En.DF.3$MakeNA_Agg == 3] <- NA

#Turn Congruence to be a factor:
En.DF.3$Encourage_Factor <- En.DF.3$Encourage
En.DF.3$Encourage_Factor[En.DF.3$Ind_Agg == 3] <- -4

##################### Merge  ##############################
SN.3 <- merge(SN.2, En.DF.3, by = c("ResponseId"))

##################### Make Survey Object ##############################
SN.4 <- svydesign(ids=SN.3$ResponseId, data=SN.3, weights=SN.3$weights)

##################### Vaccine Status ~ Encouragement ##############################
#Make into a Survey Table
Plot_DF.4 <- data.frame(svytable(~Encourage_Factor + vaccine_question, design=SN.4))

#Calulate Stats
Plot_DF.5 <- NULL
for(i in unique(Plot_DF.4$Encourage_Factor)){
  data.loop.1 <- subset(Plot_DF.4, Encourage_Factor == i)
  data.loop.1$Proportion <- (data.loop.1$Freq/ sum(data.loop.1$Freq))
  data.loop.1$Percent <- (data.loop.1$Freq/ sum(data.loop.1$Freq))*100
  data.loop.1$SE <- (sqrt(data.loop.1$Proportion*(1-data.loop.1$Proportion )/sum(data.loop.1$Freq)))

  Plot_DF.5 <- rbind(Plot_DF.5, data.loop.1)
}


sample.n <- sum(Plot_DF.5$Freq)
alpha <-  0.05
degrees.freedom = sample.n - 1
t.score <-  qt(p=alpha/2, df=degrees.freedom,lower.tail=F)

Plot_DF.5$margin.error <- t.score * Plot_DF.5$SE
Plot_DF.5$lower <- Plot_DF.5$Proportion - Plot_DF.5$margin.error
Plot_DF.5$upper <- Plot_DF.5$Proportion + Plot_DF.5$margin.error

##################### Figure: Bar Plot ##############################
#Recode to Plot
Plot_DF.5$Encourage_Plot <- Plot_DF.5$Encourage_Factor
Plot_DF.5$Encourage_Plot <- ifelse(Plot_DF.5$Encourage_Factor %in% c(1,2,3), paste("Vaccinated\nEncourage\n(", Plot_DF.5$Encourage_Factor, ")", sep = ""), Plot_DF.5$Encourage_Plot)
Plot_DF.5$Encourage_Plot <- ifelse(Plot_DF.5$Encourage_Factor %in% c(-1,-2,-3), paste("Unvaccinated\nEncourage\n(", Plot_DF.5$Encourage_Factor, ")", sep = ""), Plot_DF.5$Encourage_Plot)
Plot_DF.5$Encourage_Plot <- ifelse(Plot_DF.5$Encourage_Factor %in% c(-4), paste("Indifference\n(", 0, ")", sep = ""), Plot_DF.5$Encourage_Plot)
Plot_DF.5$Encourage_Plot <- ifelse(Plot_DF.5$Encourage_Factor %in% c(0), paste("Conflicting\nEncourage\n(", 0, ")", sep = ""), Plot_DF.5$Encourage_Plot)


Plot_DF.5$Encourage_Plot <- factor(Plot_DF.5$Encourage_Plot, levels = c("Indifference\n(0)",
                                                                        "Unvaccinated\nEncourage\n(-3)", 
                                                                        "Unvaccinated\nEncourage\n(-2)",
                                                                        "Unvaccinated\nEncourage\n(-1)",
                                                                        "Conflicting\nEncourage\n(0)",
                                                                        "Vaccinated\nEncourage\n(1)", 
                                                                        "Vaccinated\nEncourage\n(2)", 
                                                                        "Vaccinated\nEncourage\n(3)"))
levels(Plot_DF.5$Encourage_Plot)

#Checks
table(Plot_DF.5$Encourage_Plot, Plot_DF.5$Encourage_Factor)

P1 <- ggplot(Plot_DF.5, aes(x = Encourage_Plot,  y =Proportion, fill = vaccine_question)) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(x = "Egocentric Network Encouragement Scale",
       y = "P(Respondent is Vaccinated)\nWithin Each Bin",
       fill = "Respondents' Vaccination Status",
       title = "Distribution of Encouragement And Respondents' Vaccination Status",
       caption = "Note: Numbers represent estimated percentage with standard errors in parentheses.\nEstimates generated using weighted proportions.\nValues represent the proportion of respondents within each encouragement bin who have or have not received a COVID-19 vaccine.") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits= c(0, 1)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        plot.background = element_rect(fill = 'white', color = 'white')) +
  geom_text(aes(label=paste(round(Percent,0), "%", "\n(",round(SE*100, 2), ")", sep = "")), position=position_dodge(width=0.9), vjust=-0.25, lineheight = 0.9);P1

########################## Save ##########################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ggsave(P1,
       file = "Figure 6.png",
       width=9, height=6,  dpi = 300)

