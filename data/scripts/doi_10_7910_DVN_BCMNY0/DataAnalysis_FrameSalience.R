### install important packages
install.packages("tidyverse")
library(tidyverse)
install.packages("stargazer")
library(stargazer)
install.packages("mediation")
library(mediation)
install.packages("rcompanion")
library(rcompanion)
install.packages("FSA")
library(FSA)
install.packages("jtools")
library(jtools)
library(vtable)
install.packages("ggstance")
library(ggstance)
library(effects)
install.packages("cowplot")
library(cowplot)
install.packages("ggpubr")
library(ggpubr)

library(foreign)
library(nnet)
install.packages("aod")
library(aod)

### Main variables for analysis:
# Q34 - To what extent do you support the protest? 1 being strongly oppose; 6 being strongly support
# Q35 - To what extent do you think the reason why people protest is justified? 1 being completely unjustified and 6 completely justified
# Q36- Distribution of protest issue (democracy or land issues)
# Q37_1- Whether the protest is mainly about land issue?
# Protest - randomized groups


Numeric_Final <- subset(Numeric_Final, subset=!(Finished=="0"))
Numeric_Final <- subset(Numeric_Final, subset =!(Q1=="2"))

Text_Final <- subset(Text_Final, subset=!(Finished=="FALSE"))
Text_Final <- subset(Text_Final, subset =!(Q1=="No"))

## DV distribution - Figure 1 in the article 

Text_Final$Q34 = factor(Text_Final$Q34, levels = c('Strongly oppose', 'Oppose', 'Somewhat oppose', 'Somewhat support','Support','Strongly support'))
Text_Final%>%count(Q34)


graph1 <- ggplot(Text_Final, aes(x = Q34)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill='gray48') +
  xlab("Support for Protest") +
  ylab("Percentage Respondents") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12), # Change size for x-axis text
    axis.text.y = element_text(color = "black", size = 12), # Change size for y-axis text
    axis.title.x = element_text(color = "black", size = 14), # Change size for x-axis title
    axis.title.y = element_text(color = "black", size = 14)  # Change size for y-axis title
  )

graph1

Text_Final$Q35 = factor(Text_Final$Q35, levels = c('Completely unjustified', 'Unjustified', 'Somewhat unjustified', 'Somewhat justified','Justified','Completely justified'))



graph2 <- ggplot(Text_Final, aes(x = Q35)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill='gray48') +
  xlab("Protest Justification") +
  ylab("Percentage Respondents") +
  scale_y_continuous(labels = scales::percent) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12), # Change size for x-axis text
    axis.text.y = element_text(color = "black", size = 12), # Change size for y-axis text
    axis.title.x = element_text(color = "black", size = 14), # Change size for x-axis title
    axis.title.y = element_text(color = "black", size = 14)  # Change size for y-axis title
  )

graph2

### combine two graph - Figure 1 in the article 
ggarrange(graph1, graph2,
          common.legend = TRUE, legend = "bottom")


## means of protest support - input for Figure 2 in the article 
## support

Sum = groupwiseMean(Q34 ~ Protest,
                    data   = Numeric_Final,
                    conf   = 0.95,
                    digits = 3, na.rm = TRUE)
Sum


graph3 <- ggplot(Sum,                ### The data frame to use.
                 aes(x = Protest,
                     y = Mean, group = 1)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper),
                width = 0.05, 
                size = 0.5) +
  geom_point(shape = 15, 
             size = 4) + 
  ggtitle("Effect of Protest Frame on Protest Support") +
  geom_line(aes(x = 1:nrow(Sum), y = Mean)) +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(size = 9.20, color = "black"),
    plot.title = element_text(color = "black", size=12,face="bold")
  ) +
  ylab("Mean Protest Support") + 
  scale_y_continuous(limits = c(1, 6),
                     breaks = c(1, 2, 3, 4, 5, 6),
                     labels = c("Strongly Oppose", "Oppose", "Somewhat Oppose",
                                "Somewhat Support", "Support", "Strongly Support"))


graph3
## Protest Justification
Sum1 = groupwiseMean(Q35 ~ Protest,
                     data   = Numeric_Final,
                     conf   = 0.95,
                     digits = 3, na.rm = TRUE)
Sum1

graph4 <- ggplot(Sum1,                ### The data frame to use.
                 aes(x = Protest,
                     y = Mean, group = 1)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper),
                width = 0.05, 
                size = 0.5) +
  geom_line(aes(x = 1:nrow(Sum1), y = Mean)) +
  geom_point(shape = 15, 
             size = 4) +
  ggtitle("Effect of Protest Frame on Protest Justification") +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(size = 9.20, color = "black"),
    plot.title = element_text(color = "black",size = 12,face = "bold")
  ) +
  ylab("Mean Protest Justification") +
  scale_y_continuous(limits = c(1, 6),
                     breaks = c(1, 2, 3, 4, 5, 6),
                     labels = c("Strongly Unjustified", "Unjustified", "Somewhat Unjustified",
                                "Somewhat Justified", "Justified", "Strongly Justified"))

graph4

## Combine graph 3 and 4 - Figure 2 in the article 


ggarrange(graph3, graph4,
          common.legend = TRUE, legend = "bottom")



### Mechanism
## Descriptive manipulation check - Figure 3 in the article 
Text_Final$Protest<-Numeric_Final$Protest
Text_Final$Issue<-Text_Final$Q36


ggplot(Text_Final, aes(Protest, ..count..)) + 
  geom_bar(aes(fill = Issue), position = "dodge") +
  xlab("Protest") +  # Customize x-axis label
  ylab("Count") +  # Customize y-axis label
  theme(
    axis.title.x = element_text(size = 14, color = "black"),  # Set font size and color for x-axis title
    axis.title.y = element_text(size = 14, color = "black"),  # Set font size and color for y-axis title
    axis.text.x = element_text(size = 12, color = "black"),   # Set font size and color for x-axis labels
    axis.text.y = element_text(size = 12, color = "black")    # Set font size and color for y-axis labels
  )

## This protest is mainly about land issue - Figure 4 in the article 

Sum2 = groupwiseMean(Q37_1 ~ Protest,
                     data   = Numeric_Final,
                     conf   = 0.95,
                     digits = 3, na.rm = TRUE)


graph5 <- ggplot(Sum2,                ### The data frame to use.
                 aes(x = Protest,
                     y = Mean, group = 1)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper),
                width = 0.05, 
                size = 0.5) +
  geom_line(aes(x = 1:nrow(Sum1), y = Mean)) +
  geom_point(shape = 15, 
             size = 4) +
  ggtitle("Effect of Protest Frame on Perceived Protest Issue ") +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(size = 13, color = "black"),
    plot.title = element_text(color = "black",size = 14,face = "bold")
  ) +
  ylab("Mean Protest Justification") +
  scale_y_continuous(limits = c(1, 6),
                     breaks = c(1, 2, 3, 4, 5, 6),
                     labels = c("Strongly Disagree", "Disagree", "Somewhat Disagree",
                                "Somewhat Agree", "Agree", "Strongly Agree"))

graph5

### Table 2 in the article: Mediated effects of protest frames through perception of protest issues

### Protest support

Numeric_Final$Q3<-as.factor(Numeric_Final$Q3)

Numeric_Final$age<-as.numeric(Numeric_Final$age)
Med.fit_cov1 <- lm(Q37_1 ~ Protest+Q3+province+Edu+Q9+Q11+Q13+econ+pride+PartyMembership+News+ trust+age, data = Numeric_Final)
out.fit_cov1<-lm(Q34~Q37_1+Protest+Q3+province+Edu+Q9+Q11+Q13+econ+pride+PartyMembership+News+ trust+age,data = Numeric_Final)
med.out_cov1<-mediate(Med.fit_cov1,out.fit_cov1,treat = "Protest",mediator = "Q37_1", control.value = "Prodemocracy Frame", treat.value = "Land and Prodemocracy Frame", robustSE = TRUE, sims = 10000)
summary(med.out_cov1)


### Protest justification

Med.fit_cov2 <- lm(Q37_1 ~ Protest+Q3+province+Edu+Q9+Q11+Q13+econ+pride+PartyMembership+News+ trust+age, data = Numeric_Final)
out.fit_cov2<-lm(Q35~Q37_1+Protest+Q3+province+Edu+Q9+Q11+Q13+econ+pride+PartyMembership+News+ trust+age,data = Numeric_Final)
med.out_cov2<-mediate(Med.fit_cov2,out.fit_cov2,treat = "Protest",mediator = "Q37_1", control.value = "Prodemocracy Frame", treat.value = "Land and Prodemocracy Frame", robustSE = TRUE, sims = 10000)
summary(med.out_cov2)



### Balance test - Appendix 1

## recode prior to balance table: https://www.youtube.com/watch?v=tQredW74x_o&t=330s

## province: Hanoi and HCM equals "1", others = "0"

Text_Final$province <- recode(Text_Final$Q5, 'Hà Nội' = 1, `TP Hồ Chí Minh` = 1, .default = 0)
Text_Final%>%count(province)
Text_Final$province[Text_Final$province=="1"] <- "City"
Text_Final$province[Text_Final$province=="0"] <- "Others"

Numeric_Final$province <- recode(Numeric_Final$Q5, '24' = 1, `58` = 1, .default = 0)
Numeric_Final%>%count(province)
Numeric_Final$province[Numeric_Final$province=="1"] <- "Major City"
Numeric_Final$province[Numeric_Final$province=="0"] <- "Others"


## Education
Text_Final$Q6[Text_Final$Q6=="Less than high school"] <- "No College Degree"
Text_Final$Q6[Text_Final$Q6=="Completed high school"] <- "No College Degree"
Text_Final$Q6[Text_Final$Q6=="Vocational degree"] <- "No College Degree"
Text_Final$Q6[Text_Final$Q6=="Incomplete University degree"] <- "No College Degree"
Text_Final$Q6[Text_Final$Q6=="University degree (BA, BSc, etc.,)"] <- "College Degree"
Text_Final$Q6[Text_Final$Q6=="Advanced degree (MA, MSc, M.Phil, etc.,)"] <- "College Degree"
Text_Final%>%count(Q6)


## gender
Numeric_Final$Q3[Numeric_Final$Q3=="2"] <- "0"


## trust
Numeric_Final$trust <- rowMeans(cbind(Numeric_Final$Q21_1, Numeric_Final$Q21_2, Numeric_Final$Q21_3, Numeric_Final$Q21_4))
Numeric_Final$trust<-ifelse(Numeric_Final$trust > 3.75, 1, 0)
Numeric_Final$trust[Numeric_Final$trust=="1"] <- "High trust"
Numeric_Final$trust[Numeric_Final$trust=="0"] <- "Low trust"

## National pride
Numeric_Final$pride <- rowMeans(cbind(Numeric_Final$Q19, Numeric_Final$Q20))

## Party membership: 1 means party member, 0 means no

Numeric_Final$PartyMembership <- recode(Numeric_Final$Q10, '1' = 1, `1,3,5` = 1,'1,4,5' = 1, '1,2,4,5' = 1, '1,2,3,4' = 1, '1,2,5' = 1, '1,5' = 1, '1,2' = 1, '1,4' = 1, '1,3' = 1,'1,2,4' = 1,'1,7' = 1, '1,3,4,5' = 1, '1,2,3,5' = 1, '1,2,3' = 1, '1,2,3,5,6' = 1, '1,8' = 1,'1,6,7' = 1, '1,6' = 1, .default = 0)
Numeric_Final%>%count(PartyMembership)
Numeric_Final$PartyMembership[Numeric_Final$PartyMembership=="1"] <- "Party member"
Numeric_Final$PartyMembership[Numeric_Final$PartyMembership=="0"] <- "No Party member"


## Agriculture
Numeric_Final$Q11[Numeric_Final$Q11=="2"] <- "0"
Numeric_Final%>%count(Q11)

## economic perception
Numeric_Final$econ<-rowMeans(cbind(Numeric_Final$Q14, Numeric_Final$Q15))

## Age
Numeric_Final$age<-Text_Final$Q4
Numeric_Final$age<-as.numeric(Numeric_Final$age)



## create a new dataset with relevant covariates 
covs1 <- subset(Numeric_Final, select = c(FL_88_DO,Q3,province,Q6,Q10,Q9,Q11,Q13,econ,Q16,pride,trust,age))

covs1$Q3<-as.numeric(covs1$Q3)
covs1$province<-as.numeric(covs1$province)
covs1$Q6<-as.numeric(covs1$Q6)
covs1$Q9<-as.numeric(covs1$Q9)
covs1$Q11<-as.numeric(covs1$Q11)
covs1$Q13<-as.numeric(covs1$Q13)
covs1$econ<-as.numeric(covs1$econ)
covs1$Q16<-as.numeric(covs1$Q16)
covs1$pride<-as.numeric(covs1$pride)
covs1$trust<-as.numeric(covs1$trust)
covs1$age<-as.numeric(covs1$age)
covs1$Q10<-as.numeric(covs1$Q10)

## create the balance table - Appendix 1
sumtable(covs1, group = 'FL_88_DO', group.test = TRUE)
hist(Numeric_Final_24$trust)


### Analyze groups with land or prodemocracy frame first

# merge randomized frames and group

Numeric_Final$Coalition_Frame_Random<-Numeric_Final$Protest
Numeric_Final$Coalition_Frame_Random <- paste0(Numeric_Final$Protest, Numeric_Final$Policy.democracyCoalition_DO)
Numeric_Final%>%count(Coalition_Frame_Random)

Numeric_Final$Coalition_Frame_Random[Numeric_Final$Coalition_Frame_Random=="Land and Prodemocracy FrameQ27|Q28|Q29|Q30|Q31|Q32"] <- "Coalition Democracy First"

Numeric_Final$Coalition_Frame_Random[Numeric_Final$Coalition_Frame_Random=="Land and Prodemocracy FrameQ27|Q28|Q31|Q30|Q29|Q32"] <- "Coalition Land First"
Democracy_First<- subset(Numeric_Final, subset=!(Coalition_Frame_Random=="Coalition Land First"))
Land_First<- subset(Numeric_Final, subset=!(Coalition_Frame_Random=="Coalition Land First"))

## Land first- Appendix 3

Land_First$Protest<-as.factor(Land_First$Protest)
Land_First$Protest = relevel(Land_First$Protest, ref = "Land Frame")
model1<-lm(Q34~Protest,data =Land_First)
model2<-lm(Q35~Protest,data =Land_First)
stargazer(model1,model2,type = "text")

## Pro-democracy first - Appendix 4

Democracy_First$Protest<-as.factor(Democracy_First$Protest)
Democracy_First$Protest = relevel(Democracy_First$Protest, ref = "Land Frame")
model1<-lm(Q34~Protest,data =Democracy_First)
model2<-lm(Q35~Protest,data =Democracy_First)
stargazer(model1,model2,type = "text")

## Average support index - Appendix 5

Numeric_Final$Average_Support <- rowMeans(cbind(Numeric_Final$Q34, Numeric_Final$Q35))


Sum6 = groupwiseMean(Average_Support ~ Protest,
                     data   = Numeric_Final,
                     conf   = 0.95,
                     digits = 3, na.rm = TRUE)
Sum6

graph7<-ggplot(Sum6,                ### The data frame to use.
               aes(x = Protest,
                   y = Mean, group=1)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper),
                width = 0.05, 
                size  = 0.5) +
  geom_point(shape = 15, 
             size  = 4) + ggtitle("Effect of Protest Frame on Protest Support" )+ geom_line(aes(x=1:nrow(Sum6),y = Mean)) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) + theme(axis.text = element_text(size = 13)) +
  
  ylab("Mean Protest Support") + scale_y_continuous(limits = c(1,6),
                                                    breaks=c(1,2,3,4,5,6))





### Test if they support democracy as a means to achieve policy protests- APpendix 11

## Is democracy needed 

Sum8 = groupwiseMean(Q53 ~ Protest,
                     data   = Numeric_Final,
                     conf   = 0.95,
                     digits = 3, na.rm = TRUE)

Sum8

graph9<-ggplot(Sum8,                ### The data frame to use.
               aes(x = Protest,
                   y = Mean,group=1)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper),
                width = 0.05, 
                size  = 0.5) + geom_line(aes(x=1:nrow(Sum8),y = Mean)) +
  geom_point(shape = 15, 
             size  = 4) + ggtitle("Democracy Is Needed" ) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) + theme(axis.text = element_text(size = 13)) +
  
  ylab("Mean Democracy Is Needed") + scale_y_continuous(limits = c(1,6),
                                                       breaks=c(1,2,3,4,5,6),
                                                       labels = c("Strongly disagree", "disagree", "Somewhat disagree",
                                                                  "Somewhat agree", "Agree", "Strongly agree"))

graph9

## using Q66

Sum9 = groupwiseMean(Q66 ~ Protest,
                     data   = Numeric_Final,
                     conf   = 0.95,
                     digits = 3, na.rm = TRUE)

Sum8

graph10<-ggplot(Sum9,                ### The data frame to use.
               aes(x = Protest,
                   y = Mean,group=1)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper),
                width = 0.05, 
                size  = 0.5) + geom_line(aes(x=1:nrow(Sum9),y = Mean)) +
  geom_point(shape = 15, 
             size  = 4) + ggtitle("Democracy Is Needed" ) +
  theme_bw() +
  theme(axis.title   = element_text(face  = "bold")) + theme(axis.text = element_text(size = 13)) +
  
  ylab("Mean Democracy Is Needed") + scale_y_continuous(limits = c(1,6),
                                                        breaks=c(1,2,3,4,5,6),
                                                        labels = c("Strongly disagree", "disagree", "Somewhat disagree",
                                                                   "Somewhat agree", "Agree", "Strongly agree"))

graph10

### Drop party members - Appendix 9

Numeric_Final <- subset(Numeric_Final, subset= (PartyMembership=="No Party member"))

## support for protest
Numeric_Final$age<-as.numeric(Numeric_Final$age)

model1<-lm(Q34~Protest+province+Edu+pride+econ+age+News+trust,data = Numeric_Final)

model2<-lm(Q35~Protest+province+Edu+pride+econ+age+News+trust,data = Numeric_Final)

stargazer(model1,model2,type = "text")




