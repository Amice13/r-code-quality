### JCR Lucid US survey 

## Data loading 
rm(list = ls(all = TRUE))

library(readxl)
library(tidyverse)
library(dplyr)
library(rstatix)
library(MASS)
library(mediation)
library(rstatix)
library(Rmisc)
library(jtools)
library(broom.mixed)
library(stargazer)
d167<- read_xlsx("//Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/Harvard dataset/JCR_US Lucid.xlsx")

### Data Preparation for Replication of Figure 5 and 8 

## calculate the variance/CI  of control condition  for different DVs 

d11<- d167 %>% 
  filter(TR=="0") %>%
  glimpse()

n1.2<- var(d11$FP_bi)/length(d11$FP_bi) ## US support for aggressive foreign policy 
n1.3<- var(d11$FP2_bi)/length(d11$FP2_bi) ## US support for reducing trade 
## calculate the variance/CI of conciliatory group  for different DVs 
d12<- d167 %>% ## 
  filter(TR=="4") %>%
  glimpse()

n2.2<- var(d12$FP_bi)/length(d12$FP_bi) ## US support for aggressive foreign policy 
n2.3<- var(d12$FP2_bi)/length(d12$FP2_bi) ## US support for reducing trade 

var_concilitory_2<- sqrt(n1.2+n2.2)
ci_concilitory_2<- 1.96*(var_concilitory_2)

var_concilitory_3<- sqrt(n1.3+n2.3)
ci_concilitory_3<- 1.96*(var_concilitory_3)



## calculate the variance/CI of humiliation condition for different DVs 

d13<- d167 %>% ## 
  filter(TR=="1") %>%
  glimpse()

n3.2<- var(d13$FP_bi)/length(d13$FP_bi)
n3.3<- var(d13$FP2_bi)/length(d13$FP2_bi)

var_humiliation_2<- sqrt(n1.2+n3.2)
ci_humiliation_2<- 1.96*(var_humiliation_2)

var_humiliation_3<- sqrt(n1.3+n3.3)
ci_humiliation_3<- 1.96*(var_humiliation_3)



## calculate the variance/CI of pride condition for different DVs 
d14<- d167 %>% ## 
  filter(TR=="2") %>%
  glimpse()
n4.2<- var(d14$FP_bi)/length(d14$FP_bi)
n4.3<- var(d14$FP2_bi)/length(d14$FP2_bi)

var_pride_2<- sqrt(n1.2+n4.2)
ci_pride_2<- 1.96*(var_pride_2)

var_pride_3<- sqrt(n1.3+n4.3)
ci_pride_3<- 1.96*(var_pride_3)

## calculate the variance/CI of defamatory condition for different DVs 

d15<- d167 %>% ## 
  filter(TR=="3") %>%
  glimpse()

n5.2<- var(d15$FP_bi)/length(d15$FP_bi)
n5.3<- var(d15$FP2_bi)/length(d15$FP2_bi)

var_defamatory_2<- sqrt(n1.2+n5.2)
ci_defamatory_2<- 1.96*(var_defamatory_2)

var_defamatory_3<- sqrt(n1.3+n5.3)
ci_defamatory_3<- 1.96*(var_defamatory_3)

### Replication of Figure 5 

summary_table_2 <- d167 %>% group_by(TR) %>% get_summary_stats(FP_bi) 
newtable_2<- summary_table_2 [c(1,3,2,4,5),]
newtable_2$name <- c("Control group (C)",
                     "Control (C) + Pride rhetoric",
                     "C + Humiliation rhetoric",
                     "C + Defamatory rhetoric",
                     "C + Conciliatory rhetoric"
                     
)
newtable_2$groupno<- c(1,2,3,4,5)

newtable_2
newtable_2$effect <- ifelse((newtable_2$name  ==  "Control group (C)"), 
                            (newtable_2$mean - newtable_2$mean [newtable_2$name  ==  "Control group (C)"]),
                            (newtable_2$mean - newtable_2$mean [newtable_2$name  ==  "Control group (C)"]))
newtable_2

## the different ci between control and each treatment group

newtable_2$diffci<-c(0,0.072,0.072,0.073,0.071)

# Create ggplot object with larger text size and adjusted position
Approve_CI95_3_a <- ggplot(subset(newtable_2, name != "Control group (C)"), aes(x = name, y = effect * 100)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect - diffci) * 100, ymax = (effect + diffci) * 100), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  geom_text(aes(label = sprintf("%.1f", effect * 100), y = ifelse(effect > 0, effect * 100, effect * 100)), 
            position = position_dodge(0.25), vjust = 1 , hjust= -0.2 ,size = 8, color = "black") +  # Adjusted text size and position
  
  scale_y_continuous(breaks = seq(-6, 18, 4), limits = c(-6, 18)) +
  ylab("Difference in the U.S. Public Support for Aggressive Foreign Policy (% point)") + xlab("") + theme_bw() + ## ylab "The effects between control group and treatment groups (% point)\n" 
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 24),
        title = element_text(colour = "black", size = 22, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))  # Adjust the margin here (top, right, bottom, left))

Approve_CI95_3_a

Approve_CI95_3_a+scale_x_discrete(name ="", 
                                  limits=c("Control (C) + Pride rhetoric",
                                           "C + Humiliation rhetoric",
                                           "C + Defamatory rhetoric",
                                           "C + Conciliatory rhetoric"))
### Replication of Figure 8 

summary_table_3 <- d167 %>% group_by(TR) %>% get_summary_stats(FP2_bi) 
newtable_3<- summary_table_3 [c(1,3,2,4,5),]
newtable_3$name <- c("Control group (C)",
                     "Control (C) + Pride rhetoric",
                     "C + Humiliation rhetoric",
                     "C + Defamatory rhetoric",
                     "C + Conciliatory rhetoric")

newtable_3$groupno<- c(1,2,3,4,5)

newtable_3
newtable_3$effect <- ifelse((newtable_3$name  ==  "Control group (C)"), 
                            (newtable_3$mean - newtable_3$mean [newtable_3$name  ==  "Control group (C)"]),
                            (newtable_3$mean - newtable_3$mean [newtable_3$name  ==  "Control group (C)"]))
newtable_3

## the different ci between control and each treatment group

newtable_3$diffci<-c(0,0.078,0.078,0.08,0.08)

## Figure 8 
Approve_CI95_3_b <- ggplot(subset(newtable_3, name != "Control group (C)"), aes(x = name, y = effect * 100)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect - diffci) * 100, ymax = (effect + diffci) * 100), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  geom_text(aes(label = sprintf("%.1f", effect * 100), y = ifelse(effect > 0, effect * 100, effect * 100)), 
            position = position_dodge(0.25), vjust = 1 , hjust= -0.2 ,size = 8, color = "black") +  # Adjusted text size and position
  
  scale_y_continuous(breaks = seq(-20, 12, 4), limits = c(-20, 12)) +
  ylab("Difference in the U.S. Public Support for Reducing Trade with China (% point)") + xlab("") + theme_bw() + ## ylab "The effects between control group and treatment groups (% point)\n" 
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 24),
        title = element_text(colour = "black", size = 24, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))  # Adjust the margin here (top, right, bottom, left))

Approve_CI95_3_b

Approve_CI95_3_b+scale_x_discrete(name ="", 
                                  limits=c("Control (C) + Pride rhetoric",
                                           "C + Humiliation rhetoric",
                                           "C + Defamatory rhetoric",
                                           "C + Conciliatory rhetoric"))

## Replication of Figure 6

d168<- d167 %>% 
  filter (TR== 0|TR==1|TR==2|TR==3) %>%
  mutate(WWD = case_when(
    TR == "1" ~ "1",
    TR == "2" ~ "1", 
    TR == "3"~ "1",
    TRUE ~ "0")) %>%
  mutate(humli = case_when(
    TR == "1" ~ "1",
    TRUE ~ "0")) %>%
  mutate(pride = case_when(
    TR == "2" ~ "1",
    TRUE ~ "0")) %>%
  mutate(defa = case_when(
    TR == "3" ~ "1",
    TRUE ~ "0")) 

model1<- lm(threat ~ WWD, 
            data = d168)

model2<- lm(threat ~ pride+humli+defa, 
            data = d168)

plot_summs(model1,model2,
           ci_level = 0.95, scale= TRUE,
           colors = c("black","black"),
           coefs = c("Wolf warrior diplomacy" = "WWD1",
                     "Pride rhetoric"="pride1",
                     "Humiliation rhetoric" = "humli1",
                     "Defamatory rhetoric"="defa1"),
           model.names = c("Model 5",
                           "Model 6"
           ),
           rescale.distributions = TRUE) +
  theme_bw() +
  theme(
    panel.grid = element_line(color = 'white'),
    axis.title.y = element_text(size = 20, color = 'black', margin = margin(t = 0, r = 16, b = 0, l = 0)),
    axis.title.x = element_text(size = 20, color = 'black', margin = margin(t = 0, r = 16, b = 0, l = 0)),
    axis.text = element_text(size = 18, color = 'black'),
    legend.title = element_text(size=16, color = 'black'), 
    legend.text = element_text(size=14, color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Estimates",
    y = "Types of rhetoric",
    title = ""
  )


## Replication of Table A7-A9 in the appendix 

## TR0, control group 
## TR1, Humiliation
## TR2 pride
## TR3 Defamatory
## TR4 Conciliatory

# Rename treatment groups 

d167$tr1<- NA
d167$tr1[d167$TR  ==  0]<- 0 ## control 
d167$tr1[d167$TR  ==  2]<- 1 ## pride 
table(d167$tr1)

d167$tr2<-NA
d167$tr2[d167$TR  ==  0]<- 0 ## control 
d167$tr2[d167$TR  ==  1]<- 1 ## humiliation
table(d167$tr2)

d167$tr3<-NA
d167$tr3[d167$TR  ==  0]<- 0 ## control 
d167$tr3[d167$TR  ==  3]<- 1 ## defamatory
table(d167$tr3)

d167$tr4<-NA
d167$tr4[d167$TR  ==  0]<- 0 ## control 
d167$tr4[d167$TR  ==  4]<- 1 ## conciliatory
table(d167$tr4)
table(d167$TR)

## military action 
library(MASS)
robust1.1<- polr(as.factor(FP)~tr1+age+male+educ+income, data=d167,Hess = TRUE)## pride
summary(robust1.1)
robust1.2<- polr(as.factor(FP)~tr2+age+male+educ+income, data=d167,Hess = TRUE)## Humiliation
summary(robust1.2)
robust1.3<- polr(as.factor(FP)~tr3+age+male+educ+income, data=d167,Hess = TRUE)## defamatory
summary(robust1.3)
robust1.4<- polr(as.factor(FP)~tr4+age+male+educ+income, data=d167,Hess = TRUE)## concilitatory
summary(robust1.4)

## Limiting trade 
robust2.1<- polr(as.factor(FP2)~tr1+age+male+educ+income, data=d167,Hess = TRUE)## pride
summary(robust2.1)
robust2.2<- polr(as.factor(FP2)~tr2+age+male+educ+income, data=d167,Hess = TRUE)## Humiliation
summary(robust2.2)
robust2.3<- polr(as.factor(FP2)~tr3+age+male+educ+income, data=d167,Hess = TRUE)## defamatory
summary(robust2.3)
robust2.4<- polr(as.factor(FP2)~tr4+age+male+educ+income, data=d167,Hess = TRUE)## concilitatory
summary(robust2.4)

## limiting China growth 
robust5.1<- polr(as.factor(FP3)~tr1+age+male+educ+income, data=d167,Hess = TRUE)## pride
summary(robust5.1)
robust5.2<- polr(as.factor(FP3)~tr2+age+male+educ+income, data=d167,Hess = TRUE)## Humiliation
summary(robust5.2)
robust5.3<- polr(as.factor(FP3)~tr3+age+male+educ+income, data=d167,Hess = TRUE)## defamatory
summary(robust5.3)
robust5.4<- polr(as.factor(FP3)~tr4+age+male+educ+income, data=d167,Hess = TRUE)## concilitatory
summary(robust5.4)

## Promoting Dialouge 
robust6.1<- polr(as.factor(FP4)~tr1+age+male+educ+income, data=d167,Hess = TRUE)## pride
summary(robust6.1)
robust6.2<- polr(as.factor(FP4)~tr2+age+male+educ+income, data=d167,Hess = TRUE)## Humiliation
summary(robust6.2)
robust6.3<- polr(as.factor(FP4)~tr3+age+male+educ+income, data=d167,Hess = TRUE)## defamatory
summary(robust6.3)
robust6.4<- polr(as.factor(FP4)~tr4+age+male+educ+income, data=d167,Hess = TRUE)## concilitatory
summary(robust6.4)

## Replication of Table A7
stargazer(robust1.1, robust1.2,robust1.3,robust1.4,
          type = "html", 
          title = "",
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Concilitory rhetoric",
                               "Age",
                               "Male",
                               "Eduction",
                               "Income"),
          dep.var.labels   = "Support for Aggressive Foreign Policy Toward China (Five Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/微调查/result/Appendix graph/table_FP1_lucid_1.html")

## Replication of Table A8
stargazer(robust2.1, robust2.2,robust2.3,robust2.4,
          type = "html", 
          title = "",
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Concilitory rhetoric",
                               "Age",
                               "Male",
                               "Education",
                               "Income"),
          dep.var.labels   = "Support for Reducing Trade between the U.S. and China (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/微调查/result/Appendix graph/table_FP2_Lucid_1.html")
## Replication of Table A9
stargazer(robust5.1, robust5.2,robust5.3,robust5.4,
          type = "html", 
          title = "",
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Concilitory rhetoric",
                               "Age",
                               "Male",
                               "Education",
                               "Income"
                               
          ),
          dep.var.labels   = "Support for Limiting the Growth of Chinese Power (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/微调查/result/Appendix graph/table_FP3_lucid_1.html")

## Replication of Table A10
stargazer(robust6.1, robust6.2,robust6.3,robust6.4,
          type = "html", 
          title = "",
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Concilitory rhetoric",
                               "Age",
                               "Male",
                               "Education",
                               "Income"
          ),
          dep.var.labels   = " Support for Engaging in Diplomacy with China (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/微调查/result/Appendix graph/table_FP4_lucid.html")

## ## Replication of Figure A5 (mediation analysis intergroup threat)

## pride rhetoric effect
d3<- d167 %>%
  filter(TR=="0" |TR=="2") %>%
  glimpse()

med.fit1<-lm(threat~TR+age+male+educ+income, data=d3)
summary(med.fit1)
out.fit1<- lm(FP~ threat+TR+age+male+educ+income, data=d3)
set.seed(1991)
med.out1<- mediation::mediate(med.fit1, out.fit1, treat = "TR", mediator = "threat",sims = 1000,conf.level = 0.9)
summary(med.out1)## 0.74

f1<- plot(med.out1, main = "Pride rhetoric")

## humiliation rhetoric effect
d2<- d167 %>% 
  filter(TR=="0" |TR=="1") %>%
  glimpse()

med.fit2<-lm(threat~TR+age+male+educ+income, data=d2)
summary(med.fit2)
out.fit2<- lm(FP~ threat+TR+age+male+educ+income, data=d2)
summary(out.fit2)
set.seed(1991)
med.out2<- mediation::mediate(med.fit2, out.fit2, treat = "TR", mediator = "threat",sim=1000,conf.level = 0.95)## 0.92
summary(med.out2)

f2<- plot(med.out2, main = "Humiliation rhetoric")

## defamatory rhetoric effect
d4<- d167 %>%
  filter(TR=="0" |TR=="3") %>%
  glimpse()

med.fit3<-lm(threat~TR+age+male+educ+income, data=d4)
summary(med.fit3)
out.fit3<- lm(FP~ threat+TR+age+male+educ+income, data=d4)
set.seed(1991)
med.out3<- mediation::mediate(med.fit3, out.fit3, treat = "TR", mediator = "threat",sims = 1000,conf.level = 0.95)
summary(med.out3)## 0.61
f3<- plot(med.out3, main = "Defamatory rhetoric")

## WWD effect
d5<- d167 %>%
  glimpse()

med.fit4<-lm(threat~WWD+age+male+educ+income, data=d5)
summary(med.fit4)
out.fit4<- lm(FP~ threat+WWD+age+male+educ+income, data=d5)
set.seed(1991)
med.out4<- mediation::mediate(med.fit4, out.fit4, treat = "WWD", mediator = "threat",sims = 1000,conf.level = 0.95)
summary(med.out4)
f4<- plot(med.out4, main = "Wolf warrior rhetoric")

dev.off(dev.list()["RStudioGD"])
par(mfrow=c(4,1))

## Replciation of Figure A5 intergroup threat as mediator
f4<- plot(med.out4, main = "Wolf warrior rhetoric")
f1<- plot(med.out1, main = "Pride rhetoric")
f2<- plot(med.out2, main = "Humiliation rhetoric")
f3<- plot(med.out3, main = "Defamatory rhetoric")
ggsave("/Users/victorxu/Desktop/WWD/微调查/result/Appendix graph/mediator_threat_lucid_1.jpeg",
       width = 297, 
       height = 180, 
       units = "mm",
       dpi=300)

## Replication of Heterogeneous effects Figure A14-A18
# Rename treatment groups 
d167$tg <- NA
d167$tg[d167$TR  ==  0] <- 0
d167$tg[d167$TR  ==  2] <- 1 
d167$tg[d167$TR  ==  1] <- 2
d167$tg[d167$TR  ==  3] <- 3
d167$tg[d167$TR  ==  4] <- 4
xtabs(~TR + tg, d167)

d167$tg <- factor(d167$tg,
                  levels = c(0,1,2,3,4),
                  labels = c("Control group (C)", "Pride rhetoric","Humiliaiton rhetoric", 
                             "Defamatory rhetoric","Concilitaroy rhetoric"))
table(d167$tg)


## Replication of Figure A14 Ring-wing authorianism (RWA)
table(d167$rw_1)
d167$rw_1_2<- NA 
d167$rw_1_2[d167$rw_1==2]<-1
d167$rw_1_2[d167$rw_1==1]<-0
table(d167$rw_1_2)

table(d167$rw_2)
d167$rw_2_2<- NA 
d167$rw_2_2[d167$rw_2==2]<-1
d167$rw_2_2[d167$rw_2==1]<-0
table(d167$rw_2_2)

table(d167$rw_3)
d167$rw_3_2<- NA 
d167$rw_3_2[d167$rw_3==2]<-1
d167$rw_3_2[d167$rw_3==1]<-0
table(d167$rw_3_2)

table(d167$rw_4)
d167$rw_4_2<- NA 
d167$rw_4_2[d167$rw_4==1]<-1
d167$rw_4_2[d167$rw_4==2]<-0
table(d167$rw_4_2)

d167$RWA<- d167$rw_1_2+d167$rw_2_2+d167$rw_3_2+d167$rw_4_2
table(d167$RWA)

# Gen binary RWA variable
table(d167$RWA)
d167$biRWA <- ifelse((d167$RWA  ==  3 | d167$RWA  ==  4), 1, ## high RWA
                     ifelse((d167$RWA  ==  0 | d167$RWA  ==  1 | d167$RWA  ==  2), 0, NA)) ## low RWA
xtabs(~d167$RWA + d167$biRWA)

d167$biRWA <- factor(d167$biRWA,
                     levels = c(1, 0),
                     labels = c("High-level RWA", "Low-level RWA"))

xtabs(~d167$RWA + d167$biRWA)

# Summary stats for public support for the military actions against the US 
SumStat2 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$biRWA))), 
                      measurevar = "FP", groupvars = c("tg", "biRWA"), na.rm = TRUE)
print(SumStat2)

SumStat2$effect2 <- ifelse((SumStat2$biRWA  ==  "Low-level RWA"), 
                           (SumStat2$FP - SumStat2$FP [SumStat2$tg  ==  "Control group (C)" & SumStat2$biRWA  ==  "Low-level RWA"]),
                           (SumStat2$FP - SumStat2$FP [SumStat2$tg  ==  "Control group (C)" & SumStat2$biRWA  ==  "High-level RWA"]))
SumStat2


Approve_CI95_3_RWA_b <- ggplot(subset(SumStat2, SumStat2$tg != "Control group (C)"), aes(x = tg, y = effect2, group = biRWA, color = biRWA, shape = biRWA)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect2-ci), ymax = (effect2+ci)), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on the U.S. public's support for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 10),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())


Approve_CI95_3_RWA_b

### Militant assertiveness (MA)
table(d167$mili_2)
d167$mili_2_new<-NA
d167$mili_2_new[d167$mili_2==1]<- 5
d167$mili_2_new[d167$mili_2==2]<- 4
d167$mili_2_new[d167$mili_2==3]<- 3
d167$mili_2_new[d167$mili_2==4]<- 2
d167$mili_2_new[d167$mili_2==5]<- 1
table(d167$mili_2_new)

d167$MA<-NA
d167$MA<- d167$mili_1+d167$mili_2_new+d167$mili_3
table(d167$MA)

d167$biMA <- ifelse((d167$MA>= 3 & d167$MA<=9 ), 0, ## non-MA
                    ifelse((d167$MA>=10 & d167$MA<=15), 1, NA)) ## MA
xtabs(~d167$MA + d167$biMA)

d167$biMA <- factor(d167$biMA,
                    levels = c(1, 0),
                    labels = c("Militant assertiveness", "Non-militant assertiveness"))

xtabs(~d167$MA + d167$biMA)

# Summary stats for public support for the aggressive foreign policy (MA)
SumStat4 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$biMA))), 
                      measurevar = "FP", groupvars = c("tg", "biMA"), na.rm = TRUE)
print(SumStat4)

SumStat4$effect4 <- ifelse((SumStat4$biMA  ==  "Non-militant assertiveness"), 
                           (SumStat4$FP - SumStat4$FP [SumStat4$tg  ==  "Control group (C)" & SumStat4$biMA  ==  "Non-militant assertiveness"]),
                           (SumStat4$FP - SumStat4$FP [SumStat4$tg  ==  "Control group (C)" & SumStat4$biMA  ==  "Militant assertiveness"]))
SumStat4


Approve_CI95_3_MA_b <- ggplot(subset(SumStat4, SumStat4$tg != "Control group (C)"), aes(x = tg, y = effect4, group = biMA, color = biMA, shape = biMA)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect4-ci), ymax = (effect4+ci)), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on the U.S. public's support for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())

Approve_CI95_3_MA_b

## Hawk and Dove 


# Gen Hawk and dove  variable
table(d167$hawk)
d167$Hawk <- ifelse((d167$hawk == 3 |d167$hawk == 4 ), 0, ## Dove
                    ifelse((d167$hawk == 1 |d167$hawk == 2 ), 1, NA)) ## Hawk
xtabs(~d167$hawk + d167$Hawk)

d167$Hawk <- factor(d167$Hawk,
                    levels = c(1, 0),
                    labels = c("Hawk", "Dove"))

xtabs(~d167$hawk + d167$Hawk)
# Summary stats for public support for the aggressive foreign policy (hawkv.s. dove)

SumStat6 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$Hawk))), 
                      measurevar = "FP", groupvars = c("tg", "Hawk"), na.rm = TRUE)
print(SumStat6)

SumStat6$effect6 <- ifelse((SumStat6$Hawk  ==  "Dove"), 
                           (SumStat6$FP - SumStat6$FP [SumStat6$tg  ==  "Control group (C)" & SumStat6$Hawk  ==  "Dove"]),
                           (SumStat6$FP - SumStat6$FP [SumStat6$tg  ==  "Control group (C)" & SumStat6$Hawk  ==  "Hawk"]))
SumStat6


Approve_CI95_3_hawk_b <- ggplot(subset(SumStat6, SumStat6$tg != "Control group (C)"), aes(x = tg, y = effect6, group = Hawk, color = Hawk, shape = Hawk)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect6-ci), ymax = (effect6+ci)), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on the U.S. public's support for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())

Approve_CI95_3_hawk_b

## liberal - conservatives


table(d167$ideo)
d167$conser <- ifelse((d167$ideo==1|d167$ideo==2|d167$ideo==3|d167$ideo==4), 0, ## Liberals
                      ifelse((d167$ideo==5|d167$ideo==6|d167$ideo==7), 1, NA)) ## conservatives

xtabs(~d167$ideo + d167$conser)

d167$conser <- factor(d167$conser,
                      levels = c(1,0),
                      labels = c("Conservatives", "Liberals"))

xtabs(~d167$ideo + d167$conser)

# Summary stats for public support for the aggressive foreign policy (conservative/liberal)

SumStat8 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$conser))), 
                      measurevar = "FP", groupvars = c("tg", "conser"), na.rm = TRUE)
print(SumStat8)

SumStat8$effect8 <- ifelse((SumStat8$conser  ==  "Liberals"), 
                           (SumStat8$FP - SumStat8$FP [SumStat8$tg  ==  "Control group (C)" & SumStat8$conser  ==  "Liberals"]),
                           (SumStat8$FP - SumStat8$FP [SumStat8$tg  ==  "Control group (C)" & SumStat8$conser  ==  "Conservatives"]))
SumStat8

Approve_CI95_3_conser_b <- ggplot(subset(SumStat8, SumStat8$tg != "Control group (C)"), aes(x = tg, y = effect8, group = conser, color = conser, shape = conser)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect8-ci), ymax = (effect8+ci)), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on the U.S. public's support for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())

Approve_CI95_3_conser_b

## ggsave("/Users/victorxu/Desktop/WWD/微调查/result/Appendix graph/conservative_USFP_2.jpeg",
       ##width = 297, 
       ##height = 180, 
       #units = "mm",
       ##dpi=300)

## GOP/Domocrat

table(d167$political_party)
d167$gop <- ifelse((d167$political_party==1|d167$political_party==2|d167$political_party==3|d167$political_party==6), 0, ## Democrats
                   ifelse((d167$political_party==8|d167$political_party==9|d167$political_party==10|d167$political_party==5), 1, NA)) ## GOP

xtabs(~d167$political_party + d167$gop)

d167$gop <- factor(d167$GOP,
                   levels = c(1,0 ),
                   labels = c("Republican", "Democrat"))

xtabs(~d167$political_party + d167$gop)


# Summary stats for public support for the aggressive foreign policy (democrat/gop)

SumStat9 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$gop))), 
                      measurevar = "FP", groupvars = c("tg", "gop"), na.rm = TRUE)
print(SumStat9)

SumStat9$effect9 <- ifelse((SumStat9$gop  ==  "Democrat"), 
                           (SumStat9$FP - SumStat9$FP [SumStat9$tg  ==  "Control group (C)" & SumStat9$gop  ==  "Democrat"]),
                           (SumStat9$FP - SumStat9$FP [SumStat9$tg  ==  "Control group (C)" & SumStat9$gop  ==  "Republican"]))
SumStat9

Approve_CI95_3_gop <- ggplot(subset(SumStat9, SumStat9$tg != "Control group (C)"), aes(x = tg, y = effect9, group = gop, color = gop, shape = gop)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect9-ci), ymax = (effect9+ci)), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on the U.S. public's support for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())

Approve_CI95_3_gop

## Replication of Figure A2
rm(list = ls())
## Load the required packages
library(gplots)    # version 3.1.1
library(extrafont) # version 0.17
d167<- read_xlsx("//Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/Harvard dataset/JCR_US Lucid.xlsx")


### Figure A2: Univariate Balance on Pre-Treatment Covariates ###
pdf("/Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/Figure_balance_US.pdf", height = 11, width = 11)
par(mfrow = c(2,2))

## recode
## female 
d167$female <- NA
d167$female [d167$male==0]<-1 ## female
d167$female [d167$male==1]<-0 ## male 
## GOP
d167$repub<- NA
d167$repub[d167$political_party==1|d167$political_party==2|d167$political_party==3|d167$political_party==6]<-0 ## Democrats
d167$repub[d167$political_party==8|d167$political_party==9|d167$political_party==10|d167$political_party==5]<-1 ## Republican
table(d167$repub)

## college degree 
d167$col<- NA
d167$col[d167$educ>=1&d167$educ<=3]<-0 ## lower than 4-year college
d167$col[d167$educ>=4&d167$educ<=8]<-1 ## college or above
table(d167$col)
## income
table(d167$income)
d167$income2<- NA
d167$income2[d167$income>=1&d167$income<=10]<- 0 ## Low income
d167$income2[d167$income>=11&d167$income<=24]<- 1 ## high-income
table(d167$income2)
# Gender
plot(density(d167[d167$TR==0,]$female, na.rm=T, bw=0.1), xlab="Gender (0 = Male, 1 = Female)", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="Gender", lty=1, cex=1.2)
par(new=TRUE)
plot(density(d167[d167$TR==2,]$female, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=2)
par(new=TRUE)
plot(density(d167[d167$TR==1,]$female, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=3)
par(new=TRUE)
plot(density(d167[d167$TR==3,]$female, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=4)
par(new=TRUE)
plot(density(d167[d167$TR==4,]$female, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=5)

par(new=TRUE, family="Times")
legend("topleft", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), lty=c(1,2,3,4,5), cex=1, bty="n")

# GOP
plot(density(d167[d167$TR==0,]$repub, na.rm=T, bw=0.1), xlab="Republican (0 = No, 1 = Yes)", ylim=c(0,4), xlim=c(-0.4,1.4), main="Party Affiliation", lty=1, cex=1.2)
par(new=TRUE)
plot(density(d167[d167$TR==2,]$repub, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=2)
par(new=TRUE)
plot(density(d167[d167$TR==1,]$repub, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=3)
par(new=TRUE)
plot(density(d167[d167$TR==3,]$repub, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=4)
par(new=TRUE)
plot(density(d167[d167$TR==4,]$repub, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=4)
par(new=TRUE)
par(new=TRUE, family="Times")
legend("topleft", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5" ), lty=c(1,2,3,4,5), cex=1, bty="n")



# Education
plot(density(d167[d167$TR==0,]$col, na.rm=T, bw=0.1), xlab="College Degree (0 = No, 1 = Yes)", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="College Degree", lty=1, cex=1.2)
par(new=TRUE)
plot(density(d167[d167$TR==2,]$col, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=2)
par(new=TRUE)
plot(density(d167[d167$TR==1,]$col, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=3)
par(new=TRUE)
plot(density(d167[d167$TR==3,]$col, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=4)
par(new=TRUE)
plot(density(d167[d167$TR==4,]$col, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=5)

par(new=TRUE, family="Times")
legend("topleft", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), lty=c(1,2,3,4,5), cex=1, bty="n")



# Income
plot(density(d167[d167$TR==0,]$income2, na.rm=T, bw=0.1), xlab="High Income (0 = Low-Income, 1 = High-Income)", ylim=c(0,4), xlim=c(-0.4,1.4), main="Income", lty=1, cex=1.2)
par(new=TRUE)
plot(density(d167[d167$TR==2,]$income2, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=2)
par(new=TRUE)
plot(density(d167[d167$TR==1,]$income2, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=3)
par(new=TRUE)
plot(density(d167[d167$TR==3,]$income2, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=5)
par(new=TRUE)
plot(density(d167[d167$TR==4,]$income2, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=6)
par(new=TRUE, family="Times")
legend("topright", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), lty=c(1,2,3,4,5), cex=1, bty="n")

legend("topright", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), lty=c(1,2,3,4,5), cex=1, bty="n")
dev.off()

