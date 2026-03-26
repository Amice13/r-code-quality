### China survey replication_JCR 
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

d167<- read_xlsx("/Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/Harvard dataset/JCR_China Survey.xlsx")
  

## data preparation 
  
## calculate the variance of control group  for different DVs 
  
d11<- d167 %>% ## 
  filter(TR=="1") %>%
  glimpse()

n1<- var(d11$biper)/length(d11$biper) ## binary support for the government 
n1.2<- var(d11$FP_bi)/length(d11$FP_bi) ## binary support for aggressive foreign policy
n1.3<- var(d11$FP2_bi)/length(d11$FP2_bi) ## binary support for reducing trade 

# calculate the variance of conciliatory group for different DVs 
d12<- d167 %>% ## 
  filter(TR=="2") %>%
  glimpse()

n2<- var(d12$biper)/length(d12$biper) ## binary support for the government 
n2.2<- var(d12$FP_bi)/length(d12$FP_bi) ## binary support for aggressive foreign policy
n2.3<- var(d12$FP2_bi)/length(d12$FP2_bi) ## binary support for reducing trade 


var_concilitory<- sqrt(n1+n2)
ci_concilitory<- 1.96*(var_concilitory) 

var_concilitory_2<- sqrt(n1.2+n2.2)
ci_concilitory_2<- 1.96*(var_concilitory_2)


var_concilitory_3<- sqrt(n1.3+n2.3)
ci_concilitory_3<- 1.96*(var_concilitory_3)

## calculate the variance of humiliation group for different DVs 

d13<- d167 %>% ## 
  filter(TR=="3") %>%
  glimpse()
n3<- var(d13$biper)/length(d13$biper)
n3.2<- var(d13$FP_bi)/length(d13$FP_bi)
n3.3<- var(d13$FP2_bi)/length(d13$FP2_bi)

var_humiliation<- sqrt(n1+n3)
ci_humiliation<- 1.96*(var_humiliation)

var_humiliation_2<- sqrt(n1.2+n3.2)
ci_humiliation_2<- 1.96*(var_humiliation_2)

var_humiliation_3<- sqrt(n1.3+n3.3)
ci_humiliation_3<- 1.96*(var_humiliation_3)


## calculate the variance of pride group for different DVs 
d14<- d167 %>% ## 
  filter(TR=="4") %>%
  glimpse()
n4<- var(d14$biper)/length(d14$biper)
n4.2<- var(d14$FP_bi)/length(d14$FP_bi)
n4.3<- var(d14$FP2_bi)/length(d14$FP2_bi)

var_pride<- sqrt(n1+n4)
ci_pride<- 1.96*(var_pride)

var_pride_2<- sqrt(n1.2+n4.2)
ci_pride_2<- 1.96*(var_pride_2)

var_pride_3<- sqrt(n1.3+n4.3)
ci_pride_3<- 1.96*(var_pride_3)



## calculate the variance of defamatory group for different DVs 

d15<- d167 %>% ## 
  filter(TR=="5") %>%
  glimpse()
n5<- var(d15$biper)/length(d15$biper)
n5.2<- var(d15$FP_bi)/length(d15$FP_bi)
n5.3<- var(d15$FP2_bi)/length(d15$FP2_bi)

var_defamatory <- sqrt(n1+n5)
ci_defamatory <- 1.96*(var_defamatory)

var_defamatory_2<- sqrt(n1.2+n5.2)
ci_defamatory_2<- 1.96*(var_defamatory_2)

var_defamatory_3<- sqrt(n1.3+n5.3)
ci_defamatory_3<- 1.96*(var_defamatory_3)


### Replication of Figure 1 
summary_table_3 <- d167 %>% group_by(TR) %>% get_summary_stats(biper) 
newtable_3<- summary_table_3 [c(1,4,3,5,2),]
newtable_3$name <- c("Control group (C)",
                     "Control (C) + Pride rhetoric",
                     "C + Humiliation rhetoric",
                     "C + Defamatory rhetoric",
                     "C + Conciliatory rhetoric"
                     
)
newtable_3$groupno<- c(1,2,3,4,5)
newtable_3
newtable_3$effect <- ifelse((newtable_3$name  ==  "Control group (C)"), 
                            (newtable_3$mean - newtable_3$mean [newtable_3$name  ==  "Control group (C)"]),
                            (newtable_3$mean - newtable_3$mean [newtable_3$name  ==  "Control group (C)"]))
newtable_3

## the different ci between control and each treatment group

newtable_3$diffci<-c(0,0.064,0.066,0.070,0.073) ## from the previous steps 

Approve_CI95_3_c <- ggplot(subset(newtable_3, name != "Control group (C)"), aes(x = name, y = effect * 100)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect - diffci) * 100, ymax = (effect + diffci) * 100), position = position_dodge(0.25)) +
  geom_point(size = 3, position = position_dodge(0.25)) +
  geom_text(aes(label = sprintf("%.1f", effect * 100), y = ifelse(effect > 0, effect * 100, effect * 100)), 
            position = position_dodge(0.25), vjust = 1 , hjust= -0.2 ,size = 8, color = "black") +  # Adjusted text size and position
  
  scale_y_continuous(breaks = seq(-6,30,4), limits = c(-6, 30)) +
  ylab("Difference in Chinese Public Approval Rate (% point)") + xlab("") + theme_bw() + ## ylab "The effects between control group and treatment groups (% point)\n" 
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 24),
        title = element_text(colour = "black", size = 24, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))  # Adjust the margin here (top, right, bottom, left))

Approve_CI95_3_c

Approve_CI95_3_c+scale_x_discrete(name ="", 
                                  limits=c("Control (C) + Pride rhetoric",
                                           "C + Humiliation rhetoric",
                                           "C + Defamatory rhetoric",
                                           "C + Conciliatory rhetoric"))

## Replication of Figure 3

summary_table_2 <- d167 %>% group_by(TR) %>% get_summary_stats(FP_bi, type = "mean_ci") 
newtable_2<- summary_table_2 [c(1,4,3,5,2),]
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

newtable_2$diffci<-c(0,0.076,0.077,0.076,0.075)

Approve_CI95_3_a <- ggplot(subset(newtable_2, name != "Control group (C)"), aes(x = name, y = effect * 100)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect - diffci) * 100, ymax = (effect + diffci) * 100), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  geom_text(aes(label = sprintf("%.1f", effect * 100), y = ifelse(effect > 0, effect * 100 , effect * 100 )), 
            position = position_dodge(0.25), vjust = 1 , hjust= -0.2 ,size = 8, color = "black") +  # Adjusted text size and position
  
  scale_y_continuous(breaks = seq(-8, 18, 4), limits = c(-8, 18)) +
  ylab("Difference in Chinese Public Support for Aggressive Foreign Policy (% point)") + xlab("") + theme_bw() + ## ylab "The effects between control group and treatment groups (% point)\n" 
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

## Replication of Figure 7 

summary_table_3 <- d167 %>% group_by(TR) %>% get_summary_stats(FP2_bi, type = "mean_ci") 
newtable_3<- summary_table_3 [c(1,4,3,5,2),]
newtable_3$name <- c("Control group (C)",
                     "Control (C) + Pride rhetoric",
                     "C + Humiliation rhetoric",
                     "C + Defamatory rhetoric",
                     "C + Conciliatory rhetoric"
)

newtable_3$groupno<- c(1,2,3,4,5)

newtable_3
newtable_3$effect <- ifelse((newtable_3$name  ==  "Control group (C)"), 
                            (newtable_3$mean - newtable_3$mean [newtable_3$name  ==  "Control group (C)"]),
                            (newtable_3$mean - newtable_3$mean [newtable_3$name  ==  "Control group (C)"]))
newtable_3

newtable_3$diffci<-c(0,0.076,0.076,0.076,0.076)

Approve_CI95_3_c <- ggplot(subset(newtable_3, name != "Control group (C)"), aes(x = name, y = effect * 100)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect - diffci) * 100, ymax = (effect + diffci) * 100), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  geom_text(aes(label = sprintf("%.1f", effect * 100), y = ifelse(effect > 0, effect * 100, effect * 100)), 
            position = position_dodge(0.25), vjust = 1 , hjust= -0.2 ,size = 8, color = "black") +  # Adjusted text size and position
  
  scale_y_continuous(breaks = seq(-12, 10, 4), limits = c(-12, 10)) +
  ylab("Difference in Chinese Public Support for Reducing Trade with the U.S. (% point)") + xlab("") + theme_bw() + ## ylab "The effects between control group and treatment groups (% point)\n" 
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 24),
        title = element_text(colour = "black", size = 22, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank(),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))  # Adjust the margin here (top, right, bottom, left))

Approve_CI95_3_c

Approve_CI95_3_c+scale_x_discrete(name ="", 
                                  limits=c("Control (C) + Pride rhetoric",
                                           "C + Humiliation rhetoric",
                                           "C + Defamatory rhetoric",
                                           "C + Conciliatory rhetoric"))

## Replication of Figure 2 

d168<- d167 %>% 
  filter (TR== 1|TR==3|TR==4|TR==5) %>%
  mutate(WWD = case_when(
    TR == "3" ~ "1", ## humiliation 
    TR == "4" ~ "1", ## pride
    TR == "5"~ "1",  ## defamatory
    TRUE ~ "0")) %>%
  mutate(humli = case_when(
    TR == "3" ~ "1",
    TRUE ~ "0")) %>%
  mutate(pride = case_when(
    TR == "4" ~ "1",
    TRUE ~ "0")) %>%
  mutate(defa = case_when(
    TR == "5" ~ "1",
    TRUE ~ "0")) 

model1<- lm(ingroup ~ WWD, 
            data = d168)
model2<- lm(ingroup ~ pride+humli+defa, 
            data = d168)

summary(model1)
summary(model2)

plot_summs(model1,model2,
           ci_level = 0.95, scale= TRUE,
           point.size = 8,
           colors = c("black","black"),
           coefs = c("Wolf warrior diplomacy" = "WWD1",
                     "Pride rhetoric"="pride1",
                     "Humiliation rhetoric" = "humli1",
                     "Defamatory rhetoric"="defa1"),
           model.names = c("Model 1",
                           "Model 2"
           ),
           rescale.distributions = TRUE) +
  theme_bw() +
  theme(
    panel.grid = element_line(color = 'white'),
    axis.title.y = element_text(size = 20, color = 'black'),
    axis.title.x = element_text(size = 20, color = 'black'),
    axis.text = element_text(size = 18, color = 'black'),
    legend.title = element_text(size=12, color = 'black'), 
    legend.text = element_text(size=11, color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) +
  labs(
    x = "Estimates",
    y = "Types of rhetoric",
    title = ""
  )


## Replication of Figure 4 

model3<- lm(outgroup ~ WWD, 
            data = d168)
model4<- lm(outgroup ~ pride+humli+defa, 
            data = d168)

plot_summs(model3,model4,
           ci_level = 0.95, scale= TRUE,
           colors = c("black","black"),
           coefs = c("Wolf warrior diplomacy" = "WWD1",
                     "Pride rhetoric"="pride1",
                     "Humiliation rhetoric" = "humli1",
                     "Defamatory rhetoric"="defa1"),
           model.names = c("Model 3",
                           "Model 4"
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

###### Replication of figures and tables in the Appendix

## Replication of Tables A1-A6

# Rename treatment groups for appendix replication

d167$tr1<- NA
d167$tr1[d167$TR  ==  1]<- 0 ## control 
d167$tr1[d167$TR  ==  4]<- 1 ## pride 
table(d167$tr1)

d167$tr2<-NA
d167$tr2[d167$TR  ==  1]<- 0 ## control 
d167$tr2[d167$TR  ==  3]<- 1 ## humiliation
table(d167$tr2)

d167$tr3<-NA
d167$tr3[d167$TR  ==  1]<- 0 ## control 
d167$tr3[d167$TR  ==  5]<- 1 ## demafatory
table(d167$tr3)


d167$tr4<-NA
d167$tr4[d167$TR  ==  1]<- 0 ## control 
d167$tr4[d167$TR  ==  2]<- 1 ## concilitory
table(d167$tr4)
table(d167$TR)

d167$WWD<-NA
d167$WWD[d167$TR == 1]<- 0 ## Non-WWD
d167$WWD[d167$TR > 1]<- 1 ## Non-WWD
table(d167$WWD)
## binary regression 

robust1.1<- glm(biper~tr1+age+gender+educ+income, data=d167,family = binomial("logit"))## pride
summary(robust1.1)
robust1.2<- glm(biper~tr2+age+gender+educ+income, data=d167,family = binomial("logit"))## humiliation
summary(robust1.2)
robust1.3<- glm(biper~tr3+age+gender+educ+income, data=d167,family = binomial("logit"))## demafaotry
summary(robust1.3)
robust1.4<- glm(biper~tr4+age+gender+educ+income, data=d167,family = binomial("logit"))## conciliatory
summary(robust1.4)

## Replication of Table A1: regression table summary (binary)
library(stargazer)
stargazer(robust1.1, robust1.2,robust1.3,robust1.4, type = "html", 
          title            = "",
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Concilitory rhetoric",
                               "Age",
                               "Male",
                               "Education",
                               "Income"
          ),
          dep.var.caption  = "Approval",
          dep.var.labels   = "Approval",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/survey/result/Appendix graph/table_robust1.html")

## Replication of Table A2
library(MASS)

robust2.1<- polr(as.factor(seven_point)~tr1+age+gender+educ+income, data=d167,Hess = TRUE)## pride

robust2.2<- polr(as.factor(seven_point)~tr2+age+gender+educ+income, data=d167,Hess = TRUE)## Humiliation

robust2.3<- polr(as.factor(seven_point)~tr3+age+gender+educ+income, data=d167,Hess = TRUE)## defamatory

robust2.4<- polr(as.factor(seven_point)~tr4+age+gender+educ+income, data=d167,Hess = TRUE)## conciliatory


stargazer(robust2.1, robust2.2,robust2.3,robust2.4, type = "html", 
          title            = "",
          
          covariate.labels = c("Pride rhetoric",
                               "Humiliation rhetoric",
                               "Defamatory rhetoric",
                               "Concilitory rhetoric",
                               "Age",
                               "Male",
                               "Education",
                               "Income"
          ),
          dep.var.caption  = "Approval Rate (Seven Point Scale)",
          dep.var.labels   = "Approval Rate (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/survey/result/Appendix graph/table_sevenapproval.html")

## Replication of Table A3

robust3.1<- polr(as.factor(FP)~tr1+age+gender+educ+income, data=d167,Hess = TRUE)## pride
robust3.2<- polr(as.factor(FP)~tr2+age+gender+educ+income, data=d167,Hess = TRUE)## Humiliation
robust3.3<- polr(as.factor(FP)~tr3+age+gender+educ+income, data=d167,Hess = TRUE)## defamatory
robust3.4<- polr(as.factor(FP)~tr4+age+gender+educ+income, data=d167,Hess = TRUE)## concilitatory

stargazer(robust3.1, robust3.2,robust3.3,robust3.4,
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
          dep.var.labels   = "Support for Aggressive Foreign Policy toward the U.S. (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/survey/result/Appendix graph/table_FP1.html")

## Replication of Table A4
robust4.1<- polr(as.factor(FP2)~tr1+age+gender+educ+income, data=d167,Hess = TRUE)## pride
robust4.2<- polr(as.factor(FP2)~tr2+age+gender+educ+income, data=d167,Hess = TRUE)## Humiliation
robust4.3<- polr(as.factor(FP2)~tr3+age+gender+educ+income, data=d167,Hess = TRUE)## defamatory
robust4.4<- polr(as.factor(FP2)~tr4+age+gender+educ+income, data=d167,Hess = TRUE)## conciliatory

stargazer(robust4.1, robust4.2,robust4.3,robust4.4,
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
          dep.var.labels   = "Support for Reducing Trade between the U.S. and China (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/survey/result/Appendix graph/table_FP2.html")

## Replication of Table A5
robust5.1<- polr(as.factor(FP3)~tr1+age+gender+educ+income, data=d167,Hess = TRUE)## pride
robust5.2<- polr(as.factor(FP3)~tr2+age+gender+educ+income, data=d167,Hess = TRUE)## Humiliation
robust5.3<- polr(as.factor(FP3)~tr3+age+gender+educ+income, data=d167,Hess = TRUE)## defamatory
robust5.4<- polr(as.factor(FP3)~tr4+age+gender+educ+income, data=d167,Hess = TRUE)## conciliatory

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
          dep.var.labels   = "Support for Limiting the Growth of the U.S. power (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/survey/result/Appendix graph/table_FP3.html")
## Replication of Table A6
robust6.1<- polr(as.factor(FP4)~tr1+age+gender+educ+income, data=d167,Hess = TRUE)## pride
robust6.2<- polr(as.factor(FP4)~tr2+age+gender+educ+income, data=d167,Hess = TRUE)## Humiliation
robust6.3<- polr(as.factor(FP4)~tr3+age+gender+educ+income, data=d167,Hess = TRUE)## defamatory
robust6.4<- polr(as.factor(FP4)~tr4+age+gender+educ+income, data=d167,Hess = TRUE)## conciliatory

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
          dep.var.labels   = " Support for Engaging in Diplomacy with the U.S. (Seven Point Scale)",
          column.separate = c(4, 1),
          omit.stat=c("LL","aic"), no.space=TRUE,
          align = TRUE,
          style = "qje",
          notes = ("Two-tail significance levels."),
          out="/Users/victorxu/Desktop/WWD/survey/result/Appendix graph/table_FP4.html")


## Mediation analysis
library(mediation)
## pride rhetoric effect
d3<- d167 %>% ##
  filter(TR=="1" |TR=="4") %>%
  glimpse()

##  in-group favoritism 

med.fit2<-lm(ingroup~TR+age+gender+educ+income, data=d3)
summary(med.fit2)
out.fit2<- lm(biper~ingroup+TR+age+gender+educ+income, data=d3)
summary(out.fit2)
set.seed(1991)
med.out2<- mediation::mediate(med.fit2, out.fit2, treat = "TR", mediator = "ingroup",sim=1000,boost= TRUE, conf.level = 0.95)#0.07;0.067;0.07
summary(med.out2)
plot(med.out2)
f2<- plot(med.out2, main = "Pride rhetoric")

## out-group for FP 

med.fit2.2<-lm(outgroup~TR+age+gender+educ+income, data=d3)
summary(med.fit2.2)
out.fit2.2<- lm(FP~outgroup+TR+age+gender+educ+income, data=d3)
summary(out.fit2.2)
set.seed(1991)
med.out2.2<- mediation::mediate(med.fit2.2, out.fit2.2, treat = "TR", mediator = "outgroup",sim=1000,conf.level = 0.9)##0.48
summary(med.out2.2)
f6<-plot(med.out2.2, main = "Pride rhetoric")

## Humiliation rhetoric effect
d2<- d167 %>% 
  filter(TR=="1" |TR=="3") %>%
  glimpse()

## mediator ingroup 

med.fit1<-lm(ingroup~TR+age+gender+educ+income, data=d2)
summary(med.fit1)
out.fit1<- lm(biper~ingroup+TR+age+gender+educ+income, data=d2)
summary(out.fit1)
set.seed(1991)
med.out1<- mediation::mediate(med.fit1, out.fit1, treat = "TR", mediator = "ingroup",sim=1000,boost = TRUE,conf.level = 0.95)### 0.004
summary(med.out1)

f1<- plot(med.out1, main = "Humiliation rhetoric")

## mediator outgroup for FP
med.fit1.2<-lm(outgroup~TR+age+gender+educ+income, data=d2)
summary(med.fit1.2)
out.fit1.2<- lm(FP~outgroup+TR+age+gender+educ+income, data=d2)
summary(out.fit1.2)

med.out1.2<- mediation::mediate(med.fit1.2, out.fit1.2, treat = "TR", mediator = "outgroup",sim=1000,conf.level = 0.95)## 0.20
summary(med.out1.2)
f4<- plot(med.out1.2, main = "Humiliation rhetoric")


## defamatory rhetoric effect
d4<- d167 %>% 
  filter(TR=="1" |TR=="5") %>%
  glimpse()

## mediator ingroup

med.fit3<-lm(ingroup~TR+age+gender+educ+income, data=d4)
summary(med.fit3)
out.fit3<- lm(biper~ingroup+TR+age+gender+educ+income, data=d4)
summary(out.fit3)
set.seed(1991)
med.out3<- mediation::mediate(med.fit3, out.fit3, treat = "TR", mediator = "ingroup",sim=1000,boost = TRUE,conf.level = 0.95)
summary(med.out3)## 0.34
plot(med.out3)

f3<- plot(med.out3, main = "Defamatory rhetoric")

## out-group for FP

d4$FP<- as.numeric(d4$FP)
med.fit3.2<-lm(outgroup~TR+age+gender+educ+income, data=d4)
summary(med.fit3.2)
out.fit3.2<- lm(FP~outgroup+TR+age+gender+educ+income, data=d4)
summary(out.fit3.2)
set.seed(1991)
med.out3.2<- mediation::mediate(med.fit3.2, out.fit3.2, treat = "TR", mediator = "outgroup",sim=1000,conf.level = 0.95)## 0.33
summary(med.out3.2)
f5<- plot(med.out3.2, main = "Defamatory rhetoric")

## WWD ingroup 

med.fit0<-lm(ingroup~WWD+age+gender+educ+income, data=d167)
summary(med.fit0)
out.fit0<- lm(biper~ingroup+WWD+age+gender+educ+income, data=d167)
summary(out.fit0)
set.seed(1991)
med.out0<- mediation::mediate(med.fit0, out.fit0, treat = "WWD", mediator = "ingroup",sim=1000,boost = TRUE,conf.level = 0.95)### 0.004
summary(med.out0)

### WWD outgroup
d167$FP<- as.numeric(d167$FP)
med.fit7<-lm(outgroup~WWD+age+gender+educ+income, data=d167)
summary(med.fit7)
out.fit7<- lm(FP~outgroup+WWD+age+gender+educ+income, data=d167)
summary(out.fit7)
set.seed(1991)
med.out7<- mediation::mediate(med.fit7, out.fit7, treat = "WWD", mediator = "outgroup",sim=1000,conf.level = 0.95)## 0.51
summary(med.out7)
f7<- plot(med.out7, main = "Wolf Warrior rhetoric")


## Replication of Figure A3 
dev.off(dev.list()["RStudioGD"])
par(mfrow=c(4,1))
f0<- plot(med.out0, main = "Wolf Warrior rhetoric")
f2<- plot(med.out2, main = "Pride rhetoric")
f1<- plot(med.out1, main = "Humiliation rhetoric")
f3<- plot(med.out3, main = "Defamatory rhetoric")
ggsave("/Users/victorxu/Desktop/WWD/survey/result/Appendix graph/mediator_ingroup_new.jpeg",
       width = 400, 
       height = 600, 
       units = "mm",
       dpi=300)


## Replication of Figure A4
dev.off(dev.list()["RStudioGD"])
par(mfrow=c(4,1))
f7<- plot(med.out7, main = "Wolf Warrior rhetoric")
f6<-plot(med.out2.2, main = "Pride rhetoric")
f4<- plot(med.out1.2, main = "Humiliation rhetoric")
f5<- plot(med.out3.2, main = "Defamatory rhetoric")
ggsave("/Users/victorxu/Desktop/WWD/survey/result/Appendix graph/mediator_outgroup_new.jpeg",
       width = 400, 
       height = 600, 
       units = "mm",
       dpi=300)


## Heterogeneous Effect Figure A6-A13 

# Rename treatment groups for Heterogeneous Effect
d167$tg <- NA
d167$tg[d167$TR  ==  1] <- 0
d167$tg[d167$TR  ==  4] <- 1
d167$tg[d167$TR  ==  3] <- 2
d167$tg[d167$TR  ==  5] <- 3
d167$tg[d167$TR  ==  2] <- 4
xtabs(~TR + tg, d167)

d167$tg <- factor(d167$tg,
                  levels = c(0,1,2,3,4),
                  labels = c("Control group (C)", "Pride rhetoric","Humiliaiton rhetoric", 
                             "Defamatory rhetoric","Concilitaroy rhetoric"))
table(d167$tg)


## Replication of Figure A6-7

## Data preparation for Right Wing Authoritarianism (RWA) 
## recode Q4.10 
table(d167$rw3)
d167$rw3_2<- NA 
d167$rw3_2[d167$rw3==2]<-0
d167$rw3_2[d167$rw3==1]<-1
table(d167$rw3_2)


table(d167$rw1)
d167$rw1_2<- NA 
d167$rw1_2[d167$rw1==2]<-1
d167$rw1_2[d167$rw1==1]<-0
table(d167$rw1_2)


table(d167$rw2)
d167$rw2_2<- NA 
d167$rw2_2[d167$rw2==2]<-1
d167$rw2_2[d167$rw2==1]<-0
table(d167$rw2_2)

table(d167$rw4)
d167$rw4_2<- NA 
d167$rw4_2[d167$rw4==2]<-1
d167$rw4_2[d167$rw4==1]<-0
table(d167$rw4_2)

d167$RWA<- d167$rw4_2+d167$rw3_2+d167$rw2_2+d167$rw1_2
table(d167$RWA)

# Gen RWA variable
table(d167$RWA)
d167$biRWA <- ifelse((d167$RWA  ==  3 | d167$RWA  ==  4), 1, ## high RWA
                     ifelse((d167$RWA  ==  0 | d167$RWA  ==  1 | d167$RWA  ==  2), 0, NA)) ## low RWA
xtabs(~d167$RWA + d167$biRWA)

d167$biRWA <- factor(d167$biRWA,
                     levels = c(1, 0),
                     labels = c("High-level RWA", "Low-level RWA"))

xtabs(~d167$RWA + d167$biRWA)

# Replication of Figure A6
SumStat <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$biRWA))), 
                     measurevar = "biper", groupvars = c("tg", "biRWA"), na.rm = TRUE)
print(SumStat)

SumStat$effect <- ifelse((SumStat$biRWA  ==  "Low-level RWA"), 
                         (SumStat$biper - SumStat$biper [SumStat$tg  ==  "Control group (C)" & SumStat$biRWA  ==  "Low-level RWA"]),                         (SumStat$biper - SumStat$biper [SumStat$tg  ==  "Control group (C)" & SumStat$biRWA  ==  "High-level RWA"]))
SumStat

Approve_CI95_3_RWA_a <- ggplot(subset(SumStat, SumStat$tg != "Control group (C)"), aes(x = tg, y = effect*100, group = biRWA, color = biRWA, shape = biRWA)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect-ci)*100, ymax = (effect+ci)*100), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on percentage approval (% point)\n") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())

Approve_CI95_3_RWA_a

# Replication of Figure A7 
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
  ylab("Average effects on the Chinese public support's for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 10),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())

Approve_CI95_3_RWA_b

### Militant assertiveness (MA)
table(d167$mili_1)
d167$mili_1_new[d167$mili_1==1]<- 5
d167$mili_1_new[d167$mili_1==2]<- 4
d167$mili_1_new[d167$mili_1==3]<- 3
d167$mili_1_new[d167$mili_1==4]<- 2
d167$mili_1_new[d167$mili_1==5]<- 1
table(d167$mili_1_new)

table(d167$mili_3)
d167$mili_3_new[d167$mili_3==1]<- 5
d167$mili_3_new[d167$mili_3==2]<- 4
d167$mili_3_new[d167$mili_3==3]<- 3
d167$mili_3_new[d167$mili_3==4]<- 2
d167$mili_3_new[d167$mili_3==5]<- 1
table(d167$mili_3_new)

d167$MA<- d167$mili_1_new+d167$mili_2+d167$mili_3_new
table(d167$MA)

# Gen Militant assertiveness (MA) variable
table(d167$MA)
d167$biMA <- ifelse((d167$MA>= 3 & d167$MA<=9 ), 0, ## non-MA
                    ifelse((d167$MA>=10 & d167$MA<=15), 1, NA)) ## MA
xtabs(~d167$MA + d167$biMA)

d167$biMA <- factor(d167$biMA,
                    levels = c(1, 0),
                    labels = c("Militant assertiveness", "Non-militant assertiveness"))

xtabs(~d167$MA + d167$biMA)


# Replication of Figure A8
SumStat3 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$biMA))), 
                      measurevar = "biper", groupvars = c("tg", "biMA"), na.rm = TRUE)
print(SumStat3)

SumStat3$effect3 <- ifelse((SumStat3$biMA  ==  "Non-militant assertiveness"), 
                           (SumStat3$biper - SumStat3$biper [SumStat3$tg  ==  "Control group (C)" & SumStat3$biMA  ==  "Non-militant assertiveness"]),
                           (SumStat3$biper - SumStat3$biper [SumStat3$tg  ==  "Control group (C)" & SumStat3$biMA  ==  "Militant assertiveness"]))
SumStat3


Approve_CI95_3_MA_a <- ggplot(subset(SumStat3, SumStat3$tg != "Control group (C)"), aes(x = tg, y = effect3*100, group = biMA, color = biMA, shape = biMA)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect3-ci)*100, ymax = (effect3+ci)*100), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on percentage approval (% point)\n") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())
Approve_CI95_3_MA_a


# Replication of Figure A9
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
  ylab("Average effects on the Chinese public's support for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())

Approve_CI95_3_MA_b


# Gen Hawk and dove  
table(d167$hawk)
d167$hawk <- ifelse((d167$hawk == 3 |d167$hawk== 4 ), 0, ## Dove
                    ifelse((d167$hawk == 1 |d167$hawk == 2 ), 1, NA)) ## Hawk
xtabs(~d167$hawk + d167$hawk)
d167$hawk <- factor(d167$hawk,
                    levels = c(1, 0),
                    labels = c("Hawk", "Dove"))

xtabs(~d167$hawk + d167$hawk)

# # Replication of Figure A10
SumStat5 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$hawk))), 
                      measurevar = "biper", groupvars = c("tg", "hawk"), na.rm = TRUE)
print(SumStat5)

SumStat5$effect5 <- ifelse((SumStat5$hawk  ==  "Dove"), 
                           (SumStat5$biper - SumStat5$biper [SumStat5$tg  ==  "Control group (C)" & SumStat5$hawk  ==  "Dove"]),
                           (SumStat5$biper - SumStat5$biper [SumStat5$tg  ==  "Control group (C)" & SumStat5$hawk  ==  "Hawk"]))
SumStat5


Approve_CI95_3_hawk_a <- ggplot(subset(SumStat5, SumStat5$tg != "Control group (C)"), aes(x = tg, y = effect5*100, group = hawk, color = hawk, shape = hawk)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect5-ci)*100, ymax = (effect5+ci)*100), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on percentage approval (% point)\n") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())
Approve_CI95_3_hawk_a


# Replication of Figure A11

SumStat6 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$hawk))), 
                      measurevar = "FP", groupvars = c("tg", "hawk"), na.rm = TRUE)
print(SumStat6)

SumStat6$effect6 <- ifelse((SumStat6$hawk  ==  "Dove"), 
                           (SumStat6$FP - SumStat6$FP [SumStat6$tg  ==  "Control group (C)" & SumStat6$hawk  ==  "Dove"]),
                           (SumStat6$FP - SumStat6$FP [SumStat6$tg  ==  "Control group (C)" & SumStat6$hawk  ==  "Hawk"]))
SumStat6


Approve_CI95_3_hawk_b <- ggplot(subset(SumStat6, SumStat6$tg != "Control group (C)"), aes(x = tg, y = effect6, group = hawk, color = hawk, shape = hawk)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect6-ci), ymax = (effect6+ci)), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on the Chinese public's support for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())


Approve_CI95_3_hawk_b

## liberal - conservative 

table(d167$ideo)
d167$conser <- ifelse((d167$ideo==4|d167$ideo==5|d167$ideo==6|d167$ideo==7), 0, ## Liberal
                      ifelse((d167$ideo==1|d167$ideo==2|d167$ideo==3), 1, NA)) ## conservative

xtabs(~d167$ideo+ d167$conser)

d167$conser <- factor(d167$conser,
                      levels = c(1, 0),
                      labels = c("Conservatives", "Liberals"))

xtabs(~d167$ideo + d167$conser)

#Replication of Figure A12
SumStat7 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$conser))), 
                      measurevar = "biper", groupvars = c("tg", "conser"), na.rm = TRUE)
print(SumStat7)

SumStat7$effect7 <- ifelse((SumStat7$conser  ==  "Liberal"), 
                           (SumStat7$biper - SumStat7$biper [SumStat7$tg  ==  "Control group (C)" & SumStat7$conser  ==  "Liberals"]),
                           (SumStat7$biper - SumStat7$biper [SumStat7$tg  ==  "Control group (C)" & SumStat7$conser  ==  "Conservatives"]))
SumStat7


Approve_CI95_3_conser_a <- ggplot(subset(SumStat7, SumStat7$tg != "Control group (C)"), aes(x = tg, y = effect7*100, group = conser, color = conser, shape = conser)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect7-ci)*100, ymax = (effect7+ci)*100), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on percentage approval (% point)\n") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())
Approve_CI95_3_conser_a

# Replication of Figure A13
SumStat8 <- summarySE(subset(d167, (!is.na(d167$tg) & !is.na(d167$conser))), 
                      measurevar = "FP", groupvars = c("tg", "conser"), na.rm = TRUE)
print(SumStat8)

SumStat8$effect8 <- ifelse((SumStat8$conser  ==  "Liberal"), 
                           (SumStat8$FP - SumStat8$FP [SumStat8$tg  ==  "Control group (C)" & SumStat8$conser  ==  "Liberals"]),
                           (SumStat8$FP - SumStat8$FP [SumStat8$tg  ==  "Control group (C)" & SumStat8$conser  ==  "Conservatives"]))
SumStat8

Approve_CI95_3_conser_b <- ggplot(subset(SumStat8, SumStat8$tg != "Control group (C)"), aes(x = tg, y = effect8, group = conser, color = conser, shape = conser)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_errorbar(width = .1, aes(ymin = (effect8-ci), ymax = (effect8+ci)), position = position_dodge(0.25)) +
  geom_point(size = 4, position = position_dodge(0.25)) +
  ylab("Average effects on the Chinese public's support for aggressive foreign policy") + xlab("") + theme_bw() +
  scale_color_grey(start = 0.65, end = 0) +
  scale_shape_manual(values = c(20, 18)) +
  theme(axis.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 13, face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification = c(1, 1), legend.position = c(1, 1), 
        legend.title = element_blank(), legend.background = element_blank())
Approve_CI95_3_conser_b


## Replication of Figure A1
## Load the required packages

## Clean the R environment and set the working directory
# RStudio version: 1.3.959
rm(list = ls())

## Load the required packages
library(gplots)    # version 3.1.1
library(extrafont) # version 0.17
d167<- read_xlsx("/Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/Harvard dataset/JCR_China Survey.xlsx")



### Figure A2: Univariate Balance on Pre-Treatment Covariates ###

pdf("/Users/victorxu/Desktop/WWD/Journal submission/JCR/Accepted/Replication dataset/Figure_balance_china.pdf", height = 11, width = 11)## change the location
par(mfrow = c(2,2))


## female 
d167$female <- NA
d167$female [d167$gender==2]<-1 ## female
d167$female [d167$gender==1]<-0 ## male 
## CCP
d167$CCP <- NA 
d167$CCP [d167$party==1]<-1 ## CCP
d167$CCP [d167$party>=2]<-0 ## non-CCP

## college degree 
d167$col<- NA
d167$col[d167$educ<=3]<-0 ## lower than 4-year college
d167$col[d167$educ>=4]<-1 ## college or above

## income
table(d167$income)
d167$income_2[d167$income<=5 & d167$income>=1]<- 0 ## Low income
d167$income_2[d167$income>=6 & d167$income<=9]<- 1 ## high-income
table(d167$income_2)
# Gender
plot(density(d167[d167$TR==1,]$female, na.rm=T, bw=0.1), xlab="Gender (0 = Male, 1 = Female)", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="Gender", lty=1, cex=1.2)
par(new=TRUE)
plot(density(d167[d167$TR==4,]$female, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=2)
par(new=TRUE)
plot(density(d167[d167$TR==3,]$female, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=3)
par(new=TRUE)
plot(density(d167[d167$TR==5,]$female, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=4)
par(new=TRUE)
plot(density(d167[d167$TR==2,]$female, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.5), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=5)

par(new=TRUE, family="Times")
legend("topleft", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), lty=c(1,2,3,4,5), cex=1, bty="n")

# CCP
plot(density(d167[d167$TR==1,]$CCP, na.rm=T, bw=0.1), xlab="Members of Chinese Communist Party (0 = Non-CCP, 1 = CCP)", ylim=c(0,4), xlim=c(-0.4,1.4), main="Party Affilication", lty=1, cex=1.2)
par(new=TRUE)
plot(density(d167[d167$TR==4,]$CCP, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=2)
par(new=TRUE)
plot(density(d167[d167$TR==3,]$CCP, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=3)
par(new=TRUE)
plot(density(d167[d167$TR==5,]$CCP, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=4)
par(new=TRUE)
plot(density(d167[d167$TR==2,]$CCP, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=4)
par(new=TRUE)
par(new=TRUE, family="Times")
legend("topleft", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5" ), lty=c(1,2,3,4,5), cex=1, bty="n")



# Education
plot(density(d167[d167$TR==1,]$col, na.rm=T, bw=0.1), xlab="College Degree (0 = No, 1 = Yes)", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="College Degree", lty=1, cex=1.2)
par(new=TRUE)
plot(density(d167[d167$TR==4,]$col, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=2)
par(new=TRUE)
plot(density(d167[d167$TR==3,]$col, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=3)
par(new=TRUE)
plot(density(d167[d167$TR==1,]$col, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=4)
par(new=TRUE)
plot(density(d167[d167$TR==2,]$col, na.rm=T, bw=0.1), xlab="", ylim=c(0,3.7), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=5)

par(new=TRUE, family="Times")
legend("topleft", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), lty=c(1,2,3,4,5), cex=1, bty="n")

# Income
plot(density(d167[d167$TR==1,]$income_2, na.rm=T, bw=0.1), xlab="High Income (0 = Low-Income, 1 = High-Income)", ylim=c(0,4), xlim=c(-0.4,1.4), main="Income", lty=1, cex=1.2)
par(new=TRUE)
plot(density(d167[d167$TR==4,]$income_2, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=2)
par(new=TRUE)
plot(density(d167[d167$TR==3,]$income_2, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=3)
par(new=TRUE)
plot(density(d167[d167$TR==5,]$income_2, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=5)
par(new=TRUE)
plot(density(d167[d167$TR==2,]$income_2, na.rm=T, bw=0.1), xlab="", ylim=c(0,4), xlim=c(-0.4,1.4), main="", axes=F, ylab="", lty=6)
par(new=TRUE, family="Times")
legend("topright", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), lty=c(1,2,3,4,5), cex=1, bty="n")
legend("topright", legend=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"), lty=c(1,2,3,4,5), cex=1, bty="n")

dev.off()

## graph within-subject Replication of A19

## within subject humiliation 
d2within<- d167 %>%
  filter (TR=="3") %>%
  glimpse() ## 301
mean(d2within$biper)
mean(d2within$biper2)

d2.1_long <- d2within %>%
  gather(key = "group", value = "support", biper, biper2)
## head(d2.1_long, 10)
table1<- d2.1_long %>%
  group_by(group) %>%
  get_summary_stats(support)

## within subject pride
d3within<- d167 %>%
  filter (TR=="4") %>%
  glimpse() ## 300

d3.1_long <- d3within %>%
  gather(key = "group", value = "support", biper, biper2)
table2<- d3.1_long%>%
  group_by(group) %>%
  get_summary_stats(support)

t.test(support ~ group, data = d3.1_long, paired = TRUE) 
## within subject defamatory
d4within<- d167 %>%
  filter (TR=="5") %>%
  glimpse() ## 301

d4.1_long <- d4within %>%
  gather(key = "group", value = "support", biper, biper2)
head(d4.1_long, 10)
table3<- d4.1_long%>%
  group_by(group) %>%
  get_summary_stats(support)

t.test(support ~ group, data = d4.1_long, paired = TRUE) 

within_table<- rbind(table2, table1,table3)

within_table$name <- c(
  "Pride rhetoric",
  "Pride rhetoric + cost",
  "Humiliation rhetoric",
  "Humiliation rhetoric + cost",
  "Defamatory rhetoric",
  "Defamatory rhetoric + cost"
)

within_table$groupno<- c(1,2,3,4,5,6)

## graph within-subject 
library(ggplot2)

p<- ggplot(within_table, aes(x=factor(groupno), y=mean)) +
  geom_bar(stat="identity", aes(fill=factor(group)), width=0.5) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=0.2) +
  scale_x_discrete(labels=within_table$name) +
  scale_y_continuous(labels=scales::percent_format(suffix="", accuracy=1), breaks=seq(0, 1, 0.1)) + 
  theme_bw() +
  labs(
    x="Experimental groups",
    y="Percentage approval (%)",
    title=""
  ) +
  theme(
    panel.grid = element_line(color='white'),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=14, color='black', margin=margin(t=0, r=16, b=0, l=0)),
    axis.text = element_text(size=12, color='black'),
    legend.text = element_text(size=12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p+  scale_fill_grey(name = "Cost", labels = c("Without Economic Cost", "With Economic Cost"),start = 0.45, end = 0.80) 



