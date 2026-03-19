
# First, clean out any objects 
rm(list=ls())

library(ggplot2)

library(haven)

library(dplyr)
library(reshape2)
library(tidyverse)
library(ggthemes)
library(scales)
library(stringr)

library(sandwich)
library(lmtest)
library(broom)
library(margins)

library(stargazer)
library(cowplot)
library(xtable)
library(tigerstats)


# library(zeligverse) # stopped working; install directly; see below


setwd("C:/Users/msouva/Documents/Air_Power/Air superiority and war outcomes/R&P data files")

# Load the data

airpower <- read_dta("SS_airvictory.dta")

# create some variables and add them to the data
# create binary regime variables
airpower <-mutate(airpower,d6=ifelse(polity>16,1,0))
airpower <-mutate(airpower,a6=ifelse(polity <6,1,0))
airpower <-mutate(airpower,anoc=ifelse(polity>=6 & polity<=16,1,0))

# create a binary air superiority variable
# air superiority binary = 1 if air superiority, 0 otherwise
airpower <-mutate(airpower, airsupbin=ifelse(airsup >1,1,0))
airpower <-mutate(airpower, airparity=ifelse(airsup ==1,1,0))

# create binary modern system variables, one for each category
airpower <-mutate(airpower, ms3=ifelse(msadopt==3,1,0))
airpower <-mutate(airpower, ms2=ifelse(msadopt==2,1,0))
airpower <-mutate(airpower, ms1=ifelse(msadopt==1,1,0))
airpower <-mutate(airpower, ms0=ifelse(msadopt==0,1,0))
# create a single binary modern system variable
# msbinary = 1 if modern system = 3 or 4, 0 otherwise 
airpower <-mutate(airpower, msbinary=ifelse(msadopt>=2,1,0))

# create a binary IWD_Win_Loss DV
airpower <- mutate(airpower, iwd_wl=ifelse(IWD_WLT==2,1,0))
table(airpower$IWD_WLT)
table(airpower$iwd_wl)

# create a binary IWD Win Loss outcome for subwars
airpower <- mutate(airpower, subwariwd_wl=ifelse(Subwar_WLT==2,1,0))
table(airpower$Subwar_WLT)
table(airpower$subwariwd_wl)

# combine IWD DV wars and subwars into one outcome variable
airpower <- mutate(airpower, iwd_wl_all=iwd_wl)

airpower <- mutate(airpower, iwd_wl=replace_na(iwd_wl,-9)) 
airpower <- mutate(airpower, iwd_wl_all=ifelse(iwd_wl == -9, subwariwd_wl,iwd_wl_all))
table(airpower$iwd_wl_all)

airpower <- mutate(airpower, iwd_wl=na_if(iwd_wl,-9)) 
summary(airpower$iwd_wl)



# create a data frame with complete cases, i.e. drops cases that have missing values on a covariate
df <- airpower %>% 
  filter(!is.na(win)) %>%
  filter(!is.na(msadopt)) %>%
  filter(!is.na(polity)) %>%
  filter(!is.na(airsup)) %>%
  filter(!is.na(cinc)) %>%
  filter(!is.na(troopsengaged)) %>%
  filter(!is.na(opptroopsengaged)) %>%
  filter(!is.na(bankscoups))

summary(df)


# Table air superiority-binary
table(df$airsupbin)

df <- mutate(df, outcome=ifelse(win==1,"Win","Loss"))


# Table 1
# cross-tabulation: air superiority and decisive battle outcome
tb <- table(df$airsupbin, df$outcome)
tb
# obtain expected counts
expCounts(tb)

# calculate chi-square statistic
tb.prop <- prop.table(tb, 1)
tb.prop

tb.df <- as.data.frame(tb.prop)

names(tb.df) <- c("Air Superiority", "Outcome", "Frequency")

# In the paper, we report the chi-square statistic from Stata. This one differs slightly. 
chi_test <- chisq.test(df$airsupbin, df$outcome)
chi_test

chi_note <- 
  sprintf("Chi-Square Test Statistic = %.1f, P-value = %.3f", 
          chi_test$statistic,
          chi_test$p.value)
chi_note


# Figure: cross-tabulation: air superiority and decisive battle outcome

as_outcome <- ggplot(tb.df, aes(x=`Air Superiority`, y=Frequency, fill=`Outcome`)) +
  geom_col(position="dodge") +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  labs(title="Air Superiority and Decisive Battle Outcome, 1932-2003")+
  labs(subtitle = chi_note) +
  labs(caption="Notes: Loss defined as loss or tie, Grauer & Horowitz (2012) data. \n
       No Air Superiority defined as parity or inferiority.")
as_outcome


# Table 2 and Figure: Regime Type and Air Superiority

df <-mutate(df,dem3=ifelse(polity>16,"Democracy",ifelse(polity>=6 & polity <=16,"Anocracy","Autocracy")))

table(df$dem3)

df <-mutate(df,airsup_yn=ifelse(airsup>1,"Yes","No"))

# Table 2
tb2 <- table(df$dem3, df$airsup_yn)
tb2
# obtain expected counts
expCounts(tb2)

tb.prop2 <- prop.table(tb2, 1)
tb.prop2

tb.df2 <- as.data.frame(tb.prop2)

names(tb.df2) <- c("Regime Type", "Air Superiority", "Frequency")


chi_test <- chisq.test(df$dem3, df$airsup_yn)
chi_test
chi_note <- 
  sprintf("Chi-Square Test Statistic = %.1f, P-value = %.3f", 
          chi_test$statistic,
          chi_test$p.value)
chi_note

# The palette with black:
cbbPalette <- c( "#D55E00", "#0072B2")

ggplot(tb.df2, aes(x=`Regime Type`, y=Frequency, fill=`Air Superiority`)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=cbbPalette) +
  labs(title="Regime Type and Air Superiority, 1932-2003")+
  labs(subtitle=chi_note)+
  labs(caption="Notes: Regime types based on polity2 variable. \n
       No Air Superiority defined as parity or inferiority.")



# Models 

df <-mutate(df,airsupbin=ifelse(airsup>1,1,0))

# GH with binary regime vars, no air sup
m1 <- glm(win ~ msadopt + d6 +  anoc + cinc + troopsengaged + opptroopsengaged + bankscoups, 
          family=binomial(link="logit"), data=df)

clusterSEm1 <- vcovCL(m1, cluster = ~ ccode)

coeftest(m1,clusterSEm1)
AIC(m1)

# add air superiority and air parity; primary model  
m2 <- glm(win ~ airsupbin + airparity + msadopt + d6 + anoc + cinc + troopsengaged + opptroopsengaged + bankscoups, 
          family=binomial(link="logit"), data=df)

clusterSEm2 <- vcovCL(m2, cluster = ~ ccode)

coeftest(m2,clusterSEm2)
AIC(m2)



# Produce Coefficient Plot 
model1Frame <- data.frame(Variable = rownames(summary(m1)$coef),
                          Coefficient = summary(m1)$coef[, 1],
                          SE = diag(clusterSEm1),
                          Model = "M1: No Air Superiority")


model2Frame <- data.frame(Variable = rownames(summary(m2)$coef),
                          Coefficient = summary(m2)$coef[, 1],
                          SE = diag(clusterSEm2),
                          Model = "M2: Air Superiority With Controls")

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame))

# Order the variables
allModelFrame$Variable <- factor(allModelFrame$Variable, 
                                 levels = c("(Intercept)","bankscoups", "opptroopsengaged","troopsengaged", 
                                            "cinc" , "anoc" , "d6", "msadopt",
                                            "airparity", "airsupbin"))



# Specify the width of your confidence intervals
#interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
logit_plot <- ggplot(allModelFrame, aes(colour = Model)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - sqrt(SE)*interval2,
                                ymax = Coefficient + sqrt(SE)*interval2),
                            lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - sqrt(SE)*interval2,
                                 ymax = Coefficient + sqrt(SE)*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE") + 
  scale_x_discrete(labels=c("airsupbin" = "Air Superiority",
                            "airparity"="Air Parity", "msadopt"="Modern System",
                            "d6"="Democracy","anoc"="Anocracy","cinc"="National Capabilities",
                            "troopsengaged"="Troops Engaged","opptroopsengaged"="Opponent Troops",
                            "bankscoups"="Past Coups",
                            "(Intercept)"="Intercept")) +
coord_flip() + theme_bw() + 
  labs(title = "Logistic Regression Estimates, 1932-2003", subtitle = "Outcome: Decisive Battle Victory/Loss") + 
  labs(caption = "95% CIs, standard errors clustered on country")

print(logit_plot)



# Some predicted probabilities

# probability of winning with no air superiority
df1 <- with(df, data.frame(airsupbin=0, airparity=0, msadopt=1, d6=1, anoc=0, cinc=mean(cinc),troopsengaged=mean(troopsengaged), opptroopsengaged=mean(opptroopsengaged),bankscoups=median(bankscoups)))
pred_as0 <- predict(m2, newdata=df1,type="response")
pred_as0

# probability of winning with air superiority
df1 <- with(df, data.frame(airsupbin=1, airparity=0, msadopt=1, d6=1, anoc=0, cinc=mean(cinc),troopsengaged=mean(troopsengaged), opptroopsengaged=mean(opptroopsengaged),bankscoups=median(bankscoups)))
pred_as1 <- predict(m2, newdata=df1,type="response")
pred_as1


# probability of winning with no elements of modern system adoption
df1 <- with(df, data.frame(airsupbin=0, airparity=1, msadopt=0, d6=1, anoc=0, cinc=mean(cinc),troopsengaged=mean(troopsengaged), opptroopsengaged=mean(opptroopsengaged),bankscoups=median(bankscoups)))
pred_ms0 <- predict(m2, newdata=df1,type="response")
pred_ms0

# probability of winning with full modern system adoption 
df1 <- with(df, data.frame(airsupbin=0, airparity=1, msadopt=3, d6=1, anoc=0, cinc=mean(cinc),troopsengaged=mean(troopsengaged), opptroopsengaged=mean(opptroopsengaged),bankscoups=median(bankscoups)))
pred_ms3 <- predict(m2, newdata=df1,type="response")
pred_ms3


# substantive effects: first difference of air superiority; uses Zelig

#install.packages("https://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_5.1.6.1.tar.gz", 
#                 repos=NULL, 
#                 type="source")

library(Zelig)

# zelig

# air superiority and air parity; primary model  
zm2 <- zelig(win ~ airsupbin + airparity + msadopt + d6 + anoc + cinc + troopsengaged + opptroopsengaged + bankscoups, 
             model="logit", data=df)

summary(zm2)

# set values of air superiority
zm2 <- setx(zm2, airsupbin=0)
zm2 <- setx1(zm2, airsupbin=1)

# run simulations
zm2 <- sim(zm2)
# summarize simulations
summary(zm2)
# obtain zelig plots
plot(zm2)

# calculate first difference for air superiority
fd <- zm2$get_qi(xvalue="x1", qi="fd")
summary(fd)


# combine the air superiority and modern system first differences into a data frame
dfd <- data.frame(fd)
summary(dfd)

# tidy and organize the data frame to get what you want out of it
tidd <- dfd %>% 
  gather(class, simv)

tidd %>% 
  group_by(class) %>% 
  summarise(mean = mean(simv), sd = sd(simv))


# plot histograms of the simulations of the two first differences
ggplot(tidd, aes(simv)) + geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=median(simv)), color="blue", linetype="dashed", size=1) +
#  labs(title="Effect of Air Superiority on Battle Outcome, 1932-2003")+
  labs(caption="Notes: First differences based on Model 2. Dashed blue line indicates median.") +
  xlab("Air Superiority") +
  theme_classic()


packageVersion("zelig")


# Appendix and Robustness Checks 


# Summary Statistics 

#airpower <-mutate(airpower,airsupbin=ifelse(airsup>1,1,0))

df3 <- airpower %>% 
  filter(!is.na(win)) %>%
  filter(!is.na(airsupbin)) %>%
  filter(!is.na(airparity)) %>%
  filter(!is.na(msadopt)) %>%
  filter(!is.na(d6)) %>%
  filter(!is.na(anoc)) %>%
  filter(!is.na(cinc)) %>%
  filter(!is.na(troopsengaged)) %>%
  filter(!is.na(opptroopsengaged)) %>%
  filter(!is.na(bankscoups)) %>%
  select(win, airsupbin, airparity, msadopt, d6, anoc, cinc, troopsengaged, opptroopsengaged, bankscoups) %>%
  data.frame()


stargazer(df3, type="text", summary.stat = c("n", "mean", "sd", "min", "max"), digits=2,
          covariate.labels = c("Outcome", "Air Superiority","Air Parity", "Modern System", "Democracy", "Anocracy",
                               "CINC", "Troops Engaged", "Opponent Troops Engaged", "Past Coups"))

# now for latex code 
stargazer(df3, summary.stat = c("n", "mean", "sd", "min", "max"), digits=2,
          covariate.labels = c("Outcome", "Air Superiority","Air Parity", "Modern System", "Democracy", "Anocracy",
                               "CINC", "Troops Engaged", "Opponent Troops Engaged", "Past Coups"))

# Correlation Table using stargazer

correlation.matrix <- cor(df3)
# latex correlation table 
stargazer(correlation.matrix, title="Correlation Matrix")


# Correlation table with Xtable; this is easier to manipulate and only get the bottom half

mcor<-round(cor(df3),2)
mcor
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper


print(xtable(upper), type="latex")

print(xtable(upper), type="latex", file="corr_table.latex")


# Robustness Analysis


# change dem comparison, drop anoc
m3 <- glm(win ~ airsupbin + airparity + msadopt + d6 + cinc + troopsengaged + opptroopsengaged + bankscoups, 
          family=binomial(link="logit"), data=df)

clusterSEm3 <- vcovCL(m3, cluster = ~ ccode)

coeftest(m3,clusterSEm3)
AIC(m3)

# replace dem and anoc binary vars with polity and politysquared
m4 <- glm(win ~ airsupbin + airparity + msadopt + polity + politysquared + cinc + troopsengaged + opptroopsengaged + bankscoups, 
          family=binomial(link="logit"), data=df)

clusterSEm4 <- vcovCL(m4, cluster = ~ ccode)

coeftest(m4,clusterSEm4)
AIC(m4)


df46 <- df %>%
  filter(year>= 1946)


# m2 but only for 1946-2003  
m5 <- glm(win ~ airsupbin + airparity + msadopt + d6 + anoc + cinc + troopsengaged + opptroopsengaged + bankscoups, 
          family=binomial(link="logit"), data=df46)

clusterSEm5 <- vcovCL(m5, cluster = ~ ccode)

coeftest(m5,clusterSEm5)
AIC(m5)

  
# create a binary air superiority variable based on alternate coding
# air superiority binary = 1 if air superiority, 0 otherwise
df <-mutate(df, airsupbin_alt=ifelse(airsup_alt >1,1,0))
df <-mutate(df, airparity_alt=ifelse(airsup_alt ==1,1,0))


# m2 with alternate Air Superiority Coding 
m2alt <- glm(win ~ airsupbin_alt + airparity_alt + msadopt + d6 + anoc + cinc + troopsengaged + opptroopsengaged + bankscoups, 
          family=binomial(link="logit"), data=df)

clusterSEm2alt <- vcovCL(m2alt, cluster = ~ ccode)

coeftest(m2alt,clusterSEm2alt)
AIC(m2alt)



# latex code
stargazer(m1, m2, m3,m5, title="Logistic Regression Estimates, 1932-2003",
          column.labels=c("Model 1","Model 2","Model 3","Model 4"),
          dep.var.labels = "Decisive Battle Outcome",
          covariate.labels = c( "Air Superiority","Air Parity", "Modern System", 
                                "Democracy", "Anocracy","CINC",
                                "Troops Engaged", "Opponent Troops Engaged","Past Coups"),
          style="io",
          digits=2,
          digits.extra = 4,
          notes=("Two-tail significance levels; Standard Errors Clustered on Country"))


# latex code, drop 'type="text"
stargazer(m2alt, title="Logistic Regression Estimates, 1932-2003",
          column.labels=c("Model 2B"),
          dep.var.labels = "Decisive Battle Outcome",
          covariate.labels = c( "Air Superiority_Alt","Air Parity_Alt", "Modern System", 
                                "Democracy", "Anocracy","CINC",
                                "Troops Engaged", "Opponent Troops Engaged","Past Coups"),
          style="io",
          digits=2,
          digits.extra = 4,
          notes=("Two-tail significance levels; Standard Errors Clustered on Country"))



# In footnote 7, we compare AIC for a model with CINC versus one with Air Superiority
# Model 2b: air superiority and no CINC
# Model 2c: CINC and no air superiority

# airsup with controls 
m2b <- glm(win ~ airsupbin + airparity + msadopt + d6 + anoc + troopsengaged + opptroopsengaged + bankscoups, 
           family=binomial(link="logit"), data=df)

AIC(m2b)

m2c <- glm(win ~ cinc + msadopt + d6 + anoc + troopsengaged + opptroopsengaged + bankscoups, 
           family=binomial(link="logit"), data=df)

AIC(m2c)


# effect of msadopt is probably non-linear; replace msadopt with three dummy variables; ms=0 is reference
m <- glm(win ~ airsupbin + airparity + ms3 + ms2 + ms1 + d6 + anoc + cinc + troopsengaged + opptroopsengaged + bankscoups, 
         family=binomial(link="logit"), data=df)

clusterSEm <- vcovCL(m, cluster = ~ ccode)

coeftest(m,clusterSEm)
AIC(m)


# Robustness Analysis

# Change the DV from Decisive Battle to War, IWD data  

# Model 2 where unit of analysis is IWD war or subwar country major participant
# DV is IWD war or subwar outcome
# war outcome = 1 for victory, 0 otherwise
m5 <- glm(iwd_wl_all ~ airsupbin + airparity + msadopt + d6 + cinc + troopsengaged + opptroopsengaged + bankscoups, 
          family=binomial(link="logit"), data=df)

clusterSEm5 <- vcovCL(m5, cluster = ~ ccode)

coeftest(m5,clusterSEm5)
AIC(m5)

# Model 2 where unit of analysis is IWD war country major participant
# DV is IWD war outcome
# war outcome = 1 for victory, 0 otherwise
m6 <- glm(iwd_wl ~ airsupbin + airparity + msadopt + d6 + cinc + troopsengaged + opptroopsengaged + bankscoups, 
          family=binomial(link="logit"), data=df)

clusterSEm6 <- vcovCL(m6, cluster = ~ ccode)

coeftest(m6,clusterSEm6)
AIC(m6)


stargazer(m5, m6, type="text", title="Effect of Air Superiority on IWD War Outcome, 1932-2003",
          column.labels=c("Model 4","Model 5"),
          dep.var.labels = "IWD War Outcome",
          covariate.labels = c( "Air Superiority","Air Parity", "Modern System", "Democracy", "CINC",
                                "Troops Engaged", "Opponent Troops Engaged","Past Coups"),
          style="io",
          digits=2,
          digits.extra = 4,
          notes=("Two-tail significance levels; Clustered standard errors"),
          out="models5-6.txt")


# latex code: 
stargazer(m5, m6, title="Effect of Air Superiority on IWD War Outcome, 1932-2003",
          column.labels=c("Model 4","Model 5"),
          dep.var.labels = "IWD War Outcome",
          covariate.labels = c( "Air Superiority","Air Parity", "Modern System", "Democracy", "CINC",
                                "Troops Engaged", "Opponent Troops Engaged", "Past Coups"),
          style="io",
          digits=2,
          digits.extra = 4,
          notes=("Two-tail significance levels; Standard errors clustered on country"),
          out="models5-6.txt")


# Produce Coefficient Plot 
model5Frame <- data.frame(Variable = rownames(summary(m5)$coef),
                          Coefficient = summary(m5)$coef[, 1],
                          SE = diag(clusterSEm5),
                          modelName = "M5: IWD Wars and SubWars")


model6Frame <- data.frame(Variable = rownames(summary(m6)$coef),
                          Coefficient = summary(m6)$coef[, 1],
                          SE = diag(clusterSEm6),
                          modelName = "M6: IWD Wars")


# Combine these data.frames
allModelFrame2 <- data.frame(rbind(model5Frame, model6Frame))

# Order the variables
allModelFrame2$Variable <- factor(allModelFrame2$Variable, 
                                  levels = c("(Intercept)","bankscoups","opptroopsengaged","troopsengaged", 
                                             "cinc", "d6", "msadopt", "airparity", "airsupbin"))



# Specify the width of your confidence intervals
#interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
logit_plot2 <- ggplot(allModelFrame2, aes(colour = modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - sqrt(SE)*interval2,
                     ymax = Coefficient + sqrt(SE)*interval2),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - sqrt(SE)*interval2,
                      ymax = Coefficient + sqrt(SE)*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  scale_x_discrete(labels=c("airsupbin" = "Air Superiority", "airparity"="Air Parity", "msadopt"="Modern System",
                            "d6"="Democracy","cinc"="National Capabilities",
                            "troopsengaged"="Troops Engaged","opptroopsengaged"="Opponent Troops","bankscoups"="Past Coups",
                            "(Intercept)"="Intercept")) +
  coord_flip() + theme_bw() + 
  labs(title = "Logistic Regression Estimates, 1932-2003", subtitle = "Outcome: IWD War Win/Loss") + 
  labs(caption = "95% CIs, standard errors clustered on country")

print(logit_plot2)  # The trick to these is position_dodge().

