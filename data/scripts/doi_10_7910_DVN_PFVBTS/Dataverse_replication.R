########################################
## load packages
########################################
library(ggplot2)
library(dotwhisker)
library(tidyverse)
library(dplyr)
library(stargazer)
library(readxl)
library(interplot)

select <- dplyr::select
filter <- dplyr::filter


#####################################
## Load Data
#####################################

df <- read_excel("Governing Status or Ideology.xlsx")
rollcallvote <- readRDS("Governing Status Roll Call.rds")
load("Governing Status Ideal Point.RData")

df.rollcallvote <- rollcallvote %>% ungroup() %>%
    select(name, party.eng) %>% distinct(name, party.eng)



########################################
## Clean up the data 
########################################
df.ideal$legislator <- gsub("\\(.*", "", df.ideal$name)
df$proposer <- gsub("\\의원.*", "", df$`제안자`)

df.new <- left_join(df, df.ideal, by = c("proposer" = "legislator"))

## 20th congress only 
df1 <- df.new %>% filter(`대수` == 20)

## 김성태(비례) duplicated data removal
df <- df1 %>%
    filter(name != "김성태(비례)")


df$date <- as.Date(df$"제안일")
df <- df %>% mutate(president.party =
                        ifelse(date >= "2017-05-10" & partisanship == 1, "moon",
                        ifelse(date <"2017-05-10" & partisanship == 2, "park", "opposition"))) 

df$president.party <- as.factor(df$president.party)
df$president.party <- relevel(df$president.party, ref = "park")

## party dummy = conservative party = 1, otherwise = 0
df$partydummy <- ifelse(df$partisanship == "2", 1, 0)

## df.outsider = outsider bill only
df$opposition <- ifelse(df$president.party == "opposition",1, 0)
df.outsider <- subset(df, outsider == 1)

########################################
## Table 1
########################################
formula1 <- outsider ~	num_term + sex + ele_system + mc1d + opposition + committee
formula2 <- outsider ~	num_term + sex + ele_system + mc1d + committee*opposition
formula3 <- outsider ~	num_term + sex + ele_system + opposition + committee*mc1d
formula4 <- outsider ~	num_term + committee + sex + ele_system + opposition*mc1d
formula5 <- outsider ~	num_term + committee + sex + mc1d + opposition*ele_system
formula6 <- outsider ~	num_term + committee + sex + opposition + mc1d*ele_system

m1 <- glm(formula1, df, family = binomial(link = "probit"))
m2 <- glm(formula2, df, family = binomial(link = "probit"))
m3 <- glm(formula3, df, family = binomial(link = "probit"))
m4 <- glm(formula4, df, family = binomial(link = "probit"))
m5 <- glm(formula5, df, family = binomial(link = "probit"))
m6 <- glm(formula6, df, family = binomial(link = "probit"))

stargazer(m1, m2, m3, m4, m5, type = "html",
          dep.var.labels = c("Pro-Outsider Bills"),
          out="Table1.doc",
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          intercept.bottom = TRUE,  title="Results", align=TRUE)

###########################################################################
## Table 2. Robustness Check: Not individual partisanship but party-dummy 
###########################################################################
formula7 <- outsider ~	partydummy + opposition + committee + ele_system + num_term + sex 
formula8 <- outsider ~	num_term + sex + ele_system + partydummy + committee*opposition
formula9 <- outsider ~	num_term + sex + partydummy + committee + ele_system*opposition
formula10 <- outsider ~ opposition + committee*partydummy + ele_system + num_term + sex
formula11 <- outsider ~ opposition + committee + ele_system*partydummy + num_term + sex 

m7 <- glm(formula7, df, family = binomial(link = "probit"))
m8 <- glm(formula8, df, family = binomial(link = "probit"))
m9 <- glm(formula9, df, family = binomial(link = "probit"))
m10 <- glm(formula10, df, family = binomial(link = "probit"))
m11 <- glm(formula11, df, family = binomial(link = "probit"))

stargazer(m7, m8, m9, m10, m11, type = "html",
          dep.var.labels = c("Bills"),
          out="Table2.doc",
          intercept.bottom = TRUE,  title="Results", align=TRUE)


########################################
## Figure 1
########################################
## gg_interaction_governing_final.pdf
# Using the new formula numbering from the previous cleanup
formula2 <- outsider ~ num_term + sex + ele_system + mc1d + committee*opposition
formula4 <- outsider ~ num_term + committee + sex + ele_system + opposition*mc1d
formula5 <- outsider ~ num_term + committee + sex + mc1d + opposition*ele_system

m2 <- glm(formula2, df, family = binomial(link = "probit"))
m4 <- glm(formula4, df, family = binomial(link = "probit"))
m5 <- glm(formula5, df, family = binomial(link = "probit"))

p1 <- interplot(m = m2, var1 = "committee", var2 ="opposition") +
  xlab("Governing Status (Governing Party 0, Opposition Party 1)") + 
  ylab("Estimated Coefficient for CEL Members") +
  labs(subtitle="Governing Status with Committee Effect")

p2 <- interplot(m = m5, var1 = "ele_system", var2 = "opposition") +
  xlab("Governing Status (Governing Party 0, Opposition Party 1)") + 
  ylab("Estimated Coefficient for Electoral System") +
  labs(subtitle="Governing Status with Electoral System Effect")

pdf(file="Figure1.pdf",
    width=10, height = 4.5, family="sans")
NetworkChange:::multiplot(p1, p2, cols =2)
dev.off()


########################################
## Figure 2
########################################
## PredPlots1.pdf

df$moon <- ifelse(df$date>"2017-05-09", 1, 0)
df.moon <- subset(df, moon == 1)
df.park <- subset(df, moon == 0)
df.outsider.moon <- subset(df.moon, outsider == 1)
df.outsider.park <- subset(df.park, outsider == 1)

## predicted value with governing status and pro-outsider bills
opp.v <- seq(min(df$opposition, na.rm=T), max(df$opposition, na.rm=T), by=1)

pm1 <- predict(m4, newdata=data.frame(opposition=opp.v,
                                     num_term=mean(df.moon$num_term, na.rm=T),
                                     ele_system=mean(df.moon$ele_system, na.rm=T),
                                     sex=mean(df.moon$sex, na.rm=T),
                                     committee=mean(df.moon$committee, na.rm=T),
                                     mc1d=mean(df.moon$mc1d, na.rm=T)), 
              interval = "prediction", type = "response")

pp1 <- predict(m4, newdata=data.frame(opposition=opp.v,
                                     num_term=mean(df.park$num_term, na.rm=T),
                                     ele_system=mean(df.park$ele_system, na.rm=T),
                                     sex=mean(df.park$sex, na.rm=T),
                                     committee=mean(df.park$committee, na.rm=T),
                                     mc1d=mean(df.park$mc1d, na.rm=T)), 
              interval = "prediction", type = "response")


## predicted value with ideology and pro-outsider bills
id.v <- seq(min(df$mc1d, na.rm=T), max(df$mc1d, na.rm=T), 0.01)

pm2 <- predict(m4, newdata=data.frame(mc1d=id.v,
                                     num_term=mean(df.moon$num_term, na.rm=T),
                                     ele_system=mean(df.moon$ele_system, na.rm=T),
                                     sex=mean(df.moon$sex, na.rm=T),
                                     committee=mean(df.moon$committee, na.rm=T),
                                     opposition=mean(df.moon$opposition, na.rm=T)), 
              interval = "prediction", type = "response")

pp2 <- predict(m4, newdata=data.frame(mc1d=id.v,
                                     num_term=mean(df.park$num_term, na.rm=T),
                                     ele_system=mean(df.park$ele_system, na.rm=T),
                                     sex=mean(df.park$sex, na.rm=T),
                                     committee=mean(df.park$committee, na.rm=T),
                                     opposition=mean(df.park$opposition, na.rm=T)), 
              interval = "prediction", type = "response")


pdf(file="Figure2.pdf", width=11, height=5, family="sans")
par(mfrow=c(1,2))
plot(c(min(df$opposition, na.rm=T), max(df$opposition, na.rm=T)), c(0.35, 0.55), 
     type="n", main="Governing Status and Pro-outsider bills",
     ylab="Predicted (pro-outsider bills)",
     xlab="Governing Status", axes=FALSE)
box(); axis(2); axis(1, at=c(0, 1))
lines(pm1 ~ opp.v) 
lines(pp1 ~ opp.v, lty=2) 
legend(0.55, 0.55, c("Under Moon Government", "Under Park Government"), cex=0.7,
       lty=c(1,2))

plot(c(min(df$mc1d, na.rm=T), max(df$mc1d, na.rm=T)), c(0.35, 0.55), 
     type="n", main="Ideology and Pro-outsider bills",
     ylab="Predicted (pro-outsider bills)",
     xlab="Ideology")
lines(pm2 ~ id.v)
lines(pp2 ~ id.v, lty=2)
legend(-1.0, 0.55, c("Under Moon Government", "Under Park Government"), cex=0.7,
       lty=c(1,2))
dev.off()
