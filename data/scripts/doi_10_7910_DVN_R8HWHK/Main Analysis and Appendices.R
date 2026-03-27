library(tidyverse)
library(psych)
library(meta)
library(metafor)
library(dmetar)
library(corrplot)
library(corrr)
library(broom)

dt <- read.csv("Meta Dataset.csv", header = TRUE)

##### Computation of new variable ######
dt$recency_reverse <- 2025 - dt$Year
dt$Recency <- max(dt$recency_reverse)+min(dt$recency_reverse)-dt$recency_reverse


##### Main effect ####
summary(dt$es)
summary (m.l2 <- rma(es, vi, data=dt, method = "ML"))

summary (m.l3 <- rma.mv(es, vi, random = ~ 1 | studyID/esID, 
                        data=dt, method = "ML"))

dt_att <- dt %>% filter(Behavior==0)
dt_beh <- dt %>% filter(Behavior==1)
t.test(dt_att$es, dt_beh$es, alternative = "two.sided", var.equal = FALSE)


summary (m.l3.att <- rma.mv(es, vi, random = ~ 1 | studyID/esID, 
                        data=dt_att, method = "ML"))

summary (m.l3.beh <- rma.mv(es, vi, random = ~ 1 | studyID/esID, 
                        data=dt_beh, method = "ML"))

Chisquare_statistic = as.numeric((logLik(m.l3) - logLik(m.l2))*2)
Chisquare_statistic
Chisquare_pvalue = pchisq(Chisquare_statistic, df=1, lower.tail=FALSE)
Chisquare_pvalue

#### Univariate analysis #####
summary (model_uni.cap <- rma.mv(es, vi, mods = factor(Capability), 
                                 random = ~ 1 | studyID/esID, 
                                 data=dt, method = "ML"))
summary (model_uni.input <- rma.mv(es, vi, mods = factor(Input.info), 
                                 random = ~ 1 | studyID/esID, 
                                 data=dt, method = "ML"))
summary (model_uni.process <- rma.mv(es, vi, mods = factor(Process.info), 
                                   random = ~ 1 | studyID/esID, 
                                   data=dt, method = "ML"))
summary (model_uni.predictability <- rma.mv(es, vi, mods = factor(Predictability), 
                                     random = ~ 1 | studyID/esID, 
                                     data=dt, method = "ML"))
summary (model_uni.anthrop <- rma.mv(es, vi, mods = factor(Anthropomorphism), 
                                            random = ~ 1 | studyID/esID, 
                                            data=dt, method = "ML"))
summary (model_uni.general <- rma.mv(es, vi, mods = factor(Generalist), 
                                     random = ~ 1 | studyID/esID, 
                                     data=dt, method = "ML"))
summary (model_uni.involve <- rma.mv(es, vi, mods = factor(Human.involve), 
                                     random = ~ 1 | studyID/esID, 
                                     data=dt, method = "ML"))
summary (model_uni.role <- rma.mv(es, vi, mods = factor(Role.perform1), 
                                     random = ~ 1 | studyID/esID, 
                                     data=dt, method = "ML"))
summary (model_uni.cost <- rma.mv(es, vi, mods = factor(Cost), 
                                  random = ~ 1 | studyID/esID, 
                                  data=dt, method = "ML"))

#### Multivariate analysis #####
m.AI <- rma.mv(es, vi, 
               mods = ~ 
                 Capability+Input.info+Process.info+
                 Predictability+Anthropomorphism+
                 Generalist+Human.involve+
                 Role.perform1+Cost,
               random = ~ 1 | studyID/esID, 
               method="ML", data=dt)
summary(m.AI)

m.AI.all <- rma.mv(es, vi, 
                   mods = ~ 
                     Capability+Input.info+Process.info+
                     Predictability+Anthropomorphism+
                     Generalist+Human.involve+
                     Role.perform1+Cost+
                     # Contextual
                     Professional+Morality+
                     Privacy+Societal.risk+
                     # Population
                     Female+Age+English+Student+
                     # Method
                     Behavior+Design.within1+
                     Scenario.real1+Incentive+Recency,
                   random = ~ 1 | studyID/esID, 
                   method="ML", data=dt)

summary(m.AI.all)

###### Appendix #####
# Summary stats
length(unique(dt$esID))
length(unique(dt$studyID))
length(unique(dt$paperID))
sum(round(dt$N))

dt_descript <- dt %>% 
  dplyr::select(es, N, Capability, 
                Input.info, Process.info, Predictability,
                Anthropomorphism, Generalist,
                Human.involve, Role.perform1, Cost, 
                # Contextual
                Professional, Morality, Privacy, Societal.risk,
                # Population
                Female, Age, English, Student,
                # Method
                Behavior, Design.within1,
                Scenario.real1, Incentive, Recency)
summart_stats <- data.frame(round(dt_descript %>% psych::describe(), 3)) %>% dplyr::select(3:5, 8:9)
summart_stats

describeBy(dt$es, group=dt$Capability)
describeBy(dt$es, group=dt$Input.info)
describeBy(dt$es, group=dt$Process.info)
describeBy(dt$es, group=dt$Predictability)
describeBy(dt$es, group=dt$Anthropomorphism)
describeBy(dt$es, group=dt$Generalist)
describeBy(dt$es, group=dt$Human.involve)
describeBy(dt$es, group=dt$Role)
describeBy(dt$es, group=dt$Cost)

# Correlation matrix
temp <- dt_descript %>% select(-N, -es)
testRes = cor.mtest(temp, conf.level = 0.95)
corrplot(cor(temp), p.mat = testRes$p, type = 'upper',
         insig = 'label_sig', sig.level = c(0.01, 0.05, 0.1), 
         pch.cex = 0.9, pch.col = 'grey20', diag=FALSE)

# VIF
lm <- lm(es ~ Capability+Input.info+Process.info+
           Predictability+Anthropomorphism+
           Generalist+Human.involve+
           Role.perform1+Cost+
           # Contextual
           Professional+Morality+
           Privacy+Societal.risk+
           # Population
           Female+Age+English+Student+
           # Method
           Behavior+Design.within1+
           Scenario.real1+Incentive+Recency, data=dt)
summary(lm)
vif <- data.frame(car::vif(lm))
mean(car::vif(lm))

# Publication bias
# Rosenthal fail-safe N = 23390 way above the number below
dt$se <- sqrt(dt$vi)
fsn(yi = es, vi = vi, sei = se, data = dt, type="Rosenthal", alpha=.05,
    target, weighted=FALSE)
5*273 + 10

# p-curve
meta_analysis <- metagen(dt$es,
                         dt$se,
                         sm = "SMD",
                         method.tau = "ML")
pcurve(meta_analysis, effect.estimation = FALSE, N, dmin = 0, dmax = 1)

# Funnel plot and Egger test
meta_analysis <- metagen(dt$es,
                         dt$se,
                         sm = "SMD",
                         method.tau = "ML")
taf <- trimfill(meta_analysis)
taf
funnel(taf, legend=TRUE)

col.contour = c("gray75", "gray85", "gray95")
funnel.meta(taf, xlim = c(-2, 2),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour)
legend(x = 1, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
metabias(taf, method.bias = "linreg")
metabias(meta_analysis, method.bias = "linreg")


# Analysis with hedge's g
library(esc)
dt$g <- hedges_g(dt$es, dt$N)
dt$g_se <- NA
dt$n_c <- dt$N/2
dt$n_t <- dt$N/2
dt$g_se <- sqrt((dt$n_c+dt$n_t)/(dt$n_c*dt$n_t)+(1/2)*dt$g^2/(dt$n_c+dt$n_t))
dt$g_vi <- dt$g_se^2

summary (m.l3 <- rma.mv(g, g_vi, random = ~ 1 | studyID/esID, 
                        data=dt, method = "ML"))

m.AI.all <- rma.mv(g, g_vi, 
                   mods = ~ 
                     Capability+Input.info+Process.info+
                     Predictability+Anthropomorphism+
                     Generalist+Human.involve+
                     Role.perform1+Cost+
                     # Contextual
                     Professional+Morality+
                     Privacy+Societal.risk+
                     # Population
                     Female+Age+English+Student+
                     # Method
                     Behavior+Design.within1+
                     Scenario.real1+Incentive+Recency,
                   random = ~ 1 | studyID/esID, 
                   method="ML", data=dt)
summary(m.AI.all)

m.AI <- rma.mv(g, g_vi, 
               mods = ~ 
                 Capability+Input.info+Process.info+
                 Predictability+Anthropomorphism+
                 Generalist+Human.involve+
                 Role.perform1+Cost,
               random = ~ 1 | studyID/esID, 
               method="ML", data=dt)
summary(m.AI)

# Analysis with different model specifications
m.AI.c <- rma.mv(es, vi, 
                 mods = ~ Capability+Input.info+Process.info+
                   Predictability+Anthropomorphism+
                   Generalist+Human.involve+
                   Role.perform1+Cost+
                   # Contextual
                   Professional+Morality+
                   Privacy+Societal.risk,
                 random = ~ 1 | studyID/esID, 
                 method="ML", data=dt)

summary(m.AI.c)

m.AI.p <- rma.mv(es, vi, 
                 mods = ~ Capability+Input.info+Process.info+
                   Predictability+Anthropomorphism+
                   Generalist+Human.involve+
                   Role.perform1+Cost+
                   # Population
                   Female+Age+English+Student,
                   random = ~ 1 | studyID/esID, 
                 method="ML", data=dt)

summary(m.AI.p)

m.AI.om <- rma.mv(es, vi, 
                 mods = ~ Capability+Input.info+Process.info+
                   Predictability+Anthropomorphism+
                   Generalist+Human.involve+
                   Role.perform1+Cost+
                   # Contextual
                   Professional+Morality+
                   Privacy+Societal.risk+
                   # Population
                   Female+Age+English+Student,
                 random = ~ 1 | studyID/esID, 
                 method="ML", data=dt)

summary(m.AI.om)

# Sensitivity test
m.AI.sen <- rma.mv(es, vi, 
                  mods = ~ Capability+Input.info+#Process.info+
                    Predictability+#Anthropomorphism+
                    Generalist+Human.involve+
                    Role.perform1+#Cost+
                    # Contextual
                    Professional+Morality+
                    Privacy+Societal.risk+
                    # Population
                    Female+Age+English+Student+
                    # Method
                    Behavior+Design.within1+
                    Scenario.real1+Incentive+Recency,
                  random = ~ 1 | studyID/esID, 
                  method="ML", data=dt)

summary(m.AI.sen)

###### Context analysis #####
professional <- dt %>% filter(Professional== 1)
consumer <- dt %>% filter(Professional== 0)

summary (m.l3 <- rma.mv(es, vi, random = ~ 1 | studyID/esID, 
                        data=professional, method = "ML"))
summary (m.l3 <- rma.mv(es, vi, random = ~ 1 | studyID/esID, 
                        data=consumer, method = "ML"))

m.AI <- rma.mv(es, vi, 
               mods = ~ 
                 Capability+Input.info+Process.info+
                 Predictability+Anthropomorphism+
                 Generalist+Human.involve+
                 Role.perform1+Cost,
               random = ~ 1 | studyID/esID, 
               method="ML", data=professional)
summary(m.AI)

m.AI <- rma.mv(es, vi, 
               mods = ~ 
                 Capability+Input.info+Process.info+
                 Predictability+Anthropomorphism+
                 Generalist+Human.involve+
                 Role.perform1+Cost,
               random = ~ 1 | studyID/esID, 
               method="ML", data=consumer)
summary(m.AI)

