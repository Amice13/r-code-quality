############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code analyses the data from MTurk        #
#       for Studies 6 and 7                     #
#                                               #
# 2020.06.24.                                   #
# code by Alexander Bor                         #
############################################### #

# load packages
library(rio)
library(stargazer)
library(psych)
library(lme4)
library(tidyverse)
library(gridExtra)
library(merTools)

############################################################################### #
# Study 6                                                               #########
############################################################################### #



source(file = "C.code/import_s6_s7_MTurk.R")

# ~ Selection hypothesis ########################################################


# import data 

df4 <- readRDS(file = "B.analysis.data/s6_exp.rds") %>%
		filter(attentive != "not") 

# run models 

fit1.0 <- lmer(select_pol ~ 1 + sdrt + age + female + partyid + highered + 
                       (1 | position) + 
                       (1 | target),
               df4)

fit1.1 <- lmer(select_pol ~ 1 + sdrt + age + female + partyid + highered + 
                       (1 | position) + 
                       (1 + sdrt | target),
               df4)

anova(fit1.0, fit1.1)

# bootstrap confidence interval for 
set.seed(13012)
arm.fit1 <- arm::sim(fit1.0, 1e3)
preds <- fitted(arm.fit1, fit1.0)

group1 <- attributes(fit1.0)$frame$target == 1
group8 <- attributes(fit1.0)$frame$target == 8

group1.m <- colMeans(preds[group1, ])
group8.m <- colMeans(preds[group8, ])

round(mean(group1.m-group8.m), 2)
round(quantile(group1.m-group8.m, c(0.025, 0.975)), 2)


# fit1.2 <- lmer(select_pol ~ 1 + sdrt + age + female + partyid + highered + 
#                        (1 | position) + 
#                        (1 + sdrt | as.target),
#                df4)

fit2.0 <- lmer(select_pol ~ 1 + aggression + age + female + partyid + highered + 
                       (1 | position) + 
                       (1 | target),
               df4)

fit2.1 <- lmer(select_pol ~ 1 + aggression + age + female + partyid + highered + 
                       (1 | position) + 
                       (1 + aggression | target),
               df4)
anova(fit2.0, fit2.1)

stargazer(fit1.0, fit2.0, 
          digits = 2, 
          title = "Table G2. Multilevel logistic regression on selecting political post",
          out = "D.documents/tables/tg2_selecting_pol.html",
          dep.var.labels = "Selecting political post", 
          covariate.labels = c("Status drive", "Aggression","Age", "Female", 
                               "Party ID", "Higher education"), 
          single.row = T,
          report = "vcs*",
          type = "text")

fit1.0.coefs <- bind_cols(
        Model1 = rbind(coef(fit1.0)$target[1], coef(fit1.0)$position[1]), 
        Model2 =rbind(coef(fit2.0)$target[1], coef(fit2.0)$position[1])) %>% 
        mutate(random.efs = c("Hostility", rep(" ", 7), "Position", " "), 
               levels = row.names(.)) %>% 
        rename(Model1 = "(Intercept)...1", Model2 = "(Intercept)...2") %>%
        dplyr::select(random.efs, levels, Model1, Model2) %>% 
        stargazer(summary = F, type = "text", 
                  out = "D.documents/tables/tg3_selecting_pol_randomefs.html",
                  title = "Table G3. Varying intercepts for the models in Table G2")


newdat3 <- data.frame(expand.grid(
        # sdrt = quantile(df4$sdrt, c(0.25, 0.75), names = FALSE), 
        sdrt = quantile(df4$sdrt, c(0.025, 0.975), names = FALSE), 
        position = "anti",
        age = mean(df4$age), 
        female = mean(df4$female, na.rm = T), 
        partyid = mean(df4$partyid), 
        highered = mean(df4$highered), 
        target = 1:8))

pred3 <- cbind(newdat3, 
               predictInterval(fit1.0, newdata = newdat3, 
                               which = "full", level = 0.95,
                               include.resid.var = FALSE)) 

ggplot(pred3, aes(x = target, y = fit, colour = as.factor(sdrt))) + 
        geom_point(position = position_dodge(0.9)) +
        geom_errorbar(aes(ymin = lwr,
                          ymax = upr), 
                      width = 0,
                      position = position_dodge(0.9)) +
        xlab("Target hostility") + 
        ylab("% picking politics") + 
        scale_x_continuous(breaks = 1:8) + 
        # scale_fill_grey(name = "SDRT", labels = c("1st quartile", "3rd quartile")) + 
        scale_color_grey(name = "SDRT", 
                        labels = c("Low (2.5% quantile)", "High (97.5% quantile)"), 
                        start = 0.8, end = 0.2) + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 1))+ 
        # facet_grid(~position) + 
        theme_minimal() + theme(legend.position = "bottom")

ggsave(file = "D.documents/figures/fig_g1_selectpol.jpg",
       width = 6, height = 4)

# Validations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df4_pol <- readRDS(file = "B.analysis.data/s6_crowd.rds")


fit2.1 <- lmer(crowd.mean ~ 1 + sdrt + age + female + partyid + highered + 
                       (1 | target ) + 
                       (1 | position), 
               df4_pol)

fit2.2 <- lmer(crowd.mean ~ 1 + ders + age + female + partyid + highered + 
                       (1 | target ) + 
                       (1 | position), 
               df4_pol)

fit2.3 <- lmer(crowd.mean ~ 1 + aggression + age + female + partyid + highered + 
                       (1 | target ) + 
                       (1 | position), 
               df4_pol)

fit2.4 <- lmer(crowd.mean ~ 1 + hostile + age + female + partyid + highered + 
                       (1 | target ) + 
                       (1 | position), 
               df4_pol)


stargazer(fit2.1, fit2.2, fit2.3, fit2.4, 
          title = "Table G1. Study 4 - Regressing hostility of comment on personality measures",
          covariate.labels = c("Status drive", "Emotion regulation", "Trait aggression", 
                               "Self-reported hostility", "Age", "Female", 
                               "Party ID", "Higher educated"),
          dep.var.labels = "Hostility of comment", 
          out = "D.documents/tables/tg1_hostile_psych.html",
          single.row = T,
          digits  =2,
          report = "vcs*",
          type = "text")


############################################################################### #
# Study 7                                                              #########
############################################################################### #

# load data 

df5 <- readRDS("B.analysis.data/s7_crowd.rds")

# ~ overall matching ######################################################

# relying on study 5 ratings
t.test(df5$diff.s5)
sum(df5$diff.s5 > 0) / nrow(df5)

# relying on ratings from study 4 (appendix g)
t.test(df5$diff.s4)



# compare ratings across the two studies

target.ratings <- readRDS("B.analysis.data/s7_targetratings.rds")
cor.test(target.ratings$target.mean.s4, target.ratings$target.mean.s5)

# ~ tone by personality ######################################################

fit4.1 <- lmer(crowd.mean ~ sdrt + age+ female + 
                        partyid + highered +
                        (1 | position) + 
                        (1| target), 
                df5)

fit4.2 <- lmer(crowd.mean ~ sdrt + age+ female + 
                        partyid + highered +
                        (1 | position) + 
                        (1 + sdrt | target), 
                df5)
anova(fit4.1, fit4.2)


fit4.4 <- lmer(crowd.mean ~ ders + age+ female + 
                        partyid + highered +
                        (1 | position) + 
                        (1| target), 
                df5)

fit4.5 <- lmer(crowd.mean ~ ders + age+ female +
                        partyid + highered +
                        (1 | position) +
                        (1 + ders| target),
                df5)
anova(fit4.4, fit4.5)

stargazer(fit4.1, fit4.4, type = "text", 
          title = "Table G4. Regressing tone matched comments' hostility on SDRT and DERS",
          out = "D.documents/tables/tg4_matched_hostility_psych.html",
          dep.var.labels = "Response hostility", 
          digits = 2,
          single.row = T,
          report = "vcs*",
          covariate.labels = c("SDRT", "DERS", "Age", "Female", 
                               "Party ID", "Higher education"))



# Create fig 6 in manuscript ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newdat4 <- data.frame(expand.grid(
        sdrt = quantile(df5$sdrt, c(0.025, 0.975), names = FALSE), 
        position = "anti", 
        age = mean(df5$age), 
        female = mean(df5$female, na.rm = T), 
        partyid = mean(df5$partyid), 
        highered = mean(df5$highered), 
        target = 1:8))

pred4 <- cbind(newdat4, 
               predictInterval(fit4.1, newdata = newdat4, 
                               which = "full", level = 0.95,
                               include.resid.var = FALSE))  
pred4$level <- rep(c("Low", "High"), 8)

ggplot(pred4,
       aes(x = target, y = fit, color = as.factor(level))) + 
        # geom_col(position = position_dodge(0.9)) +
        geom_point(position = position_dodge(0.9)) +
        # geom_smooth(method = "lm") + 
        geom_errorbar(aes(ymin = lwr,
                          ymax = upr), 
                      width = 0,
                      position = position_dodge(0.9)) +
        xlab("Target hostility") + 
        ylab("Mean hostility of response") + 
        scale_x_continuous(breaks = 1:8) + 
        scale_color_grey(name = "SDRT", 
                         labels = c("Low (2.5% quantile)", "High (97.5% quantile)"), 
                        start = 0.8, end = 0.2) + 
        # facet_grid(~ID) +
        theme_minimal() +
        theme(legend.position="bottom")

ggsave(file = "D.documents/figures/fig_g2_hostility_sdrt.jpg", 
       width = 5, height = 3.5)

# Change interaction #############################################################

fit5.1 <-  lmer(crowd.mean ~ I(hostile/100)*ders + age+ female + 
                       partyid + highered +
                       (1 | position) + 
                       (1 | target), df5)
stargazer(fit5.1,
          covariate.labels = c("Perceived target hostility", "Emotion regulation", 
                               "Age", "Female", "Party ID", "Higher education", 
                               "P. t. Hostility × Emotion regulation"), 
          dep.var.labels = "Response hostility", 
          digits = 2,
          single.row = T,
          report = "vcs*",
          title = "Table G5. Relationship between subjective hostility of the target and response hostility",
          out = "D.documents/tables/tg5_hostility_calibration_by_ders.html",
          type = "text")

binned.cors <- df5 %>% 
        mutate(DERS_level = case_when(ders <= quantile(df5$ders, 0.33) ~ "low", 
                                      ders > quantile(df5$ders, 0.33) & 
                                              ders <= quantile(df5$ders, 0.66) ~ "medium", 
                                      TRUE ~ "high"), 
               DERS_level = fct_relevel(DERS_level, "low", "medium")) %>% 
        group_by(DERS_level) %>% 
        summarise(cor =     round(cor(crowd.mean, hostile), 2), 
                  cor.lwr = round(cor.test(crowd.mean, hostile)$conf.int[1], 2), 
                  cor.upr = round(cor.test(crowd.mean, hostile)$conf.int[2], 2))

binned.cors

# encoding and recall ##########################################################

cor.test(df5$hostile, df5$hostile_retrieve)


# Crowdsourcing stats ##########################################################

# participants' ratings 
(nrow(df4_pol) + nrow(df5))/16

range(table(paste0(df4_pol$position, df4_pol$target)) + 
              table(paste0(df5$position, df5$target)))

# rating the comments from Study 3 
mean(df4_pol$crowd.n, na.rm = T)
range(df4_pol$crowd.n, na.rm = T)
sum(df4_pol$crowd.n >= 5)   / nrow(df4_pol)

mean(df5$crowd.n, na.rm = T)
range(df5$crowd.n, na.rm = T)
sum(df5$crowd.n >= 5) / nrow(df5)
