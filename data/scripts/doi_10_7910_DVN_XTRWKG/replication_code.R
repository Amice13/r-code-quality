#### INITIALIZATION ####

# packages for data cleaning and wrangling
library(dplyr)
library(stringr)

# packages for analyses
library(cjoint)
library(multiwayvcov)
library(ri)

# packages for presenting the results
library(stargazer)
library(kableExtra)


## User: specify working directory here
setwd("")

load("TsaiTrinhLiu_Replication.RData")
source("helper_functions.R")


#### CONJOINT RESULTS, STUDY 1 AND 2 ####
## Text on Page 6 ##
fujian_amce_4attr <- amce.sub(data = fujian, 
                              id = "resp_id", 
                              title = "Study 1 Conjoint Analysis Results", 
                              outcome = "vote", outcome_lab = "Profile is preferred",
                              variables = c("punishment", "growth", "dibao", "election"),
                              labels = c("Punishment", "Growth", "Welfare", "Election"),
                              path = "../")
fujian_amce_4attr_se <- sqrt(diag(vcov(fujian_amce_4attr)))
names(fujian_amce_4attr_se) <- str_replace_all(names(fujian_amce_4attr_se), "1", "")

beijing_amce_4attr <- amce.sub(data = beijing %>% filter(exp_id_joint == 1), 
                               id = "resp_id", 
                               title = "Study 2 Conjoint Analysis Results (Round 6 Only)", 
                               outcome = "vote", outcome_lab = "Profile is preferred",
                               variables = c("punishment", "growth", "dibao", "election"),
                               labels = c("Punishment", "Growth", "Welfare", "Election"),
                               path = "../")
beijing_amce_4attr_se <- sqrt(diag(vcov(beijing_amce_4attr)))
names(beijing_amce_4attr_se) <- str_replace_all(names(beijing_amce_4attr_se), "1", "")

stargazer(list(lm(vote ~ punishment + election + dibao + growth, 
                  data = fujian),
               lm(vote ~ punishment + election + dibao + growth, 
                  data = beijing %>% filter(exp_id_joint == 1))),
          se = list(fujian_amce_4attr_se,
                    beijing_amce_4attr_se),
          sqrt(diag(vcov(fujian_amce_4attr))),
          covariate.labels = c("Punishment", "Election", "Welfare", "Growth", "Intercept"),
          column.labels = c("Study 1", "Study 2 (4-attribute round)"),
          dep.var.caption = "Conjoint Preference, Study 1 and 2",
          dep.var.labels.include = FALSE,
          keep.stat = "n",
          notes = "Clustered Standard Errors in Parentheses",
          type = "text")

#### STUDY 2 MEDIATION ANALYSIS RESULTS ####
## Table 1 ##
set.seed(12345)
# Column 1
punishment_joint_bj <- med.para.ri(outcome="vote",
                                   treatment="punishment",
                                   mediator=c("moral", "competence"),
                                   covs=c("dibao", "election", "growth"),
                                   exp.id="exp_id_joint",
                                   data=beijing,
                                   niter=2000)
punishment_joint_bj

# Column 2
punishment_moral_nat_bj <- med.para.ri(outcome="vote",
                                       treatment="punishment",
                                       mediator="moral",
                                       covs=c("dibao", "election", "growth"),
                                       exp.id="exp_id_moral_nat",
                                       data=beijing,
                                       niter=2000)
punishment_moral_nat_bj

# Column 3
punishment_moral_con_bj <- med.para.ri(outcome="vote",
                                       treatment="punishment",
                                       mediator="moral",
                                       covs=c("dibao", "election", "growth", "competence"),
                                       exp.id="exp_id_moral_con",
                                       data=beijing,
                                       niter=2000)
punishment_moral_con_bj

# Column 4
punishment_competence_nat_bj <- med.para.ri(outcome="vote",
                                            treatment="punishment",
                                            mediator="competence",
                                            covs=c("dibao", "election", "growth"),
                                            exp.id="exp_id_competence_nat",
                                            data=beijing,
                                            niter=2000)
punishment_competence_nat_bj

# Column 5
punishment_competence_con_bj <- med.para.ri(outcome="vote",
                                            treatment="punishment",
                                            mediator="competence",
                                            covs=c("dibao", "election", "growth", "moral"),
                                            exp.id="exp_id_competence_con",
                                            data=beijing,
                                            niter=2000)
punishment_competence_con_bj

# Put it all together
table1 <- sapply(list(punishment_joint_bj,
                      punishment_moral_nat_bj,
                      punishment_moral_con_bj,
                      punishment_competence_nat_bj,
                      punishment_competence_con_bj),
                 function(x) {
                   rbind(x$indirect,
                         x$indirect.greater,
                         x$total,
                         x$total.greater)
                 }) %>% 
  formatC(digits = 3, format = "f")

colnames(table1) <- c("combined", "moral - natural", "moral - controlled", "competence - natural", "competence - controlled")
rownames(table1) <- c("ind_est", "ind_se", "total_est", "total_se")
table1

write.csv(table1, file = "../table1.csv")


#### STUDY 1 DEMOGRAPHICS VS. FUJIAN CENSUS ####
## Table A1 ##

# Fujian sample stats from the data
fujian_stats <- fujian %>% 
  distinct(resp_id, .keep_all = TRUE) %>% 
  select(age_cat, gender, education_cat, consume_ind) %>%
  summarise(mean_age_cat = mean(age_cat == ">= 40", na.rm = TRUE),
            mean_female = mean(gender == "Female", na.rm = TRUE),
            mean_education_cat = mean(education_cat == "High school and above", na.rm = TRUE),
            mean_consume = round(mean(consume_ind, na.rm = TRUE), digits = 1),
            n_all = round(n(), digits = 0)) %>%
  mutate_at(vars(mean_age_cat:mean_education_cat), round, digits = 2) %>%
  as.character %>%
  as.matrix 

# Fujian census stats are hardcoded directly from Fujian Statistical Yearbook 2007
fujian_stats <- cbind(fujian_stats,
                      c(.46, .49, .29, 1075.9, NA))

write.csv(fujian_stats, file = "../tableA1.csv")

#### STUDY 1-3 DEMOGRAPHICS ####

## Table A2 ##

summary_stats <- bind_rows(fujian %>% 
                             distinct(resp_id, .keep_all = TRUE) %>% 
                             select(age, age_cat, gender, education_cat, party, consume_hhold) %>%
                             mutate(study = "Study 1"),
                           beijing %>% 
                             distinct(resp_id, .keep_all = TRUE) %>% 
                             select(age, age_cat, gender, education_cat, party, consume_hhold) %>%
                             mutate(study = "Study 2"),
                           online %>% 
                             distinct(resp_id, .keep_all = TRUE) %>% 
                             select(age, age_cat, gender, education_cat, party, consume_hhold) %>%
                             mutate(study = "Study 3")) %>%
  group_by(study) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            n_age = sum(!is.na(age)),
            sd_age = sd(age, na.rm = TRUE),
            mean_age_cat = mean(age_cat == ">= 40", na.rm = TRUE),
            n_age_cat = sum(!is.na(age_cat)),
            sd_age_cat = sqrt(mean_age_cat * (1-mean_age_cat)),
            mean_female = mean(gender == "Female", na.rm = TRUE),
            n_female = sum(!is.na(gender)),
            sd_female = sqrt(mean_female * (1-mean_female)),
            mean_education_cat = mean(education_cat == "High school and above", na.rm = TRUE),
            n_education_cat = sum(!is.na(education_cat)),
            sd_education_cat = sqrt(mean_education_cat * (1-mean_education_cat)),
            mean_party = mean(party == "1 - CCP Member" | party == "1 - Yes", na.rm = TRUE),
            n_party = sum(!is.na(party)),
            sd_party = sqrt(mean_party * (1-mean_party)),
            mean_consume = mean(consume_hhold, na.rm = TRUE),
            n_consume = sum(!is.na(consume_hhold)),
            sd_consume = sd(consume_hhold, na.rm = TRUE),
            n_all = n()) %>%
  mutate_each(list(~round(.,2)), -study) %>%
  mutate(sd_age = paste0("(sd = ", sd_age, "; ", "n = ", n_age, ")"),
         sd_age_cat = paste0("(sd = ", sd_age_cat, "; ", "n = ", n_age_cat, ")"),
         sd_female = paste0("(sd = ", sd_female, "; ", "n = ", n_female, ")"),
         sd_education_cat = paste0("(sd = ", sd_education_cat, "; ", "n = ", n_education_cat, ")"),
         sd_party = paste0("(sd = ", sd_party, "; ", "n = ", n_party, ")"),
         sd_consume = paste0("(sd = ", sd_consume, "; ", "n = ", n_consume, ")")) %>%
  select(study, 
         mean_age, sd_age, mean_age_cat, sd_age_cat, mean_female, sd_female,
         mean_education_cat, sd_education_cat, mean_party, sd_party, mean_consume, sd_consume, 
         n_all) %>%
  as.matrix %>%
  t

write.csv(summary_stats, file = "../tableA2.csv")


#### VERIFY NO INTERACTION ASSUMPTION ####
## Figure C4 ##

# Study 2
fit.beijing.cte <- lm(vote ~ punishment + punishment * moral + punishment * competence + punishment * moral * competence, 
                      data = subset(beijing, round==1))
point.est.m0c0 <- coef(fit.beijing.cte)["punishment"]
se.est.m0c0 <- sqrt(vcov(fit.beijing.cte)["punishment","punishment"])
point.est.m0c1 <- coef(fit.beijing.cte)["punishment"] + coef(fit.beijing.cte)["punishment:competence"]
se.est.m0c1 <- sqrt(c(1,1) %*% vcov(fit.beijing.cte)[c("punishment", "punishment:competence") ,c("punishment", "punishment:competence")] %*% c(1,1))
point.est.m1c0 <- coef(fit.beijing.cte)["punishment"] + coef(fit.beijing.cte)["punishment:moral"]
se.est.m1c0 <- sqrt(c(1,1) %*% vcov(fit.beijing.cte)[c("punishment", "punishment:moral") ,c("punishment", "punishment:moral")] %*% c(1,1))
point.est.m1c1 <- coef(fit.beijing.cte)["punishment"] + coef(fit.beijing.cte)["punishment:competence"] + coef(fit.beijing.cte)["punishment:moral"] + coef(fit.beijing.cte)["punishment:moral:competence"]
se.est.m1c1 <- sqrt(c(1,1,1,1) %*% vcov(fit.beijing.cte)[c("punishment", "punishment:competence", "punishment:moral", "punishment:moral:competence"),
                                                         c("punishment", "punishment:competence", "punishment:moral", "punishment:moral:competence")] %*% c(1,1,1,1))

plot.beijing.int.df <- data.frame(matrix(c(point.est.m0c0, se.est.m0c0,
                                           point.est.m0c1, se.est.m0c1,
                                           point.est.m1c0, se.est.m1c0,
                                           point.est.m1c1, se.est.m1c1), ncol=2, byrow=TRUE))
colnames(plot.beijing.int.df) <- c("mean", "se")
plot.beijing.int.df$group <- c("EY(Punishment=1, Moral=0, Competence=0) \n - EY(Punishment=0, Moral=0, Competence=0)",
                               "EY(Punishment=1, Moral=0, Competence=1) \n - EY(Punishment=0, Moral=0, Competence=1)",
                               "EY(Punishment=1, Moral=1, Competence=0) \n - EY(Punishment=0, Moral=1, Competence=0)",
                               "EY(Punishment=1, Moral=1, Competence=1) \n - EY(Punishment=0, Moral=1, Competence=1)")

plot.beijing.int.df$lower <- plot.beijing.int.df$mean - 1.96*plot.beijing.int.df$se
plot.beijing.int.df$upper <- plot.beijing.int.df$mean + 1.96*plot.beijing.int.df$se

plot.beijing.int <- ggplot(plot.beijing.int.df, aes(y = mean, x = group)) +
  geom_point(position=position_dodge(.5), size=3) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dotdash") +
  geom_errorbar(data=plot.beijing.int.df, aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(.5), size=1) +
  theme_bw() +
  theme(text = element_text(size=20), plot.title=element_text(size=20, hjust=0.5), legend.position = "none") +
  labs(title= "Assesing Validity of No Interaction Effect Assumption - Study 2") +
  xlab("") +
  #ylim(min(-0.03, 1.1*min(estimate.plot$lower)), max(0.03, 1.1*max(estimate.plot$upper))) +
  ylim(-.03, .4) +
  ylab("AMCE: Change in Pr(Profile is preferred)")

ggsave(plot.beijing.int, filename="../fgC4a.eps", width=18, height=6)

# Study 3
fit.online.cte <- lm(vote ~ punishment + punishment * moral + punishment * competence + punishment * moral * competence, data = subset(online, round==1))

point.est.m0c0 <- coef(fit.online.cte)["punishment"]
se.est.m0c0 <- sqrt(vcov(fit.online.cte)["punishment","punishment"])
point.est.m0c1 <- coef(fit.online.cte)["punishment"] + coef(fit.online.cte)["punishment:competence"]
se.est.m0c1 <- sqrt(c(1,1) %*% vcov(fit.online.cte)[c("punishment", "punishment:competence") ,c("punishment", "punishment:competence")] %*% c(1,1))
point.est.m1c0 <- coef(fit.online.cte)["punishment"] + coef(fit.online.cte)["punishment:moral"]
se.est.m1c0 <- sqrt(c(1,1) %*% vcov(fit.online.cte)[c("punishment", "punishment:moral") ,c("punishment", "punishment:moral")] %*% c(1,1))
point.est.m1c1 <- coef(fit.online.cte)["punishment"] + coef(fit.online.cte)["punishment:competence"] + coef(fit.online.cte)["punishment:moral"] + coef(fit.online.cte)["punishment:moral:competence"]
se.est.m1c1 <- sqrt(c(1,1,1,1) %*% vcov(fit.online.cte)[c("punishment", "punishment:competence", "punishment:moral", "punishment:moral:competence"),
                                                        c("punishment", "punishment:competence", "punishment:moral", "punishment:moral:competence")] %*% c(1,1,1,1))

plot.online.int.df <- data.frame(matrix(c(point.est.m0c0, se.est.m0c0,
                                          point.est.m0c1, se.est.m0c1,
                                          point.est.m1c0, se.est.m1c0,
                                          point.est.m1c1, se.est.m1c1), ncol=2, byrow=TRUE))
colnames(plot.online.int.df) <- c("mean", "se")
plot.online.int.df$group <- c("EY(Punishment=1, Moral=0, Competence=0) \n - EY(Punishment=0, Moral=0, Competence=0)",
                              "EY(Punishment=1, Moral=0, Competence=1) \n - EY(Punishment=0, Moral=0, Competence=1)",
                              "EY(Punishment=1, Moral=1, Competence=0) \n - EY(Punishment=0, Moral=1, Competence=0)",
                              "EY(Punishment=1, Moral=1, Competence=1) \n - EY(Punishment=0, Moral=1, Competence=1)")

plot.online.int.df$lower <- plot.online.int.df$mean - 1.96*plot.online.int.df$se
plot.online.int.df$upper <- plot.online.int.df$mean + 1.96*plot.online.int.df$se

plot.online.int <- ggplot(plot.online.int.df, aes(y = mean, x = group)) +
  geom_point(position=position_dodge(.5), size=3) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dotdash") +
  geom_errorbar(data=plot.online.int.df, aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(.5), size=1) +
  theme_bw() +
  theme(text = element_text(size=20), plot.title=element_text(size=20, hjust=0.5), legend.position = "none") +
  labs(title= "Assesing Validity of No Interaction Effect Assumption - Study 3") +
  xlab("") +
  #ylim(min(-0.03, 1.1*min(estimate.plot$lower)), max(0.03, 1.1*max(estimate.plot$upper))) +
  ylim(-.03, .4) +
  ylab("AMCE: Change in Pr(Profile is preferred)")

ggsave(plot.online.int, filename="../fgC4b.eps", width=18, height=6)


#### ROBUSTNESS OF AMCE FOR PUNISHMENT ####
## Figure D1 ##

# Fit all models
fujian_amce_4attr <- amce.sub(data = fujian, 
                              id = "resp_id", 
                              title = "Study 1 Conjoint Analysis Results", 
                              outcome = "vote", outcome_lab = "Profile is preferred",
                              variables = c("punishment", "growth", "dibao", "election"),
                              labels = c("Punishment", "Growth", "Welfare", "Election"),
                              path = "../")
fujian_amce_4attr_se <- sqrt(diag(vcov(fujian_amce_4attr)))
names(fujian_amce_4attr_se) <- str_replace_all(names(fujian_amce_4attr_se), "1", "")

beijing_amce_4attr <- amce.sub(data = beijing %>% filter(exp_id_joint == 1), 
                               id = "resp_id", 
                               title = "Study 2 Conjoint Analysis Results (Round 6 Only)", 
                               outcome = "vote", outcome_lab = "Profile is preferred",
                               variables = c("punishment", "growth", "dibao", "election"),
                               labels = c("Punishment", "Growth", "Welfare", "Election"),
                               path = "../")
beijing_amce_4attr_se <- sqrt(diag(vcov(beijing_amce_4attr)))
names(beijing_amce_4attr_se) <- str_replace_all(names(beijing_amce_4attr_se), "1", "")

beijing_amce_6attr <- amce.sub(data = beijing, 
                               id = "resp_id",
                               title = "Study 2 Conjoint Analysis Results (Round 1 Only)", 
                               outcome = "vote", outcome_lab = "Profile is preferred",
                               variables = c("punishment", "growth", "dibao", "election", "moral", "competence"),
                               labels = c("Punishment", "Growth", "Welfare", "Election", "Moral", "Competence"),
                               path = "../")
beijing_amce_6attr_se <- sqrt(diag(vcov(beijing_amce_6attr)))
names(beijing_amce_6attr_se) <- str_replace_all(names(beijing_amce_6attr_se), "1", "")

online_amce_4attr <- amce.sub(data = online %>% filter(exp_id_joint == 1), 
                              id = "resp_id", 
                              title = "Study 3 Conjoint Analysis Results (Round 6 Only)", 
                              outcome = "vote", outcome_lab = "Profile is preferred",
                              variables = c("punishment", "growth", "dibao", "election"),
                              labels = c("Punishment", "Growth", "Welfare", "Election"),
                              path = "../")
online_amce_4attr_se <- sqrt(diag(vcov(online_amce_4attr)))
names(online_amce_4attr_se) <- str_replace_all(names(online_amce_4attr_se), "1", "")

online_amce_6attr <- amce.sub(data = online, 
                              id = "resp_id", 
                              title = "Study 3 Conjoint Analysis Results (Round 1 Only)", 
                              outcome = "vote", outcome_lab = "Profile is preferred",
                              variables = c("punishment", "growth", "dibao", "election", "moral", "competence"),
                              labels = c("Punishment", "Growth", "Welfare", "Election", "Moral", "Competence"),
                              path = "../")
online_amce_6attr_se <- sqrt(diag(vcov(online_amce_6attr)))
names(online_amce_6attr_se) <- str_replace_all(names(online_amce_6attr_se), "1", "")


## coefficient plots for summaries
estimates <- lapply(list(fujian_amce_4attr,
                         beijing_amce_4attr,
                         online_amce_4attr,
                         beijing_amce_6attr,
                         online_amce_6attr),
                    function(object) {
                      data.frame(AMCE = c(coef(object)),
                                 Std..Error = c(sqrt(diag(vcov(object)))[-1]),
                                 var.labels = names(coef(object)))
                    })
names(estimates) <- c("Study 1", 
                      "Study 2 (4-attribute round)", "Study 3 (4-attribute round)",
                      "Study 2 (6-attribute round)", "Study 3 (6-attribute round)")

estimates.plot <- bind_rows(estimates, .id = "Sample")
estimates.plot$lower <- estimates.plot$AMCE-1.96 * estimates.plot$Std..Error
estimates.plot$upper <- estimates.plot$AMCE+1.96 * estimates.plot$Std..Error
estimates.plot$var.labels <- factor(estimates.plot$var.labels,
                                    levels = rev(c("punishment", "growth", "election", "dibao", "moral", "competence")))
levels(estimates.plot$var.labels) <- rev(c("Punishment", "Election", "Welfare", "Growth", "Moral", "Competence"))
estimates.plot$Sample <- factor(estimates.plot$Sample,
                                levels = rev(names(estimates)))


ggplot(estimates.plot[1:12,], aes(y = AMCE, x = var.labels, group = Sample, shape = Sample)) +
  geom_point(position = position_dodge(.5), size = 3) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dotdash") +
  geom_errorbar(data=estimates.plot[1:12,], aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(.5)) +
  scale_shape_manual(values = c(16, 17, 15),
                     breaks = c("Study 1", "Study 2 (4-attribute round)", "Study 3 (4-attribute round)")) +
  labs(title= "Study 1-3 Conjoint Analysis Results, 4-attribute Rounds",
       shape = "") +
  theme_bw() +
  theme(text = element_text(size=20), 
        plot.title=element_text(size=20, hjust=0.5),
        legend.position = "bottom") +
  xlab("") +
  ylim(-0.03,0.4) +
  ylab("Change in Pr(Profile is Preferred)")
ggsave("../fgD1a.eps", width = 10, height = 6, units = "in")

ggplot(estimates.plot[13:24,], aes(y = AMCE ,x = var.labels, group = Sample, shape = Sample)) +
  geom_point(position=position_dodge(.75), size = 3) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dotdash") +
  geom_errorbar(data=estimates.plot[13:24,], aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(.75)) +
  theme(text = element_text(size=20)) +
  scale_shape_manual(values = c(17, 15),
                     breaks = c("Study 2 (6-attribute round)", 
                                "Study 3 (6-attribute round)")) +
  labs(title= "Study 2-3 Conjoint Analysis Results, 6-attribute Rounds",
       shape = "") +
  theme_bw() +
  theme(text = element_text(size=20), 
        plot.title=element_text(size=20, hjust=0.5),
        legend.position = "bottom") +
  xlab("") +
  ylim(-0.03,0.4) +
  ylab("Change in Pr(Profile is Preferred)")
ggsave("../fgD1b.eps", width = 10, height = 6, units = "in")

#### VARIATION IN AMCEs OF OTHER ATTRIBUTES ####
## Table D1 ##

stargazer(list(lm(vote ~ punishment + growth + election + dibao, data = fujian),
               lm(vote ~ punishment + growth + election + dibao, data = beijing %>% filter(exp_id_joint == 1)),
               lm(vote ~ punishment + growth + election + dibao + moral + competence, data = beijing),
               lm(vote ~ punishment + growth + election + dibao, data = online %>% filter(exp_id_joint == 1)),
               lm(vote ~ punishment + growth + election + dibao + moral + competence, data = online)),
          se = list(fujian_amce_4attr_se,
                    beijing_amce_4attr_se,
                    beijing_amce_6attr_se,
                    online_amce_4attr_se,
                    online_amce_6attr_se),
          covariate.labels = c("Punishment", "Growth", "Election", "Welfare", "Moral", "Competence"),
          column.separate = c(1,2,2),
          column.labels = c("Study 1", "Study 2", "Study 3"),
          dep.var.caption = "Conjoint Preference",
          dep.var.labels.include = FALSE,
          keep.stat = c("n", "adj.rsq"),
          notes = "Clustered Standard Errors in Parentheses",
          #float = TRUE,
          label = "table:conjoint-main-results-all",
          title = "OLS Estimates of conjoint AMCEs across the three studies. For Studies 2 and 3, 
          estimates are calculated separately for four-attribute conjoint rounds and six-attribute 
          conjoint rounds.",
          type = "latex",
          out = "../tableD1.tex")


#### ROBUSTNEST OF AMCE FOR PUNISHMENT ACROSS CONJOINT ATTRIBUTES, STUDY 2 ####
## Figure D2 ##

### Effect of punishment holding other conjoint variables constant
compare.ggplot(beijing, "resp_id", "fgD2a",  
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "election", "dibao", "moral", "competence"),
               labels = c("Punishment", "Election", "Welfare", "Moral", "Competence"),
               by = "growth", by.var.label = "Growth attribute",
               by.labels = c(`0` = "Negative", `1` = "Positive"),
               path = "../", filetype = "eps")
compare.ggplot(beijing, "resp_id", "fgD2b",  
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "moral", "competence"),
               labels = c("Punishment", "Growth", "Election", "Moral", "Competence"),
               by = "dibao", by.var.label = "Welfare attribute",
               by.labels = c(`0` = "Negative", `1` = "Positive"),
               path = "../", filetype = "eps")
compare.ggplot(beijing, "resp_id", "fgD2c",  
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "dibao", "moral", "competence"),
               labels = c("Punishment", "Growth", "Welfare", "Moral", "Competence"),
               by = "election", by.var.label = "Election attribute",
               by.labels = c(`0` = "Negative", `1` = "Positive"),
               path = "../", filetype = "eps")

#### ROBUSTNEST OF AMCE FOR PUNISHMENT ACROSS CONJOINT ATTRIBUTES, STUDY 3 ####
## Figure D3 ##

compare.ggplot(online, "resp_id", "fgD3a",  
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "election", "dibao", "moral", "competence"),
               labels = c("Punishment", "Election", "Welfare", "Moral", "Competence"),
               by = "growth", by.var.label = "Growth attribute",
               by.labels = c(`0` = "Negative", `1` = "Positive"),
               path = "../", filetype = "eps")
compare.ggplot(online, "resp_id", "fgD3b",  
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "moral", "competence"),
               labels = c("Punishment", "Growth", "Election", "Moral", "Competence"),
               by = "dibao", by.var.label = "Welfare attribute",
               by.labels = c(`0` = "Negative", `1` = "Positive"),
               path = "../", filetype = "eps")
compare.ggplot(online, "resp_id", "fgD3c",  
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "dibao", "moral", "competence"),
               labels = c("Punishment", "Growth", "Welfare", "Moral", "Competence"),
               by = "election", by.var.label = "Election attribute",
               by.labels = c(`0` = "Negative", `1` = "Positive"),
               path = "../", filetype = "eps")



#### ROBUSTNEST OF AMCE FOR PUNISHMENT ACROSS SUBSAMPLES, STUDY 2 ####
## Figure D4 ##
compare.ggplot(beijing %>% filter(exp_id_joint == 1), "resp_id", "fgD4a", 
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "dibao"),
               labels = c("Punishment", "Growth", "Election", "Welfare"),
               by = "age_cat", by.var.label = "Age group",
               by.labels = c(`< 40` = "Below 40", `>= 40` = "40 and above"),
               path = "../", filetype = "eps")
compare.ggplot(beijing %>% filter(exp_id_joint == 1), "resp_id", "fgD4b", 
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "dibao"),
               labels = c("Punishment", "Growth", "Election", "Welfare"),
               by = "gender", by.var.label = "Gender",
               by.labels = c(Male = "Male", Female = "Female"),
               path = "../", filetype = "eps")
compare.ggplot(beijing %>% filter(exp_id_joint == 1), "resp_id", "fgD4c", 
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "dibao"),
               labels = c("Punishment", "Growth", "Election", "Welfare"),
               by = "education_cat", by.var.label = "Education level",
               by.labels = c(`Below Highschool` = "Below Highschool", `High school and above` = "Highschool and above"),
               path = "../", filetype = "eps")
compare.ggplot(beijing %>% filter(exp_id_joint == 1), "resp_id", "fgD4d", 
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "dibao"),
               labels = c("Punishment", "Growth", "Election", "Welfare"),
               by = "party", by.var.label = "Party Membership",
               by.labels = c(`1 - Yes` = "Party Member", `2 - No` = "Non-Member"),
               path = "../", filetype = "eps")


#### ROBUSTNEST OF AMCE FOR PUNISHMENT ACROSS SUBSAMPLES, STUDY 3 ####
## Figure D5 ##

compare.ggplot(beijing, "resp_id", "fgD5a", 
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "dibao", "moral", "competence"),
               labels = c("Punishment", "Growth", "Election", "Welfare", "Moral", "Competence"),
               by = "age_cat", by.var.label = "Age group",
               by.labels = c(`< 40` = "Below 40", `>= 40` = "40 and above"),
               path = "../", filetype = "eps")
compare.ggplot(beijing, "resp_id", "fgD5b", 
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "dibao", "moral", "competence"),
               labels = c("Punishment", "Growth", "Election", "Welfare", "Moral", "Competence"),
               by = "gender", by.var.label = "Gender",
               by.labels = c(Male = "Male", Female = "Female"),
               path = "../", filetype = "eps")
compare.ggplot(beijing, "resp_id", "fgD5c", 
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "dibao", "moral", "competence"),
               labels = c("Punishment", "Growth", "Election", "Welfare", "Moral", "Competence"),
               by = "education_cat", by.var.label = "Education level",
               by.labels = c(`Below Highschool` = "Below Highschool", `High school and above` = "Highschool and above"),
               path = "../", filetype = "eps")
compare.ggplot(beijing, "resp_id", "fgD5d", 
               outcome = "vote", outcome_lab = "Profile is preferred",
               variables = c("punishment", "growth", "election", "dibao", "moral", "competence"),
               labels = c("Punishment", "Growth", "Election", "Welfare", "Moral", "Competence"),
               by = "party", by.var.label = "Party Membership",
               by.labels = c(`1 - Yes` = "Party Member", `2 - No` = "Non-Member"),
               path = "../", filetype = "eps")



#### STUDY 2 FULL MEDIATION ANALYSIS ####
## Table E1 ##

# Top half is identical to Table 1
tableE1a <- table1

# Bottom half is identical to Table 1
set.seed(12345)
# Column 1
punishment_joint_bj_rate <- med.para.ri(outcome="rate",
                                        treatment="punishment",
                                        mediator=c("moral", "competence"),
                                        covs=c("dibao", "election", "growth"),
                                        exp.id="exp_id_joint",
                                        data=beijing,
                                        niter=2000)
punishment_joint_bj_rate

# Column 2
punishment_moral_nat_bj_rate <- med.para.ri(outcome="rate",
                                            treatment="punishment",
                                            mediator="moral",
                                            covs=c("dibao", "election", "growth"),
                                            exp.id="exp_id_moral_nat",
                                            data=beijing,
                                            niter=2000)
punishment_moral_nat_bj_rate

# Column 3
punishment_moral_con_bj_rate <- med.para.ri(outcome="rate",
                                            treatment="punishment",
                                            mediator="moral",
                                            covs=c("dibao", "election", "growth", "competence"),
                                            exp.id="exp_id_moral_con",
                                            data=beijing,
                                            niter=2000)
punishment_moral_con_bj_rate

# Column 4
punishment_competence_nat_bj_rate <- med.para.ri(outcome="rate",
                                                 treatment="punishment",
                                                 mediator="competence",
                                                 covs=c("dibao", "election", "growth"),
                                                 exp.id="exp_id_competence_nat",
                                                 data=beijing,
                                                 niter=2000)
punishment_competence_nat_bj_rate

# Column 5
punishment_competence_con_bj_rate <- med.para.ri(outcome="rate",
                                                 treatment="punishment",
                                                 mediator="competence",
                                                 covs=c("dibao", "election", "growth", "moral"),
                                                 exp.id="exp_id_competence_con",
                                                 data=beijing,
                                                 niter=2000)
punishment_competence_con_bj_rate

# Put it all together
tableE1b <- sapply(list(punishment_joint_bj_rate,
                        punishment_moral_nat_bj_rate,
                        punishment_moral_con_bj_rate,
                        punishment_competence_nat_bj_rate,
                        punishment_competence_con_bj_rate),
                   function(x) {
                     rbind(x$indirect,
                           x$indirect.greater,
                           x$total,
                           x$total.greater)
                   }) %>%
  formatC(digits = 3, format = "f")

colnames(tableE1b) <- c("combined", "moral - natural", "moral - controlled", "competence - natural", "competence - controlled")
rownames(tableE1b) <- c("ind_est", "ind_se", "total_est", "total_se")
tableE1b

write.csv(rbind(tableE1a, tableE1b), file = "../tableE1.csv")

#### STUDY 3 FULL MEDIATION ANALYSIS ####
## Table F1 ##

# Top half 
set.seed(12345)
# Column 1
punishment_joint_onl <- med.para.ri(outcome="vote",
                                    treatment="punishment",
                                    mediator=c("moral", "competence"),
                                    covs=c("dibao", "election", "growth"),
                                    exp.id="exp_id_joint",
                                    data=online,
                                    niter=2000)
punishment_joint_onl

# Column 2
punishment_moral_nat_onl <- med.para.ri(outcome="vote",
                                        treatment="punishment",
                                        mediator="moral",
                                        covs=c("dibao", "election", "growth"),
                                        exp.id="exp_id_moral_nat",
                                        data=online,
                                        niter=2000)
punishment_moral_nat_onl

# Column 3
punishment_moral_con_onl <- med.para.ri(outcome="vote",
                                        treatment="punishment",
                                        mediator="moral",
                                        covs=c("dibao", "election", "growth", "competence"),
                                        exp.id="exp_id_moral_con",
                                        data=online,
                                        niter=2000)
punishment_moral_con_onl

# Column 4
punishment_competence_nat_onl <- med.para.ri(outcome="vote",
                                             treatment="punishment",
                                             mediator="competence",
                                             covs=c("dibao", "election", "growth"),
                                             exp.id="exp_id_competence_nat",
                                             data=online,
                                             niter=2000)
punishment_competence_nat_onl

# Column 5
punishment_competence_con_onl <- med.para.ri(outcome="vote",
                                             treatment="punishment",
                                             mediator="competence",
                                             covs=c("dibao", "election", "growth", "moral"),
                                             exp.id="exp_id_competence_con",
                                             data=online,
                                             niter=2000)
punishment_competence_con_onl

# Put it all together
tableF1a <- sapply(list(punishment_joint_onl,
                        punishment_moral_nat_onl,
                        punishment_moral_con_onl,
                        punishment_competence_nat_onl,
                        punishment_competence_con_onl),
                   function(x) {
                     rbind(x$indirect,
                           x$indirect.greater,
                           x$total,
                           x$total.greater)
                   }) %>%
  formatC(digits = 3, format = "f")

colnames(tableF1a) <- c("combined", "moral - natural", "moral - controlled", "competence - natural", "competence - controlled")
rownames(tableF1a) <- c("ind_est", "ind_se", "total_est", "total_se")
tableF1a

# Bottom half
set.seed(12345)
# Column 1
punishment_joint_onl_rate <- med.para.ri(outcome="rate",
                                         treatment="punishment",
                                         mediator=c("moral", "competence"),
                                         covs=c("dibao", "election", "growth"),
                                         exp.id="exp_id_joint",
                                         data=online,
                                         niter=2000)
punishment_joint_onl_rate

# Column 2
punishment_moral_nat_onl_rate <- med.para.ri(outcome="rate",
                                             treatment="punishment",
                                             mediator="moral",
                                             covs=c("dibao", "election", "growth"),
                                             exp.id="exp_id_moral_nat",
                                             data=online,
                                             niter=2000)
punishment_moral_nat_onl_rate

# Column 3
punishment_moral_con_onl_rate <- med.para.ri(outcome="rate",
                                             treatment="punishment",
                                             mediator="moral",
                                             covs=c("dibao", "election", "growth", "competence"),
                                             exp.id="exp_id_moral_con",
                                             data=online,
                                             niter=2000)
punishment_moral_con_onl_rate

# Column 4
punishment_competence_nat_onl_rate <- med.para.ri(outcome="rate",
                                                  treatment="punishment",
                                                  mediator="competence",
                                                  covs=c("dibao", "election", "growth"),
                                                  exp.id="exp_id_competence_nat",
                                                  data=online,
                                                  niter=2000)
punishment_competence_nat_onl_rate

# Column 5
punishment_competence_con_onl_rate <- med.para.ri(outcome="rate",
                                                  treatment="punishment",
                                                  mediator="competence",
                                                  covs=c("dibao", "election", "growth", "moral"),
                                                  exp.id="exp_id_competence_con",
                                                  data=online,
                                                  niter=2000)
punishment_competence_con_onl_rate

# Put it all together
tableF1b <- sapply(list(punishment_joint_onl_rate,
                        punishment_moral_nat_onl_rate,
                        punishment_moral_con_onl_rate,
                        punishment_competence_nat_onl_rate,
                        punishment_competence_con_onl_rate),
                   function(x) {
                     rbind(x$indirect,
                           x$indirect.greater,
                           x$total,
                           x$total.greater)
                   }) %>%
  formatC(digits = 3, format = "f")

colnames(tableF1b) <- c("combined", "moral - natural", "moral - controlled", "competence - natural", "competence - controlled")
rownames(tableF1b) <- c("ind_est", "ind_se", "total_est", "total_se")
tableF1b

write.csv(rbind(tableF1a, tableF1b), file = "../tableF1.csv")

#### RATING FOR COMPETENCE AND MORAL CONDITIONAL ON THE OTHER'S CONJOINT ATTRIBUTE VALUE, STUDY 2 AND 3 ####
## Table G1 ##

tableG1 <- rbind(cbind(rbind(cbind(mean(beijing$rate_competence[which(beijing$punishment==1 & beijing$moral==0)], na.rm=TRUE),
                                   mean(beijing$rate_competence[which(beijing$punishment==0 & beijing$moral==0)], na.rm=TRUE),
                                   mean(beijing$rate_competence[which(beijing$punishment==1 & beijing$moral==0)], na.rm=TRUE) -
                                     mean(beijing$rate_competence[which(beijing$punishment==0 & beijing$moral==0)], na.rm=TRUE)),
                             cbind(mean(beijing$rate_competence[which(beijing$punishment==1 & is.na(beijing$moral))], na.rm=TRUE),
                                   mean(beijing$rate_competence[which(beijing$punishment==0 & is.na(beijing$moral))], na.rm=TRUE),
                                   mean(beijing$rate_competence[which(beijing$punishment==1 & is.na(beijing$moral))], na.rm=TRUE) -
                                     mean(beijing$rate_competence[which(beijing$punishment==0 & is.na(beijing$moral))], na.rm=TRUE)),
                             cbind(mean(beijing$rate_competence[which(beijing$punishment==1 & beijing$moral==1)], na.rm=TRUE),
                                   mean(beijing$rate_competence[which(beijing$punishment==0 & beijing$moral==1)], na.rm=TRUE),
                                   mean(beijing$rate_competence[which(beijing$punishment==1 & beijing$moral==1)], na.rm=TRUE) -
                                     mean(beijing$rate_competence[which(beijing$punishment==0 & beijing$moral==1)], na.rm=TRUE))),
                       rbind(NA,
                             cbind(mean(beijing$rate_moral[which(beijing$punishment==1 & beijing$round==6)], na.rm=TRUE),
                                   mean(beijing$rate_moral[which(beijing$punishment==0 & beijing$round==6)], na.rm=TRUE),
                                   mean(beijing$rate_moral[which(beijing$punishment==1 & beijing$round==6)], na.rm=TRUE)-
                                   mean(beijing$rate_moral[which(beijing$punishment==0 & beijing$round==6)], na.rm=TRUE)),
                             NA)),
                 cbind(rbind(cbind(mean(online$rate_competence[which(online$punishment==1 & online$moral==0)], na.rm=TRUE),
                                   mean(online$rate_competence[which(online$punishment==0 & online$moral==0)], na.rm=TRUE),
                                   mean(online$rate_competence[which(online$punishment==1 & online$moral==0)], na.rm=TRUE) -
                                   mean(online$rate_competence[which(online$punishment==0 & online$moral==0)], na.rm=TRUE)),
                             cbind(mean(online$rate_competence[which(online$punishment==1 & is.na(online$moral))], na.rm=TRUE),
                                   mean(online$rate_competence[which(online$punishment==0 & is.na(online$moral))], na.rm=TRUE),
                                   mean(online$rate_competence[which(online$punishment==1 & is.na(online$moral))], na.rm=TRUE) -
                                   mean(online$rate_competence[which(online$punishment==0 & is.na(online$moral))], na.rm=TRUE)),
                             cbind(mean(online$rate_competence[which(online$punishment==1 & online$moral==1)], na.rm=TRUE),
                                   mean(online$rate_competence[which(online$punishment==0 & online$moral==1)], na.rm=TRUE),
                                   mean(online$rate_competence[which(online$punishment==1 & online$moral==1)], na.rm=TRUE) -
                                   mean(online$rate_competence[which(online$punishment==0 & online$moral==1)], na.rm=TRUE))),
                       rbind(NA,
                             cbind(mean(online$rate_moral[which(online$punishment==1 & online$round==6)], na.rm=TRUE),
                                   mean(online$rate_moral[which(online$punishment==0 & online$round==6)], na.rm=TRUE),
                                   mean(online$rate_moral[which(online$punishment==1 & online$round==6)], na.rm=TRUE) -
                                   mean(online$rate_moral[which(online$punishment==0 & online$round==6)], na.rm=TRUE)),
                             NA))) %>% 
  formatC(digits = 3, format = "f")

write.csv(tableG1, file = "../tableG1.csv")
