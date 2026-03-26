# Replication data for Anger Expressions and Coercive Credibility in International Crises
# Author: Hohyun Yoon
# Last updated: August 2024
# Machine: M2 Macbook Air Monterey 12.5, R version 4.4.1
# This file replicates results for Study 1

#### Setup ####
library(here)
library(tidyverse)
library(patchwork)
library(ordinal)
library(lmtest)
library(sandwich)
library(texreg)
library(marginaleffects)
library(xtable)
library(caret)
library(AER)
library(countrycode)

# Read in data and generate variables
dat_raw <- readRDS(here("data", "datObs.rds")) #main dataset
df <- dat_raw %>% #create challenger-defender dyads
  filter(init_a == 1)

#### Figure 1: Data description ####
# Distribution of anger scores (avg between the two) as a function of crisis 
tags <- c("0","(0-0.1]", "(0.1-0.2]", "(0.2-0.3]", "(0.3-0.4]", "(0.4-0.5]")

# Create df and plot for anger scores
df_plot <- as_tibble(dat_raw) %>% 
  group_by(crisno) %>% 
  summarize(avg_ang = round(mean(prop_anger_a), 2),
            statement = round(mean(statement_a), 2)) %>%
  ungroup() %>% 
  mutate(value = ifelse(avg_ang > 0, "Anger",
                        ifelse(statement > 0, "No anger", "No statement"))) %>% 
  select(-statement) %>% 
  group_by(avg_ang, value) %>% 
  summarize(count = sum(n())) %>% 
  ungroup() %>% mutate(tag = case_when(
    avg_ang == 0 ~ tags[1],
    avg_ang > 0 & avg_ang <= 0.1 ~ tags[2],
    avg_ang > 0.1 & avg_ang <= 0.2 ~ tags[3],
    avg_ang > 0.2 & avg_ang <= 0.3 ~ tags[4],
    avg_ang > 0.3 & avg_ang <= 0.4 ~ tags[5],
    avg_ang > 0.4 & avg_ang <= 0.5 ~ tags[6])) %>%
  select(-avg_ang)

df_plot2 <- df_plot %>% 
  group_by(value, tag) %>% 
  summarize(count = sum(count))

# Create figure
p_anger_bins <- df_plot2 %>% 
  ggplot(aes(
    x = factor(tag, 
               level = (c("0","(0-0.1]", "(0.1-0.2]", "(0.2-0.3]", "(0.3-0.4]", "(0.4-0.5]"))),
    y = count, fill = factor(value, level = c("No statement", "Anger", "No anger")))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#999999","#F8766D", "#00BFC4"),
                    guide = guide_legend(title = NULL)) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 30)) +
  ylab("Count") +
  xlab("\n Average anger score") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.text=element_text(size=11, color = "black"),
        axis.title=element_text(size=11, color = "black"))

# Save figure
p_anger_bins
ggsave("figure1.png", width = 8, height = 4, unit = "in")

# 53% of crises involve public statements
nrow(df) #184 crises
round(nrow(df %>% filter(statement_a == 1 | statement_b == 1)) / nrow(df) * 100)

# 81% of those cases involve anger expressions
round(
  nrow(df %>% 
       filter(statement_a == 1 | statement_b == 1) %>% 
       filter(prop_anger_a > 0 | prop_anger_b > 0)) / 98 * 100
  )

# 57% of crises have 0 anger expressions
round(nrow(df %>% filter(prop_anger_a == 0 & prop_anger_b == 0)) / nrow(df) * 100)


#### Table 2: Main results ####
# Parsimonious Model
ologit_pars <- outcom_ord ~ 
  prop_anger_a*prop_threat_a +
  prop_anger_b*prop_threat_b

fit_ologit_pars <- clm(ologit_pars, data = df, link = "logit")
cov_ologit_pars <- vcovCL(fit_ologit_pars, cluster = df$dyadid, type = "HC0")
(ologit_pars_cse <- coeftest(fit_ologit_pars, vcov = cov_ologit_pars))

# Full Model
ologit2 <- outcom_ord ~ 
  prop_anger_a*prop_threat_a +
  prop_anger_b*prop_threat_b +
  sevviosy + ldrchange_a + ldrchange_b + 
  words_perday_a + words_perday_b +
  statement_a + statement_b +
  emotion_prop_a + emotion_prop_b +
  pers_gwf_a + pers_gwf_b + 
  rel_power_a + mil_issue + durac

fit_ologit2 <- clm(ologit2, data = df, link = "logit")
cov_ologit2 <- vcovCL(fit_ologit2, cluster = df$dyadid, type = "HC0")
(ologit2_cse <- coeftest(fit_ologit2, vcov = cov_ologit2))

# Create table
covars1 = list(
  "prop_anger_a:prop_threat_a" = "Anger, A $\\times$ threat, A",
  "prop_anger_a" = "Anger, A",
  "prop_threat_a" = "Threat, A",
  "prop_anger_b:prop_threat_b" = "Anger, B $\\times$ threat, B",
  "prop_anger_b" = "Anger, B",
  "prop_threat_b" = "Threat, B",
  "statement_a" = "Statement, A",
  "statement_b" = "Statement, B",
  "emotion_prop_a" = "Emotionality, A",
  "emotion_prop_b" = "Emotionality, B",
  "words_perday_a" = "Word count, A",
  "words_perday_b" = "Word count, B",
  "pers_gwf_a" = "Personalist, A",
  "pers_gwf_b" = "Personalist, B",
  "ldrchange_a" = "Leadership change, A",
  "ldrchange_b" = "Leadership change, B",
  "rel_power_a" = "Relative power, A",
  "sevviosy" = "Level of violence",
  "durac" = "Crisis duration",
  "mil_issue" = "Military issue",
  "1|2" = "1|2",
  "2|3" = "2|3"
)

screenreg(l = list(fit_ologit_pars, fit_ologit2), 
       digits = 3,
       stars = c(.10, .05, 0.01),
       custom.coef.map = covars1,
       override.se = list(c(ologit_pars_cse[3:8, 2], ologit_pars_cse[1:2, 2]), c(ologit2_cse[3:22, 2], ologit2_cse[1:2, 2])),
       override.pvalues = list(c(ologit_pars_cse[3:8, 4], ologit_pars_cse[1:2, 4]), c(ologit2_cse[3:22, 4], ologit2_cse[1:2, 4])),
       single.row = FALSE,
       booktabs = TRUE,
       dcolumn = TRUE,
       no.margin = FALSE,
       float.pos = "h!",
       include.aic = FALSE,
       include.bic = FALSE,
       include.loglik	= FALSE,
       custom.note = "%stars.")


#### Figure 2: Marginal effects plot ####
# Run Table 2 first
newdat <- slopes(fit_ologit2,
                 newdata = datagrid(prop_anger_a = seq(0, max(df$prop_anger_a), 0.01),
                                    prop_anger_b = mean(df$prop_anger_b),
                                    prop_threat_b = mean(df$prop_threat_b),
                                    sevviosy = mean(df$sevviosy),
                                    ldrchange_a = mean(df$ldrchange_a),
                                    ldrchange_b = mean(df$ldrchange_b),
                                    words_perday_a = mean(df$words_perday_a),
                                    words_perday_b = mean(df$words_perday_b),
                                    statement_a = mean(df$statement_a),
                                    statement_b = mean(df$statement_b),
                                    emotion_prop_a = mean(df$emotion_prop_a),
                                    emotion_prop_b = mean(df$emotion_prop_b),
                                    pers_gwf_a = mean(df$pers_gwf_a, na.rm = T),
                                    pers_gwf_b = mean(df$pers_gwf_b, na.rm = T),
                                    rel_power_a = mean(df$rel_power_a, na.rm = T),
                                    mil_issue = mean(df$mil_issue, na.rm = T),
                                    durac = mean(df$durac, na.rm = T)
                 ),
                 vcov = ~ dyadid,
                 variables = "prop_threat_a") %>% 
  filter(group == 3)

# Mean and SD
avg_ang <- round(mean(df$prop_anger_a), digits = 2) #0.06
sd_ang <- round(sd(df$prop_anger_a), digits = 2) #0.13

# Change in marginal effect of threat: 0.83
round(newdat$estimate[newdat$prop_anger_a == 0.19] - newdat$estimate[newdat$prop_anger_a == 0.06], 2)

plot_h1 <- newdat %>%
  ggplot() +
  geom_line(aes(x = prop_anger_a, y = estimate / 100),
            color = "#F8766D",
            linewidth = 1) +
  geom_ribbon(aes(
    x = prop_anger_a,
    ymin = conf.low / 100,
    ymax = conf.high / 100
  ),
  alpha = 0.3,
  fill = "#F8766D"
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = avg_ang,
             linetype = "dashed",
             color = "#F8766D",
             linewidth = 1) +
  geom_vline(xintercept = avg_ang + sd_ang,
             linetype = "dashed",
             color = "#F8766D",
             linewidth = 1) +
  geom_vline(xintercept = avg_ang + 2 * sd_ang,
             linetype = "dashed",
             color = "#F8766D",
             linewidth = 1) +
  theme_classic() +
  theme(axis.text=element_text(size=11, color = "black"),
        axis.title=element_text(size=11, color = "black")) +
  annotate("text", x = avg_ang - 0.01, y = 0.05, hjust = 1, vjust = 1, 
           label = "Mean \n 0.06", size = 3) +
  annotate("text", x = avg_ang + sd_ang - 0.01, y = 0.05, hjust = 1, vjust = 1, 
           label = "+1 SD\n 0.19", size = 3) +
  annotate("text", x = avg_ang + 2 * sd_ang - 0.01, y = 0.05, hjust = 1, vjust = 1, 
           label = "+2 SD\n 0.32", size = 3) +
  scale_x_continuous(name = "\n Anger score", breaks = seq(0, 0.6, 0.1), 
                     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6")) +
  scale_y_continuous(name = "Marginal effect of threat \n", breaks = seq(0, 0.1, 0.02),
                     labels = c("0", "0.02", "0.04", "0.06", "0.08", "0.01"))

plot_h1
ggsave("figure2.png", width = 8, height = 4, units = "in")


#### Table S1: Summary statistics of variables ####
# Model variables
dat_summ <- df %>% 
  select(prop_anger_a, prop_threat_a, 
         prop_anger_b, prop_threat_b, 
         sevviosy, ldrchange_a, ldrchange_b, 
         words_perday_a_orig, words_perday_b_orig, 
         mil_issue, rel_power_a, pers_gwf_a, pers_gwf_b, durac_orig,
         statement_a, statement_b,
         emotion_prop_a, emotion_prop_b) %>% 
  gather(variable, value) %>% 
  group_by(variable) %>%
  summarise(n = sum(!is.na(value)),
            `Mean` = mean(value, na.rm = T),
            `Std. Dev.` = sd(value, na.rm = T),
            `Median` = median(value, na.rm = T),
            `Min.` = min(value, na.rm = T),
            `Max.` = max(value, na.rm = T)) %>% 
  ungroup() %>% 
  select(-variable) %>% 
  arrange()

Variable <- c(
  "Crisis duration",
  "Leadership change, A",
  "Leadership change, B",
  "Emotionality, A",
  "Emotionality, B",
  "Military issue",
  "Personalist, A",
  "Personalist, B",
  "Anger, A",
  "Anger, B",
  "Threat, A",
  "Threat, B",
  "Relative power, A",
  "Level of violence",
  "Statement, A",
  "Statement, B",
  "Word count, A",
  "Word count, B"
)

dat_summ <- cbind(Variable, dat_summ)
xtable(dat_summ, 
       digits = 2,
       caption = "Summary statistics for the variables in the ordered logit regression model reported in the main text.") %>%
  print.xtable(type = "latex",
               booktabs = TRUE,
               include.rownames = FALSE,
               format.args = list(big.mark = ","))


#### Table S2: Summary statistics of statements ####
statements_summ_all <- tibble(
  "Anger" =
    c(df$prop_anger_a, df$prop_anger_b),
  "Threat" =
    c(df$prop_threat_a, df$prop_threat_b)
) %>%
  gather(Variable, value) %>%
  group_by(Variable) %>%
  summarise(
    n = sum(!is.na(value)),
    `Mean` = mean(value, na.rm = T),
    `Std. Dev.` = sd(value, na.rm = T),
    `Median` = median(value, na.rm = T),
    `Min.` = min(value, na.rm = T),
    `Max.` = max(value, na.rm = T)
  )

# Table
xtable(statements_summ_all, 
       digits = 2, 
       caption = "Summary statistics for public statements. Anger scores are coded as zero for cases where no statement is made. The N is 368 as this table describes both the challenger's and the defender's anger statements.") %>%
  print.xtable(type = "latex",
               booktabs = TRUE,
               include.rownames = FALSE,
               format.args = list(big.mark = ","))


#### Table S3: Intercoder reliability ####
# Also on Footnote 3, p.11
X <- readRDS(here("data", "X.rds")) #author's coding of anger
XI <- readRDS(here("data", "XI.rds")) #Annotator 1's coding of anger
XR <- readRDS(here("data", "XR.rds")) #Annotator 2's coding of anger

# Anger
true <- factor(X$anger)
pred_XI <- factor(XI$anger)
xtab_XI <- table(true, pred_XI)
a1aa <- confusionMatrix(xtab_XI, mode = "everything")$overall["Accuracy"]
a1af <- confusionMatrix(xtab_XI, mode = "everything")$byClass["F1"]

true <- factor(X$anger)
pred_XR <- factor(XR$anger)
xtab_XR <- table(pred_XR, true)
a1ta <- confusionMatrix(xtab_XR, mode = "everything")$overall["Accuracy"]
a1tf <- confusionMatrix(xtab_XR, mode = "everything")$byClass["F1"]

# Threat
true <- factor(X$threatening)
pred_XI <- factor(XI$threatening)
xtab_XI <- table(pred_XI, true)
a2aa <- confusionMatrix(xtab_XI, mode = "everything")$overall["Accuracy"]
a2af <- confusionMatrix(xtab_XI, mode = "everything")$byClass["F1"]

true <- factor(X$threatening)
pred_XR <- factor(XR$threatening)
xtab_XR <- table(pred_XR, true)
a2ta <- confusionMatrix(xtab_XR, mode = "everything")$overall["Accuracy"]
a2tf <- confusionMatrix(xtab_XR, mode = "everything")$byClass["F1"]

reliability <- round(rbind(cbind(a1aa, a1af, a1ta, a1tf), cbind(a2aa, a2af, a2ta, a2tf)), 2)
rownames(reliability) <- c("Accuracy", "F1")
colnames(reliability) <- c("A1 Anger", "A1 Threat", "A2 Anger", "A2 Threat")


xtable(reliability,
       digits = 2,
       caption = "Intercoder reliability between the author and Annotator 1 (A1) and Annotator 2 (A2).") %>%
  print.xtable(type = "latex",
               table.placement = "h!",
               booktabs = TRUE,
               format.args = list(big.mark = ","))

#### Table S4: Predicting anger scores 1 ####
fit_tobit_condition1 <- df %>%
  tobit(prop_anger_a ~ prop_anger_b + rival + sscore + rel_power_a + pers_gwf_a + sevviosy + mil_issue + mediate + durac,
        data = ., left = 0, right = 1, dist = "gaussian")
cov_condition1 <- vcovCL(fit_tobit_condition1, cluster = df$dyadid, type = "HC0")
(condition1_cse <- coeftest(fit_tobit_condition1, vcov = cov_condition1))

mod2_labels <-
  list(
    "(Intercept)" = "(Intercept)",
    "rel_power_a" = "Relative power",
    "prop_anger_b" = "Adversary's anger",
    "rival" = "Rivalry",
    "sscore" = "Interest similarity",
    "pers_gwf_b" = "Personalist, A",
    "pers_gwf_b" = "Personalist, B",
    "sevviosy" = "Level of violence",
    "mil_issue" = "Military issue",
    "mediate" = "Mediation",
    "durac" = "Crisis duration"
  )

screenreg(list(fit_tobit_condition1),
       digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = mod2_labels,
       override.se = list(condition1_cse[, 2]),
       override.pvalues = list(condition1_cse[, 4]),
       single.row = FALSE,
       booktabs = TRUE,
       dcolumn = TRUE,
       fontsize = "small",
       omit.coef = "Log",
       no.margin = TRUE,
       float.pos = "h!",
       include.aic = FALSE,
       include.bic = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       include.nobs = FALSE,
       include.wald = FALSE,
       custom.note = "%stars. Robust clustered standard errors at the dyad level.")


#### Table S5: Predicting anger scores 2 ####
X <- readRDS(here("data", "X.rds")) #read author's coding of anger
X_statement <- X %>% #create statement level df
  select(crisno, title, date_issued, country, anger, threatening) %>% 
  group_by(crisno, title, date_issued, country) %>% 
  summarize(angerm = mean(anger),
            resolvem = mean(threatening)) %>% 
  ungroup()

durac <- dat_raw %>% #create duration and dates df
  select(crisno, durac_orig, sys_trig_date, sys_term_date) %>% 
  group_by(crisno) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

statement_durac <- X_statement %>% #merge the two
  left_join(durac) %>% 
  mutate(date_issued = as.Date(date_issued, format = "%m/%d/%Y")) %>% #change to date var
  mutate(made = date_issued - sys_trig_date)

durac_quart <- durac %>% 
  group_by(crisno) %>% 
  summarize(begin = ceiling(quantile(seq(1:durac_orig), probs=c(0.33))),
            end = ceiling(quantile(seq(1:durac_orig), probs=c(0.66))))

statement_quart <- statement_durac %>% 
  left_join(durac_quart) %>% 
  mutate(made_when = case_when(
    made <= begin ~ 1,
    made > begin & made <= end ~ 2,
    TRUE ~ 3
  ))

phases <- c("Beginning", "Middle", "End") #define crisis phases
statement_quart <- statement_quart %>% 
  mutate(made_when = factor(made_when, labels = phases, ordered = TRUE))

outcom <- dat_raw %>% 
  select(crisno, ccode_a, outcom_ord) %>%
  mutate(country = countrycode(ccode_a, destination = "cow.name", origin = "cown")) %>% 
  select(-ccode_a) %>% 
  rename(outcom = outcom_ord)

statement_quart <- statement_quart %>% 
  left_join(outcom, by = c("crisno", "country"))
dv_labl <- c("Defeat", "Stalemate", "Victory")

statement_quart <- statement_quart %>% 
  mutate(Beginning = ifelse(made_when == "Beginning", 1, 0),
         Middle = ifelse(made_when == "Middle", 1, 0),
         End = ifelse(made_when == "End", 1, 0))

angwhen_win <- statement_quart %>% 
  filter(outcom == 3) %>% 
  tobit(angerm ~ Beginning + Middle, 
        left = 0, right = 1, dist = "gaussian", data = .)
cov_angwhen_win <- vcovCL(angwhen_win, cluster = statement_quart[statement_quart$outcom == 3, ]$crisno, type = "HC0")
angwhen_win_cse <- coeftest(angwhen_win, vcov = cov_angwhen_win)

angwhen_all <- statement_quart %>% 
  tobit(angerm ~ Beginning + Middle, 
        left = 0, right = 1, dist = "gaussian", data = .)
cov_angwhen_all <- vcovCL(angwhen_all, cluster = statement_quart$crisno, type = "HC0")
angwhen_all_cse <- coeftest(angwhen_all, vcov = cov_angwhen_all)

screenreg(list(angwhen_win, angwhen_all),
       single.row = FALSE,
       digits = 3,
       stars = c(0.01, 0.05, 0.1),
       booktabs = TRUE,
       dcolumn = TRUE,
       fontsize = "small",
       omit.coef = "Log",
       no.margin = TRUE,
       float.pos = "h!",
       include.aic = FALSE,
       include.bic = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       include.nobs = FALSE,
       include.wald = FALSE,
       custom.model.names = c("Winning outcome", "All outcome"),
       override.se = list(angwhen_win_cse[, 2], angwhen_all_cse[, 2]),
       override.pvalues = list(angwhen_win_cse[, 4], angwhen_all_cse[, 4]),
       caption = "Tobit models predicting mean anger scores by phase.",
       custom.note = "%stars. Robust clustered standard errors at the crisis level.")


#### Table S6: Other confounding issues ####
# Dropping brute force cases
brtforce <- c(155, 221, 294, 343, 277, 391)
df_nobforce <- df %>% 
  filter(!crisno %in% brtforce)

fit_nobforce <- clm(ologit2, data = df_nobforce, link = "logit")
cov_nobforce <- vcovCL(fit_nobforce, cluster = df_nobforce$dyadid, type = "HC0")
(nobforce_cse <- coeftest(fit_nobforce, vcov = cov_nobforce))

# Region control
ologit2_region <- outcom_ord ~ 
  prop_anger_a*prop_threat_a +
  prop_anger_b*prop_threat_b +
  sevviosy + ldrchange_a + ldrchange_b + 
  words_perday_a + words_perday_b +
  statement_a + statement_b +
  emotion_prop_a + emotion_prop_b +
  pers_gwf_a + pers_gwf_b + 
  rel_power_a + mil_issue + durac +
  same_region

fit_ologit2_region <- clm(ologit2_region, data = df, link = "logit")
cov_ologit2_region <- vcovCL(fit_ologit2_region, cluster = df$dyadid, type = "HC0")
(ologit2_region_cse <- coeftest(fit_ologit2_region, vcov = cov_ologit2_region))

# Time polynomials
ologit2_time <- outcom_ord ~ 
  prop_anger_a*prop_threat_a +
  prop_anger_b*prop_threat_b +
  sevviosy + ldrchange_a + ldrchange_b + 
  words_perday_a + words_perday_b +
  statement_a + statement_b +
  emotion_prop_a + emotion_prop_b +
  pers_gwf_a + pers_gwf_b + 
  rel_power_a + mil_issue + durac +
  time + I(time^2) + I(time^3)

fit_ologit2_time <- clm(ologit2_time, data = df, link = "logit")
cov_ologit2_time <- vcovCL(fit_ologit2_time, cluster = df$dyadid, type = "HC0")
(ologit2_time_cse <- coeftest(fit_ologit2_time, vcov = cov_ologit2_time))

covars_robust = list(
  "prop_anger_a:prop_threat_a" = "Anger, A $\\times$ Threat, A",
  "prop_anger_a" = "Anger, A",
  "prop_threat_a" = "Threat, A",
  "prop_anger_b:prop_threat_b" = "Anger, B $\\times$ Threat, B",
  "prop_anger_b" = "Anger, B",
  "prop_threat_b" = "Threat, B",
  "statement_a" = "Statement, A",
  "statement_b" = "Statement, B",
  "emotion_prop_a" = "Emotionality, A",
  "emotion_prop_b" = "Emotionality, B",
  "words_perday_a" = "Word count, A",
  "words_perday_b" = "Word count, B",
  "pers_gwf_a" = "Personalist, A",
  "pers_gwf_b" = "Personalist, B",
  "ldrchange_a" = "Leadership change, A",
  "ldrchange_b" = "Leadership change, B",
  "rel_power_a" = "Relative power, A",
  "sevviosy" = "Level of violence",
  "durac" = "Crisis duration",
  "mil_issue" = "Military issue",
  "same_region" = "Same region",
  "time" = "t",
  "I(time^2)" = "t$^2$",
  "I(time^3)" = "t$^3$",
  "1|2" = "1|2",
  "2|3" = "2|3"
)

screenreg(l = list(fit_nobforce, fit_ologit2_region, fit_ologit2_time),
       digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = covars_robust,
       override.se = list(c(nobforce_cse[3:22, 2], nobforce_cse[1:2, 2]),
                          c(ologit2_region_cse[3:23, 2], ologit2_region_cse[1:2, 2]),
                          c(ologit2_time_cse[3:25, 2], ologit2_time_cse[1:2, 2])),
       override.pvalues = list(c(nobforce_cse[3:22, 4], nobforce_cse[1:2, 4]),
                               c(ologit2_region_cse[3:23, 4], ologit2_region_cse[1:2, 4]),
                               c(ologit2_time_cse[3:25, 4], ologit2_time_cse[1:2, 4])),
       single.row = FALSE,
       booktabs = TRUE,
       dcolumn = TRUE,
       fontsize = "small",
       no.margin = FALSE,
       float.pos = "h!",
       include.aic = FALSE,
       include.bic = FALSE,
       include.loglik	= FALSE,
       label = "table:alternative",
       caption = "Effect of expressions of anger and threats on crisis outcome, ordered logit regression. Model 1 uses the sample without brute-force cases. Model 2 includes the region control. Model 3 accounts for time dependence.",
       custom.note = "%stars. Robust clustered standard errors at the dyad level.")


#### Table S7: Alternative measures of anger ####
# Create separate dfs
df_nrc <- df %>% 
  mutate(prop_anger_a = anger_dic_prop_a,
         prop_anger_b = anger_dic_prop_b)
df_nodirect <- df %>% 
  mutate(prop_anger_a = prop_nodirect_a,
         prop_anger_b = prop_nodirect_b)
df_noindirect <- df %>% 
  mutate(prop_anger_a = prop_noindirect_a,
         prop_anger_b = prop_noindirect_b)
df_noinsult <- df %>% 
  mutate(prop_anger_a = prop_noninsult_a,
         prop_anger_b = prop_noninsult_b)
df_nowrong <- df %>% 
  mutate(prop_anger_a = prop_nowrong_a,
         prop_anger_b = prop_nowrong_b)

# Define the equation
alt_model <- outcom_ord ~ 
  prop_anger_a*prop_threat_a +
  prop_anger_b*prop_threat_b +
  sevviosy + ldrchange_a + ldrchange_b + 
  words_perday_a + words_perday_b +
  statement_a + statement_b +
  emotion_prop_a + emotion_prop_b +
  pers_gwf_a + pers_gwf_b + 
  rel_power_a + mil_issue + durac

# NRC emotion lexicon 
ologit_nrc <- clm(alt_model, data = df_nrc, link = "logit")
cov_ologit_nrc <- vcovCL(ologit_nrc, cluster = df_nrc$dyadid, type = "HC0")
(ologit_nrc_ces <- coeftest(ologit_nrc, vcov = cov_ologit_nrc))

# No direct expressions
ologit_nodirect <- clm(alt_model, data = df_nodirect, link = "logit")
cov_ologit_nodirect <- vcovCL(ologit_nodirect, cluster = df_nodirect$dyadid, type = "HC0")
(ologit_nodirect_ces <- coeftest(ologit_nodirect, vcov = cov_ologit_nodirect))

# No indirect expressions
ologit_noindrect <- clm(alt_model, data = df_noindirect, link = "logit")
cov_ologit_noindrect <- vcovCL(ologit_noindrect, cluster = df_noindirect$dyadid, type = "HC0")
(ologit_noindrect_ces <- coeftest(ologit_noindrect, vcov = cov_ologit_noindrect))

# No verbal assaults 
ologit_noinsult <- clm(alt_model, data = df_noinsult, link = "logit")
cov_ologit_noinsult <- vcovCL(ologit_noinsult, cluster = df_noinsult$dyadid, type = "HC0")
(ologit_noinsult_ces <- coeftest(ologit_noinsult, vcov = cov_ologit_noinsult))

# No accusations of wrongdoing
ologit_nowrong <- clm(alt_model, data = df_nowrong, link = "logit")
cov_ologit_nowrong <- vcovCL(ologit_nowrong, cluster = df_nowrong$dyadid, type = "HC0")
(ologit_nowrong_ces <- coeftest(ologit_nowrong, vcov = cov_ologit_nowrong))

covars_alt = list(
  "prop_anger_a:prop_threat_a" = "Anger, A $\\times$ Threat, A",
  "prop_anger_a" = "Anger, A",
  "prop_threat_a" = "Threat, A",
  "prop_anger_b:prop_threat_b" = "Anger, B $\\times$ Threat, B",
  "prop_anger_b" = "Anger, B",
  "prop_threat_b" = "Threat, B",
  "statement_a" = "Statement, A",
  "statement_b" = "Statement, B",
  "emotion_prop_a" = "Emotionality, A",
  "emotion_prop_b" = "Emotionality, B",
  "words_perday_a" = "Word count, A",
  "words_perday_b" = "Word count, B",
  "pers_gwf_a" = "Personalist, A",
  "pers_gwf_b" = "Personalist, B",
  "ldrchange_a" = "Leadership change, A",
  "ldrchange_b" = "Leadership change, B",
  "rel_power_a" = "Relative power, A",
  "sevviosy" = "Level of violence",
  "durac" = "Crisis duration",
  "mil_issue" = "Military issue",
  "1|2" = "1|2",
  "2|3" = "2|3"
)

mod_names = c(
  "NRC", "No direct", "No indirect",
  "No insults", "No accusations"
)

screenreg(l = list(ologit_nrc, ologit_nodirect, ologit_noindrect, ologit_noinsult, ologit_nowrong),
       digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = covars_alt,
       custom.model.names = mod_names,
       override.se = list(c(ologit_nrc_ces[3:22, 2], ologit_nrc_ces[1:2, 2]),
                          c(ologit_nodirect_ces[3:22, 2], ologit_nodirect_ces[1:2, 2]),
                          c(ologit_noindrect_ces[3:22, 2], ologit_noindrect_ces[1:2, 2]),
                          c(ologit_noinsult_ces[3:22, 2], ologit_noinsult_ces[1:2, 2]),
                          c(ologit_nowrong_ces[3:22, 2], ologit_nowrong_ces[1:2, 2])),
       override.pvalues = list(c(ologit_nrc_ces[3:22, 4], ologit_nrc_ces[1:2, 4]),
                               c(ologit_nodirect_ces[3:22, 4], ologit_nodirect_ces[1:2, 4]),
                               c(ologit_noindrect_ces[3:22, 4], ologit_noindrect_ces[1:2, 4]),
                               c(ologit_noinsult_ces[3:22, 4], ologit_noinsult_ces[1:2, 4]),
                               c(ologit_nowrong_ces[3:22, 4], ologit_nowrong_ces[1:2, 4])),
       fontsize = "footnotesize",
       booktabs = TRUE,
       dcolumn = TRUE,
       float.pos = "h!",
       include.aic = FALSE,
       include.bic = FALSE,
       include.loglik	= FALSE,
       label = "table:alt_mods",
       caption = "Interaction effects of anger expressions and threats on crisis outcomes using ordered logit regression.",
       custom.note = "%stars. Robust clustered standard errors at the dyad level.")

# Mean anger w/ and w/o accusation
round(diff(c(mean(df$prop_anger_a), mean(df_nowrong$prop_anger_a)))/mean(df$prop_anger_a), 2)*100
round(diff(c(mean(df$prop_anger_b), mean(df_nowrong$prop_anger_b)))/mean(df$prop_anger_b), 2)*100


#### Table S8: Exploring scope conditions ####
# Are defenders more likely to prevail over challengers?
round(sum(df$outcom_ord == 3)/nrow(df) * 100, 1) #% of winning outcomes 16.8%
round(sum(df$outcom_ord == 1)/nrow(df) * 100, 1) #% of losing outcomes 29.3%

# Are defenders' threats more effective than challengers'? 
ologit_threats <- outcom_ord ~ 
  prop_threat_a + prop_threat_b +
  sevviosy + ldrchange_a + ldrchange_b + 
  words_perday_a + words_perday_b +
  statement_a + statement_b +
  pers_gwf_a + pers_gwf_b + 
  rel_power_a + mil_issue + durac

fit_ologit_threats <- clm(ologit_threats, data = df, link = "logit")
cov_ologit_threats <- vcovCL(fit_ologit_threats, cluster = df$dyadid, type = "HC0")
(ologit_threats_cse <- coeftest(fit_ologit_threats, vcov = cov_ologit_threats))


covars_threat = list(
  "prop_threat_a" = "Threat, A",
  "prop_threat_b" = "Threat, B",
  "statement_a" = "Statement, A",
  "statement_b" = "Statement, B",
  "words_perday_a" = "Word count, A",
  "words_perday_b" = "Word count, B",
  "pers_gwf_a" = "Personalist, A",
  "pers_gwf_b" = "Personalist, B",
  "ldrchange_a" = "Leadership change, A",
  "ldrchange_b" = "Leadership change, B",
  "rel_power_a" = "Relative power, A",
  "sevviosy" = "Level of violence",
  "durac" = "Crisis duration",
  "mil_issue" = "Military issue",
  "1|2" = "1|2",
  "2|3" = "2|3"
)

screenreg(l = list(fit_ologit_threats),
       digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.coef.map = covars_threat,
       override.se = list(c(ologit_threats_cse[3:16, 2], ologit_threats_cse[1:2, 2])),
       override.pvalues = list(c(ologit_threats_cse[3:16, 4], ologit_threats_cse[1:2, 4])),
       fontsize = "footnotesize",
       booktabs = TRUE,
       dcolumn = TRUE,
       float.pos = "h!",
       include.aic = FALSE,
       include.bic = FALSE,
       include.loglik	= FALSE,
       single.row = FALSE,
       label = "table:mod_threats",
       caption = "Effects of threats on crisis outcomes using ordered logit regression.",
       custom.note = "%stars. Robust clustered standard errors at the dyad level.")

mfx_a <- avg_slopes(fit_ologit_threats, 
                    variables = "prop_threat_a", 
                    vcov = ~ dyadid)
round(mfx_a$estimate[3], 2) #0.01 increase in threat_a -> 0.7 percentage point increase in pr(winning_a)

mfx_b <- avg_slopes(fit_ologit_threats, 
                    variables = "prop_threat_b", 
                    vcov = ~ dyadid)
round(mfx_b$estimate[3], 2) #0.01 increase in threat_b -> 0.89 percentage point decrease in pr(winning_a)

mfx_c <- avg_slopes(fit_ologit_threats, 
                    hypothesis = "b35 = b36", #compare A's and B's threats' effects on winning outcome
                    vcov = ~ dyadid)
cbind(mfx_c[2], mfx_c[5]) #the coefficients significantly differ from each other


#### Figure S2: Timing of anger expressions ####
X <- readRDS(here("data", "X.rds")) #author's coding of anger

X_statement <- X %>% #create statement level df
  select(crisno, title, date_issued, country, anger, threatening) %>% 
  group_by(crisno, title, date_issued, country) %>% 
  summarize(angerm = mean(anger),
            resolvem = mean(threatening)) %>% 
  ungroup()

durac <- dat_raw %>% #create duration and dates df
  select(crisno, durac_orig, sys_trig_date, sys_term_date) %>% 
  group_by(crisno) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

statement_durac <- X_statement %>% #merge the two
  left_join(durac) %>% 
  mutate(date_issued = as.Date(date_issued, format = "%m/%d/%Y")) %>% #change to date var
  mutate(made = date_issued - sys_trig_date)

durac_quart <- durac %>% 
  group_by(crisno) %>% 
  summarize(begin = ceiling(quantile(seq(1:durac_orig), probs=c(0.33))),
            end = ceiling(quantile(seq(1:durac_orig), probs=c(0.66))))

statement_quart <- statement_durac %>% 
  left_join(durac_quart) %>% 
  mutate(made_when = case_when(
    made <= begin ~ 1,
    made > begin & made <= end ~ 2,
    TRUE ~ 3
  ))

phases <- c("Beginning", "Middle", "End") #define crisis phases
statement_quart <- statement_quart %>% 
  mutate(made_when = factor(made_when, labels = phases, ordered = TRUE)) %>% 
  mutate(Beginning = ifelse(made_when == "Beginning", 1, 0),
         Middle = ifelse(made_when == "Middle", 1, 0),
         End = ifelse(made_when == "End", 1, 0))

outcom_a <- dat_raw %>% 
  filter(init_a == 1) %>% 
  select(crisno, ccode_a, outcom_ord) %>%
  mutate(country = countrycode(ccode_a, destination = "cow.name", origin = "cown")) %>% 
  select(-ccode_a) %>% 
  rename(outcom = outcom_ord)

outcom_b <- dat_raw %>% 
  filter(init_a == 0) %>% 
  select(crisno, ccode_a, outcom_ord) %>%
  mutate(country = countrycode(ccode_a, destination = "cow.name", origin = "cown")) %>% 
  select(-ccode_a) %>% 
  rename(outcom = outcom_ord) 

statement_quart_a <- statement_quart %>% 
  left_join(outcom_a, by = c("crisno", "country")) %>% 
  drop_na(.)
statement_quart_b <- statement_quart %>% 
  left_join(outcom_b, by = c("crisno", "country")) %>% 
  drop_na(.)

dv_labl <- c("Defeat", "Stalemate", "Victory")

# Mean levels of anger across phase for challengers
p_timing_a <- statement_quart_a %>% 
  group_by(made_when) %>% 
  summarize(anger = mean(angerm)) %>%
  ggplot(aes(x = factor(made_when), y = anger)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  xlab("Crisis phase") + 
  ylab("Mean anger score") + 
  ggtitle("(a) Challengers") +
  scale_x_discrete(labels = c("Beginning", "Middle", "End")) +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_fill_discrete(name = "Outcome",
                      labels = dv_labl) +
  theme_classic() +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        text = element_text(size = 6),
        axis.text=element_text(size=6, color = "black"),
        axis.title=element_text(size=6, color = "black"))


# Mean levels of anger across phase for defenders
p_timing_b <- statement_quart_b %>% 
  group_by(made_when) %>% 
  summarize(anger = mean(angerm)) %>% 
  ggplot(aes(x = factor(made_when), y = anger)) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean") +
  xlab("Crisis phase") +
  ylab("Mean anger score") + 
  ggtitle("(b) Defenders") +
  scale_x_discrete(labels = c("Beginning", "Middle", "End")) +
  scale_y_continuous(limits = c(0, 0.4)) +
  scale_fill_discrete(name = "Outcome",
                      labels = dv_labl) +
  theme_classic() +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        text = element_text(size = 6),
        axis.text=element_text(size=6, color = "black"),
        axis.title=element_text(size=6, color = "black"))

p_timing_a + p_timing_b
ggsave("figureS2.png", width = 5, height = 3, units = "in")


#### Table S9: Predicting timing of anger expressions ####
# Regression approach modeling anger expressions by phase
# Run Figure A2 first
angwhen_a <- tobit(angerm ~ Middle + End,
                   left = 0, right = 1, dist = "gaussian", data = statement_quart_a)
cov_angwhen_a <- vcovCL(angwhen_a, cluster = statement_quart_a$crisno, type = "HC0")
(cov_angwhen_a_cse <- coeftest(angwhen_a, vcov = cov_angwhen_a))

angwhen_b <- tobit(angerm ~ Middle + End,
                   left = 0, right = 1, dist = "gaussian", data = statement_quart_b)
cov_angwhen_b <- vcovCL(angwhen_b, cluster = statement_quart_b$crisno, type = "HC0")
(cov_angwhen_b_cse <- coeftest(angwhen_b, vcov = cov_angwhen_b))

screenreg(list(angwhen_a, angwhen_b),
       single.row = FALSE, 
       digits = 3,
       stars = c(0.01, 0.05, 0.1),
       booktabs = TRUE,
       dcolumn = TRUE,
       fontsize = "small",
       omit.coef = "Log",
       no.margin = TRUE,
       float.pos = "h!",
       include.aic = FALSE,
       include.bic = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       include.nobs = FALSE,
       include.wald = FALSE,
       label = "tab:score_reg",
       custom.model.names = c("Challenger's anger", "Defender's anger"),
       override.se = list(cov_angwhen_a_cse[, 2], cov_angwhen_b_cse[, 2]),
       override.pvalues = list(cov_angwhen_a_cse[, 4], cov_angwhen_b_cse[, 4]),
       caption = "Tobit models predicting challengers' and defenders' mean anger scores by phase",
       custom.note = "%stars. Robust clustered standard errors at the crisis level.")

# Do defenders make greater anger expressions?
round(mean(df$prop_anger_a),2)
round(sd(df$prop_anger_a),2)
round(mean(df$prop_anger_b),2)
round(sd(df$prop_anger_b),2)
t.test(df$prop_anger_a, df$prop_anger_b)
