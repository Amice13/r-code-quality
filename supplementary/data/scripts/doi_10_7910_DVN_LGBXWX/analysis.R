# Replication package for
#  "Constituent Communication through Telephone Town Halls"
#  Contact minozzi.1@osu.edu with questions

# Set up environment ----
library(data.table)
library(rstanarm)
library(rstan)
library(cowplot)
library(ggplot2)
library(extrafont)
library(Cairo)
source("functions.R")
suppressMessages(loadfonts())
family <- "Times"
theme_set(theme_minimal() + theme(
  plot.title = element_text(size = 18, hjust = .5),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 14),
  strip.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  text = element_text(family = family),
  panel.border = element_rect(fill = NA, color = "gray70"),
  legend.position = "bottom",
  legend.title = element_blank()))
lbs <- -.5
size <- .05
options(mc.cores = parallel::detectCores())
RNGkind("L'Ecuyer")

# Load data and classify respondents ----
load("tth_data.RData")
nonrespondents_to_pre <- tth_data[
  is.na(pre_q2) & is.na(pre_q3) & is.na(pre_q4) &
    is.na(pre_q6) & is.na(pre_q9) &
    is.na(pre_q10) & is.na(pre_q11) & is.na(pre_q12) &
    is.na(pre_q13) & is.na(pre_q14) & is.na(pre_q15) &
    is.na(pre_q16) & is.na(pre_q17), response_id]
nonrespondents_to_pst <- tth_data[
  is.na(pst_q2) & is.na(pst_q3) & is.na(pst_q4) &
    is.na(pst_q6) & is.na(pst_q9) &
    is.na(pst_q10) & is.na(pst_q11) & is.na(pst_q12) &
    is.na(pst_q13) & is.na(pst_q14) & is.na(pst_q15) &
    is.na(pst_q16) & is.na(pst_q17), response_id]
respondents_to_pre <- tth_data[
  !(is.na(pre_q2) & is.na(pre_q3) & is.na(pre_q4) &
      is.na(pre_q6) & is.na(pre_q9) &
      is.na(pre_q10) & is.na(pre_q11) & is.na(pre_q12) &
      is.na(pre_q13) & is.na(pre_q14) & is.na(pre_q15) &
      is.na(pre_q16) & is.na(pre_q17)), response_id]
respondents_to_pst <- tth_data[
  !(is.na(pst_q2) & is.na(pst_q3) & is.na(pst_q4) &
      is.na(pst_q6) & is.na(pst_q9) &
      is.na(pst_q10) & is.na(pst_q11) & is.na(pst_q12) &
      is.na(pst_q13) & is.na(pst_q14) & is.na(pst_q15) &
      is.na(pst_q16) & is.na(pst_q17)), response_id]
completes <- intersect(respondents_to_pre, respondents_to_pst)
# keep only those who responded to at least one pretest question:
tth_data <- tth_data[response_id %in% respondents_to_pre]

# code party ID
#1 = strong rep, 4 = ind/other, 7 = strong dem
tth_data[pre_q18 == 4 & pre_q21 == 2, party7 := 7]
tth_data[pre_q18 == 4 & pre_q21 == 1, party7 := 6]
tth_data[pre_q18 == 4 & is.na(pre_q21), party7 := 6]
tth_data[pre_q18 %in% 1:2 & pre_q23 == 3, party7 := 5]
tth_data[pre_q18 %in% 1:2 & pre_q23 == 1, party7 := 4]
tth_data[pre_q18 %in% 1:2 & pre_q23 == 2, party7 := 3]
tth_data[pre_q18 %in% 1:2 & is.na(pre_q23), party7 := 4]
tth_data[pre_q18 == 3 & pre_q22 == 1, party7 := 2]
tth_data[pre_q18 == 3 & is.na(pre_q22), party7 := 2]
tth_data[pre_q18 == 3 & pre_q22 == 2, party7 := 1]

# make attrition/reporting variables
tth_data[, `:=`(
  political_interest = pre_q33 / 4,
  attrited = as.numeric(!response_id %in% respondents_to_pst),
  reported_pre_q2 = !is.na(pre_q2),
  reported_pre_q3 = !is.na(pre_q3),
  reported_pre_q4 = !is.na(pre_q4),
  reported_pre_q6 = !is.na(pre_q6),
  reported_pre_q9 = !is.na(pre_q9),
  reported_party7 = !is.na(party7),
  reported_political_interest = !is.na(pre_q33),
  recoded_pre_q2 = ifelse(
    is.na(pre_q2), median((pre_q2 - 1) / 6, na.rm = TRUE), (pre_q2 - 1) / 6),
  recoded_pre_q3 = ifelse(
    is.na(pre_q3), median((pre_q3 - 1) / 6, na.rm = TRUE), (pre_q3 - 1) / 6),
  recoded_pre_q4 = ifelse(
    is.na(pre_q4), median((pre_q4 - 1) / 6, na.rm = TRUE), (pre_q4 - 1) / 6),
  recoded_pre_q6 = ifelse(
    is.na(pre_q6), median((pre_q6 - 1) / 4, na.rm = TRUE), (pre_q6 - 1) / 4),
  recoded_pre_q9 = ifelse(
    is.na(pre_q9), median((pre_q9 - 1) / 3, na.rm = TRUE), (pre_q9 - 1) / 3),
  recoded_pre_q10 = as.numeric(pre_q10 %in% 3:4),
  recoded_pst_q10 = as.numeric(pst_q10 %in% 3:4),
  recoded_pre_q11 = as.numeric(pre_q11 %in% 1:2),
  recoded_pst_q11 = as.numeric(pst_q11 %in% 1:2),
  recoded_pre_q12 = as.numeric(pre_q12 %in% 3:4),
  recoded_pst_q12 = as.numeric(pst_q12 %in% 3:4),
  recoded_pre_q13 = as.numeric(pre_q13 %in% 3:4),
  recoded_pst_q13 = as.numeric(pst_q13 %in% 3:4),
  recoded_pre_q14 = as.numeric(pre_q14 %in% 1:2),
  recoded_pst_q14 = as.numeric(pst_q14 %in% 1:2),
  recoded_pre_q15 = as.numeric(pre_q15 %in% 3:4),
  recoded_pst_q15 = as.numeric(pst_q15 %in% 3:4),
  recoded_pre_q16 = as.numeric(pre_q16 %in% 3:4),
  recoded_pst_q16 = as.numeric(pst_q16 %in% 3:4),
  recoded_pre_q17 = as.numeric(pre_q17 %in% 3:4),
  recoded_pst_q17 = as.numeric(pst_q17 %in% 3:4),
  recoded_party7 = ifelse(
    is.na(party7), median(party7, na.rm = TRUE), party7),
  recoded_political_interest = ifelse(
    is.na(pre_q33 / 4), median(pre_q33 / 4, na.rm = TRUE), pre_q33 / 4))]

# Make multilevel model data ----
mlm_tth_data <- melt(tth_data[pst_q19 == 2 & response_id %in% completes,
  .(response_id, treatment, party7, rep,
    pre_q2, pst_q2, pre_q3, pst_q3, pre_q4, pst_q4,
    pre_q6, pst_q6, pre_q9, pst_q9,
    pre_q10, pst_q10, pre_q11, pst_q11, pre_q12, pst_q12, pre_q13, pst_q13,
    pre_q14, pst_q14, pre_q15, pst_q15, pre_q16, pst_q16, pre_q17, pst_q17
  )],
  id.vars = c("response_id", "treatment", "party7", "rep"))
mlm_tth_data[, y := as.numeric(value)]
mlm_tth_data[, post := as.numeric(variable %like% "pst")]
mlm_tth_data[, question := substring(variable, 6, 7)]
mlm_tth_data[question %in% 2:4, y := (y - 1) / 6]
mlm_tth_data[question == 6, y := (y - 1) / 4]
mlm_tth_data[question == 9, y := (y - 1) / 3]
mlm_tth_data[question %in% c(10, 12, 13, 15:17),
  y := as.numeric(y %in% 3:4)]
mlm_tth_data[question %in% c(11, 14), y := as.numeric(y %in% 1:2)]
mlm_tth_data[question %in% 2:4, group := "Attitudes toward TTH"]
mlm_tth_data[question %in% c(6, 9), group := "Trust and Approval"]
mlm_tth_data[question %in% 10:17, group := "Presentation of Self"]
mlm_tth_data <- mlm_tth_data[!is.na(y)]

# Make fixed effects data ----
fe_tth_data <- rbind(
  melt(na.omit(tth_data[,
    .(response_id, rep, treatment,
      pre_q2, pst_q2, pre_q3, pst_q3, pre_q4, pst_q4)]),
    id.vars = c("response_id", "rep", "treatment")),
  melt(na.omit(tth_data[,
    .(response_id, rep, treatment,
      pre_q6, pst_q6, pre_q9, pst_q9)]),
    id.vars = c("response_id", "rep", "treatment")),
  melt(tth_data[response_id %in% completes, .(
    response_id, rep, treatment,
      pre_q10, pst_q10, pre_q11, pst_q11, pre_q12, pst_q12, pre_q13, pst_q13,
      pre_q14, pst_q14, pre_q15, pst_q15, pre_q16, pst_q16, pre_q17, pst_q17)],
    id.vars = c("response_id", "rep", "treatment")))
fe_tth_data[, y := as.numeric(value)]
fe_tth_data[, post := as.numeric(variable %like% "pst")]
fe_tth_data[, question := substring(variable, 6, 7)]
fe_tth_data[question %in% 2:4, y := (y - 1) / 6]
fe_tth_data[question == 6, y := (y - 1) / 4]
fe_tth_data[question == 9, y := (y - 1) / 3]
fe_tth_data[question %in% c(10, 12, 13, 15:17),
  y := as.numeric(y %in% 3:4)]
fe_tth_data[question %in% c(11, 14), y := as.numeric(y %in% 1:2)]
fe_tth_data[question %in% 2:4, group := "Attitudes toward TTH"]
fe_tth_data[question %in% c(6, 9), group := "Trust and Approval"]
fe_tth_data[question %in% 10:17, group := "Presentation of Self"]
fe_tth_data <- merge(fe_tth_data, tth_data[, .(response_id, party7)],
  by = "response_id", sort = FALSE)

# Estimate attrition model ----
set.seed(878260456)
sm_attrit <- stan_glmer(
  attrited ~
    recoded_party7 +
    recoded_political_interest +
    recoded_pre_q2 + recoded_pre_q3 + recoded_pre_q4 +
    recoded_pre_q6 + recoded_pre_q9 +
    recoded_pre_q10 + recoded_pre_q11 + recoded_pre_q12 +
    recoded_pre_q13 + recoded_pre_q14 + recoded_pre_q15 +
    recoded_pre_q16 + recoded_pre_q17 +
    reported_party7 +
    reported_political_interest +
    reported_pre_q2 + reported_pre_q3 + reported_pre_q4 +
    reported_pre_q6 + reported_pre_q9 +
    (1 | rep),
  data = tth_data, family = binomial,
  prior = normal(), prior_intercept = normal(),
  prior_aux = exponential(), prior_covariance = decov(), prior_PD = FALSE,
  adapt_delta = .99)
summary(sm_attrit, probs = c(.025, .975))

# Estimate multilevel main models ----
fmla <- y ~ treatment * post + (1 | response_id) + (1 | question) + (1 | rep)
set.seed(1915914517)
sm1 <- stan_lmer(fmla, data = mlm_tth_data[group %like% "TTH"],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm2 <- stan_lmer(fmla, data = mlm_tth_data[group %like% "Trust"],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm3 <- stan_lmer(fmla, data = mlm_tth_data[group %like% "Self"],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)

# Estimate multilevel models by party ----
set.seed(1942134414)
sm1_dem <- stan_lmer(fmla,
  data = mlm_tth_data[group %like% "TTH" & party7 > 4],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm2_dem <- stan_lmer(fmla,
  data = mlm_tth_data[group %like% "Trust" & party7 > 4],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm3_dem <- stan_lmer(fmla,
  data = mlm_tth_data[group %like% "Self" & party7 > 4],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm1_rep <- stan_lmer(fmla,
  data = mlm_tth_data[group %like% "TTH" & party7 < 4],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm2_rep <- stan_lmer(fmla,
  data = mlm_tth_data[group %like% "Trust" & party7 < 4],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm3_rep <- stan_lmer(fmla,
  data = mlm_tth_data[group %like% "Self" & party7 < 4],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)

# Estimate multilevel models with only lo responders on pretest ----
lo_response_data <- merge(
  mlm_tth_data[post == 0 & y <= .5, .(response_id, question)],
  mlm_tth_data, by = c("response_id", "question"))
set.seed(2016002091)
sm1lo <- stan_lmer(fmla,
  data = lo_response_data[group %like% "TTH"],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm2lo <- stan_lmer(fmla,
  data = lo_response_data[group %like% "Trust"],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)
sm3lo <- stan_lmer(fmla,
  data = lo_response_data[group %like% "Self"],
  prior = normal(), prior_intercept = normal(), prior_aux = exponential(),
  prior_covariance = decov(), prior_PD = FALSE, adapt_delta = .99)

# Estimate fixed effects models ----
fmla <- y ~ treatment * post +
  factor(response_id) + factor(question) - 1 - treatment
sm1fe <- lm(fmla, data = fe_tth_data[group %like% "TTH"])
sm2fe <- lm(fmla, data = fe_tth_data[group %like% "Trust"])
sm3fe <- lm(fmla, data = fe_tth_data[group %like% "Self"])

# Estimate fixed effects models, question-by-question ----
fmla <- y ~ treatment * post + factor(response_id) - treatment - 1
vars <- c("post", "treatment:post")
sm1feq1 <- lm(fmla, data = fe_tth_data[question == 2])
sm1feq2 <- lm(fmla, data = fe_tth_data[question == 3])
sm1feq3 <- lm(fmla, data = fe_tth_data[question == 4])
sm2feq1 <- lm(fmla, data = fe_tth_data[question == 6])
sm2feq2 <- lm(fmla, data = fe_tth_data[question == 9])
sm3feq1 <- lm(fmla, data = fe_tth_data[question == 10])
sm3feq2 <- lm(fmla, data = fe_tth_data[question == 11])
sm3feq3 <- lm(fmla, data = fe_tth_data[question == 12])
sm3feq4 <- lm(fmla, data = fe_tth_data[question == 13])
sm3feq5 <- lm(fmla, data = fe_tth_data[question == 14])
sm3feq6 <- lm(fmla, data = fe_tth_data[question == 15])
sm3feq7 <- lm(fmla, data = fe_tth_data[question == 16])
sm3feq8 <- lm(fmla, data = fe_tth_data[question == 17])

# Estimate fixed effects models, party moderators ----
sm1fe_dem <- lm(fmla, data = fe_tth_data[group %like% "TTH" & party7 > 4])
sm2fe_dem <- lm(fmla, data = fe_tth_data[group %like% "Trust" & party7 > 4])
sm3fe_dem <- lm(fmla, data = fe_tth_data[group %like% "Self" & party7 > 4])

sm1fe_rep <- lm(fmla, data = fe_tth_data[group %like% "TTH" & party7 < 4])
sm2fe_rep <- lm(fmla, data = fe_tth_data[group %like% "Trust" & party7 < 4])
sm3fe_rep <- lm(fmla, data = fe_tth_data[group %like% "Self" & party7 < 4])

# Make Figure 1 ----
vars <- c("Attitudes toward\nTelephone Town Halls",
  "Trust and Approval",
  "Presentation of Self")
x1 <- summary(sm1, pars = "(Intercept)", digits = 2, prob = c(.025, .975))
x2 <- summary(sm2, pars = "(Intercept)", digits = 2, prob = c(.025, .975))
x3 <- summary(sm3, pars = "(Intercept)", digits = 2, prob = c(.025, .975))
ggdata0 <- data.table(
  x = factor(vars, levels = vars),
  y = c(x1[1], x2[1], x3[1]),
  yL = c(x1[4], x2[4], x3[4]),
  yH = c(x1[5], x2[5], x3[5]))
g <- ggplot(ggdata0, aes(x = x, y = y, ymin = yL, ymax = yH)) +
  geom_hline(yintercept = 0, color = "gray", linetype = 3) +
  geom_errorbar(width = 0, size = 1.4) + geom_point(size = 3) +
  coord_cartesian(ylim = c(0, 1)) +
  xlab("") +
  ylab("Baseline Attitudes") +
  ggtitle("Baseline Attitudes toward the Events and Members")
save_figure(g, "figure-baseline.tiff", width = 6.5, height = 6.5 * .75)

# Make Figure 2 ----
x1 <- summary(sm1, regex_pars = "^post", digits = 2, prob = c(.025, .975))
x2 <- summary(sm2, regex_pars = "^post", digits = 2, prob = c(.025, .975))
x3 <- summary(sm3, regex_pars = "^post", digits = 2, prob = c(.025, .975))
ggdata1 <- data.table(
  x = factor(vars, levels = vars),
  y = c(x1[1], x2[1], x3[1]),
  yL = c(x1[4], x2[4], x3[4]),
  yH = c(x1[5], x2[5], x3[5]))
g <- ggplot(ggdata1, aes(x, y, ymin = yL, ymax = yH)) +
  geom_hline(yintercept = 0, color = "gray", linetype = 3) +
  geom_errorbar(width = 0, size = 1.4) + geom_point(size = 3) +
  coord_cartesian(ylim = c(0, .3)) +
  xlab("") +
  ylab("Effects of Participating in Control Events") +
  ggtitle(paste0("Participation in Telephone Town Halls\nImproves Attitudes ",
    "toward the Events and Members"))
save_figure(g, "figure-pre-post.tiff", width = 6.5, height = 6.5 * .75)

# Make Figure 3 ----
ex1 <- rstan::extract(sm1$stanfit)
ex2 <- rstan::extract(sm2$stanfit)
ex3 <- rstan::extract(sm3$stanfit)
ggdata2 <- cbind(CJ(
  gr = factor(c("Standard", "Modified"), levels = c("Standard", "Modified")),
  x = factor(vars, levels = vars)),
  rbind(
    summarize(ex1$beta[, 2]),
    summarize(ex2$beta[, 2]),
    summarize(ex3$beta[, 2]),
    summarize(rowSums(ex1$beta[, 2:3])),
    summarize(rowSums(ex2$beta[, 2:3])),
    summarize(rowSums(ex3$beta[, 2:3]))))
setnames(ggdata2, c("2.5%", "97.5%"), c("yL", "yH"))
g <- ggplot(ggdata2, aes(x, y, ymin = yL, ymax = yH, group = gr, fill = gr)) +
  geom_hline(yintercept = 0, color = "gray", linetype = 3) +
  geom_errorbar(width = 0, size = 1.4, position = position_dodge(width = .3)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = .3)) +
  coord_cartesian(ylim = c(-.025, .3)) +
  xlab("") +
  ylab("Post - Pre") +
  ggtitle(paste0("Modified Town Halls Enhanced Attitudes toward the Events,\n",
    "But Not toward Members")) +
  scale_fill_manual(values = c("Standard" = "black", "Modified" = "white"))
save_figure(g, "figure-treat.tiff", width = 7, height = 6.5 * .7)

# Calculate quantities reported in text ----
# total who opened survey
tth_data[, length(unique(response_id))]
# ...responded to both
length(intersect(respondents_to_pre, respondents_to_pst))
# sample sizes in each group
mlm_tth_data[!is.na(y), length(unique(response_id)), treatment]
# predicting attrition, in-sample ROC is 0.67
pred_probs <- colMeans(posterior_predict(sm_attrit))
ROCR::performance(ROCR::prediction(pred_probs, tth_data[, attrited]),
  "auc")@y.values[[1]]
# tth attitudes post effect:
summary(sm1, regex_pars = "^post", digits = 2, prob = c(.025, .975))
# cohen's d for post effect on attitudes toward TTH
fixef(sm1)["post"] / mlm_tth_data[group %like% "TTH", sd(y, na.rm = TRUE)]
# summary eval post effect:
summary(sm2, regex_pars = "^post", digits = 2, prob = c(.025, .975))
# cohen's d for post effect on trust and approval
fixef(sm2)["post"] / mlm_tth_data[group %like% "Trust", sd(y, na.rm = TRUE)]
# presentation of self post effect:
summary(sm3, regex_pars = "^post", digits = 2, prob = c(.025, .975))
# cohen's d for post effect on presentation of self
fixef(sm3)["post"] / mlm_tth_data[group %like% "Self", sd(y, na.rm = TRUE)]
#   tth attitudes treatment effect:
summary(sm1, regex_pars = "treatment:post", digits = 2, prob = c(.025, .975))
# cohen's d for treatment effect on attitudes toward TTH
fixef(sm1)["treatment:post"] /
  mlm_tth_data[group %like% "TTH", sd(y, na.rm = TRUE)]
# summary eval treatment effect:
summary(sm2, regex_pars = "treatment:post", digits = 2, prob = c(.025, .975))
# cohen's d for treatment effect on trust and approval (not reported in text)
fixef(sm2)["treatment:post"] /
  mlm_tth_data[group %like% "Trust", sd(y, na.rm = TRUE)]
# presentation of self treatment effect:
summary(sm3, regex_pars = "treatment:post", digits = 2, prob = c(.025, .975))
# cohen's d for treatment effect on presentation of self (not reported in text)
fixef(sm3)["treatment:post"] /
  mlm_tth_data[group %like% "Self", sd(y, na.rm = TRUE)]

# Attrition (Appendix Table A1) ----
make_table_objects(sm_attrit)
vars <- c("Party ID", "Political Interest", "TTHs Good to Hear Views",
  "TTHs Good to Communicate Positions", "TTHs Good to Explain Actions",
  "Approve of MC", "Trust MC", "MC Compassionate", "MC Dishonest (rev.)",
  "MC Fair", "MC Knowledgeable", "MC Weak (rev.)", "MC Accessible",
  "MC Qualified", "MC Understand People Like Me", "Intercept")
cbind(interleave(vars, rep("", 16)),
  interleave(B_sm_attrit[c(2:16, 23)], S_sm_attrit[c(2:16, 23)]))
nobs(sm_attrit)
ngrps(sm_attrit)
SD_sm_attrit[1] # error term for MCs

# Descriptive Stats and Balance (Appendix Table A2) ----
tth_data[response_id %in% completes, rbind(
  calc_balance((party7-1)/6, treatment, "Party ID"),
  calc_balance(political_interest, treatment, "Political Interest"),
  calc_balance((pre_q2-1)/6, treatment, "TTHs Good to Hear Views"),
  calc_balance((pre_q3-1)/6, treatment, "TTHs Good to Communicate Positions"),
  calc_balance((pre_q4-1)/6, treatment, "TTHs Good to Explain Actions"),
  calc_balance((pre_q6-1)/4, treatment, "Approve of MC"),
  calc_balance((pre_q9-1)/3, treatment, "Trust MC"),
  calc_balance(recoded_pre_q10, treatment, "MC Compassionate"),
  calc_balance(recoded_pre_q11, treatment, "MC Dishonest (rev.)"),
  calc_balance(recoded_pre_q12, treatment, "MC Fair"),
  calc_balance(recoded_pre_q13, treatment, "MC Knowledgeable"),
  calc_balance(recoded_pre_q14, treatment, "MC Weak (rev.)"),
  calc_balance(recoded_pre_q15, treatment, "MC Accessible"),
  calc_balance(recoded_pre_q16, treatment, "MC Qualified"),
  calc_balance(recoded_pre_q17, treatment, "MC Understand People Like Me"))]

# Main model tables (Appendix Table A3) ----
make_table_objects(sm1)
make_table_objects(sm2)
make_table_objects(sm3)
vars <- c("Treatment", "Post", "Treatment \u00d7 Post", "Intercept")
rbind(
  cbind(
    interleave(vars, rep("", 4)),
    interleave(B_sm1[c(2:4, 1)], S_sm1[c(2:4, 1)]),
    interleave(B_sm2[c(2:4, 1)], S_sm2[c(2:4, 1)]),
    interleave(B_sm3[c(2:4, 1)], S_sm3[c(2:4, 1)])),
  c("", nobs(sm1), nobs(sm2), nobs(sm3)),
  cbind("", ngrps(sm1), ngrps(sm2), ngrps(sm3)),
  cbind("", SD_sm1, SD_sm2, SD_sm3[c(1, 3, 2, 4)]))

# Fixed effects models (Table A4) ----
rbind(
  cbind(
    fe_tth_data[group %like% "TTH"][, mclx(sm1fe, 1, rep, question)],
    fe_tth_data[group %like% "Trust"][, mclx(sm2fe, 1, rep, question)],
    fe_tth_data[group %like% "Self"][, mclx(sm3fe, 1, rep, question)]),
  fe_tth_data[, .N, group][, N],
  fe_tth_data[, length(unique(response_id)), group][, V1])

# Models with Lo Pretest Respondents (Appendix Table A5) ----
make_table_objects(sm1lo)
make_table_objects(sm2lo)
make_table_objects(sm3lo)
vars <- c("Treatment", "Post", "Treatment \u00d7 Post", "Intercept")
rbind(
  cbind(
    interleave(vars, rep("", 4)),
    interleave(B_sm1lo[c(2:4, 1)], S_sm1lo[c(2:4, 1)]),
    interleave(B_sm2lo[c(2:4, 1)], S_sm2lo[c(2:4, 1)]),
    interleave(B_sm3lo[c(2:4, 1)], S_sm3lo[c(2:4, 1)])),
  c("", nobs(sm1lo), nobs(sm2lo), nobs(sm3lo)),
  cbind("", ngrps(sm1lo), ngrps(sm2lo), ngrps(sm3lo)),
  cbind("", SD_sm1lo, SD_sm2lo, SD_sm3lo[c(1, 3, 2, 4)]))

# Question-by-question fixed effects models (Appendix Table A6) ----
rbind(
  fe_tth_data[question == 2][, mclx(sm1feq1, 1, rep, vec = FALSE)],
  fe_tth_data[question == 3][, mclx(sm1feq2, 1, rep, vec = FALSE)],
  fe_tth_data[question == 4][, mclx(sm1feq3, 1, rep, vec = FALSE)],
  fe_tth_data[question == 6][, mclx(sm2feq1, 1, rep, vec = FALSE)],
  fe_tth_data[question == 9][, mclx(sm2feq2, 1, rep, vec = FALSE)],
  fe_tth_data[question == 10][, mclx(sm3feq1, 1, rep, vec = FALSE)],
  fe_tth_data[question == 11][, mclx(sm3feq2, 1, rep, vec = FALSE)],
  fe_tth_data[question == 12][, mclx(sm3feq3, 1, rep, vec = FALSE)],
  fe_tth_data[question == 13][, mclx(sm3feq4, 1, rep, vec = FALSE)],
  fe_tth_data[question == 14][, mclx(sm3feq5, 1, rep, vec = FALSE)],
  fe_tth_data[question == 15][, mclx(sm3feq6, 1, rep, vec = FALSE)],
  fe_tth_data[question == 16][, mclx(sm3feq7, 1, rep, vec = FALSE)],
  fe_tth_data[question == 17][, mclx(sm3feq8, 1, rep, vec = FALSE)])

# Models for Democrats (Appendix Table A7)----
make_table_objects(sm1_dem)
make_table_objects(sm2_dem)
make_table_objects(sm3_dem)
vars <- c("Treatment", "Post", "Treatment \u00d7 Post", "Intercept")
rbind(
  cbind(
    interleave(vars, rep("", 4)),
    interleave(B_sm1_dem[c(2:4, 1)], S_sm1_dem[c(2:4, 1)]),
    interleave(B_sm2_dem[c(2:4, 1)], S_sm2_dem[c(2:4, 1)]),
    interleave(B_sm3_dem[c(2:4, 1)], S_sm3_dem[c(2:4, 1)])),
  c("", nobs(sm1_dem), nobs(sm2_dem), nobs(sm3_dem)),
  cbind("", ngrps(sm1_dem), ngrps(sm2_dem), ngrps(sm3_dem)[c(1, 3, 2)]),
  cbind("", SD_sm1_dem, SD_sm2_dem, SD_sm3_dem[c(1, 3, 2, 4)]))

# Models for Republicans (Appendix Table A8)----
make_table_objects(sm1_rep)
make_table_objects(sm2_rep)
make_table_objects(sm3_rep)
vars <- c("Treatment", "Post", "Treatment \u00d7 Post", "Intercept")
rbind(
  cbind(
    interleave(vars, rep("", 4)),
    interleave(B_sm1_rep[c(2:4, 1)], S_sm1_rep[c(2:4, 1)]),
    interleave(B_sm2_rep[c(2:4, 1)], S_sm2_rep[c(2:4, 1)]),
    interleave(B_sm3_rep[c(2:4, 1)], S_sm3_rep[c(2:4, 1)])),
  c("", nobs(sm1_rep), nobs(sm2_rep), nobs(sm3_rep)),
  cbind("", ngrps(sm1_rep), ngrps(sm2_rep), ngrps(sm3_rep)[c(1, 3, 2)]),
  cbind("", SD_sm1_rep, SD_sm2_rep, SD_sm3_rep[c(1, 3, 2, 4)]))

# FE Models for Democrats (Appendix Table A9) ----
rbind(
  cbind(
    fe_tth_data[group %like% "TTH" & party7 > 4][,
      mclx(sm1fe_dem, 1, rep, question)],
    fe_tth_data[group %like% "Trust" & party7 > 4][,
      mclx(sm2fe_dem, 1, rep, question)],
    fe_tth_data[group %like% "Self" & party7 > 4][,
      mclx(sm3fe_dem, 1, rep, question)]),
  fe_tth_data[party7 > 4, .N, group][, N],
  fe_tth_data[party7 > 4, length(unique(response_id)), group][, V1])

# FE Models for Republicans (Appendix Table A10) ----
rbind(
  cbind(
    fe_tth_data[group %like% "TTH" & party7 < 4][,
      mclx(sm1fe_rep, 1, rep, question)],
    fe_tth_data[group %like% "Trust" & party7 < 4][,
      mclx(sm2fe_rep, 1, rep, question)],
    fe_tth_data[group %like% "Self" & party7 < 4][,
      mclx(sm3fe_rep, 1, rep, question)]),
  fe_tth_data[party7 < 4, .N, group][, N],
  fe_tth_data[party7 < 4, length(unique(response_id)), group][, V1])
