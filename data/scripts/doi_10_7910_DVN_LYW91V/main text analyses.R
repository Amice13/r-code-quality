#----------------------------------------------------------------------------------------------------#
# Code for Main Text Analyses (Results 2 [YouGov Analysis]-5)
# Engelhardt, Feldman, and Hetherington "Advancing the Measurement of Authoritarianism"
#----------------------------------------------------------------------------------------------------#
library(lavaan)
library(semTools)
library(irtoys)
library(ltm)
library(ggplot2)
library(ggthemes)
library(stringr)
library(psych)
library(survey)


# Set working directory
# setwd("")

# Loading Data ---------------------------------------------
anes1213 <- read.csv("anes1213_panel.csv")
yougov <- read.csv("yougov_panel.csv")
anes16 <- read.csv("anes16.csv")
qual_dat <- read.csv("qualtrics_study.csv")

# Utility function -----------------------------------------
source("plot_function.R")


# Results 2 -----------------------------------------------------------------
# * Defining SEMs ---------------------------------------------------------

time_struct_pid <- '
# Partiasanship
pid_17 =~ pid7_new_17 + ft_dems_both_17 + ft_reps_both_17

# Authoritarianism
auth_17 =~ w1_obey + v2*w1_manners + v3*w1_respect + v4*w1_behave
auth_18 =~ w2_obey + v2*w2_manners + v3*w2_respect + v4*w2_behave

w1_obey ~~ w2_obey
w1_manners ~~ w2_manners
w1_respect ~~ w2_respect
w1_behave ~~ w2_behave

w1_obey | a1*t1
w2_obey | a1*t1
w1_manners | b1*t1
w2_manners | b1*t1
w1_respect | c1*t1
w2_respect | c1*t1
w1_behave | d1*t1
w2_behave | d1*t1

#### Structural
auth_18 ~ auth_17 + pid_17
'

time_struct_antidem <- '
# anti-democratic attitudes
anti_dem_17 =~ leader_good_17 + leader_good_17 +  experts_bad_17 +  military_good_17

# Authoritarianism
auth_17 =~ w1_obey + v2*w1_manners + v3*w1_respect + v4*w1_behave
auth_18 =~ w2_obey + v2*w2_manners + v3*w2_respect + v4*w2_behave

w1_obey ~~ w2_obey
w1_manners ~~ w2_manners
w1_respect ~~ w2_respect
w1_behave ~~ w2_behave

w1_obey | a1*t1
w2_obey | a1*t1
w1_manners | b1*t1
w2_manners | b1*t1
w1_respect | c1*t1
w2_respect | c1*t1
w1_behave | d1*t1
w2_behave | d1*t1

#### Structural
auth_18 ~ auth_17 + anti_dem_17
'

time_struct_trump <- '
# Trump Feelings
trump_17 =~ ft_trump_17

# Authoritarianism
auth_17 =~ w1_obey + v2*w1_manners + v3*w1_respect + v4*w1_behave
auth_18 =~ w2_obey + v2*w2_manners + v3*w2_respect + v4*w2_behave

w1_obey ~~ w2_obey
w1_manners ~~ w2_manners
w1_respect ~~ w2_respect
w1_behave ~~ w2_behave

w1_obey | a1*t1
w2_obey | a1*t1
w1_manners | b1*t1
w2_manners | b1*t1
w1_respect | c1*t1
w2_respect | c1*t1
w1_behave | d1*t1
w2_behave | d1*t1

#### Structural
auth_18 ~ auth_17 + trump_17
'


# * Table 1 (YouGov Results) -----------------------------------------------------
# specifying fit values to report
FIT <- c("chisq.scaled","df","cfi.scaled",   "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")
# specifying variables to treat as ordered
VARS <- c("w1_obey", "w1_manners", "w1_respect", "w1_behave",
          "w2_obey", "w2_manners", "w2_respect", "w2_behave",
          "pid7_new_17")

fit_struct_pid <- sem(time_struct_pid, yougov, mimic = "Mplus", ordered = VARS)
summary(fit_struct_pid, standardized = T)
fitMeasures(fit_struct_pid)[FIT]

# specifying variables to treat as ordered
VARS <- c("w1_obey", "w1_manners", "w1_respect", "w1_behave",
          "w2_obey", "w2_manners", "w2_respect", "w2_behave",
          "leader_good_17", "experts_bad_17", "military_good_17")
fit_struct_antidem <- sem(time_struct_antidem, yougov, mimic = "Mplus", ordered = VARS)
summary(fit_struct_antidem, standardized = T)
fitMeasures(fit_struct_antidem)[FIT]

# specifying variables to treat as ordered
VARS <- c("w1_obey", "w1_manners", "w1_respect", "w1_behave",
          "w2_obey", "w2_manners", "w2_respect", "w2_behave")
fit_struct_trump <- sem(time_struct_trump, yougov, mimic = "Mplus", ordered = VARS)
summary(fit_struct_trump, standardized = T)
fitMeasures(fit_struct_trump)[FIT]


# Results 3 [Table 2] -----------------------------------------------------------------
# * ANES 2012-2013 [main text and appendix Table A2] ----------------------------------------------------------
cor(subset(anes1213, select = c("auth_sc_12", "auth_sc_13")),
    use = "complete.obs")

FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")
VARS <- c("obey_12", "manners_12", "respect_12", "behave_12",
          "obey_13", "manners_13", "respect_13", "behave_13")
time.mod <- '
auth_12 =~ NA*obey_12 + v1*obey_12 + v2*manners_12 + v3*respect_12 + v4*behave_12
auth_13 =~ NA*obey_13 + v1*obey_13 + v2*manners_13 + v3*respect_13 + v4*behave_13

auth_12 ~~ 1*auth_12
auth_13 ~~ 1*auth_13
obey_12 ~~ obey_13
manners_12 ~~ manners_13
respect_12 ~~ respect_13
behave_12 ~~ behave_13

obey_12 | a1*t1
obey_13 | a1*t1
manners_12 | b1*t1
manners_13 | b1*t1
respect_12 | c1*t1
respect_13 | c1*t1
behave_12 | d1*t1
behave_13 | d1*t1
'
fit.waves <- cfa(time.mod, anes1213, mimic = "Mplus", ordered = VARS)
summary(fit.waves, standardized = T)
fitMeasures(fit.waves)[FIT]

# * YouGov Panel [main text and appendix Table A3] ----------------------------------------------------------
cor(subset(yougov, select = c("auth_w1_sc", "auth_w2_sc")),
    use = "complete.obs")

FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")
VARS <- c("w1_obey", "w1_manners", "w1_respect", "w1_behave",
          "w2_obey", "w2_manners", "w2_respect", "w2_behave")
time.mod <- '
auth_w1 =~ NA*w1_obey + v1*w1_obey + v2*w1_manners + v3*w1_respect + v4*w1_behave
auth_w2 =~ NA*w2_obey + v1*w2_obey + v2*w2_manners + v3*w2_respect + v4*w2_behave

auth_w1 ~~ 1*auth_w1
auth_w2 ~~ 1*auth_w2
w1_obey ~~ w2_obey
w1_manners ~~ 0*w2_manners
w1_respect ~~ w2_respect
w1_behave ~~ w2_behave

w1_obey | a1*t1
w2_obey | a1*t1
w1_manners | b1*t1
w2_manners | b1*t1
w1_respect | c1*t1
w2_respect | c1*t1
w1_behave | d1*t1
w2_behave | d1*t1
'
fit.waves <- cfa(time.mod, yougov, mimic = "Mplus", ordered = VARS)
summary(fit.waves, standardized = T)
fitMeasures(fit.waves)[FIT]

# Results 4 -----------------------------------------------------------------
# * 2016 IRT --------------------------------------------------------------
d <- svydesign(~1, strata = ~strata.full, 
                       data = anes16, 
                       weights = ~weight.full)
prop.table(svytable(~auth_sc, d))
prop.table(table(anes16$auth_sc))

# ** Figure 1: Item Response Curves -----------------------------------------------------------------
IRT.VARS <- c("obey", "manners", "respect", "behave")

irt_dat_anes16 <- subset(anes16, select = IRT.VARS)

# Items
irt_fit_a16 <- est(irt_dat_anes16, model="2PL", engine="ltm")
irt_fit_a16$est

sort(irt_fit_a16$est[,1], decreasing = T)
sort(irt_fit_a16$est[,2])

irf_dat <- data.frame(x = irf(irt_fit_a16)$x,
                      f = c(irf(irt_fit_a16)$f[,1],
                            irf(irt_fit_a16)$f[,2],
                            irf(irt_fit_a16)$f[,3],
                            irf(irt_fit_a16)$f[,4]),
                      var = c(rep(colnames(irf(irt_fit_a16)$f)[1],
                                  length(irf(irt_fit_a16)$f[,1])),
                              rep(colnames(irf(irt_fit_a16)$f)[2],
                                  length(irf(irt_fit_a16)$f[,2])),
                              rep(colnames(irf(irt_fit_a16)$f)[3],
                                  length(irf(irt_fit_a16)$f[,3])),
                              rep(colnames(irf(irt_fit_a16)$f)[4],
                                  length(irf(irt_fit_a16)$f[,4]))),
                      diff = c(rep(irt_fit_a16$est[,2][1],
                                   length(irf(irt_fit_a16)$f[,1])),
                               rep(irt_fit_a16$est[,2][2],
                                   length(irf(irt_fit_a16)$f[,2])),
                               rep(irt_fit_a16$est[,2][3],
                                   length(irf(irt_fit_a16)$f[,3])),
                               rep(irt_fit_a16$est[,2][4],
                                   length(irf(irt_fit_a16)$f[,4]))),
                      y = 0,
                      yend = .5)
irf_dat$var <- str_to_title(irf_dat$var)
irf_dat$diff_x <- as.character(round(irf_dat$diff, 2))
irf_dat$diff_x <- str_replace(irf_dat$diff_x, "^0+", "")
irf_dat$diff_x <- str_replace(irf_dat$diff_x, "^-0+", "-")

ggplot(irf_dat, aes(x = x, y = f, group = var)) +
  geom_segment(aes(x = diff, xend = diff, y = y, yend = yend),
               color = "grey") +
  geom_line(size = 1, aes(linetype = var)) +
  geom_text(aes(x = diff, y = -0.02, label = diff_x)) +
  theme_hc() +
  labs(x = "Authoritarianism", y = "Probability of an\nAuthoritarian Response") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "right")
# ggsave("~/desktop/fig1.pdf", height = 6, width = 9)



# * Qualtrics Study -------------------------------------------------------
# ** CFA Fit [Main text and appendix Table A5] --------------------------------------------------------------
# Desired Fit Measures
FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")

# Authoritarianism Variables
vars <- c("obedience", "manners", "behaved", "orderly", "polite", 
          "respect", "loyal", "discip")

# Defining and Estimating CFA
mod_auth <- '
auth =~ NA*obedience + manners + behaved + polite + respect + discip + loyal + orderly
auth ~~ 1*auth

# error covariance given similar content
polite ~~ respect
'
fit_auth <- cfa(mod_auth, qual_dat, ordered = vars)
summary(fit_auth, standardized = T)

# Reported Fit Statistics
fitMeasures(fit_auth)[FIT]

# ** Table 4: IRT Model Estimation ----------------------------------------------------
vars <- c("obedience", "manners", "behaved", "orderly", "polite", 
          "respect", "loyal", "discip")
irt_data <- subset(qual_dat, select = vars)

# estimating 2PL Model
irt_fit_auth <- est(irt_data, model="2PL", engine="ltm")
irt_fit_auth$est

# Storing Estimates
item_estimates <- round(irt_fit_auth$est[,1:2], 3)
colnames(item_estimates) <- c("Discrimination", "Difficulty")

item_estimates

# Standard Errors
item_ses <- round(irt_fit_auth$se[,1:2], 3)
colnames(item_ses) <- c("Discrimination", "Difficulty")
rownames(item_ses) <- rownames(item_estimates)

item_ses

# ** Figure 2: Item Response Curves -------------------------------------------------
# creating data set for plotting
irf_dat <- data.frame(x = irf(irt_fit_auth$est)$x,
                      y = c(irf(irt_fit_auth$est)$f[, "obedience"],
                            irf(irt_fit_auth$est)$f[, "manners"],
                            irf(irt_fit_auth$est)$f[, "respect"],
                            irf(irt_fit_auth$est)$f[, "behaved"],
                            irf(irt_fit_auth$est)$f[, "orderly"],
                            irf(irt_fit_auth$est)$f[, "polite"],
                            irf(irt_fit_auth$est)$f[, "loyal"],
                            irf(irt_fit_auth$est)$f[, "discip"]),
                      item = c(rep("Obedience", length(irf(irt_fit_auth$est)$x)),
                               rep("Good Manners", length(irf(irt_fit_auth$est)$x)),
                               rep("Respect", length(irf(irt_fit_auth$est)$x)),
                               rep("Well-Behaved", length(irf(irt_fit_auth$est)$x)),
                               rep("Orderly", length(irf(irt_fit_auth$est)$x)),
                               rep("Polite", length(irf(irt_fit_auth$est)$x)),
                               rep("Loyal", length(irf(irt_fit_auth$est)$x)),
                               rep("Disciplined", length(irf(irt_fit_auth$est)$x))),
                      version = c(rep("Old", length(irf(irt_fit_auth$est)$x)*4),
                                  rep("New", length(irf(irt_fit_auth$est)$x)*4)))

# plotting
ggplot(irf_dat, aes(x = x, y = y, group = item)) +
  geom_line(size = 1, aes(linetype = item, color = version)) +
  #facet_wrap(~item, ncol = 4) +
  theme_hc() +
  scale_color_manual(values = c("black", "lightgrey")) +
  scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "right",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 20),
        strip.background = element_blank()) +
  labs(title = "",
       x = "Authoritarianism",
       y = "Probability of an\nAuthoritarian Response")
# ggsave("~/desktop/fig2.pdf", width = 9, height = 6)


# ** Figure 3: Test Information ---------------------------------------
Items <- c("Obedience", "Good Manners", "Well-Behaved", "Orderly", 
           "Polite", "Respect", "loyal", "Disciplined")

auth4 <- c("obedience", "manners", "respect", "behaved")

info_dat <- data.frame(x = c(tif(irt_fit_auth$est)$x,
                             tif(irt_fit_auth$est[auth4,])$x),
                       y = c(tif(irt_fit_auth$est)$f,
                             tif(irt_fit_auth$est[auth4,])$f),
                       measure = c(rep("Eight-Item", length(tif(irt_fit_auth$est)$x)),
                                   rep("Four-Item", length(tif(irt_fit_auth$est)$x))))

ggplot(info_dat, aes(x = x, y = y, group = measure)) +
  geom_line(size = 1, aes(linetype = measure)) +
  theme_hc() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 16),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 20),
        strip.background = element_blank()) +
  labs(title = "",
       x = "Authoritarianism",
       y = "Information")
# ggsave("~/desktop/fig3.pdf", width = 8, height = 6)

# ** Measure Reliabilities -----------------------------------------------------------
vars4 <- c("obedience", "manners", "respect", "behaved")
vars8 <- c("obedience", "manners", "behaved", "orderly", "polite", 
           "respect", "loyal", "discip")

psych::alpha(qual_dat[, vars4])
psych::alpha(qual_dat[, vars8])

# ** Figure 4: Distribution of Authoritarianism -----
qual_dat$auth4 <- NA
qual_dat$auth4 <- (qual_dat$obedience + qual_dat$manners + qual_dat$behaved + qual_dat$respect)/4
qual_dat$auth8 <- NA
qual_dat$auth8 <- (qual_dat$obedience + qual_dat$manners + qual_dat$behaved + qual_dat$polite + qual_dat$respect + qual_dat$discip + qual_dat$orderly + qual_dat$loyal)/8


dat1 <- subset(qual_dat, select = c("auth4", "ResponseId"))
dat1$measure <- "Four-Item"
names(dat1)[1] <- "auth"
dat2 <- subset(qual_dat, select = c("auth8", "ResponseId"))
dat2$measure <- "Eight-Item"
names(dat2)[1] <- "auth"
dat <- rbind(dat1, dat2)

dat$measure <- factor(dat$measure, levels = c("Four-Item", "Eight-Item"))

ggplot(dat, aes(x = auth, group = measure)) +
  geom_bar(aes(y = ..prop..), position = "dodge") +
  theme_hc() +
  facet_grid(measure~.) +
  scale_x_continuous(breaks = seq(0, 1, by = .2)) +
  scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 16),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 20),
        strip.background = element_blank()) +
  labs(title = "",
       x = "Authoritarianism",
       y = "Proportion")
# ggsave("~/desktop/fig4.pdf", width = 8, height = 6)


# Results 5 ---------------------------------------------------------------
# ^ Specifying Variables --------------------------------------------------
# specifying RHS 
RHS_4 <- c("auth4 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat + white + black")
RHS_8 <- c("auth8 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat + white + black")

# specifying RHS, within-race
RHS_4_race <- c("auth4 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat")
RHS_8_race <- c("auth8 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat")

# ** White ---------------------------------------------------------
wht_dat <- subset(qual_dat, white == 1)
prop.table(table(wht_dat$auth4, wht_dat$pid3), 1)
prop.table(table(wht_dat$auth8, wht_dat$pid3), 1)


pid_4_wht <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                  data = wht_dat, method = "probit", Hess = T)
pid_8_wht <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                  data = wht_dat, method = "probit", Hess = T)

# *** Bootstrapping Predictions --------------------------------------------
# predicted data
preddat_4_wht <- data.frame(auth4 = seq(0, 1, length.out = 10),
                            fem = median(wht_dat$fem),
                            age = mean(wht_dat$age),
                            south = 0,
                            religiosity = median(wht_dat$religiosity, na.rm = T),
                            relig_cath = 0,
                            relig_prot = 0,
                            relig_evang = 0,
                            relig_AthNoneDK = 1,
                            rr_sc = mean(wht_dat$rr_sc, na.rm = T),
                            inc = median(wht_dat$inc, na.rm = T),
                            inc_missing = 0,
                            educ_3cat = median(wht_dat$educ_3cat))
preddat_8_wht <- data.frame(auth8 = seq(0, 1, length.out = 10),
                            fem = median(wht_dat$fem),
                            age = mean(wht_dat$age),
                            south = 0,
                            religiosity = median(wht_dat$religiosity, na.rm = T),
                            relig_cath = 0,
                            relig_prot = 0,
                            relig_evang = 0,
                            relig_AthNoneDK = 1,
                            rr_sc = mean(wht_dat$rr_sc, na.rm = T),
                            inc = median(wht_dat$inc, na.rm = T),
                            inc_missing = 0,
                            educ_3cat = median(wht_dat$educ_3cat))

# boots
nboots <- 2000

# empty matrices to store results
preds_dem4_wht <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_wht))
preds_ind4_wht <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_wht))
preds_rep4_wht <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_wht))
preds_dem8_wht <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_wht))
preds_ind8_wht <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_wht))
preds_rep8_wht <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_wht))

set.seed(1693)
for(i in 1:nboots){
  indx <- sample(nrow(wht_dat), replace = T)
  
  pid_4_wht <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                    data = wht_dat[indx,], method = "probit", Hess = T)
  pid_8_wht <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                    data = wht_dat[indx,], method = "probit", Hess = T)
  
  preds_dem4_wht[i, ] <- predict(pid_4_wht, preddat_4_wht, type = "probs")[, "-1"]
  preds_ind4_wht[i, ] <- predict(pid_4_wht, preddat_4_wht, type = "probs")[, "0"]
  preds_rep4_wht[i, ] <- predict(pid_4_wht, preddat_4_wht, type = "probs")[, "1"]
  
  preds_dem8_wht[i, ] <- predict(pid_8_wht, preddat_8_wht, type = "probs")[, "-1"]
  preds_ind8_wht[i, ] <- predict(pid_8_wht, preddat_8_wht, type = "probs")[, "0"]
  preds_rep8_wht[i, ] <- predict(pid_8_wht, preddat_8_wht, type = "probs")[, "1"]
}

mean_preds_dem4_wht <- apply(preds_dem4_wht, 2, mean)
sd_preds_dem4_wht <- apply(preds_dem4_wht, 2, sd)
mean_preds_ind4_wht <- apply(preds_ind4_wht, 2, mean)
sd_preds_ind4_wht <- apply(preds_ind4_wht, 2, sd)
mean_preds_rep4_wht <- apply(preds_rep4_wht, 2, mean)
sd_preds_rep4_wht <- apply(preds_rep4_wht, 2, sd)


mean_preds_dem8_wht <- apply(preds_dem8_wht, 2, mean)
sd_preds_dem8_wht <- apply(preds_dem8_wht, 2, sd)
mean_preds_ind8_wht <- apply(preds_ind8_wht, 2, mean)
sd_preds_ind8_wht <- apply(preds_ind8_wht, 2, sd)
mean_preds_rep8_wht <- apply(preds_rep8_wht, 2, mean)
sd_preds_rep8_wht <- apply(preds_rep8_wht, 2, sd)

preds_pid4_wht <- data.frame(party =  c(rep("Democrat", 10),
                                        rep("Independent", 10),
                                        rep("Republican", 10)),
                             mean = c(mean_preds_dem4_wht,
                                      mean_preds_ind4_wht,
                                      mean_preds_rep4_wht),
                             sd = c(sd_preds_dem4_wht,
                                    sd_preds_ind4_wht,
                                    sd_preds_rep4_wht),
                             auth_sc = seq(0, 1, length.out = 10),
                             auth_measure = "Four-item")
preds_pid8_wht <- data.frame(party =  c(rep("Democrat", 10),
                                        rep("Independent", 10),
                                        rep("Republican", 10)),
                             mean = c(mean_preds_dem8_wht,
                                      mean_preds_ind8_wht,
                                      mean_preds_rep8_wht),
                             sd = c(sd_preds_dem8_wht,
                                    sd_preds_ind8_wht,
                                    sd_preds_rep8_wht),
                             auth_sc = seq(0, 1, length.out = 10),
                             auth_measure = "Eight-item")

preds_pid_wht <- rbind(preds_pid4_wht, preds_pid8_wht)
preds_pid_wht$auth_measure <- as.factor(preds_pid_wht$auth_measure)

preds_pid_wht$uci <- preds_pid_wht$mean + 1.39*preds_pid_wht$sd
preds_pid_wht$lci <- preds_pid_wht$mean - 1.39*preds_pid_wht$sd

preds_pid_wht$race <- "White"

# ** Black ---------------------------------------------------------
blk_dat <- subset(qual_dat, black == 1)
prop.table(table(blk_dat$auth4, blk_dat$pid3), 1)
prop.table(table(blk_dat$auth8, blk_dat$pid3), 1)


pid_4_blk <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                  data = blk_dat, method = "probit", Hess = T)
pid_8_blk <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                  data = blk_dat, method = "probit", Hess = T)

# *** Bootstrapping Predictions --------------------------------------------
# predicted data
preddat_4_blk <- data.frame(auth4 = seq(0, 1, length.out = 10),
                            fem = median(blk_dat$fem),
                            age = mean(blk_dat$age),
                            south = 0,
                            religiosity = median(blk_dat$religiosity, na.rm = T),
                            relig_cath = 0,
                            relig_prot = 0,
                            relig_evang = 0,
                            relig_AthNoneDK = 1,
                            rr_sc = mean(blk_dat$rr_sc, na.rm = T),
                            inc = median(blk_dat$inc, na.rm = T),
                            inc_missing = 0,
                            educ_3cat = median(blk_dat$educ_3cat))
preddat_8_blk <- data.frame(auth8 = seq(0, 1, length.out = 10),
                            fem = median(blk_dat$fem),
                            age = mean(blk_dat$age),
                            south = 0,
                            religiosity = median(blk_dat$religiosity, na.rm = T),
                            relig_cath = 0,
                            relig_prot = 0,
                            relig_evang = 0,
                            relig_AthNoneDK = 1,
                            rr_sc = mean(blk_dat$rr_sc, na.rm = T),
                            inc = median(blk_dat$inc, na.rm = T),
                            inc_missing = 0,
                            educ_3cat = median(blk_dat$educ_3cat))

# boots
nboots <- 2000

# empty matrices to store results
preds_dem4_blk <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_blk))
preds_ind4_blk <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_blk))
preds_rep4_blk <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_blk))
preds_dem8_blk <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_blk))
preds_ind8_blk <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_blk))
preds_rep8_blk <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_blk))

set.seed(1693)
for(i in 1:nboots){
  indx <- sample(nrow(blk_dat), replace = T)
  
  pid_4_blk <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                    data = blk_dat[indx,], method = "probit", Hess = T)
  pid_8_blk <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                    data = blk_dat[indx,], method = "probit", Hess = T)
  
  preds_dem4_blk[i, ] <- predict(pid_4_blk, preddat_4_blk, type = "probs")[, "-1"]
  preds_ind4_blk[i, ] <- predict(pid_4_blk, preddat_4_blk, type = "probs")[, "0"]
  preds_rep4_blk[i, ] <- predict(pid_4_blk, preddat_4_blk, type = "probs")[, "1"]
  
  preds_dem8_blk[i, ] <- predict(pid_8_blk, preddat_8_blk, type = "probs")[, "-1"]
  preds_ind8_blk[i, ] <- predict(pid_8_blk, preddat_8_blk, type = "probs")[, "0"]
  preds_rep8_blk[i, ] <- predict(pid_8_blk, preddat_8_blk, type = "probs")[, "1"]
}

mean_preds_dem4_blk <- apply(preds_dem4_blk, 2, mean)
sd_preds_dem4_blk <- apply(preds_dem4_blk, 2, sd)
mean_preds_ind4_blk <- apply(preds_ind4_blk, 2, mean)
sd_preds_ind4_blk <- apply(preds_ind4_blk, 2, sd)
mean_preds_rep4_blk <- apply(preds_rep4_blk, 2, mean)
sd_preds_rep4_blk <- apply(preds_rep4_blk, 2, sd)


mean_preds_dem8_blk <- apply(preds_dem8_blk, 2, mean)
sd_preds_dem8_blk <- apply(preds_dem8_blk, 2, sd)
mean_preds_ind8_blk <- apply(preds_ind8_blk, 2, mean)
sd_preds_ind8_blk <- apply(preds_ind8_blk, 2, sd)
mean_preds_rep8_blk <- apply(preds_rep8_blk, 2, mean)
sd_preds_rep8_blk <- apply(preds_rep8_blk, 2, sd)

preds_pid4_blk <- data.frame(party =  c(rep("Democrat", 10),
                                        rep("Independent", 10),
                                        rep("Republican", 10)),
                             mean = c(mean_preds_dem4_blk,
                                      mean_preds_ind4_blk,
                                      mean_preds_rep4_blk),
                             sd = c(sd_preds_dem4_blk,
                                    sd_preds_ind4_blk,
                                    sd_preds_rep4_blk),
                             auth_sc = seq(0, 1, length.out = 10),
                             auth_measure = "Four-item")
preds_pid8_blk <- data.frame(party =  c(rep("Democrat", 10),
                                        rep("Independent", 10),
                                        rep("Republican", 10)),
                             mean = c(mean_preds_dem8_blk,
                                      mean_preds_ind8_blk,
                                      mean_preds_rep8_blk),
                             sd = c(sd_preds_dem8_blk,
                                    sd_preds_ind8_blk,
                                    sd_preds_rep8_blk),
                             auth_sc = seq(0, 1, length.out = 10),
                             auth_measure = "Eight-item")

preds_pid_blk <- rbind(preds_pid4_blk, preds_pid8_blk)
preds_pid_blk$auth_measure <- as.factor(preds_pid_blk$auth_measure)

preds_pid_blk$uci <- preds_pid_blk$mean + 1.39*preds_pid_blk$sd
preds_pid_blk$lci <- preds_pid_blk$mean - 1.39*preds_pid_blk$sd

preds_pid_blk$race <- "Black"


# ** Latino ---------------------------------------------------------
lat_dat <- subset(qual_dat, latino == 1)
prop.table(table(lat_dat$auth4, lat_dat$pid3), 1)
prop.table(table(lat_dat$auth8, lat_dat$pid3), 1)

pid_4_lat <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                  data = lat_dat, method = "probit", Hess = T)
pid_8_lat <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                  data = lat_dat, method = "probit", Hess = T)

# *** Bootstrapping Predictions --------------------------------------------
# predicted data
preddat_4_lat <- data.frame(auth4 = seq(0, 1, length.out = 10),
                            fem = median(lat_dat$fem),
                            age = mean(lat_dat$age),
                            south = 0,
                            religiosity = median(lat_dat$religiosity, na.rm = T),
                            relig_cath = 0,
                            relig_prot = 0,
                            relig_evang = 0,
                            relig_AthNoneDK = 1,
                            rr_sc = mean(lat_dat$rr_sc, na.rm = T),
                            inc = median(lat_dat$inc, na.rm = T),
                            inc_missing = 0,
                            educ_3cat = median(lat_dat$educ_3cat))
preddat_8_lat <- data.frame(auth8 = seq(0, 1, length.out = 10),
                            fem = median(lat_dat$fem),
                            age = mean(lat_dat$age),
                            south = 0,
                            religiosity = median(lat_dat$religiosity, na.rm = T),
                            relig_cath = 0,
                            relig_prot = 0,
                            relig_evang = 0,
                            relig_AthNoneDK = 1,
                            rr_sc = mean(lat_dat$rr_sc, na.rm = T),
                            inc = median(lat_dat$inc, na.rm = T),
                            inc_missing = 0,
                            educ_3cat = median(lat_dat$educ_3cat))

# boots
nboots <- 2000

# empty matrices to store results
preds_dem4_lat <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_lat))
preds_ind4_lat <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_lat))
preds_rep4_lat <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4_lat))
preds_dem8_lat <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_lat))
preds_ind8_lat <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_lat))
preds_rep8_lat <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8_lat))

set.seed(1693)
for(i in 1:nboots){
  indx <- sample(nrow(lat_dat), replace = T)
  
  tryCatch({
    pid_4_lat <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                      data = lat_dat[indx,], method = "probit", Hess = T)
    pid_8_lat <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                      data = lat_dat[indx,], method = "probit", Hess = T)
    
    preds_dem4_lat[i, ] <- predict(pid_4_lat, preddat_4_lat, type = "probs")[, "-1"]
    preds_ind4_lat[i, ] <- predict(pid_4_lat, preddat_4_lat, type = "probs")[, "0"]
    preds_rep4_lat[i, ] <- predict(pid_4_lat, preddat_4_lat, type = "probs")[, "1"]
    
    preds_dem8_lat[i, ] <- predict(pid_8_lat, preddat_8_lat, type = "probs")[, "-1"]
    preds_ind8_lat[i, ] <- predict(pid_8_lat, preddat_8_lat, type = "probs")[, "0"]
    preds_rep8_lat[i, ] <- predict(pid_8_lat, preddat_8_lat, type = "probs")[, "1"]
  }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

mean_preds_dem4_lat <- apply(preds_dem4_lat, 2, mean, na.rm = T)
sd_preds_dem4_lat <- apply(preds_dem4_lat, 2, sd, na.rm = T)
mean_preds_ind4_lat <- apply(preds_ind4_lat, 2, mean, na.rm = T)
sd_preds_ind4_lat <- apply(preds_ind4_lat, 2, sd, na.rm = T)
mean_preds_rep4_lat <- apply(preds_rep4_lat, 2, mean, na.rm = T)
sd_preds_rep4_lat <- apply(preds_rep4_lat, 2, sd, na.rm = T)


mean_preds_dem8_lat <- apply(preds_dem8_lat, 2, mean, na.rm = T)
sd_preds_dem8_lat <- apply(preds_dem8_lat, 2, sd, na.rm = T)
mean_preds_ind8_lat <- apply(preds_ind8_lat, 2, mean, na.rm = T)
sd_preds_ind8_lat <- apply(preds_ind8_lat, 2, sd, na.rm = T)
mean_preds_rep8_lat <- apply(preds_rep8_lat, 2, mean, na.rm = T)
sd_preds_rep8_lat <- apply(preds_rep8_lat, 2, sd, na.rm = T)

preds_pid4_lat <- data.frame(party =  c(rep("Democrat", 10),
                                        rep("Independent", 10),
                                        rep("Republican", 10)),
                             mean = c(mean_preds_dem4_lat,
                                      mean_preds_ind4_lat,
                                      mean_preds_rep4_lat),
                             sd = c(sd_preds_dem4_lat,
                                    sd_preds_ind4_lat,
                                    sd_preds_rep4_lat),
                             auth_sc = seq(0, 1, length.out = 10),
                             auth_measure = "Four-item")
preds_pid8_lat <- data.frame(party =  c(rep("Democrat", 10),
                                        rep("Independent", 10),
                                        rep("Republican", 10)),
                             mean = c(mean_preds_dem8_lat,
                                      mean_preds_ind8_lat,
                                      mean_preds_rep8_lat),
                             sd = c(sd_preds_dem8_lat,
                                    sd_preds_ind8_lat,
                                    sd_preds_rep8_lat),
                             auth_sc = seq(0, 1, length.out = 10),
                             auth_measure = "Eight-item")

preds_pid_lat <- rbind(preds_pid4_lat, preds_pid8_lat)
preds_pid_lat$auth_measure <- as.factor(preds_pid_lat$auth_measure)

preds_pid_lat$uci <- preds_pid_lat$mean + 1.39*preds_pid_lat$sd
preds_pid_lat$lci <- preds_pid_lat$mean - 1.39*preds_pid_lat$sd

preds_pid_lat$race <- "Latino"


# *** Figure 5 ---------------------------------------------------------------
preds_pid_race <- rbind(preds_pid_wht, preds_pid_blk, preds_pid_lat)

ggplot(preds_pid_race, aes(x = auth_sc, y = mean, group = auth_measure)) +
  geom_line(size = 1.25, aes(color = auth_measure)) +
  geom_line(size = 1, linetype = "dotted", aes(color = auth_measure, y = lci)) +
  geom_line(size = 1, linetype = "dotted", aes(color = auth_measure, y = uci)) +
  theme_hc() +
  facet_grid(race~party, scales = "free_y") +
  scale_x_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
  scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
  scale_color_manual(values = c("grey", "black")) +
  scale_fill_manual(values = c("lightgrey", "darkgrey")) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 20),
        strip.background = element_blank()) +
  labs(x = "Authoritarianism",
       y = "Probability")
# ggsave("~/desktop/fig5.pdf", width = 8, height = 8)



# * Ideology ------------------------------------------------------------
prop.table(table(qual_dat$auth4, qual_dat$ideo3), 1)
prop.table(table(qual_dat$auth8, qual_dat$ideo3), 1)

ideo_4 <- polr(paste("as.factor(ideo3)", RHS_4, sep = "~"), 
               data = qual_dat, method = "probit", Hess = T)
summary(ideo_4)
(ctable_ideo_4 <- coef(summary(ideo_4)))
p <- pnorm(abs(ctable_ideo_4[, "t value"]), lower.tail = F)*2
(ctable_ideo_4 <- cbind(ctable_ideo_4, "p value" = p))

ideo_8 <- polr(paste("as.factor(ideo3)", RHS_8, sep = "~"), 
               data = qual_dat, method = "probit", Hess = T)
summary(ideo_8)
(ctable_ideo_8 <- coef(summary(ideo_8)))
p <- pnorm(abs(ctable_ideo_8[, "t value"]), lower.tail = F)*2
(ctable_ideo_8 <- cbind(ctable_ideo_8, "p value" = p))

# ** Bootstrapping Predictions --------------------------------------------
# predicted data
preddat_4 <- data.frame(auth4 = seq(0, 1, length.out = 10),
                        fem = median(qual_dat$fem),
                        age = mean(qual_dat$age),
                        white = 1,
                        black = 0,
                        south = 0,
                        religiosity = median(qual_dat$religiosity, na.rm = T),
                        relig_cath = 0,
                        relig_prot = 0,
                        relig_evang = 0,
                        relig_AthNoneDK = 1,
                        rr_sc = mean(qual_dat$rr_sc, na.rm = T),
                        inc = median(qual_dat$inc, na.rm = T),
                        inc_missing = 0,
                        educ_3cat = median(qual_dat$educ_3cat))
preddat_8 <- data.frame(auth8 = seq(0, 1, length.out = 10),
                        fem = median(qual_dat$fem),
                        age = mean(qual_dat$age),
                        white = 1,
                        black = 0,
                        south = 0,
                        religiosity = median(qual_dat$religiosity, na.rm = T),
                        relig_cath = 0,
                        relig_prot = 0,
                        relig_evang = 0,
                        relig_AthNoneDK = 1,
                        rr_sc = mean(qual_dat$rr_sc, na.rm = T),
                        inc = median(qual_dat$inc, na.rm = T),
                        inc_missing = 0,
                        educ_3cat = median(qual_dat$educ_3cat))

# boots
nboots <- 2000

# empty matrices to store results
preds_lib4 <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4))
preds_mod4 <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4))
preds_con4 <- matrix(NA, nrow = nboots, ncol = nrow(preddat_4))
preds_lib8 <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8))
preds_mod8 <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8))
preds_con8 <- matrix(NA, nrow = nboots, ncol = nrow(preddat_8))


set.seed(1693)
for(i in 1:nboots){
  indx <- sample(nrow(qual_dat), replace = T)
  
  ideo_4 <- polr(paste("as.factor(ideo3)", RHS_4, sep = "~"), 
                 data = qual_dat[indx,], method = "probit", Hess = T)
  ideo_8 <- polr(paste("as.factor(ideo3)", RHS_8, sep = "~"), 
                 data = qual_dat[indx,], method = "probit", Hess = T)
  
  preds_lib4[i, ] <- predict(ideo_4, preddat_4, type = "probs")[, "-1"]
  preds_mod4[i, ] <- predict(ideo_4, preddat_4, type = "probs")[, "0"]
  preds_con4[i, ] <- predict(ideo_4, preddat_4, type = "probs")[, "1"]
  
  preds_lib8[i, ] <- predict(ideo_8, preddat_8, type = "probs")[, "-1"]
  preds_mod8[i, ] <- predict(ideo_8, preddat_8, type = "probs")[, "0"]
  preds_con8[i, ] <- predict(ideo_8, preddat_8, type = "probs")[, "1"]
}

# Summarizing Bootstrap samples
mean_preds_lib4 <- apply(preds_lib4, 2, mean)
sd_preds_lib4 <- apply(preds_lib4, 2, sd)
mean_preds_mod4 <- apply(preds_mod4, 2, mean)
sd_preds_mod4 <- apply(preds_mod4, 2, sd)
mean_preds_con4 <- apply(preds_con4, 2, mean)
sd_preds_con4 <- apply(preds_con4, 2, sd)

mean_preds_lib8 <- apply(preds_lib8, 2, mean)
sd_preds_lib8 <- apply(preds_lib8, 2, sd)
mean_preds_mod8 <- apply(preds_mod8, 2, mean)
sd_preds_mod8 <- apply(preds_mod8, 2, sd)
mean_preds_con8 <- apply(preds_con8, 2, mean)
sd_preds_con8 <- apply(preds_con8, 2, sd)

# Stacking to Data Frames to Plot
preds_ideo4 <- data.frame(ideo =  c(rep("Liberal", 10),
                                    rep("Moderate", 10),
                                    rep("Conservative", 10)),
                          mean = c(mean_preds_lib4,
                                   mean_preds_mod4,
                                   mean_preds_con4),
                          sd = c(sd_preds_lib4,
                                 sd_preds_mod4,
                                 sd_preds_con4),
                          auth_sc = seq(0, 1, length.out = 10),
                          auth_measure = "Four-item")
preds_ideo8 <- data.frame(ideo =  c(rep("Liberal", 10),
                                    rep("Moderate", 10),
                                    rep("Conservative", 10)),
                          mean = c(mean_preds_lib8,
                                   mean_preds_mod8,
                                   mean_preds_con8),
                          sd = c(sd_preds_lib8,
                                 sd_preds_mod8,
                                 sd_preds_con8),
                          auth_sc = seq(0, 1, length.out = 10),
                          auth_measure = "Eight-item")

preds_ideo <- rbind(preds_ideo4, preds_ideo8)
preds_ideo$auth_measure <- as.factor(preds_ideo$auth_measure)
preds_ideo$ideo <- factor(preds_ideo$ideo,
                          levels = c("Liberal", "Moderate", "Conservative"))

# Adding 84% CI intervals
preds_ideo$uci <- preds_ideo$mean + 1.39*preds_ideo$sd
preds_ideo$lci <- preds_ideo$mean - 1.39*preds_ideo$sd

# ** Figure 6 ---------------------------------------------------------------
ggplot(preds_ideo, 
       aes(x = auth_sc, y = mean, group = auth_measure)) +
  geom_line(size = 1.25, aes(color = auth_measure)) +
  geom_line(size = 1, linetype = "dotted", aes(color = auth_measure, y = lci)) +
  geom_line(size = 1, linetype = "dotted", aes(color = auth_measure, y = uci)) +
  theme_hc() +
  facet_grid(~ideo) +
  scale_x_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
  scale_y_continuous(limits = c(0, .8),
                     labels = numform::ff_num(zero = 0, digits = 2)) +
  scale_color_manual(values = c("grey", "black")) +
  scale_fill_manual(values = c("lightgrey", "darkgrey")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 20),
        strip.background = element_blank()) +
  labs(x = "Authoritarianism",
       y = "Probability")
# ggsave("~/desktop/fig6.pdf", width = 8, height = 5)

# ** Table A6 ----------------------------------------------------------------
m_pid_4_wht <- pid_4_wht
m_pid_8_wht <- pid_8_wht
m_pid_4_blk <- pid_4_blk
m_pid_8_blk <- pid_8_blk
m_pid_4_lat <- pid_4_lat
m_pid_8_lat <- pid_8_lat
m_ideo_4 <- ideo_4
m_ideo_8 <- ideo_8

names(m_pid_4_wht$coefficients) <- names(coef(m_pid_4_wht))
names(m_pid_8_wht$coefficients) <- names(coef(m_pid_4_wht))
names(m_pid_4_blk$coefficients) <- names(coef(m_pid_4_blk))
names(m_pid_8_blk$coefficients) <- names(coef(m_pid_4_blk))
names(m_pid_4_lat$coefficients) <- names(coef(m_pid_4_lat))
names(m_pid_8_lat$coefficients) <- names(coef(m_pid_4_lat))
names(m_ideo_4$coefficients) <- names(coef(m_ideo_4))
names(m_ideo_8$coefficients) <- names(coef(m_ideo_4))

stargazer(m_pid_4_wht,
          m_pid_8_wht,
          m_pid_4_blk,
          m_pid_8_blk,
          m_pid_4_lat,
          m_pid_8_lat,
          m_ideo_4, m_ideo_8,
          title = "Authoritarianism and Self-Identification",
          model.numbers = T, dep.var.caption = "",
          covariate.labels = c("Authoritarianism", "Age", "Female", "South", "Religiosity", "Catholic", "Protestant", "Evangelical", "No Religion", "Income", "Income--Missing", "Education", "White", "Black"),
          column.labels = c("Party Identification", "Ideological Identification"), 
          column.separate = c(6,2),
          dep.var.labels.include = F,
          header = F,
          type = "html", out = "~/desktop/auth_IDs.html",
          digits = 3, no.space = T)


# * Table 5: Attitude Means ----------------------------------------------
c(mean(qual_dat$ft_trump_sc[which(qual_dat$auth4 == 0)], na.rm = T),
  mean(qual_dat$ft_trump_sc[which(qual_dat$auth4 == .5)], na.rm = T),
  mean(qual_dat$ft_trump_sc[which(qual_dat$auth4 == 1)], na.rm = T))
c(mean(qual_dat$ft_trump_sc[which(qual_dat$auth8 == 0)], na.rm = T),
  mean(qual_dat$ft_trump_sc[which(qual_dat$auth8 == .5)], na.rm = T),
  mean(qual_dat$ft_trump_sc[which(qual_dat$auth8 == 1)], na.rm = T))

c(mean(qual_dat$ft_rep_sc[which(qual_dat$auth4 == 0)], na.rm = T),
  mean(qual_dat$ft_rep_sc[which(qual_dat$auth4 == .5)], na.rm = T),
  mean(qual_dat$ft_rep_sc[which(qual_dat$auth4 == 1)], na.rm = T))
c(mean(qual_dat$ft_rep_sc[which(qual_dat$auth8 == 0)], na.rm = T),
  mean(qual_dat$ft_rep_sc[which(qual_dat$auth8 == .5)], na.rm = T),
  mean(qual_dat$ft_rep_sc[which(qual_dat$auth8 == 1)], na.rm = T))

c(mean(qual_dat$ft_dem_sc[which(qual_dat$auth4 == 0)], na.rm = T),
  mean(qual_dat$ft_dem_sc[which(qual_dat$auth4 == .5)], na.rm = T),
  mean(qual_dat$ft_dem_sc[which(qual_dat$auth4 == 1)], na.rm = T))
c(mean(qual_dat$ft_dem_sc[which(qual_dat$auth8 == 0)], na.rm = T),
  mean(qual_dat$ft_dem_sc[which(qual_dat$auth8 == .5)], na.rm = T),
  mean(qual_dat$ft_dem_sc[which(qual_dat$auth8 == 1)], na.rm = T))

c(mean(qual_dat$immig_sc[which(qual_dat$auth4 == 0)], na.rm = T),
  mean(qual_dat$immig_sc[which(qual_dat$auth4 == .5)], na.rm = T),
  mean(qual_dat$immig_sc[which(qual_dat$auth4 == 1)], na.rm = T))
c(mean(qual_dat$immig_sc[which(qual_dat$auth8 == 0)], na.rm = T),
  mean(qual_dat$immig_sc[which(qual_dat$auth8 == .5)], na.rm = T),
  mean(qual_dat$immig_sc[which(qual_dat$auth8 == 1)], na.rm = T))

c(mean(qual_dat$gay_sc[which(qual_dat$auth4 == 0)], na.rm = T),
  mean(qual_dat$gay_sc[which(qual_dat$auth4 == .5)], na.rm = T),
  mean(qual_dat$gay_sc[which(qual_dat$auth4 == 1)], na.rm = T))
c(mean(qual_dat$gay_sc[which(qual_dat$auth8 == 0)], na.rm = T),
  mean(qual_dat$gay_sc[which(qual_dat$auth8 == .5)], na.rm = T),
  mean(qual_dat$gay_sc[which(qual_dat$auth8 == 1)], na.rm = T))

c(mean(qual_dat$cjs_pun_sc[which(qual_dat$auth4 == 0)], na.rm = T),
  mean(qual_dat$cjs_pun_sc[which(qual_dat$auth4 == .5)], na.rm = T),
  mean(qual_dat$cjs_pun_sc[which(qual_dat$auth4 == 1)], na.rm = T))
c(mean(qual_dat$cjs_pun_sc[which(qual_dat$auth8 == 0)], na.rm = T),
  mean(qual_dat$cjs_pun_sc[which(qual_dat$auth8 == .5)], na.rm = T),
  mean(qual_dat$cjs_pun_sc[which(qual_dat$auth8 == 1)], na.rm = T))


# ** T-Tests --------------------------------------------------------------
# specifying variables
vars <- c("ft_trump_sc", "ft_rep_sc", "ft_dem_sc", "immig_sc", "gay_sc", "cjs_pun_sc")

# For 0
# subsetting
ttest_a4_0 <- subset(qual_dat, auth4 == 0, select = vars)
ttest_a4_0$group <- "auth4"
ttest_a8_0 <- subset(qual_dat, auth8 == 0, select = vars)
ttest_a8_0$group <- "auth8"

# stacking
ttest_0 <- rbind(ttest_a4_0, ttest_a8_0)

# trump
t.test(ft_trump_sc ~ group, ttest_0)
# rep
t.test(ft_rep_sc ~ group, ttest_0)
# dem
t.test(ft_dem_sc ~ group, ttest_0)
# immigration policy
t.test(immig_sc ~ group, ttest_0)
# lgbt policy
t.test(gay_sc ~ group, ttest_0)
# cjs policy
t.test(cjs_pun_sc ~ group, ttest_0)

# For 0.5
# subsetting
ttest_a4_5 <- subset(qual_dat, auth4 == .5, select = vars)
ttest_a4_5$group <- "auth4"
ttest_a8_5 <- subset(qual_dat, auth8 == .5, select = vars)
ttest_a8_5$group <- "auth8"

# stacking
ttest_5 <- rbind(ttest_a4_5, ttest_a8_5)

# trump
t.test(ft_trump_sc ~ group, ttest_5)
# rep
t.test(ft_rep_sc ~ group, ttest_5)
# dem
t.test(ft_dem_sc ~ group, ttest_5)
# immigration policy
t.test(immig_sc ~ group, ttest_5)
# lgbt policy
t.test(gay_sc ~ group, ttest_5)
# cjs policy
t.test(cjs_pun_sc ~ group, ttest_5)


# For 1
# subsetting
ttest_a4_1 <- subset(qual_dat, auth4 == 1, select = vars)
ttest_a4_1$group <- "auth4"
ttest_a8_1 <- subset(qual_dat, auth8 == 1, select = vars)
ttest_a8_1$group <- "auth8"

# stacking
ttest_1 <- rbind(ttest_a4_1, ttest_a8_1)

# trump
t.test(ft_trump_sc ~ group, ttest_1)
# rep
t.test(ft_rep_sc ~ group, ttest_1)
# dem
t.test(ft_dem_sc ~ group, ttest_1)
# immigration policy
t.test(immig_sc ~ group, ttest_1)
# lgbt policy
t.test(gay_sc ~ group, ttest_1)
# cjs policy
t.test(cjs_pun_sc ~ group, ttest_1)


# * Attitudes -----------------------------------------------------
# Specify RHS
RHS_4 <- c("auth4 + age + fem + white + black + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat")
RHS_8 <- c("auth8 + age + fem + white + black + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat")

RHS_4_immig <- c("auth4 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat")
RHS_8_immig <- c("auth8 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat")

# Estimate moodels
m_trumpFT_4 <- lm(as.formula(paste("ft_trump_sc", RHS_4, sep = "~")), qual_dat)
m_trumpFT_8 <- lm(as.formula(paste("ft_trump_sc", RHS_8, sep = "~")), qual_dat)

m_repFT_4 <- lm(as.formula(paste("ft_rep_sc", RHS_4, sep = "~")), qual_dat)
m_repFT_8 <- lm(as.formula(paste("ft_rep_sc", RHS_8, sep = "~")), qual_dat)

m_demFT_4 <- lm(as.formula(paste("ft_dem_sc", RHS_4, sep = "~")), qual_dat)
m_demFT_8 <- lm(as.formula(paste("ft_dem_sc", RHS_8, sep = "~")), qual_dat)

m_immig_4 <- lm(as.formula(paste("immig_sc", RHS_4, sep = "~")), qual_dat)
m_immig_8 <- lm(as.formula(paste("immig_sc", RHS_8, sep = "~")), qual_dat)

# white
m_immig_4_wht <- lm(as.formula(paste("immig_sc", RHS_4_immig, sep = "~")), subset(qual_dat, white == 1))
m_immig_8_wht <- lm(as.formula(paste("immig_sc", RHS_8_immig, sep = "~")), subset(qual_dat, white == 1))
# non-white
m_immig_4_nwht <- lm(as.formula(paste("immig_sc", RHS_4_immig, sep = "~")), subset(qual_dat, white == 0))
m_immig_8_nwht <- lm(as.formula(paste("immig_sc", RHS_8_immig, sep = "~")), subset(qual_dat, white == 0))

m_gay_4 <- lm(as.formula(paste("gay_sc", RHS_4, sep = "~")), qual_dat)
m_gay_8 <- lm(as.formula(paste("gay_sc", RHS_8, sep = "~")), qual_dat)

m_cjs_pun_4 <- lm(as.formula(paste("cjs_pun_sc", RHS_4, sep = "~")), qual_dat)
m_cjs_pun_8 <- lm(as.formula(paste("cjs_pun_sc", RHS_8, sep = "~")), qual_dat)

# ** Figure 7 ------------------------------------------------------------
CI <- 95
trump_4 <- to_plot(m_trumpFT_4, vars = "auth4", intercept = F, robust = F, shift = "range", ci = CI)
trump_4$df <- "four"
trump_4$dv <- "trump"
trump_8 <- to_plot(m_trumpFT_8, vars = "auth8", intercept = F, robust = F, shift = "range", ci = CI)
trump_8$df <- "eight"
trump_8$dv <- "trump"

rep_4 <- to_plot(m_repFT_4, vars = "auth4", intercept = F, robust = F, shift = "range", ci = CI)
rep_4$df <- "four"
rep_4$dv <- "rep"
rep_8 <- to_plot(m_repFT_8, vars = "auth8", intercept = F, robust = F, shift = "range", ci = CI)
rep_8$df <- "eight"
rep_8$dv <- "rep"

dem_4 <- to_plot(m_demFT_4, vars = "auth4", intercept = F, robust = F, shift = "range", ci = CI)
dem_4$df <- "four"
dem_4$dv <- "dem"
dem_8 <- to_plot(m_demFT_8, vars = "auth8", intercept = F, robust = F, shift = "range", ci = CI)
dem_8$df <- "eight"
dem_8$dv <- "dem"

immig_4_wht <- to_plot(m_immig_4_wht, vars = "auth4", intercept = F, robust = F, shift = "range", ci = CI)
immig_4_wht$df <- "four"
immig_4_wht$dv <- "immig_wht"
immig_8_wht <- to_plot(m_immig_8_wht, vars = "auth8", intercept = F, robust = F, shift = "range", ci = CI)
immig_8_wht$df <- "eight"
immig_8_wht$dv <- "immig_wht"

immig_4_nwht <- to_plot(m_immig_4_nwht, vars = "auth4", intercept = F, robust = F, shift = "range", ci = CI)
immig_4_nwht$df <- "four"
immig_4_nwht$dv <- "immig_nwht"
immig_8_nwht <- to_plot(m_immig_8_nwht, vars = "auth8", intercept = F, robust = F, shift = "range", ci = CI)
immig_8_nwht$df <- "eight"
immig_8_nwht$dv <- "immig_nwht"

gay_4 <- to_plot(m_gay_4, vars = "auth4", intercept = F, robust = F, shift = "range", ci = CI)
gay_4$df <- "four"
gay_4$dv <- "gay"
gay_8 <- to_plot(m_gay_8, vars = "auth8", intercept = F, robust = F, shift = "range", ci = CI)
gay_8$df <- "eight"
gay_8$dv <- "gay"

cjs_pun_4 <- to_plot(m_cjs_pun_4, vars = "auth4", intercept = F, robust = F, shift = "range", ci = CI)
cjs_pun_4$df <- "four"
cjs_pun_4$dv <- "cjs_pun"
cjs_pun_8 <- to_plot(m_cjs_pun_8, vars = "auth8", intercept = F, robust = F, shift = "range", ci = CI)
cjs_pun_8$df <- "eight"
cjs_pun_8$dv <- "cjs_pun"

preds <- rbind(trump_4,trump_8,rep_4, rep_8, dem_4, dem_8,
               immig_4_wht, immig_8_wht,
               immig_4_nwht, immig_8_nwht,
               gay_4, gay_8, cjs_pun_4, cjs_pun_8)

preds$df <- factor(preds$df,
                   levels = c("four", "eight"),
                   labels = c("Four-Item", "Eight-Item"))
preds$dv <- factor(preds$dv,
                   levels = c("trump", "rep", "dem",
                              "immig_wht", "immig_nwht", 
                              "gay", "cjs_pun"),
                   labels = c("Trump FT", "Republican\nParty FT",
                              "Democratic\nParty FT",
                              "Immigration\nWhites", "Immigration\nNon-Whites", 
                              "Gay Rights", "Punitive\nCriminal\nJustice"))

COLOR <- c("black", "black")
ggplot(preds, aes(x = dv, y = coef, group = df)) +
  geom_linerange(aes(ymin = lci, ymax = uci, color = df),
                 position = position_dodge(width = .5), lwd = 1.25) +
  geom_point(size = 3, aes(shape = df, color = df),
             position = position_dodge(width = .5)) +
  theme_hc() +
  scale_shape_manual(values = c(15, 16)) +
  scale_color_manual(values = COLOR) +

  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16), #color = "white"
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  labs(title = "",
       y = "Coefficient Estimate",
       x = "")
# ggsave("~/desktop/fig7.pdf", width = 9, height = 5)


# ** Table A7 ---------------------------------------------------------------
# editing objects for formatting
m_trumpFT_4_p <- m_trumpFT_4
m_trumpFT_8_p <- m_trumpFT_8
m_repFT_4_p <- m_repFT_4
m_repFT_8_p <- m_repFT_8
m_demFT_4_p <- m_demFT_4
m_demFT_8_p <- m_demFT_8
m_immig4_wht_p <- m_immig_4_wht
m_immig8_wht_p <- m_immig_8_wht
m_immig4_nwht_p <- m_immig_4_nwht
m_immig8_nwht_p <- m_immig_8_nwht
m_gay4_p <- m_gay_4
m_gay8_p <- m_gay_8
m_cjs_pun4_p <- m_cjs_pun_4
m_cjs_pun8_p <- m_cjs_pun_8
names(m_trumpFT_4_p$coefficients) <- names(coef(m_trumpFT_4))
names(m_trumpFT_8_p$coefficients) <- names(coef(m_trumpFT_4))
names(m_repFT_4_p$coefficients) <- names(coef(m_repFT_4))
names(m_repFT_8_p$coefficients) <- names(coef(m_repFT_4))
names(m_demFT_4_p$coefficients) <- names(coef(m_demFT_4))
names(m_demFT_8_p$coefficients) <- names(coef(m_demFT_4))
names(m_immig4_wht_p$coefficients) <- names(coef(m_immig_4_wht))
names(m_immig8_wht_p$coefficients) <- names(coef(m_immig_4_wht))
names(m_immig4_nwht_p$coefficients) <- names(coef(m_immig_4_wht))
names(m_immig8_nwht_p$coefficients) <- names(coef(m_immig_4_wht))
names(m_gay4_p$coefficients) <- names(coef(m_gay_4))
names(m_gay8_p$coefficients) <- names(coef(m_gay_4))
names(m_cjs_pun4_p$coefficients) <- names(coef(m_cjs_pun_4))
names(m_cjs_pun8_p$coefficients) <- names(coef(m_cjs_pun_4))


stargazer(m_trumpFT_4_p, m_trumpFT_8_p,m_repFT_4_p, m_repFT_8_p, m_demFT_4_p, m_demFT_8_p,
          m_immig4_wht_p, m_immig8_wht_p, 
          m_immig4_nwht_p, m_immig8_nwht_p, 
          m_gay4_p, m_gay8_p, m_cjs_pun4_p, m_cjs_pun8_p,
          title = "Authoritarianism and Attitudes",
          model.numbers = T, dep.var.caption = "",
          covariate.labels = c("Authoritarianism", "Age", "Female", "White", "Black","South", "Religiosity", "Catholic", "Protestant", "Evangelical", "No Religion", "Income", "Income--Missing", "Education"),
          column.separate = c(2,2,2,4,2,2),
          column.labels = c("Trump FT", "Rep Party FT", "Dem Party FT", "Immigration", "Gay Rights", "CJS"),
          dep.var.labels.include = F,
          header = F, initial.zero = F,
          type = "html", out = "~/desktop/tabA7.html",
          digits = 3, omit.stat = c("f", "adj.rsq"), df = F, no.space = T)


# Appendix Tables A15-16 ---------------------------------------------------------------
# ^ Specifying Variables --------------------------------------------------
# specifying RHS 
RHS_4 <- c("auth4 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat + rr_sc + white + black")
RHS_8 <- c("auth8 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat + rr_sc + white + black")

# specifying RHS, within-race
RHS_4_race <- c("auth4 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat+ rr_sc")
RHS_8_race <- c("auth8 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat+ rr_sc")

# ** White ---------------------------------------------------------
wht_dat <- subset(qual_dat, white == 1)
prop.table(table(wht_dat$auth4, wht_dat$pid3), 1)
prop.table(table(wht_dat$auth8, wht_dat$pid3), 1)


pid_4_wht <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                  data = wht_dat, method = "probit", Hess = T)
pid_8_wht <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                  data = wht_dat, method = "probit", Hess = T)

# ** Black ---------------------------------------------------------
blk_dat <- subset(qual_dat, black == 1)
prop.table(table(blk_dat$auth4, blk_dat$pid3), 1)
prop.table(table(blk_dat$auth8, blk_dat$pid3), 1)


pid_4_blk <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                  data = blk_dat, method = "probit", Hess = T)
pid_8_blk <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                  data = blk_dat, method = "probit", Hess = T)

# ** Latino ---------------------------------------------------------
lat_dat <- subset(qual_dat, latino == 1)
prop.table(table(lat_dat$auth4, lat_dat$pid3), 1)
prop.table(table(lat_dat$auth8, lat_dat$pid3), 1)

pid_4_lat <- polr(paste("as.factor(pid3)", RHS_4_race, sep = "~"), 
                  data = lat_dat, method = "probit", Hess = T)
pid_8_lat <- polr(paste("as.factor(pid3)", RHS_8_race, sep = "~"), 
                  data = lat_dat, method = "probit", Hess = T)

# * Ideology ------------------------------------------------------------
prop.table(table(qual_dat$auth4, qual_dat$ideo3), 1)
prop.table(table(qual_dat$auth8, qual_dat$ideo3), 1)

ideo_4 <- polr(paste("as.factor(ideo3)", RHS_4, sep = "~"), 
               data = qual_dat, method = "probit", Hess = T)
summary(ideo_4)
(ctable_ideo_4 <- coef(summary(ideo_4)))
p <- pnorm(abs(ctable_ideo_4[, "t value"]), lower.tail = F)*2
(ctable_ideo_4 <- cbind(ctable_ideo_4, "p value" = p))

ideo_8 <- polr(paste("as.factor(ideo3)", RHS_8, sep = "~"), 
               data = qual_dat, method = "probit", Hess = T)
summary(ideo_8)
(ctable_ideo_8 <- coef(summary(ideo_8)))
p <- pnorm(abs(ctable_ideo_8[, "t value"]), lower.tail = F)*2
(ctable_ideo_8 <- cbind(ctable_ideo_8, "p value" = p))

# *** Table A15 ---------------------------------------------------------------
m_pid_4_wht <- pid_4_wht
m_pid_8_wht <- pid_8_wht
m_pid_4_blk <- pid_4_blk
m_pid_8_blk <- pid_8_blk
m_pid_4_lat <- pid_4_lat
m_pid_8_lat <- pid_8_lat
m_ideo_4 <- ideo_4
m_ideo_8 <- ideo_8

names(m_pid_4_wht$coefficients) <- names(coef(m_pid_4_wht))
names(m_pid_8_wht$coefficients) <- names(coef(m_pid_4_wht))
names(m_pid_4_blk$coefficients) <- names(coef(m_pid_4_blk))
names(m_pid_8_blk$coefficients) <- names(coef(m_pid_4_blk))
names(m_pid_4_lat$coefficients) <- names(coef(m_pid_4_lat))
names(m_pid_8_lat$coefficients) <- names(coef(m_pid_4_lat))
names(m_ideo_4$coefficients) <- names(coef(ideo_4))
names(m_ideo_8$coefficients) <- names(coef(ideo_4))

stargazer(m_pid_4_wht,
          m_pid_8_wht,
          m_pid_4_blk,
          m_pid_8_blk,
          m_pid_4_lat,
          m_pid_8_lat,
          m_ideo_4, m_ideo_8,
          title = "Authoritarianism and Self-Identification",
          model.numbers = T, dep.var.caption = "",
          covariate.labels = c("Authoritarianism", "Age", "Female","South", "Religiosity", "Catholic", "Protestant", "Evangelical", "No Religion", "Income", "Income--Missing", "Education", "Racial Resentment", "White", "Black"),
          column.labels = c("Party Identification", "Ideological Identification"), 
          column.separate = c(6,2),
          dep.var.labels.include = F,
          header = F,
          type = "html", 
          out = "~/desktop/auth_IDs_rr.html",
          digits = 3, no.space = T)

# * Attitudes -----------------------------------------------------
# Specify RHS
RHS_4 <- c("auth4 + age + fem + white + black + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat + rr_sc")
RHS_8 <- c("auth8 + age + fem + white + black + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat + rr_sc")

RHS_4_immig <- c("auth4 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat + rr_sc")
RHS_8_immig <- c("auth8 + age + fem + south + religiosity + relig_cath + relig_prot + relig_evang + relig_AthNoneDK + inc + inc_missing + educ_3cat + rr_sc")

# Estimate moodels
m_trumpFT_4 <- lm(as.formula(paste("ft_trump_sc", RHS_4, sep = "~")), qual_dat)
m_trumpFT_8 <- lm(as.formula(paste("ft_trump_sc", RHS_8, sep = "~")), qual_dat)

m_repFT_4 <- lm(as.formula(paste("ft_rep_sc", RHS_4, sep = "~")), qual_dat)
m_repFT_8 <- lm(as.formula(paste("ft_rep_sc", RHS_8, sep = "~")), qual_dat)

m_demFT_4 <- lm(as.formula(paste("ft_dem_sc", RHS_4, sep = "~")), qual_dat)
m_demFT_8 <- lm(as.formula(paste("ft_dem_sc", RHS_8, sep = "~")), qual_dat)

m_immig_4 <- lm(as.formula(paste("immig_sc", RHS_4, sep = "~")), qual_dat)
m_immig_8 <- lm(as.formula(paste("immig_sc", RHS_8, sep = "~")), qual_dat)

# white
m_immig_4_wht <- lm(as.formula(paste("immig_sc", RHS_4_immig, sep = "~")), subset(qual_dat, white == 1))
m_immig_8_wht <- lm(as.formula(paste("immig_sc", RHS_8_immig, sep = "~")), subset(qual_dat, white == 1))
# non-white
m_immig_4_nwht <- lm(as.formula(paste("immig_sc", RHS_4_immig, sep = "~")), subset(qual_dat, white == 0))
m_immig_8_nwht <- lm(as.formula(paste("immig_sc", RHS_8_immig, sep = "~")), subset(qual_dat, white == 0))

m_gay_4 <- lm(as.formula(paste("gay_sc", RHS_4, sep = "~")), qual_dat)
m_gay_8 <- lm(as.formula(paste("gay_sc", RHS_8, sep = "~")), qual_dat)

m_cjs_pun_4 <- lm(as.formula(paste("cjs_pun_sc", RHS_4, sep = "~")), qual_dat)
m_cjs_pun_8 <- lm(as.formula(paste("cjs_pun_sc", RHS_8, sep = "~")), qual_dat)

# ** Table A16 ---------------------------------------------------------------
# editing objects for formatting
m_trumpFT_4_p <- m_trumpFT_4
m_trumpFT_8_p <- m_trumpFT_8
m_repFT_4_p <- m_repFT_4
m_repFT_8_p <- m_repFT_8
m_demFT_4_p <- m_demFT_4
m_demFT_8_p <- m_demFT_8
m_immig4_wht_p <- m_immig_4_wht
m_immig8_wht_p <- m_immig_8_wht
m_immig4_nwht_p <- m_immig_4_nwht
m_immig8_nwht_p <- m_immig_8_nwht
m_gay4_p <- m_gay_4
m_gay8_p <- m_gay_8
m_cjs_pun4_p <- m_cjs_pun_4
m_cjs_pun8_p <- m_cjs_pun_8
names(m_trumpFT_4_p$coefficients) <- names(coef(m_trumpFT_4))
names(m_trumpFT_8_p$coefficients) <- names(coef(m_trumpFT_4))
names(m_repFT_4_p$coefficients) <- names(coef(m_repFT_4))
names(m_repFT_8_p$coefficients) <- names(coef(m_repFT_4))
names(m_demFT_4_p$coefficients) <- names(coef(m_demFT_4))
names(m_demFT_8_p$coefficients) <- names(coef(m_demFT_4))
names(m_immig4_wht_p$coefficients) <- names(coef(m_immig_4_wht))
names(m_immig8_wht_p$coefficients) <- names(coef(m_immig_4_wht))
names(m_immig4_nwht_p$coefficients) <- names(coef(m_immig_4_wht))
names(m_immig8_nwht_p$coefficients) <- names(coef(m_immig_4_wht))
names(m_gay4_p$coefficients) <- names(coef(m_gay_4))
names(m_gay8_p$coefficients) <- names(coef(m_gay_4))
names(m_cjs_pun4_p$coefficients) <- names(coef(m_cjs_pun_4))
names(m_cjs_pun8_p$coefficients) <- names(coef(m_cjs_pun_4))


stargazer(m_trumpFT_4_p, m_trumpFT_8_p,m_repFT_4_p, m_repFT_8_p, m_demFT_4_p, m_demFT_8_p,
          m_immig4_wht_p, m_immig8_wht_p, 
          m_immig4_nwht_p, m_immig8_nwht_p, 
          m_gay4_p, m_gay8_p, m_cjs_pun4_p, m_cjs_pun8_p,
          title = "Authoritarianism and Attitudes",
          model.numbers = T, dep.var.caption = "",
          covariate.labels = c("Authoritarianism", "Age", "Female", "White", "Black","South", "Religiosity", "Catholic", "Protestant", "Evangelical", "No Religion", "Income", "Income--Missing", "Education", "Racial Resentment"),
          column.separate = c(2,2,2,4,2,2),
          column.labels = c("Trump FT", "Rep Party FT", "Dem Party FT", "Immigration", "Gay Rights", "CJS"),
          dep.var.labels.include = F,
          header = F, initial.zero = F,
          type = "text", 
          #out = "~/desktop/tabA16.html",
          digits = 3, omit.stat = c("f", "adj.rsq"), df = F, no.space = T)


