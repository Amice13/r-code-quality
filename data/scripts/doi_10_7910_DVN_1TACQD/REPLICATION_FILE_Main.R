# The analysis was run with R version 4.4.2 (2024-10-31)

rm(list=ls())
library(dplyr)
library(tidyr)
library(foreign)
library(haven)
library(readr)
library(readxl)
library(haven)
library(ggplot2)
library(ggrepel)
library(WDI)
library(countrycode)
library(data.table)
library(foreign)
library(stringr)
library(grid)
library(gridExtra)
library(ggpubr)
library(timeDate)
library(openxlsx)
library(tidyverse)
library(ordinal)
library(MASS)
library(ggeffects)
library(effects)
library(marginaleffects)
library(haven)
#library(fwildclusterboot)
library(boot)
library(margins)
library(lmtest)
library(stargazer)
library(DescTools)
library(brant)
library(nnet)
library(lmtest)
library(sandwich)
library(patchwork)

# Adjust this path to your local setup
setwd("~/Downloads/ReplicationTest")
# Load the necessary packages
forex_int_boj_d <- readRDS("japan_dataset.rds")



################################################################################
################### Table 1: Dynamic Ordered Probit models #####################
################################################################################

# Keep a numeric for linear models as robustness checks later (which do not accept factor variables)
forex_int_boj_d$interventionOrdinal_boj_numeric <- forex_int_boj_d$interventionOrdinal_boj

forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)

model1 = clm(interventionOrdinal_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model2 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model3 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model4 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model5 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model6 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model7 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
summary(model7)

stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="table1_japan.html")

stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="table1_japan.tex")


# Adding other goodness of fit measures specific to Ordered Categorical models
# Need to fit the model with polr rather than clm
# First, make sure that clm and polr actually oput the same results. Yes they are identical
summary(polr(interventionOrdinal_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
summary(clm(interventionOrdinal_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d, link = "probit"))
# Second, refit all models
model1 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
model2 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model3 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model4 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
# Third calculate the new measures and add the manually to the table
# The following are manually inserted in the last row of the Tavle
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3, which = "McFadden"), 3)
round(DescTools::PseudoR2(model4, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)



##### Quantities of interest


###########################################################################################
####### Figure 4: Predicted Probabilities with Block-Bootstrapped Standard errors #########
###########################################################################################

# Notice that we use the polr package instead of clm package for the bootstrapped as the former is quicker

######### ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 
######## MODEL 1: ONLY POLARITY and elections AS IV
forex_int_boj_d <- dplyr::mutate(forex_int_boj_d, month_year = format(date, "%Y-%m"))
# Define function to generate month-year blocks
month_year <- function(date) {
  format(date, "%Y-%m")
}
# Mutate data frame to add month_year variable
forex_int_boj_d <- forex_int_boj_d %>%
  mutate(month_year = format(date, "%Y-%m"))
# Convert interventionOrdinal_boj to factor if necessary
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
# Define number of blocks
blocks <- unique(forex_int_boj_d$month_year)
n_blocks <- length(blocks)
# Function to perform block bootstrap
block_bootstrap <- function(data, blocks) {
  bootstrapped_data <- lapply(sample(blocks, replace = TRUE), function(block) {
    data %>%
      filter(month_year == block)
  })
  return(bind_rows(bootstrapped_data))
}
# Number of bootstrap iterations
sims <- 500
# Perform block bootstrap
set.seed(123)  # Set seed for reproducibility
bootstrapped_data <- replicate(sims, block_bootstrap(forex_int_boj_d, blocks), simplify = FALSE)
# Fit model on each bootstrapped data.frame
models <- suppressWarnings(lapply(bootstrapped_data, function(boot_data) {
  polr(interventionOrdinal_boj ~ polarity_l + pre_election + post_election, data = boot_data, Hess = TRUE,method = "probit")
}))
# Create new dataframe for predictions
# Check vingtile of focus variable
vingtile <- quantile(forex_int_boj_d$polarity, probs = seq(0.05, 0.95, by = 0.05))
# Add them manually
my_test_dat <- data.frame(
  polarity_l = vingtile,
  pre_election = rep(mean(forex_int_boj_d$pre_election), 19),
  post_election = rep(mean(forex_int_boj_d$post_election), 19)
)
# Get test predictions into a data.frame
probs_boot <- lapply(models, function(model) {
  marginaleffects::predictions(model, newdata = my_test_dat, type = "probs")
})
# Combine predictions from all bootstrapped models
probs_boot_df <- do.call(rbind, probs_boot)
# Compute confidence intervals
predictions <- probs_boot_df %>%
  group_by(group, rowid) %>%
  summarise(conf.low.btsr = quantile(estimate, probs = 0.025),
            predicted.btsr = mean(estimate), 
            conf.high.btsr = quantile(estimate, probs = 0.975))

# We want the SE to be bootstrapped but the predicted probability point estimates come from the original model. Hence,
olm1 = clm(interventionOrdinal_boj~polarity_l + pre_election + post_election, data = forex_int_boj_d, link = "probit")
val <- quantile(forex_int_boj_d$polarity, probs = seq(0.05, 0.95, by = 0.05))
#val <- c(-0.05596471, 0.10338397,  0.21573449 , 0.29759424 , 0.36377347 , 0.43500379 , 0.49377215 , 0.55630538 , 0.61896748 , 0.68024899 , 0.74382960 , 0.80762977 , 0.89927017,  0.98341138 , 1.09686725 , 1.22760698 , 1.38975086 , 1.61730031 , 2.08600549)
ggpredictions_olm1 = data.frame(ggpredict(olm1, terms ="polarity_l [val]"))
ggpredictions_olm1 <- as.data.frame(ggpredictions_olm1)
ggpredictions_olm1$x <- factor(ggpredictions_olm1$x)
ggpredictions_olm1 <- dplyr::select(ggpredictions_olm1, -group   )
ggpredictions_olm1 <- dplyr::rename(ggpredictions_olm1, conf.high.delta = conf.high,  conf.low.delta = conf.low, group = response.level)
ggpredictions_olm1 <- ggpredictions_olm1 %>% arrange(group, x)  %>% dplyr::select(-group)
predictions <- predictions %>% arrange(group, rowid)
merged <- cbind(predictions, ggpredictions_olm1)
positive <- dplyr::filter(merged, group ==1)
negative <- dplyr::filter(merged, group ==-1)
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))

ggplot_positive_btsr1 <- ggplot(positive, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_btsr1

ggplot_negative_btsr1 <- ggplot(negative, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_btsr1




######### ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 
######## MODEL 2: Adding LDV

# Perform block bootstrap
# Number of bootstrap iterations
sims <- 500
set.seed(456)  # Set seed for reproducibility
bootstrapped_data <- replicate(sims, block_bootstrap(forex_int_boj_d, blocks), simplify = FALSE)
# Fit model on each bootstrapped data.frame
models <- suppressWarnings(lapply(bootstrapped_data, function(boot_data) {
  polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election, data = boot_data, Hess = TRUE,method = "probit")
}))
# Create new dataframe for predictions
my_test_dat <- data.frame(
  polarity_l = vingtile,
  pre_election = rep(mean(forex_int_boj_d$pre_election), 19),
  post_election = rep(mean(forex_int_boj_d$post_election), 19),
  interventionOrdinal_boj_l = rep(mean(forex_int_boj_d$interventionOrdinal_boj_l, na.rm=T), 19)
)
# Get test predictions into a data.frame
probs_boot <- lapply(models, function(model) {
  marginaleffects::predictions(model, newdata = my_test_dat, type = "probs")
})
# Combine predictions from all bootstrapped models
probs_boot_df <- do.call(rbind, probs_boot)
# Compute confidence intervals
predictions <- probs_boot_df %>%
  group_by(group, rowid) %>%
  summarise(conf.low.btsr = quantile(estimate, probs = 0.025, na.rm=T),
            predicted.btsr = mean(estimate), 
            conf.high.btsr = quantile(estimate, probs = 0.975, na.rm=T))

# We want the SE to be bootstrapped but the predicted probability point estimates come from the original model. hence,
olm2 = clm(interventionOrdinal_boj~polarity_l + interventionOrdinal_boj_l + pre_election + post_election, data = forex_int_boj_d, link = "probit")
val <- quantile(forex_int_boj_d$polarity, probs = seq(0.05, 0.95, by = 0.05))
ggpredictions_olm2 = data.frame(ggpredict(olm2, terms ="polarity_l [val]"))
ggpredictions_olm2 <- as.data.frame(ggpredictions_olm2)
ggpredictions_olm2$x <- factor(ggpredictions_olm2$x)
ggpredictions_olm2 <- dplyr::select(ggpredictions_olm2, -group   )
ggpredictions_olm2 <- dplyr::rename(ggpredictions_olm2, conf.high.delta = conf.high,  conf.low.delta = conf.low, group = response.level)
ggpredictions_olm2 <- ggpredictions_olm2 %>% arrange(group, x)  %>% dplyr::select(-group)
predictions <- predictions %>% arrange(group, rowid)
merged <- cbind(predictions, ggpredictions_olm2)
positive <- dplyr::filter(merged, group ==1)
negative <- dplyr::filter(merged, group ==-1)
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))

ggplot_positive_btsr2 <- ggplot(positive, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_btsr2


ggplot_negative_btsr2 <- ggplot(negative, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_btsr2


######### ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 
######## MODEL 3: Adding er_short_target

# Perform block bootstrap
# Number of bootstrap iterations
sims <- 500
set.seed(456)  # Set seed for reproducibility
bootstrapped_data <- replicate(sims, block_bootstrap(forex_int_boj_d, blocks), simplify = FALSE)
# Fit model on each bootstrapped data.frame
models <- suppressWarnings(lapply(bootstrapped_data, function(boot_data) {
  polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election, data = boot_data, Hess = TRUE,method = "probit")
}))
# Create new dataframe for predictions
my_test_dat <- data.frame(
  polarity_l = vingtile,
  pre_election = rep(mean(forex_int_boj_d$pre_election), 19),
  post_election = rep(mean(forex_int_boj_d$post_election), 19),
  interventionOrdinal_boj_l = rep(mean(forex_int_boj_d$interventionOrdinal_boj_l, na.rm=T), 19),
  er_short_target = rep(mean(forex_int_boj_d$er_short_target, na.rm=T), 19)
)
# Get test predictions into a data.frame
probs_boot <- lapply(models, function(model) {
  marginaleffects::predictions(model, newdata = my_test_dat, type = "probs")
})
# Combine predictions from all bootstrapped models
probs_boot_df <- do.call(rbind, probs_boot)
# Compute confidence intervals
predictions <- probs_boot_df %>%
  group_by(group, rowid) %>%
  summarise(conf.low.btsr = quantile(estimate, probs = 0.025, na.rm=T),
            predicted.btsr = mean(estimate), 
            conf.high.btsr = quantile(estimate, probs = 0.975, na.rm=T))
# We want the SE to be bootstrapped but the predicted probability point estimates come from the original model. hence,
olm3 = clm(interventionOrdinal_boj~polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election, data = forex_int_boj_d, link = "probit")
val <- quantile(forex_int_boj_d$polarity, probs = seq(0.05, 0.95, by = 0.05))
ggpredictions_olm3 = data.frame(ggpredict(olm3, terms ="polarity_l [val]"))
ggpredictions_olm3 <- as.data.frame(ggpredictions_olm3)
ggpredictions_olm3$x <- factor(ggpredictions_olm3$x)
ggpredictions_olm3 <- dplyr::select(ggpredictions_olm3, -group   )
ggpredictions_olm3 <- dplyr::rename(ggpredictions_olm3, conf.high.delta = conf.high,  conf.low.delta = conf.low, group = response.level)
ggpredictions_olm3 <- ggpredictions_olm3 %>% arrange(group, x)  %>% dplyr::select(-group)
predictions <- predictions %>% arrange(group, rowid)
merged <- cbind(predictions, ggpredictions_olm3)
positive <- dplyr::filter(merged, group ==1)
negative <- dplyr::filter(merged, group ==-1)
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))

ggplot_positive_btsr3 <- ggplot(positive, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_btsr3

ggplot_negative_btsr3 <- ggplot(negative, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_btsr3


######### ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 
######## MODEL 4: Adding er_medium_target

# Perform block bootstrap
# Number of bootstrap iterations
sims <- 500
set.seed(456)  # Set seed for reproducibility
bootstrapped_data <- replicate(sims, block_bootstrap(forex_int_boj_d, blocks), simplify = FALSE)
# Fit model on each bootstrapped data.frame
models <- suppressWarnings(lapply(bootstrapped_data, function(boot_data) {
  polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election + post_election, data = boot_data, Hess = TRUE, method = "probit")
}))
# Create new dataframe for predictions
my_test_dat <- data.frame(
  polarity_l = vingtile,
  pre_election = rep(mean(forex_int_boj_d$pre_election), 19),
  post_election = rep(mean(forex_int_boj_d$post_election), 19),
  interventionOrdinal_boj_l = rep(mean(forex_int_boj_d$interventionOrdinal_boj_l, na.rm=T), 19),
  er_short_target = rep(mean(forex_int_boj_d$er_short_target, na.rm=T), 19),
  er_medium_target = rep(mean(forex_int_boj_d$er_medium_target, na.rm=T), 19)
)
# Get test predictions into a data.frame
probs_boot <- lapply(models, function(model) {
  marginaleffects::predictions(model, newdata = my_test_dat, type = "probs")
})
# Combine predictions from all bootstrapped models
probs_boot_df <- do.call(rbind, probs_boot)
# Compute confidence intervals
predictions <- probs_boot_df %>%
  group_by(group, rowid) %>%
  summarise(conf.low.btsr = quantile(estimate, probs = 0.025, na.rm=T),
            predicted.btsr = mean(estimate), 
            conf.high.btsr = quantile(estimate, probs = 0.975, na.rm=T))
# We want the SE to be bootstrapped but the predicted probability point estimates come from the original model. hence,
#ols1 <- polr(interventionOrdinal_boj ~ polarity_l + pre_election + post_election, data = forex_int_boj_d, Hess = TRUE, method = "probit")
olm4 = clm(interventionOrdinal_boj~polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election + post_election, data = forex_int_boj_d, link = "probit")
summary(olm4)
val <- quantile(forex_int_boj_d$polarity, probs = seq(0.05, 0.95, by = 0.05))
ggpredictions_olm4 = data.frame(ggpredict(olm4, terms ="polarity_l [val]"))
ggpredictions_olm4 <- as.data.frame(ggpredictions_olm4)
ggpredictions_olm4$x <- factor(ggpredictions_olm4$x)
ggpredictions_olm4 <- dplyr::select(ggpredictions_olm4, -group   )
ggpredictions_olm4 <- dplyr::rename(ggpredictions_olm4, conf.high.delta = conf.high,  conf.low.delta = conf.low, group = response.level)
ggpredictions_olm4 <- ggpredictions_olm4 %>% arrange(group, x)  %>% dplyr::select(-group)
predictions <- predictions %>% arrange(group, rowid)
merged <- cbind(predictions, ggpredictions_olm4)
positive <- dplyr::filter(merged, group ==1)
negative <- dplyr::filter(merged, group ==-1)
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))

ggplot_positive_btsr4 <- ggplot(positive, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_btsr4

ggplot_negative_btsr4 <- ggplot(negative, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_btsr4


######### ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 
######## MODEL 5: Adding er_long_target1 (y year MA)

# Perform block bootstrap
# Number of bootstrap iterations
sims <- 500
set.seed(456)  # Set seed for reproducibility
bootstrapped_data <- replicate(sims, block_bootstrap(forex_int_boj_d, blocks), simplify = FALSE)
# Fit model on each bootstrapped data.frame
models <- suppressWarnings(lapply(bootstrapped_data, function(boot_data) {
  polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election, data = boot_data, Hess = TRUE, method = "probit")
}))
# Create new dataframe for predictions
my_test_dat <- data.frame(
  polarity_l = vingtile,
  pre_election = rep(mean(forex_int_boj_d$pre_election), 19),
  post_election = rep(mean(forex_int_boj_d$post_election), 19),
  interventionOrdinal_boj_l = rep(mean(forex_int_boj_d$interventionOrdinal_boj_l, na.rm=T), 19),
  er_short_target = rep(mean(forex_int_boj_d$er_short_target, na.rm=T), 19),
  er_medium_target = rep(mean(forex_int_boj_d$er_medium_target, na.rm=T), 19),
  er_long_target1 = rep(mean(forex_int_boj_d$er_long_target1, na.rm=T), 19)
)
# Get test predictions into a data.frame
probs_boot <- lapply(models, function(model) {
  marginaleffects::predictions(model, newdata = my_test_dat, type = "probs")
})
# Combine predictions from all bootstrapped models
probs_boot_df <- do.call(rbind, probs_boot)
# Compute confidence intervals
predictions <- probs_boot_df %>%
  group_by(group, rowid) %>%
  summarise(conf.low.btsr = quantile(estimate, probs = 0.025, na.rm=T),
            predicted.btsr = mean(estimate), 
            conf.high.btsr = quantile(estimate, probs = 0.975, na.rm=T))
# We want the SE to be bootstrapped but the predicted probability point estimates come from the original model. hence,
#ols1 <- polr(interventionOrdinal_boj ~ polarity_l + pre_election + post_election, data = forex_int_boj_d, Hess = TRUE, method = "probit")
olm5 = clm(interventionOrdinal_boj~polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election, data = forex_int_boj_d, link = "probit")
summary(olm5)
val <- quantile(forex_int_boj_d$polarity, probs = seq(0.05, 0.95, by = 0.05))
ggpredictions_olm5 = data.frame(ggpredict(olm5, terms ="polarity_l [val]"))
ggpredictions_olm5 <- as.data.frame(ggpredictions_olm5)
ggpredictions_olm5$x <- factor(ggpredictions_olm5$x)
ggpredictions_olm5 <- dplyr::select(ggpredictions_olm5, -group   )
ggpredictions_olm5 <- dplyr::rename(ggpredictions_olm5, conf.high.delta = conf.high,  conf.low.delta = conf.low, group = response.level)
ggpredictions_olm5 <- ggpredictions_olm5 %>% arrange(group, x)  %>% dplyr::select(-group)
predictions <- predictions %>% arrange(group, rowid)
merged <- cbind(predictions, ggpredictions_olm5)
positive <- dplyr::filter(merged, group ==1)
negative <- dplyr::filter(merged, group ==-1)
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))

ggplot_positive_btsr5 <- ggplot(positive, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("Polarity Values (5th to 95th percentile)") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_btsr5

ggplot_negative_btsr5 <- ggplot(negative, aes(x = rowid, y = predicted.btsr)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.btsr, ymax = conf.high.btsr)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("Polarity Values (5th to 95th percentile)") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_btsr5


################################################################################
################################# Figure 3 #####################################
################################################################################
# Plotting graphs with bootstrapped standard errors
combined_plots_model1_2_3_4_5_btsrSE <- ggarrange(ggplot_positive_btsr1, ggplot_negative_btsr1,
                                                  ggplot_positive_btsr2, ggplot_negative_btsr2,
                                                  ggplot_positive_btsr3, ggplot_negative_btsr3,
                                                  ggplot_positive_btsr4, ggplot_negative_btsr4,
                                                  ggplot_positive_btsr5, ggplot_negative_btsr5,
                                                  ncol = 2, nrow = 5,hjust=-0.01,
                                                  labels = c("","(1)","","(2)","","(3)","","(4)","","(5)" ))
print(combined_plots_model1_2_3_4_5_btsrSE)

ggsave("Fig3.pdf", plot = combined_plots_model1_2_3_4_5_btsrSE, width = 7.5, height = 9, units = "in")





################################################
# Not in paper but mentioned in fn.13
################################################

## Footnote 13: "We code the pre- and post-election dummy to capture the four weeks 
# (twenty business days) before and after the election. 
# We also experimented with different time windows, i.e one, two and eight weeks. 
# The results are substantively similar."

#### Here we use different time structure for pre and post election period
#### The original variable covered 20 days (4 weeks)

names(forex_int_boj_d)

### Original model for reference
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
model1 = clm(interventionOrdinal_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model2 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model3 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model4 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model5 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model6 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model7 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"))

# one week.
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
model1 = clm(interventionOrdinal_boj ~ polarity_l + pre_election_1w + post_election_1w,data = forex_int_boj_d, link = "probit")
model2 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election_1w + post_election_1w,data = forex_int_boj_d, link = "probit")
model3 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election_1w + post_election_1w,data = forex_int_boj_d, link = "probit")
model4 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election_1w + post_election_1w,data = forex_int_boj_d, link = "probit")
model5 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election_1w + post_election_1w,data = forex_int_boj_d, link = "probit")
model6 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election_1w + post_election_1w,data = forex_int_boj_d, link = "probit")
model7 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election_1w + post_election_1w,data = forex_int_boj_d, link = "probit")
stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"))


# Two weeks.
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
model1 = clm(interventionOrdinal_boj ~ polarity_l + pre_election_2w + post_election_2w,data = forex_int_boj_d, link = "probit")
model2 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election_2w + post_election_2w,data = forex_int_boj_d, link = "probit")
model3 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election_2w + post_election_2w,data = forex_int_boj_d, link = "probit")
model4 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election_2w + post_election_2w,data = forex_int_boj_d, link = "probit")
model5 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election_2w + post_election_2w,data = forex_int_boj_d, link = "probit")
model6 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election_2w + post_election_2w,data = forex_int_boj_d, link = "probit")
model7 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election_2w + post_election_2w,data = forex_int_boj_d, link = "probit")
stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"))

# eight weeks.
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
model1 = clm(interventionOrdinal_boj ~ polarity_l + pre_election_8w + post_election_8w,data = forex_int_boj_d, link = "probit")
model2 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election_8w + post_election_8w,data = forex_int_boj_d, link = "probit")
model3 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election_8w + post_election_8w,data = forex_int_boj_d, link = "probit")
model4 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election_8w + post_election_8w,data = forex_int_boj_d, link = "probit")
model5 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election_8w + post_election_8w,data = forex_int_boj_d, link = "probit")
model6 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election_8w + post_election_8w,data = forex_int_boj_d, link = "probit")
model7 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election_8w + post_election_8w,data = forex_int_boj_d, link = "probit")
stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"))









