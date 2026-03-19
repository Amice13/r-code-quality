
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

###############################################################################
############################     APPENDIX    ##################################
################################################################################



################################################################################
###################  Fig A1  ###################################################
################################################################################

graph <- ggplot(forex_int_boj_d, aes(x = polarity)) +
  geom_density(fill = "black", color = "gray10", alpha = 0.5) +
  labs(title = "", x = "Polarity", y = "Density")
print(graph)
ggsave("Fig_A1.pdf", plot = graph, width = 8, height = 5, units = "in")


###########################################################################################
####### Figure A2: Predicted Probabilities with Delta Method Standard errors #########
###########################################################################################

# Keep a numeric for linear models as robustness checks later (which do not accept factor variables)
forex_int_boj_d$interventionOrdinal_boj_numeric <- forex_int_boj_d$interventionOrdinal_boj

forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)

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

ggplot_positive_delta1 <- ggplot(positive, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_delta1

ggplot_negative_delta1 <- ggplot(negative, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_delta1



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


ggplot_positive_delta2 <- ggplot(positive, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_delta2


ggplot_negative_delta2 <- ggplot(negative, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_delta2

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


ggplot_positive_delta3 <- ggplot(positive, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_delta3


ggplot_negative_delta3 <- ggplot(negative, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_delta3

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

ggplot_positive_delta4 <- ggplot(positive, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_delta4

ggplot_negative_delta4 <- ggplot(negative, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_delta4


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

ggplot_positive_delta5 <- ggplot(positive, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("Polarity Values (5th to 95th percentile)") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_positive_delta5

ggplot_negative_delta5 <- ggplot(negative, aes(x = rowid, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low.delta, ymax = conf.high.delta)) + 
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("Polarity Values (5th to 95th percentile)") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_negative_delta5



################################################################################
#################### Figure A2  ##################
################################################################################

# Equivalent to the graph above but with delta standard errors (For the Appendix)
# Plotting graphs with delta method standard errors
combined_plots_model1_2_3_4_5_deltaSE <- ggarrange(ggplot_positive_delta1, ggplot_negative_delta1,
                                                   ggplot_positive_delta2, ggplot_negative_delta2,
                                                   ggplot_positive_delta3, ggplot_negative_delta3,
                                                   ggplot_positive_delta4, ggplot_negative_delta4,
                                                   ggplot_positive_delta5, ggplot_negative_delta5,
                                                   ncol = 2, nrow = 5,hjust=-0.01,
                                                   labels = c("","(1)","","(2)","","(3)","","(4)","","(5)" ))
print(combined_plots_model1_2_3_4_5_deltaSE)

ggsave("FigA2_delta.pdf", plot = combined_plots_model1_2_3_4_5_deltaSE, width = 7.5, height = 9, units = "in")





################################################################################
################################# Table A3 #####################################
################################################################################

# Replacing target exchange rates with raw exchange rates in model 3,4,5,6,7 above.
model1_app = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model2_app = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model3_app = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + er_j_1yL + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model4_app = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + er_j_3yL + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model5_app = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + er_j_5yL + pre_election + post_election,data = forex_int_boj_d, link = "probit")

stargazer(model1_app, model2_app, model3_app, model4_app, model5_app,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short (t-1)", "ER Medium (t-20)", "ER Long (1 year mov.av.)", "ER Long (3 year mov.av.)", "ER Long (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="TableA3_japan.tex")

stargazer(model1_app, model2_app, model3_app, model4_app, model5_app,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short (t-1)", "ER Medium (t-20)", "ER Long (1 year mov.av.)", "ER Long (3 year mov.av.)", "ER Long (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="TableA3_japan.html")

# Use polr to extract goodness of fit measures
model1_app = polr(interventionOrdinal_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit")
model2_app = polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit")
model3_app = polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit")
model4_app = polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit")
model5_app = polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + er_j_1yL + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit")
# Third calculate the new measures and add the manually to the table
round(DescTools::PseudoR2(model1_app, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2_app, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3_app, which = "McFadden"), 3)
round(DescTools::PseudoR2(model4_app, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5_app, which = "McFadden"), 3)




################################################################################
################################# Figure A2. #####################################
################################################################################

# Model 1 and Model 2 do not contain the lagged ER on the RHS, hence nothing changes.
# Below the graphs for Model 3,4,5,6,7 
dev.off()
#### Model 3: short ER (instead of short target)
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
olm3 = clm(interventionOrdinal_boj~polarity_l + interventionOrdinal_boj_l + er_j_l + pre_election + post_election, data = forex_int_boj_d, link = "probit")
# Extract the vingtile over polarity
#val <- quantile(forex_int_boj_d$polarity, probs = seq(0.05, 0.95, by = 0.05))
# Calculate predicted probability
ggpredictions_olm3 = data.frame(ggpredict(olm3, terms ="polarity_l [val]"))
ggpredictions_olm3 <- as.data.frame(ggpredictions_olm3)
ggpredictions_olm3$x <- factor(ggpredictions_olm3$x)
# Create two frames for negative, positive (2 = no intervention)
negative <- dplyr::filter(ggpredictions_olm3, response.level ==1)
positive <- dplyr::filter(ggpredictions_olm3, response.level ==3)
# Set the desired Y-axis limits
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))
# Create the g_negative plot with specified Y-axis limits
g_negative_model1_APP <- ggplot(negative, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") +ylab("") +
  #xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Appreciation-inducing intervention)") +
  #ggtitle("Appreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))

# Create the g_positive plot with specified Y-axis limits
g_positive_model1_APP <- ggplot(positive, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + xlab("") + ylab("") +
  #xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Depreciation-inducing intervention)") +
  #ggtitle("Depreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))


#### Model 4: medium ER (instead of medium target)
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
olm4 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + pre_election + post_election,data = forex_int_boj_d, link = "probit")
# By default, ggpredict holds the covariates at their means (unless specified explictly)
ggpredictions_olm4 = data.frame(ggpredict(olm4, terms ="polarity_l [val]"))
ggpredictions_olm4 <- as.data.frame(ggpredictions_olm4)
ggpredictions_olm4$x <- factor(ggpredictions_olm4$x)

negative <- dplyr::filter(ggpredictions_olm4, response.level ==1)
positive <- dplyr::filter(ggpredictions_olm4, response.level ==3)

# Set the desired Y-axis limits
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))
# Create the g_negative plot with specified Y-axis limits
g_negative_model2_APP <- ggplot(negative, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +ylab("") +xlab("") +
  #xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Appreciation-inducing intervention)") +
  #ggtitle("Appreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))

# Create the g_positive plot with specified Y-axis limits
g_positive_model2_APP <- ggplot(positive, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +ylab("") +xlab("") +
  #xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Depreciation-inducing intervention)") +
  #ggtitle("Depreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))


#### Model 5: long ER (instead of long target) (1 year)
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
olm5 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + er_j_1yL + pre_election + post_election,data = forex_int_boj_d, link = "probit")
ggpredictions_olm5 = data.frame(ggpredict(olm5, terms ="polarity_l [val]"))
ggpredictions_olm5 <- as.data.frame(ggpredictions_olm5)
ggpredictions_olm5$x <- factor(ggpredictions_olm5$x)

negative <- dplyr::filter(ggpredictions_olm5, response.level ==1)
positive <- dplyr::filter(ggpredictions_olm5, response.level ==3)
# Set the desired Y-axis limits
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))
# Create the g_negative plot with specified Y-axis limits
g_negative_model3_APP <- ggplot(negative, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +ylab("") +xlab("") +
  #xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Appreciation-inducing intervention)") +
  #ggtitle("Appreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))

# Create the g_positive plot with specified Y-axis limits
g_positive_model3_APP <- ggplot(positive, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +ylab("") +xlab("") +
  #xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Depreciation-inducing intervention)") +
  # ggtitle("Depreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))


#### Model 6: long ER (instead of long target) (3 year)
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
olm6 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + er_j_3yL + pre_election + post_election,data = forex_int_boj_d, link = "probit")
ggpredictions_olm6 = data.frame(ggpredict(olm6, terms ="polarity_l [val]"))
ggpredictions_olm6 <- as.data.frame(ggpredictions_olm6)
ggpredictions_olm6$x <- factor(ggpredictions_olm6$x)
negative <- dplyr::filter(ggpredictions_olm6, response.level ==1)
positive <- dplyr::filter(ggpredictions_olm6, response.level ==3)

# Set the desired Y-axis limits
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))
# Create the g_negative plot with specified Y-axis limits
g_negative_model4_APP <- ggplot(negative, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +ylab("") +xlab("") +
  #xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Appreciation-inducing intervention)") +
  #ggtitle("Appreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))

# Create the g_positive plot with specified Y-axis limits
g_positive_model4_APP <- ggplot(positive, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +ylab("")  +xlab("") +
  #xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Depreciation-inducing intervention)") +
  #ggtitle("Depreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))

#### Model 7: long ER (instead of long target) (5 year)
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)
olm7 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_j_l + er_j_1mL + er_j_5yL + pre_election + post_election,data = forex_int_boj_d, link = "probit")
ggpredictions_olm7 = data.frame(ggpredict(olm7, terms ="polarity_l [val]"))
ggpredictions_olm7 <- as.data.frame(ggpredictions_olm7)
ggpredictions_olm7$x <- factor(ggpredictions_olm7$x)
negative <- dplyr::filter(ggpredictions_olm7, response.level ==1)
positive <- dplyr::filter(ggpredictions_olm7, response.level ==3)

# Set the desired Y-axis limits
y_axis_limits <- range(c(negative$predicted-0.001, positive$predicted+0.035))
# Create the g_negative plot with specified Y-axis limits
g_negative_model5_APP <- ggplot(negative, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +ylab("") +
  xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Appreciation-inducing intervention)") +
  #ggtitle("Appreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))

# Create the g_positive plot with specified Y-axis limits
g_positive_model5_APP <- ggplot(positive, aes(x = x, y = predicted)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +ylab("") +
  xlab("Polarity Values (5th to 95th percentile)") +
  #ylab("Pr (Depreciation-inducing intervention)") +
  #ggtitle("Depreciation-inducing intervention") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))


# Combine the plots using grid.arrange

combined_plots_model1_2_3_4_5_annotated_APP <- ggarrange(g_positive_model1_APP, g_negative_model1_APP,
                                                         g_positive_model2_APP, g_negative_model2_APP,
                                                         g_positive_model3_APP, g_negative_model3_APP,
                                                         g_positive_model4_APP, g_negative_model4_APP,
                                                         g_positive_model5_APP, g_negative_model5_APP,hjust=-0.01,
                                                         ncol = 2, nrow = 5, labels = c("","(1)","","(2)","","(3)","","(4)","","(5)" ))
print(combined_plots_model1_2_3_4_5_annotated_APP)

ggsave("FigA3.pdf", plot = combined_plots_model1_2_3_4_5_annotated_APP, width = 7.5, height = 9, units = "in")



################################################################################
################################# Table A4. #####################################
################################################################################
# Brandt test for parallel trend assumption
model1 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
model2 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model3 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model4 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))

brant::brant(model1)
brant::brant(model2)
brant::brant(model3)
brant::brant(model4)
brant::brant(model5)
brant::brant(model6)
brant::brant(model7)


# Short and loong term target violates proportional odd assumption

# In light of the above, three robustness checks 

################################################################################
################################# Table Optional #####################################
###### These results are only mentioned in the Appendix: "First, for the models containing only one local
# violation (Model 4 and Model 7), we drop the offending variable, re-run the Brant test, and refit
# a simpler model that satisfies the proportional odds assumption. The results are similar to the
# original models and available in the replication package associated with this article."
################################################################################
# Focusing on Model 4,5,6,7
# Model 4: drop short ER
# 1) Drop the offending variable and check if any remaining variable violates PT
no_offending_var1 = polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_medium_target + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit")
brant::brant(no_offending_var1)
# 2) Re-fit using clm so that we get p values easily
no_offending_var1 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_medium_target + pre_election + post_election,data = forex_int_boj_d, link = "probit")
summary(no_offending_var1)
# Model 7
# 1) Drop the offending variable and check if any remaining variable violates PT
no_offending_var1 = polr(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit")
brant::brant(no_offending_var1)
# 2) Re-fit using clm so that we get p values easily
no_offending_var1 = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
summary(no_offending_var1)


################################################################################
################################# Table A5, A6 and Fig. A4, A5 #####################################
################################################################################

# Use a simpler set of binary probit models (stratified binomial models)
# Depreciation induucing intervention
#val <- c(-0.05596471, 0.10338397,  0.21573449 , 0.29759424 , 0.36377347 , 0.43500379 , 0.49377215 , 0.55630538 , 0.61896748 , 0.68024899 , 0.74382960 , 0.80762977 , 0.89927017,  0.98341138 , 1.09686725 , 1.22760698 , 1.38975086 , 1.61730031 , 2.08600549)
forex_int_boj_d$interventionOrdinal_boj <- as.factor(forex_int_boj_d$interventionOrdinal_boj)

model1 = glm(interventionBinary_dep_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
model2 = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model3 = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model4 = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model5 = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model6 = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model7 = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
# Goodness of fit measures (added manually to the table below)
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3, which = "McFadden"), 3)
round(DescTools::PseudoR2(model4, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)

stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="TableA5_japan.tex")


stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="TableA5_japan.html")


# Appreciation inducing intervention

model1 = glm(interventionBinary_app_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
model2 = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model3 = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model4 = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model5 = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model6 = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))
model7 = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d,  family = binomial(link = "probit"))

round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3, which = "McFadden"), 3)
round(DescTools::PseudoR2(model4, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)


stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="TableA6_japan.tex")

stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="TableA6_japan.html")

######################################################################################## 
######################## Fig A4 and A5 ############################################### 
######################################################################################## 

# Graphs for appreciation and depreciation inducing inetrvention (binary probit models)
# Graphing the results
#### Model 1
dev.off()

model1_dep = glm(interventionBinary_dep_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm1_dep = data.frame(ggpredict(model1_dep, terms ="polarity_l [val]"))
ggpredictions_glm1_dep <- as.data.frame(ggpredictions_glm1_dep)
ggpredictions_glm1_dep$x <- factor(ggpredictions_glm1_dep$x)
y_axis_limits <- range(c(ggpredictions_glm1_dep$predicted-0.005, ggpredictions_glm1_dep$predicted+0.020))

ggplot_model1_dep <- ggplot(ggpredictions_glm1_dep, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + 
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model1_dep

model1_app = glm(interventionBinary_app_boj ~ polarity_l + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm1_app = data.frame(ggpredict(model1_app, terms ="polarity_l [val]"))
ggpredictions_glm1_app <- as.data.frame(ggpredictions_glm1_app)
ggpredictions_glm1_app$x <- factor(ggpredictions_glm1_app$x)
y_axis_limits <- range(c(ggpredictions_glm1_app$predicted-0.005, ggpredictions_glm1_app$predicted+0.020))

ggplot_model1_app <- ggplot(ggpredictions_glm1_app, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model1_app


#### Model 2
model2_dep = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm2_dep = data.frame(ggpredict(model2_dep, terms ="polarity_l [val]"))
ggpredictions_glm2_dep <- as.data.frame(ggpredictions_glm2_dep)
ggpredictions_glm2_dep$x <- factor(ggpredictions_glm2_dep$x)
y_axis_limits <- range(c(ggpredictions_glm2_dep$predicted-0.005, ggpredictions_glm2_dep$predicted+0.020))

ggplot_model2_dep <- ggplot(ggpredictions_glm2_dep, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + 
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model2_dep

model2_app = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm2_app = data.frame(ggpredict(model2_app, terms ="polarity_l [val]"))
ggpredictions_glm2_app <- as.data.frame(ggpredictions_glm2_app)
ggpredictions_glm2_app$x <- factor(ggpredictions_glm2_app$x)
y_axis_limits <- range(c(ggpredictions_glm2_app$predicted-0.005, ggpredictions_glm2_app$predicted+0.020))

ggplot_model2_app <- ggplot(ggpredictions_glm2_app, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model2_app


#### Model 3
model3_dep = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm3_dep = data.frame(ggpredict(model3_dep, terms ="polarity_l [val]"))
ggpredictions_glm3_dep <- as.data.frame(ggpredictions_glm3_dep)
ggpredictions_glm3_dep$x <- factor(ggpredictions_glm3_dep$x)
y_axis_limits <- range(c(ggpredictions_glm3_dep$predicted-0.005, ggpredictions_glm3_dep$predicted+0.020))

ggplot_model3_dep <- ggplot(ggpredictions_glm3_dep, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + 
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model3_dep

model3_app = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm3_app = data.frame(ggpredict(model3_app, terms ="polarity_l [val]"))
ggpredictions_glm3_app <- as.data.frame(ggpredictions_glm3_app)
ggpredictions_glm3_app$x <- factor(ggpredictions_glm3_app$x)
y_axis_limits <- range(c(ggpredictions_glm3_app$predicted-0.005, ggpredictions_glm3_app$predicted+0.020))

ggplot_model3_app <- ggplot(ggpredictions_glm3_app, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model3_app



#### Model 4
model4_dep = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm4_dep = data.frame(ggpredict(model4_dep, terms ="polarity_l [val]"))
ggpredictions_glm4_dep <- as.data.frame(ggpredictions_glm4_dep)
ggpredictions_glm4_dep$x <- factor(ggpredictions_glm4_dep$x)
y_axis_limits <- range(c(ggpredictions_glm4_dep$predicted-0.005, ggpredictions_glm4_dep$predicted+0.020))

ggplot_model4_dep <- ggplot(ggpredictions_glm4_dep, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + 
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model4_dep

model4_app = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm4_app = data.frame(ggpredict(model4_app, terms ="polarity_l [val]"))
ggpredictions_glm4_app <- as.data.frame(ggpredictions_glm4_app)
ggpredictions_glm4_app$x <- factor(ggpredictions_glm4_app$x)
y_axis_limits <- range(c(ggpredictions_glm4_app$predicted-0.005, ggpredictions_glm4_app$predicted+0.020))

ggplot_model4_app <- ggplot(ggpredictions_glm4_app, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model4_app


#### Model 5
model5_dep = glm(interventionBinary_dep_boj ~ polarity_l + interventionBinary_dep_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm5_dep = data.frame(ggpredict(model5_dep, terms ="polarity_l [val]"))
ggpredictions_glm5_dep <- as.data.frame(ggpredictions_glm5_dep)
ggpredictions_glm5_dep$x <- factor(ggpredictions_glm5_dep$x)
y_axis_limits <- range(c(ggpredictions_glm5_dep$predicted-0.005, ggpredictions_glm5_dep$predicted+0.020))

ggplot_model5_dep <- ggplot(ggpredictions_glm5_dep, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) + 
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model5_dep

model5_app = glm(interventionBinary_app_boj ~ polarity_l + interventionBinary_app_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d, family = binomial(link = "probit"))
ggpredictions_glm5_app = data.frame(ggpredict(model5_app, terms ="polarity_l [val]"))
ggpredictions_glm5_app <- as.data.frame(ggpredictions_glm5_app)
ggpredictions_glm5_app$x <- factor(ggpredictions_glm5_app$x)
y_axis_limits <- range(c(ggpredictions_glm5_app$predicted-0.005, ggpredictions_glm5_app$predicted+0.020))

ggplot_model5_app <- ggplot(ggpredictions_glm5_app, aes(x = x, y = predicted)) +
  geom_point(size = 1) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  #geom_hline(yintercept = 0, lty = 2,na.rm = TRUE) +
  xlab("") + ylab("") +
  theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(limits = y_axis_limits, labels = scales::number_format(accuracy = 0.001))
ggplot_model5_app


dev.off()

combined_plots_dep_simpleprobit <- ggarrange(ggplot_model1_dep,
                                             ggplot_model2_dep,
                                             ggplot_model3_dep,
                                             ggplot_model4_dep,
                                             ggplot_model5_dep,
                                             ncol = 1, nrow = 5,hjust=-0.01,
                                             labels = c("(1)","(2)","(3)","(4)","(5)" ))
print(combined_plots_dep_simpleprobit)

combined_plots_app_simpleprobit <- ggarrange(ggplot_model1_app,
                                             ggplot_model2_app,
                                             ggplot_model3_app,
                                             ggplot_model4_app,
                                             ggplot_model5_app,
                                             ncol = 1, nrow = 5,hjust=-0.01,
                                             labels = c("(1)","(2)","(3)","(4)","(5)" ))
print(combined_plots_app_simpleprobit)


ggsave("FigA4_dep_simpleprobit.pdf", plot = combined_plots_dep_simpleprobit, width = 7.5, height = 9, units = "in")
ggsave("FigA5_app_simpleprobit.pdf", plot = combined_plots_app_simpleprobit, width = 7.5, height = 9, units = "in")



################################################################################
################################# Table A7. #####################################
################################################################################

# Multinomial 
# The nnet package does not provide much details. Calculate z stats and p value manually

# Model 1
forex_int_boj_d$interventionOrdinal_boj2 <- relevel(forex_int_boj_d$interventionOrdinal_boj, ref = "0")
mnm1 <- multinom(interventionOrdinal_boj2 ~ polarity_l + pre_election + post_election, data = forex_int_boj_d)
stargazer(mnm1,
          type = "text",
          apply.coef = exp,
          t.auto = FALSE,
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",
          notes.append = FALSE,
          notes.align = "l",
          title = "",
          dep.var.labels = c("Appreciation-Inducing", "Depreciation-Inducing"),
          covariate.labels = c("Polarity (t-1)", "Pre-election (Binary)", "Post-election (Binary)", "Constant"),
          digits = 3,
          keep = c("polarity_l"),   # Keep only Polarity coefficient
          out = "TableA7_japan.tex")

stargazer(mnm1,
          type = "text",
          apply.coef = exp,
          t.auto = FALSE,
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",
          notes.append = FALSE,
          notes.align = "l",
          title = "",
          dep.var.labels = c("Appreciation-Inducing", "Depreciation-Inducing"),
          covariate.labels = c("Polarity (t-1)", "Pre-election (Binary)", "Post-election (Binary)", "Constant"),
          digits = 3,
          keep = c("polarity_l"),   # Keep only Polarity coefficient
          out = "TableA7_japan.html")
# Double check if stargazer is calculating everything correctly.
summary(mnm1)
data.frame(t(exp(summary(mnm1)$coefficients)))
z <- summary(mnm1)$coefficients/summary(mnm1)$standard.errors
p <- (1 - pnorm(abs(z)))*2
exp(coef(mnm1)); data.frame(t(z)); data.frame(t(p))
# Notice that one has to add manually because it is is not in multinom output... 
# The Log Likelihood also not there although 
# the deviance is available and divided by -2 givens the log likelihood
mnm1$deviance/(-2)
#Good ness of fit
suppressWarnings(round(DescTools::PseudoR2(mnm1, which = "McFadden"), 3))
# Number of observations
nobs(mnm1)

# Model 2

mnm2 <- multinom(interventionOrdinal_boj2 ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election, data = forex_int_boj_d)
stargazer(mnm2,
          type = "text",
          apply.coef = exp,
          t.auto = FALSE,
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",
          notes.append = FALSE,
          notes.align = "l",
          title = "",
          dep.var.labels = c("Appreciation-Inducing", "Depreciation-Inducing"),
          covariate.labels = c("Polarity (t-1)", "Pre-election (Binary)", "Post-election (Binary)", "Constant"),
          digits = 3,
          keep = c("polarity_l"),   # Keep only Polarity coefficient
          out = "TableA7b_japan.tex")

mnm2$deviance/(-2)
suppressWarnings(round(DescTools::PseudoR2(mnm2, which = "McFadden"), 3))
nobs(mnm2)

# Model 3

mnm3 <- multinom(interventionOrdinal_boj2 ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election, data = forex_int_boj_d)
stargazer(mnm3,
          type = "text",
          apply.coef = exp,
          t.auto = FALSE,
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",
          notes.append = FALSE,
          notes.align = "l",
          title = "",
          dep.var.labels = c("Appreciation-Inducing", "Depreciation-Inducing"),
          covariate.labels = c("Polarity (t-1)", "Pre-election (Binary)", "Post-election (Binary)", "Constant"),
          digits = 3,
          keep = c("polarity_l"),   # Keep only Polarity coefficient
          out = "TableA7c_japan.tex")
mnm3$deviance/(-2)
suppressWarnings(round(DescTools::PseudoR2(mnm3, which = "McFadden"), 3))
nobs(mnm3)

# Model 4

mnm4 <- multinom(interventionOrdinal_boj2 ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target+ pre_election + post_election, data = forex_int_boj_d)
stargazer(mnm4,
          type = "text",
          apply.coef = exp,
          t.auto = FALSE,
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",
          notes.append = FALSE,
          notes.align = "l",
          title = "",
          dep.var.labels = c("Appreciation-Inducing", "Depreciation-Inducing"),
          covariate.labels = c("Polarity (t-1)", "Pre-election (Binary)", "Post-election (Binary)", "Constant"),
          digits = 3,
          keep = c("polarity_l"),   # Keep only Polarity coefficient
          out = "TableA7d_japan.tex")
mnm4$deviance/(-2)
suppressWarnings(round(DescTools::PseudoR2(mnm4, which = "McFadden"), 3))
nobs(mnm4)

# Model 5

mnm5 <- multinom(interventionOrdinal_boj2 ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target+ er_long_target1+ pre_election + post_election, data = forex_int_boj_d)
stargazer(mnm5,
          type = "text",
          apply.coef = exp,
          t.auto = FALSE,
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",
          notes.append = FALSE,
          notes.align = "l",
          title = "",
          dep.var.labels = c("Appreciation-Inducing", "Depreciation-Inducing"),
          covariate.labels = c("Polarity (t-1)", "Pre-election (Binary)", "Post-election (Binary)", "Constant"),
          digits = 3,
          keep = c("polarity_l"),   # Keep only Polarity coefficient
          out = "TableA7e_japan.tex")
mnm5$deviance/(-2)
suppressWarnings(round(DescTools::PseudoR2(mnm5, which = "McFadden"), 3))
nobs(mnm5)





################################################################################
################################# Table A8. #####################################
################################################################################
# Will our variables "survive" if we allow for variable selection algorithmically?

# stepAIC: Choose a model by AIC in a Stepwise Algorithm (starting from full model)
# requires excluding NAs from all column
forex_int_boj_d_no_NA <- dplyr::filter(forex_int_boj_d, !is.na(polarity_l))
forex_int_boj_d_no_NA <- dplyr::filter(forex_int_boj_d_no_NA, !is.na(interventionOrdinal_boj_l))
forex_int_boj_d_no_NA <- dplyr::filter(forex_int_boj_d_no_NA, !is.na(er_short_target))
forex_int_boj_d_no_NA <- dplyr::filter(forex_int_boj_d_no_NA, !is.na(er_medium_target))
forex_int_boj_d_no_NA <- dplyr::filter(forex_int_boj_d_no_NA, !is.na(er_long_target1))

full_model = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d_no_NA, link = "probit")
adjfit.step1 <- MASS::stepAIC(full_model, direction = "both", trace = FALSE)

full_model = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d_no_NA, link = "probit")
adjfit.step2 <- MASS::stepAIC(full_model, direction = "both", trace = FALSE)

full_model = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d_no_NA, link = "probit")
adjfit.step3 <- MASS::stepAIC(full_model, direction = "both", trace = FALSE)

stargazer(adjfit.step1, adjfit.step2, adjfit.step3,
          type = "text", 
          apply.coef = exp,
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Medium Target", "ER Long (3 year mov.av.)", "ER Long (5 year mov.av.)"),
          digits=3,
          keep.stat = c("n", "ll"),
          out="TableA8_japan.tex")

stargazer(adjfit.step1, adjfit.step2, adjfit.step3,
          type = "text", 
          apply.coef = exp,
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Medium Target", "ER Long (3 year mov.av.)", "ER Long (5 year mov.av.)"),
          digits=3,
          keep.stat = c("n", "ll"),
          out="TableA8_japan.html")


### Not in paper but mentioned in footnote 16
# Maybe trivial since the above always excludes the election dummies. But as shown below polarity remians even if we do not include dummies to begin with
full_model = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target1,data = forex_int_boj_d_no_NA, link = "probit")
adjfit.step1 <- MASS::stepAIC(full_model, direction = "both", trace = FALSE)

full_model = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target3,data = forex_int_boj_d_no_NA, link = "probit")
adjfit.step2 <- MASS::stepAIC(full_model, direction = "both", trace = FALSE)

full_model = clm(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_medium_target + er_long_target5,data = forex_int_boj_d_no_NA, link = "probit")
adjfit.step3 <- MASS::stepAIC(full_model, direction = "both", trace = FALSE)

stargazer(adjfit.step1, adjfit.step2, adjfit.step3,
          type = "text", 
          apply.coef = exp,
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Medium Target", "ER Long (3 year mov.av.)", "ER Long (5 year mov.av.)"),
          digits=3,
          keep.stat = c("n", "ll"))





################################################################################
################################# Table A9 #####################################
################################################################################

#### Extreme Bound analysis with Newey west SEs
# Running a million regressions

library(ExtremeBounds)

# Letting the lag based on T
# If no lag is provided (lag is NULL), it automatically computes a lag length based on the sample size using Andrews’ rule of thumb:0.75⋅T^1/3 as recommended in Stock and Watson (2015, eq (15.17))
se.neweywest_automatic <- function(model.object) {
  if (is.null(lag)) {
    lag <- trunc(0.75 * length(residuals(model.object))^(1/3))
  }
  model.fit <- NeweyWest(model.object, lag = NULL)
  out <- sqrt(diag(model.fit))
  return(out)
}
eba <- eba(formula =interventionOrdinal_boj_numeric ~ polarity_l + interventionOrdinal_boj_l + 
             er_short_target + er_medium_target + er_long_target1 + 
             pre_election + post_election, 
           data = forex_int_boj_d, k = 0:6, mu = 0,
           vif = 7, exclusive = NULL, reg.fun = lm, 
           se.fun = se.neweywest_automatic, include.fun = NULL, weights = "lri")
print(eba)

# Cannot use stargazer on eba objects
# See the corespondent columns for polarity under "Beta coefficients" for the first row of the table
eba$coefficients$weighted.mean$beta[2]
eba$coefficients$weighted.mean$se[2]
eba$coefficients$min$beta[2]
eba$coefficients$min$se[2]
eba$coefficients$max$beta[2]
eba$coefficients$max$se[2]
# See the corespondent columns for polarity under Distribution of beta coefficients for the second row of the table
eba$bounds$beta.below.mu[2]*100
eba$bounds$beta.above.mu[2]*100
eba$bounds$beta.significant[2]*100
eba$bounds$beta.significant.below.mu[2]*100
eba$bounds$beta.significant.above.mu[2]*100
# See the corespondent columns for polarity under Leamer's Extreme Bounds Analysis (EBA) for the third row of the table
eba$bounds$leamer.lower[2]
eba$bounds$leamer.upper[2]
eba$bounds$leamer.robust[2]
# See the corespondent columns for polarity under Sala-i-Martin's Extreme Bounds Analysis (EBA) for the fourth row of the table
eba$bounds$cdf.mu.generic[2]*100
eba$bounds$cdf.above.mu.generic[2]*100
eba$bounds$leamer.robust[2]

# Not in paper but mentioned:
# Repeating with lag = 1,2,3
# Since a lag is provided, the first part does not apply
se.neweywest1 <- function(model.object) {
  if (is.null(lag)) {
    lag <- trunc(0.75 * length(residuals(model.object))^(1/3))
  }
  model.fit <- NeweyWest(model.object, lag = 1)
  out <- sqrt(diag(model.fit))
  return(out)
}

se.neweywest2 <- function(model.object) {
  if (is.null(lag)) {
    lag <- trunc(0.75 * length(residuals(model.object))^(1/3))
  }
  model.fit <- NeweyWest(model.object, lag = 2)
  out <- sqrt(diag(model.fit))
  return(out)
}

se.neweywest3 <- function(model.object) {
  if (is.null(lag)) {
    lag <- trunc(0.75 * length(residuals(model.object))^(1/3))
  }
  model.fit <- NeweyWest(model.object, lag = 3)
  out <- sqrt(diag(model.fit))
  return(out)
}
eba1 <- eba(formula =interventionOrdinal_boj_numeric ~ polarity_l + interventionOrdinal_boj_l + 
              er_short_target + er_medium_target + er_long_target1 + 
              pre_election + post_election, 
            data = forex_int_boj_d, k = 0:6, mu = 0,
            vif = 7, exclusive = NULL, reg.fun = lm, 
            se.fun = se.neweywest1, include.fun = NULL, weights = "lri")
print(eba1)

eba2 <- eba(formula =interventionOrdinal_boj_numeric ~ polarity_l + interventionOrdinal_boj_l + 
              er_short_target + er_medium_target + er_long_target1 + 
              pre_election + post_election, 
            data = forex_int_boj_d, k = 0:6, mu = 0,
            vif = 7, exclusive = NULL, reg.fun = lm, 
            se.fun = se.neweywest2, include.fun = NULL, weights = "lri")
print(eba2)

eba3 <- eba(formula =interventionOrdinal_boj_numeric ~ polarity_l + interventionOrdinal_boj_l + 
              er_short_target + er_medium_target + er_long_target1 + 
              pre_election + post_election, 
            data = forex_int_boj_d, k = 0:6, mu = 0,
            vif = 7, exclusive = NULL, reg.fun = lm, 
            se.fun = se.neweywest3, include.fun = NULL, weights = "lri")
print(eba3)




################################################################################
################################# Table A10 #####################################
################################################################################
# Heteroskedastic ordered probit model
# Use forex_int_boj_d_no_NA
library("oglmx")
results.oprob<-suppressWarnings(oglmx(interventionOrdinal_boj ~ polarity_l + interventionOrdinal_boj_l + 
                                        er_short_target + er_medium_target + er_long_target1 + 
                                        pre_election + post_election, data=forex_int_boj_d, link="probit",
                                      constantMEAN = FALSE, constantSD = FALSE,
                                      delta=0,threshparam = NULL))

# Manually insert coefficients for polarity (after multiplying by 100 for interpretability)
margins.oglmx(results.oprob)
round(-0.00088704 * 100, 2); round(0.00030170 * 100, 2)
round(-0.0048500 * 100, 2); round(0.0014453 * 100, 2)
round(0.0057371 * 100, 2); round(0.0016789 * 100, 2)

################################################################################
################################# Table A11 #####################################
################################################################################

results.oprobhet<-suppressWarnings(oglmx(interventionOrdinal_boj 
                                         ~ polarity_l + interventionOrdinal_boj_l + 
                                           er_short_target + er_medium_target + er_long_target1 +pre_election + post_election, 
                                         ~ er_short_target + er_medium_target + er_long_target1 +pre_election + post_election, 
                                         data=forex_int_boj_d, constantMEAN = FALSE, constantSD = FALSE))

margins.oglmx(results.oprobhet)
round(-0.00128790 * 100, 2); round(0.00040133 * 100, 2)
round(-0.0047079 * 100, 2); round(0.0013529 * 100, 2)
round(0.0059958 * 100, 4); round(0.0016333 * 100, 2)


# Footnote 18 says:
# "Moreover, a likelihood ratio test comparing the homoskedastic probit 
# to the heteroskedastic probit shows that the
# latter leads to a significant improvement in model fit."
library("lmtest")
lrtest(results.oprob,results.oprobhet)
# we see that by modelling the heteroskedasticity we find a significant improvement in model fit.

# In the appendix we mention the unconditional probability of depreciation and appreication inducing interventions
# Depreciation
round(mean(forex_int_boj_d$interventionBinary_dep_boj)*100, 2)
# Appreciation
round(mean(forex_int_boj_d$interventionBinary_app_boj)*100, 2)




################################################################################
################### Table A12 #####################
################################################################################

# Make sure it is numeric for linear models 
class(forex_int_boj_d$intervAmount_boj)
class(forex_int_boj_d$intervAmount_boj_l)

# Divide by for interpretability
forex_int_boj_d <- dplyr::mutate(forex_int_boj_d, intervAmount_boj_s = intervAmount_boj/100)
forex_int_boj_d <- dplyr::mutate(forex_int_boj_d, intervAmount_boj_s_l = intervAmount_boj_l/100)

model1 = lm(intervAmount_boj_s ~ polarity_l + pre_election + post_election, data = forex_int_boj_d)
model2 = lm(intervAmount_boj_s ~ polarity_l + intervAmount_boj_s_l + pre_election + post_election,data = forex_int_boj_d)
model3 = lm(intervAmount_boj_s ~ polarity_l + intervAmount_boj_s_l + er_short_target + pre_election + post_election,data = forex_int_boj_d)
model4 = lm(intervAmount_boj_s ~ polarity_l + intervAmount_boj_s_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d)
model5 = lm(intervAmount_boj_s ~ polarity_l + intervAmount_boj_s_l + er_short_target + er_medium_target + er_j_1yL + pre_election + post_election,data = forex_int_boj_d)
model6 = lm(intervAmount_boj_s ~ polarity_l + intervAmount_boj_s_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d)
model7 = lm(intervAmount_boj_s ~ polarity_l + intervAmount_boj_s_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d)

# Calculate lags for newey west standard errors follwing Andrews' rule of thumb
lag_m1 <- 0.75*(nobs(model1))^(1/3)
lag_m2 <- 0.75*(nobs(model2))^(1/3)
lag_m3 <- 0.75*(nobs(model3))^(1/3)
lag_m4 <- 0.75*(nobs(model4))^(1/3)
lag_m5 <- 0.75*(nobs(model5))^(1/3)
lag_m6 <- 0.75*(nobs(model6))^(1/3)
lag_m7 <- 0.75*(nobs(model7))^(1/3)

# Estimate with Newey West Standard Errors
model1_nw <- coeftest(model1, vcov = NeweyWest(model1, lag = lag_m1, prewhite = TRUE))
model2_nw <- coeftest(model2, vcov = NeweyWest(model2, lag = lag_m2, prewhite = TRUE))
model3_nw <- coeftest(model3, vcov = NeweyWest(model3, lag = lag_m3, prewhite = TRUE))
model4_nw <- coeftest(model4, vcov = NeweyWest(model4, lag = lag_m4, prewhite = TRUE))
model5_nw <- coeftest(model5, vcov = NeweyWest(model5, lag = lag_m1, prewhite = TRUE))
model6_nw <- coeftest(model6, vcov = NeweyWest(model6, lag = lag_m6, prewhite = TRUE))
model7_nw <- coeftest(model7, vcov = NeweyWest(model7, lag = lag_m7, prewhite = TRUE))




# For some strange reason stargazaer does not accept a seventh model...
# I construct the table with the first six models
stargazer(model1_nw, model2_nw, model3_nw, model4_nw, model5_nw, model6_nw, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Forex Intervention Volume in 1 Mln Yen ((+) numbers mean purchases of the USD (sell Yen))",
          covariate.labels = c("Polarity (t-1)", "Intervention Amount (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A12_japan.tex")

stargazer(model1_nw, model2_nw, model3_nw, model4_nw, model5_nw, model6_nw, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Forex Intervention Volume in 1 Mln Yen ((+) numbers mean purchases of the USD (sell Yen))",
          covariate.labels = c("Polarity (t-1)", "Intervention Amount (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A12_japan.html")

# then add the sevent manually...
stargazer(model7_nw, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Forex Intervention Volume in 1 Mln Yen ((+) numbers mean purchases of the USD (sell Yen))",
          covariate.labels = c("Polarity (t-1)", "Intervention Amount (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A12b_japan.tex")

stargazer(model7_nw, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Forex Intervention Volume in 1 Mln Yen ((+) numbers mean purchases of the USD (sell Yen))",
          covariate.labels = c("Polarity (t-1)", "Intervention Amount (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A12b_japan.html")

# Add N and adj R squared manually
# Model 1
round(summary(model1)$adj.r.squared, 3)
nobs(model1)
# Model 2
round(summary(model2)$adj.r.squared, 3)
nobs(model2)
# Model 3
round(summary(model3)$adj.r.squared, 3)
nobs(model3)
# Model 4
round(summary(model4)$adj.r.squared, 3)
nobs(model4)
# Model 5
round(summary(model5)$adj.r.squared, 3)
nobs(model5)
# Model 6
round(summary(model6)$adj.r.squared, 3)
nobs(model6)
# Model 7
round(summary(model7)$adj.r.squared, 3)
nobs(model7)


################################################################################
###################  Table A13 #####################
################################################################################

# Step 1: Calculate the tercile breakpoints for negative and positive values
negative_values <- forex_int_boj_d$intervAmount_boj[forex_int_boj_d$intervAmount_boj < 0]
positive_values <- forex_int_boj_d$intervAmount_boj[forex_int_boj_d$intervAmount_boj > 0]

negative_terciles <- quantile(negative_values, probs = seq(0, 1, by = 1/3), na.rm = TRUE)
positive_terciles <- quantile(positive_values, probs = seq(0, 1, by = 1/3), na.rm = TRUE)

# Step 2: Map each value in the dataset to its corresponding tercile
forex_int_boj_d <- forex_int_boj_d %>%
  dplyr::mutate(
    intervAmount_boj_six_levels = case_when(
      # For negative values
      intervAmount_boj < 0 ~ as.numeric(as.character(cut(
        intervAmount_boj,
        breaks = negative_terciles,
        include.lowest = TRUE,
        labels = c(-3, -2, -1)
      ))),
      # For positive values
      intervAmount_boj > 0 ~ as.numeric(as.character(cut(
        intervAmount_boj,
        breaks = positive_terciles,
        include.lowest = TRUE,
        labels = c(1, 2, 3)
      ))),
      # For zero
      intervAmount_boj == 0 ~ 0
    )
  )


forex_int_boj_d <- dplyr::mutate(forex_int_boj_d, intervAmount_boj_six_levels_l = dplyr::lag(intervAmount_boj_six_levels, 1))

model1 = lm(intervAmount_boj_six_levels ~ polarity_l + pre_election + post_election, data = forex_int_boj_d)
model2 = lm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + pre_election + post_election,data = forex_int_boj_d)
model3 = lm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + pre_election + post_election,data = forex_int_boj_d)
model4 = lm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d)
model5 = lm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d)
model6 = lm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d)
model7 = lm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d)

# Calculate lags for newey west standard errors follwing Andrews' rule of thumb
lag_m1 <- 0.75*(nobs(model1))^(1/3)
lag_m2 <- 0.75*(nobs(model2))^(1/3)
lag_m3 <- 0.75*(nobs(model3))^(1/3)
lag_m4 <- 0.75*(nobs(model4))^(1/3)
lag_m5 <- 0.75*(nobs(model5))^(1/3)
lag_m6 <- 0.75*(nobs(model6))^(1/3)
lag_m7 <- 0.75*(nobs(model7))^(1/3)

# Estimate with Newey West Standard Errors
nw1 <- coeftest(model1, vcov = NeweyWest(model1, lag = lag_m1, prewhite = TRUE))
nw2 <- coeftest(model2, vcov = NeweyWest(model2, lag = lag_m2, prewhite = TRUE))
nw3 <- coeftest(model3, vcov = NeweyWest(model3, lag = lag_m3, prewhite = TRUE))
nw4 <- coeftest(model4, vcov = NeweyWest(model4, lag = lag_m4, prewhite = TRUE))
nw5 <- coeftest(model5, vcov = NeweyWest(model5, lag = lag_m1, prewhite = TRUE))
nw6 <- coeftest(model6, vcov = NeweyWest(model6, lag = lag_m6, prewhite = TRUE))
nw7 <- coeftest(model7, vcov = NeweyWest(model7, lag = lag_m7, prewhite = TRUE))



stargazer(nw1, nw2, nw3, nw4, nw5, nw6,nw7, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Seven Category Intensity Forex Intervention ((+) numbers mean purchases of the USD (sell Yen))",
          covariate.labels = c("Polarity (t-1)", "Intervention Seven Categories (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A13_japan.tex")


stargazer(nw1, nw2, nw3, nw4, nw5, nw6,nw7, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Seven Category Intensity Forex Intervention ((+) numbers mean purchases of the USD (sell Yen))",
          covariate.labels = c("Polarity (t-1)", "Intervention Seven Categories (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A13_japan.html")

# Add N and adj R squared manually
# Model 1
round(summary(model1)$adj.r.squared, 3)
nobs(model1)
# Model 2
round(summary(model2)$adj.r.squared, 3)
nobs(model2)
# Model 3
round(summary(model3)$adj.r.squared, 3)
nobs(model3)
# Model 4
round(summary(model4)$adj.r.squared, 3)
nobs(model4)
# Model 5
round(summary(model5)$adj.r.squared, 3)
nobs(model5)
# Model 6
round(summary(model6)$adj.r.squared, 3)
nobs(model6)
# Model 7
round(summary(model7)$adj.r.squared, 3)
nobs(model7)


################################################################################
###################  Table A14 #####################
################################################################################


forex_int_boj_d$intervAmount_boj_six_levels <- as.factor(forex_int_boj_d$intervAmount_boj_six_levels)

model1 = clm(intervAmount_boj_six_levels ~ polarity_l + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model2 = clm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model3 = clm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model4 = clm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model5 = clm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model6 = clm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d, link = "probit")
model7 = clm(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d, link = "probit")


stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Seven Category Intensity Forex Intervention ((+) numbers mean purchases of the USD (sell Yen))",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A14_japan.tex")

stargazer(model1, model2, model3, model4, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Seven Category Intensity Forex Intervention ((+) numbers mean purchases of the USD (sell Yen))",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Medium Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A14_japan.html")


# Adding other goodness of fit measures specific to Ordered Categorical models
# Need to fit the model with polr rather than clm
model1 = suppressWarnings(polr(intervAmount_boj_six_levels ~ polarity_l + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
model2 = suppressWarnings(polr(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model3 = suppressWarnings(polr(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model4 = suppressWarnings(polr(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model5 = suppressWarnings(polr(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_d, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(intervAmount_boj_six_levels ~ polarity_l + intervAmount_boj_six_levels_l + er_short_target + er_medium_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_d,Hess = TRUE, method = "probit"))
# The following are manually inserted in the last row of the Tavle
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3, which = "McFadden"), 3)
round(DescTools::PseudoR2(model4, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)


################################################################################
###################  Table A15 ###################################################
################################################################################

# Load monthly dataset
forex_int_boj_monthly <- readRDS("japan_dataset_m.rds")


forex_int_boj_monthly$interventionOrdinal <- as.factor(forex_int_boj_monthly$interventionOrdinal)
model1 = clm(interventionOrdinal ~ polarity_l + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model2 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model3 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model5 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model6 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model7 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")

stargazer(model1, model2, model3, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A15_japan.tex")

stargazer(model1, model2, model3, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A15_japan.html")

# Refit with polr
model1 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model2 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model3 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + er_short_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
# Third calculate the new measures and add the manually to the table
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)


################################################################################
###################  Table A16 ###################################################
################################################################################


forex_int_boj_monthly <- dplyr::mutate(forex_int_boj_monthly, curr_emp_l = dplyr::lag(curr.emp, 1))

model1 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + pre_election + post_election, data = forex_int_boj_monthly, link = "probit")
model2 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model3 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l +er_short_target+ pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model5 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l +er_short_target+ er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model6 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l +er_short_target+ er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model7 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l +er_short_target+ er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")

stargazer(model1, model2, model3, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "EMP (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A16_japan.tex")

stargazer(model1, model2, model3, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "EMP (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A16_japan.html")

# Refit with polr
model1 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + curr_emp_l + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model2 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model3 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + er_short_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + er_short_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + er_short_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
# Add the manually to the table
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)


################################################################################
###################  Table A17 ###################################################
################################################################################

model1 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + pre_election + post_election, data = forex_int_boj_monthly, link = "probit")
model2 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model5 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l+ er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model6 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l+ er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model7 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + interventionOrdinal_boj_l+ er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")

stargazer(model1, model2, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "EMP (t-1)", "Intervention (t-1)", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A17_japan.tex")

stargazer(model1, model2, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "EMP (t-1)", "Intervention (t-1)", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A17_japan.html")

model1 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + curr_emp_l + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model2 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + interventionOrdinal_boj_l + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
# Add the manually to the table
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)



################################################################################
###################  Table A18 ###################################################
################################################################################

model1 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + pre_election + post_election, data = forex_int_boj_monthly, link = "probit")
model5 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model6 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model7 = clm(interventionOrdinal ~ polarity_l + curr_emp_l + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")

stargazer(model1, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local / 0 / +1 buy foreign)",
          covariate.labels = c("Polarity (t-1)", "EMP (t-1)", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A18_japan.tex")

stargazer(model1, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local / 0 / +1 buy foreign)",
          covariate.labels = c("Polarity (t-1)", "EMP (t-1)", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A18_japan.html")

model1 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + curr_emp_l + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ curr_emp_l + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
# Add the manually to the table
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)

################################################################################
###################  Table A19 ###################################################
################################################################################

model1 = clm(interventionOrdinal ~ polarity_l + emp_lo + pre_election + post_election, data = forex_int_boj_monthly, link = "probit")
model5 = clm(interventionOrdinal ~ polarity_l + emp_lo + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model6 = clm(interventionOrdinal ~ polarity_l + emp_lo + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model7 = clm(interventionOrdinal ~ polarity_l + emp_lo + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")

stargazer(model1, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local / 0 / +1 buy foreign)",
          covariate.labels = c("Polarity (t-1)", "EMP (low) (t-1)", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A19_japan.tex")

stargazer(model1, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local / 0 / +1 buy foreign)",
          covariate.labels = c("Polarity (t-1)", "EMP (low) (t-1)", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A19_japan.html")

model1 = suppressWarnings(polr(interventionOrdinal ~ polarity_l +emp_lo + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ emp_lo + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ emp_lo + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "cloglog"))
model7 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ emp_lo + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
# Add the manually to the table
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)


################################################################################
###################  Table A20 ###################################################
################################################################################

model1 = clm(interventionOrdinal ~ polarity_l + emp_hi + pre_election + post_election, data = forex_int_boj_monthly, link = "probit")
model5 = clm(interventionOrdinal ~ polarity_l + emp_hi + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model6 = clm(interventionOrdinal ~ polarity_l + emp_hi + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model7 = clm(interventionOrdinal ~ polarity_l + emp_hi + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")

stargazer(model1, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local / 0 / +1 buy foreign)",
          covariate.labels = c("Polarity (t-1)", "EMP (high) (t-1)", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A20_japan.tex")

stargazer(model1, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local / 0 / +1 buy foreign)",
          covariate.labels = c("Polarity (t-1)", "EMP (high) (t-1)", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A20_japan.html")

model1 = suppressWarnings(polr(interventionOrdinal ~ polarity_l +emp_hi + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ emp_hi + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, Hess = TRUE,method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ emp_hi + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal ~ polarity_l+ emp_hi + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
# Add the manually to the table
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)


################################################################################
###################  Table A21 ###################################################
################################################################################

model1 = clm(interventionOrdinal ~ polarity_l + reer_51_l + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model2 = clm(interventionOrdinal ~ polarity_l + reer_51_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model3 = clm(interventionOrdinal ~ polarity_l + reer_51_l + interventionOrdinal_boj_l + er_short_target + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model5 = clm(interventionOrdinal ~ polarity_l + reer_51_l + interventionOrdinal_boj_l + er_short_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model6 = clm(interventionOrdinal ~ polarity_l + reer_51_l + interventionOrdinal_boj_l + er_short_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model7 = clm(interventionOrdinal ~ polarity_l + reer_51_l + interventionOrdinal_boj_l + er_short_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")

stargazer(model1, model2, model3, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "REER (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A21_japan.tex")


stargazer(model1, model2, model3, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "REER (t-1)", "Intervention (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A21_japan.html")


model1 = suppressWarnings(polr(interventionOrdinal ~ polarity_l +reer_51_l + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model2 = suppressWarnings(polr(interventionOrdinal ~ polarity_l +reer_51_l + interventionOrdinal_boj_l+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model3 = suppressWarnings(polr(interventionOrdinal ~ polarity_l +reer_51_l + interventionOrdinal_boj_l+ er_short_target+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal ~ polarity_l +reer_51_l + interventionOrdinal_boj_l+ er_short_target+ er_long_target1+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal ~ polarity_l +reer_51_l + interventionOrdinal_boj_l+ er_short_target+ er_long_target3+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal ~ polarity_l +reer_51_l + interventionOrdinal_boj_l+ er_short_target+ er_long_target5+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
# Add the manually to the table
round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)


################################################################################
###################  Table A22 ###################################################
################################################################################


model1 = clm(interventionOrdinal ~ polarity_l + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model2 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model3 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + reer_51_short_target + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model5 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + reer_51_short_target + reer_51_long_target1 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model6 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + reer_51_short_target + reer_51_long_target3 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")
model7 = clm(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l + reer_51_short_target + reer_51_long_target5 + pre_election + post_election,data = forex_int_boj_monthly, link = "probit")

stargazer(model1, model2, model3, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "REER Short Target", "REER Long Target (1 year mov.av.)", "REER Long Target (3 year mov.av.)", "REER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A22_japan.tex")

stargazer(model1, model2, model3, model5,model6,model7,
          type = "text", 
          apply.coef = exp,t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Coefficients exponentiated. Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="", 
          dep.var.labels = "Forex Intervention (-1 buy local, sell foreign / 0 / +1 buy foreign, sell local)",
          covariate.labels = c("Polarity (t-1)", "Intervention (t-1)", "REER Short Target", "REER Long Target (1 year mov.av.)", "REER Long Target (3 year mov.av.)", "REER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3, 
          keep.stat = c("n", "ll"),
          out="Table_A22_japan.html")



model1 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model2 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model3 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l+ reer_51_short_target+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model5 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l+ reer_51_short_target+ reer_51_long_target1+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model6 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l+ reer_51_short_target+ reer_51_long_target3+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))
model7 = suppressWarnings(polr(interventionOrdinal ~ polarity_l + interventionOrdinal_boj_l+ reer_51_short_target+ reer_51_long_target5+ pre_election + post_election,data = forex_int_boj_monthly,Hess = TRUE, method = "probit"))

round(DescTools::PseudoR2(model1, which = "McFadden"), 3)
round(DescTools::PseudoR2(model2, which = "McFadden"), 3)
round(DescTools::PseudoR2(model3, which = "McFadden"), 3)
round(DescTools::PseudoR2(model5, which = "McFadden"), 3)
round(DescTools::PseudoR2(model6, which = "McFadden"), 3)
round(DescTools::PseudoR2(model7, which = "McFadden"), 3)




################################################################################
###################  Table A23 ###################################################
################################################################################

model1 = lm(ir_j ~ polarity_l + pre_election + post_election, data = forex_int_boj_monthly)
model2 = lm(ir_j ~ polarity_l + ir_j_l + pre_election + post_election,data = forex_int_boj_monthly)
model3 = lm(ir_j ~ polarity_l + ir_j_l +er_short_target + pre_election + post_election,data = forex_int_boj_monthly)
model5 = lm(ir_j ~ polarity_l + ir_j_l +er_short_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly)
model6 = lm(ir_j ~ polarity_l + ir_j_l +er_short_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly)
model7 = lm(ir_j ~ polarity_l + ir_j_l +er_short_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly)

# Calculate lags for newey west standard errors follwing Andrews' rule of thumb
lag_m1 <- 0.75*(nobs(model1))^(1/3)
lag_m2 <- 0.75*(nobs(model2))^(1/3)
lag_m3 <- 0.75*(nobs(model3))^(1/3)
lag_m5 <- 0.75*(nobs(model5))^(1/3)
lag_m6 <- 0.75*(nobs(model6))^(1/3)
lag_m7 <- 0.75*(nobs(model7))^(1/3)

# Estimate with Newey West Standard Errors
nw_1 <- coeftest(model1, vcov = NeweyWest(model1, lag = lag_m1, prewhite = TRUE))
nw_2 <- coeftest(model2, vcov = NeweyWest(model2, lag = lag_m2, prewhite = TRUE))
nw_3 <- coeftest(model3, vcov = NeweyWest(model3, lag = lag_m3, prewhite = TRUE))
nw_5 <- coeftest(model5, vcov = NeweyWest(model5, lag = lag_m5, prewhite = TRUE))
nw_6 <- coeftest(model6, vcov = NeweyWest(model6, lag = lag_m6, prewhite = TRUE))
nw_7 <- coeftest(model7, vcov = NeweyWest(model7, lag = lag_m7, prewhite = TRUE))

# I construct the table with the first six models
stargazer(nw_1, nw_2, nw_3, nw_5, nw_6,nw_7, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Discount Interest Rate",
          covariate.labels = c("Polarity (t-1)", "Discount rate (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)","ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A23_japan.tex")

stargazer(nw_1, nw_2, nw_3, nw_5, nw_6,nw_7, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Discount Interest Rate",
          covariate.labels = c("Polarity (t-1)", "Discount rate (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)","ER Long Target (5 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A23_japan.html")

# Add N and adj R squared manually
# Model 1
round(summary(model1)$r.squared, 3)
nobs(model1)
# Model 2
round(summary(model2)$r.squared, 3)
nobs(model2)
# Model 3
round(summary(model3)$r.squared, 3)
nobs(model3)

# Model 5
round(summary(model5)$r.squared, 3)
nobs(model5)
# Model 6
round(summary(model6)$r.squared, 3)
nobs(model6)
# Model 7
round(summary(model7)$r.squared, 3)
nobs(model7)

# The descrptive statistics mentioned in the Appendix when describing Table 23
summary(forex_int_boj_monthly$ir_j)
sd(forex_int_boj_monthly$ir_j)


################################################################################
###################  Table A24 ###################################################
################################################################################

# Identify where X changes between consecutive observations
changes <- c(0, diff(forex_int_boj_monthly$ir_j)) != 0  # Prepend 0 to match length of X

# Now Dependent variable in first difference

forex_int_boj_monthly$diff_variable_ir <- c(NA, diff(forex_int_boj_monthly$ir_j))  # Add NA to align with original data
forex_int_boj_monthly <- dplyr::mutate(forex_int_boj_monthly, diff_variable_ir_l = dplyr::lag(diff_variable_ir, 1))

# The descrptive statistics mentioned in the Appendix when describing Table 24
summary(forex_int_boj_monthly$diff_variable_ir)
sd(forex_int_boj_monthly$diff_variable_ir, na.rm=T)

model1 = lm(diff_variable_ir ~ polarity_l + pre_election + post_election, data = forex_int_boj_monthly)
model2 = lm(diff_variable_ir ~ polarity_l + pre_election + post_election,data = forex_int_boj_monthly)
model3 = lm(diff_variable_ir ~ polarity_l +er_short_target + pre_election + post_election,data = forex_int_boj_monthly)
model5 = lm(diff_variable_ir ~ polarity_l +er_short_target + er_long_target1 + pre_election + post_election,data = forex_int_boj_monthly)
model6 = lm(diff_variable_ir ~ polarity_l +er_short_target + er_long_target3 + pre_election + post_election,data = forex_int_boj_monthly)
model7 = lm(diff_variable_ir ~ polarity_l +er_short_target + er_long_target5 + pre_election + post_election,data = forex_int_boj_monthly)

# Calculate lags for newey west standard errors follwing Andrews' rule of thumb
lag_m1 <- 0.75*(nobs(model1))^(1/3)
lag_m2 <- 0.75*(nobs(model2))^(1/3)
lag_m3 <- 0.75*(nobs(model3))^(1/3)
lag_m5 <- 0.75*(nobs(model5))^(1/3)
lag_m6 <- 0.75*(nobs(model6))^(1/3)
lag_m7 <- 0.75*(nobs(model7))^(1/3)

# Estimate with Newey West Standard Errors
nw_1 <- coeftest(model1, vcov = NeweyWest(model1, lag = lag_m1, prewhite = TRUE))
nw_2 <- coeftest(model2, vcov = NeweyWest(model2, lag = lag_m2, prewhite = TRUE))
nw_3 <- coeftest(model3, vcov = NeweyWest(model3, lag = lag_m3, prewhite = TRUE))
nw_5 <- coeftest(model5, vcov = NeweyWest(model5, lag = lag_m5, prewhite = TRUE))
nw_6 <- coeftest(model6, vcov = NeweyWest(model6, lag = lag_m6, prewhite = TRUE))
nw_7 <- coeftest(model7, vcov = NeweyWest(model7, lag = lag_m7, prewhite = TRUE))

stargazer(nw_1, nw_2, nw_3, nw_5, nw_6,nw_7, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Change in the Discount Interest Rate",
          covariate.labels = c("Polarity (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A24_japan.tex")

stargazer(nw_1, nw_2, nw_3, nw_5, nw_6,nw_7, type = "text",
          t.auto=F, p.auto=F, 
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Newey West Standard Errors in Parenthesis *p<0.05; **p<0.01; ***p<0.001",notes.append	=F,notes.align="l",
          title="",
          dep.var.labels = "Change in the Discount Interest Rate",
          covariate.labels = c("Polarity (t-1)", "ER Short Target", "ER Long Target (1 year mov.av.)", "ER Long Target (3 year mov.av.)", "Pre-election (Binary)", "Post-election (Binary)"),
          digits=3,
          out="Table_A24_japan.html")

# Add N and adj R squared manually
# Model 1
round(summary(model1)$r.squared, 3)
nobs(model1)
# Model 2
round(summary(model2)$r.squared, 3)
nobs(model2)
# Model 3
round(summary(model3)$r.squared, 3)
nobs(model3)
# Model 5
round(summary(model5)$r.squared, 3)
nobs(model5)
# Model 6
round(summary(model6)$r.squared, 3)
nobs(model6)
# Model 7
round(summary(model7)$r.squared, 3)
nobs(model7)


################################################################################
###################  Fig A6 (Panel a) ###################################################
################################################################################


# Load weekly dataset
japan_weekly <- readRDS("japan_dataset_w.rds")

library(lpirfs)
library(vars)
library(tseries)
library(urca)
library(forecast)
# The nominal ER is non stationary. Hence, turn into a growth variable
# We can do it directly from the raw ER (not logged) or approximate with the first difference of the logged exchange.
japan_weekly$diff_variable_er <- c(NA, diff(japan_weekly$er_j))  # Add NA to align with original data
japan_weekly$growth_er <- c(NA, diff(japan_weekly$er_j_not_logged)) / lag(japan_weekly$er_j_not_logged, 1)
#Only the six variable we need
data_w <- dplyr::select(japan_weekly, interventionOrdinal, growth_er, diff_variable_er, polarity, er_long_target1, pre_election, post_election)
# For interpretability
data_w <- dplyr::mutate(data_w, diff_variable_er = -100*diff_variable_er, growth_er = -100*growth_er)
# Have to delete observations with missing values
data_w <- dplyr::filter(data_w, !is.na(diff_variable_er))
# Three endogenous regressors
endog_data <- dplyr::select(data_w, interventionOrdinal, diff_variable_er, polarity)
# Three exogenous ones. Keep only ling target as it is the most likely to be exogenous
exog_data <- dplyr::select(data_w, er_long_target1, pre_election, post_election)

# Check that endogenous regressors are stationary
suppressWarnings(adf.test(endog_data$interventionOrdinal))
summary(ur.df(endog_data$interventionOrdinal,  type = "none"))
summary(ur.df(endog_data$interventionOrdinal,  type = "drift"))
summary(ur.df(endog_data$interventionOrdinal,  type = "trend"))
suppressWarnings(adf.test(endog_data$diff_variable_er))
summary(ur.df(endog_data$diff_variable_er,  type = "none"))
summary(ur.df(endog_data$diff_variable_er,  type = "drift"))
summary(ur.df(endog_data$diff_variable_er,  type = "trend"))
suppressWarnings(adf.test(endog_data$polarity))
summary(ur.df(endog_data$polarity,  type = "none"))
summary(ur.df(endog_data$polarity,  type = "drift"))
summary(ur.df(endog_data$polarity,  type = "trend"))

# Check for optimal lag length
VARselect(endog_data, type= "const", lag.max = 50) #                 5      5      3      5 
VARselect(endog_data, type= "both", lag.max = 50) #                5      5      3      5 

# Constant and trend
bv.est <- vars::VAR(endog_data, p = 5, type = "both", season = NULL, exog = exog_data)
# Serial correlation in the resiudals test
serial.test(bv.est,lags.bg = 1, type = "BG")
serial.test(bv.est,lags.bg = 2, type = "BG")
serial.test(bv.est,lags.bg = 5, type = "BG")
bv.est.resid <- residuals(bv.est)
Box.test(bv.est.resid[,1],lag = 1,type = "Ljung-Box")
Box.test(bv.est.resid[,1],lag = 2,type = "Ljung-Box")
Box.test(bv.est.resid[,1],lag = 5,type = "Ljung-Box")


# Make nicer graph on ggplot
# Calculate impulse response function (IRF)
irf.gdp <- irf(bv.est, impulse = "polarity", response = "diff_variable_er",  
               n.ahead = 9, boot = TRUE, seed = 87599, ci = 0.68)
# Extracting the necessary data
irf_data <- data.frame(
  Time = 0:9,
  Response = irf.gdp$irf$polarity[, "diff_variable_er"],
  Lower_CI = irf.gdp$Lower$polarity[, "diff_variable_er"],
  Upper_CI = irf.gdp$Upper$polarity[, "diff_variable_er"]
)
# Plot with ggplot
japan_var <- ggplot(irf_data, aes(x = Time)) +
  geom_line(aes(y = Response), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "lightblue", alpha = 0.3) +
  # Horizontal dashed line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "", x = "Periods (Weeks)", y = "Response") +
  # Set discrete tick marks at 0, 1, 2, ..., 9
  scale_x_continuous(breaks = 0:9) +
  theme_minimal() +
  theme(legend.position = "none")
japan_var

getwd()
ggsave("Fig_A6_Jap.pdf", plot = japan_var, width = 7, height = 5, units = "in")


# As mentioned in the text, results with constant but no trend
# Lag = 5 as suggested by info criteria
# Constant
bv.est <- vars::VAR(endog_data, p = 5, type = "const", season = NULL, exog = exog_data)
serial.test(bv.est,lags.bg = 1, type = "BG")
serial.test(bv.est,lags.bg = 2, type = "BG")
serial.test(bv.est,lags.bg = 5, type = "BG")
bv.est.resid <- residuals(bv.est)
Box.test(bv.est.resid[,1],lag = 1,type = "Ljung-Box")
Box.test(bv.est.resid[,1],lag = 2,type = "Ljung-Box")
Box.test(bv.est.resid[,1],lag = 5,type = "Ljung-Box")
irf.gdp <- irf(bv.est, impulse = "polarity", response = "diff_variable_er",  n.ahead = 9, boot = TRUE, seed = 98654, ci = 0.68)
plot(irf.gdp, ylab = "", main = "")


# As mentioned in the text, results with growth ER (rather than the log approximation)
data_w <- dplyr::select(japan_weekly, interventionOrdinal, growth_er, diff_variable_er, polarity, er_long_target1, pre_election, post_election)
data_w <- dplyr::mutate(data_w, diff_variable_er = -100*diff_variable_er, growth_er = -100*growth_er)
data_w <- dplyr::filter(data_w, !is.na(diff_variable_er))
endog_data <- dplyr::select(data_w, interventionOrdinal, growth_er, polarity)
exog_data <- dplyr::select(data_w, er_long_target1, pre_election, post_election)
VARselect(endog_data, type= "const", lag.max = 50) #                 5      5      3      5 
VARselect(endog_data, type= "both", lag.max = 50) #                5      5      3      5 
# Constant and trend
bv.est <- vars::VAR(endog_data, p = 5, type = "both", season = NULL, exog = exog_data)
irf.gdp <- irf(bv.est, impulse = "polarity", response = "growth_er",  n.ahead = 10, boot = TRUE, seed = 87599, ci = 0.68)
plot(irf.gdp, ylab = "", main = "")
# Constant
bv.est <- vars::VAR(endog_data, p = 5, type = "const", season = NULL, exog = exog_data)
irf.gdp <- irf(bv.est, impulse = "polarity", response = "growth_er",  n.ahead = 10, boot = TRUE, seed = 98654, ci = 0.68)
plot(irf.gdp, ylab = "", main = "")


# As mentioned in the text, results with the other long targets
### Long target 3 years
data_w <- dplyr::select(japan_weekly, interventionOrdinal, growth_er, diff_variable_er, polarity, er_long_target3, pre_election, post_election)
data_w <- dplyr::mutate(data_w, diff_variable_er = -100*diff_variable_er, growth_er = -100*growth_er)
data_w <- dplyr::filter(data_w, !is.na(diff_variable_er))
endog_data <- dplyr::select(data_w, interventionOrdinal, diff_variable_er, polarity)
exog_data <- dplyr::select(data_w, er_long_target3, pre_election, post_election)
VARselect(endog_data, type= "const", lag.max = 50) #                 5      5      4      5 
VARselect(endog_data, type= "both", lag.max = 50) #                5      5      4      5 
# Constant and trend
bv.est <- vars::VAR(endog_data, p = 5, type = "both", season = NULL, exog = exog_data)
irf.gdp <- irf(bv.est, impulse = "polarity", response = "diff_variable_er",  n.ahead = 10, boot = TRUE, seed = 87599, ci = 0.68)
plot(irf.gdp, ylab = "", main = "")
# Constant
bv.est <- vars::VAR(endog_data, p = 5, type = "const", season = NULL, exog = exog_data)
irf.gdp <- irf(bv.est, impulse = "polarity", response = "diff_variable_er",  n.ahead = 10, boot = TRUE, seed = 98654, ci = 0.68)
plot(irf.gdp, ylab = "", main = "")

### Long target 5 years
data_w <- dplyr::select(japan_weekly, interventionOrdinal, growth_er, diff_variable_er, polarity, er_long_target5, pre_election, post_election)
data_w <- dplyr::mutate(data_w, diff_variable_er = -100*diff_variable_er, growth_er = -100*growth_er)
data_w <- dplyr::filter(data_w, !is.na(diff_variable_er))
endog_data <- dplyr::select(data_w, interventionOrdinal, diff_variable_er, polarity)
exog_data <- dplyr::select(data_w, er_long_target5, pre_election, post_election)
VARselect(endog_data, type= "const", lag.max = 50) #                 5      5      4      5 
VARselect(endog_data, type= "both", lag.max = 50) #                5      5      4      5 
# Constant and trend
bv.est <- vars::VAR(endog_data, p = 5, type = "both", season = NULL, exog = exog_data)
irf.gdp <- irf(bv.est, impulse = "polarity", response = "diff_variable_er",  n.ahead = 10, boot = TRUE, seed = 87599, ci = 0.68)
plot(irf.gdp, ylab = "", main = "")
# Constant
bv.est <- vars::VAR(endog_data, p = 5, type = "const", season = NULL, exog = exog_data)
irf.gdp <- irf(bv.est, impulse = "polarity", response = "diff_variable_er",  n.ahead = 10, boot = TRUE, seed = 98654, ci = 0.68)
plot(irf.gdp, ylab = "", main = "")




################################################################################
###################  Fig A6 (Panel b) ###################################################
################################################################################

# Load weekly dataset
korea_df_weekly <- readRDS("korea_dataset_w.rds")

library(lpirfs)
library(vars)
library(sovereign)
library(quantmod)
library(lubridate)
library(forecast)
library(tseries)
library(urca)

# Create stationary ER
korea_df_weekly$diff_variable_er <- c(NA, diff(korea_df_weekly$er_k))  # Add NA to align with original data
# Create date variable
korea_df_weekly$date <- as.Date(paste(korea_df_weekly$year, korea_df_weekly$week, 1, sep = "-"), format = "%Y-%U-%u")
korea_df_weekly <- dplyr::filter(korea_df_weekly, !is.na(date))
korea_df_weekly <- dplyr::filter(korea_df_weekly, !is.nan(er_long_target1))
korea_df_weekly <- dplyr::select(korea_df_weekly, year, week,date, diff_variable_er, polarity, er_medium_target, er_long_target1, pre_election, post_election)
korea_df_weekly <- dplyr::mutate(korea_df_weekly, diff_variable_er = diff_variable_er*(-100))
korea_df_weekly <- dplyr::filter(korea_df_weekly, !is.na(diff_variable_er))

# Two endogenous regressors
endog_data <- dplyr::select(korea_df_weekly, diff_variable_er, polarity)

# make sure they are stationary
suppressWarnings(adf.test(endog_data$diff_variable_er))
suppressWarnings(adf.test(endog_data$polarity))
summary(ur.df(endog_data$diff_variable_er,  type = "none"))
summary(ur.df(endog_data$diff_variable_er,  type = "drift"))
summary(ur.df(endog_data$diff_variable_er,  type = "trend"))
summary(ur.df(endog_data$polarity,  type = "none"))
summary(ur.df(endog_data$polarity,  type = "drift"))
summary(ur.df(endog_data$polarity,  type = "trend"))


# Three exogenous ones. Keep only ling target as it is the most likely to be exogenous
exog_data <- dplyr::select(korea_df_weekly, er_long_target1, pre_election, post_election)
# Confirming from before
VARselect(endog_data, type= "const", lag.max = 50) #                           23      6      4     23 
VARselect(endog_data, type= "both", lag.max = 50) #                     23      4      4     23


# 23 lags
bv.est <- vars::VAR(endog_data, p = 23, type = "both", season = NULL, exog = exog_data)
# Check for AR in residuals
serial.test(bv.est,lags.bg = 10, type = "BG")
serial.test(bv.est,lags.bg = 15, type = "BG")
serial.test(bv.est,lags.bg = 23, type = "BG")
bv.est.resid <- residuals(bv.est)
Box.test(bv.est.resid[,1],lag = 10,type = "Ljung-Box")
Box.test(bv.est.resid[,1],lag = 15,type = "Ljung-Box")
Box.test(bv.est.resid[,1],lag = 23,type = "Ljung-Box")
# graph
irf.gdp <- irf(bv.est, impulse = "polarity", response = "diff_variable_er",  n.ahead = 9, boot = TRUE, seed = 09871, ci = 0.68)
plot(irf.gdp, ylab = "", main = "Polarity -> ER")


# Do it nicer on ggplot
# Extracting the necessary data
irf_data <- data.frame(
  Time = 0:9,
  Response = irf.gdp$irf$polarity[, "diff_variable_er"],
  Lower_CI = irf.gdp$Lower$polarity[, "diff_variable_er"],
  Upper_CI = irf.gdp$Upper$polarity[, "diff_variable_er"]
)
# Plot with ggplot
korea_var <- ggplot(irf_data, aes(x = Time)) +
  geom_line(aes(y = Response), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "lightblue", alpha = 0.3) +
  # Horizontal dashed line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "", x = "Periods (Weeks)", y = "Response") +
  # Set discrete tick marks at 0, 1, 2, ..., 9
  scale_x_continuous(breaks = 0:9) +
  theme_minimal() +
  theme(legend.position = "none")
korea_var

ggsave("Fig_A6_Kor.pdf", plot = korea_var, width = 7, height = 5, units = "in")



# As mentioned in the text, results with constant but no trend
bv.est <- vars::VAR(endog_data, p = 23, type = "const", season = NULL, exog = exog_data)

serial.test(bv.est,lags.bg = 10, type = "BG")
serial.test(bv.est,lags.bg = 15, type = "BG")
serial.test(bv.est,lags.bg = 23, type = "BG")
bv.est.resid <- residuals(bv.est)
Box.test(bv.est.resid[,1],lag = 10,type = "Ljung-Box")
Box.test(bv.est.resid[,1],lag = 15,type = "Ljung-Box")
Box.test(bv.est.resid[,1],lag = 23,type = "Ljung-Box")

irf.gdp <- irf(bv.est, impulse = "polarity", response = "diff_variable_er",  n.ahead = 9, boot = TRUE, seed = 64789, ci = 0.68)
plot(irf.gdp, ylab = "", main = "Polarity -> ER")
















