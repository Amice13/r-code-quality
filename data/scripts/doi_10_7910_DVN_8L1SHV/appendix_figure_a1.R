## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
library(data.table)
library(ggplot2)
library(dplyr)

load("full_trial_samples.rdata")
full_trial_samples <- full_trial_samples[eduname_cd != 999, ]
full_trial_samples$eduname_cd <- as.factor(full_trial_samples$eduname_cd)
full_trial_samples$religion_cd <- as.factor(full_trial_samples$religion_cd)

setDT(full_trial_samples)
ggdat <- full_trial_samples[, sum(promotion == TRUE, na.rm = TRUE) / .N,
  by = c("gender_cd", "years")]
ggdat[,  `:=`(low = V1 - 2 * 0.001, high = V1 + 2 * 0.001)]
ggdat[, gender_cd := ifelse(gender_cd == 1, "Female", "Male")]
setnames(ggdat, c("gender", "year", "Pr", "low", "high"))

png("~/Desktop/gender_parallel.png", res = 300, width = 40, height = 40, units = "cm")
ggplot(ggdat[year > 1990 & year < 2015], aes(x = year, y = Pr, group = gender)) +
    geom_line(aes(linetype = gender)) +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.3) +
    theme_minimal() +
    geom_vline(xintercept = 1999) +
    theme(text = element_text(size = 50)) +
    ylab("Prob. Promotion")
dev.off()


