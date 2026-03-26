# 01_Load_Survey_Experiment.R
# Purpose: The Role of Public Broadcasting in Media Bias
# Created: 2019-9-6 Taka-aki Asano
# Last Modified: 2021-10-14

# package
require("readr")
require("dplyr")
require("tidyr")
require("ggplot2")
theme_set(theme_classic(base_size = 12))


# define function
confidence <- function(x, level = 0.95, upper = TRUE){
  x <- na.omit(x)
  n <- length(x)
  mean <- mean(x, na.rm = TRUE)
  SE <- sd(x, na.rm = TRUE) / sqrt(n)
  Q <- qnorm((1 - level) / 2, lower.tail = FALSE)
  if (upper == TRUE) {
    bound <- mean + Q*SE
  } else {
    bound <- mean - Q*SE
  }
  return(bound)
}

se <- function(x){
  x <- na.omit(x)
  n <- length(x)
  se <- sd(x, na.rm = TRUE) / sqrt(n)
  return(se)
}


# load dataset
exp <- read_csv(
  "Survey_Experiment_20191216.csv", 
  locale = locale(encoding = "UTF8")
)


# omit satisficer
exp <- exp[exp$Q12_4 == 4,]


# perceived ideological positions
ideology <- data.frame(
  Actor = c("The Average Respondent", "NHK", "NTV", "EX"), 
  Mean = rep(NA, 4), 
  SE = rep(NA, 4), 
  SD = rep(NA, 4), 
  Lower = rep(NA, 4), 
  Upper = rep(NA, 4)
)
ideology$Actor <- factor(
  ideology$Actor, 
  levels = c("The Average Respondent", "NTV", "NHK", "EX")
)
## Respondent
ideology$Mean[1] <- mean(exp$Q5_1, na.rm = TRUE)
ideology$SE[1] <- se(exp$Q5_1)
ideology$SD[1] <- sd(exp$Q5_1, na.rm = TRUE)
ideology$Lower[1] <- confidence(exp$Q5_1, upper = FALSE)
ideology$Upper[1] <- confidence(exp$Q5_1, upper = TRUE)
## NHK
ideology$Mean[2] <- mean(exp$Q5_7, na.rm = TRUE)
ideology$SE[2] <- se(exp$Q5_7)
ideology$SD[2] <- sd(exp$Q5_7, na.rm = TRUE)
ideology$Lower[2] <- confidence(exp$Q5_7, upper = FALSE)
ideology$Upper[2] <- confidence(exp$Q5_7, upper = TRUE)
## NTV
ideology$Mean[3] <- mean(exp$Q5_8, na.rm = TRUE)
ideology$SE[3] <- se(exp$Q5_8)
ideology$SD[3] <- sd(exp$Q5_8, na.rm = TRUE)
ideology$Lower[3] <- confidence(exp$Q5_8, upper = FALSE)
ideology$Upper[3] <- confidence(exp$Q5_8, upper = TRUE)
## TV Asahi
ideology$Mean[4] <- mean(exp$Q5_9, na.rm = TRUE)
ideology$SE[4] <- se(exp$Q5_9)
ideology$SD[4] <- sd(exp$Q5_9, na.rm = TRUE)
ideology$Lower[4] <- confidence(exp$Q5_9, upper = FALSE)
ideology$Upper[4] <- confidence(exp$Q5_9, upper = TRUE)
## Figure 1
ideology_plot <- ggplot(
  ideology, aes(x = Actor, y = Mean, ymin = Lower, ymax = Upper)) + 
  geom_pointrange() + 
  geom_text(aes(label = paste0(round(Mean, 2), "\n(", round(SD, 2), ")")), 
            size = 4, vjust = -0.2) + 
  ylim(3, 7) + coord_flip() + 
  labs(x = "", y = expression(Left  %<-% "Ideology" %->%  Right)) + 
  theme(axis.text.x = element_text(size = 10))
plot(ideology_plot)
