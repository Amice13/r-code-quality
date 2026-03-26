# Title: The Shadow of the Future & Bargaining Delay: An Experimental Approach
# Author: Lindsay Hundley
# Date Modified: 12/3/2018
#
# Contents: 
#   - Cleaning Data & Recoding Variables
#   - Create Graphs of Equilibrium Predictions vs. Experiment Results (Appendix B.1)
#   - Games Level Analysis 
#       + Linear Models w/ Proper Standard Errors (Results for Fig. 1 in Manuscript)
#		+ Make Coefficient Plot (Fig. 1 in Manuscript)
#       + Logit Model w/ Covariates (Additional Robustness Check)
# 		+ Check if CC is a focal point (Appendix. D.5.)
#   - Welfare Loss Analysis
#       + Linear Models w/ Proper Standard Errors (Results for Fig. 2 in Manuscript)
# 		+ Create Bargaining Efficiency Plot (Fig. 2 in Manuscript)
#   - Individual Strategy Analysis 
#       + Reformat Data for Discrete Time Survival Analysis
#       + Run Regression Analysis (Appendix D.4)
#   - Other & Robustness Checks
#       + Power analysis of main results (Appendix D.2)
#       + Checking validity of cross-over design (Appendix D.3)
#       + Learning Effects (Appendix D.5)
#       + Checking if fatigue affects experiment results (Appendix D.5)
#       + Other S.E. specifications for main results (Additional Robustness Checks)
#       + Other Recodings (Additional Robustness Checks)
#       + Main OLS results with User Fixed Effects (Additional Robustness Check)
#########################################################################################
# Load required libraries
library(survival)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(multiwayvcov)
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)

# Load experiment data results 
games <- read.csv() # Add File Path to attrition_games.csv
strategies <- read.csv() # Add File Path to attrition_players.csv

# Clean and Recode Games Data 
games <- games[which(games$game_number!=0),] # Remove Practice Round 

# Code Treatment Condition
games$treatment <- ifelse(games$num_turns == 10, 1, 0)
games$round_concede <- ifelse(games$treatment == 1, 11-games$turns_in_effect, 6-games$turns_in_effect)

# Code if 5-round games first 
games$rounds5_first <- ifelse(games$experiment_id == 32 | games$experiment_id == 48, 1, 0)

# Create CDF variable for coding 
games$concede1[games$round_concede == 1] <- 1
games$concede1[games$round_concede > 1] <- 0
games$concede2[games$round_concede <= 2] <- 1
games$concede2[games$round_concede > 2] <- 0
games$concede3[games$round_concede <= 3] <- 1
games$concede3[games$round_concede > 3] <- 0
games$concede4[games$round_concede <= 4] <- 1
games$concede4[games$round_concede > 4] <- 0
games$concede5[games$round_concede <= 5] <- 1
games$concede5[games$round_concede > 5] <- 0
games$concede6[games$round_concede <= 6] <- 1 
games$concede6[games$round_concede > 6] <- 0
games$concede7[games$round_concede <= 7] <- 1
games$concede7[games$round_concede > 7] <- 0
games$concede8[games$round_concede <= 8] <- 1
games$concede8[games$round_concede > 8] <- 0
games$concede9[games$round_concede <= 9] <- 1
games$concede9[games$round_concede > 9] <- 0
games$concede10[games$round_concede <= 10] <- 1
games$concede10[games$round_concede > 10] <- 0

################# Re-code Covariates for Dyadic Interaction ####################
# Gender 
games$female = ifelse(games$p1_gender == 1 | games$p2_gender == 1, 1, 0)

# Risk aversion 
games$p1_risk_aversion = ifelse(games$p1_risk_aversion == -999 | games$p1_risk_aversion == -1, NA, games$p1_risk_aversion)
games$p2_risk_aversion = ifelse(games$p2_risk_aversion == -999 | games$p2_risk_aversion == -1, NA, games$p2_risk_aversion)

games$averse = ifelse(games$p1_risk_aversion < 50 | games$p2_risk_aversion < 50, 1, NA)
games$averse = ifelse(games$p1_risk_aversion >= 50 | games$p2_risk_aversion >= 50, 0, games$averse)

# Time Preferences 
max_time = 6 * 6 + 5 * 6 + 4 * 6 + 3 * 6 + 2 * 6 + 1 * 6
games$p1_time_preferences_raw = ((6 * games$p1_time_pref_a) + (5 * games$p1_time_pref_b) +
                                     (4 * games$p1_time_pref_c) + (3 * games$p1_time_pref_d) + (2 * games$p1_time_pref_e) +
                                     (1 * games$p1_time_pref_f))
games$p1_time_preferences = ifelse(games$p1_time_preferences_raw > (max_time/2), "Long", "Short")

games$p2_time_preferences_raw = ((6 * games$p2_time_pref_a) + (5 * games$p2_time_pref_b) +
                                   (4 * games$p2_time_pref_c) + (3 * games$p2_time_pref_d) + (2 * games$p2_time_pref_e) +
                                   (1 * games$p2_time_pref_f))
games$p2_time_preferences = ifelse(games$p2_time_preferences_raw > (max_time / 2), "Long", "Short")

games$time_preferences <- ifelse(games$p1_time_preferences == "Short" & games$p2_time_preferences == "Short", 
								"1. Short-Short", NA)
games$time_preferences <- ifelse(games$p1_time_preferences == "Long" & games$p2_time_preferences == "Long", 
								"3. Long-Long", games$time_preferences)
games$time_preferences <- ifelse((games$p1_time_preferences == "Short" & games$p2_time_preferences == "Long") | 
								(games$p1_time_preferences == "Long" & games$p2_time_preferences == "Short"), 
								"2. Short-Long", games$time_preferences)

# Altruism 
games$altruism = ifelse(games$p1_share_amount >= 50 | games$p2_share_amount >= 50, 1, 0)

# Economics Training 
games$economics = ifelse(games$p1_has_training == 1 | games$p2_has_training == 1, 1, 0)

# Age 
games$over_22 = ifelse(games$p1_age > 22 | games$p2_age > 22, 1, 0)

# Game Comprehension
games$low_comprehension = ifelse(games$p1_quiz_score < 4 | games$p2_quiz_score < 4, 1, 0)

# Game Number
games$game_number = as.factor(games$game_number)

# Create subsets based on treatment
rounds5 = games[which(games$treatment == 0),]
rounds10 = games[which(games$treatment == 1),]

#####################################################################################
#####################################################################################
#####                   RESULTS VS. MODEL PREDICTIONS GRAPH                     #####
#####################################################################################
#####################################################################################
# Equilbrium Strategies (Prob Stand Firm)
msne <- c(1/2, 4/5, 5/6, 8/9, 9/10, 12/13, 13/14, 16/17, 17/18, 20/21)

# Equilibrium Reach Agreement by Each Round
equilibrium5 <- c(1-msne[5]^2, 
                 1 - (msne[5]^2 * msne[4]^2),
                 1 - (msne[5]^2 * msne[4]^2 * msne[3]^2),
                 1 - (msne[5]^2 * msne[4]^2 * msne[3]^2 * msne[2]^2),   
                 1 - (msne[5]^2 * msne[4]^2 * msne[3]^2 * msne[2]^2 * msne[1]^2) 
)

equilibrium10 <- c(1 - msne[10]^2, 
                   1 - (msne[10]^2 * msne[9]^2), 
                   1 - (msne[10]^2 * msne[9]^2 * msne[8]^2),
                   1 - (msne[10]^2 * msne[9]^2 * msne[8]^2 * msne[7]^2), 
                   1 - (msne[10]^2 * msne[9]^2 * msne[8]^2 * msne[7]^2 * msne[6]^2), 
                   1 - (msne[10]^2 * msne[9]^2 * msne[8]^2 * msne[7]^2 * msne[6]^2 * msne[5]^2), 
                   1 - (msne[10]^2 * msne[9]^2 * msne[8]^2 * msne[7]^2 * msne[6]^2 * msne[5]^2 * msne[4]^2), 
                   1 - (msne[10]^2 * msne[9]^2 * msne[8]^2 * msne[7]^2 * msne[6]^2 * msne[5]^2 * msne[4]^2 * msne[3]^2), 
                   1 - (msne[10]^2 * msne[9]^2 * msne[8]^2 * msne[7]^2 * msne[6]^2 * msne[5]^2 * msne[4]^2 * msne[3]^2 * msne[2]^2), 
                   1 - (msne[10]^2 * msne[9]^2 * msne[8]^2 * msne[7]^2 * msne[6]^2 * msne[5]^2 * msne[4]^2 * msne[3]^2 * msne[2]^2 * msne[1]^2) 
)

# Experiment Results
experiment5 <- c(mean(rounds5$concede1), mean(rounds5$concede2), mean(rounds5$concede3), 
				mean(rounds5$concede4), mean(rounds5$concede5))
experiment10 <- c(mean(rounds10$concede1), mean(rounds10$concede2), mean(rounds10$concede3), 
				mean(rounds10$concede4), mean(rounds10$concede5),
                  mean(rounds10$concede6), mean(rounds10$concede7), mean(rounds10$concede8), 
                  mean(rounds10$concede9), mean(rounds10$concede10))

# Combine to one data frame
gdf <- data.frame(cbind(equilibrium5, equilibrium10, experiment5, experiment10))
gdf$index <- seq(from= 1, to = 10, by = 1)
gdf$equilibrium5 <- ifelse(gdf$index > 5, NA, gdf$equilibrium5)
gdf$experiment5 <- ifelse(gdf$index > 5, NA, gdf$experiment5)

# Create Plot  -- MODEL PREDICTIONS
plot_vars = c("equilibrium5", "equilibrium10")
melted_data <- melt(gdf, measure.vars = plot_vars)
states.list <- list('5-Round Predictions', '10-Round Predictions')
names(states.list) <- plot_vars
melted_data <- ddply(melted_data, .(variable), transform, State = states.list[[unique(variable)]])
ggplot(melted_data) +
   geom_line(aes(x = index, y = value, color = State, linetype = State)) +
   geom_point(aes(x = index, y = value, color = State, linetype = State)) +
   theme_minimal() +
   theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 13),) +
   scale_color_manual(values = c("red", "blue")) +
   scale_linetype_manual(values = c("solid", "solid")) +
   labs(
      title = "Cumulative Probability of Agreement by Round",
      subtitle = "(Model Predictions)",
        x = "Round Number",
        y = "Proportion of Agreements Met") +
   xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

# Create Plot -- Results vs. Model Predictions
plot_vars = c("equilibrium5", "equilibrium10", "experiment5", "experiment10")
melted_data <- melt(gdf, measure.vars = plot_vars)
states.list <- list ('5-Round Predictions', '10-Round Predictions', '5-Round Results', '10-Round Results')
names(states.list) <- plot_vars
melted_data <- ddply(melted_data, .(variable), transform, State = states.list[[unique(variable)]])
ggplot(melted_data) +
   geom_line(aes(x = index, y = value, color = State, linetype = State)) +
   geom_point(aes(x = index, y = value, color = State, linetype = State)) +
   theme_minimal() +
   theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
   ) +
   scale_color_manual(values = c("red", "blue", "red", "blue")) +
   scale_linetype_manual(values = c("dashed", "dashed", "solid", "solid")) +
   labs(
      title = "Cumulative Proportion of Agreements by Round",
      subtitle="(Results vs. Model Predictions)",
        x = "Round Number",
        y = "Proportion of Agreements Met"
   ) +
   xlim("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")






#####################################################################################
#####################################################################################
#####                           GAME LEVEL ANALYSIS                             #####
#####################################################################################
#####################################################################################
# --------------------------------------------------------------------------------- #
#                           OLS w/ Wild Bootstrapped SEs                            #
# --------------------------------------------------------------------------------- #
# Run OLS Regressions w/ Wild Bootstrapped Standard Errors (Clustered on P1 & P2 User ID)
# Concede by Round 1
cdf1 <- lm(concede1 ~ treatment, data = games)
set.seed(89302)
cdf1_cbse <- cluster.boot(cdf1, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf1_se <- sqrt(diag(cdf1_cbse))
cdf1_c <- coeftest(cdf1, cdf1_cbse) # Bootstrapped - Treatment Not Significant

# Concede by Round 2 
cdf2 <- lm(concede2 ~ treatment, data = games)
set.seed(89302)
cdf2_cbse <- cluster.boot(cdf2, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf2_se <- sqrt(diag(cdf2_cbse))
cdf2_c <- coeftest(cdf2, cdf2_cbse) # Bootstrapped - Treatment Not Significant

# Concede by Round 3
cdf3 <- lm(concede3 ~ treatment, data = games)
set.seed(89302)
cdf3_cbse <- cluster.boot(cdf3, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf3_se <- sqrt(diag(cdf3_cbse))
cdf3_c <- coeftest(cdf3, cdf3_cbse)

# Concede by Round 4
cdf4 <- lm(concede4 ~ treatment, data = games)
set.seed(89302)
cdf4_cbse <- cluster.boot(cdf4, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf4_se <- sqrt(diag(cdf4_cbse))
cdf4_c <- coeftest(cdf4, cdf4_cbse)

# Concede by Round 5
cdf5 <- lm(concede5 ~ treatment, data = games)
set.seed(89302)
cdf5_cbse <- cluster.boot(cdf5, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf5_se <- sqrt(diag(cdf5_cbse))
cdf5_c <- coeftest(cdf5, cdf5_cbse)

stargazer(cdf1, cdf2, cdf3, cdf4, cdf5, 
          se = list(cdf1_se, cdf2_se, cdf3_se, cdf4_se, cdf5_se), 
          out = "OLS Games Results.htm")


# --------------------------------------------------------------------------------- #
#                            Create Fig. 1 in Paper 	                            #
# --------------------------------------------------------------------------------- #
# Create new theme for plot appearance
new_theme <- function() {
    # Set the core colors
    color.background = "white"
    color.grid.major = "gray70"
    color.grid.minor = "gray90"
    color.axis.text = "gray30"
    color.axis.title = "gray50"
    color.title = "gray30"

    # Other key values
    theme_bw(base_size = 18) +

    # Set the backrgound
    theme(panel.background = element_rect(fill = color.background, color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(color = color.background)) +

    # Format the grid
    theme(panel.grid.major.y = element_line(color = color.grid.major, size = .25)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor = element_line(color = color.grid.minor, size = .2)) +
    theme(axis.ticks = element_blank()) +
    theme(strip.background = element_rect(fill = "white")) +

    # Format the legend, bottom by default
    theme(legend.position = "bottom") +
    theme(legend.background = element_rect(fill = color.background)) +
    theme(legend.key = element_rect(colour = 'white')) +
    theme(legend.text = element_text(size = 18, color = color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color.title, size = 18, vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 18, color = color.axis.text)) +
    theme(axis.text.y = element_text(size = 18, color = color.axis.text)) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 18, color = color.axis.title, vjust = 1.25))
}

# Make data frame of key values for Coefficient Plot (Estimate, Upper and Lower Bounds for CIs)
m1.diff = cdf1_c[2, 1]
m2.diff = cdf2_c[2, 1]
m3.diff = cdf3_c[2, 1]
m4.diff = cdf4_c[2, 1]
m5.diff = cdf5_c[2, 1]

m1.low = m1.diff - (qt(.975, nrow(games)) * cdf1_c[2, 2])
m1.high = m1.diff + (qt(.975, nrow(games)) * cdf1_c[2, 2])
m2.low = m2.diff - (qt(.975, nrow(games)) * cdf2_c[2, 2])
m2.high = m2.diff + (qt(.975, nrow(games)) * cdf2_c[2, 2])
m3.low = m3.diff - (qt(.975, nrow(games)) * cdf3_c[2, 2])
m3.high = m3.diff + (qt(.975, nrow(games)) * cdf3_c[2, 2])
m4.low = m4.diff - (qt(.975, nrow(games)) * cdf4_c[2, 2])
m4.high = m4.diff + (qt(.975, nrow(games)) * cdf4_c[2, 2])
m5.low = m5.diff - (qt(.975, nrow(games)) * cdf5_c[2, 2])
m5.high = m5.diff + (qt(.975, nrow(games)) * cdf5_c[2, 2])

d <- data.frame(round = c("Round 1", "Round 2", "Round 3", "Round 4", "Round 5"), 
                treatment = c("Long\n(10 rounds)", "Long\n(10 rounds)", "Long\n(10 rounds)", 
                			  "Long\n(10 rounds)", "Long\n(10 rounds)"),
                difference = c(m1.diff, m2.diff, m3.diff, m4.diff, m5.diff))
d$treatment <- factor(d$treatment, levels = rev(levels(d$treatment)))

# Create coefficient plot 
g3 <- ggplot() +
      new_theme() + 
      scale_y_continuous(limits = c(-.2, .05)) +
      ylab("Difference in Cumulative Proportion of Agreements\n (Long Games - Short Games)") +
      
    # Add Coefficient Estimates
    geom_point(
            aes(x = round, y = difference), 
            size = 7, color ="white",  
            data = d) +

    # Add Confidence Intervals 
    annotate("segment", x = 1, xend = 1, y = m1.low, yend = m1.high, colour = "#96caff", size = 2) + 
    annotate("segment", x = 2, xend = 2, y = m2.low, yend = m2.high, colour = "#96caff", size = 2) +
    annotate("segment", x = 3, xend = 3, y = m3.low, yend = m3.high, colour = "#002c5a", size = 2) +
    annotate("segment", x = 4, xend = 4, y = m4.low, yend = m4.high, colour = "#002c5a", size = 2) +
    annotate("segment", x = 5, xend = 5, y = m5.low, yend = m5.high, colour = "#002c5a", size = 2) + 

    # Add line at 0
    geom_hline(aes(yintercept = 0), lty = 2, size = 1.25, colour = "gray50") +

    # Add Coefficient Estimates (to go over the line)
    geom_point(
            aes(x = round, y = difference),
            size = 6, color = "red4", 
            data = d) +
        geom_point(
            aes(x = round, y = difference),
            size = 4, color = "white",
            data = d) 
g3

# --------------------------------------------------------------------------------- #
#                    Logit Regs w/ Covariates (Wild Bootstrap SEs)                  #
# --------------------------------------------------------------------------------- #
# Logit Regression Analysis w/ Covariates
log1 <- glm(concede1 ~ treatment + female + averse + time_preferences + altruism + 
			economics + over_22 + low_comprehension, data = games, family = "binomial")
summary(log1)
set.seed(354654)
log1_cbse <- cluster.boot(log1, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
log1_se <- sqrt(diag(log1_cbse))
coeftest(log1, log1_cbse)

log2 <- glm(concede2 ~ treatment + female + averse + time_preferences + altruism + 
			economics + over_22 + low_comprehension, data = games, family = "binomial")
summary(log2)
set.seed(354654)
log2_cbse <- cluster.boot(log2, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
log2_se <- sqrt(diag(log2_cbse))
coeftest(log2, log2_cbse)

log3 <- glm(concede3 ~ treatment + female + averse + time_preferences + altruism + 
			economics + over_22 + low_comprehension, data = games, family = "binomial")
summary(log3)
set.seed(354654)
log3_cbse <- cluster.boot(log3, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
log3_se <- sqrt(diag(log3_cbse))
coeftest(log3, log3_cbse)

log4 <- glm(concede4 ~ treatment + female + averse + time_preferences + altruism + 
			economics + over_22 + low_comprehension, data = games, family = "binomial")
summary(log4)
set.seed(354654)
log4_cbse <- cluster.boot(log4, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
log4_se <- sqrt(diag(log4_cbse))
coeftest(log4, log4_cbse)

log5 <- glm(concede5 ~ treatment + female + averse + time_preferences + altruism + 
			economics + over_22 + low_comprehension, data = games, family = "binomial")
summary(log5)
set.seed(354654)
log5_cbse <- cluster.boot(log5, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
log5_se <- sqrt(diag(log5_cbse))
coeftest(log5, log5_cbse)

stargazer(log1, log2, log3, log4, log5,
          se = list(log1_se, log2_se, log3_se, log4_se, log5_se),
          type = "html", out = "Logit Games Results.htm")

# --------------------------------------------------------------------------------- #
#                  			  Is CC a Focal Point? 		     		       		    #
# --------------------------------------------------------------------------------- #
prop.table(table(games$concede1, games$num_concedes), 1) 
table(games$concede1)
table(games$num_concedes)
table(games$concede1, games$num_concedes)






#####################################################################################
#####################################################################################
#####                           WELFARE ANALYSIS                                #####
#####################################################################################
#####################################################################################

# Data Prep for Welfare Analysis
games$tot_possible <- ifelse(games$treatment == 1, 20, 10)
games$tot_payment <- games$turns_in_effect * 2
games$welfare_loss <- games$tot_possible - games$tot_payment
games$per_welfare_loss <- games$welfare_loss/games$tot_possible * 100

rounds5 = games[which(games$treatment==0),]
rounds10 = games[which(games$treatment==1),]

# T-test of Welfare Analysis 
t.test(rounds5$tot_payment, rounds10$tot_payment) # Games Payment
t.test(rounds5$welfare_loss, rounds10$welfare_loss) # Nominal Welfare Loss
t.test(rounds5$per_welfare_loss, rounds10$per_welfare_loss) # Percent Loss

# OLS Regressions w/ Wild Bootstrapped SEs (clustered on P1 & P2)
pay <- lm(tot_payment ~ treatment, data = games)
set.seed(6365435)
pay_cbse <- cluster.boot(pay, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
pay_se <- sqrt(diag(pay_cbse))
coeftest(pay, pay_cbse)

loss <- lm(welfare_loss ~ treatment, data = games)
set.seed(6365435)
loss_cbse <- cluster.boot(loss, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
loss_se <- sqrt(diag(loss_cbse))
coeftest(loss, loss_cbse)

per_loss <- lm(per_welfare_loss ~ treatment, data = games)
set.seed(6365435)
per_loss_cbse <- cluster.boot(per_loss, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
per_loss_se <- sqrt(diag(per_loss_cbse))
coeftest(per_loss, per_loss_cbse)

stargazer(pay, loss, per_loss, 
          se = list(pay_se, loss_se, per_loss_se),
          type = "html", out = "OLS welfare analysis.htm")


# --------------------------------------------------------------------------------- #
#                  		   Create Fig. 2 in Manuscript      		       		    #
# --------------------------------------------------------------------------------- #
# Create new theme for plot appearance
new_theme <- function() {
    # Set the core colors
    color.background = "white"
    color.grid.major = "gray70"
    color.grid.minor = "gray90"
    color.axis.text = "gray30"
    color.axis.title = "gray50"
    color.title = "gray30"

    # Other key values
    theme_bw(base_size = 18) +

    # Set the backrgound
    theme(panel.background = element_rect(fill = color.background, color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(color = color.background)) +

    # Format the grid
    theme(panel.grid.major.y = element_line(color = color.grid.major, size = .25)) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor = element_line(color = color.grid.minor, size = .2)) +
    theme(axis.ticks = element_blank()) +
    theme(strip.background = element_rect(fill = "white")) +

    # Format the legend, bottom by default
    theme(legend.position = "bottom") +
    theme(legend.background = element_rect(fill = color.background)) +
    theme(legend.key = element_rect(colour = 'white')) +
    theme(legend.text = element_text(size = 18, color = color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color.title, size = 18, vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 18, color = color.axis.text)) +
    theme(axis.text.y = element_text(size = 18, color = color.axis.text)) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_text(size = 18, color = color.axis.title, vjust = 1.25))
}

# Make data frame of key values for Coefficient Plot (Estimate, Upper and Lower Bounds for CIs)
rounds5_payment = mean(rounds5$tot_payment)
rounds10_payment = mean(rounds10$tot_payment)

rounds10_percent = round(100 * rounds10_payment / 20)
rounds10_lost_percent = paste(100 - rounds10_percent, "% Loss", sep = "")
rounds5_percent = round(100 * rounds5_payment / 10)
rounds5_lost_percent = paste(100 - rounds5_percent, "% Loss", sep = "")

d <- data.frame(treatment = c("Long\n(10 rounds)", "Long\n(10 rounds)", "Short\n(5 rounds)", "Short\n(5 rounds)"),
                type = c("Average Payoff", "Possible Payoff", "Average Payoff", "Possible Payoff"),
                labels = c("", rounds10_lost_percent, "", rounds5_lost_percent),
                payment = c(rounds10_payment, 20 - rounds10_payment, rounds5_payment, 10 - rounds5_payment))
d$type <- factor(d$type, levels = rev(levels(d$type)))
d$treatment <- factor(d$treatment, levels = rev(levels(d$treatment)))

# Generate Plot 
g <- ggplot() + 
  new_theme() +
  labs(fill = "") +
  ylab("Game Payment") + 
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  geom_bar(
        aes(x = treatment, y = payment, fill=type),
        width = 0.43, stat = 'identity',
        data = d) +
  #geom_text(
        #aes(x = treatment, y = payment, label = labels),
        #size = 5, 
        #position = position_stack(vjust = 0.5),
        #data = d) +
  scale_fill_manual(values = c("Average Payoff" = "#96caff" , "Possible Payoff" = "#002c5a")) +
  annotate("text", x = 1.422, y = .15 + .49 * (rounds5_payment + 10), label = rounds5_lost_percent, size = 5.8, color = "red4") +
  annotate("text", x = 1.25, y = .15 + .49 * (rounds5_payment + 10), label = 'bold("}")', size = 12, parse = TRUE, color = "red4") +

  annotate("text", x = 2.45, y = .15 + .498 * (rounds10_payment + 20), label = rounds10_lost_percent, size = 5.8, color = "red4") +
  annotate("text", x = 2.26, y = .4 + .5 * (rounds10_payment + 20), label = "}", size = 22, color = "red4") 
g





#####################################################################################
#####################################################################################
#####                     INDIVIDUAL STRATEGY ANALYSIS                          #####
#####################################################################################
#####################################################################################

# --------------------------------------------------------------------------------- #
#                   Data Prep for Discrete Time Survival Analysis                   #
# --------------------------------------------------------------------------------- #
strategies <- read.csv("C:/Users/linds/Dropbox/Bargaining Delay Experiment/Experiment Results/attrition_players_fixed.csv")
strategies <- strategies[order(strategies$game_number),]
strategies <- strategies[order(strategies$user_id), ]

### Remove Practice Round Observations 
strategies <- strategies[which(strategies$game_number != 0),]

### Create a Treatment Indicator
strategies$treatment <- ifelse(strategies$num_turns == 10, 1, 0)

# Create Variable Indicating Round Conceded (NA means never conceded)
strategies$round_concede = ifelse(strategies$turn_1 == "Low", 1, 0)
strategies$round_concede = ifelse(strategies$turn_2 == "Low", 2, strategies$round_concede)
strategies$round_concede = ifelse(strategies$turn_3 == "Low", 3, strategies$round_concede)
strategies$round_concede = ifelse(strategies$turn_4 == "Low", 4, strategies$round_concede)
strategies$round_concede = ifelse(strategies$turn_5 == "Low", 5, strategies$round_concede)
strategies$round_concede = ifelse(strategies$turn_6 == "Low", 6, strategies$round_concede)
strategies$round_concede = ifelse(strategies$turn_7 == "Low", 7, strategies$round_concede)
strategies$round_concede = ifelse(strategies$turn_8 == "Low", 8, strategies$round_concede)
strategies$round_concede = ifelse(strategies$turn_9 == "Low", 9, strategies$round_concede)
strategies$round_concede = ifelse(strategies$turn_10 == "Low", 10, strategies$round_concede)

# Reshape Data for Discrete Time Survival Analysis
index <- rep(seq_len(nrow(strategies)), each = 10)
reshaped <- strategies[index,] # Create a row for each period of the game

# Create a Period Indicator
period = c()
for (i in 1:nrow(reshaped)) {
    remainder = i %% 10
    period[i] <- ifelse(remainder == 0, 10, i %% 10)
}
reshaped$period <- period

# Create Indicator for Whether to Keep Row or Not
reshaped$keep = ifelse(reshaped$period == 1 & (reshaped$turn_1 == "High" |
                                               reshaped$turn_1 == "Low"), 1, 0)
reshaped$keep = ifelse(reshaped$period == 2 & (reshaped$turn_2 == "High" |
                                               reshaped$turn_2 == "Low"), 1, reshaped$keep)
reshaped$keep = ifelse(reshaped$period == 3 & (reshaped$turn_3 == "High" |
                                               reshaped$turn_3 == "Low"), 1, reshaped$keep)
reshaped$keep = ifelse(reshaped$period == 4 & (reshaped$turn_4 == "High" |
                                               reshaped$turn_4 == "Low"), 1, reshaped$keep)
reshaped$keep = ifelse(reshaped$period == 5 & (reshaped$turn_5 == "High" |
                                               reshaped$turn_5 == "Low"), 1, reshaped$keep)
reshaped$keep = ifelse(reshaped$period == 6 & (reshaped$turn_6 == "High" |
                                               reshaped$turn_6 == "Low"), 1, reshaped$keep)
reshaped$keep = ifelse(reshaped$period == 7 & (reshaped$turn_7 == "High" |
                                               reshaped$turn_7 == "Low"), 1, reshaped$keep)
reshaped$keep = ifelse(reshaped$period == 8 & (reshaped$turn_8 == "High" |
                                               reshaped$turn_8 == "Low"), 1, reshaped$keep)
reshaped$keep = ifelse(reshaped$period == 9 & (reshaped$turn_9 == "High" |
                                               reshaped$turn_9 == "Low"), 1, reshaped$keep)
reshaped$keep = ifelse(reshaped$period == 10 & (reshaped$turn_10 == "High" |
                                                reshaped$turn_10 == "Low"), 1, reshaped$keep)

# Get rid of bad rows 
reshaped_final = reshaped[which(reshaped$keep == 1),]

# Create Covariates 
reshaped_final$time_preferences_raw = ((6 * reshaped_final$time_pref_a) + (5 * reshaped_final$time_pref_b) +
                                     (4 * reshaped_final$time_pref_c) + (3 * reshaped_final$time_pref_d) + 
                                     (2 * reshaped_final$time_pref_e) + 1 * reshaped_final$time_pref_f))
reshaped_final$time_preferences_ind = ifelse(reshaped_final$time_preferences_raw > 63, "2. Long", "1. Short")

reshaped_final$risk_preferences_ind = ifelse(reshaped_final$risk_aversion == -999 | reshaped_final$risk_aversion == -1, NA, 
										ifelse(reshaped_final$risk_aversion < 50, "2. Averse", 
											ifelse(reshaped_final$risk_aversion > 50, "3. Acceptant", "1. Neutral")))
reshaped_final$risk_preferences = ifelse(reshaped_final$risk_aversion == -999 | 
										reshaped_final$risk_aversion == -1, NA, reshaped_final$risk_aversion)

reshaped_final$altruism_ind = ifelse(reshaped_final$share_amount == 0, "1. None",
                             ifelse(reshaped_final$share_amount < 50, "2. Low", "3. High"))
reshaped_final$age_ind = ifelse(reshaped_final$age < 23, "1. 22 and Under", "2. Over 22")
reshaped_final$comprehension_ind = ifelse(reshaped_final$quiz_score < 4, "Low", "High")
reshaped_final$game_number = as.factor(reshaped_final$game_number)

# Event
reshaped_final$event = ifelse(reshaped_final$round_concede == reshaped_final$period, 1, 0)

# --------------------------------------------------------------------------------- #
#              Regression Analysis (Discrete Time Survival Analysis)                #
# --------------------------------------------------------------------------------- #
###################         MAIN RESULTS W/O COVARIATES           ################### 
### Create Data Set to only test Treatment w/ & w/o User Fixed Effects 
treat_mod <- reshaped_final[, c("event", "user_id", "period", "treatment")]
treat_mod <- treat_mod[which(complete.cases(treat_mod)),]
treat_mod$user_id <- as.factor(treat_mod$user_id)
treat_mod$period <- as.factor(treat_mod$period)

# Discrete Time Survival Analys w/o User Fixed Effects 
mod2 <- glm(event ~ period + treatment, data = treat_mod, family = "binomial")
summary(mod2)
set.seed(653245)
mod2_cbse <- cluster.boot(mod2, treat_mod$user_id, boot_type = "wild", R = 1000)
mod2_se <- sqrt(diag(mod2_cbse))
coeftest(mod2, mod2_cbse)

# Discrete Time Survival Analysis w/ User Fixed Effects
mod1 <- glm(event ~ period + treatment + user_id, data = treat_mod, family = "binomial")
summary(mod1)
set.seed(653245)
mod1_cbse <- cluster.boot(mod1, treat_mod$user_id, boot_type = "wild", R = 1000)
mod1_se <- sqrt(diag(mod1_cbse))
coeftest(mod1, mod1_cbse)

###################             RESULTS W/ COVARIATES              ################### 
### Create Data Set with Covariates
cov_mod2 <- reshaped_final[, c("event", "user_id", "period", "treatment", "gender", 
						   "risk_preferences", "time_preferences_raw", "share_amount", 
						   "has_training", "age", "quiz_score")]
cov_mod2 <- cov_mod2[which(complete.cases(cov_mod2)),]
cov_mod2$period <- as.factor(cov_mod2$period)

mod_cov2 <- glm(event ~ period + treatment + gender + risk_preferences + time_preferences_raw + 
				share_amount + has_training + age + quiz_score, data = cov_mod2, family = "binomial")
summary(mod_cov2)
set.seed(653245)
mod_cov2_cbse <- cluster.boot(mod_cov2, cov_mod2$user_id, boot_type = "wild", R = 1000)
mod_cov2_se <- sqrt(diag(mod_cov2_cbse))
coeftest(mod_cov2, mod_cov2_cbse)

### Create Data Set with Covariaties (as Indicators)
cov_mod <- reshaped_final[, c("event", "user_id", "period", "treatment", "gender", 
							  "risk_preferences_ind", "time_preferences_ind", "altruism_ind", 
							  "has_training", "age_ind", "comprehension_ind")]
cov_mod <- cov_mod[which(complete.cases(cov_mod)),]
cov_mod$period <- as.factor(cov_mod$period)

mod_cov <- glm(event ~ period + treatment + gender + risk_preferences_ind + time_preferences_ind + 
				altruism_ind + has_training + age_ind + comprehension_ind, family = "binomial", data = cov_mod)
summary(mod_cov)
set.seed(653245)
mod_cov_cbse <- cluster.boot(mod_cov, cov_mod$user_id, boot_type = "wild", R = 1000)
mod_cov_se <- sqrt(diag(mod_cov_cbse))
coeftest(mod_cov, mod_cov_cbse)

stargazer(mod1, mod_cov2,
          se = list(mod1_se, mod_cov2_se),
          type = "latex")






#####################################################################################
#####################################################################################
#####                        OTHER / ROBUSTNESS CHECKS                          #####
#####################################################################################
#####################################################################################

# --------------------------------------------------------------------------------- #
#                          Power Analysis of Main Results                           #
# --------------------------------------------------------------------------------- #
# Power of T-Tests 
power.t.test(n = nrow(rounds5), delta = 0.041, sd = sd(games$concede1), sig.level = 0.05)
power.t.test(n = nrow(rounds5), delta = 0.063, sd = sd(games$concede2), sig.level = 0.05)
power.t.test(n = nrow(rounds5), delta = 0.083, sd = sd(games$concede3), sig.level = 0.05)
power.t.test(n = nrow(rounds5), delta = 0.104, sd = sd(games$concede4), sig.level = 0.05)
power.t.test(n = nrow(rounds5), delta = 0.075, sd = sd(games$concede5), sig.level = 0.05)

# --------------------------------------------------------------------------------- #
#                       Checking Validity of Crossover Design                       #
# --------------------------------------------------------------------------------- #
# Game Analysis Results 
xo1 <- lm(concede1 ~ rounds5_first, data = games)
set.seed(100384)
xo1_cbse <- cluster.boot(xo1, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
xo1_se <- sqrt(diag(xo1_cbse))
coeftest(xo1, xo1_cbse)

xo2 <- lm(concede2 ~ rounds5_first, data = games)
set.seed(100384)
xo2_cbse <- cluster.boot(xo2, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
xo2_se <- sqrt(diag(xo2_cbse))
coeftest(xo2, xo2_cbse)

xo3 <- lm(concede3 ~ rounds5_first, data = games)
set.seed(100384)
xo3_cbse <- cluster.boot(xo3, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
xo3_se <- sqrt(diag(xo3_cbse))
coeftest(xo3, xo3_cbse)

xo4 <- lm(concede4 ~ rounds5_first, data = games)
set.seed(100384)
xo4_cbse <- cluster.boot(xo4, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
xo4_se <- sqrt(diag(xo4_cbse))
coeftest(xo4, xo4_cbse)

xo5 <- lm(concede5 ~ rounds5_first, data = games)
set.seed(100384)
xo5_cbse <- cluster.boot(xo5, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
xo5_se <- sqrt(diag(xo5_cbse))
coeftest(xo5, xo5_cbse)


stargazer(xo1, xo2, xo3, xo4, xo5,
          se = list(xo1_se, xo2_se, xo3_se, xo4_se, xo5_se), 
          type = "latex")
#          type = "html", out = "Main Results Design Validation.htm")

# Checking Individual Strategy Profiles (Discrete Time Survival Analysis)
reshaped_final$rounds5_first <- ifelse(reshaped_final$experiment_id == 32 | reshaped_final$experiment_id == 48, 1, 0)
xo_mod <- reshaped_final[, c("event", "user_id", "period", "treatment", "rounds5_first")]
xo_mod <- xo_mod[which(complete.cases(xo_mod)),]
xo_mod$user_id <- as.factor(xo_mod$user_id)
xo_mod$period <- as.factor(xo_mod$period)

# W/O User Fixed Effects
ixo1 <- glm(event ~ period + rounds5_first, data = xo_mod, family = "binomial")
set.seed(6584321)
ixo1_cbse <- cluster.boot(ixo1, xo_mod$user_id, boot_type = "wild", R = 1000)
ixo1_se <- sqrt(diag(ixo1_cbse))
coeftest(ixo1, ixo1_cbse)

# W/ User Fixed Effects 
ixo2 <- glm(event ~ period + rounds5_first + user_id, data = xo_mod, family = "binomial")
set.seed(6584321)
ixo2_cbse <- cluster.boot(ixo2, xo_mod$user_id, boot_type = "wild", R = 1000)
ixo2_se <- sqrt(diag(ixo2_cbse))
coeftest(ixo2, ixo2_cbse)

stargazer(ixo1, ixo2, 
          se = list(ixo1_se, ixo2_se),
          type = "latex")

# --------------------------------------------------------------------------------- #
#                             Learning Effects Analysis                             #
# --------------------------------------------------------------------------------- #
### Create Data Set to only test Treatment w/ & w/o User Fixed Effects
learning <- reshaped_final[, c("event", "user_id", "period", "treatment", "game_number")]
learning <- learning[which(complete.cases(learning)),]
learning$user_id <- as.factor(learning$user_id)
learning$period <- as.factor(learning$period)
learning$game_number <- as.factor(learning$game_number)

lmod <- glm(event ~ treatment + game_number + period + user_id, data = learning, family = "binomial")
summary(lmod)
set.seed(239487)
lmod_cbse <- cluster.boot(lmod, learning$user_id, boot_type = "wild", R = 1000)
lmod_se <- sqrt(diag(lmod_cbse))
coeftest(lmod, lmod_cbse)


createCoefFrame <- function(model, varcov_matrix, model_title, rel_vars){
    relevant_variables = rel_vars
    coefficients = coeftest(model, varcov_matrix)[,1][relevant_variables]
    coefficients = coefficients[!is.na(coefficients)]
    standard_errors = coeftest(model, varcov_matrix)[, 2][relevant_variables]
    standard_errors = standard_errors[!is.na(standard_errors)]
    var_names = names(coefficients)
    data.frame(Variable = names(coefficients),
                   SE = standard_errors,
                   modelName = title)
}


lmod_vars <- c("game_number2", "game_number3", "game_number4", "game_number5",
"game_number6", "game_number7", "game_number8", "game_number9",
"game_number10", "game_number11", "game_number12", "game_number13",
"game_number14", "game_number15", "game_number16", "game_number17",
"game_number18", "game_number19", "game_number20")

coefficients = coeftest(lmod, lmod_cbse)[, 1][lmod_vars]
coefficients = coefficients[!is.na(coefficients)]
standard_errors = coeftest(lmod, lmod_cbse)[, 2][lmod_vars]
standard_errors = standard_errors[!is.na(standard_errors)]

frame = data.frame(Variable = names(coefficients),
                   Coefficient = coefficients, 
                   SE = standard_errors,
                   modelName = "Learning Effects Model",
                   Order = 1:19)

frame$Variable = c("Game 02", "Game 03", "Game 04", "Game 05", "Game 06", "Game 07", "Game 08", "Game 09", "Game 10", 
                   "Game 11", "Game 12", "Game 13", "Game 14", "Game 15", "Game 16", "Game 17", "Game 18", "Game 19", "Game 20")

# Specify the width of your confidence intervals
interval1 <- -qnorm((1 - 0.9) / 2) # 90% multiplier
interval2 <- -qnorm((1 - 0.95) / 2) # 95% multiplier

# Plot
zp1 <- ggplot(frame)
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1 / 2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE * interval1,
                                ymax = Coefficient + SE * interval1),
                            lwd = 1, position = position_dodge(width = 1 / 2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE * interval2,
                                 ymax = Coefficient + SE * interval2),
                             lwd = 1 / 2, position = position_dodge(width = 1 / 2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + ggtitle("Analysis of Learning Effects")
zp1 <- zp1 + theme(plot.title = element_text(size = 22))
print(zp1)

# --------------------------------------------------------------------------------- #
#                      Examining Fatigue and Effect on Results                      #
# --------------------------------------------------------------------------------- #
####### Strategy -- Run Individual Analysis w/ Game Fixed Effects 
### Create Data Set to only test Treatment w/ & w/o User Fixed Effects 
treat_mod <- reshaped_final[, c("event", "user_id", "period", "treatment", "game_number")]
treat_mod <- treat_mod[which(complete.cases(treat_mod)),]
treat_mod$user_id <- as.factor(treat_mod$user_id)
treat_mod$period <- as.factor(treat_mod$period)

# Discrete Time Survival Analysis w/ User Fixed Effects -- Game Num FEs
mod1 <- glm(event ~ period + treatment + user_id + game_number, data = treat_mod, family = "binomial")
summary(mod1)
set.seed(653245)
mod1_cbse <- cluster.boot(mod1, treat_mod$user_id, boot_type = "wild", R = 1000)
mod1_se <- sqrt(diag(mod1_cbse))
coeftest(mod1, mod1_cbse)

# Discrete Time Survival Analysis w/ User Fixed Effects -- Game Time Trends 
treat_mod$time <- as.numeric(treat_mod$game_number)
treat_mod$time2 <- treat_mod$time^2 
treat_mod$time3 <- treat_mod$time^3

mod2 <- glm(event ~ period + treatment + user_id + time + time2 + time3, data = treat_mod, family = "binomial")
summary(mod2)
set.seed(653245)
mod2_cbse <- cluster.boot(mod2, treat_mod$user_id, boot_type = "wild", R = 1000)
mod2_se <- sqrt(diag(mod2_cbse))
coeftest(mod2, mod2_cbse)

stargazer(mod1, mod2, se = list(mod1_se, mod2_se), type = "latex")

# --------------------------------------------------------------------------------- #
#                     Other S.E. Specifications for Main Results                    #
# --------------------------------------------------------------------------------- #
# Dual Cluster Robust Standard Errors 
cdf1_crse <- cluster.vcov(cdf1, cbind(games$p1_user_id, games$p2_user_id)) # Clustered Robust SEs on both user IDs
coeftest(cdf1, cdf1_crse)

cdf2_crse <- cluster.vcov(cdf2, cbind(games$p1_user_id, games$p2_user_id)) # Clustered Robust SEs on both user IDs
coeftest(cdf2, cdf2_crse)

cdf3_crse <- cluster.vcov(cdf3, cbind(games$p1_user_id, games$p2_user_id)) # Clustered Robust SEs on both user IDs
coeftest(cdf3, cdf3_crse)

cdf4_crse <- cluster.vcov(cdf4, cbind(games$p1_user_id, games$p2_user_id)) # Clustered Robust SEs on both user IDs
coeftest(cdf4, cdf4_crse)

cdf5_crse <- cluster.vcov(cdf4, cbind(games$p1_user_id, games$p2_user_id))
coeftest(cdf5, cdf5_crse)

# Clustered Robust Standard Errors (Clustered on P1 OR P2)
# Function to generate Clustered Robust Standard Errors 
robust.se <- function(model, cluster) {
    require(sandwich)
    require(lmtest)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M / (M - 1)) * ((N - 1) / (N - K))
    uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
    rcse.cov <- dfc * sandwich(model, meat = crossprod(uj) / N)
    rcse.se <- coeftest(model, rcse.cov)
    return(list(rcse.cov, rcse.se))
}

robust.se(cdf1, games$p1_user_id)[[2]] # P1 Cluster
robust.se(cdf1, games$p2_user_id)[[2]] # P2 Cluster

robust.se(cdf2, games$p1_user_id)[[2]] # P1 Cluster
robust.se(cdf2, games$p2_user_id)[[2]] # P2 Cluster

robust.se(cdf3, games$p1_user_id)[[2]] # P1 Cluster
robust.se(cdf3, games$p2_user_id)[[2]] # P2 Cluster

robust.se(cdf4, games$p1_user_id)[[2]] # P1 Cluster
robust.se(cdf4, games$p2_user_id)[[2]] # P2 Cluster

robust.se(cdf5, games$p1_user_id)[[2]] # P1 Cluster
robust.se(cdf5, games$p2_user_id)[[2]] # P2 Cluster

# Robust Standard Errors 
cdf1$robust_se <- vcovHC(cdf1) 
cdf2$robust_se <- vcovHC(cdf2)
cdf3$robust_se <- vcovHC(cdf3)
cdf4$robust_se <- vcovHC(cdf4)
cdf5$robust_se <- vcovHC(cdf5)

# Clustered Bootstrapped Standard Errors 
# Functions to generate Clustered Bootstrapped Standard Errors 
clusbootreg <- function(formula, data, cluster, num_var, reps = 1000) {
    reg1 <- lm(formula, data)
    clusters <- names(table(cluster))
    sterrs <- matrix(NA, nrow = reps, ncol = length(coef(reg1)))
    for (i in 1:reps) {
        index <- sample(1:length(clusters), length(clusters), replace = TRUE)
        aa <- clusters[index]
        bb <- table(aa)
        bootdat <- NULL
        for (j in 1:max(bb)) {
            cc <- data[cluster %in% names(bb[bb %in% j]),]
            for (k in 1:j) {
                bootdat <- rbind(bootdat, cc)
            }
        }
        sterrs[i,] <- coef(lm(formula, bootdat))
    }

    estimate = coef(reg1)
    stderr = apply(sterrs, 2, sd)
    tval = estimate / stderr;
    pval = round(2 * pt( - abs(tval), df = nrow(data) - num_var), digits = 3)

    val <- cbind(estimate, stderr, pval)
    colnames(val) <- c("Estimate", "Std. Error", "PValue")

    return(val)
}

cdf1_cbse_p1 <- clusbootreg(formula = concede1 ~ treatment, data = games, cluster = games$p1_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P1 Cluster)
cdf1_cbse_p2 <- clusbootreg(formula = concede1 ~ treatment, data = games, cluster = games$p2_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P2 Cluster)

cdf2_cbse_p1 <- clusbootreg(formula = concede2 ~ treatment, data = games, cluster = games$p1_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P1 Cluster)
cdf2_cbse_p2 <- clusbootreg(formula = concede2 ~ treatment, data = games, cluster = games$p2_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P2 Cluster)

cdf3_cbse_p1 <- clusbootreg(formula = concede3 ~ treatment, data = games, cluster = games$p1_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P1 Cluster)
cdf3_cbse_p2 <- clusbootreg(formula = concede3 ~ treatment, data = games, cluster = games$p2_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P2 Cluster)

cdf4_cbse_p1 <- clusbootreg(formula = concede4 ~ treatment, data = games, cluster = games$p1_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P1 Cluster)
cdf4_cbse_p2 <- clusbootreg(formula = concede4 ~ treatment, data = games, cluster = games$p2_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P2 Cluster)

cdf5_cbse_p1 <- clusbootreg(formula = concede5 ~ treatment, data = games, cluster = games$p1_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P1 Cluster)
cdf5_cbse_p2 <- clusbootreg(formula = concede5 ~ treatment, data = games, cluster = games$p2_user_id, 
							num_var = 2, reps = 1000) # Clustered Bootstrapped SE (P2 Cluster)

# --------------------------------------------------------------------------------- #
#                            Other Recodings of Covariates                          #
# --------------------------------------------------------------------------------- #
# Gender 
games$gender = ifelse(games$p1_gender == 0 & games$p2_gender == 0, "1. Male-Male", "")
games$gender = ifelse(games$p1_gender == 1 & games$p2_gender == 0, "3. Female-Female", games$gender)
games$gender = ifelse(games$gender == "", "2. Male-Female", games$gender)

# Risk Aversion
games$risk_preferences = ifelse(games$p1_risk_aversion > 50 & games$p2_risk_aversion > 50, 
								"3. Acceptant-Acceptant", NA)
games$risk_preferences = ifelse(games$p1_risk_aversion == 50 & games$p2_risk_aversion == 50, 
								"1. Neutral-Neutral", games$risk_preferences)
games$risk_preferences = ifelse(games$p1_risk_aversion < 50 & games$p2_risk_aversion < 50, 
								"5. Averse-Averse", games$risk_preferences)
games$risk_preferences = ifelse((games$p1_risk_aversion > 50 & games$p2_risk_aversion == 50) | 
								(games$p1_risk_aversion == 50 & games$p2_risk_aversion > 50), 
								"2. Neutral-Acceptant", games$risk_preferences)
games$risk_preferences = ifelse((games$p1_risk_aversion < 50 & games$p2_risk_aversion == 50) | 
								(games$p1_risk_aversion == 50 & games$p2_risk_aversion < 50), 
								"4. Neutral-Averse", games$risk_preferences)
games$risk_preferences = ifelse((games$p1_risk_aversion > 50 & games$p2_risk_aversion < 50) | 
								(games$p1_risk_aversion < 50 & games$p2_risk_aversion > 50), 
								"6. Averse-Acceptant", games$risk_preferences)

# Time Preferences
games$time_preferences = ifelse(games$p1_time_preferences == "Short" & games$p2_time_preferences == "Short", 
								"1. Short-Short", NA)
games$time_preferences = ifelse(games$p1_time_preferences == "Medium" & games$p2_time_preferences == "Medium", 
								"3. Medium-Medium", games$time_preferences)
games$time_preferences = ifelse(games$p1_time_preferences == "Long" & games$p2_time_preferences == "Long", 
								"5. Long-Long", games$time_preferences)
games$time_preferences = ifelse((games$p1_time_preferences == "Short" & games$p2_time_preferences == "Medium") | 
								(games$p1_time_preferences == "Medium" & games$p2_time_preferences == "Short"), 
								"2. Medium_Short", games$time_preferences)
games$time_preferences = ifelse((games$p1_time_preferences == "Short" & games$p2_time_preferences == "Long") | 
								(games$p1_time_preferences == "Long" & games$p2_time_preferences == "Short"), 
								"5. Long-Short", games$time_preferences)
games$time_preferences = ifelse((games$p1_time_preferences == "Medium" & games$p2_time_preferences == "Long") | 
								(games$p1_time_preferences == "Long" & games$p2_time_preferences == "Medium"), 
								"4. Medium-Long", games$time_preferences)

# Altruism 
games$p1_altruism = ifelse(games$p1_share_amount == 0, "None", ifelse(games$p1_share_amount < 33, "Low", 
							ifelse(games$p1_share_amount > 33 & games$p1_share_amount < 67, "Medium", "High")))
games$p2_altruism = ifelse(games$p2_share_amount == 0, "None", ifelse(games$p2_share_amount < 33, "Low", 
							ifelse(games$p2_share_amount > 33 & games$p2_share_amount < 67, "Medium", "High")))

games$altruism = ifelse(games$p1_altruism == "None" & games$p2_altruism == "None", "01. None-None", 0)
games$altruism = ifelse((games$p1_altruism == "None" & games$p2_altruism == "Low") | 
						(games$p1_altruism == "Low" & games$p2_altruism == "None"), "02. None-Low", games$altruism)
games$altruism = ifelse((games$p1_altruism == "None" & games$p2_altruism == "Medium") | 
						(games$p1_altruism == "Medium" & games$p2_altruism == "None"), "03. None-Medium", games$altruism)
games$altruism = ifelse((games$p1_altruism == "None" & games$p2_altruism == "High") | 
						(games$p1_altruism == "High" & games$p2_altruism == "None"), "04. None-High", games$altruism)
games$altruism = ifelse(games$p1_altruism == "Low" & games$p2_altruism == "Low", "05. Low-Low", games$altruism)
games$altruism = ifelse((games$p1_altruism == "Low" & games$p2_altruism == "High") | 
						(games$p1_altruism == "High" & games$p2_altruism == "Low"), "07. Low-High", games$altruism)
games$altruism = ifelse((games$p1_altruism == "Low" & games$p2_altruism == "Medium") | 
						(games$p1_altruism == "Medium" & games$p2_altruism == "Low"), "06. Low-Medium", games$altruism)
games$altruism = ifelse(games$p1_altruism == "Medium" & games$p2_altruism == "Medium", 
						"08. Medium-Medium", games$altruism)
games$altruism = ifelse((games$p1_altruism == "High" & games$p2_altruism == "Medium") | 
						(games$p1_altruism == "Medium" & games$p2_altruism == "High"), "09. Medium-High", games$altruism)
games$altruism = ifelse(games$p1_altruism == "High" & games$p2_altruism == "High", "10. High-High", games$altruism)

# Economics Training
games$economics_training = ifelse(games$p1_has_training == 0 & games$p2_has_training == 0, "1. No-No", 
							ifelse(games$p1_has_training == 1 & games$p2_has_training == 1, "3. Yes-Yes", "2. No-Yes"))

# Age
games$age = ifelse(games$p1_age < 25 & games$p2_age < 25, "1. Under 25 - Under 25", 
				ifelse(games$p1_age >= 25 & games$p2_age >= 25, "3. 25 and Older - 25 and Older", "2. Under 25 - 25 and Older"))

# Comprehension
games$comprehension = ifelse(games$p1_quiz_score < 4 & games$p2_quiz_score < 4, "1. Low-Low", 
						ifelse(games$p1_quiz_score >= 4 & games$p2_quiz_score >= 4, "3. High-High", "2. Low-High"))

# --------------------------------------------------------------------------------- #
#                    OLS w/ User FEs &  Wild Bootstrapped SEs                       #
# --------------------------------------------------------------------------------- #
# Create User ID FEs for games data 
user_ids <- unique(strategies$user_id)
user_ids <- as.factor(user_ids)

games$u298 <- ifelse(games$p1_user_id == 298 | games$p2_user_id == 298, 1, 0)
games$u294 <- ifelse(games$p1_user_id == 294 | games$p2_user_id == 294, 1, 0)
games$u302 <- ifelse(games$p1_user_id == 302 | games$p2_user_id == 302, 1, 0)
games$u289 <- ifelse(games$p1_user_id == 289 | games$p2_user_id == 289, 1, 0)
games$u295 <- ifelse(games$p1_user_id == 295 | games$p2_user_id == 295, 1, 0)
games$u300 <- ifelse(games$p1_user_id == 300 | games$p2_user_id == 300, 1, 0)
games$u301 <- ifelse(games$p1_user_id == 301 | games$p2_user_id == 301, 1, 0)
games$u290 <- ifelse(games$p1_user_id == 290 | games$p2_user_id == 290, 1, 0)
games$u292 <- ifelse(games$p1_user_id == 292 | games$p2_user_id == 292, 1, 0)
games$u296 <- ifelse(games$p1_user_id == 296 | games$p2_user_id == 296, 1, 0)
games$u291 <- ifelse(games$p1_user_id == 291 | games$p2_user_id == 291, 1, 0)
games$u297 <- ifelse(games$p1_user_id == 297 | games$p2_user_id == 297, 1, 0)
games$u293 <- ifelse(games$p1_user_id == 293 | games$p2_user_id == 293, 1, 0)
games$u299 <- ifelse(games$p1_user_id == 299 | games$p2_user_id == 299, 1, 0)
games$u349 <- ifelse(games$p1_user_id == 349 | games$p2_user_id == 349, 1, 0)
games$u352 <- ifelse(games$p1_user_id == 352 | games$p2_user_id == 352, 1, 0)
games$u358 <- ifelse(games$p1_user_id == 358 | games$p2_user_id == 358, 1, 0)
games$u348 <- ifelse(games$p1_user_id == 348 | games$p2_user_id == 348, 1, 0)
games$u357 <- ifelse(games$p1_user_id == 357 | games$p2_user_id == 357, 1, 0)
games$u353 <- ifelse(games$p1_user_id == 353 | games$p2_user_id == 353, 1, 0)
games$u347 <- ifelse(games$p1_user_id == 347 | games$p2_user_id == 347, 1, 0)
games$u354 <- ifelse(games$p1_user_id == 354 | games$p2_user_id == 354, 1, 0)
games$u356 <- ifelse(games$p1_user_id == 356 | games$p2_user_id == 356, 1, 0)
games$u351 <- ifelse(games$p1_user_id == 351 | games$p2_user_id == 351, 1, 0)
games$u350 <- ifelse(games$p1_user_id == 350 | games$p2_user_id == 350, 1, 0)
games$u355 <- ifelse(games$p1_user_id == 355 | games$p2_user_id == 355, 1, 0)
games$u376 <- ifelse(games$p1_user_id == 376 | games$p2_user_id == 376, 1, 0)
games$u378 <- ifelse(games$p1_user_id == 378 | games$p2_user_id == 378, 1, 0)
games$u380 <- ifelse(games$p1_user_id == 380 | games$p2_user_id == 380, 1, 0)
games$u383 <- ifelse(games$p1_user_id == 383 | games$p2_user_id == 383, 1, 0)
games$u375 <- ifelse(games$p1_user_id == 375 | games$p2_user_id == 375, 1, 0)
games$u373 <- ifelse(games$p1_user_id == 373 | games$p2_user_id == 373, 1, 0)
games$u384 <- ifelse(games$p1_user_id == 384 | games$p2_user_id == 384, 1, 0)
games$u381 <- ifelse(games$p1_user_id == 381 | games$p2_user_id == 381, 1, 0)
games$u379 <- ifelse(games$p1_user_id == 379 | games$p2_user_id == 379, 1, 0)
games$u374 <- ifelse(games$p1_user_id == 374 | games$p2_user_id == 374, 1, 0)
games$u377 <- ifelse(games$p1_user_id == 377 | games$p2_user_id == 377, 1, 0)
games$u382 <- ifelse(games$p1_user_id == 382 | games$p2_user_id == 382, 1, 0)
games$u467 <- ifelse(games$p1_user_id == 467 | games$p2_user_id == 467, 1, 0)
games$u462 <- ifelse(games$p1_user_id == 462 | games$p2_user_id == 462, 1, 0)
games$u469 <- ifelse(games$p1_user_id == 469 | games$p2_user_id == 469, 1, 0)
games$u470 <- ifelse(games$p1_user_id == 470 | games$p2_user_id == 470, 1, 0)
games$u465 <- ifelse(games$p1_user_id == 465 | games$p2_user_id == 465, 1, 0)
games$u463 <- ifelse(games$p1_user_id == 463 | games$p2_user_id == 463, 1, 0)
games$u461 <- ifelse(games$p1_user_id == 461 | games$p2_user_id == 461, 1, 0)
games$u468 <- ifelse(games$p1_user_id == 468 | games$p2_user_id == 468, 1, 0)
games$u466 <- ifelse(games$p1_user_id == 466 | games$p2_user_id == 466, 1, 0)
games$u464 <- ifelse(games$p1_user_id == 464 | games$p2_user_id == 464, 1, 0)

# Run OLS Regressions w/ Wild Bootstrapped Standard Errors (Clustered on P1 & P2 User ID)
# Concede by Round 1
cdf_fe1 <- lm(concede1 ~ treatment +
                u294 + u302 + u289 + u295 + u300 + u301 + u290 + u292 + u296 + 
                u291 + u297 + u293 + u299 + u349 + u352 + u358 + u348 + u357 +
                u353 + u347 + u354 + u356 + u351 + u350 + u355 + u376 + u378 + 
                u380 + u383 + u375 + u373 + u384 + u381 + u379 + u374 + u377 + u382 +
                u467 + u462 + u469 + u470 + u465 + u463 + u461 + u468 + u466 + u464, data = games)
set.seed(89302)
cdf_fe1_cbse <- cluster.boot(cdf_fe1, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf_fe1_se <- sqrt(diag(cdf_fe1_cbse))
coeftest(cdf_fe1, cdf_fe1_cbse)

# Concede by Round 2 
cdf_fe2 <- lm(concede2 ~ treatment +
                u294 + u302 + u289 + u295 + u300 + u301 + u290 + u292 + u296 + 
                u291 + u297 + u293 + u299 + u349 + u352 + u358 + u348 + u357 +
                u353 + u347 + u354 + u356 + u351 + u350 + u355 + u376 + u378 + 
                u380 + u383 + u375 + u373 + u384 + u381 + u379 + u374 + u377 + u382 +
                u467 + u462 + u469 + u470 + u465 + u463 + u461 + u468 + u466 + u464, data = games)
set.seed(89302)
cdf_fe2_cbse <- cluster.boot(cdf_fe2, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf_fe2_se <- sqrt(diag(cdf_fe2_cbse))
coeftest(cdf_fe2, cdf_fe2_cbse)

# Concede by Round 3
cdf_fe3 <- lm(concede3 ~ treatment +
                u294 + u302 + u289 + u295 + u300 + u301 + u290 + u292 + u296 + 
                u291 + u297 + u293 + u299 + u349 + u352 + u358 + u348 + u357 +
                u353 + u347 + u354 + u356 + u351 + u350 + u355 + u376 + u378 + 
                u380 + u383 + u375 + u373 + u384 + u381 + u379 + u374 + u377 + u382 +
                u467 + u462 + u469 + u470 + u465 + u463 + u461 + u468 + u466 + u464, data = games)
set.seed(89302)
cdf_fe3_cbse <- cluster.boot(cdf_fe3, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf_fe3_se <- sqrt(diag(cdf_fe3_cbse))
coeftest(cdf_fe3, cdf_fe3_cbse)

# Concede by Round 4
cdf_fe4 <- lm(concede4 ~ treatment +
              u294 + u302 + u289 + u295 + u300 + u301 + u290 + u292 + u296 + 
              u291 + u297 + u293 + u299 + u349 + u352 + u358 + u348 + u357 +
              u353 + u347 + u354 + u356 + u351 + u350 + u355 + u376 + u378 + 
              u380 + u383 + u375 + u373 + u384 + u381 + u379 + u374 + u377 + u382 +
              u467 + u462 + u469 + u470 + u465 + u463 + u461 + u468 + u466 + u464, data = games)
set.seed(89302)
cdf_fe4_cbse <- cluster.boot(cdf_fe4, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf_fe4_se <- sqrt(diag(cdf_fe4_cbse))
coeftest(cdf_fe4, cdf_fe4_cbse)

# Concede by Round 5
cdf_fe5 <- lm(concede5 ~ treatment +
                u294 + u302 + u289 + u295 + u300 + u301 + u290 + u292 + u296 + 
                u291 + u297 + u293 + u299 + u349 + u352 + u358 + u348 + u357 +
                u353 + u347 + u354 + u356 + u351 + u350 + u355 + u376 + u378 + 
                u380 + u383 + u375 + u373 + u384 + u381 + u379 + u374 + u377 + 
                u382 + u467 + u462 + u469 + u470 + u465 + u463 + u461 + u468 + 
                u466 + u464, data = games)
set.seed(89302)
cdf_fe5_cbse <- cluster.boot(cdf_fe5, cbind(games$p1_user_id, games$p2_user_id), boot_type = "wild", R = 1000)
cdf_fe5_se <- sqrt(diag(cdf_fe5_cbse))
coeftest(cdf_fe5, cdf_fe5_cbse)

stargazer(cdf_fe1, cdf_fe2, cdf_fe3, cdf_fe4, cdf_fe5,
        se = list(cdf_fe1_se, cdf_fe2_se, cdf_fe3_se, cdf_fe4_se, cdf_fe5_se),
        type = "html", out = "OLS FE Games Results.htm")

