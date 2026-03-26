######################################################
# Script: Classification /w LSTM Network
# Manuscript: From One to Many: Identifying Issues in CJEU Jurisprudence

# Author: Philipp Schroeder, LMU Munich, and Johan Lindholm, Umea University

###--- INSTRUCTIONS ---###
# This script replicates the analysis presented in Section 5 of the manuscript,
# reproducing Figures 6, 7 and 8 in the main manuscript, as well as Figures 16 and 17 in the manuscript's appendix

library(MASS)
library(texreg)
library(GGally)
library(ggpubr)
library(ggplot2)
library(rstanarm)
library(dplyr)
library(Metrics)

rm(list = ls())

path <- "Your file directory"
path <- "C:/Users/phili/Dropbox/CJEU dataset/Papers/Issue Splitting/Replication material/"

# Read position data
issue_data <- read.csv(paste0(path, "datasets/issue_data_cleaned.csv"), header = TRUE)
names(issue_data)
judgment_data <- read.csv(paste0(path, "datasets/judgment_data_cleaned.csv"), header = TRUE)
names(judgment_data)

# relevel factor variables
issue_data$ms_conflict_cat <- as.factor(issue_data$ms_conflict_cat)
issue_data$ms_conflict_cat <- relevel(issue_data$ms_conflict_cat, ref = "In favor")
judgment_data$ms_conflict_cat <- as.factor(judgment_data$ms_conflict_cat)
judgment_data$ms_conflict_cat <- relevel(judgment_data$ms_conflict_cat, ref = "In favor")

issue_data$ag_conflict_cat <- as.factor(issue_data$ag_conflict_cat)
issue_data$ag_conflict_cat <- relevel(issue_data$ag_conflict_cat, ref = "In favor")
judgment_data$ag_conflict_cat <- as.factor(judgment_data$ag_conflict_cat)
judgment_data$ag_conflict_cat <- relevel(judgment_data$ag_conflict_cat, ref = "In favor")

issue_data$com_conflict_cat <- as.factor(issue_data$com_conflict_cat)
issue_data$com_conflict_cat <- relevel(issue_data$com_conflict_cat, ref = "In favor")
judgment_data$com_conflict_cat <- as.factor(judgment_data$com_conflict_cat)
judgment_data$com_conflict_cat <- relevel(judgment_data$com_conflict_cat, ref = "In favor")

# descriptive statistics for judgment and issue data
setEPS()
postscript("descriptive.eps", width = 8, height = 5) # Figure 7 in main manuscript
par(mfrow = c(1,2))
plot(judgment_data$ms_conflict_cat,
     ylim = c(0,300),
     xlab = "MS Conflict", ylab = "Frequency",
     main = "Judgment-level")
abline(h = seq(0,300,50), lty = 2, lwd = .5, col = "grey")
plot(issue_data$ms_conflict_cat,
     ylim = c(0,300),
     xlab = "MS Conflict", ylab = "Frequency",
     main = "Issue-level")
abline(h = seq(0,300,50), lty = 2, lwd = .5, col = "grey")
dev.off()

# judgment level
summary(judgment_data[,c("judgment_outdegree","ms_conflict","sum_obs","ag_conflict","com_conflict",
                     "treaty","complexity","chamber","common_law")])
sd(judgment_data$judgment_outdegree)
sd(judgment_data$ms_conflict)
sd(judgment_data$sum_obs)
sd(judgment_data$ag_conflict)
sd(judgment_data$com_conflict)
sd(judgment_data$treaty)
sd(judgment_data$complexity)
sd(judgment_data$chamber)
sd(judgment_data$common_law)

# issue level
summary(issue_data[,c("issue_outdegree","ms_conflict","sum_obs","ag_conflict","com_conflict",
                      "treaty","complexity","chamber","common_law")])
sd(issue_data$global_outdegree)
sd(issue_data$global_hubscore)
sd(issue_data$ms_conflict)
sd(issue_data$sum_obs)
sd(issue_data$ag_conflict)
sd(issue_data$com_conflict)
sd(issue_data$treaty)
sd(issue_data$complexity)
sd(issue_data$chamber)
sd(issue_data$common_law)

# nested issues
length(which(table(issue_data$ecli) == 1))
max(table(issue_data$ecli))

### REGRESSION MODELS ###
options(mc.cores = parallel::detectCores())

# issue-level regression /w complete pooling
#names(issue_data)
#fit.1 <- stan_glm.nb(issue_outdegree ~ ms_conflict_cat + ag_conflict_cat + com_conflict_cat + sum_obs + treaty + complexity + chamber + common_law + factor(year),
#                       data = issue_data, chains=4, seed=123, iter=15000, warmup=5000)
#print(fit.1)
#fixef(fit.1)

#BETA.1 <- as.matrix(fit.1)[,1:length(fixef(fit.1))]
#mean.BETA.1 <- apply(BETA.1,2,mean)
#sd.BETA.1 <- apply(BETA.1,2,sd)
#low.BETA.1 <- apply(BETA.1,2,quantile,c(0.025,0.975))[1,] # get 95% HPDs
#high.BETA.1 <- apply(BETA.1,2,quantile,c(0.025,0.975))[2,]

# judgment-level regression /w group-level aggregates
names(judgment_data)
fit.2 <- stan_glm.nb(judgment_outdegree ~ ms_conflict_cat + ag_conflict_cat + com_conflict_cat + sum_obs + treaty + complexity + chamber + common_law + factor(year),
                     data = judgment_data, chains=4, seed=123, iter=15000, warmup=5000)

print(fit.2)
fixef(fit.2)

BETA.2 <- as.matrix(fit.2)[,1:length(fixef(fit.2))]
mean.BETA.2 <- apply(BETA.2,2,mean)
sd.BETA.2 <- apply(BETA.2,2,sd)
low.BETA.2 <- apply(BETA.2,2,quantile,c(0.025,0.975))[1,] # get 95% HPDs
high.BETA.2 <- apply(BETA.2,2,quantile,c(0.025,0.975))[2,]

# multilevel model /w partial pooling
names(issue_data)
fit.3 <- stan_glmer.nb(issue_outdegree ~ ms_conflict_cat + ag_conflict_cat + com_conflict_cat + sum_obs + treaty + complexity + chamber + common_law + factor(year) + (1| ecli),
                     data = issue_data, chains=4, seed=123, iter=15000, warmup=5000)

print(fit.3)
fixef(fit.3)

BETA.3 <- as.matrix(fit.3)[,1:length(fixef(fit.3))]
mean.BETA.3 <- apply(BETA.3,2,mean)
sd.BETA.3 <- apply(BETA.3,2,sd)
low.BETA.3 <- apply(BETA.3,2,quantile,c(0.025,0.975))[1,] # get 95% HPDs
high.BETA.3 <- apply(BETA.3,2,quantile,c(0.025,0.975))[2,]

# plot coefficients from judgment and multi-level regressions
setEPS()
postscript("coefficients.eps", width = 10, height = 7) # Figure 6 in main manuscript
par(mar=c(5,14,3,3))
plot("", axes = FALSE,
     xlim = c(-2,2),
     ylim = c(0.5,11.5),
     xlab = "Posterior means with 95% HPDs",
     ylab = "", pch = 19, cex = 1.5,
     main = "Citation outdegree")
axis(1,at = seq(-2,2,.5))
axis(2,at = seq(11,1,-1), las = 1, tick = TRUE,
     label = c("MS Conflict: Ambivalent",
               "MS Conflict: In conflict",
               "AG Conflict: Ambivalent",
               "AG Conflict: In conflict",
               "Commission Conflict: Ambivalent",
               "Commission Conflict: In conflict",
               "Sum of observations",
               "Treaty",
               "Complexity",
               "Chamber Size",
               "Common Law Origin"))
abline(h = seq(11,1,-1), lty = 2, lwd = .5, col = "grey")
abline(v=0, lty = 2)
# judgment-level /w group aggregates
points(mean.BETA.2[2:12],seq(11.1,1.1,-1), pch = 17)
segments(low.BETA.2[2:12], seq(11.1,1.1,-1), high.BETA.2[2:12], seq(11.1,1.1,-1), lwd = 1) # 95% HPDs
# MLM estimates
segments(low.BETA.3[2:12], seq(10.9,0.9,-1), high.BETA.3[2:12], seq(10.9,0.9,-1), lwd = 1) # 95% HPDs
points(mean.BETA.3[2:12],seq(10.9,0.9,-1), pch = 24, bg = "white")
legend("topright", legend=c("Judgment-level", "MLM estimates"),
       pch = c(17,24), xpd = TRUE, bg = "white", cex = 0.8)
dev.off()


# predict number of citations at judgment-level with estimates from judgment-model and issue-model
phat_judgment <- posterior_predict(fit.2, judgment_data, type = "response", seed = 123)
judgment_data$phat_judgment <- apply(phat_judgment, 2, mean)
#View(judgment_data[,c("judgment_outdegree","phat_judgment")])

phat_issue <- posterior_predict(fit.3, issue_data, type = "response", seed = 123)
issue_data$phat_issue <- apply(phat_issue, 2, mean)
#View(issue_data[,c("issue_outdegree","phat_issue")])

for (i in issue_data$case_celex){
  df <- issue_data[issue_data$case_celex == i,]
  judgment_data$phat_issue[judgment_data$case_celex == i] <- sum(df$phat_issue)
}
#View(judgment_data[,c("judgment_outdegree","phat_judgment","phat_issue")])

rmse(judgment_data$judgment_outdegree, judgment_data$phat_judgment)
rmse(judgment_data$judgment_outdegree, judgment_data$phat_issue)

judgment_level_residuals <- judgment_data$phat_judgment - judgment_data$judgment_outdegree
issue_level_residuals <- judgment_data$phat_issue - judgment_data$judgment_outdegree

quantile(judgment_level_residuals, c(0.025,0.975))
quantile(issue_level_residuals, c(0.025,0.975))

setEPS()
postscript("residuals.eps", width = 10, height = 5) # Figure 8 in main manuscript
par(mfrow = c(1,2), mar=c(5,4,4,2))
plot(density(judgment_level_residuals), xlim = c(-80,40), ylim = c(0,0.08), main = "Residuals with judgment-level predictions")
polygon(density(judgment_level_residuals), col = alpha("lightgrey", 0.85), border = NA)
abline(v = quantile(judgment_level_residuals, c(0.025,0.975))[1], lty = 2)
abline(v = quantile(judgment_level_residuals, c(0.025,0.975))[2], lty = 2)
plot(density(issue_level_residuals), xlim = c(-80,40), ylim = c(0,0.08), main = "Residuals with issue-level predictions")
polygon(density(issue_level_residuals), col = alpha("lightgrey", 0.85), border = NA)
abline(v = quantile(issue_level_residuals, c(0.025,0.975))[1], lty = 2)
abline(v = quantile(issue_level_residuals, c(0.025,0.975))[2], lty = 2)
dev.off()


### ROBUSTNESS CHECKS ###
options(mc.cores = parallel::detectCores())

# issue-level regression /w complete pooling
summary(issue_data$issue_hubscore)
issue_data$issue_hubscore_add <- issue_data$issue_hubscore + .01 # add small value to hubscore to ensure positive values across all observations
summary(judgment_data$judgment_hubscore)
judgment_data$judgment_hubscore_add <- judgment_data$judgment_hubscore + .01

# Plot distributions
judgment.hubscore <- ggplot(judgment_data, aes(x = judgment_hubscore)) +
  geom_histogram(color="black", fill="white", bins = 40) +
  ylab("Frequency") + xlab("Hub Score") + ggtitle("Judgment-level")
issue.hubscore <- ggplot(issue_data, aes(x = issue_hubscore)) +
  geom_histogram(color="black", fill="white", bins = 40) + 
  ylab("Frequency") + xlab("Hub Score") + ggtitle("Issue-level")

judgment.hubrank <- ggplot(judgment_data, aes(x = judgment_hubrank)) +
  geom_histogram(color="black", fill="white", bins = 40) +
  ylim(0,125) + ylab("Frequency") + xlab("Hub Rank")
issue.hubrank <- ggplot(issue_data, aes(x = issue_hubrank)) +
  geom_histogram(color="black", fill="white", bins = 40) + 
  ylim(0,125) + ylab("Frequency") + xlab("Hub Rank")

setEPS()
postscript("descriptive_hub.eps", width = 10, height = 7) # Figure 16 in manuscript appendix
myplot <- ggarrange(judgment.hubscore, issue.hubscore,judgment.hubrank,issue.hubrank,
                    ncol = 2, nrow = 2)
print(myplot)
dev.off()

#--- Hub rank
# issue-level regression /w complete pooling
summary(issue_data$issue_hubrank)
issue_data$issue_hubrank_add <- issue_data$issue_hubrank + .01 # add small value to hubscore to ensure positive values across all observations
summary(judgment_data$judgment_hubrank)
judgment_data$judgment_hubrank_add <- judgment_data$judgment_hubrank + .01

# judgment-level regression /w group-level aggregates
names(judgment_data)
fit.8 <- stan_glm(judgment_hubrank_add ~ ms_conflict_cat + ag_conflict_cat + com_conflict_cat + sum_obs + treaty + complexity + chamber + common_law + factor(year),
                  data = judgment_data, family = Gamma(link = "log"), chains=4, seed=123, iter=15000, warmup=5000)

print(fit.8)
fixef(fit.8)

BETA.8 <- as.matrix(fit.8)[,1:length(fixef(fit.8))]
mean.BETA.8 <- apply(BETA.8,2,mean)
sd.BETA.8 <- apply(BETA.8,2,sd)
low.BETA.8 <- apply(BETA.8,2,quantile,c(0.025,0.975))[1,] # get 95% HPDs
high.BETA.8 <- apply(BETA.8,2,quantile,c(0.025,0.975))[2,]

# multilevel model /w partial pooling
names(issue_data)
fit.9 <- stan_glmer(issue_hubrank_add ~ ms_conflict_cat + ag_conflict_cat + com_conflict_cat + sum_obs + treaty + complexity + chamber + common_law + factor(year) + (1| ecli),
                    data = issue_data, family = Gamma(link = "log"), chains=4, seed=123, iter=15000, warmup=5000)

print(fit.9)
fixef(fit.9)

BETA.9 <- as.matrix(fit.9)[,1:length(fixef(fit.9))]
mean.BETA.9 <- apply(BETA.9,2,mean)
sd.BETA.6 <- apply(BETA.9,2,sd)
low.BETA.9 <- apply(BETA.9,2,quantile,c(0.025,0.975))[1,] # get 95% HPDs
high.BETA.9 <- apply(BETA.9,2,quantile,c(0.025,0.975))[2,]

setEPS()
postscript("coefficients_hub.eps", width = 10, height = 7) # Figure 17 in manuscript appendix
par(mar=c(5,14,3,3))
plot("", axes = FALSE,
     xlim = c(-1,1.5),
     ylim = c(0.5,11.5),
     xlab = "Posterior means with 95% HPDs",
     ylab = "", pch = 19, cex = 1.5,
     main = "Citation Hubrank")
axis(1,at = seq(-1,1.5,.5))
axis(2,at = seq(11,1,-1), las = 1, tick = TRUE,
     label = c("MS Conflict: Ambivalent",
               "MS Conflict: In conflict",
               "AG Conflict: Ambivalent",
               "AG Conflict: In conflict",
               "Commission Conflict: Ambivalent",
               "Commission Conflict: In conflict",
               "Sum of observations",
               "Treaty",
               "Complexity",
               "Chamber Size",
               "Common Law Origin"))
abline(h = seq(11,1,-1), lty = 2, lwd = .5, col = "grey")
abline(v=0, lty = 2)
# judgment-level /w group aggregates
points(mean.BETA.8[2:12],seq(11.1,1.1,-1), pch = 17)
segments(low.BETA.8[2:12], seq(11.1,1.1,-1), high.BETA.8[2:12], seq(11.1,1.1,-1), lwd = 1) # 95% HPDs
# MLM estimates
segments(low.BETA.9[2:12], seq(10.9,0.9,-1), high.BETA.9[2:12], seq(10.9,0.9,-1), lwd = 1) # 95% HPDs
points(mean.BETA.9[2:12],seq(10.9,0.9,-1), pch = 24, bg = "white")
legend("topright", legend=c("Judgment-level", "MLM estimates"),
       pch = c(17,24), xpd = TRUE, bg = "white", cex = 0.8)
dev.off()
