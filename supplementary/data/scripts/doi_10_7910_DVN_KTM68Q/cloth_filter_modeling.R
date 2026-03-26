#--------------------------------------------------------------
#- Filename: cloth_filter_modeling.R
#- Author: natalie mastin
#- Date: 6/5/24
#- Description: R script for modeling, permutation test, and figures for cloth filter backwash dataset
#- Figures Created: bw_perc_use.pdf, bw_perc_diff.pdf, fstat_perc.pdf
#--------------------------------------------------------------

#-----------------------------------------------------------------------------------
#- [0.1] load in data and libraries --------------------------------------------------
#-----------------------------------------------------------------------------------
library(viridis)
library(tidyverse)
library("caret")
library(GGally)
library("randomForest")
h = 4.5; w = 8.5 #- base height and width for figures

#- load dataset and remove event 1
load("aqua_backwash.rda")
aqua_backwash <- aqua_backwash[which(aqua_backwash$event != 1),]
#- generate percentage of backwash water used
aqua_backwash$bw_perc_used <- aqua_backwash$bw_vol_permin / aqua_backwash$mean_flow


#-----------------------------------------------------------------------------------
#- [0.2] helper functions ------------------------------------------------------------
#-----------------------------------------------------------------------------------
faov_sim <- function(data, inda, a, nobs, boot_df = FALSE) {
  
  y0ddt <- rowMeans(data[,inda == "coag"], na.rm = T)
  y1ddt <- rowMeans(data[,inda == "nocoag"], na.rm = T)
  yijdt <- cbind(y0ddt, y1ddt)
  ydddt <- rowMeans(data, na.rm = T)
  
  #- degrees of freedom
  qR <- a - 1
  qE <- a*(nobs - 1)
  
  #- [Luke Edits Start] --------------------------------------------------
  # SSRt <- nobs * a * rowSums((yijdt - ydddt)^2)
  SSRt <- nobs * rowSums((yijdt - ydddt)^2, na.rm = T) #- I removed the a
  #- [Luke Edits End] --------------------------------------------------
  
  SSEt <- rowSums((data - yijdt[, rep(1:a, each = nobs)])^2, na.rm = T)
  #- [B] build functional effects --------------------------------------------------
  
  #- mu
  muts <- matrix(ydddt, ncol = 1)
  colnames(muts) <- "mu"
  
  #- alpha_{i}(t); i = 1, 2
  alphats <- yijdt - ydddt
  colnames(alphats) <- c("alpha1t", "alpha2t")
  #- effects frame for output
  effects_frame <- as.data.frame(cbind(muts, alphats))
  
  stat_frame <- data.frame(SSR = SSRt, 
                           SSE = SSEt, 
                           dfR = qR, 
                           dfE = qE)
  
  conc_fac <- factor(inda, levels = c("coag", "nocoag"))
  alpha_indx <- as.numeric(conc_fac)
  
  mut_frame <- muts[, rep(1, times = nobs*a)]
  alphat_frame <- alphats[, alpha_indx]
  etat_frame <- mut_frame + alphat_frame
  nut_frame <- data - etat_frame
  
  if (boot_df) { #- keep the old list for the previous sections, make the data structure smaller for boots
    cbind(effects_frame, stat_frame)
  } else {
    list(effx = effects_frame, 
         ss = stat_frame,
         mu = mut_frame, 
         alpha = alphat_frame,
         eta = etat_frame,
         nu = nut_frame, 
         data = data)
  }}

#-----------------------------------------------------------------------------------
#- [1] percentage of water used for backwash by coag/no coag -----------------------
#-----------------------------------------------------------------------------------

#- pivot wider
wide <- pivot_wider(aqua_backwash[c(1,2,3,11)], names_from = event, values_from = bw_perc_used)
percs <- wide[3:13]

#- find the overall mean and the mean for coag and no coag groups
overall_mean <- apply(percs, 1, mean, na.rm=T)
coag_f <- percs[c(1,2,6,9,11)]
coag_t <- percs[c(3,4,5,7,8,10)]
mean_coag_f <- apply(coag_f, 1, mean, na.rm=T)
mean_coag_t <- apply(coag_t, 1, mean, na.rm=T)
#- calculate difference in means between coag and no coag groups
obs_mean_diff <- mean_coag_t - mean_coag_f

#- colors for each group
cols <- alpha(c("darkcyan", "coral2"), alpha = .7)
colss = c(cols[1], cols[1], cols[2], cols[2], cols[2], cols[1],
          cols[2], cols[2], cols[1], cols[2], cols[1])

#- generate figure for percentage of total water used in backwash for each group
pdf("/Users/LuisdeBonoPaula/Desktop/bw_perc_use.pdf", height = 7, width = 10)
matplot(wide$hrs, percs, type="b", col = colss, lty = 2, pch = 20, cex = .6,
        xlab = "Hours Since Event", ylab = "Backwash / Mean Influent Flow (%)",
        main = expression("Backwash as a Proportion of Flow"), 
        cex.main = 1.5, cex.lab = 1.25, xaxt="n")
axis(1, 0:16)
lines(wide$hrs, mean_coag_f, col=cols[1], lwd=4)
lines(wide$hrs, mean_coag_t, col=cols[2], lwd=4)
lines(wide$hrs, overall_mean, col=rgb(0,0,0,0.7), lwd=4)
legend("topright", 
       legend = c("Coag Events: 4, 5, 6, 8, 9, 11", "No Coag Events: 2, 3, 7, 10, 12", "Avg Coag Events", "Avg No Coag Events", "Avg All Events"),
       col = c("coral2", "darkcyan","coral2", "darkcyan", "black"), 
       lty = c(2,2,1,1,1), lwd = c(1,1,4,4,4), bty = "n", cex = 1.05)
dev.off()



#- find the overall mean and the mean for coag and no coag groups
overall_var <- apply(percs, 1, sd, na.rm=T)
var_coag_t <- apply(coag_t, 1, sd, na.rm=T)
var_coag_f <- apply(coag_f, 1, sd, na.rm=T)
#- calculate difference in means between coag and no coag groups
obs_var_diff <- var_coag_t - var_coag_f


#- generate figure for percentage of total water used in backwash for each group
pdf("/Users/LuisdeBonoPaula/Desktop/bw_perc_var.pdf", height = 7, width = 10)
matplot(wide$hrs, overall_var, type="b", col = "white", lty = 2, pch = 20, cex = .6,
        xlab = "Hours Since Event", ylab = "Std. Dev. of Backwash (gpm) / Mean Flow (gpm)",
        main = expression("Standard Deviation of Backwash as a Percentage of Flow"), 
        cex.main = 1.5, cex.lab = 1.25, ylim = c(0,0.22), xaxt="n")
lines(wide$hrs, var_coag_f, col="darkcyan", lwd=4)
lines(wide$hrs, var_coag_t, col="coral2", lwd=4)
axis(1, 0:16)
# lines(wide$hrs, overall_var, col=rgb(0,0,0,0.7), lwd=4)
legend("topright", 
       legend = c("Std. Dev. of Coag Events", "Std. Dev. of No Coag Events"),
       col = c("coral2", "darkcyan"), 
       lty = c(1,1), lwd = c(4,4), bty = "n", cex = 1.05)
dev.off()


#-----------------------------------------------------------------------------------
#- [2] permutation test ------------------------------------------------------------
#-----------------------------------------------------------------------------------
#- set up permutation test
nperm = 1000
perm_diffs <- matrix(0, nrow = nperm, ncol = 24)
for (perm in 1:nperm) {
  #- shuffle all 11 events and find difference in means of two groups
  #- (for Figure 6 since differences don't need to be balanced)
  perm_ndx <- sample(1:11, 11, replace = F)
  perm_data <- percs[, perm_ndx]
  perm_coag_f <- perm_data[c(1,2,6,9,11)]
  perm_coag_t <- perm_data[c(3,4,5,7,8,10)]
  perm_mean_coag_f <- apply(perm_coag_f, 1, mean, na.rm=T)
  perm_mean_coag_t <- apply(perm_coag_t, 1, mean, na.rm=T)
  perm_diffs[perm,] <- perm_mean_coag_t - perm_mean_coag_f
  if (perm %% 100 == 0 ) print(perm)
}

#- indicator coag versus no coag
ind_list <- list(ind1 = c("nocoag", "nocoag", "coag", "coag", "nocoag",
                           "coag", "coag", "nocoag", "coag", "nocoag"),
                 ind2 = c("nocoag", "nocoag", "coag", "coag", "nocoag",
                           "coag", "coag", "nocoag", "coag", "nocoag"),
                 ind3 = c("nocoag", "nocoag", "coag", "coag", "nocoag",
                           "coag", "coag", "nocoag", "coag", "nocoag"),
                 ind4 = c("nocoag", "nocoag", "coag", "coag", "coag", "nocoag",
                           "coag", "nocoag", "coag", "nocoag"),
                 ind5 = c("nocoag", "nocoag", "coag", "coag", "coag", "nocoag",
                           "coag", "nocoag", "coag", "nocoag"),
                 ind6 = c("nocoag", "nocoag", "coag", "coag", "coag", "nocoag",
                           "coag", "coag", "nocoag", "nocoag"))
perm_list <- list(perm_array1 = array(NA, dim = c(nperm, 24, 8)),
                  perm_array2 = array(NA, dim = c(nperm, 24, 8)),
                  perm_array3 = array(NA, dim = c(nperm, 24, 8)),
                  perm_array4 = array(NA, dim = c(nperm, 24, 8)),
                  perm_array5 = array(NA, dim = c(nperm, 24, 8)),
                  perm_array6 = array(NA, dim = c(nperm, 24, 8)))

set.seed(1998)
for (e in 1:6){
for (perm in 1:nperm) {
  #- shuffle all 11 events and find difference in means of two groups
  #- (for Figure 6 since differences don't need to be balanced)
  perm_ndx <- sample(1:11, 11, replace = F)
  perm_data <- percs[, perm_ndx]
  
  #- for fanova, we need a balanced design, 
  #- so select one of the coag events to toss
  events <- c(3,4,5,7,8,10)
  
  #- fanova on balanced data
  perm_frame <- faov_sim(perm_data[,-events[[e]]], 
                         inda = ind_list[[e]], 
                         a = 2, 
                         nobs = 5, boot_df = TRUE)
  permp     <- rep(perm, times = 24) #- column to identify which perm you are on
  
  output <- as.matrix(cbind(perm_rep = permp, perm_frame))
  
  perm_list[[e]][perm, , ] <-  output
  
  if (perm %% 100 == 0 ) print(perm)
}
}
#- calculate mean and percentiles for permuted differences
mean_perm_diff <- apply(perm_diffs, 2, mean)
lquant_perm_diff <- apply(perm_diffs, 2, function(x) quantile(x, .025))
hquant_perm_diff <- apply(perm_diffs, 2, function(x) quantile(x, .975))


#- generate figure that illustrates permutations (Figure 6)
pdf("/Users/LuisdeBonoPaula/Desktop/bw_perc_diff.pdf", height = 7, width = 10)
matplot(wide$hrs, t(perm_diffs), type="l", col = rgb(0,0,0,0.1), lty=1,
        xlab = "Hours Since Event", ylab = "Diff in Backwash Percentage / Minute",
        main = expression("Difference in Backwash Percentage Due to Coagulation"), 
        cex.main = 1.5, cex.lab = 1.05, xaxt="n")
lines(wide$hrs, mean_perm_diff, col= 5, lwd = 3, lty = 1)
lines(wide$hrs, lquant_perm_diff, col= 5, lwd = 3, lty = 5)
lines(wide$hrs, hquant_perm_diff, col= 5, lwd = 3, lty = 5)
lines(wide$hrs, obs_mean_diff, col= 6, lwd = 3, lty = 1)
axis(1, 0:16)
legend("topright", 
       legend = c("Permuted Difference in Means", "Mean of Permutations", "Quantiles of Permutations", "Observed Difference in Means"),
       col = c(1, 5, 5, 6), 
       lty = c(1, 1, 2, 1), lwd = c(1,3, 3, 3), bty = "n", cex = 0.75)
dev.off()

#-----------------------------------------------------------------------------------
#- [3] FANOVA for difference in mean for coag/no coag groups -----------------------
#-----------------------------------------------------------------------------------

#- fanova for each possible set of events with a coag event removed
faov_dat1 <- faov_sim(percs[,-3], inda = ind_list[[1]], a = 2, nobs = 5)
faov_dat2 <- faov_sim(percs[,-4], inda = ind_list[[2]], a = 2, nobs = 5)
faov_dat3 <- faov_sim(percs[,-5], inda = ind_list[[3]], a = 2, nobs = 5)
faov_dat4 <- faov_sim(percs[,-7], inda = ind_list[[4]], a = 2, nobs = 5)
faov_dat5 <- faov_sim(percs[,-8], inda = ind_list[[5]], a = 2, nobs = 5)
faov_dat6 <- faov_sim(percs[,-10], inda = ind_list[[6]], a = 2, nobs = 5)

#- observed functional F statistics
Ft_stat <- list(Ft_stat1 = (faov_dat1$ss$SSR/(faov_dat1$ss$dfR))/(faov_dat1$ss$SSE/faov_dat1$ss$dfE),
                Ft_stat2 = (faov_dat2$ss$SSR/(faov_dat2$ss$dfR))/(faov_dat2$ss$SSE/faov_dat2$ss$dfE),
                Ft_stat3 = (faov_dat3$ss$SSR/(faov_dat3$ss$dfR))/(faov_dat3$ss$SSE/faov_dat3$ss$dfE),
                Ft_stat4 = (faov_dat4$ss$SSR/(faov_dat4$ss$dfR))/(faov_dat4$ss$SSE/faov_dat4$ss$dfE),
                Ft_stat5 = (faov_dat5$ss$SSR/(faov_dat5$ss$dfR))/(faov_dat5$ss$SSE/faov_dat5$ss$dfE),
                Ft_stat6 = (faov_dat6$ss$SSR/(faov_dat6$ss$dfR))/(faov_dat6$ss$SSE/faov_dat6$ss$dfE)
)

#- set delta for integrals
delta = 23
#- observed F-type statistic
fft_obs <- c((sum(delta * faov_dat1$ss$SSR)/faov_dat1$ss$dfR[1]) / (sum(delta * faov_dat1$ss$SSE)/faov_dat1$ss$dfE[1]),
                (sum(delta * faov_dat2$ss$SSR)/faov_dat2$ss$dfR[1]) / (sum(delta * faov_dat2$ss$SSE)/faov_dat2$ss$dfE[1]),
                (sum(delta * faov_dat3$ss$SSR)/faov_dat3$ss$dfR[1]) / (sum(delta * faov_dat3$ss$SSE)/faov_dat3$ss$dfE[1]),
                (sum(delta * faov_dat4$ss$SSR)/faov_dat4$ss$dfR[1]) / (sum(delta * faov_dat4$ss$SSE)/faov_dat4$ss$dfE[1]),
                (sum(delta * faov_dat5$ss$SSR)/faov_dat5$ss$dfR[1]) / (sum(delta * faov_dat5$ss$SSE)/faov_dat5$ss$dfE[1]),
                (sum(delta * faov_dat6$ss$SSR)/faov_dat6$ss$dfR[1]) / (sum(delta * faov_dat6$ss$SSE)/faov_dat6$ss$dfE[1])
)

#- find critical value from permutations
FA_perm <- list()
FA_np_95 <- list()
ftA_perm_quan <- vector(length=6, mode="numeric")
ftA_pval <- vector(length=6, mode="numeric")
alpha = 0.05
for (e in 1:6) {
  dimnames(perm_list[[e]]) <- list(nrep_mat = 1:nperm,
                             grid_t = 1:24,
                             variable = c("perm", colnames(perm_frame)))
  FA_perm[[e]] <- (perm_list[[e]][, , "SSR"] / perm_list[[e]][, , "dfR"]) / ( (perm_list[[e]][, , "SSE"])  / ( perm_list[[e]][, , "dfE"]) )
  #- pointwise 95th percentile of permutations
  FA_np_95[[e]] <- apply(FA_perm[[e]], 2, function(x) quantile(x, .95))
  #- find F_{FT} critical value and p-value
  SSE_perm <- perm_list[[e]][, , "SSE"] / perm_list[[e]][, , "dfE"]
  SSE_int <- apply(SSE_perm[, wide$hrs], 1, function(x) sum(delta * x))
  SSA_perm <- perm_list[[e]][, , "SSR"] /  perm_list[[e]][, , "dfR"]
  SSA_int  <- apply(SSA_perm[, wide$hrs], 1, function(x) sum(delta * x))
  
  ftA_perms <- SSA_int / SSE_int #- FT statistic permutations
  (ftA_perm_quan[[e]] <- quantile(ftA_perms, 1-alpha)) #- FT critical value
  (ftA_pval[[e]] <- sum(fft_obs[[e]] < ftA_perms) / length(ftA_perms)) #- FT p-value
}

mean(fft_obs)
mean(ftA_perm_quan)
mean(ftA_pval)



newcols <- alpha(viridis(6), alpha = .8)
#- Generate figure with functional F-statistics and pointwise 95th percentiles of permutations for each case
pdf(file="/Users/LuisdeBonoPaula/Desktop/fstat_perc.pdf", width=10, height=7)
par(mfrow = c(1,1), mai = c(0.8, 0.8, 0.4, 0.4))
plot(wide$hrs, Ft_stat[[1]], type = "l", 
     ylab = "", 
     xlab = "Hours Since Event",
     main = expression("Backwash Percentage Functional F-Statistics"),
     ylim = c(0,6),
     cex.main = 1.5,
     cex.lab = 1.05,
     xaxt = "n", lwd = 3,
     col=viridis(6)[1])
abline(h = 0, lty = 1, col = "gray", lwd = 2.5)
lines(wide$hrs, FA_np_95[[1]], lty = 2, col = newcols[1], lwd = 2)
lines(wide$hrs, FA_np_95[[2]], lty = 2, col = newcols[2], lwd = 2)
lines(wide$hrs, FA_np_95[[3]], lty = 2, col = newcols[3], lwd = 2)
lines(wide$hrs, FA_np_95[[4]], lty = 2, col = newcols[4], lwd = 2)
lines(wide$hrs, FA_np_95[[5]], lty = 2, col = newcols[5], lwd = 2)
lines(wide$hrs, FA_np_95[[6]], lty = 2, col = newcols[6], lwd = 2)
lines(wide$hrs, Ft_stat[[1]], lty = 1, col = viridis(6)[1], lwd = 3)
lines(wide$hrs, Ft_stat[[2]], lty = 1, col = viridis(6)[2], lwd = 3)
lines(wide$hrs, Ft_stat[[3]], lty = 1, col = viridis(6)[3], lwd = 3)
lines(wide$hrs, Ft_stat[[4]], lty = 1, col = viridis(6)[4], lwd = 3)
lines(wide$hrs, Ft_stat[[5]], lty = 1, col = viridis(6)[5], lwd = 3)
lines(wide$hrs, Ft_stat[[6]], lty = 1, col = viridis(6)[6], lwd = 3)

legend("topleft", legend = c("Coag Events 4,5,7,8,10", "Coag Events 3,5,7,8,10", "Coag Events 3,4,7,8,10", "Coag Events 3,4,5,8,10", "Coag Events 3,4,5,7,10", "Coag Events 3,4,5,7,8"), lty = c(1), lwd = 2, col = viridis(6), bty = "n")
legend("topright", legend = c("Observed F(t)", "95% perm F"), lty = c(1,2), lwd = c(3,2), col = 1, bty = "n")
mtext(expression(F^obs*(t)), side = 2, line = 2.5, cex = 1.1)
axis(1, 0:16)
dev.off()


#-----------------------------------------------------------------------------------
#- [4] correlation matrix ------------------------------------------------------------
#-----------------------------------------------------------------------------------

ggpairs(aqua_backwash[c(5:9,11)])

#-----------------------------------------------------------------------------------
#- [5] multiple linear regression results ------------------------------------------
#-----------------------------------------------------------------------------------
#- train/test split
set.seed(1)
trainIndex <- createDataPartition(aqua_backwash$bw_perc_used, times = 1, p = 0.75, list = FALSE)
testIndex <- c(1:nrow(aqua_backwash))[-trainIndex]
train = aqua_backwash[trainIndex,]
test = aqua_backwash[testIndex,]


mlr <- lm(bw_perc_used ~ tss_permin + turb_permin + slr_lbs_sf_day + coag + hrs, 
          data = train)
summary(mlr)
plot(resid(mlr), fitted(mlr))
plot(fitted(mlr), train$bw_perc_used)
mlr_preds <- predict(mlr, newdata = test)
(mlr_rmse <- mean((test$bw_perc_used - mlr_preds)^2))

#- polynomial regression
#- form matrix with only wanted variables and some with higher degrees
sds <- apply(as.matrix(aqua_backwash[, c(5, 6, 7)]), 2, sd)
aqua_backwash$slr_lbs_sf_day_sc <- aqua_backwash$slr_lbs_sf_day / sds[3]
aqua_backwash$tss_permin_sc <- aqua_backwash$tss_permin / sds[1]
aqua_backwash$turb_permin_sc <- aqua_backwash$turb_permin / sds[2]
modeldat <- cbind(aqua_backwash[, c(11,3,9)], data.frame(polym(aqua_backwash$slr_lbs_sf_day_sc, aqua_backwash$tss_permin_sc, aqua_backwash$turb_permin_sc, degree = 3, raw = TRUE)))
modeltrain = modeldat[trainIndex,]
modeltest = modeldat[testIndex,]

#- fit
poly_fit <- lm(bw_perc_used ~ ., data = modeltrain)
n <- nrow(modeltrain)
poly_bw  <- step(poly_fit, direction = "backward", k = log(n), trace = 0)
summary(poly_bw)
plot(resid(poly_fit), fitted(poly_fit))
plot(fitted(poly_fit), train$bw_perc_used)
as.formula(
  paste0("y ~ ", round(coefficients(poly_fit)[1],2), "", 
         paste(sprintf(" %+.2f*%s ", 
                       coefficients(poly_fit)[-1],  
                       names(coefficients(poly_fit)[-1])), 
               collapse="")
  )
)

poly_preds <- predict(poly_bw, newdata = modeltest)
(poly_rmse <- mean((modeltest$bw_perc_used - poly_preds)^2))


#- random forest? no R^2
rf <- randomForest(bw_perc_used ~ tss_permin + turb_permin + slr_lbs_sf_day + coag + hrs + event, train, 
                   importance=TRUE, proximity=TRUE)

rf_preds <- predict(rf,newdata=test)
(rf_rmse <- mean((test$bw_perc_used - rf_preds)^2))
VI <-importance(rf)
VI <- data.frame(VI)
varImpPlot(rf, main="Variable Importance")

logit <- glm(bw_perc_used ~ tss_permin + turb_permin + slr_lbs_sf_day + coag + hrs, data = train, family = binomial(link = "logit"))

# Get model summary
summary(logit)
(logit_r2 <- 1-(logit$deviance/logit$null.deviance))
# Get predicted probabilities
predicted_probs <- predict(logit, newdata = test, type = "response")

# Display predicted probabilities
head(predicted_probs)
(l_rmse <- mean((test$bw_perc_used - predicted_probs)^2))
