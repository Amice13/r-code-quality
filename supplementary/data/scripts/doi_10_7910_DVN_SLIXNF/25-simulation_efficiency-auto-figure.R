##
## Simulation Study: Load results and create figures
##

# load packages
require(dplyr)
require(tidyr)

# load setups
source("21-simulation_setup.R")

bias <- rmse <- se <- list()
missing <- c()
## load Efficiency simulation
for (i in 1:nrow(sim_grid)) {
  tryCatch({
    tmp <- readRDS(paste("../results/simulation/sim_",i,'.rds', sep = ''))
    bias[[i]] <- apply(tmp, 2, function(x) abs(mean(x)))
    rmse[[i]] <- apply(tmp, 2, function(x) sqrt(mean(x^2)))
    se[[i]]   <- apply(tmp, 2, function(x) sd(x))
  }, error=function(e){
    missing <- c(missing, i)
    cat(paste("ind = ", i, "\n", sep = ""))
    })
}

# fill in NA
for(i in 1:nrow(sim_grid)){
  if(is.null(bias[[i]])) bias[[i]] <- rep(NA, 7)
  if(is.null(rmse[[i]])) rmse[[i]] <- rep(NA, 7)
  if(is.null(se[[i]])) se[[i]] <- rep(NA, 7)
}

bias_dat <- do.call("rbind", bias) %>% 
  as_tibble() %>%
  bind_cols(sim_grid)
rmse_dat <- do.call("rbind", rmse) %>% 
  as_tibble() %>%
  bind_cols(sim_grid)
se_dat <- do.call("rbind", se) %>% 
  as_tibble() %>%
  bind_cols(sim_grid)


##
## plot
##

focus_sample <- 500

## PT holds
bias_pt_diff <- bias_dat %>% 
  filter(Var1 ==  focus_sample, Var3 == 1) %>%
  select(DID, FE2, sDID, DDID, Var1, Var2, Var3) %>% 
  data.matrix()
rmse_pt_diff <- rmse_dat %>% 
  filter(Var1 == focus_sample, Var3 == 1) %>%
	select(DID, FE2, sDID, DDID, Var1, Var2, Var3) %>%
  data.matrix()
se_pt_diff <- se_dat %>%
  filter(Var1 == focus_sample, Var3 == 1) %>%
	select(DID, FE2, sDID, DDID, Var1, Var2, Var3) %>%
  data.matrix()

## PTT holds
bias_ptt_diff <- bias_dat %>%
  filter(Var1 ==  focus_sample, Var3 == 2) %>%
  select(DID, FE2, sDID, DDID, Var1, Var2, Var3) %>%
  data.matrix()
rmse_ptt_diff <- rmse_dat %>%
  filter(Var1 == focus_sample, Var3 == 2) %>%
  select(DID, FE2, sDID, DDID, Var1, Var2, Var3) %>%
  data.matrix()
se_ptt_diff <- se_dat %>%
  filter(Var1 == focus_sample, Var3 == 2) %>%
  select(DID, FE2, sDID, DDID, Var1, Var2, Var3) %>%
  data.matrix()


# plot setup
points <- c(0, 1, 2, 16, 17)
lines  <- c(2, 2, 2, 1, 1)
n_estimators <- c(2, 3, 4)
ylim_bias <- c(0, 0.3)
ylim_se <- c(0.15, 0.3)
ylim_rmse <- c(0, 0.65)
labels_auto <-  c("0", "0.2", "0.4", "0.6", "0.8")


## PT
pdf("../results/figures/figureA3_auto_sim_06052021.pdf", width  = 8, height = 7.2)
par(mfcol = c(2, 2), mar = c(4, 4.5, 1, 1), oma = c(0, 0, 1.5, 0))

  # --------------------------
  # PT 
  # --------------------------
  plot(1, 1, type = 'n', xlim = c(1, nrow(se_pt_diff)), ylim = ylim_bias,
      main = "",
      xlab = "", ylab = "", xaxt = 'n')
  title(xlab = "Autocorrelation", line = 2.1)
  axis(1, at = 1:nrow(se_pt_diff), labels = labels_auto)
  for (i in n_estimators) {
    lines(1:nrow(bias_pt_diff), bias_pt_diff[,i], type = 'b',
          pch = points[i], lty = lines[i], cex = 1.25)
  }
  legend("topright", legend = c("Extended DID", "Sequential DID", "Double DID"),
        pch = points[n_estimators], lty = lines[n_estimators], bty = "n", cex = 1.25)
  mtext(2, text = "Absolute Bias", font = 2, line = 2.8)
  mtext(3, text = "Scenario 1 (Extended Parallel Trends)", font = 2, line = 1)

  plot(1, 1, type = 'n', xlim = c(1, nrow(se_pt_diff)), ylim = ylim_se,
      main = "",
      xlab = "", ylab = "", xaxt = 'n')
  title(xlab = "Autocorrelation", line = 2.1)
  axis(1, at = 1:nrow(se_pt_diff), labels = labels_auto)
  for (i in n_estimators) {
    lines(1:nrow(bias_pt_diff), se_pt_diff[,i], type = 'b',
          pch = points[i], lty = lines[i], cex = 1.25)
  }
  mtext(2, text = "Standard Errors", font = 2, line = 2.8)

  # --------------------------
  # PTT
  # --------------------------
  plot(1, 1, type = 'n', xlim = c(1, nrow(se_ptt_diff)), ylim = ylim_bias,
      main = "",
      xlab = "", ylab = "", xaxt = 'n')
  title(xlab = "Autocorrelation", line = 2.1)
  axis(1, at = 1:nrow(se_ptt_diff), labels = labels_auto)
  for (i in n_estimators) {
    lines(1:nrow(bias_ptt_diff), bias_ptt_diff[,i], type = 'b',
          pch = points[i], lty = lines[i], cex = 1.25)
  }
  mtext(3, text = "Scenario 2 (Parallel Trends-in-Trends)", font = 2, line = 1)

  plot(1, 1, type = 'n', xlim = c(1, nrow(se_ptt_diff)), ylim = ylim_se,
      main = "",
      xlab = "", ylab = "", xaxt = 'n')
  title(xlab = "Autocorrelation", line = 2.1)
  axis(1, at = 1:nrow(se_ptt_diff), labels = labels_auto)
  for (i in n_estimators) {
    lines(1:nrow(bias_ptt_diff), se_ptt_diff[,i], type = 'b',
          pch = points[i], lty = lines[i], cex = 1.25)
  }

dev.off()