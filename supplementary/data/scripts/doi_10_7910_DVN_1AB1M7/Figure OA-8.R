
#####################################
# Appendix G                        #
#####################################

final_out <- readRDS("../Output/sps_data_231112_ver_1_sim.rds")
sim_out <- final_out$sim_out

design_table <- expand.grid("num_var" = c(3, 4, 5, 7, 9, 12, 15, 20, 25, 30),
                            "N_s" = c(3, 6, 9),
                            "coef_mul" = c(1),
                            "se_mul" = c(1),
                            "N" = c(20))

total_sim_size <- length(sim_out)
length(sim_out)/nrow(design_table)

{
  ind <- unlist(sapply(seq(1:total_sim_size), function(x) sim_out[[x]][[1]]))
  
  # TRUE
  true <- do.call("rbind", lapply(seq(1:total_sim_size),
                                  FUN = function(x) sim_out[[x]][[2]]))
  
  # SD
  true_sd <- do.call("rbind", lapply(seq(1:total_sim_size),
                                     FUN = function(x) sim_out[[x]][[3]]))
  
  # est_rs
  est_rs <- do.call("rbind", lapply(seq(1:total_sim_size),
                                    FUN = function(x) sim_out[[x]][[4]]))
  
  # est_sps
  est_sps <- do.call("rbind", lapply(seq(1:total_sim_size),
                                     FUN = function(x) sim_out[[x]][[5]]))
}
table(ind)

## Matrix
true_effect <- c() 
sps_mat <- matrix(NA, nrow = nrow(design_table), ncol = 7)
rs_mat <- matrix(NA, nrow = nrow(design_table), ncol = 7)

for(i in 1:nrow(design_table)){
  
  # TRUE_coef
  true_effect[i] <- true_tau <- mean(true[ind == i])
  
  # SPS
  sps_mat[i, 1] <- mean(est_sps[ind == i, 1], na.rm = TRUE) # Estimate
  sps_mat[i, 2] <- mean(est_sps[ind == i, 1], na.rm = TRUE) - true_tau # Bias
  ci_low  <- as.numeric(est_sps[ind == i, 1] - 1.96*est_sps[ind == i, 2] <= true_tau)
  ci_high <- as.numeric(est_sps[ind == i, 1] + 1.96*est_sps[ind == i, 2] >= true_tau)
  sps_mat[i, 3] <- mean(ci_low*ci_high, na.rm = TRUE) # CI
  sps_mat[i, 4] <- mean(est_sps[ind == i, 2], na.rm = TRUE)  # estimated SE
  sps_mat[i, 5] <- sd(est_sps[ind == i, 1], na.rm = TRUE)   # true SE
  
  sps_mat[i, 6] <- sqrt(mean((est_sps[ind == i, 1] - true_tau)^2, na.rm = TRUE))
  
  sps_mat[i, 7] <- sqrt(mean((est_sps[ind == i, 1] - true[ind == i])^2, na.rm = TRUE))
  
  # RS
  rs_mat[i, 1] <- mean(est_rs[ind == i, 1], na.rm = TRUE) # Estimate
  rs_mat[i, 2] <- mean(est_rs[ind == i, 1], na.rm = TRUE) - true_tau # Bias
  ci_low  <- as.numeric(est_rs[ind == i, 1] - 1.96*est_rs[ind == i, 2] <= true_tau)
  ci_high <- as.numeric(est_rs[ind == i, 1] + 1.96*est_rs[ind == i, 2] >= true_tau)
  rs_mat[i, 3] <- mean(ci_low*ci_high, na.rm = TRUE) # CI
  rs_mat[i, 4] <- mean(est_rs[ind == i, 2], na.rm = TRUE)  # estimated SE
  rs_mat[i, 5] <- sd(est_rs[ind == i, 1], na.rm = TRUE)   # true SE
  
  rs_mat[i, 6] <- sqrt(mean((est_rs[ind == i, 1] - true_tau)^2, na.rm = TRUE))
  
  rs_mat[i, 7] <- sqrt(mean((est_rs[ind == i, 1] - true[ind == i])^2, na.rm = TRUE))
  
}
colnames(sps_mat) <- colnames(rs_mat) <- c("Est", "Bias", "CI", "Est_SE", "True_SE", "RMSE", "RMSE_2")


# Setup function
num_var <- unique(design_table[, "num_var"])
var_l <- length(unique(design_table[, "num_var"]))
var_rep <- nrow(design_table)/var_l - 1
add_vline <- function(var_l, var_rep){
  for(z in 1:var_rep){
    abline(v = z*var_l + 0.5, lty = 2)
  }
}

ylim_bias <- c(-0.005, 0.05)
ylim_rmse <- c(0, 0.05)

#####################################
# Figure OA-8                       #
#####################################

pdf("../Figures/Figure OA-8.pdf", height = 5, width = 12)
par(mfrow = c(1, 3))
specific_ind <- seq(from = 1, to = 10)
plot(seq(1:nrow(design_table))[specific_ind], sps_mat[specific_ind, "RMSE_2"], type = "b",
     main = 'Number of Study Sites = 3',
     pch = 19, ylim = ylim_rmse, col = "blue", 
     ylab = "RMSE", xlab = "Number of variables included in SPS", xaxt = "n", 
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.3)
Axis(side = 1, at = seq(1:nrow(design_table))[specific_ind], labels = design_table[specific_ind, 1], cex.axis = 1.3)
abline(h = mean(rs_mat[specific_ind, "RMSE_2"]), lty = 2, lwd = 2, col = "red")
text("RMSE of Random Sampling", x = 5, y = mean(rs_mat[specific_ind, "RMSE_2"]) + 0.002, 
     cex = 1.3, col = "red", font = 2)
abline(h = 0, lty = 2)
arrows(x0 = 3, y0 = 0.015, x1 = 3, y1 = 0.017, length = 0.05, lwd = 2)
text("Smallest", x = 3, y = 0.013, cex = 1.5)
text("RMSE", x = 3, y = 0.010, cex = 1.5)

specific_ind <- seq(from = 11, to = 20)
plot(seq(1:nrow(design_table))[specific_ind], sps_mat[specific_ind, "RMSE_2"], type = "b",
     main = "Number of Study Sites = 6",
     pch = 19, ylim = ylim_rmse, col = "blue", 
     ylab = "RMSE", xlab = "Number of variables included in SPS", xaxt = "n",
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.3)
Axis(side = 1, at = seq(1:nrow(design_table))[specific_ind], labels = design_table[specific_ind,1], cex.axis = 1.3)
abline(h = mean(rs_mat[specific_ind, "RMSE_2"]), lty = 2, lwd = 2, col = "red")
text("RMSE of Random Sampling", x = 15, y = mean(rs_mat[specific_ind, "RMSE_2"]) + 0.002, 
     cex = 1.3, col = "red", font = 2)
abline(h = 0, lty = 2)
arrows(x0 = 13, y0 = 0.011, x1 = 13, y1 = 0.013, length = 0.05, lwd = 2)
text("Smallest", x = 13, y = 0.009, cex = 1.5)
text("RMSE", x = 13, y = 0.006, cex = 1.5)

specific_ind <- seq(from = 21, to = 30)
plot(seq(1:nrow(design_table))[specific_ind], sps_mat[specific_ind, "RMSE_2"], type = "b",
     main = "Number of Study Sites = 9",
     pch = 19, ylim = ylim_rmse, col = "blue", 
     ylab = "RMSE", xlab = "Number of variables included in SPS", xaxt = "n",
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.3)
Axis(side = 1, at = seq(1:nrow(design_table))[specific_ind], labels = design_table[specific_ind,1], cex.axis = 1.3)
abline(h = mean(rs_mat[specific_ind, "RMSE_2"]), lty = 2, lwd = 2, col = "red")
text("RMSE of Random Sampling", x = 25, y = mean(rs_mat[specific_ind, "RMSE_2"]) + 0.002, 
     cex = 1.3, col = "red", font = 2)
abline(h = 0, lty = 2)
arrows(x0 = 23, y0 = 0.008, x1 = 23, y1 = 0.01, length = 0.05, lwd = 2)
text("Smallest", x = 23, y = 0.006, cex = 1.5)
text("RMSE", x = 23, y = 0.003, cex = 1.5)
dev.off()

