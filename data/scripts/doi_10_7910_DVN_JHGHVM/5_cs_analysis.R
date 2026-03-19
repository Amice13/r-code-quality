##########################################
# Code to create plots and tables associated
# with thresholds and results for the
# wastewater treatment case study.
# Taylor Grimm
# September 22nd, 2023
##########################################
source('6_results_1235_training.R')
library(viridis)
##############################################################################################
######### Density plots of T2 and SPE with each threshold
##############################################################################################

# Plot of T2 #
##################
# ggplot version #
##################

# t2_tb_wide <- tibble(mon_stat = mon_stats$T2s,
#                      thresh_parametric = parametric_t2,
#                      thresh_silv = thresh_silverman$T2_threshold,
#                      thresh_boot = thresh_boot$T2_threshold,
#                      thresh_silv_adj = thresh_silverman_adj$T2_threshold,
#                      thresh_raw = thresh_raw$T2_threshold)
# t2_tb_long <- t2_tb_wide |>
#   pivot_longer(cols = thresh_parametric:thresh_raw, names_to = 'type', values_to = 'val')
# 
# t2_tb_long |> 
#   filter(type %in% c("thresh_parametric", "thresh_raw", "thresh_silv",
#                      "thresh_silv_adj", "thresh_boot")) |> 
#   ggplot() +
#   geom_density(aes(mon_stat), show.legend = F) +
#   geom_segment(aes(val, 0, xend = val, yend = .1, col = type)) +
#   xlab(expression(italic(T)^2)) +
#   ylab("Density") +
#   scale_color_manual(values = c('thresh_parametric' = 'purple',
#                                 'thresh_raw' = viridis(8, option = 'H')[1],
#                                 'thresh_silv' = viridis(8, option = 'H')[2],
#                                 'thresh_boot' = viridis(8, option = 'H')[4],
#                                 'thresh_silv_adj' = viridis(8, option = 'H')[5]),
#                      name = 'Threshold',
#                      breaks = c("thresh_parametric", "thresh_raw", "thresh_silv",
#                                 "thresh_boot", "thresh_silv_adj"),
#                      labels = c('Parametric', 'Sample Quantile', 'SLVM', 'BOOT', 'ADJ-SLVM')) +
#   ylim(0, 0.1) +
#   xlim(0, 75) +
#   ggtitle(expression("Density Plot of "*italic(T)^2~"with Different Thresholds"))

pdf('t2_density.pdf', width = 6.5, height = 4.5)
par(mar = c(4, 4, 2.5, 2) + 0.1)
plot(density(mon_stats$T2s), ylab = 'Density', xlab = 'Value',
     main = expression('Density Plot of'~italic(T)^2~'with Different Thresholds'), cex.main = 1.25,
     xlim = c(0, 110))
abline(v = parametric_t2, col = 'purple', lwd = 2)
abline(v = thresh_raw$T2_threshold, col = viridis(8, option = 'H')[1], lwd = 2)
abline(v = thresh_silverman$T2_threshold, col = viridis(8, option = 'H')[2], lwd = 2)
abline(v = thresh_boot$T2_threshold, col = viridis(8, option = 'H')[4], lwd = 2)
abline(v = thresh_silverman_adj$T2_threshold, col = viridis(8, option = 'H')[5], lwd = 2)
legend('topright', bty = 'n', lty = 1, lwd = 2,
       legend = c("Parametric", "Sample Quantile", "SLVM", "BOOT", "ADJ-SLVM"),
       col = c('purple', viridis(8, option = 'H')[1], viridis(8, option = 'H')[2],
               viridis(8, option = 'H')[4], viridis(8, option = 'H')[5]))
grid(5, 5)
par(mar = c(5,4,4,2) + 0.1)
dev.off()

# Plot of SPE #
##################
# ggplot version #
##################
# spe_tb_wide <- tibble(mon_stat = mon_stats$SPEs,
#                       thresh_parametric = parametric_spe,
#                       thresh_silv = thresh_silverman$SPE_threshold,
#                       thresh_boot = thresh_boot$SPE_threshold,
#                       thresh_silv_adj = thresh_silverman_adj$SPE_threshold,
#                       thresh_raw = thresh_raw$SPE_threshold)
# spe_tb_long <- spe_tb_wide |>
#   pivot_longer(cols = thresh_parametric:thresh_raw, names_to = 'type', values_to = 'val')
# 
# spe_tb_long |>
#   filter(type %in% c("thresh_parametric", "thresh_raw", "thresh_silv",
#                      "thresh_silv_adj", "thresh_boot")) |>
#   ggplot() +
#   geom_density(aes(mon_stat), show.legend = F) +
#   geom_segment(aes(val, 0, xend = val, yend = .3, col = type)) +
#   xlab(expression(italic(SPE))) +
#   ylab("Density") +
#   scale_color_manual(values = c('thresh_parametric' = 'purple',
#                                 'thresh_raw' = viridis(8, option = 'H')[1],
#                                 'thresh_silv' = viridis(8, option = 'H')[2],
#                                 'thresh_boot' = viridis(8, option = 'H')[4],
#                                 'thresh_silv_adj' = viridis(8, option = 'H')[5]),
#                      name = 'Threshold',
#                      breaks = c("thresh_parametric", "thresh_raw", "thresh_silv",
#                                 "thresh_boot", "thresh_silv_adj"),
#                      labels = c('Parametric', 'Sample Quantile', 'SLVM', 'BOOT', 'ADJ-SLVM'))+
#   ylim(0, 0.3) +
#   xlim(0, 30) +
#   ggtitle(expression("Density Plot of "*italic(SPE)~"with Different Thresholds"))

pdf('spe_density.pdf', width = 6.5, height = 4.5)
par(mar = c(4, 4, 2.5, 2) + 0.1)
plot(density(mon_stats$SPEs), ylab = 'Density', xlab = 'Value',
     main = expression('Density Plot of'~italic(SPE)~'with Different Thresholds'), cex.main = 1.25,
     xlim = c(0, 35))
abline(v = parametric_spe, col = 'purple', lwd = 2)
abline(v = thresh_raw$SPE_threshold, col = viridis(8, option = 'H')[1], lwd = 2)
abline(v = thresh_silverman$SPE_threshold, col = viridis(8, option = 'H')[2], lwd = 2)
abline(v = thresh_boot$SPE_threshold, col = viridis(8, option = 'H')[4], lwd = 2)
abline(v = thresh_silverman_adj$SPE_threshold, col = viridis(8, option = 'H')[5], lwd = 2)
legend('topright', bty = 'n', lty = 1, lwd = 2,
       legend = c("Parametric", "Sample Quantile", "SLVM", "BOOT", "ADJ-SLVM"),
       col = c('purple', viridis(8, option = 'H')[1], viridis(8, option = 'H')[2],
               viridis(8, option = 'H')[4], viridis(8, option = 'H')[5]))
grid(5, 5)
par(mar = c(5,4,4,2) + 0.1)
dev.off()

##############################################################################################
####### Time series plots of T2 and SPE with different thresholds ############################
##############################################################################################

# Plot of T2 #
##################
# ggplot version #
##################
# 
# thresh_gg_t2 <- tibble(mon_stat = c(mon_stats$T2s, fault_detect_results$T2),
#                        period = c(rep("training", length(mon_stats$T2s)), rep("monitoring", length(fault_detect_results$T2))),
#                        date = alasso.residuals.df$date) |> 
#   mutate(parametric_t2 = parametric_t2,
#          silverman = thresh_silverman$T2_threshold,
#          iid_boot = thresh_boot$T2_threshold,
#          silverman_adj = thresh_silverman_adj$T2_threshold,
#          raw = thresh_raw$T2_threshold) |> 
#   pivot_longer(cols = c(4:8), names_to = 'method', values_to = 'val') |> 
#   mutate(method = factor(method, levels = c('parametric_t2', 'raw', 'silverman', 'iid_boot',
#                                             'silverman_adj'))) |> 
#   ggplot(aes(date, mon_stat, col = period)) +
#   geom_line() +
#   geom_line(mapping = aes(date, y = val, col = method)) +
#   scale_color_manual(values = c('parametric_t2' = 'purple', 'silverman' = viridis(8, option = 'H')[2],
#                                 'mon_stat' = 'black', 'iid_boot' = viridis(8, option = 'H')[4],
#                                 'silverman_adj' = viridis(8, option = 'H')[5], 'raw' = viridis(8, option = 'H')[1],
#                                 'training' = 'gray', 'monitoring' = 'black'),
#                      name = 'Method', breaks = c("parametric_t2", 'raw',
#                                                  "silverman", "iid_boot", "silverman_adj"),
#                      labels = c('Parametric', 'Sample Quantile', 'SLVM',
#                                 'BOOT', 'ADJ-SLVM')) +
#   ylab(expression(T^2)) + xlab('') +
#   ylim(0, 200)
# thresh_gg_t2 + theme(legend.position = c(.15, .79))


# Plot of SPE #
##################
# ggplot version #
##################
# thresh_gg_spe <- tibble(mon_stat = c(mon_stats$SPEs, fault_detect_results$SPE),
#                         period = c(rep("training", length(mon_stats$SPEs)), rep("monitoring", length(fault_detect_results$SPE))),
#                         date = alasso.residuals.df$date) |> 
#   mutate(parametric_spe = parametric_spe,
#          silverman = thresh_silverman$SPE_threshold,
#          iid_boot = thresh_boot$SPE_threshold,
#          silverman_adj = thresh_silverman_adj$SPE_threshold,
#          raw = thresh_raw$SPE_threshold) |> 
#   pivot_longer(cols = c(4:8), names_to = 'method', values_to = 'val') |> 
#   mutate(method = factor(method, levels = c('parametric_spe', 'raw', 'silverman', 'iid_boot',
#                                             'silverman_adj'))) |> 
#   ggplot(aes(date, mon_stat, col = period)) +
#   geom_line() +
#   geom_line(mapping = aes(date, y = val, col = method)) +
#   scale_color_manual(values = c('parametric_spe' = 'purple', 'silverman' = viridis(8, option = 'H')[2],
#                                 'mon_stat' = 'black', 'iid_boot' = viridis(8, option = 'H')[4],
#                                 'silverman_adj' = viridis(8, option = 'H')[5], 'raw' = viridis(8, option = 'H')[1],
#                                 'training' = 'gray', 'monitoring' = 'black'),
#                      name = 'Method', breaks = c("parametric_spe", 'raw',
#                                                  "silverman", "iid_boot", "silverman_adj"),
#                      labels = c('Parametric', 'Sample Quantile', 'SLVM',
#                                 'BOOT', 'ADJ-SLVM')) +
#   ylab(expression(SPE)) + xlab('') +
#   ylim(0, 50)
# 
# thresh_gg_spe + theme(legend.position = c(.15, .79))



#pdf('t2_jrssc_thresholds.pdf', width = 6.5, height = 4.5)
pdf('t2_jrssc_thresholds.pdf', width = 10.5, height = 3)
par(mar = c(2.25, 4, 2.5, 2) + 0.1)
plot(alasso.residuals.df$date[(length(mon_stats$T2s) + 1):(length(mon_stats$T2s) + length(fault_detect_results$T2))],
     fault_detect_results$T2, type = 'l', ylab = 'Value', xlab = '',
     cex.main = 1.25, main = expression(italic(T)^2~"Monitoring Statistics"),
     ylim = c(0, 200), xlim = range(alasso.residuals.df$date))
lines(alasso.residuals.df$date[1:length(mon_stats$T2s)], mon_stats$T2s, col = 'gray')
abline(h = parametric_t2, lty = 1, col = 'purple')
abline(h = thresh_silverman$T2_threshold, lty = 1, col = viridis(8, option = 'H')[2])
abline(h = thresh_boot$T2_threshold, lty = 1, col = viridis(8, option = 'H')[4])
abline(h = thresh_silverman_adj$T2_threshold, lty = 1, col = viridis(8, option = 'H')[5])
abline(h = thresh_raw$T2_threshold, lty = 1, col = viridis(8, option = 'H')[1])
# start of the monitoring period
#abline(v = as.POSIXct("2010-04-20T01:10:00", format = "%Y-%m-%dT%H:%M:%S"), col = 'gray')
# the operators did not detect the fault until April 24th at 10.00 a.m.
abline(v = as.POSIXct("2010-04-24T10:00:00", format = "%Y-%m-%dT%H:%M:%S"), col = 'red', lty = 2)
#abline(v = as.POSIXct("2010-04-21T13:40:00", format = "%Y-%m-%dT%H:%M:%S"), col = 'green') # Kazor detection
legend('topleft', bty = 'n', lty = c(1, 1, 1, 1, 1),
       col = c('purple', viridis(8, option = 'H')[1], viridis(8, option = 'H')[2],
               viridis(8, option = 'H')[4], viridis(8, option = 'H')[5]),
       legend = c("Parametric", "Sample Quantile", "SLVM", "BOOT", "ADJ-SLVM"))
grid(5, 5)
dev.off()

#pdf('spe_jrssc_thresholds.pdf', width = 6.5, height = 4.5)
pdf('spe_jrssc_thresholds.pdf', width = 10.5, height = 3)
par(mar = c(2.25, 4, 2.5, 2) + 0.1)
plot(alasso.residuals.df$date[(length(mon_stats$SPEs) + 1):(length(mon_stats$SPEs) + length(fault_detect_results$SPE))],
     fault_detect_results$SPE, type = 'l', ylab = "Value", xlab = '', ylim = c(0, 75), cex.main = 1.25,
     main = expression(italic(SPE)~"Monitoring Statistics"), xlim = range(alasso.residuals.df$date))
lines(alasso.residuals.df$date[1:length(mon_stats$SPEs)], mon_stats$SPEs, col = 'gray')
abline(h = parametric_spe, lty = 1, col = 'purple')
abline(h = thresh_silverman$SPE_threshold, lty = 1, col = viridis(8, option = 'H')[2])
abline(h = thresh_boot$SPE_threshold, lty = 1, col = viridis(8, option = 'H')[4])
abline(h = thresh_silverman_adj$SPE_threshold, lty = 1, col = viridis(8, option = 'H')[5])
abline(h = thresh_raw$SPE_threshold, lty = 1, col = viridis(8, option = 'H')[1])
#abline(v = as.POSIXct("2010-04-20T01:10:00", format = "%Y-%m-%dT%H:%M:%S"), col = 'gray')
abline(v = as.POSIXct("2010-04-24T10:00:00", format = "%Y-%m-%dT%H:%M:%S"), col = 'red', lty = 2)
#abline(v = as.POSIXct("2010-04-21T13:40:00", format = "%Y-%m-%dT%H:%M:%S"), col = 'green')
legend('topleft', bty = 'n', lty = c(1, 1, 1, 1, 1),
       col = c('purple', viridis(8, option = 'H')[1], viridis(8, option = 'H')[2],
               viridis(8, option = 'H')[4], viridis(8, option = 'H')[5]),
       legend = c("Parametric", "Sample Quantile", "SLVM", "BOOT", "ADJ-SLVM"))
grid(5, 5)
par(mar = c(5,4,4,2) + 0.1)
dev.off()


##############################################################################################
########## Plot ACF and PACF for T2 and SPE ##################################################
##############################################################################################
pdf('t2_acf_pacf.pdf', width = 6, height = 3)
astsa::acf2(mon_stats$T2s, main = expression(italic(T)^2), ylim = c(0, 0.5))
dev.off()

pdf('spe_acf_pacf.pdf', width = 6, height = 3)
astsa::acf2(mon_stats$SPEs, main = expression(italic(SPE)), ylim = c(0, 0.5))
dev.off()

pdf('t2_ar1_acf_pacf.pdf', width = 6, height = 3)
arima(mon_stats$T2s, order = c(1, 0, 0)) |> residuals() |>
  astsa::acf2(main = expression(italic(T)^2~'Residuals From AR(1) Fit'), ylim = c(0, 0.5))
dev.off()

pdf('spe_ar1_acf_pacf.pdf', width = 6, height = 3)
arima(mon_stats$SPEs, order = c(1, 0, 0)) |> residuals() |>
  astsa::acf2(main = expression(italic(SPE)~'Residuals From AR(1) Fit'), ylim = c(0, 0.5))
dev.off()

pdf('t2_arma11_acf_pacf.pdf', width = 6, height = 3)
arima(mon_stats$T2s, order = c(1, 0, 1)) |> residuals() |>
  astsa::acf2(main = expression(italic(T)^2~'Residuals From ARMA(1,1) Fit'), ylim = c(0, 0.5))
dev.off()
pdf('spe_arma11_acf_pacf.pdf', width = 6, height = 3)
arima(mon_stats$SPEs, order = c(1, 0, 1)) |> residuals() |>
  astsa::acf2(main = expression(italic(SPE)~'Residuals From ARMA(1,1) Fit'), ylim = c(0, 0.5))
dev.off()


##############################################################################################
# Proportion of mon stats above thresholds during training and testing
##############################################################################################
# train_dates <- alasso.residuals.df$date[1:length(mon_stats$T2s)]
# test_dates <- alasso.residuals.df$date[(length(mon_stats$T2s) + 1):nrow(alasso.residuals.df)]
# num_train <- length(train_dates)
# num_test <- length(test_dates)

train_t2_prop_parametric <- mean(mon_stats$T2s > parametric_t2)
test_t2_prop_parametric <- mean(fault_detect_results$T2 > parametric_t2)
train_t2_prop_silverman <- mean(mon_stats$T2s > thresh_silverman$T2_threshold)
test_t2_prop_silverman <- mean(fault_detect_results$T2 > thresh_silverman$T2_threshold)
train_t2_prop_boot <- mean(mon_stats$T2s > thresh_boot$T2_threshold)
test_t2_prop_boot <- mean(fault_detect_results$T2 > thresh_boot$T2_threshold)
train_t2_prop_silverman_adj <- mean(mon_stats$T2s > thresh_silverman_adj$T2_threshold)
test_t2_prop_silverman_adj <- mean(fault_detect_results$T2 > thresh_silverman_adj$T2_threshold)
train_t2_prop_raw <- mean(mon_stats$T2s > thresh_raw$T2_threshold)
test_t2_prop_raw <- mean(fault_detect_results$T2 > thresh_raw$T2_threshold)

tibble(method = c("parametric", "silverman", "iid_boot", "silverman_adj", "raw"),
       train_pct = c(train_t2_prop_parametric, train_t2_prop_silverman, train_t2_prop_boot,
                     train_t2_prop_silverman_adj, train_t2_prop_raw),
       test_pct = c(test_t2_prop_parametric, test_t2_prop_silverman, test_t2_prop_boot,
                    test_t2_prop_silverman_adj, test_t2_prop_raw))


train_spe_prop_parametric <- mean(mon_stats$SPEs > parametric_spe)
test_spe_prop_parametric <- mean(fault_detect_results$SPE > parametric_spe)
train_spe_prop_silverman <- mean(mon_stats$SPEs > thresh_silverman$SPE_threshold)
test_spe_prop_silverman <- mean(fault_detect_results$SPE > thresh_silverman$SPE_threshold)
train_spe_prop_boot <- mean(mon_stats$SPEs > thresh_boot$SPE_threshold)
test_spe_prop_boot <- mean(fault_detect_results$SPE > thresh_boot$SPE_threshold)
train_spe_prop_silverman_adj <- mean(mon_stats$SPEs > thresh_silverman_adj$SPE_threshold)
test_spe_prop_silverman_adj <- mean(fault_detect_results$SPE > thresh_silverman_adj$SPE_threshold)
train_spe_prop_raw <- mean(mon_stats$SPEs > thresh_raw$SPE_threshold)
test_spe_prop_raw <- mean(fault_detect_results$SPE > thresh_raw$SPE_threshold)

tibble(method = c("parametric", "silverman", "iid_boot", "silverman_adj", "raw"),
       train_pct = c(train_spe_prop_parametric, train_spe_prop_silverman, train_spe_prop_boot,
                     train_spe_prop_silverman_adj, train_spe_prop_raw),
       test_pct = c(test_spe_prop_parametric, test_spe_prop_silverman, test_spe_prop_boot,
                    test_spe_prop_silverman_adj, test_spe_prop_raw))


################################################################################
# Counts #######################################################################
################################################################################

train_t2_count_parametric <- sum(mon_stats$T2s > parametric_t2)
test_t2_count_parametric <- sum(fault_detect_results$T2 > parametric_t2)
train_t2_count_silverman <- sum(mon_stats$T2s > thresh_silverman$T2_threshold)
test_t2_count_silverman <- sum(fault_detect_results$T2 > thresh_silverman$T2_threshold)
train_t2_count_boot <- sum(mon_stats$T2s > thresh_boot$T2_threshold)
test_t2_count_boot <- sum(fault_detect_results$T2 > thresh_boot$T2_threshold)
train_t2_count_silverman_adj <- sum(mon_stats$T2s > thresh_silverman_adj$T2_threshold)
test_t2_count_silverman_adj <- sum(fault_detect_results$T2 > thresh_silverman_adj$T2_threshold)
train_t2_count_raw <- sum(mon_stats$T2s > thresh_raw$T2_threshold)
test_t2_count_raw <- sum(fault_detect_results$T2 > thresh_raw$T2_threshold)

tibble(method = c("parametric", "silverman", "iid_boot", "silverman_adj", "raw"),
       train_count = c(train_t2_count_parametric, train_t2_count_silverman, train_t2_count_boot,
                       train_t2_count_silverman_adj, train_t2_count_raw),
       test_count = c(test_t2_count_parametric, test_t2_count_silverman, test_t2_count_boot,
                      test_t2_count_silverman_adj, test_t2_count_raw))


train_spe_count_parametric <- sum(mon_stats$SPEs > parametric_spe)
test_spe_count_parametric <- sum(fault_detect_results$SPE > parametric_spe)
train_spe_count_silverman <- sum(mon_stats$SPEs > thresh_silverman$SPE_threshold)
test_spe_count_silverman <- sum(fault_detect_results$SPE > thresh_silverman$SPE_threshold)
train_spe_count_boot <- sum(mon_stats$SPEs > thresh_boot$SPE_threshold)
test_spe_count_boot <- sum(fault_detect_results$SPE > thresh_boot$SPE_threshold)
train_spe_count_silverman_adj <- sum(mon_stats$SPEs > thresh_silverman_adj$SPE_threshold)
test_spe_count_silverman_adj <- sum(fault_detect_results$SPE > thresh_silverman_adj$SPE_threshold)
train_spe_count_raw <- sum(mon_stats$SPEs > thresh_raw$SPE_threshold)
test_spe_count_raw <- sum(fault_detect_results$SPE > thresh_raw$SPE_threshold)

tibble(method = c("parametric", "silverman", "iid_boot", "silverman_adj", "raw"),
       train_count = c(train_spe_count_parametric, train_spe_count_silverman, train_spe_count_boot,
                       train_spe_count_silverman_adj, train_spe_count_raw),
       test_count = c(test_spe_count_parametric, test_spe_count_silverman, test_spe_count_boot,
                      test_spe_count_silverman_adj, test_spe_count_raw))


#####################################################################################
##### Getting results for different training sizes ##################################
#####################################################################################
source('6_get_cs_thresholds.R')

thresholds <- c(seq(100, 800, 100), 1235) |> map_dfr(get_thresholds)

t2_thresholds <- thresholds[, c(1, seq(6, 18, 2))]
cs_t2_thresholds <- t2_thresholds[, c(1:6)]
colnames(cs_t2_thresholds) <- c("Train Size", "Parametric", "Sample Quantile", "SLVM", "BOOT", "ADJ-SLVM")
cs_t2_thresholds

spe_thresholds <- thresholds[, c(1, seq(7, 19, 2))]
cs_spe_thresholds <- spe_thresholds[, c(1:6)]
colnames(cs_spe_thresholds) <- c("Train Size", "Parametric", "Sample Quantile", "SLVM", "BOOT", "ADJ-SLVM")
cs_spe_thresholds





