##########################################
# This script has three major code sections:
#
# 1) Code for creating plots of the true
# sampling distribution of each AR(1) series
#
# 2) Code for creating plots of the true
# sampling distribution of each ARMA(1, 1) series
#
# 3) Code for creating plots of the AR(1)
# and ARMA(1, 1) effective sample sizes.
#
# Taylor Grimm
# October 4th, 2023
##########################################

library(tidyverse)
library(viridis)

# 1) ---------------------------------------------------------------------
##########################################
# Code for creating plots of the true
# sampling distribution of each AR(1) series
##########################################

# Note: any variable or skewness argument that's "normal" refers to the
# F distribution with degrees of freedon 5 and n-5, which is the "normal"
# or standard case under the assumptions of independence and normality.

# Create a function to generate a long sample that represents the true underlying time series
# for each set of experimental factors
true_series_long <- function(ar = .9, samp_size = 100, skewness = 'heavier', seed = 1234, df1 = 5) {
  if(skewness == 'heavier') {
    df2 <- 20
  } else if(skewness == 'normal') {
    df2 <- samp_size - df1
  } else{
    stop("skewness must be either 'heavier' or 'normal'.")
  }
  n <- 1e6
  
  f_dist_constant <- (df1*(samp_size^2 - 1)/ (samp_size*(samp_size-df1)))
  set.seed(seed)
  
  errors <- rf(n + 100, df1, df2)*f_dist_constant
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce AR(1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar), innov = errors, n.start = 100)
  }
  series
}

# Generate long series for each value of phi (ar) for the F_{5, n-5} error case
series_1000_0 <- true_series_long(samp_size = 1000, ar = 0, skewness = 'normal')
series_1000_1 <- true_series_long(samp_size = 1000, ar = 0.1, skewness = 'normal')
series_1000_5 <- true_series_long(samp_size = 1000, ar = 0.5, skewness = 'normal')
series_1000_9 <- true_series_long(samp_size = 1000, ar = 0.9, skewness = 'normal')

# Create a plot with a line corresponding to the density of each series
pdf(file = 'true_f_densities_samp_size_baseR.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0), xlim = c(0, 90), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression("AR(1) Series with "*italic(F)[5*', '*italic(n)-5]*" Errors"),
     col = viridis(4)[1], lwd = 2, cex.main = 1.25)
lines(density(series_1000_1), col = viridis(4)[2], lwd = 2)
lines(density(series_1000_5), col = viridis(4)[3], lwd = 2)
lines(density(series_1000_9), col = viridis(4)[4], lwd = 2)
legend('topright', lty = 1, col = c(viridis(4)[1], viridis(4)[2], viridis(4)[3], viridis(4)[4]),
       legend = c('0', '0.1', '0.5', '0.9'), title = expression(phi), bty = 'n', lwd = 2)
grid(5, 5)
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()

# Generate long series for each value of phi (ar) for the F_{5, 20} error case
series_1000_0 <- true_series_long(samp_size = 1000, ar = 0, skewness = 'heavier')
series_1000_1 <- true_series_long(samp_size = 1000, ar = 0.1, skewness = 'heavier')
series_1000_5 <- true_series_long(samp_size = 1000, ar = 0.5, skewness = 'heavier')
series_1000_9 <- true_series_long(samp_size = 1000, ar = 0.9, skewness = 'heavier')

# Create a plot with a line corresponding to the density of each series
pdf(file = 'true_f_densities_fixed_baseR.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0), xlim = c(0, 90), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression("AR(1) Series with "*italic(F)[5*', '*20]*" Errors"),
     col = viridis(4)[1], lwd = 2, cex.main = 1.25)
lines(density(series_1000_1), col = viridis(4)[2], lwd = 2)
lines(density(series_1000_5), col = viridis(4)[3], lwd = 2)
lines(density(series_1000_9), col = viridis(4)[4], lwd = 2)
legend('topright', lty = 1, col = c(viridis(4)[1], viridis(4)[2], viridis(4)[3], viridis(4)[4]),
       legend = c('0', '0.1', '0.5', '0.9'), title = expression(phi), bty = 'n', lwd = 2)
grid(5, 5)
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()

# Repeat what was done above, but use Gamma(0.05, 0.05) errors.
true_series_long_gamma <- function(ar = .9, samp_size = 100, seed = 24, alpha = .05, beta = .05) {
  n <- 3.6e7
  set.seed(seed)
  
  errors <- rgamma(n + 100, alpha, beta)
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce AR(1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar), innov = errors, n.start = 100)
  }
  series
}

series_100_9_gam <- true_series_long_gamma()
series_100_5_gam <- true_series_long_gamma(ar = .5)
series_100_1_gam <- true_series_long_gamma(ar = .1)
series_100_0_gam <- true_series_long_gamma(ar = 0)

pdf(file = 'true_gamma_densities_baseR.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_100_0_gam), xlim = c(0, 90), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression("AR(1) Series with Gamma(0.05, 0.05) Errors"),
     col = viridis(4)[1], lwd = 2, cex.main = 1.25)
lines(density(series_100_1_gam), col = viridis(4)[2], lwd = 2)
lines(density(series_100_5_gam), col = viridis(4)[3], lwd = 2)
lines(density(series_100_9_gam), col = viridis(4)[4], lwd = 2)
legend('topright', lty = 1, col = c(viridis(4)[1], viridis(4)[2], viridis(4)[3], viridis(4)[4]),
       legend = c('0', '0.1', '0.5', '0.9'), title = expression(phi), bty = 'n', lwd = 2)
grid(5, 5)
par(mar = c(5, 4, 4, 2) + 0.1)
dev.off()



# 2) ----------------------------------------------------------------------
##########################################
# Code for creating plots of the true
# sampling distribution of each ARMA(1, 1) series
##########################################

# Note: any variable or skewness argument that's "normal" refers to the
# F distribution with degrees of freedon 5 and n-5, which is the "normal"
# or standard case under the assumptions of independence and normality.

# Create a function to generate a long sample that represents the true underlying time series
# for each set of experimental factors
true_series_long <- function(ar = .9, ma = 0.9, samp_size = 100, skewness = 'heavier', seed = 1234, df1 = 5) {
  if(skewness == 'heavier') {
    df2 <- 20
  } else if(skewness == 'normal') {
    df2 <- samp_size - df1
  } else{
    stop("skewness must be either 'heavier' or 'normal'.")
  }
  n <- 1e6
  
  f_dist_constant <- (df1*(samp_size^2 - 1)/ (samp_size*(samp_size-df1)))
  set.seed(seed)
  
  errors <- rf(n + 100, df1, df2)*f_dist_constant
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    # for dependent data, introduce ARMA(1,1) dependence with arima.sim
    series <- arima.sim(n = n, model = list(ar = ar, ma = ma), innov = errors, n.start = 100)
  }
  series
}

series_1000_0 <- true_series_long(samp_size = 1000, ar = 0, ma = 0, skewness = 'heavier')
series_1000_1 <- true_series_long(samp_size = 1000, ar = 0.1, ma = 0.1, skewness = 'heavier')
series_1000_1_neg5 <- true_series_long(samp_size = 1000, ar = 0.1, ma = -0.5, skewness = 'heavier')
series_1000_1_neg9 <- true_series_long(samp_size = 1000, ar = 0.1, ma = -0.9, skewness = 'heavier')
series_1000_5 <- true_series_long(samp_size = 1000, ar = 0.5, ma = 0.5, skewness = 'heavier')
series_1000_5_neg1 <- true_series_long(samp_size = 1000, ar = 0.5, ma = -0.1, skewness = 'heavier')
series_1000_5_neg9 <- true_series_long(samp_size = 1000, ar = 0.5, ma = -0.9, skewness = 'heavier')
series_1000_9 <- true_series_long(samp_size = 1000, ar = 0.9, ma = 0.9, skewness = 'heavier')
series_1000_9_neg1 <- true_series_long(samp_size = 1000, ar = 0.9, ma = -0.1, skewness = 'heavier')
series_1000_9_neg5 <- true_series_long(samp_size = 1000, ar = 0.9, ma = -0.5, skewness = 'heavier')


# Plot for phi = 0.1
pdf(file = 'arma_true_fixed_densities_baseR1.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0), xlim = c(-10, 30), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression("Series with "*italic(F)[5*', '*20]*" Errors; "*phi*" = 0.1"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_1_neg9), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_1_neg5), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_1), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.9, -0.5, 0.1), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
dev.off()
# Plot for phi = 0.5
pdf(file = 'arma_true_fixed_densities_baseR2.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0), xlim = c(-10, 30), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression(phi*" = 0.5"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_5_neg9), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_5_neg1), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_5), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.9, -0.1, 0.5), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
dev.off()
# Plot for phi = 0.9
pdf(file = 'arma_true_fixed_densities_baseR3.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0), xlim = c(0, 130), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression(phi*" = 0.9"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_9_neg5), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_9_neg1), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_9), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.5, -0.1, 0.9), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
dev.off()


series_1000_0 <- true_series_long(samp_size = 1000, ar = 0, ma = 0, skewness = 'normal')
series_1000_1 <- true_series_long(samp_size = 1000, ar = 0.1, ma = 0.1, skewness = 'normal')
series_1000_1_neg5 <- true_series_long(samp_size = 1000, ar = 0.1, ma = -0.5, skewness = 'normal')
series_1000_1_neg9 <- true_series_long(samp_size = 1000, ar = 0.1, ma = -0.9, skewness = 'normal')
series_1000_5 <- true_series_long(samp_size = 1000, ar = 0.5, ma = 0.5, skewness = 'normal')
series_1000_5_neg1 <- true_series_long(samp_size = 1000, ar = 0.5, ma = -0.1, skewness = 'normal')
series_1000_5_neg9 <- true_series_long(samp_size = 1000, ar = 0.5, ma = -0.9, skewness = 'normal')
series_1000_9 <- true_series_long(samp_size = 1000, ar = 0.9, ma = 0.9, skewness = 'normal')
series_1000_9_neg1 <- true_series_long(samp_size = 1000, ar = 0.9, ma = -0.1, skewness = 'normal')
series_1000_9_neg5 <- true_series_long(samp_size = 1000, ar = 0.9, ma = -0.5, skewness = 'normal')


pdf(file = 'arma_true_f_samp_size_densities_baseR1.pdf', width = 6, height = 4)
# Plot for phi = 0.1
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0), xlim = c(-10, 30), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression("ARMA(1,1) Series with "*italic(F)[5*', '*n*'-5']*" Errors; "*phi*" = 0.1"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_1_neg9), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_1_neg5), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_1), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.9, -0.5, 0.1), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
dev.off()
# Plot for phi = 0.5
pdf(file = 'arma_true_f_samp_size_densities_baseR2.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0), xlim = c(-10, 30), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression("ARMA(1,1) Series with "*italic(F)[5*', '*n*'-5']*" Errors; "*phi*" = 0.5"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_5_neg9), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_5_neg1), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_5), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.9, -0.1, 0.5), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
dev.off()
# Plot for phi = 0.9
pdf(file = 'arma_true_f_samp_size_densities_baseR3.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0), xlim = c(0, 130), ylim = c(0, .16), xlab = 'Value', ylab = 'Density',
     main = expression("ARMA(1,1) Series with "*italic(F)[5*', '*n*'-5']*" Errors; "*phi*" = 0.9"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_9_neg5), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_9_neg1), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_9), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.5, -0.1, 0.9), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
dev.off()


true_series_long_gamma <- function(ar = .9, ma = .9, samp_size = 100, seed = 24, alpha = .05, beta = .05) {
  n <- 3.6e7
  set.seed(seed)
  
  errors <- rgamma(n + 100, alpha, beta)
  if(ar == 0) {
    # for independent data, the series is just a bunch of independent random error terms
    series <- errors[-c(1:100)]
  } else {
    series <- arima.sim(n = n, model = list(ar = ar, ma = ma), innov = errors, n.start = 100)
  }
  series
}

series_1000_0_gam <- true_series_long_gamma(samp_size = 1000, ar = 0, ma = 0)
series_1000_1_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.1, ma = 0.1)
series_1000_1_neg5_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.1, ma = -0.5)
series_1000_1_neg9_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.1, ma = -0.9)
series_1000_5_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.5, ma = 0.5)
series_1000_5_neg1_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.5, ma = -0.1)
series_1000_5_neg9_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.5, ma = -0.9)
series_1000_9_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.9, ma = 0.9)
series_1000_9_neg1_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.9, ma = -0.1)
series_1000_9_neg5_gam <- true_series_long_gamma(samp_size = 1000, ar = 0.9, ma = -0.5)

pdf(file = 'arma_true_gamma_densities_baseR1.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
# Plot for phi = 0.1
plot(density(series_1000_0_gam), xlim = c(0, 90), ylim = c(0, .4), xlab = 'Value', ylab = 'Density',
     main = expression("Series with Gamma(0.05, 0.05) Errors; "*phi*" = 0.1"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_1_neg9_gam), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_1_neg5_gam), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_1_gam), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.9, -0.5, 0.1), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
dev.off()
# Plot for phi = 0.5
pdf(file = 'arma_true_gamma_densities_baseR2.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0_gam), xlim = c(-10, 90), ylim = c(0, 2), xlab = 'Value', ylab = 'Density',
     main = expression(phi*" = 0.5"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_5_neg9_gam), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_5_neg1_gam), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_5_gam), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.9, -0.1, 0.5), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
dev.off()
# Plot for phi = 0.9
pdf(file = 'arma_true_gamma_densities_baseR3.pdf', width = 6, height = 4)
par(mar = c(4, 4, 3, 2) + 0.1)
plot(density(series_1000_0_gam), xlim = c(0, 90), ylim = c(0, .4), xlab = 'Value', ylab = 'Density',
     main = expression(phi*" = 0.9"),
     col = 'black', lwd = 2, cex.main = 1.25, lty = 2)
lines(density(series_1000_9_neg5_gam), col = viridis(3)[1], lwd = 2)
lines(density(series_1000_9_neg1_gam), col = viridis(3)[2], lwd = 2)
lines(density(series_1000_9_gam), col = viridis(3)[3], lwd = 2)
legend('topright', lty = 1, col = c(viridis(3)[1], viridis(3)[2], viridis(3)[3]),
       legend = c(-0.5, -0.1, 0.9), title = expression(theta), bty = 'n', lwd = 2)
grid(5, 5)
par(mfrow=c(1,1))
dev.off()


# 3) ----------------------------------------------------------------------
##########################################
# Code for creating plots of the AR(1)
# and ARMA(1, 1) effective sample sizes.
##########################################


################################################################################################
# AR(1) Effective Sample Size Plot ############################################################
################################################################################################

effective_size <- function(n, phi) {
  x <- 1:n
  y <- 1:n
  xy <- expand.grid(x=x,y=y)
  COV <- matrix(phi^abs(xy[, 1] - xy[, 2]) / (1 - phi^2), nrow = n, ncol = n)
  in.brackets <- 1 + (sum(COV) - sum(diag(COV))) / n
  floor(n/(in.brackets))
}

phi_seq <- seq(0.01, .99, length = 20)

# n = 1000
eff_sizes_1000 <- phi_seq |> map_dbl(~ effective_size(1000, .x))
par(mar = c(4, 4, 3, 2) + 0.1)
plot(phi_seq, eff_sizes_1000, type = 'l', xlab = expression(phi),
     ylab = 'Effective Size', main = expression("Effective Sample Sizes for Values of "*phi),
     cex.main = 1.25)
grid(5,5)
par(mar = c(5, 4, 4, 2) + 0.1)


################################################################################################
# ARMA(1, 1) Effective Sample Size Plots #######################################################
################################################################################################

# The plot created below shows the ARMA(1, 1) effective sample sizes in addition
# to the AR(1) line from above since the values are equal when theta = 0 in the ARMA(1, 1) model.

effective_size_arma <- function(n, phi, theta) {
  x <- 1:n
  y <- 1:n
  xy <- expand.grid(x=x,y=y)
  COV <- matrix((((1 + theta*phi)*(phi + theta)) / (1 - phi^2))*phi^abs(xy[, 1] - xy[, 2]), nrow = n, ncol = n)
  in.brackets <- 1 + (sum(COV) - sum(diag(COV))) / n
  floor(n/(in.brackets))
}
phi_seq <- seq(0.01, .99, length = 20)

# n = 1000
eff_sizes_1000_theta0 <- phi_seq |> map_dbl(~ effective_size_arma(1000, .x, 0)) # theta = 0 (equivalent to AR(1) case)
eff_sizes_1000_theta1 <- phi_seq |> map_dbl(~ effective_size_arma(1000, .x, .1)) # theta = 0.1
eff_sizes_1000_theta5 <- phi_seq |> map_dbl(~ effective_size_arma(1000, .x, .5)) # theta = 0.5
eff_sizes_1000_theta9 <- phi_seq |> map_dbl(~ effective_size_arma(1000, .x, .9)) # theta = 0.9

eff_sizes_1000_theta1_neg <- phi_seq |> map_dbl(~ effective_size_arma(1000, .x, -.1)) # theta = -0.1
eff_sizes_1000_theta5_neg <- phi_seq |> map_dbl(~ effective_size_arma(1000, .x, -.5)) # theta = -0.5
eff_sizes_1000_theta9_neg <- phi_seq |> map_dbl(~ effective_size_arma(1000, .x, -.9)) # theta = -0.9

pdf('eff_samp_1000_both.pdf', width = 6.5, height = 5)
par(mfrow = c(1,1), mar = c(4, 4, 3, 2) + 0.1)
plot(phi_seq, eff_sizes_1000_theta1, type = 'l', xlab = expression(phi),
     ylab = 'Effective Size', main = expression("Effective Sample Sizes for Values of "*phi*" and "*theta),
     cex.main = 1.5, col = turbo(6)[4], ylim = c(0, 1500), xlim = c(0, 1.2), lwd = 2, cex.lab = 1.5)
lines(phi_seq, eff_sizes_1000_theta0, col = 'black', lty = 2, lwd = 2)
lines(phi_seq, eff_sizes_1000_theta9_neg, col = turbo(6)[1], lwd = 2)
lines(phi_seq, eff_sizes_1000_theta5_neg, col = turbo(6)[2], lwd = 2)
lines(phi_seq, eff_sizes_1000_theta1_neg, col = turbo(6)[3], lwd = 2)
lines(phi_seq, eff_sizes_1000_theta5, col = turbo(6)[5], lwd = 2)
lines(phi_seq, eff_sizes_1000_theta9, col = turbo(6)[6], lwd = 2)
legend('topright', col = c(turbo(6)[1], turbo(6)[2], turbo(6)[3], 'black', turbo(6)[4],
                           turbo(6)[5], turbo(6)[6]), legend = c('-0.9', '-0.5', '-0.1', '0', '0.1', '0.5', '0.9'),
       title = expression(theta), bty = 'n', lty = c(1,1,1,2,1,1,1), lwd = 2)
grid(5,5)
dev.off()