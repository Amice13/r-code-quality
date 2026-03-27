# Install and load the following packages:
require(spikes)
require(MASS)
require(BenfordTests)
require(VGAM)

# Load data and functions
load("fulldata.Rdata") # Note: Synthethic data can be simulated using synth() in functions.R file.
source("functions.R")

# Figure 1: Density and frequency plot of Russian 2011 data
y <- data[[3]]$v/data[[3]]$t
par(mfrow = c(2, 1), mar = c(3, 3, 1, .2), mgp = c(2, 1, 0), oma = c(0, 0, 0, 0))
d <- density(y, bw = 0.0001, from = 0, to = 1, cut = TRUE, n = 1001)
plot(d$x, d$y, xaxt = "n", xlab = "", lwd = 1.2, ylab = "Density", main = "Estimated kernel density", frame = FALSE, type = "l", xaxt = "n")
abline(v=seq(0,1,by = 0.1), lty=3, col = "grey")
axis(1, at = seq(0,1,by=0.1))
rfplot(y, title = "Raw frequencies")

# Figure 2: Precinct-level PMF and population-level sample
n <- 1000
N <- 50000
X <- S(n)
y <- sapply(X, p, N = n, turnout = .5, support = .59)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 0), mgp = c(2,1,0))
title = paste("Probability mass function \n", n, "voters")
plot(X, y, type = "h", xlim = c(0.4, 0.8), xaxt = "n", ylab = "Probability", xlab = "Vote-share", main = title, bty = "n")
axis(1, at = seq(-1, 2, by = 0.1))

nn <- sample(100:1500, N, replace = TRUE)
t <- rbinom(N, nn, rbeta(n, 2, 2))
v <- rbinom(N, t,  rbeta(n, 2, 1))
y <- v/t
y <- y[y > 0 & y < 1]

title = "Population distribution \n across 50,000 precincts"
plot(as.numeric(names(table(y))), table(y), type = "h", lwd = .8, xaxt = "n", yaxt = "n", bty = "n", xlim = c(0, 1), xlab = "Vote-share", ylab = "Frequency in the population", main = title)
axis(1, at = seq(-1, 2, by = .1))
axis(2, at = c(0, seq(50, max(table(y)) + 100, by = 50)))

# Figure 3: Comparing ratio of probabilities as a function of n and dispersion
x <- c(seq(200, 1000, by = 1), seq(1000, 10000, by = 10),  seq(10000, 45000, by = 1000))
y1 <- sapply(x, function(n) log(f.1(n)))
y2 <- sapply(x, function(n) log(f.2(n, theta = 1000)))
plot(log(x), y1, type = "l")
lines(log(x), y2, lty = 3, lwd = 2)

# Table 2: Main results; Note running this will take a *long* time
results  <- lapply(data, spikes)
results  <- lapply(confInt, results)
# The output of above calculations is stored in results.Rdata file; load this file to replicate figure 4 and Table 2 without reestimating:
load("results.Rdata")
do.call("rbind", lapply(results, summary))

# Figure 4
plot(results[[3]])
plot(results[[8]])

# Table 3: Digit tests
a <- lapply(data, function(x) bdig(x$v, n = 2))
b <- lapply(data, function(x) bdig(x$t, n = 2))
c <- lapply(data, function(x) last.dig(x$v))
d <- lapply(data, function(x) last.dig(x$t))

p <- do.call("cbind", lapply(list(a, b, c, d), function(a) unlist(lapply(a, function(x) x$p.value))))
tab <- roundr(do.call("cbind", lapply(list(a, b, c, d), function(a) unlist(lapply(a, function(x) x$statistic)))), 1)
pval <- function(x) ifelse(x < 0.001, "$^{\\dagger}$", ifelse(x < 0.01, "$^{**}$", ifelse(x < 0.05, "$^{*}$", "")))
tab <- sapply(1:4, function(x) paste(tab[,x], pval(p)[,x], sep = ""))
tab

# Appendix: Figure 1
par(mfrow = c(2, 2))
pplot(replicate(100000, f1(1000)), title = "Conditionally uniform")
pplot(f2(1000), title = "Binomial")
pplot(f3(1000, theta = 1000), title = "Beta-Binomial")
pplot(replicate(100000, f4(1000)), title = "Hypergeometric")

# Appendix: Figure 2
par(mfrow = c(2, 2), mar = c(3, 3, 3, 0))
sapply(c(0, .1, .2, .5), pop.plot, N = 50000)

# Appendix: Table 1 (Calibration)
# The code below takes a *long* time to run (it is suggested to parallelize)

resamples <- c(500,  500,    1000,  2000,   1000,   1000)
bw        <- c(0.01, 0.001, 0.001,  0.0001, 0.0001, 0.0001)
grid      <- c(501,  501,    1001,  1001,   1001,   2001)

data <- data[c(7, 9)]
out <- lapply(data, spikes, resamples = 1)
out <- lapply(1:2, function(x) lapply(1:length(bw), function(i) spikes(data[[x]], resamples = resamples[i], bw = bw[i], grid = grid[i], out = out[[x]]$out)))
ci <- function(i) lapply(out[[i]], confInt)
ci <- lapply(1:2, ci)
dtab <- function(w) cbind(resamples, do.call("rbind", lapply(w, function(x) c(x$grid, x$bw, summary(x)))))
dtab <- rbind(dtab(ci[[1]]), dtab(ci[[2]]))
