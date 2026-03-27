## R CODE TO BUILD FIGURES #####################################################

# For NATURE E&E, one-column width = 88 mm (= 3.46 in) and two-column width =
# 180 mm (= 7.09 in). Text should be minimally 5 points and maximally 7 points
# (=2 mm) high after reduction, and they prefer Helvetica font, the default for
# pdf() and postscript(), but other sans-serifs (e.g., Arial) are fine. Colors
# should be in RGB rather than CMYK and avoiding red and green on same plot.
# Figures should be vector-based images (such as .pdf, .ai, or .eps) that remain
# the same resolution when size is changed. Figure parts should be labelled with
# bold lowercase a, b, c, etc. in same font size and font as elsewhere in
# figure. See www.nature.com/natecolevol/info/final-submission#preparing-figures
# for more details.

## PREPARATIONS ################################################################
rm(list = ls())
op <- par()

# Specify pdf options
pdf.options(width = 7.09, height = 7.09, family = "Helvetica", 
            colormodel = "rgb", reset = TRUE)

# Use following to reset to factory-fresh defaults
# pdf.options(reset = TRUE)

# Set working directory (point to the folder containing the input files on your
# own machine):
# setwd("[filepath to folder containing data files on your personal machine]")
setwd("~/Manuscripts/CamOrdEchinos/Data files/NA Reformatted")

library(geoscale)   # v. 2.0
library(viridisLite)# v. 0.3.0

# Modification of geoscale::geoscalePlot to allow ICS 2020 timescale and
# multipanel plots
source("~/Manuscripts/CamOrdEchinos/geoscalePlot3.R")

# strat_names <- read.csv("https://www.paleobiodb.org/data1.2/intervals/list.csv?all_records&vocab=pbdb")
strat_names <- read.csv("~/Manuscripts/CamOrdEchinos/strat_names.csv")
ages <- strat_names[which(strat_names$scale_level == 5), ]
ages <- ages[which(ages$max_ma > 444), ]
# Add Ediacaran for any pre-Cambrian nodes
ages <-
  rbind(ages, strat_names[which(strat_names$interval_name == "Ediacaran"),])
mids <- apply(ages[ ,9:10], 1, mean)

# Import ICS 2020 timescale to use in plotting
ICS2020 <- read.csv("~/Manuscripts/CamOrdEchinos/timescales2020.csv", 
                    stringsAsFactors =  TRUE)
# Replace abbreviation of Llandovery to suppress plotting
ICS2020$Abbrev <- replace(ICS2020$Abbrev, which(ICS2020$Abbrev == "Llan"), NA)

# Specify colors using RGB-sensitive colors
cols <- viridisLite::plasma(3)         # 1-blue and 2-pink
trans.cols <- viridisLite::plasma(3, alpha = 0.4)
# Replace trans col 3 with dark gray
trans.cols[3] <- rgb(0, 0, 0, alpha = .4)
  
# Other decent color combos:
# cols <- viridisLite::magma(3)         # 1-black and 2-magenta
# trans.cols <- viridisLite::magma(3, alpha = 0.5)
# cols <- viridisLite::viridis(3)         # 1-purple and 2-turquoise
# trans.cols <- viridisLite::viridis(3, alpha = 0.5)






## FIGURE 1: 4-PANEL FIGURE OF DISPARITY TRENDS ################################
# Comparing ecological and morphological disparity trends for H, rates, D, and 
# FRic. Raw H (= S for morphology), the rest sample-standardized.

# Import objects
load("morph.metrics")
load("mode.metrics")

metrics_morph <- read.csv(file = "metrics_StdG50_morph.csv", header = TRUE)
metrics_mode <- read.csv(file = "metrics_StdG50_LH_mode.csv", header = TRUE)

load("morph.rates")
load("eco.rates")

# Calculate mean trend and SDs for H
sq <- 1:length(morph.metrics)
morph.H.mean <- 
  apply(sapply(sq, function(sq) morph.metrics[[sq]]$H), 1, mean, na.rm = TRUE)
morph.H.SD <- 
  apply(sapply(sq, function(sq) morph.metrics[[sq]]$H), 1, sd, na.rm = TRUE)
mode.H.mean <- 
  apply(sapply(sq, function(sq) mode.metrics[[sq]]$H), 1, mean, na.rm = TRUE)
mode.H.SD <- 
  apply(sapply(sq, function(sq) mode.metrics[[sq]]$H), 1, sd, na.rm = TRUE)

# If wish to plot / view trend in raw lineage richnes:
# mode.S.mean <- apply(sapply(sq, function(sq) mode.metrics[[sq]]$S), 1, mean, na.rm = TRUE)
# mode.S.SD <- apply(sapply(sq, function(sq) mode.metrics[[sq]]$S), 1, sd, na.rm = TRUE)
# round(cbind(mode.S.mean, morph.H.mean, mode.S.mean - morph.H.mean), 2)

# Calculate mean value and SD (across trees) for D and FRic for Fortunian (for
# non-sample standardized values)
morph.D.mean <- 
  mean(sapply(sq, function(sq) morph.metrics[[sq]]$D[17]), na.rm = TRUE)
morph.D.SD <- 
  sd(sapply(sq, function(sq) morph.metrics[[sq]]$D[17]), na.rm = TRUE)
mode.D.mean <- 
  mean(sapply(sq, function(sq) mode.metrics[[sq]]$D[17]), na.rm = TRUE)
mode.D.SD <- 
  sd(sapply(sq, function(sq) mode.metrics[[sq]]$D[17]), na.rm = TRUE)
morph.FRic.mean <- 
  mean(sapply(sq, function(sq) morph.metrics[[sq]]$FRic[17]), na.rm = TRUE)
morph.FRic.SD <- 
  sd(sapply(sq, function(sq) morph.metrics[[sq]]$FRic[17]), na.rm = TRUE)
mode.FRic.mean <- 
  mean(sapply(sq, function(sq) mode.metrics[[sq]]$FRic[17]), na.rm = TRUE)
mode.FRic.SD <- 
  sd(sapply(sq, function(sq) mode.metrics[[sq]]$FRic[17]), na.rm = TRUE)
# Append these earliest values to the sample-standardized trend
metrics_morph$D[17] <- morph.D.mean
metrics_morph$SE.D[17] <- morph.D.SD
metrics_mode$D[17] <- mode.D.mean
metrics_mode$SE.D[17] <- mode.D.SD
metrics_morph$FRic[17] <- morph.FRic.mean
metrics_morph$SE.FRic[17] <- morph.FRic.SD
metrics_mode$FRic[17] <- mode.FRic.mean
metrics_mode$SE.FRic[17] <- mode.FRic.SD


# pdf(file = "Fig1.pdf")

# To maintain constant label point size across figures
cex.lab <- 1
cex.axis <- 0.75

# Plot bottom left time scale
lim <- c(0, max(c(morph.H.mean, mode.H.mean), na.rm = TRUE))
geoscalePlot3(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, srt = 0, 
              cex.ts = 1, cex.pt = 1, age.lim = c(540, 442), data.lim = lim, 
              ts.col = TRUE, timescale = ICS2020, type = "n", erotate = 0,
              abbrev = c("Period", "Epoch"), fig.coords = c(0, 0.5, 0, 0.14),
              only.timescale = TRUE, ts.width = 1, fig.mar = c(1, 3, 0, 0))
mtext(text = "Time (Ma)", side = 1, cex = cex.lab, line = -0.4)

# Plot bottom right time scale
geoscalePlot3(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", cex.age = 0.65, srt = 0, 
              cex.ts = 1, cex.pt = 1, age.lim = c(540, 442), data.lim = lim, 
              ts.col = TRUE, timescale = ICS2020, type = "n", erotate = 0, 
              abbrev = c("Period", "Epoch"), fig.coords = c(0.5, 1, 0, .14),
              only.timescale = TRUE, ts.width = 1, fig.mar = c(1, 3, 0, 0), 
              override.new.fig = TRUE)
mtext(text = "Time (Ma)", side = 1, cex = cex.lab, line = -0.4)

# Plot (top-left) no. of unique life habits / morphologies (raw)
geoscalePlot3(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", age.lim = c(540, 442), 
              data.lim = lim, label = "", timescale = ICS2020, type = "n", 
              fig.coords = c(0, 0.5, 0.57, 1), fig.mar = c(0, 3, 0.25, 0), 
              only.plot = TRUE, cex.axis = cex.axis)
mtext(text = "No. morphotypes / life habits", side = 2, cex = cex.lab, line = 2)
var_md <- mode.H.mean
var_md_top <- var_md + mode.H.SD
var_md_bottom <- var_md - mode.H.SD
var_mr <- morph.H.mean
var_mr_top <- var_mr + morph.H.SD
var_mr_bottom <- var_mr - morph.H.SD
lim <- range(c(var_md, var_mr), na.rm = TRUE)
column_md <- cbind(c(mids, rev(mids)), c(var_md_bottom, rev(var_md_top)))
column_md <- na.omit(column_md)
polygon(column_md[ ,1], column_md[ ,2], col = trans.cols[1], lwd = 2, border = NA)
column_mr <- cbind(c(mids, rev(mids)), c(var_mr_bottom, rev(var_mr_top)))
column_mr <- na.omit(column_mr)
polygon(column_mr[ ,1], column_mr[ ,2], col = trans.cols[2], lwd = 2, border = NA)
lines(mids, mode.H.mean, lwd = 3, lty = 1, col = cols[1])
lines(mids, morph.H.mean, lwd = 3, lty = 6, col = cols[2])
# Include if wish to plot trend in lineage richness
# var_S <- mode.S.mean
# var_S_top <- var_S + mode.S.SD
# var_S_bottom <- var_S - mode.S.SD
# column_S <- cbind(c(mids, rev(mids)), c(var_S_bottom, rev(var_S_top)))
# column_S <- na.omit(column_S)
# polygon(column_S[ ,1], column_S[ ,2], col = trans.cols[3], lwd = 2, border = NA)
# lines(mids, mode.S.mean, lwd = 1, lty = 1, col = "black")
legend("topleft", legend = c("Morphology", "Ecology"), col = cols[2:1], 
       bty = "n", lty = c(6, 1), lwd = 2.25, inset = 0, cex = 1.1)
mtext(text = "a", side = 3, line = -0.75, cex = 1.25, adj = -0.2, outer = FALSE)
mtext(text = "a", side = 3, line = -0.75, cex = 1.25, adj = -0.202, outer = FALSE)

# Plot (top-right) per-character rates through time (truncated)

# Unlist the rates and calculate trends in mean and SD, per character
load("tbins")
time_bin_mids <- as.vector(apply(tbins, 1, mean))
eco.rates.mean.pc <- 
  apply(simplify2array(lapply(sq, function(sq) eco.rates[[sq]]$time_rates[, "rate"])) / 40, 1, mean)
eco.rates.sd.pc <- 
  apply(simplify2array(lapply(sq, function(sq) eco.rates[[sq]]$time_rates[, "rate"])) / 40, 1, sd)
eco.top.pc <- eco.rates.mean.pc + eco.rates.sd.pc
eco.bottom.pc <- eco.rates.mean.pc - eco.rates.sd.pc
morph.rates.mean.pc <- 
  apply(simplify2array(lapply(sq, function(sq) morph.rates[[sq]]$time_rates[, "rate"])) / 413, 1, mean)
morph.rates.sd.pc <- 
  apply(simplify2array(lapply(sq, function(sq) morph.rates[[sq]]$time_rates[, "rate"])) / 413, 1, sd)
morph.top.pc <- morph.rates.mean.pc + morph.rates.sd.pc
morph.bottom.pc <- morph.rates.mean.pc - morph.rates.sd.pc
lim <- c(0, max(c(morph.top.pc, eco.top.pc)))

geoscalePlot3(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", age.lim = c(540, 442), 
              data.lim = lim, label = "", timescale = ICS2020, type = "n", 
              fig.coords = c(0.5, 1, 0.57, 1), fig.mar = c(0, 3, 0.25, 0), 
              only.plot = TRUE, cex.axis = cex.axis)
mtext(text = "Per-char. changes / lineage Myr", side = 2, cex = cex.lab, line = 2)
column <- cbind(c(time_bin_mids, rev(time_bin_mids)), 
                c(eco.bottom.pc, rev(eco.top.pc)), 
                c(morph.bottom.pc, rev(morph.top.pc)))
polygon(column[ ,1], column[ ,2], col = trans.cols[1], lwd = 2, border = NA)
lines(time_bin_mids, eco.rates.mean.pc, lwd = 3, lty = 1, col = cols[1])
polygon(column[ ,1], column[ ,3], col = trans.cols[2], lwd = 2, border = NA)
lines(time_bin_mids, morph.rates.mean.pc, lwd = 3, lty = 6, col = cols[2])
mtext(text = "b", side = 3, line = -0.75, cex = 1.25, adj = -0.2, outer = FALSE)
mtext(text = "b", side = 3, line = -0.75, cex = 1.25, adj = -0.202, outer = FALSE)


# Plot (bottom-left) mean Wills' GED distance (sample standardized & % max transformed)
var_md <- metrics_mode$D
var_mr <- metrics_morph$D
var_md_top <- var_md + metrics_mode$SE.D
var_md_bottom <- var_md - metrics_mode$SE.D
var_mr_top <- var_mr + metrics_morph$SE.D
var_mr_bottom <- var_mr - metrics_morph$SE.D
var_mr_top <- (var_mr_top - min(var_mr, na.rm = TRUE)) /
  (max(var_mr, na.rm = TRUE) - min(var_mr, na.rm = TRUE))
var_mr_bottom <- (var_mr_bottom - min(var_mr, na.rm = TRUE)) / 
  (max(var_mr, na.rm = TRUE) - min(var_mr, na.rm = TRUE))
var_mr <- (var_mr - min(var_mr, na.rm = TRUE)) / 
  (max(var_mr, na.rm = TRUE) - min(var_mr, na.rm = TRUE))
var_md_top <- (var_md_top - min(var_md, na.rm = TRUE)) /
  (max(var_md, na.rm = TRUE) - min(var_md, na.rm = TRUE))
var_md_bottom <- (var_md_bottom - min(var_md, na.rm = TRUE)) / 
  (max(var_md, na.rm = TRUE) - min(var_md, na.rm = TRUE))
var_md <- (var_md - min(var_md, na.rm = TRUE)) / 
  (max(var_md, na.rm = TRUE) - min(var_md, na.rm = TRUE))
lim <- range(c(var_md, var_mr), na.rm = TRUE)
geoscalePlot3(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", age.lim = c(540, 442), 
              data.lim = lim, label = "", timescale = ICS2020, type = "n", 
              fig.coords = c(0, 0.5, 0.14, 0.57), fig.mar = c(0, 3, 0.25, 0), 
              only.plot = TRUE, cex.axis = cex.axis)
mtext(text = "Wills GED (%-trans)", side = 2, cex = cex.lab, line = 2)
column_md <- cbind(c(mids, rev(mids)), c(var_md_bottom, rev(var_md_top)))
column_md <- na.omit(column_md)
polygon(column_md[ ,1], column_md[ ,2], col = trans.cols[1], lwd = 2, border = NA)
column_mr <- cbind(c(mids, rev(mids)), c(var_mr_bottom, rev(var_mr_top)))
column_mr <- na.omit(column_mr)
polygon(column_mr[ ,1], column_mr[ ,2], col = trans.cols[2], lwd = 2, border = NA)
lines(mids, var_md, lwd = 3, lty = 1, col = cols[1])
lines(mids, var_mr, lwd = 3, lty = 6, col = cols[2])
mtext(text = "c", side = 3, line = -0.75, cex = 1.25, adj = -0.2, outer = FALSE)
mtext(text = "c", side = 3, line = -0.75, cex = 1.25, adj = -0.202, outer = FALSE)

# Plot (bottom-right) convex hull volume (sample standardized & % max transformed)
var_md <- metrics_mode$FRic
var_mr <- metrics_morph$FRic
var_md_top <- var_md + metrics_mode$SE.FRic
var_md_bottom <- var_md - metrics_mode$SE.FRic
var_mr_top <- var_mr + metrics_morph$SE.FRic
var_mr_bottom <- var_mr - metrics_morph$SE.FRic
var_mr_top <- (var_mr_top - min(var_mr, na.rm = TRUE)) /
  (max(var_mr, na.rm = TRUE) - min(var_mr, na.rm = TRUE))
var_mr_bottom <- (var_mr_bottom - min(var_mr, na.rm = TRUE)) / 
  (max(var_mr, na.rm = TRUE) - min(var_mr, na.rm = TRUE))
var_mr <- (var_mr - min(var_mr, na.rm = TRUE)) / 
  (max(var_mr, na.rm = TRUE) - min(var_mr, na.rm = TRUE))
var_md_top <- (var_md_top - min(var_md, na.rm = TRUE)) /
  (max(var_md, na.rm = TRUE) - min(var_md, na.rm = TRUE))
var_md_bottom <- (var_md_bottom - min(var_md, na.rm = TRUE)) / 
  (max(var_md, na.rm = TRUE) - min(var_md, na.rm = TRUE))
var_md <- (var_md - min(var_md, na.rm = TRUE)) / 
  (max(var_md, na.rm = TRUE) - min(var_md, na.rm = TRUE))
lim <- range(c(var_md, var_mr), na.rm = TRUE)
geoscalePlot3(mids, rep(lim[1], length(mids)), units = c("Epoch", "Period"), 
              tick.scale = "Period", boxes = "Age", age.lim = c(540, 442), 
              data.lim = lim, label = "", timescale = ICS2020, type = "n", 
              fig.coords = c(0.5, 1, 0.14, 0.57), fig.mar = c(0, 3, 0.25, 0), 
              only.plot = TRUE, cex.axis = cex.axis)
mtext(text = "Convex hull volume (%-trans)", side = 2, cex = cex.lab, line = 2)
column_md <- cbind(c(mids, rev(mids)), c(var_md_bottom, rev(var_md_top)))
column_md <- na.omit(column_md)
polygon(column_md[ ,1], column_md[ ,2], col = trans.cols[1], lwd = 2, border = NA)
column_mr <- cbind(c(mids, rev(mids)), c(var_mr_bottom, rev(var_mr_top)))
column_mr <- na.omit(column_mr)
polygon(column_mr[ ,1], column_mr[ ,2], col = trans.cols[2], lwd = 2, border = NA)
lines(mids, var_md, lwd = 3, lty = 1, col = cols[1])
lines(mids, var_mr, lwd = 3, lty = 6, col = cols[2])
mtext(text = "d", side = 3, line = -0.75, cex = 1.25, adj = -0.2, outer = FALSE)
mtext(text = "d", side = 3, line = -0.75, cex = 1.25, adj = -0.202, outer = FALSE)

dev.off()






## FIGURE 2: 4-PANEL FIGURE OF RATES, ETC. #####################################
# Comparing ecological and morphological dynamics. Moving clockwise from
# topright: Subsampled rates, C1 statistics double histogram, distance moved for
# branching events, proportional character changes per branching event.

# Prep subsampling results
load("sub.AIC.results")
# Convert into clean data table
l.aic <- length(sub.AIC.results)
resampled.aic <- 
  data.frame(rate1 = NA, AIC1 = NA, AICc1 = NA, rate.morph = NA, rate.eco = NA, 
             AIC2 = NA, AICc2 = NA)
for(r in 1:l.aic) {
  resampled.aic[r, 1:7] <-
    as.numeric(unlist(sub.AIC.results[[r]]$character_test_results)[c(1:3, 5:8)])
}



if(!exists("cex.lab") | !exists("cex.axis"))
  stop("set 'cex.lab' and 'cex.axis' from figure 1.\n")
cex.axis <- 0.9 # Different because this value looks more consistent

# pdf(file = "Fig2.pdf")
par(mfrow = c(2,2), mar = c(3, 3, 0, 0))

# Plot panel 1: Subsampled partition-wide rates
breaks <- pretty(c(resampled.aic$rate.morph, resampled.aic$rate.eco), 20)
hist(resampled.aic$rate.eco, main = "", xlab = "", ylab = "", 
     breaks = breaks, col = "transparent", border = "transparent", prob = TRUE, 
     cex.axis = cex.axis)
mtext(text = "Density", side = 2, line = 2, cex = cex.lab)
mtext("Rate (per-character changes / lineage Myr)", side = 1, line = 2, cex = cex.lab)
hist(resampled.aic$rate.eco, add = TRUE, border = cols[1], col = trans.cols[1], 
     breaks = breaks, prob = TRUE)
hist(resampled.aic$rate.morph, add = TRUE, border = cols[2], col = trans.cols[2], 
     breaks = breaks, prob = TRUE)
legend("topright", inset = 0, c("Morphology", "Ecology"), pch = c(22, 22), 
       pt.bg = c(trans.cols[2], trans.cols[1]), col = c(cols[2], cols[1]), 
       cex = 1.5, pt.cex = 3, bty = "n")
# mtext(text = "a", side = 3, line = -0.75, cex = 1.25, adj = -0.17, outer = FALSE)
mtext(text = "a", side = 3, line = -1.25, cex = 1.25, adj = -0.17, outer = FALSE)
mtext(text = "a", side = 3, line = -1.25, cex = 1.25, adj = -0.172, outer = FALSE)

# Panel 2: C1 convergence histogram
load("Mode.morph.conv")
load("Mode.mode.conv")
breaks <- seq(from = 0, to = 1, by = 0.05)
hist(c(Mode.morph.conv$C1, Mode.mode.conv$C1), main = "", xlab = "", ylab = "", 
     breaks = breaks, col = "transparent", border = "transparent", prob = TRUE, 
     cex.axis = cex.axis)
mtext(text = "Density", side = 2, line = 2, cex = cex.lab)
mtext("C1 convergence among genus pairs", side = 1, line = 2, cex = cex.lab)
hist(Mode.mode.conv$C1, add = TRUE, border = cols[1], col = trans.cols[1], 
     breaks = breaks, prob = TRUE)
hist(Mode.morph.conv$C1, add = TRUE, border = cols[2], col = trans.cols[2], 
     breaks = breaks, prob = TRUE)
text(0.98, 0.75, "*", cex = 2, col = cols[1])
mtext(text = "b", side = 3, line = -1.25, cex = 1.25, adj = -0.17, outer = FALSE)
mtext(text = "b", side = 3, line = -1.25, cex = 1.25, adj = -0.172, outer = FALSE)

# Distance moved during each branching event
load("eco.branch.changes")
load("morph.branch.changes")

# Combine lists into continuous vector
bd.index <- seq(from = 3, to = 200, by = 4) # branching distances
bc.index <- seq(from = 4, to = 200, by = 4) # branching character changes
morph.br.dist <- unlist(unlist(morph.branch.changes, recursive = FALSE,
                               use.names = FALSE)[bd.index])
morph.char.ch <- unlist(unlist(morph.branch.changes, recursive = FALSE,
                               use.names = FALSE)[bc.index])
eco.br.dist <- unlist(unlist(eco.branch.changes, recursive = FALSE,
                             use.names = FALSE)[bd.index])
eco.char.ch <- unlist(unlist(eco.branch.changes, recursive = FALSE,
                             use.names = FALSE)[bc.index])

# Plotting results for all 50 trees, then dividing y-axis height by 50 to figure
# as mean across 50 trees
ntrees <- length(eco.branch.changes)
breaks.dist <- pretty(c(morph.br.dist, eco.br.dist), 10)
h.bd.eco <- hist(eco.br.dist, breaks = breaks.dist, plot = FALSE)
h.bd.morph <- hist(morph.br.dist, breaks = breaks.dist, plot = FALSE)
h.bd.eco$counts <- h.bd.eco$counts / ntrees
h.bd.morph$counts <- h.bd.morph$counts / ntrees
breaks.char <- pretty(c(morph.char.ch, eco.char.ch), 10)
h.bc.eco <- hist(eco.char.ch, breaks = breaks.char, plot = FALSE)
h.bc.morph <- hist(morph.char.ch, breaks = breaks.char, plot = FALSE)
h.bc.eco$counts <- h.bc.eco$counts / ntrees
h.bc.morph$counts <- h.bc.morph$counts / ntrees

# Plot branching distances during each branching event
plot(h.bd.eco, main = "", xlab = "", ylab = "", col = "transparent",
     border = "transparent", cex.axis = cex.axis)
mtext(text = "Mean no. branching events", side = 2, line = 2, cex = cex.lab)
mtext("Mean distance moved / branching event", side = 1, line = 2, cex = cex.lab)
plot(h.bd.eco, add = TRUE, border = cols[1], col = trans.cols[1])
plot(h.bd.morph, add = TRUE, border = cols[2], col = trans.cols[2])
mtext(text = "c", side = 3, line = -1.25, cex = 1.25, adj = -0.17, outer = FALSE)
mtext(text = "c", side = 3, line = -1.25, cex = 1.25, adj = -0.172, outer = FALSE)


# Plot number of character changes during each branching event
plot(h.bc.eco, main = "", xlab = "", ylab = "", col = "transparent", 
     border = "transparent", cex.axis = cex.axis)
mtext(text = "Mean no. branching events", side = 2, line = 2, cex = cex.lab)
mtext("Mean no. character changes / br. ev.", side = 1, line = 2, 
      cex = cex.lab)
plot(h.bc.eco, add = TRUE, border = cols[1], col = trans.cols[1])
plot(h.bc.morph, add = TRUE, border = cols[2], col = trans.cols[2])
mtext(text = "d", side = 3, line = -1.25, cex = 1.25, adj = -0.17, outer = FALSE)
mtext(text = "d", side = 3, line = -1.25, cex = 1.25, adj = -0.172, outer = FALSE)


dev.off()



