################################################################################
## R script for figures in Novack-Gottshall, Purcell, Sultan, Ranjha, Deline, ##
## & Sumrall manuscript                                                       ##
################################################################################

## 1 - PREPARATIONS (RUN FOR ALL FIGURES) ######################################
rm(list = ls())
op <- par()

# Set working directory (point to the folder containing the input files on your
# own machine):
setwd("~/Manuscripts/CamOrdEchino_EcoTrends/Revised manuscript/Figures")
# setwd("[filepath to folder containing data files on your personal machine]")

# Folder containing data objects to load
data <- "~/Manuscripts/CamOrdEchino_EcoTrends/Data & analyses/"


# Figure specific libraries and data objects are installed separately in each
# block of figure code so each figure can be drafted independently. (Code
# written using R 4.3.1 and RStudio 2023.06.0)
library(viridisLite)   # v. 0.4.2
library(palaeoverse)   # v. 1.2.0

# Colors are shared across figures. Plotting colors (using 8 levels for
# consistency across figures; use levels 2 and 7 for bicolor, levels 2, 5, and 7
# for tricolor, and 2, 5, 7, and 8 [or 2, 4, 5, 7) for quadricolor])
cols <- viridisLite::turbo(8) # Color palette chosen for manuscript
trans.cols <- viridisLite::turbo(8, alpha = 0.5)

# Modified deeptime::getScaleData intervals to use different abbreviations for
# epochs and stages
epochs_new <- deeptime::getScaleData("epochs")
epochs_new$name[28:30] <- c("Late Ord", "Middle Ord", "Early Ord")
epochs_new$abbr[28:34] <- c("L", "M", "E", "Fur", "Miao", "Ser 2", "Ter")
stages_new <- deeptime::getScaleData("stages")
stages_new$abbr <- rep("", 102)

# Boundaries
series.boundaries <- c(521, 509, 497, 470, 458.4)
pd.boundaries <- c(541, 485.4, 443.8)
axis.ticks <- c(-535, -520, -505, -490, -475, -460, -445)
axis.labels <- c("535", "520", "505", "490", "475", "460", "445")



# PALAEONTOLOGY allows single column (= 80 mm / 3.150 in.), two-thirds (= 110 mm
# / 4.331), and full-page (= 166 mm / 6.535) figure widths. Maximum page height
# is 226 mm. Font should be Arial or Helvetica. Color should be RGB. Resolution
# of 600 dpi. Tiff is "strongly preferred". Use LZW (loss-less) image
# compression for supplementary figures to reduce size of image files.



## FIG 1: RICHNESS TRENDS ######################################################

# Read and prep data (note last columns were manually added during writing of
# manuscript and can be ignored here).
PBDB.div.discrete <- read.csv(file = paste0(data, "div_PBDB_discrete.csv"))
tip.div.discrete <- read.csv(file = paste0(data, "div_tip_discrete.csv"))
tip.div.continuous <- read.csv(file = paste0(data, "div_tip_continuous.csv"))
all.div.continuous <- read.csv(file = paste0(data, "div_tip&node_continuous.csv"))

# Calculate error bars (note not possible with PBDB.div.discrete)
x <- tip.div.discrete
tdd.gen.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$gen.mean.raw - x$gen.SE.raw, 
                    rev(x$gen.mean.raw + x$gen.SE.raw)))
tdd.gen.EB <- na.omit(tdd.gen.EB)
tdd.cl.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$cl.mean.raw - x$cl.SE.raw, 
                   rev(x$cl.mean.raw + x$cl.SE.raw)))
tdd.cl.EB <- na.omit(tdd.cl.EB)
tdd.lh.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$LH.mean.raw - x$LH.SE.raw, 
                    rev(x$LH.mean.raw + x$LH.SE.raw)))
tdd.lh.EB <- na.omit(tdd.lh.EB)

x <- tip.div.continuous
tdc.gen.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$gen.mean.raw - x$gen.SE.raw, 
                                                rev(x$gen.mean.raw + x$gen.SE.raw)))
tdc.gen.EB <- na.omit(tdc.gen.EB)
tdc.cl.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$cl.mean.raw - x$cl.SE.raw, 
                                               rev(x$cl.mean.raw + x$cl.SE.raw)))
tdc.cl.EB <- na.omit(tdc.cl.EB)
tdc.lh.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$LH.mean.raw - x$LH.SE.raw, 
                                               rev(x$LH.mean.raw + x$LH.SE.raw)))
tdc.lh.EB <- na.omit(tdc.lh.EB)

x <- all.div.continuous
adc.gen.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$gen.mean.raw - x$gen.SE.raw, 
                                                rev(x$gen.mean.raw + x$gen.SE.raw)))
adc.gen.EB <- na.omit(adc.gen.EB)
adc.cl.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$cl.mean.raw - x$cl.SE.raw, 
                                               rev(x$cl.mean.raw + x$cl.SE.raw)))
adc.cl.EB <- na.omit(adc.cl.EB)
adc.lh.EB <- cbind(c(x$midpt, rev(x$midpt)), c(x$LH.mean.raw - x$LH.SE.raw, 
                                               rev(x$LH.mean.raw + x$LH.SE.raw)))
adc.lh.EB <- na.omit(adc.lh.EB)
adc.cl.EB.rare <- cbind(c(x$midpt, rev(x$midpt)), c(x$cl.mean - x$cl.SE, 
                                               rev(x$cl.mean + x$cl.SE)))
adc.cl.EB.rare <- na.omit(adc.cl.EB.rare)
adc.lh.EB.rare <- cbind(c(x$midpt, rev(x$midpt)), c(x$LH.mean - x$LH.SE, 
                                               rev(x$LH.mean + x$LH.SE)))
adc.lh.EB.rare <- na.omit(adc.lh.EB.rare)



# Set pdf options (for 2/3 column). Default font family is Arial
tiff(filename = "Fig1_Diversity_110.tif", width = 110, height = 175, units = "mm",
     res = 600)

# Build figure
par(mfrow = c(3, 1))
par(mar = c(6.3, 2.9, 0.7, 1.4))

# Panel A: genus/lineage richness
plot(x$midpt, x$gen.mean.raw, xlim = c(540, 443), ylim = c(0, 250), 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
# mtext(text = "genus / lineage richness", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 1)
palaeoverse::axis_geo(side = 1, height = 0.1, lab_size = list(1, 1.1, 1.1), 
                      cex.axis = 1, abbr = list(TRUE, TRUE, FALSE),
                      intervals = list(stages_new, epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 5.25, cex = 0.85)
mtext(text = "no. of taxa", side = 2, line = 1.6, cex = 0.85)
mtext(text = "A", side = 3, line = -0.7, cex = 1.2, adj = -0.1, outer = FALSE)

polygon(tdd.gen.EB[ ,1], tdd.gen.EB[ ,2], col = trans.cols[2], lwd = 3, border = NA)
polygon(tdc.gen.EB[ ,1], tdc.gen.EB[ ,2], col = trans.cols[4], lwd = 3, border = NA)
polygon(adc.gen.EB[ ,1], adc.gen.EB[ ,2], col = trans.cols[7], lwd = 3, border = NA)

lines(PBDB.div.discrete$midpt, PBDB.div.discrete$genus, lwd = 4, col = cols[1], lty = 1)
lines(tip.div.discrete$midpt, tip.div.discrete$gen.mean.raw, lwd = 4, col = cols[2], lty = 2)
lines(tip.div.continuous$midpt, tip.div.continuous$gen.mean.raw, lwd = 4, col = cols[4], lty = 3)
lines(all.div.continuous$midpt, all.div.continuous$gen.mean.raw, lwd = 4, col = cols[7], lty = 4)

legend("topleft", legend = c("PBDB genus richness (binned)", 
       "genus richness (binned)", "genus richness (continuous)", 
       "lineage richness (continuous)", "lineage richness (continuous) (G = 20)"), 
       box.col = NA, lty = c(1, 2, 3, 4, 5), col = cols[c(1, 2, 4, 7, 5)], 
       cex = 0.97, lwd = 2, pch = NA, inset = .01)

# Panel B: LH richness
expand <- 1.75
# 'expand' controls amount to magnify the second rarefaction curve. Note that
# because we juxtapose a second plot, 'expand' is only used to set the maximum
# axis value, which then controls everything else.
axis.2.labels <- c("0", "", "20")
axis.2.ats <- seq(0, 20, by = 10)

plot(x$midpt, x$lh.mean.raw, xlim = c(540, 443), ylim = c(0, 140), 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
# mtext(text = "life-habit richness", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 1)
palaeoverse::axis_geo(side = 1, height = 0.1, lab_size = list(1, 1.1, 1.1), 
                      cex.axis = 1, abbr = list(TRUE, TRUE, FALSE),
                      intervals = list(stages_new, epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 5.25, cex = 0.85)
mtext(text = "no. of life habits", side = 2, line = 1.6, cex = 0.85)
mtext(text = "B", side = 3, line = -0.7, cex = 1.2, adj = -0.1, outer = FALSE)

polygon(tdd.lh.EB[ ,1], tdd.lh.EB[ ,2], col = trans.cols[2], lwd = 3, border = NA)
polygon(tdc.lh.EB[ ,1], tdc.lh.EB[ ,2], col = trans.cols[4], lwd = 3, border = NA)
polygon(adc.lh.EB[ ,1], adc.lh.EB[ ,2], col = trans.cols[7], lwd = 3, border = NA)
lines(PBDB.div.discrete$midpt, PBDB.div.discrete$LH, lwd = 4, col = cols[1], lty = 1)
lines(tip.div.discrete$midpt, tip.div.discrete$LH.mean.raw, lwd = 4, col = cols[2], lty = 2)
lines(tip.div.continuous$midpt, tip.div.continuous$LH.mean.raw, lwd = 4, col = cols[4], lty = 3)
lines(all.div.continuous$midpt, all.div.continuous$LH.mean.raw, lwd = 4, col = cols[7], lty = 4)

# Add second axis
par(new = TRUE)
plot(all.div.continuous$midpt, all.div.continuous$LH.mean, 
     xlim = c(540, 443), ylim = c(0, 140 / expand), type = "n", axes = FALSE, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", ann = FALSE)
axis(4, lwd.ticks = 0.5, padj = -1.5, cex.axis = 1, labels = axis.2.labels,
     at = axis.2.ats)
mtext(text = "no. (G = 20)", side = 4, line = 0.3, cex = 0.85, at = 90 / expand)

polygon(adc.lh.EB.rare[ ,1], adc.lh.EB.rare[ ,2], 
        col = trans.cols[5], lwd = 2, border = NA)
lines(all.div.continuous$midpt, all.div.continuous$LH.mean, lwd = 4, col = cols[5], lty = 5)


# Panel C: class richness
plot(x$midpt, x$cl.mean.raw, xlim = c(540, 443), ylim = c(0, 20), 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
# mtext(text = "class richness", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 1)
palaeoverse::axis_geo(side = 1, height = 0.1, lab_size = list(1, 1.1, 1.1), 
                      cex.axis = 1, abbr = list(TRUE, TRUE, FALSE),
                      intervals = list(stages_new, epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 5.25, cex = 0.85)
mtext(text = "no. of classes", side = 2, line = 1.6, cex = 0.85)
mtext(text = "C", side = 3, line = -0.7, cex = 1.2, adj = -0.1, outer = FALSE)

polygon(tdd.cl.EB[ ,1], tdd.cl.EB[ ,2], col = trans.cols[2], lwd = 3, border = NA)
polygon(tdc.cl.EB[ ,1], tdc.cl.EB[ ,2], col = trans.cols[4], lwd = 3, border = NA)
polygon(adc.cl.EB[ ,1], adc.cl.EB[ ,2], col = trans.cols[7], lwd = 3, border = NA)
polygon(adc.cl.EB.rare[ ,1], adc.cl.EB.rare[ ,2], col = trans.cols[5], lwd = 3, border = NA)
lines(PBDB.div.discrete$midpt, PBDB.div.discrete$cl, lwd = 4, col = cols[1], lty = 1)
lines(tip.div.discrete$midpt, tip.div.discrete$cl.mean.raw, lwd = 4, col = cols[2], lty = 2)
lines(tip.div.continuous$midpt, tip.div.continuous$cl.mean.raw, lwd = 4, col = cols[4], lty = 3)
lines(all.div.continuous$midpt, all.div.continuous$cl.mean.raw, lwd = 4, col = cols[7], lty = 4)
lines(all.div.continuous$midpt, all.div.continuous$cl.mean, lwd = 4, col = cols[5], lty = 5)

dev.off()




## FIG 2: STACKED CLASSES ######################################################

# Load libraries
library(plotrix)   # v. 3.8-2

# Read and prep data
load(paste0(data, "taxon.list"))
load(paste0(data, "mids"))
load(paste0(data, "class.bins"))
unique.classes <- sort(unique(taxon.list[[1]][, "class"]))

# Set pdf options (for 1 column). Default font family is Arial
tiff(filename = "Fig2_StackedClasses_80.tif", width = 80, height = 80, units = "mm",
     res = 600)

# Build figure
par(mfrow = c(1, 1))
par(mar = c(3.8, 1.8, 0.1, 1.2))

# Choose which classes to identify
cl.table <- table(taxon.list[[1]][, "class"])
sort(cl.table)
class.order <- order((cl.table), decreasing = TRUE)
rownames(class.bins)[class.order]
# Move UNCERTAIN to last
class.order <- class.order[c(1:3, 5:22, 4)]
rownames(class.bins)[class.order]
n.cl <- 12
# Which classes to identify by color: crinoids, edrioasteroids, eocrinoids,
# stylophorans, rhombiferans, cinctans, diploporitans, asteroids, paracrinoids,
# solutes, ctenocystoids, and ophiuroids. Rest will be plotted but white with
# gray borders.
legend.names <- c("crinoids", "edrioasteroids", "eocrinoids", "stylophorans", 
                  "rhombiferans", "cinctans", "diploporitans", "asteroids", 
                  "paracrinoids", "solutes", "ctenocystoids", "ophiuroids", 
                  "other classes")

# Set pleasing, distinctive colors
stack.color <- c(viridisLite::turbo(n.cl), rep("#FFFFFF", 10))
stack.border <- c(rep(NA, n.cl), rep("#888888", 10))

# Plot stacked plot version
plotrix::stackpoly(x = -mids, y = t(class.bins[class.order, ]), 
                   col = stack.color, border = stack.border, lwd = 0.65,
                   xat = 0, xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -480), y = c(135, 230, 230, 135), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.08, lab_size = list(0.7, 0.7), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE), tick_at = axis.ticks, 
                      tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 2.8, cex = 0.75)
legend("topleft", legend = rev(legend.names), pch = rev(c(rep(15, n.cl), 22)), 
       bg = rev(c(rep(NA, n.cl), "white")), ncol = 2, pt.cex = 0.9, cex = .6, 
       col = rev(c(stack.color[seq.int(n.cl)], "#888888")), bty = "n")

dev.off()







## FIG 3: SIZE & TIERING TRENDS ################################################

# Read and prep data
load(paste0(data, "pt.sizes"))
load(paste0(data, "pt.tiers"))
load(paste0(data, "mids"))
load(paste0(data, "median.ranges"))
group <- 
  read.csv(file=paste0(data, "EchinoLHData_Mode.csv"), header=TRUE, stringsAsFactors=FALSE)

# Summarize sizes, tiers, and stratigraphic ranges across trees
sizes <- apply(simplify2array(pt.sizes), 1:2, mean, na.rm = TRUE)
sizes.sd <- apply(simplify2array(pt.sizes), 1:2, sd, na.rm = TRUE)
tiers <- apply(simplify2array(pt.tiers), 1:2, mean, na.rm = TRUE)
tiers.sd <- apply(simplify2array(pt.tiers), 1:2, sd, na.rm = TRUE)

# Only plot tiering for filter feeders
wh.ff <- which(group$FilterFeeder == 1)
wh.inf <- which(group$AbsStratDistance < 0)



# Set pdf options (for full page). Default font family is Arial
tiff(filename = "Fig3_Size&Tiering_166.tif", width = 166, height = 110, units = "mm",
     res = 600)

# Build figure
par(mfrow = c(1, 2))
par(mar = c(5.3, 2.45, 0.75, 0.2))

## Plot size trend, with individual genus sizes and mean/median trend (only
## using tips)
lim <- extendrange(log10(group$BodyVolume), f = 0.01)
plot(mids, rep(lim[1], length(mids)), xlim = c(540, 443), ylim = lim, 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
# mtext(text = "body size", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 0.75)
palaeoverse::axis_geo(side = 1, height = 0.08, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.75, abbr = list(TRUE, FALSE),
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 4.3, cex = 0.75)
mtext(text = "log10 body volume (cm3)", side = 2, line = 1.6, cex = 0.75)
mtext(text = "A", side = 3, line = -0.2, cex = 1.2, adj = -0.18, outer = FALSE)

mean.column <- cbind(c(mids, rev(mids)), 
                     c(sizes[, "mean"] - sizes.sd[, "mean"], 
                       rev(sizes[, "mean"] + sizes.sd[, "mean"])))
mean.column <- na.omit(mean.column)
polygon(mean.column[ ,1], mean.column[ ,2], col = "#7F7F7F80", lwd = 2, border = NA)
for(tip in 1:366) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(log10(group$BodyVolume[tip]), 2), border = "gray35")
}
# Truncate trends at Cambrian (because tree #25 extends Ctenoimbricata into
# Ediacaran, and similarly forcing trends)
lines(mids[1:98], sizes[1:98, "mean"], lwd = 3, col = cols[2])
lines(mids[1:98], sizes[1:98, "median"], lwd = 3, lty = 3, col = cols[7])
legend("topleft", legend = c("mean", "median"), bty = "n", lty = c(1, 3),
       pt.cex = 2, lwd = 2, col = cols[c(2, 7)], inset = c(0.01, -0.02), 
       cex = 0.75)


# Now for tiers
lim <- c(-15, 1000)
median.column <- cbind(c(mids, rev(mids)), 
                       10 ^ (c(tiers[, "median"], rep(0, length(mids)))))
median.column <- na.omit(median.column)
column.75th <- cbind(c(mids, rev(mids)), 
                     10 ^ (c(tiers[, "75th"], rep(0, length(mids)))))
column.75th <- na.omit(column.75th)
column.99th <- cbind(c(mids, rev(mids)), 
                     10 ^ (c(tiers[, "99th"], rep(0, length(mids)))))
column.99th <- na.omit(column.99th)

plot(mids, rep(lim[1], length(mids)), xlim = c(540, 443), ylim = lim, 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
# mtext(text = "tiering", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 0.75)
palaeoverse::axis_geo(side = 1, height = 0.08, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.75, abbr = list(TRUE, FALSE),
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 4.3, cex = 0.75)
mtext(text = "distance from seafloor (mm)", side = 2, line = 1.6, cex = 0.75)
mtext(text = "B", side = 3, line = -0.2, cex = 1.2, adj = -0.18, outer = FALSE)

polygon(column.99th[ ,1], column.99th[ ,2], col = trans.cols[2], lwd = 2, border = NA)
polygon(column.75th[ ,1], column.75th[ ,2], col = trans.cols[5], lwd = 2, border = NA)
polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
for(tip in wh.ff) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(group$AbsStratDistance[tip], 2), border = "gray35")
}
# Highlight infaunal ranges
for(tip in wh.inf) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(group$AbsStratDistance[tip], 2), border = cols[7])
}
abline(h = 0, lwd = 2)
legend("topleft", legend = c("99th %ile", "75th %ile", "median", "infaunal"), 
       pch = c(22, 22, 22, NA), pt.cex = 2, pt.bg = c(trans.cols[c(2, 5, 7)], NA), 
       col = c("white", "white", "white", cols[7]), bty = "n",
       lty = c(0, 0, 0, 1), lwd = c(0, 0, 0, 3), inset = c(0.01, -0.02),
       cex = 0.75)

dev.off()






## FIG 4: HABITAT & MOBILITY TRENDS ############################################

# Load libraries
library(plotrix)   # v. 3.8-2

# Read and prep data
load(paste0(data, "mids"))
load(paste0(data, "habitat.pt.traits"))
load(paste0(data, "mobility.pt.traits"))

# Summarize statistics across trees
h.trait.occs <- 
  apply(simplify2array(habitat.pt.traits), 1:2, mean, na.rm = TRUE)
h.trait.occs.SE <- 
  apply(simplify2array(habitat.pt.traits), 1:2, sd, na.rm = TRUE)
h.mean.SE <- mean(h.trait.occs.SE, na.rm = TRUE)
m.trait.occs <- 
  apply(simplify2array(mobility.pt.traits), 1:2, mean, na.rm = TRUE)
m.trait.occs.SE <- 
  apply(simplify2array(mobility.pt.traits), 1:2, sd, na.rm = TRUE)
m.mean.SE <- mean(m.trait.occs.SE, na.rm = TRUE)

h.trait.long <- c("infaunal", "semi-infaunal", "short epifaunal",
                "intermediate epifaunal", "tall epifaunal", "epibiotic", 
                "pelagic")
m.trait.long <- c("sedentary", "facultatively mobile", "intermittently mobile", 
                  "habitually mobile", "passively mobile")



# Set pdf options (for full page). Default font family is Arial
tiff(filename = "Fig4_Habitats&Mobility_166.tif", width = 166, height = 80, 
     units = "mm", res = 600)

# Build figure
par(mfrow = c(1, 2))
par(mar = c(3.8, 1.8, 0.35, 1.3))

# Plot stacked plot version for habitat
plotrix::stackpoly(x = -mids, y = h.trait.occs, col = cols, xat = 0, 
                   xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -496), y = c(140, 300, 300, 140), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.7, 0.7), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 2.8, cex = 0.75)
mtext(text = "A", side = 3, line = -0.6, cex = 1.2, adj = -0.14, outer = FALSE)
legend("topleft", legend = rev(h.trait.long), pch = 15, 
       col = cols[length(h.trait.long):1], ncol = 1, 
       pt.cex = 1.65, cex = .65, bty = "n")
SE.start <- 145
SE.end <- SE.start + h.mean.SE
SE.time <- -537.5
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1.5, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

# Plot stacked plot version for mobility
plotrix::stackpoly(x = -mids, y = m.trait.occs, col = cols, xat = 0, 
                   xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -496), y = c(140, 300, 300, 140), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.7, 0.7), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 2.8, cex = 0.75)
mtext(text = "B", side = 3, line = -0.6, cex = 1.2, adj = -0.14, outer = FALSE)
legend("topleft", legend = rev(m.trait.long), pch = 15, 
       col = cols[length(m.trait.long):1], ncol = 1, 
       pt.cex = 1.65, cex = .65, bty = "n")
SE.start <- 154
SE.end <- SE.start + m.mean.SE
SE.time <- -537.5
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1.5, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

dev.off()




## FIG 5: DIET AND FORAGING TRENDS ##########################################################

# Load libraries
library(plotrix)   # v. 3.8-2

# Read and prep data
load(paste0(data, "mids"))
load(paste0(data, "diet.pt.traits"))
load(paste0(data, "foraging.pt.traits"))

# Summarize statistics across trees
d.trait.occs <- 
  apply(simplify2array(diet.pt.traits), 1:2, mean, na.rm = TRUE)
d.trait.occs.SE <- 
  apply(simplify2array(diet.pt.traits), 1:2, sd, na.rm = TRUE)
d.mean.SE <- mean(d.trait.occs.SE, na.rm = TRUE)
f.trait.occs <- 
  apply(simplify2array(foraging.pt.traits), 1:2, mean, na.rm = TRUE)
f.trait.occs.SE <- 
  apply(simplify2array(foraging.pt.traits), 1:2, sd, na.rm = TRUE)
f.mean.SE <- mean(f.trait.occs.SE, na.rm = TRUE)

d.trait.long <- c("autotroph", "herbivore", "microbivore", "carnivore")
f.trait.long <- c("absorptive", "unknown-density filterer", 
                  "low-density filterer", "medium-density filterer", 
                  "high-density filterer", "mass-feeder", "attachment feeder", 
                  "raptorial feeder")


# Set pdf options (for full page). Default font family is Arial
tiff(filename = "Fig5_Diet&Foraging_166.tif", width = 166, height = 80, units = "mm",
     res = 600)

# Build figure
par(mfrow = c(1, 2))
par(mar = c(3.8, 1.8, 0.35, 1.3))

# Plot stacked plot version (colors reordered to match diet)
plotrix::stackpoly(x = -mids, y = d.trait.occs, col = cols[c(5, 4, 2, 7)], 
                   xat = 0, xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -496), y = c(170, 300, 300, 170), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.7, 0.7), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 2.8, cex = 0.75)
mtext(text = "A", side = 3, line = -0.6, cex = 1.2, adj = -0.14, outer = FALSE)
legend("topleft", legend = rev(d.trait.long), pch = 15, 
       col = cols[c(7, 2, 4, 5)], ncol = 1, 
       pt.cex = 1.65, cex = .65, bty = "n")
SE.start <- 178
SE.end <- SE.start + d.mean.SE
SE.time <- -537.5
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1.5, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

# Plot stacked plot version for foraging
plotrix::stackpoly(x = -mids, y = f.trait.occs, col = cols, xat = 0, 
                   xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -496), y = c(125, 300, 300, 125), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.7, 0.7), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 2.8, cex = 0.75)
mtext(text = "B", side = 3, line = -0.6, cex = 1.2, adj = -0.14, outer = FALSE)
legend("topleft", legend = rev(f.trait.long), pch = 15, 
       col = cols[length(f.trait.long):1], ncol = 1, 
       pt.cex = 1.65, cex = .65, bty = "n")
SE.start <- 123
SE.end <- SE.start + f.mean.SE
SE.time <- -537.5
lines(x = c(SE.time, SE.time), y = c(SE.start, SE.end), lwd = 1.5, lty = 1, col = "gray35")
points(x = SE.time, y = SE.start, pch = "-", cex = .75, col = "gray35")
points(x = SE.time, y = SE.end, pch = "-", cex = .75, col = "gray35")

dev.off()






## FIG 6 (& S6): PHYLOECOSPACE #################################################

# Load libraries
library(phytools)   # v. 1.9-16
library(ape)        # v. 5.7-1
if(packageVersion("ape") < "5.5.2")
  stop("outdated version of 'ape'. Get updated version from GitHub or CRAN.\n")

# Read and prep data
load(paste0(data, "mode.pcoa.49")); mode.pcoa <- mode.pcoa.49 # UEH tree no. 49 
# load(paste0(data, "mode.pcoa")) # Note only based on tree no. 57 (EAT)
load(paste0(data, "mode.anc"))
load(paste0(data, "ranges"))
load(paste0(data, "taxon.list"))

# Manually built new 'ages' data.frame by combining epochs.
bins <- c("Ordovician", "Furongian", "Series 2–Miaolingian", "Terreneuvian")
max_ma <- c(485.4, 497, 521, 541)
min_ma <- c(443.4, 485.4, 497, 521)
ages <- data.frame(interval_name = bins, max_ma = max_ma, min_ma = min_ma)

# Need to reconstruct a different 'taxon.bin' object because combining epochs.
# Simplified because only plotting for tree #50.
tr <- 49 # for UEH topology (fig. 6)
# tr <- 57  # for EAT topology (fig. S6)
tree <- mode.anc[[tr]]$topper$tree
binned.taxon.bins <- matrix(FALSE, nrow = (ape::Ntip(tree) + ape::Nnode(tree)),
                            ncol = nrow(ages))
colnames(binned.taxon.bins) <- ages$interval_name
rownames(binned.taxon.bins) <- rownames(mode.anc[[tr]]$matrix_1$matrix)
for (i in 1:ncol(binned.taxon.bins)) {
  wh.row <- which(ranges[[tr]][, 1] > ages$min_ma[i] &
                    ranges[[tr]][, 2] < ages$max_ma[i])
  binned.taxon.bins[wh.row, i] <- TRUE
}
head(binned.taxon.bins)

# Choose which classes to plot (remainder combined into default tip color)
taxon.list <- taxon.list[[tr]]
cl.labels <- c("Crinoidea", "Eocr/Rhomb/Diplo/Paracr", "Stylo/Homo/Solut/Cteno", 
               "Ast/Oph/Ech/Somas/Sten", "Edrioasteroidea")

# And assign colors (viridisLite::turbo(6), converted to transparent)
cl.tip.cols <- 
  c("#30123BAA", "#3E9BFEAA", "#46F884AA", "#E1DD37AA", "#F05B12AA")
cl.node.cols <- 
  c("#30123B66", "#3E9BFE66", "#46F88466", "#E1DD3766", "#F05B1266")
# Default color is last rev(viridisLite::turbo(6)) color for everything else
other.tip.cols <- "#7A0403AA"
other.node.cols <- "#7A040366"
    
phylo.cols <- rep(other.node.cols, (ape::Ntip(tree) + ape::Nnode(tree)))
phylo.cols[1:ape::Ntip(tree)] <- other.tip.cols
for(cl in 1:5) {
  if (cl == 1)
    wh.class <- which(taxon.list[, "class"] == "Crinoidea")
  if (cl == 2)
    wh.class <- which(taxon.list[, "class"] == "Eocrinoidea" |
                        taxon.list[, "class"] == "Rhombifera" | 
                        taxon.list[, "class"] == "'diploporitan'" | 
                        taxon.list[, "class"] == "Diploporita" | 
                        taxon.list[, "class"] == "Paracrinoidea")
  if (cl == 3)
    wh.class <- which(taxon.list[, "class"] == "Stylophora" |
                        taxon.list[, "class"] == "Homostelea" | 
                        taxon.list[, "class"] == "Soluta" | 
                        taxon.list[, "class"] == "Ctenocystoidea" | 
                        taxon.list[, "class"] == "Ctenoimbricata")
  if (cl == 4)
    wh.class <- which(taxon.list[, "class"] == "Asteroidea" |
                        taxon.list[, "class"] == "Ophiuroidea" | 
                        taxon.list[, "class"] == "Echinoidea" | 
                        taxon.list[, "class"] == "Somasteroidea" | 
                        taxon.list[, "class"] == "Stenuroidea")
  if (cl == 5)
    wh.class <- which(taxon.list[, "class"] == "Edrioasteroidea")
  
  phylo.cols[wh.class[wh.class <= ape::Ntip(tree)]] <- cl.tip.cols[cl]
  phylo.cols[wh.class[wh.class > ape::Ntip(tree)]] <- cl.node.cols[cl] 
}
# Confirm
table(phylo.cols)



# Set pdf options (for full page). Default font family is Arial
tiff(filename = "Fig6_Phyloecospace_166.tif", width = 166, height = 226, units = "mm", res = 600)
# tiff(filename = "FigS6_Phyloecospace_166.tif", width = 166, height = 226, units = "mm", res = 600)

# Use layout() because of more complicated panel layout
layout.matrix <- matrix(c(7, 5, 3, 1, 9, 8, 6, 4, 2, 10), nrow = 5, ncol = 2)
layout(layout.matrix, heights = c(1, 1, 1, 1, .6))
par(mar = c(2.75, 3, 1.25, 0.25), pty = "m")

# Plotting settings
pt.cex <- 2
cex.lab <- 1
cex.axis <- .75
branch.col <- "gray80"
tree <- mode.pcoa$tree
tip.seq <- seq.int(ape::Ntip(tree))
node.seq <-(ape::Ntip(tree) + 1):(ape::Ntip(tree) + ape::Nnode(tree))
con <- list(col.edge = setNames(rep(branch.col, nrow(tree$edge)), 
                                as.character(tree$edge[, 2])))

nt <- ncol(binned.taxon.bins)
for(t in nt:1) {
  wh.gr <- unname(which(binned.taxon.bins[, t]))
  wh.tip <- wh.gr[which(wh.gr <= ape::Ntip(tree))]
  wh.node <- wh.gr[which(wh.gr > ape::Ntip(tree))]
  
  phytools::phylomorphospace(tree = tree, X = mode.pcoa$vectors.cor[tip.seq, 1:2], 
                             A = mode.pcoa$vectors.cor[node.seq, 1:2], 
                             control = con, label = "off", xlab = "", 
                             ylab = "", pch = NA, axes = FALSE)
  axis(1, cex.axis = 0.9)
  axis(2, cex.axis = 0.9)
  mtext(paste(colnames(binned.taxon.bins)[t], "ecospace"), cex = cex.lab)
  mtext(text = "PCO 2", side = 2, cex = cex.axis, line = 1.75)
  mtext(text = "PCO 1", side = 1, cex = cex.axis, line = 1.75)
  vpts <- chull(mode.pcoa$vectors.cor[wh.gr, 1:2])
  vpts <- c(vpts, vpts[1])
  # polygon(mode.pcoa$vectors.cor[wh.gr[vpts], 1:2], col = "skyblue")
  if (t %in% c(1, 3)) {
    lines(mode.pcoa$vectors.cor[wh.gr.prior[vpts.12], 1:2], lty = 2, lwd = 1.5, 
          col = "gray60")
  }
  lines(mode.pcoa$vectors.cor[wh.gr[vpts], 1:2], lty = 1, lwd = 1.5, 
        col = "gray40")
  vpts.12 <- vpts
  points(x = mode.pcoa$vectors.cor[wh.node, 1],
         y = mode.pcoa$vectors.cor[wh.node, 2], col = phylo.cols[wh.node],
         pch = 16, cex = pt.cex)
  points(x = mode.pcoa$vectors.cor[wh.tip, 1], 
         y = mode.pcoa$vectors.cor[wh.tip, 2], col = phylo.cols[wh.tip],
         pch = 16, cex = pt.cex)
  
  phytools::phylomorphospace(tree = tree, X = mode.pcoa$vectors.cor[tip.seq, 3:4], 
                             A = mode.pcoa$vectors.cor[node.seq, 3:4], 
                             control = con, label = "off", xlab = "", 
                             ylab = "", pch = NA, axes = FALSE)
  axis(1, cex.axis = 0.9)
  axis(2, cex.axis = 0.9)
  mtext(paste(colnames(binned.taxon.bins)[t], "ecospace"), cex = cex.lab)
  mtext(text = "PCO 4", side = 2, cex = cex.axis, line = 1.75)
  mtext(text = "PCO 3", side = 1, cex = cex.axis, line = 1.75)
  vpts <- chull(mode.pcoa$vectors.cor[wh.gr, 3:4])
  vpts <- c(vpts, vpts[1])
  # polygon(mode.pcoa$vectors.cor[wh.gr[vpts], 3:4], col = "skyblue")
  if (t %in% c(1, 3)) {
    lines(mode.pcoa$vectors.cor[wh.gr.prior[vpts.34], 3:4], lty = 2, lwd = 1.5, 
          col = "gray60")
  }
  lines(mode.pcoa$vectors.cor[wh.gr[vpts], 3:4], lty = 1, lwd = 1.5, 
        col = "gray40")
  vpts.34 <- vpts
  points(x = mode.pcoa$vectors.cor[wh.node, 3],
         y = mode.pcoa$vectors.cor[wh.node, 4], col = phylo.cols[wh.node],
         pch = 16, cex = pt.cex)
  points(x = mode.pcoa$vectors.cor[wh.tip, 3], 
         y = mode.pcoa$vectors.cor[wh.tip, 4], col = phylo.cols[wh.tip],
         pch = 16, cex = pt.cex)
  
  wh.gr.prior <- wh.gr
}

# Overlay strategies
# Group 1 ranges x(+0.94, +2.52) to y(-1.61, +2.44)
# Group 2 ranges x(-1.25, +0.70) to y(-2.65, +1.46)
# Group 3 ranges x(-3.39, +0.73) to y(-2.37, +1.52)
# Group 4 ranges x(-3.98, -1.53) to y(-0.33, +3.71)
con2 <- list(col.edge = setNames(rep("white", nrow(tree$edge)), 
                                as.character(tree$edge[, 2])))
phytools::phylomorphospace(tree = tree, X = mode.pcoa$vectors.cor[tip.seq, 1:2], 
                           A = mode.pcoa$vectors.cor[node.seq, 1:2], 
                           control = con2, label = "off", xlab = "", 
                           ylab = "", pch = NA, axes = FALSE)
axis(1, cex.axis = 0.9)
axis(2, cex.axis = 0.9)
mtext(text = "PCO 2", side = 2, cex = cex.axis, line = 1.75)
mtext(text = "PCO 1", side = 1, cex = cex.axis, line = 1.75)
polygon(x = c(1, 2.6, 2.75, 1.5, 1, 1), y = c(-2.5, -1.5, 3, 3, 1.5, -2.5), col = "gray75", 
        border = "gray75")
text("1", x = 1.875, y = 0.25, cex = 3)
polygon(x = c(-1.5, .75, .9, -.5, -1.5), y = c(-3, -2, 2, 2, -2.7), col = "gray75", 
        border = "gray75")
text("2", x = -.1, y = -0.1, cex = 3)
polygon(x = c(-3.5, -1.5, -0.75, -1.25, -3.5, -3.5), y = c(-2.5, -2, 2, 2, -1, -2.5), col = "gray75", 
        border = "gray75")
text("3", x = -2, y = -0.5, cex = 3)
polygon(x = c(-4, -1, -3, -4, -4), y = c(-1, 3, 4, 2, -1), col = "gray75", 
        border = "gray75")
text("4", x = -3, y = 2, cex = 3)


# Add legend
legend.cols <- viridisLite::turbo(6)
leg.text <- c(expression("crinoids"),
              expression("eocrinoids, rhombiferans, diploporitans & paracrinoids"), 
              expression(paste("stylophorans, cinctans, solutes, ctenocystoids & ", 
                               italic("Ctenoimbricata"))),
              expression("asteroids, ophiuroids, echinoids, somasteroid & stenuroids"),
              expression("edrioasteroids"),
              expression("others"))
par(mar = c(0, 0, 0, 0))
plot(1, type = "n", axes = FALSE, xlab="", ylab = "")
legend("left", title = "", legend = leg.text, cex = 0.95, pch = 16, 
       col = legend.cols, bty = "n", pt.cex = 2.25, ncol = 1)

dev.off()







## FIG S1: SUB-SAMPLED SIZE & TIERING TRENDS & PBDB VERSIONS ###################

# Read and prep data
sub.sizes <- read.csv(file = paste0(data, "stdG24_size_tip_continuous.csv"))
sub.tiers <- read.csv(file = paste0(data, "stdG16_tier_tip_continuous.csv"))
load(paste0(data, "mids"))
load(paste0(data, "PBDB.mids"))
# Because no Terreneuvian nor Ediacaran tip taxa, remove here
PBDB.mids <- PBDB.mids[1:15]
load(paste0(data, "median.ranges"))
PBDB.ranges <- read.csv(paste0(data, "GenusStratRanges.csv"), header = TRUE)
strat_names <- read.csv(paste0(data, "strat_names.csv"))
# Eons are level 1, eras = level 2, periods = 3, epochs = 4, ages = 5
ages <- strat_names[which(strat_names$scale_level == 5), ]
# Limit to Cambrian and Ordovician
ages <- ages[which(ages$max_ma > 444), ]
# Add Ediacaran for any pre-Cambrian nodes
ages <-
  rbind(ages, strat_names[which(strat_names$interval_name == "Ediacaran"),])
top <- ages$min_ma
base <- ages$max_ma
group <- 
  read.csv(file = paste0(data, "EchinoLHData_Mode.csv"), header=TRUE, stringsAsFactors=FALSE)
# Only plot tiering for filter feeders
wh.ff <- which(group$FilterFeeder == 1)
wh.inf <- which(group$AbsStratDistance < 0)


# Set pdf options (for full page). Default font family is Arial
tiff(filename = "FigS1_SubSize&Tiering_166.tif", width = 166, height = 166, 
     units = "mm", res = 600)

# Build figure
par(mfrow = c(2, 2))
par(mar = c(5.3, 2.45, 1.0, 0.2))

# A. Plot size trend, with individual genus sizes and mean/median trend (only
# using tips)

# Summarize continuous, sample-standardized body-size trend, tips only
mean.column <- cbind(c(sub.sizes$midpt, rev(sub.sizes$midpt)),
                     c(sub.sizes$mean.size - sub.sizes$SE.mean, rev(sub.sizes$mean.size + sub.sizes$SE.mean)))
mean.column <- na.omit(mean.column)
median.column <- cbind(c(sub.sizes$midpt, rev(sub.sizes$midpt)),
                       c(sub.sizes$median.size - sub.sizes$SE.median, 
                         rev(sub.sizes$median.size + sub.sizes$SE.median)))
median.column <- na.omit(median.column)

lim <- extendrange(log10(group$BodyVolume), f = 0.01)
plot(mids, rep(lim[1], length(mids)), xlim = c(540, 443), ylim = lim, 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
mtext(text = "body size (24 genera / interval)", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 0.75)
palaeoverse::axis_geo(side = 1, height = 0.08, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.75, abbr = list(TRUE, FALSE),
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 4.3, cex = 0.75)
mtext(text = "log10 body volume (cm3)", side = 2, line = 1.6, cex = 0.75)
mtext(text = "A", side = 3, line = -0.1, cex = 1.2, adj = -0.15, outer = FALSE)
for(tip in 1:366) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(log10(group$BodyVolume[tip]), 2), border = "gray35")
}
polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
polygon(mean.column[ ,1], mean.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
lines(sub.sizes$midpt, sub.sizes$mean.size, lty = 1, lwd = 3, col = cols[2])
lines(sub.sizes$midpt, sub.sizes$median.size, lty = 3, lwd = 3, col = cols[7])
legend("topleft", legend = c("mean", "median"), bty = "n", lty = c(1, 3),
       pt.cex = 3, lwd = 3, col = cols[c(2, 7)], inset = c(0.01, -0.02))


# B. Same with tiering: Summarize continuous, sample-standardized body-size
# trend, tips only
mean.column <- cbind(c(sub.tiers$midpt, rev(sub.tiers$midpt)),
                     c(sub.tiers$mean.tier - sub.tiers$SE.mean, rev(sub.tiers$mean.tier + sub.tiers$SE.mean)))
mean.column <- na.omit(mean.column)
median.column <- cbind(c(sub.tiers$midpt, rev(sub.tiers$midpt)),
                       c(sub.tiers$median.tier - sub.tiers$SE.median, 
                         rev(sub.tiers$median.tier + sub.tiers$SE.median)))
median.column <- na.omit(median.column)
column.75th <- cbind(c(sub.tiers$midpt, rev(sub.tiers$midpt)),
                     c(sub.tiers$tier.75th - sub.tiers$SE.75th, 
                       rev(sub.tiers$tier.75th + sub.tiers$SE.75th)))
column.75th <- na.omit(column.75th)
column.99th <- cbind(c(sub.tiers$midpt, rev(sub.tiers$midpt)),
                     c(sub.tiers$tier.99th - sub.tiers$SE.99th, 
                       rev(sub.tiers$tier.99th + sub.tiers$SE.99th)))
column.99th <- na.omit(column.99th)
wh.ff <- which(group$FilterFeeder == 1)

lim <- c(-15, 1000)
plot(mids, rep(lim[1], length(mids)), xlim = c(540, 443), ylim = lim, 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
mtext(text = "tiering (16 genera / interval)", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 0.75)
palaeoverse::axis_geo(side = 1, height = 0.08, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.75, abbr = list(TRUE, FALSE),
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 4.3, cex = 0.75)
mtext(text = "distance from seafloor (mm)", side = 2, line = 1.6, cex = 0.75)
mtext(text = "B", side = 3, line = -0.1, cex = 1.2, adj = -0.15, outer = FALSE)
for(tip in wh.ff) {
  polygon(c(median.ranges[tip, "FAD"], median.ranges[tip, "LAD"]), 
          rep(group$AbsStratDistance[tip], 2), border = "gray35")
}
polygon(column.99th[ ,1], 10 ^ column.99th[ ,2], col = trans.cols[2], lwd = 2, border = NA)
polygon(column.75th[ ,1], 10 ^ column.75th[ ,2], col = trans.cols[5], lwd = 2, border = NA)
polygon(median.column[ ,1], 10 ^ median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
polygon(mean.column[ ,1], 10 ^ mean.column[ ,2], col = trans.cols[8], lwd = 2, border = NA)
lines(sub.tiers$midpt, 10 ^ sub.tiers$tier.99th, lty = 2, lwd = 3, col = cols[2])
lines(sub.tiers$midpt, 10 ^ sub.tiers$tier.75th, lty = 4, lwd = 3, col = cols[5])
lines(sub.tiers$midpt, 10 ^ sub.tiers$mean.tier, lty = 1, lwd = 3, col = cols[8])
lines(sub.tiers$midpt, 10 ^ sub.tiers$median.tier, lty = 3, lwd = 3, col = cols[7])
abline(h = 0, lwd = 1)
legend("topleft", legend = c("99%ile", "75%ile", "median (50%ile)", "mean"), 
       bty = "n", lty = c(2, 4, 3, 1), col = cols[c(2, 5, 7, 8)], lwd = 3, 
       inset = c(0.01, -0.02))

# C. Body volume, using raw PBDB ranges
lim <- extendrange(log10(group$BodyVolume), f = 0.01)
plot(PBDB.mids, rep(lim[1], length(PBDB.mids)), xlim = c(540, 443), ylim = lim, 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
mtext(text = "body size (PBDB ranges)", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 0.75)
palaeoverse::axis_geo(side = 1, height = 0.08, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.75, abbr = list(TRUE, FALSE),
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 4.3, cex = 0.75)
mtext(text = "log10 body volume (cm3)", side = 2, line = 1.6, cex = 0.75)
mtext(text = "C", side = 3, line = -0.1, cex = 1.2, adj = -0.15, outer = FALSE)

# Calculate mean and median trends
sizes <- matrix(NA, nrow = length(PBDB.mids), ncol = 3)
rownames(sizes) <- ages$interval_name[1:15]
colnames(sizes) <- c("mean", "median", "sd")
for (t in 1:length(PBDB.mids)) {
  wh.time <- which(PBDB.ranges[, "max_ma"] > top[t] &
                     PBDB.ranges[, "min_ma"] < base[t])
  if (length(wh.time) == 0L)
    next
  wh.sizes <- log10(group$BodyVolume[wh.time])
  sizes[t, "mean"] <- mean(wh.sizes, na.rm = TRUE)
  sizes[t, "median"] <- median(wh.sizes, na.rm = TRUE)
  sizes[t, "sd"] <- sd(wh.sizes, na.rm = TRUE)
}
mean.column <- cbind(c(PBDB.mids, rev(PBDB.mids)), 
                     c(sizes[, "mean"] - sizes[, "sd"], 
                       rev(sizes[, "mean"] + sizes[, "sd"])))
mean.column <- na.omit(mean.column)
polygon(mean.column[ ,1], mean.column[ ,2], col = "#DDDDDD", lwd = 2, border = NA)
for(tip in 1:366) {
  polygon(c(PBDB.ranges[tip, "max_ma"], PBDB.ranges[tip, "min_ma"]), 
          rep(log10(group$BodyVolume[tip]), 2), border = "gray35")
}
lines(PBDB.mids, sizes[, "mean"], lwd = 3, col = cols[2])
lines(PBDB.mids, sizes[, "median"], lwd = 3, lty = 3, col = cols[7])

# D. Now for tiers
lim <- c(-15, 1000)
plot(PBDB.mids, rep(lim[1], length(PBDB.mids)), xlim = c(540, 443), ylim = lim, 
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
box()
mtext(text = "tiering (PBDB ranges)", side = 3, cex = 0.95)
axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 0.75)
palaeoverse::axis_geo(side = 1, height = 0.08, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.75, abbr = list(TRUE, FALSE),
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                  "Llandovery", "Silurian"))
mtext(text = "time (Ma)", side = 1, line = 4.3, cex = 0.75)
mtext(text = "distance from seafloor (mm)", side = 2, line = 1.6, cex = 0.75)
mtext(text = "D", side = 3, line = -0.1, cex = 1.2, adj = -0.15, outer = FALSE)
# Calculate statistics
tiers <- matrix(NA, nrow = length(PBDB.mids), ncol = 4)
rownames(tiers) <- ages$interval_name[1:15]
colnames(tiers) <- c("mean", "median", "75th", "99th")
for (t in 1:length(PBDB.mids)) {
  wh.time <- which(PBDB.ranges[wh.ff, "max_ma"] > top[t] &
                     PBDB.ranges[wh.ff, "min_ma"] < base[t])
  if (length(wh.time) == 0L)
    next
  wh.tiers <- log10(group$AbsStratDistance[wh.ff[wh.time]])
  tiers[t, "mean"] <- mean(wh.tiers, na.rm = TRUE)
  tiers[t, "median"] <- median(wh.tiers, na.rm = TRUE)
  tiers[t, "75th"] <- quantile(wh.tiers, probs = 0.75, na.rm = TRUE)
  tiers[t, "99th"] <- quantile(wh.tiers, probs = 0.99, na.rm = TRUE)
}
median.column <- cbind(c(PBDB.mids, rev(PBDB.mids)), 
                       10 ^ (c(tiers[, "median"], rep(0, length(PBDB.mids)))))
median.column <- na.omit(median.column)
column.75th <- cbind(c(PBDB.mids, rev(PBDB.mids)), 
                     10 ^ (c(tiers[, "75th"], rep(0, length(PBDB.mids)))))
column.75th <- na.omit(column.75th)
column.99th <- cbind(c(PBDB.mids, rev(PBDB.mids)), 
                     10 ^ (c(tiers[, "99th"], rep(0, length(PBDB.mids)))))
column.99th <- na.omit(column.99th)
polygon(column.99th[ ,1], column.99th[ ,2], col = trans.cols[2], lwd = 2, border = NA)
polygon(column.75th[ ,1], column.75th[ ,2], col = trans.cols[5], lwd = 2, border = NA)
polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
for(tip in wh.ff) {
  polygon(c(PBDB.ranges[tip, "max_ma"], PBDB.ranges[tip, "min_ma"]), 
          rep(group$AbsStratDistance[tip], 2), border = "gray35")
}
# Highlight infaunal ranges
for(tip in wh.inf) {
  polygon(c(PBDB.ranges[tip, "max_ma"], PBDB.ranges[tip, "min_ma"]), 
          rep(group$AbsStratDistance[tip], 2), border = cols[7])
}
abline(h = 0, lwd = 2)


dev.off()





## FIG S2: MODEL SUPPORT #######################################################

# Load libraries
library(plotrix)   # v. 3.8-2

# Read and prep data
load(paste0(data, "GRWs.size"))
load(paste0(data, "URWs.size"))
load(paste0(data, "GRWs.tier"))
load(paste0(data, "URWs.tier"))



# Build figure
tiff(filename = "FigS2_ModelFits_166.tif", width = 166, height = 80, units = "mm",
     res = 600)

par(mfrow = c(1, 2))
par(mar = c(3, 2.7, 1.2, 0.67))

# Plot model support as stacked plot
GRWs <- GRWs.size
URWs <- URWs.size
model.order <- order(URWs)
model.matrix <- data.frame(URW = URWs[model.order], GRW = GRWs[model.order])
stackpoly(x = 1:100, y = model.matrix, col = cols[c(2, 5)],
          xat = c(1, seq(from = 10, to = 100, by = 10)), xlim = c(1, 100), 
          ylim = c(0, 1), xlab = "", ylab = "", stack = TRUE, 
          axis4 = FALSE, main = "")
abline(h = 0.9, col = "white", lty = 5, lwd = 2)
abline(h = 0.5, col = "white", lty = 5, lwd = 2)
mtext(text = "body size", side = 3, cex = 1.3)
mtext(text = "tree", side = 1, line = 1.8, cex = 1)
mtext(text = "Akaike weight", side = 2, line = 1.8, cex = 1)
mtext(text = "A", side = 3, line = 0.25, cex = 1.2, adj = -0.21, outer = FALSE)
legend("bottomleft", legend = c("General (biased) RW", "Unbiased RW"), pch = 22, 
       pt.cex = 2, cex = 1, pt.bg = cols[c(5, 2)], col = "white", bty = "n")

# Plot model support as stacked plot
GRWs <- GRWs.tier
URWs <- URWs.tier
model.order <- order(URWs)
model.matrix <- data.frame(URW = URWs[model.order], GRW = GRWs[model.order])
stackpoly(x = 1:100, y = model.matrix, col = cols[c(2, 5)],
          xat = c(1, seq(from = 10, to = 100, by = 10)), xlim = c(1, 100), 
          ylim = c(0, 1), xlab = "", ylab = "", stack = TRUE, 
          axis4 = FALSE, main = "")
abline(h = 0.9, col = "white", lty = 5, lwd = 2)
abline(h = 0.5, col = "white", lty = 5, lwd = 2)
mtext(text = "tiering", side = 3, cex = 1.3)
mtext(text = "tree", side = 1, line = 1.8, cex = 1)
mtext(text = "B", side = 3, line = 0.25, cex = 1.2, adj = -0.2, outer = FALSE)

dev.off()



## FIG S3: HABITAT & MOBILITY TRENDS (PROPORTIONAL AND PBDB) ###################

# Load libraries
library(plotrix)   # v. 3.8-2

# Read and prep data
load(paste0(data, "mids"))
load(paste0(data, "PBDB.mids"))
load(paste0(data, "habitat.pt.traits"))
load(paste0(data, "mobility.pt.traits"))
load(paste0(data, "PBDB.habitat.pt.traits"))
load(paste0(data, "PBDB.mobility.pt.traits"))

# For PBDB, start plotting trends at first tip FAD (i.e., truncate zero values
# below that FAD)
PBDB.ranges <- read.csv(paste0(data, "GenusStratRanges.csv"), header = TRUE)
truncate.trend <- max(PBDB.ranges$max_ma)

# Summarize statistics across trees
h.trait.occs <- 
  apply(simplify2array(habitat.pt.traits), 1:2, mean, na.rm = TRUE)
h.trait.occs.SE <- 
  apply(simplify2array(habitat.pt.traits), 1:2, sd, na.rm = TRUE)
h.mean.SE <- mean(h.trait.occs.SE, na.rm = TRUE)
m.trait.occs <- 
  apply(simplify2array(mobility.pt.traits), 1:2, mean, na.rm = TRUE)
m.trait.occs.SE <- 
  apply(simplify2array(mobility.pt.traits), 1:2, sd, na.rm = TRUE)
m.mean.SE <- mean(m.trait.occs.SE, na.rm = TRUE)

h.trait.long <- c("infaunal", "semi-infaunal", "short epifaunal",
                  "intermediate epifaunal", "tall epifaunal", "epibiotic", 
                  "pelagic")
m.trait.long <- c("sedentary", "facultatively mobile", "intermittently mobile", 
                  "habitually mobile", "passively mobile")

# Proportionally standardize:
sums <- apply(h.trait.occs, 1, sum)
h.trait.props <- h.trait.occs / sums
sums <- apply(m.trait.occs, 1, sum)
m.trait.props <- m.trait.occs / sums

# Replace missing values with zeros
wh.missing <- which(is.na(h.trait.props), arr.ind = TRUE)
h.trait.props[wh.missing] <- 0
wh.missing <- which(is.na(m.trait.props), arr.ind = TRUE)
m.trait.props[wh.missing] <- 0
wh.missing <- which(is.na(PBDB.habitat.pt.traits), arr.ind = TRUE)
PBDB.habitat.pt.traits[wh.missing] <- 0
wh.missing <- which(is.na(PBDB.mobility.pt.traits), arr.ind = TRUE)
PBDB.mobility.pt.traits[wh.missing] <- 0


# Set pdf options (for full page). Default font family is Arial
tiff(filename = "FigS3_Habitats&Mobility_166.tif", width = 166, height = 166, 
     units = "mm", res = 600)

# Build figure
par(mfrow = c(2, 2))
par(mar = c(5, 1.8, 0.35, 1.3))

# A. Proportional habitats
plotrix::stackpoly(x = -mids, y = h.trait.props, col = cols, xat = 0, 
                   xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE, ylim = c(0, 1))
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.53)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.53)
mtext(text = "proportion of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 3.5, cex = 0.75)
mtext(text = "A", side = 3, line = -0.8, cex = 1.2, adj = -0.11, outer = FALSE)

# B. Proportional mobiity
plotrix::stackpoly(x = -mids, y = m.trait.props, col = cols, xat = 0, 
                   xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE, ylim = c(0, 1))
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.53)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.53)
mtext(text = "proportion of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 3.5, cex = 0.75)
mtext(text = "B", side = 3, line = -0.8, cex = 1.2, adj = -0.12, outer = FALSE)

# C. Stacked plot for habitat (using PBDB ranges [tips only])
plotrix::stackpoly(x = -PBDB.mids, y = PBDB.habitat.pt.traits, col = cols, 
                   xat = 0, xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
# Overlay a white rectangle to avoid inference of Terreneuvian occurrences
polygon(x = c(-535, -535, -truncate.trend, -truncate.trend), y = c(0, 100, 100, 0), col = "white", border = F)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -496), y = c(110, 300, 300, 110), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 3.5, cex = 0.75)
mtext(text = "C", side = 3, line = -0.8, cex = 1.2, adj = -0.11, outer = FALSE)
legend("topleft", legend = rev(h.trait.long), pch = 15, 
       col = cols[length(h.trait.long):1], ncol = 1, 
       pt.cex = 2, cex = .8, bty = "n")

# D. Stacked plot for mobility (using PBDB ranges [tips only])
plotrix::stackpoly(x = -PBDB.mids, y = PBDB.mobility.pt.traits, col = cols, 
                   xat = 0, xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
# Overlay a white rectangle to avoid inference of Terreneuvian occurrences
polygon(x = c(-535, -535, -truncate.trend, -truncate.trend), y = c(0, 100, 100, 0), col = "white", border = F)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -496), y = c(110, 300, 300, 110), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 3.5, cex = 0.75)
mtext(text = "D", side = 3, line = -0.8, cex = 1.2, adj = -0.12, outer = FALSE)
legend("topleft", legend = rev(m.trait.long), pch = 15, 
       col = cols[length(m.trait.long):1], ncol = 1, 
       pt.cex = 2, cex = .8, bty = "n")

dev.off()




## FIG S4: 6 ECOLOGICAL CHARACTERS #############################################

# Read and prep data
load(paste0(data, "mids"))
load(paste0(data, "traits"))
load(paste0(data, "traits.SE"))

# Only plotting biotic substrate, supported, hard substrate, attached, feeding
# above primary, and feeding within primary.
trait.names <- c("biotic substrate", "supported", "hard substrate", "attached",
                 "living (partially) within seafloor", "food within seafloor")

mean.cols <- c(1, 12, 3, 5, 8, 14)
median.cols <- mean.cols + 25

# Confirm as expected
colnames(traits)[mean.cols]
colnames(traits.SE)[mean.cols]
colnames(traits)[median.cols]
colnames(traits.SE)[median.cols]

lim <- c(0, 1)

# Set pdf options (for full page). Default font family is Arial
tiff(filename = "FigS4_IndividualTraits_166.tif", width = 166, height = 166, 
     units = "mm", res = 600)

# Build figure
par(mfrow = c(2, 3))
par(mar = c(5.7, 2.47, 1.2, 0.2))

## Plot trends (+/- SE)
for(trait in 1:length(mean.cols)) {
  plot(mids, rep(lim[1], length(mids)), xlim = c(540, 443), ylim = lim, 
       type = "n", axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  box()
  mtext(text = trait.names[trait], side = 3, cex = 0.75)
  axis(2, lwd.ticks = 0.5, padj = 0.75, cex.axis = 0.8)
  # Only plot time scale for bottom panels
  palaeoverse::axis_geo(side = 1, height = 0.08, lab_size = list(1, 1), 
                        cex.axis = 0.75, abbr = list(TRUE, FALSE),
                        intervals = list("epochs", "periods"), 
                        lab_col = "black", skip = c("Ediacaran", "Rhuddanian", 
                                                    "Llandovery", "Silurian"))
  # Only plot axis label for bottom
  if(trait > 3){
    mtext(text = "time (Ma)", side = 1, line = 4.6, cex = 0.75)
  }
  # Only plot axis label for leftmost
  if(trait %in% c(1, 4)){
    mtext(text = "average state", side = 2, line = 1.6, cex = 0.75)
  }
  mtext(text = LETTERS[trait], side = 3, line = -0.1, cex = 1.2, adj = -0.19, outer = FALSE)
  # Add reference boundaries
  abline(v = unique(c(521, 509, 497, 470, 458.4, 541, 485.4, 443.8)), 
         col = "gray85")
  
  mean.column <- cbind(c(mids, rev(mids)), 
                       c(traits[, mean.cols[trait]] - traits.SE[, mean.cols[trait]], 
                         rev(traits[, mean.cols[trait]] + traits.SE[, mean.cols[trait]])))
  mean.column <- na.omit(mean.column)
  median.column <- cbind(c(mids, rev(mids)), 
                         c(traits[, median.cols[trait]] - traits.SE[, median.cols[trait]], 
                           rev(traits[, median.cols[trait]] + traits.SE[, median.cols[trait]])))
  median.column <- na.omit(median.column)
  polygon(median.column[ ,1], median.column[ ,2], col = trans.cols[7], lwd = 2, border = NA)
  polygon(mean.column[ ,1], mean.column[ ,2], col = trans.cols[2], lwd = 2, border = NA)
  lines(mids, traits[, mean.cols[trait]], col = cols[2], lwd = 3)
  lines(mids, traits[, median.cols[trait]], col = cols[7], lwd = 3, lty = 3)
  
  if(trait == 1) {
    legend("topleft", legend = c("mean", "median"), box.col = NA, lty = c(1, 3), 
           col = cols[c(2,7)], lwd = 3, pch = NA, inset = .05)
  }
  
}

dev.off()




## FIG S5: DIET & FORAGING TRENDS (PROPORTIONAL AND PBDB) ######################

# Load libraries
library(plotrix)   # v. 3.8-2

# Read and prep data
load(paste0(data, "mids"))
load(paste0(data, "PBDB.mids"))
load(paste0(data, "diet.pt.traits"))
load(paste0(data, "foraging.pt.traits"))
load(paste0(data, "PBDB.diet.pt.traits"))
load(paste0(data, "PBDB.foraging.pt.traits"))

# For PBDB, start plotting trends at first tip FAD (i.e., truncate zero values
# below that FAD)
PBDB.ranges <- read.csv(paste0(data, "GenusStratRanges.csv"), header = TRUE)
truncate.trend <- max(PBDB.ranges$max_ma)

# Summarize statistics across trees
d.trait.occs <- 
  apply(simplify2array(diet.pt.traits), 1:2, mean, na.rm = TRUE)
d.trait.occs.SE <- 
  apply(simplify2array(diet.pt.traits), 1:2, sd, na.rm = TRUE)
d.mean.SE <- mean(d.trait.occs.SE, na.rm = TRUE)
f.trait.occs <- 
  apply(simplify2array(foraging.pt.traits), 1:2, mean, na.rm = TRUE)
f.trait.occs.SE <- 
  apply(simplify2array(foraging.pt.traits), 1:2, sd, na.rm = TRUE)
f.mean.SE <- mean(f.trait.occs.SE, na.rm = TRUE)

d.trait.long <- c("autotroph", "herbivore", "microbivore", "carnivore")
f.trait.long <- c("absorptive", "unknown-density filterer", 
                  "low-density filterer", "medium-density filterer", 
                  "high-density filterer", "mass-feeder", "attachment feeder", 
                  "raptorial feeder")

# Proportionally standardize:
sums <- apply(d.trait.occs, 1, sum)
d.trait.props <- d.trait.occs / sums
sums <- apply(f.trait.occs, 1, sum)
f.trait.props <- f.trait.occs / sums

# Replace missing values with zeros
wh.missing <- which(is.na(d.trait.props), arr.ind = TRUE)
d.trait.props[wh.missing] <- 0
wh.missing <- which(is.na(f.trait.props), arr.ind = TRUE)
f.trait.props[wh.missing] <- 0
wh.missing <- which(is.na(PBDB.diet.pt.traits), arr.ind = TRUE)
PBDB.diet.pt.traits[wh.missing] <- 0
wh.missing <- which(is.na(PBDB.foraging.pt.traits), arr.ind = TRUE)
PBDB.foraging.pt.traits[wh.missing] <- 0


# Set pdf options (for full page). Default font family is Arial
tiff(filename = "FigS5_Diet&Foraging_166.tif", width = 166, height = 166, 
     units = "mm", res = 600)

# Build figure
par(mfrow = c(2, 2))
par(mar = c(5, 1.8, 0.35, 1.3))

# A. Proportional diets
plotrix::stackpoly(x = -mids, y = d.trait.props, col = cols[c(5, 4, 2, 7)], 
                   xat = 0, xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE, ylim = c(0, 1))
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.53)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.53)
mtext(text = "proportion of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 3.5, cex = 0.75)
mtext(text = "A", side = 3, line = -0.8, cex = 1.2, adj = -0.11, outer = FALSE)

# B. Proportional mobility
plotrix::stackpoly(x = -mids, y = f.trait.props, col = cols, xat = 0, 
                   xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE, ylim = c(0, 1))
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.53)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.53)
mtext(text = "proportion of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 3.5, cex = 0.75)
mtext(text = "B", side = 3, line = -0.8, cex = 1.2, adj = -0.12, outer = FALSE)

# C. Stacked plot for diet (using PBDB ranges [tips only])
plotrix::stackpoly(x = -PBDB.mids, y = PBDB.diet.pt.traits, col = cols[c(5, 4, 2, 7)], 
                   xat = 0, xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
# Overlay a white rectangle to avoid inference of Terreneuvian occurrences
polygon(x = c(-535, -535, -truncate.trend, -truncate.trend), y = c(0, 100, 100, 0), col = "white", border = F)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -496), y = c(100, 300, 300, 100), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 3.5, cex = 0.75)
mtext(text = "C", side = 3, line = -0.8, cex = 1.2, adj = -0.11, outer = FALSE)
legend("topleft", legend = rev(d.trait.long), pch = 15, 
       col = cols[c(7, 2, 4, 5)], ncol = 1, 
       pt.cex = 2, cex = .8, bty = "n")

# D. Stacked plot for foraging (using PBDB ranges [tips only])
plotrix::stackpoly(x = -PBDB.mids, y = PBDB.foraging.pt.traits, col = cols, xat = 0, 
                   xlim = c(-541, -443), stack = TRUE, axis2 = FALSE, 
                   axis4 = FALSE)
# Overlay a white rectangle to avoid inference of Terreneuvian occurrences
polygon(x = c(-535, -535, -truncate.trend, -truncate.trend), y = c(0, 100, 100, 0), col = "white", border = F)
abline(v = -series.boundaries, col = "gray85", lty = 5)
abline(v = -pd.boundaries, col = "white", lty = 1, lwd = 1.25)
# Add polygon so legend lacks stage lines
polygon(x = c(-540, -540, -475, -496), y = c(100, 300, 300, 100), border = NA,
        col = "white")
box()
palaeoverse::axis_geo(side = 1, height = 0.09, lab_size = list(0.8, 0.8), 
                      cex.axis = 0.5, padj = -2.8, lwd.ticks = 0.4, 
                      abbr = list(TRUE, FALSE),
                      tick_at = axis.ticks, tick_labels = axis.labels,
                      intervals = list(epochs_new, "periods"), 
                      lab_col = "black", skip = c("Ediacaran", "Llandovery", 
                                                  "Silurian"))
axis(2, lwd.ticks = 0.4, padj = 1.5, cex.axis = 0.55)
axis(4, lwd.ticks = 0.4, padj = -2, cex.axis = 0.55)
mtext(text = "no. of lineages", side = 2, line = 1.1, cex = 0.75)
mtext(text = "time (Ma)", side = 1, line = 3.5, cex = 0.75)
mtext(text = "D", side = 3, line = -0.8, cex = 1.2, adj = -0.12, outer = FALSE)
legend("topleft", legend = rev(f.trait.long), pch = 15, 
       col = cols[length(f.trait.long):1], ncol = 1, 
       pt.cex = 2, cex = .8, bty = "n")

dev.off()







