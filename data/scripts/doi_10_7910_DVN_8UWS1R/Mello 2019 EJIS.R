# Replication script "Paths towards Coalition Defection: Democracies and Withdrawal
# from the Iraq War", European Journal of International Security (2019)
# Author: Patrick A. Mello
# E-mail: patrick.mello@uni-erfurt.de

# Load Packages
library(QCA); library(SetMethods); library(ggplot2); library(gridExtra)

# Read Data
lead <- read.csv("Mello 2019 EJIS.csv", header = TRUE, row.names = 1, sep = ",")

### Calibration
# Conditions Leftist Partisanship (P), Low Commitment (C), and Fatalities (F)
lead$P <- calibrate(lead$P_raw, type = "fuzzy", method = "direct",
                      c(6.25, 5, 3.75), logistic = TRUE) 
lead$P <- round(lead$P, 2)

lead$C <- calibrate(lead$C_raw, type = "fuzzy", method = "direct",
                      c(1.5, 1, 0.5), logistic = TRUE) 
lead$C <- round(lead$C, 2)

lead$F <- calibrate(lead$F_raw, type = "fuzzy", method = "direct",
                      c(0.005, 0.006, 0.50), logistic = TRUE)
lead$F <- round(lead$F, 2)

### Necessary Condition Analysis
# Table 1: Analysis of Necessary Conditions for Early Withdrawal from Iraq
QCAfit(lead[, 1:5], lead$W, necessity = TRUE, names(lead[, 1:5]))
QCAfit(lead[, 1:5], 1 - lead$W, necessity = TRUE, 
        paste("~", names(lead[, 1:5])))

# Table 2: Chi-Square Test: Upcoming Elections and Early Withdrawal
detach("package:SetMethods", unload = TRUE)
detach("package:QCA", unload = TRUE)
library(poliscidata)

ld <- round(lead, digits = 0)
tlead <- xtabs(~W + E, data = ld)
options(scipen = 999) # Notation 

xtp(ld, W, E)
xtp.chi2(ld, W, E)            
CramersV(28.79531, 2, 2, 51)

### Truth Table Analysis
detach("package:poliscidata", unload = TRUE)
library(SetMethods)
library(QCA)

# Conservative Solution
TTcs <- truthTable(lead, outcome = "W", conditions = "L, E, P, C, F",
                    complete = FALSE, show.cases = TRUE, 
                    sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

SOLcs <- minimize(TTcs, outcome = "W",include = "1", row.dom = FALSE,
                    details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                    all.sol = TRUE, method = "CCubes")
SOLcs 

# Parsimonious Solution
TTps <- truthTable(lead, outcome = "W", conditions = "L, E, P, C, F",
                    complete = TRUE, show.cases = TRUE, 
                    sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

SOLps <- minimize(TTps, outcome = "W", include = "1, ?", row.dom = TRUE,
                    details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                    method = "CCubes")
SOLps 

# Intermediate Solution
# Exclusion of least-likely row #5 (no contradictory simplifying assumptions)
SOLis <- minimize(TTps, outcome = "W", include = "?", row.dom = TRUE,
                    all.sol = FALSE, details = TRUE, show.cases = TRUE,
                    use.tilde = TRUE, omit = 5, method = "CCubes")
SOLis

### XY Plots
# Figure 2: Early Withdrawal from Iraq and Solution and Path Membership

# Create data for shaded area for typical cases
poly <- data.frame(x = c(0.5, 0.5, 1.1), y = c(0.5, 1.1, 1.1))
geom_polygon(inherit.aes = FALSE, aes(x = x, y = y), data = poly,
              fill = "gray87", alpha = 0.4)

# Plot Intermediate Solution Term
DEV1 <- lead[c("DO1"),] # Subsets of deviant cases coverage
DEV2 <- lead[c("PT2"),]
COV.IS <- subset(lead, W > 0.5 & SOLis > 0.5) # Subset of covered cases
NOT.COV.IS <- subset(lead, SOLis < 0.5) # Subset of cases < 0.5 in SOLis

IS <- ggplot(lead, aes(x = SOLis, y = W)) +
  geom_polygon(inherit.aes = FALSE, aes(x = x, y = y), data = poly,
        fill = "gray95") + 
  geom_point(data = NOT.COV.IS, fill = "gray70", size = 7, shape = 21,
        color = "black", stroke = 1, 
        position = position_jitter(width = 0.03, height = 0.03)) +
  geom_point(data = COV.IS, fill = "dodgerblue", size = 7, shape = 21,
        color = "black", stroke = 1, 
        position = position_jitter(width = 0.03, height = 0.03)) +
        scale_x_continuous(name = "Intermediate Solution Term",
            breaks = seq(0.0,1.0,0.1)) +
        scale_y_continuous(name = "Early Withdrawal", 
            breaks = seq(0.0,1.0,0.1)) +
        coord_cartesian(xlim = c(-0.02,1.02), ylim = c(-0.02,1.02)) +
        theme_classic() +
        theme(panel.border = element_rect(fill = NA, color = "black",
            size = 1, linetype = 1)) +
        theme(axis.text.y = element_text(size = 18, color = "black"),
            axis.title = element_text(face = "plain", size = 20)) +
        theme(axis.text.x = element_text(size = 18, color = "black"),
            axis.title = element_text(face = "plain", size = 20)) +
    geom_hline(aes(yintercept = 0.5), color = "black", linetype = 5) +
    geom_vline(aes(xintercept = 0.5), color = "black", linetype = 5) +
    geom_abline(data = data.frame, intercept = c(0, 0), slope = 1) +
    geom_text(data = DEV1, aes(label = row.names(DEV1)), 
              size = 6, nudge_y = -0.08) +
    geom_text(data = DEV2, aes(label = row.names(DEV2)), 
              size = 6, nudge_x = 0.08)
IS

# Plot Path 1
is1 <- lead[c("AU2", "ES2", "HU2", "IT2"),]
rownames(lead)
row.names(subset(lead, W > 0.5 & SOLis > 0.5 ))  # find & create subset
NOT.is1 <- lead[c(-4,-14,-19,-21),]
a1 <- lead[c("AU2"),]
b1 <- lead[c("ES2"),] 
c1 <- lead[c("HU2"),]
d1 <- lead[c("IT2"),]

is1a <- ggplot(NOT.is1, aes(x = is.1, y = W)) +
    geom_polygon(inherit.aes = FALSE, aes(x = x, y = y), data = poly,
        fill = "gray95") +
    geom_point(data = NOT.is1, fill = "gray70", size = 7, shape = 21, 
        color = "black", stroke = 1,
        position = position_jitter(width = 0.01, height = 0.01)) +
    geom_point(data = is1, fill = "dodgerblue", size = 7, shape = 21,
        color = "black", stroke = 1,
        position = position_jitter(width = 0.01, height = 0.01)) +
    scale_x_continuous(name = "Path 1 (L*~E*P)", 
                       breaks = seq(0.0, 1.0, 0.1)) +
    scale_y_continuous(name = "Early Withdrawal", 
                       breaks = seq(0.0, 1.0, 0.1)) +
    coord_cartesian(xlim = c(-0.02, 1.02), ylim = c(-0.02, 1.02)) +
    theme_classic() + 
    theme(panel.border = element_rect(fill = NA, color = "black",
    size = 1, linetype = 1)) +
    theme(axis.text.y = element_text(size = 18, color = "black"),
    axis.title = element_text(face = "plain", size = 20)) +
    theme(axis.text.x = element_text(size = 18, color = "black"),
    axis.title = element_text(face = "plain", size = 20)) +
    geom_hline(aes(yintercept = 0.5), color = "black", linetype = 5) +
    geom_vline(aes(xintercept = 0.5), color = "black", linetype = 5) +
    geom_abline(data = data.frame, intercept = c(0, 0), slope = 1) +
    geom_text(data = a1, aes(label = row.names(a1)), 
            size = 6, nudge_x = 0.07) +
    geom_text(data = b1, aes(label = row.names(b1)), 
              size = 6, nudge_y = 0.05) +
    geom_text(data = c1, aes(label = row.names(c1)), size = 6,
            nudge_y = -0.05) +
    geom_text(data = d1, aes(label = row.names(d1)), size = 6,
            nudge_x = 0.05, nudge_y = -0.03)
is1a

# Plot Path 2
is2 <- lead[c("BG1", "LV2", "PH1", "RO2", "SK2"),]
NOT.is2 <- lead[c(-5,-27,-39, -47, -49),]
a2 <- lead[c("BG1", "RO2"),]
b2 <- lead[c("LV2"),]
c2 <- lead[c("PH1"),]
d2 <- lead[c("SK2"),]

is2a <- ggplot(NOT.is2, aes(x = is.2, y = W)) +
    geom_polygon(inherit.aes = FALSE, aes(x = x,y = y), data = poly, 
            fill = "gray95") +
    geom_point(data = NOT.is2, fill = "gray70", size = 7, shape = 21,
            color = "black", stroke = 1, 
            position = position_jitter(width = 0.01, height = 0.01)) +
    geom_point(data = is2, fill = "dodgerblue1", size = 7, shape = 21,
            color = "black", stroke = 1, 
            position = position_jitter(width = 0.01, height = 0.01)) +
    scale_x_continuous(name = "Path 2 (~E*F)",
            breaks = seq(0.0,1.0,0.1)) +
    scale_y_continuous(name = "Early Withdrawal", breaks = seq(0.0,1.0,0.1)) +
            coord_cartesian(xlim = c(-0.02,1.02), ylim = c(-0.02,1.02)) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, color = "black", size = 1,
            linetype = 1)) +
    theme(axis.text.y = element_text(size = 18, color = "black"), 
            axis.title = element_text(face = "plain", size = 20)) +
    theme(axis.text.x = element_text(size = 18, color = "black"),
            axis.title = element_text(face = "plain", size = 20)) +
    geom_hline(aes(yintercept = 0.5), color = "black", linetype = 5) +
    geom_vline(aes(xintercept = 0.5), color = "black", linetype = 5) +
    geom_abline(data = data.frame, intercept = c(0,0), slope = 1) +
    geom_text(data = a2, aes(label = row.names(a2)), 
            size = 6, nudge_y = 0.04) +
    geom_text(data = b2, aes(label = row.names(b2)), 
            size = 6, nudge_x = 0.05) +
    geom_text(data = c2, aes(label = row.names(c2)), 
            size = 6, nudge_x = -0.06) +
    geom_text(data = d2, aes(label = row.names(d2)), 
            size = 6, nudge_y = -0.05)
is2a 

# Plot Path 3
is3 <- lead[c("DK1","HN1", "JP1", "NI1", "NL1"),]
NOT.is3 <- lead[c(-9,-17,-22,-35,-36,-29),]
a3 <- lead[c("HN1","NL1"),]
b3 <- lead[c("NI1"),]
c3 <- lead[c("DK1","JP1"),]

is3a <- ggplot(NOT.is3, aes(x = is.3, y = W)) +
    geom_polygon(inherit.aes = FALSE, aes(x = x, y = y), data = poly,
          fill = "gray95") +
    geom_point(data = NOT.is3, fill = "gray70", size = 7, shape = 21,
          color = "black", stroke = 1, 
          position = position_jitter(width = 0.01, height = 0.01)) +
    geom_point(data = is3, fill = "dodgerblue1", size = 7, shape = 21,
          color = "black", stroke = 1, 
          position = position_jitter(width = 0.01, height = 0.01)) +
    scale_x_continuous(name = "Path 3 (~L*~E*~P)", breaks = seq(0.0,1.0,0.1)) +
    scale_y_continuous(name = "Early Withdrawal", breaks = seq(0.0,1.0,0.1)) +
    coord_cartesian(xlim = c(-0.02,1.02), ylim = c(-0.02,1.02)) +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA, color = "black", 
          size = 1, linetype = 1)) +
  theme(axis.text.y = element_text(size = 18, color = "black"), 
          axis.title = element_text(face = "plain", size = 20)) +
  theme(axis.text.x = element_text(size = 18, color = "black"),
          axis.title = element_text(face = "plain", size = 20)) +
  geom_hline(aes(yintercept = 0.5), color = "black", linetype = 5) +
  geom_vline(aes(xintercept = 0.5), color = "black", linetype = 5) +
  geom_abline(data = data.frame, intercept = c(0,0), slope = 1) +
  geom_text(data = a3, aes(label = row.names(a3)), 
            size = 6, nudge_y = 0.04) +
  geom_text(data = b3, aes(label = row.names(b3)), 
            size = 6, nudge_y = 0.04) +
  geom_text(data = c3, aes(label = row.names(c3)),
            size = 6, nudge_y = -0.04)
is3a

# Plot Path 4
is4 <- lead[c("NO1","NZ1"),]
NOT.is4 <- lead[c(-37, -38),]
a4 <- lead[c("NO1","NZ1"),]

is4a <- ggplot(NOT.is4, aes(x = is.4, y = W)) +
  geom_polygon(inherit.aes = FALSE, aes(x = x, y = y), data = poly,
          fill = "gray95") +
  geom_point(data = NOT.is4, fill = "gray70", size = 7, shape = 21,
          color = "black", stroke = 1, 
          position = position_jitter(width = 0.01, height = 0.01)) +
  geom_point(data = is4, fill = "dodgerblue1", size = 7, shape = 21,
          color = "black", stroke = 1, 
          position = position_jitter(width = 0.01, height = 0.01)) +
  scale_x_continuous(name = "Path 4 (~L*~E*C)", breaks = seq(0.0,1.0,0.1)) +
  scale_y_continuous(name = "Early Withdrawal", breaks = seq(0.0,1.0,0.1)) +
  coord_cartesian(xlim = c(-0.02,1.02), ylim = c(-0.02,1.02)) +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, color = "black", 
          size = 1, linetype = 1)) +
  theme(axis.text.y = element_text(size = 18, color = "black"), 
          axis.title = element_text(face = "plain", size = 20)) +
  theme(axis.text.x = element_text(size = 18, color = "black"),
          axis.title = element_text(face = "plain", size = 20)) +
  geom_hline(aes(yintercept = 0.5), color = "black", linetype = 5) +
  geom_vline(aes(xintercept = 0.5), color = "black", linetype = 5) +
  geom_abline(data = data.frame, intercept = c(0,0), slope = 1) +
  geom_text(data = a4, aes(label = row.names(a4)), 
            size = 6, nudge_y = 0.04)
is4a

grid.arrange(IS, is1a, is2a, is3a, is4a, ncol = 2)

### Supplementary File
# Figure S1: Histograms of Raw Data
par(mfrow = c(4, 3), lwd = 2)

h.P_raw <- hist(lead$P_raw, freq = TRUE, breaks = 11, col = "white", 
                ylim = c(0, 12), xlab = "Left-Right Partisanship (raw)",
                ylab = "Frequency", font = 2, font.lab = 2, main = "", 
                lty = 1, lwd = 2)

h.C_raw <- hist(lead$C_raw, freq = TRUE, breaks = 11, col = "white",
                ylim = c(0, 40), xlab = "Relative Commitment (raw)",
                ylab = "Frequency", font = 2, font.lab = 2, main = "",
                lty = 1, lwd = 2, xlim = c(0, 50))

h.F_raw <- hist(lead$F_raw, freq = TRUE, breaks = 11, col = "white",
                ylim = c(0, 35), xlab = "Relative Fatalities (raw)",
                ylab = "Frequency", font = 2, font.lab = 2, main = "",
                lty = 1, lwd = 2, xlim = c(0, 8))

# Figure S2: Histograms of Fuzzy-Set Membership Scores
par(mfrow = c(3, 2), lwd = 2)
bin = c(seq(0, 1, 0.1))
h.L <- hist(lead$L, freq = TRUE, breaks = bin, col = "white", ylim = c(0, 30),
            xlab = "Leadership Change", ylab = "Frequency", font = 2, 
            font.lab = 2, main = "", lty = 1, lwd = 2)

h.E <- hist(lead$E, freq = TRUE, breaks = bin, col = "white", ylim = c(0, 30),
            xlab = "Upcoming Elections", ylab = "Frequency", font = 2,
            font.lab = 2, main = "", lty = 1,  lwd = 2)

h.P <- hist(lead$P, freq = TRUE, breaks = bin, col = "white", ylim = c(0, 30),
            xlab = "Leftist Partisanship", ylab = "Frequency", font = 2,
            font.lab = 2, main = "", lty = 1, lwd = 2)

h.C <- hist(lead$C, freq = TRUE, breaks = bin, col = "white", ylim = c(0, 30),
            xlab = "Low Commitment", ylab = "Frequency", font = 2,
            font.lab = 2, main = "", lty = 1, lwd = 2)

h.F <- hist(lead$F, freq = TRUE, breaks = bin, col = "white", ylim = c(0, 30),
            xlab = "Fatalities", ylab = "Frequency", font = 2,
            font.lab = 2, main = "", lty = 1, lwd = 2)

h.W <- hist(lead$W, freq = TRUE, breaks = bin, col = "white", ylim = c(0, 30),
            xlab = "Early Withdrawal", ylab = "Frequency", font = 2,
            font.lab = 2, main = "", lty = 1, lwd = 2)

# Figure S3: Raw Data and Calibrated Fuzzy Sets
par(mfrow = c(3, 2), lwd = 2)
p.P <- plot(lead$P_raw, lead$P, xlab = "Left-Right Partisanship (raw)",
            ylab = "Leftist Partisanship (fuzzy)", font = 2, font.lab = 2) +
  abline(v = 5, col = "black", lwd = 0.7) +
  abline(h = 0.5, col = "black", lwd = 0.7)

p.C <- plot(lead$C_raw, lead$C, xlab = "Relative Commitment (raw)",
            ylab = "Low Commitment (fuzzy)", font = 2, font.lab = 2) +
  abline(v = 1.0, col = "black", lwd = 0.7) +
  abline(h = 0.5, col = "black", lwd = 0.7)

p.F <- plot(lead$F_raw, lead$F, xlab = "Relative Fatalities (raw)",
            ylab = "Fatalities (fuzzy)", font = 2, font.lab = 2) +
  abline(v = 0.006, col = "black", lwd = 0.7) +
  abline(h = 0.5, col = "black", lwd = 0.7)

# Table S2: Analysis of Necessary Conditions for Non-Outcome
QCAfit(lead[, 1:5], 1 - lead$W, necessity = TRUE, names(lead[, 1:5]))
QCAfit(1 - lead[, 1:5], 1 - lead$W, necessity = TRUE,
       paste("~", names(lead[, 1:5])))

# Table S3: Truth Table for Early Withdrawal, with Logical Remainders
TTps <- truthTable(lead, outcome = "W", conditions = "L, E, P, C, F",
                   complete = TRUE, show.cases = TRUE, 
                   sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)
S3tt <- TTps$tt
write.csv(S3tt, "S3tt.csv")

# Analysis of the Non-Outcome
# Table S5: Truth Table for Non-Early Withdrawal, with Logical Remainders
nlead <- lead[,1:6]
NTTps <- truthTable(nlead, outcome = "~W", conditions = "L, E, P, C, F",
                    complete = TRUE, show.cases = TRUE, 
                    sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)
NTTps

# Table S6: Solution Terms for Non-Early Withdrawal 
# Conservative Solution
NTTcs <- truthTable(nlead, outcome = "~W", conditions = "L, E, P, C, F",
                    complete = FALSE, show.cases = TRUE, 
                    sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

NSOLcs <- minimize(NTTcs, outcome = "~W",include = "1", row.dom = FALSE,
                   details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                   all.sol = TRUE, method = "CCubes")
NSOLcs 

nlead$NSOLcs <- compute("(L*E*P) + (L*~E*~P*~F) + (L*E*~C) + (E*P*~C) +
                  (~L*E*~P*F) + (~L*E*C*~F)", data = nlead, separate = FALSE)

nlead$cs.1 <- compute("(L*E*P)", data = lead, separate = FALSE)
nlead$cs.2 <- compute("(L*~E*~P*~F)", data = lead, separate = FALSE)
nlead$cs.3 <- compute("(L*E*~C)", data = lead, separate = FALSE)
nlead$cs.4 <- compute("(E*P*~C)", data = lead, separate = FALSE)
nlead$cs.5 <- compute("(~L*E*~P*F)", data = lead, separate = FALSE)
nlead$cs.6 <- compute("(~L*E*C*~F)", data = lead, separate = FALSE)

# Parsimonious Solution
NTTps <- truthTable(nlead, outcome = "~W", conditions = "L, E, P, C, F",
                    complete = TRUE, show.cases = TRUE, 
                    sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

NSOLps <- minimize(NTTps, outcome = "W", include = "1, ?", row.dom = TRUE,
                   details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                   method = "CCubes")
NSOLps 

nlead$NSOLps <- compute("E*P + ~L*E*C + L*~P*~F + E*~C*F",
                        data = nlead, separate = FALSE)
nlead$ps.1 <- compute("(E*P)", data = lead, separate = FALSE)
nlead$ps.2 <- compute("(~L*E*C)", data = lead, separate = FALSE)
nlead$ps.3 <- compute("(L*~P*~F)", data = lead, separate = FALSE)
nlead$ps.4 <- compute("(E*~C*F)", data = lead, separate = FALSE)

### Robustness Tests (see Supplementary Document)
# Table S7: Crisp-Set Analysis
cs <- round(lead, digits = 0)

TTS7 <- truthTable(cs, outcome = "W", conditions = "L, E, P, C, F",
                   complete = TRUE, show.cases = TRUE, 
                   sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

SOLS7 <- minimize(TTS7, outcome = "W", include = "?", row.dom = TRUE,
                  all.sol = FALSE, details = TRUE, show.cases = TRUE,
                  use.tilde = TRUE, omit = 5)
SOLS7

# Table 8: Restrictive Coding of Leadership Change
rled <- read.csv("Restrictive Leaders.csv", header = TRUE, 
                  row.names = 1, sep = ",")

TT8cs <- truthTable(rled, outcome = "W", conditions = "L, E, P, C, F", 
                  complete = FALSE, show.cases = TRUE, 
                  sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

TT8ps <- truthTable(rled, outcome = "W", conditions = "L, E, P, C, F", 
                  complete = TRUE, show.cases = TRUE, 
                  sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

SOL8cs <- minimize(TT8cs, outcome = "W", include = "1", row.dom = FALSE,
                  all.sol = TRUE, details = TRUE, show.cases = TRUE,
                  use.tilde = TRUE, method = "CCubes")
SOL8cs

SOL8ps <- minimize(TT8ps, outcome = "W", include = "?", row.dom = TRUE,
                   all.sol = FALSE, details = TRUE, show.cases = TRUE,
                   use.tilde = TRUE, method = "CCubes")
SOL8ps

# Table 9: Alternative Analysis with Romania (RO2) as Negative Case
dataT9 <- lead[,1:6]
dataT9["RO2","W"] <- 0.3

# Parsimonious Solution
TTT9ps <- truthTable(dataT9, outcome = "W", conditions = "L, E, P, C, F",
                   complete = TRUE, show.cases = TRUE, 
                   sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)
TTT9ps

SOLT9ps <- minimize(TTT9ps, outcome = "W", include = "1, ?", row.dom = TRUE,
                  details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                  method = "CCubes")
SOLT9ps 

# Intermediate Solution
# Exclusion of least-likely row #5 (no contradictory simplifying assumptions)
SOLT9is <- minimize(TTT9ps, outcome = "W", include = "?", row.dom = TRUE,
                  all.sol = FALSE, details = TRUE, show.cases = TRUE,
                  use.tilde = TRUE, omit = 5, method = "CCubes")
SOLT9is

# Conservative Solution
TTT9cs <- truthTable(dataT9, outcome = "W", conditions = "L, E, P, C, F",
                   complete = FALSE, show.cases = TRUE, 
                   sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

SOLT9cs <- minimize(TTT9cs, outcome = "W",include = "1", row.dom = FALSE,
                  details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                  all.sol = TRUE, method = "CCubes")
SOLT9cs 

### Table S10: Alternative Threshold for Upcoming Elections (6 Months)
# Extending the threshold from 2 to 6 months affects only Bulgaria-1 
s10 <- lead[,1:6]
s10["BG1","E"] <- "1"

# Conservative Solution
T10TTcs <- truthTable(s10, outcome = "W", conditions = "L, E, P, C, F",
                   complete = FALSE, show.cases = TRUE, 
                   sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

T10SOLcs <- minimize(T10TTcs, outcome = "W",include = "1", row.dom = FALSE,
                  details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                  all.sol = TRUE, method = "CCubes")
T10SOLcs 

# Parsimonious Solution
T10TTps <- truthTable(s10, outcome = "W", conditions = "L, E, P, C, F",
                   complete = TRUE, show.cases = TRUE, 
                   sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.75)

T10SOLps <- minimize(T10TTps, outcome = "W", include = "1, ?", row.dom = TRUE,
                  details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                  method = "CCubes")
T10SOLps

# Intermediate Solution
# Exclusion of least-likely row #5 (no contradictory simplifying assumptions)
T10SOLis <- minimize(T10TTps, outcome = "W", include = "?", row.dom = TRUE,
                  all.sol = FALSE, details = TRUE, show.cases = TRUE,
                  use.tilde = TRUE, omit = 5, method = "CCubes")
T10SOLis

# End