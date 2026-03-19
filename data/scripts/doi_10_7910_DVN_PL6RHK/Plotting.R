#################################
# Plot Replication
# Dalston Ward (2018)
# "Dynamic Effects of Electoral Laws"
# Journal of Elections, Public Opinion, and Parties
# DOI: 10.1080/17457289.2018.1537279
#################################

# NOTE: Figures are created and saved as PDFs in the current working directory.

#################################
#### Figure 1
#################################

pdf("Figure_1.pdf", width = 11, height = 8.5)

par(mfrow = c(2,2))

par(mar = c(0,0,0,0), oma = c(7.5,5.5,3,3))

# Upper-left Panel: New Zealand Path
path.plot3(synth.res=NZ_five_year.synth, dataprep.res=NZ_five_year, Ylab=c("Mean District-Level ENP"), Xlab=c(""),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1993, Legend=NA, Xlim = c(1946, 2014), Bty = "c")
polygon( x = c(1946, 2014, 2014, 1946), y = c(6, 6,6.5,6.5), col = "gray85", border = "black")
text(x = 1980, y = 6.25, label = "New Zealand, Party System Trend", cex = 1.5)
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 3, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

# Upper-right Panel: Norway Path
path.plot3(synth.res=NO_five_year.synth, dataprep.res=NO_five_year, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1988, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(6, 6,6.5,6.5), col = "gray85", border = "black")
text(x = 1980, y = 6.25, label = "Norway, Party System Trend", cex = 1.5)
axis(side = 4, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 3, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

# Lower-left Panel: New Zealand In-Space Placebos
gaps.plot3(synth.res = NZ_five_year.synth,
           dataprep.res = NZ_five_year,
           Ylab = c("ENP Gaps: Treated - Synthetic"),
           Xlab = c(""),
           Main = NULL,
           tr.intake = 1993,
           Ylim = c(-1.75, 3),
           Xlim = c(1946, 2014))

# this line adds the placebos
MultiSynthPlotter(NZ_five_year_placebos_prep, NZ_five_year_placebos_Synth, main_case = 16)

polygon( x = c(1946, 2014, 2014, 1946), y = c(2.5, 2.5,3,3), col = "gray85", border = "black")
text(x = 1980, y = 2.75, label = "New Zealand, In-Space Placebos", cex = 1.5)
axis(side = 2, at = c(-1:2), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

# Lower-right Panel: Norway In-Space Placebos
gaps.plot3(synth.res = NO_five_year.synth,
           dataprep.res = NO_five_year,
           Ylab = c(""),
           Xlab = c(""),
           Main = NULL,
           tr.intake = 1988,
           Ylim = c(-1.75, 3),
           Xlim = c(1946, 2014))

# this line adds the placebos
MultiSynthPlotter(NO_five_year_placebos_prep, NO_five_year_placebos_Synth, main_case = 17)

polygon( x = c(1946, 2014, 2014, 1946), y = c(2.5, 2.5,3,3), col = "gray85", border = "black")
text(x = 1980, y = 2.75, label = "Norway, In-Space Placebos", cex = 1.5)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)
axis(side = 4, at = c(-1:2), lwd = 0, lwd.ticks = 1)

mtext(text = "Year", side = 1, line = 3.5, outer = T, cex = 1.5)

par(xpd = NA)
legend( x = 1870, y = -2.5, legend = c("Reform Country", "Placebo Countries","Synthetic Control", "Reform Adoption"), lty = c(1,1,2,3), col = c("black", "gray48", "black", "black"), lwd = c(2,.75,2,2),  ncol = 2)

dev.off()

#################################
#### Figure SI 2.1: Influence of Pre-Reform Outcomes on New Zealand's Synthetic Control Fit
#################################

pdf("Figure_SI_2_1.pdf", height = 11, width = 8.5)

par(mfrow = c(2,1))

par(mar = c(0,0,0,0), oma = c(7.5,5.5,3,3))
path.plot3(synth.res=NZ_no_cov.synth, dataprep.res=NZ_no_cov, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,5.5), tr.intake=1993, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(5, 5,5.5,5.5), col = "gray85", border = "black")
text(x = 1980, y = 5.25, label = "Only Pre-Reform Outcomes as Covariates", cex = 1.5)
axis(side = 2, at = c(2:5), lwd = 0, lwd.ticks = 1)
axis(side = 3, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

path.plot3(synth.res=NZ_no_lag.synth, dataprep.res=NZ_no_lag, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,5.5), tr.intake=1993, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(5, 5,5.5,5.5), col = "gray85", border = "black")
text(x = 1980, y = 5.25, label = "No Pre-Reform Outcomes as Covariates", cex = 1.5)
axis(side = 2, at = c(2:5), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

mtext(text = "Year", side = 1, line = 3.5, outer = T, cex = 1.5)
mtext(text = "Mean District-Level ENP", side = 2, line = 3.5, outer = T, cex = 1.5)

par(xpd = NA)
legend( x = 1940, y = 1, legend = c("Reform Country", "Synthetic Control", "Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()


#################################
#### Figure SI 2.2: Leave-One-Out Synthetic Controls for New Zealand
#################################

pdf("Figure_SI_2_2.pdf", height = 6, width = 8.5)

par(mar = c(7,5,1,1))

plot(NZ_five_year$tag$time.plot, NZ_five_year$Y1plot,t = "o", pch = 20, col = "white", lwd = 2, main = NULL, ylab = "Mean District-Level ENP", xlab = "Year", xaxs = "i", yaxs = "i", ylim = c(1.5,4.5), xlim = c(1946, 2014), xaxt = "n", yaxt = "n")
abline(v = 1993, lty = 3, col = "black", lwd = 2)
MultiSynthPlotter(NZ_loo_cou_pre, NZ_loo_cou_Synth, gap = FALSE)
lines(NZ_five_year$tag$time.plot, (NZ_five_year$Y0plot %*% NZ_five_year.synth$solution.w), col = "black", lty = "dashed", lwd = 2, cex = 4/5)
lines(NZ_five_year$tag$time.plot, NZ_five_year$Y1plot,t = "o", pch = 20, col = "black", lwd = 2)
axis(side = 2, at = c(2:5), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

par(xpd = NA)
legend( x = 1940, y = 1.1, legend = c("Reform Country", "Leave-One-out Controls", "Synthetic Control", "Reform Adoption"), lty = c(1,1,2,3), col = c("black", "gray48", "black", "black"), lwd = c(2,.75,2,2),  ncol = 2, cex = .75)

dev.off()

#################################
#### Figure SI 2.4: New Zealand Analysis with Specifications Emphasizing Covariate Balance
#################################

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

pdf("Figure_SI_2_4.pdf", height = 11, width = 8.5)

par(mfrow = c(2,1))

par(mar = c(0,0,0,0), oma = c(7.5,5.5,3,3))
path.plot3(synth.res=NZ_equal_weight, dataprep.res=NZ_five_year, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,5.5), tr.intake=1993, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
lines(NZ_five_year$tag$time.plot, (NZ_five_year$Y0plot %*% NZ_five_year.synth$solution.w), col = cbPalette[3], lty = "twodash", lwd = 2, cex = 4/5)
polygon( x = c(1946, 2014, 2014, 1946), y = c(5, 5,5.5,5.5), col = "gray85", border = "black")
text(x = 1980, y = 5.25, label = "Equal-Covariate-Weight Synthethic Control", cex = 1.5)
axis(side = 2, at = c(2:5), lwd = 0, lwd.ticks = 1)
axis(side = 3, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

path.plot3(synth.res=NZ_UK.synth, dataprep.res=NZ_UK, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,5.5), tr.intake=1993, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
lines(NZ_five_year$tag$time.plot, (NZ_five_year$Y0plot %*% NZ_five_year.synth$solution.w), col = cbPalette[3], lty = "twodash", lwd = 2, cex = 4/5)
polygon( x = c(1946, 2014, 2014, 1946), y = c(5, 5,5.5,5.5), col = "gray85", border = "black")
text(x = 1980, y = 5.25, label = "Only U.K. in Donor-Pool", cex = 1.5)
axis(side = 2, at = c(2:5), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

mtext(text = "Year", side = 1, line = 3.5, outer = T, cex = 1.5)
mtext(text = "Mean District-Level ENP", side = 2, line = 3.5, outer = T, cex = 1.5)

par(xpd = NA)
legend( x = 1940, y = 1.1, legend = c("New Zealand", "Standard Synth.", "Alternative Synth.", "Reform Adoption"), lty = c(1,6,2,3), col = c("black", cbPalette[3], "black", "black"), lwd = c(2,2,2,2),  ncol = 2, cex = .75)

dev.off()

#################################
#### Figure SI 2.3: New Zealand Analysis Excluding Denmark and Iceland
#################################

pdf("Figure_SI_2_3.pdf", height = 6, width = 8.5)

par(mar = c(7,5,1,1))

path.plot3(synth.res = NZ_five_year.synth_noDKIC, dataprep.res = NZ_five_year_noDKIC, Ylab=c("Mean District-Level ENP"), Xlab=c("Year"),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1993, Xlim = c(1946, 2014), Legend.position = c("topleft"), Legend = NA)
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)
par(xpd = NA)
legend( x = 1940, y = 1, legend = c("Reform Country", "Synthetic Control", "Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()


#################################
#### Figure SI 2.5: New Zealand Analysis with Alternate Time Period Lengths
#################################

pdf("Figure_SI_2_5.pdf", height = 11, width = 8.5)

par(mfrow = c(2,1))

par(mar = c(0,0,0,0), oma = c(7.5,5.5,3,3))
path.plot3(synth.res=NZ_four_year.synth, dataprep.res=NZ_four_year, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,5.5), tr.intake=1993, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(5, 5,5.5,5.5), col = "gray85", border = "black")
text(x = 1980, y = 5.25, label = "Four-Year Periods", cex = 1.5)
axis(side = 2, at = c(2:5), lwd = 0, lwd.ticks = 1)
axis(side = 3, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

path.plot3(synth.res=NZ_ten_year.synth, dataprep.res=NZ_ten_year, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,5.5), tr.intake=1993, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(5, 5,5.5,5.5), col = "gray85", border = "black")
text(x = 1980, y = 5.25, label = "Ten-Year Periods", cex = 1.5)
axis(side = 2, at = c(2:5), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

mtext(text = "Year", side = 1, line = 3.5, outer = T, cex = 1.5)
mtext(text = "Mean District-Level ENP", side = 2, line = 3.5, outer = T, cex = 1.5)

par(xpd = NA)
legend( x = 1940, y = 1, legend = c("Reform Country", "Synthetic Control", "Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()

#################################
#### Figure SI 2.6: In-time placebo analysis for New Zealand
#################################

pdf("Figure_SI_2_6.pdf", height = 6, width = 8.5)

par(mar = c(8,5,1,1))

path.plot3(synth.res = NZ_time_placebo.synth, dataprep.res = NZ_time_placebo, Ylab = c("Mean District-Level ENP"), Xlab=c("Year"),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1981, Legend=NA, Legend.position = c("topleft"), Xlim = c(1946, 1996))
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

par(xpd = NA)
legend( x = 1940, y = 1, legend = c("Reform Country", "Synthetic Control", "Placebo Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()

#################################
#### Figure SI 3.1: Influence of Pre-Reform Outcomes on Norway's Synthetic Control Fit
#################################

pdf("Figure_SI_3_1.pdf", height = 11, width = 8.5)

par(mfrow = c(2,1))

par(mar = c(0,0,0,0), oma = c(7.5,5.5,3,3))
path.plot3(synth.res=NO_no_cov.synth, dataprep.res=NO_no_cov, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1988, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(6, 6,6.5,6.5), col = "gray85", border = "black")
text(x = 1980, y = 6.25, label = "Only Pre-Reform Outcomes as Covariates", cex = 1.5)
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 3, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

path.plot3(synth.res=NO_no_lag.synth, dataprep.res=NO_no_lag, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1988, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(6, 6,6.5,6.5), col = "gray85", border = "black")
text(x = 1980, y = 6.25, label = "No Pre-Reform Outcomes as Covariates", cex = 1.5)
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

mtext(text = "Year", side = 1, line = 3.5, outer = T, cex = 1.5)
mtext(text = "Mean District-Level ENP", side = 2, line = 3.5, outer = T, cex = 1.5)

par(xpd = NA)
legend( x = 1940, y = .9, legend = c("Reform Country", "Synthetic Control", "Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()

#################################
#### Figure SI 3.2: Leave-One-Out Synthetic Controls for Norway
#################################

pdf("Figure_SI_3_2.pdf", height = 6, width = 8.5)

par(mar = c(7,5,1,1))

plot(NO_five_year$tag$time.plot, NO_five_year$Y1plot,t = "o", pch = 20, col = "white", lwd = 2, main = NULL, ylab = "Mean District-Level ENP", xlab = "Year", xaxs = "i", yaxs = "i", ylim = c(1.5,6), xlim = c(1946, 2014), xaxt = "n", yaxt = "n")
abline(v = 1988, lty = 3, col = "black", lwd = 2)
MultiSynthPlotter(NO_loo_cou_pre, NO_loo_cou_Synth, gap = FALSE)
lines(NO_five_year$tag$time.plot, (NO_five_year$Y0plot %*% NO_five_year.synth$solution.w), col = "black", lty = "dashed", lwd = 2, cex = 4/5)
lines(NO_five_year$tag$time.plot, NO_five_year$Y1plot,t = "o", pch = 20, col = "black", lwd = 2)
axis(side = 2, at = c(2:5), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

par(xpd = NA)
legend( x = 1940, y = .9, legend = c("Reform Country", "Leave-One-out Controls", "Synthetic Control", "Reform Adoption"), lty = c(1,1,2,3), col = c("black", "gray48", "black", "black"), lwd = c(2,.75,2,2),  ncol = 2, cex = .75)

dev.off()

#################################
#### Figure SI 3.3: Norway Analysis Excluding Denmark and Iceland
#################################

pdf("Figure_SI_3_3.pdf", height = 6, width = 8.5)

par(mar = c(7,5,1,1))

path.plot3(synth.res=NO_noDKIC.synth, dataprep.res=NO_noDKIC, Ylab=c("Mean District-Level ENP"), Xlab=c("Year"),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1988, Legend=NA, Xlim = c(1946, 2014))
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

par(xpd = NA)
legend( x = 1940, y = .9, legend = c("Reform Country", "Synthetic Control", "Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()

#################################
#### Figure SI 3.4: Norway Analysis with Alternate Time Period Lengths
#################################

pdf("Figure_SI_3_4.pdf", height = 11, width = 8.5)

par(mfrow = c(2,1))

par(mar = c(0,0,0,0), oma = c(7.5,5.5,3,3))
path.plot3(synth.res=NO_four_year.synth, dataprep.res=NO_four_year, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1988, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(6, 6,6.5,6.5), col = "gray85", border = "black")
text(x = 1980, y = 6.25, label = "Four-Year Periods", cex = 1.5)
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 3, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

path.plot3(synth.res=NO_ten_year.synth, dataprep.res=NO_ten_year, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1988, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(6, 6,6.5,6.5), col = "gray85", border = "black")
text(x = 1980, y = 6.25, label = "Ten-Year Periods", cex = 1.5)
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

mtext(text = "Year", side = 1, line = 3.5, outer = T, cex = 1.5)
mtext(text = "Mean District-Level ENP", side = 2, line = 3.5, outer = T, cex = 1.5)

par(xpd = NA)
legend( x = 1940, y = .9, legend = c("Reform Country", "Synthetic Control", "Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()

#################################
#### Figure SI 3.5: In-Time Placebo Analysis for Norway
#################################

pdf("Figure_SI_3_5.pdf", height = 6, width = 8.5)

par(mar = c(8,5,1,1))

path.plot3(synth.res = NO_time_placebo.synth, dataprep.res = NO_time_placebo, Ylab = c("Mean District-Level ENP"), Xlab=c("Year"),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1974, Legend=NA, Legend.position = c("topleft"), Xlim = c(1946, 1989))
axis(side = 2, at = 2:6, lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

par(xpd = NA)
legend( x = 1945, y = 0.9, legend = c("Reform Country", "Synthetic Control", "Placebo Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()

#################################
#### Figure SI 4.1: Synthetic Control Analysis Results for Parliamentary Party System Size
#################################

pdf("Figure_SI_4_1.pdf", height = 11, width = 8.5)

par(mfrow = c(2,1))

par(mar = c(0,0,0,0), oma = c(7.5,5.5,3,3))
path.plot3(synth.res=NZ_mech_effect.synth, dataprep.res=NZ_mech_effect, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1993, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(6, 6,6.5,6.5), col = "gray85", border = "black")
text(x = 1980, y = 6.25, label = "New Zealand", cex = 1.5)
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 3, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

path.plot3(synth.res=NO_mech_effect.synth, dataprep.res=NO_mech_effect, Ylab=c(""), Xlab=c(""),Main=NULL, Ylim = c(1.5,6.5), tr.intake=1988, Legend=NA, Xlim = c(1946, 2014), Bty = "o")
polygon( x = c(1946, 2014, 2014, 1946), y = c(6, 6,6.5,6.5), col = "gray85", border = "black")
text(x = 1980, y = 6.25, label = "Norway", cex = 1.5)
axis(side = 2, at = c(2:6), lwd = 0, lwd.ticks = 1)
axis(side = 1, at = seq(1950, 2010, 10), lwd = 0, lwd.ticks = 1)

mtext(text = "Year", side = 1, line = 3.5, outer = T, cex = 1.5)
mtext(text = "ENP Seats", side = 2, line = 3.5, outer = T, cex = 1.5)

par(xpd = NA)
legend( x = 1940, y = .9, legend = c("Reform Country", "Synthetic Control", "Reform Adoption"), lty = c(1,2,3), ncol = 1)

dev.off()