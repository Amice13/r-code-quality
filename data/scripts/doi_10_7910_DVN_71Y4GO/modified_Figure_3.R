png("Figure_3.png", width = 6, height = 3, units = "in", pointsize = 8, res = 2000)
layout(matrix(1:8, 2, 4, byrow = TRUE))
par(mar = c(2, 4, 4, 0.5), lwd = 0.5)
EE.plot(gender.high.main.result, gender.low.main.result, 9, 
        main = "Gender", sub = "female (v. male)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(age.high.main.result, age.low.main.result, 9, 
        main = "Age", sub = "34 (v. 70)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(education.high.main.result, education.low.main.result, 9, 
        main = "Education", sub = "UTokyo (v. high school)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(occupation.high.main.result, occupation.low.main.result, 9, 
        main = "Prior occupation", sub = "local politician (v. celebrity)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(dynasty.high.main.result, dynasty.low.main.result, 9, 
        main = "Dynastic status", sub = "non-dynastic (v. minister's child)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(experience.high.main.result, experience.low.main.result, 9, 
        main = "Experience", sub = "two terms (v. no experience)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(party.high.main.result, party.low.main.result, 9, 
        main = "Party affiliation", sub = "LDP (v. CDP)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
# legend
par(mar = c(0, 4, 4, 0.5), lwd = 0.5, xpd = TRUE)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-0.2, 0.6), ylim = c(0.5, 3.5), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(0, 3.7, 0, 1.3, col = "gray")
segments(c(-0.2, 0.2, 0.4, 0.6), 3.7, 
         c(-0.2, 0.2, 0.4, 0.6), 1.3, lty = 3, col = "gray")
points(0.3, 3.2, pch = 4)
points(0.1, 2.6, pch = 4)
points(0.2, 1.6, pch = 4)
segments(0.3, 3.15, 0.3, 2.4, lwd = 0.25)
segments(0.1, 2.55, 0.1, 2.4, lwd = 0.25)
arrows(0.1, 2.4, 0.3, 2.4, length = 0.02, code = 3, lwd = 0.25)
segments(0.1, 2.4, 0, 1.6, lty = 3, lwd = 0.25)
segments(0.3, 2.4, 0.2, 1.6, lty = 3, lwd = 0.25)
arrows(0, 1.6, 0.2, 1.6, length = 0.02, code = 3, lwd = 0.25)
text(0.3, 3.2, "Effect for NMA\n(w/o special notes)", pos = 3)
text(0.05, 2.6, "Effect for MMA\n(w/ special notes)", pos = 3)
text(0.27, 1.9, "EE is the diff.\nb/w AMCE\nand ACDE", pos = 4)
mtext("AMCE", side = 2, at = 3.2, las = 1, cex = 0.8)
mtext("ACDE", side = 2, at = 2.6, las = 1, cex = 0.8)
mtext("EE", side = 2, at = 1.6, las = 1, cex = 0.8)
mtext("Notes and legends", line = 1.5, cex = 1, font = 2)
legend("bottom", bty = "n", 
       legend = c("High-populist notes", "Low-populist notes"), 
       pch = c(19, 21), pt.bg = c(NA, "white"), cex = 1.2)
dev.off()