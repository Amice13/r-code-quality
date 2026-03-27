##
## Generate figures used for illustrating assumptions
##


## load package 
require(pBrackets)

## setup
out_1  <- c(9, 13)
out_0  <- c(3, 2)
out_10 <- c(9, 8)


## Extended Parallel Trends
e_out_1  <- c(11, 9, 13)
e_out_0  <- c(5, 3, 2)
e_out_10 <- c(11, 9, 8)

e2_out_1  <- c(11, 9, 13)
e2_out_0  <- c(2, 3, 2)
e2_out_10 <- c(11, 9, 9)


##
## Figure for extended parallel trends
##
pdf("../results/figures/figure1_e_parallel_final.pdf", height  =  5, width = 12)
par(mfrow = c(1,2), mar = c(4, 1, 3, 1), oma = c(0, 2, 0, 0))

  plot(seq(1:3),  e_out_1, pch = 21, lwd = 3,  col = "black", bg = "black",
      type ="o", cex = 2,  ylim = c(0, 14), xlim = c(0.75, 3.45), xaxt = "n", xlab = "",
      ylab = "", yaxt = "n",
      main = "Extended Parallel Trends", cex.main = 1.5)
  points(seq(1:3),  e_out_0, pch = 23, lwd = 3,  col = "black", bg = "gray", type ="o", cex = 2)
  points(seq(1:3),  e_out_10, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "white", type ="o", cex = 2)
  points(seq(1:3),  e_out_1, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "black", type ="o", cex = 2)

  Axis(side = 1, at =  seq(1:3),
      labels = c("t = 0\n(before)", "t = 1\n(before)", "t = 2\n(after)"), line = 1.25,
      lwd =  0, tck = 0, cex.axis = 1.25)
  Axis(side = 1, at =  seq(1:3), labels = c("", "", ""))
  mtext(side = 2, at =  7, text = "Outcome", cex = 1.25, line =  1)

  text(x= 2, y= 13, "Treatment Group", col = "black", cex = 1.25)
  text(x= 2, y= 1, "Control Group", col = "black", cex = 1.25)
  text(x= 3, y= 9.3, "Counterfactual", col = "black", cex = 1.25)

  par(mar = c(4, 1, 3, 1))
  plot(seq(1:3),  e2_out_1, pch = 21, lwd = 3,  col = "black", bg = "black",
      type ="o", cex = 2,  ylim = c(0, 14), xlim = c(0.75, 3.45), xaxt = "n", xlab = "",
      ylab = "", yaxt = "n",
      main = "Extended Parallel Trends Violated", cex.main = 1.5)
  points(seq(1:3),  e2_out_0, pch = 23, lwd = 3,  col = "black", bg = "gray", type ="o", cex = 2)
  points(seq(1:3),  e2_out_10, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "white", type ="o", cex = 2)
  points(seq(1:3),  e2_out_1, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "black", type ="o", cex = 2)

  Axis(side = 1, at =  seq(1:3),
      labels = c("t = 0\n(before)", "t = 1\n(before)", "t = 2\n(after)"), line = 1.25,
      lwd =  0, tck = 0, cex.axis = 1.25)
  Axis(side = 1, at =  seq(1:3), labels = c("", "", ""))
dev.off()


##
## Figure for Sequential Parallel Trends
##
e_out_1  <- c(11, 9, 13)
e_out_0  <- c(5, 3, 2)
e_out_10 <- c(11, 9, 8)

e2_out_1  <- c(11, 9, 13)
e2_out_0  <- c(6.5, 3, 0.5)
e2_out_10 <- c(11, 9, 8)

e3_out_1  <- c(11, 9, 13)
e3_out_0  <- c(2, 3, 0)
e3_out_10 <- c(11, 9, 8)

pdf("../results/figures/figure2_s_parallel_all_trend_final.pdf", height  =  6, width = 13)
par(mfrow = c(1,3), mar = c(4, 1, 1, 1), oma = c(11, 2, 3, 0))
  plot(seq(1:3),  e_out_1, pch = 21, lwd = 3,  col = "black", bg = "black",
      type ="o", cex = 2,  ylim = c(-1,14), xlim = c(0.75, 3.45), xaxt = "n", xlab = "",
      ylab = "", yaxt = "n",
      main = "", cex.main = 1.5)
  points(seq(1:3),  e_out_0, pch = 23, lwd = 3,  col = "black", bg = "gray", type ="o", cex = 2)
  points(seq(1:3),  e_out_10, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "white", type ="o", cex = 2)
  points(seq(1:3),  e_out_1, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "black", type ="o", cex = 2)

  Axis(side = 1, at =  seq(1:3),
      labels = c("t = 0\n(before)", "t = 1\n(before)", "t = 2\n(after)"), line = 1.25,
      lwd =  0, tck = 0, cex.axis = 1.75)
  Axis(side = 1, at =  seq(1:3), labels = c("", "", ""))
  mtext(side = 2, at =  7, text = "Outcome", cex = 1.5, line =  1)

  text(x= 2, y= 13, "Treatment Group", col = "black", cex = 1.75)
  text(x= 2, y= 1, "Control Group", col = "black", cex = 1.75)
  text(x= 3, y= 10, "Counterfactual", col = "black", cex = 1.75)

  plot(seq(1:3),  e2_out_1, pch = 21, lwd = 3,  col = "black", bg = "black",
      type ="o", cex = 2,  ylim = c(-1,14), xlim = c(0.75, 3.45), xaxt = "n", xlab = "",
      ylab = "", yaxt = "n",
      main = "", cex.main = 1.5)
  points(seq(1:3),  e2_out_0, pch = 23, lwd = 3,  col = "black", bg = "gray", type ="o", cex = 2)
  points(seq(1:3),  e2_out_10, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "white", type ="o", cex = 2)
  points(seq(1:3),  e2_out_1, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "black", type ="o", cex = 2)

  Axis(side = 1, at =  seq(1:3),
      labels = c("t = 0\n(before)", "t = 1\n(before)", "t = 2\n(after)"), line = 1.25,
      lwd =  0, tck = 0, cex.axis = 1.75)
  Axis(side = 1, at =  seq(1:3), labels = c("", "", ""))

  plot(seq(1:3),  e3_out_1, pch = 21, lwd = 3,  col = "black", bg = "black",
      type ="o", cex = 2,  ylim = c(-1,14), xlim = c(0.75, 3.45), xaxt = "n", xlab = "",
      ylab = "", yaxt = "n",
      main = "", cex.main = 1.5)
  points(seq(1:3),  e3_out_0, pch = 23, lwd = 3,  col = "black", bg = "gray", type ="o", cex = 2)
  points(seq(1:3),  e3_out_10, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "white", type ="o", cex = 2)
  points(seq(1:3),  e3_out_1, pch = 21, lwd = 3,  lty = 2, col = "black", bg = "black", type ="o", cex = 2)

  Axis(side = 1, at =  seq(1:3),
      labels = c("t = 0\n(before)", "t = 1\n(before)", "t = 2\n(after)"), line = 1.25,
      lwd =  0, tck = 0, cex.axis = 1.75)
  Axis(side = 1, at =  seq(1:3), labels = c("", "", ""))

  mtext(side = 3, at =  0.175, text = "Extended Parallel Trends", outer = TRUE, cex = 1.5, font =  2, line =  0.5)
  mtext(side = 3, at =  0.5, text = "Parallel Trends-in-Trends", outer = TRUE, cex = 1.5, font = 2, line =  0.5)
  mtext(side = 3, at =  0.825, text = "Both are Violated", outer = TRUE, cex = 1.5, font = 2, line =  0.5)

  mtext(side = 1, at =  0.175, text = "Trend of Treatment Group", outer = TRUE, cex = 1.5, font = 2, line =  2)
  mtext(side = 1, at =  0.175, text = "(-2, -1)", outer = TRUE, cex = 1.5, font = 1, line =  4)

  mtext(side = 1, at =  0.175, text = "Trend of Control Group", outer = TRUE, cex = 1.5, font = 2, line = 7)
  mtext(side = 1, at =  0.175, text = "(-2, -1)", outer = TRUE, cex = 1.5, font = 1, line =  9)


  mtext(side = 1, at =  0.5, text = "Trend of Treatment Group", outer = TRUE, cex = 1.5, font = 2, line =  2)
  mtext(side = 1, at =  0.5, text = "(-2, -1)", outer = TRUE, cex = 1.5, font = 1, line =  4)

  mtext(side = 1, at =  0.5, text = "Trend of Control Group", outer = TRUE, cex = 1.5, font = 2, line = 7)
  mtext(side = 1, at =  0.5, text = "(-3.5, -2.5)", outer = TRUE, cex = 1.5, font = 1, line =  9)

  mtext(side = 1, at =  0.825, text = "Trend of Treatment Group", outer = TRUE, cex = 1.5, font = 2, line =  2)
  mtext(side = 1, at =  0.825, text = "(-2, -1)", outer = TRUE, cex = 1.5, font = 1, line =  4)

  mtext(side = 1, at =  0.825, text = "Trend of Control Group", outer = TRUE, cex = 1.5, font = 2, line = 7)
  mtext(side = 1, at =  0.825, text = "(1, -3)", outer = TRUE, cex = 1.5, font = 1, line =  9)
dev.off()