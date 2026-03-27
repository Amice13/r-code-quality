library(latex2exp)
workpath = "./SimuBP"

load(paste(workpath, "/Result/S2a.RData", sep = ""))
load(paste(workpath, "/Result/S2b.RData", sep = ""))
tp = 11
nsimu = 1e3
pval.z.vec = rep(NA, nsimu)
pval.x.vec = rep(NA, nsimu)
for (j in 1 : nsimu)
{
  z2.vec = z2.mat[j, ]
  x2.vec = x2.mat[j, ]
  z3.vec = z3.mat[j, ]
  x3.vec = x3.mat[j, ]
  
  ksz = ks.test(z2.vec, z3.vec)
  pval.z.vec[j] = ksz$p.value
  ksx = ks.test(x2.vec, x3.vec)
  pval.x.vec[j] = ksx$p.value
}

# setEPS()
# postscript(paste(workpath, "/Result/FigA2.eps", sep = ""), width = 3.5, height = 7)
x11()
par(mfrow = c(2, 1))
cex1 = 0.8; cex2 = 0.6
k = 6
zmin = min(min(z2.mat), min(z3.mat))
zmax = max(quantile(z2.mat, 0.99), quantile(z3.mat, 0.99))
xmin = min(min(x2.mat), min(x3.mat))
xmax = max(quantile(x2.mat, 0.99), quantile(x3.mat, 0.99))
plot(ecdf(z2.mat[k, ]), xlim = c(zmin, zmax), main = TeX("Distribution of total number of viable cells $z_t$ by time of plating"), pch = 16, cex.main = cex2, cex.lab = cex2, cex.axis = cex2, xlab = TeX("$z_t$"), ylab = "Cumulative Probability")
plot(ecdf(z3.mat[k, ]), pch = 16, col = "red", add = T)
text(zmax * 2 / 3, 0.6, paste("pval (K-S) = ", format(pval.z.vec[k], digits = 4), sep = ""), cex = cex2)
legend("bottomright", c("Algorithm 2", "Algorithm 3"), col = c("black", "red"), lwd = c(1, 1), pch = c(19, 19), cex = cex2, bty = "n")
mtext("A", side = 2, line = 3, cex = cex1, at = 1.4, las = 2)
plot(ecdf(x2.mat[k, ]), xlim = c(xmin, xmax), main = TeX("Distribution of number of mutant cells $x_t$ by time of plating"), pch = 16, cex.main = cex2, cex.lab = cex2, cex.axis = cex2, xlab = TeX("$x_t$"), ylab = "Cumulative Probability")
plot(ecdf(x3.mat[k, ]), pch = 16, col = "red", add = T)
text(xmax * 2 / 3, 0.6, paste("pval (K-S) = ", format(pval.x.vec[k], digits = 4), sep = ""), cex = cex2)
legend("bottomright", c("Algorithm 2", "Algorithm 3"), col = c("black", "red"), lwd = c(1, 1), pch = c(19, 19), cex = cex2, bty = "n")
mtext("B", side = 2, line = 3, cex = cex1, at = 1.4, las = 2)
# dev.off()

print(paste("Proportions of significant K-S tests for zt: ", mean(pval.z.vec < 0.05), sep = ""))
# 0.036
print(paste("Proportions of significant K-S tests for xt: ", mean(pval.x.vec < 0.05), sep = ""))
# 0.025
