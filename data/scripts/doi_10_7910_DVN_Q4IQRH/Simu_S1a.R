library(latex2exp)
workpath = "./SimuBP"
source(paste(workpath, "/Script/SimuBP.R", sep = ""))

para = c(1, 1) # an example for span = "fixed" or span = "exp"
# para1 = cbind(c(0, 2), c(0, 2)) # an example for span = "unif"
# para2 = cbind(c(1, 1), c(1, 1)) # an example for span = "gam"
offd = c(0, 0, 1) # binary-fission
# bran = list(span = "fixed", para = para, offd = offd)
bran = list(span = "exp", para = para, offd = offd)
# bran = list(span = "unif", para = para1, offd = offd)
# bran = list(span = "gam", para = para2, offd = offd)
mupr = c(2e-4, 0)
n0 = c(1, 0)
tp = 11
# data = SimuBP(bran, mupr, n0, tp)
nsample = 1e2
nsimu = 1e3
z.mat = matrix(NA, nsimu, nsample)
y.mat = matrix(NA, nsimu, nsample)
pval.z.vec = rep(NA, nsimu)
pval.y.vec = rep(NA, nsimu)
runt.vec = rep(NA, nsimu)
p.z = exp(-tp * para[1])
p.y = exp(-tp * para[1] * (1 - 2 * mupr[1]))

set.seed(1)
for (j in 1 : nsimu)
{
  z.vec = rep(NA, nsample)
  x.vec = rep(NA, nsample)
  runt = system.time({
    for (i in 1 : nsample)
    {
      data = SimuBP(bran, mupr, n0, tp)
      z.vec[i] = data[1]
      x.vec[i] = data[2]
    }
  })
  runt.vec[j] = runt[3]
  y.vec = z.vec - x.vec
  ksz = ks.test(z.vec, "pgeom", p.z)
  pval.z.vec[j] = ksz$p.value
  ksy = ks.test(y.vec, "pgeom", p.y)
  pval.y.vec[j] = ksy$p.value
  z.mat[j, ] = z.vec
  y.mat[j, ] = y.vec
}

# setEPS()
# postscript(paste(workpath, "/Result/Fig3.eps", sep = ""), width = 3.5, height = 7)
x11()
par(mfrow = c(2, 1))
cex1 = 0.8; cex2 = 0.6
k = 3
plot(ecdf(z.mat[k, ]), main = TeX("Distribution of total number of viable cells $z_t$ by time of plating"), pch = 16, cex.main = cex2, cex.lab = cex2, cex.axis = cex2, xlab = TeX("$z_t$"), ylab = "Cumulative Probability")
l = min(z.mat[k, ])
u = max(z.mat[k, ])
lines(l : u, pgeom(l : u, p.z), col = "red", lwd = 2)
text(max(z.mat[k, ]) * 2 / 3, 0.6, paste("pval (K-S) = ", format(pval.z.vec[k], digits = 4), sep = ""), cex = cex2)
legend("bottomright", TeX("$geo(e^{-at_p})$"), col = "red", lwd = 2, cex = cex2, bty = "n")
mtext("A", side = 2, line = 3, cex = cex1, at = 1.4, las = 2)
plot(ecdf(y.mat[k, ]), main = TeX("Distribution of number of wild-type cells $y_t$ by time of plating"), pch = 16, cex.main = cex2, cex.lab = cex2, cex.axis = cex2, xlab = TeX("$x_t$"), ylab = "Cumulative Probability")
l = min(y.mat[k, ])
u = max(y.mat[k, ])
lines(l : u, pgeom(l : u, p.y), col = "red", lwd = 2)
text(max(y.mat[k, ]) * 2 / 3, 0.6, paste("pval (K-S) = ", format(pval.y.vec[k], digits = 4), sep = ""), cex = cex2)
legend("bottomright", TeX("$geo(e^{-a(1-2\\mu)t_p})$"), col = "red", lwd = 2, cex = cex2, bty = "n")
mtext("B", side = 2, line = 3, cex = cex1, at = 1.4, las = 2)
# dev.off()

print(paste("Proportions of significant K-S tests for zt: ", mean(pval.z.vec < 0.05), sep = ""))
# 0.046
print(paste("Proportions of significant K-S tests for yt: ", mean(pval.y.vec < 0.05), sep = ""))
# 0.045
print(paste("Average running time per simulation: ", mean(runt.vec), sep = ""))
# 1.58827
