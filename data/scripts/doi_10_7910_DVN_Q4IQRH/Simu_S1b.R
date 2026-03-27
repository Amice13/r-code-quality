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
y0 = 10
n0 = c(y0, 0)
tp = 11
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
  ksz = ks.test(z.vec, "pnbinom", y0, p.z)
  pval.z.vec[j] = ksz$p.value
  ksy = ks.test(y.vec, "pnbinom", y0, p.y)
  pval.y.vec[j] = ksy$p.value
  z.mat[j, ] = z.vec
  y.mat[j, ] = y.vec
}

# setEPS()
# postscript(paste(workpath, "/Result/Fig4.eps", sep = ""), width = 3.5, height = 7)
x11()
par(mfrow = c(2, 1))
k = 3
plot(ecdf(z.mat[k, ]), main = TeX(paste("ECDF of $z_t|y_0=10$ when $t_p=$", tp, sep = "")), pch = 16, cex.main = 0.8, cex.axis = 0.8, xlab = "", ylab = "")
l = min(z.mat[k, ])
u = max(z.mat[k, ])
lines(l : u, pnbinom(l : u, y0, p.z), col = "red", lwd = 2)
text(max(z.mat[k, ]) * 3 / 4, 0.4, paste("pval (K-S) = ", format(pval.z.vec[k], digits = 4), sep = ""), cex = 0.8)
legend("bottomright", TeX("$negbin(y0,e^{-at_p})$"), col = "red", lwd = 2, cex = 0.8, bty = "n")
mtext("A", side = 2, line = 3, cex = 1.3, at = 1.4, las = 2)
plot(ecdf(y.mat[k, ]), main = TeX(paste("ECDF of $y_t|y_0=10$ when $t_p=$", tp, sep = "")), pch = 16, cex.main = 0.8, cex.axis = 0.8, xlab = "", ylab = "")
l = min(y.mat[k, ])
u = max(y.mat[k, ])
lines(l : u, pnbinom(l : u, y0, p.y), col = "red", lwd = 2)
text(max(y.mat[k, ]) * 3 / 4, 0.4, paste("pval (K-S) = ", format(pval.y.vec[k], digits = 4), sep = ""), cex = 0.8)
legend("bottomright", TeX("$negbin(y0,e^{-a(1-2\\mu)t_p})$"), col = "red", lwd = 2, cex = 0.8, bty = "n")
mtext("B", side = 2, line = 3, cex = 1.3, at = 1.4, las = 2)
# dev.off()

print(paste("Proportions of significant K-S tests for zt: ", mean(pval.z.vec < 0.05), sep = ""))
# 0.053
print(paste("Proportions of significant K-S tests for yt: ", mean(pval.y.vec < 0.05), sep = ""))
# 0.053
print(paste("Average running time per simulation: ", mean(runt.vec), sep = ""))
# 16.89066
