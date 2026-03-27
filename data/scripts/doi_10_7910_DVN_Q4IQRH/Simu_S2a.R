library(latex2exp)
workpath = "./SimuBP"
source(paste(workpath, "/Script/SimuBP.R", sep = ""))
source(paste(workpath, "/Script/Alg2.R", sep = ""))

a.vec = c(1, 1)
z0 = 1
mu = 2e-4
offd = c(0, 0, 1)
bran = list(span = "exp", para = a.vec, offd = offd)
mupr = c(mu, 0)
n0 = c(z0, 0)
tp = 11
nsample = 1e2
nsimu = 1e3
pval.x.vec = rep(NA, nsimu)
pval.z.vec = rep(NA, nsimu)
z1.mat = matrix(NA, nsimu, nsample)
x1.mat = matrix(NA, nsimu, nsample)
z2.mat = matrix(NA, nsimu, nsample)
x2.mat = matrix(NA, nsimu, nsample)
runt.vec = rep(NA, nsimu)

set.seed(1)
for (j in 1 : nsimu)
{
  z1.vec = rep(NA, nsample)
  x1.vec = rep(NA, nsample)
  z2.vec = rep(NA, nsample)
  x2.vec = rep(NA, nsample)
  runt = system.time({
    for (i in 1 : nsample)
    {
      data = SimuBP(bran, mupr, n0, tp)
      z1.vec[i] = data[1]
      x1.vec[i] = data[2]
    }
    z1.mat[j, ] = z1.vec
    x1.mat[j, ] = x1.vec
    
    for (i in 1 : nsample)
    {
      res = Alg2(z0, a.vec, mu, tp)
      z2.vec[i] = res[1]
      x2.vec[i] = res[2]
    }
    z2.mat[j, ] = z2.vec
    x2.mat[j, ] = x2.vec
    
    ksz = ks.test(z1.vec, z2.vec)
    pval.z.vec[j] = ksz$p.value
    ksx = ks.test(x1.vec, x2.vec)
    pval.x.vec[j] = ksx$p.value
  })
  runt.vec[j] = runt[3]
}

# setEPS()
# postscript(paste(workpath, "/Result/Fig5.eps", sep = ""), width = 3.5, height = 7)
x11()
par(mfrow = c(2, 1))
k = 4
zmin = min(min(z1.mat), min(z2.mat))
zmax = max(quantile(z1.mat, 0.99), quantile(z2.mat, 0.99))
xmin = min(min(x1.mat), min(x2.mat))
xmax = max(quantile(x1.mat, 0.99), quantile(x2.mat, 0.99))
plot(ecdf(z1.mat[k, ]), xlim = c(zmin, zmax), main = TeX(paste("ECDF of $z_t$ when $t_p=$", tp, sep = "")), pch = 16, cex.main = 0.8, cex.axis = 0.8, xlab = "", ylab = "")
plot(ecdf(z2.mat[k, ]), pch = 16, col = "red", add = T)
text(zmax * 2 / 3, 0.6, paste("pval (K-S) = ", format(pval.z.vec[k], digits = 3), sep = ""), cex = 0.8)
legend("bottomright", c("SimuBP (Algorithm 1)", "Algorithm 2"), col = c("black", "red"), lwd = c(1, 1), pch = c(19, 19), cex = 0.8, bty = "n")
mtext("A", side = 2, line = 3, cex = 1.3, at = 1.4, las = 2)
plot(ecdf(x1.mat[k, ]), xlim = c(xmin, xmax), main = TeX(paste("ECDF of $x_t$ when $t_p=$", tp, sep = "")), pch = 16, cex.main = 0.8, cex.axis = 0.8, xlab = "", ylab = "")
plot(ecdf(x2.mat[k, ]), pch = 16, col = "red", add = T)
text(xmax * 2 / 3, 0.6, paste("pval (K-S) = ", format(pval.x.vec[k], digits = 3), sep = ""), cex = 0.8)
legend("bottomright", c("SimuBP (Algorithm 1)", "Algorithm 2"), col = c("black", "red"), lwd = c(1, 1), pch = c(19, 19), cex = 0.8, bty = "n")
mtext("B", side = 2, line = 3, cex = 1.3, at = 1.4, las = 2)
# dev.off()

print(paste("Proportions of significant K-S tests for zt: ", mean(pval.z.vec < 0.05), sep = ""))
# 0.038
print(paste("Proportions of significant K-S tests for xt: ", mean(pval.x.vec < 0.05), sep = ""))
# 0.053
print(paste("Average running time per simulation: ", mean(runt.vec), sep = ""))
# 5.27223
