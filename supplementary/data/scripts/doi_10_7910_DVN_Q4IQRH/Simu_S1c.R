library(latex2exp)
workpath = "./SimuBP"
source(paste(workpath, "/Script/SimuBP.R", sep = ""))
source(paste(workpath, "/Script/pLD.R", sep = ""))

b1 = 1
N0 = 1
k = 1e3
Nt = 6e4
mu = 2e-4
pmf = pLD(b1, N0, Nt, mu, k)
cdf = cumsum(pmf)

tp = log(Nt) / b1
para = c(1, 1)
offd = c(0, 0, 1)
bran = list(span = "exp", para = para, offd = offd)
mupr = c(mu / b1 * log(2), 0)
n0 = c(1, 0)
nsample = 1e2
nsimu = 1e3
z.mat = matrix(NA, nsimu, nsample)
x.mat = matrix(NA, nsimu, nsample)
pval.x.vec = rep(NA, nsimu)
runt.vec = rep(NA, nsimu)
set.seed(0)
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
  ksx = ks.test(x.vec, cdf)
  pval.x.vec[j] = ksx$p.value
  z.mat[j, ] = z.vec
  x.mat[j, ] = x.vec
}

# setEPS()
# postscript(paste(workpath, "/Result/FigA1.eps", sep = ""), width = 3.5, height = 3.5)
x11()
cex1 = 0.8; cex2 = 0.6
plot(ecdf(x.vec), main = TeX("Distribution of number of mutant cells $x_t$ by time of plating"), xlim = c(0, k), pch = 16, cex.main = cex2, cex.lab = cex2, cex.axis = cex2, xlab = TeX("$x_t$"), ylab = "Cumulative Probability")
lines(0 : k, cdf, type = "l", col = "red", lwd = 2)
text(600, 0.6, "pval (K-S) < 2.2e-16", cex = cex2)
legend("bottomright", TeX("$LD(m,\\phi)$"), col = "red", lwd = 2, cex = cex2, bty = "n")
# dev.off()

print(paste("Proportions of significant K-S tests for xt: ", mean(pval.x.vec < 0.05), sep = ""))
# 1
print(paste("Average running time per simulation: ", mean(runt.vec), sep = ""))
# 2.19480
