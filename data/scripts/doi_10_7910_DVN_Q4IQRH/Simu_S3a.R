library(latex2exp)
workpath = "./SimuBP"
source(paste(workpath, "/Script/SimuBP.R", sep = ""))

estmu = function(z.vec, x.vec, tol = 1e-10, niter = 1000)
{
  y.vec = z.vec - x.vec
  y.bar = mean(y.vec)
  z.bar = mean(z.vec)
  muh0 = (1 - log(y.bar) / log(z.bar)) / 2
  
  fun = function(muh) (1 - muh) * z.bar ^ (1 - 2 * muh) + (2 * y.bar - 1) * muh - y.bar
  fun.der = function(muh) (2 * (muh - 1) * log(z.bar) - 1) * z.bar ^ (1 - 2 * muh) + 2 * y.bar - 1
  muh.old = muh0
  for (i in 1 : niter)
  {
    muh.new = muh.old - fun(muh.old) / fun.der(muh.old)
    if (abs((muh.new - muh.old)) < tol)
    {
      break
    } else
    {
      muh.old = muh.new
    }
  }
  if (i == niter)
  {
    print("exceeded allowed number of iterations")
    muh.new = NA
  }
  
  return(muh.new)
}

nsimu = 100
mu.vec = 10 ^ (-7 : -2) 
nmu = length(mu.vec)
tp.vec = seq(16, 6, by = -2)
J = 100
para = c(1, 1)
offd = c(0, 0, 1)
bran = list(span = "exp", para = para, offd = offd)
n0 = c(1, 0)
runt.vec = rep(NA, nmu)
muhat.mat = matrix(NA, nsimu, nmu)
z.ary = array(NA, c(nmu, nsimu, J))
x.ary = array(NA, c(nmu, nsimu, J))

set.seed(1)
for (i in 1 : nmu)
{
  mu = mu.vec[i]
  tp = tp.vec[i]
  mupr = c(mu, 0)
  runt = system.time({
    for (j in 1 : nsimu)
    {
      z.vec = rep(NA, J)
      x.vec = rep(NA, J)
      for (k in 1 : J)
      {
        data = SimuBP(bran, mupr, n0, tp)
        z.vec[i] = data[1]
        x.vec[i] = data[2]
      }
      muhat.mat[j, i] = estmu(z.vec, x.vec)
      z.ary[i, j, ] = z.vec
      x.ary[i, j, ] = x.vec
    }
  }) #26626.42  3328.40   417.79    63.91    14.08     5.78 sec
  runt.vec[i] = runt[3]
}

bar.mat = rbind(log10(mu.vec), log10(colMeans(muhat.mat)))
# setEPS()
# postscript(paste(workpath, "/Result/Fig7.eps", sep = ""), width = 3.5, height = 3.5)
x11()
lab.vec = c(TeX("$\\mu_1=10^{-7}$"), TeX("$\\mu_1=10^{-6}$"), TeX("$\\mu_1=10^{-5}$"), TeX("$\\mu_1=10^{-4}$"), TeX("$\\mu_1=10^{-3}$"), TeX("$\\mu_1=10^{-2}$"))
col.vec = c("blue", "red")
barplot(bar.mat, col = col.vec, xlim = c(0, 19), ylim = c(-8, 0), beside = T, axes = F, legend = c(TeX("$\\mu_1$"), TeX("$\\hat{\\mu}_1$")), args.legend = list(x = 18, y = -6.5, cex = 1)) #, bty = "n"
axis(side = 2, at = -1 : -7)
for (i in 1 : nmu)
{
  mtext(lab.vec[i], side = 3, line = 0.5, at = 3 * i - 0.75)
}
box()
# dev.off()
