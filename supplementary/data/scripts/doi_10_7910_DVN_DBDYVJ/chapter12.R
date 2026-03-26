rm(list = ls())



## residual plots
library(ggplot2)

## residual plots
n = 200
x1 = rexp(n)
x2 = runif(n)
y = x1 + x2 + rnorm(n)
lmfit = lm(y ~ x1 + x2)
res = lmfit$res
fitted = lmfit$fitted
dat1 = data.frame(x = c(x1, x2, fitted),
                  y = rep(res, 3),
                  factor = rep(c("x1", "x2", "yhat"),
                               each = n),
                  model =  "lin.homosk")


y = x1 + x2 + rnorm(n, 0, x1+x2)
lmfit = lm(y ~ x1 + x2)
res = lmfit$res
fitted = lmfit$fitted
dat2 = data.frame(x = c(x1, x2, fitted),
                  y = rep(res, 3),
                  factor = rep(c("x1", "x2", "yhat"),
                               each = n),
                  model = "lin.heterosk")


y = x1^2 + x2^2 + rnorm(n)
lmfit = lm(y ~ x1 + x2)
res = lmfit$res
fitted = lmfit$fitted
dat3 = data.frame(x = c(x1, x2, fitted),
                  y = rep(res, 3),
                  factor = rep(c("x1", "x2", "yhat"),
                               each = n),
                  model = "quad.homosk")


y = x1^2 + x2^2 + rnorm(n, 0, x1+x2)
lmfit = lm(y ~ x1 + x2)
res = lmfit$res
fitted = lmfit$fitted
dat4 = data.frame(x = c(x1, x2, fitted),
                  y = rep(res, 3),
                  factor = rep(c("x1", "x2", "yhat"),
                               each = n),
                  model = "quad.heterosk")

dat = rbind(dat1, dat2, dat3, dat4)

ggplot(dat) + 
  geom_point(aes(x = x, y = y),
             size = 0.2, alpha = 0.7) + 
  facet_grid(model ~ factor, 
             scales = "free_x") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank()) +
  ylab("residuals")
ggsave("residualplots.pdf", height = 6, width = 6)  





## Figure: problem of population OLS
pdf("populationOLS.pdf", height = 5, width = 6)
par(mfrow = c(1, 1))
x = seq(-1, 1, 0.001)
y = x^2
## approximation 1 when x ~ unif(-1,1)
ols1 = rep(1/3, length(x))
## approximation 2 when x ~ unif(0,1)
x2   = seq(0, 1, 0.001)
ols2 = -1/6 + x2
## approximation 3 when x ~ unif(-1,0)
x3   = seq(-1, 0, 0.001)
ols3 = -1/6 - x3


plot(y ~ x, type = "l",
     xlab = expression(x), ylab = expression(y),
     ylim = c(-1/5, 1), lwd = 3, lty = 2, 
     col = "grey", bty = "n")
lines(ols1 ~ x)     
lines(ols2 ~ x2)
lines(ols3 ~ x3)

text(-0.18, -0.1, expression(F[3]))
text(0.18, -0.1, expression(F[2]))
text(0, 0.38, expression(F[1]))
legend("top", c("true response curve", "best linear approximations"), 
       lty = c(2, 1), lwd = c(3, 1),
       col = c("grey", "black"))
dev.off()   





## conformal prediction 
## Boston housing data: leave one out prediction
library("mlbench")
data(BostonHousing)
attach(BostonHousing)
n = dim(BostonHousing)[1]
p = dim(BostonHousing)[2] - 1
ymin = min(medv)
ymax = max(medv)
grid.y  = seq(ymin - 30, ymax + 30, 0.1)
BostonHousing = BostonHousing[order(medv), ]
detach(BostonHousing)

ols.fit.full = lm(medv ~ ., data = BostonHousing,
                  x = TRUE, y = TRUE, qr = TRUE)
beta     = ols.fit.full$coef
e.sigma  = summary(ols.fit.full)$sigma
X        = ols.fit.full$x
Y        = ols.fit.full$y
X.QR     = ols.fit.full$qr
X.Q      = qr.Q(X.QR)
X.R      = qr.R(X.QR)
Gram.inv = solve(t(X.R)%*%X.R)
hatmat   = X.Q%*%t(X.Q) 
resmat   = diag(n) - hatmat
leverage = diag(hatmat)
Resvec   = ols.fit.full$residuals

cvt  = qt(0.975, df = n-p-1)
cvr  = ceiling(0.95*(n+1))

loo.pred = matrix(0, n, 5)
loo.cov  = matrix(0, n, 2)
for(i in 1:n)
{
  beta.i = beta - Gram.inv%*%X[i, ]*Resvec[i]/(1-leverage[i])
  e.sigma.i = sqrt(e.sigma^2*(n - p) - 
                     (Resvec[i])^2/(1 - leverage[i]))/
              sqrt(n - p - 1)
  pred.i = sum(X[i, ]*beta.i) 
  lower.i = pred.i - cvt*e.sigma.i/sqrt(1 - leverage[i])
  upper.i = pred.i + cvt*e.sigma.i/sqrt(1 - leverage[i])
  loo.pred[i, 1:3] = c(pred.i, lower.i, upper.i)
  loo.cov[i, 1] = findInterval(Y[i], c(lower.i, upper.i))
  
  grid.r  = sapply(grid.y,
                   FUN = function(y){
                     Res = Resvec + resmat[, i]*(y - Y[i]) 
                     rank(abs(Res))[i]
                   })
  Cinterval = range(grid.y[grid.r<=cvr])
  loo.pred[i, 4:5] = Cinterval
  loo.cov[i, 2] = findInterval(Y[i], Cinterval)
  
}

colnames(loo.pred) = c("point", "G.l", "G.u", "c.l", "c.u")
head(loo.pred)
 
## coverage rates
apply(loo.cov==1, 2, mean)


## plots
library("ggplot2")
first20 = 1:20
dat1 = data.frame(factor  = "first 20",
                  index   = first20,
                  yobs    = Y[first20],
                  p.pred  = loo.pred[first20, 1],
                  Gpred.l = loo.pred[first20, 2],
                  Gpred.u = loo.pred[first20, 3],
                  Cpred.l = loo.pred[first20, 4],
                  Cpred.u = loo.pred[first20, 5])


last20 = (n-20+1):n
dat2 = data.frame(factor  = "last 20",
                  index   = last20,
                  yobs    = Y[last20],
                  p.pred  = loo.pred[last20, 1],
                  Gpred.l = loo.pred[last20, 2],
                  Gpred.u = loo.pred[last20, 3],
                  Cpred.l = loo.pred[last20, 4],
                  Cpred.u = loo.pred[last20, 5])

mid20 = 241:260
dat3 = data.frame(factor  = "middle 20",
                  index   = mid20,
                  yobs    = Y[mid20],
                  p.pred  = loo.pred[mid20, 1],
                  Gpred.l = loo.pred[mid20, 2],
                  Gpred.u = loo.pred[mid20, 3],
                  Cpred.l = loo.pred[mid20, 4],
                  Cpred.u = loo.pred[mid20, 5])

dat = rbind(dat1, dat2, dat3)
dat$factor = ordered(dat$factor, 
                     levels = c("first 20", "middle 20", "last 20"))


ggplot(dat) + 
  geom_point(aes(x=index, y=yobs), pch = 19, 
             cex = 0.3, col = "black") +
geom_line(aes(x=index, y=p.pred,
              linetype = "predict"), 
          col = "grey") +
  geom_line(aes(x=index, y=Gpred.l, 
                linetype = "Normal"), 
            col = "grey") +
  geom_line(aes(x=index, y=Gpred.u, 
                linetype = "Normal"), 
            col = "grey") +
  geom_line(aes(x=index, y=Cpred.l, 
                linetype = "conformal"), 
            col = "grey") +
  geom_line(aes(x=index, y=Cpred.u, 
                linetype = "conformal"), 
            col = "grey") +
  scale_linetype_manual(breaks = c("predict", "Normal", "conformal"),
                        values = c(3, 2, 1)) + 
  facet_wrap(~factor, scale = "free") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_blank()) +
  xlab("indices") + ylab("outcomes and predicted values")
ggsave("GCprediction_intervals.pdf",
    height = 4, width = 8.5) 



ratio.length = (loo.pred[, 3] - loo.pred[, 2])/
                 (loo.pred[, 5] - loo.pred[, 4])
mean(ratio.length)
dat = data.frame(x = 1:n, y = ratio.length)
ggplot(dat) + 
  geom_point(aes(x, y), cex = 0.5, col = "grey") +
  geom_hline(yintercept = 1) + 
  xlab("observation i") + 
  ylab("Normal over conformal") +
  ggtitle("ratio of the lengths of leave-one-out prediction intervals") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("GaussianConformalLOOpred.pdf", 
    height = 4, width = 8.5)


