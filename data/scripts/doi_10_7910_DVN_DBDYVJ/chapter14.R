rm(list = ls())


library(MASS)

## setting 1
n = 200
p = 100
beta = rep(1/sqrt(p), p) 
sig  = 1/2
## independent Normals
X    = matrix(rnorm(n*p), n, p)
## standardize the covariates
X    = scale(X)
X    = X*sqrt(n/(n-1))
Y    = as.vector(X%*%beta + rnorm(n, 0, sig))


eigenxx = eigen(t(X)%*%X)
xis     = eigenxx$values
gammas  = t(eigenxx$vectors)%*%beta

lambda.seq = seq(0, 70, 0.01)
bias2.seq  = lambda.seq
var.seq    = lambda.seq
mse.seq    = lambda.seq
for(i in 1:length(lambda.seq))
{
	   ll = lambda.seq[i]
	   bias2.seq[i]  = ll^2*sum(gammas^2/(xis + ll)^2)
	   var.seq[i]    = sig^2*sum(xis/(xis + ll)^2)
	   mse.seq[i]    = bias2.seq[i] + var.seq[i]
}
 
y.min = min(bias2.seq, var.seq, mse.seq)
y.max = max(bias2.seq, var.seq, mse.seq)
pdf("biasvariancetradeoffridgeplot.pdf", 
    height = 10, width = 8.5)
par(mfrow = c(2, 2))
plot(bias2.seq ~ lambda.seq, type = "l",
     ylim = c(y.min, y.max), 
     xlab = expression(lambda), main = "", 
     ylab = "bias-variance tradeoff", 
     lty = 2, bty = "n")
lines(var.seq ~ lambda.seq, lty = 3)
lines(mse.seq ~ lambda.seq, lwd = 3, lty = 1)
abline(v = lambda.seq[which.min(mse.seq)], 
       lty = 1, col = "grey")
legend("topright", c("bias", "variance", "mse"),
       lty = c(2, 3, 1), lwd = c(1, 1, 4), bty = "n")



## ridge regression
ridge.fit = lm.ridge(Y ~ X, lambda = lambda.seq)
abline(v = lambda.seq[which.min(ridge.fit$GCV)], 
       lty = 2, col = "grey")
abline(v = ridge.fit$kHKB, lty = 3, col = "grey")
abline(v = ridge.fit$kLW, lty = 4, col = "grey")
legend("bottomright", 
       c("MSE", "GCV", "HKB", "LW"),
       lty = 1:4, col = "grey", bty = "n")


## prediction
X.new    = matrix(rnorm(n*p), n, p) 
X.new    = scale(X.new)
X.new    = X.new*matrix(sqrt(n/(n-1)), n, p)
Y.new    = as.vector(X.new%*%beta + rnorm(n, 0, sig))
predict.error = Y.new - X.new%*%ridge.fit$coef
predict.mse   = apply(predict.error^2, 2, mean)
plot(predict.mse ~ lambda.seq, type = "l",
     xlab = expression(lambda),
     ylab = "predicted MSE", bty = "n")
abline(v = lambda.seq[which.min(mse.seq)], 
       lty = 1, col = "grey")
abline(v = lambda.seq[which.min(ridge.fit$GCV)], 
       lty = 2, col = "grey")
abline(v = ridge.fit$kHKB, lty = 3, col = "grey")
abline(v = ridge.fit$kLW, lty = 4, col = "grey")
legend("bottomright", 
       c("MSE", "GCV", "HKB", "LW"),
       lty = 1:4, col = "grey", bty = "n")

mtext("independent covariates", side = 1,
      line = -58, outer = TRUE, font.main = 1, cex=1.5)
 



##########################################
##########################################
## setting 2
n = 200
p = 100
beta = rep(1/sqrt(p), p) 
sig  = 1/2
## correlated Normals
X    = matrix(rnorm(n*p), n, p) + rnorm(n, 0, 0.5)
## standardize the covariates
X    = scale(X)
X    = X*matrix(sqrt(n/(n-1)), n, p)
Y    = as.vector(X%*%beta + rnorm(n, 0, sig))

eigenxx = eigen(t(X)%*%X)
xis     = eigenxx$values
gammas  = t(eigenxx$vectors)%*%beta

lambda.seq = seq(0, 800, 1)
bias2.seq  = lambda.seq
var.seq    = lambda.seq
mse.seq    = lambda.seq
for(i in 1:length(lambda.seq))
{
  ll = lambda.seq[i]
  bias2.seq[i]  = ll^2*sum(gammas^2/(xis + ll)^2)
  var.seq[i]    = sig^2*sum(xis/(xis + ll)^2)
  mse.seq[i]    = bias2.seq[i] + var.seq[i]
}

y.min = min(bias2.seq, var.seq, mse.seq)
y.max = max(bias2.seq, var.seq, mse.seq)
plot(bias2.seq ~ lambda.seq, type = "l",
     ylim = c(y.min, y.max), 
     xlab = expression(lambda), main = "", 
     ylab = "bias-variance tradeoff", 
     lty = 2, bty = "n")
lines(var.seq ~ lambda.seq, lty = 3)
lines(mse.seq ~ lambda.seq, lwd = 3, lty = 1)
abline(v = lambda.seq[which.min(mse.seq)], 
       lty = 1, col = "grey")
legend("topright", c("bias", "variance", "mse"),
       lty = c(2, 3, 1), lwd = c(1, 1, 4), bty = "n")



## ridge regression
ridge.fit = lm.ridge(Y ~ X, lambda = lambda.seq)
abline(v = lambda.seq[which.min(ridge.fit$GCV)], 
       lty = 2, col = "grey")
abline(v = ridge.fit$kHKB, lty = 3, col = "grey")
abline(v = ridge.fit$kLW, lty = 4, col = "grey")
legend("right", 
       c("MSE", "GCV", "HKB", "LW"),
       lty = 1:4, col = "grey", bty = "n")



## prediction
X.new    = matrix(rnorm(n*p), n, p) + rnorm(n, 0, 0.5)
X.new    = scale(X.new)
X.new    = X.new*matrix(sqrt(n/(n-1)), n, p)
Y.new    = as.vector(X.new%*%beta + rnorm(n, 0, sig))
predict.error = Y.new - X.new%*%ridge.fit$coef
predict.mse   = apply(predict.error^2, 2, mean)
plot(predict.mse ~ lambda.seq, type = "l",
     xlab = expression(lambda),
     ylab = "predicted MSE", bty = "n")
abline(v = lambda.seq[which.min(mse.seq)], 
       lty = 1, col = "grey")
abline(v = lambda.seq[which.min(ridge.fit$GCV)], 
       lty = 2, col = "grey")
abline(v = ridge.fit$kHKB, lty = 3, col = "grey")
abline(v = ridge.fit$kLW, lty = 4, col = "grey")
legend("bottomright", 
       c("MSE", "GCV", "HKB", "LW"),
       lty = 1:4, col = "grey", bty = "n")

mtext("correlated covariates", side = 1,
      line = -28, outer = TRUE, font.main = 1, cex=1.5)
dev.off()

