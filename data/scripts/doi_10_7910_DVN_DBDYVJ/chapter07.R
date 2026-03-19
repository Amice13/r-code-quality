rm(list = ls())

## QR decomposition
X   = matrix(rnorm(7*3), 7, 3)
X
qrX = qr(X)
qr.Q(qrX)
qr.R(qrX)


## lm via QR
Y   = rnorm(7)
## "0 + " in lm: force R to drop the intercept
lmfit = lm(Y ~ 0 + X, qr = TRUE)
qr.Q(lmfit$qr)
qr.R(lmfit$qr)