rm(list = ls())


## Figure 15.4
pdf("softthresholdingplot.pdf", height = 5, width = 7)
lambda = 2
b0 = seq(-5, 5, 0.01)
Soft = sign(b0)*pmax(0, abs(b0)-lambda)
plot(Soft ~ b0, type = "l",
     xlab = expression(b[0]), 
     ylab = expression(S(b[0],lambda)),
     main = expression(lambda==2))   
grid()
lines(Soft ~ b0, lwd = 2)  
dev.off()




## Chapter 15.4
library("mlbench")
library("glmnet")
library("MASS")
data(BostonHousing)

## training and testing data
set.seed(230)
nsample = dim(BostonHousing)[1]
trainindex = sample(1:nsample, floor(nsample*0.9))

xmatrix = model.matrix(medv ~ ., data = BostonHousing)[, -1]
yvector = BostonHousing$medv 
dat = data.frame(yvector, xmatrix)

## linear regression
bostonlm = lm(yvector ~ ., data = dat[trainindex, ])
predicterror = dat$yvector[- trainindex] - 
                    predict(bostonlm, dat[- trainindex, ])
mse.ols = sum(predicterror^2)/length(predicterror)

## ridge regression 
lambdas= seq(0, 5, 0.01)
lm0 = lm.ridge(yvector ~ ., data = dat[trainindex, ],
               lambda = lambdas)
coefridge = coef(lm0)[which.min(lm0$GCV), ]
predicterrorridge = dat$yvector[- trainindex] -
  cbind(1, xmatrix[- trainindex, ])%*%coefridge
mse.ridge = sum(predicterrorridge^2)/length(predicterrorridge)

## lasso 
cvboston = cv.glmnet(x = xmatrix[trainindex, ], y = yvector[trainindex])
coeflasso = coef(cvboston, s = "lambda.min")
predicterrorlasso = dat$yvector[- trainindex] -
                        cbind(1, xmatrix[- trainindex, ])%*%coeflasso
mse.lasso = sum(predicterrorlasso^2)/length(predicterrorlasso)

c(mse.ols, mse.ridge, mse.lasso)


dat.plot = rbind(data.frame(estimator = "ridge",
                            index = 1:13,
                            coef = coefridge[-1]),
                 data.frame(estimator = "lasso",
                            index = 1:13,
                            coef = coeflasso[-1]))
library(ggplot2)
original = ggplot(dat.plot) + 
  geom_point(aes(x=index, y=coef), 
             cex = 0.5)+ 
  geom_hline(aes(yintercept = 0)) + 
  facet_grid(~estimator) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlab("indices of covariates") + 
  ylab("coefficients") + 
  ggtitle("(a) original data")


## adding more noisy covariates
n.noise = 200
xnoise  = matrix(rnorm(nsample*n.noise), nsample, n.noise)
xmatrix = cbind(xmatrix, xnoise)
dat = data.frame(yvector, xmatrix)

## linear regression
bostonlm = lm(yvector ~ ., data = dat[trainindex, ])
predicterror = dat$yvector[- trainindex] - 
  predict(bostonlm, dat[- trainindex, ])
mse.ols = sum(predicterror^2)/length(predicterror)

## ridge regression 
lambdas= seq(100, 150, 0.01)
lm0 = lm.ridge(yvector ~ ., data = dat[trainindex, ],
               lambda = lambdas)
coefridge = coef(lm0)[which.min(lm0$GCV), ]
predicterrorridge = dat$yvector[- trainindex] -
  cbind(1, xmatrix[- trainindex, ])%*%coefridge
mse.ridge = sum(predicterrorridge^2)/length(predicterrorridge)


## lasso 
cvboston = cv.glmnet(x = xmatrix[trainindex, ], y = yvector[trainindex])
coeflasso = coef(cvboston, s = "lambda.min")

predicterrorlasso = dat$yvector[- trainindex] -
  cbind(1, xmatrix[- trainindex, ])%*%coeflasso
mse.lasso = sum(predicterrorlasso^2)/length(predicterrorlasso)

c(mse.ols, mse.ridge, mse.lasso)


dat.plot = rbind(data.frame(estimator = "ridge",
                            index = 1:n.noise,
                            coef = coefridge[-(1:14)]),
                 data.frame(estimator = "lasso",
                            index = 1:n.noise,
                            coef = coeflasso[-(1:14)]))

noise = ggplot(dat.plot) + 
  geom_point(aes(x=index, y=coef), 
             cex = 0.5)+ 
  geom_hline(aes(yintercept = 0)) + 
  facet_grid(~estimator) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlab("indices of covariates for the noise") + 
  ylab("coefficients") +
  ggtitle("(b) original data + noisy covariates")


library("gridExtra")
pdf("ridge_lasso_coef_bostonhousing.pdf",
       width = 8.5, height = 8)
grid.arrange(original, noise, nrow = 2) 
dev.off()

 