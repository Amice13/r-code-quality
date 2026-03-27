rm(list = ls())


library("gridExtra")
library("ggplot2")


## David Freedman's simulation 
freedmanR2 = function(n = 100, p = 50, alpha1 = 0.25)
{
        y = rnorm(n)
        x = matrix(rnorm(n*(p-1)), n, p - 1)
        
        lmyx      = summary(lm(y ~ x))	
        R2full    = lmyx$r.squared
        Fstatfull = lmyx$fstatistic
        Pfull     = 1 - pf(Fstatfull[1], df1 = Fstatfull[2], df2 = Fstatfull[3])
        
        selection1 = (lmyx$coefficients[, 4] < alpha1)[-1]
        
	      xselected  = x[, selection1]
	      
	      lmyxselected  = summary(lm(y ~ xselected))
	      R2selected    = lmyxselected$r.squared
	      Fstatselected = lmyxselected$fstatistic
	      Pselected     = 1 - pf(Fstatselected[1], 
	                             df1 = Fstatselected[2], df2 = Fstatselected[3])
	     
	      return(c(R2full, Pfull, R2selected, Pselected))
}

Freedmansimulation = replicate(10^3, expr = { freedmanR2() } )



dat.freedman = rbind(data.frame(factor1 = "full model",
                                factor2 = "R2", 
                                values = Freedmansimulation[1, ]),
                     data.frame(factor1 = "full model",
                                factor2 = "p-value",
                                values = Freedmansimulation[2, ]),
                     data.frame(factor1 = "selected model",
                                factor2 = "R2",
                                values = Freedmansimulation[3, ]),
                     data.frame(factor1 = "selected model",
                                factor2 = "p-value",
                                values = Freedmansimulation[4, ]))

xx = seq(0, 1, 0.001)
n = 100
p = 50
yy = dbeta(xx, shape1 = (p-1)/2, shape2 = (n-p)/2)
uu = rep(1, length(xx))
dat.line = rbind(data.frame(factor1 = "full model",
                            factor2 = "R2",
                            xx = xx,
                            curveyy = yy),
                 data.frame(factor1 = "full model",
                            factor2 = "p-value",
                            xx = xx,
                            curveyy = uu),
                 data.frame(factor1 = "selected model",
                            factor2 = "R2",
                            xx = xx,
                            curveyy = yy),
                 data.frame(factor1 = "selected model",
                            factor2 = "p-value",
                            xx = xx,
                            curveyy = uu))


plotr2 = ggplot(data = subset(dat.freedman, factor2 == "R2")) + 
  geom_histogram(aes(x = values, y = ..density..),
                 alpha = 0.5) +
  geom_line(data = subset(dat.line, factor2 == "R2"),
            aes(x= xx, y = curveyy), 
            linetype = 2, alpha = 0.5) +
  facet_wrap(~ factor1, scale = "free") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") + ylab(expression(R^2))


plotpvalue = ggplot(data = subset(dat.freedman, factor2 == "p-value")) + 
  geom_histogram(aes(x = values, y = ..density..),
                 breaks = (0:20)/20,
                 alpha = 0.5) +
  geom_line(data = subset(dat.line, factor2 == "p-value"),
            aes(x= xx, y = curveyy), 
            linetype = 2) +
  facet_wrap(~ factor1, , scale = "free") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("") + ylab("p-value") + xlim(c(0,1))

pdf("freedman_simulation_ggplot.pdf",
    height = 7, width = 7)
grid.arrange(plotr2, plotpvalue, nrow = 2) 
dev.off()







## Example 13.1 and Example 13.2
## linear model simulation
n = 200
p = 40
beta = c(rep(1, p/4), rep(0, p*3/4))

x.train = matrix(rnorm(n*p), n, p)
y.train = as.vector(x.train%*%beta) + rnorm(n, 0, 3)
data.train = data.frame(x = x.train, y = y.train)


x.test  = matrix(rnorm(n*p), n, p) 
y.test  = as.vector(x.test%*%beta) + rnorm(n, 0, 3)
x.test  = data.frame(x = x.test)



index      = 1:p
rms.train  = index 
rms.test   = index
for(j in index)
{
  x.formula    = paste0("x.", 1:j, collapse = "+")
  reg.formula  = as.formula(paste("y~", x.formula))
  reg.train    = lm(reg.formula, data = data.train)
  rms.train[j] = sum((reg.train$residuals)^2)/n
  
  pred.test    = predict(reg.train, newdata = x.test)
  rms.test[j]  = sum((y.test - pred.test)^2)/n
}

pdf("training_testing_error.pdf", height = 5, width = 6)
y.min = min(rms.train, rms.test)
y.max = max(rms.train, rms.test)
plot(rms.train ~ index, 
     ylim = c(y.min, y.max),
     type = "b", pch = 19, cex = 0.5,
     xlab = "# covariates", ylab = "RSS/n",
     font.main = 1,
     main = "prediction errors", bty = "n")
lines(rms.test ~ index, type = "b", pch = 9, cex = 0.5)  
abline(v = 10, lty = 2, col = "grey")   
legend("topright", c("training data", "testing data"), 
       pch = c(19, 9)) 
dev.off()


## nonlinear model and polynomials
## linear model simulation
n = 200
p = 21
x = seq(0, 1, length.out = 200)
x.poly = sapply(0:20, function(k) x^k)

x.train = x.poly
y.train = sin(2*pi*x) + rnorm(n)
data.train = data.frame(x = x.train, y = y.train)

x.test  = x.poly
y.test  = sin(2*pi*x) + rnorm(n)
x.test  = data.frame(x = x.test)


index      = 1:p
rms.train  = index 
rms.test   = index
for(j in index)
{
  x.formula    = paste0("x.", 1:j, collapse = "+")
  reg.formula  = as.formula(paste("y~", x.formula))
  reg.train    = lm(reg.formula, data = data.train)
  rms.train[j] = sum((reg.train$residuals)^2)/n
  
  pred.test    = predict(reg.train, newdata = x.test)
  rms.test[j]  = sum((y.test - pred.test)^2)/n
}

pdf("training_testing_error_poly.pdf", height = 5, width = 6)
y.min = min(rms.train, rms.test)
y.max = max(rms.train, rms.test)
plot(rms.train ~ index, 
     ylim = c(y.min, y.max),
     type = "b", pch = 19, cex = 0.5,
     xlab = "# covariates", ylab = "RSS/n",
     font.main = 1,
     main = "prediction errors", bty = "n")
lines(rms.test ~ index, type = "b", pch = 9, cex = 0.5)  
legend("topright", c("training data", "testing data"), 
       pch = c(19, 9)) 
dev.off()






## Best subset and forward regression
library(ggplot2)
library(leaps)

## pennsylvania re-employment bonus experiment

## description of the DATA: 
## Koenker and Xiao (2002) Inference on the Quantile Regression Process
## published in Econometrica

penndata = read.table("pennbonus.txt")
head(penndata)

## fit a full model
fullmodel = lm(duration ~ ., data = penndata)
summary(fullmodel)

## variable selection
nsample = dim(penndata)[1]
pcovariates = dim(penndata)[2] - 1

## best subset regression
subsetmodel = summary(regsubsets(duration ~ ., 
                                 nvmax = pcovariates, 
                                 data = penndata,
                                 method = "exhaustive"))
subsetmodel$which
subsetrss     = subsetmodel$rss
subsetaic     = nsample*log(subsetrss/nsample) + 2*(2:(pcovariates + 1))
subsetbic1    = nsample*log(subsetrss/nsample) + log(nsample)*(2:(pcovariates + 1))
subsetaicbic2 = subsetmodel$bic

dat = rbind(data.frame(name = "RSS",
                       n.cov = 1:pcovariates,
                       measure = subsetrss),
            data.frame(name = "AIC",
                       n.cov = 1:pcovariates,
                       measure = subsetaic),
            data.frame(name = "BIC",
                       n.cov = 1:pcovariates,
                       measure = subsetbic1))

ggplot(dat) + 
  geom_point(aes(n.cov, measure), 
             alpha = 0.7, cex = 0.5) + 
  facet_wrap(~ name, scale = "free_y") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlab("#predictors") + ylab("") + 
  ggtitle("Penn bonus experimental data")
ggsave("bestsubsetpenn.pdf", height = 4, width = 8.5)


which.min(subsetaic)
which.min(subsetbic1)



## Boston housing data
library("mlbench")
data("BostonHousing")

nsample = dim(BostonHousing)[1]
pcovariates = dim(BostonHousing)[2] - 1

## best subset regression
subsetmodel = summary(regsubsets(medv ~ ., nvmax = pcovariates, 
                                 data = BostonHousing,
                                 method = "exhaustive"))
subsetmodel$which
subsetrss     = subsetmodel$rss
subsetaic     = nsample*log(subsetrss/nsample) + 2*(2:(pcovariates + 1))
subsetbic1    = nsample*log(subsetrss/nsample) + log(nsample)*(2:(pcovariates + 1))
subsetaicbic2 = subsetmodel$bic

dat = rbind(data.frame(name = "RSS",
                       n.cov = 1:pcovariates,
                       measure = subsetrss),
            data.frame(name = "AIC",
                       n.cov = 1:pcovariates,
                       measure = subsetaic),
            data.frame(name = "BIC",
                       n.cov = 1:pcovariates,
                       measure = subsetbic1))


ggplot(dat) + 
  geom_point(aes(n.cov, measure), 
             alpha = 0.7, cex = 0.5) + 
  facet_wrap(~ name, scale = "free_y") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlab("#predictors") + ylab("") +
  ggtitle("Boston housing data")
ggsave("bestsubsetbostonhousing.pdf", height = 4, width = 8.5)


which.min(subsetaic)
which.min(subsetbic1)

subsetmodel$which[11, ]



