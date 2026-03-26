rm(list = ls())

library(ggplot2)

## data "GaltonFamilies" from the R package "HistData"
## family: family ID, a factor with levels 001-204
## father: height of father
## mother: height of mother
## midparentHeight: mid-parent height, calculated as (father + 1.08*mother)/2
## children: number of children in this family
## childNum: number of this child within family. Children are listed in decreasing order of height for boys followed by girls
## gender: child gender, a factor with levels female male
## childHeight: height of child

GaltonFamilies = read.table("GaltonFamilies.txt", header = TRUE)

## OLS fit by the "lm" function
galton_fit = lm(childHeight ~ midparentHeight,
                data = GaltonFamilies)

## OLS coefficients and inference
summary(galton_fit)$coef

## predictions: confidence and prediction intervals
new_mph  = seq(60, 80, by = 0.5)
new_data = data.frame(midparentHeight = new_mph)
new_ci   = predict(galton_fit, new_data, 
                        interval = "confidence")
new_pi   = predict(galton_fit, new_data, 
                        interval = "prediction")
head(round(new_ci, 2))
head(round(new_pi, 2))

 
Prediction = rbind(data.frame(type="fitted line",
                              type.l = "fitted", 
                              new_mph=new_mph,
                             new_pre=new_ci[,1]),
                  data.frame(type="lower confidence",
                             type.l = "confidence",
                             new_mph=new_mph,
                             new_pre=new_ci[,2]),
                  data.frame(type="upper confidence",
                             type.l = "confidence",
                             new_mph=new_mph,
                             new_pre=new_ci[,3]),
                  data.frame(type="lower prediction",
                             type.l = "prediction",
                             new_mph=new_mph,
                             new_pre=new_pi[,2]),
                  data.frame(type="upper prediction",
                             type.l = "prediction",
                             new_mph=new_mph,
                             new_pre=new_pi[,3]))

ggplot(GaltonFamilies, 
       aes(x=midparentHeight, y=jitter(childHeight))) + 
  geom_point(size=0.6, col = "grey") +
  geom_line(Prediction,
            mapping = aes(x=new_mph, y=new_pre, 
                          group = type, 
                          linetype = type.l)) +
  scale_linetype("") + 
  ylab("childHeight") +
  ggtitle("prediction in Galton's regression") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("galton_prediction_ggplot.pdf", height = 5, width = 7)





## Anscombe's Quartet
library(datasets)
anscombe
## mean of x
c(mean(anscombe$x1),
  mean(anscombe$x2),
  mean(anscombe$x3),
  mean(anscombe$x4))
## variance of x
c(var(anscombe$x1),
  var(anscombe$x2),
  var(anscombe$x3),
  var(anscombe$x4))
## mean of y
c(mean(anscombe$y1),
  mean(anscombe$y2),
  mean(anscombe$y3),
  mean(anscombe$y4))
## variance of y
c(var(anscombe$y1),
  var(anscombe$y2),
  var(anscombe$y3),
  var(anscombe$y4))
## OLS
ols1 = lm(y1 ~ x1, data = anscombe)
summary(ols1)
ols2 = lm(y2 ~ x2, data = anscombe)
summary(ols2)
ols3 = lm(y3 ~ x3, data = anscombe)
summary(ols3)
ols4 = lm(y4 ~ x4, data = anscombe)
summary(ols4)
## scatter plot
pdf("AnscombeQuartet.pdf", height = 8, width = 8)
par(mfrow = c(2, 2))
plot(y1 ~ x1, data = anscombe)
abline(ols1)
plot(y2 ~ x2, data = anscombe)
abline(ols2)
plot(y3 ~ x3, data = anscombe)
abline(ols3)
plot(y4 ~ x4, data = anscombe)
abline(ols4)
dev.off()





## data "lalonde" from the R package "Matching"
## age: age in years.
## educ: years of schooling.
## black: indicator variable for blacks.
## hisp: indicator variable for Hispanics.
## married: indicator variable for martial status.
## nodegr: indicator variable for high school diploma.
## re74: real earnings in 1974.
## re75: real earnings in 1975.
## re78: real earnings in 1978.
## u74: indicator variable for earnings in 1974 being zero.
## u75: indicator variable for earnings in 1975 being zero.
## treat: an indicator variable for treatment status.

lalonde = read.table("lalonde.txt", header = TRUE)
lalonde_fit = lm(re78 ~ ., data = lalonde)
summary(lalonde_fit)


library("car")
linearHypothesis(lalonde_fit,
                 c("age=0", "educ=0", "black=0",
                   "hisp=0", "married=0", "nodegr=0",
                   "re74=0", "re75=0", "u74=0",
                   "u75=0"))

## predictions: confidence and prediction intervals
new_treat          = lalonde
new_treat$treat    = 1
predict_lalonde1   = predict(lalonde_fit, new_treat, 
                             interval = "none")
new_control        = lalonde
new_control$treat  = 0
predict_lalonde0   = predict(lalonde_fit, new_control, 
                             interval = "none")
mean(predict_lalonde1)
mean(predict_lalonde0)








## homework problem on the t statistics
library(MASS)
#simulate bivariate normal distribution
xy = mvrnorm(n=100, mu=c(0, 0), 
             Sigma=matrix(c(1, 0.5, 0.5, 1), ncol=2))
xy = as.data.frame(xy)
colnames(xy) = c("x", "y")
## OLS
reg.y.x = lm(y ~ x, data = xy)
reg.x.y = lm(x ~ y, data = xy)
## compare t statistics based on homoskedastic errors
summary(reg.y.x)$coef[2, 3]
summary(reg.x.y)$coef[2, 3]



