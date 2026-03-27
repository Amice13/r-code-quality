rm(list = ls())


## simulated data 
n = 100
x = seq(0, 1, length = n)
y = 1 + 3*x + rnorm(n)
lmmod = lm(y ~ x)

dat.res = rbind(data.frame(factor1 = "no outlier",
                        factor2 = "leverage",
                        x = x,
                        res = hatvalues(lmmod)),
               data.frame(factor1 = "no outlier",
                           factor2 = "standardized",
                           x = x,
                           res = rstandard(lmmod)),   
             data.frame(factor1 = "no outlier",
                        factor2 = "studentized",
                        x = x,
                        res = rstudent(lmmod)),
             data.frame(factor1 = "no outlier",
                        factor2 = "Cook",
                        x = x,
                        res = cooks.distance(lmmod)))
 
y[n] = y[n] - 8
lmmod = lm(y ~ x)

dat.res = rbind(dat.res,
                data.frame(factor1 = "outlier n",
                           factor2 = "leverage",
                           x = x,
                           res = hatvalues(lmmod)),
                data.frame(factor1 = "outlier n",
                           factor2 = "standardized",
                           x = x,
                           res = rstandard(lmmod)),
                data.frame(factor1 = "outlier n",
                           factor2 = "studentized",
                           x = x,
                           res = rstudent(lmmod)),
                data.frame(factor1 = "outlier n",
                           factor2 = "Cook",
                           x = x,
                           res = cooks.distance(lmmod)))
 
y[n] = y[n] + 8
y[n/2] = y[n/2] - 8
lmmod = lm(y ~ x)

dat.res = rbind(dat.res,
                data.frame(factor1 = "outlier n/2",
                           factor2 = "leverage",
                           x = x,
                           res = hatvalues(lmmod)),
                data.frame(factor1 = "outlier n/2",
                           factor2 = "standardized",
                           x = x,
                           res = rstandard(lmmod)),
                data.frame(factor1 = "outlier n/2",
                           factor2 = "studentized",
                           x = x,
                           res = rstudent(lmmod)),
                data.frame(factor1 = "outlier n/2",
                           factor2 = "Cook",
                           x = x,
                           res = cooks.distance(lmmod)))

library(ggplot2)
pdf("residual_diagnostics.pdf", height = 7, width = 8)
ggplot(dat.res) + 
  geom_point(aes(x = x, y = res), 
             pch = 19, cex = 0.5, alpha = 0.7) + 
  facet_grid(factor2 ~ factor1, scales="free_y") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("x") + ylab("outlier checks")
dev.off()





## real data
library("Matching")
data(lalonde)
lmmod = lm(re78 ~ ., data = lalonde)
index = 1:nrow(lalonde)
dat.res = rbind(data.frame(factor2 = "leverage",
                           index = index,
                           res = hatvalues(lmmod)),
                data.frame(factor2 = "standardized",
                           index = index,
                           res = rstandard(lmmod)),
                data.frame(factor2 = "studentized",
                           index = index,
                           res = rstudent(lmmod)),
                data.frame(factor2 = "Cook",
                           index = index,
                           res = cooks.distance(lmmod)))

pdf("residual_diagnostics_lalonde.pdf", height = 7, width = 8)
ggplot(dat.res) + 
  geom_point(aes(x = index, y = res), 
             pch = 19, cex = 0.5, alpha = 0.7) + 
  facet_grid(factor2 ~ ., scales="free_y") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("x") + ylab("outlier checks")
dev.off() 

