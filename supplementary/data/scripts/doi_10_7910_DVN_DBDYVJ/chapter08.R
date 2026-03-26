rm(list = ls())

library(ggplot2)

## Simpson's paradox 
n  = 1000
w  = rbinom(n, 1, 0.5)
x1 = rnorm(n, -1, 1)
x0 = rnorm(n, 2, 1)
x  = ifelse(w, x1, x0)
y  = x + 6*w + rnorm(n)
fit.xw = lm(y ~ x + w)$coef
fit.x  = lm(y ~ x)$coef
fit.xw 
fit.x 

simpsondata = data.frame(x=x, y=y, w=w)
pdf("simpsonparadoxexample_ggplot.pdf", height = 5, width = 5)
ggplot(simpsondata, aes(x=x, y=y)) + 
  geom_point(size=1, col = "grey",
             aes(shape = factor(w))) +
  geom_abline(intercept = fit.x[1], 
              slope = fit.x[2], alpha = 0.6, linetype = 2) + 
  geom_abline(intercept = fit.xw[1] + fit.xw[3], 
              slope = fit.xw[2], alpha = 0.7) +
  geom_abline(intercept = fit.xw[1], 
              slope = fit.xw[2], alpha = 0.7) + 
  ggtitle("reversion of the sign") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
dev.off()  



# pdf("simpsonparadoxexample.pdf", height = 5, width = 5)
# plot(y ~ x, subset = (w==1),
#      xlim = c(-4, 5), ylim = c(-3, 9), 
#      pch = 19, cex = 0.2, col = "grey",
#      main = "reversion of the sign")
# lines(y ~ x, subset = (w==0), 
#       type = "p",
#       pch = 14, cex = 0.2, col = "grey")
# legend("bottomright", c("w=1", "w=0"),
#        pch = c(19, 14), cex = 0.7,
#        col = c("grey", "grey"))
# abline(fit.x, lty = 2)
# abline(fit.xw[1], fit.xw[2])
# abline(fit.xw[1]+fit.xw[3], fit.xw[2])
# dev.off()





## ANOVA
library("Matching")
data(lalonde)
lalonde_full  = lm(re78 ~ ., data = lalonde)
lalonde_treat = lm(re78 ~ treat, data = lalonde)
anova(lalonde_treat, lalonde_full)

lalonde1   = lm(re78 ~ 1, data = lalonde)
anova(lalonde1, lalonde_treat, lalonde_full)

 