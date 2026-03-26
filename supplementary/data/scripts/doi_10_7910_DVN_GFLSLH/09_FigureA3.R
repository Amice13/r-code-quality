## Afshartous and Preston (2010).  We can use `f2.theta.rho.Z.beta()` 
## to figure out the $Z_{\gamma}$ from equation (1) in the article.  
## We can then calculate the probability of overlap using that value 
## of $Z_{\gamma}$ with a specified $\theta$ and $\rho$, that's what 
## `f()` does below.  The 0.8342237 is the two-sided $p$-value for 
## $Z_{\gamma}$ using the standard normal distribution.  This produces 
## a $5%$ type I error rate when $\theta=1$ and $\rho=0$.  

#### 1. Defining Functions ####
## the function below is a combination of 
## f2.alpha.theta.rho.Z.beta() and f2.theta.rho() 
## from Afshartous and Preston (2010). 
f <- function(theta, rho){
  2*pnorm(qnorm(1-(1-.8342237)/2)*(theta/(theta^{2} +1 - 2*rho*theta)^(1/2) + (1/theta)/(1 + theta^(-2) - 2*rho*theta^{-1})^(1/2)), lower.tail=FALSE)
}

#### 2. Data for plot ###
## expand grid over values of correlation and ratio of variances
eg <- expand.grid(theta = 1:3, rho = seq(-.9,.9, by=.3))

## Calculate the probability of overlap for the two confidence intervals
eg$po <- f(eg$theta, eg$rho)

#### 3. Plot ####
ggplot(eg, aes(x=po, y=as.factor(round(rho, 2)))) + 
  geom_point() + 
  geom_segment(aes(xend=-Inf, yend=as.factor(round(rho, 2)))) + 
  geom_vline(xintercept=.05, linetype=3) + 
  facet_wrap(~as.factor(theta)) + 
  scale_x_continuous(breaks = c(0,.05, .1, .15, .2), limits=c(0,.2), labels=scales::label_percent()) + 
  theme_bw() + 
  theme(panel.grid=element_blank()) + 
  labs(x = "Type I Error Rate", y="Correlation Between Estimates")
ggsave("results/FigureA3.png", height=6, width=12, units="in", dpi=300)