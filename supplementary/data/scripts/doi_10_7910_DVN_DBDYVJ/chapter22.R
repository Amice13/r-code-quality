rm(list = ls())


## Figure 22.1
library("ggplot2")
library("MASS")

my.dnegbin = function(k, mu, theta, log = FALSE)
{
  log.d = lgamma(k+theta) - lgamma(k+1) - lgamma(theta) +
    theta*log(theta/(mu+theta)) + k*log(mu/(mu+theta))
  
  if(log){log.d}else{exp(log.d)}
}

x  = seq(0, 20, 1) 
dat = rbind(data.frame(x = x,
                       y = dpois(x, lambda = 1, log = TRUE),
                       distribution = "Poisson",
                       mu = "mu == 1",
                       theta = "theta == 1"),
            data.frame(x = x,
                       y = my.dnegbin(x, mu = 1, theta = 1, log = TRUE),
                       distribution = "Negbin",
                       mu = "mu == 1",
                       theta = "theta == 1"),
            data.frame(x = x,
                       y = dpois(x, lambda = 1, log = TRUE),
                       distribution = "Poisson",
                       mu = "mu == 1",
                       theta = "theta == 10"),
            data.frame(x = x,
                       y = my.dnegbin(x, mu = 1, theta = 10, log = TRUE),
                       distribution = "Negbin",
                       mu = "mu == 1",
                       theta = "theta == 10"),
            data.frame(x = x,
                       y = dpois(x, lambda = 5, log = TRUE),
                       distribution = "Poisson",
                       mu = "mu == 5",
                       theta = "theta == 1"),
            data.frame(x = x,
                       y = my.dnegbin(x, mu = 5, theta = 1, log = TRUE),
                       distribution = "Negbin",
                       mu = "mu == 5",
                       theta = "theta == 1"),
            data.frame(x = x,
                       y = dpois(x, lambda = 5, log = TRUE),
                       distribution = "Poisson",
                       mu = "mu == 5",
                       theta = "theta == 10"),
            data.frame(x = x,
                       y = my.dnegbin(x, mu = 5, theta = 10, log = TRUE),
                       distribution = "Negbin",
                       mu = "mu == 5",
                       theta = "theta == 10"))


ggplot(dat) + 
  geom_point(aes(x=x, y=y, shape = distribution),
             size = 1, alpha = 0.9) + 
  facet_grid(mu ~ theta, scales = "free_y",
             labeller = label_parsed) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlab("y") + ylab("log probability mass function") + 
  ggtitle("comparing Poisson and Negative Binomial")
ggsave("poisson_negbin.pdf", height = 5, width = 8.5)





## Chapter 22.3
## gym data
library("ggplot2")
library("gridExtra")
library("foreign")
library("MASS")
gym1 = read.dta("gym_treatment_exp_weekly.dta")
f.reg = weekly_visit ~ incentive_commit + incentive + 
  target + member_gym_pre

weekids             = sort(unique(gym1$incentive_week))
lweekids            = length(weekids)
coefincentivecommit = 1:lweekids
coefincentive       = 1:lweekids
seincentivecommit   = 1:lweekids
seincentive         = 1:lweekids
AIClm               = 1:lweekids
for(i in 1:lweekids)
{
        gymweek = gym1[which(gym1$incentive_week == weekids[i]), ]   
        regweek = lm(f.reg, data = gymweek)
        regweekcoef = summary(regweek)$coef
        
        coefincentivecommit[i] = regweekcoef[2, 1]
        coefincentive[i]       = regweekcoef[3, 1]
        seincentivecommit[i]   = regweekcoef[2, 2]
        seincentive[i]         = regweekcoef[3, 2]
        
        AIClm[i]               = AIC(regweek)
}

 

dat.lm = rbind(data.frame(model = "Linear regression",
                          treat = "incentivecommit",
                          weeks = weekids,
                          coef  = coefincentivecommit,
                          se    = seincentivecommit,
                          l.ci  = coefincentivecommit - 1.96*seincentivecommit,
                          u.ci  = coefincentivecommit + 1.96*seincentivecommit),
               data.frame(model = "Linear regression",
                          treat = "incentive",
                          weeks = weekids,
                          coef  = coefincentive,
                          se    = seincentive,
                          l.ci  = coefincentive - 1.96*seincentive,
                          u.ci  = coefincentive + 1.96*seincentive))

lm.plot = ggplot(dat.lm, aes(x=weeks, y=coef)) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci),
              stat = "identity") +
  facet_grid(~treat) + 
  ylab("estimates") + 
  ggtitle("linear regression")
print(lm.plot)

## poisson regression 
AICpoisson = AIClm 
for(i in 1:lweekids)
{
  gymweek = gym1[which(gym1$incentive_week == weekids[i]), ]   
  regweek = glm(f.reg, family = poisson(link = "log"), data = gymweek)
  regweekcoef = summary(regweek)$coef
  
  coefincentivecommit[i] = regweekcoef[2, 1]
  coefincentive[i]       = regweekcoef[3, 1]
  seincentivecommit[i]   = regweekcoef[2, 2]
  seincentive[i]         = regweekcoef[3, 2]
  
  AICpoisson[i]          = regweek$aic 
}
 
dat.poisson = rbind(data.frame(model = "Poisson regression",
                               treat = "incentivecommit",
                          weeks = weekids,
                          coef  = coefincentivecommit,
                          se    = seincentivecommit,
                          l.ci  = coefincentivecommit - 1.96*seincentivecommit,
                          u.ci  = coefincentivecommit + 1.96*seincentivecommit),
               data.frame(model = "Poisson regression",
                          treat = "incentive",
                          weeks = weekids,
                          coef  = coefincentive,
                          se    = seincentive,
                          l.ci  = coefincentive - 1.96*seincentive,
                          u.ci  = coefincentive + 1.96*seincentive))

poisson.plot = ggplot(dat.poisson, aes(x=weeks, y=coef)) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci),
              stat = "identity") +
  facet_grid(~treat) + 
  ylab("estimates") +
  ggtitle("Poisson regression")
print(poisson.plot)



##negative binomial regressions
## check for over-dispersion 
gymmean = 1:lweekids
gymvar  = 1:lweekids
for(i in 1:lweekids)
{
       gymmean[i] = mean(gym1$weekly_visit[gym1$incentive_week == weekids[i]])
       gymvar[i]  = var(gym1$weekly_visit[gym1$incentive_week == weekids[i]])
}
dat.disp = data.frame(mean = gymmean,
                      variance  = gymvar)
overdisp.plot = ggplot(dat.disp) + 
  geom_point(aes(x=mean, y=variance),
             alpha = 0.7, size = 0.7) + 
  geom_abline(aes(slope=1, intercept=0), alpha = 0.7) + 
  ggtitle("checking overdispersion") + 
  xlim(c(0,3)) + ylim(c(0,3)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
print(overdisp.plot)


gymtheta   = 1:lweekids
gymSEtheta = 1:lweekids
AICnb      = AICpoisson
for(i in 1:lweekids)
{
  gymweek = gym1[which(gym1$incentive_week == weekids[i]), ]   
  regweek = glm.nb(f.reg, data = gymweek)
  regweekcoef = summary(regweek)$coef
  
  coefincentivecommit[i] = regweekcoef[2, 1]
  coefincentive[i]       = regweekcoef[3, 1]
  seincentivecommit[i]   = regweekcoef[2, 2]
  seincentive[i]         = regweekcoef[3, 2]
  
  gymtheta[i]            = summary(regweek)$theta
  gymSEtheta[i]          = summary(regweek)$SE.theta
  
  AICnb[i]               = regweek$aic 
}

dat.nb = rbind(data.frame(model = "NB regression",
                          treat = "incentivecommit",
                               weeks = weekids,
                               coef  = coefincentivecommit,
                               se    = seincentivecommit,
                               l.ci  = coefincentivecommit - 1.96*seincentivecommit,
                               u.ci  = coefincentivecommit + 1.96*seincentivecommit),
                    data.frame(model = "NB regression",
                               treat = "incentive",
                               weeks = weekids,
                               coef  = coefincentive,
                               se    = seincentive,
                               l.ci  = coefincentive - 1.96*seincentive,
                               u.ci  = coefincentive + 1.96*seincentive))

nb.plot = ggplot(dat.nb, aes(x=weeks, y=coef)) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci),
              stat = "identity") +
  facet_grid(~treat) + 
  ylab("estimates") +
  ggtitle("Negative Binomial regression")
print(nb.plot)

 
## comparison
dat = rbind(dat.lm, dat.poisson, dat.nb)
full.plot = ggplot(dat, aes(x=weeks, y=coef)) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci),
              size = 0.3, alpha = 0.8, col = "black",
              stat = "identity") +
  geom_hline(aes(yintercept = 0), 
             alpha = 0.5, linetype = "dashed") + 
  facet_grid(model~treat) + 
  ylab("point estimates and confidence intervals") +
  ggtitle("three regressions") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom")
print(full.plot)
ggsave("threeregressions.pdf", height = 7, width = 8.5)


## check theta
dat.theta = data.frame(weeks = weekids,
                       log.theta = log(gymtheta),
                       l.ci = log(gymtheta) - 1.96*gymSEtheta/gymtheta,
                       u.ci = log(gymtheta) + 1.96*gymSEtheta/gymtheta)
theta.plot = ggplot(dat.theta, aes(x=weeks, y=log.theta), alpha = 0.6) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci), 
              col = 1, size = 0.5, alpha = 0.3, 
              stat = "identity") +
  ylab(expression(log(theta))) +
  ggtitle("point and interval estimates") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
print(theta.plot)

pdf("gym_overdispersion_check.pdf", height = 4, width = 7.5)
grid.arrange(overdisp.plot, theta.plot, ncol = 2) 
dev.off()


## check for zero inflation
chosen.week = c(-5:-2, 2:5)
dat.zero = data.frame()
for(i in chosen.week)
{
   gymweek.chosen = gym1[which(gym1$incentive_week == i), ]
   dat.zero = rbind(dat.zero,
                    data.frame(week = paste("week", i),
                               y = gymweek.chosen$weekly_visit))
}
ggplot(dat.zero, aes(y)) + 
  geom_histogram(breaks = 0:5, alpha = 0.3) + 
  facet_wrap(week~., nrow = 2) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("gym_check_zeroinf.pdf", height = 6, width = 8)


##zero-inflated poisson regression
library("pscl")
coefincentivecommit0 = coefincentivecommit
coefincentive0       = coefincentive
seincentivecommit0   = seincentivecommit
seincentive0         = seincentive
AIC0poisson          = AICnb
for(i in 1:lweekids)
{
  gymweek = gym1[which(gym1$incentive_week == weekids[i]), ]   
  regweek = zeroinfl(f.reg, dist = "poisson", data = gymweek)
  regweekcoef = summary(regweek)$coef
  
  coefincentivecommit[i] = regweekcoef$count[2, 1]
  coefincentive[i]       = regweekcoef$count[3, 1]
  seincentivecommit[i]   = regweekcoef$count[2, 2]
  seincentive[i]         = regweekcoef$count[3, 2]
  
  coefincentivecommit0[i] = regweekcoef$zero[2, 1]
  coefincentive0[i]       = regweekcoef$zero[3, 1]
  seincentivecommit0[i]   = regweekcoef$zero[2, 2]
  seincentive0[i]         = regweekcoef$zero[3, 2]
  
  AIC0poisson[i]          = AIC(regweek)
}



dat.0poisson = rbind(data.frame(model = "mean",
                          treat = "incentivecommit",
                          weeks = weekids,
                          coef  = coefincentivecommit,
                          se    = seincentivecommit,
                          l.ci  = coefincentivecommit - 1.96*seincentivecommit,
                          u.ci  = coefincentivecommit + 1.96*seincentivecommit),
               data.frame(model = "mean",
                          treat = "incentive",
                          weeks = weekids,
                          coef  = coefincentive,
                          se    = seincentive,
                          l.ci  = coefincentive - 1.96*seincentive,
                          u.ci  = coefincentive + 1.96*seincentive),
               data.frame(model = "zero",
                          treat = "incentivecommit",
                          weeks = weekids,
                          coef  = coefincentivecommit0,
                          se    = seincentivecommit0,
                          l.ci  = coefincentivecommit0 - 1.96*seincentivecommit0,
                          u.ci  = coefincentivecommit0 + 1.96*seincentivecommit0),
               data.frame(model = "zero",
                          treat = "incentive",
                          weeks = weekids,
                          coef  = coefincentive0,
                          se    = seincentive0,
                          l.ci  = coefincentive0 - 1.96*seincentive0,
                          u.ci  = coefincentive0 + 1.96*seincentive0))

zeropois.plot = ggplot(dat.0poisson, aes(x=weeks, y=coef)) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci),
              stat = "identity",
              size = 0.3, alpha = 0.3, col = "black") +
  geom_hline(aes(yintercept = 0), 
             alpha = 0.3, linetype = "dashed") + 
  facet_grid(model~treat, scales = "free_y") + 
  ylab("estimates") +
  ggtitle("Zero-inflated Poisson regression") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom")
print(zeropois.plot)



##zero-inflated Negative-Binomial regression
AIC0nb          = AICnb
for(i in 1:lweekids)
{
  gymweek = gym1[which(gym1$incentive_week == weekids[i]), ]   
  regweek = zeroinfl(f.reg, dist = "negbin", data = gymweek)
  regweekcoef = summary(regweek)$coef
  
  coefincentivecommit[i] = regweekcoef$count[2, 1]
  coefincentive[i]       = regweekcoef$count[3, 1]
  seincentivecommit[i]   = regweekcoef$count[2, 2]
  seincentive[i]         = regweekcoef$count[3, 2]
  
  coefincentivecommit0[i] = regweekcoef$zero[2, 1]
  coefincentive0[i]       = regweekcoef$zero[3, 1]
  seincentivecommit0[i]   = regweekcoef$zero[2, 2]
  seincentive0[i]         = regweekcoef$zero[3, 2]
  
  gymtheta[i]            = log(summary(regweek)$theta)
  gymSEtheta[i]          = summary(regweek)$SE.logtheta
  
  AIC0nb[i]          = AIC(regweek)
}



dat.0nb = rbind(data.frame(model = "mean",
                                treat = "incentivecommit",
                                weeks = weekids,
                                coef  = coefincentivecommit,
                                se    = seincentivecommit,
                                l.ci  = coefincentivecommit - 1.96*seincentivecommit,
                                u.ci  = coefincentivecommit + 1.96*seincentivecommit),
                     data.frame(model = "mean",
                                treat = "incentive",
                                weeks = weekids,
                                coef  = coefincentive,
                                se    = seincentive,
                                l.ci  = coefincentive - 1.96*seincentive,
                                u.ci  = coefincentive + 1.96*seincentive),
                     data.frame(model = "zero",
                                treat = "incentivecommit",
                                weeks = weekids,
                                coef  = coefincentivecommit0,
                                se    = seincentivecommit0,
                                l.ci  = coefincentivecommit0 - 1.96*seincentivecommit0,
                                u.ci  = coefincentivecommit0 + 1.96*seincentivecommit0),
                     data.frame(model = "zero",
                                treat = "incentive",
                                weeks = weekids,
                                coef  = coefincentive0,
                                se    = seincentive0,
                                l.ci  = coefincentive0 - 1.96*seincentive0,
                                u.ci  = coefincentive0 + 1.96*seincentive0))

zeronb.plot = ggplot(dat.0nb, aes(x=weeks, y=coef)) + 
  geom_smooth(aes(ymin = l.ci, ymax = u.ci),
              stat = "identity",
              size = 0.3, alpha = 0.3, col = "black") +
  geom_hline(aes(yintercept = 0), 
             alpha = 0.3, linetype = "dashed") + 
  facet_grid(model~treat, scales = "free_y") + 
  ylab("estimates") +
  ggtitle("Zero-inflated NB regression") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position="bottom")
print(zeronb.plot)

pdf("gym_zeroinflated_regressions.pdf", height = 8, width = 8)
grid.arrange(zeropois.plot, zeronb.plot, nrow = 2)
dev.off()


## check theta
quantile(gymtheta, probs = c(0.01, 0.25, 0.5, 0.75, 0.99))



## model comparisons using AICs
dat.aic = rbind(data.frame(model  = "Linear",
                           aic = AIClm,
                           weeks = weekids),
                data.frame(model = "Poisson",
                           aic = AICpoisson,
                           weeks = weekids),
                data.frame(model = "NB",
                           aic = AICnb,
                           weeks = weekids),
                data.frame(model = "Z-Inf Poisson",
                           aic = AIC0poisson,
                           weeks = weekids),
                data.frame(model = "Z-Inf NB",
                           aic = AIC0nb,
                           weeks = weekids))
ggplot(dat.aic) + 
  geom_point(aes(x=weeks, y=aic, shape=model),
             size = 0.6, alpha = 0.5) +
  ylab("AIC") +
  ggtitle("Comparing AIC from different models") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("gym_comparing_aic.pdf", height = 3, width = 8)

diff.aic = AIC0nb - AIC0poisson
quantile(diff.aic, probs = c(0.01, 0.25, 0.5, 0.75, 0.99))
