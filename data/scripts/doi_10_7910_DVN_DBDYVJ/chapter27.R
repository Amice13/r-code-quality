rm(list = ls())



library(ggplot2)
## Figure 27.2
## Gamma distribution
yy = seq(0.01, 6, 0.01)
dy0.5 = dgamma(yy, shape = 0.5, scale = 2)
st0.5 = 1 - pgamma(yy, shape = 0.5, scale = 2)
dy1   = dgamma(yy, shape = 1, scale = 2)
st1   = 1 - pgamma(yy, shape = 1, scale = 2)
dy2   = dgamma(yy, shape = 2, scale = 2)
st2   = 1 - pgamma(yy, shape = 2, scale = 2)
dat.gamma = rbind(data.frame(alpha=0.5,
                             yy=yy,
                             ht=dy0.5/st0.5),
                  data.frame(alpha=1,
                             yy=yy,
                             ht=dy1/st1),
                  data.frame(alpha=2,
                             yy=yy,
                             ht=dy2/st2))
ggplot(dat.gamma) + 
  geom_line(aes(x=yy,y=ht,linetype=factor(alpha)),
            alpha = 0.6) +
  xlab("t") + ylab("hazard functions") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(linetype = expression(alpha)) 
ggsave("gamma_hazard.pdf", height = 4, width = 5)



## Log-Normal distribution
dy1 = dlnorm(yy, 0.5, 0.5)
st1 = 1 - plnorm(yy, 0.5, 0.5)
dy2 = dlnorm(yy, 2, 0.5)
st2 = 1 - plnorm(yy, 2, 0.5)
dy3 = dlnorm(yy, 0.5, 2)
st3 = 1 - plnorm(yy, 0.5, 2)
dy4 = dlnorm(yy, 2, 2)
st4 = 1 - plnorm(yy, 2, 2)

dat.lnorm = rbind(data.frame(par="(0.5, 0.5)",
                             yy=yy,
                             ht=dy1/st1),
                  data.frame(par="(2, 0.5)",
                             yy=yy,
                             ht=dy2/st2),
                  data.frame(par="(0.5, 2)",
                             yy=yy,
                             ht=dy3/st3),
                  data.frame(par="(2, 2)",
                             yy=yy,
                             ht=dy4/st4))
ggplot(dat.lnorm) + 
  geom_line(aes(x=yy,y=ht,linetype=par),
            alpha = 0.6) +
  xlab("t") + ylab("hazard functions") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(linetype = expression(lnorm(mu,sigma))) 
ggsave("lognormal_hazard.pdf", height = 4, width = 5)

 



## discrete survival functions
## Figure 27.3
pdf("discrete_survival_fn.pdf", width = 6, height = 4)
par(mar = c(4, 4, 2, 2))
plot(stepfun(x = 1:6, 
             y = c(1, 0.9, 0.85, 0.7, 0.5, 0.2, 0), 
             right = TRUE),
     xlab = "t",
     ylab = "S(t)",
     bty = "n",
     main = "discrete distribution",
     font.main = 1)
dev.off()









## proportional hazard assumption
nn   = 500
yy   = seq(0, 7, length.out = nn)
st0  = 1 - pexp(yy)
st2  = st0^2
st.5 = st0^(0.5)
dat.ph.exp = data.frame(power=rep(c("0", "2", "0.5"), c(nn, nn, nn)),
                        st=c(st0, st2, st.5),
                        yy=rep(yy, 3),
                        dist="Exponential(1)")

st0  = 1 - pgamma(yy, shape = 2, scale = 2)
st2  = st0^2
st.5 = st0^(0.5)
dat.ph.gam = data.frame(power=rep(c("0", "2", "0.5"), c(nn, nn, nn)),
                        st=c(st0, st2, st.5),
                        yy=rep(yy, 3),
                        dist="Gamma(2,2)")

st0  = 1 - plnorm(yy, 0, 1)
st2  = st0^2
st.5 = st0^(0.5)
dat.ph.lnorm = data.frame(power=rep(c("0", "2", "0.5"), c(nn, nn, nn)),
                          st=c(st0, st2, st.5),
                          yy=rep(yy, 3),
                          dist="lnorm(0,1)")

dat.ph = rbind(dat.ph.exp, dat.ph.gam, dat.ph.lnorm)
ggplot(dat.ph) + 
  geom_line(aes(x=yy,y=st,linetype=power),
            alpha = 0.5, size = 0.4, 
            stat = "identity", col = "black") +
  facet_grid(~dist) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  xlab("t") + ylab("survival functions") +
  ggtitle("Under proportional hazards assumption")
ggsave("ph_survival_curves.pdf", height = 3.5, width = 8)






library("foreign")
library("ggplot2")
## for survival analysis
library("survival")

## Lin et al. (2016)'s data: 2X2 factorial experiment
COMBINE = read.table("combine_data.txt", header = TRUE)[, -1]
head(COMBINE)
ggplot(COMBINE) + 
  geom_histogram(aes(x=futime),
                 breaks = seq(1, 112, length.out = 15),
                 alpha = 0.4) + 
  facet_grid(NALTREXONE~THERAPY,
             labeller = label_both) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank())
ggsave("combine_histograms.pdf", height = 4, width = 7)

km4groups = survfit(Surv(futime, relapse) ~ NALTREXONE + THERAPY,
                    data = COMBINE)
pdf("KMcurve2x2.pdf", height = 4, width = 7)
par(mar = c(4,4,1,1))
plot(km4groups, bty = "n", col = 1:4,
     xlab = "t", ylab = "survival functions")
legend("topright",
       c("NALTREXONE=0, THERAPY=0", 
         "NALTREXONE=0, THERAPY=1", 
         "NALTREXONE=1, THERAPY=0", 
         "NALTREXONE=1, THERAPY=1"),
       col = 1:4, lty = 1, bty = "n")
dev.off()


 

## Cox model 
## Lin et al data
cox.fit <- coxph(Surv(futime, relapse) ~ NALTREXONE*THERAPY + 
                   AGE + GENDER + T0_PDA + site,
                 data=COMBINE)
summary(cox.fit)

## robust standard error
cox.fit <- coxph(Surv(futime, relapse) ~ NALTREXONE*THERAPY + 
                   AGE + GENDER + T0_PDA + site,
                 robust = TRUE, 
                 data=COMBINE)
summary(cox.fit)





## Keele (2010)'s data: time to approve a drug by FDA
fda <- read.dta("fda.dta")
head(fda)
names(fda)
cox.fit <- coxph(Surv(acttime, censor) ~ 
                   hcomm + hfloor + scomm + sfloor + 
                   prespart + demhsmaj + demsnmaj + 
                   prevgenx + lethal + 
                   deathrt1 + acutediz + hosp01  + 
                   hospdisc  + hhosleng + 
                   mandiz01 + femdiz01 + peddiz01 + orphdum + 
                   natreg + I(natreg^2) + vandavg3 + wpnoavg3 + 
                   condavg3 + orderent + stafcder, 
                 data=fda)
summary(cox.fit)


cox.fit <- coxph(Surv(acttime, censor) ~ 
                   hcomm + hfloor + scomm + sfloor + 
                   prespart + demhsmaj + demsnmaj + 
                   prevgenx + lethal + 
                   deathrt1 + acutediz + hosp01  + 
                   hospdisc  + hhosleng + 
                   mandiz01 + femdiz01 + peddiz01 + orphdum + 
                   natreg + I(natreg^2) + vandavg3 + wpnoavg3 + 
                   condavg3 + orderent + stafcder, 
                 robust = TRUE, 
                 data=fda)
summary(cox.fit)


## Gehan's data in the MASS package 
library(MASS)
head(gehan)
km.gehan = survfit(Surv(time, cens) ~ treat,
                   data = gehan)
pdf("gehan_kmcurve.pdf", height = 4, width = 7)
par(mar = c(4,4,1,1))
plot(km.gehan, lty = 1:2, conf.int = TRUE,
     bty = "n", xlab = "t",
     ylab = "survival functions")
legend("topright",
       c("6-MP", "control"),
       lty = 1:2, bty = "n")
dev.off()

cox.gehan = coxph(Surv(time, cens) ~ treat,
                  data = gehan)
summary(cox.gehan)

survdiff(Surv(time, cens) ~ treat,
         data = gehan)



## Chapter 27.5.1
## stratified Cox model 
cox.fit <- coxph(Surv(futime, relapse) ~ NALTREXONE*THERAPY + 
                   AGE + GENDER + T0_PDA + strata(site), 
                 robust = TRUE, 
                 data=COMBINE)
summary(cox.fit)



## Chapter 27.5.2
## paired data
library("timereg")
data(diabetes)
pair.cox = coxph(Surv(time, status) ~ treat + adult + agedx,
                 robust = TRUE, 
                 data = diabetes)
summary(pair.cox)
pair.cox = coxph(Surv(time, status) ~ treat + adult + agedx,
                 robust = TRUE, cluster = id,
                 data = diabetes)
summary(pair.cox)




