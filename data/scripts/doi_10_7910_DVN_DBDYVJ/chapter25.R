rm(list = ls())


## continuous outcome
## split-plot design
Pten = read.csv("PtenAnalysisData.csv")[, -(7:9)]
head(Pten)
table(Pten$mouseid, Pten$fa, Pten$pten)

library("gee")
Pten.gee = gee(somasize ~ factor(fa)*pten, 
               id = mouseid, 
               family = gaussian, 
               corstr = "independence",
               data = Pten)
summary(Pten.gee)$coef 


Pten.gee = gee(somasize ~ factor(fa)*pten, 
               id = mouseid, 
               family = gaussian, 
               corstr = "exchangeable",
               data = Pten)
summary(Pten.gee)$coef


Pten.gee = gee(somasize ~ factor(fa)*pten + numctrl + numpten, 
               id = mouseid, 
               family = gaussian, 
               corstr = "independence",
               data = Pten)
summary(Pten.gee)$coef 
 

Pten.gee = gee(somasize ~ factor(fa)*pten + numctrl + numpten, 
               id = mouseid, 
               family = gaussian, 
               corstr = "exchangeable",
               data = Pten)
summary(Pten.gee)$coef


## binary outcome
hygaccess = read.csv("hygaccess.csv")
hygaccess = hygaccess[,c("r4_hyg_access", "treat_cat_1", 
                         "bl_c_hyg_access", "vid", "eligible")]
hygaccess = hygaccess[which(hygaccess$eligible=="Eligible"&
                              hygaccess$r4_hyg_access!="Missing"),]
hygaccess$y = ifelse(hygaccess$r4_hyg_access=="Yes", 1, 0)
hygaccess$z = hygaccess$treat_cat_1
hygaccess$x = hygaccess$bl_c_hyg_access


hygaccess.gee = gee(y ~ z, id = vid,
                    family = binomial(link = logit),
                    corstr = "independence", 
                    data = hygaccess)
summary(hygaccess.gee)$coef

hygaccess.gee = gee(y ~ z, id = vid,
                    family = binomial(link = logit),
                    corstr = "exchangeable", 
                    data = hygaccess)
summary(hygaccess.gee)$coef


hygaccess.gee = gee(y ~ z + x, id = vid,
                    family = binomial(link = logit),
                    corstr = "independence", 
                    data = hygaccess)
summary(hygaccess.gee)$coef

hygaccess.gee = gee(y ~ z + x, id = vid,
                    family = binomial(link = logit),
                    corstr = "exchangeable", 
                    data = hygaccess)
summary(hygaccess.gee)$coef


## longtudinal data analysis
library("gee")
library("foreign")
gym1 = read.dta("gym_treatment_exp_weekly.dta")
f.reg = weekly_visit ~ incentive_commit + incentive + target + member_gym_pre

## normal gee
normal.gee = gee(f.reg, id = id, 
               family = gaussian, 
               corstr = "independence", 
               data = gym1)
normal.gee = summary(normal.gee)$coef
normal.gee

## before 
normal.gee1 = gee(f.reg, id = id, 
                 subset = (incentive_week<0),
                 family = gaussian, 
                 corstr = "independence", 
                 data = gym1)
normal.gee1 = summary(normal.gee1)$coef
normal.gee1
 
## short term 
normal.gee2 = gee(f.reg, id = id, 
                  subset = (incentive_week>0&incentive_week<15),
                  family = gaussian, 
                  corstr = "independence", 
                  data = gym1)
normal.gee2 = summary(normal.gee2)$coef  
normal.gee2

## long term 
normal.gee3 = gee(f.reg, id = id, 
                  subset = (incentive_week>=15),
                  family = gaussian, 
                  corstr = "independence", 
                  data = gym1)
normal.gee3 = summary(normal.gee3)$coef
normal.gee3


## poisson gee 
poisson.gee = gee(f.reg, id = id, 
                  family = poisson(link = log), 
                  corstr = "independence", 
                  data = gym1)
poisson.gee = summary(poisson.gee)$coef
poisson.gee
 
poisson.gee1 = gee(weekly_visit ~ incentive_commit + incentive + target, 
                  id = id, 
                  subset = (incentive_week<0),
                  family = poisson(link = log), 
                  corstr = "independence", 
                  data = gym1)
poisson.gee1 = summary(poisson.gee1)$coef
poisson.gee1


poisson.gee2 = gee(f.reg, id = id, 
                  subset = (incentive_week>0&incentive_week<15),
                  family = poisson(link = log), 
                  corstr = "independence", 
                  data = gym1)
poisson.gee2 = summary(poisson.gee2)$coef
poisson.gee2

poisson.gee3 = gee(f.reg, id = id, 
                  subset = (incentive_week>=15),
                  family = poisson(link = log), 
                  corstr = "independence", 
                  data = gym1)
poisson.gee3 = summary(poisson.gee3)$coef 
poisson.gee3

## graphical summary
library(ggplot2)
dat.normal = rbind(data.frame(data = "pooled",
                              family = "Normal",
                              treat = "incentivecommit",
                              est = normal.gee[2,1],
                              l.ci.naive = normal.gee[2,1]-1.96*normal.gee[2,2],
                              u.ci.naive = normal.gee[2,1]+1.96*normal.gee[2,2],
                              l.ci.crse = normal.gee[2,1]-1.96*normal.gee[2,4],
                              u.ci.crse = normal.gee[2,1]+1.96*normal.gee[2,4]),
                   data.frame(data = "pooled",
                              family = "Normal",
                              treat = "incentive",
                              est = normal.gee[3,1],
                              l.ci.naive = normal.gee[3,1]-1.96*normal.gee[3,2],
                              u.ci.naive = normal.gee[3,1]+1.96*normal.gee[3,2],
                              l.ci.crse = normal.gee[3,1]-1.96*normal.gee[3,4],
                              u.ci.crse = normal.gee[3,1]+1.96*normal.gee[3,4]),
                   data.frame(data = "before",
                              family = "Normal",
                              treat = "incentivecommit",
                              est = normal.gee1[2,1],
                              l.ci.naive = normal.gee1[2,1]-1.96*normal.gee1[2,2],
                              u.ci.naive = normal.gee1[2,1]+1.96*normal.gee1[2,2],
                              l.ci.crse = normal.gee1[2,1]-1.96*normal.gee1[2,4],
                              u.ci.crse = normal.gee1[2,1]+1.96*normal.gee1[2,4]),
                   data.frame(data = "before",
                              family = "Normal",
                              treat = "incentive",
                              est = normal.gee1[3,1],
                              l.ci.naive = normal.gee1[3,1]-1.96*normal.gee1[3,2],
                              u.ci.naive = normal.gee1[3,1]+1.96*normal.gee1[3,2],
                              l.ci.crse = normal.gee1[3,1]-1.96*normal.gee1[3,4],
                              u.ci.crse = normal.gee1[3,1]+1.96*normal.gee1[3,4]),
                   data.frame(data = "short",
                              family = "Normal",
                              treat = "incentivecommit",
                              est = normal.gee2[2,1],
                              l.ci.naive = normal.gee2[2,1]-1.96*normal.gee2[2,2],
                              u.ci.naive = normal.gee2[2,1]+1.96*normal.gee2[2,2],
                              l.ci.crse = normal.gee2[2,1]-1.96*normal.gee2[2,4],
                              u.ci.crse = normal.gee2[2,1]+1.96*normal.gee2[2,4]),
                   data.frame(data = "short",
                              family = "Normal",
                              treat = "incentive",
                              est = normal.gee2[3,1],
                              l.ci.naive = normal.gee2[3,1]-1.96*normal.gee2[3,2],
                              u.ci.naive = normal.gee2[3,1]+1.96*normal.gee2[3,2],
                              l.ci.crse = normal.gee2[3,1]-1.96*normal.gee2[3,4],
                              u.ci.crse = normal.gee2[3,1]+1.96*normal.gee2[3,4]),
                   data.frame(data = "long",
                              family = "Normal",
                              treat = "incentivecommit",
                              est = normal.gee3[2,1],
                              l.ci.naive = normal.gee3[2,1]-1.96*normal.gee3[2,2],
                              u.ci.naive = normal.gee3[2,1]+1.96*normal.gee3[2,2],
                              l.ci.crse = normal.gee3[2,1]-1.96*normal.gee3[2,4],
                              u.ci.crse = normal.gee3[2,1]+1.96*normal.gee3[2,4]),
                   data.frame(data = "long",
                              family = "Normal",
                              treat = "incentive",
                              est = normal.gee3[3,1],
                              l.ci.naive = normal.gee3[3,1]-1.96*normal.gee3[3,2],
                              u.ci.naive = normal.gee3[3,1]+1.96*normal.gee3[3,2],
                              l.ci.crse = normal.gee3[3,1]-1.96*normal.gee3[3,4],
                              u.ci.crse = normal.gee3[3,1]+1.96*normal.gee3[3,4])) 

dat.poisson = rbind(data.frame(data = "pooled",
                              family = "Poisson",
                              treat = "incentivecommit",
                              est = poisson.gee[2,1],
                              l.ci.naive = poisson.gee[2,1]-1.96*poisson.gee[2,2],
                              u.ci.naive = poisson.gee[2,1]+1.96*poisson.gee[2,2],
                              l.ci.crse = poisson.gee[2,1]-1.96*poisson.gee[2,4],
                              u.ci.crse = poisson.gee[2,1]+1.96*poisson.gee[2,4]),
                   data.frame(data = "pooled",
                              family = "Poisson",
                              treat = "incentive",
                              est = poisson.gee[3,1],
                              l.ci.naive = poisson.gee[3,1]-1.96*poisson.gee[3,2],
                              u.ci.naive = poisson.gee[3,1]+1.96*poisson.gee[3,2],
                              l.ci.crse = poisson.gee[3,1]-1.96*poisson.gee[3,4],
                              u.ci.crse = poisson.gee[3,1]+1.96*poisson.gee[3,4]),
                   data.frame(data = "before",
                              family = "Poisson",
                              treat = "incentivecommit",
                              est = poisson.gee1[2,1],
                              l.ci.naive = poisson.gee1[2,1]-1.96*poisson.gee1[2,2],
                              u.ci.naive = poisson.gee1[2,1]+1.96*poisson.gee1[2,2],
                              l.ci.crse = poisson.gee1[2,1]-1.96*poisson.gee1[2,4],
                              u.ci.crse = poisson.gee1[2,1]+1.96*poisson.gee1[2,4]),
                   data.frame(data = "before",
                              family = "Poisson",
                              treat = "incentive",
                              est = poisson.gee1[3,1],
                              l.ci.naive = poisson.gee1[3,1]-1.96*poisson.gee1[3,2],
                              u.ci.naive = poisson.gee1[3,1]+1.96*poisson.gee1[3,2],
                              l.ci.crse = poisson.gee1[3,1]-1.96*poisson.gee1[3,4],
                              u.ci.crse = poisson.gee1[3,1]+1.96*poisson.gee1[3,4]),
                   data.frame(data = "short",
                              family = "Poisson",
                              treat = "incentivecommit",
                              est = poisson.gee2[2,1],
                              l.ci.naive = poisson.gee2[2,1]-1.96*poisson.gee2[2,2],
                              u.ci.naive = poisson.gee2[2,1]+1.96*poisson.gee2[2,2],
                              l.ci.crse = poisson.gee2[2,1]-1.96*poisson.gee2[2,4],
                              u.ci.crse = poisson.gee2[2,1]+1.96*poisson.gee2[2,4]),
                   data.frame(data = "short",
                              family = "Poisson",
                              treat = "incentive",
                              est = poisson.gee2[3,1],
                              l.ci.naive = poisson.gee2[3,1]-1.96*poisson.gee2[3,2],
                              u.ci.naive = poisson.gee2[3,1]+1.96*poisson.gee2[3,2],
                              l.ci.crse = poisson.gee2[3,1]-1.96*poisson.gee2[3,4],
                              u.ci.crse = poisson.gee2[3,1]+1.96*poisson.gee2[3,4]),
                   data.frame(data = "long",
                              family = "Poisson",
                              treat = "incentivecommit",
                              est = poisson.gee3[2,1],
                              l.ci.naive = poisson.gee3[2,1]-1.96*poisson.gee3[2,2],
                              u.ci.naive = poisson.gee3[2,1]+1.96*poisson.gee3[2,2],
                              l.ci.crse = poisson.gee3[2,1]-1.96*poisson.gee3[2,4],
                              u.ci.crse = poisson.gee3[2,1]+1.96*poisson.gee3[2,4]),
                   data.frame(data = "long",
                              family = "Poisson",
                              treat = "incentive",
                              est = poisson.gee3[3,1],
                              l.ci.naive = poisson.gee3[3,1]-1.96*poisson.gee3[3,2],
                              u.ci.naive = poisson.gee3[3,1]+1.96*poisson.gee3[3,2],
                              l.ci.crse = poisson.gee3[3,1]-1.96*poisson.gee3[3,4],
                              u.ci.crse = poisson.gee3[3,1]+1.96*poisson.gee3[3,4])) 

dat = rbind(dat.normal, dat.poisson)
ggplot(dat) + 
  geom_point(aes(x=treat, y=est), size = 0.6) + 
  geom_segment(aes(x=treat, y=l.ci.naive,
                   xend=treat, yend=u.ci.naive),
               alpha = 0.3, size = 3) + 
  geom_segment(aes(x=treat, y=l.ci.crse,
                   xend=treat, yend=u.ci.crse),
               alpha = 0.3, size = 1) +
  geom_hline(aes(yintercept = 0), 
             alpha = 0.3, size = 0.3, linetype = "dotted") + 
  facet_grid(family~data) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  scale_x_discrete(labels= c("inc_com", "inc")) +
  ggtitle("Confidence intervals based on EHW and LZ standard errors")
ggsave("ci_ehw_lz_gym.pdf", height = 4, width = 7)




