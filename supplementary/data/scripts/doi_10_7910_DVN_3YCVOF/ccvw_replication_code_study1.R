### Replication code for Study One in 
### Campbell, R., Cowley, P., Vivyan, N & Wagner, M. (2016) 'Legislator dissent as a valence signal'.
### British Journal of Political Science


### set working directory

rm(list = ls()); gc()
setwd("~/Dropbox/MP independence/replication_materials")


### load packages

library(car)
library(ggplot2)
library(stargazer)
library(ordinal)


### Load in Study 1 data: observational survey data on voter attitudes toward MPs

obsdat <- readRDS("data/study1data.rds")


### Create neat labels for variables

resp.control.labels <- c("Fairly interested in politics",
                         "Not very interested in politics",
                         "Not at all interested in politics",
                         "DK interest in politics",
                         "Age 18-24",
                         "Age 25-34",
                         "Age 35-44",
                         "Age 45-54",
                         "Age 55-64",
                         "Scotland",
                         "North",
                         "South (non-London)",
                         "Midlands & Wales",
                         "Female",
                         "Social grade C1",
                         "Social grade C2",
                         "Social grade DE"
)

loyalist.labels <- c("MP fairly party loyalist", "MP neither", "MP fairly independent-minded",
                     "MP very indepdendent-minded", "DK MP independence")

voteagainst.labels <- c("MP rarely rebels", "MP regularly rebels", "DK MP rebellion") 

pmpcong.labels <- c("MP perceieved as co-partisan")
pmpcongfg.labels <- c("MP perceieved as opposing partisan", "MP perceieved as co-partisan")

Rebellion_r.labels <- c("MP rebelled 1-5 times in 2012-13","MP rebelled more than times in 2012-13")
Rebellion201213.labels <- c("Number of MP rebellions 2012-13")



### Table 1 (main manuscript)

### Regression models of satisfaction with local MP.
### Key predictor is a continuous measure of perceived MP independence (note: this means that a number of don't know responses to perceived inpdendence are dropped)

# m1
the.vars <- c("satismp", "loyalist.num", "PANO")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist.num, data = the.dat, x = TRUE, y = TRUE)
print(m1cont <- robcov(mtmp, cluster = the.dat$PANO))
# m2
the.vars <- c("satismp", "loyalist.num", "PANO", "loyalist.num", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist.num + mpcongfg + payroll + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(m2cont <- robcov(mtmp, cluster = the.dat$PANO))
# m3
the.vars <- c("satismp", "loyalist.num", "PANO", "loyalist.num", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8","mpknowledge")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist.num + mpcongfg  + mpknowledge + payroll + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(m3cont <- robcov(mtmp, cluster = the.dat$PANO))
# m4
the.vars <- c("satismp", "loyalist.num", "PANO", "loyalist.num", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8","mpknowledge")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist.num*mpcongfg  + mpknowledge + payroll + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(m4cont <- robcov(mtmp, cluster = the.dat$PANO))
# neat covariate names
covnames  <- c("Intercept", 
               "Perceived MP independence",
               "Voter-MP partisanship: Non-partisans",
               "Voter-MP partisanship: Co-partisans",
               "Perceived MP independence X Non-partisans",
               "Perceived MP independence X Co-partisans",
               "Knowledge about MP")
# generate table
stargazer(list(m1cont, m2cont, m3cont,m4cont),type="html",
          out="tables/table1.html",
          dep.var.labels.include=FALSE, dep.var.caption="",
          intercept.bottom=FALSE,
          order = c("Constant","loyalist.num$", "^mpcongfg=Non-partisan", "^mpcongfg=Co-partisan",
                    "loyalist.num","mpknowledge"),
          omit = c("(socialgrade)|(bpcas)|(newgor)|(interest)",
                   "(payroll)|(parlTenure)|(birthyear)|(degree)|(Maj10)|(WomanMP)"),
          omit.labels = c("Respondent controls","MP controls"),
          omit.stat = c("adj.rsq", "chi2"),
          covariate.labels = covnames,
          omit.table.layout = "n"
)



###	Table S1 (Online appendix)

### Ordered probit regression models of satisfaction with local MP

# m1
the.vars <- c("satismp", "loyalist.num", "PANO")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
summary(clm1 <- clm(factor(satismp) ~ loyalist.num, data = the.dat, link = "probit"))
# m2
the.vars <- c("satismp", "loyalist.num", "PANO", "loyalist.num", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
summary(clm2 <- clm(factor(satismp) ~ loyalist.num + mpcongfg + payroll + parlTenure + 
                      I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
                      interest + bpcas2 + socialgrade + newgor_w8
                    , data = the.dat, link = "probit"))
# m3
the.vars <- c("satismp", "loyalist.num", "PANO", "loyalist.num", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8","mpknowledge")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
summary(clm3 <- clm(factor(satismp) ~ loyalist.num + mpcongfg  + mpknowledge + payroll + parlTenure + 
                      I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
                      interest + bpcas2 + socialgrade + newgor_w8
                    , data = the.dat, link = "probit"))
# m4
the.vars <- c("satismp", "loyalist.num", "PANO", "loyalist.num", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8","mpknowledge")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
summary(clm4 <- clm(factor(satismp) ~ loyalist.num*mpcongfg  + mpknowledge + payroll + parlTenure + 
                      I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
                      interest + bpcas2 + socialgrade + newgor_w8
                    , data = the.dat, link = "probit"))

covnames  <- c("Perceived MP independence",
               "Voter-MP partisanship: Non-partisans",
               "Voter-MP partisanship: Co-partisans",
               "Perceived MP independence X Non-partisans",
               "Perceived MP independence X Co-partisans",
               "Knowledge about MP",
               "Threshold 1:2","Threshold 2:3","Threshold 3:4","Threshold 4:5")
stargazer(list(clm1, clm2, clm3, clm4),type="html",
          out="tables/tableS1.html",
          dep.var.labels.include=FALSE, dep.var.caption="",
          intercept.bottom=FALSE,
          order = c("Constant","loyalist.num$", "^mpcongfg",
                    "loyalist.num","mpknowledge"),
          omit = c("(socialgrade)|(bpcas)|(newgor)|(interest)",
                   "(payroll)|(parlTenure)|(birthyear)|(degree)|(Maj10)|(WomanMP)"),
          omit.labels = c("Respondent controls","MP controls"),
          omit.stat = c("adj.rsq", "chi2")
          ,ord.intercepts = TRUE
          ,covariate.labels = covnames,
          omit.table.layout = "n"
)



###	Table S2 (Online appendix) 

### OLS models of satisfaction with MP where perceived independence is included as a categorical predictor

# m1
the.vars <- c("satismp", "loyalist", "PANO")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist, data = the.dat, x = TRUE, y = TRUE)
print(m1c <- robcov(mtmp, cluster = the.dat$PANO))
# m2
the.vars <- c("satismp", "loyalist", "PANO", "loyalist", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist + mpcongfg + payroll + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(m2c <- robcov(mtmp, cluster = the.dat$PANO))
# m3
the.vars <- c("satismp", "loyalist", "PANO", "loyalist", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8","mpknowledge")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist + mpcongfg  + mpknowledge + payroll + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(m3c <- robcov(mtmp, cluster = the.dat$PANO))

covnames  <- c("Intercept", 
               "MP independence: Fairly loyal",
               "MP independence: Neither",
               "MP independence: Fairly independent-minded", 
               "MP independence: Very independent-minded",
               "MP independence: Don\'t know",
               "Voter-MP partisanship: Non-partisans",
               "Voter-MP partisanship: Co-partisans",
               "Knowledge about MP"
)
stargazer(list(m1c, m2c, m3c),type="html",
          out="tables/tableS2.html",
          dep.var.labels.include=FALSE, dep.var.caption="",
          intercept.bottom=FALSE,
          order = c("Constant","loyalist=Fairly party loyalist$","loyalist=Neither$",
                    "loyalist=Fairly independent-minded$", "loyalist=Very independent-minded$",
                    "loyalist=Don\\'t know$",
                    "^mpcongfg","\\*","mpknowledge"),
          omit = c("(socialgrade)|(bpcas)|(newgor)|(interest)",
                   "(payroll)|(parlTenure)|(birthyear)|(degree)|(Maj10)|(WomanMP)"),
          omit.labels = c("Respondent controls","MP controls"),
          omit.stat = c("adj.rsq", "chi2")
          ,covariate.labels = covnames,
          omit.table.layout = "n"
)


###	Figures S2 (Online appendix) 

### Plot main effects of perceived independance based on Model 2 in Table S2

coeftab <- data.frame(coefname = c("Very loyal","Fairly loyal", "Neither", "Fairly independent-minded",
                                   "Very independent-minded", "Don't know"))
coeftab$coefhat <- c(0,m2c$coef[grep("loyalist", names(m2c$coef))])
se.clus <- sqrt(diag(m2c$var))
coeftab$se.clus <- c(0,se.clus[grep("loyalist", names(m2c$coef))])
coeftab$cilo <- coeftab$coefhat - (1.96*coeftab$se.clus)
coeftab$cihi <- coeftab$coefhat + (1.96*coeftab$se.clus)
coeftab$coefname <- factor(coeftab$coefname, levels = c("Don't know", "Very loyal", "Fairly loyal", "Neither", "Fairly independent-minded", "Very independent-minded"))

ggplot(coeftab, aes(x = coefname, y = coefhat)) +
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = cilo, ymax = cihi),size = 1, shape = 21, fill = "white") + 
  labs(y = "Effect on satisfaction with MP",x = "Perceived independence of MP") + 
  coord_flip() + 
  theme(axis.ticks.y = element_blank(), axis.text.x = element_text(colour = "black", size = 12), 
        axis.title.x = element_text(size = 12, face = "bold", vjust = 0),
        axis.title.y = element_text(size = 12, face = "bold", vjust = 1),
        axis.text.y = element_text(colour = "black", size = 12, hjust=1)) +
  theme(legend.position="bottom") +
  scale_shape_discrete(name="") + scale_color_discrete(name="")
ggsave("figures/figureS2.eps", height = 4, width = 8)



###	Table S3 (online appendix)

### OLS regression of satisfaction with local MP; perceived independence entered as a categorical variable and interacted with partisanship

# Collapse fairly and very independent-minded categories due to sparseness
obsdat$loyalistalt <- car:::recode(obsdat$loyalist, 
                                   "c('Fairly independent-minded', 'Very independent-minded')='Fairly/Very independent-minded'",
                                   as.factor.result=TRUE,
                                   levels = c("Very party loyalist", "Fairly party loyalist", "Neither", 
                                              "Fairly/Very independent-minded",
                                              "Don't know")                        
)
with(obsdat, table(loyalist, loyalistalt, useNA="ifany"))


# m4
the.vars <- c("satismp", "loyalistalt", "PANO", "loyalistalt", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8","mpknowledge")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalistalt*mpcongfg  + mpknowledge + payroll + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(m4alt <- robcov(mtmp, cluster = the.dat$PANO))

covnames  <- c("Intercept", 
               "MP independence: Fairly loyal",
               "MP independence: Neither",
               "MP independence: Fairly/Very independent-minded", 
               "MP independence: Don\'t know",
               "Voter-MP partisanship: Non-partisans",
               "Voter-MP partisanship: Co-partisans",
               "Fairly loyal X Non-partisans",
               "Neither X Non-partisans",
               "Fairly/Very independent-minded X Non-partisans", 
               "Don\'t know X Non-partisans",
               "Fairly loyal X Co-partisans",
               "Neither X Co-partisans",
               "Fairly/Very independent-minded X Co-partisans", 
               "Don\'t know X Co-partisans",
               "Knowledge about MP")
stargazer(list(m4alt),type="html",
          out="tables/tableS3.html",
          dep.var.labels.include=FALSE, dep.var.caption="",
          intercept.bottom=FALSE,
          order = c("Constant","loyalistalt=Fairly party loyalist$","loyalistalt=Neither$",
                    "loyalistalt=Fairly/Very independent-minded$",
                    "loyalistalt=Don\\'t know$",
                    "^mpcongfg","\\*","mpknowledge"),
          omit = c("(socialgrade)|(bpcas)|(newgor)|(interest)",
                   "(payroll)|(parlTenure)|(birthyear)|(degree)|(Maj10)|(WomanMP)"),
          omit.labels = c("Respondent controls","MP controls"),
          omit.stat = c("adj.rsq", "chi2")
          ,covariate.labels = covnames,
          omit.table.layout = "n"
)



### Figure S3 (online appendix) 

### Plot effects of perceived independnece categories conditional on partisanship (based on model in Table S3)

beta.hat <- m4alt$coef
cov <- m4alt$var
coeftab <- expand.grid(loyalist = factor(c("Very party loyalist","Fairly party loyalist", "Neither", "Fairly/Very independent-minded", "Don't know"), levels = c("Very party loyalist","Fairly party loyalist", "Neither", "Fairly/Very independent-minded"
                                                                                                                                                                 , "Don't know")),
                       mpcongfg = factor(c("Opp. partisan", "Non-partisan", "Co-partisan"),
                                         levels = c("Opp. partisan", "Non-partisan", "Co-partisan"))
)
coeftab$me <- NA
coeftab$se <- NA
coeflist <- split(coeftab, coeftab$mpcongfg)
# marginal effects and se in baseline condition 
tmpdf <- coeflist$"Opp. partisan"
tmpdf$me[1] <- 0
tmpdf$se[1] <- 0
for(i in 2:nrow(tmpdf)){
  the.name <- paste0("loyalistalt=", tmpdf$loyalist[i])
  tmpdf$me[i] <- beta.hat[the.name]
  tmpdf$se[i] <- sqrt(cov[the.name,the.name])
}
coeflist$"Opp. partisan" <- tmpdf
# marginal effects for non-baseline conditions
for(j in 2:3){
  tmpdf <- coeflist[[j]]
  tmpdf$me[1] <- 0
  tmpdf$se[1] <- 0
  for(i in 2:nrow(tmpdf)){
    the.mainname <- paste0("loyalistalt=", tmpdf$loyalist[i])
    the.cpname <- paste0(the.mainname, " * mpcongfg=", tmpdf$mpcongfg[i])
    tmpdf$me[i] <- beta.hat[the.mainname] + beta.hat[the.cpname]
    tmpdf$se[i] <- sqrt(cov[the.mainname,the.mainname] +
                          (1^2*cov[the.cpname, the.cpname]) + (2*1*cov[the.mainname, the.cpname])) 
  }
  coeflist[[j]] <- tmpdf
}
# now rbind together and calculate CIs
coeftab <- do.call("rbind", coeflist)
coeftab$cilo <- coeftab$me - (1.96*coeftab$se)
coeftab$cihi <- coeftab$me + (1.96*coeftab$se)

coeftab$mpcongfg <- factor(coeftab$mpcongfg,
                           levels = rev(c("Opp. partisan","Non-partisan", "Co-partisan")))
coeftab$loyalist <- car:::recode(coeftab$loyalist, 
                                 "'Very party loyalist'='Very loyal'; 'Fairly party loyalist'='Fairly loyal'",
                                 as.factor.result=TRUE, levels = c("Don't know", "Very loyal", "Fairly loyal", 
                                                                   "Neither", "Fairly/Very independent-minded"))

ggplot(coeftab, aes(x = loyalist, y = me, shape = mpcongfg, fill = mpcongfg)) +
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = cilo, ymax = cihi),
                  position = position_dodge(width = - 0.6),size = 0.8) + 
  labs(y = "Effect on satisfaction with MP",x = "Perceived independence of MP") + 
  coord_flip() + 
  theme(axis.ticks.y = element_blank(), axis.text.x = element_text(colour = "black", size = 12), 
        axis.title.x = element_text(size = 12, face = "bold", vjust = 0),
        axis.title.y = element_text(size = 12, face = "bold", vjust = 1),
        axis.text.y = element_text(colour = "black", size = 12, hjust=1)) +
  scale_shape_manual(name = "Voter-MP partisanship",  
                     values = c(21,21,21)) +
  scale_fill_manual(name = "Voter-MP partisanship",  
                    values = c("White","Gray","Black")) +
  theme(legend.position="bottom")
ggsave("figures/figureS3.eps", height = 5.5, width = 8)



###	Table S4 (Online Appendix)

### Regression models of perceived MP independent-mindedness (respondents with non-payroll MPs only)

obsdat$log1prebellion <- log1p(obsdat$rebellionrate)

# m1
the.vars <- c("loyalist.num", "log1prebellion", "PANO")
the.dat <- obsdat[obsdat$payroll==0,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(loyalist.num ~ log1prebellion, data = the.dat, x = TRUE, y = TRUE)
print(aux1 <- robcov(mtmp, cluster = the.dat$PANO))
# m2
the.vars <- c("loyalist.num", "log1prebellion", "PANO", "loyalist", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8")
the.dat <- obsdat[obsdat$payroll==0,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(loyalist.num ~ log1prebellion + mpcongfg + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(aux2 <- robcov(mtmp, cluster = the.dat$PANO))
# write out table
stargazer(list(aux1, aux2), type="text")
covnames  <- c("Intercept", 
               "Log(MP rebellion + 1)",
               "Voter-MP partisanship: Non-partisans",
               "Voter-MP partisanship: Co-partisans",
               "MP tenure",
               "MP age", 
               "MP degree", 
               "Majority size",
               "MP female")
stargazer(list(aux1, aux2),type="html",
          out="tables/tableS4.html",
          dep.var.labels.include=FALSE, dep.var.caption="",
          intercept.bottom=FALSE,
          omit = "(socialgrade)|(bpcas)|(newgor)|(interest)",
          omit.labels = "Respondent characteristics",
          omit.stat = c("adj.rsq", "chi2"),
          covariate.labels = covnames,
          omit.table.layout = "n"
)




###	Table S.5 (Online appendix)

## Interaction between perceived MP independence and MP knowledge when predicting MP satisfaction

# m6, interact MP knowledge and perceived independence
the.vars <- c("satismp", "loyalist.num", "PANO", "loyalist.num", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8","mpknowledge")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist.num*mpknowledge + mpcongfg + payroll + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(m6cont <- robcov(mtmp, cluster = the.dat$PANO))
# m7, interact MP knowledge and perceived independence, as well as partisan congruence and perceievd independnece
the.vars <- c("satismp", "loyalist.num", "PANO", "loyalist.num", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8","mpknowledge")
the.dat <- obsdat[,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(satismp ~ loyalist.num*mpknowledge + loyalist.num*mpcongfg + payroll + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(m7cont <- robcov(mtmp, cluster = the.dat$PANO))


covnames  <- c("Intercept", 
               "Perceived MP independence",
               "Voter-MP partisanship: Non-partisans",
               "Voter-MP partisanship: Co-partisans",
               "Perceived MP independence X Non-partisans",
               "Perceived MP independence X Co-partisans",
               "Knowledge about MP",
               "Perceived MP independence X Knowledge about MP")
stargazer(list(m6cont, m7cont),type="html",
          out="tables/tableS5.html",
          dep.var.labels.include=FALSE, dep.var.caption="",
          intercept.bottom=FALSE,
          order = c("Constant","loyalist.num$", "^mpcongfg=Non-partisan", 
                    "^mpcongfg=Co-partisan","mpcongfg=Non-partisan$", 
                    "mpcongfg=Co-partisan$",
                    "^mpknowledge"),
          omit = c("(socialgrade)|(bpcas)|(newgor)|(interest)",
                   "(payroll)|(parlTenure)|(birthyear)|(degree)|(Maj10)|(WomanMP)"),
          omit.labels = c("Respondent controls","MP controls"),
          omit.stat = c("adj.rsq", "chi2"),
          covariate.labels = covnames,
          omit.table.layout = "n"
)


###	Table A1 (Printed appendix)

## Regression models of voter knowledge of MP rebellion

obsdat$log1prebellion <- log1p(obsdat$rebellionrate)

# m3
the.vars <- c("mpknowledge", "log1prebellion", "PANO")
the.dat <- obsdat[obsdat$payroll==0,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(mpknowledge ~ log1prebellion, data = the.dat, x = TRUE, y = TRUE)
print(aux3 <- robcov(mtmp, cluster = the.dat$PANO))
# m4
the.vars <- c("mpknowledge", "log1prebellion", "PANO", "loyalist", "mpcongfg", 
              "payroll", "parlTenure", "birthyear", "degree", "Maj10",
              "WomanMP", "interest", "bpcas2", "socialgrade", "newgor_w8")
the.dat <- obsdat[obsdat$payroll==0,the.vars]
the.dat <- na.omit(the.dat)
mtmp <- ols(mpknowledge ~ log1prebellion + mpcongfg + parlTenure + 
              I(2014-birthyear) + degree + I(Maj10/100) + WomanMP +
              interest + bpcas2 + socialgrade + newgor_w8
            , data = the.dat, x = TRUE, y = TRUE)
print(aux4 <- robcov(mtmp, cluster = the.dat$PANO))

# write out table
covnames  <- c("Intercept", 
               "Log(MP rebellion + 1)",
               "Voter-MP partisanship: Non-partisans",
               "Voter-MP partisanship: Co-partisans",
               "MP tenure",
               "MP age", 
               "MP degree", 
               "Majority size",
               "MP female")
stargazer(list(aux3, aux4),type="html",
          out="tables/tableA1.html",
          dep.var.labels.include=FALSE, dep.var.caption="",
          intercept.bottom=FALSE,
          omit = "(socialgrade)|(bpcas)|(newgor)|(interest)",
          omit.labels = "Respondent characteristics",
          omit.stat = c("adj.rsq", "chi2"),
          covariate.labels = covnames,
          omit.table.layout = "n"
)












