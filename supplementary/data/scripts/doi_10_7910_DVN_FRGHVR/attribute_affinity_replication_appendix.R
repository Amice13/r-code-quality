## Setup ----
rm(list=ls())


## ---- Install Packages (Uncomment if needed) 
# install.packages('foreign');install.packages('stringr'); 
# install.packages('ggplot2'); install.packages('ggthemes'); 
# install.packages('dplyr'); install.packages('tidyr');
# install.packages('lfe'); install.packages('AER'); install.packages('car');
# install.packages('lmtest');install.packages('multiwayvcov')
# install.packages('psych'); install.packages('stargazer')


## ---- Load Packages 
library('foreign');library('stringr'); 
library('ggplot2'); library('ggthemes'); 
library('dplyr'); library('tidyr');library(lfe); library('AER');
library('lmtest');library('multiwayvcov')
library('psych'); library('stargazer')


## --- Set dir 
#setwd('..')


## --- Creating dir to store figures
plot.dir <- "plots"
dir.create(file.path(plot.dir))

appendix.dir <- "plots/in appendix"
dir.create(file.path(appendix.dir))


## ---- functions
source("code/functions.R")

## ---- Load data conjoint 2014
imm <- read.dta("data/SSI_conjoint_2014.dta")
imm$Mus <- (imm$religion == "Muslim")
imm$Rel <- (imm$attends %in% c("Once a week","More than once a week") )
imm$NotRel <- (imm$attends %in% c("Seldom","Never") )
imm$MidRel <- (imm$attends %in% c("Once or twice a month","A few times a year") )

imm$rel_resp <- (imm$attends_resp %in% c("Once a week","More than once a week") )
imm$notrel_resp <- (imm$attends_resp %in% c("Seldom","Never"))

# Eliminate all profiles that are either very or not very religious
imm <- imm[imm$MidRel == F,]

## ---- Rescaling rating
imm$rating <- ((imm$rating - min(imm$rating, na.rm=T))/(max(imm$rating, na.rm=T) - min(imm$rating, na.rm=T)))


## ---- Load mturk data
load("data/Mturk_rel_2013.Rdata")

## ---- Rescaling rating
mturk13$rating <- ((mturk13$rating - min(mturk13$rating, na.rm=T))/(max(mturk13$rating, na.rm=T) - min(mturk13$rating, na.rm=T)))


## ---- Load omnibus 2015
load("data/Omnibus15_long.Rdata")



### Figure 1: Religiosity Measure by Importance of Religion ----

imm$rel_important_resp <- factor(imm$rel_important_resp,levels = c('6','11','7','8','10'), labels = c('Extremely important','Very important','Somewhat important','Not very important','Not at all important'))

plotdata <- data.frame(prop.table(table(imm$rel_important_resp, imm$attends_resp),2))

import_rel <- ggplot(plotdata, aes(x = Var2, y=Freq, fill=factor(Var1))) +
  geom_bar( stat="identity", position=position_dodge()) + 
  xlab("Respondents' Frequency Attendance Religious Services") + ylab("Density") +
  scale_fill_manual("Importance of Religion in Life", values = c(
    c('Extremely important'='black',
      'Very important'='grey24',
      'Somewhat important'='darkgrey',
      'Not very important'='grey',
      'Not at all important' ='lightgrey')))

pdf("plots/in appendix/Figure1_importancereligion.pdf",height=6, width=10)
import_rel
dev.off()


### Table 1: Full Results (MTurk–religion/religiosity) ----

fit.all <- lm(rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time), data = mturk13)

# Cluster  SE:

fit.all.cls <- cluster.vcov(fit.all, mturk13$responseid)
fit.all.cls <- sqrt(diag(fit.all.cls))


fit.all.cath <- lm(rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time), data = mturk13[mturk13$cath==T,])

# Cluster  SE:

fit.allcath.cls <- cluster.vcov(fit.all.cath, mturk13[mturk13$cath == T,]$responseid)
fit.allcath.cls <- sqrt(diag(fit.allcath.cls))

fit.cathrel <- lm(rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time), data = mturk13[mturk13$cath==T & mturk13$resp_rel==T,])


# Catholic respondents - Cluster bootstrapped SE:

fit.cathrel.cls <- clusbootreg(formula = (rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time)), 
                               data    = mturk13[mturk13$cath == T & mturk13$resp_rel == T,],
                               cluster = "responseid",
                               reps    = 5000)



fit.cathnonrel <- lm(rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time), data = mturk13[mturk13$cath==T & mturk13$resp_notrel==T,])

# Catholic respondents - Cluster bootstrapped SE:

fit.cathnonrel.cls <- clusbootreg(formula = (rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time)), 
                                  data    = mturk13[mturk13$cath == T & mturk13$resp_notrel == T,],
                                  cluster = "responseid",
                                  reps    = 5000)


stargazer(fit.all, fit.all.cath, fit.cathrel, fit.cathnonrel, 
          title = "Full Results Mturk Religion/Religiosity", 
          dep.var.labels = "Rating",
          covariate.labels = c("Religious Immigrant","Muslim Immigrant", "Religious * Muslim Immigrant", "2nd Period", "Constant"),
          column.labels = c("All Respondents","All Catholics","Religious Catholics", "Non Religious Catholics"),
          add.lines = list(
            c("Period Dummy","Yes","Yes","Yes","Yes")),
          keep.stat = "n",
          digits = 2,
          se = list(fit.all.cls, fit.allcath.cls, fit.cathrel.cls$estim[,2], fit.cathnonrel.cls$estim[,2]),
          type ='text')



### Figure 3: Results for Non-Religious and Religious Catholic Respondents (MTurk– religion/religiosity) ----


mturk13$responseid <- as.character(mturk13$responseid)
mturk13$tx.time <- as.factor(mturk13$tx.time) 



## ---- Religious Catholics


fit.cathrel <- lm(rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time), data = mturk13[mturk13$cath==T & mturk13$resp_rel==T,])

# Religious Catholics - Cluster bootstrapped SE:

fit.cathrel.cls <- clusbootreg(formula = (rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time)), 
                               data    = mturk13[mturk13$cath == T & mturk13$resp_rel == T,],
                               cluster = "responseid",
                               reps    = 5000)


# Obtain the following effects (means) for all combinations

# yhat(Muslim, Religious)
yhat00 <- fit.cathrel.cls$estim[1,1]
yhat01 <- fit.cathrel.cls$estim[1,1] + fit.cathrel.cls$estim[2,1]
yhat10 <- fit.cathrel.cls$estim[1,1] + fit.cathrel.cls$estim[3,1]
yhat11 <- fit.cathrel.cls$estim[1,1] + fit.cathrel.cls$estim[2,1] + fit.cathrel.cls$estim[3,1] + fit.cathrel.cls$estim[4,1]


# Calculating all covariances:

covar.names <- colnames(fit.cathrel.cls$boot.vcov)
all.combs   <- combn(covar.names[-5], 2)
all.cov     <- c()
for(i in 1:dim(all.combs)[2]){
  all.cov[i] <- fit.cathrel.cls$boot.vcov[all.combs[1,i], all.combs[2,i]]   
}

all.var <- diag(fit.cathrel.cls$boot.vcov)[-5] # remove time fx effect

var00 <- all.var[1] 
var01 <- all.var[1] + all.var[2] + 2*all.cov[2]
var10 <- all.var[1] + all.var[3] + 2*all.cov[1]
var11 <- sum(all.var) + sum(2*all.cov)

yhat <- c(yhat00, yhat01, yhat10, yhat11)
var_all <- c(var00, var01, var10, var11)
lab <- c("Non-Religious","Religious","Non-Religious","Religious")


pdf("plots/in appendix/Figure3_rel_cath_resp_part1.pdf")
plot(c(yhat), 
     xlim = c(0.5, 4.5),
     ylim = c(0, 1.2),
     pch = 20,
     xaxt='n',
     main="Among Religious Catholics Respondents",
     xlab="",
     ylab="Rating")
lim <- par("usr")
rect(0, lim[1]-1,2.5, lim[4]+1, border="black", col="lightgray", lwd=3)
points(c(yhat)[1:2], pch=20)
text(x=c(1.5, 3.5), y=1.1, label=c("Catholic \n Immigrants", "Muslim \n Immigrants"), cex=.8)
text(x=c(1.5, 4), y=0.3, label=c("|Diff in Means| = 0.03 \n pvalue = 0.55", "|Diff in Means| = 0.25 \n pvalue = 0.02"), cex=.9)

axis(1, pos=.25, at=c(1:4), tick=F, lab, cex.axis=0.7, las=1)
for(i in 1:4){
  arrows(x0=i, x1=i,
         y0=c(yhat)[i]+ qt(.975, df = fit.cathrel$df) * c(sqrt(var_all[i])),
         y1=c(yhat)[i]- qt(.975, df = fit.cathrel$df) * c(sqrt(var_all[i])),
         length=0.2, 
         angle=90, 
         code=3)  
}
dev.off()


## ---- Non-Religious Catholics


fit.cathnonrel <- lm(rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time), data = mturk13[mturk13$cath==T & mturk13$resp_notrel==T,])

# Non Religious Catholics - Cluster bootstrapped SE:

fit.cathnonrel.cls <- clusbootreg(formula = (rating ~ Rel + Mus + I(Rel*Mus) + as.factor(tx.time)), 
                                  data    = mturk13[mturk13$cath == T & mturk13$resp_notrel == T,],
                                  cluster = "responseid",
                                  reps    = 5000)


# Obtain the following effects (means) for all possible combinations

# yhat(Muslim, Religious)
yhat00 <- fit.cathnonrel.cls$estim[1,1]
yhat01 <- fit.cathnonrel.cls$estim[1,1] + fit.cathnonrel.cls$estim[2,1]
yhat10 <- fit.cathnonrel.cls$estim[1,1] + fit.cathnonrel.cls$estim[3,1]
yhat11 <- fit.cathnonrel.cls$estim[1,1] + fit.cathnonrel.cls$estim[2,1] + fit.cathnonrel.cls$estim[3,1] + fit.cathnonrel.cls$estim[4,1]


# Calculating all covariances:

covar.names <- colnames(fit.cathnonrel.cls$boot.vcov)
all.combs   <- combn(covar.names[-5], 2)
all.cov     <- c()
for(i in 1:dim(all.combs)[2]){
  all.cov[i] <- fit.cathnonrel.cls$boot.vcov[all.combs[1,i], all.combs[2,i]]   
}

all.var <- diag(fit.cathnonrel.cls$boot.vcov)[-5] # remove time fx effect

var00 <- all.var[1] 
var01 <- all.var[1] + all.var[2] + 2*all.cov[2]
var10 <- all.var[1] + all.var[3] + 2*all.cov[1]
var11 <- sum(all.var) + sum(2*all.cov)

yhat <- c(yhat00, yhat01, yhat10, yhat11)
var_all <- c(var00, var01, var10, var11)
lab <- c("Non-Religious","Religious","Non-Religious","Religious")


# Save plot 
pdf("plots/in appendix/Figure3_nonrel_cath_resp_part2.pdf")
plot(c(yhat), 
     xlim = c(0.5, 4.5),
     ylim = c(0, 1.2),
     pch = 20,
     xaxt='n',
     main="Among Non Religious Catholics Respondents",
     xlab="",
     ylab="Rating")
lim <- par("usr")
rect(0, lim[1]-1,2.5, lim[4]+1, border="black", col="lightgray", lwd=3)
points(c(yhat)[1:2], pch=20)
text(x=c(1.5, 3.5), y=1.1, label=c("Catholic \n Immigrants", "Muslim \n Immigrants"), cex=.8)
text(x=c(1.5, 3.5), y=0.3, label=c("|Diff in Means| = 0.01 \n pvalue = 0.97", "|Diff in Means| = 0.03 \n pvalue = 0.61"), cex=.9)
axis(1, pos=.25, at=c(1:4), tick=F, lab, cex.axis=0.7, las=1)
for(i in 1:4){
  arrows(x0 = i, x1 = i,
         y0 = c(yhat)[i] + qt(.975, df = fit.cathnonrel$df) * c(sqrt(var_all[i])),
         y1 = c(yhat)[i] - qt(.975, df = fit.cathnonrel$df) * c(sqrt(var_all[i])),
         length=0.2, 
         angle=90, 
         code=3)  
}
dev.off()


### Table 2: All respondents (SSI–conjoint) ----

# Religious Respondents Model Rating:
fit.rel <- (lm(rating ~ Rel + Mus + I(Rel*Mus),
               data = imm[imm$rel_resp==T & imm$religion_resp!='Atheist',]))

fit.rel.vcov <- cluster.vcov(fit.rel, imm[imm$rel_resp==T & imm$religion_resp!='Atheist',]$caseid)


# Religious Respondents Model Choice:
fit.rel2 <- (lm(choice ~ Rel + Mus + I(Rel * Mus),
                data = imm[imm$rel_resp == T & imm$religion_resp != 'Atheist',]))


fit.rel.vcov2 <- cluster.vcov(fit.rel2, imm[imm$rel_resp == T & imm$religion_resp != 'Atheist',]$caseid)


# Non- Religious Respondents Model Rating:
fit.notrel <- (lm(rating ~ Rel + Mus + I(Rel * Mus),
                  data = imm[imm$notrel_resp == T & imm$religion_resp != 'Atheist',]))

fit.notrel.vcov <- cluster.vcov(fit.notrel, imm[imm$notrel_resp == T & imm$religion_resp != 'Atheist',]$caseid)


# Non- Religious Respondents Model Choice:
fit.notrel2 <- (lm(choice ~ Rel + Mus + I(Rel * Mus),
                   data = imm[imm$notrel_resp == T & imm$religion_resp != 'Atheist',]))

fit.notrel.vcov2 <- cluster.vcov(fit.notrel2, imm[imm$notrel_resp == T & imm$religion_resp != 'Atheist',]$caseid)



## Generates Table 2
stargazer(fit.rel, fit.rel2, fit.notrel, fit.notrel2, 
          title = "All respondents, SSI-Conjoint", 
          dep.var.labels = c("Rating", "Choice (0,1)", "Rating", "Choice (0,1)"),
          covariate.labels = c("Religious Immigrant","Muslim Immigrant","Religious * Muslim", "Constant"),
          column.labels = c("Religious Respondents", "Non-Religious Respondents"),
          digits = 2, 
          keep.stat = 'n',
          se = list(sqrt(diag(fit.rel.vcov)), sqrt(diag(fit.rel.vcov2)), sqrt(diag(fit.notrel.vcov)), sqrt(diag(fit.notrel.vcov2))),
          type = 'text',
          model.numbers = F)


### Figure 4: Results for Non-Religious and Religious Respondent Preferences over Muslim and Non-Muslim Immigrants (SSI-conjoint) ----

## ---- Religious Respondents

fit.rel <- (lm(rating ~ Rel + Mus + I(Rel*Mus),
               data = imm[imm$rel_resp==T & imm$religion_resp!='Atheist',]))

fit.rel.vcov <- cluster.vcov(fit.rel, imm[imm$rel_resp==T & imm$religion_resp!='Atheist',]$caseid)


# yhat(Muslim, Religious) Rating means
yhat00 <- fit.rel$coef[1]  # Non Muslim/Non Religious
yhat01 <- fit.rel$coef[1] + fit.rel$coef[2] #Non Muslim/Religious
yhat10 <- fit.rel$coef[1] + fit.rel$coef[3] # Muslim/ Non Religious
yhat11 <- fit.rel$coef[1] + fit.rel$coef[2] + fit.rel$coef[3] + fit.rel$coef[4] #Muslim/ Religious

# Calculating variance

all.var <- diag(fit.rel.vcov)

var00 <- all.var[1] 
var01 <- all.var[1] + all.var[2] + 2*fit.rel.vcov[2,1]
var10 <- all.var[1] + all.var[3] + 2*fit.rel.vcov[3,1]
var11 <- all.var[1] + all.var[2] + all.var[3] + all.var[4] + 
  2 * fit.rel.vcov[2,1] + 2 * fit.rel.vcov[3,1] + 2 * fit.rel.vcov[4,1] +
  2 * fit.rel.vcov[3,2] + 2 * fit.rel.vcov[4,2] + 
  2 * fit.rel.vcov[4,3]

yhat <- c(yhat00, yhat01, yhat10, yhat11)
var_all <- c(var00, var01, var10, var11)

### --- DV: Choice

fit.rel2 <- (lm(choice ~ Rel + Mus + I(Rel * Mus),
                data = imm[imm$rel_resp == T & imm$religion_resp != 'Atheist',]))


fit.rel.vcov2 <- cluster.vcov(fit.rel2, imm[imm$rel_resp == T & imm$religion_resp != 'Atheist',]$caseid)


# yhat(Muslim, Religious)
yhat00.2 <- fit.rel2$coef[1]
yhat01.2 <- fit.rel2$coef[1] + fit.rel2$coef[2]
yhat10.2 <- fit.rel2$coef[1] + fit.rel2$coef[3]
yhat11.2 <- fit.rel2$coef[1] + fit.rel2$coef[2] + fit.rel2$coef[3] + fit.rel$coef[4]

# Calculating variance

all.var2 <- diag(fit.rel.vcov2)

var00.2 <- all.var2[1] 
var01.2 <- all.var2[1] + all.var2[2] + 2 * fit.rel.vcov2[2,1]
var10.2 <- all.var2[1] + all.var2[3] + 2 * fit.rel.vcov2[3,1]
var11.2 <- all.var2[1] + all.var2[2] + all.var2[3] + all.var2[4]+
  2 * fit.rel.vcov2[2,1] + 2 * fit.rel.vcov2[3,1] + 2 * fit.rel.vcov2[4,1] +
  2 * fit.rel.vcov2[3,2] + 2 * fit.rel.vcov2[4,2] + 
  2 * fit.rel.vcov2[4,3]


yhat2 <- c(yhat00.2, yhat01.2, yhat10.2, yhat11.2)
var_all2 <- c(var00.2, var01.2, var10.2, var11.2)
lab <- c("Non-Religious","Religious","Non-Religious","Religious")


pdf("plots/in appendix/Figure4_rel_resp_conjoint_part1.pdf")
plot(c(yhat), 
     xlim = c(0.5, 4.5),
     ylim = c(0.2,.8),
     pch = 20,
     xaxt = 'n',
     main = "Among Religious Respondents",
     xlab = "",
     ylab = "Rating")
points(c(1:4) - .25,yhat2)

lim <- par("usr")
rect(0, lim[1]-1,2.5, lim[4]+1, border="black", col="lightgray", lwd=3)
points(c(yhat)[1:2], pch=20)
points(c(1:2) - .25, c(yhat2)[1:2])
text(x = c(1.5, 3.5), y = .7, label=c("Non-Muslim \n Immigrants", "Muslim \n Immigrants"), cex=.8)
text(x=c(1.5, 3.5), y=0.3, label=c("Rating: |Diff in Means| = 0.02 \n pvalue < 0.001", "Rating: |Diff in Means| = 0.05 \n pvalue = 0.03"), cex=.7)

axis(1, pos = .3, at = c(1:4), tick = F, lab, cex.axis = 0.7, las = 1)
for(i in 1:4){
  arrows(x0 = i, x1=i,
         y0 = c(yhat)[i] + qt(.975, df = fit.rel$df) * c(sqrt(var_all[i])),
         y1 = c(yhat)[i] - qt(.975, df = fit.rel$df) * c(sqrt(var_all[i])),
         length = 0.2, 
         angle = 90, 
         code = 3)  
}

for(i in 1:4){
  arrows(x0 = i-.25, x1=i-.25,
         y0 = c(yhat2)[i] + qt(.975, df = fit.rel2$df) * c(sqrt(var_all2[i])),
         y1 = c(yhat2)[i] - qt(.975, df = fit.rel2$df) * c(sqrt(var_all2[i])),
         length = 0.2, 
         angle = 90, 
         code = 3,
         lty = 2)  
}

legend("topright", c('DV: Rating', "DV: Choice"), lty = c(1,2), cex = .7, bty = 'n')
dev.off()




## ---- NON Religious Respondents


fit.notrel <- (lm(rating ~ Rel + Mus + I(Rel * Mus),
                  data = imm[imm$notrel_resp == T & imm$religion_resp != 'Atheist',]))

fit.notrel.vcov <- cluster.vcov(fit.notrel, imm[imm$notrel_resp == T & imm$religion_resp != 'Atheist',]$caseid)


# yhat(Muslim, Religious)
yhat00 <- fit.notrel$coef[1]
yhat01 <- fit.notrel$coef[1] + fit.notrel$coef[2]
yhat10 <- fit.notrel$coef[1] + fit.notrel$coef[3]
yhat11 <- fit.notrel$coef[1] + fit.notrel$coef[2] + fit.notrel$coef[3] + fit.notrel$coef[4] 

# Calculating variance

all.var <- diag(fit.notrel.vcov)

var00 <- all.var[1] 
var01 <- all.var[1] + all.var[2] + 2 * fit.notrel.vcov[2,1]
var10 <- all.var[1] + all.var[3] + 2 * fit.notrel.vcov[3,1]
var11 <- all.var[1] + all.var[2] + all.var[3] + all.var[4] + 
  2 * fit.notrel.vcov[2,1] + 2 * fit.notrel.vcov[3,1] + 2 * fit.notrel.vcov[4,1] +
  2 * fit.notrel.vcov[3,2] + 2 * fit.notrel.vcov[4,2] + 
  2 * fit.notrel.vcov[4,3]

yhat <- c(yhat00, yhat01, yhat10, yhat11)
var_all <- c(var00, var01, var10, var11)

### --- DV: Choice

fit.notrel2 <- (lm(choice ~ Rel + Mus + I(Rel * Mus),
                   data = imm[imm$notrel_resp == T & imm$religion_resp != 'Atheist',]))

#x <- clusbootreg(formula=rating ~ Rel + Mus, data = imm[imm$rel_resp==T & imm$religion_resp!='Atheist',], cluster="caseid")
fit.notrel.vcov2 <- cluster.vcov(fit.notrel2, imm[imm$notrel_resp == T & imm$religion_resp != 'Atheist',]$caseid)


# yhat(Muslim, Religious)
yhat00.2 <- fit.notrel2$coef[1]
yhat01.2 <- fit.notrel2$coef[1] + fit.notrel2$coef[2]
yhat10.2 <- fit.notrel2$coef[1] + fit.notrel2$coef[3]
yhat11.2 <- fit.notrel2$coef[1] + fit.notrel2$coef[2] + fit.notrel2$coef[3] + fit.notrel2$coef[4]

# Calculating variance

all.var2 <- diag(fit.notrel.vcov2)

var00.2 <- all.var2[1] 
var01.2 <- all.var2[1] + all.var2[2] + 2 * fit.notrel.vcov2[2,1]
var10.2 <- all.var2[1] + all.var2[3] + 2 * fit.notrel.vcov2[3,1]
var11.2 <- all.var2[1] + all.var2[2] + all.var2[3] + all.var2[4] + 
  2 * fit.notrel.vcov2[2,1] + 2 * fit.notrel.vcov2[3,1] + 2 * fit.notrel.vcov2[4,1] +
  2 * fit.notrel.vcov2[3,2] + 2 * fit.notrel.vcov2[4,2] + 
  2 * fit.notrel.vcov2[4,3]



yhat2 <- c(yhat00.2, yhat01.2, yhat10.2, yhat11.2)
var_all2 <- c(var00.2, var01.2, var10.2, var11.2)
lab <- c("Non-Religious","Religious","Non-Religious","Religious")


pdf("plots/in appendix/Figure4_nonrel_resp_conjoint_part2.pdf")
plot(c(yhat), 
     xlim = c(0.5, 4.5),
     ylim = c(0.2,.8),
     pch = 20,
     xaxt = 'n',
     main = "Among Non-Religious Respondents",
     xlab = "",
     ylab = "Rating")
points(c(1:4) - .25,yhat2)

lim <- par("usr")
rect(0, lim[1]-1,2.5, lim[4]+1, border="black", col="lightgray", lwd=3)
points(c(yhat)[1:2], pch=20)
points(c(1:2) - .25, c(yhat2)[1:2])
text(x=c(1.5, 3.5), y=.7, label=c("Non-Muslim \n Immigrants", "Muslim \n Immigrants"), cex=.8)
text(x=c(1.5, 3.5), y=0.3, label=c("Rating: |Diff in Means| = 0.01 \n pvalue = 0.13", "Rating: |Diff in Means| = 0.03 \n pvalue = 0.05"), cex=.7)
axis(1, pos=.3, at=c(1:4), tick=F, lab, cex.axis=0.7, las=1)
for(i in 1:4){
  arrows(x0 = i, x1 = i,
         y0 = c(yhat)[i] + qt(.975, df = fit.notrel$df) * c(sqrt(var_all[i])),
         y1 = c(yhat)[i] - qt(.975, df = fit.notrel$df) * c(sqrt(var_all[i])),
         length = 0.2, 
         angle = 90, 
         code = 3,
         lty = 1)  
}

for(i in 1:4){
  arrows(x0 = i-.25, x1 = i-.25,
         y0 = c(yhat2)[i] + qt(.975, df = fit.notrel2$df) * c(sqrt(var_all2[i])),
         y1 = c(yhat2)[i] - qt(.975, df = fit.notrel2$df) * c(sqrt(var_all2[i])),
         length = 0.2, 
         angle = 90, 
         code = 3,
         lty=2)  
}

legend("topright", c('DV: Rating', "DV: Choice"), lty=c(1,2), cex=.7, bty='n')
dev.off()




### Figures 5 - 11: Setup ----

imm_plots <- read.dta("data/SSI_conjoint_2014.dta")
imm_plots$Mus <- (imm_plots$religion == "Muslim")
imm_plots$Rel <- (imm_plots$attends %in% c("Once a week","More than once a week") )
imm_plots$NotRel <- (imm_plots$attends %in% c("Seldom","Never") )
imm_plots$MidRel <- (imm_plots$attends %in% c("Once or twice a month","A few times a year") )

imm_plots$rel_resp <- (imm_plots$attends_resp %in% c("Once a week","More than once a week") )
imm_plots$notrel_resp <- (imm_plots$attends_resp %in% c("Seldom","Never"))


## Formula
form_all <- formula(choice ~ gender + relevel(country, ref="Mexico") + education + language + religion + attends)


## Ploting data 
data_all <- create.plot.data(form = form_all, 
                             data = imm_plots,
                             basecategory = c("Female","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))

data_ath <- create.plot.data(form = form_all, 
                             data = imm_plots[imm_plots$religion_resp == 'Atheist',],
                             basecategory = c("Female","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))

data_cath <- create.plot.data(form = form_all, 
                              data = imm_plots[imm_plots$religion_resp == 'Catholic',],
                              basecategory = c("Female","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))

data_cathrel <- create.plot.data(form = form_all, 
                                 data = imm_plots[imm_plots$religion_resp == 'Catholic' & imm_plots$rel_resp == T,],
                                 basecategory = c("Female","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))

data_cathnotrel <- create.plot.data(form = form_all, 
                                    data = imm_plots[imm_plots$religion_resp == 'Catholic' & imm_plots$notrel_resp == T,],
                                    basecategory = c("Female","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))

data_jew <- create.plot.data(form = form_all, 
                             data = imm_plots[imm_plots$religion_resp == 'Jewish',],
                             basecategory = c("Female","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))

data_prot <- create.plot.data(form = form_all, 
                              data = imm_plots[imm_plots$religion_resp == 'Protestant',],
                              basecategory = c("Female","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))


 # Ignore Warnings # 

### Figure 5: Full Sample (SSI–conjoint) ----
cjoint_plot(d = data_all, filename = 'plots/in appendix/Figure5_full_sample')

### Figure 6: Atheist Respondents (SSI–conjoint) ----
cjoint_plot(d = data_ath, filename = 'plots/in appendix/Figure6_atheist', a=-0.6, b=0.6)

### Figure 7: Catholic Respondents (SSI–conjoint ----
cjoint_plot(d = data_cath, filename = 'plots/in appendix/Figure7_catholic')

### Figure 8: Religious Catholic Respondents (SSI–conjoint) ----
cjoint_plot(d = data_cathrel, filename = 'plots/in appendix/Figure8_cathrel')

### Figure 9: Non Religious Catholic Respondents (SSI–conjoint) ----
cjoint_plot(d = data_cathnotrel, filename = 'plots/in appendix/Figure9_cathnotrel')

### Figure 10: Jewish Respondents (SSI–conjoint) ----
cjoint_plot(d = data_jew, filename = 'plots/in appendix/Figure10_jew')

### Figure 11: Protestant Respondents (SSI–conjoint) ----
cjoint_plot(d = data_prot, filename = 'plots/in appendix/Figure11_protestant')


rm(imm_plots)



### Table 4: Full Sample, 1st and 2nd time periods with No Label Category (SSI 2015) ----

table4app <- list()
table4app$fullsam <- lm(rating ~ Rel * Mus + as.factor(tx.time), data = ssi15)
table4app$fullsam$se <- coeftest(table4app$fullsam , vcov = vcovCluster(table4app$fullsam, cluster = ssi15$pid))[,2]

table4app$firstperiod <- lm(rating ~ Rel * Mus, data = ssi15[ssi15$tx.time==1,])
table4app$firstperiod$se <- coeftest(table4app$firstperiod, vcov = vcovCluster(table4app$firstperiod, cluster = ssi15[ssi15$tx.time==1,]$pid))[,2]

table4app$secondperiod <- lm(rating ~ Rel * Mus, data = ssi15[ssi15$tx.time==2,])
table4app$secondperiod$se <- coeftest(table4app$secondperiod, vcov = vcovCluster(table4app$secondperiod, cluster = ssi15[ssi15$tx.time==2,]$pid))[,2]

stargazer(table4app,
          se = lapply(table4app, function(x) x$se),
          omit = c("^as.factor"),
          covariate.labels = c("Religious Immigrant","No Label Immigrant", "Muslim Immigrant", "Religious * Muslim","No Label * Muslim","Constant"),
          add.lines = list(
            c("Period Dummy","Yes","N/A","N/A")),
          keep.stat = "n",
          digits = 3
          ,type = "text"
)

### Table 5: Religious Catholic Respondents, 1st and 2nd time periods (SSI 2015) ----

relcath <- filter(ssi15, religion =='Catholic' & religious=='Religious')

table5app <- list()
table5app$fullsam <- lm(rating ~ Rel * Mus + as.factor(tx.time), data = relcath)
table5app$fullsam$se <- coeftest(table5app$fullsam , vcov = vcovCluster(table5app$fullsam, cluster = relcath$pid))[,2]

table5app$firstperiod <- lm(rating ~ Rel * Mus, data = relcath[relcath$tx.time==1,])
table5app$firstperiod$se <- coeftest(table5app$firstperiod, vcov = vcovCluster(table5app$firstperiod, cluster = relcath[relcath$tx.time==1,]$pid))[,2]

table5app$secondperiod <- lm(rating ~ Rel * Mus, data = relcath[relcath$tx.time==2,])
table5app$secondperiod$se <- coeftest(table5app$secondperiod, vcov = vcovCluster(table5app$secondperiod, cluster = relcath[relcath$tx.time==2,]$pid))[,2]

stargazer(table5app,
          se = lapply(table5app, function(x) x$se),
          omit = c("^as.factor"),
          covariate.labels = c("Religious Immigrant","No Label Immigrant", "Muslim Immigrant", "Religious * Muslim","No Label * Muslim","Constant"),
          add.lines = list(
            c("Period Dummy","Yes","N/A","N/A")),
          keep.stat = "n",
          digits = 3
          ,type = "text"
)

### Table 6: Religious Catholic Respondents, 1st and 2nd time periods (without NoLabel) (SSI 2015) ----

relcath_nl <- filter(ssi15, religion == 'Catholic' & religious == 'Religious' & Rel != "Nolab")

table6app <- list()
table6app$fullsam <- lm(rating ~ Rel * Mus + as.factor(tx.time), data = relcath_nl)
table6app$fullsam$se <- coeftest(table6app$fullsam, vcov = vcovCluster(table6app$fullsam, cluster = relcath_nl$pid))[,2]

table6app$firstperiod <- lm(rating ~ Rel * Mus, data = relcath_nl[relcath_nl$tx.time==1,])
table6app$firstperiod$se <- coeftest(table6app$firstperiod, vcov = vcovCluster(table6app$firstperiod, cluster = relcath_nl[relcath_nl$tx.time==1,]$pid))[,2]

table6app$secondperiod <- lm(rating ~ Rel * Mus, data = relcath_nl[relcath_nl$tx.time==2,])
table6app$secondperiod$se <- coeftest(table6app$secondperiod, vcov = vcovCluster(table6app$secondperiod, cluster = relcath_nl[relcath_nl$tx.time==2,]$pid))[,2]

stargazer(table6app,
          se = lapply(table6app, function(x) x$se),
          omit = c("^as.factor"),
          covariate.labels = c("Religious Immigrant", "Muslim Immigrant", "Religious * Muslim","Constant"),
          add.lines = list(
            c("Period Dummy","Yes","N/A","N/A")),
          keep.stat = "n",
          digits = 3
          ,type = "text"
)
















