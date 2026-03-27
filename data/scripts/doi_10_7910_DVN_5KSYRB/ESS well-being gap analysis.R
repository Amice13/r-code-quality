### This file replicates the ESS data analyses reported in Toshkov and Mazepus 'Does the Election Winner–Loser Gap Extend to Subjective Health and Well-Being?' (2022) in Political Studies Review
### All original data files are available from the ESS website after registration: https://www.europeansocialsurvey.org/data/
### The file used below makes use of ESS data Round 6 and Round 9.
### This file starts with data that is already pre-processed. For the preparation of the data file, see Mazepus and Toshkov (2022) in Comparative Political Studies  https://doi.org/10.7910/DVN/6HGYSA

### 0. Libraries and functions -------------------------------------------------
library(tidyverse)
library(lme4)
library(arm)
library(car)
library(tableHTML)
library(sjPlot)
library(scales)
library(xtable)

blue4 = rgb(7, 47, 95, max=255)
blue3 = rgb(18, 97, 160, max=255)
blue2 = rgb(56, 149, 211, max=255)
blue1 = rgb(88, 204, 237, max=255)

make.color<-function (x){
  if (x<0.001)
    blue4
  else 
    if (x<0.01)
      blue3
  else
    if (x<0.05)
      blue2
  else
    if (x<0.10)
      blue1
  else
    'grey'
}

re.scale <- function (x){rescale(abs(x), from = c(0.0,0.6), to = c(0.5,2.2))}
### 1. Data preparation and table of descriptives  --------------------------------------------------------
load(file = './data/dat.RData')
#dat<-read.csv(file = './data/dat.csv'))

vars <-c ("feel.pessimistic", "life.gets.better", "feel.hopefull",
          "feel.unsafe","feel.unhealthy", "feel.happy", "life.satisfaction")

### Table of descriptives
dat <- data.frame(dat)
descr_table<-data.frame(matrix(0, nrow=length(vars), ncol=6))
descr_table[,1] <- vars
colnames(descr_table) <- c('Variable', 'Min','Median','Mean', 'Max', 'StDv')
for (i in 1: length(vars)){
  descr_table[i,2]<-round(min(dat[, vars[i]], na.rm=TRUE),1)
  descr_table[i,3]<-round(median(dat[, vars[i]], na.rm=TRUE),1)
  descr_table[i,4]<-round(mean(dat[, vars[i]], na.rm=TRUE),1)
  descr_table[i,5]<-round(max(dat[, vars[i]], na.rm=TRUE),1)
  descr_table[i,6]<-round(sd(dat[, vars[i]], na.rm=TRUE),1)
}

descr_table_health<-tableHTML(descr_table, rownames=FALSE, border = 0.5) %>% add_theme('scientific')
write_tableHTML(descr_table_health, file='./tables/descr_table_health.html')

allvars<-c(vars, 'voted','party.close','age','sex','edu','empl','newdem', 'party.gov.all', 'year', 'country')

dat_out<-dat[, allvars]
names(dat_out)[names(dat_out) == 'party.gov.all'] <- 'winner.status'

dat<-dat %>% 
  mutate_at(vars, scale)
dat <- data.frame(dat)

temp <- dat %>% dplyr::select (vars) %>% na.omit
round(cor(temp[,], temp[,]),2)

### Different subsets
dat.voters <- dat [!is.na(dat$voted) & (dat$voted == 1), ] # those who voted (66k from 48k)
dat.partisans <- dat [!is.na(dat$party.close) & (dat$party.close ==1 | dat$party.close == 2), ] # those very close to any party (30k)
dat$partisans <- ifelse ((dat$party.close ==1 | dat$party.close == 2), 1, 0)

round(prop.table(table(dat$voted, dat$party.close),1)*100,0)

### 2a. Baseline regression models and Table 2 -------------------------------
sm1<-lmer(feel.pessimistic ~ age + sex + edu + empl +  newdem +  party.gov.all + (1 + party.gov.all | country), data=dat)
sm2<-update(sm1, life.gets.better ~ .)
sm3<-update(sm1, feel.hopefull ~ .)
sm4<-update(sm1, feel.unsafe ~ .)
sm5<-update(sm1, feel.unhealthy ~ .)
sm6<-update(sm1, feel.happy ~ .)
sm7<-update(sm1, life.satisfaction ~ .)

tab_model(sm1, sm2, sm3, sm4, sm5, sm6, sm7, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, file = './tables/baseline_models_health2.html')

rem1<-lmer(feel.pessimistic ~ age + edu + empl + sex + lr + lr.ex + newdem + party.gov.all:newdem + (1 + party.gov.all | country), data=dat)
rem2<-update(rem1, life.gets.better ~ .)
rem3<-update(rem1, feel.hopefull ~ .)
rem4<-update(rem1, feel.unsafe ~ .)
rem5<-update(rem1, feel.unhealthy ~ .)
rem6<-update(rem1, feel.happy ~ .)
rem7<-update(rem1, life.satisfaction ~ .)

tab_model(rem1, rem2, rem3, rem4, rem5, rem6, rem7, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, file = './tables/interactive_models_health2.html')
### 2b. Additional models with proportionality -------------------------------
dat$disproportionality <- scale(dat$disproportionality)
prem1<-lmer(feel.pessimistic ~ age + edu + empl + sex + lr + lr.ex + newdem + party.gov.all + disproportionality + party.gov.all*disproportionality + (1 + party.gov.all | country), data=dat)
prem2<-update(prem1, life.gets.better ~ .)
prem3<-update(prem1, feel.hopefull ~ .)
prem4<-update(prem1, feel.unsafe ~ .)
prem5<-update(prem1, feel.unhealthy ~ .)
prem6<-update(prem1, feel.happy ~ .)
prem7<-update(prem1, life.satisfaction ~ .)

tab_model(prem1, prem2, prem3, prem4, prem5, prem6, prem7, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, file = './tables/disporp_interactive_models_health2.html')

### 2c. Additional models with time since election -------------------------------
dat$time <- scale(dat$time)
trem1<-lmer(feel.pessimistic ~ age + edu + empl + sex + lr + lr.ex + newdem + time + party.gov.all + party.gov.all*time + (1 + party.gov.all | country), data=dat)
trem2<-update(trem1, life.gets.better ~ .)
trem3<-update(trem1, feel.hopefull ~ .)
trem4<-update(trem1, feel.unsafe ~ .)
trem5<-update(trem1, feel.unhealthy ~ .)
trem6<-update(trem1, feel.happy ~ .)
trem7<-update(trem1, life.satisfaction ~ .)

tab_model(trem1, trem2, trem3, trem4, trem5, trem6, trem7, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, file = './tables/time_interactive_models_health2.html')

### 3a. Get the parameters for plotting from baseline models-------------------------------
out.table1.base<-data.frame(matrix(0, nrow=length(vars), ncol=7))
out.table1.base[,1] <- vars
colnames(out.table1.base) <- c('Variable','Coef.Status','SE.Status', 'Pval.Status', 'Coef.Status2','SE.Status2', 'Pval.Status2')

for (i in 1: length(vars)){
  rem2<-lmer(dat[, vars[i]]~ age + sex + edu + empl +  newdem + party.gov.all + (1 + party.gov.all | country), data=dat)
  rem1<-lmer(dat[, vars[i]]~ age + sex + edu + empl +  newdem + lr + lr.ex + party.gov.all + (1 + party.gov.all | country), data=dat)
  out.table1.base[i,2] <-round(fixef(rem1)['party.gov.all'],2)
  out.table1.base[i,3] <-round(2*se.fixef(rem1)['party.gov.all'], 2)
  out.table1.base[i,4] <-last(round(Anova(rem1, type='III')$'Pr(>Chisq)',3))
  out.table1.base[i,5] <-round(fixef(rem2)['party.gov.all'],2)
  out.table1.base[i,6] <-round(2*se.fixef(rem2)['party.gov.all'], 2)
  out.table1.base[i,7] <-last(round(Anova(rem2, type='III')$'Pr(>Chisq)',3))
}
out.table1.base

# voters
out.table2.base<-data.frame(matrix(0, nrow=length(vars), ncol=7))
out.table2.base[,1] <- vars
colnames(out.table2.base) <- c('Variable','Coef.Status','SE.Status', 'Pval.Status', 'Coef.Status2','SE.Status2', 'Pval.Status2')

for (i in 1: length(vars)){
  rem2<-lmer(dat.voters[, vars[i]]~ age + sex + edu + empl +  newdem + party.gov.all + (1 + party.gov.all | country), data=dat.voters)
  rem1<-lmer(dat.voters[, vars[i]]~ age + sex + edu + empl +  newdem + lr + lr.ex + party.gov.all + (1 + party.gov.all | country), data=dat.voters)
  out.table2.base[i,2] <-round(fixef(rem1)['party.gov.all'],2)
  out.table2.base[i,3] <-round(2*se.fixef(rem1)['party.gov.all'], 2)
  out.table2.base[i,4] <-last(round(Anova(rem1, type='III')$'Pr(>Chisq)',3))
  out.table2.base[i,5] <-round(fixef(rem2)['party.gov.all'],2)
  out.table2.base[i,6] <-round(2*se.fixef(rem2)['party.gov.all'], 2)
  out.table2.base[i,7] <-last(round(Anova(rem2, type='III')$'Pr(>Chisq)',3))
}
out.table2.base

# strong party attachment
out.table3.base<-data.frame(matrix(0, nrow=length(vars), ncol=7))
out.table3.base[,1] <- vars
colnames(out.table3.base) <- c('Variable','Coef.Status','SE.Status', 'Pval.Status', 'Coef.Status2','SE.Status2', 'Pval.Status2')

for (i in 1: length(vars)){
  rem2<-lmer(dat.partisans[, vars[i]]~ age + sex + edu + empl +  newdem + party.gov.all + (1 + party.gov.all | country), data=dat.partisans)
  rem1<-lmer(dat.partisans[, vars[i]]~ age + sex + edu + empl +  newdem + lr + lr.ex + party.gov.all + (1 + party.gov.all | country), data=dat.partisans)
  out.table3.base[i,2] <-round(fixef(rem1)['party.gov.all'],2)
  out.table3.base[i,3] <-round(2*se.fixef(rem1)['party.gov.all'], 2)
  out.table3.base[i,4] <-last(round(Anova(rem1, type='III')$'Pr(>Chisq)',3))
  out.table3.base[i,5] <-round(fixef(rem2)['party.gov.all'],2)
  out.table3.base[i,6] <-round(2*se.fixef(rem2)['party.gov.all'], 2)
  out.table3.base[i,7] <-last(round(Anova(rem2, type='III')$'Pr(>Chisq)',3))
}
out.table3.base

### 3b. Figure 2. Baseline model marginal effects in one plot for citizens/voters/partisans ------------------------------
s =3
offset = 0.01

png ('./figures/F2_marginal_effects_health_cvp.png', width=1280*s, height=1280*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,3,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(2,8,2,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    bg='white' # background color
)

xmin=min(c(out.table1.base[,2]-out.table1.base[,3],out.table2.base[,2]-out.table2.base[,3],out.table3.base[,2]-out.table3.base[,3]))
xmax=max(c(out.table1.base[,2]+out.table1.base[,3],out.table2.base[,2]+out.table2.base[,3],out.table3.base[,2]+out.table3.base[,3]))
nvars = length(vars)

plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c(xmin, xmax), ylim = c(1-0.5, nvars*4-0.5))

axis (1, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 1,
      font=2, # font type (bold)
      at=seq(-1,1,0.10),
      labels=seq(-1,1,0.10),
      las=1 # orientation of the labels
)

axis (2, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', # the actual axis (line) 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 1., 
      font=2, # font type (bold)
      at=seq(2,nvars*4,4), # where to put labels  
      labels= vars, # text of labels 
      las=1 # orientation of the labels
)

abline (v=seq(-1,1,0.10), col='lightgrey', lty =2, lwd=1*s)
abline (v=0, col='red', lty =2, lwd=1*s)
abline (h=seq(0,nvars*4,4), col='lightgrey', lty =2, lwd=1*s)

for (i in 1:nvars){
  segments (y0 = seq(1,nvars*4,4)[i], y1=seq(1,nvars*4,4)[i], x0 = out.table1.base[i,2] - out.table1.base[i,3], x1 = out.table1.base[i,2] + out.table1.base[i,3], col=blue4, lwd=2*s)
  points (y = seq(1,nvars*4,4)[i], x= out.table1.base[i,2], pch=18, col=blue4, cex=1.5)
  
  segments (y0 = seq(2,nvars*4,4)[i], y1=seq(2,nvars*4,4)[i], x0 = out.table2.base[i,2] - out.table2.base[i,3], x1 = out.table2.base[i,2] + out.table2.base[i,3], col=blue2, lwd=2*s)
  points (y = seq(2,nvars*4,4)[i], x= out.table2.base[i,2], pch=18, col=blue2, cex=1.5)
  
  segments (y0 = seq(3,nvars*4,4)[i], y1=seq(3,nvars*4,4)[i], x0 = out.table3.base[i,2] - out.table3.base[i,3], x1 = out.table3.base[i,2] + out.table3.base[i,3], col=blue1, lwd=2*s)
  points (y = seq(3,nvars*4,4)[i], x= out.table3.base[i,2], pch=18, col=blue1, cex=1.5)
  
}

for (i in 1:nvars){
  text ('citizens', y = seq(1,nvars*4,4)[i]-0.4, x= out.table1.base[i,2], col=blue4, cex=0.85)
  text ('voters', y = seq(2,nvars*4,4)[i]-0.4, x= out.table2.base[i,2], col=blue2, cex=0.85)
  text ('partisans', y = seq(3,nvars*4,4)[i]-0.4, x= out.table3.base[i,2], col = blue1, cex=0.85)
}

#title
mtext(expression(bold('Coefficients and 95% confidence intervals for the effects of voting for a winning party')),
      side = 3, line = 2.25, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col='black', cex = 1.8*s)

mtext(expression('among' * phantom (' citizens, voters, ') * ' and ' * phantom(' partisans')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='black', cex = 2*s)

mtext(expression(phantom ('among') * ' citizens, ' * phantom ('voters, and partisans')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=2, col=blue4, cex = 2*s)

mtext(expression(phantom ('among citizens, ') * ' voters, ' * phantom (' and partisans')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=2, col=blue2, cex = 2*s)

mtext(expression(phantom ('among citizens, voters, and ') * ' partisans' ),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=2, col=blue1, cex = 2*s)

dev.off()



### 2d. Baseline models with old/new as moderator -------------------------------
# all respondents
out.table1<-data.frame(matrix(0, nrow=length(vars), ncol=5))
out.table1[,1] <- vars
colnames(out.table1) <- c('Variable','Coef.New','Coef.Old','SE.New','SE.Old')

for (i in 1: length(vars)){
  rem1<-lmer(dat[, vars[i]]~ age + sex + edu + empl +  lr + lr.ex + newdem + party.gov.all:newdem + (1 + party.gov.all | country), data=dat)
  out.table1[i,2] <-round(fixef(rem1)['newdemnew:party.gov.all'],2)
  out.table1[i,3] <-round(fixef(rem1)['newdemold:party.gov.all'],2)
  out.table1[i,4] <-round(2*se.fixef(rem1)['newdemnew:party.gov.all'], 2)
  out.table1[i,5] <-round(2*se.fixef(rem1)['newdemold:party.gov.all'], 2)
}
out.table1

# voters
out.table2<-data.frame(matrix(0, nrow=length(vars), ncol=5))
out.table2[,1] <- vars
colnames(out.table2) <- c('Variable','Coef.New','Coef.Old','SE.New','SE.Old')

for (i in 1: length(vars)){
  rem1<-lmer(dat.voters[, vars[i]]~ age + sex + edu + empl +  newdem + party.gov.all:newdem + (1 + party.gov.all | country), data=dat.voters)
  out.table2[i,2] <-round(fixef(rem1)['newdemnew:party.gov.all'],2)
  out.table2[i,3] <-round(fixef(rem1)['newdemold:party.gov.all'],2)
  out.table2[i,4] <-round(2*se.fixef(rem1)['newdemnew:party.gov.all'], 2)
  out.table2[i,5] <-round(2*se.fixef(rem1)['newdemold:party.gov.all'], 2)
}
out.table2

# strong party attachement
out.table3<-data.frame(matrix(0, nrow=length(vars), ncol=5))
out.table3[,1] <- vars
colnames(out.table3) <- c('Variable','Coef.New','Coef.Old','SE.New','SE.Old')

for (i in 1: length(vars)){
  rem1<-lmer(dat.partisans[, vars[i]]~ age + sex + edu + empl +  newdem + party.gov.all:newdem + (1 + party.gov.all | country), data=dat.partisans)
  out.table3[i,2] <-round(fixef(rem1)['newdemnew:party.gov.all'],2)
  out.table3[i,3] <-round(fixef(rem1)['newdemold:party.gov.all'],2)
  out.table3[i,4] <-round(2*se.fixef(rem1)['newdemnew:party.gov.all'], 2)
  out.table3[i,5] <-round(2*se.fixef(rem1)['newdemold:party.gov.all'], 2)
}
out.table3


### 4a. Figure 1. Health effects for partisans for old and new democracies -------
out.table <- out.table1

s =3
offset = 0.01

png ('./figures/F1_marginal_effects_health_citizens.png', width=1280*s, height=1280*s, res=96)
#height=905.5*

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,3,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(2,8,2,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    bg='white' # background color
)

xmin=min(c(out.table[,2]-out.table[,4]),out.table[,3]-out.table[,5])
xmax=max(c(out.table[,2]+out.table[,4]),out.table[,3]+out.table[,5])
nvars = length(vars)

plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c(xmin, xmax), ylim = c(1, nvars*3-1))

axis (1, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 1,
      font=2, # font type (bold)
      at=seq(-1,1,0.10),
      labels=seq(-1,1,0.10),
      las=1 # orientation of the labels
)

axis (2, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', # the actual axis (line) 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 1, 
      font=2, # font type (bold)
      at=seq(1.5,nvars*3-1+0.5,3), # where to put labels  
      labels= vars, # text of labels 
      las=1 # orientation of the labels
)

abline (v=seq(-1,1,0.10), col='lightgrey', lty =2, lwd=1*s)
abline (v=0, col='red', lty =2, lwd=1*s)
abline (h=seq(0,nvars*3,3), col='lightgrey', lty =2, lwd=1*s)

for (i in 1:nvars){
  segments (y0 = seq(1,nvars*3,3)[i], y1=seq(1,nvars*3,3)[i], x0 = out.table[i,2] - out.table[i,4], x1 = out.table[i,2] + out.table[i,4], col=blue4, lwd=2*s)
  points (y = seq(1,nvars*3,3)[i], x= out.table[i,2], pch=18, col=blue4, cex=1.5)
  segments (y0 = seq(2,nvars*3,3)[i], y1=seq(2,nvars*3,3)[i], x0 = out.table[i,3] - out.table[i,5], x1 = out.table[i,3] + out.table[i,5], col=blue1, lwd=2*s)
  points (y = seq(2,nvars*3,3)[i], x= out.table[i,3], pch=18, col=blue1, cex=1.5)
}

for (i in 1:nvars){
  text ('new dem-s', y = seq(1,nvars*3,3)[i]-0.4, x= out.table[i,2], col=blue4, cex=0.85)
  text ('old dem-s', y = seq(2,nvars*3,3)[i]-0.4, x= out.table[i,3], col=blue1, cex=0.85)
  
}

#title
mtext(expression(bold('Coefficients and 95% confidence intervals for the effects of voting for a winning party')),
      side = 3, line = 2.25, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col='black', cex = 1.8*s)

mtext(expression('for citizens in ' * phantom ('new') * ' and ' * phantom ('old ') * 'democracies: '),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='black', cex = 1.8*s)

mtext(expression(phantom('for citizens in ') * phantom ('new') * phantom(' and ') * 'old ' * phantom('democracies: ')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col=blue1, cex = 1.8*s)

mtext(expression(phantom('for citizens in ') *  'new' * phantom(' and ') * phantom ('old ') * phantom('democracies: ')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col=blue4, cex = 1.8*s)
dev.off()




### The end
sessionInfo()