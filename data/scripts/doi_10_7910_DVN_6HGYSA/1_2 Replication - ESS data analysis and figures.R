########### This file analyzes ESS data on support for democracy for supporters of parties in government and in opposition
########### and replicates the analysis in Mazepus and Toshkov (2021), Comparative Political Studies
########### Part 2 of 2. Last update: 1 September 2021

### Libraries and functions ----------------------------------------------------------------
library(lme4)
library(lmerTest)
library(arm)
library(sjPlot)
library(plyr)
library(multcomp)
### Load the data (if needed). The data is named [dat6.s] ----------------------------------------------------------------
load('./data - output/replication_dat6_subset_new.RData')
### Results from multilevel models I: Effect of election winner status. Table A2, A2a-A2c ----------------------------------------------------------------

# the models
summary(rem1<-lmer(imp.opposition.free ~ age + sex + lr + lr.ex + edu + empl + status + (1 + status | country), data=dat6.s))
summary(rem2<-lmer(imp.media.free ~ age + sex + lr + lr.ex + edu + empl + status + (1 + status | country), data=dat6.s))
summary(rem3<-lmer(imp.courts.stop ~ age + sex + lr + lr.ex + edu + empl + status + (1 + status | country), data=dat6.s))
summary(rem4<-lmer(imp.fair.elections ~ age + sex + lr + lr.ex + edu + empl + status + (1 + status | country), data=dat6.s))
summary(rem5<-lmer(imp.dem.checks ~ age + sex + lr + lr.ex + edu + empl + status + (1 + status | country), data=dat6.s))

# collect the results in a table (Table A2)
tab_model(rem1, rem2, rem3, rem4, rem5, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/replicate_new_lmer_reg_set1.html')

# all contrasts (Tables A2a-A2c)
table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), 
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)
  
}

# Three times for the three DVs
summary(con<-glht(rem1, mcp(status="Tukey")))
con.out <- round(table_glht(con),2)
write.csv (con.out, file='./tables/replicate_contrasts_model_impoppositionfree.csv')

summary(con<-glht(rem2, mcp(status="Tukey")))
con.out <- round(table_glht(con),2)
write.csv (con.out, file='./tables/replicate_contrasts_model_impmediafree.csv')

summary(con<-glht(rem3, mcp(status="Tukey")))
con.out <- round(table_glht(con),2)
write.csv (con.out, file='./tables/replicate_contrasts_model_impcourtsstop.csv')

# Extract standard errors for the figure
se1 = fixef(rem1)['statusvote.closeto.winner'] + c(-2,2)* se.fixef(rem1)['statusvote.closeto.winner']
se2 = fixef(rem2)['statusvote.closeto.winner'] + c(-2,2)* se.fixef(rem2)['statusvote.closeto.winner']
se3 = fixef(rem3)['statusvote.closeto.winner'] + c(-2,2)* se.fixef(rem3)['statusvote.closeto.winner']
se4 = fixef(rem4)['statusvote.closeto.winner'] + c(-2,2)* se.fixef(rem4)['statusvote.closeto.winner']
se5 = fixef(rem5)['statusvote.closeto.winner'] + c(-2,2)* se.fixef(rem5)['statusvote.closeto.winner']

se1n = fixef(rem1)['statusno vote'] + c(-2,2)* se.fixef(rem1)['statusno vote']
se2n = fixef(rem2)['statusno vote'] + c(-2,2)* se.fixef(rem2)['statusno vote']
se3n = fixef(rem3)['statusno vote'] + c(-2,2)* se.fixef(rem3)['statusno vote']
se4n = fixef(rem4)['statusno vote'] + c(-2,2)* se.fixef(rem4)['statusno vote']
se5n = fixef(rem5)['statusno vote'] + c(-2,2)* se.fixef(rem5)['statusno vote']

### Figure 0. Marginal effects from the models (figure not included) ----------------------------------------------------------------
s =3
offset = 0.01

png ('./figures/replicate_coef_plot_1.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,3,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(2,8,2,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    bg='white' # background color
)


plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c(-0.40,0.12), ylim = c(1, 5))

axis (1, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 0.85,
      font=2, # font type (bold)
      at=round(seq(-0.40,0.200,0.100),1),
      labels=round(seq(-0.40,0.200,0.100),1),
      las=1 # orientation of the labels
)

axis (2, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', # the actual axis (line) 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 0.85, 
      font=2, # font type (bold)
      at=1:5, # where to put labels  
      labels= c('opposition free\nto criticize','media free to\ncriticize',"courts can limit\ngovernment action",'importance of\nfree elections','Index: importance of\ndemocratic checks'), # text of labels 
      las=1 # orientation of the labels
)

abline (v=seq(-0.4,0.2,0.10), col='lightgrey', lty =2, lwd=1.5*s)
abline (v=0, col='red', lty =2, lwd=1.5*s)

segments (y0=1:5, y1=1:5, x0  = c(se1[1],se2[1],se3[1],se4[1],se5[1]), x1= c(se1[2],se2[2],se3[2],se4[2],se5[2]), col='black', lwd=2.5*s) 
points (y=1:5, x = c(fixef(rem1)['statusvote.closeto.winner'],fixef(rem2)['statusvote.closeto.winner'],fixef(rem3)['statusvote.closeto.winner'],
                     fixef(rem4)['statusvote.closeto.winner'], fixef(rem5)['statusvote.closeto.winner']), pch=18, col='black', cex=1.8)

#title
mtext(expression(bold('Coefficients and 95% confidence intervals for the effect of being a supporter of')),
      side = 3, line = 2.25, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col='black', cex = 1.7*s)

mtext(expression(bold('a winning party compared to being a supporter of a losing party on attitudes towards:')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col='black', cex = 1.7*s)

dev.off()



### Figure 2. Country-specific random effects for strong supporters --------------------------
c.coefs<-ranef(rem2)$country[,5] + fixef(rem2)['statusvote.closeto.winner']
se.coefs<-se.ranef(rem2)$country[,5]

t.coefs<-data.frame(cbind(c.coefs, se.coefs))
t.coefs<-t.coefs[order(t.coefs[,1]), ]

y.min = min(c.coefs-2*se.coefs)
y.max = max(c.coefs+2*se.coefs)

newdems <- c('PL', 'BG', 'CZ',  'SI' ,'CY' ,'HU', 'EE' , 'AL', 'SK' ,'XK' ,'UA' , 'RU')

# Figure 2
s =3
offset = 0.01

png ('./figures/replicate_coef_re_plot_new_bw.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(2,4,2,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    bg='white' # background color
)


plot(NULL, yaxt = 'n', xaxt = 'n', ylim = c(min(y.min, 0), max(0, y.max)), xlim = c(1, 26))

axis (2, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 0.85,
      font=2, # font type (bold)
      at=round(seq (-1.2, 0.6, 0.3),1),
      labels=round(seq (-1.2, 0.6, 0.3),1),
      las=1 # orientation of the labels
)

axis (1, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', # the actual axis (line) 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 0.85, 
      font=2, # font type (bold)
      at=1:26, # where to put labels  
      labels= rownames(t.coefs),
      las=1 # orientation of the labels
)

abline (h=round(seq (-1.2, 0.6, 0.3),1), col='lightgrey', lty =2, lwd=1.2*s)
#abline (h=0, col='lightgrey', lty =2, lwd=1.5*s)
abline (h=fixef(rem2)['statusvote.closeto.winner'], col='darkgrey', lty = 1, lwd=1.5*s)

segments (x0=1:26, x1=1:26, y0  = t.coefs$c.coefs-2* t.coefs$se.coefs, y1=  t.coefs$c.coefs+2* t.coefs$se.coefs, lwd=2.5*s,
          col=ifelse(rownames(t.coefs)%in% newdems==FALSE , 'blue','red'), lty=ifelse(rownames(t.coefs)%in% newdems==FALSE , 1, 4)) 
points (x=1:26, y =  t.coefs$c.coefs, col=ifelse(rownames(t.coefs)%in% newdems==FALSE, 'blue','red'), 
        pch=ifelse(rownames(t.coefs)%in% newdems==FALSE, 18, 16),
        cex=1.8)

dev.off()

### Results from multilevel models II: Effect of satisfaction with the economy. Table A3 ----------------------------------------------------------------
summary(rem1b<-lmer(imp.opposition.free ~ age + sex + lr + lr.ex + edu + empl + status + gdp.pc.change + unempl + sat.economy + (1 + sat.economy | country), data=dat6.s))
summary(rem2b<-lmer(imp.media.free ~ age + sex + lr + lr.ex + edu + empl + status + gdp.pc.change +  unempl + sat.economy + (1 + sat.economy | country), data=dat6.s))
summary(rem3b<-lmer(imp.courts.stop ~ age + sex + lr + lr.ex + edu + empl + status + gdp.pc.change +  unempl + sat.economy + (1 + sat.economy | country), data=dat6.s))
summary(rem4b<-lmer(imp.fair.elections ~ age + sex + lr + lr.ex + edu + empl + status + gdp.pc.change +  unempl + sat.economy + (1 + sat.economy | country), data=dat6.s))
summary(rem5b<-lmer(imp.dem.checks ~ age + sex + lr + lr.ex + edu + empl + status + gdp.pc.change +  unempl + sat.economy + (1 + sat.economy | country), data=dat6.s))

tab_model(rem1b, rem2b, rem3b, rem4b, rem5b, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/replicate_new2_lmer_reg_set3.html')

se1b = fixef(rem1b)['sat.economy'] + c(-2,2)* se.fixef(rem1b)['sat.economy']
se2b = fixef(rem2b)['sat.economy'] + c(-2,2)* se.fixef(rem2b)['sat.economy']
se3b = fixef(rem3b)['sat.economy'] + c(-2,2)* se.fixef(rem3b)['sat.economy']
se4b = fixef(rem4b)['sat.economy'] + c(-2,2)* se.fixef(rem4b)['sat.economy']
se5b = fixef(rem5b)['sat.economy'] + c(-2,2)* se.fixef(rem5b)['sat.economy']
### Figure 1. All marginal effects in one plot ------------------------------
s =3
offset = 0.01

png ('./figures/replicate_coef_plot_all_bw.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(2,8,2,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    bg='white' # background color
)


plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c(-0.52,0.12), ylim = c(1, 15))

axis (1, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 0.85,
      font=2, # font type (bold)
      at=seq(-0.5,0.1,0.10),
      labels=seq(-0.5,0.1,0.10),
      las=1 # orientation of the labels
)

axis (2, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', # the actual axis (line) 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 0.85, 
      font=2, # font type (bold)
      at=c(2, 6, 10, 14), # where to put labels  
      labels= c('opposition free\nto criticize','media free to\ncriticize',"courts can limit\ngovernment action",'importance of\nfree elections'), # text of labels 
      las=1 # orientation of the labels
)

abline (v=seq(-0.5,0.1,0.10), col='lightgrey', lty =2, lwd=1.2*s)
abline (v=0, col='darkgrey', lty =1, lwd=1.2*s)
abline (h=c(4,8,12), col='lightgrey', lty =2, lwd=1.2*s)

segments (y0=c(1,5,9,13), y1=c(1,5,9,13), x0  = c(se1b[1],se2b[1],se3b[1],se4b[1]), x1= c(se1b[2],se2b[2],se3b[2],se4b[2]), col='black', lty =3, lwd=2.5*s) 
points (y=c(1,5,9,13), x = c(fixef(rem1b)['sat.economy'],fixef(rem2b)['sat.economy'],fixef(rem3b)['sat.economy'],fixef(rem4b)['sat.economy']), 
        pch=18, col='black', cex=2)

segments (y0=c(2, 6, 10, 14), y1=c(2, 6, 10, 14), x0  = c(se1[1],se2[1],se3[1],se4[1]), x1= c(se1[2],se2[2],se3[2],se4[2]), col='blue', lwd=2.5*s, lty=5) 
points (y=c(2, 6, 10, 14), x = c(fixef(rem1)['statusvote.closeto.winner'],fixef(rem2)['statusvote.closeto.winner'],fixef(rem3)['statusvote.closeto.winner'],fixef(rem4)['statusvote.closeto.winner']), 
        pch=16, col='blue', cex=1.8)

segments (y0=c(3,7,11,15), y1=c(3,7,11,15), x0  = c(se1n[1],se2n[1],se3n[1],se4n[1]), x1= c(se1n[2],se2n[2],se3n[2],se4n[2]), col='red', lty = 1, lwd=2.5*s) 
points (y=c(3,7,11,15), x = c(fixef(rem1)['statusno vote'],fixef(rem2)['statusno vote'],fixef(rem3)['statusno vote'],fixef(rem4)['statusno vote']), 
        pch=17, col='red', cex=1.5)

text(c('no vote vs. vote loser','vote winner vs. vote loser','satisfied with econ. vs. not satisfied'),
     x=c(fixef(rem4)['statusno vote'],fixef(rem4)['statusvote.closeto.winner'],fixef(rem4b)['sat.economy']),
     y = c(14.7, 13.7, 12.7),
     col= c('red','blue','black'),
     cex=0.75)

text(c('no vote vs. vote loser','vote winner vs. vote loser','satisfied with econ. vs. not satisfied'),
     x=c(fixef(rem3)['statusno vote'],fixef(rem3)['statusvote.closeto.winner'],fixef(rem3b)['sat.economy']),
     y = c(10.7, 9.7, 8.7),
     col= c('red','blue','black'),
     cex=0.75)

text(c('no vote vs. vote loser','vote winner vs. vote loser','satisfied with econ. vs. not satisfied'),
     x=c(fixef(rem2)['statusno vote'],fixef(rem2)['statusvote.closeto.winner'],fixef(rem2b)['sat.economy']),
     y = c(6.7, 5.7, 4.7),
     col= c('red','blue','black'),
     cex=0.75)

text(c('no vote vs. vote loser','vote winner vs. vote loser','satisfied with econ. vs. not satisfied'),
     x=c(fixef(rem1)['statusno vote'],fixef(rem1)['statusvote.closeto.winner'],fixef(rem1b)['sat.economy']),
     y = c(2.7, 1.7, 0.7),
     col= c('red','blue','black'),
     cex=0.75)


dev.off()



### Results from multilevel models III: Interactions with country level predictors (three sets of models). Tables A4-A6 --------------------------------------------
summary(rem1C<-lmer(imp.opposition.free ~ age + sex + lr + lr.ex + edu + empl + status2*newdem + (1 + status2 | country), data=dat6.s))
summary(rem2C<-lmer(imp.media.free ~ age + sex + lr + lr.ex + edu + empl + status2*newdem + (1 + status2 | country), data=dat6.s))
summary(rem3C<-lmer(imp.courts.stop ~ age + sex + lr + lr.ex + edu + empl + status2*newdem + (1 + status2 | country), data=dat6.s))
summary(rem4C<-lmer(imp.fair.elections ~ age + sex + lr + lr.ex + edu + empl + status2*newdem + (1 + status2 | country), data=dat6.s))
summary(rem5C<-lmer(imp.dem.checks ~ age + sex + lr + lr.ex + edu + empl + status2*newdem + (1 + status2 | country), data=dat6.s))

tab_model(rem1C, rem2C, rem3C, rem4C, rem5C, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/replicate_new_lmer_reg_set4.html')

summary(rem6C<-lmer(imp.opposition.free ~ age + sex + lr + lr.ex + edu + empl + status2*polarization + (1 + status2 | country), data=dat6.s))
summary(rem7C<-lmer(imp.media.free ~ age + sex + lr + lr.ex + edu + empl + status2*polarization + (1 + status2 | country), data=dat6.s))
summary(rem8C<-lmer(imp.courts.stop ~ age + sex + lr + lr.ex + edu + empl + status2*polarization + (1 + status2 | country), data=dat6.s))
summary(rem9C<-lmer(imp.fair.elections ~ age + sex + lr + lr.ex + edu + empl + status2*polarization + (1 + status2 | country), data=dat6.s))
summary(rem10C<-lmer(imp.dem.checks ~ age + sex + lr + lr.ex + edu + empl + status2*polarization + (1 + status2 | country), data=dat6.s))

tab_model(rem6C, rem7C, rem8C, rem9C, rem10C, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/replicate_new_lmer_reg_set5.html')

summary(rem11C<-lmer(imp.opposition.free ~ age + sex + lr + lr.ex + edu + empl + status2*time + (1 + status2 | country), data=dat6.s))
summary(rem12C<-lmer(imp.media.free ~ age + sex + lr + lr.ex + edu + empl + status2*time + (1 + status2 | country), data=dat6.s))
summary(rem13C<-lmer(imp.courts.stop ~ age + sex + lr + lr.ex + edu + empl + status2*time + (1 + status2 | country), data=dat6.s))
summary(rem14C<-lmer(imp.fair.elections ~ age + sex + lr + lr.ex + edu + empl + status2*time + (1 + status2 | country), data=dat6.s))
summary(rem15C<-lmer(imp.dem.checks ~ age + sex + lr + lr.ex + edu + empl + status2*time + (1 + status2 | country), data=dat6.s))

tab_model(rem11C, rem12C, rem13C, rem14C, rem15C, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/replicate_new_lmer_reg_set6.html')
### The end of Part 2 of 2 ------------------------------------------------------

