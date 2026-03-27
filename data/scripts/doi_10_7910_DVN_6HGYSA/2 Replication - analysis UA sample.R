########### This file analyzes experimental data from Ukraine on support for democracy for supporters of parties in government and in opposition
########### and replicates the analysis in Mazepus and Toshkov (2021), Comparative Political Studies
########### The dataset is part of a larger unrelated data collection project. The unrelated variables have been removed.
########### Part 1 of 1. Last update: 1 September 2021

# ### Libraries and functions --------------------------------------------------
library(haven)
library(tidyverse)
library(plyr)
library(xtable)
library(sjPlot)
library(ggplot2)
library(PupillometryR)
library(cowplot)
# ### Read data --------------------------------------------------
## This block extracts and saves the relevant variables from the original datafile (unavailable)
#d <- read_sav('./data/COVID19(weight).sav')
#d <- d %>%
#  select (starts_with('Situation'), starts_with('Q1.'), 'Q1A', starts_with('Q7.'), 
#          'political_ideology', 'Labels', 'P3.7.1', 'P3.7.2', 'Q8', 'Q9', starts_with('Q10.'),
#          'sex', 'age', 'urban', 'Ladder', 'employ_status', 'Settlement', 'M1', 'M2', 'M3', 'weight_$')
## Save file
#write.csv(d, './data - survey/replicate_exp_survey_ua.csv')
#save (d, file = './data - survey/replicate_exp_survey_ua.RData')

# Load the datafile with the relevant variables
load (file = './data - survey/replicate_exp_survey_ua.RData')

# ### Recode variables --------------------------------------------------

# Experimental condition
d$condition <- NA
for (i in 1:nrow(d)){
  if (d$Situation1[i]==1)
    d$condition[i] <- '1.neutral.stable'
  else 
    if(d$Situation2[i]==1)
      d$condition[i] <- '2.neutral.up'
    else 
      if(d$Situation3[i]==1)
        d$condition[i] <- '3.support.stable'
      else 
        if(d$Situation4[i]==1)
          d$condition[i] <- '4.support.up'
        else 
          if(d$Situation5[i]==1)
            d$condition[i] <- '5.oppose.stable'
          else 
            d$condition[i] <- '6.oppose.up'
        }
table(d$condition)

# Experimental contrasts
d$condition.economy <- ifelse (( d$condition == '2.neutral.up'| d$condition == '4.support.up'|d$condition == '6.oppose.up'), 'up', 'stable')
d$condition.elections <- ifelse (( d$condition == '1.neutral.stable'| d$condition == '2.neutral.up'), 'neutral', ifelse(( d$condition == '3.support.stable'| d$condition == '4.support.up'), 'support','oppose'))

table(d$condition, d$condition.economy)
table(d$condition, d$condition.elections)

# Outcome variables
d$gov.support <- as.numeric(as.character(d$Q1.1)) # support for the government
d$gov.trust <- as.numeric(as.character(d$Q1.2)) # trust in the government
d$justified <- as.numeric(as.character(d$Q1.3)) # find the reforms justified
d$accepted <- as.numeric(as.character(d$Q1.4)) # find the reforms acceptable
d$vote <- as.numeric(as.character(d$Q1A)) # would vote for the party
d$dv.index <- (d$gov.support + d$gov.trust + d$justified + d$accepted + d$vote)/5

# Political orientations
d$lr <- as.numeric(as.character(d$political_ideology)) # left-right
d$conservative <- as.numeric(as.character(d$Labels)) # conservative
d$soc.liberal <- as.numeric(as.character(d$P3.7.2)) # support for gays
d$interventionist <- as.numeric(as.character(d$P3.7.1)) # support for gov. intervention in the economy

# Political attitudes and evaluations
d$courts.same <- as.numeric(as.character(d$Q7.1)) # courts should treat people the same
d$elections.imp <- as.numeric(as.character(d$Q7.2)) # importance of free elections
d$oppose.free <- 8 - as.numeric(as.character(d$Q7.3)) # importance of opposition parties to critisize (wording inverted)
d$judges.imp <- as.numeric(as.character(d$Q7.4)) # importance of independence of judges
d$media.free <- as.numeric(as.character(d$Q7.5)) # importance of media free to criticize gov
d$courts.stop <- 8 - as.numeric(as.character(d$Q7.6)) # importance of courts to stop (wording inverted)
d$dem.best <- as.numeric(as.character(d$Q7.7)) # democracy is best

d$dem.imp <- as.numeric(as.character(d$Q9)) # importance of democracy
d$dem.country <- as.numeric(as.character(d$Q8)) # UA democratic

# Trust in institutions
d$trust.president <- as.numeric(as.character(d$Q10.1)) # trust in the president of UA
d$trust.gov <- as.numeric(as.character(d$Q10.2)) # trust in the government of UA
d$trust.courts <- as.numeric(as.character(d$Q10.3)) # trust in the courts in UA

# Demographics
d$Sex <- ifelse (d$sex ==1, 'male', 'female')
d$Age <- as.numeric(as.character(d$age)) # age
d$edu <- ifelse (d$urban == 5, 'higher,', ifelse ((d$urban == 4 ), 'higher.incomplete', 'middle'))
d$soc.status <- as.numeric(as.character(d$Ladder)) # social status via Ladder question 
d$empl <- ifelse ((d$employ_status==1 |d$employ_status==2 | d$employ_status==9 ), 'working', ifelse (d$employ_status==4, 'student', 'not.working'))
d$place <- ifelse (d$Settlement == 1, 'small,', ifelse (d$Settlement == 2 , 'middle', 'big'))

# Manipulation and comprehension tests
d$income.raise <-  as.numeric(as.character(d$M3))
d$which.reform <-  as.numeric(as.character(d$M2)) 
d$which.party <- ifelse (d$M1==1, 'suppport',ifelse(d$M1==2,'oppose', 'neutral'))  

# Weights
d$weights <- d$`weight_$`

# ### Descriptive stats -------------------------------------------------------
# ### T-test of comprehension test ----------------------------------------
d$z = ifelse (d$condition.economy=='stable', 0 ,1)
t.test(x=d$z, y=d$income.raise)

t.test(income.raise ~ condition.economy, d)
# ### First table of descriptive stats -------------------------------------------------------------
dtf.table <- sapply(d[,c('gov.support','gov.trust','justified', 'accepted', 'vote', 'dv.index')], each(min, mean, weighted.mean, median, max, sd))
dtf.table[3,]<- sapply(d[,c('gov.support','gov.trust','justified', 'accepted', 'vote', 'dv.index')], weighted.mean, w=d$weights)

dtf.table <- xtable(t(dtf.table), caption = 'Table of descriptive statistics (outcome variables)')
colnames(dtf.table)<-c('Minimum', 'Mean', 'Weighted mean','Median', 'Maximum', 'Standard deviation')
rownames(dtf.table)<-c("Government support", 'Government trust', 'Reforms justified', 'Reforms acceptable', 'Vote for government', 'Composite index')
print(dtf.table, type='html', file='./tables/descr_stats_ua.html')

d %>%
  group_by(condition.elections) %>%
  summarise (gov.support = mean (gov.support, na.rm=T),
             justified = mean (justified, na.rm=T),
             accepted = mean (accepted, na.rm=T)) %>%
  ungroup()
             

# ### Second table of descriptive stats -------------------------------------------------------------
dtf.table2 <- sapply(d[,c('courts.same','oppose.free','judges.imp','courts.stop','media.free', 'dem.best', 'lr', 'trust.courts')], each(min, mean, weighted.mean, median, max, sd))
dtf.table2[3,]<-  sapply(d[,c('courts.same','oppose.free','judges.imp','courts.stop','media.free', 'dem.best', 'lr', 'trust.courts')], weighted.mean, w=d$weights)

dtf.table2 <- xtable(t(dtf.table2), caption = 'Table of descriptive statistics (sample description)')
colnames(dtf.table2)<-c('Minimum', 'Mean', 'Weighted mean','Median', 'Maximum', 'Standard deviation')
rownames(dtf.table2)<-c("Courts should treat people the same", 'Opposition free to critisize the government', 
                        'Judges should be independent from elected politicians','It is important that the courts are able to stop the government acting beyond its authority',
                        'Media free to critisize the government', 
                        'Democracy is the best form of government', 'Left-right self-placement', 'Trust in courts in the country')
print(dtf.table2, type='html', file='./tables/descr_stats_2_ua.html')

# ### Demographics descriptive stats -------------------------------------------------------------
# sex
round(prop.table(table(d$Sex))*100,0)
round(sum(d$weights[d$Sex=='female'])/577*100)

# age
round(mean(d$Age),2)
round(mean(d$Age*d$weights),2)
sd(d$Age)

# ### Cross-correlation table -------------------------------------------------------------
table.cor <- round(cor(d[,c('gov.support','gov.trust','justified', 'accepted', 'vote')], use='complete'),2)
table.cor <- xtable(table.cor, caption = 'Table of bivariate correlations')
rownames(table.cor)<-c("1. Government support", '2. Government trust', '3. Reforms justified', '4. Reforms acceptable', '5. Vote for government')
colnames(table.cor)<-c('1', '2', '3', '4', '5')
print(table.cor, type='html', file='./tables/corr_table_ua.html')


# ### Statistical models w/o covariates, weighted --------------------------------------------------
summary(m1<-lm(gov.support~condition.elections + condition.economy, data=d, weights=weights))
summary(m2<-lm(gov.trust~condition.elections + condition.economy, data=d, weights=weights))
summary(m3<-lm(justified~condition.elections + condition.economy, data=d, weights=weights))
summary(m4<-lm(accepted~condition.elections + condition.economy, data=d, weights=weights))
summary(m5<-lm(vote~condition.elections + condition.economy, data=d, weights=weights))
summary(m6<-lm(dv.index~condition.elections + condition.economy, data=d, weights=weights))

# ### Save the table and prepare data for coef plot ----------------------------------
tab_model(m1, m2, m3, m4, m5, m6, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/lm_reg_set2.html')

coef1a<-m1$coefficients['condition.electionsoppose']
coef1b<-m1$coefficients['condition.electionssupport']
coef1c<-m1$coefficients['condition.economyup']

coef2a<-m2$coefficients['condition.electionsoppose']
coef2b<-m2$coefficients['condition.electionssupport']
coef2c<-m3$coefficients['condition.economyup']

coef3a<-m3$coefficients['condition.electionsoppose']
coef3b<-m3$coefficients['condition.electionssupport']
coef3c<-m3$coefficients['condition.economyup']

coef4a<-m4$coefficients['condition.electionsoppose']
coef4b<-m4$coefficients['condition.electionssupport']
coef4c<-m4$coefficients['condition.economyup']

coef5a<-m5$coefficients['condition.electionsoppose']
coef5b<-m5$coefficients['condition.electionssupport']
coef5c<-m5$coefficients['condition.economyup']

se1a<-summary(m1)$coef['condition.electionsoppose',2]
se1b<-summary(m1)$coef['condition.electionssupport',2]
se1c<-summary(m1)$coef['condition.economyup',2]

se2a<-summary(m2)$coef['condition.electionsoppose',2]
se2b<-summary(m2)$coef['condition.electionssupport',2]
se2c<-summary(m2)$coef['condition.economyup',2]

se3a<-summary(m3)$coef['condition.electionsoppose',2]
se3b<-summary(m3)$coef['condition.electionssupport',2]
se3c<-summary(m3)$coef['condition.economyup',2]

se4a<-summary(m4)$coef['condition.electionsoppose',2]
se4b<-summary(m4)$coef['condition.electionssupport',2]
se4c<-summary(m4)$coef['condition.economyup',2]

se5a<-summary(m5)$coef['condition.electionsoppose',2]
se5b<-summary(m5)$coef['condition.electionssupport',2]
se5c<-summary(m5)$coef['condition.economyup',2]
# ### Figure 3. Marginal effects ----------------------------------------------------------------
s =3
offset = 0.01

png ('./figures/replicate_coef_plot_2_bw.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(2,8,2,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    bg='white' # background color
)


plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c(-1.5,1), ylim = c(1, 19))

axis (1, 
      line = -0.5, # position
      tck = -0.01,
      lwd = 0,
      col = 'white', 
      col.axis = 'black', # colors of the actual labels
      cex.axis = 0.85,
      font=2, # font type (bold)
      at=seq(-2,2,0.50),
      labels=seq(-2,2,0.50),
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
      at=c(2,6,10,14,18), # where to put labels  
      labels= c('government support','government trust',"reforms acceptable",'reforms justified', 'vote for party'), # text of labels 
      las=1 # orientation of the labels
)

abline (v=seq(-2,2,0.5), col='lightgrey', lty =2, lwd=1.5*s)
abline (v=0, col='red', lty =2, lwd=1.5*s)
abline (h=c(4,8,12,16), col='lightgrey', lty =2, lwd=1.5*s)

segments (y0=c(1,5,9,13,17), y1=c(1,5,9,13,17), x0  = c(coef1a-1.96*se1a, coef2a-1.96*se2a, coef3a-1.96*se3a, coef4a-1.96*se4a, coef5a-1.96*se5a), 
          x1= c(coef1a+1.96*se1a, coef2a+1.96*se2a,coef3a+1.96*se3a, coef4a+1.96*se4a, coef5a+1.96*se5a), col='blue', lwd=3*s, lty=5) 
points (y=c(1,5,9,13,17), x = c(coef1a, coef2a, coef3a,coef4a,coef5a), pch=16, col='blue', cex=1.8)

segments (y0=c(2,6,10,14,18), y1=c(2,6,10,14,18), x0  = c(coef1b-1.96*se1b, coef2b-1.96*se2b, coef3b-1.96*se3b,coef4b-1.96*se4b,coef5b-1.96*se5b), 
          x1= c(coef1b+1.96*se1b,coef2b+1.96*se2b, coef3b+1.96*se3b, coef4b+1.96*se4b, coef5b+1.96*se5b), col='red', lwd=3*s, lty=1) 
points (y=c(2,6,10,14,18), x = c(coef1b, coef2b, coef3b, coef4b, coef5b), pch=17, col='red', cex=1.8)

segments (y0=c(3,7,11,15,19), y1=c(3,7,11,15,19), x0  = c(coef1c-1.96*se1c, coef2c-1.96*se2c, coef3c-1.96*se3c,coef4c-1.96*se4c,coef5c-1.96*se5c), 
          x1= c(coef1c+1.96*se1c,coef2c+1.96*se2c, coef3c+1.96*se3c, coef4c+1.96*se4c, coef5c+1.96*se5c), col='black', lwd=3*s, lty=3) 
points (y=c(3,7,11,15,19), x = c(coef1c, coef2c, coef3c, coef4c, coef5c), pch=18, col='black', cex=2)

text(c('economy improved vs. stable','voted winner vs. neutral','voted loser vs. neutral'),
     x=c(coef1c, coef1b, coef1a),
     y = c(2.6, 1.6, 0.6),
     col= c('black','red','blue'),
     cex=0.75)

text(c('economy improved vs. stable','voted winner vs. neutral','voted loser vs. neutral'),
     x=c(coef2c, coef2b, coef2a),
     y = c(6.6, 5.6, 4.6),
     col= c('black','red','blue'),
     cex=0.75)

text(c('economy improved vs. stable','voted winner vs. neutral','voted loser vs. neutral'),
     x=c(coef3c, coef3b, coef3a),
     y = c(10.6, 9.6, 8.6),
     col= c('black','red','blue'),
     cex=0.75)

text(c('economy improved vs. stable','voted winner vs. neutral','voted loser vs. neutral'),
     x=c(coef4c, coef4b, coef4a),
     y = c(14.6, 13.6, 12.6),
     col= c('black','red','blue'),
     cex=0.75)

text(c('economy improved vs. stable','voted winner vs. neutral','voted loser vs. neutral'),
     x=c(coef5c, coef5b, coef5a),
     y = c(18.6, 17.6, 16.6),
     col= c('black','red','blue'),
     cex=0.75)

dev.off()

# ### Statistical models w/o covaraites --------------------------------------------------
summary(m1a<-lm(gov.support~condition.elections + condition.economy, data=d))
summary(m2a<-lm(gov.trust~condition.elections + condition.economy, data=d))
summary(m3a<-lm(justified~condition.elections + condition.economy, data=d))
summary(m4a<-lm(accepted~condition.elections + condition.economy, data=d))
summary(m5a<-lm(vote~condition.elections + condition.economy, data=d))
summary(m6a<-lm(dv.index~condition.elections + condition.economy, data=d))

tab_model(m1a, m2a, m3a, m4a, m5a, m6a, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/lm_reg_set2a.html')


# ### Statistical models without weights with covariates --------------------------------------------------
summary(m1b<-lm(gov.support~condition.elections + condition.economy + Age + Sex + lr + dem.best + trust.courts, data=d))
summary(m2b<-lm(gov.trust~condition.elections + condition.economy + Age + Sex + lr + dem.best + trust.courts, data=d))
summary(m3b<-lm(justified~condition.elections + condition.economy + Age + Sex + lr + dem.best + trust.courts, data=d))
summary(m4b<-lm(accepted~condition.elections + condition.economy + Age + Sex + lr + dem.best + trust.courts, data=d))
summary(m5b<-lm(vote~condition.elections + condition.economy + Age + Sex + lr + dem.best + trust.courts, data=d))
summary(m6b<-lm(dv.index~condition.elections + condition.economy + Age + Sex + lr + dem.best + trust.courts, data=d))

tab_model(m1b, m2b, m3b, m4b, m5b, m6b, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/lm_reg_set2b.html')

# ### The end of Part 1 of 1 ------------------------------------------------------

