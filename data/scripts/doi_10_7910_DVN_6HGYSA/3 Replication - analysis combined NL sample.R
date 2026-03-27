########### This file analyzes experimental data from the Netherlands (November 2019 and May 2020) on support for democracy for supporters of parties in government and in opposition
########### and replicates the analysis in Mazepus and Toshkov (2021), Comparative Political Studies
########### Part 1 of 1. Last update: 1 September 2021
# ### Libraries and functions --------------------------------------------------
library(car)
library(dplyr)
library(sjPlot)
# ### Read data --------------------------------------------------
df1<-read.csv('./data - survey/Student+project_Mazepus_November+18,+2019_07.17.csv') # Load November 2019 data
df2<-read.csv('./data - survey/Student+project_Pilot+2_Mazepus_May+16,+2020_11.08.csv') # Load May 2020 data

edf1.names<-df1[c(1,2),] # questions and labels
df2.names<-df2[c(1,2),] # questions and labels

df1<-df1[-c(1:25),] #filter column names and test responses
df1<-df1[df1$consent=='I want to participate in this survey',] #remove respondents who did not give consent
df2<-df2[-c(1:12),] #filter column names and test responses
df2<-df2[df2$consent=='I want to participate in this survey',] #remove respondents who did not give consent

# ### Recode variables --------------------------------------------------
# Experimental condition
df1$condition<-ifelse(df1$condition=='support', 'support', 'oppose')
df2$condition<-ifelse(df2$condition=='support',  'support', ifelse(df2$condition=='opposition', 'oppose','neutral'))

# Outcome variables
df1$gov.support1<-as.numeric(substr(as.character(df1$Q1_1), 1,1))
df1$gov.support2<-as.numeric(substr(as.character(df1$Q2_1), 1,1))
df1$gov.support<-rowSums(cbind(df1$gov.support1,df1$gov.support2), na.rm=T)

df1$gov.trust1<-as.numeric(substr(as.character(df1$Q1_2), 1,1))
df1$gov.trust2<-as.numeric(substr(as.character(df1$Q2_2), 1,1))
df1$gov.trust<-rowSums(cbind(df1$gov.trust1,df1$gov.trust2), na.rm=T)

df1$justified1<-as.numeric(substr(as.character(df1$Q1_3), 1,1))
df1$justified2<-as.numeric(substr(as.character(df1$Q2_3), 1,1))
df1$justified<-rowSums(cbind(df1$justified1,df1$justified2), na.rm=T)

df1$accepted1<-as.numeric(substr(as.character(df1$Q1_4), 1,1))
df1$accepted2<-as.numeric(substr(as.character(df1$Q2_4), 1,1))
df1$accepted<-rowSums(cbind(df1$accepted1,df1$accepted2), na.rm=T)

# May survey
df2$gov.support1<-as.numeric(substr(as.character(df2$Q1_1), 1,1))
df2$gov.support2<-as.numeric(substr(as.character(df2$Q2_1), 1,1))
df2$gov.support3<-as.numeric(substr(as.character(df2$Q3_1), 1,1))
df2$gov.support<-rowSums(cbind(df2$gov.support1,df2$gov.support2,df2$gov.support3), na.rm=T)

df2$gov.trust1<-as.numeric(substr(as.character(df2$Q1_2), 1,1))
df2$gov.trust2<-as.numeric(substr(as.character(df2$Q2_2), 1,1))
df2$gov.trust3<-as.numeric(substr(as.character(df2$Q3_2), 1,1))
df2$gov.trust<-rowSums(cbind(df2$gov.trust1,df2$gov.trust2,df2$gov.trust3), na.rm=T)

df2$justified1<-as.numeric(substr(as.character(df2$Q1_3), 1,1))
df2$justified2<-as.numeric(substr(as.character(df2$Q2_3), 1,1))
df2$justified3<-as.numeric(substr(as.character(df2$Q3_3), 1,1))
df2$justified<-rowSums(cbind(df2$justified1,df2$justified2,df2$justified3), na.rm=T)

df2$accepted1<-as.numeric(substr(as.character(df2$Q1_4), 1,1))
df2$accepted2<-as.numeric(substr(as.character(df2$Q2_4), 1,1))
df2$accepted3<-as.numeric(substr(as.character(df2$Q3_4), 1,1))
df2$accepted<-rowSums(cbind(df2$accepted1,df2$accepted2,df2$accepted3), na.rm=T)

df2$vote1<-as.numeric(substr(as.character(df2$Q1A), 1,1))
df2$vote2<-as.numeric(substr(as.character(df2$Q2A), 1,1))
df2$vote3<-as.numeric(substr(as.character(df2$Q3A), 1,1))
df2$vote<-rowSums(cbind(df2$vote1,df2$vote2,df2$vote3), na.rm=T)

# Demographics
df1$age<-as.numeric(as.character(df1$Q7))
df1$sex<-ifelse(df1$Q8=='Male', 'male', 'female')

df2$age<-as.numeric(as.character(df2$Q7))
df2$sex<-ifelse(df2$Q8=='Male', 'male', 'female')

# Political orientations
df1$conservative<-as.numeric(as.character(car::recode(df1$Q3, " 'Conservative (6)'= 6;'Extremely liberal (1)'= 1;'Liberal (2)'= 2;'Moderate, middle of the road (4)'= 4; 'Slightly conservative (5)'= 5; 'Slightly liberal (3)'= 3")))
df2$conservative<-as.numeric(as.character(car::recode(df2$Q3, " 'Conservative (6)'= 6;'Extremely liberal (1)'= 1;'Liberal (2)'= 2;'Moderate, middle of the road (4)'= 4; 'Slightly conservative (5)'= 5; 'Slightly liberal (3)'= 3")))

df1$lr<-as.numeric(substr(as.character(df1$Q4), 1,1))
df2$lr<-as.numeric(substr(as.character(df2$Q4), 1,1))

df1$conservative.extreme<-abs(4-df1$conservative) #extremity: distance from middle on the lib/cons scale
df1$lr.extreme<-abs(5-df1$lr) #extremity: distance from middle on the LR scale
df2$conservative.extreme<-abs(4-df2$conservative) #extremity: distance from middle on the lib/cons scale
df2$lr.extreme<-abs(5-df2$lr) #extremity: distance from middle on the LR scale

# Political attitudes and evaluations
df1$dem.country<-as.numeric(substr(as.character(df1$Q6), 1,1))
df2$dem.country<-as.numeric(substr(as.character(df2$Q6), 1,1))

df1$courts.same<-as.numeric(substr(as.character(df1$Q5_1), 1,1))
df1$elections.imp<-as.numeric(substr(as.character(df1$Q5_2), 1,1))
df1$oppose.free<-as.numeric(substr(as.character(df1$Q5_3), 1,1))
df1$media.free<-as.numeric(substr(as.character(df1$Q5_4), 1,1))
df1$courts.stop<-as.numeric(substr(as.character(df1$Q5_5), 1,1))

df2$courts.same<-as.numeric(substr(as.character(df2$Q5_1), 1,1))
df2$elections.imp<-as.numeric(substr(as.character(df2$Q5_2), 1,1))
df2$oppose.free<-7 - as.numeric(substr(as.character(df2$Q5_3), 1,1))
df2$judges.imp<- as.numeric(substr(as.character(df2$Q5_4), 1,1))
df2$media.free<-as.numeric(substr(as.character(df2$Q5_5), 1,1))
df2$courts.stop<-7 - as.numeric(substr(as.character(df2$Q5_6), 1,1))


# Comprehension and manipulations checks
df1$which.reform<-ifelse(df1$m2=='The reform of the appointment of judges', 1, 0)
df1$which.party<-ifelse((df1$m1=='The party you oppose' & df1$condition=='oppose') |
                    (df1$m1=='The party you support' & df1$condition=='support'), 1, 0)

df2$which.reform<-ifelse(df2$m2=='The reform of the appointment of judges', 1, 0)
df2$which.party<-ifelse((df2$m1=='The party you oppose' & df2$condition=='oppose') |
                    (df2$m1=='The party you support' & df2$condition=='support') | 
                    df2$m1=='The information about which party won was not specified in the text' & df2$condition=='neutral', 1, 0)
df1 <- df1 [, 39:65]
df2 <- df2 [, 47:82]

df <- bind_rows(df2, df1)

# Save file
write.csv(df, './data - output/exp_survey_nl_combined.csv')
save (df, file = './data - output/exp_survey_nl_combined.RData')

load (file = './data - output/exp_survey_nl_combined.RData')

# ### Table of descriptive stats -------------------------------------------------------------
df$dv.index <- (df$gov.support + df$gov.trust + df$justified + df$accepted)/4
df.s<-df[df$which.reform==1 & df$which.party==1,]

dtf.table <- sapply(df.s[,c('gov.support','gov.trust','justified', 'accepted')], each(min, mean, median, max, sd))
dtf.table <- xtable(t(dtf.table), caption = 'Table of descriptive statistics')
colnames(dtf.table)<-c('Minimum', 'Mean', 'Median', 'Maximum', 'Standard deviation')
rownames(dtf.table)<-c("Government support", 'Government trust', 'Reforms justified', 'Reforms acceptable')
print(dtf.table, type='html', file='./tables/descr_stats_nl.html')

# ### Cross-correlation table -------------------------------------------------------------
table.cor <- round(cor(df.s[,c('gov.support','gov.trust','justified', 'accepted')]),2)
table.cor <- xtable(table.cor, caption = 'Table of bivariate correlations')
rownames(table.cor)<-c("1. Government support", '2. Government trust', '3. Reforms justified', '4. Reforms acceptable')
colnames(table.cor)<-c('1', '2', '3', '4')
print(table.cor, type='html', file='./tables/corr_table_nl.html')

# ### Statistical models --------------------------------------------------
# Note: age has 19 missings (all female!)
summary(m1<-lm(gov.support~condition + sex, data=df))
summary(m2<-lm(gov.trust~condition+sex, data=df))
summary(m3<-lm(justified~condition+sex, data=df))
summary(m4<-lm(accepted~condition+sex, data=df))
summary(m5<-lm(dv.index~ condition+sex, data=df))


# ### Export the table and make the coefficient plot --------------------------
tab_model(m1, m2, m3, m4, m5, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/lm_reg_set1.html')

coef1a<-m1$coefficients['conditionoppose']
coef1b<-m1$coefficients['conditionsupport']
coef2a<-m2$coefficients['conditionoppose']
coef2b<-m2$coefficients['conditionsupport']
coef3a<-m3$coefficients['conditionoppose']
coef3b<-m3$coefficients['conditionsupport']
coef4a<-m4$coefficients['conditionoppose']
coef4b<-m4$coefficients['conditionsupport']

se1a<-summary(m1)$coef['conditionoppose',2]
se1b<-summary(m1)$coef['conditionsupport',2]
se2a<-summary(m2)$coef['conditionoppose',2]
se2b<-summary(m2)$coef['conditionsupport',2]
se3a<-summary(m3)$coef['conditionoppose',2]
se3b<-summary(m3)$coef['conditionsupport',2]
se4a<-summary(m4)$coef['conditionoppose',2]
se4b<-summary(m4)$coef['conditionsupport',2]

# ### Figure A1. Marginal effects ----------------------------------------------------------------
s =3
offset = 0.01

png ('./figures/coef_plot_1.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,3,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(2,8,2,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    bg='white' # background color
)


plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c(-1.6,1), ylim = c(1, 11))

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
      at=c(1,4,7,10), # where to put labels  
      labels= c('government support','government trust',"reforms acceptable",'reforms justified'), # text of labels 
      las=1 # orientation of the labels
)

abline (v=seq(-2,2,0.5), col='lightgrey', lty =2, lwd=1.5*s)
abline (v=0, col='red', lty =2, lwd=1.5*s)
abline (h=c(2,6,9), col='lightgrey', lty =2, lwd=1.5*s)

segments (y0=c(1,4,7,10), y1=c(1,4,7,10), x0  = c(coef1a-2*se1a, coef2a-2*se2a, coef3a-2*se3a, coef4a-2*se4a), 
          x1= c(coef1a+2*se1a, coef2a+2*se2a,coef3a+2*se3a, coef4a+2*se4a), col='blue', lwd=2.5*s) 
points (y=c(1,4,7,10), x = c(coef1a, coef2a, coef3a,coef4a), pch=18, col='blue', cex=1.8)

segments (y0=c(2,5,8,11), y1=c(2,5,8,11), x0  = c(coef1b-2*se1b, coef2b-2*se2b, coef3b-2*se3b,coef4b-2*se4b), 
          x1= c(coef1b+2*se1b,coef2b+2*se2b, coef3b+2*se3b, coef4b+2*se4b), col='red', lwd=2.5*s) 
points (y=c(2,5,8,11), x = c(coef1b, coef2b, coef3b, coef4b), pch=18, col='red', cex=1.8)

#title
mtext(expression(bold('Coefficients and 95% confidence intervals for the effects of')),
      side = 3, line = 2.25, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col='black', cex = 1.8*s)

mtext(expression('having supported' * phantom (' or ') * phantom ('having opposed') * phantom (' the party in government on: ')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='red', cex = 1.8*s)

mtext(expression(phantom('having supported') * phantom (' or ') * 'having opposed' * phantom (' the party in government on: ')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='blue', cex = 1.8*s)

mtext(expression(phantom('having supported') * ' or ' * phantom('having opposed') * ' the party in government on: '),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='black', cex = 1.8*s)
dev.off()
# ### Models of attitudes -----------------------------------------------------
summary(lm(elections.imp~condition+sex+age, data=df))
summary(lm(courts.stop~condition+sex+age, data=df))
summary(lm(oppose.free~condition+sex+age, data=df))
summary(lm(media.free~condition+sex+age, data=df))
# ### Run the models on subsample ---------------------------------------------
# on subsample
summary(m1<-lm(gov.support~condition + sex + age, data=df.s))
summary(m2<-lm(gov.trust~condition+sex + age, data=df.s))
summary(m3<-lm(justified~condition+sex + age, data=df.s))
summary(m4<-lm(accepted~condition+sex + age, data=df.s))
summary(m5<-lm(dv.index~condition+sex + age, data=df.s))

tab_model(m1, m2, m3, m4, m5, show.ci=FALSE, show.se=TRUE, digits=2, digits.p=2, digits.re=2, 
          string.se='SE', string.p='p', string.est = 'Coef.' , file = './tables/lm_reg_set1a.html')

coef1a<-m1$coefficients['conditionoppose']
coef1b<-m1$coefficients['conditionsupport']
coef2a<-m2$coefficients['conditionoppose']
coef2b<-m2$coefficients['conditionsupport']
coef3a<-m3$coefficients['conditionoppose']
coef3b<-m3$coefficients['conditionsupport']
coef4a<-m4$coefficients['conditionoppose']
coef4b<-m4$coefficients['conditionsupport']

se1a<-summary(m1)$coef['conditionoppose',2]
se1b<-summary(m1)$coef['conditionsupport',2]
se2a<-summary(m2)$coef['conditionoppose',2]
se2b<-summary(m2)$coef['conditionsupport',2]
se3a<-summary(m3)$coef['conditionoppose',2]
se3b<-summary(m3)$coef['conditionsupport',2]
se4a<-summary(m4)$coef['conditionoppose',2]
se4b<-summary(m4)$coef['conditionsupport',2]

# ### Figure A1a. Marginal effects ----------------------------------------------------------------
s =3
offset = 0.01

png ('./figures/coef_plot_1a.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,3,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(2,8,2,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    bg='white' # background color
)


plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c(-2.3,0.7), ylim = c(1, 11))

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
      at=c(1,4,7,10), # where to put labels  
      labels= c('government support','government trust',"reforms acceptable",'reforms justified'), # text of labels 
      las=1 # orientation of the labels
)

abline (v=seq(-2,2,0.5), col='lightgrey', lty =2, lwd=1.5*s)
abline (v=0, col='red', lty =2, lwd=1.5*s)
abline (h=c(2,6,9), col='lightgrey', lty =2, lwd=1.5*s)

segments (y0=c(1,4,7,10), y1=c(1,4,7,10), x0  = c(coef1a-2*se1a, coef2a-2*se2a, coef3a-2*se3a, coef4a-2*se4a), 
          x1= c(coef1a+2*se1a, coef2a+2*se2a,coef3a+2*se3a, coef4a+2*se4a), col='blue', lwd=2.5*s) 
points (y=c(1,4,7,10), x = c(coef1a, coef2a, coef3a,coef4a), pch=18, col='blue', cex=1.8)

segments (y0=c(2,5,8,11), y1=c(2,5,8,11), x0  = c(coef1b-2*se1b, coef2b-2*se2b, coef3b-2*se3b,coef4b-2*se4b), 
          x1= c(coef1b+2*se1b,coef2b+2*se2b, coef3b+2*se3b, coef4b+2*se4b), col='red', lwd=2.5*s) 
points (y=c(2,5,8,11), x = c(coef1b, coef2b, coef3b, coef4b), pch=18, col='red', cex=1.8)

#title
mtext(expression(bold('Coefficients and 95% confidence intervals for the effects of')),
      side = 3, line = 2.25, adj = 0, padj = 1, outer = TRUE, at = offset,
      font=1, col='black', cex = 1.8*s)

mtext(expression('having supported' * phantom (' or ') * phantom ('having opposed') * phantom (' the party in government on: ')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='red', cex = 1.8*s)

mtext(expression(phantom('having supported') * phantom (' or ') * 'having opposed' * phantom (' the party in government on: ')),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='blue', cex = 1.8*s)

mtext(expression(phantom('having supported') * ' or ' * phantom('having opposed') * ' the party in government on: '),
      side = 3, line = 0.75, adj = 0, padj = 1, outer = TRUE, at = offset, font=1, col='black', cex = 1.8*s)
dev.off()


# ### Write session info ------------------------------------------------------
writeLines(capture.output(sessionInfo()), "./sessionInfo.txt")
# ### The end of Part 1 of 1 ------------------------------------------------------