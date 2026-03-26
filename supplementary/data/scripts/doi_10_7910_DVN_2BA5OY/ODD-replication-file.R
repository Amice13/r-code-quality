rm(list=ls())
library(tidyverse)
library(broom)
library(lubridate)
library(stargazer)
library(interflex)
library(survey)
library(mice)
options(scipen=999)


# Working directory should be the replication folder, wherever that is.
#setwd('C:/Users/jdt34/Box/ODDs - Political Behavior/replication-materials/')
setwd('C:/Users/arvind.k/Box/ODDs - Political Behavior/replication-materials/')

########## Manuscript ##########


########## Study 1 ##########


# Data
d1 = read.csv("survey-1-results-numeric.csv") %>%
  subset(Finished==1L) %>%
  mutate(ODDPrimed=as.integer(correction==0L & condition!='control'),
         ODDTreated=as.integer(correction==1L & condition!='control'),
         AllCandidates=as.integer(condition=='nonpart'),
         OneCandidate=as.integer(condition=='partisan'),
         ExpCondition='Control',
         Sex=plyr::mapvalues(x=sex, from=c(-99, 0, 1), to=c(NA,'Female','Male')),
         Race=plyr::mapvalues(x=race, from=c(1:5), to=c('White','Black','Latino','Asian','Other')),
         Race=factor(Race, levels=c('White','Black','Latino','Asian','Other')),
         Party='Independent',
         SenParty=NA,
         PartisanMatch=NA,
         Outpartisan=NA,
         Education=plyr::mapvalues(x=educ, from=c(1:6), 
                                   to=c('Some HS','HS diploma','Some college',"Bachelor's",'Some grad','Grad degree')),
         Education=factor(Education, 
                          levels=c('Some HS','HS diploma','Some college', "Bachelor's",'Some grad','Grad degree')),
         Income=plyr::mapvalues(x=inc, from=c(1:12), 
                                to=c('x<$10k','$10k<=x<$20k','$20k<=x<$30k','$30k<=x<$40k','$40k<=x<$50k',
                                     '$50k<=x<$60k','$60k<=x<$70k','$70k<=x<$80k','$80k<=x<$90k',
                                     '$90k<=x<$100k','$100k<=x<$150k','x>$150k')),
         Income=factor(Income, 
                       levels=c('x<$10k','$10k<=x<$20k','$20k<=x<$30k','$30k<=x<$40k','$40k<=x<$50k',
                                '$50k<=x<$60k','$60k<=x<$70k','$70k<=x<$80k','$80k<=x<$90k',
                                '$90k<=x<$100k','$100k<=x<$150k','x>$150k')),
         TimeEffort=NA,
         Efficacy=(eff.1+eff.2)/12+0.5,
         TrustCongress=(congress.does.best+elected.rep.interest+govt.run.by)/3,
         SouthernID=(south.id.1+south.id.2+south.id.3+Q123)/4)
d1$ExpCondition[d1$ODDPrimed==1L & d1$AllCandidates==1L] = 'All Candidates Treatment + ODD Prime'
d1$ExpCondition[d1$ODDPrimed==1L & d1$OneCandidate==1L] = 'Specific Candidate Treatment + ODD Prime'
d1$ExpCondition[d1$ODDTreated==1L & d1$AllCandidates==1L] = 'All Candidates Treatment + ODD Treated'
d1$ExpCondition[d1$ODDTreated==1L & d1$OneCandidate==1L] = 'Specific Candidate Treatment + ODD Treated'
d1$ExpCondition = as.factor(d1$ExpCondition) %>%
  relevel(ref='Control')
d1$ODDTreatedSpecificCandidate = as.integer(d1$ExpCondition=='Specific Candidate Treatment + ODD Treated')
d1$Party[d1$PartyID==-1 | d1$Leaner==-1] = 'Republican'
d1$Party[d1$PartyID==1 | d1$Leaner==1] = 'Democrat'
d1$SenParty[d1$cand %in% c('david','kelly')] = 'Republican'
d1$SenParty[d1$cand %in% c('jon','raphael')] = 'Democrat'
d1$PartisanMatch[d1$Party=='Republican' & d1$SenParty=='Republican'] = 'Copartisan Republican'
d1$PartisanMatch[d1$Party=='Democrat' & d1$SenParty=='Democrat'] = 'Copartisan Democrat'
d1$PartisanMatch[d1$Party=='Republican' & d1$SenParty=='Democrat'] = 'Out-partisan Republican'
d1$PartisanMatch[d1$Party=='Democrat' & d1$SenParty=='Republican'] = 'Out-partisan Democrat'
d1$Outpartisan[d1$PartisanMatch %in% c('Copartisan Republican','Copartisan Democrat')] = 0L
d1$Outpartisan[d1$PartisanMatch %in% c('Out-partisan Republican','Out-partisan Democrat')] = 1L
d1$TimeEffort[!is.na(d1$oss.work.for.ga_1)] = d1$oss.work.for.ga_1[!is.na(d1$oss.work.for.ga_1)]
d1$TimeEffort[!is.na(d1$warn.work.for.ga_1)] = d1$warn.work.for.ga_1[!is.na(d1$warn.work.for.ga_1)]
d1$TimeEffort[!is.na(d1$loeff.work.for.ga_1)] = d1$loeff.work.for.ga_1[!is.na(d1$loeff.work.for.ga_1)]
d1$TimeEffort[!is.na(d1$perd.work.for.ga_1)] = d1$perd.work.for.ga_1[!is.na(d1$perd.work.for.ga_1)]
d1$TimeEffort[!is.na(d1$sen.work.for.ga_1)] = d1$sen.work.for.ga_1[!is.na(d1$sen.work.for.ga_1)]
d1$TimeEffort[!is.na(d1$sen.work.for.ga.2_1)] = d1$sen.work.for.ga.2_1[!is.na(d1$sen.work.for.ga.2_1)]
d1$TimeEffort = d1$TimeEffort / 100


# Table 1
count(d1, ExpCondition)
count(d1, ODDPrimed)
count(d1, ODDTreated)
count(d1, AllCandidates)
count(d1, OneCandidate)


# Table 2
count(d1, Sex) %>%
  mutate(p=round(n/sum(n), 2))

count(d1, Race) %>%
  mutate(p=round(n/sum(n), 2))

count(d1, Party) %>%
  mutate(p=round(n/sum(n), 2))

count(d1, Education) %>%
  mutate(p=round(n/sum(n), 2))

count(d1, Income) %>%
  mutate(p=round(n/sum(n), 2),
         summed=cumsum(p),
         half=summed>0.5)


# Table 3
tab3col1 = lm(TimeEffort ~ ExpCondition, data=d1)
summary(tab3col1)
tidy(tab3col1)

tab3col2 = lm(Efficacy ~ ExpCondition, data=d1)
summary(tab3col2)
tidy(tab3col2)

tab3col3 = lm(TrustCongress ~ ExpCondition, data=d1)
summary(tab3col3)
tidy(tab3col3)

stargazer(tab3col1, tab3col2, tab3col3)


# Figure 1
f1 = interflex(estimator='binning', na.rm=T, Y='TimeEffort', X='SouthernID', D='ODDTreated',
               data=subset(d1, ExpCondition %in% c('Control','Specific Candidate Treatment + ODD Treated')))
p1 = plot(f1, theme.bw=T, ylim=c(-0.3, 0.3), xlab='Moderator: southern identity', 
          ylab='Marginal effect of Specific Candidate ODD Treatment on time and effort')
p1
ggsave(filename='figure-1-alt.pdf', plot=p1, width=9.75, height=6.75, units='in')

f1 = interflex(estimator='linear', na.rm=T, Y='TimeEffort', X='SouthernID', D='ODDTreated',
               data=subset(d1, ExpCondition %in% c('Control','Specific Candidate Treatment + ODD Treated')))
inter.test(f1, diff.values=c(0, 1))


########## Study 2 ##########


# Data
senate2022states = c('Alabama','Alaska','Arizona','Arkansas','California','Colorado',
                     'Connecticut','Florida','Georgia','Hawaii','Idaho',
                     'Illinois','Indiana','Iowa','Kansas','Kentucky',
                     'Louisiana','Maryland','Missouri','Nevada','New Hampshire',
                     'New York','North Carolina','North Dakota','Ohio',
                     'Oklahoma','Oregon','Pennsylvania','South Carolina',
                     'South Dakota','Utah','Vermont','Washington','Wisconsin')

d2 = read.csv("survey-2-results-wordy.csv") %>%
  subset(State %in% senate2022states & Finished=='TRUE') %>%
  mutate(Sex=plyr::mapvalues(x=gender, from=c(1:2), to=c('Male','Female')),
         Race=plyr::mapvalues(x=ethnicity, from=c(1:16), 
                              to=c('White','Black','Other', rep('Asian', 11),'Other','Other')),
         Race=ifelse(hispanic!=1, 'Latino', Race),
         Race=factor(Race, levels=c('White','Black','Latino','Asian','Other')),
         Party='Independent',
         Education=plyr::mapvalues(x=as.numeric(education), from=c(-3105, 1:8),
                                   to=c(NA,'Some HS','HS diploma','Some Vocational Training',"Some College",
                                         "Associates", "Bachelor's","Master's",'PhD')),
         Education=factor(Education, levels=c('Some HS','HS diploma','Some Vocational Training',"Some College",
                                              "Associates", "Bachelor's","Master's",'PhD')),
         Income=plyr::mapvalues(x=as.numeric(hhi), 
                                from=c(-3105,1:24), 
                                to=c(NA, '<$15,000','$15,000-$19,999','$20,000-$24,999','$25,000-$29,999',
                                     '$30,000-$34,999','$35,000-$39,999','$40,000-$44,999','$45,000-$49,999',
                                     '$50,000-$54,999','$55,000-$59,999','$60,000-$64,999','$65,000-$69,999',
                                     '$70,000-$74,999','$75,000-$79,999','$80,000-$84,999','$85,000-$89,999',
                                     '$90,000-$94,999','$95,000-$99,999','$100,000-$124,999','$125,000-$149,999',
                                     '$150,000-$174,999','$175,000-$199,999','$200,000-$249,999','\u2265$250,000')),
         Income=factor(Income, levels=c('<$15,000','$15,000-$19,999','$20,000-$24,999','$25,000-$29,999',
                                        '$30,000-$34,999','$35,000-$39,999','$40,000-$44,999','$45,000-$49,999',
                                        '$50,000-$54,999','$55,000-$59,999','$60,000-$64,999','$65,000-$69,999',
                                        '$70,000-$74,999','$75,000-$79,999','$80,000-$84,999','$85,000-$89,999',
                                        '$90,000-$94,999','$95,000-$99,999','$100,000-$124,999','$125,000-$149,999',
                                        '$150,000-$174,999','$175,000-$199,999','$200,000-$249,999','\u2265$250,000')),
         TimeEffort=as.numeric(SenatorWillWork_1)/100,
         LocalPrime=as.integer(ODDExperiment=='LocalWriting'),
         ODDTreated=as.integer(ODDInfo=='Yes'),
         Age18.25=as.integer(age<26),
         Age26.34=as.integer(age>25 & age<35),
         Age35.54=as.integer(age>34 & age<55),
         Age55.64=as.integer(age>54 & age<65),
         Age65.Up=as.integer(age>64),
         Age='18-25')
d2$Age[d2$Age26.34==1L] = '26-34'
d2$Age[d2$Age35.54==1L] = '35-54'
d2$Age[d2$Age55.64==1L] = '55-64'
d2$Age[d2$Age65.Up==1L] = '65+'
d2$Party[d2$PartyID=='Republican' | d2$Leaner=='Closer to Republican Party'] = 'Republican'
d2$Party[d2$PartyID=='Democrat' | d2$Leaner=='Closer to Democratic Party'] = 'Democrat'


# Table 4
count(d2, Sex) %>%
  mutate(p=round(n/sum(n), 2))

count(d2, Race) %>%
  mutate(p=round(n/sum(n), 2))

count(d2, Party) %>%
  mutate(p=round(n/sum(n), 2))

count(d2, Education) %>%
  mutate(p=round(n/sum(n), 2))

count(d2, Income) %>%
  subset(!is.na(Income)) %>%
  mutate(p=round(n/sum(n), 2),
         summed=cumsum(p),
         half=summed>0.5)
  

# Table 5
d2 = subset(d2, !is.na(TimeEffort))
count(d2, LocalPrime)
count(d2, ODDTreated)
count(d2, LocalPrime, ODDTreated)


# Table 6
tab6 = lm(TimeEffort ~ LocalPrime*ODDTreated, data=d2)
summary(tab6)
tidy(tab6)
stargazer::stargazer(tab6)


########## Appendix ##########


########## Study 1 ###########


# Table B1
summary(d1$TimeEffort); sd(d1$TimeEffort, na.rm=T)
summary(d1$Efficacy); sd(d1$Efficacy, na.rm=T)
summary(d1$TrustCongress); sd(d1$TrustCongress, na.rm=T)
summary(d1$SouthernID); sd(d1$SouthernID, na.rm=T)


# Figure B3
partisanHTE1 = lm(TimeEffort ~ ExpCondition*(Party=='Democrat'), 
                  data=subset(d1, Party %in% c('Democrat','Republican'))) %>%
  tidy(conf.int=T) %>%
  subset(str_detect(term, 'Democrat')) %>%
  mutate(Treatment=str_extract(term, '(?<=ExpCondition).*(Prime|Treated)'),
         Treatment=str_c('Democrat x\n', Treatment),
         Treatment=str_replace(Treatment, ' ODD', '\nODD'),
         Treatment=str_remove(Treatment, ' Treatment')) %>%
  subset(!is.na(Treatment)) %>%
  ggplot(aes(x=Treatment, y=estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high)) +
  labs(title='Heterogeneous treatment effects by partisanship, time and effort (Study 1)',
       y='Effect estimate') +
  geom_hline(yintercept=0, linetype=2) +
  coord_cartesian(ylim=c(-0.15, 0.15)) +
  theme_bw() +
  theme(axis.text=element_text(size=8))
partisanHTE1


# Figure B4
partisanHTE2 = lm(Efficacy ~ ExpCondition*(Party=='Democrat'), 
                  data=subset(d1, Party %in% c('Democrat','Republican'))) %>%
  tidy(conf.int=T) %>%
  subset(str_detect(term, 'Democrat')) %>%
  mutate(Treatment=str_extract(term, '(?<=ExpCondition).*(Prime|Treated)'),
         Treatment=str_c('Democrat x\n', Treatment),
         Treatment=str_replace(Treatment, ' ODD', '\nODD'),
         Treatment=str_remove(Treatment, ' Treatment')) %>%
  subset(!is.na(Treatment)) %>%
  ggplot(aes(x=Treatment, y=estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high)) +
  labs(title='Heterogeneous treatment effects by partisanship, external efficacy (Study 1)',
       y='Effect estimate') +
  geom_hline(yintercept=0, linetype=2) +
  coord_cartesian(ylim=c(-0.15, 0.15)) +
  theme_bw() +
  theme(axis.text=element_text(size=8))
partisanHTE2


# Figure B5
partisanHTE3 = lm(TrustCongress ~ ExpCondition*(Party=='Democrat'), 
                  data=subset(d1, Party %in% c('Democrat','Republican'))) %>%
  tidy(conf.int=T) %>%
  subset(str_detect(term, 'Democrat')) %>%
  mutate(Treatment=str_extract(term, '(?<=ExpCondition).*(Prime|Treated)'),
         Treatment=str_c('Democrat x\n', Treatment),
         Treatment=str_replace(Treatment, ' ODD', '\nODD'),
         Treatment=str_remove(Treatment, ' Treatment')) %>%
  subset(!is.na(Treatment)) %>%
  ggplot(aes(x=Treatment, y=estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high)) +
  labs(title='Heterogeneous treatment effects by partisanship, trust in Congress (Study 1)',
       y='Effect estimate') +
  geom_hline(yintercept=0, linetype=2) +
  coord_cartesian(ylim=c(-0.15, 0.15)) +
  theme_bw() +
  theme(axis.text=element_text(size=8))
partisanHTE3


# Table B2
lm(TimeEffort ~ ExpCondition*Outpartisan, 
   data=subset(d1, Party %in% c('Democrat','Republican') & 
                 str_detect(ExpCondition, 'Specific'))) %>%
  stargazer


# Figure B6
southernHTE = d1 %>%
  subset(SouthernID<=quantile(SouthernID, 0.25, na.rm=T) | 
           SouthernID>=quantile(SouthernID, 0.75, na.rm=T)) %>% 
  mutate(VerySouthern=as.integer(SouthernID>0.5)) %>%
  lm(TimeEffort ~ ExpCondition*VerySouthern, data=.) %>%
  tidy(conf.int=T) %>%
  subset(str_detect(term, 'Southern')) %>%
  mutate(Treatment=str_extract(term, '(?<=ExpCondition).*(Prime|Treated)'),
         Treatment=str_c('High southern ID x\n', Treatment),
         Treatment=str_replace(Treatment, ' ODD', '\nODD'),
         Treatment=str_remove(Treatment, ' Treatment')) %>%
  subset(!is.na(Treatment)) %>%
  ggplot(aes(x=Treatment, y=estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high)) +
  labs(title='Heterogeneous treatment effects by southern identity, time and effort (Study 1)',
       y='Effect estimate') +
  geom_hline(yintercept=0, linetype=2) +
  coord_cartesian(ylim=c(-0.25, 0.25)) +
  theme_bw() +
  theme(axis.text=element_text(size=8))
southernHTE


# Table B3
d1.raking = d1[complete.cases(d1[, c('Race','Sex','age')]),] %>%
  mutate(init_weight=1)
dummy.pop = nrow(d1.raking)

marginal.race = data.frame(Race=c('White','Black','Latino','Asian','Other'),
                           prop=c(0.52, 0.32, 0.09, 0.04, 0.03)*dummy.pop)
marginal.sex = data.frame(Sex=c('Male','Female'),
                          prop=c(0.48, 0.52)*dummy.pop)
marginal.age = data.frame(age=c('1','2','3','4'),
                          prop=c(0.2, 0.25, 0.30, 0.25)*dummy.pop)

raked.design1 = svydesign(ids=~1, data=d1.raking, weights=~init_weight) %>%
  rake(sample.margins=list(~Race, ~Sex, ~age),
       population.margins=list(marginal.race, marginal.sex, marginal.age))

weights(raked.design1) %>%
  summary

tabB3col1 = svyglm(TimeEffort ~ ExpCondition, design=raked.design1)
summary(tabB3col1)
tidy(tabB3col1)

tabB3col2 = svyglm(Efficacy ~ ExpCondition, design=raked.design1)
summary(tabB3col2)
tidy(tabB3col2)

tabB3col3 = svyglm(TrustCongress ~ ExpCondition, design=raked.design1)
summary(tabB3col3)
tidy(tabB3col3)

stargazer(tabB3col1, tabB3col2, tabB3col3)



########## Study 2 ###########


# Table C1
summary(d2$TimeEffort); sd(d2$TimeEffort, na.rm=T)


# Figure C1
library(wordcloud)
library(RColorBrewer)
library(tm)

wordviz <- d2$LocalPride
wordviz <- tolower(wordviz)
wordviz <-removePunctuation(wordviz)
wordviz <-removeNumbers(wordviz)

# Remove specified words
words_to_remove <- c("and", "not", "that", "the", "are", "here", "our", "they")
wordviz <- removeWords(wordviz, words_to_remove)

myCorpus <- Corpus(VectorSource(wordviz))
dtm <- TermDocumentMatrix(myCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



# Table C2
d2.raking = d2[complete.cases(d2[, c('Race','Sex','Age')]),] %>%
  mutate(init_weight=1)
dummy.pop = nrow(d2.raking)

marginal.race = data.frame(Race=c('White','Black','Latino','Asian','Other'),
                           prop=c(0.58, 0.12, 0.18, 0.05, 0.06)*dummy.pop)
marginal.sex = data.frame(Sex=c('Male','Female'),
                          prop=c(0.5, 0.5)*dummy.pop)
marginal.age = data.frame(Age=c('18-25','26-34','35-54','55-64','65+'),
                          prop=c(0.087, 0.123, 0.257, 0.129, 0.174)/0.769*dummy.pop)

raked.design2 = svydesign(ids=~1, data=d2.raking, weights=~init_weight) %>%
  rake(sample.margins=list(~Race, ~Sex, ~Age),
       population.margins=list(marginal.race, marginal.sex, marginal.age))

weights(raked.design2) %>%
  summary

svyglm(TimeEffort ~ LocalPrime*ODDTreated, design=raked.design2) %>%
  stargazer


# Figure C2
partisanHTE.d2 = lm(TimeEffort ~ LocalPrime*ODDTreated*(Party=='Democrat'), 
   data=subset(d2, Party %in% c('Democrat','Republican'))) %>%
  tidy(conf.int=T) %>%
  subset(str_detect(term, ':Party')) %>%
  mutate(Treatment=str_remove(term, ':Party.*'),
         Treatment=str_replace(Treatment, 'LocalPrime', 'Local ID Prime'),
         Treatment=str_replace(Treatment, 'ODDTreated', 'Odd Treatment'),
         Treatment=str_replace(Treatment, ':', ' x '),
         Treatment=str_c('Democrat x\n', Treatment)) %>%
  subset(!is.na(Treatment)) %>%
  ggplot(aes(x=Treatment, y=estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high)) +
  labs(title='Heterogeneous treatment effects by partisanship, time and effort (Study 2)',
       y='Effect estimate') +
  geom_hline(yintercept=0, linetype=2) +
  coord_cartesian(ylim=c(-0.15, 0.15)) +
  theme_bw() +
  theme(axis.text=element_text(size=8))
partisanHTE.d2

