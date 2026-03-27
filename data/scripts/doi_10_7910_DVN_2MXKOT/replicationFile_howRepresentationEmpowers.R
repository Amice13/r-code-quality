#_____________________________________________________________________________#
#  File-Name:   replicationFile_howRepresentationEmpowers.R.r   		       		
#  Date:        24/05/14                		
#_____________________________________________________________________________#
pkgs <- c('ggplot2','dplyr','tidyverse','estimatr','conflicted','cregg','Hmisc',
          'haven','magrittr','tidyr','texreg','qwraps2','xtable','gridExtra',
          'quanteda','scales','tidytext','quanteda.dictionaries',
          'quanteda.sentiment','broom','rsample')
# Comment in below if packages need to be installed
# for (pkg in pkgs) install.packages(pkg, character.only = TRUE)
for (pkg in pkgs) library(pkg, character.only = TRUE)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("data_dictionary_LSD2015","quanteda.sentiment")
conflict_prefer("mean_sd","qwraps2")

#_____________________________________________________________________________#
#_____________________________________________________________________________#
#_____________________________________________________________________________#
# Loading data ----
#_____________________________________________________________________________#
data_survey_women <- read.csv('data_survey_women.csv',stringsAsFactors=T)
data_survey_men <- read.csv('data_survey_men.csv',stringsAsFactors=T)
data_conjoint_women <- read.csv('data_conjoint_women.csv',stringsAsFactors=T)
data_conjoint_men <- read.csv('data_conjoint_men.csv',stringsAsFactors=T)
data_newsArticles <- read.csv('data_newsArticles.csv')
data_namesCongressMember <- read.csv('data_namesCongressMember.csv')

#_____________________________________________________________________________#
# 3 Methods ----
#_____________________________________________________________________________#
# Experimental treatments ----
#_____________________________________________________________________________#
# Table 1: ----
# Treatment randomization of N=4545 across four conditions by self-reported 
# partisanship.
#_____________________________________________________________________________#
t1 <- data_survey_women %>%
  group_by(treatment,pid) %>%
  summarise(n=n()) %>%
  pivot_wider(id_cols=treatment,values_from=n,names_from=pid) %>%
  ungroup() %>%
  select(-treatment) %>%
  as.matrix()

t2 <- data_survey_women %>%
  group_by(treatment) %>%
  summarise(n=n()) %>%
  select(-treatment) %>%
  pull() %>%
  as.matrix()

t3 <- data_survey_women %>%
  group_by(pid) %>%
  summarise(n=n()) %>%
  pull(n) %>%
  as.matrix()

t4 <- cbind(t1,t2)

N <- length(unique(data_survey_women$sid))
c1 <- c('Primary','treatments','Supplemental','treatments','')
c2 <- c('Negative frame','Positive frame','No frame','No information','')

t <- cbind(c1,c2,rbind(t4[1,],t4[4,],t4[2,],t4[3,],cbind(t(t3),N)))
p <- xtable(t)
names(p) <- c("","","Democrat","Other","Republican","Total")

print(xtable(p,type='latex',align="rrlllll"),file='table1.tex',include.rownames=F)

#_____________________________________________________________________________#
# Figure 1: ----
# Sentiment of text in a 25 word window around the names of congresswomen as
# well as the terms "women", "woman", "female" or the names of congressmen as 
# well as the terms "men", "man", "male" in a text corpus containing 24039 
# articles in the ProQuest news database when searching for "representation", 
# "congress", "member of congress", "women", and "female".
#_____________________________________________________________________________#
corpus <- corpus(data_newsArticles)
docvars(corpus,field="textNoPubDate") <- 
  paste(docvars(corpus,field="textNo"),
        docvars(corpus,field="publicationDate"),sep=';')

namesCongressWomen <- 
  data_namesCongressMember %>% 
  filter(gender==1) %>% select(-gender) %>% t %>% c %>% unique %>%
  append(c('women','woman','female',"women's","woman's"))
namesCongressMen <- 
  data_namesCongressMember %>% 
  filter(gender==2) %>% select(-gender) %>% t %>% c %>% unique %>%
  append(c('men','man','male',"men's","man's"))

toksWomen <- dfm(tokens(corpus,remove_punct=T,remove_symbols=T,remove_numbers=T) %>% 
  tokens_keep(pattern=phrase(namesCongressWomen),window=25,padding=F,verbose=T))

outWomen <- 
  dfm_lookup(toksWomen,data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups=textNoPubDate) %>%
  convert(.,to="data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
    date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
    gender='Women',
    window=25) %>%
  filter(date>='2016-09-01')

toksMen <- dfm(tokens(corpus,remove_punct=T,remove_symbols=T,remove_numbers=T) %>% 
  tokens_keep(pattern=phrase(namesCongressMen),window=25,padding=F,verbose=T))

outMen <- dfm_lookup(toksMen,data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
    date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
    gender='Men',
    window=25) %>%
  filter(date>='2016-09-01')

out <- rbind(outMen,outWomen)

dates_vline <- c(as.Date("2016-11-08"),as.Date("2018-11-06"),
  as.Date("2020-11-03"))
dates_vline <- which(out$date %in% dates_vline)  

pdf('figure1.pdf',height=3)
out %>% 
  mutate(
    gender=factor(gender),
    election=ifelse(date<='2017-01-31','2016',
      ifelse(date<='2019-01-31','2018','2020'))) %>%
  ggplot(aes(x=date,y=sentiment,color=gender,fill=gender,linetype=gender)) +
  facet_grid(~election,scales='free_x') +
  geom_smooth() +
  geom_vline(xintercept=as.numeric(out$date[dates_vline]),color='gray') +
  geom_hline(aes(yintercept=0),color='gray') +
  scale_fill_manual(values=c('gray','black')) +
  scale_color_manual(values=c('gray','black')) +
  scale_linetype_manual(values=c("dotted", "solid")) +
  labs(x='',y='Sentiment') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()

#______________________________________________________________________________#
# Manipulation checks ----
#______________________________________________________________________________#
data_survey_women %>% 
  mutate(deviation=abs(127-guess.female.MOC),
    treatment.binary=factor(ifelse(treatment=='No information',
    'No information','Information'),levels=c("No information",'Information'))) %>%
  group_by(treatment.binary,sid) %>%
  summarise(dev=mean(deviation),guess=mean(guess.MOC)) %>%
  lm_robust(dev~treatment.binary + guess,data=.) %>%
  summary()

out <- data_survey_women %>% 
  mutate(
    concernBinary=ifelse(!is.na(concern)&concern!='Concern',0,
      ifelse(concern=='Concern',1,NA)),
    achievementBinary=ifelse(!is.na(concern)&concern!='Achievement',0,
      ifelse(concern=='Achievement',1,NA))) 

data_survey_women %>% 
  group_by(pid,concern) %>% summarise(n=n()) %>% 
  mutate(N=max(cumsum(n)),prop=n/N) %>%
  filter(pid!='Other')

out %>%
  filter(pid=='Republican'|pid=='Democrat') %>% 
  droplevels() %$% 
  chisq.test(table(.$concernBinary,.$pid))

out %>%
  filter(pid=='Republican'|pid=='Democrat') %$% 
  length(unique(sid))
  
#______________________________________________________________________________#
# 4 Results ----
#______________________________________________________________________________#
# 4.1 Framing effect on likely to apply to jobs ----
#_______________________________________________________________________________
# Figure 2 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of willingness to apply overall and by job attribute.                                                                                 
#_______________________________________________________________________________
out <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  droplevels()

p1 <- out %>%
  lm_robust(likely.apply~treatment + pid,data=.,clusters=sid) %>%
  tidy() %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high)) + 
  geom_pointrange(size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),linewidth=1,size=.5) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"))

p2 <- out %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  droplevels() %>%
  cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~treatment) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error,
    level=factor(level,levels=c('Admin Assistant','Assistant Manager',
      'Branch Manager','Executive Assistant','CEO','Not [most competitive]',
      'Most competitive','Not [highest pay]','Highest pay','Education','Tech'))) %>% 
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figure2.pdf',height=3)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

#_______________________________________________________________________________
# Figure 3 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of willingness to apply overall and by job attribute and 
# partisan identity.                                                                                 
#_______________________________________________________________________________
out <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>% 
  droplevels() 

p1 <- out %>%
  group_by(pid) %>%
  do(lm_robust(likely.apply~treatment,data=.,clusters=sid) %>%
    tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.65*std.error,u2=estimate+1.65*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"),
    legend.position='None')

p2 <- cbind(
  out %>%
    group_by(pid) %>%
    do(cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
      industry.label,id=~sid,estimate='mm_differences',by=~treatment)),
  out %>%
    group_by(pid) %>%
    do(cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
      industry.label,id=~sid,estimate='mm_differences',by=~treatment,alpha=0.1)) %>%
    mutate(l2=lower,u2=upper) %>%
    ungroup() %>%
    select(l2,u2)) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figure3.pdf',height=5)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

out %>%
  lm_robust(likely.apply~treatment + pid, clusters=sid, data=.) %>% 
  tidy() %>%
  filter(grepl('pid',term)==F) %>%
  mutate(percentEffect=estimate/lag(estimate)) %>%
  select(estimate,statistic,p.value,percentEffect)

out %$% length(sid)

out %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  group_by(value) %>% 
  do(lm_robust(likely.apply~treatment + pid, clusters=sid, data=.) %>% 
       tidy()) %>%
  filter(grepl('pid',term)==F) %>%
  mutate(percentEffect=estimate/lag(estimate)) %>%
  filter(!is.na(percentEffect)) %>%
  select(estimate,statistic,p.value,percentEffect)

out %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  group_by(value) %>%
  summarise(N=n())

out %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  group_by(pid,value) %>% 
  do(lm_robust(likely.apply~treatment, clusters=sid, data=.) %>% tidy()) %>% 
  filter(
    (pid=='Democrat'&value=='CEO')|
      (pid=='Democrat'&value=='Executive Assistant')|
      (pid!='Other'&value=='Most competitive')|
      (pid!='Other'&value=='Tech')|
      (pid=='Republican'&value=='Education')|
      (pid=='Republican'&value=='Not [highest pay]')|
      (pid=='Republican'&value=='Branch Manager')|
      (pid=='Republican'&value=='Assistant Manager')) %>%
  mutate(p.value=p.value/2,
         percentEffect=estimate/lag(estimate)) %>%
  filter(!is.na(percentEffect)) %>%
  select(pid,value,estimate,statistic,p.value,percentEffect)

data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  droplevels() %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  filter(
    (pid=='Democrat'&value=='CEO')|
      (pid=='Democrat'&value=='Executive Assistant')|
      (pid!='Other'&value=='Most competitive')|
      (pid!='Other'&value=='Tech')|
      (pid=='Republican'&value=='Education')|
      (pid=='Republican'&value=='Not [highest pay]')|
      (pid=='Republican'&value=='Branch Manager')|
      (pid=='Republican'&value=='Assistant Manager')) %>%
  group_by(pid,value) %>%
  summarise(N=n())

#_______________________________________________________________________________
# 4.2 Mechanism
#_______________________________________________________________________________
# 4.2.1 Empowerment through change in self-efficacy, self-esteem, group-based 
# pride, or feelings of qualification
#_______________________________________________________________________________
data_survey_women %>% 
  mutate(across(c(self.efficacy,self.esteem,ftm.women),
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>% 
  group_by(treatment,pid) %>%
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>% 
  droplevels() %>%
  pivot_longer(cols=c(self.efficacy,self.esteem,ftm.women)) %>%
  group_by(pid,name) %>%
  do(lm_robust(value~treatment,data=.) %>% tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(name=factor(recode(name,
    'self.esteem'='Self-esteem','self.efficacy'='Self-efficacy',
    'ftm.women'='Feelings towards\nwomen'))) %>%
  select(pid,name,estimate,statistic,p.value)

data_survey_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>% 
  droplevels() %>%
  pivot_longer(cols=c(self.efficacy,self.esteem,ftm.women)) %>%
  group_by(pid,name) %>%
  summarise(N=n())

#_______________________________________________________________________________
# Figure 4 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of feeling qualified for job overall and by attribute and 
# partisanship.
#_______________________________________________________________________________
p1 <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  droplevels() %>%
  lm_robust(qualify.job~treatment + pid,data=.,clusters=sid) %>%
  tidy() %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),linewidth=1,size=.5) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,10)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"))

p2 <- data_conjoint_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>% 
  droplevels() %>%
  group_by(pid) %>%
  do(cj(.,qualify.job ~ category.label + comp.label + high.pay.label + 
          industry.label,id=~sid,estimate='mm_differences',by=~treatment)) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  filter(pid!='Other') %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.5),
                  linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-8,10)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figure4.pdf',height=5)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  droplevels() %>%
  lm_robust(qualify.job~treatment + pid,data=.,clusters=sid) %>%
  tidy() %>%
  filter(grepl('treatment',term)==T) %>%
  select(estimate,statistic,p.value)

data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  mutate(N=n()) %>%
  select(N)

data_conjoint_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>% 
  droplevels() %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  group_by(pid,value) %>% 
  do(lm_robust(likely.apply~treatment + vignette, clusters=sid, data=.) %>% 
       tidy()) %>% 
  filter(pid=='Democrat'&value=='CEO') %>%
  filter(term=='treatmentPositive') %>%
  mutate(p.value=p.value/2) %>%
  select(pid,value,estimate,statistic,p.value)

data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  droplevels() %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  filter(pid=='Democrat'&value=='CEO') %>%
  group_by(pid,value) %>%
  summarise(N=n())


#______________________________________________________________________________#
# 4.2.2 Empowerment through change in Beliefs ----
#______________________________________________________________________________#
# Figure 5: ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on expectations of discrimination and expectation that others will rectify 
# discrimination by partisanship.         
#_______________________________________________________________________________
out <- data_survey_women %>%
  mutate(across(c(expect.discr,others.rectify),
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>%
  select(treatment,pid,expect.discr,others.rectify)

data_survey_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>%
  pivot_longer(c(expect.discr,others.rectify)) %>%
  filter(!is.na(value)) %>%
  group_by(pid,name) %>%
  summarise(N=length(unique(sid)))

r <- out %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>%
  pivot_longer(cols=-c(treatment,pid)) %>%
  group_by(name,pid) %>%
  do(lm_robust(value~treatment,data=.) %>% tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(
    name=factor(recode(name,
      'expect.discr'='Expected\ndiscrimination',
      'others.rectify'='Rectify\ndiscrimination'),
    levels=c('Expected\ndiscrimination','Rectify\ndiscrimination')))

pdf('figure5.pdf',height=2)
r %>% 
  filter(pid!='Other') %>%
  mutate(l2=estimate-std.error*1.64,u2=estimate+std.error*1.64) %>%
  ggplot(aes(y=estimate,x=name,ymin=conf.low,ymax=conf.high,
    fill=pid,color=pid)) +
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.5),
    linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() +
  labs(y='Difference in means',x='') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()

out %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid=='Democrat') %>%
  t.test(expect.discr~treatment,data=.,alternative='greater')  
out %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid=='Republican') %>%
  t.test(expect.discr~treatment,data=.,alternative='greater')  
out %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid=='Democrat') %>%
  t.test(others.rectify~treatment,data=.,alternative='greater')  
out %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid=='Republican') %>%
  t.test(others.rectify~treatment,data=.,alternative='greater')  

data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!="Other") %>%
  group_by(pid,high.pay.label) %>%
  do(lm_robust(likely.apply~treatment*expect.discr,data=.,clusters=sid) %>% 
       tidy()) %>%
  filter(term=="treatmentPositive:expect.discr")

#______________________________________________________________________________#
# 4.3 Information effects ----
#______________________________________________________________________________#
data_conjoint_women %>% 
  filter((treatment=='No information'|treatment=='No frame')&pid!='Other') %>%
  droplevels() %>%
  lm_robust(likely.apply~treatment + pid,data=.,clusters=sid) %>% 
  tidy()

data_conjoint_women %>% 
  filter((treatment=='No information'|treatment=='No frame')&pid!='Other') %$%
  length(likely.apply)

data_survey_women %>% 
  mutate(across(c(self.efficacy,self.esteem,ftm.women),
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>% 
  group_by(treatment,pid,sid) %>% 
  summarise(across(c(self.efficacy,self.esteem,ftm.women),
    ~mean(.x,na.rm=T))) %>%
  select(-sid) %>%
  filter((treatment=='No information'|treatment=='No frame')&pid!='Other') %>% 
  droplevels() %>%
  pivot_longer(cols=-c(treatment,pid)) %>%
  group_by(name) %>%
  do(lm_robust(value~treatment + pid,data=.) %>% tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(
    name=factor(recode(name,
      'self.esteem'='Self-esteem',
      'self.efficacy'='Self-efficacy',
      'ftm.women'='Feelings towards\nwomen'))) %>%
  select(name,estimate,statistic,p.value)

data_survey_women %>%
  filter((treatment=='No information'|treatment=='No frame')&pid!='Other') %$% 
  length(self.esteem)

data_conjoint_women %>% 
  filter((treatment=='No information'|treatment=='No frame')&pid!='Other') %>%
  droplevels() %>%
  lm_robust(qualify.job~treatment + pid,data=.,clusters=sid) %>%
  tidy()

data_conjoint_women %>% 
  filter((treatment=='No information'|treatment=='No frame')&pid!='Other') %$%
  length(qualify.job)

data_survey_women %>% 
  select(treatment,pid,expect.discr,others.rectify) %>%
  mutate(across(c(expect.discr,others.rectify),
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>% 
  filter((treatment=='No information'|treatment=='No frame')&pid!='Other') %>%
  pivot_longer(cols=-c(treatment,pid)) %>%
  group_by(pid,name) %>%
  do(lm_robust(value~relevel(treatment,ref='No information'),data=.) %>% tidy()) %>% 
  filter(grepl('treatment',term)==T) %>%
  mutate(name=factor(recode(name,
    'expect.discr'='Expected\ndiscrimination',
    'others.rectify'='Rectify\ndiscrimination'),
    levels=c('Expected\ndiscrimination','Rectify\ndiscrimination'))) %>%
  select(name,estimate,statistic,p.value)

data_survey_women %>% 
  filter((treatment=='No information'|treatment=='No frame')&pid!='Other')  %>%
  pivot_longer(c(expect.discr,others.rectify)) %>%
  group_by(pid,name) %>%
  summarise(N=sum(!is.na(value)))

#______________________________________________________________________________#
# 4.4 Effect heterogeneity ----
#______________________________________________________________________________#
data_survey_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  lm_robust(group.marginal~pid,data=.) %>%
  tidy()

data_survey_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %$%
  length(group.marginal)

#_______________________________________________________________________________
# 4.4 Effect heterogeneity ----
#______________________________________________________________________________#
data_survey_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  lm_robust(group.marginal~pid,data=.) %>%
  tidy()

data_survey_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %$%
  length(group.marginal)

#______________________________________________________________________________#
# Figure 6: ----
# Framing effect on marginal mean of willingness to apply by job attribute, 
# partisan identity, and race.    
#______________________________________________________________________________#
r <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&
    race!='Prefer not to say') %>% 
  droplevels() %>%
  group_by(race,pid) %>%
  do(lm_robust(likely.apply~treatment,clusters=sid,data=.) %>% tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error,
    race=factor(recode(race,'Non-Hispanic white'='White',
      'Non-Hispanic black'='Black','Non-Hispanic other'='Other'),
      levels=c('Other','Hispanic','Black','White'))) 

pdf('figure6.pdf', height=3)
r %>% filter(pid!='Other') %>%
  ggplot(aes(x=race,y=estimate,ymin=conf.low,ymax=conf.high,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.3,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.5),
    linewidth=1,size=.3,shape=21) +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank(),
    strip.background=element_blank())
dev.off()

data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&
    race!='Prefer not to say'&pid!='Other') %>% 
  droplevels() %>%
  group_by(race,pid) %>%
  summarise(n=n(),K=length(unique(sid)))

#______________________________________________________________________________#
# 4.5 Robustness and Extensions ----
#______________________________________________________________________________#
data_conjoint_men %>% 
  lm_robust(likely.apply~frame.treat+pid,data=.,clusters=sid) %>%
  tidy()

data_conjoint_men %$% length(likely.apply) 

data_conjoint_men %>% 
  group_by(pid) %>%
  do(lm_robust(likely.apply~frame.treat,data=.,clusters=sid) %>% tidy())

data_conjoint_men %>% 
  group_by(pid) %>%
  summarise(N=length(likely.apply))

data_conjoint_men %>% 
  group_by(pid) %>%
  do(cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~frame.treat))

data_conjoint_men %>% 
  pivot_longer(c(category.label,comp.label,high.pay.label,industry.label)) %>% 
  group_by(value,pid) %>%
  summarise(N=n()) %>% View()

#______________________________________________________________________________#
# Appendix ----
#______________________________________________________________________________#
# A Study design appendix ----
#______________________________________________________________________________#
# A.3 Sample characteristics and treatment condition balance ----
#______________________________________________________________________________#
# Table A.1: ----
# Relative frequencies in our sample vs. 2020 ANES Data
#______________________________________________________________________________#
out <- data_survey_women %>% 
  select(pid,race,education.num,income.num,age) %>% 
  mutate(
    age=ifelse(age<=29,'18 to 29',ifelse(age>29&age<=39,'30 to 39',
      ifelse(age>39&age<=49,'40 to 49',ifelse(age>49&age<=69,'50 to 69',
        '70 and over')))),
    education=ifelse(education.num<=2,'Less than high school diploma',
      ifelse(education.num==3,'High school diploma or equivalent',
      ifelse(education.num>3&education.num<=5,'Some college',
      ifelse(education.num==6,"Bachelor's degree",
      ifelse(education.num>6&education.num<=8,'Graduate degree',
      ifelse(education.num==9,'Professional degree',NA)))))),
    income=ifelse(income.num<=5,'$1,000 to $14,999',
      ifelse(income.num>5&income.num<=7,'$15,000 to $24,999',
      ifelse(income.num>7&income.num<=9,'$25,000 to $34,999',
      ifelse(income.num>9&income.num<=11,'$35,000 to $49,999',
      ifelse(income.num>11&income.num<=13,'$50,000 to $74,999',
      ifelse(income.num>13&income.num<=15,'$75,000 to $99,999',
      ifelse(income.num==16,'$100,000 to $124,999',
      ifelse(income.num==17,'$125,000 to $149,999',
      ifelse(income.num==18,'$150,000 to $174,999',
      ifelse(income.num>18,'$175,000 or more',NA))))))))))) %>%
  select(pid,race,education,income,age) %>% 
  gather(variable,value,race:age) %>%
  group_by(pid,variable,value) %>%
  summarise(n=n()) %>% mutate(N = max(cumsum(n)), freq = n/N) %>%
  filter(!is.na(value)&value!='Prefer not to say') %>%
  spread(pid,freq) %>% group_by(variable,value) %>%
  summarise_all(mean,na.rm=T) %>%
  select(-c(n,N)) %>%
  as.matrix()

t <- cbind(out[,2],out[,3],out[,5],out[,4])
p <- xtable(t)
names(p) <- c("","Democrat","Republican","Other")

print(xtable(p,type='latex',align="rrlll"),file='tableA1.tex',
  include.rownames=F,digits=c(0,0,2,2,2))

#______________________________________________________________________________#
# Table A.2: ----
# Means and standard deviation of pre-treatment variables by treatment condition. 
# Difference-in-means and difference-in-distribution tests of pre-treatment 
# variables for No information vs Information (No frame) treatment, Negative vs 
# Information (No frame) treatment, and Positive vs Negative treatment.
#______________________________________________________________________________#
var.list <- c('age','highSchoolOrLess','someCollege','bachelors',
  'graduateDegree','income','race','turnout12','turnout16',
  'employed','unemployed','homemaker','democrat','republican','ideology',
  'polKnowledge','femininity','modern.sexism','women.role',
  'women.role.politics','ftm.women.liberation','col.esteem',
  'change.unfair.laws','job.discriminated','life.affected',
  'group.marginal','guess.MOC')
out.0 <- data_survey_women %>% 
  mutate(
    highSchoolOrLess=ifelse(as.numeric(education)<=3,1,0),
    someCollege=ifelse(as.numeric(education)>3&as.numeric(education)<=5,1,0),
    bachelors=ifelse(as.numeric(education)==6,1,0),
    graduateDegree=ifelse(as.numeric(education)>6,1,0),
    race=ifelse(race=='Non-Hispanic white',1,0),
    turnout12=ifelse(turnout12=='Yes',1,ifelse(turnout12=='No',0,NA)),
    turnout16=ifelse(turnout16=='Yes',1,ifelse(turnout16=='No',0,NA)),
    employed=ifelse(work=='Working now',1,0),
    unemployed=ifelse(work=='Temporarily laid off'|work=='Unemployed',1,0),
    homemaker=ifelse(work=='Homemaker',1,0),
    democrat=ifelse(pid=='Democrat',1,0),
    republican=ifelse(pid=='Republican',1,0),
    ideology=ifelse(ideology!='Prefer not to say',
      as.numeric(ideology),NA),
    income=as.numeric(income)) %>%
  select(var.list,treatment) %>%
  pivot_longer(cols=var.list,names_to='variable')

out.ttest <- rbind(
  out.0 %>% filter(treatment=='No information'|treatment=='No frame') %>% 
    group_by(variable) %>% do(tidy(t.test(value~treatment,data=.))) %>% 
    mutate(comparison='No frame vs no information'),
  out.0 %>% filter(treatment=='Negative'|treatment=='No frame') %>% 
    group_by(variable) %>% do(tidy(t.test(value~treatment,data=.))) %>% 
    mutate(comparison='Negative vs no frame'),
  out.0 %>% filter(treatment=='Negative'|treatment=='Positive') %>% 
    group_by(variable) %>% do(tidy(t.test(value~treatment,data=.))) %>% 
    mutate(comparison='Positive vs negative'))

out.rankSum <- rbind(
  out.0 %>% filter(treatment=='No information'|treatment=='No frame') %>% 
    group_by(variable) %>% do(tidy(wilcox.test(value~treatment,data=.))) %>% 
    mutate(comparison='No frame vs no information'),
  out.0 %>% filter(treatment=='Negative'|treatment=='No frame') %>% 
    group_by(variable) %>% do(tidy(wilcox.test(value~treatment,data=.))) %>% 
    mutate(comparison='Negative vs no frame'),
  out.0 %>% filter(treatment=='Negative'|treatment=='Positive') %>% 
    group_by(variable) %>% do(tidy(wilcox.test(value~treatment,data=.))) %>% 
    mutate(comparison='Positive vs negative')) 

out <- out.0 %>% group_by(variable,treatment) %>% 
  summarise_all(list(mean=~mean(.,na.rm=T),sd=~sd(.,na.rm=T))) %>%
  pivot_wider(id_cols=variable,names_from=treatment,values_from=c(mean,sd)) %>%
  select(variable,`mean_No information`,`sd_No information`,`mean_No frame`,
    `sd_No frame`,`mean_Negative`,`sd_Negative`,`mean_Positive`,`sd_Positive`) %>%
  left_join(out.ttest %>% select(variable,comparison,estimate,p.value) %>%
    mutate(estimate=(-1)*estimate) %>%
    pivot_wider(id_cols=variable,names_from=comparison,values_from=c(estimate,p.value)) %>%
    select(variable,`estimate_No frame vs no information`,
      `p.value_No frame vs no information`,`estimate_Negative vs no frame`,
      `p.value_Negative vs no frame`,`estimate_Positive vs negative`,
      `p.value_Positive vs negative`)) %>%
  left_join(out.rankSum %>% select(variable,comparison,p.value) %>%
    rename(p.value.rankSum=p.value) %>%
    pivot_wider(id_cols=variable,names_from=comparison,values_from=p.value.rankSum,
    names_glue="{.value}_{comparison}")) %>%
  as.matrix()

r1 <- c("","No","","Information","","","","","","No frame vs","","Negative vs","",
  "Positive vs","","No frame vs","Negative vs","Positive vs") %>% as.matrix()
r2 <- c("","Information","","(No frame)","","Negative","","Positive","","no information","",
  "no frame","","negative","","no information","no frame","negative") %>% as.matrix()
r3 <- c("Variable","Mean","(SD)","Mean","(SD)","Mean","(SD)","Mean","(SD)","Diff","(p)","Diff","(p)",
  "Diff","(p)","p","p","p") %>% as.matrix()
r <- rbind(t(r2),t(r3))
c <- c("Age","Share white","Share up to hs degree","Share some college",
       "Share bachelors degree","Share graduate degree","Income level",
       "Share employed","Share unemployed","Share homemaker",
       "Turnout rate 2012","Turnout rate 2016","Share Democrats",
       "Share Republicans","Ideology","Political knowledge",
       "Change unfair laws","Collective esteem","Femininity",
       "FTM women liberation","Group marginalized","Women discriminated",
       "Life affected","Modern sexism scale","Women equal role",
       "Women role in politics","Guess # MOC")
t <- rbind(r,cbind(c,rbind(
  out[1,],out[20,],out[12,],out[22,],out[2,],out[9,],out[15,],out[6,],
  out[25,],out[13,],out[23,],out[24,],out[5,],out[21,],out[14,],out[19,],
  out[3,],out[4,],out[7,],out[8,],out[10,],out[16,],out[17,],out[18,],
  out[26,],out[27,],out[11,])[,-1]))
p <- xtable(t,type='latex',align="rrlllllllllllllllll") 
names(p) <- r1
print(xtable(p),file='tableA2.tex',include.rownames=F)

#______________________________________________________________________________#
# A.5 Outcome screens ----
#______________________________________________________________________________#
# Figure A.2 ---- 
# Distribution of outcome measures by party identity
#______________________________________________________________________________#
outcome.labels = c(
  'likely.apply'='Willingness to apply',
  'qualify.job'='Feeling qualified')
pdf('figureA2.pdf', height=4)
data_conjoint_women %>% gather(outcome.measure,values,c(likely.apply,qualify.job)) %>%
  ggplot(aes(x=values)) +
  geom_histogram(aes(y=..count../sum(..count..)), colour="black", fill="white", bins=20) +
  geom_density(color='red',alpha=.2) +
  scale_color_manual(values=c('blue','gray','purple')) +
  scale_fill_manual(values=c('blue','gray','purple')) +
  facet_grid(outcome.measure~pid,labeller=labeller(outcome.measure=outcome.labels)) +
  labs(x = "Outcome measure", y = 'Relative frequency') +
  theme_bw()
dev.off()

#______________________________________________________________________________#
# Figure A.3 ----
# Distribution of outcome measures by party identity and frame treatment
#______________________________________________________________________________#
pdf('figureA3.pdf', height=4)
data_conjoint_women %>% 
  gather(outcome.measure,values,c(likely.apply,qualify.job)) %>%
  ggplot(aes(x=values,color=treatment,fill=treatment)) +
  geom_density(alpha=.2) +
  facet_grid(outcome.measure~pid,labeller=labeller(outcome.measure=outcome.labels)) +
  scale_color_manual(values=c('gray','black','blue','purple')) +
  scale_fill_manual(values=c('gray','white','white','purple')) +
  labs(x = "Outcome measure", y = 'Relative frequency') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()

#______________________________________________________________________________#
# A.7 Manipulation check results ----
#______________________________________________________________________________#
# Table A.4 ----
# Regression of deviation of subjects’ guesses of the number of women among 
# members of Congress on a binary indicator of the information treatments 
# (positive, negative, and no frame treatment) and subjects' guesses of the 
# number of members of Congress.
#______________________________________________________________________________#
m <- data_survey_women %>% 
  mutate(deviation=abs(127-guess.female.MOC),
    treatment.binary=factor(ifelse(treatment=='No information',
      'No information','Information'),
      levels=c("No information",'Information'))) %>%
  group_by(treatment.binary,sid) %>%
  summarise(dev=mean(deviation),guess=mean(guess.MOC)) %>%
  lm_robust(dev~treatment.binary + guess,data=.) 
texreg(m,digits=3, single.row=F, include.ci=F, stars=c(.1,.05,.01),
  reorder.coef=c(2,3,1),
  custom.coef.names=c('Constant','Information treatment','Guess # members of Congress'),
  table=F,file='tableA4.tex') 

#______________________________________________________________________________#
# Figure A.7 ----
# Distribution of guesses about the number of female members of Congress by 
# treatment and party identity
#______________________________________________________________________________#
pdf('figureA7.pdf',height=3)
data_survey_women %>% 
  ggplot(aes(x=guess.female.MOC,color=pid,fill=pid)) +
  geom_density(alpha=.4) +
  facet_grid(~treatment) +
  scale_color_manual(values=c('blue','gray','red')) +
  scale_fill_manual(values=c('blue','gray','red')) +
  scale_x_continuous(limits=c(0,210),breaks=c(0,50,100,127,150,200)) +
  labs(y='Guess number of female MOC',x='') +
  geom_vline(aes(xintercept=127),color='black') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    legend.spacing.x = unit(.25,'cm'),axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
dev.off()

#______________________________________________________________________________#
# A.9 Correlation of partisanship with moderators ----
#______________________________________________________________________________#
# Table A.5 ----
# (Point-biserial) correlation coefficient for moderator variables with whether 
# respondents indicate to be Democrat (and not Republican), Democrat (and not 
# partisan of another party/independent), Republican (and not partisan of 
# another party/independent).
#______________________________________________________________________________#
var.list <- 
  c('col.esteem','group.marginal','femininity','masculinity',
    'modern.sexism','women.role','women.role.politics','ftm.women.liberation',
    'change.unfair.laws','job.discriminated','life.affected')

out <- data_survey_women %>%
  mutate(binaryComp=ifelse(pid=='Democrat',1,
    ifelse(pid=='Republican',0,NA))) %>%
  filter(!is.na(binaryComp)) 

r1 <- cbind(
  var.list,
  rbind(
    out %$% cor.test(col.esteem,binaryComp)$estimate,
    out %$% cor.test(group.marginal,binaryComp)$estimate,
    out %$% cor.test(femininity,binaryComp)$estimate,
    out %$% cor.test(masculinity,binaryComp)$estimate,
    out %$% cor.test(modern.sexism,binaryComp)$estimate,
    out %$% cor.test(women.role,binaryComp)$estimate,
    out %$% cor.test(women.role.politics,binaryComp)$estimate,
    out %$% cor.test(ftm.women.liberation,binaryComp)$estimate,
    out %$% cor.test(change.unfair.laws,binaryComp)$estimate,
    out %$% cor.test(job.discriminated,binaryComp)$estimate,
    out %$% cor.test(life.affected,binaryComp)$estimate),
  rbind(
    out %$% cor.test(col.esteem,binaryComp)$p.value,
    out %$% cor.test(group.marginal,binaryComp)$p.value,
    out %$% cor.test(femininity,binaryComp)$p.value,
    out %$% cor.test(masculinity,binaryComp)$p.value,
    out %$% cor.test(modern.sexism,binaryComp)$p.value,
    out %$% cor.test(women.role,binaryComp)$p.value,
    out %$% cor.test(women.role.politics,binaryComp)$p.value,
    out %$% cor.test(ftm.women.liberation,binaryComp)$p.value,
    out %$% cor.test(change.unfair.laws,binaryComp)$p.value,
    out %$% cor.test(job.discriminated,binaryComp)$p.value,
    out %$% cor.test(life.affected,binaryComp)$p.value)) %>% 
  as.data.frame() %>%
  rename(p=V3) %>%
  mutate(pidComp='Democrat vs Republican')

out <- data_survey_women %>%
  mutate(binaryComp=ifelse(pid=='Democrat',1,
    ifelse(pid=='Other',0,NA))) %>%
  filter(!is.na(binaryComp)) 

r2 <- cbind(
  var.list,
  rbind(
    out %$% cor.test(col.esteem,binaryComp)$estimate,
    out %$% cor.test(group.marginal,binaryComp)$estimate,
    out %$% cor.test(femininity,binaryComp)$estimate,
    out %$% cor.test(masculinity,binaryComp)$estimate,
    out %$% cor.test(modern.sexism,binaryComp)$estimate,
    out %$% cor.test(women.role,binaryComp)$estimate,
    out %$% cor.test(women.role.politics,binaryComp)$estimate,
    out %$% cor.test(ftm.women.liberation,binaryComp)$estimate,
    out %$% cor.test(change.unfair.laws,binaryComp)$estimate,
    out %$% cor.test(job.discriminated,binaryComp)$estimate,
    out %$% cor.test(life.affected,binaryComp)$estimate),
  rbind(
    out %$% cor.test(col.esteem,binaryComp)$p.value,
    out %$% cor.test(group.marginal,binaryComp)$p.value,
    out %$% cor.test(femininity,binaryComp)$p.value,
    out %$% cor.test(masculinity,binaryComp)$p.value,
    out %$% cor.test(modern.sexism,binaryComp)$p.value,
    out %$% cor.test(women.role,binaryComp)$p.value,
    out %$% cor.test(women.role.politics,binaryComp)$p.value,
    out %$% cor.test(ftm.women.liberation,binaryComp)$p.value,
    out %$% cor.test(change.unfair.laws,binaryComp)$p.value,
    out %$% cor.test(job.discriminated,binaryComp)$p.value,
    out %$% cor.test(life.affected,binaryComp)$p.value)) %>% 
  as.data.frame() %>%
  rename(p=V3) %>%
  mutate(pidComp='Democrat vs Other')

out <- data_survey_women %>%
  mutate(binaryComp=ifelse(pid=='Republican',1,
    ifelse(pid=='Other',0,NA))) %>%
  filter(!is.na(binaryComp)) 

r3 <- cbind(
  var.list,
  rbind(
    out %$% cor.test(col.esteem,binaryComp)$estimate,
    out %$% cor.test(group.marginal,binaryComp)$estimate,
    out %$% cor.test(femininity,binaryComp)$estimate,
    out %$% cor.test(masculinity,binaryComp)$estimate,
    out %$% cor.test(modern.sexism,binaryComp)$estimate,
    out %$% cor.test(women.role,binaryComp)$estimate,
    out %$% cor.test(women.role.politics,binaryComp)$estimate,
    out %$% cor.test(ftm.women.liberation,binaryComp)$estimate,
    out %$% cor.test(change.unfair.laws,binaryComp)$estimate,
    out %$% cor.test(job.discriminated,binaryComp)$estimate,
    out %$% cor.test(life.affected,binaryComp)$estimate),
  rbind(
    out %$% cor.test(col.esteem,binaryComp)$p.value,
    out %$% cor.test(group.marginal,binaryComp)$p.value,
    out %$% cor.test(femininity,binaryComp)$p.value,
    out %$% cor.test(masculinity,binaryComp)$p.value,
    out %$% cor.test(modern.sexism,binaryComp)$p.value,
    out %$% cor.test(women.role,binaryComp)$p.value,
    out %$% cor.test(women.role.politics,binaryComp)$p.value,
    out %$% cor.test(ftm.women.liberation,binaryComp)$p.value,
    out %$% cor.test(change.unfair.laws,binaryComp)$p.value,
    out %$% cor.test(job.discriminated,binaryComp)$p.value,
    out %$% cor.test(life.affected,binaryComp)$p.value)) %>% 
  as.data.frame() %>%
  rename(p=V3) %>%
  mutate(pidComp='Republican vs Other') 

r <- cbind( 
  rbind(r1,r2,r3) %>%
    pivot_wider(id_cols=var.list,values_from=cor,names_from=pidComp,
      names_glue="{.value}_{pidComp}"),
  rbind(r1,r2,r3) %>%
    pivot_wider(id_cols=var.list,values_from=p,names_from=pidComp,
      names_glue="{.value}_{pidComp}") %>%
    select(-c('var.list'))) %>%
  select(var.list,
    `cor_Democrat vs Republican`,`p_Democrat vs Republican`,
    `cor_Democrat vs Other`,`p_Democrat vs Other`,
    `cor_Republican vs Other`,`p_Republican vs Other`) %>%
  mutate(var.list=factor(recode(var.list,
    'masculinity'='Masculinity', 
    'femininity'='Femininity',
    'modern.sexism'='Modern sexism',
    'women.role'='Role for women in society',
    'women.role.politics'='Role for women in politics',
    'ftm.women.liberation'='Feeling towards women\nliberation movement',
    'col.esteem'='Collective self-esteem',
    'change.unfair.laws'='Important to change laws',
    'job.discriminated'='Labor market discrimination\n more likely to hit women',
    'life.affected'='What happens to\nwomen affects life',
    'group.marginal'='Group marginalization'),
    levels=c('Group marginalization','What happens to\nwomen affects life',
      'Labor market discrimination\n more likely to hit women',
      'Important to change laws','Collective self-esteem',
      'Feeling towards women\nliberation movement',
      'Role for women in politics','Role for women in society',
      'Modern sexism','Femininity','Masculinity'))) %>%
  mutate(across(`cor_Democrat vs Republican`:`p_Republican vs Other`,
    parse_number))

print(xtable(r,digits=c(0,0,2,3,2,3,2,3)),file='tableA5.tex',include.rownames=F)

#______________________________________________________________________________#
# A.10 Sentiment analysis ----
#______________________________________________________________________________#
# Figure A.8: ----
# Distribution of news items search hits in ProQuest
#_______________________________________________________________________________
pdf('figureA8.pdf',height=5,width=8)
data_newsArticles %>%
  ggplot(aes(x=publicationDate)) +
  facet_grid(~election, scales="free_x") +
  geom_histogram(stat="count") +
  labs(y='Number of news search hits',x='') +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
dev.off()

#_______________________________________________________________________________
# Figure A.9: ----
# Distribution of news items search hits by news outlet in ProQuest
#_______________________________________________________________________________
textListMajorNews <- 
  data_newsArticles %>%
  rowwise() %>%
  mutate(newsOutlet =
    ifelse(grepl('New York Times',Publication.title)==T,'New York Times',
    ifelse(grepl('Washington Post',Publication.title)==T,'Washington Post',
    ifelse(grepl('Wall Street Journal',Publication.title)==T,'Wall Street Journal',
    ifelse(grepl('Los Angeles Times',Publication.title)==T,'Los Angeles Times',
    ifelse(grepl('Epoch Times',Publication.title)==T,'The Epoch Times',
    ifelse(grepl('Pittsburgh Post - Gazette',Publication.title)==T,'Pittsburgh Post - Gazette',
    ifelse(grepl('Philadelphia Inquirer',Publication.title)==T,'Philadelphia Inquirer',
    ifelse(grepl('Philadelphia Tribune;',Publication.title)==T,'Philadelphia Tribune',
    ifelse(grepl('Chicago Tribune',Publication.title)==T,'Chicago Tribune','Other')))))))))) %>%
  mutate(date=as.Date(publicationDate,format="%Y-%m-%d")) 

pdf('figureA9.pdf',height=5)
textListMajorNews %>% group_by(newsOutlet) %>% summarise(n=n()) %>%
  mutate(N=max(cumsum(n)),prop=n/N) %>%
  ggplot(aes(x=reorder(newsOutlet,prop),y=prop)) +
  geom_col() +
  labs(y='Relative frequency',x='') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.x=element_text(angle=45,hjust=1,vjust=1))
dev.off()

#_______________________________________________________________________________
# Figure A.10: ----
# Sentiment of text in a 10, 25, and 50 word window around the names of 
# congresswomen as well as the terms "women", "woman", "female" or the names of 
# congressmen as well as the terms "men", "man", "male" in a text corpus 
# containing 24039 articles in the ProQuest news database when searching for 
# "representation", "congress", "member of congress", "women", and "female".
#_______________________________________________________________________________
corpus <- corpus(data_newsArticles)
docvars(corpus,field="textNoPubDate") <- 
  paste(docvars(corpus,field="textNo"),
    docvars(corpus,field="publicationDate"),sep=';')

namesCongressWomen <- 
  data_namesCongressMember %>% 
  filter(gender==1) %>% select(-gender) %>% t %>% c %>% unique %>%
  append(c('women','woman','female',"women's","woman's"))
namesCongressMen <- 
  data_namesCongressMember %>% 
  filter(gender==2) %>% select(-gender) %>% t %>% c %>% unique %>%
  append(c('men','man','male',"men's","man's"))

toksWomen10 <- 
  dfm(tokens(corpus,remove_punct=T,remove_symbols=T,remove_numbers=T) %>% 
  tokens_keep(
    pattern = phrase(namesCongressWomen), 
    window = 10, padding = F, verbose = T))

toksWomen25 <- 
  dfm(tokens(corpus,remove_punct=T,remove_symbols=T,remove_numbers=T) %>% 
  tokens_keep(
    pattern = phrase(namesCongressWomen), 
    window = 25, padding = F, verbose = T))

toksWomen50 <- 
  dfm(tokens(corpus,remove_punct=T,remove_symbols=T,remove_numbers=T) %>% 
  tokens_keep(
    pattern = phrase(namesCongressWomen),  
    window = 50, padding = F, verbose = T))

outWomen10 <- dfm_lookup(toksWomen10,data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups=textNoPubDate) %>%
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/20,
         date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
         gender='Women',
         window=10) %>%
  filter(date>='2016-09-01')

outWomen25 <- 
  dfm_lookup(toksWomen25,data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups=textNoPubDate) %>%
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
         date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
         gender='Women',
         window=25) %>%
  filter(date>='2016-09-01')

outWomen50 <- dfm_lookup(toksWomen50,data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/100,
         date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
         gender='Women',
         window=50) %>%
  filter(date>='2016-09-01')

toksMen10 <- dfm(tokens(corpus,remove_punct=T,remove_symbols=T,remove_numbers=T) %>% 
  tokens_keep(    
    pattern = phrase(namesCongressMen),       
    window = 10, padding = F, verbose = T))

toksMen25 <- dfm(tokens(corpus,remove_punct=T,remove_symbols=T,remove_numbers=T) %>% 
  tokens_keep(
    pattern = phrase(namesCongressMen), 
    window = 25, padding = F, verbose = T))

toksMen50 <- dfm(tokens(corpus,remove_punct=T,remove_symbols=T,remove_numbers=T) %>% 
  tokens_keep(
    pattern = phrase(namesCongressMen),  
    window = 50, padding = F, verbose = T))

outMen10 <- dfm_lookup(toksMen10,data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/20,
         date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
         gender='Men',
         window=10) %>%
  filter(date>='2016-09-01')

outMen25 <- dfm_lookup(toksMen25,data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
         date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
         gender='Men',
         window=25) %>%
  filter(date>='2016-09-01')

outMen50 <- dfm_lookup(toksMen50,data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/100,
         date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
         gender='Men',
         window=50) %>%
  filter(date>='2016-09-01')

out <- rbind(outMen10,outMen25,outMen50,outWomen10,outWomen25,outWomen50)

dates_vline <- c(as.Date("2016-11-08"),as.Date("2018-11-06"),
  as.Date("2020-11-03"))
dates_vline <- which(out$date %in% dates_vline)  

pdf('figureA10.pdf',height=5)
out %>% 
  mutate(
    window=factor(recode(window,`10`='10 Words',`25`='25 Words',`50`='50 Words'),
      levels=c('10 Words', '25 Words','50 Words')),
    gender=factor(gender),
    election=ifelse(date<='2017-01-31','2016',
      ifelse(date<='2019-01-31','2018','2020'))) %>%
  ggplot(aes(x=date,y=sentiment,color=gender,fill=gender,linetype=gender)) +
  facet_grid(window~election,scales='free_x') +
  geom_smooth() +
  geom_vline(xintercept=as.numeric(out$date[dates_vline]),color='gray') +
  geom_hline(aes(yintercept=0),color='gray') +
  scale_fill_manual(values=c('gray','black')) +
  scale_color_manual(values=c('gray','black')) +
  scale_linetype_manual(values=c("dotted", "solid")) +
  labs(x='',y='Sentiment') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()

#_______________________________________________________________________________
# # Figure A.11: ----
# Sentiment of text in a 25 word window around the names of 
# congresswomen as well as the terms "women", "woman", "female" or the names of 
# congressmen as well as the terms "men", "man", "male" in a text corpus 
# containing 24039 articles in the ProQuest news database when searching for 
# "representation", "congress", "member of congress", "women", and "female". 
# Results shown for three different affective words dictionaries not specifically
# developed for political texts: Affective Norms for English Words dictionary 
# (Nielsen, 2011), positive and negative words dictionary by Hu and Liu (2004), 
# and NRC Word-Emotion Association Lexicon (Mohammad and Turney, 2013).
#_______________________________________________________________________________
outWomenGening <- 
  dfm_lookup(toksWomen25,data_dictionary_geninqposneg) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
    date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
    gender='Women',
    dictionary='General Inquirer') %>%
  filter(date>='2016-09-01')

outWomenNRC <- 
  dfm_lookup(toksWomen25,data_dictionary_NRC[6:7]) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
    date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
    gender='Women',
    dictionary='NRC') %>%
  filter(date>='2016-09-01')

outWomenHuLiu <- 
  dfm_lookup(toksWomen25,data_dictionary_HuLiu) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
    date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
    gender='Women',
    dictionary='Hu and Liu') %>%
  filter(date>='2016-09-01')

outMenGening <- 
  dfm_lookup(toksMen25,data_dictionary_geninqposneg) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
    date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
    gender='Men',
    dictionary='General Inquirer') %>%
  filter(date>='2016-09-01')

outMenNRC <- dfm_lookup(toksMen25,data_dictionary_NRC[6:7]) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
    date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
    gender='Men',
    dictionary='NRC') %>%
  filter(date>='2016-09-01')

outMenHuLiu <- 
  dfm_lookup(toksMen25,data_dictionary_HuLiu) %>%
  dfm_group(groups=textNoPubDate) %>% 
  convert(., to = "data.frame") %>%
  mutate(sentiment=(positive-negative)/50,
    date=as.Date(str_sub(doc_id,-10,nchar(doc_id)),format="%Y-%m-%d"),
    gender='Men',
    dictionary='Hu and Liu') %>%
  filter(date>='2016-09-01')

out <- rbind(outMenGening,outMenNRC,outMenHuLiu,outWomenGening,outWomenNRC,
  outWomenHuLiu)

pdf('figureA11.pdf',height=5)
out %>% 
  mutate(
    gender=factor(gender),
    election=ifelse(date<='2017-01-31','2016',
      ifelse(date<='2019-01-31','2018','2020'))) %>%
  ggplot(aes(x=date,y=sentiment,color=gender,fill=gender)) +
  facet_grid(dictionary~election,scales='free_x') +
  geom_smooth() +
  geom_vline(xintercept=as.numeric(out$date[dates_vline]),color='gray') +
  geom_hline(aes(yintercept=0),color='gray') +
  scale_fill_manual(values=c('gray','black')) +
  scale_color_manual(values=c('gray','black')) +
  labs(x='',y='Sentiment') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()

#______________________________________________________________________________#
#______________________________________________________________________________#
# B Empirical analysis appendix ----
#______________________________________________________________________________#
# B.1 Summary statistics
#______________________________________________________________________________#
# Table B.1: ----
# Summary statistics – mean, standard deviation, minimum, and maximum or 
# frequencies, of outcome measures and manipulation check variables (N = 4,545)
#______________________________________________________________________________#
summary <-
  list(
    "Treatment" =
      list(
        "No information" = ~ n_perc(data_survey_women$treatment=='No information'),
        "No frame" = ~ n_perc(data_survey_women$treatment=='No frame'),
        "Negative"  = ~ n_perc(data_survey_women$treatment=="Negative"),
        "Positive"  = ~ n_perc(data_survey_women$treatment=="Positive")),
    "Guess female MOC" = list(
      "mean (sd)" = ~ mean_sd(data_survey_women$guess.female.MOC),
      "min" = ~ min(data_survey_women$guess.female.MOC),
      "max" = ~ max(data_survey_women$guess.female.MOC)),
    "Achievement or concern" =
      list(
      "Concern" = ~ n_perc(data_survey_women$concern=='Concern'&!is.na(data_survey_women$concern)),
      "Neither" = ~ n_perc(data_survey_women$concern=='Neither'&!is.na(data_survey_women$concern)),
      "Achievement" = ~ n_perc(data_survey_women$concern=='Achievement'&!is.na(data_survey_women$concern)),
      "No answer" = ~ n_perc(is.na(data_survey_women$concern))),
    "Self-efficacy" = list("mean (sd)" = ~ mean_sd(data_survey_women$self.efficacy[!is.na(data_survey_women$self.efficacy)]),
      "min" = ~ min(data_survey_women$self.efficacy[!is.na(data_survey_women$self.efficacy)]),
      "max" = ~ max(data_survey_women$self.efficacy[!is.na(data_survey_women$self.efficacy)])),
    "Self-esteem" = list("mean (sd)" = ~ mean_sd(data_survey_women$self.esteem[!is.na(data_survey_women$self.esteem)]),
      "min" = ~ min(data_survey_women$self.esteem[!is.na(data_survey_women$self.esteem)]),
      "max" = ~ max(data_survey_women$self.esteem[!is.na(data_survey_women$self.esteem)])),
    "Feelings towards women" = list("mean (sd)" = ~ mean_sd(data_survey_women$ftm.women[!is.na(data_survey_women$ftm.women)]),
      "min" = ~ min(data_survey_women$ftm.women[!is.na(data_survey_women$ftm.women)]),
      "max" = ~ max(data_survey_women$ftm.women[!is.na(data_survey_women$ftm.women)])),
    "Bias in feelings towards female celebrities" = list("mean (sd)" = ~ mean_sd(data_survey_women$ftm.celebrities),
      "min" = ~ min(data_survey_women$ftm.celebrities),
      "max" = ~ max(data_survey_women$ftm.celebrities)),
    "Willingness to apply" = list("mean (sd)" = ~ mean_sd(data_conjoint_women$likely.apply),
      "min" = ~ min(data_conjoint_women$likely.apply),
      "max" = ~ max(data_conjoint_women$likely.apply)),
    "Qualified for job" = list("mean (sd)" = ~ mean_sd(data_conjoint_women$qualify.job),
      "min" = ~ min(data_conjoint_women$qualify.job),
      "max" = ~ max(data_conjoint_women$qualify.job)),
    "Expected discrimination" = list("mean (sd)" = ~ mean_sd(data_survey_women$expect.discr),
      "min" = ~ min(data_survey_women$expect.discr),
      "max" = ~ max(data_survey_women$expect.discr)),
    "Rectify discrimination" = list("mean (sd)" = ~ mean_sd(data_survey_women$others.rectify),
      "min" = ~ min(data_survey_women$others.rectify),
      "max" = ~ max(data_survey_women$others.rectify)))

summary_table(data_survey_women,summary)

#______________________________________________________________________________#
# B.2 Main Results ----
#______________________________________________________________________________#
# Table B.2: ----
# Regression of outcome measures likely to apply on positive frame treatment 
# indicator pooled over Democrats and Republicans (see Figure 2). Standard 
# errors clustered at the respondent-level.
#______________________________________________________________________________#
m <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>% 
  droplevels() %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  group_by(value) %>% 
  do(m=lm_robust(likely.apply ~ treatment + pid, clusters= sid, data=.))

texreg(list(m$m[[1]],m$m[[2]],m$m[[3]],m$m[[4]],m$m[[5]],m$m[[6]],
  m$m[[7]],m$m[[8]],m$m[[9]],m$m[[10]],m$m[[11]]),
    custom.model.names=c('Admin Assistant','Assistant Manager',
      'Branch Manager','Executive Assistant','CEO',
      'Not [most competitive]','Most competitive','Not [highest pay]',
      'Highest pay','Education','Tech'),
    reorder.coef=c(2,3,1),
    custom.coef.names=c('(Intercept)','Positive frame','Republican'),
    digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),table=F,
    file='tableB2.tex') 

#______________________________________________________________________________#
# Table B.3: ----
# Regression of outcome measures likely to apply on positive frame treatment 
# indicator and partisanship (see Figure 3). Standard errors clustered at the 
# respondent-level.
#______________________________________________________________________________#
m <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>% 
  droplevels() %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  group_by(pid,value) %>% 
  do(m=lm_robust(likely.apply ~ treatment, clusters= sid, data=.))

texreg(list(m$m[[1]],m$m[[2]],m$m[[3]],m$m[[4]],m$m[[5]],m$m[[6]],
  m$m[[7]],m$m[[8]],m$m[[9]],m$m[[10]],m$m[[11]]),
    custom.model.names=c('Admin Assistant','Assistant Manager',
      'Branch Manager','Executive Assistant','CEO',
      'Not [most competitive]','Most competitive','Not [highest pay]',
      'Highest pay','Education','Tech'),
    reorder.coef=c(2,1),
    custom.coef.names=c('(Intercept)','Positive frame'),
    digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),table=F,
    file='tableB3_1.tex') 

texreg(list(m$m[[12]],m$m[[13]],m$m[[14]],m$m[[15]],m$m[[16]],m$m[[17]],
  m$m[[18]],m$m[[19]],m$m[[20]],m$m[[21]],m$m[[22]]),
  custom.model.names=c('Admin Assistant','Assistant Manager',
    'Branch Manager','Executive Assistant','CEO',
    'Not [most competitive]','Most competitive','Not [highest pay]',
    'Highest pay','Education','Tech'),
  reorder.coef=c(2,1),
  custom.coef.names=c('(Intercept)','Positive frame'),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),table=F,
  file='tableB3_2.tex') 

#______________________________________________________________________________#
# Table B.4: ----
# Regression of outcome measures qualify for job on positive frame treatment 
# indicator and partisanship (see Figure 4). Standard errors clustered at the 
# respondent-level.
#______________________________________________________________________________#
m <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>% 
  droplevels() %>%
  pivot_longer(cols=c(category.label,comp.label,high.pay.label,industry.label)) %>%
  group_by(pid,value) %>% 
  do(m=lm_robust(qualify.job ~ treatment, clusters= sid, data=.))

texreg(list(m$m[[1]],m$m[[2]],m$m[[3]],m$m[[4]],m$m[[5]],m$m[[6]],
  m$m[[7]],m$m[[8]],m$m[[9]],m$m[[10]],m$m[[11]]),
  custom.model.names=c('Admin Assistant','Assistant Manager',
    'Branch Manager','Executive Assistant','CEO',
    'Not [most competitive]','Most competitive','Not [highest pay]',
    'Highest pay','Education','Tech'),
  reorder.coef=c(2,1),
  custom.coef.names=c('(Intercept)','Positive frame'),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),table=F,
  file='tableB4_1.tex')

texreg(list(m$m[[12]],m$m[[13]],m$m[[14]],m$m[[15]],m$m[[16]],m$m[[17]],
  m$m[[18]],m$m[[19]],m$m[[20]],m$m[[21]],m$m[[22]]),
  custom.model.names=c('Admin Assistant','Assistant Manager',
    'Branch Manager','Executive Assistant','CEO',
    'Not [most competitive]','Most competitive','Not [highest pay]',
    'Highest pay','Education','Tech'),
  reorder.coef=c(2,1),
  custom.coef.names=c('(Intercept)','Positive frame'),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),table=F,
  file='tableB4_2.tex') 

#______________________________________________________________________________#
# Figure B.1: ----
# Average framing treatment effects on marginal mean of likely to apply by job 
# attribute. We show 90% (thin line) and 95% (bold line) confidence bounds 
# around the means.
#_______________________________________________________________________________
r <- data_survey_women %>%
  filter((treatment=='Positive'|treatment=='Negative')&pid!='Other') %>%
  mutate(across(ftm.celebrities,
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>% 
  group_by(pid) %>%
  do(lm_robust(ftm.celebrities~treatment,data=.) %>% tidy()) %>%
  filter(grepl('treatment',term)==T)

pdf('figureB1.pdf',height=2)
r %>% 
  mutate(l2=estimate-std.error*1.64,u2=estimate+std.error*1.64) %>%
  ggplot(aes(y=estimate,x=1,ymin=conf.low,ymax=conf.high,fill=pid,color=pid)) +
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  coord_flip() +  
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  labs(y='Difference in means',x='') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
      axis.text.y=element_blank(),axis.ticks.y=element_blank())
dev.off()

#______________________________________________________________________________#
# Figure B.2: ----
# Proportion of nobody, male politician, female politician mentioned who 
# inspired recently by Treatment. We show 90% (thin line) and 95% (bold line) 
# bootstrapped confidence bounds around the differences in means. 
#_______________________________________________________________________________
r <- 
  map(bootstraps(
    data_survey_women %>% 
      filter((treatment=='Positive'|treatment=='Negative')&pid!='Other'), 
    times=1000)$splits, ~as_tibble(.) %>% 
      group_by(pid,femaleMocInspire,treatment) %>% 
      summarise(n=n()) %>% 
      mutate(N=max(cumsum(n)),prop=n/N,ate=prop-lag(prop)) %>%
      filter(!is.na(ate))) %>%
  bind_rows(.id = 'boots') %>% group_by(treatment,pid,femaleMocInspire) %>%
  summarise(y=mean(ate),l=quantile(ate,0.025),u=quantile(ate,0.975),
    l2=quantile(ate,0.05),u2=quantile(ate,0.95))

pdf('figureB2.pdf',height=3)
r %>% 
  mutate(femaleMocInspire=factor(recode(femaleMocInspire,`0`='Nobody',
    `1`='Woman',`2`='Man'),levels=c('Nobody','Man','Woman'))) %>%
  ggplot(aes(x=1,y=y,ymin=l,ymax=u,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  facet_grid(femaleMocInspire~.) +
  geom_pointrange(position=position_dodge(width=.5),fatten=.5,size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),size=.3,shape=21) + 
  coord_flip() + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) + 
  labs(x='',y='Difference in share of respondents naming ... ') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_blank(),axis.ticks.y=element_blank())
dev.off()

#______________________________________________________________________________#
# Figure B.3: ----
# Framing effect on marginal means of likely to apply by job attribute, partisan 
# identity, and a median split of perception of group marginalization.                                                  line) condence bounds around the means.
#_______________________________________________________________________________
out <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  droplevels() %>%
  mutate(groupMarginal=factor(ifelse(group.marginal<=median(group.marginal,na.rm=T),
    'Feeling more marginalized','Feeling less marginalized')))

p1 <- out %>%
  group_by(groupMarginal,pid) %>%
  do(lm_robust(likely.apply~treatment,data=.,clusters=sid) %>%
    tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high,color=pid,
    fill=pid)) + 
  facet_grid(.~groupMarginal) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-13,18)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"),
    legend.position='None')

p2 <- out %>%
  group_by(groupMarginal,pid) %>% 
  do(cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~treatment)) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  filter(pid!='Other') %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=pid,fill=pid)) + 
  facet_grid(.~groupMarginal) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-13,18)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figureB3.pdf',height=6)
grid.arrange(p1,p2,heights=c(.15,1))
dev.off()

#______________________________________________________________________________#
# Figure B.4: ----
# Distribution of employment status in our sample                                                                                  line) condence bounds around the means.
#_______________________________________________________________________________
pdf('figureB4.pdf', height=2) 
data_survey_women %>%
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>%
  mutate(employed=ifelse(work=='Working now','Employed',
    ifelse(work=='Temporarly laid off'|work=='Unemployed'|work=='Student','Seeking employment',
    ifelse(work=='Homemaker','Homemaker',NA)))) %>% 
  filter(is.na(employed)==F) %>%
  group_by(pid,employed) %>% summarise(n=n()) %>% 
  mutate(N = max(cumsum(n)), prop = n/N) %>%
  ggplot(aes(y=prop,x=employed,color=employed,fill=employed)) +
  geom_col() +
  facet_grid(~pid) +
  scale_color_manual(values=c('black','black','gray')) + 
  scale_fill_manual(values=c('black','white','gray')) + 
  labs(x = '', y = 'Relative frequency') +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.spacing.x = unit(.25, 'cm'),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dev.off()

#_______________________________________________________________________________
# Figure B.5: ----
# Framing effect on marginal mean of willingness to apply by job attribute, 
# partisan identity, and employment status.                                                                                     line) condence bounds around the means.
#_______________________________________________________________________________
out <- data_conjoint_women %>% 
  filter((treatment=='Negative'|treatment=='Positive')&pid!='Other') %>% 
  droplevels() %>%
  mutate(employed=ifelse(work=='Working now','Employed',
    ifelse(work=='Temporarly laid off'|work=='Unemployed'|work=='Student',
    'Seeking employment',ifelse(work=='Homemaker','Homemaker',NA)))) %>%
  filter(!is.na(employed))

p1 <- out %>%
  group_by(employed,pid) %>%
  do(lm_robust(likely.apply~treatment,data=.,clusters=sid) %>%
    tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high,color=pid,
    fill=pid)) + 
  facet_grid(.~employed) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-27,43)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"),
    legend.position='None')

p2 <- out %>%
  group_by(employed,pid) %>% 
  do(cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~treatment)) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  filter(pid!='Other') %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=pid,fill=pid)) + 
  facet_grid(.~employed) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-27,43)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figureB5.pdf',height=5)
grid.arrange(p1,p2,heights=c(.15,1))
dev.off()

#______________________________________________________________________________#
# B.3 Main results including independents ----
#______________________________________________________________________________#
# Figure B.6 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of willingness to apply overall and by job attribute.                                                                                 
#_______________________________________________________________________________
p1 <- data_conjoint_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>%
  droplevels() %>%
  lm_robust(likely.apply~treatment + pid,data=.,clusters=sid) %>%
  tidy() %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),linewidth=1,size=.5) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"))

p2 <- data_conjoint_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>%
  droplevels() %>%
  cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~treatment) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figureB6.pdf',height=3)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

#_______________________________________________________________________________
# Figure B.7 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of willingness to apply overall and by job attribute and 
# partisan identity.                                                                                 
#_______________________________________________________________________________
p1 <- data_conjoint_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>%
  droplevels() %>%
  group_by(pid) %>%
  do(lm_robust(likely.apply~treatment,data=.,clusters=sid) %>%
    tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','gray','red')) +
  scale_fill_manual(values=c('white','gray','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"),
    legend.position='None')

p2 <- data_conjoint_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>% 
  droplevels() %>%
  group_by(pid) %>%
  do(cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~treatment)) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','gray','red')) +
  scale_fill_manual(values=c('white','gray','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figureB7.pdf',height=5)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

#______________________________________________________________________________#
# Figure B.8 ----
# Framing effect on attitudinal outcome measures by partisanship.
#______________________________________________________________________________#
pdf('figureB8.pdf',height=2)
data_survey_women %>% 
  mutate(across(c(self.efficacy,self.esteem,ftm.women),
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>% 
  group_by(treatment,pid,sid) %>% 
  summarise(across(c(self.efficacy,self.esteem,ftm.women),
    ~mean(.x,na.rm=T))) %>%
  select(-sid) %>%
  filter(treatment=='Negative'|treatment=='Positive') %>% 
  droplevels() %>%
  pivot_longer(cols=-c(treatment,pid)) %>%
  group_by(pid,name) %>%
  do(lm_robust(value~treatment,data=.) %>% tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(
    name=factor(recode(name,
      'self.esteem'='Self-esteem',
      'self.efficacy'='Self-efficacy',
      'ftm.women'='Feelings towards\nwomen'))) %>% 
  group_by(name) %>%
  mutate(l2=estimate-std.error*1.64,u2=estimate+std.error*1.64) %>%
  ggplot(aes(y=estimate,x=name,ymin=conf.low,ymax=conf.high,color=pid,
    fill=pid)) +
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.3),size=.3,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.3),
    linewidth=1,size=.3,shape=21) +
  scale_color_manual(values=c('blue','gray','red')) +
  scale_fill_manual(values=c('white','gray','red')) +
  coord_flip() +
  labs(y='Difference in means',x='') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.9 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of feeling qualified for job overall and by attribute and 
# partisanship.
#_______________________________________________________________________________
p1 <- data_conjoint_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>%
  droplevels() %>%
  lm_robust(qualify.job~treatment + pid,data=.,clusters=sid) %>%
  tidy() %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),linewidth=1,size=.5) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,10)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"))

p2 <- data_conjoint_women %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>% 
  droplevels() %>%
  group_by(pid) %>%
  do(cj(.,qualify.job ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~treatment)) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.5),
    linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','gray','red')) +
  scale_fill_manual(values=c('white','gray','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-10,10)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figureB9.pdf',height=5)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

#______________________________________________________________________________#
# Figure B.10: ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on expectations of discrimination and expectation that others will rectify 
# discrimination by partisanship.                                                                                  line) condence bounds around the means.
#_______________________________________________________________________________
out <- data_survey_women %>%
  mutate(across(c(expect.discr,others.rectify),
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>%
  select(treatment,pid,expect.discr,others.rectify)

r <- out %>% 
  filter(treatment=='Negative'|treatment=='Positive') %>%
  pivot_longer(cols=-c(treatment,pid)) %>%
  group_by(name,pid) %>%
  do(lm_robust(value~treatment,data=.) %>% tidy()) %>%
  filter(grepl('treatment',term)==T) %>%
  mutate(
    name=factor(recode(name,
      'expect.discr'='Expected\ndiscrimination',
      'others.rectify'='Rectify\ndiscrimination'),
      levels=c('Expected\ndiscrimination','Rectify\ndiscrimination')))

pdf('figureB10.pdf',height=2)
r %>% 
  mutate(l2=estimate-std.error*1.64,u2=estimate+std.error*1.64) %>%
  ggplot(aes(y=estimate,x=name,ymin=conf.low,ymax=conf.high,
             fill=pid,color=pid)) +
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.5),
                  linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','gray','red')) +
  scale_fill_manual(values=c('white','gray','red')) +
  coord_flip() +
  labs(y='Difference in means',x='') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()

#______________________________________________________________________________#
# B.4 Men study ----
#______________________________________________________________________________#
# B.4.1 Sample characteristics and treatment condition balance ----
#______________________________________________________________________________#
# Table B.5: ----
# Relative frequencies in our sample vs. ANES 2020
#______________________________________________________________________________#
out <- data_survey_men %>% 
  select(pid,race,education.num,income.num,age) %>% 
  mutate(
    age=ifelse(age<=29,'18 to 29',ifelse(age>29&age<=39,'30 to 39',
      ifelse(age>39&age<=49,'40 to 49',ifelse(age>49&age<=69,'50 to 69',
      '70 and over')))),
    education=ifelse(education.num<=2,'Less than high school diploma',
      ifelse(education.num==3,'High school diploma or equivalent',
      ifelse(education.num>3&education.num<=5,'Some college',
      ifelse(education.num==6,"Bachelor's degree",
      ifelse(education.num>6&education.num<=8,'Graduate degree',
      ifelse(education.num==9,'Professional degree',NA)))))),
    income=ifelse(income.num<=5,'$1,000 to $14,999',
      ifelse(income.num>5&income.num<=7,'$15,000 to $24,999',
      ifelse(income.num>7&income.num<=9,'$25,000 to $34,999',
      ifelse(income.num>9&income.num<=11,'$35,000 to $49,999',
      ifelse(income.num>11&income.num<=13,'$50,000 to $74,999',
      ifelse(income.num>13&income.num<=15,'$75,000 to $99,999',
      ifelse(income.num==16,'$100,000 to $124,999',
      ifelse(income.num==17,'$125,000 to $149,999',
      ifelse(income.num==18,'$150,000 to $174,999',
      ifelse(income.num>18,'$175,000 or more',NA))))))))))) %>%
  select(pid,race,education,income,age) %>% 
  gather(variable,value,race:age) %>%
  group_by(pid,variable,value) %>%
  summarise(n=n()) %>% mutate(N = max(cumsum(n)), freq = n/N) %>%
  filter(!is.na(value)&value!='Prefer not to say') %>%
  spread(pid,freq) %>% group_by(variable,value) %>%
  summarise_all(mean,na.rm=T) %>%
  select(-c(n,N)) %>%
  as.matrix()

t <- cbind(out[,2],out[,3],out[,4])
p <- xtable(t)
names(p) <- c("","Democrat","Republican")

print(xtable(p,type='latex',align="rrll"),file='tableB5.tex',
  include.rownames=F,digits=c(0,0,2,2))

#______________________________________________________________________________#
# Table B.6: ----
# Means and standard deviation of pre-treatment variables by treatment condition. 
# Difference-in-means and difference-in-distribution tests of pre-treatment 
# variables for Positive vs Negative treatment.
#______________________________________________________________________________#
var.list <- c('age','highSchoolOrLess','someCollege','bachelors',
  'graduateDegree','income','race','turnout12','turnout16',
  'employed','unemployed','homemaker','democrat','republican','ideology',
  'femininity','modern.sexism','women.role','polKnowledge',
  'women.role.politics','ftm.women.liberation','col.esteem',
  'change.unfair.laws','job.discriminated','life.affected',
  'group.marginal','guess.MOC')

out.0 <- data_survey_men %>% 
  mutate(
    highSchoolOrLess=ifelse(as.numeric(education)<=3,1,0),
    someCollege=ifelse(as.numeric(education)>3&as.numeric(education)<=5,1,0),
    bachelors=ifelse(as.numeric(education)==6,1,0),
    graduateDegree=ifelse(as.numeric(education)>6,1,0),
    race=ifelse(race=='Non-Hispanic white',1,0),
    turnout12=ifelse(turnout12=='Yes',1,ifelse(turnout12=='No',0,NA)),
    turnout16=ifelse(turnout16=='Yes',1,ifelse(turnout16=='No',0,NA)),
    employed=ifelse(work=='Working now',1,0),
    unemployed=ifelse(work=='Temporarily laid off'|work=='Unemployed',1,0),
    homemaker=ifelse(work=='Homemaker',1,0),
    democrat=ifelse(pid=='Democrat',1,0),
    republican=ifelse(pid=='Republican',1,0),
    ideology=ifelse(ideology!='Prefer not to say',as.numeric(ideology),NA),
    income=as.numeric(income)) %>%
  select(var.list,frame.treat) %>%
  pivot_longer(cols=var.list,names_to='variable')

out.ttest <- out.0 %>% 
  group_by(variable) %>% 
  do(tidy(t.test(value~frame.treat,data=.))) 

out.rankSum <- out.0 %>% 
  group_by(variable) %>% 
  do(tidy(wilcox.test(value~frame.treat,data=.))) 

out <- out.0 %>% 
  group_by(variable,frame.treat) %>% 
  summarise_all(list(mean=~mean(.,na.rm=T),sd=~sd(.,na.rm=T))) %>%
  pivot_wider(id_cols=variable,names_from=frame.treat,values_from=c(mean,sd)) %>%
  select(variable,`mean_Negative`,`sd_Negative`,`mean_Positive`,`sd_Positive`) %>%
  left_join(out.ttest %>% 
    select(variable,estimate,p.value) %>%
    mutate(estimate=(-1)*estimate)) %>%
  left_join(out.rankSum %>% 
    select(variable,p.value) %>%
    rename(p.value.rankSum=p.value)) %>%
  as.matrix()

r1 <- c("","Negative","","Positive","","Difference-in-means-tests","",
  "Difference-in-distribution tests") %>% as.matrix()
r2 <- c("Variable","Mean","(SD)","Mean","(SD)","Diff","(p)","p") %>% as.matrix()
c <- c("Age","Share white","Share up to hs degree","Share some college",
       "Share bachelors degree","Share graduate degree","Income level",
       "Share employed","Share unemployed","Share homemaker",
       "Turnout rate 2012","Turnout rate 2016","Share Democrats",
       "Share Republicans","Ideology","Political knowledge",
       "Change unfair laws","Collective esteem","Femininity",
       "FTM women liberation","Group marginalized","Women discriminated",
       "Life affected","Modern sexism scale","Women equal role",
       "Women role in politics","Guess # MOC")
t <- rbind(t(r2),cbind(c,rbind(
  out[1,],out[20,],out[12,],out[22,],out[2,],out[9,],out[15,],out[6,],
  out[25,],out[13,],out[23,],out[24,],out[5,],out[21,],out[14,],out[19,],
  out[3,],out[4,],out[7,],out[8,],out[10,],out[16,],out[17,],out[18,],
  out[26,],out[27,],out[11,])[,-1]))
p <- xtable(t,type='latex',align="rrlllllll") 
names(p) <- r1
print(xtable(p),file='tableB6.tex',include.rownames=F)

#______________________________________________________________________________#
# B.4.2 Manipulation check results ----
#______________________________________________________________________________#
# Figure B.11 ----
# Distribution of guesses about the number of female members of Congress by 
# treatment and party identity
#______________________________________________________________________________#
pdf('figureB11.pdf',height=3)
data_survey_men %>% 
  ggplot(aes(x=guess.female.MOC,color=pid,fill=pid)) +
  geom_density(alpha=.4) +
  facet_grid(~frame.treat) +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('blue','red')) +
  scale_x_continuous(limits=c(0,210),breaks=c(0,50,100,127,150,200)) +
  labs(y='Guess number of female MOC',x='') +
  geom_vline(aes(xintercept=127),color='black') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
        legend.spacing.x = unit(.25,'cm'),axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
dev.off()

#______________________________________________________________________________#
# B.4.3 Summary statistics ----
#______________________________________________________________________________#
# Table B.7: ----
# Summary statistics – mean, standard deviation, minimum, and maximum or 
# frequencies, of outcome measures and manipulation check vari2,082)
#______________________________________________________________________________#
summary <-
  list(
    "Treatment" =
      list(
        "Negative"  = ~ n_perc(data_survey_men$frame.treat=="Negative"),
        "Positive"  = ~ n_perc(data_survey_men$frame.treat=="Positive")),
    "Guess female MOC" = list(
      "mean (sd)" = ~ mean_sd(data_survey_men$guess.female.MOC),
      "min" = ~ min(data_survey_men$guess.female.MOC),
      "max" = ~ max(data_survey_men$guess.female.MOC)),
    "Achievement or concern" =
      list(
        "Concern" = ~ n_perc(data_survey_men$concern=='Concern'&!is.na(data_survey_men$concern)),
        "Neither" = ~ n_perc(data_survey_men$concern=='Neither'&!is.na(data_survey_men$concern)),
        "Achievement" = ~ n_perc(data_survey_men$concern=='Achievement'&!is.na(data_survey_men$concern)),
        "No answer" = ~ n_perc(is.na(data_survey_men$concern))),
    "Self-efficacy" = list("mean (sd)" = ~ mean_sd(data_survey_men$self.efficacy[!is.na(data_survey_men$self.efficacy)]),
        "min" = ~ min(data_survey_men$self.efficacy[!is.na(data_survey_men$self.efficacy)]),
        "max" = ~ max(data_survey_men$self.efficacy[!is.na(data_survey_men$self.efficacy)])),
    "Self-esteem" = list("mean (sd)" = ~ mean_sd(data_survey_men$self.esteem[!is.na(data_survey_men$self.esteem)]),
        "min" = ~ min(data_survey_men$self.esteem[!is.na(data_survey_men$self.esteem)]),
        "max" = ~ max(data_survey_men$self.esteem[!is.na(data_survey_men$self.esteem)])),
    "Feelings towards women" = list("mean (sd)" = ~ mean_sd(data_survey_men$ftm.women[!is.na(data_survey_men$ftm.women)]),
        "min" = ~ min(data_survey_men$ftm.women[!is.na(data_survey_men$ftm.women)]),
        "max" = ~ max(data_survey_men$ftm.women[!is.na(data_survey_men$ftm.women)])),
    "Bias in feelings towards female celebrities" = list("mean (sd)" = ~ mean_sd(data_survey_men$ftm.celebrities),
        "min" = ~ min(data_survey_men$ftm.celebrities),
        "max" = ~ max(data_survey_men$ftm.celebrities)),
    "Willingness to apply" = list("mean (sd)" = ~ mean_sd(data_conjoint_men$likely.apply),
        "min" = ~ min(data_conjoint_men$likely.apply),
        "max" = ~ max(data_conjoint_men$likely.apply)),
    "Qualified for job" = list("mean (sd)" = ~ mean_sd(data_conjoint_men$qualify.job),
        "min" = ~ min(data_conjoint_men$qualify.job),
        "max" = ~ max(data_conjoint_men$qualify.job)),
    "Expected discrimination" = list("mean (sd)" = ~ mean_sd(data_survey_men$expect.discr),
        "min" = ~ min(data_survey_men$expect.discr),
        "max" = ~ max(data_survey_men$expect.discr)),
    "Rectify discrimination" = list("mean (sd)" = ~ mean_sd(data_survey_men$others.rectify),
        "min" = ~ min(data_survey_men$others.rectify),
        "max" = ~ max(data_survey_men$others.rectify)))

summary_table(data_survey_men,summary)

#_______________________________________________________________________________
# B.4.4 Main results ----
#_______________________________________________________________________________
# Figure B.12 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of willingness to apply overall and by job attribute.                                                                                 
#_______________________________________________________________________________
p1 <- data_conjoint_men %>% 
  lm_robust(likely.apply~frame.treat + pid,data=.,clusters=sid) %>%
  tidy() %>%
  filter(grepl('frame.treat',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),linewidth=1,size=.5) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"))

p2 <- data_conjoint_men %>% 
  cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~frame.treat) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figureB12.pdf',height=3)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

#_______________________________________________________________________________
# Figure B.13 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of willingness to apply overall and by job attribute and 
# partisan identity.                                                                                 
#_______________________________________________________________________________
p1 <- data_conjoint_men %>% 
  group_by(pid) %>%
  do(lm_robust(likely.apply~frame.treat,data=.,clusters=sid) %>%
    tidy()) %>%
  filter(grepl('frame.treat',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-8,11)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"),
    legend.position='None')

p2 <- data_conjoint_men %>% 
  group_by(pid) %>%
  do(cj(.,likely.apply ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~frame.treat)) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),
    position=position_dodge(width=.5),linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-8,12)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figureB13.pdf',height=5)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

#______________________________________________________________________________#
# Figure B.14 ----
# Framing effect (average treatment effect positive vs negative frame treatment)
# on attitudinal outcome measures by partisanship.
#______________________________________________________________________________#
pdf('figureB14.pdf',height=2)
data_conjoint_men %>% 
  mutate(across(c(self.efficacy,self.esteem,ftm.women),
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>% 
  group_by(frame.treat,pid,sid) %>% 
  summarise(across(c(self.efficacy,self.esteem,ftm.women),
    ~mean(.x,na.rm=T))) %>%
  select(-sid) %>%
  pivot_longer(cols=-c(frame.treat,pid)) %>%
  group_by(pid,name) %>%
  do(lm_robust(value~frame.treat,data=.) %>% tidy()) %>%
  filter(grepl('frame.treat',term)==T) %>%
  mutate(
    name=factor(recode(name,
      'self.esteem'='Self-esteem',
      'self.efficacy'='Self-efficacy',
      'ftm.women'='Feelings towards\nwomen'))) %>% 
  group_by(name) %>%
  mutate(l2=estimate-std.error*1.64,u2=estimate+std.error*1.64) %>%
  ggplot(aes(y=estimate,x=name,ymin=conf.low,ymax=conf.high,color=pid,
    fill=pid)) +
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.3),size=.3,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.3),
    linewidth=1,size=.3,shape=21) +
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() +
  labs(y='Difference in means',x='') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.15 ----
# Framing effect (average treatment effect positive vs negative frame treatment) 
# on marginal mean of feeling qualified for job overall and by attribute and 
# partisanship.
#_______________________________________________________________________________
p1 <- data_conjoint_men %>% 
  lm_robust(qualify.job~frame.treat + pid,data=.,clusters=sid) %>%
  tidy() %>%
  filter(grepl('frame.treat',term)==T) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>%
  ggplot(aes(x='Overall',y=estimate,ymin=conf.low,ymax=conf.high)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),linewidth=1,size=.5) + 
  coord_flip() + 
  scale_y_continuous(limits=c(-8,12)) + 
  labs(x='',y='') + 
  theme_bw() +
  theme(axis.title.x=element_blank(),axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),plot.margin=unit(c(.2,.2,-.1,2.3),"cm"))

p2 <- data_conjoint_men %>% 
  group_by(pid) %>%
  do(cj(.,qualify.job ~ category.label + comp.label + high.pay.label + 
    industry.label,id=~sid,estimate='mm_differences',by=~frame.treat)) %>%
  mutate(l2=estimate-1.64*std.error,u2=estimate+1.64*std.error) %>% 
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=pid,fill=pid)) + 
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2,shape=21) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.5),
    linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() + 
  scale_y_continuous(limits=c(-8,12)) + 
  labs(x='',y='Difference in marginal means') + 
  theme_bw() + 
  theme(legend.position='bottom',legend.title=element_blank())

pdf('figureB15.pdf',height=5)
grid.arrange(p1,p2,heights=c(.1,1))
dev.off()

#______________________________________________________________________________#
# Figure B.16: ----
# Mean of expectations of discrimination and expectation that others will 
# rectify discrimination.                                                                                    line) condence bounds around the means.
#_______________________________________________________________________________
out <- data_survey_men %>%
  mutate(across(c(expect.discr,others.rectify),
    ~(.x-min(.x,na.rm=T))/(max(.x,na.rm=T)-min(.x,na.rm=T)))) %>%
  select(frame.treat,pid,expect.discr,others.rectify)

r <- out %>% 
  pivot_longer(cols=-c(frame.treat,pid)) %>%
  group_by(name,pid) %>%
  do(lm_robust(value~frame.treat,data=.) %>% tidy()) %>%
  filter(grepl('frame.treat',term)==T) %>%
  mutate(
    name=factor(recode(name,
      'expect.discr'='Expected\ndiscrimination',
      'others.rectify'='Rectify\ndiscrimination'),
    levels=c('Expected\ndiscrimination','Rectify\ndiscrimination')))

pdf('figureB16.pdf',height=2)
r %>% 
  mutate(l2=estimate-std.error*1.64,u2=estimate+std.error*1.64) %>%
  ggplot(aes(y=estimate,x=name,ymin=conf.low,ymax=conf.high,
    fill=pid,color=pid)) +
  geom_hline(aes(yintercept=0),color='gray') +
  geom_pointrange(position=position_dodge(width=.5),size=.2) + 
  geom_pointrange(aes(ymin=l2,ymax=u2),position=position_dodge(width=.5),
    linewidth=1,size=.5,shape=21) + 
  scale_color_manual(values=c('blue','red')) +
  scale_fill_manual(values=c('white','red')) +
  coord_flip() +
  labs(y='Difference in means',x='') +
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank())
dev.off()
