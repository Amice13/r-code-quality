# ---------------------------
# R version: 4.0.5
# Date: 5/12/2021
# Title: 'Where is Presidential Power? Measuring Presidential Discretion with Experts'
# Authors: Kenneth Lowande and Charles R. Shipan
# Summary: Replication code for producing all tables and figures. 
# Please report errors to: lowande@umich.edu
# ---------------------------

rm(list=ls())

# Packages
require(doBy)
require(eba)
require(foreign)
require(ggplot2)
require(ggpubr)
require(gridExtra)
require(knitr)
require(stringr)
require(xtable)
options(stringsAsFactors=F)
Sys.setenv(TEXINPUTS=getwd(),
           BIBINPUTS=getwd(),
           BSTINPUTS=getwd())
# ---------------------------


# Figure 1: Expert estimates of presidential discretion
load('exp_est.Rda')
exp_bs=ggplot(exp_est) + geom_point(aes(x = topic_name, y = est, size= 2)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = topic_name, ymin = est-(1.645*se),ymax = est+(1.645*se)),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = topic_name, ymin = est-(1.96*se),ymax = est+(1.96*se)),lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() + xlab('') + ylab('Estimated Discretion') +
  scale_y_continuous(limits=c(-3,3.7)) +
  theme(legend.title=element_blank(),legend.position='none')
# ---------------------------

# Figure 2: The two presidencies thesis; Figure B7: The two presidencies thesis (according to legal scholars)
load('exp_est.Rda')
load('nonpolsci_est.Rda')

names(exp_est)[5]='Expert Rank'
names(nonpolsci_est)[5]='Law Rank'
exp_est$topic_name=trimws(exp_est$topic_name)
nonpolsci_est$topic_name=trimws(nonpolsci_est$topic_name)
nonpolsci_est$topic_name[nonpolsci_est$topic==39]=exp_est$topic_name[exp_est$topic==39]
ranks=merge(exp_est[,4:5],nonpolsci_est[,4:5],by='topic_name',all=T)

# select foreign policy issues
foreign=c(3,8,9,16,19,20,29,32,35,39,51)
ranks$`Policy Area`='Domestic'
ranks$`Policy Area`[foreign]='Foreign'
y=1:54

# Figure 2
a=ranks[,c(2,4)]
names(a)[1]='Rank'
twopres=ggplot(data=a,aes(x=`Policy Area`,y=Rank, fill=`Policy Area`)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme_bw() + scale_fill_manual(values=c("#00274c", "#d86018"))

# Figure B7
b=ranks[,c(3,4)]
names(b)[1]='Rank'
twopres_law=ggplot(data=b,aes(x=`Policy Area`,y=Rank, fill=`Policy Area`)) +
  geom_boxplot() + geom_jitter(width=0.1,alpha=0.2) +
  theme_bw() + scale_fill_manual(values=c("#00274c", "#d86018"))
# ---------------------------

# Table 1: Discretion is positively associated with presidential action; Figure 3: Presidential discretion and unilateral action by policy area, 1992-2018
load('actions.Rda')

# Table 1
tab_ua1=data.frame()
x=cor.test(dat$Pre,dat$RUM)
tab_ua1[1,'Data Source']='Comparative Agendas Project'
tab_ua1[1,'Outcome']='Pre-1992 Exec. Orders'
tab_ua1[1,'$r$']=round(x$estimate,2)
tab_ua1[1,'95\\% Conf. Int.']=paste('(',round(x$conf.int[1:2],2)[1],', ',round(x$conf.int[1:2],2)[2],')',sep='')

x=cor.test(dat$Post,dat$RUM)
tab_ua1[2,'Outcome']='Post-1992 Exec. Orders'
tab_ua1[2,'$r$']=round(x$estimate,2)
tab_ua1[2,'95\\% Conf. Int.']=paste('(',round(x$conf.int[1:2],2)[1],', ',round(x$conf.int[1:2],2)[2],')',sep='')

x=cor.test(dat$Total,dat$RUM)
tab_ua1[3,'Outcome']='All Exec. Orders'
tab_ua1[3,'$r$']=round(x$estimate,2)
tab_ua1[3,'95\\% Conf. Int.']=paste('(',round(x$conf.int[1:2],2)[1],', ',round(x$conf.int[1:2],2)[2],')',sep='')

x=cor.test(dat$sPre,dat$RUM)
tab_ua1[4,'Data Source']='Chiou and Rothenberg (2014)'
tab_ua1[4,'Outcome']='Pre-1992 Sig. Orders'
tab_ua1[4,'$r$']=round(x$estimate,2)
tab_ua1[4,'95\\% Conf. Int.']=paste('(',round(x$conf.int[1:2],2)[1],', ',round(x$conf.int[1:2],2)[2],')',sep='')

x=cor.test(dat$sPost,dat$RUM)
tab_ua1[5,'Outcome']='Post-1992 Sig. Orders'
tab_ua1[5,'$r$']=round(x$estimate,2)
tab_ua1[5,'95\\% Conf. Int.']=paste('(',round(x$conf.int[1:2],2)[1],', ',round(x$conf.int[1:2],2)[2],')',sep='')

x=cor.test(dat$sTotal,dat$RUM)
tab_ua1[6,'Outcome']='All Sig. Orders'
tab_ua1[6,'$r$']=round(x$estimate,2)
tab_ua1[6,'95\\% Conf. Int.']=paste('(',round(x$conf.int[1:2],2)[1],', ',round(x$conf.int[1:2],2)[2],')',sep='')

x=cor.test(dat$Lowande,dat$RUM)
tab_ua1[7,'Data Source']='Lowande (2019)'
tab_ua1[7,'Outcome']='Total Actions'
tab_ua1[7,'$r$']=round(x$estimate,2)
tab_ua1[7,'95\\% Conf. Int.']=paste('(',round(x$conf.int[1:2],2)[1],', ',round(x$conf.int[1:2],2)[2],')',sep='')

# Figure 3
F_UA_1=ggplot(dat,aes(x=RUM, y=Post)) + geom_point() + geom_smooth(colour='#00274c',method='loess',formula = 'y~x') + 
  xlab('Discretion (RUM)') + ylab('Executive Orders (CAP)') +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.background = element_blank()) 

F_UA_2=ggplot(dat,aes(x=RUM, y=sPost)) + geom_point() + geom_smooth(colour='#00274c',method='loess',formula = 'y~x') + 
  xlab('Discretion (RUM)') + ylab('Significant Orders') +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.background = element_blank()) 

F_UA_3=ggplot(dat,aes(x=RUM, y=Lowande)) + geom_point() + geom_smooth(colour='#00274c',method='loess',formula = 'y~x') + 
  xlab('Discretion (RUM)') + ylab('Unilateral Action') +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.background = element_blank()) 
# ---------------------------
