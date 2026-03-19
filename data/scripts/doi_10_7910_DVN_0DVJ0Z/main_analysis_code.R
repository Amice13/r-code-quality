setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set wd to source file location. For RStudio only
rm(list=ls())

library(tidyverse) 
library(openxlsx)
library(lubridate)
library(zoo)
library(ggplot2)
library(psych) 
library(tseries) 
library(mFilter) 
library(quantmod)
library(pracma)
library(dplyr)
library(forecast)
library(car) 
library(texreg) 
library(MASS) 
library(pastecs)
library(dotwhisker) 
library(margins)
library(clubSandwich) 
library(sandwich)
library(lfe)
library(fixest)
library(lmtest)
library(Hmisc)
library(xtable)
library(jtools)
library(ggpubr) 

options(max.print = 999999)
options(warn=-1)

########## IMPORT DATA

data_all<-read.xlsx('analysis_dataset.xlsx',1) 
data<-data_all %>% subset(volume>2) 

Sys.setlocale("LC_TIME", "C") # Language issue with zoo package for dates
data$month<-as.yearmon(data$month)

######### FIGURES WITH DV TRENDS #########

cbPalette <- c('#000000','#E69F00','#56B4E9','#009E73','#F0E442','#0072B2','#D55E00','#CC79A7')

data2<-data[!(data$newspaper=='Daily Mail' & data$month<as.yearmon(as.Date('1999/07/01'))),]

evalcov_fig<-ggplot(data2,aes(x=month,y=evaluative_ma,group=newspaper,color=newspaper))+scale_colour_manual(values=cbPalette)+geom_line()+theme_classic()+ylab('')+labs(color='')+
  ggtitle('Evaluative coverage')+theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5,size=20),axis.text=element_text(size=15),legend.text = element_text(size = 14))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

negativity_fig<-ggplot(data2,aes(x=month,y=negativity_ma,group=newspaper,color=newspaper))+scale_colour_manual(values=cbPalette)+geom_line()+theme_classic()+ylab('')+labs(color='')+
ggtitle('Negative coverage')+theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5,size=20),axis.text=element_text(size=15),legend.text = element_text(size = 14))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

panel1x2<-ggarrange(evalcov_fig,negativity_fig,ncol=1,nrow=2,widths=c(1,1,1,1),common.legend=TRUE, legend = c('bottom'))

######### MAIN MODELS #########

### EVALUATIVE COVERAGE ###

meval1<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 +
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
               factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 +
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
               rate_absch + qe_annc + bailouts + log_speeches + hearings + 
               log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg1<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
              abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
              factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
              abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
              rate_absch + qe_annc + bailouts + log_speeches + hearings + 
              log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(meval1,meval2,mneg1,mneg2),stars=c(0.01,0.05,0.1))

######### PRE AND POST MODELS #########

# EVALUATIVE PRE AND POST

meval_pre<-felm(sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi +  
                  rate_absch + log_speeches + hearings + 
                  log_allappnt + election + factor(month_dummy)+ factor(newspaper)|0|0|newspaper, data=data[data$pre_crisis==1,])

meval_post<-felm(sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                   abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi +  
                   rate_absch + + qe_annc + log_speeches + hearings + 
                   log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper, data=data[data$post_crisis==1,])

screenreg(list(meval2,meval_pre,meval_post),digits=3,stars = c(0.01,0.05,0.1))

# Coefficient plot

# Clean models 
cmeval_pre<-tidy(meval_pre) %>% filter(!grepl('factor*|brexit|election',term)) %>% mutate(model = "1997-2007")
cmeval_post<-tidy(meval_post) %>% filter(!grepl('factor|brexit|election',term)) %>% mutate(model = "2009-2020")

evaluative_models<-rbind(cmeval_post,cmeval_pre)

coefp_eval <- evaluative_models %>%
  by_2sd(data) %>%
  dwplot() %>%
  relabel_predictors(c(sent_negpos_pc_sum1='Evaluative (t-1)',
                       abs_dis_lag1='Distance inflation target (t-1)',
                       gbpusd_log="Abs. change exchange rate (logged)",
                       unempch_lag1='Change unemployment rate (t-1)',
                       ukfsi='Financial stress',
                       rate_absch='Abs. change bank rate',
                       qe_annc='QE announcement',
                       log_speeches='No. of speeches (logged)',
                       hearings='No. of hearings',
                       log_allappnt='No. of appointments (logged)')) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + xlab("Coefficient") + ylab("") + scale_x_continuous(limits = c(-2, 2)) +
  theme_bw()+theme(plot.title = element_text(size=20),axis.text=element_text(size=14),legend.title = element_blank(),legend.text = element_text(size = 14),
                   legend.position = 'bottom') + labs(title='Evaluative coverage') +
  scale_color_manual(values = c('#000000','#56B4E9'),guide = guide_legend(ncol=2,reverse=T))

# NEGATIVE PRE AND POST

mneg_pre<-felm(sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi +  
                 rate_absch + log_speeches + hearings + 
                 log_allappnt + election + factor(month_dummy)+ factor(newspaper)|0|0|newspaper, data=data[data$pre_crisis==1,])

mneg_post<-felm(sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                  abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + 
                  rate_absch + + qe_annc + log_speeches + hearings +  
                  log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper, data=data[data$post_crisis==1,])

screenreg(list(mneg2,mneg_pre,mneg_post),digits=3,stars = c(0.01,0.05,0.1))

# Coefficient plot

# Clean models 
cmneg_pre<-tidy(mneg_pre) %>% filter(!grepl('factor*|brexit|election',term)) %>% mutate(model = "1997-2007")
cmneg_post<-tidy(mneg_post) %>% filter(!grepl('factor|brexit|election',term)) %>% mutate(model = "2009-2020")

negativity_models<-rbind(cmneg_post,cmneg_pre)

coefp_neg <- negativity_models %>%
  by_2sd(data) %>%
  dwplot() %>%
  relabel_predictors(c(sent_negpos_pc_diff1='Negativity (t-1)',
                       abs_dis_lag1='Distance inflation target (t-1)',
                       gbpusd_log="Abs. change exchange rate (logged)",
                       unempch_lag1='Change unemployment rate (t-1)',
                       ukfsi='Financial stress',
                       rate_absch='Abs. change bank rate',
                       qe_annc='QE announcement',
                       log_speeches='No. of speeches (logged)',
                       hearings='No. of hearings',
                       log_allappnt='No. of appointments (logged)')) +  
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + xlab("Coefficient") + ylab("")  + scale_x_continuous(limits = c(-2, 2)) + 
  theme_bw()+theme(plot.title = element_text(size=20),axis.text=element_text(size=14),legend.title = element_blank(),legend.text = element_text(size = 14),
                   legend.position = 'bottom') + labs(title='Negativity') +
  scale_color_manual(values = c('#000000','#56B4E9'),guide = guide_legend(ncol=2,reverse=T))

panel_coef_1x2<-ggarrange(coefp_eval,coefp_neg,ncol=1,nrow=2,widths=c(1,1,1,1),common.legend=TRUE,legend = c('bottom'))

###################################################################
########## APPENDIX B: DESCRIPTIVE STATS AND CORRELATION ##########
###################################################################

str(data)

# Evaluative coverage
round(stat.desc(data$sent_negpos_pc_sum), 2)
# Negative coverage
round(stat.desc(data$sent_negpos_pc_diff), 2)
# Distance from target
round(stat.desc(data$abs_dis_targ), 2)
# Exchange rate change (original)
round(stat.desc(data$gbpusd_absch), 2)
# Exchange rate logged
round(stat.desc(data$gbpusd_log), 2)
# Unemployment change
round(stat.desc(data$unemp_ch), 2)
# Financial stress
round(stat.desc(data$ukfsi), 2)
# Post crisis dummy
round(stat.desc(data$post_crisis), 2)
# Rate change
round(stat.desc(data$rate_absch), 2)
# QE announcement
round(stat.desc(data$qe_annc), 2)
# Bank bailout
round(stat.desc(data$bailouts), 2)
# Hearings
round(stat.desc(data$hearings), 2)
# Speeches (original)
round(stat.desc(data$speeches), 2)
# Speeches (log-transformed)
round(stat.desc(data$log_speeches), 2)
# Appointments (original) (sum new + reappoint)
round(stat.desc(data$sum_appnt), 2)
# Appointments (log-transformed)
round(stat.desc(data$log_allappnt), 2)
# Elections
round(stat.desc(data$election), 2)
# Brexit
round(stat.desc(data$brexit), 2)

# Correlation matrix 
# ------------------

corstarsl <- function(x){
  require(Hmisc)
  x <- as.matrix(x)
  R <- rcorr(x)$r
  p <- rcorr(x)$P
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .01, "***", ifelse(p < .05, "** ", ifelse(p < .1, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their appropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew)
}

# Subsetting the relevant variables (in our main model)
names(data)
data_corr <- data[, c('abs_dis_lag1','gbpusd_log','unempch_lag1','ukfsi','post_crisis','rate_absch','qe_annc','bailouts','log_speeches','hearings','log_allappnt','election','brexit')]

corstarsl(data_corr)

corrmat <- xtable(corstarsl(data_corr), 
                  caption="Correlation matrix",
                  label="tab:corrmat", 
                  latex.environments="center")
print(corrmat)

# Correlation matrix of dvs by newspaper

# Evaluative coverage
eval_bynp<-as.data.frame(data_all[data_all$newspaper=='The Times',]$sent_negpos_pc_sum)
eval_bynp$dm<-as.data.frame(data_all[data_all$newspaper=='Daily Mail',]$sent_negpos_pc_sum)
eval_bynp$grdn<-as.data.frame(data_all[data_all$newspaper=='The Guardian',]$sent_negpos_pc_sum)
colnames(eval_bynp)<-c('eval_times','eval_dm','eval_grdn')

corrmat_eval_bynp <- xtable(corstarsl(eval_bynp),caption="Correlation matrix",label="tab:corrmat_eval_bynp",latex.environments="center")
corstarsl(eval_bynp)

# Negativity
neg_bynp<-as.data.frame(data_all[data_all$newspaper=='The Times',]$sent_negpos_pc_diff)
neg_bynp$neg_dm<-as.data.frame(data_all[data_all$newspaper=='Daily Mail',]$sent_negpos_pc_diff)
neg_bynp$neg_grdn<-as.data.frame(data_all[data_all$newspaper=='The Guardian',]$sent_negpos_pc_diff)
colnames(neg_bynp)<-c('neg_times','neg_dm','neg_grdn')

corrmat_neg_bynp <- xtable(corstarsl(neg_bynp),caption="Correlation matrix",label="tab:corrmat_neg_bynp",latex.environments="center")
corstarsl(neg_bynp)

# Correlation between coverage volume and the dvs by newspaper
times<-as.data.frame(data_all[data_all$newspaper=='The Times',]$volume)
times$eval<-as.data.frame(data_all[data_all$newspaper=='The Times',]$sent_negpos_pc_sum)
times$neg<-as.data.frame(data_all[data_all$newspaper=='The Times',]$sent_negpos_pc_diff)
colnames(times)<-c('volume','evalcov','negativity')
corstarsl(times)

dm<-as.data.frame(data_all[data_all$newspaper=='Daily Mail',]$volume)
dm$eval<-as.data.frame(data_all[data_all$newspaper=='Daily Mail',]$sent_negpos_pc_sum)
dm$neg<-as.data.frame(data_all[data_all$newspaper=='Daily Mail',]$sent_negpos_pc_diff)
colnames(dm)<-c('volume','evalcov','negativity')
corstarsl(dm)

grdn<-as.data.frame(data_all[data_all$newspaper=='The Guardian',]$volume)
grdn$eval<-as.data.frame(data_all[data_all$newspaper=='The Guardian',]$sent_negpos_pc_sum)
grdn$neg<-as.data.frame(data_all[data_all$newspaper=='The Guardian',]$sent_negpos_pc_diff)
colnames(grdn)<-c('volume','evalcov','negativity')
corstarsl(grdn)

#### STATIONARITY TESTS ####
library(plm)
library(tseries)

### EVALUATIVE ###

pdata<-data.frame(split(data_all$sent_negpos_pc_sum,data_all$newspaper))

# Im-Pesaran-Shin test (2003) - Ha: at least one series is stationary 
purtest(pdata,lags='AIC',exo='intercept',test='ips') 
purtest(pdata,lags='AIC',exo='trend',test='ips')

# Maddala-Wu test (1999) - Ha: at least one series is stationary
purtest(pdata,lags='AIC',exo='intercept',test='madwu') 
purtest(pdata,lags='AIC',exo='trend',test='madwu') 

# Levin-Lin-Chu test (2003) - Ha: all series are stationary 
purtest(pdata,lags='AIC',exo='intercept',test='levinlin')
purtest(pdata,lags='AIC',exo='trend',test='levinlin') 

### NEGATIVITY ###

pdata<-data.frame(split(data_all$sent_negpos_pc_diff,data_all$newspaper))

# Im-Pesaran-Shin test (2003) - Ha: at least one series is stationary 
purtest(pdata,lags='AIC',exo='intercept',test='ips') 
purtest(pdata,lags='AIC',exo='trend',test='ips')

# Maddala-Wu test (1999) - Ha: at least one series is stationary
purtest(pdata,lags='AIC',exo='intercept',test='madwu') 
purtest(pdata,lags='AIC',exo='trend',test='madwu') 

# Levin-Lin-Chu test (2003) - Ha: all series are stationary 
purtest(pdata,lags='AIC',exo='intercept',test='levinlin')
purtest(pdata,lags='AIC',exo='trend',test='levinlin') 

#################################################################
########## APPENDIX C: ALTERNATIVE OPERATIONALISATIONS ##########
#################################################################

# Absolute distance as squared term
# ---------------------------------

### EVALUATIVE COVERAGE ###

meval1sq<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  abs_dis_lag1 + abs_dis_sq_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                  factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2sq<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  abs_dis_lag1 + abs_dis_sq_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                  rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                  log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg1sq<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 abs_dis_lag1 + abs_dis_sq_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                 factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2sq<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 abs_dis_lag1 + abs_dis_sq_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                 rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                 log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(meval1sq,meval2sq,mneg1sq,mneg2sq))

# Letter dummy instead of distance from the target
# ------------------------------------------------

### EVALUATIVE COVERAGE ###

meval1let<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  letter_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                  factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2let<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  letter_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                  rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                  log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg1let<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 letter_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                 factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2let<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 letter_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                 rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                 log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(meval1let,meval2let,mneg1let,mneg2let))

# Inflation levels rather than distance from the target
# -----------------------------------------------------

### EVALUATIVE COVERAGE ###

meval1cpi<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  cpi_yoy_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                  factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2cpi<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  cpi_yoy_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                  rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                  log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg1cpi<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 cpi_yoy_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                 factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2cpi<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 cpi_yoy_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                 rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                 log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(meval1cpi,meval2cpi,mneg1cpi,mneg2cpi))

# Inflation report dummy instead of month dummy
#---------------------------------------------------

meval1rep<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
               infl_rep + factor(newspaper)|0|0|newspaper)

meval2rep<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 +
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
               rate_absch + qe_annc + bailouts + log_speeches + hearings + 
               log_allappnt + election + brexit + infl_rep + factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg1rep<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
              abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
              infl_rep + factor(newspaper)|0|0|newspaper)

mneg2rep<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
              abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
              rate_absch + qe_annc + bailouts + log_speeches + hearings + 
              log_allappnt + election + brexit + infl_rep + factor(newspaper)|0|0|newspaper)

# GDP growth instead of unemployment rate
# ---------------------------------------

### EVALUATIVE COVERAGE ###

meval1gdp<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
               abs_dis_lag1 + gbpusd_log + gdp_mom1 + ukfsi + post_crisis +
               factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2gdp<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  abs_dis_lag1 + gbpusd_log + gdp_mom1 + ukfsi + post_crisis + 
               rate_absch + qe_annc + bailouts + log_speeches + hearings + 
               log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg1gdp<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 abs_dis_lag1 + gbpusd_log + gdp_mom1 + ukfsi + post_crisis +
              factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2gdp<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 abs_dis_lag1 + gbpusd_log + gdp_mom1 + ukfsi + post_crisis + 
              rate_absch + qe_annc + bailouts + log_speeches + hearings + 
              log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

# Bank share prices rather than UKFSI
# -----------------------------------

### EVALUATIVE COVERAGE ###

meval1share<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                    abs_dis_lag1 + gbpusd_log + unempch_lag1 + share_ch_sm3 + post_crisis +
               factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2share<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                    abs_dis_lag1 + gbpusd_log + unempch_lag1 + share_ch_sm3 + post_crisis + 
               rate_absch + qe_annc + bailouts + log_speeches + hearings + 
               log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg1share<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                   abs_dis_lag1 + gbpusd_log + unempch_lag1 + share_ch_sm3 + post_crisis +
              factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2share<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                   abs_dis_lag1 + gbpusd_log + unempch_lag1 + share_ch_sm3 + post_crisis + 
              rate_absch + qe_annc + bailouts + log_speeches + hearings + 
              log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

# Any rate change (rather than absolute change)
# ---------------------------------------------

### EVALUATIVE COVERAGE ###

meval2rate<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                   abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
               rate_anych + qe_annc + bailouts + log_speeches + hearings + 
               log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg2rate<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                  abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
              rate_anych + qe_annc + bailouts + log_speeches + hearings + 
              log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

# All news announcements instead of speeches
# ------------------------------------------

### EVALUATIVE COVERAGE ###

meval2nws<-felm(data=data,sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
               rate_absch + qe_annc + bailouts + log_news + hearings + 
               log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

### NEGATIVITY ###

mneg2nws<-felm(data=data,sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
              rate_absch + qe_annc + bailouts + log_news + hearings + 
              log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

#########################################################
########## APPENDIX D: ANALYSIS BY NEWSPAPER ############
#########################################################

# EVALUATIVE COVERAGE MODELS
# --------------------------

mevaltm<-felm(data=data[data$newspaper=='The Times',],sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + rate_absch + qe_annc + bailouts + 
               log_speeches + hearings + log_allappnt + election + brexit +
               factor(month_dummy))

mevalgrdn<-felm(data=data[data$newspaper=='The Guardian',],sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                  abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + rate_absch + qe_annc + bailouts + 
                 log_speeches + hearings + log_allappnt + election + brexit +
                 factor(month_dummy))

mevaldm<-felm(data=data[data$newspaper=='Daily Mail',],sent_negpos_pc_sum ~ sent_negpos_pc_sum1 + 
                abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + rate_absch + qe_annc + bailouts + 
               log_speeches + hearings + log_allappnt + election + brexit +
               factor(month_dummy))

# NEGATIVITY MODELS
# -----------------

mnegtm<-felm(data=data[data$newspaper=='The Times',],sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + rate_absch + qe_annc + bailouts + 
               log_speeches + hearings + log_allappnt + election + brexit +
               factor(month_dummy))

mneggrdn<-felm(data=data[data$newspaper=='The Guardian',],sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
                 abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + rate_absch + qe_annc + bailouts + 
                 log_speeches + hearings + log_allappnt + election + brexit +
                 factor(month_dummy))

mnegdm<-felm(data=data[data$newspaper=='Daily Mail',],sent_negpos_pc_diff ~ sent_negpos_pc_diff1 + 
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + rate_absch + qe_annc + bailouts + 
               log_speeches + hearings + log_allappnt + election + brexit +
               factor(month_dummy))

# PLOTTING TIME SERIES
# --------------------

# Import determinants dataset
data_det<-read.xlsx('accountability_determinants.xlsx',1) 

#data_det$quarter<-NULL
Sys.setlocale("LC_TIME", "C") # Language issue with zoo package for dates
data_det$month<-as.yearmon(data_det$month)

# Outcome figure

# Distance from the inflation target
fdistance <- ggplot(data = data_det, aes(x = month, y = abs_dis_targ, group = 1))+geom_line()+ylab('')+
  theme_classic()+ggtitle('Distance from inflation target')+theme(legend.position = 'bottom',
                                                                  plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# Letter
fletter <- ggplot(data = data_det, aes(x = month, y = letter, group=1))+geom_col(colour='#56B4E9',fill='#56B4E9')+
  ylab('')+xlab('')+theme_classic()+ggtitle('Months with governor\'s letter')+
  theme(legend.position = 'bottom',plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# Absolute change in the exchange rate
fexchange <- ggplot(data = data_det, aes(x = month, y = gbpusd_absch, group = 1))+geom_line()+ylab('')+
  theme_classic()+ggtitle('Absolute change exchange rate')+theme(legend.position = 'bottom',
                                                                 plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# Change unemployment rate
funemploy <- ggplot(data = data_det, aes(x = month, y = unemp_ch, group = 1))+geom_line()+ylab('')+
  theme_classic()+ggtitle('Change unemployment rate')+theme(legend.position = 'bottom',
                                                                 plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# Systemic financial stress
fstress <- ggplot(data = data_det, aes(x = month, y = ukfsi, group = 1))+geom_line()+ylab('')+
  theme_classic()+ggtitle('Financial stress')+theme(legend.position = 'bottom',
                                                            plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

panel1x4<-ggarrange(fdistance,fexchange,funemploy,fstress,ncol=1,nrow=4,widths=c(1,1,1,1),common.legend = F, legend = c('bottom'))

# Main controls figures
# ---------------------

# Absolute rate change
frate <- ggplot(data = data_det, aes(x = month, y = rate_absch, group=1))+geom_col(colour='#56B4E9',fill='#56B4E9')+
  ylab('')+xlab('')+theme_classic()+ggtitle('Bank rate change (absolute)')+
  theme(legend.position = 'bottom',plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# QE announcement
fqe <- ggplot(data = data_det, aes(x = month, y = qe_annc, group=1))+geom_col(colour='#56B4E9',fill='#56B4E9')+
  ylab('')+xlab('')+theme_classic()+ggtitle('QE announcement (dummy)')+
  theme(legend.position = 'bottom',plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# Bailouts
fbailouts <- ggplot(data = data_det, aes(x = month, y = bailouts, group=1))+geom_col(colour='#56B4E9',fill='#56B4E9')+
  ylab('')+xlab('')+theme_classic()+ggtitle('Bank bailouts')+
  theme(legend.position = 'bottom',plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# Number of speeches
fspeeches <- ggplot(data = data_det, aes(x = month, y = speeches, group=1))+geom_col(colour='#56B4E9',fill='#56B4E9')+
  ylab('')+xlab('')+theme_classic()+ggtitle('Number of central bank speeches')+
  theme(legend.position = 'bottom',plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# Number of hearings
fhearings <- ggplot(data = data_det, aes(x = month, y = hearings, group=1))+geom_col(colour='#56B4E9',fill='#56B4E9')+
  ylab('')+xlab('')+theme_classic()+ggtitle('Number of parliamentary hearings')+
  theme(legend.position = 'bottom',plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

# Number of appointments
data_det$sum_appnt <- data_det$n_appnt + data_det$n_reappnt
fappoint <- ggplot(data = data_det, aes(x = month, y = sum_appnt, group=1))+geom_col(colour='#56B4E9',fill='#56B4E9')+
  ylab('')+xlab('')+theme_classic()+ggtitle('Number of central bank appointments')+
  theme(legend.position = 'bottom',plot.title=element_text(hjust=0.5,size=20),axis.text=element_text(size=15),legend.text=element_text(size=14))+
  labs(color='')+theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

panel1x4<-ggarrange(frate,fspeeches,fhearings,fappoint,ncol=1,nrow=4,widths=c(1,1,1,1),common.legend = F, legend = c('bottom'))

##########################################################################
########## APPENDIX E: ANALYSIS ON PARAGRAPHS OR FULL ARTICLE ############
##########################################################################

### EVALUATIVE COVERAGE ###

meval1.para<-felm(data=data,para_negpos_pc_sum ~ para_negpos_pc_sum1 + 
                       abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                       factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2.para<-felm(data=data,para_negpos_pc_sum ~ para_negpos_pc_sum1 + 
                       abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                       rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                       log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(meval1.para,meval2.para))

### NEGATIVITY ###

mneg1.para<-felm(data=data,para_negpos_pc_diff ~ para_negpos_pc_diff1 + 
                      abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                      factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2.para<-felm(data=data,para_negpos_pc_diff ~ para_negpos_pc_diff1 + 
                      abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                      rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                      log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(mneg1.para,mneg2.para),stars=c(.1,.05,.01))

### EVALUATIVE COVERAGE ###

meval1.article<-felm(data=data,negpos_pc_sum ~ negpos_pc_sum1 + 
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
               factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2.article<-felm(data=data,negpos_pc_sum ~ negpos_pc_sum1 + 
                       abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
               rate_absch + qe_annc + bailouts + log_speeches + hearings + 
               log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(meval1.article,meval2.article),stars=c(.1,.05,.01))

### NEGATIVITY ###

mneg1.article<-felm(data=data,negpos_pc_diff ~ negpos_pc_diff1 + 
                      abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
              factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2.article<-felm(data=data,negpos_pc_diff ~ negpos_pc_diff1 + 
                      abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
              rate_absch + qe_annc + bailouts + log_speeches + hearings + 
              log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(mneg1.article,mneg2.article),stars=c(.1,.05,.01))

##########################################################################
########## APPENDIX F: ANALYSIS WITH ALTERNATIVE DICTIONARIES ############ 
##########################################################################

evalcov_fig_loumcd<-ggplot(data2,aes(x=month,y=evaluative_loumcd_ma,group=newspaper,color=newspaper))+scale_colour_manual(values=cbPalette)+geom_line()+theme_classic()+ylab('')+labs(color='')+
  ggtitle('Evaluative coverage (Loughran and McDonald dictionary)')+theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5,size=20),axis.text=element_text(size=15),legend.text = element_text(size = 14))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

negativity_fig_loumcd<-ggplot(data2,aes(x=month,y=negativity_loumcd_ma,group=newspaper,color=newspaper))+scale_colour_manual(values=cbPalette)+geom_line()+theme_classic()+ylab('')+labs(color='')+
  ggtitle('Negative coverage (Loughran and McDonald dictionary)')+theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5,size=20),axis.text=element_text(size=15),legend.text = element_text(size = 14))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+scale_x_yearmon('',breaks=seq(1998,2020,2))

loumcd_dict<-ggarrange(evalcov_fig_loumcd,negativity_fig_loumcd,ncol=1,nrow=2,widths=c(1,1,1,1),common.legend = TRUE, legend = c('bottom'))

### EVALUATIVE COVERAGE (Loughran and McDonald) ###

meval1loumcd<-felm(data=data,sent_loumcd_negpos_pc_sum ~ sent_loumcd_negpos_pc_sum1 + 
               abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
               factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

meval2loumcd<-felm(data=data,sent_loumcd_negpos_pc_sum ~ sent_loumcd_negpos_pc_sum1 + 
                     abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis + 
                     rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                     log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(meval1,meval1loumcd,meval2,meval2loumcd),stars = c(.1,.05,.01))

### NEGATIVITY (Loughran and McDonald) ###

mneg1loumcd<-felm(data=data,sent_loumcd_negpos_pc_diff ~ sent_loumcd_negpos_pc_diff1 + 
                    abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                    factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

mneg2loumcd<-felm(data=data,sent_loumcd_negpos_pc_diff ~ sent_loumcd_negpos_pc_diff1 + 
                    abs_dis_lag1 + gbpusd_log + unempch_lag1 + ukfsi + post_crisis +
                    rate_absch + qe_annc + bailouts + log_speeches + hearings + 
                    log_allappnt + election + brexit + factor(month_dummy)+ factor(newspaper)|0|0|newspaper)

screenreg(list(mneg1,mneg1loumcd,mneg2,mneg2loumcd),stars = c(.1,.05,.01))