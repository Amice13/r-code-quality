### Yu, Whang, and Lee ###
### When Does Audience Matter? Challengers' Stability and Audience Costs ###
### Replication Stability Trend ###

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
rm(list=ls())

### Load Packages
library(dplyr)
library(tidyr)
library(colorspace)

### Load Dataset
data = read.csv('Yu, Whang, and Lee_dataset.csv')

## Threat Variable
threat = data %>%
  select(ccode1,styear) %>%
  distinct() %>%
  mutate(threat = 1)

## Reciprocation Variable
recip = data %>%
  filter(recip == 1) %>%
  select(ccode1,endyear) %>%
  distinct() %>%
  mutate(recip = 1)

## Stability Variable (need to install vdemdata package)
stab = vdemdata::vdem %>%
  select(COWcode,year,e_wbgi_pve) %>%
  drop_na() %>%
  distinct()

## Dataset including Observations without Conflict
trend = data.frame() %>%
  expand(ccode = unique(stab$COWcode),
         year = max(stab$year):min(stab$year)) %>%
  left_join(stab,by=c('ccode'='COWcode','year')) %>%
  left_join(threat,by=c('ccode'='ccode1','year'='styear')) %>%
  left_join(recip,by=c('ccode'='ccode1','year'='endyear')) %>%
  mutate(threat = ifelse(is.na(threat),0,threat)) %>%
  mutate(recip = ifelse(is.na(recip),0,recip)) %>%
  mutate(s = e_wbgi_pve) %>%
  arrange(ccode,year) %>%
  group_by(ccode) %>%
  ## Stability from t-5 to t+5
  mutate(sb1 = lag(e_wbgi_pve,1)) %>%
  mutate(sb2 = lag(e_wbgi_pve,2)) %>%
  mutate(sb3 = lag(e_wbgi_pve,3)) %>%
  mutate(sb4 = lag(e_wbgi_pve,4)) %>%
  mutate(sb5 = lag(e_wbgi_pve,5)) %>%
  mutate(sa1 = lead(e_wbgi_pve,1)) %>%
  mutate(sa2 = lead(e_wbgi_pve,2)) %>%
  mutate(sa3 = lead(e_wbgi_pve,3)) %>%
  mutate(sa4 = lead(e_wbgi_pve,4)) %>%
  mutate(sa5 = lead(e_wbgi_pve,5)) %>%
  ungroup()

s = seq(min(trend$s,na.rm=T),max(trend$s,na.rm=T),len=11)
t = 1:11

## Mean of Stability before/after Threat (Reciprocation)
threat.agg = apply(apply(trend[trend$threat==1,],1,function(x) x[c(11:6,12:16)]),1,mean,na.rm=T)
recip.agg = apply(apply(trend[trend$recip==1,],1,function(x) x[c(11:6,12:16)]),1,mean,na.rm=T)

### Figure OA1

par(mfrow=c(2,2),mar=c(4,5,2,2))

### Panel A: Variation of Stability Before/After Threat (N=75)

plot(s~t,type='n',
     ylab='Stability',
     xlab='',xaxt='n',
     main='A. Before/After Threat (N=75)')
for(i in sample(1:nrow(trend[trend$threat==1,]),75,replace=F)){
  ss = trend[i,c(paste('sb',5:1,sep=''),
                 's',
                 paste('sa',1:5,sep=''))]
  lines(as.vector(as.matrix(ss))~t,
        col=adjust_transparency('gray',alpha=0.5))  
}
abline(v=6,col='red',lty='dashed')
lines(threat.agg)
axis(side=1,
     at=1:11,
     labels=c('t-5',
              't-4',
              't-3',
              't-2',
              't-1',
              't',
              't+1',
              't+2',
              't+3',
              't+4',
              't+5'))

### Panel B: Variation of Stability Before/After Reciprocation (N=75)

plot(s~t,type='n',
     ylab='Stability',
     xlab='',xaxt='n',
     main='B. Before/After Reciprocation (N=75)')
for(i in sample(1:nrow(trend[trend$recip==1,]),75,replace=F)){
  ss = trend[i,c(paste('sb',5:1,sep=''),
                 's',
                 paste('sa',1:5,sep=''))]
  lines(as.vector(as.matrix(ss))~t,
        col=adjust_transparency('gray',alpha=0.5))  
}
abline(v=6,col='red',lty='dashed')
lines(recip.agg)
axis(side=1,
     at=1:11,
     labels=c('t-5',
              't-4',
              't-3',
              't-2',
              't-1',
              't',
              't+1',
              't+2',
              't+3',
              't+4',
              't+5'))

### Panel C: Variation of Stability Before/After Threat (N=150)

plot(s~t,type='n',
     ylab='Stability',
     xlab='',xaxt='n',
     main='C. Before/After Threat (N=150)')
for(i in sample(1:nrow(trend[trend$threat==1,]),150,replace=F)){
  ss = trend[i,c(paste('sb',5:1,sep=''),
                 's',
                 paste('sa',1:5,sep=''))]
  lines(as.vector(as.matrix(ss))~t,
        col=adjust_transparency('gray',alpha=0.5))  
}
abline(v=6,col='red',lty='dashed')
lines(threat.agg)
axis(side=1,
     at=1:11,
     labels=c('t-5',
              't-4',
              't-3',
              't-2',
              't-1',
              't',
              't+1',
              't+2',
              't+3',
              't+4',
              't+5'))

### Panel D: Variation of Stability Before/After Reciprocation (N=150)

plot(s~t,type='n',
     ylab='Stability',
     xlab='',xaxt='n',
     main='D. Before/After Reciprocation (N=150)')
for(i in sample(1:nrow(trend[trend$recip==1,]),150,replace=F)){
  ss = trend[i,c(paste('sb',5:1,sep=''),
                 's',
                 paste('sa',1:5,sep=''))]
  lines(as.vector(as.matrix(ss))~t,
        col=adjust_transparency('gray',alpha=0.5))  
}
abline(v=6,col='red',lty='dashed')
lines(recip.agg)
axis(side=1,
     at=1:11,
     labels=c('t-5',
              't-4',
              't-3',
              't-2',
              't-1',
              't',
              't+1',
              't+2',
              't+3',
              't+4',
              't+5'))
