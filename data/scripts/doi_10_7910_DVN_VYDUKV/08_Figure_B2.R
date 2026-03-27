#Set working directory to replication folder
#-----------------------------
#Packages necessary to run analyses
#library(tidyverse)
#library(latex2exp)
#-----------------------------------------------------------------

#Set parameters for plots
title.size <- 20
x.axis.tick.size <- 14
y.axis.tick.size <- 14
x.axis.label.size <- 18
y.axis.label.size <- 18
facet.text <- 15
#-----------------------------------------------------------------
#Load data
upper<- read.csv('Data/Split_Aversion_upper_bound.csv', header=F)
lower<- read.csv('Data/Split_Aversion_lower_bound.csv',header=F)

colnames(upper)=c('Upper','y2')
colnames(lower)=c('Lower','y2')
data  = data.frame(Upper=upper$Upper, Lower = lower$Lower, y2=upper$y2)

#Bounds given no split aversion
original_range = data.frame(upper = 0.625,lower =0.375, y2 = seq(0.25,0.75,.001))
#-----------------------------------------------------------------
#Figure B2

ggplot(data,aes(x=y2,y=Lower))+
  geom_ribbon(data=data,aes(ymin=Lower,ymax=Upper), fill="black", alpha=0.15)+
  geom_ribbon(data=data,aes(ymin=Upper,ymax=0.75), fill="black", alpha=0.35)+
  geom_ribbon(data=data,aes(ymax=Lower,ymin=0.25), fill="black", alpha=0)+
  geom_line(size=1.25, color = 'black')+
  geom_line(data=data,aes(x=y2,y=Upper), size=1.25,color = 'black')+
  scale_x_continuous(name = TeX('$y_2$'), limits = c(0.25,0.75), expand = c(0,0),
                     breaks = c(0.375,0.625), 
                     labels = c(TeX('$\\leftarrow$ More Liberal'),TeX('More Conservative $\\rightarrow$') ))+
  scale_y_continuous(name = TeX('$y_1$'), breaks = c(0.45,0.75), 
                     expand = c(0,0),
                     labels = c(TeX('$\\leftarrow$ More Liberal'),TeX('More Conservative $\\rightarrow$') ))+
  theme_bw()    +
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=15, angle = 90), axis.title.y = element_text(size=y.axis.label.size),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  geom_hline(yintercept = 0.375, linetype = 'dashed', color = 'black', size = 1.25)+
  geom_hline(yintercept = 0.625, linetype = 'dashed', color = 'black',size = 1.25)+
  annotate(geom='text',y = 0.675, x = 0.6, label = TeX('$d_1=con$'), cex = 10)+
  annotate(geom='text',y = 0.5, x = 0.5, label = TeX('$d_1=s_1$'), cex = 10)+
  annotate(geom='text',y = 0.3, x = 0.45, label = TeX('$d_1=lib$'), cex = 10)
  
ggsave('Plots/Figure_B2.pdf',width=7, height=5)






