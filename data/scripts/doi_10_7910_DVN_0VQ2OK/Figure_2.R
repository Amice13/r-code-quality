#Code to plot side by side bar graphs labelled as figure 2 in the manuscript
#setwd() Set working directory
library(ggplot2)
library(plotly)
library(webshot) #For exporting graphs from plotyl
library(orca)#export is deprecated use orca instead
library(reshape2)
library(gridExtra)

#Data from proportios of GP genotypes countrwide data. Manually imported from databook 

Period = c("Pre-vaccine","Post-vaccine")
G1P8 = c(45.8,52.1)
G2P4 =c(7.0,20.7)
G8P4 = c(15.8,0.4)
G3P8 =c(1.3,16.1)
G9P8 = c(13.2,5.4)
#Range of genotypes proportion

#Confidnece intterval 95%
cmaxg1 = c(49.8,58.3)
cming1 = c(41.8,45.9)
cmaxg2 = c(9.4,26.2)
cming2 = c(5.2,16.0)
cmaxg3 = c(2.7,21.2)
cming3 = c(0.6,12.0)
cmaxg8 = c(19.2,2.4)
cming8 = c(13.2,0)
cmaxg9 = c(16.2,9.0)
cming9 = c(10.7,3.1)



datag1 = data.frame(Period, G1P8,cmaxg1,cming1)
datag2 = data.frame(Period, G2P4,cmaxg2,cming2)
datag8 = data.frame(Period, G8P4,cmaxg8,cming8)
datag9 = data.frame(Period, G9P8,cmaxg9,cming9)
datag3 = data.frame(Period, G3P8,cmaxg3,cming3)


#Plotting CI error bargraphs and proportions

d.g1p8 = ggplot(datag1) +
  geom_bar( aes(x=Period, y=G1P8), stat="identity", fill=c("gray40","steelblue1")) +
  geom_errorbar(aes(Period), ymin=cming1,ymax=cmaxg1,width=.3)+
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size=12,color='black'),
        aspect.ratio = 3/1,axis.text.x = element_text(size=10, color="black"))+
  scale_y_continuous(limits = c(0,100),name = 'Proportions(%)')+
  scale_x_discrete(limits=Period,labels=NULL,names(NA))+
  ggtitle("G1P[8]") + theme(plot.title = element_text(size = 12,color = "black",face='bold'),axis.text.x.bottom = 
                              element_text(color='black'))
d.g1p8


d.g2p4 = ggplot(datag2) +
  geom_bar( aes(x=Period, y=G2P4), stat="identity", fill=c("gray40","steelblue1")) +
  geom_errorbar(aes(Period), ymin=cming2,ymax=cmaxg2,width=.3)+
  theme(panel.grid.major.y = element_blank(),axis.ticks.y = element_blank() ,axis.text.y = element_text(size=12,color='black'),
        aspect.ratio = 3/1,axis.text.x = element_text(size=10, color="black"))+
  scale_y_continuous(limits = c(0,100),name = '')+
  scale_x_discrete(limits=Period,labels=NULL,names(NA))+
  ggtitle("G2P[4]")+ theme(plot.title = element_text(size = 12,color = "black",face='bold'),axis.text.x.bottom = 
                             element_text(color='black'))

d.g2p4 


d.g8p4 = ggplot(datag8) +
  geom_bar( aes(x=Period, y=G8P4), stat="identity", fill=c("gray40","steelblue1")) +
  geom_errorbar(aes(Period), ymin=cming8,ymax=cmaxg8,width=.3)+
  theme(panel.grid.major.y = element_blank(),axis.ticks.y = element_blank() ,axis.text.y = element_text(size=12,color='black'),
        aspect.ratio = 3/1,axis.text.x = element_text(size=10, color="black"))+
  scale_y_continuous(limits = c(0,100),name = '')+
  scale_x_discrete(limits=Period,labels=NULL,names(NA))+
  ggtitle("G8P[4]")  + theme(plot.title = element_text(size = 12,color = "black",face='bold'),axis.text.x.bottom = 
                               element_text(color='black'))

  d.g8p4  
  
d.g9p8 = ggplot(datag9) +
  geom_bar( aes(x=Period, y=G9P8), stat="identity", fill=c("gray40","steelblue1")) +
  geom_errorbar(aes(Period), ymin=cming9,ymax=cmaxg9,width=.3)+
  theme(panel.grid.major.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_text(size=12,color='black'),
        aspect.ratio = 3/1,axis.text.x = element_text(size=10, color="black"))+
  scale_y_continuous(limits = c(0,100),name = '')+
  scale_x_discrete(limits=Period,labels=NULL,names(NA))+
  ggtitle("G9P[8]")+ theme(plot.title = element_text(size = 12,color = "black",face='bold'),axis.text.x.bottom = 
                             element_text(color='black'))
d.g9p8

d.g3p8 = ggplot(datag3) +
  geom_bar( aes(x=Period, y=G3P8), stat="identity", fill=c("gray40","steelblue1"),width = .7) +
  geom_errorbar(aes(Period), ymin=cming3,ymax=cmaxg3,width=.3)+
  theme(panel.grid.major.y = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_text(size=12,color='black'),
        aspect.ratio = 3/1,axis.text.x = element_text(size=10, color="black"))+
  scale_y_continuous(limits = c(0,100),name = '')+
  scale_x_discrete(limits=Period,labels=NULL,names(NA))+
  ggtitle("G3P[8]") + theme(plot.title = element_text(size = 12,color = "black",face='bold'),axis.text.x.bottom = 
                              element_text(color='black'))
d.g3p8



grid.arrange(d.g1p8,d.g2p4,d.g3p8,d.g8p4,d.g9p8,nrow=1)


