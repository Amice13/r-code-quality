##################################################
### Data Analysis for "International Tourism's ###
### Impact on Regional Autonomy: Evidence      ###
### from 2004 EU Accession Countries"          ###
### in "Tourism Economics," 2017.              ###
### William O'Brochta                          ###
### Washington University in St. Louis         ###
##################################################

#Note: to run this file, you will need the following other files in your working directory
#Estonia 2003-2009.csv, Poland 2000-2014.csv, Latvia 2001-2011.csv, Slovenia 2000-2013.csv
#Hungary 2000-2014.csv, CZ 2003-2009.csv, EU Air Hotel.csv

#Instructions: run the whole file. The figures are reproduced starting on line 250.

library(foreign)
library(readstata13)
library(car)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(grid)

#EU Air and Hotel
EU_Islands<-read.csv("EU Air Hotel.csv", stringsAsFactors = FALSE)
EU_Islands$Region<-as.factor(EU_Islands$Region)
EU_Islands$Year<-as.Date(EU_Islands$Year, format ="%m/%d/%y")
EU_Islands$Label<-c(1:13)
EU<-EU_Islands[which(EU_Islands$Label!=c(9,10)),]

#Estonia ##No effect expected: No increase in tourism. No increase in GVA.
Est<-read.csv("Estonia 2003-2009.csv", stringsAsFactors = FALSE)
Est$Region<-as.factor(Est$Region)

T1<-ggplot(data=Est, aes(x=Year, y=Tourism_capita)) + geom_line(aes(colour=Region, group=Region),size=0.7)+
  geom_text(data=subset(Est, Year==2009), aes(label=Region, colour=Region, x=Inf, y=Tourism_capita), hjust= -.1, size=5)+ 
  scale_colour_discrete(guide='none') + theme_bw()+theme(plot.margin=unit(c(1,6,1,1),'lines'))+
  labs(title='Estonia')+
  scale_y_continuous('Hotel Nights/capita', limits=c(0,4))+
  scale_x_continuous('Year', limits=c(2003,2013), 
                     breaks=c(2003, 2005, 2007, 2009, 2011, 2013))

T1.1<-ggplotGrob(T1)
T1.1$layout$clip[T1.1$layout$name=='panel']<-'off'
grid.draw(T1.1)


#Poland ##Predict an effect around 2009.
Pol<-read.csv("Poland 2000-2014.csv", stringsAsFactors = FALSE)
Pol$Region<-as.factor(Pol$Region)
Pol$Shock<-Pol$Year>=2009

Pol2<-Pol[Pol$Region=='ZP' | Pol$Region=='Malopolskie' | Pol$Region=='Northwest' | 
            Pol$Region=='Lubuskie'  |
            Pol$Region=='Southern',]

T2<-ggplot(data=Pol2, aes(x=Year, y=Tourism_capita)) + geom_line(aes(colour=Region, group=Region),size=0.7)+
  geom_text(data=subset(Pol2, Year==2013), aes(label=Region, colour=Region, x=Inf, y=Tourism_capita), hjust= -.1, size=5)+ 
  scale_colour_discrete(guide='none') + theme_bw()+theme(plot.margin=unit(c(1,6,1,1),'lines'))+
  labs(title='Poland')+
  scale_y_continuous('Hotel Nights/capita', limits=c(0,4))+
  scale_x_continuous('Year', limits=c(2003,2013), 
                     breaks=c(2003, 2005, 2007, 2009, 2011, 2013))

T2.1<-ggplotGrob(T2)
T2.1$layout$clip[T2.1$layout$name=='panel']<-'off'
grid.draw(T2.1)

G2<-ggplot(data=Pol[which(Pol$Region==c("Poland") & Pol$Year<2015),], aes(x=Year, y=GVA_capita, color=factor(Shock))) + geom_point() + 
  stat_smooth(method='lm') + theme_bw() + theme(legend.position="none")+geom_vline(xintercept=2009)+labs(title='Poland')+scale_y_continuous('GVA/capita', limits=c(2000,7250), breaks=c(2000, 3000, 4000, 5000, 6000, 7000))+
  scale_x_continuous('Year', limits=c(2000,2014), breaks=c(2001, 2003, 2005, 2007, 2009, 2011, 2013))

G2.1<-ggplot(data=Pol[which(Pol$Region==c("Poland") & Pol$Year<2015),], aes(x=Year, y=GVA_capita, linetype=factor(Shock))) + geom_point() + stat_smooth(method='lm', fullrange=TRUE) +
  theme_bw() +geom_vline(xintercept=2009)+labs(title='Poland')+scale_y_continuous('GVA/capita', limits=c(2000,7250), breaks=c(2000, 3000, 4000, 5000, 6000, 7000))+
  scale_x_continuous('Year', limits=c(2000,2014), breaks=c(2001, 2003, 2005, 2007, 2009, 2011, 2013))+theme(legend.position='none')

YearP<-c(2000:2014)

modelP.l<-lm(GVA_capita~Year, data=Pol[which(Pol$Region==c("Poland") & Pol$Year<=2009),])
summary(modelP.l)
coefP.l<-modelP.l$coefficients
predP.l<-coefP.l[1] + coefP.l[2]*YearP
predP.l

modelP.u<-lm(GVA_capita~Year, data=Pol[which(Pol$Region==c("Poland") & Pol$Year>2009),])
summary(modelP.u)
coefP.u<-modelP.u$coefficients
predP.u<-coefP.u[1] + coefP.u[2]*YearP
predP.u

Poland<-predP.u-predP.l

#Effect of tourism increase is increase of GVA_capita of $234.15.
#Effect lasts throughout extrapolation period.

#Maximum effect on Central Poland.
Pol$Shock<-Pol$Year>=2009
ggplot(data=Pol[which(Pol$Region==c("Central") & Pol$Year<2015),], aes(x=Year, y=Tourism_capita, color=factor(Shock))) + geom_point() + stat_smooth(method='lm')

G2.2<-ggplot(data=Pol[which(Pol$Region==c("Central") & Pol$Year<2015),], aes(x=Year, y=GVA_capita, color=factor(Shock))) + geom_point() + stat_smooth(method='lm')+ 
  theme_bw() + theme(legend.position="none")+geom_vline(xintercept=2009)+labs(title='Central Poland')+scale_y_continuous('GVA/capita', limits=c(2000,7250), breaks=c(2000, 3000, 4000, 5000, 6000, 7000))+
  scale_x_continuous('Year', limits=c(2000,2014), breaks=c(2001, 2003, 2005, 2007, 2009, 2011, 2013))

G2.3<-ggplot(data=Pol[which(Pol$Region==c("Central") & Pol$Year<2015),], aes(x=Year, y=GVA_capita, linetype=factor(Shock))) + geom_point() + stat_smooth(method='lm', fullrange=TRUE)+
  theme_bw() + theme(legend.position="none")+geom_vline(xintercept=2009)+labs(title='Central Poland')+scale_y_continuous('GVA/capita', limits=c(2000,7250), breaks=c(2000, 3000, 4000, 5000, 6000, 7000))+
  scale_x_continuous('Year', limits=c(2000,2014), breaks=c(2001, 2003, 2005, 2007, 2009, 2011, 2013))+
  geom_segment(aes(x=2010,y=2500,xend=2011,yend=2500),linetype=1, size=0.7, colour='blue')+
  geom_segment(aes(x=2010,y=2200,xend=2011,yend=2200),linetype=2, size=0.7, colour='blue')

G2.3<-G2.3+annotate('text',x=2012.5,y=c(2500,2200),label=c('Before Shock','After Shock'))

modelP1.l<-lm(GVA_capita~Year, data=Pol[which(Pol$Region==c("Central") & Pol$Year<=2009),])
summary(modelP1.l)
coefP1.l<-modelP1.l$coefficients
predP1.l<-coefP1.l[1] + coefP1.l[2]*YearP
predP1.l

modelP1.u<-lm(GVA_capita~Year, data=Pol[which(Pol$Region==c("Central") & Pol$Year>2009),])
summary(modelP1.u)
coefP1.u<-modelP1.u$coefficients
predP1.u<-coefP1.u[1] + coefP1.u[2]*YearP
predP1.u

Central_Poland<-predP1.u-predP1.l

PredPoland<-as.data.frame(cbind(YearP, Poland,Central_Poland))

meltPP<-melt(PredPoland, id=c('YearP'))

PP<-ggplot(meltPP, aes(YearP, value))+ geom_line(aes(linetype=variable),size=0.7)+theme_bw()+
  labs(title='Impact of Air Traffic Growth on GVA/capita')+scale_y_continuous('Delta GVA/capita', limits=c(-50,1500), breaks=c(0,500,1000,1500))+
  scale_x_continuous('Year', limits=c(2000, 2014), breaks=c(2001, 2003, 2005, 2007, 2009, 2011, 2013))+
  theme(legend.position='none')+geom_vline(xintercept = 2009)+
  geom_segment(aes(x=2010,y=1500,xend=2010.5,yend=1500),linetype=1, size=0.7)+
  geom_segment(aes(x=2010,y=1450,xend=2010.5,yend=1450),linetype=2, size=0.7)

PP<-PP+annotate('text',x=2012,y=c(1500,1450),label=c('Poland','Central Poland'))

#Latvia ##Expect effect and find none.
Lat<-read.csv("Latvia 2001-2011.csv", stringsAsFactors = FALSE)
Lat$Region<-as.factor(Lat$Region)
Lat$Region_name<-as.factor(Lat$Region_name)
Lat$Shock<-Lat$Year>=2007

Lat2<-Lat[Lat$Region_name=='Vidzeme' | Lat$Region_name=='Pieriga' | Lat$Region_name=='Latvia' |
            Lat$Region_name=='Zemgale' | Lat$Region_name=='Riga',]

T3<-ggplot(data=Lat2, aes(x=Year, y=Hotels_capita)) + geom_line(aes(colour=Region_name, group=Region_name),size=0.7)+
  geom_text(data=subset(Lat2, Year==2011), aes(label=Region_name, colour=Region_name, x=Inf, y=Hotels_capita), hjust= -.1, size=5)+ 
  scale_colour_discrete(guide='none') + theme_bw()+theme(plot.margin=unit(c(1,5,1,1),'lines'))+
  labs(title='Latvia')+scale_y_continuous('Hotels/capita', limits=c(0,0.0006))+
  scale_x_continuous('Year', limits=c(2003,2013), breaks=c(2003, 2005, 2007, 2009, 2011, 2013))

T3.1<-ggplotGrob(T3)
T3.1$layout$clip[T3.1$layout$name=='panel']<-'off'
grid.draw(T3.1)

Lat2<-Lat[Lat$Region!='LV005',]

L1<-ggplot(data=Lat2, aes(x=Year, y=GVA_capita)) + geom_line(aes(colour=Region_name, group=Region_name),size=0.7)+
  geom_text(data=subset(Lat2, Year==2009), aes(label=Region_name, colour=Region_name, x=Inf, y=GVA_capita), hjust= -.1, size=5)+ 
  scale_colour_discrete(guide='none') + theme_bw()+theme(plot.margin=unit(c(1,5,1,1),'lines'))+geom_vline(xintercept=2007)+labs(title='Latvia')+scale_y_continuous('GVA/capita', limits=c(500,2750), breaks=c(500, 1000, 1500, 2000, 2500))+
  scale_x_continuous('Year', limits=c(2005,2009), breaks=c(2005, 2007, 2009))
  
L2<-ggplotGrob(L1)
L2$layout$clip[L2$layout$name=='panel']<-'off'
grid.draw(L2)

#Slovenia ##Effect should go from 2005-2008. Looks like a very small (if any) effect.
Slo<-read.csv("Slovenia 2000-2013.csv", stringsAsFactors = FALSE)
Slo$Region<-as.factor(Slo$Region)
Slo$Shock<-Slo$Year>=2005

T4<-ggplot(data=Slo, aes(x=Year, y=Tourism_capita)) + geom_line(aes(colour=Region, group=Region),size=0.7)+
  geom_text(data=subset(Slo, Year==2013), aes(label=Region, colour=Region, x=Inf, y=Tourism_capita), hjust= -.1, size=5)+ 
  scale_colour_discrete(guide='none') + theme_bw()+theme(plot.margin=unit(c(1,5,1,1),'lines'))+
  labs(title='Slovenia')+
  scale_y_continuous('Hotel Nights/capita', limits=c(0,4))+
  scale_x_continuous('Year',
                     limits=c(2003,2013), breaks=c(2003, 2005, 2007, 2009, 2011, 2013))

T4.1<-ggplotGrob(T4)
T4.1$layout$clip[T4.1$layout$name=='panel']<-'off'
grid.draw(T4.1)

G4<-ggplot(data=Slo[which(Slo$Region==c("Slovenia")),], aes(x=Year, y=GVA_capita_USD, color=factor(Shock))) + geom_point() + stat_smooth(method='lm')+ 
  theme_bw() + theme(legend.position="none")+geom_vline(xintercept=2005)+labs(title='Slovenia')+scale_y_continuous('GVA/capita', limits=c(2000,7250), breaks=c(2000, 3000, 4000, 5000, 6000, 7000))+
  scale_x_continuous('Year', limits=c(2000,2014), breaks=c(2001, 2003, 2005, 2007, 2009, 2011, 2013))

G4.1<-ggplot(data=Slo[which(Slo$Region==c("Slovenia")),], aes(x=Year, y=GVA_capita_USD, linetype=factor(Shock))) + geom_point() + stat_smooth(method='lm', fullrange=TRUE)+
  theme_bw() + theme(legend.position="none")+geom_vline(xintercept=2005)+labs(title='Slovenia')+scale_y_continuous('GVA/capita', limits=c(2000,7250), breaks=c(2000, 3000, 4000, 5000, 6000, 7000))+
  scale_x_continuous('Year', limits=c(2000,2014), breaks=c(2001, 2003, 2005, 2007, 2009, 2011, 2013))+
  geom_segment(aes(x=2010,y=2500,xend=2011,yend=2500),linetype=1, size=0.7, colour='blue')+
  geom_segment(aes(x=2010,y=2200,xend=2011,yend=2200),linetype=2, size=0.7, colour='blue')

G4.1<-G4.1+annotate('text',x=2012.5,y=c(2500,2200),label=c('Before Shock','After Shock'))

YearS<-c(2005:2014)

modelS.l<-lm(GVA_capita_USD~Year, data=Slo[which(Slo$Region==c("Slovenia") & Slo$Year<2005),])
summary(modelS.l)
coefS.l<-modelS.l$coefficients
predS.l<-coefS.l[1] + coefS.l[2]*YearS
predS.l

modelS.u<-lm(GVA_capita_USD~Year, data=Slo[which(Slo$Region==c("Slovenia") & Slo$Year>=2005),])
summary(modelP.u)
coefS.u<-modelS.u$coefficients
predS.u<-coefS.u[1] + coefS.u[2]*YearS
predS.u

predS.u-predS.l
##Indeed, effect goes from 2005-2006. By 2007, it is zeroed out.

#Czech Republic ##No effect expected: Tourism actually declining while GVA increases.
CZ<-read.csv("CZ 2003-2009.csv", stringsAsFactors = FALSE)
CZ$Region<-as.factor(CZ$Region)

CZ2<-CZ[CZ$Region=='Northeast' | CZ$Region=='Southwest' | CZ$Region=='Central Moravia'
        | CZ$Region=='Northwest' | CZ$Region=='Prague', ]

T5<-ggplot(data=CZ2, aes(x=Year, y=Tourism_capita)) + geom_line(aes(colour=Region, group=Region),size=0.7)+
  geom_text(data=subset(CZ2, Year==2009), aes(label=Region, colour=Region, x=Inf, y=Tourism_capita), hjust= -.1, size=5)+ 
  scale_colour_discrete(guide='none') + theme_bw()+theme(plot.margin=unit(c(1,8,1,1),'lines'))+
  labs(title='Czech Republic')+
  scale_y_continuous('Hotel Nights/capita', limits=c(0,4))+
  scale_x_continuous('Year', limits=c(2003,2013), breaks=c(2003, 2005, 2007, 2009, 2011, 2013))

T5.1<-ggplotGrob(T5)
T5.1$layout$clip[T5.1$layout$name=='panel']<-'off'
grid.draw(T5.1)

#Hungary ##No effect expected.
HU<-read.csv("Hungary 2000-2014.csv", stringsAsFactors = FALSE)
HU$Region<-as.factor(HU$Region)

HU2<-HU[HU$Region=='Central' | HU$Region=='Hungary' | HU$Region=='S Great Plains' | 
          HU$Region=='C Transdanubia', ]

T6<-ggplot(data=HU2, aes(x=Year, y=Tourism_capita)) + geom_line(aes(colour=Region, group=Region),size=0.7)+
  geom_text(data=subset(HU2, Year==2013), aes(label=Region, colour=Region, x=Inf, y=Tourism_capita), hjust= -.1, size=5)+ 
  scale_colour_discrete(guide='none') + theme_bw()+theme(plot.margin=unit(c(1,8,1,1),'lines'))+
  labs(title='Hungary')+scale_y_continuous('Hotel Nights/capita', limits=c(0,4))+
  scale_x_continuous('Year', limits=c(2003,2013), breaks=c(2003, 2005, 2007, 2009, 2011, 2013))

T6.1<-ggplotGrob(T6)
T6.1$layout$clip[T6.1$layout$name=='panel']<-'off'
grid.draw(T6.1)

#####Plots for paper
###Figure 1
grid.draw(T1.1)
grid.draw(T2.1)
grid.draw(T4.1)
grid.draw(T5.1)
grid.draw(T6.1)
grid.draw(T3.1)

###Figure 2
#Poland
T2.1<-ggplot() + geom_line(data=Pol[which(Pol$Region==c("Malopolskie")),], aes(x=Year, y=Tourism_capita), size=1)+geom_vline(xintercept=2009)+
  theme_bw()+theme(legend.position="none")+labs(title='Central Poland')+scale_y_continuous('Hotel Nights/capita', limits=c(0.55,0.85))+
  scale_x_continuous('Year', limits=c(2005,2014), breaks=c(2005, 2007, 2009, 2011, 2013))

#Slovenia
T4.1<-ggplot()+geom_line(data=Slo[which(Slo$Region==c("Eastern")),], aes(x=Year, y=Tourism_capita),size=1)+geom_vline(xintercept=2005)+
  theme_bw()+theme(legend.position="none")+labs(title='Eastern Slovenia')+scale_y_continuous('Hotel Nights/capita', limits=c(1.6,2.05))+
  scale_x_continuous('Year', limits=c(2001,2013), breaks=c(2001,2003, 2005, 2007, 2009, 2011, 2013))

#Latvia
T3.1<-ggplot()+geom_line(data=Lat[which(Lat$Region_name==c("Pieriga")),], aes(x=Year, y=Hotels_capita),size=1)+geom_vline(xintercept=2007)+
  theme_bw()+theme(legend.position="none")+labs(title='Pieriga Latvia')+scale_y_continuous('Hotels/capita', limits=c(0.0002,0.00055))+
  scale_x_continuous('Year', limits=c(2001,2011), breaks=c(2001,2003, 2005, 2007, 2009, 2011))

grid.arrange(T2.1, T4.1, T3.1, nrow=3)

###Figure 3
#Air Travel
#Poland
library(ggExtra)
A2<-ggplot()+geom_line(data=EU[which(EU$Label==c(6)),], aes(x=Year, y=Air_pchange), size=1)+theme_bw()+
  geom_vline(xintercept=as.numeric(as.Date('2009-01-01')))+
  labs(title='Poland')+scale_y_continuous('Air Pass. (% inc)', limits=c(-60,130))+
  scale_x_date('Year', limits=as.Date(c('2005-01-01','2011-01-01')))+
  geom_line(aes(x=Year,y=Warsaw), data=EU[which(EU$Label==c(6)),], linetype=3, size=0.7)+
  geom_line(aes(x=Year,y=Regional), data=EU[which(EU$Label==c(6)),], linetype=4, size=0.7)+
  geom_segment(aes(x=as.Date('2009-06-01'),y=130,xend=as.Date('2009-10-01'),yend=130),linetype=1, size=1)+
  geom_segment(aes(x=as.Date('2009-06-01'),y=110,xend=as.Date('2009-10-01'),yend=110),linetype=3, size=0.7)+
  geom_segment(aes(x=as.Date('2009-06-01'),y=90,xend=as.Date('2009-10-01'),yend=90),linetype=4, size=0.7)

A2<-A2+annotate('text',x=as.Date('2010-06-01'),y=c(130,110,90),label=c('Total Growth','Warsaw Chopin','Regional Airports'))

#Slovenia
A4<-ggplot()+geom_line(data=EU[which(EU$Label==c(7)),], aes(x=Year, y=Air_pchange), size=1)+theme_bw()+geom_vline(xintercept=as.numeric(as.Date('2005-01-01')))+theme(legend.position="none")+
  labs(title='Slovenia')+scale_y_continuous('Air Pass. (% inc)', limits=c(-60,130))+scale_x_date('Year', limits=as.Date(c('2005-01-01','20011-01-01')))

#Latvia
A3<-ggplot()+geom_line(data=EU[which(EU$Label==c(4)),], aes(x=Year, y=Air_pchange), size=1)+theme_bw()+geom_vline(xintercept=as.numeric(as.Date('2007-01-01')))+theme(legend.position="none")+
  labs(title='Latvia')+scale_y_continuous('Air Pass (% inc)', limits=c(-60,130))+scale_x_date('Year', limits=as.Date(c('2005-01-01','2011-01-01')))

grid.arrange(A2, A4, A3, nrow=3)

###Figure 4
#Total Polish Air Traffic
TotalTraffic<-matrix(c(5.5,6,7.5,7.8,6,6.2,7.5,7.5,9.5,9,10,10.5,11,11,11.5,12,4.6,9,10.5,12.2,12,13.8,14.5,16.5,15,17,18,19,20.5,22,23,24),nrow=2,byrow=TRUE)
colnames(TotalTraffic)<-c(2005:2020)
rownames(TotalTraffic)<-c('Warsaw Chopin','Regional')
barplot(TotalTraffic, col=c('firebrick','olivedrab'), xlab='Year', ylab='Passengers (Millions)', main='Polish Actual and Projected Air Traffic')
legend('topleft',legend=c('Warsaw Chopin','Regional'),fill=c('firebrick','olivedrab'), bty='n')

###Figure 5
grid.draw(L2)

###Figure 6
G4.1

###Figure 7
grid.arrange(G2.1, G2.3, nrow=2)

###Figure 8
PP

