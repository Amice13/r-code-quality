# Set working directory to project directory

wd <- "project diretory"
setwd(wd)
library(readstata13)
library(ggrepel)
dat <- read.dta13("Data/dd.dta")
library(ggplot2)
library(ggthemes)
library(ggallin)
library(dplyr)


# Get proportions
d <- dat %>% filter(is.na(termed) & !is.na(no_election))
mean(d$gdpchange<0,na.rm=T)
mean(d$gdpchange>0,na.rm=T)
mean(d$runagain[d$gdpchange<0],na.rm=T)
mean(d$runagain[d$gdpchange>0],na.rm=T)
mean(d$win[d$gdpchange<0],na.rm=T)
mean(d$win[d$gdpchange>=0],na.rm=T)


# Plots
d <- dat %>% filter(is.na(termed))
d$win1<-NA; d$win1[d$win==1]<-1
d$run1<-NA; d$run1[d$runagain==1]<-1

# Year 4
d <- dat %>% filter(is.na(termed))
d$win1<-NA; d$win1[which(d$win==1)]<-1
d$run1<-NA; d$run1[which(d$runagain==1)]<-1
d$eligible1 <-NA; d$eligible1[which(!is.na(d$runagain))]<-1

d2 <- data.frame(gdpchange=rep(d$gdpchange,3),y=c(d$win1,d$run1,d$eligible1),year=c(d$year,d$year,d$year),outcome=as.factor(c(rep("Ran and won",nrow(d)),rep("Ran",nrow(d)),rep("Eligible to Run",nrow(d)))),initials=as.factor(rep(d$initials,3)))

# Generate jitter
j <- runif(length(d$win1),-0.2,0.2)
d2$y <- d2$y + j
#pos <- position_jitter(width = 0, height=0.4,seed = 1)

png("Output/figure_1.png",  width = 780, height = 480, units = "px",)

ggplot(d2,mapping = aes(x = gdpchange,y=y,label=initials)) +  
  geom_point() + 
  facet_grid(rows=vars(outcome)) + geom_text_repel(cex=3, seed = 65) + geom_vline(xintercept=0,linetype="dashed")+
  scale_x_continuous(trans = pseudolog10_trans,limits = c(-15, 17),breaks=c(-10,-5,-1,0,1,5,10))+ ylim(0.75,1.25)+
  labs(y="", x="Election-year GDP growth", col=NULL)+
  theme_tufte()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),strip.text = element_text(size = 15))
dev.off()


# Year 3 
d <- dat %>% filter(is.na(termed))
d$win1<-NA; d$win1[which(d$win==1)]<-1
d$run1<-NA; d$run1[which(d$runagain==1)]<-1
d$eligible1 <-NA; d$eligible1[which(d$runagain==1 | d$runagain==0)]<-1
pos <- position_jitter(width = 0, height=0.1,seed = 1)
d2 <- data.frame(gdpchange=rep(d$gdpchangeyr3,3),y=c(d$win1,d$run1,d$eligible1),outcome=as.factor(c(rep("Ran and won",nrow(d)),rep("Ran",nrow(d)),rep("Eligible to Run",nrow(d)))),initials=as.factor(rep(d$initials,3)))
d2$y <- d2$y + j
png("Output/figure_1y3.png",  width = 780, height = 480, units = "px",)
ggplot(d2,mapping = aes(x = gdpchange,y=y,label=initials)) +  
  geom_point() + 
  facet_grid(rows=vars(outcome)) + geom_text_repel(cex=3, seed = 65) + geom_vline(xintercept=0,linetype="dashed")+
  scale_x_continuous(trans = pseudolog10_trans,limits = c(-15, 30),breaks=c(-10,-5,-1,0,1,5,10))+ ylim(0.75,1.25)+
  labs(y="", x="Year-3 GDP growth", col=NULL)+
  theme_tufte()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),strip.text = element_text(size = 15))
dev.off()


# Scatterplots of growth and vote share

dat$keynes <- as.factor(ifelse(dat$year>1946,1,0))
levels(dat$keynes) <- c("1789-1944","1944-2016")
png("Output/scatteryr3_prekeynes.png")
ggplot(subset(dat,dat$year<1946),aes(gdpchangeyr3,partyincshr,label=year)) + geom_text_repel(cex=3, seed = 65) + theme_bw() + geom_smooth(method="lm",se=F) + xlab("GDP Log Change") + ylab("Incumbent Party Vote") + theme(legend.title = element_blank())  + theme(legend.position = "bottom") + ylim(30,70) + xlim(-8,15)
dev.off()

png("Output/scatteryr3_postkeynes.png")
ggplot(subset(dat,dat$year>1946),aes(gdpchangeyr3,partyincshr,label=year)) + geom_text_repel(cex=3, seed = 65) + theme_bw() + geom_smooth(method="lm",se=F) + xlab("GDP Log Change") + ylab("Incumbent Party Vote") + theme(legend.title = element_blank())  + theme(legend.position = "bottom") + ylim(30,70) + xlim(-8,15)
dev.off()

png("Output/scatteryr4_prekeynes.png")
ggplot(subset(dat,dat$year<1946),aes(gdpchangeyr4,partyincshr,label=year)) + geom_text_repel(cex=3, seed = 65) + theme_bw() + geom_smooth(method="lm",se=F) + xlab("GDP Log Change") + ylab("Incumbent Party Vote") + theme(legend.title = element_blank())  + theme(legend.position = "bottom") + ylim(30,70) + xlim(-8,15)
dev.off()

png("Output/scatteryr4_postkeynes.png")
ggplot(subset(dat,dat$year>1946),aes(gdpchangeyr4,partyincshr,label=year)) + geom_text_repel(cex=3, seed = 65) + theme_bw() + geom_smooth(method="lm",se=F) + xlab("GDP Log Change") + ylab("Incumbent Party Vote") + theme(legend.title = element_blank())  + theme(legend.position = "bottom") + ylim(30,70) + xlim(-8,15)
dev.off()

