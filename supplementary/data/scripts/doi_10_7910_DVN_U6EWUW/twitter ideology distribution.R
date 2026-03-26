rm(list=ls())

#install.packages("rddtools")
#install.packages("rdrobust")

library(foreign)
library(ggplot2)
library(readxl)

setwd("/Users/johnholbein/Dropbox (Batten School @ UVA)/Work/Journalist Audit Study")

############################## Mean Differences ############################## 
priority<-read.dta("journalist-survey with Twitter ideo2 just ideo.dta")

attach(priority) 

plot1<-ggplot(priority, aes(ideot2)) +
  geom_density(aes(y=..count..), size=1.1, colour="white", fill="blue", alpha = 0.3) +
      
      geom_vline(aes(xintercept=0), colour="grey1", linetype="dashed") +
        annotate("text", x=0.1, y = 2000, label = "Average Twitter User", size=8.5, fontface="bold", colour="grey1", angle=90) +
      
      geom_vline(aes(xintercept=-0.72), colour="grey50", linetype="dashed") +
        annotate("text", x=-0.62, y = 2000, label = "Barack Obama", size=8.5, fontface="bold", colour="grey50", angle=90) +
      
      geom_vline(aes(xintercept=1.0), colour="grey50", linetype="dashed") +
        annotate("text", x=1.1, y = 2000, label = "Mitt Romney", size=8.5, fontface="bold", colour="grey50", angle=90) +
      
      geom_vline(aes(xintercept=-0.85), colour="grey50", linetype="dashed") +
        annotate("text", x=-0.95, y = 2000, label = "Median Senate D.", size=8.5, fontface="bold", colour="grey50", angle=90) +
      
      geom_vline(aes(xintercept=0.65), colour="grey50", linetype="dashed") +
        annotate("text", x=0.75, y = 2000, label = "Median Senate R.", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
       geom_vline(aes(xintercept=-2.29), colour="grey50", linetype="dashed") +
       annotate("text", x=-2.19, y = 2000, label = "Alexandria Ocasio-Cortez", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
      geom_vline(aes(xintercept=-1.07), colour="grey50", linetype="dashed") +
      annotate("text", x=-1.17, y = 2000, label = "Bernie Sanders", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
  
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Journalist Ideology (Lower=More Liberal)", y="Count", title="") +
  theme(text = element_text(size=25))  + theme(legend.position="none") 
plot1
ggsave(plot1, file="twitterideodist.pdf", width=10, height=5, scale=2)

############################## No Duplicates ############################## 
priority<-read.dta("/Users/johnholbein/Dropbox (Batten School @ UVA)/Work/Journalist Audit Study/journalist-survey with Twitter ideo2 no multiple papers.dta")

attach(priority) 

plot1<-ggplot(priority, aes(ideot2)) +
  geom_density(aes(y=..count..), size=1.1, colour="white", fill="blue", alpha = 0.3) +
  
  geom_vline(aes(xintercept=0), colour="grey1", linetype="dashed") +
  annotate("text", x=0.1, y = 2000, label = "Average Twitter User", size=8.5, fontface="bold", colour="grey1", angle=90) +
  
  geom_vline(aes(xintercept=-0.72), colour="grey50", linetype="dashed") +
  annotate("text", x=-0.62, y = 2000, label = "Barack Obama", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
  geom_vline(aes(xintercept=1.0), colour="grey50", linetype="dashed") +
  annotate("text", x=1.1, y = 2000, label = "Mitt Romney", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
  geom_vline(aes(xintercept=-0.85), colour="grey50", linetype="dashed") +
  annotate("text", x=-0.95, y = 2000, label = "Median Senate D.", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
  geom_vline(aes(xintercept=0.65), colour="grey50", linetype="dashed") +
  annotate("text", x=0.75, y = 2000, label = "Median Senate R.", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
  geom_vline(aes(xintercept=-2.29), colour="grey50", linetype="dashed") +
  annotate("text", x=-2.19, y = 2000, label = "Alexandria Ocasio-Cortez", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
  geom_vline(aes(xintercept=-1.07), colour="grey50", linetype="dashed") +
  annotate("text", x=-1.17, y = 2000, label = "Bernie Sanders", size=8.5, fontface="bold", colour="grey50", angle=90) +
  
  
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Journalist Ideology (Lower=More Liberal)", y="Count", title="") +
  theme(text = element_text(size=25))  + theme(legend.position="none") 
plot1
ggsave(plot1, file="twitterideodistnomultiplepaperjournalists.pdf", width=10, height=5, scale=2)


