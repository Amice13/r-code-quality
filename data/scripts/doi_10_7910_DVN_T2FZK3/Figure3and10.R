#############################################################################################
#############################################################################################
#############################################################################################

#Paper: Illiberal Communication and Election Intervention During the Refugee Crisis in Germany

#Ashrakat Elshehawy, Konstantin Gavras, Nikolay Marinov, Federico Nanni, and Harald Schoen

#Perspectives on Politics

#Replication for figures: 3 and 10


#############################################################################################
#############################################################################################
#############################################################################################

#clean environment
rm(list=ls())

#please install all packages if you have not already done so
#install.packages("ggpubr")
#install.packages("cowplot")
#install.packages("ggplot2")

#load libraries
library(readr)
library(ggplot2)
library(cowplot)
library(ggpubr)
require(gridExtra)
library(grid)


#load data - please change to your own working directory
refugeedata_indomain <- read_csv("/Users/ashrakatelshehawy/FIgure 3 and 10 Data and R Code/refugee-indomain-class-score_v2.csv")
refugeedata_wikifeb <- read_csv("/Users/ashrakatelshehawy/FIgure 3 and 10 Data and R Code/refugee-wikifeb-class-score_v2.csv")


#create a russian dummy
refugeedata_indomain$rus <- ifelse(grepl("sputnik" , refugeedata_indomain$url), "1",  
                                   ifelse(grepl("deutsch.rt", refugeedata_indomain$url), "1",
                                          ifelse(grepl("rtdeutsch", refugeedata_indomain$url),"1","0")))


refugeedata_wikifeb$rus <- ifelse(grepl("sputnik" , refugeedata_wikifeb$url), "1",  
                                  ifelse(grepl("deutsch.rt", refugeedata_wikifeb$url), "1",
                                         ifelse(grepl("rtdeutsch", refugeedata_wikifeb$url),"1","0")))
#transform dummy to factor
refugeedata_indomain$rus<-as.factor(refugeedata_indomain$rus)
summary(refugeedata_indomain$rus)

refugeedata_wikifeb$rus<-as.factor(refugeedata_wikifeb$rus)
summary(refugeedata_wikifeb$rus)

############german data with indomain embeddings articles with no-content############

#In our German data, specifically the FAZ media articles, there were around 292 articles
#that heavily included java code in the text of the article without any real article text.
#We, therefore, have dropped these cases before creating our plots.

##############################plot-figure 3#######################################################
##########################################################################################

#conspiracy

figure3<-ggplot(refugeedata_indomain, aes(x = PopulismConspiracyColRevolt)) +
  stat_density(aes(group = rus, color = rus, linetype=rus),position="identity",geom="line")+
  scale_color_manual(values=c("red","black"),guide=F) +
  scale_linetype_manual(values=c("solid", "dashed"), guide=F)+
  labs( y="Density", x="Conspiracy Score") +  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  expand_limits(x = c(-0.25, 1), y=c(0,15))+
  scale_x_continuous(breaks= seq(-0.25,1,by=0.05),labels = c( -0.25, rep("",4),0, rep("",4),
                                                          0.25, rep("",4),
                                                          0.5, rep("",4),
                                                          0.75, rep("",4),
                                                          1))+
  theme(axis.text.x = element_text( size=12),
        axis.text.y = element_text( size=12),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        plot.title = element_text( size=15, face="bold"))+
  ggtitle("Indomain Embeddings")+
  theme(axis.title.x=element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

figure3
#here output of figure 3 of the paper

##############################plot-figure 10#######################################################
##########################################################################################


b3<-ggplot(refugeedata_wikifeb, aes(x = PopulismConspiracyColRevolt)) +
  stat_density(aes(group = rus, color = rus, linetype=rus),position="identity",geom="line")+
  scale_color_manual(values=c("red","black"),guide=F) +
  scale_linetype_manual(values=c("solid", "dashed"), guide=F)+
  labs( y="Density", x="Conspiracy Score") +  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  expand_limits(x = c(0.25, 1), y=c(0,15))+
  scale_x_continuous(breaks= seq(-0.25,1,by=0.05),labels = c(-0.25, rep("",4),
                                                             0, rep("",4),
                                                             0.25, rep("",4),
                                                             0.5, rep("",4),
                                                             0.75, rep("",4),
                                                             1))+
  theme(axis.text.x = element_text( size=12),
        axis.text.y = element_text( size=12),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        plot.title = element_text( size=15, face="bold"))+
  ggtitle("Wikipedia Embeddings")+
  theme(axis.title.x=element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))
b3


grid.arrange(b3,figure3 , nrow=2, top = textGrob("",gp=gpar(fontsize=20,font=3)))
#this creates figure 10
