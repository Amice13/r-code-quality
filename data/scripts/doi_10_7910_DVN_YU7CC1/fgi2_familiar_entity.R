########################################
# fig2: selection of familiar entity   #
########################################


###-----load package
library(cjoint)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(psych)




###--------YAHOO--------###


###-------load data

cd<-read.csv("covid_cj.csv")
attach(cd)


#set variables

famil<-prop.table(table(Q19.1))
famil<-famil[-1]
famil_lab<-c("colleague","friend","lover","neighbor",
             "older family","spouse","younger family")

#set data frame for table
tb_famil<-data.frame(cbind(famil_lab,famil))

#set levels order
tb_famil$famil_lab <- factor(tb_famil$famil_lab,levels = c("neighbor","colleague","lover","friend",
                                                           "younger family","older family","spouse"))
tb_famil$famil<-round(as.numeric(famil*100),1)

#plot
famil_bar<-ggplot(data=tb_famil, aes(x=famil_lab,y=famil)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("")+
  ggtitle("Familiar Entity (YSC)")+
  geom_text(aes(label=famil), hjust=1.3, color="red", size=4.0)
famil_bar


####----------------Lucid-------------------###

###-------load data
cdl<-read.csv("covid19_normal_lucid.csv")
attach(cdl)


#set variables

famillu<-prop.table(table(cdl$Q10.1))
famil_lablu<-c("older family","neighbor","lover","spouse",
               "friend","younger family","colleague")

#set data frame for table
tb_famillu<-data.frame(cbind(famil_lablu,famillu))

#set levels order
tb_famillu$famil_lablu <- factor(tb_famillu$famil_lablu,levels = c("neighbor","colleague","lover","friend",
                                                                   "younger family","older family","spouse"))
tb_famillu$famillu<-round(as.numeric(famillu*100),1)


#plot
famil_barlu<-ggplot(data=tb_famillu, aes(x=famil_lablu,y=famillu)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("")+
  ggtitle("Familiar Entity (Lucid)")+
  geom_text(aes(label=famillu), hjust=1.3, color="white", size=4.0)
famil_barlu

windows(width = 20, height = 10)
img<-grid.arrange(famil_bar,famil_barlu, ncol = 2, nrow = 1)
print(img)
ggsave(file = "fig2_familiar_entity.pdf", plot = img)



