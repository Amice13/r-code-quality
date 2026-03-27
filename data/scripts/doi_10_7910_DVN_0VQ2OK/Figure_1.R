#Code for generating stacked bar graphs labelled Figure 1 in the manuscript

#set working directory
#setwd()
#install these libraries 
library(readxl)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggpubr)

#Generate graph representing the country - combined all sites in Kenya
#Load data from excel
Kenya_GP_proportion = read_excel("./Genotype_Data.xlsx",sheet = "Kenya_Genotype_Proportions",col_names=T,
                                  col_types = NULL, na = "",skip = 0)

#melt the data
Kenya_GP_proportion.m = melt(Kenya_GP_proportion,id.vars="Genotype")

#Draw graphs
#panel a
Kenya = ggplot(Kenya_GP_proportion.m,aes(x=Genotype,y=value,fill=variable))+
  geom_bar(stat = "identity",width=.3)+scale_y_continuous(name = NULL)+
  scale_fill_manual(values = c("gray50","steelblue1","#a6611a","#018571","#E4E46A","red4","#333300"))+
  scale_x_discrete(name = "Year",labels=c("2010","2011","2012","2013","2014",
                                          "2015","2016","2017","2018"))+
  
 theme(legend.title = element_blank(),
        legend.key.size = unit(0.3,"cm"),
        legend.spacing.x= unit(0.5, 'cm'),
        legend.text = element_text(size=8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black"),
        panel.grid.minor.x = element_blank(), 
        panel.spacing = element_blank(),
        text=element_text(size=10,colour = "black"))

Kenya
####################

#Generate graph representing the Kilifi county 
#Load data from excel
Kilifi_GP_proportion = read_excel("./Genotype_Data.xlsx",sheet = "Kilifi_Genotype_Proportions",col_names=T,
                                  col_types = NULL, na = "",skip = 0)

#melt the data
#panel b
Kilifi_GP_proportion.m = melt(Kilifi_GP_proportion,id.vars="Genotype")

Kilifi = ggplot(Kilifi_GP_proportion.m,aes(x=Genotype,y=value,fill=variable))+ 
  geom_bar(stat = "identity",width=.3)+scale_y_continuous(name = "GP Proportions")+
  scale_fill_manual(values = c("gray50","steelblue1","#a6611a","#018571","#E4E46A","red4","#333300"))+
  scale_x_discrete(name = "Year",labels=c("2010","2011","2012","2013","2014",
                                          "2015","2016","2017","2018"))+
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.3,"cm"),
        legend.spacing.x= unit(0.5, 'cm'),
        legend.text = element_text(size=8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black"),
        panel.grid.minor.x = element_blank(), 
        panel.spacing = element_blank(),
        text=element_text(size=10,colour = "black"))

Kilifi
#Generate graph representing the Siaya county 
#Load data from excel 


Siaya_GP_proportion = read_excel("./Genotype_Data.xlsx",sheet = "Siaya_Lwak_Combined_proportions",col_names=T,
                                 col_types = NULL, na = "",skip = 0)
#Melt the data
Siaya_GP_proportion.m = melt(Siaya_GP_proportion,id.vars="Genotype")

Siaya=ggplot(Siaya_GP_proportion.m,aes(x=Genotype,y=value,fill=variable))+ 
  geom_bar(stat = "identity",width=.3)+scale_y_continuous(name = "")+
  scale_fill_manual(values = c("gray50","steelblue1","#a6611a","#018571","#E4E46A","red4","#333300"))+
  scale_x_discrete(name = "Year",labels=c("2010","2011","2012","2013","2014",
                                          "2015","2016","2017","2018"))+
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.3,"cm"),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size=8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black"),
        panel.grid.minor.x = element_blank(), 
        panel.spacing = element_blank(),
        text=element_text(size=10,colour = "black"))

Siaya
#Generate graph representing the Kibera county 
#Load data from excel


Kibera_GP_proportion = read_excel("./Genotype_Data.xlsx",sheet = "Kibera_Genotype_proportions",col_names=T,
                                  col_types = NULL, na = "",skip = 0)

Kibera_GP_proportion.m = melt(Kibera_GP_proportion,id.vars="Genotype")

Kibera = ggplot(Kibera_GP_proportion.m,aes(x=Genotype,y=value,fill=variable))+ 
  geom_bar(stat = "identity",width=.3)+scale_y_continuous(name = "GP Proportions")+
  scale_fill_manual(values = c("gray50","steelblue1","#a6611a","#018571","#E4E46A","red4","#333300"))+
  scale_x_discrete(name = "Year",labels=c("2010","2011","2012","2013","2014",
                                          "2015","2016","2017","2018"))+
  theme(legend.title = element_blank(),
        legend.key.size = unit(0.3,"cm"),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size=8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black"),
        panel.grid.minor.x = element_blank(), 
        panel.spacing = element_blank(), 
        text=element_text(size=10,colour = "black"))

Kibera

#####################


#Combined the individual plot into a single panel plot.
figure = ggarrange(Kilifi, Siaya, Kibera, Kenya, 
                   common.legend = T,legend="bottom",
                   nrow = 2, ncol=2, labels = "auto",font.label = list(size = 10, colour="black")) 
figure





