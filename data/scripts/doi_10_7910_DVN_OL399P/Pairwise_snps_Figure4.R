library(tidyverse)
library(reshape2)
library(ggplot2)

#script to generate distance plots sequemce pairwise distances
#use this script to run the following datasets i.e. RV-A12, RV-A49, RV-A78, RV-A101; RV-C2, RV-C3, RV-C6, RV-C11, RV-C21, RV-C38


#Load the pairwise csv file

VP1<- read.csv("RV-A101.csv", header = T)


#Melt the data
VP1.m<- melt(VP1, variable.name ="Strain2",value.name="Dist")

#Run ggplot
ggplot(VP1.m, aes(Dist)) +
  geom_bar(show.legend = F) +
  labs(y="Number of sequence pairs",x="Number of pairwise SNPs")+
  theme_minimal() +
  theme(text=element_text(size=8),axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 10))

