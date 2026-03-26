## Flu B genetic distance analysis
getwd()
setwd("/Volumes/Data-Curation/VEC/Influenza_FestusNyasimi/R_code_and_data/data/")
#Loading packages
#install.packages("tidyverse")
#install.packages("melt")
library(tidyverse)
library(reshape2)
#install.packages("ggplot2")
library(ggplot2)

##Reading the csv data (the p genetic distance data was generated from Mega 7.02 in a long format)

flub_lineage <- read.csv("KHDSS_All_lineages_genetic_Tr_dist.csv",header = T, sep=",")
yamagataall <- read.csv("KHDSS_All_Yamagata_genetic_dist.csv",header = T, sep=",")
head(yamagataall)


##Melt the data to transform it
lineage <- melt(flub_lineage,variable.name ="region",value.name="genetic_distance")
yamclades <- melt(yamagataall,variable.name ="region",value.name="genetic_distance")

##Figure 3 (a): Box plot for lineages
ggplot(data = lineage, aes(x=region, y=genetic_distance)) + 
  geom_boxplot(outlier.colour = "red", aes(fill=Lineage)) +
  labs(x = "Gene segment", y = "Genetic distance") 

##Figure 3 (a): Box plot for Yamagata clades
ggplot(data = yamclades, aes(x=region, y=genetic_distance)) + 
  geom_boxplot(outlier.colour = "red", aes(fill=Clade)) +
  labs(x = "Gene segment", y = "Genetic distance") 


