library(readxl)
library(tidyverse) #data manipulation
library(cluster) #clustering algorithms
library(NbClust)
library(factoextra) #clustering visualization
library(dendextend) #compare two dendrograms and visualization
library(ggpubr)
library(ggplot2)
library(FactoMineR)
library(corrplot)
library(clValid)
library(caret) #normalization/standardization for preprocessing data clustering
library(dplyr)


set.seed(2020)

All_River<- read.csv("/Hierarical clustering-structure.csv", header=TRUE)

summary(All_River)
any(is.na(All_River))

rownames(All_River)<- All_River[,1]
All_River<- All_River[,-1]
#selecting the corresponding columns for each time period
Mat_1993<- All_River[,c(1,2)]
Mat_2005<- All_River[,c(3,4)]
Mat_2017<- All_River[,c(5,6)]

#Std_Deg_River<- as.data.frame(preProcess(Deg_River, method=c("range")))
#Normalisation
Std<- function(x){(x-min(x))/(max(x)-min(x))}

Mat_1993[,1]<-Std(Mat_1993[,1])
Mat_1993[,2]<-Std(Mat_1993[,2])
Mat_2005[,1]<- Std(Mat_2005[,1])
Mat_2005[,2]<- Std(Mat_2005[,2])
Mat_2017[,1]<- Std(Mat_2017[,1])
Mat_2017[,2]<- Std(Mat_2017[,2])

#compute the distance between observations/data samples to allow clustering based on the distance
#allow correlation-based distance measures including 'pearson', 'kendall' and 'spearman'
#computes a distance matrix between the rows of a data matrix
dist_1993<- get_dist(Std_1993, method = 'euclidean')
dist_2005<- get_dist(Std_2005, method = 'euclidean')
dist_2017<- get_dist(Std_2017, method = 'euclidean')

#https://www.datacamp.com/community/tutorials/hierarchical-clustering-R?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=255798340456&utm_targetid=dsa-429603003980&utm_loc_interest_ms=&utm_loc_physical_ms=9069099&gclid=CjwKCAjwtNf6BRAwEiwAkt6UQoOH0zancZ11i28B6xZDsVoXOcg4s7rZqj-lMdXnMFRStqpEnEmwnBoCqicQAvD_BwE
Hclust1_1993<- NbClust(Mat_1993, distance='euclidean', method = "ward.D2")
Hclust2_1993<- hclust(dist_1993, method = "ward.D2")

Hclust1_2005<- NbClust(Mat_2005, distance='euclidean', method = "ward.D2")
Hclust2_2005<- hclust(dist_2005, method = "ward.D2")

Hclust1_2017<- NbClust(Mat_2017, distance='euclidean', method = "ward.D2")
Hclust2_2017<- hclust(dist_2017, method = "ward.D2")

plot(Hclust2_1993)

#cut_River<- cutree(Hclust_River, k=3)
rect.hclust(Hclust2_2017, k=2)
#draw a cut line and superimpose on the rectangular compartments for each cluster
abline(h=0.5, col='yellow')
#another visualization using 'dendextend' libarary
pdf(file = "1993-4 groups rivers.pdf", width = 15, height=12)
bran_River1<- color_branches((Hclust2_1993),k=4)
plot(bran_River1)
dev.off()

pdf(file = "2005-4 groups rivers.pdf", width = 15, height=12)
bran_River2<- color_branches((Hclust2_2005),k=4)
plot(bran_River2)
dev.off()

pdf(file = "2017-4 groups rivers.pdf", width = 15, height=12)
bran_River3<- color_branches((Hclust2_2017),k=4)
plot(bran_River3)
dev.off()

#appending clustering results to rivers
cut_river1<- cutree(Hclust2_1993, k=4)
Clu_River1<- mutate(Mat_1993, cluster1=cut_river1)
write.csv(Clu_River1, file = "1993-hier clus.csv")

cut_river2<- cutree(Hclust2_2005, k=4)
Clu_River2<- mutate(Mat_2005, cluster2=cut_river2)
write.csv(Clu_River2, file = "2005-hier clus.csv")

cut_river3<- cutree(Hclust2_2017, k=4)
Clu_River3<- mutate(Mat_2017, cluster3=cut_river3)
write.csv(Clu_River3, file = "2017-hier clus.csv")

#count how many rivers in each cluster
count(Clu_River3, cluster3)

#plot the groups wrt their standardised values
#http://environmentalcomputing.net/plotting-with-ggplot-adding-titles-and-axis-names/
#https://ggplot2.tidyverse.org/reference/geom_text.html
pdf(file = "1993-4 groups rivers VS structure2.pdf", width = 12, height=12)
ggplot(Clu_River1, aes(x=Std_1993[,1], y=Std_1993[,2], color=factor(cluster1), label=rownames(Std_1993)))+geom_point()+labs(y="Normalised closeness", x="Normalised degree", colour="River clusters")+geom_text(check_overlap = TRUE)
dev.off()

pdf(file = "2005-4 groups rivers VS structure2.pdf", width = 12, height=12)
ggplot(Clu_River2, aes(x=Std_2005[,1], y=Std_2005[,2], color=factor(cluster2), label=rownames(Std_2005)))+geom_point()+labs(y="Normalised closeness", x="Normalised degree", colour="River clusters")+geom_text(check_overlap = TRUE)
dev.off()

pdf(file = "2017-4 groups rivers VS structure2.pdf", width = 12, height=12)
ggplot(Clu_River3, aes(x=Std_2017[,1], y=Std_2017[,2], color=factor(cluster3), label=rownames(Std_2017)))+geom_point()+labs(y="Normalised closeness", x="Normalised degree", colour="River clusters")+geom_text(check_overlap = TRUE)
dev.off()

#finding optimum k
fviz_nbclust(Hclust1_2017, method = "wss")

#Elbow method: minimise within-cluster sum of square
fviz_nbclust(Mat_2017, kmeans, method = "wss")

#Average Silhouette Method: measure the quality of a clustering
#a high average silhouette width indicates a good clustering
#the optimal number of clusters k is the one that maximizes the average silhouette over a range of possible k
fviz_nbclust(Mat_2017, kmeans, method = "silhouette")
