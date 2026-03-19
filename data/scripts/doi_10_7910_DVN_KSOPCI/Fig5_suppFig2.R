

setwd("")
rm(list=ls())

# required packages

# Install the Biostrings package if you haven't already
install.packages("BiocManager")
   require("BiocManager")
   BiocManager::install("Biostrings")

require("pacman")
pacman::p_load(seqinr, tidyverse, stringr, Biostrings, ggforce, patchwork, factoextra, cowplot, ggridges)

# load data #####

# fasta
fastaFile <- readDNAStringSet("./data/anonym_sequences.fasta")
# csv
fastaFile_metadata <- read.csv("./data/anonym_metadata.csv") %>%
  dplyr::select(pseudo_id, site, scale, type, date_collect)%>%
  dplyr::mutate(new_id = as.factor(pseudo_id))

# extract header and sequence
new_id = names(fastaFile)
sequence = paste(fastaFile)

fastaFile_as_df<-data.frame(new_id, sequence)
# merge csv and fasta
fastaFile_metadata <- fastaFile_metadata %>%
  left_join(., fastaFile_as_df, by = 'new_id')
# format date
fastaFile_metadata$date_collect <- as.Date(fastaFile_metadata$date_collect, '%d/%m/%Y')
# view
fastaFile_metadata[c(1:3),]

# Define waves #### 
## based on epidemic curves/ calendar dates that infections occurred 
  ### no more than 14 days between subsequent samples of the same wave 
        # this step ain't fun :(

## a15 ####
a15_waves <- fastaFile_metadata %>%
  dplyr::filter(type == "A15" 
                & !new_id == "863") %>% #looks like a misclassified seq
  dplyr::arrange(date_collect) %>%
  mutate(wave = case_when (
    site =="Matsangoni" ~ "wave1",
    site =="Ngerenya" ~ "wave1",
    site =="Sokoke" ~ "wave1",
    (site =="Mtondia" | site =="KCH") & date_collect < as.Date("2016-05-01") ~ "wave1",
    (site =="Mtondia" | site =="KCH") & between(date_collect, 
                                                as.Date("2016-05-02"), as.Date("2016-08-02")) ~ "wave2",
    (site =="Mtondia" | site =="KCH") & date_collect > as.Date("2016-08-03") ~ "wave3",
    site =="Mavueni" & date_collect < as.Date("2016-07-15") ~ "wave1",
    site =="Mavueni" & date_collect > as.Date("2016-08-01") ~ "wave2",
    site =="Jaribuni" & date_collect < as.Date("2016-09-01") ~ "wave1",
    site =="Jaribuni" & date_collect > as.Date("2016-09-01") ~ "wave2",
    site =="Chasimba" ~ "wave1",
    site =="Pingilikani" & date_collect < as.Date("2016-07-01") ~ "wave1",
    site =="Pingilikani" & between(date_collect, as.Date("2016-07-02"), as.Date("2016-09-05")) ~ "wave2",
    site =="Pingilikani" & date_collect > as.Date("2016-09-15") ~ "wave3",
    site =="Junju" & date_collect < as.Date("2016-06-01") ~ "wave1",
    site =="Junju" & between(date_collect, as.Date("2016-06-02"), as.Date("2016-08-01" )) ~ "wave2",
    site =="Junju" & date_collect > as.Date("2016-08-02" ) ~ "wave3"
  ))

## c11 ####
c11_waves <- fastaFile_metadata %>%
  dplyr::filter(type == "C11") %>%
  dplyr::arrange(date_collect) %>%
  mutate(wave = case_when (
    site =="KCH" ~ "wave1",
    (site =="Matsangoni" | site =="Sokoke" | site =="Jaribuni" | site =="Chasimba" | site =="Pingilikani" | site =="Junju") & date_collect < as.Date("2016-05-21") ~ "wave1",
    (site =="Matsangoni" | site =="Sokoke" | site =="Jaribuni" | site =="Chasimba" | site =="Pingilikani" | site =="Junju") & date_collect > as.Date("2016-05-21") ~ "wave2",
    (site =="Ngerenya" | site =="Mavueni") & date_collect > as.Date("2016-05-01") ~ "wave2",
    (site =="Ngerenya" | site =="Mavueni") & date_collect > as.Date("2016-05-01") ~ "wave3"
  ))

## c1 ####
c1_waves <- fastaFile_metadata %>%
  dplyr::filter(type == "C1") %>%
  dplyr::arrange(date_collect) %>%
  mutate(wave = case_when (
    (site =="Ngerenya" | site =="Mavueni") & date_collect < as.Date("2016-07-01") ~ "wave1",
    (site =="Ngerenya" | site =="Mavueni") & date_collect > as.Date("2016-07-01") ~ "wave2",
    (site =="Mtondia" | site =="Sokoke" | site =="KCH") & date_collect < as.Date("2016-04-01") ~ "wave1",
    (site =="Mtondia" | site =="Sokoke" | site =="KCH") & date_collect > as.Date("2016-04-01") ~ "wave2",
    site =="Matsangoni" & date_collect < as.Date("2016-05-01") ~ "wave1",
    site =="Matsangoni" & between(date_collect, as.Date("2016-05-01"), as.Date("2016-07-01")) ~ "wave2",
    site =="Matsangoni" & date_collect > as.Date("2016-07-01") ~ "wave3",
  ))

## a22 ####
a22_waves <- fastaFile_metadata %>%
  dplyr::filter(type == "A22") %>%
  mutate_at(                                      
    "site", recode,
    "KNH" = "Nairobi",
    "Kibera" = "Nairobi",
    "KCH" = "Kilifi"
  ) %>%
  mutate(wave = case_when (
    (site =="Mombasa" | site =="Nyeri" | site =="Nairobi" | site =="Kakuma") & date_collect < as.Date("2014-05-01") ~ "wave1",
    (site =="Mombasa" | site =="Nyeri" | site =="Nairobi" | site =="Kakuma") & date_collect > as.Date("2014-05-01") ~ "wave2",
    site =="Kilifi" & date_collect < as.Date("2014-02-01") ~ "wave1",
    site =="Kilifi" & between(date_collect, as.Date("2014-02-01"), as.Date("2014-05-01")) ~ "wave2",
    site =="Kilifi" & between(date_collect, as.Date("2014-05-01"), as.Date("2014-08-01")) ~ "wave3",
    site =="Kilifi" & between(date_collect, as.Date("2014-08-01"), as.Date("2014-09-30")) ~ "wave4",
    site =="Kilifi" & date_collect > as.Date("2014-09-21") ~ "wave5",
    site =="Mombasa" & date_collect > as.Date("2016-10-01") ~ "wave3",
    site =="Siaya" & date_collect < as.Date("2014-02-01") ~ "wave1",
    site =="Siaya" & date_collect > as.Date("2014-02-01") ~ "wave2",
    site =="Kakamega" ~ "wave1"
  ))

## a34 ####
a34_waves <- fastaFile_metadata %>%
  dplyr::filter(type == "A34") %>%
  mutate_at(                                      
    "site", recode,
    "KNH" = "Nairobi",
    "Kibera" = "Nairobi",
    "KCH" = "Kilifi"
  ) %>%
  mutate(wave = case_when (
    site =="Nakuru" ~ "wave1",
    (site =="Mombasa" | site =="Siaya" | site =="Nairobi") & date_collect < as.Date("2014-05-01") ~ "wave1",
    (site =="Mombasa" | site =="Siaya" | site =="Nairobi") & between(date_collect, as.Date("2014-05-01"), as.Date("2014-08-01")) ~ "wave2",
    (site =="Siaya" | site =="Nairobi")  & date_collect > as.Date("2014-08-01") ~ "wave3",
    site =="Kakamega" & date_collect < as.Date("2014-03-15") ~ "wave1",
    site =="Kakamega" & between(date_collect, as.Date("2014-03-15"), as.Date("2014-08-01")) ~ "wave2",
    site =="Kakamega" & between(date_collect, as.Date("2014-08-01"), as.Date("2014-11-01")) ~ "wave3", 
    site =="Kakamega"  & date_collect > as.Date("2014-11-01") ~ "wave4"
    
  ))

## a49 ####
a49_waves <- fastaFile_metadata %>%
  dplyr::filter(type == "A49") %>%
  mutate_at(                                      
    "site", recode,
    "KNH" = "Nairobi",
    "Kibera" = "Nairobi",
    "KCH" = "Kilifi"
  ) %>%
  dplyr::arrange(date_collect) %>%
  mutate(wave = case_when (
    site =="Nairobi" ~ "wave1",
    (site =="Mombasa" | site =="Siaya" | site =="Nakuru") & date_collect < as.Date("2014-03-01") ~ "wave1",
    (site =="Siaya" | site =="Nakuru") & date_collect > as.Date("2014-04-01") ~ "wave2",
    site =="Mombasa" & between(date_collect, as.Date("2014-05-01"), as.Date("2014-08-01")) ~ "wave2",
    site =="Mombasa" & between(date_collect, as.Date("2014-08-01"), as.Date("2014-11-01")) ~ "wave3",
    site =="Mombasa" & date_collect > as.Date("2014-11-01") ~ "wave4",
    site =="Kilifi" & date_collect < as.Date("2014-08-01") ~ "wave1",
    site =="Kilifi" & between(date_collect, as.Date("2014-08-01"), as.Date("2014-11-01")) ~ "wave2",
    site =="Kilifi" & date_collect > as.Date("2014-11-01") ~ "wave3"
  ))

## c22 ####
c22_waves <- fastaFile_metadata %>%
  dplyr::filter(type == "C22") %>%
  dplyr::arrange(date_collect) %>%
  mutate(wave = case_when (
    (site =="Matsangoni" | site =="Ngerenya" | site =="KCH" | site =="Mavueni" | site =="Jaribuni" | site =="Chasimba" |  site =="Junju") ~ "wave1",
    site =="Sokoke" & date_collect < as.Date("2016-06-20") ~ "wave1",
    site =="Sokoke" & date_collect > as.Date("2016-06-20") ~ "wave2",
  ))


## Euclidean Distances based on seq similarity #####

pca_function  <- function(dataset){
  # as matrix
  sites <- as.matrix(dataset %>% 
                       drop_na(wave) %>% 
                       dplyr::select(new_id, sequence))
  
  # print seqs per analysis
  print(paste(dim(sites)[1], "sequences"))
  
  # finding the size of data
  n_sample <- dim(sites)[1]
  n_seq <- nchar(sites[2,2])
  
  ### translation of the sequence to boolean vectors 
  bool <- array(0, dim=c(n_sample, 5*n_seq))
  
  colnames(bool) <- c(paste("A_", 1:n_seq, sep=""),paste("T_", 1:n_seq, sep=""),paste("G_", 1:n_seq, sep=""),paste("C_", 1:n_seq, sep=""),paste("N_", 1:n_seq, sep=""))
  rownames(bool) <- sites[ ,1]
  # Euclidean Distances
  # loop through sequence bases 
  for (s in 1:n_sample){
    se <- sites[s, 2]
    se <- tolower(se)
    
    for (le in  1:n_seq){
      base <- substr(se, le,le)
      
      if(base =="a") {
        bool[s, le] <-1
      } else {
        
        if(base =="t") {
          bool[s, le+n_seq] <-1
        } else {
          
          if(base =="g") {
            bool[s, le+n_seq*2] <-1
          } else {
            
            if(base =="c") {
              bool[s, le+n_seq*3] <-1
            } else {
              
              bool[s, le+n_seq*4] <-1
            }}}}
    }}
  bool<- bool
  #return(list_of_outputs)
  # centering : the center can be replaced to certain group
  center<- apply(bool, 2, mean)
  diffs<-sweep(bool, 2, center)
  # compensating the doubled counts in Euclidean distance metrics
  diffs <- diffs/(2^0.5) 
  
  ### PCA core
  res_svd <<- svd(diffs)  # single vector decomposition
  #str(res_svd)
  Left <- res_svd$u		# the left singular vector
  Right <- res_svd$v		# the right singular vector
  sqL <- diag(res_svd$d)		# diagonal matrix of the singular values
  
  ### calculatinf of pc's 
  sPC_nuc 	<-	 Right %*% sqL / (n_sample^0.5)
  sPC_sample <-	 Left %*% sqL/ (n_seq^0.5)
  
  rownames(sPC_nuc)<- colnames(bool) 
  rownames(sPC_sample)<- rownames(bool)
  
  sPC_sample <- data.frame(sPC_sample)
  sPC_sample$new_id <- rownames(sPC_sample)
  
  dataset <- dataset %>%
    drop_na()%>%
    left_join(., sPC_sample, by = "new_id")
  
  return(dataset)
  
  sPC_sample <<- sPC_sample
  
}

# Run PCA and find number of PCs that contribute most ( >10% contribution)
set.seed(123)
a15_pca <- pca_function(a15_waves)
# plot contribution of PCs
plot(1:20, (res_svd$d/sum(res_svd$d)*100)[1:20], pch=1, type="b",  
     lty=3, ylab="(%)", xlab="PC", main="Contribution", col="gray50") # PCs 1 and 2

a22_pca <- pca_function(a22_waves)
# plot contribution of PCs
plot(1:20, (res_svd$d/sum(res_svd$d)*100)[1:20], pch=1, type="b",  
     lty=3, ylab="(%)", xlab="PC", main="Contribution", col="gray50") # PCs 1-3 

a34_pca <- pca_function(a34_waves)
# plot contribution of PCs
plot(1:20, (res_svd$d/sum(res_svd$d)*100)[1:20], pch=1, type="b",  
     lty=3, ylab="(%)", xlab="PC", main="Contribution", col="gray50") # PCs 1 and 2

a49_pca <- pca_function(a49_waves)
# plot contribution of PCs
plot(1:20, (res_svd$d/sum(res_svd$d)*100)[1:20], pch=1, type="b",  
     lty=3, ylab="(%)", xlab="PC", main="Contribution", col="gray50") # PCs 1 and 2

c1_pca <- pca_function(c1_waves)
# plot contribution of PCs
plot(1:20, (res_svd$d/sum(res_svd$d)*100)[1:20], pch=1, type="b",  
     lty=3, ylab="(%)", xlab="PC", main="Contribution", col="gray50") # PCs 1 and 2

c11_pca <- pca_function(c11_waves)
# plot contribution of PCs
plot(1:20, (res_svd$d/sum(res_svd$d)*100)[1:20], pch=1, type="b",  
     lty=3, ylab="(%)", xlab="PC", main="Contribution", col="gray50") # PCs 1 and 2


# Figure 5C #####
install.packages("ggplot2")
library(ggplot2)
theme_set(theme_bw())

customColors <- c("mediumpurple1","palevioletred1","palegreen2","royalblue2", "black")
customColors <- c("violetred1","darkgoldenrod","seagreen3","deepskyblue3","darkorchid")

ggplot_all<- function(mydata){
  ggplot()+
    geom_point(data=mydata, aes(x=X1, y=X2, colour=wave),size=1.5, position=position_jitter(h=0.02, w=0.02)) +
    labs(title = mydata[['type']][2], x = "PC1", y="PC2")+
    scale_colour_manual(values=customColors)+
    #stat_ellipse(data=mydata, aes(x=X1, y=X2),type = "norm", linetype = 2)+
    #ggforce::geom_mark_ellipse(data=mydata,aes(x=X1, y=X2)) +
    # geom_ellipse(aes(x0 = 0.01, y0 = 0, a =0.1, b = 0.1, angle = 90))+
    # geom_ellipse(aes(x0 = -0.4, y0 = 0, a =0.1, b = 0.1, angle = 90))+
    theme(legend.position = 'bottom') +
    coord_equal()
}

ggplot_all(a15_pca)
ggplot_all(a22_pca)
ggplot_all(a34_pca)
ggplot_all(a49_pca)
ggplot_all(c1_pca)
ggplot_all(c11_pca)



## K-means clustering #######

# find optimal number of clusters for kmeans clustering (use PCs with highest contribution per dataset)
    # wss(within cluster sums of squares) method 
install.packages("factoextra")
library(factoextra)

fviz_nbclust(scale(a15_pca[,c(9:10)]), kmeans, nstart=100, method = "wss") # n=3 clusters
fviz_nbclust(scale(a22_pca[,c(9:11)]), kmeans, nstart=100, method = "wss") #n=4 
fviz_nbclust(scale(a34_pca[,c(9:10)]), kmeans, nstart=100, method = "wss") #n=4
fviz_nbclust(scale(a49_pca[,c(9:10)]), kmeans, nstart=100, method = "wss") #n=3
fviz_nbclust(scale(c1_pca[,c(9:10)]), kmeans, nstart=100, method = "wss")  #n=4
fviz_nbclust(scale(c11_pca[,c(9,10)]), kmeans, nstart=100, method = "wss") #n=3

# k-means clustering
# number of centers are guided by scree plots above (using wss method)
k_means_clustering <- function(dataset2, centers){
  #drop_nas
  dataset2 <- dataset2 %>% drop_na()
  #k-means
  kmeans_basic <- kmeans(dataset2[,-c(1:8)], # keep only PC info for clustering
                         centers = centers)
  kmeans_basic_table <- data.frame(kmeans_basic$size, kmeans_basic$centers)
  kmeans_basic_df <- data.frame(Cluster = kmeans_basic$cluster, dataset2)
  kmeans_basic_df$Cluster <- as.factor(kmeans_basic_df$Cluster)
  return(kmeans_basic_df)
}

install.packages("magrittr")
library(magrittr)
library(tidyr)
a15_kmeans<-k_means_clustering(a15_pca[,-c(11:ncol(a15_pca))], 3) # drop all PCs except PC 1 and 2
a22_kmeans<-k_means_clustering(a22_pca[,-c(12:ncol(a22_pca))], 4) # drop all PCs except PC 1 ,2 and 3
a34_kmeans<-k_means_clustering(a34_pca[,-c(11:ncol(a34_pca))], 4) # drop all PCs except PC 1 and 2
a49_kmeans<-k_means_clustering(a49_pca[,-c(11:ncol(a49_pca))], 3) # drop all PCs except PC 1 and 2
c1_kmeans<-k_means_clustering(c1_pca[,-c(11:ncol(c1_pca))], 4)    # drop all PCs except PC 1 and 2
c11_kmeans<-k_means_clustering(c11_pca[,-c(11:ncol(c11_pca))],3)  # drop all PCs except PC 1 and 2


# Plot ###

merged <- rbind(a15_kmeans[, 1:ncol(a15_kmeans)], a22_kmeans[, 1:(ncol(a22_kmeans)-1)], a34_kmeans[, 1:ncol(a34_kmeans)], 
                a49_kmeans[, 1:ncol(a49_kmeans)],c1_kmeans[, 1:ncol(c1_kmeans)], c11_kmeans[, 1:ncol(c11_kmeans)]) %>%
  dplyr::rename(Wave = wave)



customColors2 <- c("violetred1","deepskyblue3","darkgoldenrod","seagreen3", "darkorchid" )

Fig5C <- ggplot(data = merged) +
  geom_point(aes(x=X1, y =X2, colour = Wave, shape = Cluster), alpha= 0.8, size=3,
             position=position_jitter(h=0.03, w=0.01, seed = 189))  +
  # ADD ggforce's ellipses
  ggforce::geom_mark_ellipse(aes(x=X1, y =X2, fill = Cluster), linetype = 2, 
                             size =0.1, alpha=0.09) +
  theme(legend.position = 'bottom', 
        #legend.text = element_text(size = 14),
        text=element_text(size = 17),
        strip.background = element_blank(),
        strip.placement = "outside")+
  coord_equal() + labs(x = 'PC1', y = 'PC2') +
  facet_wrap(vars(type))+
  #scale_shape_manual(values = c(0,1,2,5))+
  scale_colour_manual(values=customColors2)


# Figure 5B #######
# Create a plot to aid in visualizing how we defined epidemic curves 
# Define waves visualization
install.packages("ggridges")
library(ggridges)
library(ggplot2)
library(magrittr)
Fig5B <- a34_kmeans %>%
  ggplot(., aes(x = date_collect, y=site)) +
  geom_density_ridges2(fill= 'cyan4',bandwidth=10,alpha = 0.5, scale = 0.5)+
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  theme_bw()+ylab("Density")+ xlab(NULL)+ 
  theme(axis.text = element_text(size=8),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_rect(colour=NA, fill=NA)) +
  geom_text(aes(label=wave), size = 3, nudge_y = -0.2,  check_overlap = TRUE)+
  geom_segment(aes(x = as.Date("2014-04-01"), y = 5.3, 
                   xend = as.Date("2014-05-03"), yend = 5.3, linetype="At least 14 days"), lineend = "round") +
  geom_segment(aes(x = as.Date("2014-08-01"), y = 5.3, 
                   xend = as.Date("2014-10-03"), yend = 5.3), lineend = "round") +
  geom_segment(aes(x = as.Date("2014-04-01"), y = 3.3, 
                   xend = as.Date("2014-05-03"), yend = 3.3), lineend = "round") +
  geom_segment(aes(x = as.Date("2014-07-01"), y = 3.3, 
                   xend = as.Date("2014-08-03"), yend = 3.3), lineend = "round") +
  geom_segment(aes(x = as.Date("2014-04-01"), y = 2.3, 
                   xend = as.Date("2014-05-03"), yend = 2.3), lineend = "round") +
  geom_segment(aes(x = as.Date("2014-03-01"), y = 1.3, 
                   xend = as.Date("2014-04-03"), yend = 1.3), lineend = "round") +
  geom_segment(aes(x = as.Date("2014-06-01"), y = 1.3, 
                   xend = as.Date("2014-08-30"), yend = 1.3), lineend = "round") +
  geom_segment(aes(x = as.Date("2014-10-25"), y = 1.3, 
                   xend = as.Date("2014-11-13"), yend = 1.3), lineend = "round") +
  scale_linetype_manual(values=c("At least 14 days"=1))

Fig5B

# Suppl Figure 2 ####
ggplot_facet<- function(data){
  ggplot(data=data)+
    geom_point(aes(x=X1, y =X2, colour = wave, shape = Cluster),alpha=0.7, size=2, 
               position=position_jitter(h=0.02, w=0.01, seed = 189))  +
    # ADD ggforce's ellipses
    ggforce::geom_mark_ellipse(aes(x=X1, y =X2, fill = Cluster), linetype = 2, 
                               size =0.1, alpha=0.1)+
    theme(legend.position = 'right', strip.background = element_blank(),
          strip.placement = "outside")+
    coord_equal() + labs(subtitle = data[['type']][2], x = 'PC1', y = 'PC2') +
    facet_wrap(vars(site),ncol = 4)+
    scale_colour_manual(values=customColors)
  # theme(strip.background = element_rect(fill='#FAFAFA'))
  
}



p1<- ggplot_facet(a15_kmeans) + theme(legend.position="NONE")+ 
  ggtitle("KHDSS", subtitle = "A15")+
  theme(plot.title = element_text(hjust = 0.5))

p2<- ggplot_facet(a22_kmeans) + ggtitle("Countrywide", subtitle = "A22")+
  theme(plot.title = element_text(hjust = 0.5))

p3<- ggplot_facet(a34_kmeans)
p4<- ggplot_facet(a49_kmeans)
p5<- ggplot_facet(c1_kmeans)+ theme(legend.position="NONE")
p6<- ggplot_facet(c11_kmeans) + theme(legend.position="NONE")

# combine
suppFig2<- p1+p2+p5+p3+p6+p4+plot_layout(ncol = 2, widths = c(1,1)) # save to pdf for better rendering

suppFig2



