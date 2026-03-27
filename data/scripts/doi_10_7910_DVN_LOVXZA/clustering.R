#####
# Supplementary code for submission to The Plant Journal
# Performs clustering analysis utilizing market-class
# related shape and size parameters on carrot roots drawn from 5
# common U.S. carrot market classes
####

library(here)
library(factoextra)

phenos <- read.csv("clustering_pheno.csv", as.is = T, header = T)

mc_pca <- prcomp(phenos[c(4:12)], center = TRUE, scale = TRUE)
fviz_pca_ind(mc_pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = phenos$market_class, 
             col.ind = "black",   
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Market Class") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))
