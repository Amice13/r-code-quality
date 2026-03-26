## This code is used to compute the variable importance for the XGB models (all and fatality only) for each region

#packages
lspack <- c("raster","sp","pdp", "ggplot2","viridis","scales","gplots","reshape2",
            "xtable","dplyr", "caret", "rgdal", "rgeos", "countrycode","foreach","doParallel","rmapshaper","doSNOW","xgboost")

new.packages <- lspack[!(lspack %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(lspack, library, character.only = TRUE)

#check prevalence of terrorism (percentage of positive week-cells) for each region
mapid <- c("A", "B","C","K","L","J", "I","D","H","E","G","F")
regionfull <-c("North America", "Central America & Caribbean", "South America", 
               "East Asia", "Southeast Asia", "South Asia", "Central Asia","Europe (EU28 & Schengen)",
               "Russia & East Europe", "Middle East & North Africa (MENA)", "South Africa",
               "West Africa")
mapid <- data.frame(Index=mapid,Region=regionfull)

#1. VARIABLE IMPORTANCE
#load RDS files saved from XGB.R from which variable importance will be computed
impl <- list()
for (i in 1:length(regions)){
  # the RDS files are large so the process might take a few minutes to run
  imp <- readRDS(paste0("results/xgb/xgb-until-2016_",regions[[i]],".Rds"))
  imp <- caret::varImp(imp,useModel = FALSE)
  imp <- data.frame(imp$importance)
  imp <- data.frame(feature=rownames(imp),importance=imp[,1],Index=mapid[i,]$Index,Region=mapid[i,]$Region)
  imp$importance <- round(imp$importance,0)
  impl[[i]]<- imp
  impl[[i]]$feature <- as.character(impl[[i]]$feature)
  #do not include country effect here for merging and better comparison of the results
  impl[[i]]<- impl[[i]][!grepl("country", impl[[i]]$feature),]
  #rename structural variables for publication
  impl[[i]]$feature[impl[[i]]$feature == 'x'] <- 'longitude (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'y'] <- 'latitude (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'altitude'] <- 'altitude (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'border'] <- 'distance to nearest national border (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'capital'] <- 'distance to nearest capital city (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'conf'] <- 'number of conflict events (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'popdens'] <- 'population density (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'lum'] <- 'satellite night lights (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'access'] <- 'travel time to the nearest large city (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'gem'] <- 'gems or diamonds deposits (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'imr'] <- 'infant mortality rate (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'mountain'] <- 'mountain coverage (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'oil'] <- 'petroleum deposits (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'drug'] <- 'large-scale drug cultivation (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'excethn'] <- 'excluded ethnic groups (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'gcp'] <- 'gross cell product (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'grip'] <- 'road network density (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'gold'] <- 'gold deposits (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'oil'] <- 'petroleum deposits (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'LDI'] <- 'liberal democracy index (country)'
  impl[[i]]$feature[impl[[i]]$feature == 'pcgdp'] <- 'per capita GDP (country)'
  #rename procedural variables for publication
  impl[[i]]$feature[impl[[i]]$feature == 'terrorl1'] <- 'terrorism in week lag 1 (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'terrorl2'] <- 'terrorism in week lag 2 (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'terrorl3'] <- 'terrorism in week lag 3 (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'terrorl4'] <- 'terrorism in week lag 4 (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'terrorl_month'] <- 'terrorism in month lag 1 (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'total_terror_l1'] <- 'terrorism in week lag 1 (country)'
  impl[[i]]$feature[impl[[i]]$feature == 'total_terror_l2'] <- 'terrorism in week lag 2 (country)'
  impl[[i]]$feature[impl[[i]]$feature == 'total_terror_l3'] <- 'terrorism in week lag 3 (country)'
  impl[[i]]$feature[impl[[i]]$feature == 'total_terror_l4'] <- 'terrorism in week lag 4 (country)'
  impl[[i]]$feature[impl[[i]]$feature == 'total_terror_l52'] <- 'terrorism in year lag 1 (country)'
  impl[[i]]$feature[impl[[i]]$feature == 'total_terror_month'] <- 'terrorism in month lag 1 (country)'
  impl[[i]]$feature[impl[[i]]$feature == 'total_terror_change'] <- 'change of terrorism in the past (country)'
  impl[[i]]$feature[impl[[i]]$feature == 'time_since_terror'] <- 'time since previous terrorist event (cell)'
  impl[[i]]$feature[impl[[i]]$feature == 'terror_100km_l1'] <- 'terrorism in week lag 1 (within 100km radius)'
}

#list of all variables
allfeat<-data.frame(Feature = unique(impl[[1]]$feature))
allfeat$Feature <- as.factor(allfeat$Feature)
names(allfeat)<-"Feature"
rownames(allfeat)<-1:nrow(allfeat)
library(xtable)
print(xtable(allfeat, type = "latex"),include.rownames=FALSE, file = paste0("results/varimportance/allfeaturesfat.tex"))
write.csv(allfeat, paste0("results/varimportance/allfeaturesfat.csv"),row.names = FALSE)

#put lists together
imp <- do.call(rbind,impl)
gbv <- 'Index'
find.maximum <- TRUE
x <-imp[ order( imp[ , gbv ] , decreasing = find.maximum ) , ]
# figure out the ranks of each variables, within region columns
if ( find.maximum ){
  x$ranks <- unlist( rev( tapply( -x$importance , x[ , gbv ] , rank,ties.method = 'min' ) ) )
} else {
  x$ranks <- unlist( tapply( x$importance , x[ , gbv ] , rank, ties.method = 'min' ) )
}
#plot matrix of importance per region and per feature
library(gplots);library(reshape2)
impmat <- x[c("importance","Index","feature")]
#reorder based on category
fnames <- unique(impmat$feature)
type <- c(rep("structural",20),rep("procedural",14))
type <- data.frame(feature=fnames,type=type)
impmat <- merge(impmat,type,by="feature")
#compute general average of each feature to use it to sort the data
overallrank <- aggregate(impmat$importance, by=list(feature=impmat$feature),FUN=mean)
overallrank  <- overallrank[order(overallrank$x,decreasing=TRUE),]
overallrank$rank <- 1:nrow(overallrank)
overallrank  <- merge(overallrank,type,by="feature")

impmat <- merge(impmat, overallrank,by="feature")
impmat <- impmat[order(impmat$rank,decreasing=FALSE),]

#get rank for all variables to reorder matrix based on rownumber
overallrank  <- overallrank[order(overallrank$feature,decreasing=FALSE),]
heatmat<-reshape2::acast(impmat, feature~Index, value.var="importance")
heatmat <-cbind(heatmat,round(rowMeans(heatmat),0))
colnames(heatmat)[13] <- "A-L"
#reorder by average mean across region (last column)
heatmat <- heatmat[order(heatmat[,ncol(heatmat)],decreasing=TRUE),]

save(heatmat,file="results/varimportance/heatfat.Rdata")

#plot heatmap
col_breaks = c(seq(0,100,length=11))
mycol <- viridis(10)
lwid = c(0.1,1)
lhei = c(0.3,6.5,1)
lmat = rbind(c(0,3),c(2,1),c(0,4))

#color in grey procedural variables
cols <- c('grey50', rep('black', 5), rep('grey50', 1),rep('black', 3),
          rep('grey50', 2),rep('black', 2),rep('grey50', 2),rep('black', 2),
          rep('grey50', 1),rep('black', 4),'grey50','black',rep('grey', 5),
          rep('black', 3),'grey50')

plot.new()
pdf(file="results/varimportance/heatmapfat.pdf",family="serif",width=10,height=10)
par(cex.main=2, cex.lab=1, cex.axis=1) 
hm <- gplots::heatmap.2(heatmat,lmat = lmat,lwid = lwid, lhei = lhei,
                        cellnote = heatmat,  # same data set for cell labels
                        Rowv = FALSE,
                        notecol="transparent",# change font color of cell labels to black
                        density.info="none",  # turns off density plot inside color legend
                        trace="none",         # turns off trace lines inside the heat map
                        margins =c(5,31),     # widens margins around plot
                        col=viridis(10),      # use on color palette defined earlier
                        breaks=col_breaks,    # enable color transition at specified limits
                        dendrogram="none",    # only draw a row dendrogram
                        Colv="NA",srtCol=0,key.par=list(mar=c(6,15,1,15)),
                        colRow = cols,
                        adjCol=c(0.5,1),cexRow=1.5,cexCol=1.5, key = TRUE,key.title=NA, # no title,keysize = 0.1,
                        key.xlab="Feature importance (%)")
print(hm)
while (!is.null(dev.list()))  dev.off()


#compute difference in variable importance compared to XGB with all terrorist events
load("results/varimportance/heatfat.Rdata")
heatfat <- heatmat

load("../Main/results/varimportance/heatall.Rdata")
heatall <- heatmat

#remove Australia and average
heatall <- heatall[,-c(13:14)]
#remove average
heatfat <- heatfat[,-c(13)]
heatall <- data.frame(heatall)
heatall$feature <- row.names(heatall)

heatfat <- data.frame(heatfat)
heatfat$feature <- row.names(heatfat)
heatfat<- heatfat[order(heatfat[,ncol(heatfat)],decreasing=FALSE),]
heatall<- heatall[order(heatall[,ncol(heatall)],decreasing=FALSE),]

#check rownames matching
if (identical(rownames(heatall),rownames(heatfat))){
  heatfat <- heatfat[,-c(ncol(heatfat))]
  heatall <- heatall[,-c(ncol(heatall))]
  heat <- heatall- heatfat
  }
#compute mean of differences
heat$'A-L' <- round(rowMeans(heat),0)

#reorder by same order as heatmap 
heat <- heat[t(rownames(heatmat)),]
heat <-as.matrix(heat)

library(plyr)
minbr <-plyr::round_any(min(heat),10)
maxbr <-plyr::round_any(max(heat),10)

#variable importance values may change (randomness)
#therefore the number of colors might need to be adjusted accordingly
col_breaks = c(seq(minbr,maxbr,10))

mycol <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
           "#F7F7F7", "#F7F7F7",#"#D1E5F0",
            "#92C5DE", "#4393C3", "#2166AC", "#053061")
lwid = c(0.1,1)
lhei = c(0.3,6.5,1)
lmat = rbind(c(0,3),c(2,1),c(0,4))

#color in grey procedural variables
cols <- c('grey50', rep('black', 5), rep('grey50', 5),rep('black', 2),
          rep('grey50', 1),rep('black', 3),'grey50',rep('black', 7),
          rep('grey50', 5),rep('black', 3),'grey50')

#plot difference in variable importance between main and robustness specifications
plot.new()
pdf(file="results/varimportance/heatdiff.pdf",family="serif",width=10,height=10)
par(cex.main=2, cex.lab=1, cex.axis=1) 
hm <- gplots::heatmap.2(heat,lmat = lmat,lwid = lwid, lhei = lhei,
                        cellnote = heat,  # same data set for cell labels
                        Rowv = FALSE,
                        notecol="transparent",# change font color of cell labels to black
                        density.info="none",  # turns off density plot inside color legend
                        trace="none",         # turns off trace lines inside the heat map
                        margins =c(5,31),     # widens margins around plot
                        col=mycol,            # use on color palette defined earlier
                        breaks=col_breaks,    # enable color transition at specified limits
                        dendrogram="none",    # only draw a row dendrogram
                        Colv="NA",srtCol=0,key.par=list(mar=c(6,15,1,15)),
                        colRow = cols,
                        adjCol=c(0.5,1),cexRow=1.5,cexCol=1.5, key = TRUE,key.title=NA, 
                        key.xlab="Difference feature importance (%)")
print(hm)
while (!is.null(dev.list()))  dev.off()
#END
