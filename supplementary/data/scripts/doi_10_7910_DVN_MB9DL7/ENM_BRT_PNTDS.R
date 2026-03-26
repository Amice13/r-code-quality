library(maps)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(mapdata)
library(gbm)
library(dismo)
library(ResourceSelection)
library(dplyr)
library(tidyverse)
library(SDMPlay)
library(spatstat)
library(xlsx)
library(MASS)
library(spatialEco)
library(pROC)
library(randomForest)
library(boot)
library(Hmisc)
library(verification)
memory.limit(size=40000)
inputs<- ("C:/RF_data/Inputs") #data path
setwd(inputs)

#Read spatial data
#-------------------
studarea_bnd <- readOGR("historicalCentWestSouthSubs_bnd.shp")
kenya1<-readOGR("kenya1.shp")
projcrs <- crs(kenya1)
maskedbnd<- readOGR("historicalWestSouthSubsLess5kmbuff.shp")
plot(maskedbnd)

#read occurrences-presence CSV
#-------------------------------
anthrax_occurences <- read.csv("occurrence69Points.csv",header=TRUE, sep=",")
anthrax_occurences <-anthrax_occurences[,3:4]
head(anthrax_occurences)

#accessing and stacking the output files 
#---------------------------------------
setwd("C:\\RF_data\\Inputs\\VIF\\BRT") #Tiffs path
tifFiles <- Sys.glob ('*.tif')
predictors_anthrax <-stack(tifFiles,quick
                           =F)
#loop run steps
#--------------
#set.seed(100)
mylist<-list()
bs.list <- list()
dps <- list()
nrep=5
for (r in 1:nrep){
  setwd("C:\\RF_data\\Inputs\\VIF\\runs_brt2")
  print(paste("run", r))
  set.seed(100)
  
  #generate random points
  anthrax_pseudos<-spsample(maskedbnd,n=69,"random", cellsize=5000)
  anthrax_pseudos_df<-data.frame(anthrax_pseudos)
  anthrax_pseudos_df<-anthrax_pseudos_df[,1:2]
  plot(anthrax_pseudos)
  
  #convert dataframes to shape
  anthrax_occurences_points <- SpatialPointsDataFrame(anthrax_occurences,anthrax_occurences,proj4string = projcrs)
  head(anthrax_occurences_points)
  
  #plot(anthrax_occurences_points,Add=T)
  anthrax_pseudos_points <- SpatialPointsDataFrame(anthrax_pseudos,anthrax_pseudos_df,proj4string = projcrs)
  head(anthrax_pseudos_points)

  #training(75%) and Testing (25%) 
  #---------------------------------
  #presence data
  group<-kfold(anthrax_occurences_points,10)
  prescence_train<-anthrax_occurences_points[group!=1,]  #set 75% of the presence data as your training data
  prescence_test<-anthrax_occurences_points[group==1,]   #set 25% of the presence data as your testing data
  
  #pseudo-absence data
  group<-kfold(anthrax_pseudos_points,10)
  absence_train<-anthrax_pseudos_points[group!=1,]   #set 75% of the absence data as your training data
  absence_test<-anthrax_pseudos_points[group==1,]    #set 25% of the absence data as your training data
  
  #ploting training abscence points
  #-------------------------------
  train_ponts <- rbind( prescence_train , absence_train)
  plot(maskedbnd)
  points (train_ponts)
  
  #extracting train prescence and abscence data
  #----------------------------------------------
  file1<-raster::extract(predictors_anthrax,prescence_train,method='simple')
  outcome<-rep(1,dim(file1)[[1]])
  file1<-cbind(outcome,file1)
  
  file2<-raster::extract(predictors_anthrax,absence_train,method='simple')
  outcome<-rep(0,dim(file2)[[1]])
  file2<-cbind(outcome,file2)
  
  train_data<-as.data.frame(rbind(file1,file2))

  write.csv(train_data,paste0(getwd(),Sep="/","trainData.csv"),row.names=TRUE,col.names=FALSE)                       
  
  #extracting testing data
  #--------------------------
  pre_testdata<-raster::extract(predictors_anthrax,prescence_test,method='simple')
  outcome<-rep(1,dim(pre_testdata)[[1]])
  pre_testdata<-cbind(outcome,pre_testdata)
  pre_testdata<-na.omit(pre_testdata)

  
  abs_testdata<-raster::extract(predictors_anthrax,absence_test,method='simple')
  outcome<-rep(0,dim(abs_testdata)[[1]])
  abs_testdata<-cbind(outcome,abs_testdata)
  abs_testdata<-na.omit(abs_testdata)

  
  #presence absence test data
  #----------------------------
  test_data<-as.data.frame(rbind(pre_testdata,abs_testdata))
  
  #modelling & Eavluation
  #########################

  #fitting model and selecting variables
  #######################################
  col<-ncol(train_data)
  brt_step <-gbm.step(data = train_data, gbm.x = c(2:col), gbm.y = 1,family = "bernoulli", tree.complexity = 5,learning.rate = 0.001, bag.fraction = 0.5,max.trees = 2500,n.folds = 10)
        #n.trees=brt_simplify_step$n.tree, type='response', format='GTiff',overwrite=TRUE)
  
  #storing brt runnodel objects
  #-----------------------------
  brt_step.bs <- try(gbm.step(data=train_data[sample(NROW(train_data), NROW(train_data), replace=T),], gbm.x =c(2:col),
                            gbm.y = 1, family = "bernoulli", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,max.trees = 2500,n.folds = 10,
                            verbose=TRUE, silent=FALSE, plot.main=TRUE))
  if (class(brt_step.bs) == "try-error") next
  bs.list[[r]] <- brt_step.bs
  cat("This is replicate number ", r, "\n")
  save(bs.list, file="bs.list.Rdata")
  rm(brt_step.bs)

  #storing run AUcs
  #----------------
  AUC<-brt_step$cv.statistics$discrimination.mean
  mylist<-append(mylist,AUC)
}
#create AUC excel file
#--------------------------
write.xlsx(unlist(mylist),"AUC.xlsx")

#Get CI values
#--------------
CI.mat <- matrix(ncol=nrep, nrow=124)
for (i in 1:nrep) { CI.mat[,i] <- predict.gbm(bs.list[[i]], newdata=train_data, n.trees=2500,
                                            type="response")
  CIs <- apply(CI.mat, 1, quantile, c(0.25,0.5, 0.975), na.rm=T)
 }
  
# BRT partial plots with CI
#--------------------------
xlims=c(0,520)
partial <- plot.gbm(brt_step, i.var="CDens", return.grid=T)
plot.new()
plot(partial, type="l", main="", col="blue",ylab="", xlim=xlims, xlab="EVI", ylim=c(-0.15,0.2),
     yaxs="i", las=1, xaxs="r", cex.lab=1, tcl=0)
abline(h=0, col="red")
newx <- seq(min(train_data$CDens, na.rm=T), max(train_data$CDens, na.rm=T),len=124)

#bootstrap lines:
lines(newx, predict.gbm(brt_step, newdata=train_data, n.trees=200, type="response"), col="grey40", lwd=2,
      type="s")
matlines(newx, t(CIs), col=rgb(.2,.2,.2,.4), lwd=3, type="o", lty=c(2,1,2),xlim = xlims,ylim=c(-0.15,0.2))
  
#creating mean of the predictions and plot
#-----------------------------------------
tifFiles <- Sys.glob('*.tif') 
predicted<-stack(tifFiles,quick=F)
mean_narm = function(x,...){mean(x,na.rm=TRUE)}
Predict_mean<- do.call(overlay, c(predicted, fun = mean_narm))
writeRaster(Predict_mean,filename='mean_predictBRT', format="GTiff", overwrite=TRUE)
meanPred<-raster("C:\\RF_data\\Inputs\\VIF\\runs_brt2\\mean_predictBRT.tif")
plot(meanPred)







