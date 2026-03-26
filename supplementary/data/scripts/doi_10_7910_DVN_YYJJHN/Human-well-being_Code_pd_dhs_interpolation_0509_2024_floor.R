#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

library(foreign)
library(plyr)
library(sf)
library(caret)
library(mgcv)
library(xgboost)
library(glmnet)
library(INLA)
library(ggplot2)
library(sf)
library(inlabru)
library(gridExtra)
library(raster)
library(sp)
library(dplyr)

##### using DHS to create interpolated surfaces of human health and wellbeing indicators#####
### this will implement the following steps
# 1: summarize the DHS data to each cluster location
# 2: summarize the covariates to each cluster location
# 3: fit three submodels: GAM, LASSO, GBM
# 4: use the results of the submodels in a Bayseian geostatistical model using INLA



##### 1. summarize the DHS data to each cluster location#####

#import DHS data - this is the household recode file and will be used to calculate floor type
#the unit of analysis is households
#use other recode files for child or maternal variables
HR<- read.dta("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/SNHR8BDT/SNHR8BFL.DTA", convert.factors = FALSE)

#subset the data to include relevent variables
sub_df<-subset(HR, select=c(hv001, hv002, hv005, hv024, hv213)) #floor and household variables

#rename the variables
names(sub_df)<-c("clust_no", "hh_no", "sample_wt", "region", "floor")

#create the weight variable
sub_df$sample_wt <- sub_df$sample_wt/1000000

#remove households with missing floor information
sub_df<-subset(sub_df, sub_df$floor<90)

#create binary variable that shows rudimentary/unfinished floor
sub_df$floor[sub_df$floor %in% c(10:19)] = 1 # dirt or dung
sub_df$floor[sub_df$floor %in% c(20:29)] = 2 # wood, bamboo, adobe, rocks
sub_df$floor[sub_df$floor %in% c(30:39)] = 3 # ceramic, concrete, or carpet
sub_df$floor[sub_df$floor %in% c(35)] = 4 # other

sub_df$floor_pct<-ifelse(sub_df$floor<3,1,0)


sub_df$floor_wt<-ifelse(sub_df$floor<3,sub_df$sample_wt,0)

#calculate the percent positive cases by cluster (number of HH with electricity out of all HH in a cluster)
floor_clus<-ddply(sub_df,~clust_no,summarise,pct_floor=weighted.mean(floor_pct, sample_wt))

#count the number of households within a cluster (to be used in INLA stack)
join_df<-sub_df %>%
  group_by(clust_no) %>%
  summarise(floor_hh=sum(floor_wt),
            hh=sum(sample_wt))



##### 2. summarize the covariates for each cluster location#####
#import necessary data
gps_dhs<- st_read("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/sen_19_gps/SNGE8BFL.shp") #gps coordinates for DHS clusters

#create raster stack of covariates 
r_list<-vector(mode='list')

files_all <- list.files(path="/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/covariates_5km/",
                        pattern='*tif$',full.names=TRUE)
l<-files_all

for(i in 1:length(files_all)){
  
  r_list[[i]] <- raster::stack(files_all[i])
  
}


stack_all<-stack(r_list)
stack_all<-setNames(stack_all, l)
NAvalue(stack_all)= -9999

#clean up names
names(stack_all) <- c("ai", "evi","lstd", "lstn", "nl", "pet", "popd", "prcp", "tt")


#extract pixel value for each DHS cluster for each covariate
  ##normally I would take an average pixel value for a 10km buffer around the DHS cluster but ##
  ##this is not done here to keep with the standard methods ##
  ##the DHS randomly displaces each gps coordinate up to 10km to preserve confidentiality ##
  ##the DHS tested their interpolation methods on a non-displaced dataset and 
  ##found small differences (more info in their SAR 11)##
dkc<-extract(stack_all,gps_dhs,fun=mean,na.rm=TRUE,sp=FALSE,buffer=1,df=TRUE) #extract raster data to each DHS cluster

#add DHS ID to each cluster
dkc$dhsid<-gps_dhs$DHSID
dkc<-dplyr::select(dkc,dhsid,everything())
dkc<-dplyr::select(dkc,-ID)

#clean up the names
#names(dkc)<-c("ai", "evi","lstd", "lstn", "nl", "pet", "popd", "prcp", "tt")

#add DHS ID to electricity data
floor_clus$dhsid<-gps_dhs$DHSID
floor_clus<-dplyr::select(floor_clus,dhsid,everything())
floor_clus<-dplyr::select(floor_clus,-clust_no)

#merge the DHS data with the covariate data and add gps data
df<-merge(floor_clus, dkc, by="dhsid")
df<-merge(df, gps_dhs, by.x="dhsid", by.y="DHSID")

df<-merge(df, join_df, by.x="DHSCLUST", by.y="clust_no")


#export a DHS data and raster stack as R data file
save(df, file="/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/exports/df_floor.RData")
#save(stack_all, file="/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/exports/stack_all.RData")

rm(list=ls())

##### 3. Fit three submodels - GAM, LASSO, GBM

# Generalized Additive Model
setwd("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/exports/")
load("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/exports/df_floor.RData")
load("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/exports/stack_all.RData")
sen_shp<- st_read("/Users/rburrows/Dropbox/USAID RA/GIS_Boundaries/Shapefiles/sen_admbnda_adm0_1m_gov_ocha_20190426.shp")


#remove any NA values
df=na.omit(df)

logistic_transformation <- function(x) {
  1 / (1 + exp(-x))
}


#attach(df)
gam_model<-gam(pct_floor~ s(ai) + s(evi) + s(lstd) + s(lstn) + s(nl) + s(pet) + s(popd) + s(pet) + s(prcp) +s(tt), data = df, family = gaussian())
#detach(df)
gam_pred<-predict(gam_model, df)
gam_pred_log<-logistic_transformation(gam_pred)

#log transformation
gam_rast_binary <- predict(stack_all, gam_model, type = "response")
gam_rast_binary <- logistic_transformation(gam_rast_binary)
#gam_rast_binary <- ifelse(gam_rast_binary > threshold, 1, 0)
#gam_rast_binary_raster <- raster(gam_rast_binary, crs = crs(stack_all), xmn = xmin(stack_all), xmx = xmax(stack_all), ymn = ymin(stack_all), ymx = ymax(stack_all), ncols = ncol(stack_all), nrows = nrow(stack_all))
plot(gam_rast_binary, col = RColorBrewer::brewer.pal(11, "RdBu"), main = "Prediction with GAM (Binary)")

#export raster
writeRaster(gam_rast_binary, filename="gam_raster_floor_binary", format = "GTiff")





#LASSO
#due to compatability issues, the raster stack is converted to a matrix and the predictions are made for the matrix
#the values from the matrix are then added to a raster with the same resolution, extent, and projection as the covariates
x<-data.matrix(df[, c("ai", "evi","lstd", "lstn", "nl", "pet", "popd", "prcp", "tt")])
y<-df$pct_floor #define response variable 

cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
lasso_pred<-predict(best_model, newx = x)
lasso_pred_log<-logistic_transformation(lasso_pred)
x_pred <- as.matrix(stack_all) #convert raster with covariates into a matrix in preparation for prediction

#prep a raster grid to make the prediction over Senegal
r<-raster("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/covariates_5km/evi_r.tif")
new_raster<-raster(extent(r), nrows = nrow(r), ncols = ncol(r), crs = crs(r))

# LASSO log
r<-raster("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/covariates_5km/evi_r.tif")
new_raster_binary<-raster(extent(r), nrows = nrow(r), ncols = ncol(r), crs = crs(r))
lasso_raster_binary <- predict(best_model, s = best_lambda, newx = x_pred, type = "response")
lasso_raster_binary <- logistic_transformation(lasso_raster_binary)
values(new_raster_binary) <- lasso_raster_binary

plot(new_raster_binary, col = RColorBrewer::brewer.pal(11, "RdBu"), main = "Predictions")



#export raster
writeRaster(new_raster_binary, filename="las_raster_floor_binary", format = "GTiff")



#GBM
#similar to the LASSO model, predictions are made over a data frame and then converted to a raster

# Convert the raster stack to a matrix
raster_df <- raster::as.data.frame(stack_all, xy = TRUE)

xgb_matrix <- xgb.DMatrix(data = as.matrix(df[,c("ai", "evi","lstd", "lstn", "nl", "pet", "popd", "prcp", "tt")]), label = df$pct_floor)
gbm_model <- xgboost(data = xgb_matrix, nrounds = 100, objective = "reg:squarederror")
xgb_pred_df<- predict(gbm_model, xgb_matrix)
xgb_pred_df_log<-logistic_transformation(xgb_pred_df)

# GBM
xgb_raster_binary <- predict(gbm_model, as.matrix(raster_df[,-c(1, 2)]), outputmargin = TRUE)
xgb_raster_binary_log <- logistic_transformation(xgb_raster_binary)
xgb_raster_binary_ext<-raster(extent(r), nrows = nrow(r), ncols = ncol(r), crs = crs(r)) #raster from LASSO prediction
values(xgb_raster_binary_ext) <- xgb_raster_binary_log
plot(xgb_raster_binary_ext, col = RColorBrewer::brewer.pal(11, "RdBu"), main = "Prediction with GBM (Binary)")

#export raster
writeRaster(xgb_raster_binary_ext, filename="xgb_raster_floor_binary", format = "GTiff")


#Combine predictions into a new data frame
predict_df_bin <- data.frame(gam = gam_pred_log, lasso = lasso_pred_log, gbm = xgb_pred_df_log)
predict_df_bin$clus_no <- rownames(predict_df_bin)
bind <- merge(df, predict_df_bin, by.x=c("DHSCLUST"), by.y=c("clus_no"))
predict_df_bin <- subset(bind, select=c(gam, s0, gbm, pct_floor, LATNUM, LONGNUM, floor_hh, hh))


#save dataset
save(predict_df_bin, file="predict_df_floor_bin.RData")

rm(list=ls())





##### 4. use the results of the submodels in a Bayseian geostatistical model using INLA####
##in this section we take the results of the three submodels and use them in the Bayesian MBG model

#import the rasters from the previous steps
gam_r<-raster("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/submodel_rasters/floor/binary/gam_raster_floor_binary.tif")
lasso_r<-raster("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/submodel_rasters/floor/binary/las_raster_floor_binary.tif")
gbm_r<-raster("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/submodel_rasters/floor/binary/xgb_raster_floor_binary.tif")

#import df with predictions and shapefile of DHS clusters
load("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/exports/predict_df_floor_bin.RData")
sen_shp<- st_read("/Users/rburrows/Dropbox/USAID RA/GIS_Boundaries/Shapefiles/sen_admbnda_adm0_1m_gov_ocha_20190426.shp")
gps_dhs<- st_read("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/sen_19_gps/SNGE8BFL.shp") #gps coordinates for DHS clusters
load("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/exports/stack_all.RData")




#create mesh around the DHS points that will be used in the interpolation
#I use the coordinates of the DHS clusters and the Senegal boundary to create the mesh
coords = cbind(predict_df_bin$LONGNUM, predict_df_bin$LATNUM)
bdry<- inla.sp2segment(sen_shp)
bdry$loc <- inla.mesh.map(bdry$loc)

mesh3 <- inla.mesh.2d(loc = coords, boundary = bdry, max.edge=c(0.1,5)) #same max edge as DHS workflow
plot(mesh3) #plot mesh to make sure there aren't too many small triangles

#create the spatial structure
A<-inla.spde.make.A(mesh=mesh3,loc=as.matrix(coords));dim(A)
spde <- inla.spde2.matern(mesh3, alpha=2)
iset <- inla.spde.make.index(name = "spatial.field", n.spde=spde$n.spde)

predict_df_bin$floor_hh_round<-floor(predict_df_bin$floor_hh)
predict_df_bin$hh_round<-floor(predict_df_bin$hh)

#create INLA stack, applies the A matrix to the spatial effect of the variable at the DHS cluster coordinates
#there are three main inputs: the data, projector matrix, list of effects
stk <- inla.stack(data=list(y=predict_df_bin$floor_hh_round, n=predict_df_bin$hh_round), #NOTE (CHANGE n= here to the number of households)
                  
                  A=list(A,1),  #the A matrix
                  
                  effects=list(c(list(Intercept=1), #the Intercept
                                 iset),  #the spatial index
                               #the covariates which are the results of the submodels
                               list(las = predict_df_bin$s0,
                                    gbm_m = predict_df_bin$gbm,
                                    gam_m = predict_df_bin$gam)
                  ), 
                  tag='df')


#fit the model
formula0<-y ~ -1 + Intercept + las + gbm_m + gam_m + f(spatial.field, model=spde) 

model0<-inla(formula0, #the formula
             data=inla.stack.data(stk,spde=spde),  #the data stack from the previous step
             family= 'binomial',
             Ntrials = n,#continuous data
             control.predictor=list(A=inla.stack.A(stk),compute=TRUE),  
             control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
             control.inla = list(strategy = "adaptive", control.vb = list(emergency = 50)), #model diagnostics and config = TRUE gives you the GMRF
             verbose = FALSE)
             

#check the model results
model0$summary.fixed

#look at the spatial field of this model
gproj <- inla.mesh.projector(mesh3,  dims = c(300, 300))
g.mean <- inla.mesh.project(gproj, model0$summary.random$spatial.field$mean)
g.sd <- inla.mesh.project(gproj, model0$summary.random$spatial.field$sd)

grid.arrange(levelplot(g.mean, scales=list(draw=F), xlab='', ylab='', main='mean',col.regions = heat.colors(16)),
             levelplot(g.sd, scal=list(draw=F), xla='', yla='', main='sd' ,col.regions = heat.colors(16)), nrow=1)


#model prediction
#the following code uses the INLA model results to predict the prevalence of the variable on a grid of Senegal
#in this code I will do this outside of the INLA function to streamline computational costs

#first create the grid for prediction
reference.image <- raster("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/covariates_5km/evi_r.tif")
in.country <- which(!is.na(getValues(reference.image)))
reference.coordinates <- coordinates(reference.image)[in.country,]
pred.points<-SpatialPoints(coords = reference.coordinates)
proj4string(pred.points) <- CRS(st_crs(gps_dhs)$proj4string)
covs <- list.files('/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/submodel_rasters/floor/binary/', full.names = T) %>% stack()
pred.covs <- raster::extract(covs, pred.points, df=T) #this will take some time

#make observation matrix for the prediction
Aprediction <- inla.spde.make.A(mesh = mesh3, loc = reference.coordinates); #this may take some time
dim(Aprediction)
#load("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/exports/Aprediction.RData")


#covariates for prediction (these have been extracted over the grid)
gam_p <- pred.covs$gam_raster
las_p <- pred.covs$las_raster
gbm_p <- pred.covs$xgb_raster

#create spatial structure
sfield_nodes <- model0$summary.random$spatial.field['mean']
field <- (Aprediction %*% as.data.frame(sfield_nodes)[, 1])

#make empty matrix to fill predictions
pred <- matrix(NA, nrow = dim(Aprediction)[1], ncol = 1)

## Calculate Predicted values using regression formula
pred <- model0$summary.fixed['Intercept', 'mean'] +
  model0$summary.fixed['las', 'mean'] * las_p +
  model0$summary.fixed['gbm_m', 'mean'] * gbm_p +
  model0$summary.fixed['gam_m', 'mean'] * gam_p +
  field 

results <- exp(pred)/(1+exp(pred))

# write results to a raster
x <- as.matrix(reference.coordinates)
z <- as.matrix(results)
pr.floor.out <- rasterFromXYZ(cbind(x, z))

#plot raster
plot(pr.floor.out, col = RColorBrewer::brewer.pal(11, "YlGnBu"), main = "Households with unfinished floor(%)")

proj4string(pr.floor.out) <- CRS(st_crs(sen_shp)$proj4string)
writeRaster(pr.floor.out, filename = "pred_floor_0515.tif", format = "GTiff", overwrite = TRUE)




#####scrap#####


tt<-raster("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/covariates/export_travelTime_new.tif")
nl<-raster("/Users/rburrows/Dropbox/Geography PhD/Project Drawdown work/Data/covariates/export_nightLights_new.tif")

