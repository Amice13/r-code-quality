##########################################################################
########################### 1.0 Load required libraries 


library(sp)
library(raster)
library(ggplot2)
library(lattice)
library(caret)
library(sf)
library(dplyr)
library(terra)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(rasterVis)


#####################################################################################
############## CALLING THE STUDY AREA SHAPEFILE (TAIN II RESERVE) ################

study_area <- st_read("C:/Users/USER/Desktop/Data repository/Study area Shapefile/TAIN II FOREST RESERVE.shp")
plot(study_area)

####################################################################################
########  LOADING AND CLIPPING JANUARY SATELLITE BANDS TO THE STUDY AREA #################################
####################################################################################


Blue_Jan <- raster("C:/Users/USER/Desktop/Data repository/January/LC08_L2SP_195055_20220121_20220128_02_T1_SR_B2.TIF")
Blue_Jan <- crop(Blue_Jan,study_area)
Blue_Jan_clip <- mask(Blue_Jan,study_area)
plot(Blue_Jan_clip)

Green_Jan <- raster("C:/Users/USER/Desktop/Data repository/January/LC08_L2SP_195055_20220121_20220128_02_T1_SR_B3.TIF")
Green_Jan <- crop(Green_Jan,study_area)
Green_Jan_clip <- mask(Green_Jan,study_area)
plot(Green_Jan_clip)

Red_Jan <- raster("C:/Users/USER/Desktop/Data repository/January/LC08_L2SP_195055_20220121_20220128_02_T1_SR_B4.TIF")
Red_Jan <- crop(Red_Jan,study_area)
Red_Jan_clip <- mask(Red_Jan,study_area)
plot(Red_Jan_clip)

NIR_Jan <- raster("C:/Users/USER/Desktop/Data repository/January/LC08_L2SP_195055_20220121_20220128_02_T1_SR_B5.TIF")
NIR_Jan <- crop(NIR_Jan,study_area)
NIR_Jan_clip <- mask(NIR_Jan,study_area)
plot(NIR_Jan_clip)

SWIR1_Jan <- raster("C:/Users/USER/Desktop/Data repository/January/LC08_L2SP_195055_20220121_20220128_02_T1_SR_B6.TIF")
SWIR1_Jan <- crop(SWIR1_Jan,study_area)
SWIR1_Jan_clip <- mask(SWIR1_Jan,study_area)
plot(SWIR1_Jan_clip)

SWIR2_Jan <- raster("C:/Users/USER/Desktop/Data repository/January/LC08_L2SP_195055_20220121_20220128_02_T1_SR_B7.TIF")
SWIR2_Jan <- crop(SWIR2_Jan,study_area)
SWIR2_Jan_clip <- mask(SWIR2_Jan,study_area)
plot(SWIR2_Jan_clip)


#######################################################################################
############## LOADING AND CLIPPING MAY SATELLITE BANDS TO THE STUDY AREA ###########################


Blue_May <- raster("C:/Users/USER/Desktop/Data repository/May/LC08_L2SP_195055_20220513_20220519_02_T1_SR_B2.TIF")
Blue_May <- crop(Blue_May,study_area)
Blue_May_clip <- mask(Blue_May,study_area)
plot(Blue_May_clip)


Green_May <- raster("C:/Users/USER/Desktop/Data repository/May/LC08_L2SP_195055_20220513_20220519_02_T1_SR_B3.TIF")
Green_May <- crop(Green_May,study_area)
Green_May_clip <- mask(Green_May,study_area)
plot(Green_May_clip)
     
     
Red_May <- raster("C:/Users/USER/Desktop/Data repository/May/LC08_L2SP_195055_20220513_20220519_02_T1_SR_B4.TIF")
Red_May <- crop(Red_May,study_area)
Red_May_clip <- mask(Red_May,study_area)
plot(Red_May_clip)

NIR_May <- raster("C:/Users/USER/Desktop/Data repository/May/LC08_L2SP_195055_20220513_20220519_02_T1_SR_B5.TIF")
NIR_May <- crop(NIR_May,study_area)
NIR_May_clip <- mask(NIR_May,study_area)
plot(NIR_May_clip)

SWIR1_May <- raster("C:/Users/USER/Desktop/Data repository/May/LC08_L2SP_195055_20220513_20220519_02_T1_SR_B6.TIF")
SWIR1_May <- crop(Blue_May,study_area)
SWIR1_May_clip <- mask(SWIR1_May,study_area)
plot(SWIR1_May_clip)


SWIR2_May <- raster("C:/Users/USER/Desktop/Data repository/May/LC08_L2SP_195055_20220513_20220519_02_T1_SR_B7.TIF")
SWIR2_May <- crop(SWIR2_May,study_area)
SWIR2_May_clip <- mask(SWIR2_May, study_area)
plot(SWIR2_May_clip)


###########################################################################################
###########        CALCULATING SPECTRAL INDICES FOR JANUARY BANDS
#######################################################################################


######## NORMALIZED DIFFERENCE VEGETATION INDEX (NDVI)

NDVI_Jan <-(NIR_Jan_clip-Red_Jan_clip)/(NIR_Jan_clip+Red_Jan_clip)   #Normalized Difference Vegetation Index (NDVI)
plot(NDVI_Jan)

############# SOIL ADJUSTED VEGETATION INDEX (SAVI)
SAVI_Jan <- 1.5* (NIR_Jan_clip-Red_Jan_clip)/(NIR_Jan_clip+Red_Jan_clip+0.5)
plot(SAVI_Jan)

############# ENHANCED VEGETATION INDEX (EVI)
EVI_Jan <- 2.5*(NIR_Jan_clip-Red_Jan_clip)/(NIR_Jan_clip+Red_Jan_clip+0.5)
plot(EVI_Jan)

NDVI_Jan
SAVI_Jan
EVI_Jan

######################################################################################
#######    CALCULATING SPECTRAL INDICES FOR MAY BANDS
###################################################################################


######## NORMALIZED DIFFERENCE VEGETATION INDEX (NDVI)

NDVI_May <-(NIR_May_clip-Red_May_clip)/(NIR_May_clip+Red_May_clip)   
plot(NDVI_May)

############# SOIL ADJUSTED VEGETATION INDEX (SAVI)
SAVI_May <- 1.5* (NIR_May_clip-Red_May_clip)/(NIR_May_clip+Red_May_clip+0.5)
plot(SAVI_May)

############# ENHANCED VEGETATION INDEX (EVI)
EVI_May <- 2.5*(NIR_May - Red_May)/(NIR_May + Red_May + 0.5)
plot(EVI_May)

NDVI_May
SAVI_May
EVI_May


##############################################################################
##################### VEGETATION INDEX DIFFERENCING ############################

NDVI_Difference <- (NDVI_May) - (NDVI_Jan)
plot(NDVI_Difference)

SAVI_Difference <- (SAVI_May - SAVI_Jan)
plot(SAVI_Difference)

EVI_Difference <- (EVI_May - EVI_Jan)
plot(EVI_Difference)

Red_Difference <- (Red_May - Red_Jan)
plot(Red_Difference)

Blue_Difference <- (Blue_May -Blue_Jan)
plot(Blue_Difference)

Green_Difference <- (Green_May - Green_Jan)
plot(Green_Difference)

NIR_Difference <- (NIR_May -NIR_Jan)
plot(NIR_Difference)

SWIR1_Difference <- (SWIR1_May - SWIR1_Jan)
plot(SWIR1_Difference)

SWIR2_Difference <- (SWIR2_May - SWIR2_Jan)
plot(SWIR2_Difference)

NDVI_Difference
SAVI_Difference
EVI_Difference
Red_Difference
Blue_Difference
Green_Difference
NIR_Difference
SWIR1_Difference
SWIR2_Difference


##############################################################################
#############    VEGETATION INDEX RATIOING     ####################################
##############################################################################

NDVI_Ratio <- (NDVI_May / NDVI_Jan)
plot(NDVI_Ratio)

SAVI_Ratio <- (SAVI_May / SAVI_Jan)
plot(SAVI_Ratio)

EVI_Ratio <- (EVI_May / EVI_Jan)
plot(EVI_Ratio)

Red_Ratio <- (Red_May / Red_Jan)
plot(Red_Ratio)

Blue_Ratio <- (Blue_May / Blue_Jan)
plot(Blue_Ratio)

Green_Ratio <- (Green_May / Green_Jan)
plot(Green_Ratio)

NIR_Ratio <- (NIR_May /NIR_Jan)
plot(NIR_Ratio)

SWIR1_Ratio <- (SWIR1_May/SWIR1_Jan)
plot(SWIR1_Ratio)

SWIR2_Ratio <- (SWIR2_May/SWIR2_Jan)
plot(SWIR1_Ratio)

NDVI_Ratio
SAVI_Ratio
EVI_Ratio
Red_Ratio
Blue_Ratio
Green_Ratio
NIR_Ratio
SWIR1_Ratio
SWIR2_Ratio


######################################################################################
###########         STACKING, CLIPPING AND NAMING BANDS    ############################################
######################################################################################

Difference_Image<- stack(NDVI_Difference, SAVI_Difference, EVI_Difference, Red_Difference, Blue_Difference, Green_Difference, NIR_Difference, SWIR1_Difference, SWIR2_Difference)
Difference_image_clipped <- mask(Difference_Image, study_area)
names(Difference_image_clipped) <- c("NDVI_Difference", "SAVI_Difference", "EVI_Difference", "Red_Difference", "Blue_Difference", "Green_Difference", "NIR_Difference", "SWIR1_Difference", "SWIR2_Difference")
plot(Difference_image_clipped)


Ratio_Image<-stack(NDVI_Difference, SAVI_Difference, EVI_Difference, Red_Difference, Blue_Difference, Green_Difference, NIR_Difference, SWIR1_Difference, SWIR2_Difference)
Ratio_image_clipped <- mask(Ratio_Image, study_area)
names(Ratio_image_clipped) <- c("NDVI_Ratio", "SAVI_Ratio", "EVI_Ratio", "Red_Ratio", "Blue_Ratio", "Green_Ratio", "NIR_Ratio", "SWIR1_Ratio", "SWIR2_Ratio")
plot(Ratio_image_clipped)

Dry_season_image <- stack(NDVI_Jan, SAVI_Jan, EVI_Jan, Red_Jan, Green_Jan, Blue_Jan, NIR_Jan, SWIR1_Jan, SWIR2_Jan)
Dry_season_image_clipped <- mask(Dry_season_image, study_area)
names(Dry_season_image_clipped) <- c("NDVI_Dry_season", "SAVI_Dry_season", "EVI_Dry_season", "Red_Dry_season", "Blue_Dry_season", "Green_Dry_season", "NIR_Dry_season", "SWIR1_Dry_season", "SWIR2_Dry_season")
plot(Dry_season_image_clipped)


######################################################################################################################################################
############# LOADING TRAINING POINTS SHAPEFILE ######################################################################################################################

training_points <- st_read("C:/Users/USER/Desktop/Data repository/Coordinates/Classification coordinates/classification shapefile.shp")
training_points
plot(training_points, add=TRUE)


#################################################################################################################################################################
#############      LOADING VALIDATION POINTS SHAPEFILE      ############################################################################################

validation_points <- st_read("C:/Users/USER/Desktop/Data repository/Coordinates/Validation coordinates/VALIDATION SHAPEFILE.shp")
validation_points
plot(validation_points, add=TRUE)



############################################################################################################################################################
################################# EXPLORING THE CRS OF THE SHAPEFILE #########################################################################################################

compareCRS(Difference_image_clipped, training_points)
compareCRS(Ratio_image_clipped, training_points)
compareCRS(Dry_season_image_clipped, training_points)



###################################################################################################################
##########     CREATING DATA FRAME WITH TRAINING POINTS    ##############################################################################


## Repeat the process from this point by replacing the "difference image" with the 
#####  other two images; "Ratio image" and "Dry season image" to classify each

LULC <- as.factor(training_points$class)
LULC
Coord_FieldGPS<- st_coordinates(training_points)
Coord_FieldGPS
training_values <- raster::extract(Difference_image_clipped, Coord_FieldGPS)
training_values<-as.data.frame(training_values)
training_values<-cbind(LULC, training_values)



#########################################################################################################################
############### CREATING DATA FRAME WITH VALIDATION POINTS #############################################################

LULC_2 <- as.factor(validation_points$class)
LULC_2
Coord_FieldGPS2<- st_coordinates(validation_points)
Coord_FieldGPS2
validationvalues<-raster::extract(Difference_image_clipped,Coord_FieldGPS2)
validationvalues<-as.data.frame(validationvalues)
validationvalues<-cbind(LULC_2,validationvalues)


########################################################################################################
##############   RANDOM FOREST MODEL    #########################################################

set.seed(123)
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
RF_model <- train(LULC~.,data = training_values, method="rf",trControl=fitControl,prox=TRUE,
                  fitBest = FALSE,
                  returnData = TRUE)

##########################################################################################################################
####################     CHECKING MODEL PARAMETERS    ############################################################

RF_model$finalModel

###########################################################################################################################
##################ASSESSING VARIABLE IMPORTANCE########################################################

RF_VarImportance <- varImp(RF_model, compete = FALSE)
plot(RF_VarImportance)


#################################################################################################################################
##############    CLASSIFYING THE IMAGE USING RANDOM FOREST MODEL   ##################################################################

classified_image <-predict(Difference_image_clipped,RF_model)
classified_image
plot(classified_image)


#####################################################################################################################
################PERFORMING ACCURACY ASSESSMENT##########################################################

predict_RF <- predict(RF_model$finalModel,
                      newdata = validationvalues)
confusionMatrix(data = predict_RF, validationvalues$LULC_2)
em_table <- table(data = predict_RF, validationvalues$LULC_2)



####################################################################################################################
####################    OVERALL ACCURACY       ########################################################################

overall_accuracy <-(sum(diag(em_table))/sum(em_table))*100
overall_accuracy

######################################################################################################################
#####################    USER ACCURACY      #####################################################################################

User_accuracy <- (diag(em_table)/rowSums(em_table))*100
User_accuracy

###########################################################################################################################
######################   PRODUCER ACCURACY    ##############################################################################

Producer_accuracy <- (diag(em_table)/colSums(em_table))*100
Producer_accuracy


####################################################################################################################
##########    SAVING THE CLASSIFIED IMAGE TO THE WORKING DIRECTORY    ##################################################

writeRaster(classified_image,filename = ("C:/Users/USER/Desktop/EVERYTHING PROJECT/Project/Data/L8 Jan 2022 image/preprocessed/classified1"),format="GTiff",overwrite=TRUE)







