

#load libraries needed for plotting
library(RColorBrewer)
library(ggplot2)


#import main dataset

datafile<-read.csv("Gridded_dataset_April_2020.csv",header=T)

datafile$X<-datafile$Long
datafile$Y<-datafile$Lat
datafile$SteppeMax<-datafile$maxSteppe
datafile$SteppeMin<-datafile$minSteppe

X<-datafile$X
Y<-datafile$Y


###########################################################################################
###         PLOTS WITH PREDICTOR VARIABLES                                              ###


# violin plots showing predictor variables against imperial density

# text below divides up variables into categories based on their distributions

summary(datafile$maxSteppe_distance)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   814.1  2107.4  2400.1  3872.9  6547.5 
Steppe_cats<-cut(datafile$maxSteppe_distance,  c(-0.01,1000,2000,3000,4000,5000,6000,7000), labels=c("0-1","1-2","2-3","3-4","4-5","5-6","6-7"))

summary(datafile$agriBest)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-10700.83  -5569.58  -4272.36  -4436.33  -3172.05    -14.05 
Agri_cats<-cut(datafile$agriBest,  c(-11000,-9000,-8000,-7000,-6000,-5000,-4000,-3000,-2000,0), labels=c("9-11","8-9","7-8","6-7","5-6","4-5","3-4","2-3","0-2")) #note that first and last categories cover 2000 year periods, colour bar only allows 9 colours

summary(datafile$FirstEmp_distance)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0   950.5  1860.8  1912.0  2772.1  5931.3
FE_cats<-cut(datafile$FirstEmp_distance,  c(-0.01,1000,2000,3000,4000,5000,6000), labels=c("0-1","1-2","2-3","3-4","4-5","5-6"))

summary(log10(datafile$elevStd+1))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.604   1.999   1.977   2.383   3.229 
Elev_cats<-cut(log10(datafile$elevStd+1),  c(-0.01,0.5,1,1.5,2,2.5,3,3.5), labels=c("0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-3.5"))

summary(datafile$allYield/10^9) #raw values of yield are very large so divided by 10^9 to make numbers more easily manageable
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000351 0.1748379 0.6162763 0.6405682 1.0119151 2.0678351 
Prod_cats<-cut(datafile$allYield/10^9,  c(-0.01,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25), labels=c("0-0.25","0.25-0.5","0.5-0.75","0.75-1","1-1.25","1.25-1.5","1.5-1.75","1.75-2","2-2.25"))

#add new category variables to the dataset so they can be used in the ggplot2

datafile$Steppe_cats<-Steppe_cats
datafile$Agri_cats<-Agri_cats
datafile$FE_cats<-FE_cats
datafile$Elev_cats<-Elev_cats
datafile$Prod_cats<-Prod_cats



# Violin plots with boxplots added
Steppe_plot <- ggplot(datafile, aes(x=Steppe_cats, y=Total_emp_dens)) + geom_violin(trim=T, scale="width", fill='#A4A4A4', color="darkred") + theme_bw() + geom_boxplot(width=0.1) +labs(x = "Distance from the Steppe (1000 km)", y="Imperial Density")
Steppe_plot

Agri_plot <- ggplot(datafile, aes(x=Agri_cats, y=Total_emp_dens)) + geom_violin(trim=T, scale="width", fill='#A4A4A4', color="darkred") + theme_bw() + geom_boxplot(width=0.1)+labs(x = "Duration of Agriculture (k years)", y="Imperial Density")
Agri_plot

FE_plot <- ggplot(datafile, aes(x=FE_cats, y=Total_emp_dens)) + geom_violin(trim=T, scale="width", fill='#A4A4A4', color="darkred") + theme_bw() + geom_boxplot(width=0.1)+labs(x = "Distance from First Empires (1000 km) ", y="Imperial Density")
FE_plot

Elev_plot <- ggplot(datafile, aes(x=Elev_cats, y=Total_emp_dens)) + geom_violin(trim=T, scale="width", fill='#A4A4A4', color="darkred") + theme_bw() + geom_boxplot(width=0.1)+labs(x = "Std of Elevation (m)", y="Imperial Density")
Elev_plot

Prod_plot <- ggplot(datafile, aes(x=Prod_cats, y=Total_emp_dens)) + geom_violin(trim=T, scale="width", fill='#A4A4A4', color="darkred") + theme_bw() + geom_boxplot(width=0.1)+labs(x = "Potential Productivity 10^7 kcal per hectare ", y="Imperial Density")
Prod_plot




#############################################
##  Maps of predictor variables            ## 

par(bg = 'lightgrey') #change background of plots

#different colour palettes are selected for different variables - they are then plotted using Long,Lat coordinates

map_col<-brewer.pal(n = 7, name = "YlOrBr")
map_col<-map_col[7:1]  #reverses direction to palette to give more appropriate direction to colour scheme
palette(map_col)
plot(X,Y,col=Steppe_cats,pch=15,  bg="grey")

map_col<-brewer.pal(n = 9, name = "YlGnBu")
map_col<-map_col[9:1]
palette(map_col)
plot(X,Y,col=Agri_cats,pch=15)

map_col<-brewer.pal(n = 6, name = "BuPu")
map_col<-map_col[6:1]
palette(map_col)
plot(X,Y,col=FE_cats,pch=15)

map_col<-brewer.pal(n = 7, name = "Blues")
palette(map_col)
plot(X,Y,col=Elev_cats,pch=15)

map_col<-brewer.pal(n = 9, name = "YlGn")
palette(map_col)
plot(X,Y,col=Prod_cats,pch=15)






###########################################################################################
###            SCATTERPLOT                                                              ###

#scale variables

Total_emp_dens<-scale(datafile$Total_emp_dens)
allYield<-scale(datafile$allYield)
agriBest<-scale(datafile$agriBest*-1)
elevStd<-scale((log10(datafile$elevStd+1))*-1)
maxSteppe_distance<-scale(datafile$maxSteppe_distance*-1)
X<-datafile$X
Y<-datafile$Y


#create variable that relates to predicted values from GLS analyses


#mean parameter value estimates from main analysis
#agriBest			    allYield			elevStd			maxSteppe_distance			agriBest:maxSteppe_distance
#0.2442           0.0050        0.0076      0.4649                  0.1514

predicted_values<-(0.2442*agriBest)+(0.4649*maxSteppe_distance)+(0.0050*allYield)+(0.0076*elevStd)+(0.1514*agriBest*maxSteppe_distance)


#plot these values against imperial denisty 
plot(predicted_values,Total_emp_dens,pch=19,cex=0.5,xlab="Model-predicted Values", ylab="Total Imperial Density", cex.lab=1.3)



#descale predicted values based on relationship between original imperial denisty and scaled imperial density
scale_model<-lm(datafile$Total_emp_dens~Total_emp_dens)
intercept<-scale_model$coefficients[1]
slope<-scale_model$coefficients[2]

predicted_values_descaled<-intercept+(slope*predicted_values)

plot(datafile$Total_emp_dens~predicted_values_descaled,pch=19,cex=0.5,xlab="Model-predicted Values", ylab="Total Imperial Density", cex.lab=1.3)



