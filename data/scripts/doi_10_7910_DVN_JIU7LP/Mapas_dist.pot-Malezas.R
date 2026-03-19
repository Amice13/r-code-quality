##Script to reproduce  analysis of the article:

#Agrestal weeds of Colima: distribution and biogeographic composition 

#Script author: Leopardi-Verde, Carlos L.
#Facultad de Ciencias Biologicas y Agropecuarias, Universidad de Colima, Km. 40 Autopista Colima-Manzanillo, Crucero de Tecoman, Tecoman 28930, Colima, México.
#DATE: 2020-10-01

###If you find an error or have doubts, please send me an email at cleopardi@ucol.mx  

####Recommended/required libraries

library(raster)
library(rasterVis)
library(maptools) ## 0.8-21 install.packages("maptools", repos="http://R-Forge.R-project.org")
library(latticeExtra)
library(colorspace)
library(rgeos)
library(rgdal)
library(sp)
library(maps)
library(mapdata)
library(dismo)
library(gdalUtils)

############This function or parts can be neccesary to the script#####

select07 <- function(X, y, family="binomial",univar="gam", threshold=0.7, method="pearson", sequence=NULL, ...){
    # selects variables based on removing correlations > 0.7, retaining those
    # variables more important with respect to y
    # when a sequence is given, this will be used instead (Damaris)

    # 1. step: cor-matrix
    # 2. step: importance vector
    # 3. step: identify correlated pairs
    # 4. step: remove less important from pairs
    #get rid of it II: seqreg, select07,maxspan
    # written by Carsten F. Dormann;
    # last changed by  Tamara Münkemüller and Damaris Zurell, 12.12.2007
    # last changed by Damaris Zurell, 19.12.2008
    # last changed by Sven Lautenbach, 04.12.2009 -> switch for negativ.binomial vs negbin
    require(mgcv)

    var.imp <- function (variable, response, univar=univar, family="gaussian"){
        # calculates the univariate (=marginal) importance of a variable for a response
        if (!univar %in% c("glm", "gam")) stop("Invalid univariate screening method: choose 'glm' or 'gam' (default).")
        
        if (univar=="glm"){
          fm.glm <- glm(response ~ variable, family=family)
          summary(fm.glm)$aic
        } else {
          # consider special case of negative binomial function
			    # gam comes with a special implementation of negative.binomial
			    # called negbin
			    if( class(family) == "family")
    			{
				    if ((substring(family$family, 1, 3) == "Neg" & univar=="gam"))
						{
							# extract theta parameter
							tmp1 <- strsplit(family$family, "(", fixed=TRUE)[[1]][2]
							theta <- strsplit(tmp1, ")", fixed=TRUE)[[1]][1]
							family=paste("negbin(", theta, ")", sep="")
						}
					}
          fm.gam <- gam(response ~ s(variable, ...), family=family)
          AIC(fm.gam)
        }
    } 
	
    cm <- cor(X, method=method)
    pairs <- which(abs(cm)>= threshold, arr.ind=T) # identifies correlated variable pairs
    index <- which(pairs[,1]==pairs[,2])           # removes entry on diagonal
    pairs <- pairs[-index,]                        # -"-
    exclude <- NULL
    if (NROW(pairs)!=0)
    {
        if (is.null(sequence)) {
          #importance as AIC: the lower the better!
          imp <- apply(X, 2, var.imp, response=y, family=family, univar=univar)           
          for (i in 1:NROW(pairs))
          {
              a <- imp[rownames(cm)[pairs[i,1]]]
              b <- imp[rownames(cm)[pairs[i,2]]]
              exclude <- c(exclude, ifelse(a>b, names(a), names(b)))
          }
        } else { 
          for (i in 1:NROW(pairs))
          {
              a <- which(pairs[i,1]==sequence)
              b <- which(pairs[i,2]==sequence)
              exclude <- c(exclude, ifelse(a>b, rownames(cm)[pairs[i,1]], rownames(cm)[pairs[i,2]]))
          }
        }
    }
    X <- X[,!(colnames(X) %in% unique(exclude)),drop=F]
    return(X)
}
#select07(X=LE, y=LanExc12[,"c"], family="binomial", threshold=0.7, method="spearman")[1:10,]


##### First, we check our collecting sites. 

proj <- CRS(' +proj=longlat +ellps=WGS84')

#This is required to adjust the layout

cenAme<- map("worldHires",xlim=c(-105,-103.4), ylim=c(18.5,19.6), col="gray90", fill=TRUE)# Here it is represented Colima. It is useful to adjust the coordinates to crop.

#The map --  

MEX.edo <- readOGR(file.choose()) #Mexido administrative polygons at level 1 (states). This shapefile can be downloaded from any free server elsewhere.
COL <- readOGR(file.choose()) #Colima administrative - municipalities. This shapefile can be downloaded from any free server elsewhere.

#Sampled agricultural field to analyze

plant<-read.csv(file.choose())    #Load files with samples. There are two files, one with samples of group 1, "hot_cult.csv". This file contains sampled localities of papaya, mexican lime and onion fields. The second file, "cold_cult.csv", cotains sampled localities blueberry, blackberry, and sugarcane. Please load one and run the scrpit. After end, clean R workspace and load the second file and run again the script.

#To check localities, please plot:

plot(MEX.edo, xlim=c(-105,-103.4), ylim=c(18.5,19.6), col="gray95", fill=TRUE)
plot(COL, col="gray85", add=TRUE)
 
points(plant$lon, plant$lat, pch=20, col="red", cex=1)  #plot my sample sites

#Add scales and axes
map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
map.axes()
###############################################

#Bioclimatic variables from wordlclim:

#Way 1: Load variables directly from worldclim web site:
#biov <- getData("worldclim",var="bio",res=0.5, lon=-104, lat=19)

#Way 2: Download variables from worldclim website and load in R manually:

bio1<-raster(file.choose()) 
bio2<-raster(file.choose()) 
bio3<-raster(file.choose()) 
bio4<-raster(file.choose()) 
bio5<-raster(file.choose()) 
bio6<-raster(file.choose()) 
bio7<-raster(file.choose()) 
bio8<-raster(file.choose()) 
bio9<-raster(file.choose()) 
bio10<-raster(file.choose()) 
bio11<-raster(file.choose()) 
bio12<-raster(file.choose()) 
bio13<-raster(file.choose()) 
bio14<-raster(file.choose()) 
bio15<-raster(file.choose()) 
bio16<-raster(file.choose()) 
bio17<-raster(file.choose()) 
bio18<-raster(file.choose()) 
bio19<-raster(file.choose()) 


extn<-extent(-105,-103.4,18.5,19.6) #Define the extent of the layer (in gps coordinates) 

# Crop layers

bio1.cr<-crop(bio1, extn)
bio2.cr<-crop(bio2, extn) 
bio3.cr<-crop(bio3, extn) 
bio4.cr<-crop(bio4, extn) 
bio5.cr<-crop(bio5, extn) 
bio6.cr<-crop(bio6, extn) 
bio7.cr<-crop(bio7, extn) 
bio8.cr<-crop(bio8, extn)
bio9.cr<-crop(bio9, extn) 
bio10.cr<-crop(bio10, extn) 
bio11.cr<-crop(bio11, extn) 
bio12.cr<-crop(bio12, extn) 
bio13.cr<-crop(bio13, extn) 
bio14.cr<-crop(bio14, extn) 
bio15.cr<-crop(bio15, extn) 
bio16.cr<-crop(bio16, extn)
bio17.cr<-crop(bio17, extn) 
bio18.cr<-crop(bio18, extn) 
bio19.cr<-crop(bio19, extn) 

#Put together the cropped layers:

vars<-stack(bio1.cr,bio2.cr, bio3.cr, bio4.cr, bio5.cr, bio6.cr, bio7.cr, bio8.cr,bio9.cr, bio10.cr, bio11.cr, bio12.cr, bio13.cr, bio14.cr, bio15.cr, bio16.cr,bio17.cr, bio18.cr, bio19.cr)

plot(vars) ##Check the crops


bio.sp<-extract(vars, plant) #Extract data from cropped layers


#First we need to generate a matrix of presence/absence and add it to the climatic variables. This is achieved with this lines:

presvals <- extract(vars, plant)
set.seed(0)
backgr <- randomPoints(vars, n=1000, extf = 1.25)
absvals <- extract(vars, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
bio.en.data <- data.frame(cbind(pb, rbind(presvals, absvals)))
head(bio.en.data)


#####To test models#####

group <- kfold(plant, 2)
pres_train <- plant[group != 1, ]
pres_test <- plant[group == 1, ]


colnames(backgr) = c('lon', 'lat')
group <- kfold(backgr, 2)
backg_train <- backgr[group != 1, ]
backg_test <- backgr[group == 1, ]

#####To generate the Maxent general model  ####### 


general.mod <- maxent(vars, pres_train)
plot(general.mod) #Here we can identify the potentially most important bioclimatic data

# From here is complementary, if you want to review the model. But for the interest of our paper in unnecesary

response(general.mod)

e <- evaluate(pres_test, backg_test, general.mod, vars)
e ##Muestra la evaluación (ajuste) del modelo, ¿Qué tan bueno es?

px <- predict(vars, general.mod, progress='') 

plot(px, main='Maxent, raw values') 


##Some relevevant bioclimatic variables suggested by Maxent: 

#Bio4 - Temperature seasonality 

bio4.COL<-mask(bio4.cr, COL)

plot(bio4.COL)
plot(COL, lwd = 0.3, add=TRUE)
points(papaya$lon, papaya$lat, pch=1, col="black", cex=1)  #plot my sample sites
points(blueb$lon, blueb$lat, pch=2, col="black", cex=1)
points(blackb$lon, blackb$lat, pch=3, col="black", cex=1)
points(coffee$lon, coffee$lat, pch=4, col="black", cex=1)
points(on.pepp$lon, on.pepp$lat, pch=5, col="black", cex=1)
points(sug.pepp$lon, sug.pepp$lat, pch=6, col="black", cex=1)
points(mex.lime$lon, mex.lime$lat, pch=7, col="black", cex=1)
points(maize$lon, maize$lat, pch=8, col="black", cex=1)

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
#map.axes()

#Bio 8


bio8.COL<-mask(bio8.cr, COL)

plot(bio8.COL)
plot(COL, lwd = 0.3, add=TRUE)
points(papaya$lon, papaya$lat, pch=1, col="black", cex=1)  #plot my sample sites
points(blueb$lon, blueb$lat, pch=2, col="black", cex=1)
points(blackb$lon, blackb$lat, pch=3, col="black", cex=1)
points(coffee$lon, coffee$lat, pch=4, col="black", cex=1)
points(on.pepp$lon, on.pepp$lat, pch=5, col="black", cex=1)
points(sug.pepp$lon, sug.pepp$lat, pch=6, col="black", cex=1)
points(mex.lime$lon, mex.lime$lat, pch=7, col="black", cex=1)
points(maize$lon, maize$lat, pch=8, col="black", cex=1)

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)



pdf(file="bio4_cult.pdf")

plot(bio4.COL)
plot(COL, lwd = 0.3, add=TRUE)
points(papaya$lon, papaya$lat, pch=1, col="black", cex=1)  #plot my sample sites
points(blueb$lon, blueb$lat, pch=2, col="black", cex=1)
points(blackb$lon, blackb$lat, pch=3, col="black", cex=1)
points(coffee$lon, coffee$lat, pch=4, col="black", cex=1)
points(on.pepp$lon, on.pepp$lat, pch=5, col="black", cex=1)
points(sug.pepp$lon, sug.pepp$lat, pch=6, col="black", cex=1)
points(mex.lime$lon, mex.lime$lat, pch=7, col="black", cex=1)
points(maize$lon, maize$lat, pch=8, col="black", cex=1)

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
#map.axes()

dev.off()


#Bio7 - Annual range of temperature 

bio7.COL<-mask(bio7.cr, COL)

plot(bio7.COL)
plot(COL, lwd = 0.3, add=TRUE)
points(papaya$lon, papaya$lat, pch=1, col="black", cex=1)  #plot my sample sites
points(blueb$lon, blueb$lat, pch=2, col="black", cex=1)
points(blackb$lon, blackb$lat, pch=3, col="black", cex=1)
points(coffee$lon, coffee$lat, pch=4, col="black", cex=1)
points(on.pepp$lon, on.pepp$lat, pch=5, col="black", cex=1)
points(sug.pepp$lon, sug.pepp$lat, pch=6, col="black", cex=1)
points(mex.lime$lon, mex.lime$lat, pch=7, col="black", cex=1)
points(maize$lon, maize$lat, pch=8, col="black", cex=1)

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
#map.axes()

#Bio17

bio17.COL<-mask(bio17.cr, COL)

plot(bio17.COL)
plot(COL, lwd = 0.3, add=TRUE)
points(papaya$lon, papaya$lat, pch=1, col="black", cex=1)  #plot my sample sites
points(blueb$lon, blueb$lat, pch=2, col="black", cex=1)
points(blackb$lon, blackb$lat, pch=3, col="black", cex=1)
points(coffee$lon, coffee$lat, pch=4, col="black", cex=1)
points(on.pepp$lon, on.pepp$lat, pch=5, col="black", cex=1)
points(sug.pepp$lon, sug.pepp$lat, pch=6, col="black", cex=1)
points(mex.lime$lon, mex.lime$lat, pch=7, col="black", cex=1)
points(maize$lon, maize$lat, pch=8, col="black", cex=1)

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)


pdf(file="bio7_cult.pdf")

plot(bio7.COL)
plot(COL, lwd = 0.3, add=TRUE)
points(papaya$lon, papaya$lat, pch=1, col="black", cex=1)  #plot my sample sites
points(blueb$lon, blueb$lat, pch=2, col="black", cex=1)
points(blackb$lon, blackb$lat, pch=3, col="black", cex=1)
points(coffee$lon, coffee$lat, pch=4, col="black", cex=1)
points(on.pepp$lon, on.pepp$lat, pch=5, col="black", cex=1)
points(sug.pepp$lon, sug.pepp$lat, pch=6, col="black", cex=1)
points(mex.lime$lon, mex.lime$lat, pch=7, col="black", cex=1)
points(maize$lon, maize$lat, pch=8, col="black", cex=1)

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)

dev.off()
