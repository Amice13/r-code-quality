##Script to reproduce  analysis of the article:

#Agrestal weeds of Colima: distribution and biogeographic composition 

#Script author: Leopardi-Verde, Carlos L.
#Facultad de Ciencias Biologicas y Agropecuarias, Universidad de Colima, Km. 40 Autopista Colima-Manzanillo, Crucero de Tecoman, Tecoman 28930, Colima, México.
#DATE: 2020-10-01

###If you find an error or have doubts, please send me an email at cleopardi@ucol.mx  

####Recommended/required libraries

#library(raster)
#library(rasterVis)
#library(maptools) ## 0.8-21 install.packages("maptools", repos="http://R-Forge.R-project.org")
library(latticeExtra)
library(colorspace)
#require(rgeos)
#require(rgdal)
require(maptools)
require(sp)
require(maps)
require(mapdata)
#require(dismo)
#require(ape)
#require(geomorph)

proj <- CRS(' +proj=longlat +ellps=WGS84')

#Adjust layout size

cenAme<- map("worldHires",xlim=c(-105,-103.4), ylim=c(18.5,19.6), col="gray90", fill=TRUE)# Esto representa el mapa de México y parte de Centro América.

#The map -- Load shape files. Administrative shapes can downloand elsewhere from public sites

MEX.edo <- readShapePoly(file.choose())
COL <- readShapePoly(file.choose())

#Sample

blackb <- read.csv(file.choose()) #my data for sampling sites, contains a column of "lat" and a column of "lon" with GPS points in decimal degrees

blueb <- read.csv(file.choose())
coffee <- read.csv(file.choose())
maize <- read.csv(file.choose())
mex.lime <- read.csv(file.choose())
on.pepp <- read.csv(file.choose())
papaya <- read.csv(file.choose())
sug.pepp <- read.csv(file.choose())

#Now plot:

plot(MEX.edo, xlim=c(-105,-103.4), ylim=c(18.5,19.6), col="gray95", fill=TRUE)
plot(COL, col="white", add=TRUE)
 
points(papaya$lon, papaya$lat, pch=1, col="black", cex=1)  #plot my sample sites
points(blueb$lon, blueb$lat, pch=2, col="red", cex=1)
points(blackb$lon, blackb$lat, pch=3, col="green3", cex=1)
points(coffee$lon, coffee$lat, pch=4, col="blue", cex=1)
points(on.pepp$lon, on.pepp$lat, pch=5, col="cyan", cex=1)
points(sug.pepp$lon, sug.pepp$lat, pch=6, col="magenta", cex=1)
points(mex.lime$lon, mex.lime$lat, pch=7, col="yellow", cex=1)
points(maize$lon, maize$lat, pch=8, col="gray", cex=1)

#Add scale and axes

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
map.axes()

#####################################################################
#PDF output

pdf(file="mapa malezas x cultivo.pdf")

#plot(MEX.edo, xlim=c(-105,-103.4), ylim=c(18.5,19.6), col="gray95", fill=TRUE)
plot(COL, col="white", lwd = 0.5)
points(papaya$lon, papaya$lat, pch=1, col="black", cex=1)  #plot my sample sites
points(blueb$lon, blueb$lat, pch=2, col="black", cex=1)
points(blackb$lon, blackb$lat, pch=3, col="black", cex=1)
points(coffee$lon, coffee$lat, pch=4, col="black", cex=1)
points(on.pepp$lon, on.pepp$lat, pch=5, col="black", cex=1)
points(sug.pepp$lon, sug.pepp$lat, pch=6, col="black", cex=1)
points(mex.lime$lon, mex.lime$lat, pch=7, col="black", cex=1)
points(maize$lon, maize$lat, pch=8, col="black", cex=1)
#añade escala y ejes
map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
map.axes()

dev.off()

