###################Spatstat script for 250m Buffers###################

#Written by:
  #Dr. Elena Favaro (University of Calgary, Currently at The Open University)
  #With input from Dr. David Chartash (Yale University)

#For 500m, 1km, and 2km buffers, simply edit this script;
  #If you are running this script for other buffers, 
  #Either clear the Global Environment before moving to the next buffer set,
  #Or change the object renamed for as.owin (eg. line 49 in Subset 10)
  #to a leter of the alphabet not yet used.

#Note:
  #This script launches a seperate window where you will choose your shape file.
  #For this script to work, ensure all shape and text files are present 
  #in the folder you set as the working directory

#set working directory
setwd("C:/Users/<whereever you've stored your data>")

#Load libraries
#I make no gaurentees about the viability of these packages
{
  library(spatstat)
  library(sp)
  library(maptools)
  library(rgdal)
  library(readxl) 
}

#Subset Scripts
  #Any line which requires an edit to run another subset is marked by 
  #EDIT


#Subset 10
{
  #Import text file from Text (base)
  YDpts_10_250 <- read.table("YDpts_10_250.txt", header = TRUE) #EDIT
  #View(YDpts_10_250)#EDIT
  
  #Choose your shape file with choose.files(). 
  #It will open up a dialog box
  #Navigate to your working directory and choose the POLYGON .shp file
  YDpts_10SHP <- choose.files()#EDIT
  YD10Poly <- readOGR(YDpts_10SHP)#EDIT
  plot(YD10Poly)#EDIT
  
  #Spatstat code begins here
  as.owin(YD10Poly)#EDIT
  A <- as.owin(YD10Poly)#EDIT "A"
  
  #Read and edit table to include yardang points
  df <- read.table("YDpts_10_250.txt", header = TRUE) #EDIT
  x <- df$x
  y <- df$y
  YD10 <- ppp(x, y, A)#EDIT "YD0" and "A"
  #View(YD10)
  
  #Provide unit name for the distances between points
  unitname(YD10) <- "m" #EDIT
  
  #This is where the magic happens
  YD10_subset <- YD10 #EDIT
  summary(YD10_subset)#EDIT
  plot(YD10_subset)#EDIT
  
}

#Subset 9
{
#Import text file from Text (base)

YDpts_9_250 <- read.table("YDpts_9_250.txt", header = TRUE) #EDIT
#View(YDpts_9_250)#EDIT

#Choose your shape file with choose.files(). 
#It will open up a dialog box
#Navigate to your working directory and choose the POLYGON .shp file
YDpts_9SHP <- choose.files()#EDIT
YD9Poly <- readOGR(YDpts_9SHP)#EDIT

plot(YD9Poly)#EDIT

#Spatstat code begins here
as.owin(YD9Poly)#EDIT
B <- as.owin(YD9Poly)#EDIT "A"

#Read and edit table to include yardang points
df <- read.table("YDpts_9_250.txt", header = TRUE) #EDIT
x <- df$x
y <- df$y
YD9 <- ppp(x, y, B)#EDIT "YD0" and "A"
#View(YD9)

#Provide unit name for the distances between points
unitname(YD9) <- "m" #EDIT

#This is where the magic happens
YD9_subset <- YD9 #EDIT
summary(YD9_subset)#EDIT
plot(YD9_subset)#EDIT

}

#Subset 8
{
  #Import text file from Text (base)
  
  YDpts_8_250 <- read.table("YDpts_8_250.txt", header = TRUE) #EDIT
  #View(YDpts_8_250)#EDIT
  
  #Choose your shape file with choose.files(). 
  #It will open up a dialog box
  #Navigate to your working directory and choose the POLYGON .shp file
  YDpts_8SHP <- choose.files()#EDIT
  YD8Poly <- readOGR(YDpts_8SHP)#EDIT
  
  plot(YD8Poly)#EDIT
  
  #Spatstat code begins here
  as.owin(YD8Poly)#EDIT
  C <- as.owin(YD8Poly)#EDIT "A"
  
  #Read and edit table to include yardang points
  df <- read.table("YDpts_8_250.txt", header = TRUE) #EDIT
  x <- df$x
  y <- df$y
  YD8 <- ppp(x, y, C)#EDIT "YD0" and "A"
  #View(YD8)
  
  #Provide unit name for the distances between points
  unitname(YD8) <- "m" #EDIT
  
  #This is where the magic happens
  YD8_subset <- YD8 #EDIT
  summary(YD8_subset)#EDIT
  plot(YD8_subset)#EDIT
  
}

#Subset 7
{
  #Import text file from Text (base)
  
  YDpts_7_250 <- read.table("YDpts_7_250.txt", header = TRUE) #EDIT
  #View(YDpts_7_250)#EDIT
  
  #Choose your shape file with choose.files(). 
  #It will open up a dialog box
  #Navigate to your working directory and choose the POLYGON .shp file
  YDpts_7SHP <- choose.files()#EDIT
  YD7Poly <- readOGR(YDpts_7SHP)#EDIT
  
  plot(YD7Poly)#EDIT
  
  #Spatstat code begins here
  as.owin(YD7Poly)#EDIT
  D <- as.owin(YD7Poly)#EDIT "A"
  
  #Read and edit table to include yardang points
  df <- read.table("YDpts_7_250.txt", header = TRUE) #EDIT
  x <- df$x
  y <- df$y
  YD7 <- ppp(x, y, D)#EDIT "YD0" and "A"
  #View(YD7)
  
  #Provide unit name for the distances between points
  unitname(YD7) <- "m" #EDIT
  
  #This is where the magic happens
  YD7_subset <- YD7 #EDIT
  summary(YD7_subset)#EDIT
  plot(YD7_subset)#EDIT
  
}

#subset 6
{
#Import text file from Text (base)

YDpts_6_250 <- read.table("YDpts_6_250.txt", header = TRUE) #EDIT
#View(YDpts_6_250)#EDIT

#Choose your shape file with choose.files(). 
#It will open up a dialog box
#Navigate to your working directory and choose the POLYGON .shp file
YDpts_6SHP <- choose.files()#EDIT
YD6Poly <- readOGR(YDpts_6SHP)#EDIT

plot(YD6Poly)#EDIT

#Spatstat code begins here
as.owin(YD6Poly)#EDIT
E <- as.owin(YD6Poly)#EDIT "A"

#Read and edit table to include yardang points
df <- read.table("YDpts_6_250.txt", header = TRUE) #EDIT
x <- df$x
y <- df$y
YD6 <- ppp(x, y, E)#EDIT "YD0" and "A"
#View(YD6)

#Provide unit name for the distances between points
unitname(YD6) <- "m" #EDIT

#This is where the magic happens
YD6_subset <- YD6 #EDIT
summary(YD6_subset)#EDIT
plot(YD6_subset)#EDIT

}

#Subset 5
{
#Import text file from Text (base)

YDpts_5_250 <- read.table("YDpts_5_250.txt", header = TRUE) #EDIT
#View(YDpts_5_250)#EDIT

#Choose your shape file with choose.files(). 
#It will open up a dialog box
#Navigate to your working directory and choose the POLYGON .shp file
YDpts_5SHP <- choose.files()#EDIT
YD5Poly <- readOGR(YDpts_5SHP)#EDIT

plot(YD5Poly)#EDIT

#Spatstat code begins here
as.owin(YD5Poly)#EDIT
F <- as.owin(YD5Poly)#EDIT "A"

#Read and edit table to include yardang points
df <- read.table("YDpts_5_250.txt", header = TRUE) #EDIT
x <- df$x
y <- df$y
YD5 <- ppp(x, y, F)#EDIT "YD0" and "A"
#View(YD5)

#Provide unit name for the distances between points
unitname(YD5) <- "m" #EDIT

#This is where the magic happens
YD5_subset <- YD5 #EDIT
summary(YD5_subset)#EDIT
plot(YD5_subset)#EDIT

}

#subset 4
{
  #Import text file from Text (base)
  
  YDpts_4_250 <- read.table("YDpts_4_250.txt", header = TRUE) #EDIT
  #View(YDpts_4_250)#EDIT
  
  #Choose your shape file with choose.files(). 
  #It will open up a dialog box
  #Navigate to your working directory and choose the POLYGON .shp file
  YDpts_4SHP <- choose.files()#EDIT
  YD4Poly <- readOGR(YDpts_4SHP)#EDIT
  
  plot(YD4Poly)#EDIT
  
  #Spatstat code begins here
  as.owin(YD4Poly)#EDIT
  G <- as.owin(YD4Poly)#EDIT "A"
  
  #Read and edit table to include yardang points
  df <- read.table("YDpts_4_250.txt", header = TRUE) #EDIT
  x <- df$x
  y <- df$y
  YD4 <- ppp(x, y, G)#EDIT "YD0" and "A"
  #View(YD4)
  
  #Provide unit name for the distances between points
  unitname(YD4) <- "m" #EDIT
  
  #This is where the magic happens
  YD4_subset <- YD4 #EDIT
  summary(YD4_subset)#EDIT
  plot(YD4_subset)#EDIT
  
}

#Subset 3
{
  #Import text file from Text (base)
  
  YDpts_3_250 <- read.table("YDpts_3_250.txt", header = TRUE) #EDIT
  #View(YDpts_3_250)#EDIT
  
  #Choose your shape file with choose.files(). 
  #It will open up a dialog box
  #Navigate to your working directory and choose the POLYGON .shp file
  YDpts_3SHP <- choose.files()#EDIT
  YD3Poly <- readOGR(YDpts_3SHP)#EDIT
  
  plot(YD3Poly)#EDIT
  
  #Spatstat code begins here
  as.owin(YD3Poly)#EDIT
  H <- as.owin(YD3Poly)#EDIT "A"
  
  #Read and edit table to include yardang points
  df <- read.table("YDpts_3_250.txt", header = TRUE) #EDIT
  x <- df$x
  y <- df$y
  YD3 <- ppp(x, y, H)#EDIT "YD0" and "A"
  #View(YD3)
  
  #Provide unit name for the distances between points
  unitname(YD3) <- "m" #EDIT
  
  #This is where the magic happens
  YD3_subset <- YD3 #EDIT
  summary(YD3_subset)#EDIT
  plot(YD3_subset)#EDIT
  
}

#Subset 2
{
#Import text file from Text (base)

YDpts_2_250 <- read.table("YDpts_2_250.txt", header = TRUE) #EDIT
#View(YDpts_2_250)#EDIT

#Choose your shape file with choose.files(). 
#It will open up a dialog box
#Navigate to your working directory and choose the POLYGON .shp file
YDpts_2SHP <- choose.files()#EDIT
YD2Poly <- readOGR(YDpts_2SHP)#EDIT

plot(YD2Poly)#EDIT

#Spatstat code begins here
as.owin(YD2Poly)#EDIT
I <- as.owin(YD2Poly)#EDIT "A"

#Read and edit table to include yardang points
df <- read.table("YDpts_2_250.txt", header = TRUE) #EDIT
x <- df$x
y <- df$y
YD2 <- ppp(x, y, I)#EDIT "YD0" and "A"
#View(YD2)

#Provide unit name for the distances between points
unitname(YD2) <- "m" #EDIT

#This is where the magic happens
YD2_subset <- YD2 #EDIT
summary(YD2_subset)#EDIT
plot(YD2_subset)#EDIT

}

#Subset 1
{
  #Import text file from Text (base)
  
  YDpts_1_250 <- read.table("YDpts_1_250.txt", header = TRUE) #EDIT
  #View(YDpts_1_250)#EDIT
  
  #Choose your shape file with choose.files(). 
  #It will open up a dialog box
  #Navigate to your working directory and choose the POLYGON .shp file
  YDpts_1SHP <- choose.files()#EDIT
  YD1Poly <- readOGR(YDpts_1SHP)#EDIT
  
  plot(YD1Poly)#EDIT
  
  #Spatstat code begins here
  as.owin(YD1Poly)#EDIT
  J <- as.owin(YD1Poly)#EDIT "A"
  
  #Read and edit table to include yardang points
  df <- read.table("YDpts_1_250.txt", header = TRUE) #EDIT
  x <- df$x
  y <- df$y
  YD1 <- ppp(x, y, J)#EDIT "YD0" and "A"
  #View(YD1)
  
  #Provide unit name for the distances between points
  unitname(YD1) <- "m" #EDIT
  
  #This is where the magic happens
  YD1_subset <- YD1 #EDIT
  summary(YD1_subset)#EDIT
  plot(YD1_subset)#EDIT
  
}

