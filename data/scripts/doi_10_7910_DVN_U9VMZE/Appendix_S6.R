## R code for analyses
rm(list=ls())
setwd("C:/Users/Jason/Dropbox/Hutchinson Project (Jason)/Draft/Flower_analyses/2023/Submission_Nat_Clim_Change")
getwd()

## Packages:
#***********
library(vegan) ; library(adespatial) ; library(ggplot2) ; library(forecast)   
library(spacodiR) ; library(ade4) ; library(FactoMineR) ; library("factoextra")
library(usdm) ; library(relaimpo) ; library(spacodiR) ; library(stringr)
library(phytools) ; library(brranching) ; library(spdep) ; library(phyloseq)
library(labdsv) ; library(easynls) ; library(rethinking) 
library(readr) ; library(tidyverse) ; library(brms)  ; library(tidybayes) 
library(bayesplot) ; library(ggridges) ; library(gridExtra)
library(astsa) ; library(V.PhyloMaker) ; library(brranching) ; library(spacodiR)
library("plotrix") ; library(varhandle)

## Function to calculate the day at which the phenological year starts for each species:
#---------------------------------------------------------------------------------------
# The function BeginYr calculates day of year that phenological years begin.
# This day of year minimizes the variance of linearized dates (0 to 364) of 
#   phenological events over years with day 0 taking the 365 possible values.
# Beginyr requires the package 'circular'.
# BeginYr inputs one dataframe, one vector & prs.abs (T or F).
#   The dataframe must hold sp, part, fecha & quantity.
#       sp - mnemonics for species
#       part - integer representing structure (1 fruit, 2 seed, etc)
#       fecha - census data formatted YYYYMMDD
#       quantity - integer count; number of items
#   The vector must hold the parts to be used.
#   If prs.abs==F (T), quantity is used (ignored).
# BeginYr returns one dataframe, which holds species, the day its 
#   phenological year begins, & sample size.
#   The format for day is 0 for 1 Jan & 364 for 31 Dec.
# J head(datafile)
library(circular)
BeginYr <- function(datafile, parts, prs.abs) {
  a = datafile[datafile$part %in% parts,]
  x = strptime(as.character(a$fecha), format="%Y%m%d")
  a$day = x$yday
  if (prs.abs==F) g = aggregate(a$quantity,list(a$sp,a$day),sum)
  if (prs.abs==T) g = data.frame(table(a$sp,a$day))
  names(g) = c("sp","dayofyr","quantity")
  g$sp = as.character(g$sp)
  g$dayofyr = as.numeric(as.character(g$dayofyr))
  # Calculate variance of LINEAR dayofyr inside a loop over the 365 days of year.
  # Use machine formula to weight variance by quantity.
  # J head(g) ; dim(g)
  # J table(a$day)
  # J head(a) ; dim(a)
  sp  = g$sp      # vectorize
  day = g$dayofyr # vectorize
  q = g$quantity  # vectorize
  n = tapply(q, sp, sum) # sample size
  # calculate variance starting year on each possible day of year for each sp.
  for (i in 0:365) {
    x  = ifelse(day>i, day-i, 365+day-i)
    x2 = x^2 * q
    x  = x * q
    sumx = tapply(x, sp, sum)
    sumx2 = tapply(x2, sp, sum)
    results = as.data.frame.table(sumx2 - sumx^2/n) 
    results$beginyr = i
    if (i==0) allresults=results else allresults=rbind(results,allresults)
  }
  names(allresults) = c("sp","variance","beginyr")
  allresults$sp = as.character(allresults$sp)
  allresults = allresults[!allresults$variance==0,] # remove sp with 0 variance
  # Select days of year corresponding to minimum variance for each species.
  minvar = aggregate(allresults$variance, list(allresults$sp), min)
  names(minvar) = c("sp", "minvar")
  j = merge(allresults, minvar, by="sp", all.y=TRUE)
  j = j[j$variance==j$minvar, c(1,3)]
  # Select median day of year corresponding to minimum variance for each species.
  # The loops catch medians that are not exact days.
  j$radians = circular(2*pi*j$beginyr/366, units="radians", modulo="2pi")
  k = aggregate(j$radians, by=list(j$sp), median.circular)
  l = merge(j, k, by.x="sp", by.y="Group.1")
  l = l[order(l$sp, l$radians),]
  species = data.frame(table(l$sp))
  names(species) = c("sp", "dates")
  species$sp = as.character(species$sp)
  counter = 0
  l$mark = FALSE
  for (i in 1:dim(species)[1]) {
    flag = 0
    for (j in 1:species$dates[i]) {
      counter = counter + 1
      if (flag==0 & l$radians[counter]>=l$x[counter]) first = counter
      if (flag==0 & l$radians[counter]>=l$x[counter]) flag==1
    }
    l$mark[first] = TRUE
  }
  l = l[l$mark, c(1,2)]
  return(l)
}



#****************************************************************************
###   Data importation and calculate phenological years for each species  ###
#****************************************************************************
T2 <- as.data.frame(read.table("Appendix_S2.txt"))
head(T2);dim(T2)

## Calculate the starting date of the flowering phenological year of each each species:
#--------------------------------------------------------------------------------------
## => Go directly to point (3) if (1) and (2) already done

## (1). Perform the BeginYr function:
# - - - - - - - - - - - - - - - - - -
dim(T2)
fecha0 <- T2[,which(colnames(T2)%in%c("YR","MON","DY","CODIGO","PART","QUANTITY"))] ; head(fecha0) ; dim(fecha0)
fecha0 <- fecha0[fecha0$QUANTITY>0,] ; head(fecha0) ; dim(fecha0)
table(fecha0$QUANTITY)
table(ceiling(log(fecha0$QUANTITY+1)))
fecha0$QUANTITY <- ceiling(log(fecha0$QUANTITY+1))
table(fecha0$QUANTITY)
fecha  <- c()
i=1
for(i in 1:nrow(fecha0)){
  if(fecha0[i,5]<10  & fecha0[i,6]<10) { fecha <- c(fecha,paste0(fecha0[i,4],0,fecha0[i,5],0,fecha0[i,6])) }
  if(fecha0[i,5]<10  & fecha0[i,6]>=10){ fecha <- c(fecha,paste0(fecha0[i,4],0,fecha0[i,5],  fecha0[i,6])) }
  if(fecha0[i,5]>=10 & fecha0[i,6]<10) { fecha <- c(fecha,paste0(fecha0[i,4],  fecha0[i,5],0,fecha0[i,6])) }
  if(fecha0[i,5]>=10 & fecha0[i,6]>=10){ fecha <- c(fecha,paste0(fecha0[i,4],  fecha0[i,5],  fecha0[i,6])) }
}
fecha

head(fecha0) ; dim(fecha0)
Z <- as.data.frame(fecha0[,c(1:3)]) ; head(Z) ; dim(Z)
Z <- cbind(Z,as.numeric(fecha)) ; head(Z) ; dim(Z)
colnames(Z) <- c("sp", "part", "quantity","fecha") ; head(Z) ; dim(Z)
Z$fecha <- as.integer(fecha) ; head(Z) ; dim(Z)
Z2 <- Z[Z$part%in%c(6,9),] ; head(Z2) ; dim(Z2) ## select parts= c(6,9) => flower fragments
parts <- as.vector(Z2$part) ; parts
head(Z2) ; dim(Z2)
table(Z2$quantity)
table(Z2$fecha)
str(Z2)

beg <- BeginYr(datafile=Z2,parts=parts,prs.abs=FALSE)
beg$beginyr[which(beg$beginyr==365)] <- 1

for(i in 1:nrow(beg)){
  beg$AB[i] <- nrow(Z2[Z2$sp%in%beg$sp[i],])
}
head(beg);dim(beg)
BE <- beg
BE$MON <- ceiling(BE$beginyr/31)    ## starting phenological month
table(BE$MON) ; plot(table(BE$MON))
head(BE);dim(BE)

## (2). Save data:
# - - - - - - -
#write.table(beg,"STARTING_DAY_OF_PHENOLOGICAL_YEAR_FOR_EACH_SPECIES.txt")

## (3). Re-import data:
# - - - - - - - - - -
BE <- as.data.frame(read.table("STARTING_DAY_OF_PHENOLOGICAL_YEAR_FOR_EACH_SPECIES.txt"))
head(BE);dim(BE)
head(BE)
BE$MON <- ceiling(BE$beginyr/31)    ## starting phenological month
table(BE$MON) ; plot(table(BE$MON))
head(BE);dim(BE)


#***************************************************************
###   Data importation: climate and flower production data   ###
#***************************************************************
YM3 <- as.data.frame(read.table("Appendix_S3.txt",h=T)) ## Climate data
TS4 <- as.data.frame(read.table("Appendix_S4.txt",h=T)) ## Flower production data

head(YM3);dim(YM3)
head(TS4);dim(TS4)

corresp2 <- as.data.frame(read.table("Appendix_S5.txt",head=TRUE)) ## Name correspondence between species acronyms and species names in Appendices S2 and S4
head(corresp2);dim(corresp2)

## Normalize-standardize YM3 but keep YM33 un-transformed:
#---------------------------------------------------------
YM33 <- YM3
head(YM3);dim(YM3)
colnames(YM3)
sels <- c(4:19) ; colnames(YM3)[sels]
head(YM3[,sels])

## Normalize variables:
for(i in sels){
  lambda1 <- BoxCox.lambda(YM3[,i]+abs(min(YM3[,i]))+0.0000000001,method = "loglik", lower = -3,  upper = 3) 
  YM3[,i] <- BoxCox(YM3[,i], lambda=lambda1)
}
par(mfrow=c(4,4),mex=0.3)
for(j in sels){hist(YM3[,j],col="blue",main = colnames(YM3)[j])}
par(mfrow=c(1,1))

## Standardize variables:
for(j in sels){ YM3[,j] <- decostand(YM3[,j],"standardize") }
par(mfrow=c(4,4),mex=0.3)
for(j in sels){hist(YM3[,j],col="blue",main = colnames(YM3)[j])}
par(mfrow=c(1,1))
head(YM3);dim(YM3)

## Calculate MEMs to produce null MSR climate data preserving the original temporal structures:
#-----------------------------------------------------------------------------------------------
C1 <- cbind(YM3$YRMON,1) ; C1 ; dim(C1)

## E0 = untransformed climate data:
E0 <- cbind(YM33$IRRA,YM33$RAIN,YM33$TMIN,YM33$TAVE,YM33$TMAX,YM33$HMIN,YM33$HAVE,YM33$HMAX)
colnames(E0) <- c("IRRA","RAIN","TMIN","TAVE","TMAX","HMIN","HAVE","HMAX")
head(E0);dim(E0)

## E1 = transformed climate data:
E1           <- cbind(YM3$IRRA,YM3$RAIN,YM3$TMIN,YM3$TAVE,YM3$TMAX,YM3$HMIN,YM3$HAVE,YM3$HMAX)
colnames(E1) <- c("IRRA","RAIN","TMIN","TAVE","TMAX","HMIN","HAVE","HMAX")
pca9 <- rda(E1)
#E1 <- cbind(E1,pca9$CA$u[,1:2]) 
#colnames(E1)[9:10] <- c("PCA1","PCA2")
head(E1);dim(E1)
E1 <- as.data.frame(E1)

candidates <- listw.candidates(coord=C1,style="B",nb=c("gab","mst"),weights=c("flin","up"),y_fup = 0.5)
spatial.effect <- 0  ## restera = 0 si pas d'effet global
#MEM <- listw.select(E1, candidates, MEM.autocor = "positive",
MEM <- listw.select(E0, candidates, MEM.autocor = "positive",
                    method = "FWD", MEM.all = TRUE, nperm = 999,
                    nperm.global = 999, alpha = 0.01, p.adjust = TRUE)      
DN <- 0
if(is.list(MEM)){
  if(is.list(MEM$best)){
    MEM.find           <- 1
    MEM.select         <- as.matrix(MEM$best$MEM.select)
    spatial.effect     <- 1
    DN                 <- candidates[[MEM$best.id]]
    if(i==1){DN2 <- DN}
  }
}
w1 <- which(MEM$best$summary[,6]<=0.001)
MEMs <- MEM.select[,w1]
head(MEMs);dim(MEMs)

## MSR randomization:
nMSR=4999
MSR <- msr(E0, DN, nrepet = nMSR, method = "pair")
MSR[[1]]
E0 <- as.data.frame(E0) ; head(E0) ; dim(E0)
E1 <- as.data.frame(E1) ; head(E1) ; dim(E1)

par(mfrow=c(2,1),mex=0.75)
plot(E0$IRRA~YM3$YRMON,type="l",xlab="year",ylab="Observed solar irradiance")
plot(MSR[[2]][,1]~YM3$YRMON,type="l",col="red",xlab="year",ylab="Randomized solar irradiance")
par(mfrow=c(1,1))

List.Null <- MSR

#**************************************************************************************
###  Matrix MAT with Nr of traps per species per census and total flower production ###
#**************************************************************************************

## Generate matrix MAT containing all species-level info:
#--------------------------------------------------------
MAT <- as.data.frame(matrix(0,ncol=1,nrow=ncol(TS4)+6)) 
colnames(MAT) <- c("A") 
rownames(MAT) <- c(colnames(TS4),"int1","int2","int3","int4","int5","int6")
head(MAT);dim(MAT)

dim(TS4)
par(mfrow=c(1,1))
j1=1 ; colnames(TS4)[j1] ; TS4[,j1]

plot(0:10,col="white")

## Matrix E2 of the climate variables (untransformed):
#-----------------------------------------------------
E2      <- as.data.frame(E0)
E2$YR   <- YM3$YR
E2$MON  <- YM3$MON
E2$YMON <- YM3$YRMON
head(E2);dim(E2) 

## alpha thresholds for tests:
#-----------------------------
alph1 <- 0.05 ; alph2 <- 0.95

#-------------
## Start Loop:
#-------------

## Matrix list to store the null effects for each climate variable:
effect.null.irra <- as.data.frame(matrix(0,ncol=nMSR,nrow=nrow(MAT)))
effect.null.rain <- effect.null.irra ; effect.null.tmin <- effect.null.irra
effect.null.tave <- effect.null.irra ; effect.null.tmax <- effect.null.irra
effect.null.hmin <- effect.null.irra ; effect.null.have <- effect.null.irra 
effect.null.hmax <- effect.null.irra

effect.null.tmin.part      <- effect.null.irra
effect.null.hmax.part      <- effect.null.irra
effect.null.tmin.hmax.part <- effect.null.irra
effect.null.tmin.hmax.only <- effect.null.irra

## Calculate community-level flower production:
#----------------------------------------------
#TS5 <- decostand(TS4,"standardize")
TS5 <- TS4 ; dim(TS5)
for(i in 1:ncol(TS5)){TS5[,i]<-log(TS5[,i]+1)}
for(i in 1:ncol(TS5)){TS5[,i]<-TS5[,i]/sum(TS5[,i])}
head(TS5);dim(TS5)

colSums(TS5);colMeans(TS5)
Y.community <- as.numeric(rowSums(TS5)) ; Y.community
#plot(Y.community,type="l")

TS6 <- cbind(TS4,Y.community)
colnames(TS6)[ncol(TS6)] <- "COMMUNITY"

dim(MAT)
head(TS6);dim(TS6)


## Harmonize species names in TS6 and BE (phenological year info for each species) matrices
BE$sp
head(BE);dim(BE)
BE5 <- BE[BE$sp%in%colnames(TS6),] ; head(BE5) ; dim(BE5)


## Calculate the phenological year at the community level (Fcom):
dim(TS6)
Z6          <- Z2[1:218,] ; head(Z6) ; dim(Z6)
Z6$quantity <- (TS6[,187]) 
head(Z6) ; dim(Z6)

head(E2) ; dim(E2)
fecha06 <- E2[,c(9,10:11)] ; head(fecha06) ; dim(fecha06)
fecha06$YMON <- rep(15,218)
head(fecha06) ; dim(fecha06)

fecha6  <- c()
i=1
for(i in 1:nrow(fecha06)){
  if(fecha06[i,2]<10  & fecha06[i,3]<10) { fecha6 <- c(fecha6,paste0(fecha06[i,1],0,fecha06[i,2],0,fecha06[i,3])) }
  if(fecha06[i,2]<10  & fecha06[i,3]>=10){ fecha6 <- c(fecha6,paste0(fecha06[i,1],0,fecha06[i,2],  fecha06[i,3])) }
  if(fecha06[i,2]>=10 & fecha06[i,3]<10) { fecha6 <- c(fecha6,paste0(fecha06[i,1],  fecha06[i,2],0,fecha06[i,3])) }
  if(fecha06[i,2]>=10 & fecha06[i,3]>=10){ fecha6 <- c(fecha6,paste0(fecha06[i,1],  fecha06[i,2],  fecha06[i,3])) }
}
fecha6

Z6$sp    <- "COMMUNITY"
Z6$fecha <- fecha6
head(Z6) ; dim(Z6)

# calculate phenological year
beg <- BeginYr(datafile=Z6,parts=parts,prs.abs=FALSE)
beg
ceiling(beg$beginyr/31)


#***********************************************************************************
###  Inter-annual trends in flower production and climate variables (Figure 2)  ###
#***********************************************************************************

par(mfrow=c(3,3),mex=0.7)
head(YM3);dim(YM3)

Y.community <- TS6[,ncol(TS6)]
my.vector3 <- ts(1:218, frequency = 12, start = c(2000, 1)) 
my.vector3[1:218] <- Y.community
my.vector3

stl1 <- stl(my.vector3, "per") # plot(stl1)
season <- as.numeric(stl1$time.series[,1]) # plot(season,type="l")
intera <- as.numeric(stl1$time.series[,2]) # plot(intera,type="l")
plot  (             Y.community~YM3$YRMON,type="l",col="grey80",main="ACFP",xlab="",ylab="ACFP")
points(   (intera)~YM3$YRMON,type="l",col="black",ylab="",xlab="",ylim=c(3.5,7.5))   
abline(lm((intera)~YM3$YRMON),lty=2,col="black")

plot  (YM3$IRRA~YM3$YRMON,type="l",main="Irradiance",col="grey80",ylab="Irradiance (W/m?)",xlab="year")
points(YM3$Irra.intera~YM3$YRMON,type="l",ylab="",xlab="",col="black") 
abline(lm(YM3$Irra.intera~YM3$YRMON),lty=2,col="grey10");cor.test(YM3$Irra.intera,YM3$YRMON)

plot  (YM3$RAIN~YM3$YRMON,type="l",main="Rainfall",col="grey80",ylab="Rainfall (mm)",xlab="year")
points(YM3$Rain.intera~YM3$YRMON,type="l",ylab="",xlab="",col="black")   
abline(lm(YM3$Rain.intera~YM3$YRMON),lty=2,col="grey10")  ;cor.test(YM3$Rain.intera,YM3$YRMON)

plot  (YM3$TMIN~YM3$YRMON,type="l",main="Min. Temperature",col="grey80",ylab="Min. Temperature (C)",xlab="year")
points(YM3$Tmin.intera~YM3$YRMON,type="l",ylab="",xlab="",col="black")  
abline(lm(YM3$Tmin.intera~YM3$YRMON),lty=2,col="grey10") ;cor.test(YM3$Tmin.intera,YM3$YRMON)

plot  (YM3$TAVE~YM3$YRMON,type="l",main="Average Temperature",col="grey80",ylab="Ave. Temperature (C)",xlab="year")
points(YM3$Tave.intera~YM3$YRMON,type="l",ylab="",xlab="",col="black")                      
abline(lm(YM3$Tave.intera~YM3$YRMON),lty=2,col="grey10")  ;cor.test(YM3$Tave.intera,YM3$YRMON)

plot  (YM3$TMAX~YM3$YRMON,type="l",main="Max. Temperature",col="grey80",ylab="Max. Temperature (C)",xlab="year")
points(YM3$Tmax.intera~YM3$YRMON,type="l",ylab="",xlab="",col="black")                        
abline(lm(YM3$Tmax.intera~YM3$YRMON),lty=2,col="grey10")   ;cor.test(YM3$Tmax.intera,YM3$YRMON)

plot  (YM3$HMIN~YM3$YRMON,type="l",main="Min. Rel. Humidity",col="grey80",ylab="Min. Rel. Humidity (%)",xlab="year")
points(YM3$Hmin.intera~YM3$YRMON,type="l",ylab="",xlab="",col="black")  
abline(lm(YM3$Hmin.intera~YM3$YRMON),lty=2,col="grey10") ;cor.test(YM3$Hmin.intera,YM3$YRMON)

plot   (YM3$HAVE~YM3$YRMON,type="l",main="Average Rel. Humidity",col="grey80",ylab="Ave. Rel. Humidity (%)",xlab="year")
points(YM3$Have.intera~YM3$YRMON,type="l",ylab="",xlab="",col="black")                      
abline(lm(YM3$Have.intera~YM3$YRMON),lty=2,col="grey10") ;cor.test(YM3$Have.intera,YM3$YRMON)

plot  (YM3$HMAX~YM3$YRMON,type="l",main="Max. Rel. Humidity",col="grey80",ylab="Max. Rel. Humidity (%)",xlab="year")
points(YM3$Hmax.intera~YM3$YRMON,type="l",ylab="",xlab="",col="black")                        
abline(lm(YM3$Hmax.intera~YM3$YRMON),lty=2,col="grey10") ;cor.test(YM3$Hmax.intera,YM3$YRMON)

par(mfrow=c(1,1))


## Create a season score column for monthly variables:
#-----------------------------------------------------
head(YM3) ; dim(YM3)
day   <- YM3$YRMON-YM3$YR ; day
day   <- day-min(day)
day   <- day/max(day)
day2  <- ceiling(day*364)+1
range(day2)

day4 <- cos(ceiling(day*360)/57.3) ; range(day4)
day5 <- as.matrix(day4) 
plot(decostand(day5[,1],"standardize"),type="l",col="red")
dim(day5)


#****************************************
###  Regression models (Figures 3-5)  ###
#****************************************
plot(1,1,col="white",xlim=c(0,10),ylim=c(0,10))


select1 <- c(1:192)
j=1

## START LOOP ##
#---------------
for(j in select1){
  
  if(j< 187){int=0;col1=j}
  if(j==187){int=1;col1=ncol(TS6)}
  if(j==188){int=2;col1=ncol(TS6)}
  if(j==189){int=3;col1=ncol(TS6)}
  if(j==190){int=4;col1=ncol(TS6)}
  if(j==191){int=5;col1=ncol(TS6)}
  if(j==192){int=6;col1=ncol(TS6)}
  
  #dim(TS6);dim(E2);dim(day5);dim(YM3)

  if(j<ncol(TS6)) { 
    w2              <- which(TS6[,j]>0) ; w2 ## in which plot species j is present
    resid           <- day5[w2]
    weighted.season <- rep(resid,TS6[w2,j])  ## weight season score by species occurrence
    MAT$SEAS.OPT[j] <- mean(weighted.season, na.rm=TRUE)
  } 
  
  if(j>=ncol(TS6)){ w2 <- c(1:nrow(E2)) }

  ## Abundance, and Nr years and months in which species is present
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Z        <- E2[w2,] 
  Z$ntraps <- TS6[w2,col1]
  #head(Z);dim(Z)

  MAT$ab[j]  <- sum(Z$ntraps)
  MAT$nyr[j] <- length(table(Z$YR)[table(Z$YR)>0])
  MAT$nmo[j] <- length(table(Z$MON)[table(Z$MON)>0])
    
  ## Matrix "intmo" containing the mean of each climate variable for each phenological year:
  intmo <- as.data.frame(matrix(0,ncol=9,nrow=18))
  rownames(intmo) <- c(2000:2017)
  colnames(intmo) <- c("IRRA","RAIN","TMIN","TAVE","TMAX","HMIN","HAVE","HMAX","Y")
  
  start.py.comm <- round(mean(BE$beginyr)/31) ; start.py.comm
  
  if(j<ncol(TS6)) {
    head(BE);dim(BE)
    wsp.be   <- which(BE$sp%in%colnames(TS6)[j]) ; wsp.be ## retrieve species i in the starting phenology year data
    start.py <- BE$MON[wsp.be] ; start.py                 ## starting month of the phenological year for species i
    #head(E2)
    wmon.start.py <- which(E2$MON%in%start.py)[1]  ; wmon.start.py
  }
  
  if(j>=ncol(TS6)) {
    wmon.start.py <- 5 ## starting phenological month at the comminity-level (obtained using the same function to calculate phenological years of each species)
  }
  
  TS66     <- TS6[-c(1:(wmon.start.py-1)),] ; head(TS66) ; dim(TS66) ## Truncate climate data to make it start the first month of the phenological year of species j
  E22      <- E2 [-c(1:(wmon.start.py-1)),] ; head(E22)  ; dim(E22)  ## idem for the climate data
        
  E22$YR   <- E2$YR  [1:nrow(E22)] ## do as if the phenological year starts in 2000 (year identity does not matter in OLS analyses)
  E22$MON  <- E2$MON [1:nrow(E22)] ## do as if the phenological month started in January
  E22$YMON <- E2$YMON[1:nrow(E22)] ## idem for YMON
       

  tabs     <- table(E22$YR) ; tabs                 ## to get the number of observations (months) per year
  keepyr   <- which(as.numeric(tabs)>=12) ; keepyr ## keep only the years with 12 observations to have equilibrate data among years
  yr.intmo <- names(tabs)[keepyr] ; yr.intmo       ## identities of these years
  intmo    <- intmo[rownames(intmo)%in%yr.intmo,]  ## truncated intmo matrix with these years, only.

  list.msr.irra <- as.data.frame(matrix(0,ncol=nMSR,nrow=nrow(intmo))) ; rownames(list.msr.irra) <- rownames(intmo)
  list.msr.rain <- list.msr.irra
  list.msr.tmin <- list.msr.irra ; list.msr.tave <- list.msr.irra ; list.msr.tmax <- list.msr.irra
  list.msr.hmin <- list.msr.irra ; list.msr.have <- list.msr.irra ; list.msr.hmax <- list.msr.irra
  
  vecxx <- c()
  v999  <- c()
  col1 = j
  if(j>=ncol(TS6)){col1=ncol(TS6)}

  for(h1 in 1:8){  ## for each climate variable...
       
       X100 <- E22[,h1] ; Y100 <- TS66[,col1] 
       
       Z200 <- as.data.frame(cbind(Y100,X100,E22$YR,E22$MON)) 
       colnames(Z200) <- c("Y","X","years","month") ; head(Z200) ; dim(Z200)
       Z200 <- Z200[Z200$years%in%yr.intmo,]        ; head(Z200) ; dim(Z200)

       ## Matrix of the sum of the flower production over 12 months, for each phenological year:
       Z401 <- as.data.frame(Z200 %>% group_by(years) %>% summarize(Fsum = sum(Y))) ; head(Z401) ; dim(Z401)
       
       if(h1==1){intmo$Y = Z401$Fsum} ## Store annual flower production (do it once for each species)
       
       ## Which years have presence of flowers:
       yr.pres <- Z401$years[which(Z401$Fsum>0)] ; yr.pres
       
 
       for(y1 in 1:length(yr.pres)){  ## for each year of flower presence
         
           if(yr.pres[y1]==2000){
             Z2Y <- rbind(Z200[Z200$years%in%c(yr.pres[y1]),],Z200[Z200$years%in%c(yr.pres[y1]),]) ## Z200 ok (not Z250 !)
           }
           if(yr.pres[y1]>2000){
             Z2Y <- Z200[Z200$years%in%c(yr.pres[y1]-1,yr.pres[y1]),] ## Z200 ok (not Z250 !)
           }
           Y01 <- Z2Y$Y[13:24] ; Y01 ## the response (flower production) for year y1
           X01 <- Z2Y$X[1 :24] ; X01 ## the climate values are for two consecutive years (24 months)

           #i5=8;j5=1
           vec.Y <- c() ; vec.X <- c()
           cors0 = 0

           if(j < ncol(TS6)){
             for(i5 in 3:12){
               for(j5 in 1:12){
                 Y02 = Y01[1:i5]
                 X02 = X01[(12+(1:i5))-j5+1] 
                 v9  =   c((12+(1:i5))-j5+1) ; v9
                 if(min(v9)>=1){
                 if(sum(Y02)==sum(Y01) & sd(X02)>0){
                   cors1 = cor(Y02,X02)
                   if(abs(cors1)>cors0){
                     vec.Y = c(1:i5) 
                     vec.X = c((12+(1:i5))-j5+1)
                     cors0 = cors1
                   }
                 }
                 }
               }
             }
           }
           
           cors0 = 0
           lim = 12 
           if(j>=ncol(TS6)){
             for(i5 in 1:12){
               for(j5 in 1:12){
                 Y02 = Y01[1:i5]
                 X02 = X01[(lim+(1:i5))-j5+1] 
                 v9  =   c((lim+(1:i5))-j5+1) ; v9
                 if(min(v9)>=1){
                 if(sum(Y02)>(0.9*sum(Y01)) & max(Y02)==max(Y01) & sd(X02)>0){
                   cors1 = cor(Y02,X02)
                   if(abs(cors1)>cors0){
                     vec.Y = c(1:i5) 
                     vec.X = c((lim+(1:i5))-j5+1)
                     cors0 = cors1
                     i6 = i5 ; j6 = j5 ; v10 = length(v9)
                   }
                 }
                 }
               }
             }
           }
           vec.Y ; vec.X
           cor(Y01[vec.Y],X01[vec.X])
           
           vecxx <- c(vecxx,vec.X)
           if(h1==3) { vecxx.tmin <- vecxx }
           
           intmo[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h1] <- mean(Z2Y$X[vec.X])
           
           for(h7 in 1:nMSR){
             
             MSR.E4 <- as.numeric(MSR[[h7]][,h1])
             MSR.E4 <- c(MSR.E4,MSR.E4) ; MSR.E4
  
             sta1 <- sample(length(MSR.E4)/2)[1]   ; sta1
             end1 <- sta1 + length(vec.X)-1        ; end1

             if(h1==1){list.msr.irra[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h7]<-mean(MSR.E4[sta1:end1])}
             if(h1==2){list.msr.rain[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h7]<-mean(MSR.E4[sta1:end1])}
             if(h1==3){list.msr.tmin[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h7]<-mean(MSR.E4[sta1:end1])}
             if(h1==4){list.msr.tave[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h7]<-mean(MSR.E4[sta1:end1])}
             if(h1==5){list.msr.tmax[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h7]<-mean(MSR.E4[sta1:end1])}
             if(h1==6){list.msr.hmin[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h7]<-mean(MSR.E4[sta1:end1])}
             if(h1==7){list.msr.have[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h7]<-mean(MSR.E4[sta1:end1])}
             if(h1==8){list.msr.hmax[which(as.numeric(rownames(intmo))%in%yr.pres[y1]),h7]<-mean(MSR.E4[sta1:end1])}
           }
           
       } ## END "for(y1 in 1:length(yr.pres)){"
       
       
  }  ## END "for(h1 in 1:8){  
       

  head(E2)
  E99 <- E2 %>% group_by(YR) %>% summarize(IRRA = mean(IRRA),RAIN = sum(RAIN),
                                           TMIN = mean(TMIN),TAVE = mean(TAVE),TMAX = mean(TMAX),
                                           HMIN = mean(HMIN),HAVE = mean(HAVE),HMAX = mean(HMAX)) 
  E98 <- as.data.frame(E99) ; head(E98) ; dim(E98) 
  mean(E98$RAIN[1:18])
  
  WY0    <- which(intmo$Y>0) ; WY0
  intmo2 <- intmo[WY0,]
  cor(intmo2,log(intmo2$Y+1))

  IRRA <- intmo2$IRRA ; RAIN <- intmo2$RAIN
  TMIN <- intmo2$TMIN ; TAVE <- intmo2$TAVE
  TMAX <- intmo2$TMAX ; HMIN <- intmo2$HMIN
  HAVE <- intmo2$HAVE ; HMAX <- intmo2$HMAX

  #------------------------------
  ## Store coefficient estimates:
  #------------------------------
  ABS55 <- intmo2
  ABS55$Y <- log(ABS55$Y+1)
  
  for(g in 1:9){ABS55[,g]<-decostand(ABS55[,g],"standardize")}

  head(ABS55) ; dim(ABS55)

  ## Response variable = total Nr of traps during each year of presence:
  #---------------------------------------------------------------------
  Y10  = ABS55$Y
  
  ## For each explanatory variables, calculate and test coefficient of the OLS model:
  #----------------------------------------------------------------------------------
  #u3=3
  for(u3 in 1:8){
    
  if(u3==1){ X10 = ABS55$IRRA } ; if(u3==2){ X10 = ABS55$RAIN }
  if(u3==3){ X10 = ABS55$TMIN } ; if(u3==4){ X10 = ABS55$TAVE }
  if(u3==5){ X10 = ABS55$TMAX } ; if(u3==6){ X10 = ABS55$HMIN }
  if(u3==7){ X10 = ABS55$HAVE } ; if(u3==8){ X10 = ABS55$HMAX }
  
  ## Calculate and store observed coefficient:
  #-------------------------------------------
  mod1 = glm(Y10~X10, family = gaussian()) ; mod1
  obs  = as.numeric(mod1$coefficients[2]) 
  
  if(u3==1){ MAT$irra[j] = obs } ; if(u3==2){ MAT$rain[j] = obs }
  if(u3==3){ MAT$tmin[j] = obs } ; if(u3==4){ MAT$tave[j] = obs }
  if(u3==5){ MAT$tmax[j] = obs } ; if(u3==6){ MAT$hmin[j] = obs }
  if(u3==7){ MAT$have[j] = obs } ; if(u3==8){ MAT$hmax[j] = obs }
  
  ## MSR test:
  #-----------
  null <- c() 
  #rd=1
  #dim(intmo2)
  
  for(rd in 1:nMSR){
    
    if(u3==1){ Xnull = list.msr.irra[WY0,rd] }
    if(u3==2){ Xnull = list.msr.rain[WY0,rd] }
    if(u3==3){ Xnull = list.msr.tmin[WY0,rd] }
    if(u3==4){ Xnull = list.msr.tave[WY0,rd] }
    if(u3==5){ Xnull = list.msr.tmax[WY0,rd] }
    if(u3==6){ Xnull = list.msr.hmin[WY0,rd] }
    if(u3==7){ Xnull = list.msr.have[WY0,rd] }
    if(u3==8){ Xnull = list.msr.hmax[WY0,rd] }
    
    ## Normalize: 
    lambda1 <- BoxCox.lambda(Xnull+abs(min(Xnull))+0.0000000001,method = "loglik", lower = -3,  upper = 3) 
    Xnull   <- BoxCox(Xnull, lambda=lambda1)
    ## Standardize: 
    Xnull = decostand(Xnull,"standardize")
    mod2  = glm(Y10~Xnull,family = gaussian()) 
    null  = c(null,as.numeric(mod2$coefficients[2]))
    if(u3==1){effect.null.irra[j,rd] <- as.numeric(mod2$coefficients[2])}
    if(u3==2){effect.null.rain[j,rd] <- as.numeric(mod2$coefficients[2])}
    if(u3==3){effect.null.tmin[j,rd] <- as.numeric(mod2$coefficients[2])}
    if(u3==4){effect.null.tave[j,rd] <- as.numeric(mod2$coefficients[2])}
    if(u3==5){effect.null.tmax[j,rd] <- as.numeric(mod2$coefficients[2])}
    if(u3==6){effect.null.hmin[j,rd] <- as.numeric(mod2$coefficients[2])}
    if(u3==7){effect.null.have[j,rd] <- as.numeric(mod2$coefficients[2])}
    if(u3==8){effect.null.hmax[j,rd] <- as.numeric(mod2$coefficients[2])}

  }
  quant0 = range(null)
  quant1 = quantile(null,probs=c(0.05,0.95))
  quant2 = quantile(null,probs=c(0.025,0.975))
  quant3 = quantile(null,probs=c(0.005,0.995))
  
  if(u3==1){ MAT$irra.low[j] = quant0[1] ; MAT$irra.ups[j] = quant0[2] ; MAT$irra.sign[j] = 0 }
  if(u3==2){ MAT$rain.low[j] = quant0[1] ; MAT$rain.ups[j] = quant0[2] ; MAT$rain.sign[j] = 0 }
  if(u3==3){ MAT$tmin.low[j] = quant0[1] ; MAT$tmin.ups[j] = quant0[2] ; MAT$tmin.sign[j] = 0 }
  if(u3==4){ MAT$tave.low[j] = quant0[1] ; MAT$tave.ups[j] = quant0[2] ; MAT$tave.sign[j] = 0 }
  if(u3==5){ MAT$tmax.low[j] = quant0[1] ; MAT$tmax.ups[j] = quant0[2] ; MAT$tmax.sign[j] = 0 }
  if(u3==6){ MAT$hmin.low[j] = quant0[1] ; MAT$hmin.ups[j] = quant0[2] ; MAT$hmin.sign[j] = 0 }
  if(u3==7){ MAT$have.low[j] = quant0[1] ; MAT$have.ups[j] = quant0[2] ; MAT$have.sign[j] = 0 }
  if(u3==8){ MAT$hmax.low[j] = quant0[1] ; MAT$hmax.ups[j] = quant0[2] ; MAT$hmax.sign[j] = 0 }
  
  ## encode "1" if P<=0.05
  if(obs<=quant1[1]){
    if(u3==1){MAT$irra.sign[j]=1} ; if(u3==2){MAT$rain.sign[j]=1}
    if(u3==3){MAT$tmin.sign[j]=1} ; if(u3==4){MAT$tave.sign[j]=1}
    if(u3==5){MAT$tmax.sign[j]=1} ; if(u3==6){MAT$hmin.sign[j]=1}
    if(u3==7){MAT$have.sign[j]=1} ; if(u3==8){MAT$hmax.sign[j]=1}
  }
  if(obs>=quant1[2]){
    if(u3==1){MAT$irra.sign[j]=1} ; if(u3==2){MAT$rain.sign[j]=1}
    if(u3==3){MAT$tmin.sign[j]=1} ; if(u3==4){MAT$tave.sign[j]=1}
    if(u3==5){MAT$tmax.sign[j]=1} ; if(u3==6){MAT$hmin.sign[j]=1}
    if(u3==7){MAT$have.sign[j]=1} ; if(u3==8){MAT$hmax.sign[j]=1}
  }
  
  ## encode "2" if P<=0.01
  if(obs<=quant2[1]){
    if(u3==1){MAT$irra.sign[j]=2} ; if(u3==2){MAT$rain.sign[j]=2}
    if(u3==3){MAT$tmin.sign[j]=2} ; if(u3==4){MAT$tave.sign[j]=2}
    if(u3==5){MAT$tmax.sign[j]=2} ; if(u3==6){MAT$hmin.sign[j]=2}
    if(u3==7){MAT$have.sign[j]=2} ; if(u3==8){MAT$hmax.sign[j]=2}
  }
  if(obs>=quant2[2]){
    if(u3==1){MAT$irra.sign[j]=2} ; if(u3==2){MAT$rain.sign[j]=2}
    if(u3==3){MAT$tmin.sign[j]=2} ; if(u3==4){MAT$tave.sign[j]=2}
    if(u3==5){MAT$tmax.sign[j]=2} ; if(u3==6){MAT$hmin.sign[j]=2}
    if(u3==7){MAT$have.sign[j]=2} ; if(u3==8){MAT$hmax.sign[j]=2}
  }
  
  ## encode "3" if P<=0.005
  if(obs<=quant3[1]){
    if(u3==1){MAT$irra.sign[j]=3} ; if(u3==2){MAT$rain.sign[j]=3}
    if(u3==3){MAT$tmin.sign[j]=3} ; if(u3==4){MAT$tave.sign[j]=3}
    if(u3==5){MAT$tmax.sign[j]=3} ; if(u3==6){MAT$hmin.sign[j]=3}
    if(u3==7){MAT$have.sign[j]=3} ; if(u3==8){MAT$hmax.sign[j]=3}
  }
  if(obs>=quant3[2]){
    if(u3==1){MAT$irra.sign[j]=3} ; if(u3==2){MAT$rain.sign[j]=3}
    if(u3==3){MAT$tmin.sign[j]=3} ; if(u3==4){MAT$tave.sign[j]=3}
    if(u3==5){MAT$tmax.sign[j]=3} ; if(u3==6){MAT$hmin.sign[j]=3}
    if(u3==7){MAT$have.sign[j]=3} ; if(u3==8){MAT$hmax.sign[j]=3}
  }
  
  } ## END "for(u3 in 1:8)

  ## Test the same way but for interaction coefficients:
  # - - - - - - - - - - - - - - - - - - - - - - - - - - 

  if(int==0){
    
  MAT$tmin.part[j]          =999
  MAT$hmax.part[j]          =999
  MAT$tmin.hmax.part[j]     =999
  MAT$tmin.hmax.only[j]     =999
  MAT$tmin.part.low[j]      =999
  MAT$hmax.part.low[j]      =999
  MAT$tmin.hmax.part.low[j] =999
  MAT$tmin.part.ups[j]      =999
  MAT$hmax.part.ups[j]      =999
  MAT$tmin.hmax.part.ups[j] =999
  MAT$tmin.part.sign[j]     =999
  MAT$hmax.part.sign[j]     =999
  MAT$tmin.hmax.part.sign[j]=999
  
  MAT$tmin.hmax.only.low[j] =999
  MAT$tmin.hmax.only.ups[j] =999
  MAT$tmin.hmax.only.sign[j]=999
  }
  
  if(int>0){
    
  if(int==1){
    modd1 = glm(Y10~ABS55$TMIN*ABS55$HMAX,family = gaussian())
    modd2 = glm(Y10~ABS55$TMIN:ABS55$HMAX,family = gaussian())
  }
  if(int==2){
    modd1 = glm(Y10~ABS55$TMAX*ABS55$HMAX,family = gaussian())
    modd2 = glm(Y10~ABS55$TMAX:ABS55$HMAX,family = gaussian())
  }
  if(int==3){
    modd1 = glm(Y10~ABS55$TMIN*ABS55$IRRA,family = gaussian())
    modd2 = glm(Y10~ABS55$TMIN:ABS55$IRRA,family = gaussian())
  }
  if(int==4){
    modd1 = glm(Y10~ABS55$TMAX*ABS55$IRRA,family = gaussian())
    modd2 = glm(Y10~ABS55$TMAX:ABS55$IRRA,family = gaussian())
  }
  if(int==5){
    modd1 = glm(Y10~ABS55$TMIN*ABS55$HAVE,family = gaussian())
    modd2 = glm(Y10~ABS55$TMIN:ABS55$HAVE,family = gaussian())
  }
  if(int==6){
    modd1 = glm(Y10~ABS55$TMAX*ABS55$HAVE,family = gaussian())
    modd2 = glm(Y10~ABS55$TMAX:ABS55$HAVE,family = gaussian())
  }
  
  obs.tmin      = as.numeric(modd1$coefficients[2]) ; MAT$tmin.part[j] = obs.tmin
  obs.hmax      = as.numeric(modd1$coefficients[3]) ; MAT$hmax.part[j] = obs.hmax
  obs.tmin.hmax = as.numeric(modd1$coefficients[4]) ; MAT$tmin.hmax.part[j] = obs.tmin.hmax
  
  obss2 = as.numeric(modd2$coefficients[2]) ; MAT$tmin.hmax.only[j] = obss2 
  
  null.tmin <- c()
  null.hmax <- c() 
  null.tmin.hmax <- c() 
  null2 <- c()
  
  for(rd in 1:nMSR){
    
     if(int==1){
       Tminnull = list.msr.tmin[WY0,rd]
       Hmaxnull = list.msr.hmax[WY0,rd]
     }
     if(int==2){
       Tminnull = list.msr.tmax[WY0,rd]
       Hmaxnull = list.msr.hmax[WY0,rd]
     }
     if(int==3){
       Tminnull = list.msr.tmin[WY0,rd]
       Hmaxnull = list.msr.irra[WY0,rd]
     }
     if(int==4){
       Tminnull = list.msr.tmax[WY0,rd]
       Hmaxnull = list.msr.irra[WY0,rd]
     }
     if(int==5){
       Tminnull = list.msr.tmin[WY0,rd]
       Hmaxnull = list.msr.have[WY0,rd]
     }
    if(int==6){
       Tminnull = list.msr.tmax[WY0,rd]
       Hmaxnull = list.msr.have[WY0,rd]
    }
    
     ## Normalize -standardize: 
     lambda1  <- BoxCox.lambda(Tminnull+abs(min(Tminnull))+0.0000000001,method = "loglik", lower = -3,  upper = 3) 
     Tminnull <- BoxCox(Tminnull, lambda=lambda1) ; Tminnull <- decostand(Tminnull,"standardize")
     
     lambda2  <- BoxCox.lambda(Hmaxnull+abs(min(Hmaxnull))+0.0000000001,method = "loglik", lower = -3,  upper = 3) 
     Hmaxnull <- BoxCox(Hmaxnull, lambda=lambda2) ; Hmaxnull <- decostand(Hmaxnull,"standardize")
     
     mod22 = glm(Y10~Tminnull*Hmaxnull,family = gaussian())
     mod23 = glm(Y10~Tminnull:Hmaxnull,family = gaussian()) 
     
     null.tmin      = c(null.tmin,      as.numeric(mod22$coefficients[2]))
     null.hmax      = c(null.hmax,      as.numeric(mod22$coefficients[3]))
     null.tmin.hmax = c(null.tmin.hmax, as.numeric(mod22$coefficients[4]))
     
     null2 = c(null2,as.numeric(mod23$coefficients[2]))
     
     effect.null.tmin.part[j,rd]      <- as.numeric(mod22$coefficients[2])
     effect.null.hmax.part[j,rd]      <- as.numeric(mod22$coefficients[3])
     effect.null.tmin.hmax.part[j,rd] <- as.numeric(mod22$coefficients[4])
     
     effect.null.tmin.hmax.only[j,rd] <- as.numeric(mod23$coefficients[2])
     
  }

  for(q2 in 1:4){
    
  if(q2==1){null9 <- null.tmin}  
  if(q2==2){null9 <- null.hmax}  
  if(q2==3){null9 <- null.tmin.hmax}  
  if(q2==4){null9 <- null2}  
    
  quant0 = range(null9)
  quant5 = quantile(null9,probs=c(0.1,0.9))
  quant1 = quantile(null9,probs=c(0.05,0.95))
  quant2 = quantile(null9,probs=c(0.025,0.975))
  quant3 = quantile(null9,probs=c(0.005,0.995))
  
  if(q2==1){
    MAT$tmin.part.low[j]  = quant0[1] 
    MAT$tmin.part.ups[j]  = quant0[2] 
    MAT$tmin.part.sign[j] = 0
    if(obs.tmin<=quant1[1]){ MAT$tmin.part.sign[j]=1 }
    if(obs.tmin>=quant1[2]){ MAT$tmin.part.sign[j]=1 }
    if(obs.tmin<=quant2[1]){ MAT$tmin.part.sign[j]=2 }
    if(obs.tmin>=quant2[2]){ MAT$tmin.part.sign[j]=2 }
    if(obs.tmin<=quant3[1]){ MAT$tmin.part.sign[j]=3 }
    if(obs.tmin>=quant3[2]){ MAT$tmin.part.sign[j]=3 }
  }
  if(q2==2){
    MAT$hmax.part.low[j]  = quant0[1] 
    MAT$hmax.part.ups[j]  = quant0[2] 
    MAT$hmax.part.sign[j] = 0
    if(obs.hmax<=quant1[1]){ MAT$hmax.part.sign[j]=1 }
    if(obs.hmax>=quant1[2]){ MAT$hmax.part.sign[j]=1 }
    if(obs.hmax<=quant2[1]){ MAT$hmax.part.sign[j]=2 }
    if(obs.hmax>=quant2[2]){ MAT$hmax.part.sign[j]=2 }
    if(obs.hmax<=quant3[1]){ MAT$hmax.part.sign[j]=3 }
    if(obs.hmax>=quant3[2]){ MAT$hmax.part.sign[j]=3 }
  }
  if(q2==3){
    MAT$tmin.hmax.part.low[j]  = quant0[1] 
    MAT$tmin.hmax.part.ups[j]  = quant0[2] 
    MAT$tmin.hmax.part.sign[j] = 0
    if(obs.tmin.hmax<=quant5[1]){ MAT$tmin.hmax.part.sign[j]=0.5 }
    if(obs.tmin.hmax>=quant5[2]){ MAT$tmin.hmax.part.sign[j]=0.5 }
    if(obs.tmin.hmax<=quant1[1]){ MAT$tmin.hmax.part.sign[j]=1 }
    if(obs.tmin.hmax>=quant1[2]){ MAT$tmin.hmax.part.sign[j]=1 }
    if(obs.tmin.hmax<=quant2[1]){ MAT$tmin.hmax.part.sign[j]=2 }
    if(obs.tmin.hmax>=quant2[2]){ MAT$tmin.hmax.part.sign[j]=2 }
    if(obs.tmin.hmax<=quant3[1]){ MAT$tmin.hmax.part.sign[j]=3 }
    if(obs.tmin.hmax>=quant3[2]){ MAT$tmin.hmax.part.sign[j]=3 }
  }
  if(q2==4){
    MAT$tmin.hmax.only.low[j]  = quant0[1] 
    MAT$tmin.hmax.only.ups[j]  = quant0[2] 
    MAT$tmin.hmax.only.sign[j] = 0
    if(obss2<=quant1[1]){ MAT$tmin.hmax.only.sign[j]=1 }
    if(obss2>=quant1[2]){ MAT$tmin.hmax.only.sign[j]=1 }
    if(obss2<=quant2[1]){ MAT$tmin.hmax.only.sign[j]=2 }
    if(obss2>=quant2[2]){ MAT$tmin.hmax.only.sign[j]=2 }
    if(obss2<=quant3[1]){ MAT$tmin.hmax.only.sign[j]=3 }
    if(obss2>=quant3[2]){ MAT$tmin.hmax.only.sign[j]=3 }
  }
  
  }
    
  } ## END "if(int>0)"

  ## Guild, family and genus of each species:
  # - - - - - - - - - - - - - - - - - - - - -
  wsp2          <- which(T2$CODIGO%in%colnames(TS6)[j])[1]
  MAT$guild[j]  <- as.character(T2$guild[wsp2])      
  MAT$guild2[j] <- as.character(T2$guild2[wsp2])  
  MAT$fam[j]    <- as.character(T2$fam[wsp2])      
  MAT$gen[j]    <- as.character(T2$gen[wsp2])      
  
  points(5,5,pch=15,cex=10,col="white");text(5,5,paste0(j,"/",ncol(TS4)),cex=1,col="black")
  write.table(MAT,"MAT.ongoing.txt")
  
  if(int==1){
    MAT1        <- MAT
    part.tmin.1 <- MAT1$tmin.part[j]      ; null.part.tmin.1 <- as.numeric(effect.null.tmin.part     [j,]) 
    part.hmax.1 <- MAT1$hmax.part[j]      ; null.part.hmax.1 <- as.numeric(effect.null.hmax.part     [j,]) 
    tmin.hmax.1 <- MAT1$tmin.hmax.part[j] ; null.tmin.hmax.1 <- as.numeric(effect.null.tmin.hmax.part[j,]) 
  }
  
  if(int==2){
    MAT2        <- MAT
    part.tmax.2 <- MAT2$tmin.part[j]      ; null.part.tmax.2 <- as.numeric(effect.null.tmin.part     [j,]) 
    part.hmax.2 <- MAT2$hmax.part[j]      ; null.part.hmax.2 <- as.numeric(effect.null.hmax.part     [j,]) 
    tmax.hmax.2 <- MAT2$tmin.hmax.part[j] ; null.tmax.hmax.2 <- as.numeric(effect.null.tmin.hmax.part[j,]) 
  }
    
  if(int==3){
    MAT3        <- MAT
    part.tmin.3 <- MAT3$tmin.part[j]      ; null.part.tmin.3 <- as.numeric(effect.null.tmin.part     [j,]) 
    part.irra.3 <- MAT3$hmax.part[j]      ; null.part.irra.3 <- as.numeric(effect.null.hmax.part     [j,]) 
    tmin.irra.3 <- MAT3$tmin.hmax.part[j] ; null.tmin.irra.3 <- as.numeric(effect.null.tmin.hmax.part[j,])
  }
  
  if(int==4){
    MAT4        <- MAT
    part.tmax.4 <- MAT4$tmin.part[j]      ; null.part.tmax.4 <- as.numeric(effect.null.tmin.part     [j,]) 
    part.irra.4 <- MAT4$hmax.part[j]      ; null.part.irra.4 <- as.numeric(effect.null.hmax.part     [j,]) 
    tmax.irra.4 <- MAT4$tmin.hmax.part[j] ; null.tmax.irra.4 <- as.numeric(effect.null.tmin.hmax.part[j,])
  }
  
  if(int==5){
    MAT5        <- MAT
    part.tmin.5 <- MAT5$tmin.part[j]      ; null.part.tmin.5 <- as.numeric(effect.null.tmin.part     [j,]) 
    part.have.5 <- MAT5$hmax.part[j]      ; null.part.have.5 <- as.numeric(effect.null.hmax.part     [j,]) 
    tmin.have.5 <- MAT5$tmin.hmax.part[j] ; null.tmin.have.5 <- as.numeric(effect.null.tmin.hmax.part[j,]) 
  }
  
  if(int==6){
    MAT6        <- MAT
    part.tmax.6 <- MAT6$tmin.part[j]      ; null.part.tmax.6 <- as.numeric(effect.null.tmin.part     [j,]) 
    part.have.6 <- MAT6$hmax.part[j]      ; null.part.have.6 <- as.numeric(effect.null.hmax.part     [j,]) 
    tmax.have.6 <- MAT6$tmin.hmax.part[j] ; null.tmax.have.6 <- as.numeric(effect.null.tmin.hmax.part[j,]) 
  }
  
} ### END LOOP #####


#MAT <- as.data.frame(read.table("MAT.ongoing.txt",h=T))
head(MAT) ; dim(MAT)
MAT[187:192,]

effect.null.tmin.part[187:192,]


#*************************************************
###    See results and make figures 3 and 4    ###
#*************************************************

head(MAT);dim(MAT)
MAT[1:10,]


##***************************************
## JANUARY 2023 (community-level signal):
##***************************************

com1 = 187 ; RES <- as.data.frame(MAT[com1,]) ; RES 
effect.null.irra[187:190,]

## Plots for Fig. 3 => simple OLS models at the community level:
#---------------------------------------------------------------
par(mfrow=c(4,3),mex=0.6)
i=9

for(i in 1:9){
  
if(i==1) {A <- RES$irra ; N1 <- as.numeric(effect.null.irra[com1,]) ; titlex <- "coefficient" ; title2 <- "Irradiance"}
if(i==2) {A <- RES$rain ; N1 <- as.numeric(effect.null.rain[com1,]) ; titlex <- "coefficient" ; title2 <- "Rainfall"}
if(i==3) {plot(1,1,col="white",fg = "white",col.axis="white",ylab="",xlab="")}
if(i==4) {A <- RES$tmin ; N1 <- as.numeric(effect.null.tmin[com1,]) ; titlex <- "coefficient" ; title2 <- "TMIN"}
if(i==5) {A <- RES$tave ; N1 <- as.numeric(effect.null.tave[com1,]) ; titlex <- "coefficient" ; title2 <- "TAVE"}
if(i==6) {A <- RES$tmax ; N1 <- as.numeric(effect.null.tmax[com1,]) ; titlex <- "coefficient" ; title2 <- "TMAX"}
if(i==7) {A <- RES$hmin ; N1 <- as.numeric(effect.null.hmin[com1,]) ; titlex <- "coefficient" ; title2 <- "HMIN"}
if(i==8) {A <- RES$have ; N1 <- as.numeric(effect.null.have[com1,]) ; titlex <- "coefficient" ; title2 <- "HAVE"}
if(i==9) {A <- RES$hmax ; N1 <- as.numeric(effect.null.hmax[com1,]) ; titlex <- "coefficient" ; title2 <- "HMAX"}

if(i%in%c(1,2,4,5,6,7,8,9)){
  
all <- c(A,N1) ; P <- length(all[all<=A])/(nMSR+1) ; P
1-P

col30="grey50";width=1
if(P<=0.05){col30="red";width=2}
if(P>=0.95){col30="blue";width=2}

x <- stats::density(N1)$x
y <- stats::density(N1)$y ; y0 <- y/sum(y) ; sum(y0)
plot(x,y,type="l",col="white",main=title2,xlab=titlex,ylab="density")
polygon(c(x[x>=A], A), c(y[x>=A], y[x==max(x)]), col="#E3F2FD", border=NA)
polygon(c(x[x<=A], A), c(y[x<=A], y[x==max(x)]), col="#FCE4EC", border=NA)

abline(v=A,col=col30,lwd=width);abline(v=0,lty=2,col="grey60")

}

}
par(mfrow=c(1,1))

## Plots for Fig. 4 => partial and interaction terms at the community level:
#---------------------------------------------------------------------------
Z = cbind(ABS55,as.numeric(rownames(ABS55))) ; colnames(Z)[ncol(Z)] = "YR" ; head(Z) ; dim(Z)
head(Z);dim(Z)

Y30 <- Z$Y # plot(Y30,type="l")

IRRA <- Z$IRRA
RAIN <- Z$RAIN
TMIN <- Z$TMIN
TAVE <- Z$TAVE
TMAX <- Z$TMAX
HMIN <- Z$HMIN
HAVE <- Z$HAVE
HMAX <- Z$HMAX

## lowhigh is to check X50 effect when lowhigh is low or high

par(mfrow=c(6,5),mex=0.5)

for(i in 1:30){

  if(i==1)  {A <- part.tmin.1 ; N1 <- null.part.tmin.1 ; titlex <- "coefficient" ; title2 <- "Tmin (partial)"}
  if(i==2)  {A <- part.hmax.1 ; N1 <- null.part.hmax.1 ; titlex <- "coefficient" ; title2 <- "Hmax (partial)"}
  if(i==3)  {A <- tmin.hmax.1 ; N1 <- null.tmin.hmax.1 ; titlex <- "coefficient" ; title2 <- "Tmin : Hmax"}

  if(i==6)  {A <- part.tmax.2 ; N1 <- null.part.tmax.2 ; titlex <- "coefficient" ; title2 <- "Tmax (partial)"}
  if(i==7)  {A <- part.hmax.2 ; N1 <- null.part.hmax.2 ; titlex <- "coefficient" ; title2 <- "Hmax (partial)"}
  if(i==8)  {A <- tmax.hmax.2 ; N1 <- null.tmax.hmax.2 ; titlex <- "coefficient" ; title2 <- "Tmax : Hmax"}

  if(i==11) {A <- part.tmin.3 ; N1 <- null.part.tmin.3 ; titlex <- "coefficient" ; title2 <- "Tmin (partial)"}
  if(i==12) {A <- part.irra.3 ; N1 <- null.part.irra.3 ; titlex <- "coefficient" ; title2 <- "Irrad (partial)"}
  if(i==13) {A <- tmin.irra.3 ; N1 <- null.tmin.irra.3 ; titlex <- "coefficient" ; title2 <- "Tmin : Irrad"}

  if(i==16) {A <- part.tmax.4 ; N1 <- null.part.tmax.4 ; titlex <- "coefficient" ; title2 <- "Tmax (partial)"}
  if(i==17) {A <- part.irra.4 ; N1 <- null.part.irra.4 ; titlex <- "coefficient" ; title2 <- "Irrad (partial)"}
  if(i==18) {A <- tmax.irra.4 ; N1 <- null.tmax.irra.4 ; titlex <- "coefficient" ; title2 <- "Tmax : Irrad"}

  if(i==21) {A <- part.tmin.5 ; N1 <- null.part.tmin.5 ; titlex <- "coefficient" ; title2 <- "Tmin (partial)"}
  if(i==22) {A <- part.have.5 ; N1 <- null.part.have.5 ; titlex <- "coefficient" ; title2 <- "Have (partial)"}
  if(i==23) {A <- tmin.have.5 ; N1 <- null.tmin.have.5 ; titlex <- "coefficient" ; title2 <- "Tmin : Have"}

  if(i==26) {A <- part.tmax.6 ; N1 <- null.part.tmax.6 ; titlex <- "coefficient" ; title2 <- "Tmax (partial)"}
  if(i==27) {A <- part.have.6 ; N1 <- null.part.have.6 ; titlex <- "coefficient" ; title2 <- "Have (partial)"}
  if(i==28) {A <- tmax.have.6 ; N1 <- null.tmax.have.6 ; titlex <- "coefficient" ; title2 <- "Tmax : Have"}

  if(i%in%c(4,5,9,10,14,15,19,20,24,25,29,30)){
    
    if(i==4) {X50 <- HMAX ; lowhigh <- TMIN ; titl = "Hmax effect ~ Tmin"; titx <- "Hmax"}
    if(i==5) {X50 <- TMIN ; lowhigh <- HMAX ; titl = "Tmin effect ~ Hmax"; titx <- "Tmin"}
    if(i==9) {X50 <- HMAX ; lowhigh <- TMAX ; titl = "Hmax effect ~ Tmax"; titx <- "Hmax"}
    if(i==10){X50 <- TMAX ; lowhigh <- HMAX ; titl = "Tmax effect ~ Hmax"; titx <- "Tmax"}
    if(i==14){X50 <- IRRA ; lowhigh <- TMIN ; titl = "Irrad effect ~ Tmin"; titx <- "Irradiance"}
    if(i==15){X50 <- TMIN ; lowhigh <- IRRA ; titl = "Tmin effect ~ Irrad"; titx <- "Tmin"}
    if(i==19){X50 <- IRRA ; lowhigh <- TMAX ; titl = "Irrad effect ~ Tmax"; titx <- "Irradiance"}
    if(i==20){X50 <- TMAX ; lowhigh <- IRRA ; titl = "Tmax effect ~ Irrad"; titx <- "Tmax"}
    if(i==24){X50 <- HAVE ; lowhigh <- TMIN ; titl = "Have effect ~ Tmin"; titx <- "Have"}
    if(i==25){X50 <- TMIN ; lowhigh <- HAVE ; titl = "Tmin effect ~ Have"; titx <- "Tmin"}
    if(i==29){X50 <- HAVE ; lowhigh <- TMAX ; titl = "Have effect ~ Tmax"; titx <- "Have"}
    if(i==30){X50 <- TMAX ; lowhigh <- HAVE ; titl = "Tmax effect ~ Have"; titx <- "Tmax"}
    
    # Y effect when X is low and high
    q1 <- quantile(lowhigh,probs=c(0.5,0.5)) ; q1
    wlow  <- which(lowhigh<=q1[1]) ; wups <- which(lowhigh>=q1[2])
    wlow ; wups
    cor1 <- round(cor(Y30[wlow],X50[wlow]),2) ; if(cor1<0){col50="red"}  ; if(cor1>0){col50="red"}  ; cor1
    cor2 <- round(cor(Y30[wups],X50[wups]),2) ; if(cor2<0){col60="blue"} ; if(cor2>0){col60="blue"} ; cor2
    Y40 <- decostand(Y30,"standardize")
    
    
    for(e in 1:2){
      
      if(e==1){y = Y40[wlow] ; x = X50[wlow]}
      if(e==2){y = Y40[wups] ; x = X50[wups]}
      
      lm.out <- lm(y ~ x)
      newx = seq(min(X50),max(X50),by = 0.01)
      conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence",
                               level = 0.95)
      if(e==1){plot  (x,y,ylab="Fcom",pch=16,cex=1,col="white",cex.axis=1,main=titl,xlab=titx,
                      xlim=c(min(X50),max(X50)),ylim=c(min(Y40),max(Y40)*2.2))
        abline(lm.out, col="red",lwd=2,lty=1)
        #lines(newx, conf_interval[,1], col="red", lty=1,lwd=2)
        #lines(newx, conf_interval[,2], col="red", lty=2)
        #lines(newx, conf_interval[,3], col="red", lty=2)
        shade(rbind(conf_interval[,2],conf_interval[,3]),newx,col=col.alpha("red",0.08))
        
      }
      if(e==2){#points(x,y,xlab="x",ylab="y",pch=16,cex=1,col="white")         
        abline(lm.out, col="blue",lwd=2,lty=1)
        #lines(newx, conf_interval[,1], col="blue", lty=1,lwd=2)
        #lines(newx, conf_interval[,2], col="blue", lty=2)
        #lines(newx, conf_interval[,3], col="blue", lty=2)
        shade(rbind(conf_interval[,2],conf_interval[,3]),newx,col=col.alpha("blue",0.08))
      }
      
    }
    
  }
  
  if(i%in%c(1,2,3,6,7,8,11,12,13,16,17,18,21,22,23,26,27,28)){
  
    all <- c(A,N1) ; P <- length(all[all<=A])/(nMSR+1) ; P
    1-P
    P2 = 1-P
    if(P==1) {P=0.001}
    if(P2==0){P=0.001}
    
    col30="grey50";width=1
    if(P<=0.1) {col30="#FFB74D" ;width=2;lin <- 1}
    if(P>=0.9) {col30="#81D4FA";width=2;lin <- 1}
    if(P<=0.05){col30="red" ;width=2;lin <- 1}
    if(P>=0.95){col30="blue";width=2;lin <- 1} 
    
    x <- stats::density(N1)$x
    y <- stats::density(N1)$y ; y0 <- y/sum(y) ; sum(y0)
    plot(x,y,type="l",col="white",main=title2,xlab=titlex,ylab="density")
    polygon(c(x[x>=A], A), c(y[x>=A], y[x==max(x)]), col="#E3F2FD", border=NA)
    polygon(c(x[x<=A], A), c(y[x<=A], y[x==max(x)]), col="#FCE4EC", border=NA)
    
    abline(v=A,col=col30,lwd=width,lty=lin);abline(v=0,lty=2,col="grey60")
    
    if(P>0.5) {text(0,0.8,1-P)}
    if(P<=0.5){text(0,0.8,P)}
    
  }
  
}

#--------------------------------------
## Compare response among guilds - OK!:
#--------------------------------------
guild20 <- MAT$guild

## Choose pair of guilds to compare
sel1 <- which(guild20%in%c("TE","TC","TU")) ; sel1

guild30 <- guild20[sel1] ; guild40 <- rep(1,length(guild30)) ; guild40[1:length(guild30)] <- as.character(guild30) ; guild40
resp1   <- MAT$irra[sel1]
table(guild40)
guild40[which(guild40%in%"TU")] <- "1.Subcanopy"
guild40[which(guild40%in%"TC")] <- "2.Canopy"
guild40[which(guild40%in%"TE")] <- "3.Emergent"

par(mfrow=c(1,1))
boxplot(resp1~as.factor(guild40),cex.lab=1.2,
        ylab="Species response to irradiance",
        xlab="Tree guild",
        col=c("lightblue","lightgreen","pink"))
abline(h=0,lty=3,col="grey30")
kt <- kruskal.test(guild40,resp1)
res <- paste0("Pval (Krusk-W. test) = ",round(kt$p.value,2))
text(1,0.6,res)



#**************************************************
## Plot distribution of species coefficient values:
#**************************************************

par(mfrow=c(1,1))

MAT0  <- MAT[1:186,]
#MAT0  <- MAT ; head(MAT0) ; dim(MAT0)
MAT00 <- MAT0[which(MAT0$nyr>=10),]
head(MAT0);dim(MAT0)

Z <- MAT0[,which(colnames(MAT0)%in%c("irra","rain","tmin","tave","tmax",
                                     "hmin","have","hmax"))]
head(Z);dim(Z)

L <- MAT0[,which(colnames(MAT0)%in%c("irra.low","rain.low","tmin.low","tave.low","tmax.low",
                                     "hmin.low","have.low","hmax.low"))]
head(L);dim(L)

U <- MAT0[,which(colnames(MAT0)%in%c("irra.ups","rain.ups","tmin.ups","tave.ups","tmax.ups",
                                     "hmin.ups","have.ups","hmax.ups","pca1.ups","pca2.ups"))]
head(U);dim(U)

S <- MAT0[,which(colnames(MAT0)%in%c("irra.sign","rain.sign","tmin.sign","tave.sign","tmax.sign",
                                     "hmin.sign","have.sign","hmax.sign","pca1.sign","pca2.sign"))]
head(S);dim(S)

title <- c("Irradiance","Rainfall","Tmin","Tave","Tmax","Hmin","Have","Hmax","PCA1","PCA2")

## Season score matrix:
P7 <- as.data.frame(cbind(MAT0,1,1,1,1,1,1,1,1,1,1,1,1))
colnames(P7)[(ncol(MAT0)+1):ncol(P7)] <- c("Ja","Fe","Ms","Ap","My","Jn","Jy","Au","Se","Oc","No","De")
head(P7);dim(P7)
i=3
for(i in 1:nrow(P7)){
  sub.T2 <- T2[which(T2$CODIGO%in%rownames(P7)[i]),] 
  t1     <- table(sub.T2$MON)[table(sub.T2$MON)>=0] ; t1
  w99    <- as.numeric(names(t1)) ; w99
  P7[i,(ncol(MAT0)+w99)] <- as.numeric(t1)
}
head(P7);dim(P7)
rownames(P7)[17];rownames(MAT0)[17]
colnames(P7)
seas10 <- P7[,which(colnames(P7)%in%"Ja"):ncol(P7)]
head(seas10);dim(seas10)
seas11 <- decostand(seas10,"hellinger")
head(seas11);dim(seas11)

## monthly min and max values of the environmental variables across years:
ME <- YM3 %>% group_by(MON) %>% summarize(IRRA1=min(IRRA),RAIN1=min(RAIN),
                                          TMIN1=min(TMIN),TAVE1=min(TAVE),
                                          TMAX1=min(TMAX),HMIN1=min(HMIN),
                                          HAVE1=min(HAVE),HMAX1=min(HMAX),
                                          IRRA2=max(IRRA),RAIN2=max(RAIN),
                                          TMIN2=max(TMIN),TAVE2=max(TAVE),
                                          TMAX2=max(TMAX),HMIN2=max(HMIN),
                                          HAVE2=max(HAVE),HMAX2=max(HMAX))
ME <- as.data.frame(ME[,-1])
head(ME);dim(ME)


## store values for the kernell densities of parameter estimates:
store.sign <- as.data.frame(matrix(0,nrow=ncol(S),ncol=2))
colnames(store.sign) <- c("red","blue")
rownames(store.sign) <- colnames(Z)
store.sign 

title9 <- c("Irradiance","Rainfall","Tmin","Tave","Tmax","Hmin","Have","Hmax")

par(mfrow=c(1,1))
par(mfrow=c(4,4),mex=0.3)


## LOOP 1 => Showing Venn diagrams and lmg lines:
## => No used anymore, go to LOOP 2 !!
#------------------------------------------------
k=1
i=1
for(k in 1:8){
  µ <- Z[,k] ; l <- L[,k] ; u <- U[,k] ; s <- S[,k] ; t <- title[k] ; col8 <- "black"
  ord <- order(µ)
  µ <- Z[ord,k] ; l <- L[ord,k] ; u <- U[ord,k] ; s <- S[ord,k] ; t <- title[k] ; col8 <- "black"
  seas12   <- seas11[ord,]
  guilds10 <- MAT0$guild2[ord]
  MAT7 <- MAT0[ord,]
  
  adj <- 40 ; adj2 <- 5000
  
  #w1 <- which(abs(s)> 0.1)
  #w2 <- which(abs(s)<=0.1)
  #s[w1] <- 0
  #s[w2] <- 1
  
  range1 <- range(µ) ; r1 <- range1[1] ; r2 <- range1[2] ; r1;r2
  
  plot(µ,c(1:nrow(MAT0)),cex=0.5,xlim=c(r1,r2),ylim=c(-adj,nrow(MAT0)+2),
       ylab="",xlab="", col="white",main=t,fg = "grey70",cex.axis=0.8)
  
  for(j in 1:nrow(MAT0)){
    segments(l[j],j,u[j],j,col="grey95",lwd=2)
  }

  abline(v=0,lty=2,lwd=0.1,col="grey30")
  dens <- stats::density(µ)

  x  <- dens$x
  y0 <- dens$y ; y0 <- y0/sum(y0) ; sum(y0)
  y <- (y0*adj2)-adj
  points(x,y,type="l",col="white")
  polygon(c(x[x>=0], 0), c(y[x>=0], y[x==max(x)]), col=col.alpha("blue",1),border=NA)
  polygon(c(x[x<=0], 0), c(y[x<=0], y[x==max(x)]), col=col.alpha("red",1), border=NA)

  for(i in 1:nrow(MAT0)){
    points(µ[i],i,col="grey70",cex=0.4,pch=16)
  }
  
  for(i in 1:nrow(MAT0)){
    if(s[i]==1){
      if(µ[i]<0){
        points(µ[i],i,col="red",cex=0.8,pch=16)
        store.sign[k,1] = store.sign[k,1]+1
      }
      if(µ[i]>0){
        points(µ[i],i,col="blue",cex=0.8,pch=16)
        store.sign[k,2] = store.sign[k,2]+1
      }
    }
    if(s[i]==1){
      if(µ[i]<0){
        points(µ[i],i,col="red",cex=0.8,pch=16)
        store.sign[k,1] = store.sign[k,1]+1
      }
      if(µ[i]>0){
        points(µ[i],i,col="blue",cex=0.8,pch=16)
        store.sign[k,2] = store.sign[k,2]+1
      }
    }
    if(i==nrow(MAT0)){text(r1*0.75,75, paste0(round(store.sign[k,1]/nrow(MAT0)*100,2),"%"),cex=0.9,col="red")}
    if(i==nrow(MAT0)){text(r2*0.75,75, paste0(round(store.sign[k,2]/nrow(MAT0)*100,2),"%"),cex=0.9,col="blue")}
  }
  
  rem1 <- which(guilds10%in%"Epiphyte") ; rem1
  if(length(rem1)==0){rem1=1}
  if(length(rem1)>0) {rem1=c(1, rem1)}
  
  guilds11 <- to.dummy(guilds10[-rem1],"G") ; head(guilds11) ; dim(guilds11)
  
  seas13 <- seas12[-rem1,]
  pval5 <- anova.cca(rda(µ[-rem1],seas12[-rem1,]))$Pr[1] ; pval5
  if(pval5<=0.05){
      fwd    <- forward.sel(µ[-rem1],seas12[-rem1,],alpha=0.1) # fwd
      seas13 <- seas12[-rem1,fwd[,2]]
      whichmonths <- fwd[,2]
  }

  for(m in 1:ncol(guilds11)){
      w.miss <- which(is.na(guilds11[,m])%in%TRUE)
      guilds11[w.miss,m] <- 0
  }
  
  guilds11 <- guilds11[,colSums(guilds11)>0]

  vp   <- varpart(µ[-rem1],seas13,guilds11) # vp ; plot(vp)
  vals <- round(vp$part$indfract$Adj.R.squared[1:3]*100,2) ; vals[vals<0] <- 0 ; vals
  colfunc1 <- colorRampPalette(c("grey95","grey75","grey55"))
  
  q1    <- quantile(0:10,  probs=seq(from=0.05,to=0.95,by=0.02)) ; q1 ; length(q1)
  cols1 <- colfunc1(length(q1)+1) # plot(1:length(q1),cex=5,pch=15,col=cols1) 
  
  T1 <- vals
  
  col9 <- c()
  for(j in 1:length(T1)){
    L1   <- length(which(q1<=T1[j]))+1  ; L1 
    col9 <- c(col9,cols1[L1])
  }

  plot(-10:10,-10:10,col="white",ylab="",xlab="",col.axis="white",fg = "white")
  bord <- "white"
  if(vals[1]>= 5){ bord="black" }
  draw.ellipse(x=-3,y=3,a=7,b=4,col=col9[1],border=bord)
  draw.ellipse(x=3, y=3,a=7,b=4,col=col9[2],border="white")
  draw.ellipse(x=-3,y=3,a=7,b=4,border=bord)
  
  text(-4,8.5,"Season") ; text(4,8.5,"Guild")
  if(vals[1]< 3){text(-6.5,3,paste0(vals[1],"%"),cex=0.9,col="grey30")}
  if(vals[2]< 3){text(0,3,   paste0(vals[2],"%"),cex=0.9,col="grey30")}
  if(vals[3]< 3){text(6.5,3, paste0(vals[3],"%"),cex=0.9,col="grey30")}
  if(vals[1]>=3){text(-6.5,3,paste0(vals[1],"%"),cex=1.1,col="black")}
  if(vals[2]>=3){text(0,3,   paste0(vals[2],"%"),cex=1.1,col="black")}
  if(vals[3]>=3){text(6.5,3, paste0(vals[3],"%"),cex=1.1,col="black")}
  
  months <-c("J","F","M","A","M","J","J","A","S","O","N","D")
  axis(1, at=seq(-10,10,by=1.7),labels=months, las=0,cex.axis=0.7)
  
  ME1 = as.numeric(decostand(ME[,k],  "standardize")) ; ME1 = ME1+abs(min(ME1)) ; ME1
  ME2 = as.numeric(decostand(ME[,k+8],"standardize")) ; ME2 = ME2+abs(min(ME2)) ; ME2
  shade1 <- rbind(ME1,ME2) ; shade1
  shade( shade1-10 , seq(-10,10,by=1.7) , col="grey80" )
  text(-7,-4,title9[k],col="grey50")
  
  if(vals[1]>=5){
    lmg <- calc.relimp(µ[-rem1],seas12[-rem1,])@lmg*100 ; lmg
    lmg <- as.numeric(decostand(lmg,"standardize")) ; lmg
    lmg <- lmg+abs(min(lmg)) ; lmg <- lmg^1.4 ; lmg
    cor.lmg <- cor(µ[-rem1],seas12[-rem1,])
    col.cor <- rep("black",12)
    pch.cor <- rep(1,12)
    for(n1 in 1:12){
      if(n1%in%whichmonths){
        if(cor.lmg[n1]<0) {col.cor[n1] <- "red"  ; pch.cor[n1] <- 16}
        if(cor.lmg[n1]>=0){col.cor[n1] <- "blue" ; pch.cor[n1] <- 16}
      }
    }
    points(seq(-10,10,by=1.7),lmg-10,type="b",cex=1,col="black",pch=1)
    points(seq(-10,10,by=1.7),lmg-10,cex=1,col=col.cor,pch=pch.cor)
    head(seas12)
    seas12[-rem1,4]
    text(5,-4,"LMG")
  }
  
}

store.sign/nrow(MAT0)*100


#------------------------------------------------------------------------
## LOOP 2 => showing boxplots for guild effects and bars for seasonality:
#------------------------------------------------------------------------
## store values for the kernell densities of parameter estimates:
store.sign <- as.data.frame(matrix(0,nrow=ncol(S),ncol=2))
colnames(store.sign) <- c("red","blue")
rownames(store.sign) <- colnames(Z)
store.sign 

title9 <- c("Irradiance","Rainfall","Tmin","Tave","Tmax","Hmin","Have","Hmax")

## Seasonal month optimum for each species:
seqmo <- seq(min(MAT$SEAS.OPT),max(MAT$SEAS.OPT),by=0.15) ; seqmo
MAT$seasmon <- 1
a1=1
for(a1 in 1:nrow(MAT)){
  MAT$seasmon[a1] <- which.min(abs(MAT$SEAS.OPT[a1]-seqmo))
}

#newred <- "#e44a43" ; plot(5,5,pch=15,cex=5,col=newred)
#newblu <- "#5e8bca" ; plot(5,5,pch=15,cex=5,col=newblu)
newred <- "red" ; plot(5,5,pch=15,cex=5,col=newred)
newblu <- "blue" ; plot(5,5,pch=15,cex=5,col=newblu)

par(mfrow=c(1,1))
par(mfrow=c(4,3),mex=0.5)

## Start loop:
# - - - - - - -
k=1
i=1

for(k in 5:8){
  µ <- Z[,k] ; l <- L[,k] ; u <- U[,k] ; s <- S[,k] ; t <- title[k] ; col8 <- "black"
  ord <- order(µ)
  µ <- Z[ord,k] ; l <- L[ord,k] ; u <- U[ord,k] ; s <- S[ord,k] ; t <- title[k] ; col8 <- "black"
  seas12   <- seas11[ord,]
  guilds10 <- MAT0$guild2[ord]
  MAT7 <- MAT0[ord,]
  
  adj <- 40 ; adj2 <- 5000
  
  #w1 <- which(abs(s)> 0.1)
  #w2 <- which(abs(s)<=0.1)
  #s[w1] <- 0
  #s[w2] <- 1
  
  range1 <- range(µ) ; r1 <- range1[1] ; r2 <- range1[2] ; r1;r2
  
  plot(µ,c(1:nrow(MAT0)),cex=0.5,xlim=c(-1,1),ylim=c(-adj,nrow(MAT0)+2),
        ylab="",xlab="", col="white",fg = "white",cex.axis=0.8,col.axis="white")
  
  perc.neg <- round((length(µ[µ<0])/length(µ))*100,2) ; perc.neg
  perc.pos <- round((length(µ[µ>0])/length(µ))*100,2) ; perc.pos
  
  for(j in 1:nrow(MAT0)){
    segments(l[j],j,u[j],j,col="grey95",lwd=2)
  }
  title(main=t,line=0,cex.main=1.4)
  segments(0,0,0,185,lty=2,col="grey30")
  #abline(v=0,lty=2,lwd=0.1,col="grey30")

  axis(1, at=c(-1,-0.5,0,0.5,1), las=0,cex.axis=0.9,col.axis="grey30")
  axis(2, at=c(0,length(µ)),las=2,cex.axis=0.9,col.axis="grey30")
  title(xlab="Slope", line=3, cex.lab=1.3,col="grey50")
  title(ylab="     Species", line=1.9, cex.lab=1.3)
  
  dens <- stats::density(µ)
  x  <- dens$x
  y0 <- dens$y ; y0 <- y0/sum(y0) ; sum(y0)
  y <- (y0*adj2)-adj
  points(x,y,type="l",col="white")
  polygon(c(x[x>=0], 0), c(y[x>=0], y[x==max(x)]), col=col.alpha(newblu,1),border=NA)
  polygon(c(x[x<=0], 0), c(y[x<=0], y[x==max(x)]), col=col.alpha(newred,1),border=NA)
  
  for(i in 1:nrow(MAT0)){
    points(µ[i],i,col="grey70",cex=0.4,pch=16)
  }
  
  for(i in 1:nrow(MAT0)){
    if(s[i]>=1){
      if(µ[i]<0){
        points(µ[i],i,col=newred,cex=1,pch=16)
        store.sign[k,1] = store.sign[k,1]+1
      }
      if(µ[i]>0){
        points(µ[i],i,col=newblu,cex=1,pch=16)
        store.sign[k,2] = store.sign[k,2]+1
      }
    }
    if(i==nrow(MAT0)){text(r1*0.65,85, paste0(round(store.sign[k,1]/nrow(MAT0)*100,2),"%"),cex=0.95,col=newred)}
    if(i==nrow(MAT0)){text(r2*0.65,65, paste0(round(store.sign[k,2]/nrow(MAT0)*100,2),"%"),cex=0.95,col=newblu)}
  }
  text(r1*0.65,120,paste0(perc.neg,"%"),cex=1.1,col="grey30")
  text(r2*0.65,100,paste0(perc.pos,"%"),cex=1.1,col="grey30")
  
  #rem1 <- which(guilds10%in%"Epiphyte") ; rem1
  #if(length(rem1)==0){rem1=1}
  #if(length(rem1)>0) {rem1=c(1, rem1)}
  
  ## Test guild effect:
  # - - - - - - - - - -
  EXPL <- MAT$SEAS.OPT ; ylab1 <- "coeff" ; xlab1 <- "seas"
  if(k==1){RESP <- MAT$irra ; sign1 <- MAT$irra.sign}
  if(k==2){RESP <- MAT$rain ; sign1 <- MAT$rain.sign}
  if(k==3){RESP <- MAT$tmin ; sign1 <- MAT$tmin.sign}
  if(k==4){RESP <- MAT$tave ; sign1 <- MAT$tave.sign}
  if(k==5){RESP <- MAT$tmax ; sign1 <- MAT$tmax.sign}
  if(k==6){RESP <- MAT$hmin ; sign1 <- MAT$hmin.sign}
  if(k==7){RESP <- MAT$have ; sign1 <- MAT$have.sign}
  if(k==8){RESP <- MAT$hmax ; sign1 <- MAT$hmax.sign}
  
  guilds11 <- as.data.frame(to.dummy(guilds10,"G")) ; head(guilds11) ; dim(guilds11)
  mean.trees.coef <- mean(µ[which(guilds11$G.Tree>0)])    ; col5 <- newred ; if(mean.trees.coef>0){ col5 <- newblu }
  mean.climb.coef <- mean(µ[which(guilds11$G.Climber>0)]) ; col6 <- newred ; if(mean.climb.coef>0){ col6 <- newblu }
  mean.shrub.coef <- mean(µ[which(guilds11$G.Shrub>0)])   ; col7 <- newred ; if(mean.shrub.coef>0){ col7 <- newblu }
  
  low.trees.coef <- quantile(µ[which(guilds11$G.Tree>0)],   probs=c(0.025,0.975))[1]
  low.climb.coef <- quantile(µ[which(guilds11$G.Climber>0)],probs=c(0.025,0.975))[1]
  low.shrub.coef <- quantile(µ[which(guilds11$G.Shrub>0)],  probs=c(0.025,0.975))[1]
  
  ups.trees.coef <- quantile(µ[which(guilds11$G.Tree>0)],   probs=c(0.025,0.975))[2]
  ups.climb.coef <- quantile(µ[which(guilds11$G.Climber>0)],probs=c(0.025,0.975))[2]
  ups.shrub.coef <- quantile(µ[which(guilds11$G.Shrub>0)],  probs=c(0.025,0.975))[2]
  
  wtest.TC <- wilcox.test(µ[which(guilds11$G.Tree>0)],   µ[which(guilds11$G.Climber>0)])
  wtest.CS <- wilcox.test(µ[which(guilds11$G.Climber>0)],µ[which(guilds11$G.Shrub>0)])
  wtest.TS <- wilcox.test(µ[which(guilds11$G.Tree>0)],   µ[which(guilds11$G.Shrub>0)])
  
  pw.TC <- wtest.TC$p.value ; pw.TC
  pw.CS <- wtest.CS$p.value ; pw.CS
  pw.TS <- wtest.TS$p.value ; pw.TS
  
  plot(0:10,0:10,col="white",ylab="",xlab="",col.axis="white",fg = "white",
       ylim=c(-1,1))
  
  wna <- which(is.na(guilds11[,1])) ; wna
  for(b1 in 1:nrow(guilds11)){
    ok=1
    if(b1%in%wna){ok = 0}
    if(ok==1){
    colonne <- which.max(guilds11[b1,])
    if(colnames(guilds11)[colonne]%in%"G.Tree")   { points(3,RESP[b1],cex=0.4,col="grey40",pch=15)}
    if(colnames(guilds11)[colonne]%in%"G.Climber"){ points(6,RESP[b1],cex=0.4,col="grey40",pch=15)}
    if(colnames(guilds11)[colonne]%in%"G.Shrub")  { points(9,RESP[b1],cex=0.4,col="grey40",pch=15)}
    }
  }
  
  bord <- "white"
  col4 <- "black"  
  segments(3,0,9,0,col="grey70",lty=2)
  points(3,mean.trees.coef,pch=1,cex=2.0,col=col5)
  points(6,mean.climb.coef,pch=1,cex=2.0,col=col6)
  points(9,mean.shrub.coef,pch=1,cex=2.0,col=col7)
  #segments(3,low.trees.coef,3,ups.trees.coef,col=col4,lty=1) ; points(3,mean.trees.coef,pch=16,cex=2,col=col5)
  #segments(6,low.climb.coef,6,ups.climb.coef,col=col4,lty=1) ; points(6,mean.climb.coef,pch=16,cex=2,col=col6)
  #segments(9,low.shrub.coef,9,ups.shrub.coef,col=col4,lty=1) ; points(9,mean.shrub.coef,pch=16,cex=2,col=col7)
  axis(1, at=c(3,6,9),labels=c("Tree","Climb.","Shrubs"), las=0,cex.axis=1.4)
  axis(2, at=c(-1,0,1),las=0,cex.axis=1,pos=2)
  text(6,0.9,"Slope ~ Guild (ns)",cex=1.3)
  title(ylab="Slope", line=-4, cex.lab=1.4)
  
  if(pw.TC<=0.05){ 
    points  (2,mean.trees.coef,pch=16,cex=1.5,col="red");points(5,mean.climb.coef,pch=16,cex=1.5,col="red")
    segments(2,mean.trees.coef,5,mean.climb.coef,lty=1,lwd=1,col="red") 
  }
  if(pw.CS<=0.05){ 
    points  (5,mean.climb.coef,pch=16,cex=1.5,col="red");points(8,mean.shrub.coef,pch=16,cex=1.5,col="red")
    segments(5,mean.climb.coef,8,mean.shrub.coef,lty=1,lwd=1,col="red") 
  }
  if(pw.TS<=0.05){ 
    points  (2,mean.trees.coef,pch=16,cex=1.5,col="red");points(8,mean.shrub.coef,pch=16,cex=1.5,col="red")
    segments(2,mean.trees.coef,8,mean.shrub.coef,lty=1,lwd=1,col="red") 
  }
  
  ## Test seasonality effect:
  # - - - - - - - - - - - - -
  neg1  <- which(sign1==1 & RESP<0) ; pos1 <- which(sign1==1 & RESP>0)
  #col20 <- rep("grey80",nrow(MAT)) ; col20[neg1] <- "blue" ; col20[pos1] <- "red"
  
  #par(mfrow=c(1,1),mex=1)
  plot(RESP~EXPL,col="white",xlab="",col.axis="white",fg = "white",ylab="",
        xlim=c(0.5,13.5),ylim=c(-1,1),fg = "white")
  segments(2,0,13,0,col="grey50",lty=2)
  months <-c("J","F","M","A","M","J","J","A","S","O","N","D")
  axis(1, at=c(2:13),labels=months, las=1.1,cex.axis=0.85)
  #axis(2, at=c(-1,0,1),labels=c(-1,0,1), las=0,cex.axis=1)
  title(ylab="Slope", line=2.1, cex.lab=1.4)
  axis(2, at=c(-1,0,1),labels=c(-1,0,1), las=0,cex.axis=1,line=-1.7)
  
  ME1 = as.numeric(decostand(ME[,k],  "standardize")) ; ME1 = ME1+abs(min(ME1)) ; ME1
  ME2 = as.numeric(decostand(ME[,k+8],"standardize")) ; ME2 = ME2+abs(min(ME2)) ; ME2
  shade1 <- rbind(ME1,ME2) 
  shade1[1,] <- shade1[1,]/max(shade1[1,]) ; shade1[1,] <- (shade1[1,]*2)-1 ; shade1[1,]
  shade1[2,] <- shade1[2,]/max(shade1[2,]) ; shade1[2,] <- (shade1[2,]*2)-1 ; shade1[2,]
  shade( shade1 , c(2:13) , col="#FFFF66" )
  text(7,0.95,"Slope ~ Month",cex=1.3)

  test.season1 <- as.data.frame(matrix(0,ncol=12,nrow=2)) 
  for(a1 in 1:(ncol(TS6)-1)){
     points(MAT$seasmon[a1]+1,RESP[a1],cex=0.5,col="grey40",pch=15)
     test.season1[2,MAT$seasmon[a1]] <- test.season1[2,MAT$seasmon[a1]]+1
     if(RESP[a1]<0){test.season1[1,MAT$seasmon[a1]]<-test.season1[1,MAT$seasmon[a1]]+1}
  }
  p.season <- test.season1[1,]/test.season1[2,] ; p.season
  pch.seas <- rep(1,12) ; pch.seas
  w.sign.seas <- which(p.season<0.05 & p.season>0.95)
  if(length(w.sign.seas)>0){pch.seas[w.sign.seas]<-16}
  S0 <- as.data.frame(cbind(RESP,MAT$seasmon))
  colnames(S0) <- c("coeff","seas")
  S1 <- S0 %>% group_by(seas) %>% summarize(ave = mean(coeff)) 
  S1 <- as.data.frame(S1) ; S1$seas <- S1$seas+1
  colseas <- rep(newblu,nrow(S1)) ; colseas[which(S1$ave<0)] <- newred 
  points(S1$ave~S1$seas,col="black",pch=pch.seas,cex=2,type="b")
  points(S1$ave~S1$seas,col=colseas,pch=pch.seas,cex=2)
  
}

store.sign/nrow(MAT0)*100

par(mfrow=c(1,1))


#-----------------------------------------------------------
## Convert acronym species names in TS4 into complete names:
#-----------------------------------------------------------
head(corresp2)
for(j in 1:ncol(TS4)){
  w <- which(corresp2$CODIGO%in%colnames(TS4)[j]) ; w
  if(length(w)>0){colnames(TS4)[j] <- as.character(corresp2$Complete_Name_Jason[w[1]])}
}

head(TS4);dim(TS4)
colnames(TS4)
corresp2$Complete_Name_Jason

## Remove double names in columns (e.g. several Inga.sp occur):
#--------------------------------------------------------------
TS5 <- TS4
w   <- c() ; for(i in 2:ncol(TS4)){ if(colnames(TS4)[i]==colnames(TS4)[i-1]){w<-c(w,i)} } ; w
#colnames(TS4)[w] 
MAT4 <- MAT
if(length(w>0)){ TS5 <- TS4[,-w] ; MAT4 <- MAT[-w,]}
head(TS5);dim(TS5)
head(MAT4);dim(MAT4)
colnames(TS5)[29];rownames(MAT4)[29]

## Prepare data frame for the phylo.maker function:
#--------------------------------------------------
head(T2)
sp_list <- colnames(TS5); ge_list <- c() ; fa_list <- c()
g=1
for(g in 1:length(sp_list)){
  w <- which(as.character(T2$CODIGO)%in%rownames(MAT4)[g])[1]
  ge_list <- c(ge_list,as.character(T2$gen[w]))
  fa_list <- c(fa_list,as.character(T2$fam[w]))
}
sp_list[37];ge_list[37];fa_list[37]

spec <- as.character(sp_list) ; spec <- str_replace(spec, "_", " ")
gens <- as.character(ge_list)  
fams <- as.character(fa_list)  
example <- data.frame(species = spec, genus = gens, family = fams)
head(example);dim(example)
tree <- phylo.maker(example, scenarios=c("S3"))

## Render the tree ultrametric:
#------------------------------
tree2 <- as.phylo(tree$scenario.3)
tree2 <- force.ultrametric(tree2, method=c("nnls"))
is.ultrametric(tree2)

## Remove a few taxa in TS that are not found in the tree and vice versa:
#------------------------------------------------------------------------
w2 = which(tree2$tip.label%in%colnames(TS5)=="FALSE") ; w2
tree3 <- tree2
if(length(w2)>0){tree3 <- drop.tip(tree2,tree2$tip.label[w2])}
w3 = which(tree3$tip.label%in%colnames(TS5)=="FALSE") ; w3

w  = which(colnames(TS5)%in%tree3$tip.label) ; w

TS6 <- TS5[,w] ; MAT5 <- MAT4[w,]
dim(TS6);dim(MAT5);length(tree3$tip.label)
colnames(TS6)[11];rownames(MAT5)[11]

ord <- c() ; for(j in 1:ncol(TS6)){ord<-c(ord,which(colnames(TS6)%in%tree3$tip.label[j]))} ; ord
TS7  <- TS6[,ord]
MAT6 <- MAT5[ord,]
colnames(TS7)[11];rownames(MAT6)[11];tree3$tip.label[11]
dim(TS7);dim(MAT6);length(tree3$tip.label)
head(MAT6)

#tree.NSF<-read.tree(file="phylo.txt") 
tree33 = multi2di(tree3)
tree33 = compute.brlen(tree33)
tree33


#-------------------------------------------------------
## Plot phylogeny with traits with phylo4d in phylobase:
#-------------------------------------------------------
library(ape)
library(adephylo)
library(phylobase)
library(phylosignal)

## Add abundance in each month for each species:
# - - - - - - - - - - - - - - - - - - - - - - - -
P2 <- as.data.frame(cbind(MAT6,1,1,1,1,1,1,1,1,1,1,1,1))
colnames(P2)[(ncol(MAT6)+1):ncol(P2)] <- c("Ja","Fe","Ms","Ap","My","Jn","Jy","Au","Se","Oc","No","De")
head(P2);dim(P2)
i=3
for(i in 1:nrow(P2)){
  sub.T2 <- T2[which(T2$CODIGO%in%rownames(P2)[i]),] 
  t1     <- table(sub.T2$MON)[table(sub.T2$MON)>=0] ; t1
  w99    <- as.numeric(names(t1)) ; w99
  P2[i,(ncol(MAT6)+w99)] <- as.numeric(t1)
}
head(P2);dim(P2)
ncol1 = ncol(P2)

colnames(P2)

## Add abundance in each year for each species:
# - - - - - - - - - - - - - - - - - - - - - - - -
P2 <- as.data.frame(cbind(P2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
colnames(P2)[(ncol1+1):ncol(P2)] <- c(2000:2017)
head(P2);dim(P2)
i=3
for(i in 1:nrow(P2)){
  sub.T2 <- T2[which(T2$CODIGO%in%rownames(P2)[i]),] 
  t1     <- table(sub.T2$YR)[table(sub.T2$YR)>=0] ; t1
  w99    <- as.numeric(names(t1)) ; w99
  w10    <- which(colnames(P2)%in%w99)
  P2[i,w10] <- as.numeric(t1)
}
head(P2);dim(P2)


tree4 <- tree33

par(mfrow=c(1,1))

rownames(P2)<-tree4$tip.label
rownames(P2)[21] ; tree4$tip.label[21]

## Select variables to plot as a color gradient in the phylogeny:
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colnames(P2)
sel <- which(colnames(MAT0)%in%c("irra","rain","tmin","tave","tmax",
                                 "hmin","have","hmax"))
colnames(P2)[sel]

P3 <- P2[which(P2$nyr>=10),] ; head(P3) ; dim(P3)

# if plotting climate effects 
P4 <- P3[,sel] ; head(P4) ; dim(P4)
colnames(P4) <- c("I","R","T1","T2","T3","H1","H2","H3")
P44 <- P4 ## for Pagel's lambda test (see next section)

# if plotting season scores
#P4 <- P2[,which(colnames(P2)%in%"Ja"):(which(colnames(P2)%in%"Ja")+11)]  
#colnames(P4) <- c("J","F","M","A","M","J","J","A","S","O","N","D") ; head(P4) ; dim(P4)

# if plotting year scores
#P4 <- P2[,which(colnames(P2)%in%"2000"):(which(colnames(P2)%in%"2000")+17)] ; head(P4) ; dim(P4) 


## P5 used for the final graph:
P5 <- P4
round(cor(P5),2)
head(P5);dim(P5)

## Normalize variables:
for(i in 1:ncol(P5)){
  lambda1 <- BoxCox.lambda(P5[,i]+abs(min(P5[,i]))+0.0000000001,method = "loglik", lower = -3,  upper = 3) 
  P5[,i]  <- BoxCox(P5[,i], lambda=lambda1)
}
par(mfrow=c(4,3),mex=0.3)
for(j in 1:ncol(P5)){hist(P5[,j],col="blue",main = colnames(P5)[j])}
par(mfrow=c(1,1))
head(P5);dim(P5)

## Standardize variables:
for(j in 1:ncol(P5)){ P5[,j] <- decostand(P5[,j],"standardize") }
par(mfrow=c(4,3),mex=0.3)
for(j in 1:ncol(P5)){hist(P5[,j],col="blue",main = colnames(P5)[j])}
par(mfrow=c(1,1))

head(P5);dim(P5)
#P5 <- abs(P5)

## Quantiles per column (relative abundances):
for(j in 1:ncol(P5)){
  q1  <- quantile(as.numeric(P5[,j]),  probs=seq(from=0.05,to=0.95,by=0.05), na.rm=TRUE) ; q1 ; length(q1)
for(k in 1:nrow(P5)){ 
  L1<-length(which(q1<=as.numeric(P5[k,j])))+1  ; P5[k,j]  <- L1 
}
}

## Quantiles per line (across months to check seasonality in flowering and fruiting for each species):
#j=1
#for(j in 1:nrow(P5)){
#  q1  <- quantile(as.numeric(P5[j,]),  probs=seq(from=0.05,to=0.95,by=0.05)) ; q1 ; length(q1)
#  for(k in 1:ncol(P5)){ 
#    L1<-length(which(q1<=as.numeric(P5[j,k])))+1  ; P5[j,k]  <- L1 
#  }
#}

head(P5);dim(P5)
round(cor(P5),2)

w2 <- which(tree4$tip.label%in%rownames(P5)) ;w2
c(1:length(tree4$tip.label))[-w2]
tree5 <- tree4
tree5 <- drop.tip(tree4,tree4$tip.label[c(1:length(tree4$tip.label))[-w2]])

tree5$tip.label[17] ; rownames(P5)[17]

p4d   <- phylo4d(tree5, P5)
par(mfrow=c(1,1),mex=0.5)

## Choose color palette:
colfunc1 <- colorRampPalette(c("red","grey95","blue"))
colfunc1 <- colorRampPalette(c("grey95","lightblue","blue"))
colfunc1 <- colorRampPalette(c("grey99","grey90","black"))

cols1 <- colfunc1(100) 
plot(1:100,cex=5,pch=15,col=cols1)

gridplot(p4d,tip.cex = 0.3, cell.col=cols1)
#gridplot(p4d,tree.type = "fan",tip.cex = 0.3, show.trait = FALSE, show.tip = FALSE,
#         tree.ratio = 0.7, cell.col=cols1)


## Pagel's lambda test:
#----------------------
library(motmot)
tree5 <- tree4
tree5 <- drop.tip(tree4,tree4$tip.label[c(1:length(tree4$tip.label))[-w2]])
tree5$tip.label[17] ; rownames(P44)[17]

Pagel <- as.data.frame(matrix(0,ncol=2,nrow=8))
rownames(Pagel) <- colnames(P44)
colnames(Pagel) <- c("Lambda","Pval")

for(i in 1:8){
pagel <- phylosig(tree5, P44[,i], method="lambda", test=TRUE, nsim=1000, se=NULL, start=NULL,
             control=list()) ; pagel
Pagel[i,1] <- round(pagel$lambda,3)
Pagel[i,2] <- round(pagel$P,3)
}

Pagel
write.table(Pagel,"Pagel_tests.xls")



