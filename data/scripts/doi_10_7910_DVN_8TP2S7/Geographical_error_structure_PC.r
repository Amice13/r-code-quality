
########################################
### Choosing Geographical Structure  ###



datafile<-read.csv("Gridded_dataset_April_2020.csv",header=T)

datafile$X<-datafile$Long
datafile$Y<-datafile$Lat
datafile$SteppeMax<-datafile$maxSteppe
datafile$SteppeMin<-datafile$minSteppe




#######################
##create random sample for fitting spatial structure
NUM=1000
datafile2=datafile[sample(nrow(datafile), NUM), ]  


#######################
#scale the variables in order to calculate standardized coefficients


Total_emp_dens<-scale(datafile2$Total_emp_dens)
allYield<-scale(datafile2$allYield)
agriBest<-scale(datafile2$agriBest*-1)
agriMin<-scale(datafile2$agriMin*-1)
agriMax<-scale(datafile2$agriMax*-1)
elevStd<-scale((log10(datafile2$elevStd+1))*-1)
minSteppe_distance<-scale(datafile2$minSteppe_distance*-1)
maxSteppe_distance<-scale(datafile2$maxSteppe_distance*-1)
FirstEmp_distance<-scale(datafile2$FirstEmp_distance*-1)
X<-datafile2$X
Y<-datafile2$Y



#base analyses on a full model with main effects

OLS1=gls(Total_emp_dens~agriBest+allYield+elevStd+maxSteppe_distance+FirstEmp_distance, method = "ML")


vag<-Variogram(OLS1, form=~X+Y, maxDist=40,resType="n")
plot(vag$variog~vag$dist,ylim=c(0,1.2))
abline(h=1)


#Fit different error structures, parameters of the error structure are not fixed at this point to allow suitable values to be found for subsequent analyses (a linear correlation structure was also explored but not included as the model had problems converging) -starting parameters were chosen after exploratory analyses

# models estimateed without a nugget effect
GLS1=update(OLS1,corr=corSpher(c(2000),form=~X+Y,nugget=F, fixed=F, metric="rdist.earth"),method="ML")
GLS2=update(OLS1,corr=corRatio(c(500),form=~X+Y,nugget=F,fixed=F, metric="rdist.earth"),method="ML")
GLS3=update(OLS1,corr=corGaus(c(500),form=~X+Y,nugget=F,fixed=F, metric="rdist.earth"),method="ML")
GLS4=update(OLS1,corr=corExp(c(2000),form=~X+Y,nugget=F,fixed=F, metric="rdist.earth"),method="ML")


vag1<-Variogram(GLS1, form=~X+Y, maxDist=40,resType="n")
vag2<-Variogram(GLS2, form=~X+Y, maxDist=40,resType="n")
vag3<-Variogram(GLS3, form=~X+Y, maxDist=40,resType="n")
vag4<-Variogram(GLS4, form=~X+Y, maxDist=40,resType="n")


#models estimated with nugget

GLS1_n=update(OLS1,corr=corSpher(c(2000,0.05),form=~X+Y,nugget=T, fixed=F, metric="rdist.earth"),method="ML")
GLS2_n=update(OLS1,corr=corRatio(c(500,0.05),form=~X+Y,nugget=T,fixed=F, metric="rdist.earth"),method="ML")
GLS3_n=update(OLS1,corr=corGaus(c(500,0.05),form=~X+Y,nugget=T,fixed=F, metric="rdist.earth"),method="ML")
GLS4_n=update(OLS1,corr=corExp(c(2000,0.05),form=~X+Y,nugget=T,fixed=F, metric="rdist.earth"),method="ML")

vag_n1<-Variogram(GLS1_n, form=~X+Y, maxDist=40,resType="n")
vag_n2<-Variogram(GLS2_n, form=~X+Y, maxDist=40,resType="n")
vag_n3<-Variogram(GLS3_n, form=~X+Y, maxDist=40,resType="n")
vag_n4<-Variogram(GLS4_n, form=~X+Y, maxDist=40,resType="n")



##############################
##plot the variograms from above

par(mfrow=c(3,2))


#without nugget

plot(vag$variog~vag$dist,ylim=c(0,1.2))
abline(h=1)
plot(vag1$variog~vag1$dist,ylim=c(0,1.2))
abline(h=1)
plot(vag2$variog~vag2$dist,ylim=c(0,1.2))
abline(h=1)
plot(vag3$variog~vag3$dist,ylim=c(0,1.2))
abline(h=1)
plot(vag4$variog~vag4$dist,ylim=c(0,1.2))
abline(h=1)

#with nugget
par(mfrow=c(3,2))

plot(vag$variog~vag$dist,ylim=c(0,1.2))
abline(h=1)
plot(vag_n1$variog~vag_n1$dist,ylim=c(0,1.2))
abline(h=1)
plot(vag_n2$variog~vag_n2$dist,ylim=c(0,1.2))
abline(h=1)
plot(vag_n3$variog~vag_n3$dist,ylim=c(0,1.2))
abline(h=1)
plot(vag_n4$variog~vag_n4$dist,ylim=c(0,1.2))
abline(h=1)



models<-AIC(OLS1,GLS1,GLS2,GLS3,GLS4, GLS1_n,GLS2_n,GLS3_n,GLS4_n)



models

##########
###model GLS2_n tends to fit best and plots show how it reduces auto-correlation in the residuals effectively so cor_Ratio with nugget is selected as the error structure for subsequent analyses
