#Set working directory 
setwd("C:/...")

#Import Training 70% of Carbon Stock Data (from *.txt file) 
Cstock <- read.table("master_stocks_training70.txt", header = TRUE)
head(Cstock)

#Install required packages for spatial autocorrelation analyses
install.packages("ncf")
library(ncf)
install.packages("spdep")
library(spdep)
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)

#Calculate Spatial autocorrelation for carbon stock values across all habitats
head(Cstock)
attach(Cstock)

SplineCorr_Cstock <- spline.correlog(x=Cstock[,"Easting"],
                                     y=Cstock[,"Northing"],
                                     z=Cstock[,"X30cmStocks_kgha"], resamp = 100)
summary(SplineCorr_Cstock)

ncf:::plot.spline.correlog(SplineCorr_Cstock)

detach(Cstock)

#create a scatterplot - assess relationship between pairs of x y variables
plot(Cstock$X30cmStocks_kgha, Cstock$Ecosystem)
plot(Cstock$X30cmStocks_kgha, Cstock$MeanPopDen)
plot(Cstock$X30cmStocks_kgha, Cstock$SumPopDen)
plot(Cstock$X30cmStocks_kgha, Cstock$Topo)
plot(Cstock$X30cmStocks_kgha, Cstock$HydroEucD)
plot(Cstock$X30cmStocks_kgha, Cstock$CoastEucD)
plot(Cstock$X30cmStocks_kgha, Cstock$X1Urb100)
plot(Cstock$X30cmStocks_kgha, Cstock$X2Prim100)
plot(Cstock$X30cmStocks_kgha, Cstock$X3Nat100)
plot(Cstock$X30cmStocks_kgha, Cstock$Slope)
plot(Cstock$X30cmStocks_kgha, Cstock$DominantSpEVC)
plot(Cstock$X30cmStocks_kgha, Cstock$PrimLith)

#create a correlation matrix and return a correlation and p-value of each pair.

cor.prob <- function(X, dfr = nrow(X)-2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col (R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1-r2)
  R[above] <- 1- pf(Fstat, 1, dfr)
  R[row(R)==col(R)] <- NA
  R
}

flattenSquareMatrix <- function(m) {
  if((class(m)!="matrix")|(nrow(m)!=ncol(m))) stop("Must be a square matrix.")
  if(!identical(rownames(m), colnames(m))) stop("row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i=rownames(m)[row(m)[ut]],
             j=rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

#Open new data table for correlation analysis only - includes no text values for variables. Table made in Excel.
Cstock_cor <- read.table("Cstock_cor_70_2.txt", header = TRUE)
head(Cstock_cor)
#create a flattened square Matrix for the table
corMasterList <- flattenSquareMatrix(cor.prob(Cstock_cor))
dim(corMasterList)
head(corMasterList)

#Look at those variables that are the most correlated with carbon - you can adjust the 
#threshold value of correlation to be included
selectedSub <-subset(corMasterList, (abs(cor) > 0.2 & j =='X30cmstocks_Mgha'))
selectedSub

#To include more variables, do 10% correlation threshold
selectedSub <-subset(corMasterList, (abs(cor) > 0.1 & j =='X30cmstocks_Mgha'))
selectedSub
#checked corMasterList to choose between x's that correlated with one another
#Choose variables >10% correlation and p < 0.05 (and no correlating x's)

#To see all and pick manually which to include: 
selectedSub <-subset(corMasterList, (abs(cor) > 0.0 & j =='X30cmstocks_Mgha'))
selectedSub

#Select those variables that have the desired level of correlation, you have to count the rows down 
#the list in "selectedSub"
bestSub <- as.character(selectedSub$i[c(2,5,6,8,9,10,11,12,13,14,15)])
bestSub
#pairwise plot - first install packages 
install.packages("psych")
library(psych)

#create pairwise plot
pairs.panels(Cstock[c(bestSub, 'X30cmstocks_Mgha')])     #with response variable
pairs.panels(Cstock[c(bestSub)])                        #without response variable

##########

#GLMM Analysis
#Install packages needed
install.packages("lme4")
install.packages("glmmML")
library(lme4)
library(glmmML)

#Scale all continuous variables 
Cstock$Topo.std <- scale(Cstock$Topo)
Cstock$Slope.std <- scale(Cstock$Slope)
Cstock$CoastEucD.std <- scale(Cstock$CoastEucD)
Cstock$HydroEucD.std <- scale(Cstock$HydroEucD)
Cstock$MeanPopDen.std <- scale(Cstock$MeanPopDen)
Cstock$X1Urb100.std <- scale(Cstock$X1Urb100)
Cstock$X2Prim100.std <- scale(Cstock$X2Prim100)
Cstock$X3Nat100.std <- scale(Cstock$X3Nat100)

#verify that the standardized variables are now included
head(Cstock)

#############################################

#Develop 12 "global model" GLMMs for comparison (compare variable influence and AICc); excludes correlated x variables

glmer1 <- glmer(X30cmstocks_Mgha ~ factor(Ecosystem) + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + MeanPopDen.std + (1|Site),
                data = Cstock ,family = (Gamma))


summary(glmer1)

# check predicted v. real
predicted_values <- predict(glmer1)
plot(Cstock$X30cmStocks_kgha, predicted_values)

##############################################################

#glmer2 
glmer2 <- glmer(X30cmstocks_Mgha ~ factor(DominantSpEVC_codes) + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + MeanPopDen.std + (1|Site),
                data = Cstock, family = (Gamma))

summary(glmer2)

# check predicted v. real
predicted_values <- predict(glmer2)
plot(Cstock$X30cmStocks_kgha, predicted_values)

#############################################################

#glmer3
glmer3 <- glmer(X30cmstocks_Mgha ~ Topo.std + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + MeanPopDen.std + (1|Site),
                data = Cstock, family = (Gamma))

summary(glmer3)

# check predicted v. real
predicted_values <- predict(glmer3)
plot(Cstock$X30cmStocks_kgha, predicted_values)

############################################################

#glmer4
glmer4 <- glmer(X30cmstocks_Mgha ~ factor(Ecosystem) + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + X1Urb100.std + (1|Site),
                data = Cstock, family = (Gamma))

summary(glmer4)

# check predicted v. real
predicted_values <- predict(glmer4)
plot(Cstock$X30cmStocks_kgha, predicted_values)

############################################################

#glmer5 
glmer5 <- glmer(X30cmstocks_Mgha ~ factor(DominantSpEVC_codes) + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + X1Urb100.std + (1|Site),
                data = Cstock, family = (Gamma))

summary(glmer5)

# check predicted v. real
predicted_values <- predict(glmer5)
plot(Cstock$X30cmStocks_kgha, predicted_values)


###########################################################

#glmer6 
glmer6 <- glmer(X30cmstocks_Mgha ~ Topo.std + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + X1Urb100.std + (1|Site),
                data = Cstock, family = (Gamma))

summary(glmer6)

# check predicted v. real
predicted_values <- predict(glmer6)
plot(Cstock$X30cmStocks_kgha, predicted_values)

###########################################################

#glmer7 
glmer7 <- glmer(X30cmstocks_Mgha ~ factor(Ecosystem) + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + X2Prim100.std + (1|Site),
                data = Cstock, family = (Gamma))

summary(glmer7)

# check predicted v. real
predicted_values <- predict(glmer7)
plot(Cstock$X30cmStocks_kgha, predicted_values)

############################################################

#glmer8 
glmer8 <- glmer(X30cmstocks_Mgha ~ factor(DominantSpEVC_codes) + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + X2Prim100.std + (1|Site),
                data = Cstock, family = (Gamma))

summary(glmer8)

# check predicted v. real
predicted_values <- predict(glmer8)
plot(Cstock$X30cmStocks_kgha, predicted_values)

############################################################

#glmer9 
glmer9 <- glmer(X30cmstocks_Mgha ~ Topo.std + Slope.std + CoastEucD.std 
                + HydroEucD.std + factor(PrimLith)
                + X2Prim100.std + (1|Site),
                data = Cstock, family = (Gamma))

summary(glmer9)

# check predicted v. real
predicted_values <- predict(glmer9)
plot(Cstock$X30cmStocks_kgha, predicted_values)

############################################################

#glmer10 
glmer10 <- glmer(X30cmstocks_Mgha ~ factor(Ecosystem) + Slope.std + CoastEucD.std 
                 + HydroEucD.std + factor(PrimLith)
                 + X3Nat100.std + (1|Site),
                 data = Cstock, family = (Gamma))

summary(glmer10)

# check predicted v. real
predicted_values <- predict(glmer10)
plot(Cstock$X30cmStocks_kgha, predicted_values)

############################################################

#glmer11 
glmer11 <- glmer(X30cmstocks_Mgha ~ factor(DominantSpEVC_codes) + Slope.std + CoastEucD.std 
                 + HydroEucD.std + factor(PrimLith)
                 + X3Nat100.std + (1|Site),
                 data = Cstock, family = (Gamma))

summary(glmer11)

# check predicted v. real
predicted_values <- predict(glmer11)
plot(Cstock$X30cmStocks_kgha, predicted_values)

############################################################

#glmer12 
glmer12 <- glmer(X30cmstocks_Mgha ~ Topo.std + Slope.std + CoastEucD.std 
                 + HydroEucD.std + factor(PrimLith)
                 + X3Nat100.std + (1|Site),
                 data = Cstock, family = (Gamma))

summary(glmer12)

# check predicted v. real
predicted_values <- predict(glmer12)
plot(Cstock$X30cmStocks_kgha, predicted_values)

##########################################################

#Next:
# 1. Compare 12 global models (each with 6 fixed predictor variables and site as random factor)
# to determine which collinear covariate is most linked to C stocks from each of the two
# groups of correlating variables - "plant community" (ecosystem, domsp/evc, and topo) and
# "human impact" (mean pop dens, or 3 variables of proportion of catchment land use - 
# X1Urb100, X2Prim100, X3Nat100)

# 2. When identified model(s) with most important predictors, run dredge on those. For each
# set of models (from full model), average models for delta AICc <2. 

#3. Then compare predictive accuracy of averaged models
# using cross validation - i.e. testing their predictive power on the remaining 30% of data. 

# 1. AICc of 12 global models
install.packages("AICcmodavg")
library(AICcmodavg)

#generate list of models
my.models <- list(glmer1, glmer2, glmer3, glmer4, glmer5, glmer6, glmer7,
                  glmer8, glmer9, glmer10, glmer11, glmer12)

#test it worked:
my.models

#make list of model names
my.modelnames <- c("glmer1", "glmer2", "glmer3", "glmer4", "glmer5", "glmer6", "glmer7",
                   "glmer8", "glmer9", "glmer10", "glmer11", "glmer12")

#test it worked:
my.modelnames

#generate AICc values and sort models into order of AIC value
#"second.ord = true" means AICc rather than AIC. "sort = true" means all models
aictab(my.models, my.modelnames, second.ord = TRUE, sort = TRUE)

#Model selection based on AICc:

#            K      AICc  Delta_AICc AICcWt Cum.Wt      LL
#   glmer11  30   1823.61       0.00   0.70   0.70 -876.27
#   glmer5   30   1826.39       2.79   0.17   0.87 -877.66
#   glmer2   30   1828.21       4.60   0.07   0.94 -878.57
#   glmer8   30   1828.65       5.04   0.06   1.00 -878.79
#   glmer10  24   1858.48      34.87   0.00   1.00 -901.79
#   glmer4   24   1860.99      37.38   0.00   1.00 -903.05
#   glmer1   24   1864.31      40.71   0.00   1.00 -904.71
#   glmer7   24   1864.75      41.14   0.00   1.00 -904.93
#   glmer12  23   1886.26      62.65   0.00   1.00 -916.98
#   glmer6   23   1893.33      69.72   0.00   1.00 -920.51
#   glmer9   23   1895.62      72.01   0.00   1.00 -921.66
#   glmer3   23   1895.97      72.37   0.00   1.00 -921.83

#Top four models separate out as much better than the rest (delta AICc ~5 or less, then 
#jumps up to >30 for all other models). These top four models all include 
#DominantSpEVC_codes as the "plant community" variable. The difference in the "human
#impacts" relationship to C stocks is not as clear (there is one of each in top 4 models).

#2. Now we will dredge each of the top four models. Along with dredging, we will identify
#the best models from the dredge based on delta AICc<2 and average these to create an averaged
#model to represent each of these top four models. These averaged models will then go on to be tested
#for how well they predict C values of the 30% of data we set aside.

#To dredge best models:
install.packages("MuMIn")
library(MuMIn)
options(na.action = na.fail)

#Dredge model glmer11
dredge.glmer11<-dredge(glmer11)

#see Model selection table of all models 
dredge.glmer11

#see summary of range of values for fixed factors across all models
summary(dredge.glmer11)

#"get.models" fxn generates a list of the fitted models.
#"model.avg" fxn does a model averaged based on AICc. 
#"subset=TRUE" makes the fxn calculate the average model using all models
summary(model.avg(get.models(dredge.glmer11, subset = TRUE)))

#BUT to do this with only the models that have delta AICc<2, use "subset=delta<2"
summary(model.avg(get.models(dredge.glmer11, subset = delta<2)))

#See which models are which
get.models(dredge.glmer11, subset = delta<2)

#To generate confidence intervals for full averaged parameters,
#first name the averaged model (while generating it)
modelavg.glmer11 <- model.avg(get.models(dredge.glmer11, subset = delta<2))

#then generate 95% confidence intervals for the averaged model using that name;
# the full = T condition ensures CI are for the full averages (rather than subset averages):
confint(modelavg.glmer11, full=T)

######################

#Dredge second best model:
dredge.glmer5<-dredge(glmer5)

#rate models created by dredge with AICc and model average based on delta AICc<2:
summary(model.avg(get.models(dredge.glmer5, subset = delta<2)))

#See which models are which
dredge.glmer5

#To generate confidence intervals for full averaged parameters,
#first name the averaged model (while generating it)
modelavg.glmer5 <- model.avg(get.models(dredge.glmer5, subset = delta<2))

#then generate 95% confidence intervals for the averaged model using that name;
# the full = T condition ensures CI are for the full averages (rather than subset averages):
confint(modelavg.glmer5, full=T)

#############################

#Dredge third best model:
dredge.glmer2<-dredge(glmer2)

#rate models created by dredge with AICc and model average based on delta AICc<2:
summary(model.avg(get.models(dredge.glmer2, subset = delta<2)))

#To generate confidence intervals for full averaged parameters,
#first name the averaged model (while generating it)
modelavg.glmer2 <- model.avg(get.models(dredge.glmer2, subset = delta<2))

#then generate 95% confidence intervals for the averaged model using that name;
# the full = T condition ensures CI are for the full averages (rather than subset averages):
confint(modelavg.glmer2, full=T)

#to see the dredge table
View(dredge.glmer2)
############################

#Dredge fourth best model:
dredge.glmer8<-dredge(glmer8)

#rate models created by dredge with AICc and model average based on delta AICc<2:
summary(model.avg(get.models(dredge.glmer8, subset = delta<2)))

#To generate confidence intervals for full averaged parameters,
#first name the averaged model (while generating it)
modelavg.glmer8 <- model.avg(get.models(dredge.glmer8, subset = delta<2))

#then generate 95% confidence intervals for the averaged model using that name;
# the full = T condition ensures CI are for the full averages (rather than subset averages):
confint(modelavg.glmer8, full=T)

##############################################################################

#Now, dredge & model average next 4 best models - i.e. the ones that include 
#ECOSYSTEM as the ecological variable rather than dominantSp/EVC: glmers 10, 4, 1, & 7
#These are the averaged models that will be compared then best will be used to model shallow sediment C stock
#distribution across VIC

#Dredge model glmer10
dredge.glmer10<-dredge(glmer10)

#rate models created by dredge with AICc and model average based on delta AICc<2:
summary(model.avg(get.models(dredge.glmer10, subset = delta<2)))

#To generate confidence intervals for full averaged parameters,
#first name the averaged model (while generating it)
modelavg.glmer10 <- model.avg(get.models(dredge.glmer10, subset = delta<2))

#then generate 95% confidence intervals for the averaged model using that name;
# the full = T condition ensures CI are for the full averages (rather than subset averages):
confint(modelavg.glmer10, full=T)


#######

#Dredge model glmer4
dredge.glmer4<-dredge(glmer4)

#rate models created by dredge with AICc and model average based on delta AICc<2:
summary(model.avg(get.models(dredge.glmer4, subset = delta<2)))

#To generate confidence intervals for full averaged parameters,
#first name the averaged model (while generating it)
modelavg.glmer4 <- model.avg(get.models(dredge.glmer4, subset = delta<2))

#then generate 95% confidence intervals for the averaged model using that name;
# the full = T condition ensures CI are for the full averages (rather than subset averages):
confint(modelavg.glmer4, full=T)


#######

#Dredge model glmer1
dredge.glmer1<-dredge(glmer1)

#rate models created by dredge with AICc and model average based on delta AICc<2:
summary(model.avg(get.models(dredge.glmer1, subset = delta<2)))

#To generate confidence intervals for full averaged parameters,
#first name the averaged model (while generating it)
modelavg.glmer1 <- model.avg(get.models(dredge.glmer1, subset = delta<2))

#then generate 95% confidence intervals for the averaged model using that name;
# the full = T condition ensures CI are for the full averages (rather than subset averages):
confint(modelavg.glmer1, full=T)


#######

#Dredge model glmer7
dredge.glmer7<-dredge(glmer7)

#rate models created by dredge with AICc and model average based on delta AICc<2:
summary(model.avg(get.models(dredge.glmer7, subset = delta<2)))

#To generate confidence intervals for full averaged parameters,
#first name the averaged model (while generating it)
modelavg.glmer7 <- model.avg(get.models(dredge.glmer7, subset = delta<2))

#then generate 95% confidence intervals for the averaged model using that name;
# the full = T condition ensures CI are for the full averages (rather than subset averages):
confint(modelavg.glmer7, full=T)


##############################################################################################
#Save the averaged models to disk

#save averaged model of glmer11
saveRDS(modelavg.glmer11, "C:/...modelavg.glmer11")

#save averaged model of glmer5
saveRDS(modelavg.glmer5, "C:/...modelavg.glmer5")

#save averaged model of glmer2
saveRDS(modelavg.glmer2, "C:/...modelavg.glmer2")

#save averaged model of glmer10
saveRDS(modelavg.glmer10, "C:/...modelavg.glmer10")

#save averaged model of glmer4
saveRDS(modelavg.glmer4, "C:/...modelavg.glmer4")

#save averaged model of glmer1
saveRDS(modelavg.glmer1, "C:/...modelavg.glmer1")

#save averaged model of glmer7
saveRDS(modelavg.glmer7, "C:/...modelavg.glmer7")


######
#Models saved. If come back later do the following:
install.packages("MuMIn")
library(MuMIn)
#Load the model modelavg.glmer11
finalmod11<-readRDS("C:/...modelavg.glmer11")
#Check the model 
print(finalmod11)

#Load the model modelavg.glmer5
finalmod5<-readRDS("C:/...modelavg.glmer5")
#See the model
print(finalmod5)

#Load the model modelavg.glmer2
finalmod2<-readRDS("C:/...modelavg.glmer2")
#See the model
print(finalmod2)

#Load the model modelavg.glmer10
finalmod10<-readRDS("C:/...modelavg.glmer10")
#See the model
print(finalmod10)

#Load the model modelavg.glmer4
finalmod4<-readRDS("C:/...modelavg.glmer4")
#See the model
print(finalmod4)

#Load the model modelavg.glmer1
finalmod1<-readRDS("C:/...modelavg.glmer1")
#See the model
print(finalmod1)

#Load the model modelavg.glmer7
finalmod7<-readRDS("C:/...modelavg.glmer7")
#See the model
print(finalmod7)

#################################################################################

# 3. Now make predictions for the 30% evaluation dataset to test models

#bring in the new dataset
Eval30 <- read.table("C:/.../master_stocks_evaluating30.txt",
                     header = TRUE)

#Scale continuous variables to match training data

Eval30$Topo.std <- scale(Eval30$Topo)
Eval30$Slope.std <- scale(Eval30$Slope)
Eval30$CoastEucD.std <- scale(Eval30$CoastEucD)
Eval30$HydroEucD.std <- scale(Eval30$HydroEucD)
Eval30$MeanPopDen.std <- scale(Eval30$MeanPopDen)
Eval30$X1Urb100.std <- scale(Eval30$X1Urb100)
Eval30$X2Prim100.std <- scale(Eval30$X2Prim100)
Eval30$X3Nat100.std <- scale(Eval30$X3Nat100)

head(Eval30)

#predict carbon from model and input predictions into a new column in the table labelled #"pred"

#averaged model for glmer11

#Use top code if calling model from disk (i.e. if don't want to run all previous code)
pred11=predict(finalmod11, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred11=pred11

#or run it from modelavg.glmer11 if ran all code above
pred11=predict(modelavg.glmer11, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred11=pred11

#Check new column is there ("pred")
head(Eval30)

#Look at the accuracy of the predictions using a linear model

g <- lm(X30cmstocks_Mgha ~ pred11, data = Eval30)

plot(g) 

summary(g)

#########

#averaged model for glmer5

#Use top code if calling model from disk (i.e. if don't want to run all previous code)
pred5=predict(finalmod5, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred5=pred5

#or run it from modelavg.glmer11 if ran all code above
pred5=predict(modelavg.glmer5, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred5=pred5

#Check new column is there ("pred")
head(Eval30)

#Look at the accuracy of the predictions using a linear model

g <- lm(X30cmstocks_Mgha ~ pred5, data = Eval30)

plot(g) 

summary(g)

#########

#averaged model for glmer2

#Use top code if calling model from disk (i.e. if don't want to run all previous code)
pred2=predict(finalmod2, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred2=pred2

#or run it from modelavg.glmer11 if ran all code above
pred2=predict(modelavg.glmer2, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred2=pred2

#Check new column is there ("pred")
head(Eval30)

#Look at the accuracy of the predictions using a linear model

g <- lm(X30cmstocks_Mgha ~ pred2, data = Eval30)

plot(g) 

summary(g)

#########

#averaged model for glmer10

#Use top code if calling model from disk (i.e. if don't want to run all previous code)
pred10=predict(finalmod10, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred10=pred10

#or run it from modelavg.glmer11 if ran all code above
pred10=predict(modelavg.glmer10, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred10=pred10

#Check new column is there ("pred")
head(Eval30)

#Look at the accuracy of the predictions using a linear model

g <- lm(X30cmstocks_Mgha ~ pred10, data = Eval30)

plot(g) 

summary(g)

#########

#averaged model for glmer4

#Use top code if calling model from disk (i.e. if don't want to run all previous code)
pred4=predict(finalmod4, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred4=pred4

#or run it from modelavg.glmer11 if ran all code above
pred4=predict(modelavg.glmer4, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred4=pred4

#Check new column is there ("pred")
head(Eval30)

#Look at the accuracy of the predictions using a linear model

g <- lm(X30cmstocks_Mgha ~ pred4, data = Eval30)

plot(g) 

summary(g)

#########

#averaged model for glmer1

#Use top code if calling model from disk (i.e. if don't want to run all previous code)
pred1=predict(finalmod1, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred1=pred1

#or run it from modelavg.glmer11 if ran all code above
pred1=predict(modelavg.glmer1, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred1=pred1

#Check new column is there ("pred")
head(Eval30)

#To list all predictions
pred1
#resulted in table of 86 predictions. Compare this to the 30% set aside.

#Look at the accuracy of the predictions using a linear model

g <- lm(X30cmstocks_Mgha ~ pred1, data = Eval30)


plot(g) 

summary(g)

#########

#averaged model for glmer7

#Use top code if calling model from disk (i.e. if don't want to run all previous code)
pred7=predict(finalmod7, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred7=pred7

#or run it from modelavg.glmer11 if ran all code above
pred7=predict(modelavg.glmer7, newdata = Eval30, type = c("response"), allow.new.levels = TRUE)
Eval30$pred7=pred7

#Check new column is there ("pred")
head(Eval30)

#Look at the accuracy of the predictions using a linear model

g <- lm(X30cmstocks_Mgha ~ pred7, data = Eval30)

plot(g) 

summary(g)

#################
#Plot predicted vs. actual C stock value from each model

#finalmod11
plot(X30cmstocks_Mgha ~ pred11, data = Eval30, xlab="Predicted", ylab="Actual",
     xlim=c(0,300), ylim=c(0,300))
abline(a=0, b=1, col="Black")
text(220,50, "Adj R-sq=0.4868", pos = 1)

#finalmod5
plot(X30cmstocks_Mgha ~ pred5, data = Eval30, xlab="Predicted", ylab="Actual",
     xlim=c(0,300), ylim=c(0,300))
abline(a=0, b=1, col="Black")
text(220,50, "Adj R-sq=0.4829", pos = 1)

#finalmod2
plot(X30cmstocks_Mgha ~ pred2, data = Eval30, xlab="Predicted", ylab="Actual",
     xlim=c(0,300), ylim=c(0,300))
abline(a=0, b=1, col="Black")
text(220,50, "Adj R-sq=0.4881", pos = 1)

#finalmod10
plot(X30cmstocks_Mgha ~ pred10, data = Eval30, xlab="Predicted", ylab="Actual",
     xlim=c(0,300), ylim=c(0,300))
abline(a=0, b=1, col="Black")
text(220,50, "Adj R-sq=0.4514", pos = 1)

#finalmod4
plot(X30cmstocks_Mgha ~ pred4, data = Eval30, xlab="Predicted", ylab="Actual",
     xlim=c(0,300), ylim=c(0,300))
abline(a=0, b=1, col="Black")
text(220,50, "Adj R-sq=0.4465", pos = 1)

#finalmod1
plot(X30cmstocks_Mgha ~ pred1, data = Eval30, xlab="Predicted", ylab="Actual",
     xlim=c(0,300), ylim=c(0,300))
abline(a=0, b=1, col="Black")
text(220,50, "Adj R-sq=0.4566", pos = 1)

#finalmod7
plot(X30cmstocks_Mgha ~ pred7, data = Eval30, xlab="Predicted", ylab="Actual",
     xlim=c(0,300), ylim=c(0,300))
abline(a=0, b=1, col="Black")
text(220,50, "Adj R-sq=0.4618", pos = 1)

##############################################################################
###############################################################################

#Generating predictions using the raster package

#Load packages needed
install.packages("rgdal")
library(rgdal)
install.packages("raster")
library(raster)
install.packages("lme4")
library(lme4)
install.packages("MuMIn")
library(MuMIn)

#Prediction of 30-cm deep sediment C stocks using best averaged model that uses ECOSYSTEM as the ecological variable (averaged model 7).

#Set working directory for rasters extracted to mask of current "ecosystems"
setwd("C:/...")

#see a list of the rasters in the working directory
ascii.list <- list.files(pattern = "*.asc")
ascii.list 

#Bring asciis in as rasters and check their dimensions, pixel size, etc 
#to make sure they line up:
Ecosystem <- raster("eco123_vg94_2.asc")
CoastEucD <- raster("coast_d_ce.asc")
Slope <- raster("slope10_ce_2.asc")

#See info about rasters
Ecosystem
CoastEucD
Slope

#see map plot of rasters 
plot(Ecosystem)
plot(CoastEucD)
plot(Slope)

#scale continuous rasters to match scaled variables used to generate models

CoastEucD.std <- scale(CoastEucD, center = TRUE, scale = TRUE)
#Check the scaled raster dimensions etc.
CoastEucD.std

#Scale slope
Slope.std <- scale(Slope, center = TRUE, scale = TRUE)
#Check scaled raster of slope
Slope.std

#write scaled rasters to disk
CoastEucD.std <- writeRaster(CoastEucD.std, filename="coastce_std.asc", datatype='FLT4S')
Slope.std <- writeRaster(Slope.std, filename="slopece2_std.asc", datatype='FLT4S')

#Read in files with new names in new location.
#library(raster)
#setwd("C:/...")

Ecosystem <- raster("eco123_vg94_2.asc")
CoastEucD.std <- raster("coastce_std.asc")
Slope.std <- raster("slopece2_std.asc")

#Check the rasters look right
plot(Ecosystem)
plot(CoastEucD.std)
plot(Slope.std)

#Load the model modelavg.glmer7, which performed best of the "ecosystem" models when averaged models were validated.
finalmod7<-readRDS("C:/.../modelavg.glmer7")
#See the model
print(finalmod7)

#If trouble recalling saved model, regenerate it with earlier code, then call it with modelavg.glmer7
print(modelavg.glmer7)

#Check the name of each raster and rename it to match the model variables
names(Ecosystem)                           
names(Ecosystem)<-'Ecosystem'
names(Ecosystem)

names(CoastEucD.std)                        
names(CoastEucD.std)<-'CoastEucD.std'
names(CoastEucD.std)                        

names(Slope.std)                            
names(Slope.std)<- 'Slope.std'
names(Slope.std)                            

#create a raster list of the scaled (and categorical) rasters
raster.list <- as.list(Ecosystem, CoastEucD.std, Slope.std)
raster.list

#create stack of rasters
raster.stack <- stack(raster.list)
#check it
raster.stack

#Create prediction grid

raster::predict(raster.stack, model = finalmod7, filename='CurEcoC', fun = predict, format='GTiff', re.form = ~0, progress='text', type='response')
CurEcoC<-raster("CurEcoC.tif")
plot(CurEcoC)

### !!!NOTE: units of predicted shallow sediment C stocks need converting:
### original model C is in units of Mg C/ha. Resolution of predictions map is 10 x 10 m cells (i.e. 100m2), 
### so to sum shallow sediment C stocks in an area, need to convert from Mg C/ha to Mg C/100m2 (i.e. divide C values by 100). 

#####################################
# END

