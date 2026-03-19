############################ FULL DATA PREDICTION ##############################

### The analysis took 4 hours to run on a local Linux machine housing an Intel 
### 9900K Processor.

# FRONT MATTER
## Clean up
## rm(list=ls())

# PRELIMINARIES
## Load packages
library(krige)

## Load estimated results and data
load("results/krige.fit2.RData")
predictive<-read.csv('data/krigedPointsFine.csv')

## Clean predictive data
predictive$cathOrth<-predictive$cath+predictive$ortho
predictive$consRelig<-predictive$mormon+predictive$evan
predictive$jewMus<-predictive$jewish+predictive$islam
predictive$black <- as.numeric(predictive$race==2)
predictive$nonBlack <- as.numeric(predictive$race==3)
predictive$unemployed <- as.numeric(predictive$empstat==2)
predictive$notWorkforce <- as.numeric(predictive$empstat==3)

predInput <- subset(predictive, select = c(age, educ, black, nonBlack, female,
                                        cathOrth, consRelig, jewMus, main, rural,
                                        ownership, unemployed, notWorkforce, inc14, 
                                        eastings, northings))

dimnames(predInput)[[2]] <- c("Age", "Education", "African.American", 
                             "Nonwhite.nonblack", "Female", "Catholic.Orthodox", 
                             "Evang.Mormon", "Jewish.Muslim", "Mainline", "Ruralism", 
                             "Homeowner", "Unemployed", "Not.in.workforce","Income",
                             "Eastings", "Northings")

# RUN PREDICTION
## Set seed
set.seed(20200202)

## Index subsets
batch <- data.frame(start = seq(1, nrow(predInput), by = 10000), 
                    end = ifelse(seq(1, nrow(predInput), by = 10000) + 
                                   10000 < nrow(predInput),
                             seq(1, nrow(predInput), by = 10000) + 10000, 
                             nrow(predInput)))

## Create a empty vector to store the predicted results
predicted.ideology <- rep(NA, nrow(predInput))

#begin.time <- Sys.time()
## Run prediction on batches using a loop
for (i in 1:nrow(batch)){
  start <- batch$start[i]
  end <- batch$end[i]
  predicted.ideology[start:end] <-
    predict(fullData.fit2, newdata = predInput[start:end,])
}
#end.time <- Sys.time()
#end.time - begin.time

# RESULTS
## Summary
summary(predictive$pred.ideol)

## Combine predictions with original data and save the file
predictive$pred.ideol <- predicted.ideology
write.csv(predictive, file="results/predictions.csv", row.names=F)

## Compute district-level ideology
predictive$SLDLA<-as.character(predictive$SLDLA)
lowerKriging<-aggregate(x=predictive$pred.ideol, 
                        by=list(STATEA=predictive$STATEA,lower=predictive$SLDLA), 
                        FUN=mean)
colnames(lowerKriging)[3]<-'new.ideol'
lowerNew<-merge(x=lowerCombined,y=lowerKriging,all.x=T, by=c("STATEA","lower"))