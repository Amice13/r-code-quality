#--------------------------------------------------------------
#- Filename: geosmin_modeling.R
#- Author: natalie mastin
#- Date: 6/5/24
#- Description: R script for figures and modeling for geosmin dataset
#- Table Info Created: Multiple Linear Regression Model Results, Random Forest Classification Results
#- Figures Created: geosmin_hist.pdf, varimp.pdf
#--------------------------------------------------------------


#-----------------------------------------------------------------------------------
#- [0] load in data and libraries --------------------------------------------------
#-----------------------------------------------------------------------------------
load("geosmin.rda")
library("viridis")
library("caret")
library("randomForest")
library("GGally")
h = 4.5; w = 8.5 #- base height and width for figures


#-----------------------------------------------------------------------------------
#- [1] helper functions ------------------------------------------------------------
#-----------------------------------------------------------------------------------
geosmin$geo_and_wq_time_avg$geo_and_wq_30min$loggeo <- log(geosmin$geo_and_wq_time_avg$geo_and_wq_30min$GeosminEntry)
geosmin$geo_and_wq_time_avg$geo_and_wq_30min$loggeo[!is.finite(geosmin$geo_and_wq_time_avg$geo_and_wq_30min$loggeo)] <- 0

geosmin$geo_and_wq_time_avg$geo_and_wq_1hr$loggeo <- log(geosmin$geo_and_wq_time_avg$geo_and_wq_1hr$GeosminEntry)
geosmin$geo_and_wq_time_avg$geo_and_wq_1hr$loggeo[!is.finite(geosmin$geo_and_wq_time_avg$geo_and_wq_1hr$loggeo)] <- 0

geosmin$geo_and_wq_time_avg$geo_and_wq_2hr$loggeo <- log(geosmin$geo_and_wq_time_avg$geo_and_wq_2hr$GeosminEntry)
geosmin$geo_and_wq_time_avg$geo_and_wq_2hr$loggeo[!is.finite(geosmin$geo_and_wq_time_avg$geo_and_wq_2hr$loggeo)] <- 0

geosmin$geo_and_wq_time_avg$geo_and_wq_12hr$loggeo <- log(geosmin$geo_and_wq_time_avg$geo_and_wq_12hr$GeosminEntry)
geosmin$geo_and_wq_time_avg$geo_and_wq_12hr$loggeo[!is.finite(geosmin$geo_and_wq_time_avg$geo_and_wq_12hr$loggeo)] <- 0

geosmin$geo_and_wq_time_avg$geo_and_wq_24hr$loggeo <- log(geosmin$geo_and_wq_time_avg$geo_and_wq_24hr$GeosminEntry)
geosmin$geo_and_wq_time_avg$geo_and_wq_24hr$loggeo[!is.finite(geosmin$geo_and_wq_time_avg$geo_and_wq_24hr$loggeo)] <- 0

geosmin$geo_all_original_times$measurements$loggeo <- log(geosmin$geo_all_original_times$measurements$GeosminEntry)

geosmin$geosmin$geo_and_wq_time_avg$geo_and_wq_30min$event <- ifelse(geosmin$geo_and_wq_time_avg$geo_and_wq_30min$loggeo>=log(5), "high", "low")
geosmin$geosmin$geo_and_wq_time_avg$geo_and_wq_1hr$event <- ifelse(geosmin$geo_and_wq_time_avg$geo_and_wq_1hr$loggeo>=log(5), "high", "low")
geosmin$geosmin$geo_and_wq_time_avg$geo_and_wq_2hr$event <- ifelse(geosmin$geo_and_wq_time_avg$geo_and_wq_2hr$loggeo>=log(5), "high", "low")
geosmin$geosmin$geo_and_wq_time_avg$geo_and_wq_12hr$event <- ifelse(geosmin$geo_and_wq_time_avg$geo_and_wq_12hr$loggeo>=log(5), "high", "low")
geosmin$geosmin$geo_and_wq_time_avg$geo_and_wq_24hr$event <- ifelse(geosmin$geo_and_wq_time_avg$geo_and_wq_24hr$loggeo>=log(5), "high", "low")

#- multiple linear regression function
create_mlr <- function(modeldata) {
  set.seed(1)
  modeldata$loggeo[!is.finite(modeldata$loggeo)] <- 0
  trainIndex <- createDataPartition(modeldata$event, times = 1, p = 0.75, list = FALSE)
  testIndex <- c(1:nrow(modeldata))[-trainIndex]
  
  mlr <- lm(loggeo ~ Conductivity + DO + pH + Temperature + Turbidity,
            data=modeldata, subset=trainIndex)
  mlr_preds <- predict(mlr, modeldata[testIndex,])
  
  pred_accuracy <- c()
  
  for(i in 1:length(testIndex)) {
    if(modeldata$loggeo[testIndex[i]] < log(5) & mlr_preds[i] < log(5)) {
      pred_accuracy <- c(pred_accuracy, 1)
    }
    else if(modeldata$loggeo[testIndex[i]] > log(5) & mlr_preds[i] > log(5)) {
      pred_accuracy <- c(pred_accuracy, 2)
    }
    else if(modeldata$loggeo[testIndex[i]] < log(5) & mlr_preds[i] > log(5)) {
      pred_accuracy <- c(pred_accuracy, 3)
    }
    else if(modeldata$loggeo[testIndex[i]] > log(5) & mlr_preds[i] < log(5)) {
      pred_accuracy <- c(pred_accuracy, 4)
    }
  }
  
  final_list <- list("full model"=mlr, "predictions"=mlr_preds, "actual"=modeldata$loggeo[testIndex], "accuracy class"=pred_accuracy)
  return(final_list)
}

#- function for accuracy of multiple linear regression
create_accuracy <- function(modellist) {
  length(which(modellist$`accuracy class` == 1))
  length(which(modellist$`accuracy class` == 2))
  length(modellist$`accuracy class`)
  accuracy <- ((length(which(modellist$`accuracy class` == 1)) 
                + length(which(modellist$`accuracy class` == 2)))/
                 length(modellist$`accuracy class`))
  return(accuracy)
}

#- random forest function
create_rf <- function(data) {
  data <- data[,-c(1,2,3,8)]
  data$loggeo[!is.finite(data$loggeo)] <- 0
  data$event[is.na(data$event)] <- "low"
  data$event <- as.factor(data$event)
  # event = ifelse(data$loggeo <= log(5), "Low", "High")
  
  set.seed(1)
  trainIndex <- createDataPartition(data$event, times = 1, p = 0.75, list = FALSE)
  rf.train <- data[trainIndex,]
  rf.test <- data[-trainIndex,]
  event.test <- data$event[-trainIndex]
  
  #rf.train <- sample(1:nrow(data), nrow(data)*.75)
  #rf.test <- data[-rf.train,]
  #event.test <- data$event[-rf.train]
  
  rf.geos.class <- randomForest(event ~ .-loggeo-GeosminEntry, na.exclude(data), 
                                sustain=rf.train, metry=6/3, importance=TRUE, proximity=TRUE)
  
  VI <-importance(rf.geos.class)
  VI <- data.frame(VI)
  # varImpPlot(rf.geos.class, main="Variable Importance")
  rf.pred <- predict(rf.geos.class,newdata=rf.test)
  table_predict <- table(event.test,rf.pred)
  accuracy <- (table_predict[1,1]+table_predict[2,2])/sum(table_predict)
  sensitivity <- (table_predict[1,1])/sum(table_predict[1,])
  specificity <- (table_predict[2,2])/sum(table_predict[2,])
  list_rf <- list("Full Model"=rf.geos.class,
                  "VI"=VI,
                  "accuracy"=accuracy,
                  "sensitivity"=sensitivity,
                  "specificity"=specificity,
                  "table"=table_predict)
  return(list_rf)
}

#-----------------------------------------------------------------------------------
#- [2] visualize geosmin -----------------------------------------------------------
#-----------------------------------------------------------------------------------

#- majority of geosmin observations are from depth == 1
table(geosmin$geo_all_original_times$measurements$Depth)

sum(geosmin$geo_all_original_times$measurements$GeosminEntry>5, na.rm = T)
#- majority of wq occur around depth 5. For now we will use depth == 1
table(round(geosmin$geo_all_original_times$water_quality$Depth))

#- plot histogram of geosmin level
pdf("geosmin_hist.pdf", height = 6, width = 9)
par(mfrow=c(1,2), mai = c(0, 0, 0.45, 0.5), omi = c(0.7, 0.7, 0, 0))
hist(geosmin$geo_all_original_times$measurements$GeosminEntry, breaks = 60, col = "white",
     xlab = "Geosmin (ng/L)",
     main="Histogram of Geosmin Level", axes=F, cex.main=1.25)
axis(1, at = seq(0, 200, by=50))
axis(2, at = seq(0, 600, by=100))
mtext("Frequency", side = 2, line = 2, cex = .95)
mtext("Geosmin (ng/L)", side=1, line=2.5, cex=0.95)
hist(log(geosmin$geo_all_original_times$measurements$GeosminEntry), breaks = 35, col = "white",
     xlab = "Log Geosmin (log(ng/L))",
     main="Histogram of Log Geosmin Level", axes=F, cex.main=1.25)
axis(1, at = seq(-5, 6, by=1))
axis(2, at = seq(0, 80, by=20))
# mtext("Frequency", side = 2, line = 2, cex = .95)
mtext("Log Geosmin (log(ng/L))", side=1, line=2.5, cex=0.95)
dev.off()


#-----------------------------------------------------------------------------------
#- [3] multiple linear regression for geosmin --------------------------------------------------
#-----------------------------------------------------------------------------------

#- multiple linear regression results for each time window
mlr_30min <- create_mlr(geosmin$geo_and_wq_time_avg$geo_and_wq_30min)
summary(mlr_30min$`full model`)
r2_30min <- summary(mlr_30min$`full model`)$r.squared
(acc_30min <- create_accuracy(mlr_30min))

mlr_1hr <- create_mlr(geosmin$geo_and_wq_time_avg$geo_and_wq_1hr)
summary(mlr_1hr$`full model`)
r2_1hr <- summary(mlr_1hr$`full model`)$r.squared
(acc_1hr <- create_accuracy(mlr_1hr))

mlr_2hr <- create_mlr(geosmin$geo_and_wq_time_avg$geo_and_wq_2hr)
summary(mlr_2hr$`full model`)
r2_2hr <- summary(mlr_2hr$`full model`)$r.squared
(acc_2hr <- create_accuracy(mlr_2hr))

mlr_12hr <- create_mlr(geosmin$geo_and_wq_time_avg$geo_and_wq_12hr)
summary(mlr_12hr$`full model`)
r2_12hr <- summary(mlr_12hr$`full model`)$r.squared
(acc_12hr <- create_accuracy(mlr_12hr))

mlr_24hr <- create_mlr(geosmin$geo_and_wq_time_avg$geo_and_wq_24hr)
summary(mlr_24hr$`full model`)
r2_24hr <- summary(mlr_24hr$`full model`)$r.squared
(acc_24hr <- create_accuracy(mlr_24hr))

df_mlr_accuracy <- data.frame(TimeScale=c("30mins", "1hr", "2hr", "12hr", "24hr"), 
                          Accuracy= c(acc_30min, acc_1hr, 
                                      acc_2hr, acc_12hr,
                                      acc_24hr)*100)
df_mlr_rsq <- data.frame(TimeScale=c("30mins", "1hr", "2hr", "12hr", "24hr"), 
                              rsq= c(r2_30min, r2_1hr, 
                                          r2_2hr, r2_12hr,
                                          r2_24hr))
df_mlr_eva <- data.frame(`Multiple R-Squared`=df_mlr_rsq$rsq,
                         Accuracy=df_mlr_accuracy$Accuracy)

rownames(df_mlr_eva) <- c("30mins", "1hr", "2hr", "12hr", "24hr")

#- Table: Multiple Linear Regression Model Results
t(df_mlr_eva)


#-----------------------------------------------------------------------------------
#- [4] random forest classification ------------------------------------------------
#-----------------------------------------------------------------------------------

rf_30min <- create_rf(geosmin$geo_and_wq_time_avg$geo_and_wq_30min)
rf_1hr   <- create_rf(geosmin$geo_and_wq_time_avg$geo_and_wq_1hr)
rf_2hr   <- create_rf(geosmin$geo_and_wq_time_avg$geo_and_wq_2hr)
rf_12hr  <- create_rf(geosmin$geo_and_wq_time_avg$geo_and_wq_12hr)
rf_24hr  <- create_rf(geosmin$geo_and_wq_time_avg$geo_and_wq_24hr)

df_accuracy <- data.frame(TimeScale=c("30mins", "1hr", "2hr", "12hr", "24hr"), 
                            Accuracy= c(rf_30min$accuracy, rf_1hr$accuracy, 
                                        rf_2hr$accuracy, rf_12hr$accuracy,
                                        rf_24hr$accuracy)*100)
df_sensitivity <- data.frame(TimeScale=c("30mins", "1hr", "2hr", "12hr", "24hr"), 
                               Sensitivity= c(rf_30min$sensitivity, rf_1hr$sensitivity, 
                                              rf_2hr$sensitivity, rf_12hr$sensitivity,
                                              rf_24hr$sensitivity)*100)
df_specificity <- data.frame(TimeScale=c("30mins", "1hr", "2hr", "12hr", "24hr"), 
                               Specificity= c(rf_30min$specificity, rf_1hr$specificity, 
                                              rf_2hr$specificity, rf_12hr$specificity,
                                              rf_24hr$specificity)*100)

df_eva <- data.frame(Accuracy=df_accuracy$Accuracy,
                     Specificity=df_specificity$Specificity,
                     Sensitivity=df_sensitivity$Sensitivity)

rownames(df_eva) <- c("30mins", "1hr", "2hr", "12hr", "24hr")

#- Table: Random Forest Classification Results 
t(df_eva)


#- generate variable importance based on mean decrease in accuracy
df_VI_accuracy <- data.frame("30mins"=rf_30min$VI$MeanDecreaseAccuracy,
                               "1hr"= rf_1hr$VI$MeanDecreaseAccuracy, 
                               "2hr"=rf_2hr$VI$MeanDecreaseAccuracy,
                               "12hr"=rf_12hr$VI$MeanDecreaseAccuracy,
                               "24hr"=rf_24hr$VI$MeanDecreaseAccuracy)
colnames(df_VI_accuracy) <- c("30mins", "1hr", "2hr", "12hr", "24hr")
rownames(df_VI_accuracy) <- rownames(rf_30min$VI)
df_VI_accuracy


#- plot variable importance for random forest classification
VIrange <- c(range(df_VI_accuracy))

pdf(file="/Users/LuisdeBonoPaula/Desktop/varimp.pdf", width=w-1, height=h+1)
plot(NULL,
     type="n",
     main=expression("Variable Importance for Random Forest Classifier"),
     xlab="Time Frame",
     ylab="Mean Decrease in Percent Accuracy",
     xlim=c(1,5),
     ylim=range(VIrange),
     xaxt="n",
     cex.main = 1.5,
     cex.lab = 1.05)

VIcolors = turbo(5, end=0.9)
VIcolors[3] = turbo(1, begin=0.35)

for(i in 1:5) {
  lines(1:5, df_VI_accuracy[i,], type="b", pch=19, col = VIcolors[i],lwd=2, cex=1.4, lty=3)
}

axis(1, at=c(1:5), labels=c("±30 min", "±1 hr", "±2 hr", "±12 hr", "±24 hr"))

legend("topleft",
       cex=1.05,
       pt.cex=1.05,
       text.width=1.05,
       legend = rownames(df_VI_accuracy),
       col=VIcolors,
       pch=19,
       x.intersp=0.4,
       y.intersp=0.9)
dev.off()
