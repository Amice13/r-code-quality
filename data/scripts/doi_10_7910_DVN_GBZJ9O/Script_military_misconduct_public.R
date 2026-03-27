### -----------------------------------
##
## Script name: Military misconduct
##
## Author: Juan D. Gelvez
## Date created: May 1st, 2024
## Updated: March 14, 2025
## Email: j.gelvez@umd.edu
## Email: juan.gelvezf@gmail.com
## ------------------------------------

# clear all the space objects
rm(list=ls())

# Load up the packages

library(tidyverse)
library(readxl)
library(ggplot2)
library(skimr)
library(writexl)
library(haven)
library(glmnet)
library(caret)
library(caretEnsemble)
library(rpart)
library(mlbench)
library(partykit)
library(strucchange)
library(magrittr)
library(ranger)
library(party)
library(SuperLearner)
library(pROC)
library(xgboost)
library(nnet)
library(MLmetrics)
library(pROC)
library(gridExtra)
library(vip)


# setwd("-")


################################### COLOMBIA ################################

##### I use three main datasets for the "Falsos Positivos" in Colombia 
#Acemoglu, et al. (2020) Replication package for: The Perils of High-Powered Incentives: Evidence from Colombia’s False Positives. American Economic Association [publisher], 2020. https://doi.org/10.3886/E111542V1
#From Acemoglu et a, use the database "FPMunyear_falsepositives_cleaned.dta"

#I also use Gelvez, Juan; Johnson, Marcus, 2023, "Replication Data for: “Los nadies y las nadies”: The effect of peace-building on political behavior in Colombia", https://doi.org/10.7910/DVN/ASO8IB, Harvard Dataverse, V1
#In this script refered as "controles_col"

fp <- read_dta("./FPMunyear_falsepositives_cleaned.dta")
controles_col <- read_excel("data/false_positives/losnadies.xlsx")
controles_col <- controles_col %>%
  distinct(codmpio, .keep_all = TRUE)

# Create a new dataset with selected variables using subset()

fp_subset0 <- subset(fp, select = c(fpd, fp_ln, M_code, justice_percent, rank_w_any, atgue, atpar, atgov, rainfall,
                                   aptitud, erosion, discapital, agua, altura, areasuphe,
                                   poptotal2000, matematicas, lenguaje, ciencias, tributario2000, nbi2000,
                                   Infanteria, total_unemployment, 
                                   peinglec_pc, coca_ph, protest9599, justice_percent9599,
                                   year))

sapply(fp_subset0, function(x) sum(is.na(x))) # there are some NA in land quality (aptitud, erosion, agua), and violence (guerrilla-paramilitaries attacks), but putting them together is less than 7% of the dataset so I decided to work with completed cases
fp_subset0 <- na.omit(fp_subset0)


fp_subset <- left_join(fp_subset0, controles_col, by = c("M_code" = "codmpio"))
names(fp_subset)
sapply(fp_subset, function(x) sum(is.na(x))) # there are some NA in land quality (aptitud, erosion, agua), and violence (guerrilla-paramilitaries attacks), but putting them together is less than 7% of the dataset so I decided to work with completed cases
fp_subset <- na.omit(fp_subset)


#fpd as a factor
fp_subset$fpd <- as.factor(fp_subset$fpd)
fp_subset$fpd <- factor(fp_subset$fpd, levels = c(0, 1), labels = c("No_FalsePositive", "FalsePositive"))

# Check the new structure to ensure changes
str(fp_subset$fpd)#variable fpd is indicator of false positives
head(fp_subset)


fp_subset <- subset(fp_subset, select = c(-M_code))
names(fp_subset)

# Train-test split
set.seed(1293)
inTrain <- createDataPartition(fp_subset$fpd, 
                               p = .8, 
                               list = FALSE, 
                               times = 1)
fp_train <- fp_subset[inTrain,]
fp_test <- fp_subset[-inTrain,]

# CV
cv_folds_fp <- createFolds(fp_train$fpd, returnTrain = TRUE)

# Train control
ctrl_fp <- trainControl(method = "cv",
                        number = 10,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE,  # This needs to be true for ROC metric
                        index = cv_folds_fp,
                        savePredictions = 'final')

#xgboost model

xgboost_model_fp <- train(fpd ~ .-fp_ln,
                          data = fp_train,
                          trControl = ctrl_fp,
                          method = 'xgbTree',
                          metric = 'ROC')

xgboost_model_fp
plot(xgboost_model_fp)

#Neural Network	

neural_model_fp <- train(fpd ~ .-fp_ln,
                         data = fp_train,
                         trControl = ctrl_fp,
                         method = 'nnet',
                         metric = 'ROC')

neural_model_fp
plot(neural_model_fp)

#random forests
ranger_model_fp <- train(fpd ~.-fp_ln,
                         data = fp_train,
                         trControl = ctrl_fp,
                         method = 'ranger',
                         metric = 'ROC')
ranger_model_fp
plot(ranger_model_fp)

#glmnet forests LASSO
lasso_fp <- train(fpd ~.-fp_ln,
                  data = fp_train,
                  trControl = ctrl_fp,
                  method = 'glmnet',
                  metric = 'ROC')
lasso_fp
summary(lasso_fp)


plot(lasso_fp)

mods_fp <- c('glmnet','xgbTree','nnet','ranger')

model_list_fp <- caretList(fpd ~.-fp_ln,
                           data = fp_train,
                           trControl = ctrl_fp,
                           metric = 'ROC',
                           methodList = mods_fp)
model_list_fp

model_list_fp_Accuracy <- caretList(fpd ~.-fp_ln,
                                    data = fp_train,
                                    trControl = ctrl_fp,
                                    metric = 'Accuracy',
                                    methodList = mods_fp)
model_list_fp_Accuracy


plot(model_list_fp$glmnet)
plot(model_list_fp$xgbTree)
plot(model_list_fp$nnet)
plot(model_list_fp$ranger)

summary(resamples(model_list_fp))

resamp_results <- resamples(model_list_fp)
dotplot_res <- dotplot(resamp_results, metric = 'ROC')
dotplot_res
dotplot_res <- update(dotplot_res,
                      main = "ROC Performance of Models",
                      xlab = "ROC Score",
                      ylab = "Model Names", 
                      scales = list(y = list(labels = c("Neural Networks", "Lasso", "Random Forest", "XGBoost"))),
                      auto.key = list(space = "right", lines = TRUE, points = TRUE),)

print(dotplot_res)




as.data.frame(predict(model_list_fp, newdata = head(fp_train)))
modelCor(resamples(model_list_fp))



# Take the list of models and create meta-ensemble
level2ctrl_fp <- trainControl(method = "cv",
                              number = 10,
                              savePredictions = "final",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

glm_ensemble_fp <- caretStack(model_list_fp,
                              method = "glm",
                              metric = "ROC",
                              trControl = level2ctrl_fp)
glm_ensemble_fp
coef(glm_ensemble_fp$ens_model$finalModel)
summary(glm_ensemble_fp)

#ensemble sin neural networks 
mods_fp_sinnnet <- c('glmnet','xgbTree','ranger')

model_list_fp_sinnnet <- caretList(fpd ~.-fp_ln,
                           data = fp_train,
                           trControl = ctrl_fp,
                           metric = 'ROC',
                           methodList = mods_fp_sinnnet)
model_list_fp_sinnnet

glm_ensemble_fp_sinnet <- caretStack(model_list_fp_sinnnet,
                              method = "glm",
                              metric = "ROC",
                              trControl = level2ctrl_fp)
glm_ensemble_fp_sinnet
coef(glm_ensemble_fp_sinnet$ens_model$finalModel)
summary(glm_ensemble_fp_sinnet)

# Prediction
#ranger_model,knn_model, neural_model,xgboost_model,tree_model,glm_model

p_ranger_fp <- predict(ranger_model_fp, newdata = fp_test, type = "prob")
p_neural_fp <- predict(neural_model_fp, newdata = fp_test, type = "prob")
p_xgboost_fp <- predict(xgboost_model_fp, newdata = fp_test, type = "prob")
p_glmet_fp <- predict(lasso_fp, newdata = fp_test, type = "prob")

p_ensemble_fp <- predict(glm_ensemble_fp, newdata = fp_test, type = "prob")
p_ensemble_fp_sinnet <- predict(glm_ensemble_fp_sinnet, newdata = fp_test, type = "prob")

###############
# ROC
ranger_roc <- roc(fp_test$fpd, p_ranger_fp$FalsePositive)
neural_roc <- roc(fp_test$fpd, p_neural_fp$FalsePositive)
xgboost_roc <- roc(fp_test$fpd, p_xgboost_fp$FalsePositive)
lasso_roc <- roc(fp_test$fpd, p_glmet_fp$FalsePositive)
ensemble_roc <- roc(fp_test$fpd, p_ensemble_fp)
ensemble_roc_sinnet <- roc(fp_test$fpd, p_ensemble_fp_sinnet)

ggroc(list(Ranger = ranger_roc,
           Lasso = lasso_roc,
           #Neural = neural_roc,
           XGBoost=xgboost_roc,
           SuperLearner = ensemble_roc_sinnet)) +
  geom_line(size = 1.1) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
               color="darkgrey", linetype="dashed")
######## altering thresholds
############ Adjust the Classification Threshold to improve specificity
#### XGBoost model
proba_xgboost_fp <- p_xgboost_fp$FalsePositive  
roc_xgboost_fp <- roc(fp_test$fpd, proba_xgboost_fp) # ROC curve for XGBoost model
coords_xgboost_fp <- coords(roc_xgboost_fp, "best", best.method=c("closest.topleft"))
optimal_threshold_xgboost_fp <- coords_xgboost_fp$threshold
print(paste("Optimal threshold for XGBoost: ", optimal_threshold_xgboost_fp))
new_predictions_xgboost_fp <- ifelse(proba_xgboost_fp > optimal_threshold_xgboost_fp, "FalsePositive", "No_FalsePositive")
new_predictions_xgboost_fp <- factor(new_predictions_xgboost_fp)
conf_matrix_xgboost_fp <- confusionMatrix(new_predictions_xgboost_fp, fp_test$fpd)
print(conf_matrix_xgboost_fp)

#### Ranger model
proba_ranger_fp <- p_ranger_fp$FalsePositive
roc_ranger_fp <- roc(fp_test$fpd, proba_ranger_fp)
coords_ranger_fp <- coords(roc_ranger_fp, "best", best.method=c("youden", "closest.topleft"))
optimal_threshold_ranger_fp <- coords_ranger_fp$threshold
print(paste("Optimal threshold for Ranger: ", optimal_threshold_ranger_fp))
new_predictions_ranger_fp <- ifelse(proba_ranger_fp > optimal_threshold_ranger_fp, "FalsePositive", "No_FalsePositive")
new_predictions_ranger_fp <- factor(new_predictions_ranger_fp, levels = levels(fp_test$fpd))
conf_matrix_ranger_fp <- confusionMatrix(new_predictions_ranger_fp, fp_test$fpd)
print(conf_matrix_ranger_fp)

# Neural model
proba_neural_fp <- p_neural_fp$FalsePositive
roc_neural_fp <- roc(fp_test$fpd, proba_neural_fp)
coords_neural_fp <- coords(roc_neural_fp, "best", best.method=c("youden","closest.topleft"))
optimal_threshold_neural_fp <- coords_neural_fp$threshold
print(paste("Optimal threshold for Neural N: ", optimal_threshold_neural_fp))

new_predictions_neural_fp <- ifelse(proba_neural_fp > optimal_threshold_neural_fp, "FalsePositive", "No_FalsePositive")
new_predictions_neural_fp <- factor(new_predictions_neural_fp, levels = levels(fp_test$fpd))
conf_matrix_neural_fp <- confusionMatrix(new_predictions_neural_fp, fp_test$fpd)
print(conf_matrix_neural_fp)

# Lasso model (glmnet)
proba_lasso_fp <- p_glmet_fp$FalsePositive
roc_lasso_fp <- roc(fp_test$fpd, proba_lasso_fp)
coords_lasso_fp <- coords(roc_lasso_fp, "best",best.method=c("closest.topleft"))
optimal_threshold_lasso_fp <- coords_lasso_fp$threshold
print(paste("Optimal threshold for Lasso ", optimal_threshold_lasso_fp))

new_predictions_lasso_fp <- ifelse(proba_lasso_fp > optimal_threshold_lasso_fp, "FalsePositive", "No_FalsePositive")
new_predictions_lasso_fp <- factor(new_predictions_lasso_fp, levels = levels(fp_test$fpd))
conf_matrix_lasso_fp <- confusionMatrix(new_predictions_lasso_fp, fp_test$fpd)
print(conf_matrix_lasso_fp)

# Ensemble
roc_ensemble_fp <- roc(fp_test$fpd, p_ensemble_fp)
coords_ensemble_fp <- coords(roc_ensemble_fp, "best", best.method=c("youden"))
coords_ensemble_fp
optimal_threshold_ensemble_fp <- coords_ensemble_fp$threshold
print(paste("Optimal threshold for ensemble ", optimal_threshold_ensemble_fp))
new_predictions_ensemble_fp <- ifelse(p_ensemble_fp > optimal_threshold_ensemble_fp, "FalsePositive", "No_FalsePositive")
new_predictions_ensemble_fp <- factor(new_predictions_ensemble_fp, levels = c("FalsePositive", "No_FalsePositive"))
conf_matrix_ensemble_fp <- confusionMatrix(new_predictions_ensemble_fp, fp_test$fpd)
print(conf_matrix_ensemble_fp)

# Ensemble sin nnet
roc_ensemble_f_sinnet <- roc(fp_test$fpd, p_ensemble_fp_sinnet)
coords_ensemble_fp_sinnet <- coords(roc_ensemble_f_sinnet, "best", best.method=c("youden"))
coords_ensemble_fp_sinnet
optimal_threshold_ensemble_fp_sinnet <- coords_ensemble_fp_sinnet$threshold
print(paste("Optimal threshold for ensemble ", optimal_threshold_ensemble_fp_sinnet))
new_predictions_ensemble_fp_sinnet <- ifelse(p_ensemble_fp_sinnet > optimal_threshold_ensemble_fp_sinnet, "FalsePositive", "No_FalsePositive")
new_predictions_ensemble_fp_sinnet <- factor(new_predictions_ensemble_fp_sinnet, levels = c("FalsePositive", "No_FalsePositive"))
conf_matrix_ensemble_fp_sinnet <- confusionMatrix(new_predictions_ensemble_fp_sinnet, fp_test$fpd)
print(conf_matrix_ensemble_fp_sinnet)


#########feature importance


# Calculate feature importance using varImp
importance_xgb_fp <- varImp(xgboost_model_fp, scale = TRUE)
print(importance_xgb_fp)
plot(importance_xgb_fp)

#names(fp_subset)
# Define variables in each category
# Define variables in each category based on your list
socioeconomic_vars_fp <- c( "matematicas", "lenguaje", "ciencias","poptotal2000",
                            "tributario2000", "nbi2000", "total_unemployment",  
                            "slave_ratio_norm", "minorias", "pob_ind", "minimum_dist_mines", "dummyminas",  
                             "indrural" )
            
political_vars_fp <- c("justice_percent", "justice_percent9599", "protest9599","peinglec_pc")

military_vars_fp <- c("Infanteria", "coca_ph", "atgue", "atpar", "atgov", "conflicto_1901_1931","rank_w_any", "violencia_48_a_53")

geographic_vars_fp <- c("rainfall", "aptitud", "erosion", "discapital", "agua", "altura",  
                        "temperature", "disbogota","distancia_mercado","areasuphe",
                        "gcaribe","gpacifica", "gorinoquia","gamazonia","gandina")



# Add a category column to your feature importance DataFrame
importance_df_fp <- as.data.frame(importance_xgb_fp$importance)

importance_df_fp$category <- ifelse(rownames(importance_df_fp) %in% socioeconomic_vars_fp, 'Socioeconomic & Demographic',
                                    ifelse(rownames(importance_df_fp) %in% political_vars_fp, 'Political & Institutional',
                                           ifelse(rownames(importance_df_fp) %in% military_vars_fp, 'Military & Security',
                                                  ifelse(rownames(importance_df_fp) %in% geographic_vars_fp, 'Geographic & Environmental', 'Year'))))

# View the categorized DataFrame
print(importance_df_fp)

# Sum the importance by category
importance_by_category_fp <- aggregate(importance_df_fp$Overall, by=list(importance_df_fp$category), FUN=sum)
colnames(importance_by_category_fp) <- c('Category', 'Total_Importance_XGB')

# View the importance by category
print(importance_by_category_fp)

# Plot the importance by category
ggplot(importance_by_category_fp, aes(x = Category, y = Total_Importance_XGB, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +  # Adjust bar width
  theme_minimal() +
  labs(title = "Total Feature Importance by Category (Colombian Data - XGBoost Model)",
       x = "Category", y = "Total Importance") 
  #+scale_fill_brewer(palette = "Set2")


# Now with ensemble 
importance_ensemble_fp <- vip::vi_permute(
  object = glm_ensemble_fp_sinnet, 
  feature_names = colnames(fp_train), 
  train = fp_train,
  target = fp_train$fpd,
  metric = "accuracy", 
  pred_wrapper = predict
)

# Convert to data frame
importance_df_ensemble_fp <- as.data.frame(importance_ensemble_fp)

importance_df_ensemble_fp$category <- ifelse(importance_df_ensemble_fp$Variable %in% socioeconomic_vars_fp, 'Socioeconomic & Demographic',
                                             ifelse(importance_df_ensemble_fp$Variable %in% political_vars_fp, 'Political & Institutional',
                                                    ifelse(importance_df_ensemble_fp$Variable %in% military_vars_fp, 'Military & Security',
                                                           ifelse(importance_df_ensemble_fp$Variable %in% geographic_vars_fp, 'Geographic & Environmental', 'Year'))))

# View the categorized importance DataFrame
print(importance_df_ensemble_fp)

# Sum the importance for each category
importance_by_category_ensemble_fp <- aggregate(importance_df_ensemble_fp$Importance, 
                                                by = list(importance_df_ensemble_fp$category), 
                                                FUN = sum)
colnames(importance_by_category_ensemble_fp) <- c('Category', 'Total_Importance_Ensemble')

# View the importance by category
print(importance_by_category_ensemble_fp)

# Plot the importance by category
ggplot(importance_by_category_ensemble_fp, aes(x = Category, y = Total_Importance_Ensemble, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Total Feature Importance by Category (Colombian Data - Ensemble Model)",
       x = "Category", y = "Total Importance") +
  scale_fill_brewer(palette = "Set3")


importance_combined_fp <- merge(importance_by_category_fp, importance_by_category_ensemble_fp, by = "Category", all = TRUE)


category_colors <- c("Geographic & Environmental" = "#4D4D4D", 
                     "Socioeconomic & Demographic" = "#7F7F7F", 
                     "Year" = "#A6A6A6", 
                     "Military & Security" = "#CCCCCC", 
                     "Political & Institutional" = "#E6E6E6")  # Grayscale shades reordered
importance_combined_fp$Category <- factor(importance_combined_fp$Category, 
                                          levels = c("Geographic & Environmental", 
                                                     "Socioeconomic & Demographic", 
                                                     "Year", 
                                                     "Military & Security", 
                                                     "Political & Institutional"))

# XGBoost Plot
p1 <- ggplot(importance_combined_fp, aes(x = Category, y = Total_Importance_XGB, fill = Category)) +
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = category_colors) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),  # Remove category labels from x-axis
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "XGBoost Model Feature Importance",
       x = NULL, y = "Total Importance")  # Remove "Category" from labels

# Ensemble Plot
p2 <- ggplot(importance_combined_fp, aes(x = Category, y = Total_Importance_Ensemble, fill = Category)) +
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = category_colors) +  # Color legend for this plot
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),  # Remove category labels from x-axis
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Ensemble Model Feature Importance",
       x = NULL, y = "Total Importance")  # Remove "Category" from labels

# Arrange the two plots side by side
grid.arrange(p1, p2, ncol = 2)


################################### MEXICO #########################
#The main data "mex" comes from Flores-Macías, Gustavo; Zarkin, Jessica, 2023, "Replication Data for: The Consequences of Militarized Policing for Human Rights: Evidence from Mexico", https://doi.org/10.7910/DVN/HB2CCD, Harvard Dataverse, V1
#I also use "controles_mex" from Angulo, J. C. (2023). Green gold: Avocado production and conflict in Mexico.

#Municipality-level analysis: Mexico
mex <- read_dta("C:/Users/juang/Dropbox/Papers/Military misconduct/data/mexico/ForMatching.dta")
controles_mex <- read_dta("C:/Users/juang/Dropbox/Papers/Military misconduct/data/mexico/mexico_characteristics_angulo.dta")


# I create a new dataset with selected variables using subset()

mex_subset0 <- subset(mex, select = c(inegi, year, operativo, subsetSECFORCESpc, 
                                     yearsop, lnpob, rural, pob15a29pc, pobh15a44pc, 
                                     thom, samepartypres, samegovpres, isal, ieduc, iing, gradorezagosoc2010num))
head(mex_subset0)
mex_subset0$inegi <- as.character(mex_subset0$inegi)
mex_subset0$inegi <- sub("^0+", "", mex_subset0$inegi) #Remove leading zeros from inegi
view(mex_subset0)

controles_mex <- subset(controles_mex, select = c(cvemunicipio, soil, altitude, water, 
                                                  prov1, prov2, prov3, prov4, prov5, prov6, 
                                                  prov7, prov8, prov9, prov10, prov11, prov12, 
                                                  prov13, prov14, prov15, prov16))
view(controles_mex)
controles_mex$cvemunicipio <- as.character(controles_mex$cvemunicipio)

#mergin datasets

mex_subset <- left_join(mex_subset0, controles_mex, by = c("inegi" = "cvemunicipio"))
view(mex_subset)
mex_subset <- subset(mex_subset, select = c(-inegi))

# Removing NAs
sapply(mex_subset, function(x) sum(is.na(x))) 
mex_subset <- na.omit(mex_subset)

# Convert outcome variable 'subsetSECFORCESpc' to a factor
mex_subset <- mex_subset %>%
  mutate( subsetSECFORCESpc = if_else(subsetSECFORCESpc > 0, 1, 0)
  )


mex_subset$subsetSECFORCESpc <- as.factor(mex_subset$subsetSECFORCESpc)
mex_subset$subsetSECFORCESpc <- factor(mex_subset$subsetSECFORCESpc, levels = c(0, 1), labels = c("No_Misconduct", "Misconduct"))

# Train-test split
set.seed(1293)
inTrain_mex <- createDataPartition(mex_subset$subsetSECFORCESpc, 
                                   p = .8, 
                                   list = FALSE, 
                                   times = 1)
mex_train <- mex_subset[inTrain_mex,]
mex_test <- mex_subset[-inTrain_mex,]

# Cross-validation folds
cv_folds_mex <- createFolds(mex_train$subsetSECFORCESpc, returnTrain = TRUE)

# Training control setup
ctrl_mex <- trainControl(method = "cv",
                         number = 10,
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE, 
                         index = cv_folds_mex,
                         savePredictions = 'final')

# XGBoost model for Mexico
xgboost_model_mex <- train(subsetSECFORCESpc ~ .,
                           data = mex_train,
                           trControl = ctrl_mex,
                           method = 'xgbTree',
                           metric = 'ROC')


# Neural Network model
neural_model_mex <- train(subsetSECFORCESpc ~ .,
                          data = mex_train,
                          trControl = ctrl_mex,
                          method = 'nnet',
                          metric = 'ROC')


# Random Forest model
ranger_model_mex <- train(subsetSECFORCESpc ~ .,
                          data = mex_train,
                          trControl = ctrl_mex,
                          method = 'ranger',
                          metric = 'ROC')


# LASSO model using glmnet
lasso_mex <- train(subsetSECFORCESpc ~ .,
                   data = mex_train,
                   trControl = ctrl_mex,
                   method = 'glmnet',
                   metric = 'ROC')


# Aggregate the models in a list
mods_mex <- c('glmnet', 'xgbTree', 'nnet', 'ranger')
model_list_mex <- caretList(subsetSECFORCESpc ~ .,
                            data = mex_train,
                            trControl = ctrl_mex,
                            metric = 'ROC',
                            methodList = mods_mex)

#now with accuracy
model_list_mex_Accuracy <- caretList(subsetSECFORCESpc ~ .,
                                    data = mex_train,
                                    trControl = ctrl_mex,
                                    metric = 'Accuracy',
                                    methodList = mods_mex)




plot(model_list_mex$glmnet)
plot(model_list_mex$xgbTree)
plot(model_list_mex$nnet)
plot(model_list_mex$ranger)

# Compare models
summary(resamples(model_list_mex))
dotplot(resamples(model_list_mex), metric = 'ROC')
dotplot(resamples(model_list_mex), metric = 'Spec')
dotplot(resamples(model_list_mex), metric = 'Sens')




as.data.frame(predict(model_list_mex, newdata = head(mex_train)))
modelCor(resamples(model_list_mex))



# Take the list of models and create meta-ensemble
level2ctrl_mex <- trainControl(method = "cv",
                              number = 10,
                              savePredictions = "final",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

glm_ensemble_mex <- caretStack(model_list_mex,
                              method = "glm",
                              metric = "ROC",
                              trControl = level2ctrl_mex)
glm_ensemble_mex
coef(glm_ensemble_mex$ens_model$finalModel)
summary(glm_ensemble_mex)

# Prediction
#ranger_model,knn_model, neural_model,xgboost_model,tree_model,glm_model

p_ranger_mex <- predict(ranger_model_mex, newdata = mex_test, type = "prob")
p_neural_mex <- predict(neural_model_mex, newdata = mex_test, type = "prob")
p_xgboost_mex <- predict(xgboost_model_mex, newdata = mex_test, type = "prob")
p_glmet_mex <- predict(lasso_mex, newdata = mex_test, type = "prob")

p_ensemble_mex <- predict(glm_ensemble_mex, newdata = mex_test, type = "prob")
summary(p_ensemble_mex)

###############
# ROC
#ranger_roc <- roc(fp_test$fpd, p_ranger_fp$FalsePositive)

ranger_roc_mex <- roc(mex_test$subsetSECFORCESpc, p_ranger_mex$Misconduct)
neural_roc_mex <- roc(mex_test$subsetSECFORCESpc, p_neural_mex$Misconduct)
xgboost_roc_mex <- roc(mex_test$subsetSECFORCESpc, p_xgboost_mex$Misconduct)
lasso_roc_mex <- roc(mex_test$subsetSECFORCESpc, p_glmet_mex$Misconduct)
ensemble_roc_mex <- roc(mex_test$subsetSECFORCESpc, p_ensemble_mex)
summary(ensemble_roc_mex)

ggroc(list(Ranger = ranger_roc_mex,
           Lasso = lasso_roc_mex,
           Neural = neural_roc_mex,
           XGBoost=xgboost_roc_mex,
           SuperLearner = ensemble_roc_mex)) +
  geom_line(size = 1.1) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
               color="darkgrey", linetype="dashed")


############ Adjust the Classification Threshold to improve specificity
#### XGBoost model
proba_xgboost_mex <- p_xgboost_mex$Misconduct  
roc_xgboost_mex <- roc(mex_test$subsetSECFORCESpc, proba_xgboost_mex) # ROC curve for XGBoost model
coords_xgboost_mex <- coords(roc_xgboost_mex, "best", best.method=c("closest.topleft"))
coords_xgboost_mex
optimal_threshold_xgboost_mex <- coords_xgboost_mex$threshold
print(paste("Optimal threshold for XGBoost: ", optimal_threshold_xgboost_mex))
new_predictions_xgboost_mex <- ifelse(proba_xgboost_mex > optimal_threshold_xgboost_mex, "Misconduct", "No_Misconduct")
new_predictions_xgboost_mex <- factor(new_predictions_xgboost_mex, levels = levels(mex_test$subsetSECFORCESpc))
conf_matrix_xgboost_mex <- confusionMatrix(new_predictions_xgboost_mex, mex_test$subsetSECFORCESpc)
print(conf_matrix_xgboost_mex)

#### Ranger model
proba_ranger_mex <- p_ranger_mex$Misconduct
roc_ranger_mex <- roc(mex_test$subsetSECFORCESpc, proba_ranger_mex)
coords_ranger_mex <- coords(roc_ranger_mex, "best", best.method=c("youden", "closest.topleft"))
coords_ranger_mex
optimal_threshold_ranger_mex <- coords_ranger_mex$threshold
print(paste("Optimal threshold for Ranger: ", optimal_threshold_ranger_mex))
new_predictions_ranger_mex <- ifelse(proba_ranger_mex > optimal_threshold_ranger_mex, "Misconduct", "No_Misconduct")
new_predictions_ranger_mex <- factor(new_predictions_ranger_mex, levels = levels(mex_test$subsetSECFORCESpc))
conf_matrix_ranger_mex <- confusionMatrix(new_predictions_ranger_mex, mex_test$subsetSECFORCESpc)
print(conf_matrix_ranger_mex)

# Neural model
proba_neural_mex <- p_neural_mex$Misconduct
roc_neural_mex <- roc(mex_test$subsetSECFORCESpc, proba_neural_mex)
coords_neural_mex <- coords(roc_neural_mex, "best", best.method=c("youden","closest.topleft"))
optimal_threshold_neural_mex <- coords_neural_mex$threshold
print(paste("Optimal threshold for Neural N: ", optimal_threshold_neural_mex))

new_predictions_neural_mex <- ifelse(proba_neural_mex > optimal_threshold_neural_mex, "Misconduct", "No_Misconduct")
new_predictions_neural_mex <- factor(new_predictions_neural_mex, levels = levels(mex_test$subsetSECFORCESpc))
conf_matrix_neural_mex <- confusionMatrix(new_predictions_neural_mex, mex_test$subsetSECFORCESpc)
print(conf_matrix_neural_mex)

# Lasso model (glmnet)
proba_lasso_mex <- p_glmet_mex$Misconduct
roc_lasso_mex <- roc(mex_test$subsetSECFORCESpc, proba_lasso_mex)
coords_lasso_mex <- coords(roc_lasso_mex, "best",best.method=c("closest.topleft"))
optimal_threshold_lasso_mex <- coords_lasso_mex$threshold
print(paste("Optimal threshold for Lasso: ", optimal_threshold_lasso_mex))

new_predictions_lasso_mex <- ifelse(proba_lasso_mex > optimal_threshold_lasso_mex, "Misconduct", "No_Misconduct")
new_predictions_lasso_mex <- factor(new_predictions_lasso_mex, levels = levels(mex_test$subsetSECFORCESpc))
conf_matrix_lasso_mex <- confusionMatrix(new_predictions_lasso_mex, mex_test$subsetSECFORCESpc)
print(conf_matrix_lasso_mex)

# Ensemble model
roc_ensemble_mex <- roc(mex_test$subsetSECFORCESpc, p_ensemble_mex)
coords_ensemble_mex <- coords(roc_ensemble_mex, "best", best.method=c("youden"))
coords_ensemble_mex
optimal_threshold_ensemble_mex <- coords_ensemble_mex$threshold
print(paste("Optimal threshold for ensemble: ", optimal_threshold_ensemble_mex))

new_predictions_ensemble_mex <- ifelse(p_ensemble_mex > optimal_threshold_ensemble_mex, "Misconduct", "No_Misconduct")
new_predictions_ensemble_mex <- factor(new_predictions_ensemble_mex, levels = levels(mex_test$subsetSECFORCESpc))
conf_matrix_ensemble_mex <- confusionMatrix(new_predictions_ensemble_mex, mex_test$subsetSECFORCESpc)
print(conf_matrix_ensemble_mex)



######################## Feature importance#################################
###############MEXICO###########
# Calculate feature importance for XGBoost
importance_xgb <- varImp(xgboost_model_mex, scale = TRUE)
print(importance_xgb)
plot(importance_xgb)

# Define variables in each category
socioeconomic_vars <- c("lnpob","gradorezagosoc2010num", "pob15a29pc", "pobh15a44pc","isal", "ieduc", "iing")

political_vars <- c("samepartypres", "samegovpres")

military_vars <- c("operativo", "yearsop", "thom")

geographic_vars <- c("rural", "soil", "altitude", "water", "prov1", "prov2", "prov3", "prov4", "prov5", "prov6",
                     "prov7", "prov8", "prov9", "prov10", "prov11", "prov12",
                     "prov13", "prov14", "prov15", "prov16")

#Other is year

# Add a category column to your feature importance DataFrame
importance_df_xgb <- as.data.frame(importance_xgb$importance)
importance_df_xgb$category <- ifelse(rownames(importance_df_xgb) %in% socioeconomic_vars, 'Socioeconomic & Demographic',
                                     ifelse(rownames(importance_df_xgb) %in% political_vars, 'Political & Institutional',
                                            ifelse(rownames(importance_df_xgb) %in% military_vars, 'Military & Security',
                                                   ifelse(rownames(importance_df_xgb) %in% geographic_vars, 'Geographic & Environmental', 'Year'))))

# Sum the importance by category
importance_by_category_xgb <- aggregate(importance_df_xgb$Overall, by=list(importance_df_xgb$category), FUN=sum)
colnames(importance_by_category_xgb) <- c('Category', 'Total_Importance_XGB')

# Permutation importance for Ensemble (Superlearner)
importance_ensemble <- vi_permute(object = glm_ensemble_mex, 
                                  feature_names = colnames(mex_train), 
                                  train = mex_train,
                                  target = mex_train$subsetSECFORCESpc,
                                  metric = "accuracy", 
                                  pred_wrapper = predict)

# Convert to data frame and categorize
importance_df_ensemble <- as.data.frame(importance_ensemble)
importance_df_ensemble$category <- ifelse(importance_df_ensemble$Variable %in% socioeconomic_vars, 'Socioeconomic & Demographic',
                                          ifelse(importance_df_ensemble$Variable %in% political_vars, 'Political & Institutional',
                                                 ifelse(importance_df_ensemble$Variable %in% military_vars, 'Military & Security',
                                                        ifelse(importance_df_ensemble$Variable %in% geographic_vars, 'Geographic & Environmental', 'Year'))))

# Sum the importance for each category
importance_by_category_ensemble <- aggregate(importance_df_ensemble$Importance, 
                                             by = list(importance_df_ensemble$category), 
                                             FUN = sum)
colnames(importance_by_category_ensemble) <- c('Category', 'Total_Importance_Ensemble')

# Define the category order
category_order <- c("Socioeconomic & Demographic", "Military & Security", "Geographic & Environmental","Year", "Political & Institutional")

# Define the color palette
library(RColorBrewer)
blue_palette <- c("Socioeconomic & Demographic" = brewer.pal(9, "Blues")[11],  # Darkest
                  "Military & Security" = brewer.pal(9, "Blues")[9],
                  "Geographic & Environmental" = brewer.pal(9, "Blues")[7],
                  "Year" = brewer.pal(9, "Blues")[5],
                  "Political & Institutional" = brewer.pal(9, "Blues")[3])  # Lightest

# Plotting the importance by category for both models
p1 <- ggplot(importance_by_category_xgb, aes(x = factor(Category, levels = category_order), y = Total_Importance_XGB, fill = Category)) +
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = blue_palette) + 
  theme_minimal()   +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),  # Remove category labels from x-axis
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "XGBoost Model Feature Importance", x = "", y = "Total Importance")

p2 <- ggplot(importance_by_category_ensemble, aes(x = factor(Category, levels = category_order), y = Total_Importance_Ensemble, fill = Category)) +
  geom_bar(stat = "identity") +  
  scale_fill_manual(values = blue_palette) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),  # Remove category labels from x-axis
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  labs(title = "Ensemble Model Feature Importance", x = "", y = "Total Importance")


# Arrange the two plots side by side
grid.arrange(p1, p2, ncol = 2)

