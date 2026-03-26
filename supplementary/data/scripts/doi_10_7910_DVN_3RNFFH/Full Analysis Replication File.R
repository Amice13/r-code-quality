#*************************************************************************#
# Replication File for "Lethal Force in Black and White:                  #
# Assessing Racial Disparities in the Circumstances of Police Killings"   # 
#                    by Shea Streeter                                     #
#*************************************************************************#

#######################################
# Purpose of File:
# Full Analysis Replication 
#######################################

#######################################
#Input: jop_analysis_data.csv
#######################################

#######################################
# Outputs: comparison_plot.png
#**************************************
# Appendix Outputs:
#   descriptives.csv
#   plot_lasso.png
#   rf_plot.png
#   RF_var_importance.csv
#   top_coefs_lasso.csv
#######################################


########################
#Ensure that all packages are installed
# install.packages('dplyr')
# install.packages('randomForest')
# install.packages('caret')
# install.packages('foreign')
# install.packages('glmnet')
# install.packages('e1071')
# install.packages(ggplot2)
# install.packages('neuralnet')
# install.packages('reptree')
# install.packages('splitstackshape')
# install.packages('lubridate')
# install.packages('stringr')


#Load Libraries 
library(dplyr)
library(randomForest)
library(caret)
library(foreign)
library(glmnet)
library(e1071)
library(ggplot2)
library(neuralnet)
library(reptree)

############################################################
# Data created by running script called called "Cleaning Variables for JOP Analysis"
# To ensure that the proper data exists, script can be run from here:
source("Cleaning Variables for JOP Analysis.R")

#Load Data
rf_data<- read.csv("jop_analysis_data.csv")
#############################################################
#Random Forest
#############################################################

# Full Random Forest ------------------------------------------------------
set.seed(7)
full_rf <- randomForest(race ~ ., data = rf_data, ntree = 750,  importance = T)  


# RF Accuracy
rf_accuracy <- (1-mean(full_rf$err.rate[,1])) *100

#################################################

# RF Analysis for Each of 5 Variable Categories ---------------------------


#Create variable name vectors for 5 categories
categories<- read.csv("variable_names.csv")
categories<- categories %>% mutate_all(as.character)

c_vars<- categories[which(categories$variable_category == "Criminal Activity"),]$analysis_name
i_vars<- categories[which(categories$variable_category == "Individual"),]$analysis_name
p_vars<- categories[which(categories$variable_category  == "Policing"),]$analysis_name
s_vars<- categories[which(categories$variable_category  == "Situational"),]$analysis_name
t_vars<- categories[which(categories$variable_category  == "Threat"),]$analysis_name

#######
#Divide into 5 data categories

criminal<- rf_data%>% select(one_of(c(c_vars, "race")))
individual<- rf_data%>% select(one_of(c(i_vars, "race")))
policing<- rf_data%>% select(one_of(c(p_vars, "race")))
situational<- rf_data%>% select(one_of(c(s_vars, "race")))
threat<- rf_data%>% select(one_of(c(t_vars, "race")))

###################
#Criminal

set.seed(7)
c_rf<- randomForest(race ~ ., data = criminal, ntree = 500,  importance = T)  
# Criminal Accuracy
c_accuracy <- (1-mean(c_rf$err.rate[,1])) *100

##################
#Individual
set.seed(7)
i_rf<- randomForest(race ~ ., data = individual, ntree = 500,  importance = T)  
# Individual Accuracy
i_accuracy <- (1-mean(i_rf$err.rate[,1])) *100

##################
#Policing
set.seed(7)
p_rf<- randomForest(race ~ ., data = policing, ntree = 500,  importance = T)  
#Policing Accuracy
p_accuracy <- (1-mean(p_rf$err.rate[,1])) *100

##################
#Situational
set.seed(7)
s_rf<- randomForest(race ~ ., data = situational, ntree = 500,  importance = T)  
# Situational Accuracy
s_accuracy <- (1-mean(s_rf$err.rate[,1])) *100

##################
#Threat
set.seed(7)
t_rf<- randomForest(race ~ ., data = threat, ntree = 500,  importance = T)  
# Threat Accuracy
t_accuracy <- (1-mean(t_rf$err.rate[,1])) *100

#########################################################################
#########################################################################

# LASSO Regression --------------------------------------------------------

#Make outcome variable binary

lasso_data<- rf_data %>% mutate(black = ifelse(race == "Black", 1,0)) %>% select(- c(race))

#### Split data in label and matrix
y <- lasso_data$black
x <- lasso_data %>% select(-black)
x <- as.matrix(x)

### To select the right lambda we are going to use k-fold crossvalidation 
lasso_cv<- cv.glmnet(x = x, y = y, family = "binomial", type.measure = "class", nfolds = nrow(x))

#LASSO Accuracy
lasso_accuracy<- (1 - min(lasso_cv$cvm))*100

############################################################################
############################################################################


# Support Vector Machine --------------------------------------------------

#Split data into test and training
set.seed(124)
train_r <-  sample(nrow(rf_data), size=nrow(rf_data)*.8, replace=F)
svm_train<- rf_data[train_r,] 
svm_test  <- rf_data[-train_r,]

###
#Run SVM
svmfit = svm(race ~ ., data = svm_train, kernel = "radial", cost = 10, scale = FALSE)
#print(svmfit)

### Use results to predict race for test data
pred = predict(svmfit,svm_test,type="class")

## Compare predicted race to actual race
compare <- cbind.data.frame(pred, svm_test$race)
compare_tab <- table(compare)

##
svm_accuracy <- ((compare_tab[1,1] + compare_tab[2,2]) / sum(compare_tab))*100



############################################################################
############################################################################

# Neural Networks  --------------------------------------------------

# fit neural network
nn <- neuralnet(race ~ ., data=svm_train, hidden=15,rep=10, act.fct = "logistic",
                linear.output = FALSE)


## Prediction using neural network
probs <- predict(nn , svm_test)

# Converting probabilities into binary classes setting threshold level 0.5
pred <- ifelse(probs>0.5, 1, 0)
compare_nn <- table(cbind.data.frame(pred[,2], svm_test$race))

nn_accuracy <- ((compare_nn[1,1] + compare_nn[2,2]) / sum(compare_nn))*100


############################################################################
############################################################################
#-------------------------------------------------------------------------
### Code to create Figure 1 
# Plot different accuracies in a barplot

average_accuracy <- mean(c(rf_accuracy, lasso_accuracy, svm_accuracy, nn_accuracy))

#Naive accuacy

n_accuracy<- (1-(sum(lasso_data$black)/ nrow(lasso_data))) *100

#Combine model accuracies into dataframe
accuracies <- c( rf_accuracy, s_accuracy, t_accuracy, p_accuracy, 
                c_accuracy, i_accuracy, lasso_accuracy, svm_accuracy, nn_accuracy)
model      <- c( "Full Random Forest", "Situational Variables (RF)", 
                "Threat Variables (RF)", "Policing Variables (RF)",
                "Criminal Variables (RF)", "Individual Variables (RF)", 
                "Lasso Regression", "Support Vector Machine", "Neural Networks")

df <- cbind.data.frame(accuracies, model)
df <- df %>% mutate(model=as.factor(model))


#Accuracy Plot
g <- ggplot(df, aes(y=accuracies, x=reorder(model, accuracies))) +
  geom_bar(stat="identity", width=0.3) +
  theme_bw() +
  ylab("Accuracy Rate") +
  scale_y_continuous(breaks=c(0,20,40,60,80, 100), limits = c(0,100)) +
  xlab("")+
  geom_hline(aes(yintercept = n_accuracy), color="red", linetype="dashed")+ 
  coord_flip() +
  theme(legend.position="none")
g

## Save
ggsave("comparison_plot.png", g, width=6)

###################################################################
###################################################################





###################################################################
#                     Appendix Data                               #
###################################################################    

# A1: Data Descriptives ---------------------------------------------------
descrip <- lasso_data %>% select(-black)


## Mean all sample
mean_all <- apply(descrip, 2, function(x) mean(x, na.rm=T))

## Mean white
mean_white  <- apply(descrip[lasso_data$black==0,], 2, function(x) mean(x, na.rm=T))

## Mean black
mean_black  <- apply(descrip[lasso_data$black==1,], 2, function(x) mean(x, na.rm=T))

## Diference
dif_w_b <- mean_white - mean_black 


### Dataset

data <- cbind.data.frame(round(mean_all,4), round(mean_white, 4), round(mean_black,4), round(dif_w_b,4))

## p vlaue
p_val <- rep(NA, nrow(data))
#460 white 240 black
for(i in 1:nrow(data)){
  p <- t.test(descrip[,i]~ as.factor(lasso_data$black))
  p <- p$p.value
  p_val[i] <- round(p, 5)
}

data$p_val <- p_val

### Add stars
star <- rep("", nrow(data))
for(i in 1:nrow(data)){
  s <- ifelse(p_val[i]<=0.05, "*", star[i])
  s <- ifelse(p_val[i]<=0.01, "**", s)
  s <- ifelse(p_val[i]<=0.001, "***", s)
  star[i] <- s
}

data$star <- star


## Name cols
names(data) <- c("Mean_Total", "Mean_White", "Mean_Black", "Difference", "p", "S")

data$Variables <- rownames(data)

## Add Categories
cat_sub<- categories %>% select(analysis_name, variable_category, name_in_tables)
data <- data %>% left_join(cat_sub,  by = c("Variables" = "analysis_name")) 

data<- data %>% select(Category = variable_category, name_in_tables, Mean_Total, Mean_White, 
                       Mean_Black, Difference, p, S )

##
data <- data %>% arrange(Category)

## Final Name cols
names(data) <- c("Category", "Variable Name", "Mean (Full sample)", "Mean White",
                 "Mean Black", "Difference", "p-value", "")

#Download csv version of Table A1
#write.csv(data, "descriptives.csv", row.names=F)



# A2: Model Accuracy Plots ------------------------------------------------

#plot demonstrating Random forest error rate
Trees<- c(1:750)
df_rf <- cbind.data.frame(full_rf$err.rate[,1], Trees)
names(df_rf) <- c("OOB", "Trees")
p <- ggplot(df_rf, aes(x=Trees, y=OOB)) + geom_line() + theme_bw() +ylab("OOB Error Rate")
p
#ggsave("rf_plot.png", p, width=6)
######################################


#Plot Demonstrating lasso regression error rate
png("plot_lasso.png")
plot(lasso_cv)
dev.off()


##############################################################################
###############################################################################
# A3 Variable Importance Measures --------------------------------------------

#Variable Importance Measures for Random Forest
imp_results<- importance(full_rf)
varImpPlot(full_rf, type =1, n.var = 10)
#write.csv(imp_results, "RF_var_importance.csv") #csv file of all variable importance rankings

#############################################
#Variable Importance Measures for Lasso Regression

#Extract the lambda which minimizes the error
cv_lambda.1se <- lasso_cv$lambda.1se

# First, create an object with the function coef()
coef_lasso <- coef(lasso_cv,s=cv_lambda.1se )

## Fron this object, select only those that are non zero
coef_lasso <- round(coef_lasso[which(coef_lasso[,] !=0),] , 4)
## Sort them from large to small based on absolute value
coef_lasso<- coef_lasso[order(-abs(coef_lasso))]
#For top 10 variables (excluding intercept)
coef_lasso[1:11]
# If desired, write csv file which contains all variables in model, ordered in magnitude of coefficient
#write.csv(coef_lasso, "top_coefs_lasso.csv")



##############################################################################
#Extra Analysis
##########################################################################

########################################################################
# Robustness Check with .66 Classifcaion Cutoff --------------------------------------------------------
#Because 34% of dataset is Black and 66% is White, I test whether accuracy improves with
#different classification cutoffs taking these uneven categories into consideration
#Random forest
cut_rf2<- randomForest(race ~ ., data = rf_data, ntree = 750,  importance = T, cutoff = c(.34, .66))  
cut_rf2

#SVM
### Use results to predict race for test data
pred_svm = predict(svmfit,svm_test,type="class", cutoff = c(.66))

## Compare predicted race to actual race
compare_svm <- cbind.data.frame(pred_svm, svm_test$race)
compare_tab <- table(compare_svm)

##
svm_accuracy_alt <- ((compare_tab[1,1] + compare_tab[2,2]) / sum(compare_tab))*100

#Neural Net
# Converting probabilities into binary classes setting threshold level 0.66 the naive accuracy
pred <- ifelse(probs>0.66, 1, 0)
compare_nn_alt <- table(cbind.data.frame(pred[,2], svm_test$race))

nn_accuracy_alt <- ((compare_nn_alt[1,1] + compare_nn_alt[2,2]) / sum(compare_nn_alt))*100

#These alternative cuttoffs do not produce better predictions

