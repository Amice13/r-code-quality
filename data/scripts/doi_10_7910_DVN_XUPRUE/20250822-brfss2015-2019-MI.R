#This R script file multiply impute the BRFSS 2015, 2017, 2019 data.
#This R script adopt the passive approach for the interaction (generate the passive variables of sum scores and interactions)
#Then save the multiply imputed data at the end (called brfss2015-impute-10.dta, brfss2017-impute-10.dta, brfss2019-impute-10.dta)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readstata13) #load Stata data file
library(mice) #multiple imputation
library(doFuture) #required package for parallel computing
library(foreach) #parallelize loop
library(fastDummies) #create dummy variables







###############################MI for BRFSS 2015 Start###############################
#Create subfolder
dir.create("graph", showWarnings = FALSE)
dir.create("graph/brfss2015-MI", showWarnings = FALSE)


brfss <- read.dta13("Data Ready for Analysis/brfss2015-readyforimpute.dta")



#declare the class of variables
brfss$exercise <- as.factor(brfss$exercise)

brfss$exercise1type <- as.factor(brfss$exercise1type)

brfss$exercise1freq <- as.numeric(brfss$exercise1freq)

brfss$exercise1min <- as.numeric(brfss$exercise1min)

brfss$exercise2type <- as.factor(brfss$exercise2type)

brfss$exercise2freq <- as.numeric(brfss$exercise2freq)

brfss$exercise2min <- as.numeric(brfss$exercise2min)

brfss$muscleexercise <- as.numeric(brfss$muscleexercise)

brfss$exe1_intensity <- as.ordered(brfss$exe1_intensity)

brfss$exe2_intensity <- as.ordered(brfss$exe2_intensity)

brfss$exe_intensity_simplest <- as.ordered(brfss$exe_intensity_simplest)

brfss$exe_active <- as.ordered(brfss$exe_active)

brfss$exe_aerobic_advice <- as.ordered(brfss$exe_aerobic_advice)

brfss$exe_strength_advice <- as.factor(brfss$exe_strength_advice)

brfss$yearsold <- as.numeric(brfss$yearsold)

brfss$educ <- as.ordered(brfss$educ)

brfss$praceethnicgp_multi_sim <- as.factor(brfss$praceethnicgp_multi_sim)

brfss$hhincome <- as.ordered(brfss$hhincome)

brfss$svyweight <- as.numeric(brfss$svyweight)

brfss$male <- as.factor(brfss$male)

brfss$strata <- as.factor(brfss$strata)

brfss$psu <- as.factor(brfss$psu)

brfss$state <- as.factor(brfss$state)

brfss$id <- as.factor(brfss$id)

brfss$srh <- as.ordered(brfss$srh)

brfss$badphyhlth <- as.integer(brfss$badphyhlth)

brfss$badmenhlth <- as.integer(brfss$badmenhlth)

brfss$bmicat <- as.ordered(brfss$bmicat)

brfss$employstatus_full <- as.factor(brfss$employstatus_full)



brfss_ready_to_impute <- subset(brfss, !is.na(male), select=-c(psu, strata, id))




#Get the predictor matrix of multiple imputation such that I can change which variables are predictors and should be imputed
initial <- mice::mice(brfss_ready_to_impute, maxit=0)
pred_matrix <- initial$predictorMatrix

#change the predictor matrix to fit my requirements
#Note: each row in the predictor matrix represents the outcome in the imputation equation, each column represents the predictor in the imputation equation
#Value of 1 means that the predictor is present in the imputation equation. Value of 0 means that the predictor is absent in the imputation equation.
#The following lines are not necessary but to make sure that these variables will not be predicted by other variables or predict other variables explicitly.

#All these variables contains no missing values. I just want to make sure that they are not used in the imputation process.
pred_matrix["svyweight",] <- 0 #svyweight will not be predicted by any variables

# pred_matrix["strata",] <- 0 #strata will not be predicted by any variables
# 
# pred_matrix["psu",] <- 0 #psu will not be predicted by any variables

pred_matrix["state",] <- 0 #state will not be predicted by any variables

pred_matrix["male",] <- 0 #male will not be predicted by any variables



#Get the method matrix of multiple imputation such that I can change which imputation method should be used for a specific variable
method_matrix <- initial$method

#Customize the imputation method for specific variables
#method_matrix["hhincome"] <- "pmm" #Use predictive mean matching (PMM) to impute hhincome

#change the prediction method for all outcomes
for (i in seq_along(method_matrix)) {
  # Check if item is NOT empty (defined as: not character(0) AND not a single "")
  if (!identical(method_matrix[[i]], character(0)) && !identical(method_matrix[[i]], "")) {
    method_matrix[[i]] <- "rf" #Use random forest for imputation
  }
}



#Create a where matrix to exclude certain variables/observations to be imputed
where_matrix <- make.where(brfss_ready_to_impute, keyword="missing") #"missing" marks all missing observations TRUE (i.e., all missing observations will be imputed)

#marking these variables FALSE in the where matrix means that they will not be imputed
where_matrix[,c("svyweight", "state", "male")] <- FALSE


#start multiple imputation
start.time <- Sys.time()
start.time
#imputed <- mice(brfss_ready_to_impute, m=10, predictorMatrix=pred_matrix, method=method_matrix, where=where_matrix, maxit=10, seed=9537)
imputed <- mice::futuremice(brfss_ready_to_impute, m=10,
                            predictorMatrix=pred_matrix, method=method_matrix, where=where_matrix,
                            parallelseed=9537, nnet.MaxNWts = 10, n.core=8) #set n.core=no. of physical core in Intel CPU. set n.core=no.of available threads in AMD CPU.
end.time <- Sys.time()
used_time <- difftime(end.time, start.time, units="mins")
used_time

#Check the logged events of the imputed dataset
imputed$loggedEvents

#Check the method matrix of the imputed dataset
imputed$method

#Check the predictor matrix of the imputed dataset
imputed$predictorMatrix

#Check if the imputed dataset is complete
colSums(is.na(complete(imputed)))


#Extract the list of imputed variables
imputedvarlist <- names(imputed$method)[sapply(imputed$method, function(x) nchar(x) > 0)]



##########Create trace plots in a pdf file Start##########
#Create multipage PDF
pdf("graph/brfss2015-MI/trace-plots.pdf", width = 8, height = 6)  # Start PDF device

for (i in seq_along(imputedvarlist)) {
  # Generate trace plot for the current variable
  p <- plot(imputed, y=imputedvarlist[[i]], main=paste("Trace plot of", imputedvarlist[[i]]))
  print(p)  # Explicitly print the ggplot object
  
}
dev.off()
##########Create trace plots in a pdf file End##########




##########Create trace plots in multiple svg files Start##########
for (i in seq_along(imputedvarlist)) {
  # Define output filename (column-safe)
  filename <- paste0("traceplot_", make.names(imputedvarlist[[i]]), ".svg")
  
  # Open SVG device (customize dimensions here)
  svg(file = paste("graph/brfss2015-MI/", filename), width = 6, height = 5)
  
  # Generate trace plot for the current variable
  p <- plot(imputed, y=imputedvarlist[[i]], main=paste("Trace plot of", imputedvarlist[[i]]))
  print(p)  # Explicitly print the ggplot object
  dev.off()
}
##########Create trace plots in multiple svg files End##########



#Setup a list to contain all imputed datasets
impList <- list()
impList[[1]] <- imputed$data
for (i in 1:imputed$m) {
  impList[[i+1]] <- complete(imputed, action = i)
}


#generate a marker to mark the number of imputation in each imputed dataset
for (i in 1:length(impList)){
  impList[[i]]$"m_num" <- i-1
}

#generate the respondent's unique ID for merging with psu and strata variables
impList <- lapply(impList, function(df) {
  df$id <- seq_len(nrow(df))
  return(df)
})


#collapse the original and all imputed datasets together for subsequent Stata estimation
entiredf <- dplyr::bind_rows(impList, .id = "column_label")



save(brfss_ready_to_impute, pred_matrix, method_matrix, where_matrix, imputed, impList, entiredf, file="Data Ready for Analysis/brfss2015-impute-10.RData")

save.dta13(subset(entiredf, select=-c(state)), "Data Ready for Analysis/brfss2015-impute-10.dta") #I delete the state variable as its value coding is messed up. I will merge it in Stata later (state doesn't have any missing values)
###############################MI for BRFSS 2015 End###############################







###############################MI for BRFSS 2017 Start###############################
#Create subfolder
dir.create("graph", showWarnings = FALSE)
dir.create("graph/brfss2017-MI", showWarnings = FALSE)


brfss <- read.dta13("Data Ready for Analysis/brfss2017-readyforimpute.dta")



#declare the class of variables
brfss$exercise <- as.factor(brfss$exercise)

brfss$exercise1type <- as.factor(brfss$exercise1type)

brfss$exercise1freq <- as.numeric(brfss$exercise1freq)

brfss$exercise1min <- as.numeric(brfss$exercise1min)

brfss$exercise2type <- as.factor(brfss$exercise2type)

brfss$exercise2freq <- as.numeric(brfss$exercise2freq)

brfss$exercise2min <- as.numeric(brfss$exercise2min)

brfss$muscleexercise <- as.numeric(brfss$muscleexercise)

brfss$exe1_intensity <- as.ordered(brfss$exe1_intensity)

brfss$exe2_intensity <- as.ordered(brfss$exe2_intensity)

brfss$exe_intensity_simplest <- as.ordered(brfss$exe_intensity_simplest)

brfss$exe_active <- as.ordered(brfss$exe_active)

brfss$exe_aerobic_advice <- as.ordered(brfss$exe_aerobic_advice)

brfss$exe_strength_advice <- as.factor(brfss$exe_strength_advice)

brfss$yearsold <- as.numeric(brfss$yearsold)

brfss$educ <- as.ordered(brfss$educ)

brfss$praceethnicgp_multi_sim <- as.factor(brfss$praceethnicgp_multi_sim)

brfss$hhincome <- as.ordered(brfss$hhincome)

brfss$svyweight <- as.numeric(brfss$svyweight)

brfss$male <- as.factor(brfss$male)

brfss$strata <- as.factor(brfss$strata)

brfss$psu <- as.factor(brfss$psu)

brfss$state <- as.factor(brfss$state)

brfss$id <- as.factor(brfss$id)

brfss$srh <- as.ordered(brfss$srh)

brfss$badphyhlth <- as.integer(brfss$badphyhlth)

brfss$badmenhlth <- as.integer(brfss$badmenhlth)

brfss$bmicat <- as.ordered(brfss$bmicat)

brfss$employstatus_full <- as.factor(brfss$employstatus_full)



brfss_ready_to_impute <- subset(brfss, !is.na(male), select=-c(psu, strata, id))




#Get the predictor matrix of multiple imputation such that I can change which variables are predictors and should be imputed
initial <- mice::mice(brfss_ready_to_impute, maxit=0)
pred_matrix <- initial$predictorMatrix

#change the predictor matrix to fit my requirements
#Note: each row in the predictor matrix represents the outcome in the imputation equation, each column represents the predictor in the imputation equation
#Value of 1 means that the predictor is present in the imputation equation. Value of 0 means that the predictor is absent in the imputation equation.
#The following lines are not necessary but to make sure that these variables will not be predicted by other variables or predict other variables explicitly.

#All these variables contains no missing values. I just want to make sure that they are not used in the imputation process.
pred_matrix["svyweight",] <- 0 #svyweight will not be predicted by any variables

# pred_matrix["strata",] <- 0 #strata will not be predicted by any variables
# 
# pred_matrix["psu",] <- 0 #psu will not be predicted by any variables

pred_matrix["state",] <- 0 #state will not be predicted by any variables

pred_matrix["male",] <- 0 #male will not be predicted by any variables



#Get the method matrix of multiple imputation such that I can change which imputation method should be used for a specific variable
method_matrix <- initial$method

#Customize the imputation method for specific variables
#method_matrix["hhincome"] <- "pmm" #Use predictive mean matching (PMM) to impute hhincome

#change the prediction method for all outcomes
for (i in seq_along(method_matrix)) {
  # Check if item is NOT empty (defined as: not character(0) AND not a single "")
  if (!identical(method_matrix[[i]], character(0)) && !identical(method_matrix[[i]], "")) {
    method_matrix[[i]] <- "rf" #Use random forest for imputation
  }
}



#Create a where matrix to exclude certain variables/observations to be imputed
where_matrix <- make.where(brfss_ready_to_impute, keyword="missing") #"missing" marks all missing observations TRUE (i.e., all missing observations will be imputed)

#marking these variables FALSE in the where matrix means that they will not be imputed
where_matrix[,c("svyweight", "state", "male")] <- FALSE


#start multiple imputation
start.time <- Sys.time()
start.time
#imputed <- mice(brfss_ready_to_impute, m=10, predictorMatrix=pred_matrix, method=method_matrix, where=where_matrix, maxit=10, seed=9537)
imputed <- mice::futuremice(brfss_ready_to_impute, m=10,
                            predictorMatrix=pred_matrix, method=method_matrix, where=where_matrix,
                            parallelseed=9537, nnet.MaxNWts = 10, n.core=8) #set n.core=no. of physical core in Intel CPU. set n.core=no.of available threads in AMD CPU.
end.time <- Sys.time()
used_time <- difftime(end.time, start.time, units="mins")
used_time

#Check the logged events of the imputed dataset
imputed$loggedEvents

#Check the method matrix of the imputed dataset
imputed$method

#Check the predictor matrix of the imputed dataset
imputed$predictorMatrix

#Check if the imputed dataset is complete
colSums(is.na(complete(imputed)))


#Extract the list of imputed variables
imputedvarlist <- names(imputed$method)[sapply(imputed$method, function(x) nchar(x) > 0)]



##########Create trace plots in a pdf file Start##########
#Create multipage PDF
pdf("graph/brfss2017-MI/trace-plots.pdf", width = 8, height = 6)  # Start PDF device

for (i in seq_along(imputedvarlist)) {
  # Generate trace plot for the current variable
  p <- plot(imputed, y=imputedvarlist[[i]], main=paste("Trace plot of", imputedvarlist[[i]]))
  print(p)  # Explicitly print the ggplot object
  
}
dev.off()
##########Create trace plots in a pdf file End##########




##########Create trace plots in multiple svg files Start##########
for (i in seq_along(imputedvarlist)) {
  # Define output filename (column-safe)
  filename <- paste0("traceplot_", make.names(imputedvarlist[[i]]), ".svg")
  
  # Open SVG device (customize dimensions here)
  svg(file = paste("graph/brfss2017-MI/", filename), width = 6, height = 5)
  
  # Generate trace plot for the current variable
  p <- plot(imputed, y=imputedvarlist[[i]], main=paste("Trace plot of", imputedvarlist[[i]]))
  print(p)  # Explicitly print the ggplot object
  dev.off()
}
##########Create trace plots in multiple svg files End##########



#Setup a list to contain all imputed datasets
impList <- list()
impList[[1]] <- imputed$data
for (i in 1:imputed$m) {
  impList[[i+1]] <- complete(imputed, action = i)
}


#generate a marker to mark the number of imputation in each imputed dataset
for (i in 1:length(impList)){
  impList[[i]]$"m_num" <- i-1
}

#generate the respondent's unique ID for merging with psu and strata variables
impList <- lapply(impList, function(df) {
  df$id <- seq_len(nrow(df))
  return(df)
})


#collapse the original and all imputed datasets together for subsequent Stata estimation
entiredf <- dplyr::bind_rows(impList, .id = "column_label")



save(brfss_ready_to_impute, pred_matrix, method_matrix, where_matrix, imputed, impList, entiredf, file="Data Ready for Analysis/brfss2017-impute-10.RData")

save.dta13(subset(entiredf, select=-c(state)), "Data Ready for Analysis/brfss2017-impute-10.dta") #I delete the state variable as its value coding is messed up. I will merge it in Stata later (state doesn't have any missing values)
###############################MI for BRFSS 2017 End###############################











###############################MI for BRFSS 2019 Start###############################
#Create subfolder
dir.create("graph", showWarnings = FALSE)
dir.create("graph/brfss2019-MI", showWarnings = FALSE)


brfss <- read.dta13("Data Ready for Analysis/brfss2019-readyforimpute.dta")



#declare the class of variables
brfss$exercise <- as.factor(brfss$exercise)

brfss$exercise1type <- as.factor(brfss$exercise1type)

brfss$exercise1freq <- as.numeric(brfss$exercise1freq)

brfss$exercise1min <- as.numeric(brfss$exercise1min)

brfss$exercise2type <- as.factor(brfss$exercise2type)

brfss$exercise2freq <- as.numeric(brfss$exercise2freq)

brfss$exercise2min <- as.numeric(brfss$exercise2min)

brfss$muscleexercise <- as.numeric(brfss$muscleexercise)

brfss$exe1_intensity <- as.ordered(brfss$exe1_intensity)

brfss$exe2_intensity <- as.ordered(brfss$exe2_intensity)

brfss$exe_intensity_simplest <- as.ordered(brfss$exe_intensity_simplest)

brfss$exe_active <- as.ordered(brfss$exe_active)

brfss$exe_aerobic_advice <- as.ordered(brfss$exe_aerobic_advice)

brfss$exe_strength_advice <- as.factor(brfss$exe_strength_advice)

brfss$yearsold <- as.numeric(brfss$yearsold)

brfss$educ <- as.ordered(brfss$educ)

brfss$praceethnicgp_multi_sim <- as.factor(brfss$praceethnicgp_multi_sim)

brfss$hhincome <- as.ordered(brfss$hhincome)

brfss$svyweight <- as.numeric(brfss$svyweight)

brfss$male <- as.factor(brfss$male)

brfss$strata <- as.factor(brfss$strata)

brfss$psu <- as.factor(brfss$psu)

brfss$state <- as.factor(brfss$state)

brfss$id <- as.factor(brfss$id)

brfss$srh <- as.ordered(brfss$srh)

brfss$badphyhlth <- as.integer(brfss$badphyhlth)

brfss$badmenhlth <- as.integer(brfss$badmenhlth)

brfss$bmicat <- as.ordered(brfss$bmicat)

brfss$employstatus_full <- as.factor(brfss$employstatus_full)



brfss_ready_to_impute <- subset(brfss, !is.na(male), select=-c(psu, strata, id))




#Get the predictor matrix of multiple imputation such that I can change which variables are predictors and should be imputed
initial <- mice::mice(brfss_ready_to_impute, maxit=0)
pred_matrix <- initial$predictorMatrix

#change the predictor matrix to fit my requirements
#Note: each row in the predictor matrix represents the outcome in the imputation equation, each column represents the predictor in the imputation equation
#Value of 1 means that the predictor is present in the imputation equation. Value of 0 means that the predictor is absent in the imputation equation.
#The following lines are not necessary but to make sure that these variables will not be predicted by other variables or predict other variables explicitly.

#All these variables contains no missing values. I just want to make sure that they are not used in the imputation process.
pred_matrix["svyweight",] <- 0 #svyweight will not be predicted by any variables

# pred_matrix["strata",] <- 0 #strata will not be predicted by any variables
# 
# pred_matrix["psu",] <- 0 #psu will not be predicted by any variables

pred_matrix["state",] <- 0 #state will not be predicted by any variables

pred_matrix["male",] <- 0 #male will not be predicted by any variables



#Get the method matrix of multiple imputation such that I can change which imputation method should be used for a specific variable
method_matrix <- initial$method

#Customize the imputation method for specific variables
#method_matrix["hhincome"] <- "pmm" #Use predictive mean matching (PMM) to impute hhincome

#change the prediction method for all outcomes
for (i in seq_along(method_matrix)) {
  # Check if item is NOT empty (defined as: not character(0) AND not a single "")
  if (!identical(method_matrix[[i]], character(0)) && !identical(method_matrix[[i]], "")) {
    method_matrix[[i]] <- "rf" #Use random forest for imputation
  }
}



#Create a where matrix to exclude certain variables/observations to be imputed
where_matrix <- make.where(brfss_ready_to_impute, keyword="missing") #"missing" marks all missing observations TRUE (i.e., all missing observations will be imputed)

#marking these variables FALSE in the where matrix means that they will not be imputed
where_matrix[,c("svyweight", "state", "male")] <- FALSE


#start multiple imputation
start.time <- Sys.time()
start.time
#imputed <- mice(brfss_ready_to_impute, m=10, predictorMatrix=pred_matrix, method=method_matrix, where=where_matrix, maxit=10, seed=9537)
imputed <- mice::futuremice(brfss_ready_to_impute, m=10,
                            predictorMatrix=pred_matrix, method=method_matrix, where=where_matrix,
                            parallelseed=9537, nnet.MaxNWts = 10, n.core=8) #set n.core=no. of physical core in Intel CPU. set n.core=no.of available threads in AMD CPU.
end.time <- Sys.time()
used_time <- difftime(end.time, start.time, units="mins")
used_time

#Check the logged events of the imputed dataset
imputed$loggedEvents

#Check the method matrix of the imputed dataset
imputed$method

#Check the predictor matrix of the imputed dataset
imputed$predictorMatrix

#Check if the imputed dataset is complete
colSums(is.na(complete(imputed)))


#Extract the list of imputed variables
imputedvarlist <- names(imputed$method)[sapply(imputed$method, function(x) nchar(x) > 0)]



##########Create trace plots in a pdf file Start##########
#Create multipage PDF
pdf("graph/brfss2019-MI/trace-plots.pdf", width = 8, height = 6)  # Start PDF device

for (i in seq_along(imputedvarlist)) {
  # Generate trace plot for the current variable
  p <- plot(imputed, y=imputedvarlist[[i]], main=paste("Trace plot of", imputedvarlist[[i]]))
  print(p)  # Explicitly print the ggplot object
  
}
dev.off()
##########Create trace plots in a pdf file End##########




##########Create trace plots in multiple svg files Start##########
for (i in seq_along(imputedvarlist)) {
  # Define output filename (column-safe)
  filename <- paste0("traceplot_", make.names(imputedvarlist[[i]]), ".svg")
  
  # Open SVG device (customize dimensions here)
  svg(file = paste("graph/brfss2019-MI/", filename), width = 6, height = 5)
  
  # Generate trace plot for the current variable
  p <- plot(imputed, y=imputedvarlist[[i]], main=paste("Trace plot of", imputedvarlist[[i]]))
  print(p)  # Explicitly print the ggplot object
  dev.off()
}
##########Create trace plots in multiple svg files End##########



#Setup a list to contain all imputed datasets
impList <- list()
impList[[1]] <- imputed$data
for (i in 1:imputed$m) {
  impList[[i+1]] <- complete(imputed, action = i)
}


#generate a marker to mark the number of imputation in each imputed dataset
for (i in 1:length(impList)){
  impList[[i]]$"m_num" <- i-1
}

#generate the respondent's unique ID for merging with psu and strata variables
impList <- lapply(impList, function(df) {
  df$id <- seq_len(nrow(df))
  return(df)
})


#collapse the original and all imputed datasets together for subsequent Stata estimation
entiredf <- dplyr::bind_rows(impList, .id = "column_label")



save(brfss_ready_to_impute, pred_matrix, method_matrix, where_matrix, imputed, impList, entiredf, file="Data Ready for Analysis/brfss2019-impute-10.RData")

save.dta13(subset(entiredf, select=-c(state)), "Data Ready for Analysis/brfss2019-impute-10.dta") #I delete the state variable as its value coding is messed up. I will merge it in Stata later (state doesn't have any missing values)
###############################MI for BRFSS 2019 End###############################





