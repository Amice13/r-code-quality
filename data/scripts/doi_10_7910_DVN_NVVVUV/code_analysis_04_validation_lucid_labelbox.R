############################################################################
############################################################################
#### 
#### Constantine Boussalis, Travis G. Coan, Mirya R. Holman, Stefan Müller
#### Gender, Candidate Emotional Expression,
#### and Voter Reactions During Televised Debates 
####
#### American Political Science Review 
####
#### code_analysis_04_validation_lucid_labelbox.R
#### Note: this file assesses the FaceAPI performance using large samples
#### of human annotations
#### 
#### For details on all datasets, R scripts, and instructions 
#### please consult the file "0000_replication_instructions.pdf" in the 
#### Dataverse of this project.
############################################################################
############################################################################


library(dplyr)   # CRAN v1.0.5
options(dplyr.summarise.inform = FALSE)
library(tibble)  # CRAN v3.1.1
library(caret)   # CRAN v6.0-86
library(ggplot2) # CRAN v3.3.3
library(cowplot) # CRAN v1.1.1
library(stringr) # CRAN v1.4.0

# set working directory
setwd("")

# set seed for classification
set.seed(42)

# load custom ggplot2 scheme
source("code_function_theme_base.R")

# Read data
labelbox <- read.csv('data_annotations_labelbox.csv', stringsAsFactors = F)

#--------------------------------------------------
# Initialize CV and annotations
#--------------------------------------------------

# Setup CV
train_control_rmse <- trainControl(method="repeatedcv", 
                                   number=5,
                                   repeats = 3)

train_control_class <- trainControl(method="repeatedcv", 
                                    number=5,
                                    repeats = 3,
                                    summaryFunction = multiClassSummary, 
                                    classProbs = TRUE)
# Initalize runs
dvs <- c("happy", "angry", "any_emotion")
metrics <- c("RMSE", "F1")
clips <- c("all", "opponents", "merkel")
runs <-  expand.grid(dv = dvs, 
                     metric = metrics, 
                     clips = clips, 
                     stringsAsFactors = FALSE)
# Initialze variable to hold the results
runs$metric_value <- NA

# Change binary measures to factor for caret
labelbox$happy_binary <- make.names(as.factor(labelbox$happy_binary))
labelbox$angry_binary <- make.names(as.factor(labelbox$angry_binary))
labelbox$any_emotion_binary <- make.names(as.factor(labelbox$any_emotion_binary))

#--------------------------------------------------
# Performance
#--------------------------------------------------

for (i in 1:nrow(runs)){
    # Who's video clips should be include? (all, merkel, or opponents)
    if (runs$clips[i] == "all"){
        print("Evaluating performance using all clips.")
        data <- labelbox
    } else if (runs$clips[i] == "merkel") {
        print("Evaluating performance using Merkel clips.")
        data <- labelbox[labelbox$merkel == 1,]
    } else {
        print("Evaluating performance using opponents clips.")
        data <- labelbox[labelbox$merkel == 0,]
    }
    
    # How should performance be evaluated? (RMSE or F1)
    if (runs$metric[i] == "RMSE"){
        # Subset variables needed for run
        data <- subset(data, select = c(runs$dv[i], 
                                        paste(runs$dv[i], 'api', 'prediction', sep="_")))
        # Train CV model
        fit <- train(as.formula(paste(runs$dv[i], '~', '.')),
                     data=data,
                     trControl = train_control_rmse,
                     method = "lm",
                     metric = runs$metric[i])
        runs$metric_value[i] <- fit$results$RMSE
    } else {
        # Subset variables needed for run
        binary <- paste(runs$dv[i], 'binary', sep = "_")
        data <- subset(data, select = c(binary, 
                                        paste(runs$dv[i], 'api', 'prediction', sep = "_")))
        # Train CV model
        fit <- train(as.formula(paste(binary, '~', '.')),
                     data=data,
                     trControl = train_control_class,
                     method = "bayesglm",
                     metric = runs$metric[i])
        runs$metric_value[i] <- fit$results$F1
    }
}


dat_labelbox <- runs

dat_labelbox$dataset_id <- "Trained Coders"

## Repeat analysis for Lucid sample

set.seed(42)

# -----------------------------------------------------------
# Helper function

cross_validate <- function(dat, iv, subset_candidate = TRUE){
    # Takes a dataset and an emotion predictor variable and
    # returns the estimated error based on 5-fold cross-validation.
    # We employ the following procedure:
    # 1. Run 5-fold cross validation on the 50 clips and save the 
    #    the relevant clip IDs for the training and test sets.
    # 2. Subset the full data (i.e., the data for each respondent)
    #    into a data.frame that includes the training clips and
    #    another dataset that holds the test clips.
    # 3. Fit a linear model on the training data and use it
    #    to make out-of-sample predictions for the test data.
    # 4. Calculate the error as actual - predicted. Use the error to
    #    calculate various measures of performance (e.g., MSE and MAE)
    #
    # Inputs:
    # -------
    # dat = data.frame with annotations for each clip and each respondent.
    # iv  = The predictor variable to use when fitting the linear model. 
    #       This variable is determined by the emotion of interest.
    # subset_candidate = TRUE if we want to distinguish between Merkel and
    #       and her male opponents and 0 otherwise.
    #
    # Output:
    # -------
    # A data.frame with the estimated performance metrics by individual,
    # averaged over the 5 folds.
    
    # Cross-validate based on the 50 clipsgrp_rmse <- merged %>%
    clips <- dat %>%
        group_by(clip_id) %>%
        summarize(clip_id = clip_id[1], 
                  merkel = merkel[1])
    
    # Generate index
    group_var = "merkel"
    d <- rownames_to_column(clips, var = "id") %>% mutate_at(vars(id), as.integer)
    id <- subset(d, select = c('id', group_var))
    
    # Fold names
    fold_num <- 5
    fold_names = sapply(1:fold_num, function(x) paste(c("Fold", x), collapse=''))
    
    # Get statified CV indices for 50 clips
    folds_train = createFolds(clips$merkel, k = 5, list = TRUE, returnTrain = TRUE)
    
    # Next, we get the approriate indices for the values to include in the
    # full dataset based on the CV results for the 50 clips.
    results <- c()
    for (i in seq(fold_num)){
        print(paste("Training fold =", i))
        # Extract relevant observations and train
        train_clips <- clips$clip_id[folds_train[[i]]]
        training <- dat[dat$clip_id %in% train_clips, ]
        # Fit model
        fit <- lm(as.formula(paste("response", "~", iv)), data = dat)
        testing <- dat[!dat$clip_id %in% train_clips, ]
        # Get predictions and error
        testing$prediction <- predict(fit, newdata = testing)
        testing$squared_error <- (testing$response - testing$prediction)^2
        testing$absolute_error <- abs(testing$response - testing$prediction)
        # Find each respondent's MSE and MAE
        if (subset_candidate == TRUE){
            result <- testing %>%
                group_by(ResponseId, merkel) %>%
                summarize(mse = mean(squared_error), 
                          mae = mean(absolute_error),
                          gender = gender[1])
        } else{
            result <- testing %>%
                group_by(ResponseId) %>%
                summarize(mse = mean(squared_error), 
                          mae = mean(absolute_error),
                          gender = gender[1])
        }
        # Save results
        result$rmse <- sqrt(result$mse)
        result$fold <- fold_names[i]
        results[[fold_names[i]]] <- result
    }
    
    # Combine folds
    combined <- bind_rows(results)
    
    # Average over folds
    if (subset_candidate == TRUE){
        averaged <- combined %>%
            group_by(ResponseId, merkel) %>%
            summarize(mse = mean(mse),
                      rmse = mean(rmse),
                      mae = mean(mae),
                      gender = gender[1])
    } else{
        averaged <- combined %>%
            group_by(ResponseId) %>%
            summarize(mse = mean(mse),
                      rmse = mean(rmse),
                      mae = mean(mae),
                      gender = gender[1])
    }
    return(averaged)
}



# --------------------------------------------------------------
# Read annotations from Lucid sample
lucid <- read.csv('data_annotations_lucid.csv', stringsAsFactors = F)


# --------------------------------------------------------------
# Loop over subsets of respondents and candidates, and save the
# results

# Initalize runs
dvs <- c("happy", "angry", "any_emotion")
candidate <- c("all", "subset")
gender <- c("Female", "Male")

set.seed(42)
runs <- expand.grid(dv = dvs, 
                    candidate = candidate,
                    gender = gender,
                    stringsAsFactors = FALSE)

# Initialze variable to hold the results
runs$metric_value <- NA

results <- c()
for (i in 1:nrow(runs)){
    data <- lucid
    # Who's video clips should be include? (all, merkel, or opponents)
    if (runs$candidate[i] == "all"){
        print("Evaluating performance using all candidates")
        subset_candidates = FALSE
    } else {
        print("Subsetting Merkel and opponents clips.")
        subset_candidates = TRUE
    }
    
    # How should we subset the gender of the respondent? (all, Female, or Male)
    if (runs$gender[i] == "Female") {
        print("Evaluating performance for female respondents.")
        data <- data[data$gender == "Female",]
    } else {
        print("Evaluating performance for male respondents.")
        data <- data[data$gender == "Male",]
    }
    
    # Which emotion should we evaluate?
    if (runs$dv[i] == "happy"){
        print("Evaluating performance for happy.")
        data <- data[data$happy == 1,]
    } else if (runs$dv[i] == "angry") {
        print("Evaluating performance for angry.")
        data <- data[data$angry == 1,]
    } else {
        print("Evaluating performance for any emotion.")
        data <- data[data$any_emotion == 1,]
    }
    
    # Cross-validate
    result <- cross_validate(dat=data,
                             iv=paste(runs$dv[i], 'api', 'prediction', sep="_"),
                             subset_candidate = subset_candidates)
    
    result_label = paste(runs$dv[i], runs$candidate[i], runs$gender[i], sep="_")
    results[[result_label]] <- result
}

dat_lucid <- runs

# Parse RMSE results for plots

# Set up labels and vector to hold results
emotion <- c("happy", "angry", "any_emotion")
candidate <- c("all", "Merkel", "Opponents")
gender <- c("all", "Female", "Male")
data_for_plots <- expand.grid(emotion = emotion, 
                              candidate = candidate,
                              gender = gender,
                              stringsAsFactors = FALSE)
data_for_plots$rmse <- NA
data_for_plots <- data_for_plots[order(data_for_plots$candidate, 
                                       data_for_plots$gender,
                                       data_for_plots$emotion),]

# Extract data for all candidates combined
angry_all <- bind_rows(results$angry_all_Female, results$angry_all_Male)
any_emotion_all <- bind_rows(results$any_emotion_all_Female, results$any_emotion_all_Male)
happy_all <- bind_rows(results$happy_all_Female, results$happy_all_Male)

# Extract data by candidate
angry_candidate <- bind_rows(results$angry_subset_Female, results$angry_subset_Male)
any_emotion_candidate <- bind_rows(results$any_emotion_subset_Female, results$any_emotion_subset_Male)
happy_candidate <- bind_rows(results$happy_subset_Female, results$happy_subset_Male)

# Combine results
# All candidates
data_for_plots$rmse[1] <- mean(angry_all$rmse)
data_for_plots$rmse[2] <- mean(any_emotion_all$rmse)
data_for_plots$rmse[3] <- mean(happy_all$rmse)
data_for_plots$rmse[4] <- mean(angry_all$rmse[angry_all$gender == "Female"])
data_for_plots$rmse[5] <- mean(any_emotion_all$rmse[any_emotion_all$gender == "Female"])
data_for_plots$rmse[6] <- mean(happy_all$rmse[happy_all$gender == "Female"])
data_for_plots$rmse[7] <- mean(angry_all$rmse[angry_all$gender == "Male"])
data_for_plots$rmse[8] <- mean(any_emotion_all$rmse[any_emotion_all$gender == "Male"])
data_for_plots$rmse[9] <- mean(happy_all$rmse[happy_all$gender == "Male"])

# Merkel
data_for_plots$rmse[10] <- mean(angry_candidate$rmse[angry_candidate$merkel==1])
data_for_plots$rmse[11] <- mean(any_emotion_candidate$rmse[any_emotion_candidate$merkel==1])
data_for_plots$rmse[12] <- mean(happy_candidate$rmse[happy_candidate$merkel==1])
data_for_plots$rmse[13] <- mean(angry_candidate$rmse[(angry_candidate$gender == "Female") & (angry_candidate$merkel == 1)])
data_for_plots$rmse[14] <- mean(any_emotion_candidate$rmse[(any_emotion_candidate$gender == "Female") & (any_emotion_candidate$merkel == 1)])
data_for_plots$rmse[15] <- mean(happy_candidate$rmse[(happy_candidate$gender == "Female") & (happy_candidate$merkel == 1)])
data_for_plots$rmse[16] <- mean(angry_candidate$rmse[(angry_candidate$gender == "Male") & (angry_candidate$merkel == 1)])
data_for_plots$rmse[17] <- mean(any_emotion_candidate$rmse[(any_emotion_candidate$gender == "Male") & (any_emotion_candidate$merkel == 1)])
data_for_plots$rmse[18] <- mean(happy_candidate$rmse[(happy_candidate$gender == "Male") & (happy_candidate$merkel == 1)])

# Opponents
data_for_plots$rmse[19] <- mean(angry_candidate$rmse[angry_candidate$merkel==0])
data_for_plots$rmse[20] <- mean(any_emotion_candidate$rmse[any_emotion_candidate$merkel==0])
data_for_plots$rmse[21] <- mean(happy_candidate$rmse[happy_candidate$merkel==0])
data_for_plots$rmse[22] <- mean(angry_candidate$rmse[(angry_candidate$gender == "Female") & (angry_candidate$merkel == 0)])
data_for_plots$rmse[23] <- mean(any_emotion_candidate$rmse[(any_emotion_candidate$gender == "Female") & (any_emotion_candidate$merkel == 0)])
data_for_plots$rmse[24] <- mean(happy_candidate$rmse[(happy_candidate$gender == "Female") & (happy_candidate$merkel == 0)])
data_for_plots$rmse[25] <- mean(angry_candidate$rmse[(angry_candidate$gender == "Male") & (angry_candidate$merkel == 0)])
data_for_plots$rmse[26] <- mean(any_emotion_candidate$rmse[(any_emotion_candidate$gender == "Male") & (any_emotion_candidate$merkel == 0)])
data_for_plots$rmse[27] <- mean(happy_candidate$rmse[(happy_candidate$gender == "Male") & (happy_candidate$merkel == 0)])

# rename object for plotting at a later stage
dat_lucid_rsme <- data_for_plots

# --------------------------------------------------------------
# Estimate statistical models and plot

# Angry
angry_candidate$female <- ifelse(angry_candidate$gender == "Female", 1, 0)
fit_angry <- lm(rmse~female + merkel + female*merkel, data = angry_candidate)
vars <- names(fit_angry$coefficients)
betas <- fit_angry$coefficients
ci_angry <- as.data.frame(cbind(vars, betas, confint(fit_angry), confint(fit_angry, level = 0.90)))
ci_angry$label <- "Angry"

# Any emotion
any_emotion_candidate$female <- ifelse(any_emotion_candidate$gender == "Female", 1, 0)
fit_any_emotion <- lm(rmse~female + merkel + female*merkel, data = any_emotion_candidate)
vars <- names(fit_any_emotion$coefficients)
betas <- fit_any_emotion$coefficients
ci_any_emotion<- as.data.frame(cbind(vars, betas, confint(fit_any_emotion), confint(fit_any_emotion, level = 0.90)))
ci_any_emotion$label <- "Any emotion"

# Happy
happy_candidate$female <- ifelse(happy_candidate$gender == "Female", 1, 0)
fit_happy <- lm(rmse~female + merkel + female*merkel, data = happy_candidate)
vars <- names(fit_happy$coefficients)
betas <- fit_happy$coefficients
ci_happy <- as.data.frame(cbind(vars, betas, confint(fit_happy), confint(fit_happy, level = 0.90)))
ci_happy$label <- "Happy"

# bind dataframes
dat_lucid_coefs_raw <- rbind(ci_angry, ci_any_emotion, ci_happy)


## Based on these results, create plots for appendix

# Figure A03 ----

# nicer labels for all variables
dat_labelbox_recoded <- dat_labelbox %>% 
    mutate(emotion = dplyr::recode(dv,'happy'='Happiness',
                                   'angry'='Anger',
                                   'any_emotion'='Any Emotion')) %>% 
    mutate(candidates = dplyr::recode(clips, 'merkel'='Merkel',
                                      'opponents'='Opponents',
                                      'all'='All'))


# reorder factors
dat_labelbox_recoded$candidates <- factor(dat_labelbox_recoded$candidates,
                                          levels = c("All", "Merkel", "Opponents"))

dat_labelbox_recoded$emotion <- factor(dat_labelbox_recoded$emotion,
                                       levels = c("Any Emotion", "Happiness", "Anger"))



# create plot with F1 scores
p_labelbox_f1 <- dat_labelbox_recoded %>% 
    filter(metric == "F1") %>% 
    ggplot(aes(x = candidates, y = metric_value,
               shape = candidates, colour = candidates)) +
    geom_point(position = position_dodge(width = 0.8),
               size = 4) +
    facet_grid(~emotion) +
    scale_shape_manual(values = c(17, 16, 1)) +
    scale_colour_manual(values = c("black", "grey50", "black")) +
    scale_y_continuous(limits = c(0, 1)) +
    geom_text(aes(label = round(metric_value, 2)), nudge_y = -0.15,
              size = 4.5, colour = "grey40") +
    labs(x = "Candidate", y = "F1 Score", title = "F1 Score") +
    theme(legend.position = "none", plot.title = element_text(face = "bold",
                                                              hjust = 0.5)) 
p_labelbox_f1


# create plot with RMSE 
p_labelbox_rmse <- dat_labelbox_recoded %>% 
    filter(metric == "RMSE") %>% 
    ggplot(aes(x = candidates, y = metric_value,
               shape = candidates, colour = candidates)) +
    geom_point(position = position_dodge(width = 0.8),
               size = 4) +
    facet_grid(~emotion) +
    scale_shape_manual(values = c(17, 16, 1)) +
    scale_colour_manual(values = c("black", "grey50", "black")) +
    scale_y_continuous(limits = c(0.5, 1.5)) +
    geom_text(aes(label = round(metric_value, 2)), nudge_y = -0.15,
              size = 4.5, colour = "grey40") +
    labs(x = "Candidate", y = "RMSE", title = "Root Mean Squared Error (RMSE)") +
    theme(legend.position = "none", 
          plot.title = element_text(face = "bold",
                                    hjust = 0.5))
p_labelbox_rmse

# combine plots and save as Figure A03
plot_grid(p_labelbox_rmse, p_labelbox_f1, nrow = 2)
ggsave("fig_a03.pdf", 
       width = 10, height = 6)


## Figure A04 ----


# create nicer labels for all variables
dat_lucid_recoded <- dat_lucid_rsme %>% 
    mutate(emotion = dplyr::recode(emotion,'happy'='Happiness',
                                   'angry'='Anger',
                                   'any_emotion'='Any Emotion')) %>% 
    mutate(candidates = dplyr::recode(candidate, 'merkel'='Merkel',
                                      'opponents'='Opponents',
                                      'all'='All')) %>% 
    mutate(gender = str_to_title(gender))


# reorder factors
dat_lucid_recoded$candidates <- factor(dat_lucid_recoded$candidates,
                                       levels = c("All", "Merkel", "Opponents"))

dat_lucid_recoded$emotion <- factor(dat_lucid_recoded$emotion,
                                    levels = c("Any Emotion", "Happiness", "Anger"))


# create and save Figure A04 
ggplot(dat_lucid_recoded, 
       aes(x = candidates, y = rmse,
           shape = candidates, colour = candidates)) +
    geom_point(position = position_dodge(width = 0.8),
               size = 4) +
    facet_grid(gender~emotion) +
    scale_shape_manual(values = c(17, 16, 1)) +
    scale_colour_manual(values = c("black", "grey50", "black")) +
    scale_y_continuous(limits = c(0.5, 1.5)) +
    geom_text(aes(label = round(rmse, 2)), nudge_y = -0.15,
              size = 4, colour = "grey40") +
    labs(x = "Candidate", y = "Root Mean Squared Error (RMSE)") +
    theme(legend.position = "none")
ggsave("fig_a04.pdf", 
       width = 10, height = 6)


## Figure A05 ----

# plot coefficients of linear models

# rename column names
names(dat_lucid_coefs_raw)

colnames(dat_lucid_coefs_raw) <- c("vars", "betas",
                                   "ci_95_low", "ci_95_high",
                                   "ci_90_low", "ci_90_high",
                                   "emotion")

# convert some variables to numeric
dat_lucid_coefs_raw$betas <- as.numeric(dat_lucid_coefs_raw$betas)
dat_lucid_coefs_raw$ci_95_low <- as.numeric(dat_lucid_coefs_raw$ci_95_low)
dat_lucid_coefs_raw$ci_95_high <- as.numeric(dat_lucid_coefs_raw$ci_95_high)
dat_lucid_coefs_raw$ci_90_low <- as.numeric(dat_lucid_coefs_raw$ci_90_low)
dat_lucid_coefs_raw$ci_90_high <- as.numeric(dat_lucid_coefs_raw$ci_90_high)

# change labels of Emotions
dat_lucid_coefs_raw <- dat_lucid_coefs_raw %>% 
    mutate(emotion = ifelse(emotion == "Angry", "Anger",
                            ifelse(emotion == "Happy", "Happiness", "Any Emotion")))

# relevel factor
dat_lucid_coefs_raw$emotion <- factor(dat_lucid_coefs_raw$emotion,
                                      levels = c("Any Emotion", "Happiness", "Anger"))


# recode independent variables
dat_lucid_coefs_raw <- dat_lucid_coefs_raw %>% 
    mutate(vars = dplyr::recode(vars,
                                "merkel" = "Merkel",
                                "female" = "Coder: Female",
                                "female:merkel" = "Coder: Female x Merkel"
    )) %>% 
    filter(vars != "(Intercept)") # remove intercept


dat_lucid_coefs_raw$vars <- factor(dat_lucid_coefs_raw$vars,
                                   levels = c("Coder: Female x Merkel", "Merkel",
                                              "Coder: Female"))


# create and save Figure A05
ggplot(dat_lucid_coefs_raw, aes(x = betas, y = vars)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    geom_point(position = position_dodge(width = 0.6),
               size = 3.5) +
    geom_errorbarh(aes(xmin = ci_95_low,
                       xmax =ci_95_high),
                   size = 0.5, height = 0,
                   position = position_dodge(width = 0.6)) +
    geom_errorbarh(aes(xmin = ci_90_low,
                       xmax = ci_90_high),
                   size = 1.3, height = 0,
                   position = position_dodge(width = 0.6))  +
    facet_wrap(~emotion) +
    labs(x = "Estimated Effects for RMSE", y = NULL)
ggsave("fig_a05.pdf", 
       width = 10, height = 3)

