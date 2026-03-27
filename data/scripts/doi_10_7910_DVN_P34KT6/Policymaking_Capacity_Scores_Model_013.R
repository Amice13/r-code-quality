######################
#####            #####
##### 1.0 Set Up #####
#####            #####
######################

##### 1.1 Cleaning the Environment
rm(list = ls())
gc()

##### 1.2 Packages
library(tidyverse)
library(brms)
library(parallel)
library(rstan)
library(stargazer)
library(sandwich)
library(lmtest)
library(survival)
library(survminer)
library(gridExtra)
options(mc.cores = parallel::detectCores())

##### 1.3 Setting Working Directory
setwd("A:/Research/Measuring Workforce Capacity")


##### 1.4 Functions
distplots <- function(x){
plotdata <- data[,x]
colnames(plotdata)[1] <- "var"
    ggplot(plotdata, aes(x=var)) +
              geom_density()+
              geom_vline(aes(xintercept=mean(var,na.rm=TRUE)))
}

`%notin%` <- Negate(`%in%`)

normalize_sample <- function(i,df,mean,sd){
row <- as.numeric(df[i,])
norm <- (row-mean)/sd
return(norm)
}

#############################
#####                   #####
##### 2.0 Cleaning Data #####
#####                   #####
#############################

##### PARTS 2.0 AND 3.0 BELONG TO THE SAME FUNCTION. RUN ESTIMATES IN PART 4.0
##### For running in BRMS, the Data needs to be in long-format. It should contain the following:
# 1. resp: The value of the agency-year for a particular item (i.e. variable of the fedscope data).
# 2. rater: An item in the fedscope dataset.
# 3. item: An agency-year combination.
Capacity_IRT <- function(items,filename){
  
data <- read_csv("ehri_data.csv") %>%
        mutate(POLICYEMPLOYEE_GENRESEARCHINCLUDED_RAWNUM_LOG = ifelse(POLICYEMPLOYEE_GENRESEARCHINCLUDED_RAWNUM_LOG == -Inf,
                                                                      NA,
                                                                      as.numeric(POLICYEMPLOYEE_GENRESEARCHINCLUDED_RAWNUM_LOG)),
               POLICYEMPLOYEE_DC_GENRESEARCHINCLUDED_RAWNUM_LOG = ifelse(POLICYEMPLOYEE_DC_GENRESEARCHINCLUDED_RAWNUM_LOG == -Inf,
                                                                      NA,
                                                                      as.numeric(POLICYEMPLOYEE_DC_GENRESEARCHINCLUDED_RAWNUM_LOG)),
               POLICYEMPLOYEE_GENRESEARCHEXCLUDED_RAWNUM_LOG = ifelse(POLICYEMPLOYEE_GENRESEARCHEXCLUDED_RAWNUM_LOG == -Inf,
                                                                      NA,
                                                                      as.numeric(POLICYEMPLOYEE_GENRESEARCHEXCLUDED_RAWNUM_LOG))) %>%
        mutate(POLICYEMPLOYEE_GENRESEARCHINCLUDED_RAWNUM_LOG = as.numeric(POLICYEMPLOYEE_GENRESEARCHINCLUDED_RAWNUM_LOG),
               POLICYEMPLOYEE_DC_GENRESEARCHINCLUDED_RAWNUM_LOG = as.numeric(POLICYEMPLOYEE_GENRESEARCHINCLUDED_RAWNUM_LOG),
               POLICYEMPLOYEE_GENRESEARCHEXCLUDED_RAWNUM_LOG = as.numeric(POLICYEMPLOYEE_GENRESEARCHEXCLUDED_RAWNUM_LOG))
   
  
##### 2.1 Specify Item Set
total_itemset <- items

##### 2.1 Basic Cleaning Activities
data <- data %>%

#### 2.1.1 Creating an id variable
        mutate(ID = paste(AGYSUB, DATECODE, sep = "-")) %>% 

#### 2.1.2 Subsetting to September Observations
        filter(str_detect(DATECODE, "09$") == TRUE) 

#### 2.1.3 Adjusting Salary Data for Inflation
data$DATECODE <- as.numeric(data$DATECODE)
inflation_data <- data.frame(RATE = c(1.588, 1.553, 1.503, 1.462, 1.439, 1.407,
                                      1.370, 1.325, 1.285, 1.248, 1.202, 1.206,
                                      1.187, 1.151, 1.127, 1.111, 1.093, 1.092, 
                                      1.078, 1.056, 1.031, 1.012, 1, 0.95),
                             DATECODE = c(199809, 199909, 200009, 200109, 200209,
                                          200309, 200409, 200509, 200609, 200709,
                                          200809, 200909, 201009, 201109, 201209,
                                          201309, 201409, 201509, 201609, 201709,
                                          201809, 201909, 202009, 202109))
for(i in 1:nrow(inflation_data)){
  year <- inflation_data$DATECODE[i]
  rate <- inflation_data$RATE[i]
  data$POLICYEMPLOYEE_GENRESEARCHINCLUDED_SALARY_MEAN[data$DATECODE == year] <- (data$POLICYEMPLOYEE_GENRESEARCHINCLUDED_SALARY_MEAN[data$DATECODE == year]*rate)/1000
  data$POLICYEMPLOYEE_GENRESEARCHEXCLUDED_SALARY_MEAN[data$DATECODE == year] <- (data$POLICYEMPLOYEE_GENRESEARCHEXCLUDED_SALARY_MEAN[data$DATECODE == year]*rate)/1000
  rm(year,rate)
}  
rm(inflation_data)

#### 2.1.4 Subsetting to agencies in agency list and those with more than fifteen employees
agylist <- read.csv("AGENCY_LIST.csv")
colnames(agylist)[1] <- "ABBR" 
agylist <- agylist %>%
           pivot_longer(cols = 5:length(agylist),
                        names_to = "DATE",
                        values_to = "FEDSCOPE_ID") %>%
           mutate(DATE = str_remove_all(DATE, "X")) %>%
           mutate(DATE = str_remove_all(DATE, " "),
                  FEDSCOPE_ID = str_remove_all(FEDSCOPE_ID," ")) %>%
           filter(FEDSCOPE_ID != "")
  
 
data <- data %>%
        filter(AGYSUB %in% agylist$FEDSCOPE_ID) %>%
        group_by(AGYSUB) %>%
        mutate(MEAN_EMPLOYMENT = mean(TOTAL_RAWNUM),
               MEAN_POLICYMAKER = mean(POLICYEMPLOYEE_GENRESEARCHEXCLUDED_RAWNUM,na.rm = T)) %>%
        ungroup() 

data <- data %>%
        filter(MEAN_EMPLOYMENT > 15) %>%
        filter(POLICYEMPLOYEE_GENRESEARCHEXCLUDED_RAWNUM > 0) %>%
        filter(MEAN_POLICYMAKER >= 5)


#### 2.1.5 Creating a Set of IDS
agy_ids <- data %>% select(ID, AGYSUB, AGYSUBT, DATECODE)
agy_ids$AGY_ID_NUM <- seq(1, length(unique(agy_ids$ID)))

#### 2.1.6 Selecting Only Needed Variables
total_set <- data %>%
             select(ID, all_of(total_itemset)) 



##### 2.2 Scaling Variables

for(i in 2:length(total_set)){
total_set[,i] <- scale(total_set[,i])
}


##### 2.3 Converting Data to Long Format
total_set <- total_set %>%
             pivot_longer(cols = !ID,
                          names_to = "rater",
                          values_to = "rating")

rater_ids <- data.frame(rater = unique(total_set$rater),
                        RATER_ID_NUM = seq(1, length(unique(total_set$rater))))

##### 2.4 Joining Agency IDs
total_set <- total_set %>%
             left_join(agy_ids) %>% 
             left_join(rater_ids) %>%
             filter(is.na(rating) == FALSE) %>%
             distinct()
     
rm(data,agylist,rater_ids) #Agy_List is reloaded for formatting but removed here to save memory

#####################################
##### 3.0 Specifying STAN Model #####
#####################################

##### 3.1 Prepping Data for STAN

J <- length(unique(total_set$ID)) ## Total Number of Agency--Years
K <- length(unique(total_set$rater)) ## Total Number of Indicators
N <- nrow(total_set)
jj <- total_set$AGY_ID_NUM
kk <- total_set$RATER_ID_NUM
y <- total_set$rating
rm(total_set)
##### 3.2 Running the Model in Stan

model_fit <- stan(file = "model.stan", 
                  data = c("J", "K", "N", "jj", "kk", "y"),
                  iter = 10000,
                  warmup = 5000,
                  chains = 4,
                  cores = 4,
                  seed = "1989")

rm(jj,kk,y)

##### 3.4 Model Diagnostic

#### 3.4.1 Divergent transitions (Should be 0)
check_divergences(model_fit)

#### 3.4.2 Check that Rhat is 1 for all parameters and the Eff Sample
print(model_fit)

#### 3.4.3 Check Parameters
param_sum <- as.data.frame(summary(model_fit))
param_sum <- param_sum[(J+1):nrow(param_sum),1:10]
write.csv(param_sum, paste(filename, "_params.csv", sep =""))
rm(param_sum)
##### 3.5 Normalizing 

#### 3.5.1 Extracting the Sample
samples <- as.matrix(model_fit)

#### 3.5.2 Removing Columns Unrelated to Measures

samples <-as.matrix(samples[,1:J])
samp.mean<-mean(samples) #Storing Sample Mean
samp.sd<-sd(samples) #Storing Sample SD

#### 3.5.3 Creating a New Matrix for Normalized Results
samp.local<-matrix(NA,nrow=nrow(samples),ncol=ncol(samples))

colnames(samp.local)<-colnames(samples)

#### 3.5.4 Normalizing Data Row By Row

for(i in 1:nrow(samples)){
    samp.local[i,]<-(samples[i,]-samp.mean)/samp.sd
}
rm(samples,samp.mean,samp.sd)
#### 3.5.5 Creating a New Matrix For Summary Results
scores.sum<-matrix(NA,nrow=J,ncol=5)
rownames(scores.sum)<-agy_ids$ID
scores.sum[,5]<-rownames(scores.sum)


for(i in 1:nrow(agy_ids)){
  scores.sum[i,1]<-mean(samp.local[,i])
  scores.sum[i,2]<-sd(samp.local[,i])  
  scores.sum[i,3]<-quantile(samp.local[,i],probs=c(0.025))
  scores.sum[i,4]<-quantile(samp.local[,i],probs=c(0.975))
}
colnames(scores.sum)<-c("Capacity_Score","Capacity_Score_SD","Capacity_Score_LB","Capacity_Score_UB","ID")
scores.sum <- as.data.frame(scores.sum)
scores.sum <- scores.sum %>%
              mutate(Capacity_Score = as.numeric(Capacity_Score),
                     Capacity_Score_SD = as.numeric(Capacity_Score_SD),
                     Capacity_Score_LB = as.numeric(Capacity_Score_LB),
                     Capacity_Score_UB = as.numeric(Capacity_Score_UB)) %>%
              left_join(agy_ids)

##### 3.6 Formating and Saving Scores

#### 3.6.1 Loading Agency List
agylist <- read.csv("AGENCY_LIST.csv")
colnames(agylist)[1] <- "ABBR" 
agylist <- agylist %>%
  pivot_longer(cols = 5:length(agylist),
               names_to = "DATE",
               values_to = "FEDSCOPE_ID") %>%
  mutate(DATE = str_remove_all(DATE, "X")) %>%
  mutate(DATE = str_remove_all(DATE, " "),
         FEDSCOPE_ID = str_remove_all(FEDSCOPE_ID," ")) %>%
  filter(FEDSCOPE_ID != "")

#### 3.6.1 Renaming 
scores.sum <- scores.sum %>%
              rename(FEDSCOPE_ID = AGYSUB,
                     DATE = DATECODE,
                     UNIQUE_ID = ID,
                     CAPACITY_SCORE = Capacity_Score,
                     CAPACITY_SD = Capacity_Score_SD,
                     CAPACITY_LOWERBOUND = Capacity_Score_LB,
                     CAPACITY_UPPERBOUND = Capacity_Score_UB) %>%
              select(FEDSCOPE_ID, DATE, UNIQUE_ID, CAPACITY_SCORE, CAPACITY_SD, CAPACITY_LOWERBOUND, CAPACITY_UPPERBOUND)

rows_premerge <- nrow(scores.sum)

#### 3.6.2 Merging 

scores.sum <- scores.sum %>% 
              mutate(DATE = as.character(DATE)) %>%
              left_join(agylist) %>%
              select(UNIQUE_ID, ABBR, AGENCY, BUREAU, AKA,  FEDSCOPE_ID, DATE, CAPACITY_SCORE, CAPACITY_SD, CAPACITY_LOWERBOUND, CAPACITY_UPPERBOUND)

rows_postmerge <- nrow(scores.sum)

rows_postmerge - rows_premerge #Check whether the merge created new rows on accident. Should = 0.
sum(is.na(scores.sum$ABBR)) #Check whether any rows are missing an Abbreviation.



#### 3.6.3 Saving Scores

write.csv(scores.sum, file=paste(filename, "_ests.csv", sep =""))
}

##############################
#####                    #####
##### 4.0 RUNNING MODELS #####
#####                    #####
##############################

#### Model 013
items013 <- c("POLICYEMPLOYEE_GENRESEARCHEXCLUDED_RAWNUM_LOG",
              "POLICYEMPLOYEE_GENRESEARCHEXCLUDED_PROP",
              "POLICYEMPLOYEE_GENRESEARCHEXCLUDED_LOS_MEAN",
              "POLICYEMPLOYEE_GENRESEARCHEXCLUDED_SALARY_MEAN",
              "POLICYEMPLOYEE_GENRESEARCHEXCLUDED_COLLEGE_PROP")
name013 <- "policymaking_scores_m013"
Capacity_IRT(items013,name013)
rm(items013,name013)