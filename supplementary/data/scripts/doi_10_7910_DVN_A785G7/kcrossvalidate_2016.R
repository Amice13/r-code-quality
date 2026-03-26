require(pacman)
pacman::p_load(bigKRLS, update=TRUE)
p_load(grf, # version 0.9.5 used but adapted to task at hand as below
       glmnet, randomForest)

##########################################################
# setting paths - change to reflect your own information #
##########################################################
out_path <- '~/Dropbox/pa_replication/Appendix_E/Appendix E.2/cv_non_krls/kcv_results_cf.RData'
data <- read.csv('~/Dropbox/pa_replication/2016_election_application/2016_election_dataset.csv')

################
# getting data #
################

data$dem_2016_percent <- 100*data$dem_2016/(data$dem_2016 + data$gop_2016)
data$gop_2016_percent <- 100*data$gop_2016/(data$dem_2016 + data$gop_2016)
data$dem_2012_percent <- 100*data$dem_2012/(data$dem_2012 + data$gop_2012)
data$gop_2012_percent <- 100*data$gop_2012/(data$dem_2012 + data$gop_2012)

data$white_population[is.na(data$white_population)] <- 0
data$latino_population[is.na(data$latino_population)] <- 0
data$black_population[is.na(data$black_population)] <- 0
data$asian_population[is.na(data$asian_population)] <- 0

data$percent_white <- 100*data$white_population/data$total_population
data$percent_latino <- 100*data$latino_population/data$total_population
data$percent_black <- 100*data$black_population/data$total_population
data$percent_asian <- 100*data$asian_population/data$total_population

data$all_mortality_2009.2011 <- data$all_mortality_2009.2011/100
data$all_mortality_2013.2015 <- data$all_mortality_2013.2015/100

data$all_2013.2015_despair_mortality <- data$all_2013.2015_despair_mortality/100

data$mortality_delta <- data$all_mortality_2013.2015 - data$all_mortality_2009.2011

data$gop_2016_delta <- data$gop_2016_percent - data$gop_2012_percent

data$percent_poverty <- 100*data$POVALL_2015/data$total_population

data$Median_Household_Income_2015 <- data$Median_Household_Income_2015/10000
data$AGE050210D <- data$AGE050210D/10

# creating the model preliminaries
X <- data.frame('all_mortality' = data$all_mortality_2013.2015)
X$mortality_delta <- data$mortality_delta

#X <- data.frame('despair_mortality' = data$all_2013.2015_despair_mortality)

X$unemployment <- data$Unemployment_rate_2015
X$rural <- data$Rural.urban_Continuum_Code_2013

X$age <- data$AGE050210D
X$income <- data$Median_Household_Income_2015
X$poverty <- data$percent_poverty

X$high_school_dropout <- data$Percent.of.adults.with.less.than.a.high.school.diploma..2011.2015
X$high_school_grad <- data$Percent.of.adults.with.a.high.school.diploma.only..2011.2015
X$some_college <- data$Percent.of.adults.completing.some.college.or.associate.s.degree..2011.2015
X$college_grad <- data$Percent.of.adults.with.a.bachelor.s.degree.or.higher..2011.2015

X$percent_white <- data$percent_white
X$percent_latino <- data$percent_latino
X$percent_black <- data$percent_black
X$percent_asian <- data$percent_asian

X$lat <- data$lat
X$lon <- data$lon

# alaska is the excluded category
states <- model.matrix(~data$state)[,2:51]
colnames(states) <- sort(unique(data$state))[2:51]

X <- cbind(X, states)

# fitting the model
gop_2016_delta <- data$gop_2016_delta
complete <- complete.cases(X) & !is.na(gop_2016_delta)
gop_2016_delta <- gop_2016_delta[complete]
X <- X[complete,]
X <- as.matrix(X)

census <- read.csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv", 
                   stringsAsFactors = FALSE)

state <- colnames(X)[18:67][apply(X[,18:67], 1, function(x) which(x == 1))]
# region <- census$Region[match(state, census$State.Code)]
division <- census$Division[match(state, census$State.Code)]
r <- matrix(nrow=nrow(X), ncol=length(unique(census$Division)), 0)
r[cbind(1:nrow(r), match(division, unique(census$Division)))] <- 1
X <- cbind(X[,1:17], r)
colnames(X)[18:ncol(X)] <- unique(census$Division)
X <- X[,-2]

y <- as.matrix(gop_2016_delta)

K <- 5
N <- nrow(X)

##########################
# building folds indices #
##########################
folds <- matrix(NA, nrow=100, ncol=N)
for(i in 1:100){
  set.seed(i)
  folds[i,] <- as.integer(cut(sample(N), breaks = 5))
}

##############
# Model runs #
##############

split_data <- function(X, Y, train_inds){
  train_x <- X[train_inds,]
  train_y <- Y[train_inds,]
  
  test_x <- X[!train_inds,]
  test_y <- Y[!train_inds,]
  
  return(list('train_x'=train_x, 'train_y'=train_y, 'test_x'=test_x, 'test_y'=test_y))
}

rmse <- function(y, yhat){
  return(sqrt(mean((y - yhat)^2)))
}

cf_est <- function(df, seed, f_id, treat_ind = 1){
  c_forest <- causal_forest(df[['train_x']][,(1:ncol(df[['train_x']])) != treat_ind], 
                            df[['train_y']],
                            df[['train_x']][,treat_ind])
  
  xy_forest <- regression_forest(df[['train_x']][,(1:ncol(df[['train_x']])) != treat_ind], 
                                 df[['train_y']])
  xw_forest <- regression_forest(df[['train_x']][,(1:ncol(df[['train_x']])) != treat_ind], 
                                 df[['train_x']][,treat_ind])
  
  p_y <- predict(xy_forest, df[['test_x']][,(1:ncol(df[['train_x']])) != treat_ind])$predictions
  p_w <- predict(xw_forest, df[['test_x']][,(1:ncol(df[['train_x']])) != treat_ind])$predictions
  p_tau <- predict(c_forest, df[['test_x']][,(1:ncol(df[['train_x']])) != treat_ind])$predictions
  
  p_is <- c_forest$Y.hat + (df[['train_x']][,treat_ind] - c_forest$W.hat)*predict(c_forest)$predictions
  p_oos <- p_y + (df[['test_x']][,treat_ind] - p_w)*p_tau
  
  return(data.frame('Model'= 'CF', 'Seed'=seed, 'Fold'=f_id, 'RMSE'=rmse(p_is, df[['train_y']]), 'RMSPE'=rmse(p_oos, df[['test_y']])))
}

rf_pred <- function(df, seed, f_id){
  fit <- tuneRF(df[['train_x']], df[['train_y']], doBest=TRUE, trace=FALSE, plot=FALSE)
  p_oos <- predict(fit, df[['test_x']])
  p_is <- fit$predicted
  
  return(data.frame('Model'= 'RF', 'Seed'=seed, 'Fold'=f_id, 'RMSE'=rmse(p_is, df[['train_y']]), 'RMSPE'=rmse(p_oos, df[['test_y']])))
}

glmnet_pred <- function(df, alpha, seed, f_id, interactions=FALSE){
  if(alpha == 0){
    model <- 'LASSO'
  } else if(alpha == 1){
    model <- 'Ridge'
  } else{
    model <- 'Elastic'
  }
  
  if(interactions){
    MM_train <- model.matrix(~.^2, data = data.frame(df[['train_x']]))
    MM_test <- model.matrix(~.^2, data= data.frame(df[['test_x']]))
    
    model <- paste(model, '(2-way)')
  } else{
    MM_train <- df[['train_x']]
    MM_test <- df[['test_x']]
  }
  
  fit <- cv.glmnet(MM_train, df[['train_y']], alpha=alpha)
  p_oos <- predict(fit, MM_test)
  p_is <- predict(fit, MM_train)
  
  return(data.frame('Model'= model, 'Seed'=seed, 'Fold'=f_id, 'RMSE'=rmse(p_is, df[['train_y']]), 'RMSPE'=rmse(p_oos, df[['test_y']])))
}

sparsereg_pred <- function(df, seed, f_id){
  # from docs, EM is faster and returns point ests only, which is what we care about for prediction (I think?)
  # TX seems to give two-way interactions, though i'm not certain
  # gibbs/other sampling parameters?
  # no out-of-sample prediction function?
  
  fit <- sparsereg(df[['train_y']], df[['train_x']], EM=TRUE)
}

out <- data.frame('Model'=character(), 'RMSE'=numeric(), 'RMSPE'=numeric(), 'Seed'=numeric(), 'Fold'=numeric(),
                  stringsAsFactors = FALSE)
for(i in 1:100){
  for(j in 1:5){
    timestamp()
    print(paste('Starting seed ', i, ' fold ', j, '...', sep=''))
    
    train_inds <- folds[i,] != j
    to_fit <- split_data(X, y, train_inds)

    out <- rbind(out,
                 cf_est(to_fit, i, j),
                 #sparsereg_pred(to_fit, i, j),
                 rf_pred(to_fit, i, j),
                 glmnet_pred(to_fit, 0, i, j),
                 glmnet_pred(to_fit, 1, i, j),
                 glmnet_pred(to_fit, 0.5, i, j),
                 glmnet_pred(to_fit, 0, i, j, interactions=TRUE),
                 glmnet_pred(to_fit, 1, i, j, interactions=TRUE),
                 glmnet_pred(to_fit, 0.5, i, j, interactions=TRUE))
  }
}

save(out, file = out_path)

