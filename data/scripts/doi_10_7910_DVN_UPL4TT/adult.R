######################################################################
                                                                  
##      PROJECT TITLE:    MIDAS                                        
##      CODE AUTHOR:      THOMAS S. ROBINSON                        
                                                                  
##      DESCRIPTION:      ADULT DATASET TEST (APPLIED ACCURACY TEST)
                                                                  
######################################################################

#### Packages & Init ####
library(dplyr)
library(readr)
library(purrr)
library(mi)
library(betareg)
library(Amelia)
library(mvnmle)
library(MASS)
library(norm2)
library(nnet)
library(arm)
library(parallel)

set.seed(89)

if (!file.exists("adult/data_tmp")) {
  dir.create("adult/data_tmp")
} else {
  unlink("adult/data_tmp", recursive = TRUE)
  dir.create("adult/data_tmp")
}

#### Functions ####
minmax <- function(x) {
  
  xmin <- min(x, na.rm = TRUE)
  xmax <- max(x, na.rm = TRUE)
  return((x-xmin)/(xmax-xmin))
  
}

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

one_hot <- function(data, cats, cont, direction = "encode", decode_cats = NA) {
  
  if (direction == "encode") {
    
    cats <- cats[cats %in% colnames(data)]
    
    df_cont <- data[,cont]
    df_cats <- data[,cats]
    
    dummies <- matrix(nrow = nrow(df_cats), ncol = 0)
    
    for (cat in cats) {
      
      cat_vec <- df_cats[[cat]]
      
      levels <- unique(df_cats[[cat]])
      levels <- levels[!is.na(levels)]
      tmp <- matrix(nrow = nrow(df_cats), ncol = length(levels))
      colnames(tmp) <- paste0(cat,"_",levels)
      
      for (col in 1:ncol(tmp)) {
        
        tmp[,col] <- ifelse(cat_vec  == levels[col],1,0)
        tmp[,col] <- ifelse(is.na(cat_vec),NA,tmp[,col])
        
      }
      dummies <- cbind(dummies, tmp)
    }
    
    return(cbind(df_cont, dummies))
  } else if (direction == "decode") {
    
    if (is.na(decode_cats[1])) {stop("Decoded category names needed.")}
    
    df_cont <- data[,cont]
    
    decoded <- matrix(nrow = nrow(data), ncol = 0)
    
    for (cat in decode_cats) {
      
      cols <- grep(cat,colnames(data))
      tmp <- data[,cols]
      levels <- colnames(tmp)
      
      new_col <- rep(NA,nrow(data))
      
      col_max <- apply(tmp,1,max)
      
      for (c in 1:ncol(tmp)) {
         new_col <- ifelse(tmp[,c] == col_max, levels[c],new_col)
      }
      
      new_col <- gsub(paste0(cat,"_"),"", new_col)
      
      decoded <- cbind(decoded, new_col)
    }
    
    colnames(decoded) <- decode_cats
    
    return(cbind(df_cont, decoded))
    
    # For each var in cats, get variables, squish down so only 1
  } else {
    print("direction must be either 'encode' or 'decode'")
    return(data)
  }
}

data_gen <- function(data, type, miss_prop) {
  
  if (!(type %in% c("mcar","mar","mnar","full"))) {
    stop("Type must be one of 'mcar','mar', and 'mnar'")
  }
    
  y <- data$class_labels
  
  test_data <- data

  cols <- sample(1:ncol(test_data), size = miss_prop*ncol(test_data), replace =  FALSE)
  
  # MCAR procedure
  if (type == "mcar") {
    for (c in cols) {
      # R is drawn from Bernoulli with equal probability of success/failure
      r <- rbernoulli(nrow(test_data), p = 0.5)
      
      if (class(test_data[[c]]) == "factor") {
        test_data[,c] <- factor(ifelse(r, NA, levels(test_data[[c]])[test_data[[c]]]), exclude = NA, levels(data[[c]]))
      } else {
        test_data[,c] <- ifelse(r, NA, test_data[[c]])
      }
      
    }
  } 
  # MAR procedure
  else if (type == "mar") {
    
    # Choose latent column from among those not selected to be missing
    l_col <- sample(which(!(1:ncol(test_data)  %in% cols)),1)
    
    if (class(test_data[[l_col]]) == "numeric") {
      cut <- median(test_data[[l_col]])
      r  <- (test_data[[l_col]] <= cut)
      r_missing <- which(r)
      
      # Prevent overly-missing data for cases of capital_... where >90% = 0.
      r_sample <- sample(r_missing, ceiling(nrow(test_data)/2), replace = FALSE)
      r[r_missing] <- ifelse(r_missing %in% r_sample, TRUE, FALSE)
      
    } else if (class(test_data[[l_col]]) == "factor") {
      r_cats <- sample(levels(test_data[[l_col]]),ceiling(length(levels(test_data[[l_col]]))/2))
      r <- (test_data[[l_col]] %in% r_cats)
      
      # Prevent overly missing where random sample of categories is more than 50% of dataset
      if (sum(r) > 0.5*nrow(test_data)) {
        r_missing <- which(r)
        r_sample <- sample(r_missing, ceiling(0.5*nrow(test_data)), replace = FALSE)
        r[r_missing] <- ifelse(r_missing %in% r_sample, TRUE, FALSE)
      }
    }
    
    for (c in cols) {
      if (class(test_data[[c]]) == "factor") {
        test_data[,c] <- factor(ifelse(r, NA, levels(test_data[[c]])[test_data[[c]]]), exclude = NA, levels(data[[c]]))
      } else {
        test_data[,c] <- ifelse(r,NA,test_data[[c]])
      }
    }
  } 
  
  # MNAR procedure
  else if (type == "mnar") {
    for (c in cols) {
      if (class(test_data[[c]]) == "numeric") {
        cut <- median(test_data[[c]])
        r  <- (test_data[[c]] <= cut)
        r_missing <- which(r)
        r_sample <- sample(r_missing, ceiling(nrow(data)/2), replace = FALSE)
        r[r_missing] <- ifelse(r_missing %in% r_sample, TRUE, FALSE)
        test_data[,c] <- ifelse(r,NA, test_data[[c]])
        
      } else if (class(test_data[[c]]) == "factor") {
        r_cat <- get_mode(test_data[[c]])
        r <- (test_data[[c]] == r_cat)
        r_missing <- which(r)
        r_sample <- sample(r_missing, ceiling(0.05*length(r_missing)), replace = FALSE)
        r[r_sample] <- FALSE
        test_data[,c] <- factor(ifelse(r, NA, levels(test_data[[c]])[test_data[[c]]]), exclude = NA, levels(data[[c]]))
      }
    }
  }
  
  missing.cols <- colnames(test_data)[cols]
  missing.rows <- as.data.frame(is.na(test_data[, missing.cols]))
  
  return(list(data = test_data,
              missing.cols = missing.cols,
              missing.rows = missing.rows))
  
}

imp_acc <- function(missing_data, complete_data, imputed_data) {
  
  missing_cols <- missing_data$missing.cols
  missing_rows <- missing_data$missing.rows
  
  cont.rmse <- c()
  cat.correct  <- c()
    
  for (col in missing_cols) {
    complete_col <- complete_data[[col]][missing_rows[[col]]]
    imputed_col <- imputed_data[[col]][missing_rows[[col]]]
    
    if (class(complete_data[[col]]) == "numeric") {
      diff <- complete_col - imputed_col
      rmse_col <- sqrt(mean(diff^2))
      cont.rmse[length(cont.rmse)+1] <- rmse_col
    } else {
      correct_col <- sum(complete_col == imputed_col)/length(complete_col)
      cat.correct[length(cat.correct)+1] <- correct_col
    }
  }
  
  cont.rmse <- mean(cont.rmse)
  cat.correct <- mean(cat.correct)
  return(c(cont.acc = cont.rmse, cat.acc = cat.correct))
}

norm_factors <- function(imp, missing_data, cats2) {
  df_cont <- imp[,!(colnames(imp) %in% cats2)]
  df_cat <- imp[,cats2]
  
  df_cat_new <- matrix(nrow = nrow(df_cat), ncol = ncol(df_cat))
  colnames(df_cat_new) <- colnames(df_cat)
  
  for (c in cats2)  {
    if (c %in% missing_data$missing.cols) {
      cmax <- nlevels(missing_data$data[[c]])
      
      imp_vec <- round(df_cat[,c])
      imp_vec <- ifelse(imp_vec < 1, 1, imp_vec)
      imp_vec <- ifelse(imp_vec > cmax, cmax, imp_vec)
      
      df_cat_new[,c] <- levels(missing_data$data[[c]])[imp_vec]
      
    } else {
      df_cat_new[,c] <- levels(missing_data$data[[c]])[df_cat[,c]]  
    }
    
  }
  return(cbind(df_cat_new, df_cont))
}

as_df <- function(data, complete_data) {
  
  if (class(data) == "matrix") {
    new_df <- as.data.frame(matrix(ncol = ncol(data), nrow = nrow(data)))
    names <- colnames(complete_data)
    colnames(new_df) <- names
    
    
    for (c in names) {
      if (class(complete_data[[c]]) == "factor") {
        new_df[[c]] <- factor(data[,c], levels = levels(complete_data[[c]]))
      } else {
        new_df[[c]] <- as.numeric(data[,c])
      }
    }
    return(new_df)
    
  } else {
    stop("data must be a matrix")
  }
}

#### Data setup ####

adult_data <- read_csv("data/adult_data.csv")

cats <- c("workclass","education","marital_status","occupation",
          "relationship","race","native_country","class_labels","sex")

cats2 <- cats[!(cats %in% c("native_country","education","occupation"))]

cont <- c("age","fnlwgt","education_num","capital_gain","capital_loss",
          "hours_per_week")

complete_data <- adult_data %>% 
  filter(complete.cases(.)) %>% 
  dplyr::select(cats2,cont) %>% 
  mutate_at(cont, minmax) %>% 
  mutate_at(cats2, as.factor)

f1 <- as.formula("class_labels ~ .")

complete_mod <- glm(f1, data = complete_data, family = binomial(link = "logit"))

#### LOOP parameters ####

result_names <- c("iteration","miss_prop","type","method","time_elapsed",
                  "cont.acc","cat.acc","coef2.mns", "fit.all","fit.av", "coefficients")

results <- matrix(ncol = length(result_names), nrow = 0)
colnames(results) <- result_names

types <- c("mcar","mar","mnar")
miss_props <- c(0.3,0.5,0.7,0.9)
I <- 10 # No. of imputations per MI method
L <- 5 # No. of loops per type/miss_prop combo

#### LOOP ####
for (l in 1:L) {
  for (type in types) {
    for (miss_prop in miss_props) {
      
      print(paste0(l,"-",type,"-",miss_prop))
      
      missing_data <- data_gen(complete_data, type, miss_prop)
      
      print("Data generated")
      
      #### Listwise deletion ####
      
      complete_rows <- complete.cases(missing_data$data) # Must be kept for entire loop!
      
      lw_t0 <- proc.time()
      lwd <- missing_data$data[complete_rows,]
      lw_t1 <- proc.time()
      
      lwd_mod <- NA
      try(lwd_mod <- glm(f1, data = lwd, family = "binomial"), silent = TRUE)
      
      lwd_results <- list()
      lwd_results$time <- lw_t1[3]-lw_t0[3]
      lwd_results$fit.all <- NA
      lwd_results$cont.acc <- NA
      lwd_results$cat.acc <- NA
      
      lwd_results$coef2.mns <- NA
      try(lwd_results$coef2.mns <- mahalanobis(x=coef(complete_mod), center=coef(lwd_mod), cov=vcov(complete_mod)),
          silent = TRUE)
      
      lwd_results$fit.av <- NA
      try(lwd_results$fit.av <- sqrt(mean((complete_mod$fitted.values[complete_rows] - lwd_mod$fitted.values)^2)),
          silent = TRUE)
      
      lwd_results$coefficients <- NA
      try(lwd_results$coefficients <- length(coef(lwd_mod)), silent = TRUE)
      
      results <- rbind(results, c(l, miss_prop, type, "lwd", lwd_results$time,
                                  lwd_results$cont.acc, lwd_results$cat.acc,
                                  lwd_results$coef2.mns,
                                  lwd_results$fit.all, lwd_results$fit.av,
                                  lwd_results$coefficients))
      
      rm(lwd, lw_t0, lw_t1, lwd_mod, lwd_results)
      
      
      print("LWD Done")
      
      #### Amelia ####
      
      amelia_t0 <- proc.time()
      # NB: 1% ridge prior added for cases when missingness induces collinearity
      amelia <- amelia(as.data.frame(missing_data$data), noms = cats2, m = I,
                       empri = .01*nrow(missing_data$data), incheck = FALSE) 
      amelia_t1 <- proc.time()
      
      amelia_pooled <- pool(f1, amelia$imputations, m=I, FUN=glm, family=binomial(link="logit"))
      
      amelia_results <- list()
      amelia_results$time <- amelia_t1[3] - amelia_t0[3]
      
      amelia_acc <- matrix(ncol = 2, nrow = I)
      colnames(amelia_acc) <- c("cont.acc","cat.acc")
      amelia_sum_fit_all <- 0
      amelia_sum_fit_av <- 0
      
      for (i in 1:I) {
        amelia_acc[i,] <- imp_acc(missing_data, complete_data, amelia$imputations[[i]])
        amelia_sum_fit_all <- amelia_sum_fit_all + amelia_pooled@models[[i]]$fitted.values
        amelia_sum_fit_av <- amelia_sum_fit_av + amelia_pooled@models[[i]]$fitted.values[complete_rows]
      }
      
      amelia_results$cont.acc <- mean(amelia_acc[,"cont.acc"])
      amelia_results$cat.acc <- 1 - mean(amelia_acc[,"cat.acc"])
      amelia_results$coef2.mns <- mahalanobis(x=coef(complete_mod), center=amelia_pooled@coefficients, cov=vcov(complete_mod))
      amelia_results$fit.all <- sqrt(mean((complete_mod$fitted.values - (amelia_sum_fit_all/I))^2))
      amelia_results$fit.av <- sqrt(mean((complete_mod$fitted.values[complete_rows] - (amelia_sum_fit_av/I))^2))
      amelia_results$coefficients <- length(amelia_pooled@coefficients)
      
      results <- rbind(results, c(iteration = l, 
                                  miss_prop = miss_prop, 
                                  type = type, 
                                  method = "amelia", 
                                  time_elapsed = as.numeric(amelia_results$time),
                                  cont.acc = amelia_results$cont.acc, 
                                  cat.acc = amelia_results$cat.acc,
                                  coef2.mns = amelia_results$coef2.mns,
                                  fit.all = amelia_results$fit.all, 
                                  fit.av = amelia_results$fit.av,
                                  coefficients = amelia_results$coefficients))
      
      rm(amelia, amelia_t0, amelia_t1, amelia_pooled, amelia_results,
         amelia_acc, amelia_sum_fit_all, amelia_sum_fit_av)
      
      print("Amelia done")
      
      #### norm ####
      
      norm_t0 <- proc.time()
      emResult <- emNorm(missing_data$data, iter.max = 1000, estimate.worst =  FALSE)
      norm_prov <- list()
      for (m in 1:I) {
        print(paste0("Norm iteration: ",m))
        mcmcResult  <- mcmcNorm(emResult)
        norm_prov[[m]] <- impNorm(mcmcResult)  
      }
      norm_t1 <- proc.time()
      
      norm <- list()
      for (m in 1:I) {
        norm[[m]] <- as_df(norm_factors(norm_prov[[m]],missing_data,cats2), complete_data)
      }
      
      norm_pooled <- pool(f1, norm, m=I, FUN=glm, family=binomial(link="logit"))
      
      norm_results <- list()
      norm_results$time <- norm_t1[3] - norm_t0[3]
      
      norm_acc <- matrix(ncol = 2, nrow = I)
      colnames(norm_acc) <- c("cont.acc","cat.acc")
      norm_sum_fit_all <- 0
      norm_sum_fit_av <- 0
      
      for (i in 1:I) {
        norm_acc[i,] <- imp_acc(missing_data, complete_data, norm[[i]])
        norm_sum_fit_all <- norm_sum_fit_all + norm_pooled@models[[i]]$fitted.values
        norm_sum_fit_av <- norm_sum_fit_av + norm_pooled@models[[i]]$fitted.values[complete_rows]
      }
      
      norm_results$cont.acc <- mean(norm_acc[,"cont.acc"])
      norm_results$cat.acc <- 1 - mean(norm_acc[,"cat.acc"])
      norm_results$coef2.mns <- mahalanobis(x=coef(complete_mod), center=norm_pooled@coefficients, cov=vcov(complete_mod))
      norm_results$fit.all <- sqrt(mean((complete_mod$fitted.values - (norm_sum_fit_all/I))^2))
      norm_results$fit.av <- sqrt(mean((complete_mod$fitted.values[complete_rows] - (norm_sum_fit_av/I))^2))
      norm_results$coefficients <- length(norm_pooled@coefficients)
      
      results <- rbind(results, c(iteration = l, 
                                  miss_prop = miss_prop, 
                                  type = type, 
                                  method = "norm", 
                                  time_elapsed = as.numeric(norm_results$time),
                                  cont.acc = norm_results$cont.acc, 
                                  cat.acc = norm_results$cat.acc,
                                  coef2.mns = norm_results$coef2.mns,
                                  fit.all = norm_results$fit.all, 
                                  fit.av = norm_results$fit.av,
                                  coefficients = norm_results$coefficients))
      
      rm(emResult, mcmcResult, norm, norm_acc, norm_pooled, norm_prov, norm_results)
      
      print("Norm done")
      
      #### mi - set up for both MNL and MCAR ####
      
      # Use all cores available!
      options(mc.cores = detectCores())
      
      mi_data <- as.data.frame(missing_data$data) %>% 
        missing_data.frame(.) %>% 
        change(., y = c("age","fnlwgt","education_num","capital_gain","capital_loss","hours_per_week"),
               what = "type",  to = "continuous")
      
      ## mi - MNL
      mnl_data <- mi_data
      mnl_data@variables[["class_labels"]]@imputation_method <- "ppd"
    
      mnl_t0 <- proc.time()
      mnl <- mi(mnl_data, n.iter = 15, n.chains = I)
      mnl_t1 <- proc.time()
      
      mnl_imps <- complete(mnl,I)
      
      for (i in 1:I) {
        mnl_imps[[i]] <- mnl_imps[[i]][,!grepl("missing_",colnames(mnl_imps[[i]]))]
      }
    
      mnl_pooled <- pool(f1, mnl_imps, m=I, FUN=glm, family=binomial(link="logit"))
      
      mnl_results <- list()
      mnl_results$time <- mnl_t1[3] - mnl_t0[3]
      
      mnl_acc <- matrix(ncol = 2, nrow = I)
      colnames(mnl_acc) <- c("cont.acc","cat.acc")
      mnl_sum_fit_all <- 0
      mnl_sum_fit_av <- 0
      
      for (i in 1:I) {
        mnl_acc[i,] <- imp_acc(missing_data, complete_data, mnl_imps[[i]])
        mnl_sum_fit_all <- mnl_sum_fit_all + mnl_pooled@models[[i]]$fitted.values
        mnl_sum_fit_av <- mnl_sum_fit_av + mnl_pooled@models[[i]]$fitted.values[complete_rows]
      }
      
      mnl_results$cont.acc <- mean(mnl_acc[,"cont.acc"])
      mnl_results$cat.acc <- 1 - mean(mnl_acc[,"cat.acc"])
      
      mnl_results$coef2.mns <- mahalanobis(x=coef(complete_mod),center=mnl_pooled@coefficients,cov=vcov(complete_mod))
      mnl_results$fit.all <- sqrt(mean((complete_mod$fitted.values - (mnl_sum_fit_all/I))^2))
      mnl_results$fit.av <- sqrt(mean((complete_mod$fitted.values[complete_rows] - (mnl_sum_fit_av/I))^2))
      mnl_results$coefficients <- length(mnl_pooled@coefficients)
      
      results <- rbind(results, c(iteration = l, 
                                  miss_prop = miss_prop, 
                                  type = type, 
                                  method = "mnl", 
                                  time_elapsed = as.numeric(mnl_results$time),
                                  cont.acc = mnl_results$cont.acc, 
                                  cat.acc = mnl_results$cat.acc,
                                  coef2.mns = mnl_results$coef2.mns,
                                  fit.all = mnl_results$fit.all, 
                                  fit.av = mnl_results$fit.av,
                                  coefficients = mnl_results$coefficients))
      
      rm(mnl, mnl_t0, mnl_t1, mnl_pooled, mnl_results, mnl_sum_fit_av, mnl_sum_fit_all,
         mnl_acc, mnl_imps, mnl_data)
      
      ## mi - MCAR
      mcar_data <- mi_data
      
      mcar_t0 <- proc.time()
      mcar <- mi(mcar_data, n.iter = 0, n.chains = I)
      mcar_t1 <- proc.time()
      
      mcar_imps <- complete(mcar,I)
      
      for (i in 1:I) {
        mcar_imps[[i]] <- mcar_imps[[i]][,!grepl("missing_",colnames(mcar_imps[[i]]))]
      }
      
      mcar_pooled <- pool(f1, mcar_imps, m=I, FUN=glm, family=binomial(link="logit"))
      
      mcar_results <- list()
      mcar_results$time <- mcar_t1[3] - mcar_t0[3]
      
      mcar_acc <- matrix(ncol = 2, nrow = I)
      colnames(mcar_acc) <- c("cont.acc","cat.acc")
      mcar_sum_fit_all <- 0
      mcar_sum_fit_av <- 0
      
      for (i in 1:I) {
        mcar_acc[i,] <- imp_acc(missing_data, complete_data, mcar_imps[[i]])
        mcar_sum_fit_all <- mcar_sum_fit_all + mcar_pooled@models[[i]]$fitted.values
        mcar_sum_fit_av <- mcar_sum_fit_av + mcar_pooled@models[[i]]$fitted.values[complete_rows]
      }
      
      mcar_results$cont.acc <- mean(mcar_acc[,"cont.acc"])
      mcar_results$cat.acc <- 1 - mean(mcar_acc[,"cat.acc"])
      mcar_results$coef2.mns <- mahalanobis(x=coef(complete_mod), center=mcar_pooled@coefficients, cov=vcov(complete_mod))
      mcar_results$fit.all <- sqrt(mean((complete_mod$fitted.values - (mcar_sum_fit_all/I))^2))
      mcar_results$fit.av <- sqrt(mean((complete_mod$fitted.values[complete_rows] - (mcar_sum_fit_av/I))^2))
      mcar_results$coefficients <- length(mcar_pooled@coefficients)
        
      results <- rbind(results, c(iteration = l, 
                                  miss_prop = miss_prop, 
                                  type = type, 
                                  method = "mcar", 
                                  time_elapsed = as.numeric(mcar_results$time),
                                  cont.acc = mcar_results$cont.acc, 
                                  cat.acc = mcar_results$cat.acc,
                                  coef2.mns = mcar_results$coef2.mns,
                                  fit.all = mcar_results$fit.all, 
                                  fit.av = mcar_results$fit.av,
                                  coefficients = mcar_results$coefficients))
      
      rm(mcar, mcar_t0, mcar_t1, mcar_pooled, mcar_results, mcar_sum_fit_av, mcar_sum_fit_all,
         mcar_acc, mcar_imps, mcar_data, mi_data)
      
      print("mi done")
      
      #### MIDAS ####
      filename <- paste0("adult/data_tmp/adult_midas_data.csv")
      write_csv(missing_data$data, filename)
      
      markerfile <- "adult/data_tmp/marker.csv"
      file.create(markerfile)
      
      midas_t0 <- proc.time()
      
      while (file.exists(markerfile)) {
        Sys.sleep(1)
      }
      midas_t1 <- proc.time()

      # Read in MIDAS dataframes
      print("Commencing with MIDAS")
      midas_imps <- list()
      
      for (i in 1:I) {
        imp_file <- paste0("adult/data_tmp/midas_imp_",i,".csv")
        decoded <- one_hot(read_csv(imp_file), cats, cont, direction = "decode", decode_cats  = cats2)
        midas_imps[[i]] <- decoded[,c(cats2,cont)]
        file.remove(imp_file)
      }
      file.remove(filename)
      
      midas_pooled <- pool(f1, midas_imps, m=I, FUN=glm, family=binomial(link="logit"))
      
      midas_results <- list()
      midas_results$time <- midas_t1[3] - midas_t0[3]
      
      midas_acc <- matrix(ncol = 2, nrow = I)
      colnames(midas_acc) <- c("cont.acc","cat.acc")
      midas_sum_fit_all <- 0
      midas_sum_fit_av <- 0
      
      for (i in 1:I) {
        midas_acc[i,] <- imp_acc(missing_data, complete_data, midas_imps[[i]])
        midas_sum_fit_all <- midas_sum_fit_all + midas_pooled@models[[i]]$fitted.values
        midas_sum_fit_av <- midas_sum_fit_av + midas_pooled@models[[i]]$fitted.values[complete_rows]
      }
      
      midas_results$cont.acc <- mean(midas_acc[,"cont.acc"])
      midas_results$cat.acc <- 1 - mean(midas_acc[,"cat.acc"])
      midas_results$coef2.mns <- mahalanobis(x=coef(complete_mod), center=midas_pooled@coefficients, cov=vcov(complete_mod))
      midas_results$fit.all <- sqrt(mean((complete_mod$fitted.values - (midas_sum_fit_all/I))^2))
      midas_results$fit.av <- sqrt(mean((complete_mod$fitted.values[complete_rows] - (midas_sum_fit_av/I))^2))
      midas_results$coefficients <- length(midas_pooled@coefficients)
        
      results <- rbind(results, c(iteration = l, 
                                  miss_prop = miss_prop, 
                                  type = type, 
                                  method = "midas", 
                                  time_elapsed = as.numeric(midas_results$time),
                                  cont.acc = midas_results$cont.acc, 
                                  cat.acc = midas_results$cat.acc,
                                  coef2.mns = midas_results$coef2.mns,
                                  fit.all = midas_results$fit.all, 
                                  fit.av = midas_results$fit.av,
                                  coefficients = midas_results$coefficients))
      
      rm(midas_t0, midas_t1, midas_pooled, midas_results, midas_sum_fit_av, midas_sum_fit_all,
         midas_acc, midas_imps)
      
      print("MIDAS done")
    
    }
  }
}

# Save results
write_csv(as.data.frame(results), paste0("adult/adult_test_results.csv"))