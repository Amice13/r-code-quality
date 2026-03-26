library(arm)
library(mi)
library(nnet)
library(foreign)
library(Amelia)
library(norm)

#### Workflow ####
## 1. Generate and save 20 missing datasets using Kropko dgp
## 2. Impute using existing strategies
## 3. Impute MIDAS versions from saved files
## 4. Read in MIDAS complete datasets
## 5. Calculate imputation accuracy


source("kropko/kropko_anes_test_helper.R")
set.seed(89)

#### GENERATE MISSING DATA VERSIONS ####
for (t in 1:20) {
  
  dgp <- .dgp()
  
  imp_meth <- dgp$imp_meth
  na.mat <- is.na(dgp$data)
  miss_data <- dgp$truedata
  
  miss_data$white <- ifelse(miss_data$white == "White",1,0)
  miss_data$female <- ifelse(miss_data$female == "Female",1,0)
  miss_data$imp_enviro <- ifelse(miss_data$imp_enviro == "Important",1,0)
  miss_data$education <- as.numeric(miss_data$education)
  miss_data$income <- as.numeric(miss_data$income)
  miss_data$jobs_r <- as.numeric(miss_data$jobs_r)
  
  if (t == 1) {
    write.csv(miss_data, "kropko/anes_midas/kropko_ANES_data_complete.csv",
              row.names = FALSE)
  }
  
  is.na(miss_data) <- na.mat
  
  # Save data for 
  write.csv(miss_data, paste0("kropko/anes_midas/kropko_ANES_data_missing_",t,".csv"),
            row.names = FALSE)
  
  rm(dgp, miss_data)
  
}

#### IMPUTE USING EXISTING STRATEGIES ####
m = 5
it_imps <- list() # stores lists of imputations for each strategy, per DGP draw
missing_mats <- list() # stores missingness indicator matrices

for (t in 1:20) {
  
  miss_data <- read.csv(paste0("kropko/anes_midas/kropko_ANES_data_missing_",t,".csv"))
  
  # Transform after (for ease of MIDAS imputation)
  miss_data$voteMcCain <- ifelse(miss_data$vote == "McCain",1,0)
  miss_data$voteObama <- ifelse(miss_data$vote == "Obama",1,0)
  
  miss_data$`religionCatholic/Orthodox` <- ifelse(miss_data$religion == "Catholic/Orthodox",1,0)
  miss_data$religionProtestant <- ifelse(miss_data$religion == "Protestant",1,0)
  
  miss_data$marriedMarried <- ifelse(miss_data$married == "Married",1,0)
  miss_data$marriedSingle <- ifelse(miss_data$married == "Single",1,0)
  
  miss_data$vote <- miss_data$religion <- miss_data$married <- NULL
  missing_mats[[t]] <- is.na(miss_data)
  
  ## AMELIA
  amelia_imps <- amelia(miss_data, m = m, p2s = 0)$imputations
  
  ## NORM
  norm_imps <- list()
  for(j in 1:m){
    prelim <- prelim.norm(as.matrix(miss_data))
    text <- capture.output(thetahat <- em.norm(prelim))
    rngseed(round(runif(1, min=0, max=10000000)))
    draws <- imp.norm(prelim, thetahat, miss_data)
    norm_imps[[j]] <- data.frame(draws)
  }
  
  ## MCAR
  mnl_mdf <- missing_data.frame(miss_data)
  # mnl_mdf@variables[["vote"]]@estimator <- "MNL"
  # mnl_mdf@variables[["religion"]]@estimator <- "MNL"
  for(im in seq_along(mnl_mdf@variables)) {
    mnl_mdf@variables[[im]]@imputation_method <- "ppd"
  }
  mcar_mod <- mi(mnl_mdf, n.chains=m, n.iter=0, verbose = FALSE, max.minutes=1000)
  mcar_imps <- lapply(mi::complete(mcar_mod, m),
                      function (x) x[,!grepl("missing_", colnames(x))])
  
  mcar_imps <- lapply(mcar_imps, function(x) {
    as.data.frame(apply(x,2, function(y) as.numeric(as.character(y))))
  })

  
  ## MI:MNL
  # mnl_mod <- mi(mnl_mdf, n.chains=m, n.iter=30, verbose = FALSE, max.minutes=1000)
  # mnl_imps <- mi::complete(mnl_mod, m)
  
  ## MI
  mi_mod <- mi(mnl_mdf, n.chains=m, n.iter=30, verbose = FALSE, max.minutes=1000)
  mi_imps <- lapply(mi::complete(mi_mod, m),
                    function (x) x[,!grepl("missing_", colnames(x))])
  
  mi_imps <- lapply(mi_imps, function(x) {
    apply(x,2, function(y) as.numeric(as.character(y)))
  })
  
  ## MI:RNL
  # rnl_mdf <- missing_data.frame(dgp$data)
  # rnl_mdf@variables[["vote"]]@estimator <- "RNL"
  # rnl_mdf@variables[["religion"]]@estimator <- "RNL"
  # for(im in seq_along(rnl_mdf@variables)) {
  #   rnl_mdf@variables[[im]]@imputation_method <- dgp$imp_meth
  # }
  # rnl_mod <- mi(rnl_mdf, n.chains=m, n.iter=30, verbose = FALSE, max.minutes=1000)
  # rnl_imps <- mi::complete(rnl_mod, m)
  
  it_imps[[t]] <- list(amelia = amelia_imps,
                       norm = norm_imps,
                       mcar = mcar_imps,
                       mi = mi_imps)
                       # mnl = mnl_imps,
                       # rnl = rnl_imps)
  
}

# !! RUN MIDAS NOW !!

# Import MIDAS imputations
df_names <- colnames(it_imps[[1]]$amelia[[1]])
for (t in 1:20) {
  
  mid_imps <- list()
  
  for(j in 1:m){
    
    mid_imp <- read.csv(paste0("kropko/anes_midas/ANES_imp_mid_",t,"_",j,".csv"))
    
    names(mid_imp)[names(mid_imp) == "religion_Catholic.Orthodox"] <- "religionCatholic/Orthodox"
    names(mid_imp)[names(mid_imp) == "religion_Protestant"] <- "religionProtestant"
    names(mid_imp)[names(mid_imp) == "married_Married"] <- "marriedMarried"
    names(mid_imp)[names(mid_imp) == "married_Single"] <- "marriedSingle"
    names(mid_imp)[names(mid_imp) == "vote_McCain"] <- "voteMcCain"
    names(mid_imp)[names(mid_imp) == "vote_Obama"] <- "voteObama"
    
    mid_imps[[j]] <- mid_imp[,df_names] #reorder columns
    
  }
  
  it_imps[[t]]$midas <- mid_imps
}

true_data <- read.csv("kropko/anes_midas/kropko_ANES_data_complete.csv")

true_data$voteMcCain <- ifelse(true_data$vote == "McCain",1,0)
true_data$voteObama <- ifelse(true_data$vote == "Obama",1,0)
true_data$`religionCatholic/Orthodox` <- ifelse(true_data$religion == "Catholic/Orthodox",1,0)
true_data$religionProtestant <- ifelse(true_data$religion == "Protestant",1,0)
true_data$marriedMarried <- ifelse(true_data$married == "Married",1,0)
true_data$marriedSingle <- ifelse(true_data$married == "Single",1,0)

true_data$vote <- true_data$religion <- true_data$married <- NULL

true_data <- true_data[,df_names]

imp_err <- matrix(ncol = 16, nrow = 0)
for (it in 1:20) {
  for (method in c("amelia","norm","mcar","mi","midas")) {
    err <- lapply(it_imps[[it]][[method]], 
                  function (x) sqrt(colMeans((true_data - x)^2)))
    
    avg_err <- Reduce("+",err)/m
    
    imp_err <- rbind(imp_err, c(it, method, avg_err))
  }
}

colnames(imp_err)[1] <- "iteration"
colnames(imp_err)[2] <- "method"

imp_err <- as.data.frame(imp_err)
imp_err$education <- NULL
imp_err$marriedMarried <- NULL
imp_err$marriedSingle <- NULL
imp_err$female <- NULL
imp_err$age <- NULL

write.csv(imp_err,
          "kropko/kropko_anes_results.csv", row.names = FALSE)
