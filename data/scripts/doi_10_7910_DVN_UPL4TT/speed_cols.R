library(haven)
library(Amelia)
library(dplyr)
library(purrr)
library(norm2)
library(mi)
library(doParallel)
library(ggplot2)

# if (!file.exists("speed/data_tmp")) {
#   dir.create("speed/data_tmp")
# } else {
#   unlink("speed/data_tmp", recursive = TRUE)
#   dir.create("speed/data_tmp")
# }

set.seed(89)

#### Function ####

col_select <- function(test_levels, target, run_max = 300) {
  
  columns <- 2
  runs <- 0
  chosen_cols <- c("birthyr","gender")
  
  while (columns <  target & runs < run_max) {
    runs <- runs + 1
    pick <- as.character(sample(test_levels$variable,1))
    
    if (pick %in% chosen_cols) {
      next
    }
    
    pick_cols <- test_levels[test_levels$variable == pick,]$cols
    columns <- columns + pick_cols
    
    if (columns > 1.25*target) {
      columns <- columns - pick_cols
    } else {
      chosen_cols[length(chosen_cols) + 1] <- pick
    }
  }
  
  return(chosen_cols)
}

data_gen <- function(data, miss_prop) {
  
  new_data <- data
  
  for (c in colnames(data)) {
    
    if (c %in% c("birthyr","gender")) {
      next
    }
    
    r <- rbernoulli(nrow(data),p=miss_prop)
    
    # new_data[[c]] <- ifelse(r, NA, data[[c]])
    
    if (c %in% c(profile_bin, profile_cat)) {
      new_data[[c]] <- factor(ifelse(r, NA, levels(new_data[[c]])[new_data[[c]]]), exclude = NA, levels = levels(data[[c]]))
    } else if (c %in% c(profile_ord)) {
      new_data[[c]] <- factor(ifelse(r, NA, levels(new_data[[c]])[new_data[[c]]]), exclude = NA, levels = levels(data[[c]]))
    } else {
      new_data[[c]] <- ifelse(r, NA, data[[c]])
    }
    
  }
  
  return(new_data)
}

#### LOAD DATA ####

cces_subset <- read.csv("data/cces_subset.csv", stringsAsFactors = FALSE)

# Removal of DKs etc. where data is otherwise ordered

cces_subset$trans <- ifelse(cces_subset$trans == 3,NA,cces_subset$trans)
cces_subset$votereg <- ifelse(cces_subset$votereg == 3,NA,cces_subset$votereg)
cces_subset$pid7 <- ifelse(cces_subset$pid7 == 8,NA,cces_subset$pid7)
cces_subset$ideo5 <- ifelse(cces_subset$ideo5 == 6,NA,cces_subset$ideo5)
cces_subset$pew_churatd <- ifelse(cces_subset$pew_churatd == 7,NA,cces_subset$pew_churatd)
cces_subset$pew_prayer <- ifelse(cces_subset$pew_prayer == 8,NA,cces_subset$pew_prayer)
cces_subset$newsint <- ifelse(cces_subset$newsint == 7,NA,cces_subset$newsint)
cces_subset$faminc_new <- ifelse(cces_subset$faminc_new == 97,NA,cces_subset$faminc_new)

cces_subset$CC18_app_dtrmp_post <- ifelse(cces_subset$CC18_app_dtrmp_post == 5,NA,cces_subset$CC18_app_dtrmp_post)

test_data <-  cces_subset %>% filter(complete.cases(.))

# Set column types

profile_bin <- c("gender","pew_bornagain","cit1","investor","trans","votereg","edloan",
                 "CC18_417a_1","CC18_417a_2","CC18_417a_3","CC18_417a_4",
                 "CC18_417a_5","CC18_417a_6","CC18_417a_7","CC18_417a_8",
                 "CC18_418a","CC18_414A","CC18_414B","CC18_414C","CC18_414D","CC18_414E",
                 "CC18_324a","CC18_324b","CC18_324c","CC18_324d",
                 "CC18_415a","CC18_415b","CC18_415c","CC18_415d","CC18_416",
                 "CC18_417_a","CC18_417_b","CC18_417_c","CC18_417_d","CC18_417_e",
                 
                 "healthins_1","healthins_2","healthins_3",
                 "healthins_4","healthins_5","healthins_6","healthins_7",
                 "CC18_300_1","CC18_300_2","CC18_300_3","CC18_300_4",
                 "CC18_300_5","CC18_300_6",
                 "CC18_303_1","CC18_303_2","CC18_303_3","CC18_303_4","CC18_303_5",
                 "CC18_303_6","CC18_303_7","CC18_303_8","CC18_303_9","CC18_303_10","CC18_303_11",
                 "CC18_320a","CC18_320c",
                 "CC18_320d","CC18_321a","CC18_321b","CC18_321c","CC18_321d",
                 "CC18_322a","CC18_322b","CC18_322c_new","CC18_322d_new","CC18_322c","CC18_322f",
                 "CC18_325a","CC18_325b","CC18_325c","CC18_325d","CC18_325e_new","CC18_325f_new",
                 "CC18_326","CC18_327a","CC18_327c","CC18_327d","CC18_327e",
                 "CC18_328b","CC18_328d","CC18_328e","CC18_328f","CC18_331a",
                 "CC18_331b","CC18_331c","CC18_332a","CC18_332b","CC18_332c",
                 "CC18_332e") #1 = yes/male

profile_cont <- c("birthyr", "citylength_1")

profile_ord <- c("pew_religimp","pid7","ideo5","pew_churatd","pew_prayer",
                 "newsint","faminc_new","CC18_421a","CC18_app_dtrmp_post",
                 "CC18_422a","CC18_422b","CC18_422c","CC18_422d",
                 "CC18_422e","CC18_422f","CC18_422g",
                 "CC18_426_1","CC18_426_2","CC18_426_3","CC18_426_4","CC18_426_5",
                 "CC18_427_a","CC18_427_b","CC18_427_c","CC18_427_d",
                 
                 "CC18_302")

profile_cat <- colnames(test_data)[!(colnames(test_data) %in% c(profile_bin, profile_cont, profile_ord))]

# Binary vars
test_data$gender <- ifelse(test_data$gender == 1, "Male","Female")

test_data <- test_data %>% 
  mutate_at(profile_bin[profile_bin != "gender"],
            function(x) ifelse(x == 1,"Yes","No"))

# Categorical/ordinal vars
test_data <- test_data %>% 
  mutate_at(c(profile_cat, profile_bin), as.factor) %>% 
  mutate_at(profile_ord, as.ordered)

# Table of levels

test_levels <- data.frame(variable = c(profile_cont,profile_bin,profile_ord,profile_cat),
                          type = c(rep("continuous",length(profile_cont)),
                                   rep("binary",length(profile_bin)),
                                   rep("ordinal", length(profile_ord)),
                                   rep("categorical", length(profile_cat))),
                          cols = c(rep(1,length(profile_cont)),
                                   rep(2,length(profile_bin)),
                                   rep(NA, length(c(profile_cat, profile_ord)))))

for (v in c(profile_cat,profile_ord)) {
  test_levels[test_levels$variable == v,]$cols <- nlevels(test_data[[v]])
}


#### LOOP ####

targets <- c(25,50,75,100,125,150,175,200,225,250,300,350,400,440)
I <- 10
L <- 10
miss_prop <- 0.5

results_cols <- c("columns","variables","iteration","method","time")
speed_results <- matrix(ncol = length(results_cols), nrow= 0)
colnames(speed_results) <- results_cols

for (l in 1:L) {
  for (t in targets) {
    print(paste0(l,"-",t))
    
    chosen_vars <- col_select(test_levels, t)
    
    missing_data <- data_gen(miss_prop = 0.3, test_data[,chosen_vars])
    
    cats <- profile_cat[profile_cat %in% colnames(missing_data)]
    bins <- profile_bin[profile_bin %in% colnames(missing_data)]
    ords <- profile_ord[profile_ord %in% colnames(missing_data)]
    conts <- profile_cont[profile_cont %in% colnames(missing_data)]
    
    n_cols <- sum(test_levels[test_levels$variable %in% colnames(missing_data),]$cols)
    n_vars <- length(chosen_vars)
    
    if (length(cats) ==  0) {
      cats <- NULL
    } 
    
    if (length(ords) == 0) {
      ords <- NULL
    }
    
    if (length(bins) == 0) {
      bins <- NULL
    }
    
    if (length(conts) == 0) {
      conts <- NULL
    }
    
    #### Amelia ####
    
    amelia_time <- FALSE
    
    try({
      
      amelia_t0 <- proc.time()
      amelia <- amelia(as.data.frame(missing_data), noms = c(cats, bins), ords = ords, m = I) 
      amelia_t1 <- proc.time()
      
      amelia_time <- amelia_t1[3] - amelia_t0[3]
    })
    
    speed_results <- rbind(speed_results,
                           c(n_cols, n_vars, l, "amelia", amelia_time))
    
    #### norm ####
    if (t < 250) {
      norm_t0 <- proc.time()
      emResult <- emNorm(missing_data, iter.max = 1000, estimate.worst =  FALSE)
      
      cl <- makeCluster(detectCores())
      registerDoParallel(cl)
      foreach(m = 1:I, .packages = "norm2",.verbose = TRUE) %dopar% {
        print(paste0("Norm iteration: ",m))
        mcmcResult  <- mcmcNorm(emResult)
        impNorm(mcmcResult)
      }
      stopCluster(cl)
      norm_t1 <- proc.time()
      
      
      norm_time <- norm_t1[3] - norm_t0[3]
      
      speed_results <- rbind(speed_results,
                             c(n_cols, n_vars, l, "norm", norm_time))
    }
    
    #### mi - set up for both MNL and MCAR ####
    
    if (t <= 60) {
      # Use all cores available!
      options(mc.cores = detectCores())
      
      # Construct vector of vars in order of type
      variables <- c(cats, conts, ords, bins)
      # Construct corresponding vector of types, to pass to change function
      variable_types <- c(rep("unordered-categorical",length(cats)),
                          rep("continuous", length(conts)),
                          rep("ordered-categorical",length(ords)),
                          rep("binary",length(bins)))
      
      mi_t0 <- proc.time()
      
      mi_data <- as.data.frame(missing_data) %>% 
        missing_data.frame(.) %>% 
        change(., y = variables,
               what = "type",  to = variable_types)
      
      mi_t1 <- proc.time()
      
      mi_time <- mi_t1[3] - mi_t0[3]
      
      if (t <= 30) {
        ## mi - MNL
        mnl_data <- mi_data
        
        mnl_t0 <- proc.time()
        mnl <- mi(mnl_data, n.iter = 15, n.chains = I, max.minutes = 20)
        mnl_imps <- complete(mnl, I)
        mnl_t1 <- proc.time()
        
        mnl_time <- mi_time + mnl_t1[3] - mnl_t0[3]
        
        speed_results <- rbind(speed_results,
                               c(n_cols, n_vars, l, "mnl", mnl_time))
      }
      
      ## mi - MCAR
      mcar_data <- mi_data
      
      mcar_t0 <- proc.time()
      mcar_algo <- mi(mcar_data, n.iter = 0, n.chains = I, max.minutes = 20)
      mcar_imps <- complete(mcar_algo,I)
      mcar_t1 <- proc.time()
      
      mcar_time <- mi_time + mcar_t1[3] - mcar_t0[3]
      
      speed_results <- rbind(speed_results,
                             c(n_cols, n_vars, l, "mcar", mcar_time))
    }
    
    #### MIDAS ####
    filename <- paste0("speed/data_tmp/cces_cols_midas_data.csv")
    write.csv(missing_data, filename, row.names = FALSE)
    
    markerfile <- "speed/data_tmp/cols_marker.csv"
    
    midas_t0 <- proc.time()
    file.create(markerfile)
    while (file.exists(markerfile)) {
      Sys.sleep(0.5)
    }
    midas_t1 <- proc.time()
    
    midas_time <- midas_t1[3] - midas_t0[3]
    
    speed_results <- rbind(speed_results,
                           c(n_cols, n_vars, l, "midas", midas_time))
  }
}

write.csv(speed_results, "speed/speed_col_results.csv")