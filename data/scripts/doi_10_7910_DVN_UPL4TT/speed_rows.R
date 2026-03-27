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

data_gen <- function(data, miss_prop) {
  
  new_data <- data
  
  for (c in colnames(data)) {
    
    if (c %in% c("birthyr","gender")) {
      next
    }
    
    r <- rbernoulli(nrow(data),p=miss_prop)
    
    if (c %in% c(profile_bin, profile_cat)) {
      new_data[[c]] <- factor(ifelse(r, NA, levels(new_data[[c]])[new_data[[c]]]), exclude = NA, levels = levels(data[[c]]))
    } else if (c %in% c(profile_ord)) {
      new_data[[c]] <- ordered(ifelse(r, NA, levels(new_data[[c]])[new_data[[c]]]), exclude = NA, levels = levels(data[[c]]))
    } else {
      new_data[[c]] <- ifelse(r, NA, data[[c]])
    }
    
  }
  
  return(new_data)
}

#### LOAD DATA ####

select_cols <- c("birthyr","citylength_1","gender", "pew_bornagain",
  "cit1","investor","trans","votereg","pew_religimp",
  "ideo5","pew_churatd","newsint","sexuality","educ",
  "internethome","internetwork","marstat","pid3","ownhome",
  "urbancity","immstat","unionhh")

cces_subset <- read.csv("data/cces_subset.csv", stringsAsFactors = FALSE) %>% 
  select(select_cols)

# Removal of DKs etc. where data is otherwise ordered

cces_subset$trans <- ifelse(cces_subset$trans == 3,NA,cces_subset$trans)
cces_subset$votereg <- ifelse(cces_subset$votereg == 3,NA,cces_subset$votereg)
cces_subset$ideo5 <- ifelse(cces_subset$ideo5 == 6,NA,cces_subset$ideo5)
cces_subset$pew_churatd <- ifelse(cces_subset$pew_churatd == 7,NA,cces_subset$pew_churatd)
cces_subset$newsint <- ifelse(cces_subset$newsint == 7,NA,cces_subset$newsint)

test_data <-  cces_subset %>% filter(complete.cases(.))

# Set column types

profile_bin <- c("gender","pew_bornagain","cit1","investor","trans","votereg") #1 = yes/male

profile_cont <- c("birthyr", "citylength_1")

profile_ord <- c("pew_religimp","pid7","ideo5","pew_churatd","pew_prayer","newsint","faminc_new")

profile_cat <- colnames(test_data)[!(colnames(test_data) %in% c(profile_bin, profile_cont, profile_ord))]

# Binary vars
test_data$gender <-  ifelse(test_data$gender == 1, "Male","Female")
test_data$pew_bornagain <-  ifelse(test_data$pew_bornagain == 1, "Yes","No")
test_data$cit1 <- ifelse(test_data$cit1 == 1,"Yes","No")
test_data$investor <- ifelse(test_data$investor == 1,"Yes","No")
test_data$trans <- ifelse(test_data$trans == 1,"Yes","No")
test_data$votereg <- ifelse(test_data$votereg == 1, "Yes","No")

# Categorical/ordinal vars
test_data <- test_data %>% 
  mutate_at(select_cols[which(select_cols %in% c(profile_cat, profile_bin))], as.factor) %>% 
  mutate_at(select_cols[which(select_cols %in% profile_ord)], as.ordered)

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

row_lengths <- c(5000,10000,50000,100000,250000,500000)
L <- 10
I <- 10
miss_prop <- 0.3

results_cols <- c("rows","iteration","method","time")
speed_row_results <- matrix(ncol = length(results_cols), nrow= 0)
colnames(speed_row_results) <- results_cols

for (l in 1:L) {
  for (r in row_lengths) {
    
    print(paste0(l,"-",r))
    
    rows <- sample(1:nrow(test_data), r, replace = TRUE)
    row_data <- test_data[rows,]
    
    missing_data <- data_gen(row_data, miss_prop)
    
    cats <- select_cols[(select_cols %in% profile_cat)]
    ords <- select_cols[(select_cols %in% profile_ord)]
    bins <- profile_bin
    conts <- profile_cont
    
    print("data gen done")
    #### Amelia ####
    
    amelia_time <- FALSE
    
    try({
      
      amelia_t0 <- proc.time()
      amelia(as.data.frame(missing_data), noms = c(cats, bins), ords = ords, m = I,
             empri = .005*nrow(missing_data)) 
      amelia_t1 <- proc.time()
      
      amelia_time <- amelia_t1[3] - amelia_t0[3]
    })
    
    speed_row_results <- rbind(speed_row_results,
                           c(r, l, "amelia", amelia_time))
    
    print("amelia done")
    
    #### norm ####
    
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

    speed_row_results <- rbind(speed_row_results,
                               c(r, l, "norm", norm_time))
    
    print("norm done")

    #### mi - set up for both MNL and MCAR ####

    if (r < 10000) {
      # Use all cores available!
      options(mc.cores = detectCores())

      # Construct vector of vars in order of type
      variables <- c(cats, conts, bins, ords)
      # Construct corresponding vector of types, to pass to change function
      variable_types <- c(rep("unordered-categorical",length(cats)),
                          rep("continuous", length(conts)),
                          rep("binary",length(bins)),
                          rep("ordered-categorical", length(ords)))

      mi_t0 <- proc.time()

      mi_data <- as.data.frame(missing_data) %>%
        missing_data.frame(.) %>%
        change(., y = variables,
               what = "type",  to = variable_types)

      mi_t1 <- proc.time()

      mi_time <- mi_t1[3] - mi_t0[3]

      ## mi - MCAR
      mcar_data <- mi_data

      mcar_t0 <- proc.time()
      mcar_algo <- mi(mcar_data, n.iter = 0, n.chains = I, max.minutes = 20)
      mcar_imps <- complete(mcar_algo,I)
      mcar_t1 <- proc.time()

      mcar_time <- mi_time + mcar_t1[3] - mcar_t0[3]

      speed_row_results <- rbind(speed_row_results,
                                 c(r, l, "mcar", mcar_time))
      
      print("mcar done")

      ## mi - MNL

      mnl_data <- mi_data

      mnl_t0 <- proc.time()
      mnl <- mi(mnl_data, n.iter = 15, n.chains = I, max.minutes = 20)
      mnl_imps <- complete(mnl, I)
      mnl_t1 <- proc.time()

      mnl_time <- mi_time + mnl_t1[3] - mnl_t0[3]

      speed_row_results <- rbind(speed_row_results,
                             c(r, l, "mnl", mnl_time))
      
      print("mnl done")
    }
    
    #### MIDAS ####
    filename <- paste0("speed/data_tmp/cces_rows_midas_data.csv")
    write.csv(missing_data, filename, row.names = FALSE)
    
    markerfile <- "speed/data_tmp/rows_marker.csv"
    
    file.create(markerfile)
    midas_t0 <- proc.time()
    while (file.exists(markerfile)) {
      Sys.sleep(0.5)
    }
    midas_t1 <- proc.time()
    
    midas_time <- midas_t1[3] - midas_t0[3]
    
    speed_row_results <- rbind(speed_row_results,
                               c(r, l, "midas", midas_time))
    
    file.remove(file.path("speed/data_tmp", list.files("speed/data_tmp")))
    
    print("midas done")
    
  }
}

write.csv(speed_row_results, "speed/speed_row_results.csv", row.names = FALSE)